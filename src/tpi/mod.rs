// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use std::fmt;
use std::marker::PhantomData;
use std::result;

use crate::common::*;
use crate::msf::Stream;
use crate::FallibleIterator;

pub(crate) mod constants;
mod data;
mod header;
mod id;
mod primitive;

use self::header::*;
use self::primitive::type_data_for_primitive;

pub use self::data::*;
pub use self::id::*;
pub use self::primitive::{Indirection, PrimitiveKind, PrimitiveType};

/// An index into either the type stream or id stream.
pub trait ItemIndex: Copy + PartialEq + PartialOrd + From<u32> + Into<u32> {}

/// `TypeInformation` provides zero-copy access to a PDB type data stream.
///
/// PDB type information is stored as a stream of length-prefixed `Type` records, and thus the most
/// fundamental operation supported by `TypeInformation` is to iterate over `Type`s.
///
/// Types are uniquely identified by `TypeIndex`, and types are stored within the PDB in ascending
/// order of `TypeIndex`.
///
/// Many types refer to other types by `TypeIndex`, and these references may refer to other types
/// forming a chain that's arbitrarily long. Fortunately, PDB format requires that types refer only
/// to types with lower `TypeIndex`es; thus, the stream of types form a directed acyclic graph.
///
/// `TypeInformation` can iterate by `TypeIndex`, since that's essentially the only operation
/// permitted by the data. `TypeFinder` is a secondary data structure to provide efficient
/// backtracking.
///
/// # Examples
///
/// Iterating over the types while building a `TypeFinder`:
///
/// ```
/// # use pdb::FallibleIterator;
/// #
/// # fn test() -> pdb::Result<usize> {
/// # let file = std::fs::File::open("fixtures/self/foo.pdb")?;
/// # let mut pdb = pdb::PDB::open(file)?;
///
/// let type_information = pdb.type_information()?;
/// let mut type_finder = type_information.finder();
///
/// # let expected_count = type_information.len();
/// # let mut count: usize = 0;
/// let mut iter = type_information.iter();
/// while let Some(typ) = iter.next()? {
///     // build the type finder as we go
///     type_finder.update(&iter);
///
///     // parse the type record
///     match typ.parse() {
///         Ok(pdb::TypeData::Class(pdb::ClassType {name, properties, fields: Some(fields), ..})) => {
///             // this Type describes a class-like type with fields
///             println!("type {} is a class named {}", typ.index(), name);
///
///             // `fields` is a TypeIndex which refers to a FieldList
///             // To find information about the fields, find and parse that Type
///             match type_finder.find(fields)?.parse()? {
///                 pdb::TypeData::FieldList(list) => {
///                     // `fields` is a Vec<TypeData>
///                     for field in list.fields {
///                         if let pdb::TypeData::Member(member) = field {
///                             // follow `member.field_type` as desired
///                             println!("  - field {} at offset {:x}", member.name, member.offset);
///                         } else {
///                             // handle member functions, nested types, etc.
///                         }
///                     }
///
///                     if let Some(more_fields) = list.continuation {
///                         // A FieldList can be split across multiple records
///                         // TODO: follow `more_fields` and handle the next FieldList
///                     }
///                 }
///                 _ => { }
///             }
///
///         },
///         Ok(_) => {
///             // ignore everything that's not a class-like type
///         },
///         Err(pdb::Error::UnimplementedTypeKind(_)) => {
///             // found an unhandled type record
///             // this probably isn't fatal in most use cases
///         },
///         Err(e) => {
///             // other error, probably is worth failing
///             return Err(e);
///         }
///     }
///     # count += 1;
/// }
///
/// # assert_eq!(expected_count, count);
/// # Ok(count)
/// # }
/// # assert!(test().expect("test") > 8000);
/// ```
#[derive(Debug)]
pub struct ItemInformation<'s, I> {
    stream: Stream<'s>,
    header: Header,
    _ph: PhantomData<&'s I>,
}

impl<'s, I> ItemInformation<'s, I>
where
    I: ItemIndex,
{
    /// Parses `TypeInformation` from raw stream data.
    pub(crate) fn parse(stream: Stream<'s>) -> Result<Self> {
        let mut buf = stream.parse_buffer();
        let header = Header::parse(&mut buf)?;
        let _ph = PhantomData;
        Ok(Self {
            stream,
            header,
            _ph,
        })
    }

    /// Returns an iterator that can traverse the type table in sequential order.
    pub fn iter(&self) -> ItemIter<'_, I> {
        // get a parse buffer
        let mut buf = self.stream.parse_buffer();

        // drop the header
        // this can't fail; we've already read this once
        buf.take(self.header.header_size as usize)
            .expect("dropping TPI header");

        ItemIter {
            buf,
            index: self.header.minimum_index,
            _ph: PhantomData,
        }
    }

    /// Returns the number of types contained in this `TypeInformation`.
    ///
    /// Note that primitive types are not stored in the PDB file, so the number of distinct types
    /// reachable via this `TypeInformation` will be higher than `len()`.
    pub fn len(&self) -> usize {
        (self.header.maximum_index - self.header.minimum_index) as usize
    }

    /// Returns whether this `TypeInformation` contains any types.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns a `TypeFinder` with a default time-space tradeoff.
    ///
    /// The `TypeFinder` is initially empty and must be populated by iterating.
    pub fn finder(&self) -> ItemFinder<'_, I> {
        ItemFinder::new(self, 3)
    }
}

/// This buffer is used when a `Type` refers to a primitive type. It doesn't contain anything
/// type-specific, but it does parse as `raw_type() == 0xffff`, which is a reserved value. Seems
/// like a reasonable thing to do.
const PRIMITIVE_TYPE: &[u8] = b"\xff\xff";

/// Represents a type from the type table. A `Type` has been minimally processed and may not be
/// correctly formed or even understood by this library.
///
/// To avoid copying, `Type`s exist as references to data owned by the parent `TypeInformation`.
/// Therefore, a `Type` may not outlive its parent.
#[derive(Copy, Clone, PartialEq)]
pub struct Item<'t, I> {
    index: I,
    data: &'t [u8],
}

impl<'t, I> Item<'t, I>
where
    I: ItemIndex,
{
    /// Returns this type's `TypeIndex`.
    pub fn index(&self) -> I {
        self.index
    }

    /// Returns the length of this type's data in terms of bytes in the on-disk format.
    ///
    /// Types are prefixed by length, which is not included in this count.
    pub fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns whether this type's data is empty.
    ///
    /// Types are prefixed by length, which is not included in this operation.
    pub fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Returns the kind of type identified by this `Type`.
    ///
    /// As a special case, if this `Type` is actually a primitive type, `raw_kind()` will return
    /// `0xffff`.
    #[inline]
    pub fn raw_kind(&self) -> u16 {
        debug_assert!(self.data.len() >= 2);

        // assemble a little-endian u16
        u16::from(self.data[0]) | (u16::from(self.data[1]) << 8)
    }
}

impl<'t, I> fmt::Debug for Item<'t, I>
where
    I: ItemIndex,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Type{{ kind: 0x{:04x} [{} bytes] }}",
            self.raw_kind(),
            self.data.len()
        )
    }
}

/// A `TypeFinder` is a secondary, in-memory data structure that permits efficiently finding types
/// by `TypeIndex`. It starts out empty and must be populated by calling `update(&TypeIter)` while
/// iterating.
///
/// `TypeFinder` allocates all the memory it needs when it is first created. The footprint is
/// directly proportional to the total number of types; see `TypeInformation.len()`.
///
/// # Time/space trade-off
///
/// The naïve approach is to store the position of each `Type` as they are covered in the stream.
/// The cost is memory: namely one `u32` per `Type`.
///
/// Compare this approach to a `TypeFinder` that stores the position of every Nth type. Memory
/// requirements would be reduced by a factor of N in exchange for requiring an average of (N-1)/2
/// iterations per lookup. However, iteration is cheap sequential memory access, and spending less
/// memory on `TypeFinder` means more of the data can fit in the cache, so this is likely a good
/// trade-off for small-ish values of N.
///
/// `TypeFinder` is parameterized by `shift` which controls this trade-off as powers of two:
///
///   * If `shift` is 0, `TypeFinder` stores 4 bytes per `Type` and always performs direct lookups.
///   * If `shift` is 1, `TypeFinder` stores 2 bytes per `Type` and averages 0.5 iterations per lookup.
///   * If `shift` is 2, `TypeFinder` stores 1 byte per `Type` and averages 1.5 iterations per lookup.
///   * If `shift` is 3, `TypeFinder` stores 4 bits per `Type` and averages 3.5 iterations per lookup.
///   * If `shift` is 4, `TypeFinder` stores 2 bits per `Type` and averages 7.5 iterations per lookup.
///   * If `shift` is 5, `TypeFinder` stores 1 bit per `Type` and averages 15.5 iterations per lookup.
///
/// This list can continue but with rapidly diminishing returns. Iteration cost is proportional to
/// type size, which varies, but typical numbers from a large program are:
///
///   * 24% of types are    12 bytes
///   * 34% of types are <= 16 bytes
///   * 84% of types are <= 32 bytes
///
/// A `shift` of 2 or 3 is likely appropriate for most workloads. 500K types would require 1 MB or
/// 500 KB of memory respectively, and lookups -- though indirect -- would still usually need only
/// one or two 64-byte cache lines.
#[derive(Debug)]
pub struct ItemFinder<'t, I> {
    buffer: ParseBuffer<'t>,
    minimum_index: u32,
    maximum_index: u32,
    positions: Vec<u32>,
    shift: u8,
    _ph: PhantomData<&'t I>,
}

impl<'t, I> ItemFinder<'t, I>
where
    I: ItemIndex,
{
    fn new(info: &'t ItemInformation<'_, I>, shift: u8) -> Self {
        let count = info.header.maximum_index - info.header.minimum_index;
        let shifted_count = (count >> shift) as usize;

        let mut positions = Vec::with_capacity(shifted_count);

        // add record zero, which is identical regardless of shift
        positions.push(info.header.header_size);

        Self {
            buffer: info.stream.parse_buffer(),
            minimum_index: info.header.minimum_index,
            maximum_index: info.header.maximum_index,
            positions,
            shift,
            _ph: PhantomData,
        }
    }

    /// Given a `TypeIndex`, find which position in the Vec we should jump to and how many times we
    /// need to iterate to find the requested type.
    ///
    /// `shift` refers to the size of these bit shifts.
    #[inline]
    fn resolve(&self, type_index: u32) -> (usize, usize) {
        let raw = type_index - self.minimum_index;
        (
            (raw >> self.shift) as usize,
            (raw & ((1 << self.shift) - 1)) as usize,
        )
    }

    /// Returns the highest `TypeIndex` which is currently served by this `TypeFinder`.
    ///
    /// In general, you shouldn't need to consider this. Types always refer to types with lower
    /// `TypeIndex`es, and either:
    ///
    ///  * You obtained a `Type` by iterating, in which case you should be calling `update()` as you
    ///    iterate, and in which case all types it can reference are <= `max_index()`, or
    ///  * You got a `Type` from this `TypeFinder`, in which case all types it can reference are
    ///    still <= `max_index()`.
    ///
    #[inline]
    pub fn max_index(&self) -> I {
        I::from((self.positions.len() << self.shift) as u32 + self.minimum_index - 1)
    }

    /// Update this `TypeFinder` based on the current position of a `TypeIter`.
    ///
    /// Do this each time you call `.next()`.
    #[inline]
    pub fn update(&mut self, iterator: &ItemIter<'t, I>) {
        let (vec_index, iteration_count) = self.resolve(iterator.index);
        if iteration_count == 0 && vec_index == self.positions.len() {
            let pos = iterator.buf.pos();
            assert!(pos < u32::max_value() as usize);
            self.positions.push(pos as u32);
        }
    }

    /// Find a type by `TypeIndex`.
    ///
    /// # Errors
    ///
    /// * `Error::TypeNotFound(type_index)` if you ask for a type that doesn't exist
    /// * `Error::TypeNotIndexed(type_index, max_index)` if you ask for a type that is known
    ///   to exist but is not currently known by this `TypeFinder`.
    pub fn find(&self, index: I) -> Result<Item<'t, I>> {
        let index: u32 = index.into();
        if index < self.minimum_index {
            return Ok(Item {
                index: I::from(index),
                data: PRIMITIVE_TYPE,
            });
        } else if index > self.maximum_index {
            return Err(Error::TypeNotFound(index));
        }

        // figure out where we'd find this
        let (vec_index, iteration_count) = self.resolve(index);

        if let Some(pos) = self.positions.get(vec_index) {
            // hit
            let mut buf = self.buffer.clone();

            // jump forwards
            buf.take(*pos as usize)?;

            // skip some records
            for _ in 0..iteration_count {
                let length = buf.parse_u16()?;
                buf.take(length as usize)?;
            }

            // read the type
            let length = buf.parse_u16()?;

            Ok(Item {
                index: I::from(index),
                data: buf.take(length as usize)?,
            })
        } else {
            // miss
            Err(Error::TypeNotIndexed(index, self.max_index().into()))
        }
    }
}

/// A `TypeIter` iterates over a `TypeInformation`, producing `Types`s.
///
/// Type streams are represented internally as a series of records, each of which have a length, a
/// type, and a type-specific field layout. Iteration performance is therefore similar to a linked
/// list.
#[derive(Debug)]
pub struct ItemIter<'t, I> {
    buf: ParseBuffer<'t>,
    index: u32,
    _ph: PhantomData<&'t I>,
}

impl<'t, I> FallibleIterator for ItemIter<'t, I>
where
    I: ItemIndex,
{
    type Item = Item<'t, I>;
    type Error = Error;

    fn next(&mut self) -> result::Result<Option<Self::Item>, Self::Error> {
        // see if we're at EOF
        if self.buf.is_empty() {
            return Ok(None);
        }

        // read the length of the next type
        let length = self.buf.parse_u16()? as usize;

        // validate
        if length < 2 {
            // this can't be correct
            return Err(Error::TypeTooShort);
        }

        // grab the type itself
        let type_buf = self.buf.take(length)?;
        let index = self.index;

        self.index += 1;

        // Done
        Ok(Some(Item {
            index: I::from(index),
            data: type_buf,
        }))
    }
}

impl ItemIndex for TypeIndex {}

pub type TypeInformation<'s> = ItemInformation<'s, TypeIndex>;
pub type Type<'t> = Item<'t, TypeIndex>;
pub type TypeFinder<'t> = ItemFinder<'t, TypeIndex>;

impl<'t> Item<'t, TypeIndex> {
    /// Parse this Type into a TypeData.
    ///
    /// # Errors
    ///
    /// * `Error::UnimplementedTypeKind(kind)` if the type record isn't currently understood by this
    ///   library
    /// * `Error::UnexpectedEof` if the type record is malformed
    pub fn parse(&self) -> Result<TypeData<'t>> {
        if self.index < TypeIndex(0x1000) {
            // Primitive type
            type_data_for_primitive(self.index)
        } else {
            let mut buf = ParseBuffer::from(self.data);
            parse_type_data(&mut buf)
        }
    }
}

impl ItemIndex for IdIndex {}

pub type IdInformation<'s> = ItemInformation<'s, IdIndex>;
pub type Id<'t> = Item<'t, IdIndex>;
pub type IdFinder<'t> = ItemFinder<'t, IdIndex>;

impl<'t> Item<'t, IdIndex> {
    /// Parse this Id into a IdData.
    ///
    /// # Errors
    ///
    /// * `Error::UnimplementedTypeKind(kind)` if the type record isn't currently understood by this
    ///   library
    /// * `Error::UnexpectedEof` if the type record is malformed
    pub fn parse(&self) -> Result<IdData<'t>> {
        ParseBuffer::from(self.data).parse()
    }
}
