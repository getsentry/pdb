// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use std::fmt;
use std::result;

use common::*;
use msf::Stream;
use FallibleIterator;

pub(crate) mod constants;
mod data;
mod header;
mod primitive;

use self::data::parse_type_data;
use self::header::*;
use self::primitive::type_data_for_primitive;

pub use self::data::*;
pub use self::primitive::{Indirection, PrimitiveKind, PrimitiveType};

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
/// let mut type_finder = type_information.new_type_finder();
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
///             println!("type {} is a class named {}", typ.type_index(), name);
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
pub struct TypeInformation<'t> {
    stream: Stream<'t>,
    header: Header,
}

impl<'t> TypeInformation<'t> {
    /// Returns an iterator that can traverse the type table in sequential order.
    pub fn iter(&self) -> TypeIter {
        // get a parse buffer
        let mut buf = self.stream.parse_buffer();

        // drop the header
        // this can't fail; we've already read this once
        buf.take(self.header.header_size as usize)
            .expect("dropping TPI header");

        TypeIter {
            buf,
            type_index: self.header.minimum_type_index,
        }
    }

    /// Returns the number of types contained in this `TypeInformation`.
    ///
    /// Note that primitive types are not stored in the PDB file, so the number of distinct types
    /// reachable via this `TypeInformation` will be higher than `len()`.
    pub fn len(&self) -> usize {
        (self.header.maximum_type_index - self.header.minimum_type_index) as usize
    }

    /// Returns whether this `TypeInformation` contains any types.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns a `TypeFinder` with a default time-space tradeoff.
    ///
    /// The `TypeFinder` is initially empty and must be populated by iterating.
    pub fn new_type_finder(&self) -> TypeFinder {
        new_type_finder(self, 3)
    }
}

pub(crate) fn new_type_information(stream: Stream) -> Result<TypeInformation> {
    let header = {
        let mut buf = stream.parse_buffer();
        Header::parse(&mut buf)?
    };

    Ok(TypeInformation { stream, header })
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
pub struct Type<'t>(TypeIndex, &'t [u8]);

impl<'t> Type<'t> {
    /// Returns this type's `TypeIndex`.
    pub fn type_index(&self) -> TypeIndex {
        self.0
    }

    /// Returns the length of this type's data in terms of bytes in the on-disk format.
    ///
    /// Types are prefixed by length, which is not included in this count.
    pub fn len(&self) -> usize {
        self.1.len()
    }

    /// Returns whether this type's data is empty.
    ///
    /// Types are prefixed by length, which is not included in this operation.
    pub fn is_empty(&self) -> bool {
        self.1.is_empty()
    }

    /// Returns the kind of type identified by this `Type`.
    ///
    /// As a special case, if this `Type` is actually a primitive type, `raw_kind()` will return
    /// `0xffff`.
    #[inline]
    pub fn raw_kind(&self) -> u16 {
        debug_assert!(self.1.len() >= 2);

        // assemble a little-endian u16
        u16::from(self.1[0]) | (u16::from(self.1[1]) << 8)
    }

    /// Parse this Type into a TypeData.
    ///
    /// # Errors
    ///
    /// * `Error::UnimplementedTypeKind(kind)` if the type record isn't currently understood by this
    ///   library
    /// * `Error::UnexpectedEof` if the type record is malformed
    pub fn parse(&self) -> Result<TypeData<'t>> {
        if self.0 < 0x1000 {
            // Primitive type
            type_data_for_primitive(self.0)
        } else {
            let mut buf = ParseBuffer::from(self.1);
            parse_type_data(&mut buf)
        }
    }
}

impl<'t> fmt::Debug for Type<'t> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Type{{ kind: 0x{:4x} [{} bytes] }}",
            self.raw_kind(),
            self.1.len()
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
pub struct TypeFinder<'t> {
    buffer: ParseBuffer<'t>,
    minimum_type_index: TypeIndex,
    maximum_type_index: TypeIndex,
    positions: Vec<u32>,
    shift: u8,
}

fn new_type_finder<'b, 't: 'b>(type_info: &'b TypeInformation<'t>, shift: u8) -> TypeFinder<'b> {
    let count = type_info.header.maximum_type_index - type_info.header.minimum_type_index;
    let shifted_count = (count >> shift) as usize;

    let mut positions = Vec::with_capacity(shifted_count);

    // add record zero, which is identical regardless of shift
    positions.push(type_info.header.header_size);

    TypeFinder {
        buffer: type_info.stream.parse_buffer(),
        minimum_type_index: type_info.header.minimum_type_index,
        maximum_type_index: type_info.header.maximum_type_index,
        positions,
        shift,
    }
}

impl<'t> TypeFinder<'t> {
    /// Given a `TypeIndex`, find which position in the Vec we should jump to and how many times we
    /// need to iterate to find the requested type.
    ///
    /// `shift` refers to the size of these bit shifts.
    #[inline]
    fn resolve(&self, type_index: TypeIndex) -> (usize, usize) {
        let raw = type_index - self.minimum_type_index;
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
    ///    iterate, and in which case all types it can reference are <= `max_indexed_type()`, or
    ///  * You got a `Type` from this `TypeFinder`, in which case all types it can reference are
    ///    still <= `max_indexed_type()`.
    ///
    #[inline]
    pub fn max_indexed_type(&self) -> TypeIndex {
        (self.positions.len() << self.shift) as TypeIndex + self.minimum_type_index - 1
    }

    /// Update this `TypeFinder` based on the current position of a `TypeIter`.
    ///
    /// Do this each time you call `.next()`.
    #[inline]
    pub fn update(&mut self, iterator: &TypeIter) {
        let (vec_index, iteration_count) = self.resolve(iterator.type_index);
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
    /// * `Error::TypeNotIndexed(type_index, max_indexed_type)` if you ask for a type that is known
    ///   to exist but is not currently known by this `TypeFinder`.
    pub fn find(&self, type_index: TypeIndex) -> Result<Type<'t>> {
        if type_index < self.minimum_type_index {
            return Ok(Type(type_index, PRIMITIVE_TYPE));
        } else if type_index > self.maximum_type_index {
            return Err(Error::TypeNotFound(type_index));
        }

        // figure out where we'd find this
        let (vec_index, iteration_count) = self.resolve(type_index);

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
            Ok(Type(type_index, buf.take(length as usize)?))
        } else {
            // miss
            Err(Error::TypeNotIndexed(type_index, self.max_indexed_type()))
        }
    }
}

/// A `TypeIter` iterates over a `TypeInformation`, producing `Types`s.
///
/// Type streams are represented internally as a series of records, each of which have a length, a
/// type, and a type-specific field layout. Iteration performance is therefore similar to a linked
/// list.
#[derive(Debug)]
pub struct TypeIter<'t> {
    buf: ParseBuffer<'t>,
    type_index: TypeIndex,
}

impl<'t> FallibleIterator for TypeIter<'t> {
    type Item = Type<'t>;
    type Error = Error;

    fn next(&mut self) -> result::Result<Option<Self::Item>, Self::Error> {
        // see if we're at EOF
        if self.buf.len() == 0 {
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
        let my_type_index = self.type_index;

        self.type_index += 1;

        // Done
        Ok(Some(Type(my_type_index, type_buf)))
    }
}
