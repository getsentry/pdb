// Copyright 2026 PDB Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! PDB Information Stream parsing.
//!
//! The PDB information stream (stream 1) contains metadata about the PDB file,
//! including the GUID, age, and a map of named streams.
//!
//! # Stream Names Map Format
//!
//! The names map provides a mapping from stream names to stream indices.
//! It consists of a block of data comprising the names as null-terminated C strings,
//! followed by a map of stream indices to the offset of their names within the names block.
//!
//! The map itself is stored as:
//! - A 32-bit count of the number of entries
//! - A 32-bit value giving the number of bytes taken up by the entries
//! - Two sets: one for names that are present, and one for names that have been deleted
//! - The map entries, each a pair of 32-bit values: offset into names block and stream ID
//!
//! The two sets are each stored as a bit array, which consists of a 32-bit count,
//! followed by that many 32-bit words containing the bits in the array.
//!
//! ## References
//!
//! - [nmtni.h](https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/include/nmtni.h#L76)
//! - [map.h](https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/include/map.h#L474)
//! - [iset.h](https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/include/iset.h#L62)
//! - [array.h](https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/include/array.h#L209)

use core::convert::TryInto;
use core::mem;
use core::slice::Iter;

#[cfg(feature = "alloc")]
use alloc::vec;

#[cfg(feature = "alloc")]
use alloc::vec::Vec;

use uuid::Uuid;

use crate::common::*;

#[cfg(feature = "alloc")]
use crate::dbi::PdbHeaderVersion;

#[cfg(feature = "alloc")]
use crate::msf::*;

/// A 32-bit timestamp written into the PDB info stream when the file was written.
///
/// Microsoft's PDB writer sets this field to the result of `time()` at the
/// moment the file was created, so it usually holds a Unix timestamp (seconds
/// since 1970-01-01 UTC). It is not a magic value and it does not reliably
/// identify a specific PDB: two PDBs written in the same second collide, which
/// is part of why the GUID was introduced in VC7.
///
/// Prefer matching a PDB to an image by GUID + age when both are present. Use
/// the signature only as a coarse sanity check, or when reading PDBs from VC6
/// and earlier that predate the GUID field.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PDBSignature(u32);

impl PDBSignature {
    /// Creates a `PDBSignature` from the raw 32-bit value 
    pub const fn new(value: u32) -> Self {
        Self(value)
    }

    /// Decodes the signature as a UTC date-time.
    #[cfg(feature = "chrono")]
    pub const fn to_datetime(self) -> Option<chrono::DateTime<chrono::Utc>> {
        chrono::DateTime::from_timestamp(self.0 as i64, 0)
    }

    /// Formats the signature as an RFC 3339 string.
    #[cfg(all(feature = "chrono", feature = "alloc"))]
    pub fn to_rfc3339(self) -> Option<alloc::string::String> {
        self.to_datetime().map(|dt| dt.to_rfc3339())
    }
}

/// A PDB info stream header parsed from a stream.
///
/// The [PDB information stream] contains the GUID and age fields that can be used to
/// verify that a PDB file matches a specific binary, as well a list of named PDB streams
/// with their stream indices.
///
/// [PDB information stream]: http://llvm.org/docs/PDB/PdbStream.html
#[cfg(feature = "alloc")]
#[derive(Debug)]
pub struct PDBInformation<'s> {
    stream: Stream<'s>,

    /// The version of the PDB format in use.
    pub version: PdbHeaderVersion,
    
    /// A 32-bit timestamp.
    pub signature: PDBSignature,
    
    /// The number of times this PDB file has been written.
    ///
    /// This number is bumped by the linker and other tools every time the PDB is modified. It does
    /// not necessarily correspond to the age declared in the image. Consider using
    /// [`DebugInformation::age`](crate::DebugInformation::age) for a better match.
    ///
    /// This PDB matches an image, if the `guid` values match and the PDB age is equal or higher
    /// than the image's age.
    pub age: u32,
    
    /// A `Uuid` generated when this PDB file was created that should uniquely identify it.
    ///
    /// `None` for PDBs predating VC7 (info stream version < `20000404`),
    /// which do not include a GUID field. See [`PdbHeaderVersion::has_guid`].
    pub guid: Option<Uuid>,
    
    /// The offset of the start of the stream name data within the stream.
    pub names_offset: usize,
    
    /// The size of the stream name data, in bytes.
    pub names_size: usize,
}

#[cfg(feature = "alloc")]
impl<'s> PDBInformation<'s> {
    /// Parses a `PDBInformation` from raw stream data.
    pub(crate) fn parse(stream: Stream<'s>) -> Result<Self> {
        let mut buf = stream.parse_buffer();

        let version = PdbHeaderVersion::from(buf.parse_u32()?);
        let signature = PDBSignature(buf.parse_u32()?);
        let age = buf.parse_u32()?;

        let guid = if version.has_guid() {
            Some(Uuid::from_bytes_le(buf.take_array()?))
        } else {
            None
        };

        let names_size = buf.parse_u32()? as usize;
        let names_offset = buf.pos();

        Ok(PDBInformation {
            stream,
            version,
            signature,
            age,
            guid,
            names_size,
            names_offset,
        })
    }

    /// Get a `StreamNames` object that can be used to iterate over named streams contained
    /// within the PDB file.
    ///
    /// This can be used to look up certain PDB streams by name.
    ///
    /// # Example
    ///
    /// ```
    /// # use pdb::FallibleIterator;
    /// #
    /// # fn test() -> pdb::Result<()> {
    /// let file = std::fs::File::open("fixtures/self/foo.pdb")?;
    /// let mut pdb = pdb::PDB::open(file)?;
    /// let info = pdb.pdb_information()?;
    /// let names = info.stream_names()?;
    /// let mut v: Vec<_> = names.iter().map(|n| n.name.to_string()).collect();
    /// v.sort();
    /// assert_eq!(&v, &["mystream", "/LinkInfo", "/names", "/src/headerblock"]);
    /// # Ok(())
    /// # }
    /// ```
    #[cfg(feature = "alloc")]
    pub fn stream_names(&self) -> Result<StreamNames<'_>> {
      
        let mut names = vec![];
        let mut buf = self.stream.parse_buffer();

        buf.take(self.names_offset + self.names_size)?;
        let count = buf.parse_u32()?;

        let _entries_size = buf.parse_u32()?;
        let ok_words = buf.parse_u32()?;
        let _ok_bits = buf.take(ok_words as usize * mem::size_of::<u32>())?;
        let deleted_words = buf.parse_u32()?;
        let _deleted_bits = buf.take(deleted_words as usize * mem::size_of::<u32>())?;

        let mut names_reader = self.stream.parse_buffer();
        names_reader.take(self.names_offset)?;

        let names_buf = names_reader.take(self.names_size)?;
        for _ in 0..count {
            let name_offset = buf.parse_u32()? as usize;
            let stream_id = StreamIndex(buf.parse_u32()? as u16);
            let name = ParseBuffer::from(&names_buf[name_offset..]).parse_cstring()?;
            names.push(StreamName { name, stream_id });
        }

        Ok(StreamNames(names.into()))
    }

    /// Returns a lazy iterator over named streams
    #[cfg(feature = "alloc")]
    pub fn stream_names_iter(&self) -> Result<StreamNamesIter<'_>> {
        let mut buf = self.stream.parse_buffer();
        
        buf.take(self.names_offset + self.names_size)?;
        let count = buf.parse_u32()?;
        let _entries_size = buf.parse_u32()?;
        let ok_words = buf.parse_u32()?;
        let _ok_bits = buf.take(ok_words as usize * mem::size_of::<u32>())?;
        let deleted_words = buf.parse_u32()?;
        let _deleted_bits = buf.take(deleted_words as usize * mem::size_of::<u32>())?;
        
        let mut names_reader = self.stream.parse_buffer();
        names_reader.take(self.names_offset)?;
        let names_buf = names_reader.take(self.names_size)?;
        
        Ok(StreamNamesIter {
            buf,
            names_buf,
            count,
            index: 0,
        })
    }
}

/// A named stream contained within the PDB file.
#[derive(Debug)]
pub struct StreamName<'n> {
    /// The stream's name.
    pub name: RawString<'n>,
    /// The index of this stream.
    pub stream_id: StreamIndex,
}

/// A lazy iterator over named streams in the PDB.
///
/// This iterator parses stream names on-demand without allocating a Vec.
#[cfg(feature = "alloc")]
#[derive(Debug)]
pub struct StreamNamesIter<'s> {
    /// The buffer containing the map entries
    buf: ParseBuffer<'s>,
    /// The buffer containing the name data
    names_buf: &'s [u8],
    /// Number of entries to iterate over
    count: u32,
    /// Current index
    index: u32,
}

#[cfg(feature = "alloc")]
impl<'s> StreamNamesIter<'s> {
    /// Returns the next stream name, or `None` if all entries have been consumed.
    ///
    /// # Errors
    ///
    /// Returns an error if the data is malformed.
    fn next_inner(&mut self) -> Result<StreamName<'s>> {
        let name_offset = self.buf.parse_u32()? as usize;
        let stream_id = StreamIndex(self.buf.parse_u32()? as u16);
        let name = ParseBuffer::from(&self.names_buf[name_offset..]).parse_cstring()?;
        Ok(StreamName { name, stream_id })
    }
}

#[cfg(feature = "alloc")]
impl<'s> Iterator for StreamNamesIter<'s> {
    type Item = Result<StreamName<'s>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.count {
            return None;
        }
        self.index += 1;
        Some(self.next_inner())
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = (self.count - self.index) as usize;
        (remaining, Some(remaining))
    }
}

/// A list of named streams contained within the PDB file.
///
/// Call [`StreamNames::iter`] to iterate over the names. The iterator produces [`StreamName`]
/// objects.
#[cfg(feature = "alloc")]
#[derive(Debug)]
pub struct StreamNames<'s>(Box<[StreamName<'s>]>);

/// An iterator over [`StreamName`]s.
pub type NameIter<'a, 'n> = Iter<'a, StreamName<'n>>;

#[cfg(feature = "alloc")]
impl<'s> StreamNames<'s> {
    /// Return an iterator over named streams and their stream indices.
    #[inline]
    pub fn iter(&self) -> NameIter<'_, 's> {
        self.0.iter()
    }
}

#[cfg(feature = "alloc")]
impl<'a, 's> IntoIterator for &'a StreamNames<'s> {
    type Item = &'a StreamName<'s>;
    type IntoIter = NameIter<'a, 's>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
