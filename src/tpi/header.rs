// Copyright 2026 PDB Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use crate::{PdbHeaderVersion, common::*};

/// A slice descriptor referencing a portion of a stream.
///
/// Contains an offset and size pair used to locate data within the TPI stream.
/// The offset is stored as a signed 32-bit value (matching the original PDB
/// implementation's "long" type), while the size is unsigned.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Slice {
    /// Offset into the stream (signed 32-bit, from the original PDB code)
    pub offset: i32,
    /// Size of the slice in bytes
    pub size: u32,
}

impl Slice {
    pub const fn default() -> Self {
        Self { offset: 0, size: 0 }
    }
}

/// TPI (Type Provider Index) stream header.
///
/// Contains metadata about the type information stream, including version
/// information, type index ranges, and hash table layout for fast type lookup.
///
/// # Reference
///
/// Based on the Microsoft PDB implementation:
/// <https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/tpi.h#L45>
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct TPIHeader {
     /// TPI stream version number
    pub version: u32,
    /// Size of this header in bytes
    pub header_size: u32,
    /// First valid type index
    pub minimum_index: u32,
    /// Last valid type index
    pub maximum_index: u32,
    /// Size of the global type record buffer
    pub gprec_size: u32,
    /// Stream containing the type hash table
    pub tpi_hash_stream: u16,
    /// Stream containing hash table padding
    pub tpi_hash_pad_stream: u16,
    /// Size of hash keys in bytes
    pub hash_key_size: u32,
    /// Number of buckets in the hash table
    pub hash_bucket_size: u32,
    /// Hash values array
    pub hash_values: Slice,
    /// Type index offset table
    pub ti_off: Slice,
    /// Hash chain heads: maps (hash value, type index) to the head of each hash chain
    pub hash_adj: Slice,
}

impl TPIHeader {
    pub(crate) const fn empty() -> Self {
        let empty_slice = Slice::default();

        Self {
            version: 0,
            header_size: 0,
            minimum_index: 0,
            maximum_index: 0,
            gprec_size: 0,
            tpi_hash_stream: 0,
            tpi_hash_pad_stream: 0,
            hash_key_size: 0,
            hash_bucket_size: 0,
            hash_values: empty_slice,
            ti_off: empty_slice,
            hash_adj: empty_slice,
        }
    }

    pub(crate) fn parse(buf: &mut ParseBuffer<'_>) -> Result<Self> {
        debug_assert!(buf.pos() == 0);

        if buf.is_empty() {
            // Special case when the buffer is completely empty. This indicates a missing TPI or IPI
            // stream. In this case, `ItemInformation` acts like an empty shell that never resolves
            // any types.
            return Ok(Self::empty());
        }

        let version: u32 = buf.parse()?;

        // Reject streams whose first 4 bytes aren't a recognized PDB stream version.
        // This catches things like VC6's stream 4 (a hash table, not an IPI stream)
        // which would otherwise be mis-parsed as a TPI header.
        if matches!(PdbHeaderVersion::from(version), PdbHeaderVersion::OtherValue(_)) {
            return Err(Error::InvalidTypeInformationHeader(
                "unrecognized type information stream version",
            ));
        }

        let header = Self {
            version,
            header_size: buf.parse()?,
            minimum_index: buf.parse()?,
            maximum_index: buf.parse()?,
            gprec_size: buf.parse()?,
            tpi_hash_stream: buf.parse()?,
            tpi_hash_pad_stream: buf.parse()?,
            hash_key_size: buf.parse()?,
            hash_bucket_size: buf.parse()?,
            hash_values: Slice {
                offset: buf.parse()?,
                size: buf.parse()?,
            },
            ti_off: Slice {
                offset: buf.parse()?,
                size: buf.parse()?,
            },
            hash_adj: Slice {
                offset: buf.parse()?,
                size: buf.parse()?,
            },
        };

        let bytes_read = buf.pos() as u32;
        if header.header_size < bytes_read {
            return Err(Error::InvalidTypeInformationHeader(
                "header size is impossibly small",
            ));
        } else if header.header_size > 1024 {
            return Err(Error::InvalidTypeInformationHeader(
                "header size is unreasonably large",
            ));
        }

        buf.take((header.header_size - bytes_read) as usize)?;

        if header.minimum_index < 4096 {
            return Err(Error::InvalidTypeInformationHeader(
                "minimum type index is < 4096",
            ));
        }
        if header.maximum_index < header.minimum_index {
            return Err(Error::InvalidTypeInformationHeader(
                "maximum type index is < minimum type index",
            ));
        }

        Ok(header)
    }
}
