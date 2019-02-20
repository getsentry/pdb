// Copyright 2018 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use byteorder::{ByteOrder, LittleEndian};
use common::*;
use msf::Stream;
use std::cmp::Ordering;
use std::mem;
use std::slice;

/// A address translation record from an `OMAPTable`.
///
/// This record applies to the half-open interval [ `record.source_address`,
/// `next_record.source_address` ).
#[repr(C, packed)]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct OMAPRecord {
    source_address: [u8; 4],
    target_address: [u8; 4],
}

impl OMAPRecord {
    /// Returns the address in the source space.
    #[inline]
    pub fn source_address(self) -> u32 {
        LittleEndian::read_u32(&self.source_address)
    }

    /// Returns the start of the mapped portion in the target address space.
    #[inline]
    pub fn target_address(self) -> u32 {
        LittleEndian::read_u32(&self.target_address)
    }
}

impl PartialOrd for OMAPRecord {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.source_address.partial_cmp(&other.source_address)
    }
}

impl Ord for OMAPRecord {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.source_address.cmp(&other.source_address)
    }
}

/// PDBs can contain OMAP tables, which translate relative virtual addresses (RVAs) from one address
/// space into another.
///
/// How can executables end up in a situation needing such translation? This is not well understood,
/// but according to [1997 reference material](https://www.microsoft.com/msj/0597/hood0597.aspx):
///
/// > Yet another form of debug information is relatively new and undocumented, except for a few
/// > obscure references in `WINNT.H` and the Win32 SDK help. This type of information is known as
/// > OMAP. Apparently, as part of Microsoft's internal build procedure, small fragments of code in
/// > EXEs and DLLs are moved around to put the most commonly used code at the beginning of the code
/// > section. This presumably keeps the process memory working set as small as possible. However,
/// > when shifting around the blocks of code, the corresponding debug information isn't updated.
/// > Instead, OMAP information is created. It lets symbol table code translate between the original
/// > address in a symbol table and the modified address where the variable or line of code really
/// > exists in memory.
///
/// Normal build processes build and link an executable, create a corresponding PDB, and that's the
/// end of it. Executable addresses are PDB addresses and vice-versa.
///
/// However: Microsoft uses unknown tools to subsequently rearrange certain linked executables. A
/// naïve and unsuspecting user would imagine such tools would additionally rearrange the PDB, but
/// no. Instead, these tools leave PDB internals alone, store copies of both the old _and_ the new
/// executable section headers, generate both a "PDB to executable" OMAP table and an "executable to
/// PDB" OMAP table, and require the user to reference these tables whenever appropriate.
///
/// # Structure
///
/// OMAP tables are dense arrays, sequentially storing `OMAPRecord` structs sorted by source
/// address.
///
/// Each record applies to a range of addresses: i.e. record N indicates that addresses in the
/// half-open interval [ `record[n].source_address`, `record[n+1].source_address` ) were relocated
/// to a starting address of `record[n].target_address`. If `target_address` is zero, the `lookup()`
/// will return None, since this indicates a non-existent location in the target address space.
///
/// Given that the table is sorted, lookups by source address can be efficiently serviced using a
/// binary search directly against the underlying data without secondary data structures. This is
/// not the most cache efficient data structure (especially given that half of each cache line is
/// storing target addresses), but given that OMAP tables are an uncommon PDBs feature, the obvious
/// binary search implementation seems appropriate.
#[derive(Debug)]
pub struct OMAPTable<'s> {
    stream: Stream<'s>,
}

impl<'s> OMAPTable<'s> {
    pub(crate) fn parse(stream: Stream<'s>) -> Result<Self> {
        if stream.as_slice().len() % 8 != 0 {
            Err(Error::UnimplementedFeature(
                "OMAP tables must be a multiple of the record size",
            ))
        } else {
            Ok(OMAPTable { stream })
        }
    }

    /// Returns a direct view onto the records stored in this OMAP table.
    #[inline]
    pub fn records(&self) -> &[OMAPRecord] {
        let bytes = self.stream.as_slice();
        unsafe {
            slice::from_raw_parts(
                bytes.as_ptr() as *const OMAPRecord,
                bytes.len() / mem::size_of::<OMAPRecord>(),
            )
        }
    }

    /// Look up `source_address` to yield a target address.
    ///
    /// Note that `lookup()` can return zero, which (probably) means that `source_address` does not
    /// exist in the target address space. This is not a lookup failure per sé, so it's not a
    /// `Result::Error`, and zero _is_ a valid address, so it's not an `Option::None`. It's just
    /// zero.
    pub fn lookup(&self, source_address: u32) -> Option<u32> {
        let records = self.records();

        let index = match records.binary_search_by_key(&source_address, |r| r.source_address()) {
            Ok(i) => i,
            Err(0) => return None,
            Err(i) => i - 1,
        };

        let record = records[index];

        // As a special case, `target_address` can be zero, which seems to indicate that the
        // `source_address` does not exist in the target address space.
        if record.target_address() == 0 {
            return None;
        }

        debug_assert!(record.source_address() <= source_address);
        Some((source_address - record.source_address()) + record.target_address())
    }
}
