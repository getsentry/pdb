// Copyright 2018 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use common::*;
use byteorder::{ByteOrder,LittleEndian,ReadBytesExt};
use msf::Stream;
use std::cmp::Ordering;

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
/// OMAP tables are dense arrays, sequentially storing records of the form:
///
/// ```
/// struct Record {
///     source_address: u32,
///     target_address: u32
/// }
/// ```
///
/// Each table is sorted by source address.
///
/// As a (potentially) special case, `target_address` can be zero, which seems to indicate that the
/// `source_address` does not exist in the target address space. However, zero may not be a
/// strictly invalid address, as RVA zero points to the PE header.
///
/// Each record applies to a range of addresses: i.e. record N indicates that addresses in the
/// half-open interval [ `record[n].source_address`, `record[n+1].source_address` ) were relocated
/// to a starting address of `record[n].target_address`. If `target_address` is zero, the `lookup()`
/// will return zero, since this seems more generally correct than treating it as an offset.
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
    pub fn new(stream: Stream<'s>) -> Result<OMAPTable> {
        if stream.as_slice().len() % 8 != 0 {
            Err(Error::UnimplementedFeature("OMAP tables must be a multiple of the record size"))
        } else {
            Ok(OMAPTable{
                stream
            })
        }
    }

    #[inline]
    fn records(&self) -> usize {
        self.stream.as_slice().len() / 8
    }

    #[inline]
    fn read_source_address(&self, n: usize) -> u32 {
        let bytes = self.stream.as_slice();
        let offset = n * 8;
        LittleEndian::read_u32(&bytes[offset..offset+4])
    }

    #[inline]
    fn read_target_address(&self, n: usize) -> u32 {
        let bytes = self.stream.as_slice();
        let offset = n * 8 + 4;
        LittleEndian::read_u32(&bytes[offset..offset+4])
    }

    /// Look up `source_address` to yield a target address.
    ///
    /// Note that `lookup()` can return zero, which (probably) means that `source_address` does not
    /// exist in the target address space. This is not a lookup failure per sé, so it's not a
    /// `Result::Error`, and zero _is_ a valid address, so it's not an `Option::None`. It's just
    /// zero.
    pub fn lookup(&self, source_address: u32) -> u32 {
        // We want to search using Price Is Right rules: closest without going over wins.
        let n = {
            let mut size = self.records();
            let mut left = 0usize;
            while size > 1 {
                size /= 2;
                let center = left + size;
                let center_address = self.read_source_address(center);
                match center_address.cmp(&source_address) {
                    Ordering::Less => {
                        // go right
                        left = center;
                    },
                    Ordering::Greater => {
                        // go left
                        left = left;
                    },
                    Ordering::Equal => {
                        // exact match
                        left = center;
                        break;
                    }
                };
            }
            left
        };

        let record_source_address = self.read_source_address(n);
        let record_target_address = self.read_target_address(n);

        assert!(record_source_address <= source_address);

        if record_target_address == 0 {
            0
        } else {
            (source_address - record_source_address) + record_target_address
        }
    }
}
