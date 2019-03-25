// Copyright 2018 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

//! Utilities for translating addresses between PDB offsets and _Relative Virtual Addresses_ (RVAs).

use std::cmp::Ordering;
use std::fmt;
use std::mem;
use std::slice;

use crate::common::*;
use crate::msf::Stream;
use crate::pe::ImageSectionHeader;

/// A address translation record from an `OMAPTable`.
///
/// This record applies to the half-open interval [ `record.source_address`,
/// `next_record.source_address` ).
#[repr(C, packed)]
#[derive(Clone, Copy, Eq, PartialEq)]
pub(crate) struct OMAPRecord {
    source_address: u32,
    target_address: u32,
}

impl OMAPRecord {
    /// Returns the address in the source space.
    #[inline]
    pub fn source_address(self) -> u32 {
        u32::from_le(self.source_address)
    }

    /// Returns the start of the mapped portion in the target address space.
    #[inline]
    pub fn target_address(self) -> u32 {
        u32::from_le(self.target_address)
    }
}

impl fmt::Debug for OMAPRecord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("OMAPRecord")
            .field("source_address", &FixedHexFmt(self.source_address()))
            .field("target_address", &FixedHexFmt(self.target_address()))
            .finish()
    }
}

impl PartialOrd for OMAPRecord {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.source_address().partial_cmp(&other.source_address())
    }
}

impl Ord for OMAPRecord {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        self.source_address().cmp(&other.source_address())
    }
}

/// PDBs can contain OMAP tables, which translate relative virtual addresses (RVAs) from one address
/// space into another.
///
/// For more information on the pratical use of OMAPs, see the [module level documentation] and
/// [`AddressMap`]. A PDB can contain two OMAPs:
///
///  - `omap_from_src`: A mapping from the original address space to the transformed address space
///    of an optimized binary. Use `PDB::omap_from_src` to obtain an instance of this OMAP. Also,
///    `PdbInternalRva::rva` performs this conversion in a safe manner.
///  - `omap_to_src`: A mapping from the transformed address space back into the original address
///    space of the unoptimized binary. Use `PDB::omap_to_src` to obtain an instace of this OMAP.
///    Also, `Rva::original_rva` performs this conversion in a safe manner.
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
///
/// [module level documentation]: ./index.html
/// [`AddressMap`]: struct.AddressMap.html
pub(crate) struct OMAPTable<'s> {
    stream: Stream<'s>,
}

impl<'s> OMAPTable<'s> {
    pub(crate) fn parse(stream: Stream<'s>) -> Result<Self> {
        if stream.as_slice().len() % 8 != 0 {
            Err(Error::InvalidStreamLength("OMAP"))
        } else {
            Ok(OMAPTable { stream })
        }
    }

    /// Returns a direct view onto the records stored in this OMAP table.
    #[inline]
    pub fn records(&self) -> &'s [OMAPRecord] {
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

impl fmt::Debug for OMAPTable<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_tuple("OMAPTable").field(&self.records()).finish()
    }
}

/// A mapping between addresses and offsets used in the PDB and PE file.
///
/// To obtain an instace of this address map, call `PDB::address_map`. It will determine the correct
/// translation mode and read all internal state from the PDB. Then use the conversion methods on
/// the address and offset types to translate addresses.
///
/// # Background
///
/// Addresses in PDBs are stored as offsets into sections of the PE file. The `AddressMap` contains
/// the PE's section headers to translate between the offsets and virtual addresses relative to the
/// image base (RVAs).
///
/// Additionally, Microsoft has been reordering the Windows system and application binaries to
/// optimize them for paging reduction, using a toolset reported to be derived from and/or built on
/// top of the [Vulcan research project]. Relatively little else is known about the tools or the
/// methods they use. Looking at Windows system binaries like `ntoskrnl.exe`, it is apparent that
/// their layout has been rearranged, and their respective symbol files contain _OMAP_ re-mapping
/// information. The [Microsoft Binary Technologies Projects] may be involved in this.
///
/// The internals of this transformation are not well understood. According to [1997 reference
/// material]:
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
/// # Usage
///
/// To aid with translating addresses and offsets, this module exposes `AddressMap`, a helper that
/// contains all information to apply the correct translation of any kind of address or offset to
/// another. Due to the rearranging optimizations, there are four types involved:
///
///  - [`Rva`]: A _Relative Virtual Address_ in the actual binary. This address directly corresponds
///    to instruction pointers seen in stack traces and symbol addresses reported by debuggers.
///  - [`PdbInternalRva`]: An RVA as it would have appeared before the optimization. This value does
///    not have any practical use, as it never occurs in the PDB or the actual binary.
///  - [`SectionOffset`]: An offset into a section of the actual binary. A `section` member of _n_
///    refers to section _n - 1_, which makes a section number of _0_ a null pointer.
///  - [`PdbInternalSectionOffset`]: An offset into a section of the original binary. These offsets
///    are used throughout the PDB and can be converted to either `SectionOffset`, or directly to
///    `Rva` in the actual address space.
///
/// For binaries that have not been optimized that way, the `PdbInternal*` values are effectively
/// equal to their regular counterparts and the conversion between the two are no-ops. Address
/// translation still has to assume different address spaces, which is why there is no direct
/// conversion without an `AddressMap`.
///
/// # Example
///
/// ```rust
/// # use pdb::{Rva, FallibleIterator};
/// #
/// # fn test() -> pdb::Result<()> {
/// # let source = std::fs::File::open("fixtures/self/foo.pdb")?;
/// let mut pdb = pdb::PDB::open(source)?;
///
/// // Compute the address map once and reuse it
/// let address_map = pdb.address_map()?;
///
/// # let symbol_table = pdb.global_symbols()?;
/// # let symbol = symbol_table.iter().next()?.unwrap();
/// # match symbol.parse() { Ok(pdb::SymbolData::PublicSymbol(pubsym)) => {
/// // Obtain some section offset, eg from a symbol, and convert it
/// match pubsym.offset.to_rva(&address_map) {
///     Some(rva) => {
///         println!("symbol is at {}", rva);
/// #       assert_eq!(rva, Rva(26048));
///     }
///     None => {
///         println!("symbol refers to eliminated code");
/// #       panic!("symbol should exist");
///     }
/// }
/// # } _ => unreachable!() }
/// # Ok(())
/// # }
/// # test().unwrap()
/// ```
///
/// [Vulcan research project]: https://research.microsoft.com/pubs/69850/tr-2001-50.pdf
/// [Microsoft Binary Technologies Projects]: https://microsoft.com/windows/cse/bit_projects.mspx
/// [1997 reference material]: https://www.microsoft.com/msj/0597/hood0597.aspx
/// [`Rva`]: struct.Rva.html
/// [`PdbInternalRva`]: struct.PdbInternalRva.html
/// [`SectionOffset`]: struct.SectionOffset.html
/// [`PdbInternalSectionOffset`]: struct.PdbInternalSectionOffset.html
#[derive(Debug)]
pub struct AddressMap<'s> {
    pub(crate) original_sections: Vec<ImageSectionHeader>,
    pub(crate) transformed_sections: Option<Vec<ImageSectionHeader>>,
    pub(crate) transformed_to_original: Option<OMAPTable<'s>>,
    pub(crate) original_to_transformed: Option<OMAPTable<'s>>,
}

fn get_section_offset(sections: &[ImageSectionHeader], address: u32) -> Option<(u16, u32)> {
    // Section headers are sorted by virtual_address, so we only need to iterate until we exceed
    // the desired address. Since the number of section headers is relatively low, a sequential
    // search is the fastest option here.
    let (index, section) = sections
        .iter()
        .take_while(|s| s.virtual_address <= address)
        .enumerate()
        .find(|(_, s)| address < s.virtual_address + s.size_of_raw_data)?;

    Some((index as u16 + 1, address - section.virtual_address))
}

fn get_virtual_address(sections: &[ImageSectionHeader], section: u16, offset: u32) -> Option<u32> {
    let section = sections.get(section as usize - 1)?;
    Some(section.virtual_address + offset)
}

impl Rva {
    /// Resolves a PDB-internal Relative Virtual Address.
    ///
    /// This address is not necessarily compatible with the executable's address space and should
    /// therefore not be used for debugging purposes.
    pub fn to_internal_rva(self, translator: &AddressMap<'_>) -> Option<PdbInternalRva> {
        match translator.transformed_to_original {
            Some(ref omap) => omap.lookup(self.0).map(PdbInternalRva),
            None => Some(PdbInternalRva(self.0)),
        }
    }

    /// Resolves the section offset in the PE headers.
    ///
    /// This is an offset into PE section headers of the executable. To retrieve section offsets
    /// used in the PDB, use [`to_internal_offset`] instead.
    ///
    /// [`to_internal_offset`]: struct.Rva.html#method.to_internal_offset
    pub fn to_section_offset(self, translator: &AddressMap<'_>) -> Option<SectionOffset> {
        let (section, offset) = match translator.transformed_sections {
            Some(ref sections) => get_section_offset(sections, self.0)?,
            None => get_section_offset(&translator.original_sections, self.0)?,
        };

        Some(SectionOffset { section, offset })
    }

    /// Resolves the PDB internal section offset.
    ///
    /// This is the offset value used in the PDB file. To index into the actual PE section headers,
    /// use [`to_section_offset`] instead.
    ///
    /// [`to_section_offset`]: struct.Rva.html#method.to_section_offset
    pub fn to_internal_offset(
        self,
        translator: &AddressMap<'_>,
    ) -> Option<PdbInternalSectionOffset> {
        self.to_internal_rva(translator)?
            .to_internal_offset(translator)
    }
}

impl PdbInternalRva {
    /// Resolves an actual Relative Virtual Address in the executable's address space.
    pub fn to_rva(self, translator: &AddressMap<'_>) -> Option<Rva> {
        match translator.original_to_transformed {
            Some(ref omap) => omap.lookup(self.0).map(Rva),
            None => Some(Rva(self.0)),
        }
    }

    /// Resolves the section offset in the PE headers.
    ///
    /// This is an offset into PE section headers of the executable. To retrieve section offsets
    /// used in the PDB, use [`to_internal_offset`] instead.
    ///
    /// [`to_internal_offset`]: struct.PdbInternalRva.html#method.to_internal_offset
    pub fn to_section_offset(self, translator: &AddressMap<'_>) -> Option<SectionOffset> {
        self.to_rva(translator)?.to_section_offset(translator)
    }

    /// Resolves the PDB internal section offset.
    ///
    /// This is the offset value used in the PDB file. To index into the actual PE section headers,
    /// use [`to_section_offset`] instead.
    ///
    /// [`to_section_offset`]: struct.Rva.html#method.to_section_offset
    pub fn to_internal_offset(
        self,
        translator: &AddressMap<'_>,
    ) -> Option<PdbInternalSectionOffset> {
        let (section, offset) = get_section_offset(&translator.original_sections, self.0)?;
        Some(PdbInternalSectionOffset { section, offset })
    }
}

impl SectionOffset {
    /// Resolves an actual Relative Virtual Address in the executable's address space.
    pub fn to_rva(self, translator: &AddressMap<'_>) -> Option<Rva> {
        let address = match translator.transformed_sections {
            Some(ref sections) => get_virtual_address(sections, self.section, self.offset)?,
            None => get_virtual_address(&translator.original_sections, self.section, self.offset)?,
        };

        Some(Rva(address))
    }

    /// Resolves a PDB-internal Relative Virtual Address.
    ///
    /// This address is not necessarily compatible with the executable's address space and should
    /// therefore not be used for debugging purposes.
    pub fn to_internal_rva(self, translator: &AddressMap<'_>) -> Option<PdbInternalRva> {
        self.to_rva(translator)?.to_internal_rva(translator)
    }

    /// Resolves the PDB internal section offset.
    pub fn to_internal_offset(
        self,
        translator: &AddressMap<'_>,
    ) -> Option<PdbInternalSectionOffset> {
        if translator.transformed_sections.is_none() {
            // Fast path to avoid section table lookups
            let Self { section, offset } = self;
            return Some(PdbInternalSectionOffset { section, offset });
        }

        self.to_internal_rva(translator)?
            .to_internal_offset(translator)
    }
}

impl PdbInternalSectionOffset {
    /// Resolves an actual Relative Virtual Address in the executable's address space.
    pub fn to_rva(self, translator: &AddressMap<'_>) -> Option<Rva> {
        self.to_internal_rva(translator)?.to_rva(translator)
    }

    /// Resolves a PDB-internal Relative Virtual Address.
    ///
    /// This address is not necessarily compatible with the executable's address space and should
    /// therefore not be used for debugging purposes.
    pub fn to_internal_rva(self, translator: &AddressMap<'_>) -> Option<PdbInternalRva> {
        get_virtual_address(&translator.original_sections, self.section, self.offset)
            .map(PdbInternalRva)
    }

    /// Resolves the section offset in the PE headers.
    pub fn to_section_offset(self, translator: &AddressMap<'_>) -> Option<SectionOffset> {
        if translator.transformed_sections.is_none() {
            // Fast path to avoid section table lookups
            let Self { section, offset } = self;
            return Some(SectionOffset { section, offset });
        }

        self.to_rva(translator)?.to_section_offset(translator)
    }
}
