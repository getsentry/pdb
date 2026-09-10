// Copyright 2026 PDB Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

// DBI = "Debug Information"

#[cfg(feature = "alloc")]
use alloc::borrow::Cow;

use core::fmt;
use core::result;

use crate::common::*;

#[cfg(feature = "alloc")]
use crate::msf::*;

use crate::{FallibleIterator, SectionCharacteristics};

/// Provides access to the "DBI" stream inside the PDB.
///
/// This is only minimally implemented; it's really just so `PDB` can find the global symbol table.
///
/// # Example
///
/// ```
/// # use pdb::FallibleIterator;
/// #
/// # fn test() -> pdb::Result<usize> {
/// let file = std::fs::File::open("fixtures/self/foo.pdb")?;
/// let mut pdb = pdb::PDB::open(file)?;
///
/// let dbi = pdb.debug_information()?;

///
/// # let mut count: usize = 0;
/// let mut modules = dbi.modules()?;
/// while let Some(module) = modules.next()? {
///     println!("module name: {}, object file name: {}",
///              module.module_name(), module.object_file_name());
/// #   count += 1;
/// }
///
/// # Ok(count)
/// # }
/// # assert!(test().expect("test") == 194);
#[cfg(feature = "alloc")]
#[derive(Debug)]
pub struct DebugInformation<'s> {
    stream: Stream<'s>,
    header: DBIHeader,
    header_len: usize,
}

#[cfg(feature = "alloc")]
impl<'s> DebugInformation<'s> {
    #[cfg(feature = "alloc")]
    pub(crate) fn parse(stream: Stream<'s>) -> Result<Self> {
        let mut buf = stream.parse_buffer();
        let header = DBIHeader::parse_buf(&mut buf)?;
        let header_len = buf.pos();

        Ok(DebugInformation {
            stream,
            header,
            header_len,
        })
    }

    /// Returns a copy of the parsed DBI header.
    pub(crate) const fn header(&self) -> DBIHeader {
        self.header
    }

    /// Returns the target's machine type (architecture).
    pub fn machine_type(&self) -> Result<MachineType> {
        Ok(self.header.machine_type.into())
    }

    /// Returns this PDB's original `age`.
    ///
    /// This number is written by the linker and should be equal to the image's `age` value. In
    /// contrast, [`PDBInformation::age`] may be bumped by other tools and should be greater or
    /// equal to the image's `age` value.
    ///
    /// Old PDB files may not specify an age, in which case only [`PDBInformation::age`] should be
    /// checked for matching the image.
    ///
    /// [`PDBInformation::age`]: crate::PDBInformation::age
    pub const fn age(&self) -> Option<u32> {
        match self.header.age {
            0 => None,
            age => Some(age),
        }
    }

    /// Returns an iterator that can traverse the modules list in sequential order.
    pub fn modules(&self) -> Result<ModuleIter<'_>> {
        let mut buf = self.stream.parse_buffer();
        // drop the header
        buf.take(self.header_len)?;
        let modules_buf = buf.take(self.header.module_list_size as usize)?;
        
        Ok(ModuleIter::new(modules_buf))
    }

    /// Returns an iterator that can traverse the section contributions list in sequential order.
    pub fn section_contributions(&self) -> Result<DBISectionContributionIter<'_>> {
        let mut buf = self.stream.parse_buffer();
        // drop the header and modules list
        buf.take(self.header_len + self.header.module_list_size as usize)?;
        let contributions_buf = buf.take(self.header.section_contribution_size as usize)?;
        DBISectionContributionIter::parse(contributions_buf.into())
    }
}

/// Version of the DBI stream header.
///
/// See: <https://llvm.org/docs/PDB/DbiStream.html>
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[non_exhaustive]
#[allow(missing_docs)]
pub enum DbiVersion {
    V41,
    V50,
    V60,
    V70,
    V80,
    V110,
    V140,
    OtherValue(u32),
}

impl From<u32> for DbiVersion {
    #[allow(clippy::inconsistent_digit_grouping)]
    fn from(v: u32) -> Self {
        match v {
            930_803     => Self::V41,
            1996_0307   => Self::V50,
            1997_0606   => Self::V60,
            1999_0903   => Self::V70,
            2003_0901   => Self::V80,
            2009_1201   => Self::V110,
            2014_0508   => Self::V140,
            _ => Self::OtherValue(v),
        }
    }
}

/// The version of the PDB format.
///
/// This version type is used in multiple locations: the DBI header and the PDBI header.
///
/// The numeric values are taken from LLVM's `PdbStreamVersion` enum.
/// See: <https://llvm.org/docs/PDB/PdbStream.html#stream-header>
#[non_exhaustive]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[allow(missing_docs)]
pub enum PdbHeaderVersion {
    /// Visual C++ 2.0
    Vc2,
    /// Visual C++ 4.0
    Vc4,
    /// Visual C++ 4.1
    Vc41,
    /// Visual C++ 5.0
    Vc50,
    /// Visual C++ 6.0 / Visual Studio 98
    Vc98,
    /// Visual C++ 7.0, pre-release (deprecated)
    Vc70Dep,
    /// Visual C++ 7.0 / Visual Studio .NET (2002)
    Vc70,
    /// Visual C++ 8.0 / Visual Studio 2005
    Vc80,
    /// Visual C++ 11.0 / Visual Studio 2012
    Vc110,
    /// Visual C++ 14.0 / Visual Studio 2015
    Vc140,
    /// A version value not covered by the variants above.
    OtherValue(u32),
}

impl fmt::Display for PdbHeaderVersion {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Vc2 => f.write_str("VC2"),
            Self::Vc4 => f.write_str("VC4"),
            Self::Vc41 => f.write_str("VC4.1"),
            Self::Vc50 => f.write_str("VC5"),
            Self::Vc98 => f.write_str("VC6"),
            Self::Vc70Dep => f.write_str("VC7 (pre-release)"),
            Self::Vc70 => f.write_str("VC7"),
            Self::Vc80 => f.write_str("VC8"),
            Self::Vc110 => f.write_str("VC11"),
            Self::Vc140 => f.write_str("VC14"),
            Self::OtherValue(v) => write!(f, "Unknown (0x{v:08x})"),
        }
    }
}

impl PdbHeaderVersion {
    /// Returns `true` if this version's info stream includes a GUID field.
    ///
    /// The GUID was introduced in VC7 (`20000404`). Earlier PDBs go
    /// directly from `age` to `names_size`.
    pub const fn has_guid(self) -> bool {
        match self {
            Self::Vc2 | Self::Vc4 | Self::Vc41 | Self::Vc50 | Self::Vc98 | Self::Vc70Dep => false,
            Self::Vc70 | Self::Vc80 | Self::Vc110 | Self::Vc140 => true,
            Self::OtherValue(v) => v >= 2000_0404,
        }
    }
}

impl From<u32> for PdbHeaderVersion {
    #[allow(clippy::inconsistent_digit_grouping)]
    fn from(v: u32) -> Self {
        match v {
            1994_1610 => Self::Vc2,
            1995_0623 => Self::Vc4,
            1995_0814 => Self::Vc41,
            1996_0307 => Self::Vc50,
            1997_0604 => Self::Vc98,
            1999_0604 => Self::Vc70Dep,
            2000_0404 => Self::Vc70,
            2003_0901 => Self::Vc80,
            2009_1201 => Self::Vc110,
            2014_0508 => Self::Vc140,

            1996_1031 => Self::Vc98,   // VC5/VC6 TPI
            2004_0203 => Self::Vc80,   // VC8+ TPI

            _ => Self::OtherValue(v),
        }
    }
}

/// DBI (Debug Information) stream header - `NewDBIHdr` format.
///
/// The DBI header appears at the start of the DBI stream (stream 3) and contains
/// metadata about the debug information, including stream indices for symbols,
/// module information, and various debug subsections.
///
/// # Reference
///
/// Based on the Microsoft PDB implementation:
/// <https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.h#L124>
#[derive(Debug, Copy, Clone)]
#[allow(dead_code)]
pub struct DBIHeader {
    /// DBI stream signature (should be `0xffffffff`)
    pub signature: u32,

    /// DBI format version (e.g., `VC70`, `VC80`, `VC110`)
    pub version: DbiVersion,

    /// DBI age (incremented when the PDB is modified)
    pub age: u32,

    /// Stream index containing global symbols
    pub gs_symbols_stream: StreamIndex,

    /// Internal version of the PDB DLL that last built this PDB.
    ///
    /// This is a packed field containing version information.
    /// See the union in the reference for bit layout.
    pub internal_version: u16,

    /// Stream index containing public symbols
    pub ps_symbols_stream: StreamIndex,

    /// Build version of the PDB DLL that last built this PDB
    pub pdb_dll_build_version: u16,

    /// Stream index containing symbol records
    pub symbol_records_stream: StreamIndex,

    /// Rbld (rebuild) version of the PDB DLL that last built this PDB
    pub pdb_dll_rbld_version: u16,

    /// Size of the module list substream (in bytes)
    pub module_list_size: u32,

    /// Size of the section contribution substream (in bytes)
    pub section_contribution_size: u32,

    /// Size of the section map substream (in bytes)
    pub section_map_size: u32,

    /// Size of the file info substream (in bytes)
    pub file_info_size: u32,

    /// Size of the Type Server Map substream (in bytes)
    pub type_server_map_size: u32,

    /// Index of the MFC type server
    pub mfc_type_server_index: u32,

    /// Size of optional DbgHdr info appended to the end of the stream (in bytes)
    pub debug_header_size: u32,

    /// Size of the EC (Edit & Continue) substream (in bytes)
    ///
    /// If 0, EC is not enabled for this module.
    pub ec_substream_size: u32,

    /// DBI flags.
    ///
    /// Bit layout:
    /// - Bit 0: `fIncLink` - true if linked incrementally (ilink thunks present)
    /// - Bit 1: `fStripped` - true if private data was stripped (via PDB::CopyTo)
    /// - Bit 2: `fCTypes` - true if using CTypes
    /// - Bits 3-15: Reserved (must be 0)
    pub flags: u16,

    /// Target machine type (e.g., `IMAGE_FILE_MACHINE_AMD64`, `IMAGE_FILE_MACHINE_I386`)
    pub machine_type: u16,

    /// Reserved field (should be 0)
    pub reserved: u32,
}

impl DBIHeader {
    /// Parses a `DBIHeader` from a stream.
    #[cfg(feature = "alloc")]
    pub fn parse(stream: Stream<'_>) -> Result<Self> {
        Self::parse_buf(&mut stream.parse_buffer())
    }

    /// Parses a `DBIHeader` from a [`ParseBuffer`] positioned at the start of
    /// the DBI stream.
    pub fn parse_buf(buf: &mut ParseBuffer<'_>) -> Result<Self> {
        let header = Self {
            signature: buf.parse_u32()?,
            version: From::from(buf.parse_u32()?),
            age: buf.parse_u32()?,
            gs_symbols_stream: buf.parse()?,
            internal_version: buf.parse_u16()?,
            ps_symbols_stream: buf.parse()?,
            pdb_dll_build_version: buf.parse_u16()?,
            symbol_records_stream: buf.parse()?,
            pdb_dll_rbld_version: buf.parse_u16()?,
            module_list_size: buf.parse_u32()?,
            section_contribution_size: buf.parse_u32()?,
            section_map_size: buf.parse_u32()?,
            file_info_size: buf.parse_u32()?,
            type_server_map_size: buf.parse_u32()?,
            mfc_type_server_index: buf.parse_u32()?,
            debug_header_size: buf.parse_u32()?,
            ec_substream_size: buf.parse_u32()?,
            flags: buf.parse_u16()?,
            machine_type: buf.parse_u16()?,
            reserved: buf.parse_u32()?,
        };

        if header.signature != u32::MAX {
            // this is likely a DBIHdr, not a NewDBIHdr
            // it could be promoted:
            //   https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.cpp#L291-L313
            //   https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/langapi/include/pdb.h#L1180-L1184
            // but that seems like a lot of work
            return Err(Error::UnimplementedFeature("ancient DBI header"));
        }

        Ok(header)
    }
}

/// The target machine's architecture.
/// Reference: <https://docs.microsoft.com/en-us/windows/desktop/debug/pe-format#machine-types>
#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MachineType {
    /// The contents of this field are assumed to be applicable to any machine type.
    Unknown = 0x0,
    /// Matsushita AM33
    Am33 = 0x13,
    /// x64
    Amd64 = 0x8664,
    /// ARM little endian
    Arm = 0x1C0,
    /// ARM64 little endian
    Arm64 = 0xAA64,
    /// ARM Thumb-2 little endian
    ArmNT = 0x1C4,
    /// EFI byte code
    Ebc = 0xEBC,
    /// Intel 386 or later processors and compatible processors
    X86 = 0x14C,
    /// Intel Itanium processor family
    Ia64 = 0x200,
    /// Mitsubishi M32R little endian
    M32R = 0x9041,
    /// MIPS16
    Mips16 = 0x266,
    /// MIPS with FPU
    MipsFpu = 0x366,
    /// MIPS16 with FPU
    MipsFpu16 = 0x466,
    /// Power PC little endian
    PowerPC = 0x1F0,
    /// Power PC with floating point support
    PowerPCFP = 0x1F1,
    /// MIPS little endian
    R4000 = 0x166,
    /// RISC-V 32-bit address space
    RiscV32 = 0x5032,
    /// RISC-V 64-bit address space
    RiscV64 = 0x5064,
    /// RISC-V 128-bit address space
    RiscV128 = 0x5128,
    /// Hitachi SH3
    SH3 = 0x1A2,
    /// Hitachi SH3 DSP
    SH3DSP = 0x1A3,
    /// Hitachi SH4
    SH4 = 0x1A6,
    /// Hitachi SH5
    SH5 = 0x1A8,
    /// Thumb
    Thumb = 0x1C2,
    /// MIPS little-endian WCE v2
    WceMipsV2 = 0x169,
    /// Invalid value
    Invalid = 0xffff,
}

impl fmt::Display for MachineType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Invalid => write!(f, "Invalid"),
            Self::Unknown => write!(f, "Unknown"),
            Self::Am33 => write!(f, "Am33"),
            Self::Amd64 => write!(f, "Amd64"),
            Self::Arm => write!(f, "Arm"),
            Self::Arm64 => write!(f, "Arm64"),
            Self::ArmNT => write!(f, "ArmNT"),
            Self::Ebc => write!(f, "Ebc"),
            Self::X86 => write!(f, "X86"),
            Self::Ia64 => write!(f, "Ia64"),
            Self::M32R => write!(f, "M32R"),
            Self::Mips16 => write!(f, "Mips16"),
            Self::MipsFpu => write!(f, "MipsFpu"),
            Self::MipsFpu16 => write!(f, "MipsFpu16"),
            Self::PowerPC => write!(f, "PowerPC"),
            Self::PowerPCFP => write!(f, "PowerPCFP"),
            Self::R4000 => write!(f, "R4000"),
            Self::RiscV32 => write!(f, "RiscV32"),
            Self::RiscV64 => write!(f, "RiscV64"),
            Self::RiscV128 => write!(f, "RiscV128"),
            Self::SH3 => write!(f, "SH3"),
            Self::SH3DSP => write!(f, "SH3DSP"),
            Self::SH4 => write!(f, "SH4"),
            Self::SH5 => write!(f, "SH5"),
            Self::Thumb => write!(f, "Thumb"),
            Self::WceMipsV2 => write!(f, "WceMipsV2"),
        }
    }
}

impl From<u16> for MachineType {
    fn from(value: u16) -> Self {
        match value {
            0xffff => Self::Invalid,
            0x0 => Self::Unknown,
            0x13 => Self::Am33,
            0x8664 => Self::Amd64,
            0x1C0 => Self::Arm,
            0xAA64 => Self::Arm64,
            0x1C4 => Self::ArmNT,
            0xEBC => Self::Ebc,
            0x14C => Self::X86,
            0x200 => Self::Ia64,
            0x9041 => Self::M32R,
            0x266 => Self::Mips16,
            0x366 => Self::MipsFpu,
            0x466 => Self::MipsFpu16,
            0x1F0 => Self::PowerPC,
            0x1F1 => Self::PowerPCFP,
            0x166 => Self::R4000,
            0x5032 => Self::RiscV32,
            0x5064 => Self::RiscV64,
            0x5128 => Self::RiscV128,
            0x1A2 => Self::SH3,
            0x1A3 => Self::SH3DSP,
            0x1A6 => Self::SH4,
            0x1A8 => Self::SH5,
            0x1C2 => Self::Thumb,
            0x169 => Self::WceMipsV2,
            _ => Self::Unknown,
        }
    }
}

/// Information about a module's contribution to a section.
/// `struct SC` in Microsoft's code:
/// <https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/include/dbicommon.h#L42>
#[derive(Debug, Copy, Clone)]
pub struct DBISectionContribution {
    /// Start offset of the section.
    pub offset: PdbInternalSectionOffset,
    /// The size of the contribution, in bytes.
    pub size: u32,
    /// The characteristics, which map to [`ImageSectionHeader::characteristics`] in binaries.
    ///
    /// [`ImageSectionHeader::characteristics`]: crate::ImageSectionHeader::characteristics
    pub characteristics: SectionCharacteristics,
    /// Index of the module in [`DebugInformation::modules`] containing the actual symbol.
    pub module: usize,
    /// CRC of the contribution(?)
    pub data_crc: u32,
    /// CRC of relocations(?)
    pub reloc_crc: u32,
}

impl DBISectionContribution {
    fn parse(buf: &mut ParseBuffer<'_>) -> Result<Self> {
        let section = buf.parse_u16()?;
        let _padding = buf.parse_u16()?;
        let offset = buf.parse_u32()?;
        let size = buf.parse_u32()?;
        let characteristics = buf.parse()?;
        let module = buf.parse_u16()?.into();
        let _padding = buf.parse_u16()?;

        Ok(Self {
            offset: PdbInternalSectionOffset { offset, section },
            size,
            characteristics,
            module,
            data_crc: buf.parse_u32()?,
            reloc_crc: buf.parse_u32()?,
        })
    }
}

/// Information about a module parsed from the DBI stream.
///
/// Named `MODI` in the Microsoft PDB source:
/// <https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.h#L1197>
#[derive(Debug, Copy, Clone)]
#[allow(dead_code)] // reason = "unused fields added for completeness"
pub(crate) struct DBIModuleInfo {
    /// Currently open module.
    pub opened: u32,
    /// This module's first section contribution.
    pub section: DBISectionContribution,
    /// Flags, expressed as bitfields in the C struct:
    /// written, EC enabled, unused, tsm
    /// <https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.h#L1201-L1204>
    pub flags: u16,
    /// Stream number of module debug info (syms, lines, fpo).
    pub stream: StreamIndex,
    /// Size of local symbols debug info in `stream`.
    pub symbols_size: u32,
    /// Size of line number debug info in `stream`.
    pub lines_size: u32,
    /// Size of C13 style line number info in `stream`.
    pub c13_lines_size: u32,
    /// Number of files contributing to this module.
    pub files: u16,
    _padding: u16,
    /// Used as a pointer into an array of filename indicies in the Microsoft code.
    pub filename_offsets: u32,
    /// Source file name index.
    pub source: u32,
    /// Path to compiler PDB name index.
    pub compiler: u32,
}

impl DBIModuleInfo {
    fn parse(buf: &mut ParseBuffer<'_>) -> Result<Self> {
        Ok(Self {
            opened: buf.parse_u32()?,
            section: DBISectionContribution::parse(buf)?,
            flags: buf.parse_u16()?,
            stream: buf.parse()?,
            symbols_size: buf.parse_u32()?,
            lines_size: buf.parse_u32()?,
            c13_lines_size: buf.parse_u32()?,
            files: buf.parse_u16()?,
            _padding: buf.parse_u16()?,
            filename_offsets: buf.parse_u32()?,
            source: buf.parse_u32()?,
            compiler: buf.parse_u32()?,
        })
    }
}

/// Represents a module from the DBI stream.
///
/// A `Module` is a single item that contributes to the binary, such as an object file or import
/// library.
///
/// Much of the useful information for a `Module` is stored in a separate stream in the PDB. It can
/// be retrieved by calling [`PDB::module_info`](crate::PDB::module_info) with a specific module.
#[derive(Debug, Clone)]
pub struct Module<'m> {
    info: DBIModuleInfo,
    module_name: RawString<'m>,
    object_file_name: RawString<'m>,
}

impl<'m> Module<'m> {
    /// The `DBIModuleInfo` from the module info substream in the DBI stream.
    pub(crate) const fn info(&self) -> &DBIModuleInfo {
        &self.info
    }

    /// Returns the module name as a UTF-8 lossy string.
    ///
    /// Usually either a full path to an object file or a string of the form `Import:<dll name>`.
    #[cfg(feature = "alloc")]
    #[inline]
    pub fn module_name(&self) -> Cow<'m, str> {
        self.module_name.to_string()
    }

    /// Returns the raw bytes of the module name.
    #[inline]
    pub fn module_name_bytes(&self) -> &[u8] {
        self.module_name.as_bytes()
    }

    /// The object file name.
    ///
    /// May be the same as `module_name` for object files passed directly
    /// to the linker. For modules from static libraries, this is usually
    /// the full path to the archive.
    #[cfg(feature = "alloc")]
    #[inline]
    pub fn object_file_name(&self) -> Cow<'m, str> {
        self.object_file_name.to_string()
    }

    /// Returns the raw bytes of the object file name.
    #[inline]
    pub fn object_file_name_bytes(&self) -> &[u8] {
        self.object_file_name.as_bytes()
    }
}

/// A `ModuleIter` iterates over the modules in the DBI section, producing `Module`s.
#[derive(Debug)]
pub struct ModuleIter<'m>(ParseBuffer<'m>);

impl<'m> ModuleIter<'m> {

    #[inline]
    pub fn new(buffer: impl Into<ParseBuffer<'m>>) -> Self {
        Self(buffer.into())
    }
}

impl<'m> FallibleIterator for ModuleIter<'m> {
    type Item = Module<'m>;
    type Error = Error;

    fn next(&mut self) -> result::Result<Option<Self::Item>, Self::Error> {
        // see if we're at EOF
        if self.0.is_empty() {
            return Ok(None);
        }

        let info = DBIModuleInfo::parse(&mut self.0)?;
        let module_name = self.0.parse_cstring()?;
        let object_file_name = self.0.parse_cstring()?;
        self.0.align(4)?;

        Ok(Some(Module {
            info,
            module_name,
            object_file_name,
        }))
    }
}

/// The version of the section contribution stream.
#[derive(Debug, Copy, Clone, PartialEq)]
#[allow(missing_docs)]
enum DBISectionContributionStreamVersion {
    V60,
    V2,
    OtherValue(u32),
}

impl From<u32> for DBISectionContributionStreamVersion {
    fn from(v: u32) -> Self {
        const V60: u32 = 0xeffe_0000 + 19_970_605;
        const V2: u32 = 0xeffe_0000 + 20_140_516;
        match v {
            V60 => Self::V60,
            V2 => Self::V2,
            _ => Self::OtherValue(v),
        }
    }
}

/// A `DBISectionContributionIter` iterates over the section contributions in the DBI section, producing `DBISectionContribution`s.
#[derive(Debug)]
pub struct DBISectionContributionIter<'c> {
    buf: ParseBuffer<'c>,
    version: DBISectionContributionStreamVersion,
}

impl<'c> DBISectionContributionIter<'c> {
    fn parse(mut buf: ParseBuffer<'c>) -> Result<Self> {
        let version = buf.parse_u32()?.into();
        Ok(Self { buf, version })
    }
}

impl<'c> FallibleIterator for DBISectionContributionIter<'c> {
    type Item = DBISectionContribution;
    type Error = Error;

    fn next(&mut self) -> result::Result<Option<Self::Item>, Self::Error> {
        // see if we're at EOF
        if self.buf.is_empty() {
            return Ok(None);
        }

        let contribution = DBISectionContribution::parse(&mut self.buf)?;
        if self.version == DBISectionContributionStreamVersion::V2 {
            self.buf.parse_u32()?;
        }
        Ok(Some(contribution))
    }
}

/// Extra debug streams referenced by the DBI header.
///
/// This struct corresponds to the `DbgDataHdr` structure in the Microsoft PDB implementation.
/// It contains optional stream indices for various debug data subsections.
///
/// Each field is a `StreamIndex` where `0xffff` indicates the stream is not present.
///
/// # Reference
///
/// - Struct definition:
///   <https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.h#L250-L274>
/// - Array indices:
///   <https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/langapi/include/pdb.h#L439-L449>
///
/// # Notes
///
/// This struct may be truncated in the PDB file. Missing fields are treated as
/// `StreamIndex::none()` as long as the read stops on a `u16` boundary.
#[derive(Debug, Copy, Clone)]
#[allow(dead_code)]
pub(crate) struct DBIExtraStreams {
    /// FPO (Frame Pointer Omission) debug data stream
    pub fpo: StreamIndex,

    /// Exception handling data stream
    pub exception: StreamIndex,

    /// Fixup data stream
    pub fixup: StreamIndex,

    /// OMAP (Optimized Map) from source to destination addresses
    pub omap_to_src: StreamIndex,

    /// OMAP from destination to source addresses
    pub omap_from_src: StreamIndex,

    /// PE section headers stream
    pub section_headers: StreamIndex,

    /// Token to RID (Record ID) mapping stream
    pub token_rid_map: StreamIndex,

    /// XDATA (exception handling data) stream
    pub xdata: StreamIndex,

    /// PDATA (procedure data) stream
    pub pdata: StreamIndex,

    /// Frame data stream (stack frame information)
    pub framedata: StreamIndex,

    /// Original PE section headers (before optimization)
    pub original_section_headers: StreamIndex,
}

impl DBIExtraStreams {
    #[cfg(feature = "alloc")]
    pub(crate) fn new(debug_info: &DebugInformation<'_>) -> Result<Self> {
        // calculate the location of the extra stream information
        let header = debug_info.header;
        let offset = debug_info.header_len
            + (header.module_list_size
                + header.section_contribution_size
                + header.section_map_size
                + header.file_info_size
                + header.type_server_map_size
                + header.ec_substream_size) as usize;

        let mut buf = debug_info.stream.parse_buffer();
        buf.take(offset)?;

        let bytes = buf.take(header.debug_header_size as _)?;

        let mut extra_streams_buf = ParseBuffer::from(bytes);
        Self::parse(&mut extra_streams_buf)
    }

    pub(crate) fn parse(buf: &mut ParseBuffer<'_>) -> Result<Self> {
        // short reads are okay, as are long reads -- this struct is actually an array
        // what's _not_ okay are
        if buf.len() % 2 != 0 {
            return Err(Error::InvalidStreamLength("DbgDataHdr"));
        }

        fn next_index(buf: &mut ParseBuffer<'_>) -> Result<StreamIndex> {
            if buf.is_empty() {
                Ok(StreamIndex::none())
            } else {
                buf.parse()
            }
        }

        Ok(Self {
            fpo: next_index(buf)?,
            exception: next_index(buf)?,
            fixup: next_index(buf)?,
            omap_to_src: next_index(buf)?,
            omap_from_src: next_index(buf)?,
            section_headers: next_index(buf)?,
            token_rid_map: next_index(buf)?,
            xdata: next_index(buf)?,
            pdata: next_index(buf)?,
            framedata: next_index(buf)?,
            original_section_headers: next_index(buf)?,
        })
    }
}

#[cfg(all(test, feature = "alloc"))]
mod tests {
    use crate::dbi::*;
    use alloc::vec;

    #[test]
    fn test_dbi_extra_streams() {
        let bytes = vec![0xff, 0xff, 0x01, 0x02, 0x03, 0x04, 0xff, 0xff, 0x05, 0x06];

        let mut buf = ParseBuffer::from(bytes.as_slice());
        let extra_streams = DBIExtraStreams::parse(&mut buf).expect("parse");

        // check readback
        assert_eq!(extra_streams.fpo, StreamIndex::none());
        assert_eq!(extra_streams.exception, StreamIndex(0x0201));
        assert_eq!(extra_streams.fixup, StreamIndex(0x0403));
        assert_eq!(extra_streams.omap_to_src, StreamIndex::none());
        assert_eq!(extra_streams.omap_from_src, StreamIndex(0x0605));

        // check that short reads => StreamIndex::none()
        assert_eq!(extra_streams.section_headers, StreamIndex::none());
        assert_eq!(extra_streams.token_rid_map, StreamIndex::none());
        assert_eq!(extra_streams.original_section_headers, StreamIndex::none());
    }
}
