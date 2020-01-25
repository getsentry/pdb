// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

// DBI = "Debug Information"

use std::borrow::Cow;
use std::fmt;
use std::result;

use crate::common::*;
use crate::msf::*;
use crate::FallibleIterator;

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
#[derive(Debug)]
pub struct DebugInformation<'s> {
    stream: Stream<'s>,
    header: DBIHeader,
    header_len: usize,
}

impl<'s> DebugInformation<'s> {
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

    pub(crate) fn header(&self) -> DBIHeader {
        self.header
    }

    /// Returns the target's machine type (architecture).
    pub fn machine_type(&self) -> Result<MachineType> {
        Ok(self.header.machine_type.into())
    }

    /// Returns this PDB's original `age`.
    ///
    /// This number is written by the linker and should be equal to the image's `age` value.
    /// In contrast, [`PDBInformation::age`] may be bumped by other tools and should be greater or
    /// equal to the image's `age` value.
    ///
    /// Old PDB files may not specify an age, in which case only [`PDBInformation::age`] should be
    /// checked for matching the image.
    ///
    /// [`PDBInformation::age`]: struct.PDBInformation.html#structfield.age
    pub fn age(&self) -> Option<u32> {
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
        Ok(ModuleIter {
            buf: modules_buf.into(),
        })
    }
}

/// The version of the PDB format.
///
/// This version type is used in multiple locations: the DBI header, and the PDBI header.
#[derive(Debug, Copy, Clone)]
#[allow(missing_docs)]
pub enum HeaderVersion {
    V41,
    V50,
    V60,
    V70,
    V110,
    OtherValue(u32),
}

impl From<u32> for HeaderVersion {
    #[allow(clippy::inconsistent_digit_grouping)]
    fn from(v: u32) -> Self {
        match v {
            93_08_03 => HeaderVersion::V41,
            1996_03_07 => HeaderVersion::V50,
            1997_06_06 => HeaderVersion::V60,
            1999_09_03 => HeaderVersion::V70,
            2009_12_01 => HeaderVersion::V110,
            _ => HeaderVersion::OtherValue(v),
        }
    }
}

/// A DBI header -- `NewDBIHdr`, really -- parsed from a stream.
/// Reference:
/// https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.h#L124
#[derive(Debug, Copy, Clone)]
pub(crate) struct DBIHeader {
    pub signature: u32,
    pub version: HeaderVersion,
    pub age: u32,
    pub gs_symbols_stream: StreamIndex,

    /*
    https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.h#L143-L155:
        union {
        struct {
            USHORT      usVerPdbDllMin : 8; // minor version and
            USHORT      usVerPdbDllMaj : 7; // major version and
            USHORT      fNewVerFmt     : 1; // flag telling us we have rbld stored elsewhere (high bit of original major version)
        } vernew;                           // that built this pdb last.
        struct {
            USHORT      usVerPdbDllRbld: 4;
            USHORT      usVerPdbDllMin : 7;
            USHORT      usVerPdbDllMaj : 5;
        } verold;
        USHORT          usVerAll;
    };
    */
    pub internal_version: u16,
    pub ps_symbols_stream: StreamIndex,
    // "build version of the pdb dll that built this pdb last."
    pub pdb_dll_build_version: u16,

    pub symbol_records_stream: StreamIndex,

    // "rbld version of the pdb dll that built this pdb last."
    pub pdb_dll_rbld_version: u16,
    pub module_list_size: u32,
    pub section_contribution_size: u32,
    pub section_map_size: u32,
    pub file_info_size: u32,

    // "size of the Type Server Map substream"
    pub type_server_map_size: u32,

    // "index of MFC type server"
    pub mfc_type_server_index: u32,

    // "size of optional DbgHdr info appended to the end of the stream"
    pub debug_header_size: u32,

    // "number of bytes in EC substream, or 0 if EC no EC enabled Mods"
    pub ec_substream_size: u32,

    /*
    https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.h#L187-L192:
        USHORT  fIncLink:1;     // true if linked incrmentally (really just if ilink thunks are present)
        USHORT  fStripped:1;    // true if PDB::CopyTo stripped the private data out
        USHORT  fCTypes:1;      // true if this PDB is using CTypes.
        USHORT  unused:13;      // reserved, must be 0.
    */
    pub flags: u16,

    pub machine_type: u16,
    pub reserved: u32,
}

impl DBIHeader {
    pub fn parse(stream: Stream<'_>) -> Result<Self> {
        Self::parse_buf(&mut stream.parse_buffer())
    }

    fn parse_buf(buf: &mut ParseBuffer<'_>) -> Result<Self> {
        let header = DBIHeader {
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

        if header.signature != u32::max_value() {
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
/// Reference: https://docs.microsoft.com/en-us/windows/desktop/debug/pe-format#machine-types
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
        match *self {
            MachineType::Invalid => write!(f, "Invalid"),
            MachineType::Unknown => write!(f, "Unknown"),
            MachineType::Am33 => write!(f, "Am33"),
            MachineType::Amd64 => write!(f, "Amd64"),
            MachineType::Arm => write!(f, "Arm"),
            MachineType::Arm64 => write!(f, "Arm64"),
            MachineType::ArmNT => write!(f, "ArmNT"),
            MachineType::Ebc => write!(f, "Ebc"),
            MachineType::X86 => write!(f, "X86"),
            MachineType::Ia64 => write!(f, "Ia64"),
            MachineType::M32R => write!(f, "M32R"),
            MachineType::Mips16 => write!(f, "Mips16"),
            MachineType::MipsFpu => write!(f, "MipsFpu"),
            MachineType::MipsFpu16 => write!(f, "MipsFpu16"),
            MachineType::PowerPC => write!(f, "PowerPC"),
            MachineType::PowerPCFP => write!(f, "PowerPCFP"),
            MachineType::R4000 => write!(f, "R4000"),
            MachineType::RiscV32 => write!(f, "RiscV32"),
            MachineType::RiscV64 => write!(f, "RiscV64"),
            MachineType::RiscV128 => write!(f, "RiscV128"),
            MachineType::SH3 => write!(f, "SH3"),
            MachineType::SH3DSP => write!(f, "SH3DSP"),
            MachineType::SH4 => write!(f, "SH4"),
            MachineType::SH5 => write!(f, "SH5"),
            MachineType::Thumb => write!(f, "Thumb"),
            MachineType::WceMipsV2 => write!(f, "WceMipsV2"),
        }
    }
}

impl From<u16> for MachineType {
    fn from(value: u16) -> Self {
        match value {
            0xffff => MachineType::Invalid,
            0x0 => MachineType::Unknown,
            0x13 => MachineType::Am33,
            0x8664 => MachineType::Amd64,
            0x1C0 => MachineType::Arm,
            0xAA64 => MachineType::Arm64,
            0x1C4 => MachineType::ArmNT,
            0xEBC => MachineType::Ebc,
            0x14C => MachineType::X86,
            0x200 => MachineType::Ia64,
            0x9041 => MachineType::M32R,
            0x266 => MachineType::Mips16,
            0x366 => MachineType::MipsFpu,
            0x466 => MachineType::MipsFpu16,
            0x1F0 => MachineType::PowerPC,
            0x1F1 => MachineType::PowerPCFP,
            0x166 => MachineType::R4000,
            0x5032 => MachineType::RiscV32,
            0x5064 => MachineType::RiscV64,
            0x5128 => MachineType::RiscV128,
            0x1A2 => MachineType::SH3,
            0x1A3 => MachineType::SH3DSP,
            0x1A6 => MachineType::SH4,
            0x1A8 => MachineType::SH5,
            0x1C2 => MachineType::Thumb,
            0x169 => MachineType::WceMipsV2,
            _ => MachineType::Unknown,
        }
    }
}

/// Information about a module's contribution to a section.
/// `struct SC` in Microsoft's code:
/// https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/include/dbicommon.h#L42
#[derive(Debug, Copy, Clone)]
pub(crate) struct DBISectionContribution {
    /// The index of the section.
    section: u16,
    _padding1: u16,
    /// The offset within the section.
    offset: u32,
    /// The size of the contribution, in bytes.
    size: u32,
    /// The characteristics, which map to the `Characteristics` field of
    /// the [`IMAGE_SECTION_HEADER`][1] field in binaries.
    /// [1]: https://msdn.microsoft.com/en-us/library/windows/desktop/ms680341(v=vs.85).aspx
    characteristics: u32,
    /// The index of the module.
    module: u16,
    _padding2: u16,
    /// CRC of the contribution(?)
    data_crc: u32,
    /// CRC of relocations(?)
    reloc_crc: u32,
}

impl DBISectionContribution {
    fn parse(buf: &mut ParseBuffer<'_>) -> Result<Self> {
        Ok(DBISectionContribution {
            section: buf.parse_u16()?,
            _padding1: buf.parse_u16()?,
            offset: buf.parse_u32()?,
            size: buf.parse_u32()?,
            characteristics: buf.parse_u32()?,
            module: buf.parse_u16()?,
            _padding2: buf.parse_u16()?,
            data_crc: buf.parse_u32()?,
            reloc_crc: buf.parse_u32()?,
        })
    }
}

/// Information about a module parsed from the DBI stream. Named `MODI` in
/// the Microsoft PDB source:
/// https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.h#L1197
#[derive(Debug, Copy, Clone)]
pub(crate) struct DBIModuleInfo {
    /// Currently open module.
    pub opened: u32,
    /// This module's first section contribution.
    pub section: DBISectionContribution,
    /// Flags, expressed as bitfields in the C struct:
    /// written, EC enabled, unused, tsm
    /// https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.h#L1201-L1204
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
        Ok(DBIModuleInfo {
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
/// A `Module` is a single item that contributes to the binary, such as an
/// object file or import library.
///
/// Much of the useful information for a `Module` is stored in a separate stream in the PDB.
/// It can be retrieved by calling [`PDB::module_info`] with a specific module.
///
/// [`PDB::module_info`]: struct.PDB.html#method.module_info
#[derive(Debug, Clone)]
pub struct Module<'m> {
    info: DBIModuleInfo,
    module_name: RawString<'m>,
    object_file_name: RawString<'m>,
}

impl<'m> Module<'m> {
    /// The `DBIModuleInfo` from the module info substream in the DBI stream.
    pub(crate) fn info(&self) -> &DBIModuleInfo {
        &self.info
    }
    /// The module name.
    ///
    /// Usually either a full path to an object file or a string of the form `Import:<dll name>`.
    pub fn module_name(&self) -> Cow<'m, str> {
        self.module_name.to_string()
    }
    /// The object file name.
    ///
    /// May be the same as `module_name` for object files passed directly
    /// to the linker. For modules from static libraries, this is usually
    /// the full path to the archive.
    pub fn object_file_name(&self) -> Cow<'m, str> {
        self.object_file_name.to_string()
    }
}

/// A `ModuleIter` iterates over the modules in the DBI section, producing `Module`s.
#[derive(Debug)]
pub struct ModuleIter<'m> {
    buf: ParseBuffer<'m>,
}

impl<'m> FallibleIterator for ModuleIter<'m> {
    type Item = Module<'m>;
    type Error = Error;

    fn next(&mut self) -> result::Result<Option<Self::Item>, Self::Error> {
        // see if we're at EOF
        if self.buf.is_empty() {
            return Ok(None);
        }

        let info = DBIModuleInfo::parse(&mut self.buf)?;
        let module_name = self.buf.parse_cstring()?;
        let object_file_name = self.buf.parse_cstring()?;
        self.buf.align(4)?;
        Ok(Some(Module {
            info,
            module_name,
            object_file_name,
        }))
    }
}

/// A `DbgDataHdr`, which contains a series of (optional) MSF stream numbers.
#[derive(Debug, Copy, Clone)]
pub(crate) struct DBIExtraStreams {
    // The struct itself is defined at:
    //    https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.h#L250-L274
    // It's just an array of stream numbers; `u16`s where 0xffff means "no stream".
    //
    // The array indices are:
    //    https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/langapi/include/pdb.h#L439-L449
    // We'll map those to fields.
    //
    // The struct itself can be truncated. This is an internal struct; we'll treat missing fields as
    // StreamIndex::none() even if it's a short read, so long as the short read stops on a u16 boundary.
    pub fpo: StreamIndex,
    pub exception: StreamIndex,
    pub fixup: StreamIndex,
    pub omap_to_src: StreamIndex,
    pub omap_from_src: StreamIndex,
    pub section_headers: StreamIndex,
    pub token_rid_map: StreamIndex,
    pub xdata: StreamIndex,
    pub pdata: StreamIndex,
    pub framedata: StreamIndex,
    pub original_section_headers: StreamIndex,
}

impl DBIExtraStreams {
    pub(crate) fn new(debug_info: &DebugInformation<'_>) -> Result<Self> {
        // calculate the location of the extra stream information
        let header = debug_info.header;
        let offset = debug_info.header_len
            + (header.module_list_size
                + header.section_contribution_size
                + header.section_map_size
                + header.file_info_size
                + header.type_server_map_size
                + header.mfc_type_server_index
                + header.ec_substream_size) as usize;

        // seek
        let mut buf = debug_info.stream.parse_buffer();
        buf.take(offset)?;

        // grab that section as bytes
        let bytes = buf.take(header.debug_header_size as _)?;

        // parse those bytes
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

        Ok(DBIExtraStreams {
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

#[cfg(test)]
mod tests {
    use crate::dbi::*;

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
