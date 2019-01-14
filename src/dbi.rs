// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

// DBI = "Debug Information"

use std::borrow::Cow;
use std::result;

use common::*;
use msf::*;
use FallibleIterator;

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
    header: Header,
    header_len: usize,
}

impl<'s> DebugInformation<'s> {
    /// Returns the target's machine type (architecture).
    pub fn machine_type(&self) -> Result<MachineType> {
        Ok(self.header.machine_type.into())
    }
    /// Returns an iterator that can traverse the modules list in sequential order.
    pub fn modules(&self) -> Result<ModuleIter> {
        let mut buf = self.stream.parse_buffer();
        // drop the header
        buf.take(self.header_len)?;
        let modules_buf = buf.take(self.header.module_list_size as usize)?;
        Ok(ModuleIter { buf: modules_buf.into() })
    }
}

pub fn new_debug_information(stream: Stream) -> Result<DebugInformation> {
    let (header, len) = {
        let mut buf = stream.parse_buffer();
        let header = parse_header(&mut buf)?;
        (header, buf.pos())
    };

    Ok(DebugInformation{
        stream: stream,
        header: header,
        header_len: len,
    })
}

pub fn get_header(dbi: &DebugInformation) -> Header {
    dbi.header
}

#[derive(Debug,Copy,Clone)]
pub enum HeaderVersion {
    V41,
    V50,
    V60,
    V70,
    V110,
    OtherValue(u32)
}

impl From<u32> for HeaderVersion {
    fn from(v: u32) -> Self {
        match v {
            930803 => HeaderVersion::V41,
            19960307 => HeaderVersion::V50,
            19970606 => HeaderVersion::V60,
            19990903 => HeaderVersion::V70,
            20091201 => HeaderVersion::V110,
            _ => HeaderVersion::OtherValue(v),
        }
    }
}

/// A DBI header -- `NewDBIHdr`, really -- parsed from a stream.
/// Reference:
/// https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.h#L124
#[derive(Debug,Copy,Clone)]
pub struct Header {
    pub signature: u32,
    pub version: HeaderVersion,
    pub age: u32,
    pub gs_symbols_stream: u16,

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
    pub ps_symbols_stream: u16,
    // "build version of the pdb dll that built this pdb last."
    pub pdb_dll_build_version: u16,

    pub symbol_records_stream: u16,

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

pub fn parse_header(buf: &mut ParseBuffer) -> Result<Header> {
    let header = Header {
        signature: buf.parse_u32()?,
        version: From::from(buf.parse_u32()?),
        age: buf.parse_u32()?,
        gs_symbols_stream: buf.parse_u16()?,
        internal_version: buf.parse_u16()?,
        ps_symbols_stream: buf.parse_u16()?,
        pdb_dll_build_version: buf.parse_u16()?,
        symbol_records_stream: buf.parse_u16()?,
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

    if header.signature != 0xffffffff {
        // this is likely a DBIHdr, not a NewDBIHdr
        // it could be promoted:
        //   https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.cpp#L291-L313
        //   https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/langapi/include/pdb.h#L1180-L1184
        // but that seems like a lot of work
        return Err(Error::UnimplementedFeature("ancient DBI header"));
    }

    Ok(header)
}

/// The target machine's architecture.
// Reference: https://github.com/llvm-mirror/llvm/blob/8e47a8d1a66b89cd59fbc2fdc7e19dbe7a15c6f8/include/llvm/DebugInfo/PDB/PDBTypes.h#L124
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum MachineType {
    Invalid = 0xffff,
    Unknown = 0x0,
    Am33 = 0x13,
    Amd64 = 0x8664,
    Arm = 0x1C0,
    ArmNT = 0x1C4,
    Ebc = 0xEBC,
    X86 = 0x14C,
    Ia64 = 0x200,
    M32R = 0x9041,
    Mips16 = 0x266,
    MipsFpu = 0x366,
    MipsFpu16 = 0x466,
    PowerPC = 0x1F0,
    PowerPCFP = 0x1F1,
    R4000 = 0x166,
    SH3 = 0x1A2,
    SH3DSP = 0x1A3,
    SH4 = 0x1A6,
    SH5 = 0x1A8,
    Thumb = 0x1C2,
    WceMipsV2 = 0x169,
}

impl ::std::fmt::Display for MachineType {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            MachineType::Invalid => write!(f, "Invalid"),
            MachineType::Unknown => write!(f, "Unknown"),
            MachineType::Am33 => write!(f, "Am33"),
            MachineType::Amd64 => write!(f, "Amd64"),
            MachineType::Arm => write!(f, "Arm"),
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
pub struct DBISectionContribution {
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

/// Information about a module parsed from the DBI stream. Named `MODI` in
/// the Microsoft PDB source:
/// https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.h#L1197
#[derive(Debug, Copy, Clone)]
pub struct DBIModuleInfo {
    /// Currently open module.
    pub opened: u32,
    /// This module's first section contribution.
    pub section: DBISectionContribution,
    /// Flags, expressed as bitfields in the C struct:
    /// written, EC enabled, unused, tsm
    /// https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/dbi/dbi.h#L1201-L1204
    pub flags: u16,
    /// Stream number of module debug info (syms, lines, fpo).
    pub stream: u16,
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

fn parse_module_info(buf: &mut ParseBuffer) -> Result<DBIModuleInfo> {
    Ok(DBIModuleInfo {
        opened: buf.parse_u32()?,
        section: parse_section_contribution(buf)?,
        flags: buf.parse_u16()?,
        stream: buf.parse_u16()?,
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

fn parse_section_contribution(buf: &mut ParseBuffer) -> Result<DBISectionContribution> {
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
    pub fn info(&self) -> &DBIModuleInfo { &self.info }
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
        if self.buf.len() == 0 {
            return Ok(None);
        }

        let info = parse_module_info(&mut self.buf)?;
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
