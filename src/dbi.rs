// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

// DBI = "Debug Information"

use common::*;
use msf::*;

/// Provides access to the "DBI" stream inside the PDB.
///
/// This is only minimally implemented; it's really just so `PDB` can find the global symbol table.
///
/// `DebugInformation` should be able to tell you about all the modules that are present in this
/// PDB.
#[derive(Debug)]
pub struct DebugInformation<'s> {
    stream: Stream<'s>,
    header: Header,
}

pub fn new_debug_information(stream: Stream) -> Result<DebugInformation> {
    let header;

    {
        let mut buf = stream.parse_buffer();
        header = parse_header(&mut buf)?;
    }

    Ok(DebugInformation{
        stream: stream,
        header: header,
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
