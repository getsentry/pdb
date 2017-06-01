// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use common::*;
use msf::*;
use uuid::Uuid;
use dbi::HeaderVersion;

/// A PDB info stream header parsed from a stream.
/// Reference:
/// http://llvm.org/docs/PDB/PdbStream.html
#[derive(Debug,Copy,Clone)]
pub struct PDBInformation {
    pub version: HeaderVersion,
    pub signature: u32,
    pub age: u32,
    pub guid: Uuid
}

pub fn new_pdb_information(stream: Stream) -> Result<PDBInformation> {
    let mut buf = stream.parse_buffer();

    let header = PDBInformation {
        version: From::from(buf.parse_u32()?),
        signature: buf.parse_u32()?,
        age: buf.parse_u32()?,
        guid: Uuid::from_fields(
            buf.parse_u32()?,
            buf.parse_u16()?,
            buf.parse_u16()?,
            buf.take(8)?
        ).unwrap()
    };

    Ok(header)
}
