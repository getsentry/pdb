extern crate ms_pdb_msf;

use ms_pdb_msf::*;
use std::{io::Read, path::Path};

fn main() -> anyhow::Result<()> {

    let options = CreateOptions::default();
    let path: &Path = "test.pdb".as_ref();
    let mut msf = Msf::create(path, options.clone())?;

    let stream1_data = b"Hello, PDB Stream 1!";
    let stream1 = msf.new_stream_data(stream1_data)?;
    println!("Created stream 1 with index: {}", stream1);

    let stream2_data = b"Type information data";
    let stream2 = msf.new_stream_data(stream2_data)?;
    println!("Created stream 2 with index: {}", stream2);

    let stream3_data = b"Debug information data";
    let stream3 = msf.new_stream_data(stream3_data)?;
    println!("Created stream 3 with index: {}", stream3);

    msf.commit()?;
    println!("MSF file written to: {:?}", path);
    
    Ok(())
}