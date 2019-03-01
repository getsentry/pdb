extern crate pdb;

use std::env;
use std::ffi::OsStr;
use std::io::Write;

fn dump_stream_names(filename: &OsStr) -> pdb::Result<()> {
    let file = std::fs::File::open(filename)?;
    let mut pdb = pdb::PDB::open(file)?;
    let info = pdb.pdb_information()?;
    let names = info.stream_names()?;
    println!("index, name");
    for name in names.iter() {
        let stream = pdb.raw_stream(name.stream_id)?;
        println!(
            "{:5}, {} {} bytes",
            name.stream_id,
            name.name,
            stream.parse_buffer().len()
        );
    }
    Ok(())
}

fn main() {
    let filename = env::args_os().nth(1).expect("Missing PDB filename");

    match dump_stream_names(&filename) {
        Ok(_) => {}
        Err(e) => {
            writeln!(&mut std::io::stderr(), "error dumping PDB: {}", e).expect("stderr write");
        }
    }
}
