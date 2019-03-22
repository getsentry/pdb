use std::ffi::OsStr;

fn dump_stream_names(filename: &OsStr) -> pdb::Result<()> {
    let file = std::fs::File::open(filename)?;
    let mut pdb = pdb::PDB::open(file)?;
    let info = pdb.pdb_information()?;
    let names = info.stream_names()?;
    println!("index, name");
    for name in &names {
        let stream = pdb.raw_stream(name.stream_id)?.expect("named stream");
        println!("{:5}, {} {} bytes", name.stream_id, name.name, stream.len());
    }
    Ok(())
}

fn main() {
    let filename = std::env::args_os().nth(1).expect("Missing PDB filename");

    match dump_stream_names(&filename) {
        Ok(_) => (),
        Err(e) => eprintln!("error dumping PDB: {}", e),
    }
}
