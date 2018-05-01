extern crate getopts;
extern crate pdb;

use getopts::Options;
use pdb::FallibleIterator;
use std::env;
use std::io::Write;

fn dump_pdb(filename: &str) -> pdb::Result<()> {
    let file = std::fs::File::open(filename)?;
    let mut pdb = pdb::PDB::open(file)?;



    println!("Module private symbols:");
    let s = pdb.string_table()?;
    let dbi = pdb.debug_information()?;
    let mut modules = dbi.modules()?;
    while let Some(module) = modules.next()? {
        println!("Module: {}", module.object_file_name());
        let info = pdb.module_info(&module)?;
        let mut lines = info.lines()?;
        while let Some(item) = lines.next()? {
            let file = s.get(item.file_name_index as usize)?;
            // work with item
            println!("File: {:?} {} {:?}", file, item.lines.len(), item.file_name_index);
            for l in item.lines {
                println!("{:?}", l);
            }
        }
    }

    s.dump();
    //println!("{}", s.dump());

    Ok(())
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };

    let filename = if matches.free.len() == 1 {
        &matches.free[0]
    } else {
        //print_usage(&program, opts);
        return;
    };

    match dump_pdb(&filename) {
        Ok(_) => {}
        Err(e) => {
            writeln!(&mut std::io::stderr(), "error dumping PDB: {}", e)
                .expect("stderr write");
        }
    }
}
