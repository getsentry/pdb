use std::env;
use std::io::Write;

use getopts::Options;

use pdb::{FallibleIterator, SymbolData, PDB};

fn dump_pdb(filename: &str) -> pdb::Result<()> {
    let file = std::fs::File::open(filename)?;
    let mut pdb = PDB::open(file)?;

    let address_map = pdb.address_map()?;
    let string_table = pdb.string_table()?;

    println!("Module private symbols:");
    let dbi = pdb.debug_information()?;
    let mut modules = dbi.modules()?;
    while let Some(module) = modules.next()? {
        println!();
        println!("Module: {}", module.module_name());

        let info = match pdb.module_info(&module)? {
            Some(info) => info,
            None => {
                println!("  no module info");
                continue;
            }
        };

        let program = info.line_program()?;
        let mut symbols = info.symbols()?;

        while let Some(symbol) = symbols.next()? {
            if let Ok(SymbolData::Procedure(proc)) = symbol.parse() {
                let sign = if proc.global { "+" } else { "-" };
                println!("{} {}", sign, symbol.name()?.to_string());

                let mut lines = program.lines_at_offset(proc.offset);
                while let Some(line_info) = lines.next()? {
                    let rva = line_info.offset.to_rva(&address_map).expect("invalid rva");
                    let file_info = program.get_file_info(line_info.file_index)?;
                    let file_name = file_info.name.to_string_lossy(&string_table)?;
                    println!("  {} {}:{}", rva, file_name, line_info.line_start);
                }
            }
        }
    }

    Ok(())
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string()),
    };

    let filename = if matches.free.len() == 1 {
        &matches.free[0]
    } else {
        //print_usage(&program, opts);
        println!("specify path to a PDB");
        return;
    };

    match dump_pdb(&filename) {
        Ok(_) => {}
        Err(e) => {
            writeln!(&mut std::io::stderr(), "error dumping PDB: {}", e).expect("stderr write");
        }
    }
}
