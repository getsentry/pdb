use std::env;

use getopts::Options;
use pdb::{FallibleIterator, PdbInternalSectionOffset};

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} input.pdb", program);
    print!("{}", opts.usage(&brief));
}

fn print_row(offset: PdbInternalSectionOffset, kind: &str, name: pdb::RawString<'_>) {
    println!(
        "{:x}\t{:x}\t{}\t{}",
        offset.section,
        offset.offset,
        kind,
        name.to_string()
    );
}

fn print_symbol(symbol: &pdb::Symbol<'_>) -> pdb::Result<()> {
    match symbol.parse()? {
        pdb::SymbolData::PublicSymbol(data) => {
            print_row(data.offset, "function", symbol.name()?);
        }
        pdb::SymbolData::DataSymbol(data) => {
            print_row(data.offset, "data", symbol.name()?);
        }
        pdb::SymbolData::Procedure(data) => {
            print_row(data.offset, "function", symbol.name()?);
        }
        _ => {
            // ignore everything else
        }
    }

    Ok(())
}

fn walk_symbols(mut symbols: pdb::SymbolIter<'_>) -> pdb::Result<()> {
    println!("segment\toffset\tkind\tname");

    while let Some(symbol) = symbols.next()? {
        match print_symbol(&symbol) {
            Ok(_) => (),
            Err(e) => eprintln!("error printing symbol {:?}: {}", symbol, e),
        }
    }

    Ok(())
}

fn dump_pdb(filename: &str) -> pdb::Result<()> {
    let file = std::fs::File::open(filename)?;
    let mut pdb = pdb::PDB::open(file)?;
    let symbol_table = pdb.global_symbols()?;
    println!("Global symbols:");
    walk_symbols(symbol_table.iter())?;

    println!("Module private symbols:");
    let dbi = pdb.debug_information()?;
    let mut modules = dbi.modules()?;
    while let Some(module) = modules.next()? {
        println!("Module: {}", module.object_file_name());
        let info = pdb.module_info(&module)?;
        walk_symbols(info.symbols()?)?;
    }
    Ok(())
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let program = args[0].clone();

    let mut opts = Options::new();
    opts.optflag("h", "help", "print this help menu");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => m,
        Err(f) => panic!(f.to_string()),
    };

    let filename = if matches.free.len() == 1 {
        &matches.free[0]
    } else {
        print_usage(&program, opts);
        return;
    };

    match dump_pdb(&filename) {
        Ok(_) => (),
        Err(e) => eprintln!("error dumping PDB: {}", e),
    }
}
