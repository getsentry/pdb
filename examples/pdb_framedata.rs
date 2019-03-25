use std::env;

use getopts::Options;
use pdb::FallibleIterator;

fn print_usage(program: &str, opts: Options) {
    let brief = format!("Usage: {} input.pdb", program);
    print!("{}", opts.usage(&brief));
}

fn dump_framedata(filename: &str) -> pdb::Result<()> {
    let file = std::fs::File::open(filename)?;
    let mut pdb = pdb::PDB::open(file)?;

    let string_table = pdb.string_table()?;
    println!("Frame data:");

    println!(
        "Address    Blk Size   Locals   Params   StackMax Prolog    SavedRegs   SEH C++EH Start BP"
    );
    println!("           Program/Type");
    println!();

    let frame_table = pdb.frame_table()?;
    let mut frames = frame_table.iter();
    while let Some(data) = frames.next()? {
        println!(
            "{} {:8x} {:8x} {:8x} {:8x} {:8x} {:8x}        {}   {}      {}     {}",
            data.code_rva,
            data.code_size,
            data.locals_size,
            data.params_size,
            data.max_stack_size.unwrap_or(0),
            data.prolog_size,
            data.saved_regs_size,
            if data.has_structured_eh { 'Y' } else { 'N' },
            if data.has_cpp_eh { 'Y' } else { 'N' },
            if data.is_function_start { 'Y' } else { 'N' },
            if data.uses_base_pointer { 'Y' } else { 'N' },
        );

        match data.info {
            pdb::FrameInfo::Program(prog_ref) => {
                let program_string = prog_ref.to_string_lossy(&string_table)?;
                println!("           {}", program_string);
            }
            pdb::FrameInfo::Type(ty) => println!("          {}", ty),
        }
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

    match dump_framedata(&filename) {
        Ok(_) => (),
        Err(e) => eprintln!("error dumping PDB: {}", e),
    }
}
