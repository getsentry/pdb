use std::io::Error;

use pdb::{FallibleIterator, Rva, PDB, Result};

#[test]
fn test_module_lines() -> Result<()> {
    let file = std::fs::File::open("fixtures/self/foo.pdb")?;
    let mut pdb = PDB::open(file)?;

    let address_map = pdb.address_map()?;
    let string_table = pdb.string_table()?;

    let dbi = pdb.debug_information()?;
    let mut modules = dbi.modules()?;
    let module = modules.next()?
        .ok_or(Error::new(std::io::ErrorKind::InvalidData, "invalid module"))?;

    let module_info = pdb
        .module_info(&module)
        .expect("parse module info")
        .expect("module info");

    let line_program = module_info.line_program()?;
    let mut lines = line_program.lines();
    let line_info = lines.next().expect("parse line info")
        .ok_or(Error::new(std::io::ErrorKind::InvalidData, "invalid line_info"))?;

    let rva = line_info.offset.to_rva(&address_map)
        .ok_or(Error::new(std::io::ErrorKind::InvalidData, "invalid rva"))?;

    let file_info = line_program
        .get_file_info(line_info.file_index)?;

    let file_name = file_info
        .name
        .to_string_lossy(&string_table)?;

    assert_eq!(line_info.line_start, 29);
    assert_eq!(line_info.column_start, None);
    assert_eq!(rva, Rva(0x64f0));
    assert_eq!(file_name, "c:\\users\\user\\desktop\\self\\foo.cpp");

    Ok(())
}
