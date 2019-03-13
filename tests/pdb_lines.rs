use pdb::{FallibleIterator, Rva, PDB};

#[test]
fn test_module_lines() {
    let file = std::fs::File::open("fixtures/self/foo.pdb").expect("opening file");
    let mut pdb = PDB::open(file).expect("parse pdb");

    let address_map = pdb.address_map().expect("address map");
    let string_table = pdb.string_table().expect("string table");

    let dbi = pdb.debug_information().expect("dbi");
    let mut modules = dbi.modules().expect("modules");
    let module = modules.next().expect("parse module").expect("no module");
    let module_info = pdb.module_info(&module).expect("module info");

    let line_program = module_info.line_program().expect("line program");
    let mut lines = line_program.lines();
    let line_info = lines.next().expect("parse line info").expect("no lines");

    let rva = line_info.offset.to_rva(&address_map).expect("line rva");
    let file_info = line_program
        .get_file_info(line_info.file_index)
        .expect("file info");
    let file_name = file_info
        .name
        .to_string_lossy(&string_table)
        .expect("file name");

    assert_eq!(line_info.line_start, 29);
    assert_eq!(line_info.column_start, Some(0)); // looks like useless column info
    assert_eq!(rva, Rva(0x64f0));
    assert_eq!(file_name, "c:\\users\\user\\desktop\\self\\foo.cpp");
}
