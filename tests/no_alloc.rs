use pdb::{DbiVersion, FallibleIterator, MachineType, MsfKind, Result, StreamIndex, noalloc};

/// Parses `big_msf_rust_fixture.pdb`, a Big MSF file, through the
/// no-alloc reader.
///
/// Exercises the Big MSF container path (three-level directory indirection,
/// 32-bit page numbers, per-page chunk iteration), the DBI header parse,
/// and the module list. The module list is read into a caller-supplied
/// buffer, since the noalloc reader cannot allocate one itself.
#[test]
fn should_parse_pdb_without_alloc() -> Result<()> {

    let path = r#"C:\repos\pdb\fixtures\rust\basic\target\debug\big_msf_rust_fixture.pdb"#;
    let data = std::fs::read(path)?;
    let pdb = noalloc::PDB::open(&data)?;

    assert_eq!(pdb.msf_kind(), MsfKind::Big);
    assert_eq!(pdb.page_size(), 4096);
    assert_eq!(pdb.stream_count(), 160);

    // Stream 0 should always be readable without error.
    assert_eq!(pdb.stream_size(0)?.unwrap(), 48);
    let mut buf = vec![0u8; 48 as usize];
    assert_eq!(pdb.read_stream(0, &mut buf)?, 48 as usize);
    let debug = pdb.debug_information()?;

    assert_eq!(debug.machine_type()?, MachineType::Amd64);
    assert_eq!(debug.header().version, DbiVersion::V70);
    assert_eq!(debug.header().symbol_records_stream, StreamIndex(151));
    assert_eq!(debug.header().gs_symbols_stream, StreamIndex(149));
    assert_eq!(debug.header().ps_symbols_stream, StreamIndex(150));
    assert_eq!(debug.header().section_map_size, 124);
    assert_eq!(debug.header().file_info_size, 66056);
    assert_eq!(debug.header().flags, 0);
    assert_eq!(debug.header().module_list_size, 44320);
    assert!(debug.header().module_list_size > 100);

    let mut buf = [0u8; 44320];
    let modules = debug.modules(&pdb, &mut buf)?;
    assert!(modules.count()? > 100);
    
    let mut modules = debug.modules(&pdb, &mut buf)?;
    let module = modules.next().unwrap().unwrap();
    
    assert!(module.module_name_bytes().len() > 0);

    Ok(())
}