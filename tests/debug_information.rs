use pdb::Result;

#[test]
fn should_parse_big_msf_pdb_debug_info() -> Result<()> {
    let file = std::fs::File::open("fixtures/cpp/foo.pdb")?;

    let mut pdb = pdb::PDB::open(file)?;
    let pdb_info = pdb.debug_information()?;

    assert_eq!(
        pdb_info.machine_type()?,
        pdb::MachineType::Amd64
    );

    Ok(())
}
