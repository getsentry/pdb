use pdb::{PDBSignature, Result};
use uuid::Uuid;

#[test]
fn pdb_info() -> Result<()> {
    let file = std::fs::File::open("fixtures/self/foo.pdb")?;
    let mut pdb = pdb::PDB::open(file)?;
    let pdb_info = pdb.pdb_information()?;

    assert_eq!(pdb_info.age, 2);
    assert_eq!(
        pdb_info.guid.unwrap(),
        "2B3C3FA5-5A2E-44B8-8BBA-C3300FF69F62".parse::<Uuid>().unwrap(),
    );

    assert_eq!(pdb_info.signature.to_rfc3339().unwrap(), "2017-01-15T16:41:05+00:00");

    Ok(())
}
