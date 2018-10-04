extern crate pdb;
extern crate uuid;

use std::str::FromStr;

#[test]
fn pdb_info() {
    let file = std::fs::File::open("fixtures/self/foo.pdb").expect("opening file");

    let mut pdb = pdb::PDB::open(file).expect("opening pdb");
    let pdb_info = pdb.pdb_information().expect("pdb information");

    assert_eq!(pdb_info.age, 2);
    assert_eq!(pdb_info.guid, uuid::Uuid::from_str("A53F3C2B-2E5A-B844-8BBA-C3300FF69F62").unwrap());
    assert_eq!(pdb_info.signature, 1484498465);
}
