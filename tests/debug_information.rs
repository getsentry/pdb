extern crate pdb;

use std::str::FromStr;

#[test]
fn pdb_info() {
    let file = std::fs::File::open("fixtures/self/foo.pdb").expect("opening file");

    let mut pdb = pdb::PDB::open(file).expect("opening pdb");
    let pdb_info = pdb.debug_information().expect("pdb information");

    assert_eq!(
        pdb_info.machine_type().expect("machien type"),
        pdb::MachineType::Amd64
    );
}
