extern crate pdb;
extern crate uuid;

use std::str::FromStr;
use std::sync::{Once, ONCE_INIT};
use pdb::FallibleIterator;

// This test is intended to cover OMAP address translation:
//   https://github.com/willglynn/pdb/issues/17

static DOWNLOADED: Once = ONCE_INIT;
fn open_file() -> std::fs::File {
    DOWNLOADED.call_once(|| {
        // TODO: download
        //   https://msdl.microsoft.com/download/symbols/ntkrnlmp.pdb/3844dbb920174967be7aa4a2c20430fa2/ntkrnlmp.pdb
        // to the path we try to open
    });

    std::fs::File::open("fixtures/symbol_server/3844dbb920174967be7aa4a2c20430fa2-ntkrnlmp.pdb")
        .expect("opening file")
}

#[test]
fn verify_pdb_identity() {
    // make sure this is the right PDB
    let mut pdb = pdb::PDB::open(open_file()).expect("opening pdb");

    let pdb_info = pdb.pdb_information().expect("pdb information");
    assert_eq!(pdb_info.guid, uuid::Uuid::from_str("3844DBB9-2017-4967-BE7A-A4A2C20430FA").unwrap());
    assert_eq!(pdb_info.age, 5);
    assert_eq!(pdb_info.signature, 1290245416);
}

#[test]
fn test_omap() {
    let mut pdb = pdb::PDB::open(open_file()).expect("opening pdb");

    let global_symbols = pdb.global_symbols().expect("global_symbols");

    // find the target symbol
    let target_symbol = {
        let target_name = pdb::RawString::from("NtWaitForSingleObject");
        let mut iter = global_symbols.iter();
        iter.filter(|sym| sym.name().expect("symbol name") == target_name).next()
            .expect("iterate symbols")
            .expect("find target symbol")
    };

    // extract the PublicSymbol data
    let pubsym = match target_symbol.parse().expect("parse symbol") {
        pdb::SymbolData::PublicSymbol(pubsym) => pubsym,
        _ => panic!("expected public symbol")
    };

    // ensure the symbol has the correct location
    assert_eq!(pubsym.segment, 0x000c);
    assert_eq!(pubsym.offset, 0x0004aeb0);

    // great, good to go
    // find the debug information
    pdb.debug_information().expect("debug_information");

    // TODO:
    //   build an address translator
    //   translate the segment+offset
    //   assert_eq!(rva, 0x003768c0)
}
