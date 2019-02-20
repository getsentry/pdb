extern crate pdb;
extern crate uuid;
extern crate reqwest;

use std::path::Path;
use std::str::FromStr;
use std::sync::{Once, ONCE_INIT};
use pdb::FallibleIterator;

// This test is intended to cover OMAP address translation:
//   https://github.com/willglynn/pdb/issues/17

static DOWNLOADED: Once = ONCE_INIT;
fn open_file() -> std::fs::File {
    let path = "fixtures/symbol_server/3844dbb920174967be7aa4a2c20430fa2-ntkrnlmp.pdb";
    let url = "https://msdl.microsoft.com/download/symbols/ntkrnlmp.pdb/3844dbb920174967be7aa4a2c20430fa2/ntkrnlmp.pdb";

    DOWNLOADED.call_once(|| {
        if !Path::new(path).exists() {
            let mut response = reqwest::get(url).expect("GET request");
            let mut destination = std::fs::File::create(path).expect("create PDB");
            response.copy_to(&mut destination).expect("download");
        }
    });

    std::fs::File::open(path).expect("open PDB")
}

#[test]
fn verify_pdb_identity() {
    // make sure this is the right PDB
    let mut pdb = pdb::PDB::open(open_file()).expect("opening pdb");

    let pdb_info = pdb.pdb_information().expect("pdb information");
    assert_eq!(pdb_info.guid, uuid::Uuid::from_str("3844DBB9-2017-4967-BE7A-A4A2C20430FA").unwrap());
    assert_eq!(pdb_info.age, 5);
    assert_eq!(pdb_info.signature, 1_290_245_416);
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
    assert_eq!(pubsym.offset, 0x0004_aeb0);

    // translate the segment offset to an RVA
    let translator = pdb.address_translator().expect("address translator");
    let rva = translator.to_rva(pubsym.segment, pubsym.offset);
    assert_eq!(rva, Some(0x0037_68c0));
}
