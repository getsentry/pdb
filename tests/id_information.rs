//! Tests that IdInformation works on files where the IPI is missing (empty stream).

use std::path::Path;
use std::sync::Once;

use pdb::{FallibleIterator, IdIndex, PDB};

static DOWNLOADED: Once = Once::new();
fn open_file() -> std::fs::File {
    let path = "fixtures/symbol_server/0ea7c70545374958ad3307514bdfc8642-wntdll.pdb";
    let url = "https://msdl.microsoft.com/download/symbols/wntdll.pdb/0ea7c70545374958ad3307514bdfc8642/wntdll.pdb";

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
fn test_missing_ipi() {
    let mut pdb = PDB::open(open_file()).expect("opening pdb");

    let id_information = pdb.id_information().expect("get id information");

    // Check ItemInformation API
    assert_eq!(id_information.len(), 0);
    assert!(id_information.is_empty());

    // Check ItemIter API
    let mut iter = id_information.iter();
    assert!(iter.next().expect("iter empty IPI").is_none());

    // Check ItemFinder API
    let finder = id_information.finder();
    assert_eq!(finder.max_index(), IdIndex(0));
    finder.find(IdIndex(0)).expect_err("find index");
    finder.find(IdIndex(4097)).expect_err("find index");
}
