//! Tests that IdInformation works on files where the IPI is missing (empty stream).

use pdb::{FallibleIterator, Result, IdIndex, PDB};

fn open_file() -> Result<std::fs::File> {
    let path = "fixtures/symbol_server/0ea7c70545374958ad3307514bdfc8642-wntdll.pdb";
    Ok(std::fs::File::open(path)?)
}

#[test]
fn test_missing_ipi() -> Result<()> {
    let mut pdb = PDB::open(open_file()?)?;

    let id_information = pdb.id_information()?;

    assert_eq!(id_information.len(), 0);
    assert!(id_information.is_empty());

    let mut iter = id_information.iter();
    assert!(iter.next().expect("iter empty IPI").is_none());

    let finder = id_information.finder();
    assert_eq!(finder.max_index(), IdIndex(0));
    finder.find(IdIndex(0)).expect_err("find index");
    finder.find(IdIndex(4097)).expect_err("find index");

    Ok(())
}
