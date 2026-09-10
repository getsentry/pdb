use std::collections::hash_map::{Entry, HashMap};

use pdb::{Result, FallibleIterator};

fn setup<F>(func: F) -> Result<()>
where
    F: FnOnce(&pdb::SymbolTable<'_>, bool) -> Result<()>,
{
    let (file, is_fixture) = if let Ok(filename) = std::env::var("PDB_FILE") {
        (std::fs::File::open(filename).expect("opening file"), false)
    } else {
        (
            std::fs::File::open("fixtures/self/foo.pdb").expect("opening file"),
            true,
        )
    };

    let mut pdb = pdb::PDB::open(file).expect("opening pdb");
    let symbol_table = pdb.global_symbols().expect("global symbols");

    func(&symbol_table, is_fixture)?;

    Ok(())
}

#[test]
fn count_symbols() -> Result<()> {
    setup(|global_symbols: &pdb::SymbolTable<'_>, is_fixture: bool| {
        let mut map: HashMap<u16, usize> = HashMap::new();

        let mut iter = global_symbols.iter();
        while let Some(sym) = iter.next().expect("next symbol") {
            let kind = sym.raw_kind();
            let entry = map.entry(kind).or_insert(0);

            if *entry == 0 && is_fixture {
                // first symbol of this kind seen
                // emit a unit test
                println!("#[test]");
                println!("fn kind_{:04x}() {{", sym.raw_kind());
                println!("    let buf = &{:?};", sym.raw_bytes());
                println!("    let (symbol, data, name) = parse(buf).expect(\"parse\");");
                println!(
                    "    assert_eq!(symbol.raw_kind(), 0x{:04x});",
                    sym.raw_kind()
                );
                println!(
                    "    assert_eq!(data, SymbolData::{:?});",
                    sym.parse().expect("parse")
                );
                println!("}}");
                println!();
            }

            *entry += 1;
        }

        println!("symbol counts by kind:");
        for (kind, count) in &map {
            println!("  - kind: 0x{:04x}, count: {}", kind, count);
        }

        assert!(*map.get(&0x1107).expect("0x1107") >= 500);
        assert!(*map.get(&0x1108).expect("0x1108") >= 400);
        assert!(*map.get(&0x110c).expect("0x110c") >= 90);
        assert!(*map.get(&0x110d).expect("0x110d") >= 120);
        assert!(*map.get(&0x110e).expect("0x110e") >= 3000);
        assert!(*map.get(&0x110e).expect("0x110e") >= 3000);
        assert!(*map.get(&0x1125).expect("0x1125") >= 2000);
        assert!(*map.get(&0x1127).expect("0x1127") >= 500);

        Ok(())
    })?;

    Ok(())
}

#[test]
fn find_symbols() -> Result<()> {
    setup(|global_symbols: &pdb::SymbolTable<'_>, is_fixture: bool| {
        if !is_fixture {
            return Ok(());
        }

        let mut map: HashMap<&[u8], Option<pdb::SymbolData<'_>>> = HashMap::new();

        map.insert(b"main", None);
        map.insert(b"memcpy", None);
        map.insert(b"HeapAlloc", None);
        map.insert(b"?static_f_public@Baz@@SAXXZ", None);

        let mut iter = global_symbols.iter();
        while let Some(sym) = iter.next().expect("next symbol") {
            
            let data = sym.parse().expect("symbol parsing");
            let name = data.name().unwrap_or_default();

            if let Entry::Occupied(mut e) = map.entry(name.as_bytes()) {
                e.insert(Some(data));
            }
        }

        for (key, value) in map {
            match value {
                Some(data) => {
                    println!("found {} => {:?}", String::from_utf8_lossy(key), data);
                }
                None => {
                    panic!("couldn't find {}", String::from_utf8_lossy(key));
                }
            }
        }

        Ok(())
    })?;

    Ok(())
}
