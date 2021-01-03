use std::collections::hash_map::{Entry, HashMap};

use pdb::FallibleIterator;

fn setup<F>(func: F)
where
    F: FnOnce(&pdb::SymbolTable<'_>, bool),
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

    func(&symbol_table, is_fixture);
}

#[test]
fn count_symbols() {
    setup(|global_symbols, is_fixture| {
        let mut map: HashMap<u16, usize> = HashMap::new();

        // walk the symbol table
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
    })
}

#[test]
fn find_symbols() {
    setup(|global_symbols, is_fixture| {
        // can't do much if we don't know which PDB we're using
        if !is_fixture {
            return;
        }

        let mut map: HashMap<&[u8], Option<pdb::SymbolData<'_>>> = HashMap::new();

        // look for:
        // main(), defined in the program
        map.insert(b"main", None);

        // malloc(), defined in libc
        map.insert(b"memcpy", None);

        // HeapAlloc(), defined... somewhere
        map.insert(b"HeapAlloc", None);

        // Baz::static_f_public(), except MSVC-mangled
        map.insert(b"?static_f_public@Baz@@SAXXZ", None);

        // walk the symbol table
        let mut iter = global_symbols.iter();
        while let Some(sym) = iter.next().expect("next symbol") {
            // ensure we can parse all the symbols, even though we only want a few
            let data = sym.parse().expect("symbol parsing");

            // get symbol name
            let name = data.name().unwrap_or_default();

            if let Entry::Occupied(mut e) = map.entry(name.as_bytes()) {
                // this is a symbol we wanted to find
                // store our data
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
    })
}
