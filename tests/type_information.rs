use std::collections::HashMap;

use pdb::{FallibleIterator, Result};

fn setup<F>(func: F) -> Result<()>
where
    F: FnOnce(&pdb::TypeInformation<'_>) -> Result<()>,
{
    let file = if let Ok(filename) = std::env::var("PDB_FILE") {
        std::fs::File::open(filename)
    } else {
        std::fs::File::open("fixtures/self/foo.pdb")
    }?;

    let mut pdb = pdb::PDB::open(file)?;
    let type_information = pdb.type_information()?;

    func(&type_information)?;

    Ok(())
}

#[test]
fn iteration() -> Result<()> {
    setup(|type_information| -> Result<()> {
        let len = type_information.len();

        let mut count: usize = 0;
        let mut last_index = pdb::TypeIndex(4095);
        let mut iter = type_information.iter();
        while let Some(typ) = iter.next().expect("next type") {
            assert_eq!(typ.index().0, last_index.0 + 1);
            last_index = typ.index();
            count += 1;
        }

        assert_eq!(len, count);
        Ok(())
    })?;

    Ok(())
}

#[test]
fn type_finder() -> Result<()> {
    setup(|type_information| -> Result<()> {
        let mut type_finder = type_information.finder();
        let mut map: HashMap<pdb::TypeIndex, pdb::Type<'_>> = HashMap::new();

        assert_eq!(type_finder.max_index().0 >> 3, 4096 >> 3);

        let mut iter = type_information.iter();
        while let Some(typ) = iter.next().expect("next type") {
            assert_eq!(type_finder.max_index().0 >> 3, typ.index().0 >> 3);
            type_finder.update(&iter);
            map.insert(typ.index(), typ);
        }

        for (index, typ) in map.iter() {
            let found = type_finder.find(*index).expect("find");
            assert_eq!(*typ, found);
        }

        Ok(())
    })?;

    Ok(())
}

#[test]
fn find_classes() -> Result<()> {
    setup(|type_information| -> Result<()> {
        let mut type_finder = type_information.finder();

        let mut iter = type_information.iter();
        while let Some(typ) = iter.next().expect("next type") {
            type_finder.update(&iter);

            match typ.parse() {
                Ok(pdb::TypeData::Class(pdb::ClassType {
                    name,
                    fields: Some(fields),
                    ..
                })) => {
                    println!("class {} (type {}):", name, typ.index());

                    match type_finder.find(fields).expect("find fields").parse() {
                        Ok(pdb::TypeData::FieldList(list)) => {
                            for field in list.fields {
                                println!("  - {:?}", field);
                            }

                            if let Some(c) = list.continuation {
                                println!("TODO: follow to type {}", c);
                            }
                        }
                        Ok(value) => {
                            panic!("expected a field list, got {:?}", value);
                        }
                        Err(e) => {
                            println!("field parse error: {}", e);
                        }
                    }
                }
                Ok(pdb::TypeData::Enumeration(data)) => {
                    println!("enum {} (type {}):", data.name, data.fields);

                    match type_finder.find(data.fields).expect("find fields").parse() {
                        Ok(pdb::TypeData::FieldList(list)) => {
                            for field in list.fields {
                                println!("  - {:?}", field);
                            }

                            if let Some(c) = list.continuation {
                                println!("TODO: follow to type {}", c);
                            }
                        }
                        Ok(value) => {
                            panic!("expected a field list, got {:?}", value);
                        }
                        Err(e) => {
                            println!("field parse error: {}", e);
                        }
                    }
                }
                Ok(pdb::TypeData::FieldList(_)) => {
                    // ignore, since we find these by class
                }
                Ok(_) => {
                    // ignore other types
                }
                Err(pdb::Error::UnimplementedTypeKind(kind)) => {
                    println!("unimplemented: 0x{:04x}", kind);
                }
                Err(e) => {
                    println!(
                        "other parse error on type {} (raw type {:04x}): {}",
                        typ.index(),
                        typ.raw_kind(),
                        e
                    );
                    panic!("dying due to parse error");
                }
            }
        }

        Ok(())
    })?;

    Ok(())
}

/*
#[bench]
fn bench_type_finder(b: &mut test::Bencher) {
    setup(|type_information| {
        let mut type_finder = type_information.finder();

        assert_eq!(type_finder.max_index() >> 3, 4096 >> 3);

        // iterate over all the types
        let mut iter = type_information.iter();
        while let Some(typ) = iter.next().expect("next type") {
            assert_eq!(type_finder.max_index() >> 3, typ.index() >> 3);
            type_finder.update(&iter);
        }

        let mut rng = rand::thread_rng();
        let count: pdb::TypeIndex = type_information.len() as pdb::TypeIndex;
        let base: pdb::TypeIndex = 4096;

        // time how long it takes to build a map
        b.iter(|| {
            let lucky = rng.gen_range(base, base + count);
            let found = type_finder.find(lucky).expect("find");
            test::black_box(&found);
        });
    })
}
*/

/*
#[test]
fn type_length_histogram() {
    setup(|type_information| {
        let mut lens: Vec<usize> = Vec::new();
        lens.resize(1025, 0);

        // iterate over all the types
        let mut iter = type_information.iter();
        while let Some(typ) = iter.next().expect("next type") {
            let mut len = typ.len() + 2;
            if len > 1024 {
                len = 1024;
            }
            lens[len] += 1;
        }

        for (len, count) in lens.as_slice().iter().enumerate() {
            println!("{}\t{}", len, count);
        }

        panic!();
    })
}
*/
