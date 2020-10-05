use std::path::Path;
use std::sync::Once;

use pdb::{FallibleIterator, PdbInternalRva, PdbInternalSectionOffset, Rva};

// This test is intended to cover OMAP address translation:
//   https://github.com/willglynn/pdb/issues/17

static DOWNLOADED: Once = Once::new();
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
fn test_omap_section_zero() {
    // https://github.com/willglynn/pdb/issues/87

    let mut pdb = pdb::PDB::open(open_file()).expect("opening pdb");

    let address = pdb::PdbInternalSectionOffset {
        offset: 0,
        section: 0x1234,
    };

    let address_map = pdb.address_map().expect("address map");

    assert_eq!(address.to_rva(&address_map), None);
}

#[test]
fn test_omap_symbol() {
    let mut pdb = pdb::PDB::open(open_file()).expect("opening pdb");

    let global_symbols = pdb.global_symbols().expect("global_symbols");

    // find the target symbol
    let target_symbol = {
        let target_name = pdb::RawString::from("NtWaitForSingleObject");
        let mut iter = global_symbols.iter();
        iter.find(|sym| {
            let matches = sym
                .parse()
                .ok()
                .and_then(|d| d.name())
                .map_or(false, |n| n == target_name);
            Ok(matches)
        })
        .expect("iterate symbols")
        .expect("find target symbol")
    };

    // extract the PublicSymbol data
    let pubsym = match target_symbol.parse().expect("parse symbol") {
        pdb::SymbolData::Public(pubsym) => pubsym,
        _ => panic!("expected public symbol"),
    };

    // ensure the symbol has the correct location
    assert_eq!(
        pubsym.offset,
        PdbInternalSectionOffset {
            section: 0xc,
            offset: 0x0004_aeb0,
        }
    );

    // translate the segment offset to an RVA
    let address_map = pdb.address_map().expect("address map");
    assert_eq!(pubsym.offset.to_rva(&address_map), Some(Rva(0x0037_68c0)));
    assert_eq!(
        Rva(0x0037_68c0).to_internal_offset(&address_map),
        Some(pubsym.offset)
    );
}

#[test]
fn test_omap_range() {
    let mut pdb = pdb::PDB::open(open_file()).expect("opening pdb");
    let address_map = pdb.address_map().expect("address map");

    // Range partially covered by OMAPs
    // [
    //   OMAPRecord {
    //       source_address: 0x000010aa,
    //       target_address: 0x00015de6
    //   },
    //   OMAPRecord {
    //       source_address: 0x000010bd,
    //       target_address: 0x00000000
    //   },
    //   OMAPRecord {
    //       source_address: 0x000010c4,
    //       target_address: 0x0002da00
    //   },
    //   OMAPRecord {
    //       source_address: 0x000010c8,
    //       target_address: 0x0002da04
    //   },
    // ]
    let start = PdbInternalRva(0x10b0);
    let end = PdbInternalRva(0x10c6);

    assert_eq!(
        address_map.rva_ranges(start..end).collect::<Vec<_>>(),
        vec![
            Rva(0x15dec)..Rva(0x15df9), // 0x10aa - 0x10bd
            // 0x10bd - 0x10c4 omitted due to missing target address
            Rva(0x2da00)..Rva(0x2da02), // 0x10c4 - 0x10c6
        ],
    );

    // Range starting outside OMAPs
    // [
    //   OMAPRecord {
    //       source_address: 0x00001000,
    //       target_address: 0x00000000
    //   },
    //   OMAPRecord {
    //       source_address: 0x00001008,
    //       target_address: 0x00015d44
    //   },
    // ]
    let start = PdbInternalRva(0x0);
    let end = PdbInternalRva(0x1010);
    assert_eq!(
        address_map.rva_ranges(start..end).collect::<Vec<_>>(),
        vec![Rva(0x15d44)..Rva(0x15d4c)],
    );

    // Range ending outside OMAPs
    // [
    //   OMAPRecord {
    //       source_address: 0x005e40e0,
    //       target_address: 0x005e50e0
    //   },
    //   OMAPRecord {
    //       source_address: 0x005e5000,
    //       target_address: 0x00000000
    //   },
    //   OMAPRecord {
    //       source_address: 0x005e70c0,
    //       target_address: 0x00000000
    //   }
    // ]
    let start = PdbInternalRva(0x5e_4fe0);
    let end = PdbInternalRva(0x5e_8000);
    assert_eq!(
        address_map.rva_ranges(start..end).collect::<Vec<_>>(),
        vec![Rva(0x005e_5fe0)..Rva(0x5e_6000)],
    );

    // Range fully before OMAPs
    let start = PdbInternalRva(0x0);
    let end = PdbInternalRva(0x100);
    assert_eq!(
        address_map.rva_ranges(start..end).collect::<Vec<_>>(),
        vec![],
    );

    // Range fully after OMAPs
    let start = PdbInternalRva(0x005e_8000);
    let end = PdbInternalRva(0x005e_9000);
    assert_eq!(
        address_map.rva_ranges(start..end).collect::<Vec<_>>(),
        vec![], // last record targets 0, thus the range is omitted
    );
}
