use std::io::Cursor;
use pdb::{FallibleIterator, MsfKind, PdbHeaderVersion, Result};

/// Parses `fixtures/small1.pdb`, a Small MSF (VC6-era) file, through the
/// alloc-enabled `PDB` reader.
///
/// Exercises the Small MSF container path end to end: header detection,
/// MSF metadata, the PDB info stream (which predates the GUID field and
/// reports `guid = None`), the named-stream map, the string table, and the
/// TPI stream. The DBI stream is nil in this fixture and is not parsed.
#[test]
fn should_parse_small_pdb() -> Result<()> {
    let path = r#"C:\repos\pdb\fixtures\small1.pdb"#;
    let data = std::fs::read(path).unwrap();

    let cursor = Cursor::new(data.as_slice());
    let mut pdb = pdb::PDB::open(cursor)?;
    assert_eq!(pdb.msf_kind(), crate::MsfKind::Small);
    assert_eq!(pdb.stream_count()?, 6);

    let pdb_info = pdb.pdb_information()?;
    assert_eq!(pdb_info.age, 13);
    assert_eq!(pdb_info.guid, None);
    assert_eq!(pdb_info.names_offset, 16);
    assert_eq!(pdb_info.names_size, 7);
    assert_eq!(pdb_info.signature.to_rfc3339().unwrap(), "2005-05-01T14:24:21+00:00");
    assert_eq!(pdb_info.version, PdbHeaderVersion::Vc98);

    let mut stream_names = pdb_info.stream_names_iter()?;
    let stream_name = stream_names.next().unwrap()?;
    assert_eq!(stream_name.name.as_bytes(), b"/names");

    let string_table = pdb.string_table()?;
    let mut iter = string_table.iter()?;
    let (ref_str, raw_str,) = iter.next()?.unwrap();
    assert_eq!(ref_str.0, 0);
    assert_eq!(raw_str.as_bytes(), b"");

    let stream = pdb.raw_stream(pdb::StreamIndex(3))?.unwrap();
    assert_eq!(stream.len(), 0);

    let type_info = pdb.type_information()?;

    let mut iter = type_info.iter();

    while let Some(item_type) = iter.next()? {
        println!("{} {}", item_type.len(), item_type.kind());

        let parsed = item_type.parse().unwrap();
    }

    // let stream = pdb.raw_stream(pdb::StreamIndex(2))?.unwrap();

    // let bytes = stream.as_slice();
    // for (i, chunk) in bytes.chunks(16).enumerate() {
    //     print!("STREAM2  {:04x}:", i * 16);
    //     for b in chunk { print!(" {:02x}", b); }
    //     println!();
    // }

    Ok(())
}

#[test]
fn small_stream_sizes_match_toc() -> Result<()> {
    let data = std::fs::read(r#"C:\repos\pdb\fixtures\small1.pdb"#).unwrap();
    let cursor = Cursor::new(data.as_slice());
    let mut pdb = pdb::PDB::open(cursor)?;

    assert_eq!(pdb.msf_kind(), MsfKind::Small);
    assert_eq!(pdb.stream_count()?, 6);

    assert_eq!(pdb.stream_size(0)?, Some(62));
    assert_eq!(pdb.stream_size(1)?, Some(55));
    assert_eq!(pdb.stream_size(2)?, Some(1104));
    assert_eq!(pdb.stream_size(3)?, Some(0));
    assert_eq!(pdb.stream_size(4)?, Some(132));
    assert_eq!(pdb.stream_size(5)?, Some(25));

    Ok(())
}