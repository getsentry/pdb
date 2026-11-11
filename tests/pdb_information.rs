#[test]
fn pdb_info() {
    let file = std::fs::File::open("fixtures/self/foo.pdb").expect("opening file");

    let mut pdb = pdb::PDB::open(file).expect("opening pdb");
    let pdb_info = pdb.pdb_information().expect("pdb information");

    assert_eq!(pdb_info.age, 2);
    assert_eq!(
        pdb_info.guid,
        "2B3C3FA5-5A2E-44B8-8BBA-C3300FF69F62".parse().unwrap(),
    );
    assert_eq!(pdb_info.signature, 0x587B_A621);
}

/// Builds a minimal MSF whose PDB Information stream holds one named stream
/// whose `name_offset` points into the names block at the given offset.
///
/// The names block is `b"a\0b\0\0"`, so offsets 0 and 2 name a stream and
/// anything past 5 is out of range.
fn msf_with_name_offset(name_offset: u32) -> Vec<u8> {
    const PAGE_SIZE: usize = 512;

    let names: &[u8] = b"a\0b\0\0";

    // PDB Information stream: header, names block, then the names map.
    let mut info = Vec::new();
    info.extend_from_slice(&20000430u32.to_le_bytes()); // version
    info.extend_from_slice(&0u32.to_le_bytes()); // signature
    info.extend_from_slice(&1u32.to_le_bytes()); // age
    info.extend_from_slice(&0x0123_4567u32.to_le_bytes()); // guid data1
    info.extend_from_slice(&0x89abu16.to_le_bytes()); // guid data2
    info.extend_from_slice(&0xcdefu16.to_le_bytes()); // guid data3
    info.extend_from_slice(&[0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef]); // guid data4
    info.extend_from_slice(&(names.len() as u32).to_le_bytes());
    info.extend_from_slice(names);
    info.extend_from_slice(&1u32.to_le_bytes()); // one entry
    info.extend_from_slice(&8u32.to_le_bytes()); // entry size
    info.extend_from_slice(&0u32.to_le_bytes()); // no ok words
    info.extend_from_slice(&0u32.to_le_bytes()); // no deleted words
    info.extend_from_slice(&name_offset.to_le_bytes());
    info.extend_from_slice(&2u32.to_le_bytes()); // stream id

    // Stream table: two streams, the first nil, the second on page 3.
    let mut stream_table = Vec::new();
    stream_table.extend_from_slice(&2u32.to_le_bytes());
    stream_table.extend_from_slice(&u32::MAX.to_le_bytes());
    stream_table.extend_from_slice(&(info.len() as u32).to_le_bytes());
    stream_table.extend_from_slice(&3u32.to_le_bytes());

    // Superblock: page size, free page map, page count, stream table size,
    // reserved, then the page list holding the stream table.
    let mut superblock = Vec::new();
    superblock.extend_from_slice(b"Microsoft C/C++ MSF 7.00\r\n\x1aDS\0\0\0");
    superblock.extend_from_slice(&(PAGE_SIZE as u32).to_le_bytes());
    superblock.extend_from_slice(&0u32.to_le_bytes());
    superblock.extend_from_slice(&4u32.to_le_bytes());
    superblock.extend_from_slice(&(stream_table.len() as u32).to_le_bytes());
    superblock.extend_from_slice(&0u32.to_le_bytes());
    superblock.extend_from_slice(&1u32.to_le_bytes());
    superblock.extend_from_slice(&2u32.to_le_bytes());

    let mut file = Vec::new();
    for page in [superblock, 2u32.to_le_bytes().to_vec(), stream_table, info] {
        file.extend_from_slice(&page);
        file.resize(file.len().next_multiple_of(PAGE_SIZE), 0);
    }
    // The MSF header is read as a 4096-byte page regardless of the page size.
    file.resize(4096, 0);
    file
}

#[test]
fn stream_names_reads_a_name_offset_inside_the_names_block() {
    let file = msf_with_name_offset(2);
    let mut pdb = pdb::PDB::open(std::io::Cursor::new(file)).expect("opening pdb");
    let info = pdb.pdb_information().expect("pdb information");
    let names = info.stream_names().expect("stream names");

    let names: Vec<_> = names.iter().map(|n| n.name.to_string()).collect();
    assert_eq!(names, ["b"]);
}

#[test]
fn stream_names_rejects_a_name_offset_past_the_names_block() {
    let file = msf_with_name_offset(0xFFFF_0000);
    let mut pdb = pdb::PDB::open(std::io::Cursor::new(file)).expect("opening pdb");
    let info = pdb.pdb_information().expect("pdb information");

    info.stream_names()
        .expect_err("a name offset past the names block must not panic");
}
