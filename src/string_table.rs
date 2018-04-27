use common::*;
use msf::Stream;


#[derive(Debug, Pread)]
struct Header {
    signature: u32,   // PDBStringTableSignature
    hash_version: u32, // 1 or 2
    byte_size: u32, // Number of bytes of names buffer.
}

pub struct StringTable<'s> {
    stream: Stream<'s>,
}

pub fn new_string_table<'s>(stream: Stream<'s>) -> Result<StringTable<'s>> {
    {
        let mut names = stream.parse_buffer();
        let _header: Header = names.parse()?;
        // XXX: check the header
    }
    Ok(StringTable{stream})
}

impl<'s> StringTable<'s> {
    pub fn get(&self, offset: usize) -> Result<RawString> {
        let mut names = self.stream.parse_buffer();
        // XXX: reading the header every time is not good
        let _header: Header = names.parse()?;
        names.take(offset)?;
        names.parse_cstring()
    }
}