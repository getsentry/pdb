use core::convert::TryInto;
use crate::{DBI_HEADER_LEN, DBI_STREAM, DBIHeader, MachineType, ModuleIter, ParseBuffer, Result, msf, noalloc::{big::BigMsf, misc::StreamChunks, small::SmallMsf}};

mod misc;
mod small;
mod big;

#[derive(Debug)]
pub struct PDB<'a> {
    msf: Msf<'a>,
}

#[derive(Debug)]
pub enum Msf<'a> {
    Small(SmallMsf<'a>),
    Big(BigMsf<'a>),
}

#[derive(Debug)]
pub struct DebugInformation {
    header: DBIHeader,
    header_len: usize,
}

impl DebugInformation {
    pub fn parse(data: &[u8]) -> Result<Self> {
        let mut parse_buffer = ParseBuffer::from(data);
        let header = DBIHeader::parse_buf(&mut parse_buffer)?;
        let header_len = parse_buffer.pos();
        
        Ok(Self {
            header,
            header_len
        })
    }

    /// Returns a copy of the parsed DBI header.
    pub const fn header(&self) -> DBIHeader {
        self.header
    }

    /// Returns this PDB's original `age`.
    pub const fn age(&self) -> Option<u32> {
        match self.header.age {
            0 => None,
            age => Some(age),
        }
    }

    /// Returns the target's machine type (architecture).
    pub fn machine_type(&self) -> Result<MachineType> {
        Ok(self.header.machine_type.into())
    }

    /// Reads the DBI module list into `dst` and returns an iterator over the
    /// modules it contains.
    pub fn modules<'b>(
        &self,
        pdb: &PDB<'_>,
        dst: &'b mut [u8],
    ) -> Result<ModuleIter<'b>> {
        let want = self.header.module_list_size as usize;
        if dst.len() < want {
            return Err(crate::Error::UnexpectedEof);
        }

        let n = pdb.read_stream_at(
            DBI_STREAM,
            self.header_len as u32,
            &mut dst[..want],
        )?;

        if n < want {
            return Err(crate::Error::UnexpectedEof);
        }

        Ok(ModuleIter::new(&dst[..want]))
    }
}

impl<'a> PDB<'a> {
    pub fn open(data: &'a [u8]) -> Result<Self> {
        if msf::header_matches(data, msf::small::MAGIC) {
            return Ok(Self {
                msf: Msf::Small(SmallMsf::parse(data)?),
            });
        }
        if msf::header_matches(data, msf::big::MAGIC) {
            return Ok(Self {
                msf: Msf::Big(BigMsf::parse(data)?),
            });
        }
        Err(crate::Error::UnrecognizedFileFormat)
    }

    pub fn debug_information(&self) -> Result<DebugInformation> {
        let mut buf = [0u8; DBI_HEADER_LEN];
        let n = self.read_stream(DBI_STREAM, &mut buf)?;
        
        if n < DBI_HEADER_LEN {
            return Err(crate::Error::UnexpectedEof);
        }

        DebugInformation::parse(&buf)
    }

    pub fn msf_kind(&self) -> msf::MsfKind {
        match self.msf {
            Msf::Small(_) => msf::MsfKind::Small,
            Msf::Big(_) => msf::MsfKind::Big,
        }
    }

    pub fn page_size(&self) -> u32 {
        match &self.msf {
            Msf::Small(m) => m.page_size,
            Msf::Big(m) => m.page_size,
        }
    }

    pub fn stream_count(&self) -> u32 {
        match &self.msf {
            Msf::Small(m) => m.num_streams,
            Msf::Big(m) => m.num_streams,
        }
    }

    pub fn stream_size(&self, stream: u32) -> Result<Option<u32>> {
        match &self.msf {
            Msf::Small(m) => m.stream_size(stream),
            Msf::Big(m) => m.stream_size(stream),
        }
    }

    pub fn stream_chunks(&self, stream: u32) -> Result<StreamChunks<'a>> {
        match &self.msf {
            Msf::Small(m) => m.stream_chunks(stream),
            Msf::Big(m) => m.stream_chunks(stream),
        }
    }

    pub fn read_stream(&self, stream: u32, dst: &mut [u8]) -> Result<usize> {
        let mut written = 0;
        for chunk in self.stream_chunks(stream)? {
            let chunk = chunk?;
            if written == dst.len() {
                break;
            }
            let n = chunk.len().min(dst.len() - written);
            dst[written..written + n].copy_from_slice(&chunk[..n]);
            written += n;
        }
        Ok(written)
    }

    /// Reads up to `dst.len()` bytes from `stream`, starting at byte `start`
    /// within that stream. Returns the number of bytes written.
    pub fn read_stream_at(
        &self,
        stream: u32,
        start: u32,
        dst: &mut [u8],
    ) -> Result<usize> {
        let mut written = 0usize;
        let mut stream_pos: u32 = 0;
        let mut remaining_start = start;

        for chunk in self.stream_chunks(stream)? {
            let chunk = chunk?;
            let chunk_len = chunk.len() as u32;

            // Skip chunks entirely before `start`.
            if remaining_start >= chunk_len {
                remaining_start -= chunk_len;
                stream_pos += chunk_len;
                continue;
            }

            // Take the part of this chunk that's at or after `start`.
            let chunk_start = remaining_start as usize;
            let available = &chunk[chunk_start..];
            let take = available.len().min(dst.len() - written);
            dst[written..written + take].copy_from_slice(&available[..take]);
            written += take;

            if written == dst.len() {
                break;
            }

            stream_pos += chunk_len;
            remaining_start = 0;
        }

        Ok(written)
    }
}
