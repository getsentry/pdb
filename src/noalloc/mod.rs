use core::convert::TryInto;
use crate::{DBI_HEADER_LEN, DBI_STREAM, DBIHeader, MachineType, ModuleIter, ParseBuffer, Result, msf};

const BIG_HEADER_LEN: usize = 52;
const SMALL_BLOCK_SIZE_OFFSET: usize = 44;
const SMALL_TOTAL_ALLOC_OFFSET: usize = 50;
const SMALL_TOC_SIZE_OFFSET: usize = 52;
const SMALL_TOC_PAGES_OFFSET: usize = 60;
const BIG_PAGE_SIZE_OFFSET: usize = 32;
const BIG_PAGES_USED_OFFSET: usize = 40;
const BIG_DIRECTORY_SIZE_OFFSET: usize = 44;

fn read_u32_at(data: &[u8], offset: usize) -> Result<u32> {
    if offset + 4 > data.len() {
        return Err(crate::Error::UnexpectedEof);
    }
    Ok(u32::from_le_bytes(data[offset..offset + 4].try_into().unwrap()))
}

fn read_u16_at(data: &[u8], offset: usize) -> Result<u16> {
    if offset + 2 > data.len() {
        return Err(crate::Error::UnexpectedEof);
    }
    Ok(u16::from_le_bytes(data[offset..offset + 2].try_into().unwrap()))
}

fn pages_needed_u32(size: u32, page_size: u32) -> u32 {
    (size + page_size - 1) / page_size
}

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

#[derive(Debug)]
pub struct SmallMsf<'a> {
    data: &'a [u8],
    page_size: u32,
    max_page: u32,
    toc: &'a [u8],
    num_streams: u32,
}

impl<'a> SmallMsf<'a> {
    fn parse(data: &'a [u8]) -> Result<Self> {
        if data.len() < SMALL_TOC_PAGES_OFFSET + 2 {
            return Err(crate::Error::UnexpectedEof);
        }

        let page_size = read_u32_at(data, SMALL_BLOCK_SIZE_OFFSET)?;
        if page_size == 0 || !page_size.is_power_of_two() {
            return Err(crate::Error::InvalidPageSize(page_size));
        }

        let max_page = read_u16_at(data, SMALL_TOTAL_ALLOC_OFFSET)? as u32;
        let toc_size = read_u32_at(data, SMALL_TOC_SIZE_OFFSET)? as usize;

        let toc_pages_needed = pages_needed_u32(toc_size as u32, page_size);

        if toc_pages_needed == 0 {
            return Ok(Self {
                data,
                page_size,
                max_page,
                toc: &[],
                num_streams: 0,
            });
        }

        if toc_pages_needed > 1 {
            return Err(crate::Error::UnimplementedFeature(
                "multi-page TOC",
            ));
        }

        let toc_page = read_u16_at(data, SMALL_TOC_PAGES_OFFSET)? as u32;
        if toc_page == 0 || toc_page > max_page {
            return Err(crate::Error::PageReferenceOutOfRange(toc_page));
        }

        let toc_start = toc_page as usize * page_size as usize;
        let toc_end = toc_start + toc_size;
        if toc_end > data.len() {
            return Err(crate::Error::UnexpectedEof);
        }
        let toc = &data[toc_start..toc_end];

        if toc.len() < 4 {
            return Err(crate::Error::UnexpectedEof);
        }
        let num_streams = u32::from_le_bytes(toc[0..4].try_into().unwrap());

        Ok(Self {
            data,
            page_size,
            max_page,
            toc,
            num_streams,
        })
    }

    fn stream_size(&self, stream: u32) -> Result<Option<u32>> {
        if stream >= self.num_streams {
            return Err(crate::Error::StreamNotFound(stream));
        }
        let off = 4 + stream as usize * 4;
        if off + 4 > self.toc.len() {
            return Err(crate::Error::UnexpectedEof);
        }
        let size = u32::from_le_bytes(self.toc[off..off + 4].try_into().unwrap());
        if size == u32::MAX {
            Ok(None)
        } else {
            Ok(Some(size))
        }
    }

    fn pages_offset_for(&self, stream: u32) -> Result<usize> {
        let mut offset = 4 + self.num_streams as usize * 4;
        for i in 0..stream {
            let size_off = 4 + i as usize * 4;
            let size = u32::from_le_bytes(
                self.toc[size_off..size_off + 4].try_into().unwrap(),
            );
            if size != u32::MAX {
                offset += pages_needed_u32(size, self.page_size) as usize * 2;
            }
        }
        Ok(offset)
    }

    fn stream_chunks(&self, stream: u32) -> Result<StreamChunks<'a>> {
        let size = self
            .stream_size(stream)?
            .ok_or(crate::Error::StreamNotFound(stream))?;

        if size == 0 {
            return Ok(StreamChunks::empty(self.data));
        }

        Ok(StreamChunks {
            data: self.data,
            page_size: self.page_size,
            source: PageSource::Small {
                toc: self.toc,
                pages_offset: self.pages_offset_for(stream)?,
            },
            total_pages: pages_needed_u32(size, self.page_size),
            current: 0,
            remaining: size,
        })
    }
}

#[derive(Debug)]
pub struct BigMsf<'a> {
    dir: BigDirReader<'a>,
    page_size: u32,
    num_streams: u32,
}

impl<'a> BigMsf<'a> {
    fn parse(data: &'a [u8]) -> Result<Self> {
        if data.len() < BIG_HEADER_LEN {
            return Err(crate::Error::UnexpectedEof);
        }

        let page_size = read_u32_at(data, BIG_PAGE_SIZE_OFFSET)?;
        if page_size < 0x100 || page_size > 128 * 0x10000 || !page_size.is_power_of_two() {
            return Err(crate::Error::InvalidPageSize(page_size));
        }

        let max_page = read_u32_at(data, BIG_PAGES_USED_OFFSET)?;
        let directory_size = read_u32_at(data, BIG_DIRECTORY_SIZE_OFFSET)?;

        if directory_size % 4 != 0 {
            return Err(crate::Error::UnexpectedEof);
        }

        let dir = BigDirReader {
            data,
            page_size,
            directory_size,
            max_page,
        };

        let num_streams = if directory_size >= 4 {
            dir.read_u32(0)?
        } else {
            0
        };

        Ok(Self {
            dir,
            page_size,
            num_streams,
        })
    }

    fn stream_size(&self, stream: u32) -> Result<Option<u32>> {
        if stream >= self.num_streams {
            return Err(crate::Error::StreamNotFound(stream));
        }
        let size = self.dir.read_u32(4 + stream as usize * 4)?;
        if size == u32::MAX {
            Ok(None)
        } else {
            Ok(Some(size))
        }
    }

    fn pages_offset_for(&self, stream: u32) -> Result<usize> {
        let mut offset = 4 + self.num_streams as usize * 4;
        for i in 0..stream {
            let size = self.dir.read_u32(4 + i as usize * 4)?;
            if size != u32::MAX {
                offset += pages_needed_u32(size, self.page_size) as usize * 4;
            }
        }
        Ok(offset)
    }

    fn stream_chunks(&self, stream: u32) -> Result<StreamChunks<'a>> {
        let size = self
            .stream_size(stream)?
            .ok_or(crate::Error::StreamNotFound(stream))?;

        if size == 0 {
            return Ok(StreamChunks::empty(self.dir.data));
        }

        Ok(StreamChunks {
            data: self.dir.data,
            page_size: self.page_size,
            source: PageSource::Big {
                dir: self.dir,
                pages_offset: self.pages_offset_for(stream)?,
            },
            total_pages: pages_needed_u32(size, self.page_size),
            current: 0,
            remaining: size,
        })
    }
}

#[derive(Debug, Clone, Copy)]
struct BigDirReader<'a> {
    data: &'a [u8],
    page_size: u32,
    directory_size: u32,
    max_page: u32,
}

impl<'a> BigDirReader<'a> {
    fn read_u32(&self, dir_offset: usize) -> Result<u32> {
        if dir_offset + 4 > self.directory_size as usize {
            return Err(crate::Error::UnexpectedEof);
        }

        let page_size = self.page_size as usize;
        let l2_page_index = dir_offset / page_size;
        let within_l2 = dir_offset % page_size;

        let entries_per_l1 = page_size / 4;
        let l1_page_index = l2_page_index / entries_per_l1;
        let l1_entry_index = l2_page_index % entries_per_l1;

        let l1_page_num_offset = BIG_HEADER_LEN + l1_page_index * 4;
        let l1_page_num = read_u32_at(self.data, l1_page_num_offset)?;
        if l1_page_num == 0 || l1_page_num > self.max_page {
            return Err(crate::Error::PageReferenceOutOfRange(l1_page_num));
        }

        let l1_page_start = l1_page_num as usize * page_size;
        let l2_page_num_offset = l1_page_start + l1_entry_index * 4;
        let l2_page_num = read_u32_at(self.data, l2_page_num_offset)?;
        if l2_page_num == 0 || l2_page_num > self.max_page {
            return Err(crate::Error::PageReferenceOutOfRange(l2_page_num));
        }

        let l2_page_start = l2_page_num as usize * page_size;
        let value_offset = l2_page_start + within_l2;
        read_u32_at(self.data, value_offset)
    }
}

#[derive(Debug, Clone, Copy)]
enum PageSource<'a> {
    Small {
        toc: &'a [u8],
        pages_offset: usize,
    },
    Big {
        dir: BigDirReader<'a>,
        pages_offset: usize,
    },
}

impl<'a> PageSource<'a> {
    fn read_page_number(&self, index: u32) -> Result<u32> {
        match self {
            PageSource::Small { toc, pages_offset } => {
                let off = pages_offset + index as usize * 2;
                if off + 2 > toc.len() {
                    return Err(crate::Error::UnexpectedEof);
                }
                Ok(u16::from_le_bytes(toc[off..off + 2].try_into().unwrap()) as u32)
            }
            PageSource::Big { dir, pages_offset } => {
                let off = pages_offset + index as usize * 4;
                dir.read_u32(off)
            }
        }
    }
}

#[derive(Debug)]
pub struct StreamChunks<'a> {
    data: &'a [u8],
    page_size: u32,
    source: PageSource<'a>,
    total_pages: u32,
    current: u32,
    remaining: u32,
}

impl<'a> StreamChunks<'a> {
    fn empty(data: &'a [u8]) -> Self {
        Self {
            data,
            page_size: 0,
            source: PageSource::Small {
                toc: &[],
                pages_offset: 0,
            },
            total_pages: 0,
            current: 0,
            remaining: 0,
        }
    }
}

impl<'a> Iterator for StreamChunks<'a> {
    type Item = Result<&'a [u8]>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current >= self.total_pages {
            return None;
        }

        let page = match self.source.read_page_number(self.current) {
            Ok(p) => p,
            Err(e) => {
                self.current = self.total_pages;
                return Some(Err(e));
            }
        };

        let mut run: u32 = 1;
        while self.current + run < self.total_pages {
            match self.source.read_page_number(self.current + run) {
                Ok(next) if next == page + run => run += 1,
                Ok(_) => break,
                Err(e) => {
                    self.current = self.total_pages;
                    return Some(Err(e));
                }
            }
        }

        let start = page as usize * self.page_size as usize;
        let run_bytes = (run * self.page_size).min(self.remaining);
        let end = start + run_bytes as usize;

        if end > self.data.len() {
            self.current = self.total_pages;
            return Some(Err(crate::Error::PageReferenceOutOfRange(page)));
        }

        self.remaining -= run_bytes;
        self.current += run;
        Some(Ok(&self.data[start..end]))
    }
}

impl core::iter::FusedIterator for StreamChunks<'_> {}