use core::convert::TryInto;
use crate::Result;

pub const BIG_HEADER_LEN: usize = 52;
pub const SMALL_BLOCK_SIZE_OFFSET: usize = 44;
pub const SMALL_TOTAL_ALLOC_OFFSET: usize = 50;
pub const SMALL_TOC_SIZE_OFFSET: usize = 52;
pub const SMALL_TOC_PAGES_OFFSET: usize = 60;
pub const BIG_PAGE_SIZE_OFFSET: usize = 32;
pub const BIG_PAGES_USED_OFFSET: usize = 40;
pub const BIG_DIRECTORY_SIZE_OFFSET: usize = 44;

pub fn read_u32_at(data: &[u8], offset: usize) -> Result<u32> {
    if offset + 4 > data.len() {
        return Err(crate::Error::UnexpectedEof);
    }
    Ok(u32::from_le_bytes(data[offset..offset + 4].try_into().unwrap()))
}

pub fn read_u16_at(data: &[u8], offset: usize) -> Result<u16> {
    if offset + 2 > data.len() {
        return Err(crate::Error::UnexpectedEof);
    }
    Ok(u16::from_le_bytes(data[offset..offset + 2].try_into().unwrap()))
}

pub const fn pages_needed_u32(size: u32, page_size: u32) -> u32 {
    (size + page_size - 1) / page_size
}


#[derive(Debug, Clone, Copy)]
pub struct BigDirReader<'a> {
    pub data: &'a [u8],
    pub page_size: u32,
    pub directory_size: u32,
    pub max_page: u32,
}

impl<'a> BigDirReader<'a> {
    pub fn read_u32(&self, dir_offset: usize) -> Result<u32> {
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
pub enum PageSource<'a> {
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
    pub data: &'a [u8],
    pub page_size: u32,
    pub source: PageSource<'a>,
    pub total_pages: u32,
    pub current: u32,
    pub remaining: u32,
}

impl<'a> StreamChunks<'a> {
    pub const fn empty(data: &'a [u8]) -> Self {
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