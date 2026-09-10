use core::convert::TryInto;

use crate::noalloc::misc::*;
use crate::Result;

#[derive(Debug)]
pub struct SmallMsf<'a> {
    data: &'a [u8],
    pub page_size: u32,
    max_page: u32,
    toc: &'a [u8],
    pub num_streams: u32,
}

impl<'a> SmallMsf<'a> {
    const SMALL_ENTRY_SIZE: usize = 8;

    pub fn parse(data: &'a [u8]) -> Result<Self> {
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

    pub fn stream_size(&self, stream: u32) -> Result<Option<u32>> {

        if stream >= self.num_streams {
            return Err(crate::Error::StreamNotFound(stream));
        }
        
        let off = 4 + stream as usize * Self::SMALL_ENTRY_SIZE;
        
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
        let mut offset = 4 + self.num_streams as usize * Self::SMALL_ENTRY_SIZE;
        
        for i in 0..stream {
            let size_off = 4 + i as usize * Self::SMALL_ENTRY_SIZE;
        
            if size_off + 4 > self.toc.len() {
                return Err(crate::Error::UnexpectedEof);
            }
        
            let size = u32::from_le_bytes(
                self.toc[size_off..size_off + 4].try_into().unwrap(),
            );
        
            if size != u32::MAX {
                offset += pages_needed_u32(size, self.page_size) as usize * 2;
            }
        }
        Ok(offset)
    }

    pub fn stream_chunks(&self, stream: u32) -> Result<StreamChunks<'a>> {
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
