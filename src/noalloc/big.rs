use crate::noalloc::misc::*;
use crate::Result;

#[derive(Debug)]
pub struct BigMsf<'a> {
    dir: BigDirReader<'a>,
    pub page_size: u32,
    pub num_streams: u32,
}

impl<'a> BigMsf<'a> {
    
    pub fn parse(data: &'a [u8]) -> Result<Self> {
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

    pub fn stream_size(&self, stream: u32) -> Result<Option<u32>> {
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

    pub fn stream_chunks(&self, stream: u32) -> Result<StreamChunks<'a>> {
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
