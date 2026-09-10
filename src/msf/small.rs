use core::marker::PhantomData;
use scroll::ctx::TryFromCtx;
use scroll::{Endian, Pread, LE};

#[cfg(feature = "alloc")]
use alloc::boxed::Box;

#[cfg(feature = "alloc")]
use alloc::vec::Vec;

use crate::msf::MSFHeader;

#[cfg(feature = "alloc")]
use crate::Source;

#[cfg(feature = "alloc")]
use crate::msf::{MsfImpl, MsfKind};

use crate::{Error, ParseBuffer, Result};

#[cfg(feature = "alloc")]
use crate::SourceView;

#[cfg(feature = "alloc")]
use crate::msf::page_list::PageList;

#[cfg(feature = "alloc")]
use super::Stream;

#[cfg(feature = "alloc")]
use super::Msf;

pub const MAGIC: &[u8] = b"Microsoft C/C++ program database 2.00\r\n\x1a\x4a\x47\0\0";

/// Raw Small MSF header as stored on disk.
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct RawHeader {
    magic: [u8; 44],
    block_size: u32,
    free_list_block: u16,
    total_alloc: u16,
    toc_size: u32,      // PDB_FILE.size
    toc_unknown: u32,   // PDB_FILE.unknown
}

impl<'t> TryFromCtx<'t, Endian> for RawHeader {
    type Error = scroll::Error;

    #[inline]
    fn try_from_ctx(this: &'t [u8], le: Endian) -> scroll::Result<(Self, usize)> {
        let mut offset = 0;
        let data = Self {
            magic: {
                let mut tmp = [0; 44];
                this.gread_inout_with(&mut offset, &mut tmp, le)?;
                tmp
            },
            block_size: this.gread_with(&mut offset, le)?,
            free_list_block: this.gread_with(&mut offset, le)?,
            total_alloc: this.gread_with(&mut offset, le)?,
            toc_size: this.gread_with(&mut offset, le)?,
            toc_unknown: this.gread_with(&mut offset, le)?,
        };
        Ok((data, offset))
    }
}

/// Small MSF (version 2.00) container.
///
/// This implementation handles the older MSF format with fixed 512-byte pages.
/// Directory pages are stored directly in the header at offset 44.
#[cfg(feature = "alloc")]
#[derive(Debug)]
pub struct SmallMSF<'s, S> {
    /// Header information (page size, max page number)
    header: MSFHeader,
    /// Data source providing page access
    source: S,
    /// Directory page list pointing to the stream directory
    directory: PageList,
    /// Phantom data for lifetime
    _phantom: PhantomData<&'s ()>,
}

#[cfg(feature = "alloc")]
impl<'s, S: Source<'s>> SmallMSF<'s, S> {

    /// Creates a new SmallMSF instance by parsing the MSF header.
    pub fn new(source: S, header_view: SourceView<'_>) -> Result<Self> {
        let mut buf = ParseBuffer::from(header_view.as_slice());
        
        let raw_header = buf.parse::<RawHeader>()?;

        if raw_header.magic != MAGIC {
            return Err(Error::UnrecognizedFileFormat);
        }

        let page_size = raw_header.block_size as usize;

        if page_size == 0 || !page_size.is_power_of_two() {
            return Err(Error::InvalidPageSize(page_size as u32));
        }

        let header = MSFHeader {
            page_size,
            maximum_valid_page_number: raw_header.total_alloc as u32,
        };
        
        let mut directory = PageList::new(page_size);
        let pages_needed = header.pages_needed_to_store(raw_header.toc_size as usize);

        for _ in 0..pages_needed {

            let page = buf.parse_u16()?;
            directory.push(header.validate_page_number(page as _)?);
        }

        directory.truncate(raw_header.toc_size as usize);
        
        Ok(SmallMSF {
            header,
            source,
            directory,
            _phantom: PhantomData,
        })
    }

    /// Looks up a stream by number and returns its page list.
    fn look_up_stream(&mut self, stream_number: u32) -> Result<PageList> {
        let directory_view = self.source.view_pages(&self.directory)?;
        let mut dir = ParseBuffer::from(directory_view.as_slice());
        let stream_count = dir.parse_u32()?;

        if stream_number >= stream_count {
            return Err(Error::StreamNotFound(stream_number));
        }

        let mut stream_sizes = Vec::with_capacity(stream_count as usize);
        for i in 0..stream_count {
            let size = dir.parse_u32()?;
            let unknown = dir.parse_u32()?;
            stream_sizes.push(size);
        }

        let block_list_start = dir.pos();
        let remaining = directory_view.as_slice().len() - block_list_start;
        
        if remaining > 0 {
            let preview_len = core::cmp::min(32, remaining);
        }

        let mut page_offset = 0;
        for i in 0..stream_number {
            let size = stream_sizes[i as usize];
            if size != u32::MAX {
                let pages = self.header.pages_needed_to_store(size as usize);
                page_offset += pages;
            }
        }
 
        let stream_size = stream_sizes[stream_number as usize];
        if stream_size == u32::MAX {
            return Err(Error::StreamNotFound(stream_number));
        }
        let page_numbers_to_read = self.header.pages_needed_to_store(stream_size as usize);
      
        let skip_bytes = page_offset * 2;
        let _ = dir.take(skip_bytes)?;

        let mut list = PageList::new(self.header.page_size);
        for i in 0..page_numbers_to_read {
            let page = dir.parse_u16()?;
            list.push(self.header.validate_page_number(page as u32)?);
        }

        list.truncate(stream_size as usize);

        Ok(list)
    }
}

#[cfg(feature = "alloc")]
impl<'s, S: Source<'s>> MsfImpl<'s, S> for SmallMSF<'s, S> {

    #[inline]
    fn get(&mut self, stream_number: u32, limit: Option<usize>) -> Result<Stream<'s>> {

        let mut page_list = self.look_up_stream(stream_number)?;

        if let Some(limit) = limit {
            page_list.truncate(limit);
        }

        let view = self.source.view_pages(&page_list)?;
        Ok(Stream::new(view))
    }

    #[inline]
    fn kind(&self) -> MsfKind {
        MsfKind::Small
    }
    
    #[inline]
    fn stream_count(&mut self) -> Result<u32> {
        let directory_view = self.source.view_pages(&self.directory)?;
        let mut dir = ParseBuffer::from(directory_view.as_slice());
        dir.parse_u32()
    }
    
    fn has_stream(&mut self, stream_number: u32) -> Result<bool> {
        let directory_view = self.source.view_pages(&self.directory)?;
        let mut dir = ParseBuffer::from(directory_view.as_slice());

        let num_files = dir.parse_u32()?;
        if stream_number >= num_files {
            return Ok(false);
        }

        // Skip preceding file entries (each is 8 bytes: size u32 + unknown u32)
        let _ = dir.take(stream_number as usize * 8)?;

        let size = dir.parse_u32()?;
        Ok(size != u32::MAX)
    }

    #[inline]
    fn stream_size(&mut self, stream_number: u32) -> Result<Option<u32>> {
        let directory_view = self.source.view_pages(&self.directory)?;
        let mut dir = ParseBuffer::from(directory_view.as_slice());

        let stream_count = dir.parse_u32()?;
        if stream_number >= stream_count {
            return Err(Error::StreamNotFound(stream_number));
        }

        // Small MSF entries are 8 bytes: size u32 + reserved u32.
        let _ = dir.take(stream_number as usize * 8)?;

        let size = dir.parse_u32()?;
        if size == u32::MAX {
            Ok(None)
        } else {
            Ok(Some(size))
        }
    }
}