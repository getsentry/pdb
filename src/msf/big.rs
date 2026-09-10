
#[cfg(feature = "alloc")]
use alloc::boxed::Box;

use super::*;

pub const MAGIC: &[u8] = b"Microsoft C/C++ MSF 7.00\r\n\x1a\x44\x53\x00\x00\x00";

/// Raw MSF header (52 bytes) as stored at page 0, offset 0.
///
/// Contains the magic identifier, page size, and directory metadata needed
/// to parse the container format.
///
/// # Field Descriptions
///
/// - `magic`: 32-byte format identifier
/// - `page_size`: Page size in bytes (must be power of two, 256-8MB)
/// - `free_page_map`: Page number of the free page map
/// - `pages_used`: Total number of pages in the file
/// - `directory_size`: Size of the stream directory in bytes
/// - `_reserved`: Reserved (should be zero)
///
/// See: <https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/PDB/msf/msf.cpp#L946>
#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct RawHeader {
    /// 32-byte magic string, e.g. `"Microsoft C/C++ MSF 7.00..."`
    magic: [u8; 32],
    /// Page size in bytes (power of two, >= 256)
    page_size: u32,
    /// Free page map page number
    free_page_map: u32,
    /// Total page count in the file
    pages_used: u32,
    /// Stream directory size in bytes
    directory_size: u32,
    /// Reserved (should be zero)
    _reserved: u32,
}

impl<'t> TryFromCtx<'t, Endian> for RawHeader {
    type Error = scroll::Error;

    fn try_from_ctx(this: &'t [u8], le: Endian) -> scroll::Result<(Self, usize)> {
        let mut offset = 0;
        let data = Self {
            magic: {
                let mut tmp = [0; 32];
                this.gread_inout_with(&mut offset, &mut tmp, le)?;
                tmp
            },
            page_size: this.gread_with(&mut offset, le)?,
            free_page_map: this.gread_with(&mut offset, le)?,
            pages_used: this.gread_with(&mut offset, le)?,
            directory_size: this.gread_with(&mut offset, le)?,
            _reserved: this.gread_with(&mut offset, le)?,
        };
        Ok((data, offset))
    }
}

/// MSF 7.00 container with source access and stream lookup.
#[cfg(feature = "alloc")]
#[derive(Debug)]
pub struct BigMSF<'s, S: Source<'s>> {
    /// Header information (page size, max page number)
    header: MSFHeader,
    /// Data source providing page access
    source: S,
    /// Stream table in various stages of discovery
    stream_table: StreamTable<'s>,
}

#[cfg(feature = "alloc")]
impl<'s, S: Source<'s>> BigMSF<'s, S> {
    
    /// Creates a new BigMSF instance by parsing the MSF header.
    ///
    /// # Overview
    ///
    /// The MSF format uses a two-level indirection to locate the stream table:
    ///
    /// 1. **Header** → Contains directory size and page size
    /// 2. **Pages after header** → List of page numbers for the stream table
    /// 3. **Stream table pages** → The actual stream directory
    ///
    /// This method parses the header and captures the first level of indirection
    /// as a `StreamTable::HeaderOnly` state.
    ///
    /// # Page Size Validation
    ///
    /// The page size must be:
    /// - A power of two (`count_ones() == 1`)
    /// - At least 256 bytes (0x100)
    /// - At most 8 MB (128 * 0x10000)
    ///
    /// # Arguments
    ///
    /// * `source` - The data source providing page access
    /// * `header_view` - The raw header bytes from page 0
    ///
    /// # Returns
    ///
    /// A `BigMSF` instance ready for stream lookup, or an error if the
    /// header is invalid or the format is unrecognized.
    pub fn new(source: S, header_view: SourceView<'_>) -> Result<BigMSF<'s, S>> {
        let mut buf = ParseBuffer::from(header_view.as_slice());
        let header: RawHeader = buf.parse()?;

        if header.magic != MAGIC {
            return Err(Error::UnrecognizedFileFormat);
        }

        if header.page_size.count_ones() != 1
            || header.page_size < 0x100
            || header.page_size > (128 * 0x10000)
        {
            return Err(Error::InvalidPageSize(header.page_size));
        }

        let header_object = MSFHeader {
            page_size: header.page_size as usize,
            maximum_valid_page_number: header.pages_used,
        };

        let size_of_stream_table_in_pages =
            header_object.pages_needed_to_store(header.directory_size as usize);

      
        let size_of_stream_table_page_list_in_pages =
            header_object.pages_needed_to_store(size_of_stream_table_in_pages * 4);

        let mut stream_table_page_list_page_list = PageList::new(header_object.page_size);
        for _ in 0..size_of_stream_table_page_list_in_pages {
            let n = buf.parse_u32()?;
            stream_table_page_list_page_list.push(header_object.validate_page_number(n)?);
        }

        stream_table_page_list_page_list.truncate(size_of_stream_table_in_pages * 4);

        Ok(BigMSF {
            header: header_object,
            source,
            stream_table: StreamTable::HeaderOnly {
                size_in_bytes: header.directory_size as usize,
                stream_table_location_location: stream_table_page_list_page_list,
            },
        })
    }

    /// Locates the stream table's page list from the header-only information.
    ///
    /// This is the second step in the three-stage discovery process:
    ///
    /// 1. `HeaderOnly` → We know where the page list *locations* are
    /// 2. `TableFound` → We've found the page list *locations*
    /// 3. `Available` → We've loaded the actual stream table
    ///
    /// # How it works
    ///
    /// The header told us where to find the list of pages that contain the
    /// stream table's page list. This method:
    ///
    /// 1. Reads the page numbers from `stream_table_location_location`
    /// 2. Parses the page numbers from those pages into a `PageList`
    /// 3. Truncates to the correct size
    /// 4. Transitions to `StreamTable::TableFound` state
    ///
    /// The resulting `PageList` points to the actual stream table data.
    fn find_stream_table(&mut self) -> Result<()> {
        let mut new_stream_table: Option<StreamTable<'_>> = None;

        if let StreamTable::HeaderOnly {
            size_in_bytes,
            ref stream_table_location_location,
        } = self.stream_table
        {
            let location_location = self.source.view_pages(stream_table_location_location)?;

            let mut page_list = PageList::new(self.header.page_size);
            let mut buf = ParseBuffer::from(location_location.as_slice());
            while !buf.is_empty() {
                let n = buf.parse_u32()?;
                page_list.push(self.header.validate_page_number(n)?);
            }

            page_list.truncate(size_in_bytes);

            new_stream_table = Some(StreamTable::TableFound {
                stream_table_location: page_list,
            });
        }

        if let Some(st) = new_stream_table {
            self.stream_table = st;
        }

        Ok(())
    }

    /// Ensures the stream table data is loaded and available.
    ///
    /// This method coordinates the full discovery process:
    ///
    /// 1. If still in `HeaderOnly` state, calls `find_stream_table()`
    /// 2. If in `TableFound` state, loads the stream table data
    /// 3. Transitions to `StreamTable::Available` state
    ///
    /// # Panics
    ///
    /// Panics if the stream table is not available after this method returns.
    /// This should never happen and indicates a bug in the implementation.
    fn make_stream_table_available(&mut self) -> Result<()> {

        if let StreamTable::HeaderOnly { .. } = self.stream_table {
            self.find_stream_table()?;
        }

        let mut new_stream_table = None;
        if let StreamTable::TableFound {
            ref stream_table_location,
        } = self.stream_table
        {
            let stream_table_view = self.source.view_pages(stream_table_location)?;
            new_stream_table = Some(StreamTable::Available { stream_table_view });
        }

        if let Some(st) = new_stream_table {
            self.stream_table = st;
        }

        assert!(matches!(self.stream_table, StreamTable::Available { .. }));

        Ok(())
    }

    /// Looks up a stream by number and returns its page list.
    ///
    /// # Stream Table Layout
    ///
    /// The stream table is structured as:
    ///
    /// ```text
    /// [stream_count] (u32)
    /// [stream 0 size] (u32)
    /// [stream 1 size] (u32)
    /// ...
    /// [stream N size] (u32)
    /// [stream 0 pages...] (u32 per page)
    /// [stream 1 pages...] (u32 per page)
    /// ...
    /// [stream N pages...] (u32 per page)
    /// ```
    ///
    /// Where `0xffffffff` indicates a stream does not exist.
    ///
    /// # How It Works
    ///
    /// 1. Ensures the stream table is available
    /// 2. Parses the stream count
    /// 3. Calculates how many pages to skip for streams before the requested one
    /// 4. Reads the requested stream's size and page numbers
    /// 5. Returns a `PageList` pointing to the stream's data
    ///
    /// # Arguments
    ///
    /// * `stream_number` - The index of the stream to look up
    ///
    /// # Returns
    ///
    /// A `PageList` containing the pages for the requested stream,
    /// or an error if the stream doesn't exist.
    fn look_up_stream(&mut self, stream_number: u32) -> Result<PageList> {

        self.make_stream_table_available()?;

        let header = self.header;

        let bytes_in_stream: u32;
        let page_list: PageList;

        if let StreamTable::Available {
            ref stream_table_view,
        } = self.stream_table
        {
            let stream_table_slice = stream_table_view.as_slice();
            let mut stream_table = ParseBuffer::from(stream_table_slice);
            let stream_count = stream_table.parse_u32()?;

            if stream_number >= stream_count {
                return Err(Error::StreamNotFound(stream_number));
            }
        
            let mut page_numbers_to_skip: usize = 0;
            for _ in 0..stream_number {
                let bytes = stream_table.parse_u32()?;
                if bytes != u32::MAX {
                    page_numbers_to_skip += header.pages_needed_to_store(bytes as usize);
                }
            }

            bytes_in_stream = stream_table.parse_u32()?;

            if bytes_in_stream == u32::MAX {
                return Err(Error::StreamNotFound(stream_number));
            }
            let pages_in_stream = header.pages_needed_to_store(bytes_in_stream as usize);

            let _ = stream_table.take((stream_count - stream_number - 1) as usize * 4)?;
            let _ = stream_table.take(page_numbers_to_skip * 4)?;

            let mut list = PageList::new(header.page_size);

            for _ in 0..pages_in_stream {
                let page_number = stream_table.parse_u32()?;
                list.push(self.header.validate_page_number(page_number)?);
            }

            list.truncate(bytes_in_stream as usize);

            page_list = list;
        } else {
            unreachable!();
        }

        Ok(page_list)
    }
}

#[cfg(feature = "alloc")]
impl<'s, S: Source<'s>> MsfImpl<'s, S> for BigMSF<'s, S> {

    #[inline]
    fn get(&mut self, stream_number: u32, limit: Option<usize>) -> Result<Stream<'s>> {
        let mut page_list = self.look_up_stream(stream_number)?;

        if let Some(limit) = limit {
            page_list.truncate(limit);
        }

        let view = self.source.view_pages(&page_list)?;

        let stream = Stream::new(view);

        Ok(stream)
    }
    
    #[inline]
    fn kind(&self) -> MsfKind {
        MsfKind::Big
    }

    #[inline]
    fn stream_count(&mut self) -> Result<u32> {
        self.make_stream_table_available()?;
    
        if let StreamTable::Available { ref stream_table_view } = self.stream_table {
            let mut stream_table = ParseBuffer::from(stream_table_view.as_slice());
            stream_table.parse_u32()
        } else {
            unreachable!()
        }
    }
    
    #[inline]
    fn has_stream(&mut self, stream_number: u32) -> Result<bool> {
        self.make_stream_table_available()?;

        if let StreamTable::Available { ref stream_table_view } = self.stream_table {
            let stream_table_slice = stream_table_view.as_slice();
            let mut stream_table = ParseBuffer::from(stream_table_slice);
            
            let stream_count = stream_table.parse_u32()?;
            if stream_number >= stream_count {
                return Ok(false);
            }

            // Skip preceding stream sizes
            let _ = stream_table.take(stream_number as usize * 4)?;
            
            let size = stream_table.parse_u32()?;
            Ok(size != u32::MAX)
        } else {
            unreachable!()
        }
    }
}

#[cfg(all(test, feature = "alloc"))]
mod tests {
    use alloc::vec::Vec;
    use alloc::vec;
    use super::*;
    use crate::source::MemorySource;

    /// Creates a complete valid MSF 7.00 file.
    fn create_test_msf(page_size: u32) -> Vec<u8> {
        let page_size = page_size as usize;
        let mut file = Vec::new();

        // Page 0: Header (52 bytes) + padding
        // directory_size = 0 means no stream table
        let header = create_test_header(page_size as u32, 2, 0);
        file.extend_from_slice(&header);
        let header_padding = page_size - header.len();
        file.extend(vec![0u8; header_padding]);

        // Page 1: Empty directory (just padding to fill the page)
        file.extend(vec![0u8; page_size]);

        file
    }

    fn create_test_header(page_size: u32, pages_used: u32, directory_size: u32) -> Vec<u8> {
        let mut header = Vec::with_capacity(52);
        header.extend_from_slice(MAGIC);
        header.extend_from_slice(&page_size.to_le_bytes());
        header.extend_from_slice(&0u32.to_le_bytes());
        header.extend_from_slice(&pages_used.to_le_bytes());
        header.extend_from_slice(&directory_size.to_le_bytes());
        header.extend_from_slice(&0u32.to_le_bytes());
        header
    }

    #[test]
    fn test_header_parsing_valid() {
        let file = create_test_msf(4096);
        let mut source = MemorySource::new(&file);
        let header_view = source.view(&[SourceSlice { offset: 0, size: 52 }]).unwrap();

        let msf = BigMSF::new(source, header_view).unwrap();

        assert_eq!(msf.header.page_size, 4096);
        assert_eq!(msf.header.maximum_valid_page_number, 2);
        assert!(matches!(msf.stream_table, StreamTable::HeaderOnly { .. }));
    }

    #[test]
    fn test_header_parsing_invalid_magic() {
        let mut file = create_test_msf(4096);
        file[0] = 0xFF; // Corrupt magic

        let mut source = MemorySource::new(&file);
        let header_view = source.view(&[SourceSlice { offset: 0, size: 52 }]).unwrap();

        let result = BigMSF::new(source, header_view);
        assert!(matches!(result, Err(Error::UnrecognizedFileFormat)));
    }

    #[test]
    fn test_invalid_page_size() {
        // Page size must be power of two
        let file = create_test_msf(3000);
        let mut source = MemorySource::new(&file);
        let header_view = source.view(&[SourceSlice { offset: 0, size: 52 }]).unwrap();

        let result = BigMSF::new(source, header_view);
        assert!(matches!(result, Err(Error::InvalidPageSize(3000))));
    }
}