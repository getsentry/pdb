#[cfg(feature = "alloc")]
use alloc::boxed::Box;

use crate::common::*;

#[cfg(feature = "alloc")]
use crate::msf::page_list::PageList;

use crate::source::*;

use core::fmt;
use core::ops::Deref;

pub type PageNumber = u32;

/// MSF file header information.
///
/// Contains the page size and the maximum valid page number used to validate
/// page references in the stream table.
#[derive(Debug, Copy, Clone)]
pub struct MSFHeader {
    /// Size of each page in bytes (must be power of two, >= 256)
    pub(crate) page_size: usize,
    /// The highest valid page number in this file
    pub(crate) maximum_valid_page_number: PageNumber,
}

impl MSFHeader {
    /// Calculates how many pages are needed to store `bytes` of data.
    pub const fn pages_needed_to_store(&self, bytes: usize) -> usize {
        (bytes + (self.page_size - 1)) / self.page_size
    }

    /// Validates that a page number is within the valid range.
    ///
    /// Page 0 is invalid (reserved) and page numbers must not exceed
    /// `maximum_valid_page_number`.
    pub fn validate_page_number(&self, page_number: u32) -> Result<PageNumber> {
        if page_number == 0 || page_number > self.maximum_valid_page_number {
            Err(Error::PageReferenceOutOfRange(page_number))
        } else {
            Ok(page_number as PageNumber)
        }
    }
}

/// The state of stream table discovery in an MSF file.
///
/// The stream table in an MSF (Multi-Stream File) is located through a two-level indirection:
///
/// 1. **Header** → Tells us where to find the page list that points to the stream table
/// 2. **Page List** → Contains the actual pages of the stream table
/// 3. **Stream Table** → The final data structure mapping stream numbers to their pages
///
/// This enum tracks which stage of discovery we're in, allowing lazy loading.
#[cfg(feature = "alloc")]
#[doc(hidden)]
#[derive(Debug)]
pub enum StreamTable<'s> {
     /// We only know the size and the location of the location.
    ///
    /// The header gave us:
    /// - `size_in_bytes`: How many bytes the stream table occupies
    /// - `stream_table_location_location`: A `PageList` pointing to pages that contain
    ///   the actual page numbers of the stream table
    ///
    /// This is the initial state after parsing the MSF header.
    HeaderOnly {
        /// The total size of the stream table in bytes
        size_in_bytes: usize,
        /// A page list pointing to pages that contain the stream table's page numbers
        stream_table_location_location: PageList,
    },

    /// We've located the stream table's page list.
    ///
    /// `stream_table_location` is a `PageList` that directly points to the pages
    /// containing the stream table data. The stream table can now be read.
    TableFound {
        /// The page list containing the actual stream table data
        stream_table_location: PageList,
    },

    /// The stream table data is loaded and available for reading.
    ///
    /// `stream_table_view` is a contiguous view of the stream table data,
    /// ready to be parsed.
    Available {
        /// A contiguous view of the stream table data
        stream_table_view: SourceView<'s>,
    },
}

/// A stream within an MSF (Multi-Stream File) container.
///
/// Streams are logical collections of data that span multiple pages in the MSF.
/// This type provides read-only access to the stream's raw bytes.
#[cfg(feature = "alloc")]
#[derive(Debug)]
pub struct Stream<'s>(SourceView<'s>);

#[cfg(feature = "alloc")]
impl<'s> Stream<'s> {
    /// Creates a new `Stream` from a contiguous view of the underlying source.
    pub const fn new(view: SourceView<'s>) -> Self {
        Self(view)
    }

    /// Returns a `ParseBuffer` over this stream's bytes.
    #[inline]
    pub fn parse_buffer(&self) -> ParseBuffer<'_> {
        let slice = self.0.as_slice();
        ParseBuffer::from(slice)
    }

    /// Returns this stream's bytes as a slice.
    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }
}

#[cfg(feature = "alloc")]
impl Deref for Stream<'_> {
    type Target = [u8];

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.as_slice()
    }
}

/// Provides access to a "multi-stream file", which is the container format used by PDBs.
pub trait MsfImpl<'s, S>: fmt::Debug {
    fn kind(&self) -> MsfKind;

    /// Accesses a stream by stream number, optionally restricted by a byte limit.
    #[cfg(feature = "alloc")]
    fn get(&mut self, stream_number: u32, limit: Option<usize>) -> Result<Stream<'s>>;

    /// Returns the total number of streams (including nil streams).
    fn stream_count(&mut self) -> Result<u32>;
    
    /// Returns true if the stream exists and is not nil.
    fn has_stream(&mut self, stream_number: u32) -> Result<bool>;
}

/// MSF (Multi-Stream File) format version.
///
/// The MSF container format has two major versions:
/// - **Small MSF**: Legacy format used by Visual Studio 6 and earlier
/// - **Big MSF**: Modern format used by Visual Studio 7+ (most PDB files)
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MsfKind {
    /// Legacy MSF format (version 2.00) with fixed 512-byte pages.
    ///
    /// This format is obsolete and rarely seen in practice.
    Small,

    /// Modern MSF format (version 7.00) with variable page sizes.
    ///
    /// This is the format used by all modern PDB files.
    Big,
}