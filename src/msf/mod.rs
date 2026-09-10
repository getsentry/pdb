// Copyright 2026 PDB Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#[cfg(feature = "alloc")]
use alloc::boxed::Box;

use scroll::{ctx::TryFromCtx, Endian, Pread};

use crate::common::*;
use crate::io::ErrorKind;
use crate::source::*;

mod types;
pub mod big;
pub mod small;
mod page_list;

pub use types::*;

#[cfg(feature = "alloc")]
pub use page_list::PageList;

pub fn header_matches(actual: &[u8], expected: &[u8]) -> bool {
    actual.len() >= expected.len() && &actual[0..expected.len()] == expected
}

#[cfg(feature = "alloc")]
#[derive(Debug)]
pub struct Msf<'s, S>(Box<dyn MsfImpl<'s, S> + 's>);

#[cfg(feature = "alloc")]
impl<'s, S: Source<'s> + 's> Msf<'s, S> {

    /// Opens an MSF (Multi-Stream File) container from a source.
    ///
    /// This function detects the MSF format version by reading the header and
    /// returns the appropriate implementation (`BigMSF` or `SmallMSF`).
    pub fn open(mut source: S) -> Result<Self> {

        // We don't yet know the page size, so we can't use a PageList here.
        // Read a fixed probe large enough for both magics and both fixed
        // headers. 512 is the minimum valid Small MSF page size, so anything
        // shorter than this isn't a valid MSF file regardless of format.
        const PROBE_LEN: usize = 512;

        let header_view = match source.view(&[SourceSlice { offset: 0, size: PROBE_LEN }]) {
            Ok(view) => view,
            Err(err) => {
                if err.kind() == ErrorKind::UnexpectedEof {
                    return Err(Error::UnrecognizedFileFormat);
                } else {
                    return Err(Error::IoError(err));
                }
            }
        };

        if header_matches(&header_view, big::MAGIC) {
            let msf = big::BigMSF::new(source, header_view)?;
            return Ok(Self(Box::new(msf)));
        }

        if header_matches(&header_view, small::MAGIC) {

            let msf = small::SmallMSF::new(source, header_view)?;
            return Ok(Self(Box::new(msf)));
        }

        Err(Error::UnrecognizedFileFormat)
    }

    /// Returns the MSF format kind (Big or Small).
    #[inline]
    pub fn kind(&self) -> MsfKind {
        self.0.kind()
    }

    /// Returns the total number of streams in this MSF, including nil streams.
    #[inline]
    pub fn stream_count(&mut self) -> Result<u32> {
        self.0.stream_count()
    }

    /// Returns the total number of streams in this MSF, including nil streams.
    #[inline]
    pub fn stream_size(&mut self, stream_number: u32) -> Result<Option<u32>> {
        self.0.stream_size(stream_number)
    }

    /// Returns `true` if the stream exists and is not a nil stream.
    #[inline]
    pub fn has_stream(&mut self, stream_number: u32) -> Result<bool> {
        self.0.has_stream(stream_number)
    }

    /// Retrieves a stream by number, optionally limited to a byte count.
    #[inline]
    pub fn get(&mut self, stream_number: u32, limit: Option<usize>) -> Result<Stream<'s>> {
        self.0.get(stream_number, limit)
    }
}

#[cfg(all(test, feature = "alloc"))]
mod tests {
    mod header {
        use crate::common::Error;
        use crate::io::Cursor;
        use crate::msf::{MSFHeader, Msf};

        #[test]
        fn test_pages_needed_to_store() {
            let h = MSFHeader {
                page_size: 4096,
                maximum_valid_page_number: 15,
            };
            assert_eq!(h.pages_needed_to_store(0), 0);
            assert_eq!(h.pages_needed_to_store(1), 1);
            assert_eq!(h.pages_needed_to_store(1024), 1);
            assert_eq!(h.pages_needed_to_store(2048), 1);
            assert_eq!(h.pages_needed_to_store(4095), 1);
            assert_eq!(h.pages_needed_to_store(4096), 1);
            assert_eq!(h.pages_needed_to_store(4097), 2);
        }

        #[test]
        fn test_validate_page_number() {
            let h = MSFHeader {
                page_size: 4096,
                maximum_valid_page_number: 15,
            };
            assert!(matches!(
                h.validate_page_number(0),
                Err(Error::PageReferenceOutOfRange(0))
            ));
            assert!(matches!(h.validate_page_number(1), Ok(1)));
            assert!(matches!(h.validate_page_number(2), Ok(2)));
            assert!(matches!(h.validate_page_number(14), Ok(14)));
            assert!(matches!(h.validate_page_number(15), Ok(15)));
            assert!(matches!(
                h.validate_page_number(16),
                Err(Error::PageReferenceOutOfRange(16))
            ));
            assert!(matches!(
                h.validate_page_number(17),
                Err(Error::PageReferenceOutOfRange(17))
            ));
        }

        #[test]
        fn test_small_file_unrecognized_file_format() {
            let small_file = Cursor::new(b"\x7FELF");

            match Msf::open(small_file) {
                Ok(_) => panic!("4 byte file should not parse as msf"),
                Err(e) => match e {
                    Error::UnrecognizedFileFormat => (),
                    _ => panic!("4 byte file should parse as unrecognized file format"),
                },
            };
        }
    }
}
