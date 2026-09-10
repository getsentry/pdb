// Copyright 2026 PDB Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use core::{fmt, ops::Deref};

#[cfg(feature = "alloc")]
use alloc::boxed::Box;

#[cfg(feature = "alloc")]
use alloc::vec::Vec;

use crate::io::{IoError, Read, Seek, SeekFrom};

#[cfg(feature = "alloc")]
use crate::msf::PageList;

/// Represents an offset + size of the source file.
///
/// The multi-stream file implementation (used by `pdb::PDB`) determines which byte ranges it needs
/// to satisfy its requests, and it describes those requests as a `&[SourceSlice]`.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct SourceSlice {
    /// Offset into the source file.
    pub offset: u64,
    /// Size of the slice.
    pub size: usize,
}

/// The `pdb` crate accesses PDB files via the `pdb::Source` trait.
///
/// This library is written with zero-copy in mind. `Source`s provide [`SourceView`]s which need not
/// outlive their parent, supporting implementations of e.g. memory mapped files.
///
/// PDB files are "multi-stream files" (MSF) under the hood. MSFs have various layers of
/// indirection, but ultimately the MSF code asks a `Source` to view a series of
/// [`{ offset, size }` records](SourceSlice), which the `Source` provides as a
/// contiguous `&[u8]`.
///
/// # Default
///
/// There is a default `Source` implementation for `std::io::Read` + `std::io::Seek` +
/// `std::fmt::Debug`, allowing a `std::fs::File` to be treated as `pdb::Source`. This
/// implementation provides views by allocating a buffer, seeking, and reading the contents into
/// that buffer.
///
/// # Alignment
///
/// The requested offsets will always be aligned to the MSF's page size, which is always a power of
/// two and is usually (but not always) 4096 bytes. The requested sizes will also be multiples of
/// the page size, except for the size of the final `SourceSlice`, which may be smaller.
///
/// PDB files are specified as always being a multiple of the page size, so `Source` implementations
/// are free to e.g. map whole pages and return a sub-slice of the requested length.
#[cfg(feature = "alloc")]
pub trait Source<'s>: fmt::Debug {

    /// Provides a contiguous view of the source file composed of the requested position(s).
    ///
    /// Note that the SourceView's as_slice() method cannot fail, so `view()` is the time to raise
    /// IO errors.
    fn view(&mut self, slices: &[SourceSlice]) -> Result<SourceView<'s>, IoError>;

    /// Provides a contiguous view of the pages in a [`PageList`].
    ///
    /// This is a convenience wrapper around [`Self::view`] that accepts a
    /// [`PageList`] instead of a raw slice of [`SourceSlice`]s, and additionally
    /// verifies that the returned view matches the expected length.
    #[inline]
    fn view_pages(&mut self, page_list: &PageList) -> crate::Result<SourceView<'s>> {

        let view = self.view(page_list.source_slices())?;

        assert_eq!(view.as_slice().len(), page_list.len());

        Ok(view)
    }
}

#[cfg(feature = "alloc")]
#[derive(Debug)]
pub struct SourceView<'s>(Box<dyn SourceViewImpl<'s>>);

#[cfg(feature = "alloc")]
impl Deref for SourceView<'_> {
    type Target = [u8];

    #[inline]
    fn deref(&self) -> &Self::Target {
        self.0.as_slice()
    }
}

#[cfg(feature = "alloc")]
impl<'s> SourceView<'s> {
    
    /// Returns a view to the raw data.
    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }
}

/// An owned, droppable, read-only view of the source file which can be referenced as a byte slice.
pub trait SourceViewImpl<'s>: fmt::Debug {
    /// Returns a view to the raw data.
    fn as_slice(&self) -> &[u8];
}

/// A read-only view backed by an owned `Vec<u8>`.
#[cfg(feature = "alloc")]
#[derive(Clone)]
pub struct ReadView(Vec<u8>);

#[cfg(feature = "alloc")]
impl ReadView {
    /// Creates a new `ReadView` with the specified capacity.
    pub fn with_capacity(len: usize) -> Self {
        Self(Vec::with_capacity(len))
    }

    /// Returns a mutable reference to the underlying buffer.
    #[inline]
    pub fn as_mut_slice(&mut self) -> &mut [u8] {
        self.0.as_mut_slice()
    }

    /// Resizes the underlying buffer to the specified length.
    #[inline]
    pub fn resize(&mut self, len: usize, value: u8) {
        self.0.resize(len, value);
    }
}

#[cfg(feature = "alloc")]
impl fmt::Debug for ReadView {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ReadView({} bytes)", self.0.len())
    }
}

#[cfg(feature = "alloc")]
impl SourceViewImpl<'_> for ReadView {
    #[inline]
    fn as_slice(&self) -> &[u8] {
        self.0.as_slice()
    }
}

#[derive(Debug, Clone)]
pub struct MemorySource<'a>(&'a [u8]);

impl<'a> MemorySource<'a> {
    pub const fn new(data: &'a [u8]) -> Self {
        Self(data)
    }
}

#[cfg(feature = "alloc")]
impl<'a> Source<'a> for MemorySource<'a> {
    
    #[inline]
    fn view(&mut self, slices: &[SourceSlice]) -> Result<SourceView<'a>, IoError> {
        let len = slices.iter().fold(0, |acc, s| acc + s.size);
        let mut view = ReadView::with_capacity(len);
        view.resize(len, 0);

        let bytes = view.as_mut_slice();
        let mut output_offset = 0;
        for slice in slices {
            let start = slice.offset as usize;
            let end = start + slice.size;

            if end > self.0.len() {
                return Err(IoError::new(
                    crate::io::ErrorKind::UnexpectedEof,
                    "source data too short",
                ));
            }

            bytes[output_offset..(output_offset + slice.size)]
                .copy_from_slice(&self.0[start..end]);
            output_offset += slice.size;
        }

        Ok(SourceView(Box::new(view)))
    }
}

#[cfg(feature = "alloc")]
impl<'s, T> Source<'s> for T
where
    T: Read + Seek + fmt::Debug + 's,
{
    fn view(&mut self, slices: &[SourceSlice]) -> Result<SourceView<'s>, IoError> {
        let len = slices.iter().fold(0, |acc, s| acc + s.size);

        let mut v = ReadView::with_capacity(len);
        v.resize(len, 0);

        {
            let bytes = v.as_mut_slice();
            let mut output_offset: usize = 0;
            for slice in slices {
                self.seek(SeekFrom::Start(slice.offset))?;
                self.read_exact(&mut bytes[output_offset..(output_offset + slice.size)])?;
                output_offset += slice.size;
            }
        }

        Ok(SourceView(Box::new(v)))
    }
}

#[cfg(all(test, feature = "alloc"))]
mod tests {
    mod read_view {
        use crate::io::{Cursor, ErrorKind};
        use crate::*;
        use alloc::boxed::Box;
        use alloc::vec;
        
        #[test]
        fn test_basic_reading() {
            let mut data = vec![0; 4096];
            data[42] = 42;

            let mut source: Box<dyn Source<'_>> = Box::new(Cursor::new(data.as_slice()));

            let source_slices = vec![SourceSlice {
                offset: 40,
                size: 4,
            }];
            let view = source
                .view(source_slices.as_slice())
                .expect("viewing must succeed");
            assert_eq!(&[0u8, 0, 42, 0], &*view);
        }

        #[test]
        fn test_discontinuous_reading() {
            let mut data = vec![0; 4096];
            data[42] = 42;
            data[88] = 88;

            let mut source: Box<dyn Source<'_>> = Box::new(Cursor::new(data.as_slice()));

            let source_slices = vec![
                SourceSlice {
                    offset: 88,
                    size: 1,
                },
                SourceSlice {
                    offset: 40,
                    size: 4,
                },
            ];
            let view = source
                .view(source_slices.as_slice())
                .expect("viewing must succeed");
            assert_eq!(&[88u8, 0, 0, 42, 0], &*view);
        }

        #[test]
        fn test_duplicate_reading() {
            let mut data = vec![0; 4096];
            data[42] = 42;
            data[88] = 88;

            let mut source: Box<dyn Source<'_>> = Box::new(Cursor::new(data.as_slice()));

            let source_slices = vec![
                SourceSlice {
                    offset: 88,
                    size: 1,
                },
                SourceSlice {
                    offset: 40,
                    size: 4,
                },
                SourceSlice {
                    offset: 88,
                    size: 1,
                },
            ];
            let view = source
                .view(source_slices.as_slice())
                .expect("viewing must succeed");
            assert_eq!(&[88u8, 0, 0, 42, 0, 88], &*view);
        }

        #[test]
        fn test_eof_reading() {
            let data = vec![0; 4096];

            let mut source: Box<dyn Source<'_>> = Box::new(Cursor::new(data.as_slice()));

            // one byte is readable, but we asked for two
            let source_slices = vec![SourceSlice {
                offset: 4095,
                size: 2,
            }];
            let r = source.view(source_slices.as_slice());
            match r {
                Ok(_) => panic!("should have failed"),
                Err(e) => {
                    assert_eq!(ErrorKind::UnexpectedEof, e.kind());
                }
            }
        }
    }
}
