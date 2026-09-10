// Copyright 2026 PDB Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#[cfg(feature = "alloc")]
use alloc::vec::Vec;

use crate::msf::PageNumber;
use crate::source::SourceSlice;

/// A list of page numbers that will be presented as a slice of `SourceSlice`s.
///
/// `PageList` is used to track which pages in the MSF (Multi-Stream File)
/// container belong to a particular stream or data block.
#[cfg(feature = "alloc")]
#[derive(Debug)]
pub struct PageList {
    /// Size of each page in bytes (e.g., 4096)
    page_size: usize,
    /// The list of contiguous data ranges to read
    source_slices: Vec<SourceSlice>,
    /// The last page added, used to detect sequential runs
    last_page: Option<PageNumber>,
    /// Whether this list has been truncated (no more pages can be added)
    truncated: bool,
}

#[cfg(feature = "alloc")]
impl PageList {
    /// Create a new PageList for a given page size.
    pub const fn new(page_size: usize) -> Self {
        Self {
            page_size,
            source_slices: Vec::new(),
            last_page: None,
            truncated: false,
        }
    }

    /// Add a page to the PageList. If this page number is sequential with the previous page number,
    /// it will be combined into the previous `SourceSlice` for efficiency.
    pub fn push(&mut self, page: PageNumber) {
        assert!(!self.truncated);

        let is_continuous = match self.last_page {
            Some(n) => n.checked_add(1) == Some(page),
            None => false,
        };

        if is_continuous {
            // extend by one page
            debug_assert!(!self.source_slices.is_empty());
            let last_slice = self.source_slices.last_mut().unwrap();
            last_slice.size += self.page_size;
        } else {
            let slice = SourceSlice {
                offset: (self.page_size as u64) * u64::from(page),
                size: self.page_size,
            };
            self.source_slices.push(slice);
        }

        self.last_page = Some(page);
    }

    /// Extend the PageList with multiple pages.
    pub fn extend(&mut self, pages: impl IntoIterator<Item = PageNumber>) {
        for page in pages {
            self.push(page);
        }
    }

    /// Truncate the `PageList` to request only a certain number of bytes, regardless of how many
    /// pages were pushed. Truncatation is optional, but it must be last; `push()` may not be
    /// called after `truncate()`.
    pub fn truncate(&mut self, bytes: usize) {
        let mut bytes = bytes;
        let mut new_slices: Vec<SourceSlice> = Vec::new();

        for slice in self.source_slices.iter().copied() {
            let mut slice: SourceSlice = slice;
            if bytes > 0 {
                // we need something from this slice
                // restrict this slice to the number of bytes remaining
                if slice.size > bytes {
                    slice.size = bytes;
                }

                new_slices.push(slice);
                bytes -= slice.size;
            } else {
                break;
            }
        }

        self.source_slices = new_slices;
        self.truncated = true;
    }

    /// Return the total length of this PageList.
    pub fn len(&self) -> usize {
        self.source_slices.iter().fold(0, |acc, s| acc + s.size)
    }

    /// Return a slice of SourceSlices.
    pub const fn source_slices(&self) -> &[SourceSlice] {
        self.source_slices.as_slice()
    }
}

#[cfg(all(test, feature = "alloc"))]
mod tests {
    use alloc::vec;
    use crate::msf::page_list::*;

    #[test]
    fn test_push() {
        let mut list = PageList::new(4096);

        // PageList should coalesce sequential pages
        list.push(0);
        list.push(1);
        let expected = vec![SourceSlice {
            offset: 0,
            size: 8192,
        }];
        assert_eq!(list.source_slices(), expected.as_slice());
        assert_eq!(list.len(), 8192);

        // PageList should handle nonsequential runs too
        list.push(4);
        list.push(5);
        let expected = vec![
            SourceSlice {
                offset: 0,
                size: 8192,
            },
            SourceSlice {
                offset: 16384,
                size: 8192,
            },
        ];
        assert_eq!(list.source_slices(), expected.as_slice());
        assert_eq!(list.len(), 16384);

        // ...including nonsequential runs that go backwards
        list.push(2);
        let expected = vec![
            SourceSlice {
                offset: 0,
                size: 8192,
            },
            SourceSlice {
                offset: 16384,
                size: 8192,
            },
            SourceSlice {
                offset: 8192,
                size: 4096,
            },
        ];
        assert_eq!(list.source_slices(), expected.as_slice());
        assert_eq!(list.len(), 20480);

        // ...and runs that repeat themselves
        list.push(2);
        let expected = vec![
            SourceSlice {
                offset: 0,
                size: 8192,
            },
            SourceSlice {
                offset: 16384,
                size: 8192,
            },
            SourceSlice {
                offset: 8192,
                size: 4096,
            },
            SourceSlice {
                offset: 8192,
                size: 4096,
            },
        ];
        assert_eq!(list.source_slices(), expected.as_slice());
        assert_eq!(list.len(), 24576);
    }

    #[test]
    fn test_truncate() {
        let mut list = PageList::new(4096);
        list.extend([0, 1, 4, 5, 2, 2]);
        assert_eq!(list.len(), 24576);

        list.truncate(25000);
        let expected = vec![
            SourceSlice {
                offset: 0,
                size: 8192,
            },
            SourceSlice {
                offset: 16384,
                size: 8192,
            },
            SourceSlice {
                offset: 8192,
                size: 4096,
            },
            SourceSlice {
                offset: 8192,
                size: 4096,
            },
        ];
        assert_eq!(list.source_slices(), expected.as_slice());
        assert_eq!(list.len(), 24576);

        list.truncate(24000);
        let expected = vec![
            SourceSlice {
                offset: 0,
                size: 8192,
            },
            SourceSlice {
                offset: 16384,
                size: 8192,
            },
            SourceSlice {
                offset: 8192,
                size: 4096,
            },
            SourceSlice {
                offset: 8192,
                size: 3520,
            },
        ];
        assert_eq!(list.source_slices(), expected.as_slice());
        assert_eq!(list.len(), 24000);

        list.truncate(10000);
        let expected = vec![
            SourceSlice {
                offset: 0,
                size: 8192,
            },
            SourceSlice {
                offset: 16384,
                size: 1808,
            },
        ];
        assert_eq!(list.source_slices(), expected.as_slice());
        assert_eq!(list.len(), 10000);

        // and again, it shouldn't do anything if we re-truncate to a larger size
        list.truncate(12000);
        let expected = vec![
            SourceSlice {
                offset: 0,
                size: 8192,
            },
            SourceSlice {
                offset: 16384,
                size: 1808,
            },
        ];
        assert_eq!(list.source_slices(), expected.as_slice());
        assert_eq!(list.len(), 10000);

        list.truncate(0);
        assert_eq!(list.source_slices().len(), 0);
        assert_eq!(list.len(), 0);
    }

    #[test]
    #[should_panic]
    fn test_push_after_truncate() {
        // push after truncate isn't permitted
        let mut list = PageList::new(4096);
        list.push(5);
        list.truncate(2000);
        // so far so good

        // bam!
        list.push(6);
    }

    #[test]
    fn push_overflow() {
        let mut list = PageList::new(4096);
        list.push(u32::MAX);
        list.push(u32::MAX);
    }
}
