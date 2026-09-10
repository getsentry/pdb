// Copyright 2026 PDB Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use core::mem;
use core::slice;

mod types;
mod parse_buffer;
mod error;

pub use types::*;
pub use parse_buffer::*;
pub use error::*;

/// Cast a binary slice to a slice of types.
///
/// This function performs a cast of a binary slice to a slice of some type, returning `Some` if the
/// following two conditions are met:
///
///  1. The size of the slize must be a multiple of the type's size.
///  2. The slice must be aligned to the alignment of the type.
///
/// Note that this function will not convert any endianness. The types must be capable of reading
/// endianness correclty in case data from other hosts is read.
pub(crate) fn cast_aligned<T>(data: &[u8]) -> Option<&[T]> {
    let alignment = mem::align_of::<T>();
    let size = mem::size_of::<T>();

    let ptr = data.as_ptr();
    let bytes = data.len();

    match (bytes % size, ptr.align_offset(alignment)) {
        (0, 0) => Some(unsafe { slice::from_raw_parts(ptr as *const T, bytes / size) }),
        (_, _) => None,
    }
}

#[cfg(all(test, feature = "alloc"))]
mod tests {
    mod parse_buffer {
        use alloc::vec;
        use alloc::vec::Vec;
        use crate::common::*;

        #[test]
        fn test_parse_u8() {
            let vec: Vec<u8> = vec![1, 2, 3, 4];
            let mut buf = ParseBuffer::from(vec.as_slice());
            assert_eq!(buf.pos(), 0);

            assert_eq!(buf.peek_u8().expect("peek"), 1);
            assert_eq!(buf.peek_u8().expect("peek"), 1);
            assert_eq!(buf.peek_u8().expect("peek"), 1);
            let val = buf.parse_u8().unwrap();
            assert_eq!(buf.len(), 3);
            assert_eq!(buf.pos(), 1);
            assert_eq!(val, 1);

            assert_eq!(buf.peek_u8().expect("peek"), 2);
            let val = buf.parse_u8().unwrap();
            assert_eq!(buf.len(), 2);
            assert_eq!(buf.pos(), 2);
            assert_eq!(val, 2);

            assert_eq!(buf.peek_u8().expect("peek"), 3);
            let val = buf.parse_u8().unwrap();
            assert_eq!(buf.len(), 1);
            assert_eq!(buf.pos(), 3);
            assert_eq!(val, 3);

            assert_eq!(buf.peek_u8().expect("peek"), 4);
            let val = buf.parse_u8().unwrap();
            assert_eq!(buf.len(), 0);
            assert_eq!(buf.pos(), 4);
            assert_eq!(val, 4);

            match buf.parse_u8() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF"),
            }
        }

        #[test]
        fn test_parse_u16() {
            let vec: Vec<u8> = vec![1, 2, 3];
            let mut buf = ParseBuffer::from(vec.as_slice());

            assert_eq!(buf.peek_u16().expect("peek"), 0x0201);
            assert_eq!(buf.peek_u16().expect("peek"), 0x0201);

            let val = buf.parse_u16().unwrap();
            assert_eq!(buf.len(), 1);
            assert_eq!(buf.pos(), 2);
            assert_eq!(val, 0x0201);

            match buf.parse_u16() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF"),
            }

            buf.take(1).unwrap();
            match buf.parse_u16() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF"),
            }
        }

        #[test]
        fn test_parse_u32() {
            let vec: Vec<u8> = vec![1, 2, 3, 4, 5, 6, 7];
            let mut buf = ParseBuffer::from(vec.as_slice());

            let val = buf.parse_u32().unwrap();
            assert_eq!(buf.len(), 3);
            assert_eq!(buf.pos(), 4);
            assert_eq!(val, 0x0403_0201);

            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF"),
            }

            buf.take(1).unwrap();
            assert_eq!(buf.pos(), 5);
            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF"),
            }

            buf.take(1).unwrap();
            assert_eq!(buf.pos(), 6);
            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF"),
            }

            buf.take(1).unwrap();
            assert_eq!(buf.pos(), 7);
            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF"),
            }
        }

        #[test]
        fn test_parse_u64() {
            let vec: Vec<u8> = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
            let mut buf = ParseBuffer::from(vec.as_slice());

            let val = buf.parse_u64().unwrap();
            assert_eq!(val, 0x0807_0605_0403_0201);

            match buf.parse_u64() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF"),
            }
        }

        #[test]
        fn test_parse_i32() {
            let vec: Vec<u8> = vec![254, 255, 255, 255, 5, 6, 7];
            let mut buf = ParseBuffer::from(vec.as_slice());

            let val = buf.parse_i32().unwrap();
            assert_eq!(buf.len(), 3);
            assert_eq!(val, -2);
            assert_eq!(buf.pos(), 4);

            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF"),
            }

            buf.take(1).unwrap();
            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF"),
            }

            buf.take(1).unwrap();
            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF"),
            }

            buf.take(1).unwrap();
            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF"),
            }
        }

        #[test]
        fn test_parse_cstring() {
            let mut buf = ParseBuffer::from(&b"hello\x00world\x00\x00\x01"[..]);

            let val = buf.parse_cstring().unwrap();
            assert_eq!(buf.len(), 8);
            assert_eq!(buf.pos(), 6);
            assert_eq!(val, RawString::from(&b"hello"[..]));

            let val = buf.parse_cstring().unwrap();
            assert_eq!(buf.len(), 2);
            assert_eq!(buf.pos(), 12);
            assert_eq!(val, RawString::from(&b"world"[..]));

            let val = buf.parse_cstring().unwrap();
            assert_eq!(buf.len(), 1);
            assert_eq!(buf.pos(), 13);
            assert_eq!(val, RawString::from(&b""[..]));

            match buf.parse_cstring() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF"),
            }
        }

        #[test]
        fn test_parse_u8_pascal_string() {
            let mut buf = ParseBuffer::from(&b"\x05hello\x05world\x00\x01"[..]);

            let val = buf.parse_u8_pascal_string().unwrap();
            assert_eq!(buf.len(), 8);
            assert_eq!(buf.pos(), 6);
            assert_eq!(val, RawString::from(&b"hello"[..]));

            let val = buf.parse_u8_pascal_string().unwrap();
            assert_eq!(buf.len(), 2);
            assert_eq!(buf.pos(), 12);
            assert_eq!(val, RawString::from(&b"world"[..]));

            let val = buf.parse_u8_pascal_string().unwrap();
            assert_eq!(buf.len(), 1);
            assert_eq!(buf.pos(), 13);
            assert_eq!(val, RawString::from(&b""[..]));

            match buf.parse_u8_pascal_string() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF"),
            }
        }

        #[test]
        fn test_parse_buffer_align() {
            let mut buf = ParseBuffer::from(&b"1234"[..]);
            buf.take(1).unwrap();
            assert!(buf.align(4).is_ok());
            assert_eq!(buf.pos(), 4);
            assert_eq!(buf.len(), 0);

            let mut buf = ParseBuffer::from(&b"1234"[..]);
            buf.take(3).unwrap();
            assert!(buf.align(4).is_ok());
            assert_eq!(buf.pos(), 4);
            assert_eq!(buf.len(), 0);

            let mut buf = ParseBuffer::from(&b"12345"[..]);
            buf.take(3).unwrap();
            assert!(buf.align(4).is_ok());
            assert_eq!(buf.pos(), 4);
            assert_eq!(buf.len(), 1);

            let mut buf = ParseBuffer::from(&b"123"[..]);
            buf.take(3).unwrap();
            assert!(buf.align(4).is_err());
        }

        #[test]
        fn test_seek() {
            let mut buf = ParseBuffer::from(&b"hello"[..]);
            buf.seek(5);
            assert_eq!(buf.pos(), 5);
            buf.seek(2);
            assert_eq!(buf.pos(), 2);
            buf.seek(10);
            assert_eq!(buf.pos(), 5);
        }
    }

    mod newtypes {
        use alloc::format;
        use crate::common::*;

        // These tests use SymbolIndex as a proxy for all other types.

        #[test]
        fn test_format_newtype() {
            let val = SymbolIndex(0x42);
            assert_eq!(format!("{}", val), "0x42");
        }

        #[test]
        fn test_debug_newtype() {
            let val = SymbolIndex(0x42);
            assert_eq!(format!("{:?}", val), "SymbolIndex(0x42)");
        }

        #[test]
        fn test_pread() {
            let mut buf = ParseBuffer::from(&[0x42, 0, 0, 0][..]);
            let val = buf.parse::<SymbolIndex>().expect("parse");
            assert_eq!(val, SymbolIndex(0x42));
            assert!(buf.is_empty());
        }
    }

    mod cast_aligned {
        use crate::common::cast_aligned;
        use core::slice;

        #[test]
        fn test_cast_aligned() {
            let data: &[u32] = &[1, 2, 3];

            let ptr = data.as_ptr() as *const u8;
            let bin: &[u8] = unsafe { slice::from_raw_parts(ptr, 12) };

            assert_eq!(cast_aligned(bin), Some(data));
        }

        #[test]
        fn test_cast_empty() {
            let data: &[u32] = &[];

            let ptr = data.as_ptr() as *const u8;
            let bin: &[u8] = unsafe { slice::from_raw_parts(ptr, 0) };

            assert_eq!(cast_aligned(bin), Some(data));
        }

        #[test]
        fn test_cast_unaligned() {
            let data: &[u32] = &[1, 2, 3];

            let ptr = data.as_ptr() as *const u8;
            let bin: &[u8] = unsafe { slice::from_raw_parts(ptr.offset(2), 8) };

            assert_eq!(cast_aligned::<u32>(bin), None);
        }

        #[test]
        fn test_cast_wrong_size() {
            let data: &[u32] = &[1, 2, 3];

            let ptr = data.as_ptr() as *const u8;
            let bin: &[u8] = unsafe { slice::from_raw_parts(ptr, 11) };

            assert_eq!(cast_aligned::<u32>(bin), None);
        }
    }
}
