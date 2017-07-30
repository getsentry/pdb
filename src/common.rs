// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use std::borrow::Cow;
use std::convert;
use std::error;
use std::fmt;
use std::io;
use std::result;

use byteorder::{ByteOrder,LittleEndian};

/// `TypeIndex` refers to a type somewhere in `PDB.type_information()`.
pub type TypeIndex = u32;

/// An error that occurred while reading or parsing the PDB.
#[derive(Debug)]
pub enum Error {
    /// The input data was not recognized as a MSF (PDB) file.
    UnrecognizedFileFormat,

    /// The MSF header specifies an invalid page size.
    InvalidPageSize(u32),

    /// MSF referred to page number out of range.
    ///
    /// This likely indicates file corruption.
    PageReferenceOutOfRange(u32),

    // The requested stream is not stored in this file.
    StreamNotFound(u32),

    /// An IO error occurred while reading from the data source.
    IoError(io::Error),

    /// Unexpectedly reached end of input.
    UnexpectedEof,

    /// This data might be understandable, but the code needed to understand it hasn't been written.
    UnimplementedFeature(&'static str),

    /// A symbol record's length value was impossibly small.
    SymbolTooShort,

    /// Support for symbols of this kind is not implemented.
    UnimplementedSymbolKind(u16),

    /// The type information header was invalid.
    InvalidTypeInformationHeader(&'static str),

    /// A type record's length value was impossibly small.
    TypeTooShort,

    /// Type not found.
    TypeNotFound(TypeIndex),

    /// Type not indexed -- the requested type (`.0`) is larger than the maximum `TypeIndex` covered
    /// by the `TypeFinder` (`.1`).
    TypeNotIndexed(TypeIndex, TypeIndex),

    /// Support for types of this kind is not implemented.
    UnimplementedTypeKind(u16),

    /// Variable-length numeric parsing encountered an unexpected prefix.
    UnexpectedNumericPrefix(u16),
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::UnrecognizedFileFormat => "The input data was not recognized as a MSF (PDB) file",
            Error::InvalidPageSize(_) => "The MSF header specifies an invalid page size",
            Error::PageReferenceOutOfRange(_) => "MSF referred to page number out of range",
            Error::StreamNotFound(_) => "The requested stream is not stored in this file",
            Error::IoError(ref e) => e.description(),
            Error::UnexpectedEof => "Unexpectedly reached end of input",
            Error::UnimplementedFeature(_) => "Unimplemented PDB feature",
            Error::SymbolTooShort => "A symbol record's length value was impossibly small",
            Error::UnimplementedSymbolKind(_) => "Support for symbols of this kind is not implemented",
            Error::InvalidTypeInformationHeader(_) => "The type information header was invalid",
            Error::TypeTooShort => "A type record's length value was impossibly small",
            Error::TypeNotFound(_) => "Type not found",
            Error::TypeNotIndexed(_, _) => "Type not indexed",
            Error::UnimplementedTypeKind(_) => "Support for types of this kind is not implemented",
            Error::UnexpectedNumericPrefix(_) => "Variable-length numeric parsing encountered an unexpected prefix",
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> ::std::result::Result<(), fmt::Error> {
        match *self {
            Error::PageReferenceOutOfRange(p) => write!(f, "MSF referred to page number ({}) out of range", p),
            Error::InvalidPageSize(n) => write!(f, "The MSF header specifies an invalid page size ({} bytes)", n),
            Error::StreamNotFound(s) => write!(f, "The requested stream ({}) is not stored in this file", s),
            Error::IoError(ref e) => write!(f, "IO error while reading PDB: {}", e),
            Error::UnimplementedFeature(feature) => write!(f, "Unimplemented PDB feature: {}", feature),
            Error::UnimplementedSymbolKind(kind) => write!(f, "Support for symbols of kind 0x{:04x} is not implemented", kind),
            Error::InvalidTypeInformationHeader(reason) => write!(f, "The type information header was invalid: {}", reason),
            Error::TypeNotFound(type_index) => write!(f, "Type {} not found", type_index),
            Error::TypeNotIndexed(type_index, indexed_count) => write!(f, "Type {} not indexed (index covers {})", type_index, indexed_count),
            Error::UnimplementedTypeKind(kind) => write!(f, "Support for types of kind 0x{:04x} is not implemented", kind),
            Error::UnexpectedNumericPrefix(prefix) => write!(f, "Variable-length numeric parsing encountered an unexpected prefix (0x{:04x}", prefix),
            _ => fmt::Debug::fmt(self, f)
        }
    }
}

impl convert::From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::IoError(e)
    }
}

pub type Result<T> = result::Result<T, Error>;

/// Provides little-endian access to a &[u8].
#[doc(hidden)]
#[derive(Debug,Clone)]
pub struct ParseBuffer<'b> (&'b [u8], usize);

impl<'b> ParseBuffer<'b> {
    /// Return the remaining length of the buffer.
    #[doc(hidden)]
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len() - self.1
    }

    /// Return the position within the parent slice.
    #[doc(hidden)]
    #[inline]
    pub fn pos(&self) -> usize {
        self.1
    }

    /// Parse a `u8` from the input.
    #[doc(hidden)]
    #[inline]
    pub fn parse_u8(&mut self) -> Result<u8> {
        let input = &self.0[self.1..];
        if input.len() < 1 {
            Err(Error::UnexpectedEof)
        } else {
            self.1 += 1;
            Ok(input[0])
        }
    }

    /// Peek at the next u8 without advancing the cursor.
    #[doc(hidden)]
    #[inline]
    pub fn peek_u8(&mut self) -> Result<u8> {
        let input = &self.0[self.1..];
        if input.len() < 1 {
            Err(Error::UnexpectedEof)
        } else {
            Ok(input[0])
        }
    }

    /// Parse a `u16` from the input.
    #[doc(hidden)]
    #[inline]
    pub fn parse_u16(&mut self) -> Result<u16> {
        let input = &self.0[self.1..];
        if input.len() < 2 {
            Err(Error::UnexpectedEof)
        } else {
            self.1 += 2;
            Ok(LittleEndian::read_u16(input))
        }
    }

    /// Parse an `i16` from the input.
    #[doc(hidden)]
    #[inline]
    pub fn parse_i16(&mut self) -> Result<i16> {
        let input = &self.0[self.1..];
        if input.len() < 2 {
            Err(Error::UnexpectedEof)
        } else {
            self.1 += 2;
            Ok(LittleEndian::read_i16(input))
        }
    }

    /// Peek at the next u16 without advancing the cursor.
    #[doc(hidden)]
    #[inline]
    pub fn peek_u16(&mut self) -> Result<u16> {
        let input = &self.0[self.1..];
        if input.len() < 2 {
            Err(Error::UnexpectedEof)
        } else {
            Ok(LittleEndian::read_u16(input))
        }
    }

    /// Parse a `u32` from the input.
    #[doc(hidden)]
    #[inline]
    pub fn parse_u32(&mut self) -> Result<u32>
    {
        let input = &self.0[self.1..];
        if input.len() < 4 {
            Err(Error::UnexpectedEof)
        } else {
            self.1 += 4;
            Ok(LittleEndian::read_u32(input))
        }
    }

    /// Parse an `i32` from the input.
    #[doc(hidden)]
    #[inline]
    pub fn parse_i32(&mut self) -> Result<i32>
    {
        let input = &self.0[self.1..];
        if input.len() < 4 {
            Err(Error::UnexpectedEof)
        } else {
            self.1 += 4;
            Ok(LittleEndian::read_i32(input))
        }
    }

    /// Parse a NUL-terminated string from the input.
    #[doc(hidden)]
    #[inline]
    pub fn parse_cstring(&mut self) -> Result<RawString<'b>> {
        let input = &self.0[self.1..];
        let null_idx = input.iter().position(|ch| *ch == 0);

        if let Some(idx) = null_idx {
            self.1 += idx + 1;
            Ok(RawString::from(&input[..idx]))
        } else {
            Err(Error::UnexpectedEof)
        }
    }

    /// Parse a `u64` from the input.
    #[doc(hidden)]
    #[inline]
    pub fn parse_u64(&mut self) -> Result<u64>
    {
        let input = &self.0[self.1..];
        if input.len() < 8 {
            Err(Error::UnexpectedEof)
        } else {
            self.1 += 8;
            Ok(LittleEndian::read_u64(input))
        }
    }

    /// Parse an `i64` from the input.
    #[doc(hidden)]
    #[inline]
    pub fn parse_i64(&mut self) -> Result<i64>
    {
        let input = &self.0[self.1..];
        if input.len() < 8 {
            Err(Error::UnexpectedEof)
        } else {
            self.1 += 8;
            Ok(LittleEndian::read_i64(input))
        }
    }

    /// Parse a u8-length-prefixed string from the input.
    #[doc(hidden)]
    #[inline]
    pub fn parse_u8_pascal_string(&mut self) -> Result<RawString<'b>> {
        let length = self.parse_u8()? as usize;
        let input = &self.0[self.1..];
        if input.len() >= length {
            self.1 += length;
            Ok(RawString::from(&input[..length]))
        } else {
            Err(Error::UnexpectedEof)
        }
    }

    /// Take n bytes from the input
    #[doc(hidden)]
    #[inline]
    pub fn take(&mut self, n: usize) -> Result<&'b [u8]> {
        let input = &self.0[self.1..];
        if input.len() >= n {
            self.1 += n;
            Ok(&input[..n])
        } else {
            Err(Error::UnexpectedEof)
        }
    }
}

impl<'b> From<&'b [u8]> for ParseBuffer<'b> {
    fn from(buf: &'b [u8]) -> Self {
        ParseBuffer(buf, 0)
    }
}

impl<'b> fmt::LowerHex for ParseBuffer<'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> result::Result<(), fmt::Error> {
        write!(f, "ParseBuf::from(\"")?;
        for byte in self.0 {
            write!(f, "\\x{:02x}", byte)?;
        }
        write!(f, "\").as_bytes() at offset {}", self.1)
    }
}

/// `RawString` refers to a `&[u8]` that physically resides somewhere inside a PDB data structure.
///
/// A `RawString` may not be valid UTF-8.
#[derive(Clone,PartialEq,Eq,Hash,PartialOrd,Ord)]
pub struct RawString<'b>(&'b [u8]);

impl<'b> fmt::Debug for RawString<'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "RawString::from({:?})", self.to_string())
    }
}

impl<'b> fmt::Display for RawString<'b> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.to_string())
    }
}

impl<'b> RawString<'b> {
    /// Return the raw bytes of this string, as found in the PDB file.
    #[inline]
    pub fn as_bytes(&self) -> &'b [u8] {
        self.0
    }

    /// Return the length of this string in bytes.
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns a boolean indicating if this string is empty.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0.len() == 0
    }

    /// Returns a UTF-8 `String`, substituting in replacement characters as needed.
    ///
    /// This uses [`String::from_utf8_lossy()`](https://doc.rust-lang.org/std/string/struct.String.html#method.from_utf8_lossy)
    /// and thus avoids copying in cases where the original string was valid UTF-8. This is the
    /// expected case for strings that appear in PDB files, since they are almost always composed of
    /// printable 7-bit ASCII characters.
    #[inline]
    pub fn to_string(&self) -> Cow<'b, str> {
        String::from_utf8_lossy(self.0)
    }
}

impl<'b> From<RawString<'b>> for &'b [u8] {
    fn from(str: RawString<'b>) -> Self {
        str.as_bytes()
    }
}

impl<'b> From<&'b str> for RawString<'b> {
    fn from(buf: &'b str) -> Self {
        RawString(buf.as_bytes())
    }
}

impl<'b> From<&'b [u8]> for RawString<'b> {
    fn from(buf: &'b [u8]) -> Self {
        RawString(buf)
    }
}

#[cfg(test)]
mod tests {
    mod parse_buffer {
        use common::*;

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
                _ => panic!("expected EOF")
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
                _ => panic!("expected EOF")
            }

            buf.take(1).unwrap();
            match buf.parse_u16() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF")
            }
        }

        #[test]
        fn test_parse_u32() {
            let vec: Vec<u8> = vec![1, 2, 3, 4, 5, 6, 7];
            let mut buf = ParseBuffer::from(vec.as_slice());

            let val = buf.parse_u32().unwrap();
            assert_eq!(buf.len(), 3);
            assert_eq!(buf.pos(), 4);
            assert_eq!(val, 0x04030201);

            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF")
            }

            buf.take(1).unwrap();
            assert_eq!(buf.pos(), 5);
            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF")
            }

            buf.take(1).unwrap();
            assert_eq!(buf.pos(), 6);
            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF")
            }

            buf.take(1).unwrap();
            assert_eq!(buf.pos(), 7);
            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF")
            }
        }

        #[test]
        fn test_parse_u64() {
            let vec: Vec<u8> = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15];
            let mut buf = ParseBuffer::from(vec.as_slice());

            let val = buf.parse_u64().unwrap();
            assert_eq!(val, 0x0807060504030201);

            match buf.parse_u64() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF")
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
                _ => panic!("expected EOF")
            }

            buf.take(1).unwrap();
            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF")
            }

            buf.take(1).unwrap();
            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF")
            }

            buf.take(1).unwrap();
            match buf.parse_u32() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF")
            }
        }

        #[test]
        fn test_parse_cstring() {
            let mut buf = ParseBuffer::from("hello\x00world\x00\x00\x01".as_bytes());

            let val = buf.parse_cstring().unwrap();
            assert_eq!(buf.len(), 8);
            assert_eq!(buf.pos(), 6);
            assert_eq!(val, RawString::from("hello".as_bytes()));

            let val = buf.parse_cstring().unwrap();
            assert_eq!(buf.len(), 2);
            assert_eq!(buf.pos(), 12);
            assert_eq!(val, RawString::from("world".as_bytes()));

            let val = buf.parse_cstring().unwrap();
            assert_eq!(buf.len(), 1);
            assert_eq!(buf.pos(), 13);
            assert_eq!(val, RawString::from("".as_bytes()));

            match buf.parse_cstring() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF")
            }
        }

        #[test]
        fn test_parse_u8_pascal_string() {
            let mut buf = ParseBuffer::from("\x05hello\x05world\x00\x01".as_bytes());

            let val = buf.parse_u8_pascal_string().unwrap();
            assert_eq!(buf.len(), 8);
            assert_eq!(buf.pos(), 6);
            assert_eq!(val, RawString::from("hello".as_bytes()));

            let val = buf.parse_u8_pascal_string().unwrap();
            assert_eq!(buf.len(), 2);
            assert_eq!(buf.pos(), 12);
            assert_eq!(val, RawString::from("world".as_bytes()));

            let val = buf.parse_u8_pascal_string().unwrap();
            assert_eq!(buf.len(), 1);
            assert_eq!(buf.pos(), 13);
            assert_eq!(val, RawString::from("".as_bytes()));

            match buf.parse_u8_pascal_string() {
                Err(Error::UnexpectedEof) => (),
                _ => panic!("expected EOF")
            }
        }
    }
}
