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

use scroll::{self, Endian, Pread, LE};
use scroll::ctx::TryFromCtx;

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

    StreamNameNotFound,

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

    /// Required mapping for virtual addresses (OMAP) was not found.
    AddressMapNotFound,

    /// A parse error from scroll.
    ScrollError(scroll::Error),
}

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::UnrecognizedFileFormat => "The input data was not recognized as a MSF (PDB) file",
            Error::InvalidPageSize(_) => "The MSF header specifies an invalid page size",
            Error::PageReferenceOutOfRange(_) => "MSF referred to page number out of range",
            Error::StreamNotFound(_) => "The requested stream is not stored in this file",
            Error::StreamNameNotFound => "The requested stream is not stored in this file",
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
            Error::AddressMapNotFound => "Required mapping for virtual addresses (OMAP) was not found",
            Error::ScrollError(ref e) => e.description(),
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
            Error::AddressMapNotFound => write!(f, "Required mapping for virtual addresses (OMAP) was not found"),
            _ => fmt::Debug::fmt(self, f)
        }
    }
}

impl convert::From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::IoError(e)
    }
}

impl convert::From<scroll::Error> for Error {
    fn from(e: scroll::Error) -> Self {
        match e {
            // Convert a couple of scroll errors into EOF.
            scroll::Error::BadOffset(_) | scroll::Error::TooBig { .. } => Error::UnexpectedEof,
            _ => Error::ScrollError(e),
        }
    }
}

pub type Result<T> = result::Result<T, Error>;

/// A Relative Virtual Address as it appears in a PE file.
///
/// RVAs are always relative to the image base address, as it is loaded into process memory. This
/// address is reported by debuggers in stack traces and may refer to symbols or instruction
/// pointers.
#[derive(Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Rva(pub u32);

impl From<u32> for Rva {
    fn from(addr: u32) -> Self {
        Rva(addr)
    }
}

impl From<Rva> for u32 {
    fn from(addr: Rva) -> Self {
        addr.0
    }
}

impl fmt::Display for Rva {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#08x}", self.0)
    }
}

impl fmt::Debug for Rva {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Rva({})", self)
    }
}

/// An offset relative to a PE section.
///
/// This offset can be converted to an `Rva` to receive the address relative to the entire image.
/// Note that this offset applies to the actual PE headers. The PDB debug information actually
/// stores [`PdbInternalSectionOffsets`].
///
/// [`PdbInternalSectionOffsets`]: struct.PdbInternalSectionOffset.html
#[derive(Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SectionOffset {
    /// The memory offset relative from the start of the section's memory.
    pub offset: u32,

    /// The index of the section in the PE's section headers list, incremented by `1`. A value of
    /// `0` indicates an invalid or missing reference.
    pub section: u16,
}

impl SectionOffset {
    pub fn new(section: u16, offset: u32) -> Self {
        SectionOffset { offset, section }
    }
}

impl fmt::Debug for SectionOffset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("SectionOffset")
            .field("section", &format!("{:#x}", self.section))
            .field("offset", &format!("{:#08x}", self.offset))
            .finish()
    }
}

/// A Relative Virtual Address in an unoptimized PE file.
///
/// This instance can be converted into an actual [`Rva`] using [`rva`].
///
/// [`Rva`]: struct.Rva.html
/// [`rva`]: struct.PdbInternalRva.html#method.rva
#[derive(Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct PdbInternalRva(pub u32);

impl From<u32> for PdbInternalRva {
    fn from(addr: u32) -> Self {
        PdbInternalRva(addr)
    }
}

impl From<PdbInternalRva> for u32 {
    fn from(addr: PdbInternalRva) -> Self {
        addr.0
    }
}

impl fmt::Display for PdbInternalRva {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#08x}", self.0)
    }
}

impl fmt::Debug for PdbInternalRva {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "PdbInternalRva({})", self)
    }
}

/// An offset relative to a PE section in the original unoptimized binary.
///
/// For optimized Microsoft binaries, this offset points to a virtual address space before the
/// rearrangement of sections has been performed. This kind of offset is usually stored in PDB debug
/// information. It can be converted to an RVA in the transformed address space of the optimized
/// binary using [`rva`]. Likewise, there is a conversion to [`SectionOffset`] in the actual address
/// space.
///
/// For binaries and their PDBs that have not been optimized, both address spaces are equal and the
/// offsets are interchangeable. The conversion operations are cheap no-ops in this case.
///
/// [`rva`]: struct.PdbInternalSectionOffset.html#method.rva
/// [`SectionOffset`]: struct.SectionOffset.html
#[derive(Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct PdbInternalSectionOffset {
    pub offset: u32,
    pub section: u16,
}

impl PdbInternalSectionOffset {
    pub fn new(section: u16, offset: u32) -> Self {
        PdbInternalSectionOffset { offset, section }
    }
}

impl fmt::Debug for PdbInternalSectionOffset {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.debug_struct("PdbInternalSectionOffset")
            .field("section", &format!("{:#x}", self.section))
            .field("offset", &format!("{:#08x}", self.offset))
            .finish()
    }
}

/// Provides little-endian access to a &[u8].
#[doc(hidden)]
#[derive(Debug,Clone)]
pub struct ParseBuffer<'b> (&'b [u8], usize);

macro_rules! def_parse {
    ( $( ($n:ident, $t:ty) ),* $(,)* ) => {
        $(#[doc(hidden)]
          #[inline]
          pub fn $n(&mut self) -> Result<$t> {
              Ok(self.parse()?)
          })*
    }
}

macro_rules! def_peek {
    ( $( ($n:ident, $t:ty) ),* $(,)* ) => {
        $(#[doc(hidden)]
          #[inline]
          pub fn $n(&mut self) -> Result<$t> {
              Ok(self.0.pread_with(self.1, LE)?)
          })*
    }
}

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

    /// Align the current position to the next multiple of `alignment` bytes.
    #[doc(hidden)]
    #[inline]
    pub fn align(&mut self, alignment: usize) -> Result<()> {
        let diff = self.1 % alignment;
        if diff > 0 {
            if self.len() < diff {
                return Err(Error::UnexpectedEof);
            }
            self.1 += alignment - diff;
        }
        Ok(())
    }

    pub fn parse<T>(&mut self) -> Result<T>
        where T: TryFromCtx<'b, Endian, [u8], Error=scroll::Error, Size=usize>,
    {
        Ok(self.0.gread_with(&mut self.1, LE)?)
    }

    def_parse!(
        (parse_u8, u8),
        (parse_u16, u16),
        (parse_i16, i16),
        (parse_u32, u32),
        (parse_i32, i32),
        (parse_u64, u64),
        (parse_i64, i64),
    );

    def_peek!(
        (peek_u8, u8),
        (peek_u16, u16),
    );

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

    /// Parse a u8-length-prefixed string from the input.
    #[doc(hidden)]
    #[inline]
    pub fn parse_u8_pascal_string(&mut self) -> Result<RawString<'b>> {
        let length = self.parse_u8()? as usize;
        Ok(RawString::from(self.take(length)?))
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

    pub fn parse_variant(&mut self) -> Result<Variant> {
        let leaf = self.parse_u16()?;
        if leaf < ::tpi::constants::LF_NUMERIC {
            // the u16 directly encodes a value
            return Ok(Variant::U16(leaf));
        }

        match leaf {
            ::tpi::constants::LF_CHAR =>      { Ok(Variant::U8 (self.parse_u8()? )) },
            ::tpi::constants::LF_SHORT =>     { Ok(Variant::I16(self.parse_i16()?)) },
            ::tpi::constants::LF_LONG =>      { Ok(Variant::I32(self.parse_i32()?)) },
            ::tpi::constants::LF_QUADWORD =>  { Ok(Variant::I64(self.parse_i64()?)) },
            ::tpi::constants::LF_USHORT =>    { Ok(Variant::U16(self.parse_u16()?)) },
            ::tpi::constants::LF_ULONG =>     { Ok(Variant::U32(self.parse_u32()?)) },
            ::tpi::constants::LF_UQUADWORD => { Ok(Variant::U64(self.parse_u64()?)) },
            _ => {
                debug_assert!(false);
                Err(Error::UnexpectedNumericPrefix(leaf))
            }
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

#[derive(Debug,Copy,Clone,PartialEq,Eq)]
pub enum Variant {
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
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
