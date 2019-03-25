// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use std::borrow::Cow;
use std::fmt;
use std::io;
use std::ops::Add;
use std::result;

use scroll::ctx::TryFromCtx;
use scroll::{self, Endian, Pread, LE};

use crate::tpi::constants;

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

    /// The requested stream is not stored in this file.
    StreamNotFound(u32),

    /// A stream requested by name was not found.
    StreamNameNotFound,

    /// Invalid length of a stream.
    InvalidStreamLength(&'static str),

    /// An IO error occurred while reading from the data source.
    IoError(io::Error),

    /// Unexpectedly reached end of input.
    UnexpectedEof,

    /// This data might be understandable, but the code needed to understand it hasn't been written.
    UnimplementedFeature(&'static str),

    /// The global shared symbol table is missing.
    GlobalSymbolsNotFound,

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

    /// This debug subsection kind is unknown or unimplemented.
    UnimplementedDebugSubsection(u32),

    /// This source file checksum kind is unknown or unimplemented.
    UnimplementedFileChecksumKind(u8),

    /// There is no source file checksum at the given offset.
    InvalidFileChecksumOffset(u32),

    /// The lines table is missing.
    LinesNotFound,
}

impl std::error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::UnrecognizedFileFormat => {
                "The input data was not recognized as a MSF (PDB) file"
            }
            Error::InvalidPageSize(_) => "The MSF header specifies an invalid page size",
            Error::PageReferenceOutOfRange(_) => "MSF referred to page number out of range",
            Error::StreamNotFound(_) => "The requested stream is not stored in this file",
            Error::StreamNameNotFound => "The requested stream is not stored in this file",
            Error::InvalidStreamLength(_) => "Stream has an invalid length",
            Error::IoError(ref e) => e.description(),
            Error::UnexpectedEof => "Unexpectedly reached end of input",
            Error::UnimplementedFeature(_) => "Unimplemented PDB feature",
            Error::GlobalSymbolsNotFound => "The global symbol stream is missing",
            Error::SymbolTooShort => "A symbol record's length value was impossibly small",
            Error::UnimplementedSymbolKind(_) => {
                "Support for symbols of this kind is not implemented"
            }
            Error::InvalidTypeInformationHeader(_) => "The type information header was invalid",
            Error::TypeTooShort => "A type record's length value was impossibly small",
            Error::TypeNotFound(_) => "Type not found",
            Error::TypeNotIndexed(_, _) => "Type not indexed",
            Error::UnimplementedTypeKind(_) => "Support for types of this kind is not implemented",
            Error::UnexpectedNumericPrefix(_) => {
                "Variable-length numeric parsing encountered an unexpected prefix"
            }
            Error::AddressMapNotFound => {
                "Required mapping for virtual addresses (OMAP) was not found"
            }
            Error::ScrollError(ref e) => e.description(),
            Error::UnimplementedDebugSubsection(_) => {
                "Debug module subsection of this kind is not implemented"
            }
            Error::UnimplementedFileChecksumKind(_) => "Unknown source file checksum kind",
            Error::InvalidFileChecksumOffset(_) => "Invalid source file checksum offset",
            Error::LinesNotFound => "Line information not found for a module",
        }
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> ::std::result::Result<(), fmt::Error> {
        match *self {
            Error::PageReferenceOutOfRange(p) => {
                write!(f, "MSF referred to page number ({}) out of range", p)
            }
            Error::InvalidPageSize(n) => write!(
                f,
                "The MSF header specifies an invalid page size ({} bytes)",
                n
            ),
            Error::StreamNotFound(s) => {
                write!(f, "The requested stream ({}) is not stored in this file", s)
            }
            Error::InvalidStreamLength(s) => write!(
                f,
                "{} stream has a length that is not a multiple of its records",
                s
            ),
            Error::IoError(ref e) => write!(f, "IO error while reading PDB: {}", e),
            Error::UnimplementedFeature(feature) => {
                write!(f, "Unimplemented PDB feature: {}", feature)
            }
            Error::UnimplementedSymbolKind(kind) => write!(
                f,
                "Support for symbols of kind 0x{:04x} is not implemented",
                kind
            ),
            Error::InvalidTypeInformationHeader(reason) => {
                write!(f, "The type information header was invalid: {}", reason)
            }
            Error::TypeNotFound(type_index) => write!(f, "Type {} not found", type_index),
            Error::TypeNotIndexed(type_index, indexed_count) => write!(
                f,
                "Type {} not indexed (index covers {})",
                type_index, indexed_count
            ),
            Error::UnimplementedTypeKind(kind) => write!(
                f,
                "Support for types of kind 0x{:04x} is not implemented",
                kind
            ),
            Error::UnexpectedNumericPrefix(prefix) => write!(
                f,
                "Variable-length numeric parsing encountered an unexpected prefix (0x{:04x}",
                prefix
            ),
            Error::UnimplementedDebugSubsection(kind) => write!(
                f,
                "Debug module subsection of kind 0x{:04x} is not implemented",
                kind
            ),
            Error::UnimplementedFileChecksumKind(kind) => {
                write!(f, "Unknown source file checksum kind {}", kind)
            }
            Error::InvalidFileChecksumOffset(offset) => {
                write!(f, "Invalid source file checksum offset {:#x}", offset)
            }
            _ => fmt::Debug::fmt(self, f),
        }
    }
}

impl From<io::Error> for Error {
    fn from(e: io::Error) -> Self {
        Error::IoError(e)
    }
}

impl From<scroll::Error> for Error {
    fn from(e: scroll::Error) -> Self {
        match e {
            // Convert a couple of scroll errors into EOF.
            scroll::Error::BadOffset(_) | scroll::Error::TooBig { .. } => Error::UnexpectedEof,
            _ => Error::ScrollError(e),
        }
    }
}

/// The result type returned by this crate.
pub type Result<T> = result::Result<T, Error>;

/// Helper to format hexadecimal numbers.
pub(crate) struct HexFmt<T>(pub T);

impl<T> fmt::Debug for HexFmt<T>
where
    T: fmt::LowerHex,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#x}", self.0)
    }
}

impl<T> fmt::Display for HexFmt<T>
where
    T: fmt::LowerHex,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

/// Helper to format hexadecimal numbers with fixed width.
pub(crate) struct FixedHexFmt<T>(pub T);

impl<T> fmt::Debug for FixedHexFmt<T>
where
    T: fmt::LowerHex,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let width = 2 * std::mem::size_of::<T>();
        write!(f, "{:#01$x}", self.0, width + 2)
    }
}

impl<T> fmt::Display for FixedHexFmt<T>
where
    T: fmt::LowerHex,
{
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

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

impl Add<u32> for Rva {
    type Output = Self;

    /// Adds the given offset to this `Rva`.
    fn add(mut self, offset: u32) -> Self {
        self.0 += offset;
        self
    }
}

impl fmt::Display for Rva {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        HexFmt(self.0).fmt(f)
    }
}

impl fmt::Debug for Rva {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Rva({})", self)
    }
}

impl<'a> TryFromCtx<'a, Endian> for Rva {
    type Error = scroll::Error;
    type Size = usize;

    fn try_from_ctx(this: &'a [u8], le: Endian) -> scroll::Result<(Self, Self::Size)> {
        u32::try_from_ctx(this, le).map(|(i, s)| (Rva(i), s))
    }
}

/// An offset relative to a PE section.
///
/// This offset can be converted to an `Rva` to receive the address relative to the entire image.
/// Note that this offset applies to the actual PE headers. The PDB debug information actually
/// stores [`PdbInternalSectionOffsets`].
///
/// [`PdbInternalSectionOffsets`]: struct.PdbInternalSectionOffset.html
#[derive(Clone, Copy, Default, Eq, Hash, PartialEq)]
pub struct SectionOffset {
    /// The memory offset relative from the start of the section's memory.
    pub offset: u32,

    /// The index of the section in the PE's section headers list, incremented by `1`. A value of
    /// `0` indicates an invalid or missing reference.
    pub section: u16,
}

impl SectionOffset {
    /// Creates a new PE section offset.
    pub fn new(section: u16, offset: u32) -> Self {
        SectionOffset { offset, section }
    }

    /// Returns whether this section offset points to a valid section or into the void.
    pub fn is_valid(self) -> bool {
        self.section != 0
    }
}

impl Add<u32> for SectionOffset {
    type Output = Self;

    /// Adds the given offset to this section offset.
    ///
    /// This does not check whether the offset is still valid within the given section. If the
    /// offset is out of bounds, the conversion to `Rva` will return `None`.
    fn add(mut self, offset: u32) -> Self {
        self.offset += offset;
        self
    }
}

impl PartialOrd for SectionOffset {
    /// Compares offsets if they reside in the same section.
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.section == other.section {
            Some(self.offset.cmp(&other.offset))
        } else {
            None
        }
    }
}

impl fmt::Debug for SectionOffset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SectionOffset")
            .field("section", &HexFmt(self.section))
            .field("offset", &FixedHexFmt(self.offset))
            .finish()
    }
}

/// A Relative Virtual Address in an unoptimized PE file.
///
/// An internal RVA points into the PDB internal address space and may not correspond to RVAs of the
/// executable. It can be converted into an actual [`Rva`] suitable for debugging purposes using
/// [`rva`].
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

impl Add<u32> for PdbInternalRva {
    type Output = Self;

    /// Adds the given offset to this `PdbInternalRva`.
    fn add(mut self, offset: u32) -> Self {
        self.0 += offset;
        self
    }
}

impl fmt::Display for PdbInternalRva {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        HexFmt(self.0).fmt(f)
    }
}

impl fmt::Debug for PdbInternalRva {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
#[derive(Clone, Copy, Default, Eq, Hash, PartialEq, Pread)]
pub struct PdbInternalSectionOffset {
    /// The memory offset relative from the start of the section's memory.
    pub offset: u32,

    /// The index of the section in the PDB's section headers list, incremented by `1`. A value of
    /// `0` indicates an invalid or missing reference.
    pub section: u16,
}

impl PdbInternalSectionOffset {
    /// Creates a new PDB internal section offset.
    pub fn new(section: u16, offset: u32) -> Self {
        PdbInternalSectionOffset { offset, section }
    }

    /// Returns whether this section offset points to a valid section or into the void.
    pub fn is_valid(self) -> bool {
        self.section != 0
    }
}

impl Add<u32> for PdbInternalSectionOffset {
    type Output = Self;

    /// Adds the given offset to this section offset.
    ///
    /// This does not check whether the offset is still valid within the given section. If the
    /// offset is out of bounds, the conversion to `Rva` will return `None`.
    #[inline]
    fn add(mut self, offset: u32) -> Self {
        self.offset += offset;
        self
    }
}

impl PartialOrd for PdbInternalSectionOffset {
    /// Compares offsets if they reside in the same section.
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.section == other.section {
            Some(self.offset.cmp(&other.offset))
        } else {
            None
        }
    }
}

impl fmt::Debug for PdbInternalSectionOffset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PdbInternalSectionOffset")
            .field("section", &HexFmt(self.section))
            .field("offset", &FixedHexFmt(self.offset))
            .finish()
    }
}

/// Index of a PDB stream.
///
/// This index can either refer to a stream, or indicate the absence of a stream. Check [`is_none`]
/// to see whether a stream should exist.
///
/// Use [`StreamIndex::get`] to load data for this stream.
///
/// [`is_none`]: struct.StreamIndex.html#method.is_none
/// [`StreamIndex::get`]: struct.StreamIndex.html#method.get
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct StreamIndex(pub u16);

impl StreamIndex {
    /// Creates a stream index that points to no stream.
    pub fn none() -> Self {
        StreamIndex(0xffff)
    }

    /// Determines whether this index indicates the absence of a stream.
    ///
    /// Loading a missing stream from the PDB will result in `None`. Otherwise, the stream is
    /// expected to be present in the MSF and will result in an error if loading.
    #[inline]
    pub fn is_none(self) -> bool {
        self.msf_number().is_none()
    }

    /// Returns the MSF stream number, if this stream is not a NULL stream.
    #[inline]
    pub(crate) fn msf_number(self) -> Option<u32> {
        match self.0 {
            0xffff => None,
            index => Some(u32::from(index)),
        }
    }
}

impl Default for StreamIndex {
    fn default() -> Self {
        Self::none()
    }
}

impl fmt::Display for StreamIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.msf_number() {
            Some(number) => write!(f, "{}", number),
            None => write!(f, "None"),
        }
    }
}

impl fmt::Debug for StreamIndex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "StreamIndex({})", self)
    }
}

impl<'a> TryFromCtx<'a, Endian> for StreamIndex {
    type Error = scroll::Error;
    type Size = usize;

    fn try_from_ctx(this: &'a [u8], le: Endian) -> scroll::Result<(Self, Self::Size)> {
        u16::try_from_ctx(this, le).map(|(i, s)| (StreamIndex(i), s))
    }
}

/// A reference to a string in the string table.
///
/// This type stores an offset into the global string table of the PDB. To retrieve the string
/// value, use [`to_raw_string`], [`to_string_lossy`] or methods on [`StringTable`].
///
/// [`to_raw_string`]: struct.StringRef.html#method.to_raw_string
/// [`to_string_lossy`]: struct.StringRef.html#method.to_string_lossy
/// [`StringTable`]: struct.StringTable.html
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct StringRef(pub u32);

impl From<u32> for StringRef {
    fn from(offset: u32) -> Self {
        StringRef(offset)
    }
}

impl From<StringRef> for u32 {
    fn from(string_ref: StringRef) -> Self {
        string_ref.0
    }
}

impl fmt::Display for StringRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#010x}", self.0)
    }
}

impl fmt::Debug for StringRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "StringRef({})", self)
    }
}

impl<'a> TryFromCtx<'a, Endian> for StringRef {
    type Error = scroll::Error;
    type Size = usize;

    fn try_from_ctx(this: &'a [u8], le: Endian) -> scroll::Result<(Self, Self::Size)> {
        u32::try_from_ctx(this, le).map(|(i, s)| (StringRef(i), s))
    }
}

/// Provides little-endian access to a &[u8].
#[doc(hidden)]
#[derive(Debug, Clone)]
pub(crate) struct ParseBuffer<'b>(&'b [u8], usize);

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
    #[inline]
    pub fn len(&self) -> usize {
        self.0.len() - self.1
    }

    /// Determines whether this ParseBuffer has been consumed.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Return the position within the parent slice.
    #[inline]
    pub fn pos(&self) -> usize {
        self.1
    }

    /// Align the current position to the next multiple of `alignment` bytes.
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

    /// Parse an object that implements `Pread`.
    pub fn parse<T>(&mut self) -> Result<T>
    where
        T: TryFromCtx<'b, Endian, [u8], Error = scroll::Error, Size = usize>,
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

    def_peek!((peek_u8, u8), (peek_u16, u16),);

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
        if leaf < constants::LF_NUMERIC {
            // the u16 directly encodes a value
            return Ok(Variant::U16(leaf));
        }

        match leaf {
            constants::LF_CHAR => Ok(Variant::U8(self.parse_u8()?)),
            constants::LF_SHORT => Ok(Variant::I16(self.parse_i16()?)),
            constants::LF_LONG => Ok(Variant::I32(self.parse_i32()?)),
            constants::LF_QUADWORD => Ok(Variant::I64(self.parse_i64()?)),
            constants::LF_USHORT => Ok(Variant::U16(self.parse_u16()?)),
            constants::LF_ULONG => Ok(Variant::U32(self.parse_u32()?)),
            constants::LF_UQUADWORD => Ok(Variant::U64(self.parse_u64()?)),
            _ => {
                debug_assert!(false);
                Err(Error::UnexpectedNumericPrefix(leaf))
            }
        }
    }

    #[doc(hidden)]
    #[inline]
    pub(crate) fn get_variant_size(&mut self) -> usize {
        let leaf = self.parse_u16();
        match leaf {
            Ok(leaf) => {
                if leaf < constants::LF_NUMERIC {
                    // the u16 directly encodes a value
                    return 2;
                }

                match leaf {
                    constants::LF_CHAR => 2 + 1,
                    constants::LF_SHORT => 2 + 2,
                    constants::LF_LONG => 2 + 4,
                    constants::LF_QUADWORD => 2 + 8,
                    constants::LF_USHORT => 2 + 2,
                    constants::LF_ULONG => 2 + 4,
                    constants::LF_UQUADWORD => 2 + 8,
                    _ => {
                        debug_assert!(false);
                        2
                    }
                }
            }
            Err(_) => {
                debug_assert!(false);
                2
            }
        }
    }
}

impl Default for ParseBuffer<'_> {
    fn default() -> Self {
        ParseBuffer(&[], 0)
    }
}

impl<'b> From<&'b [u8]> for ParseBuffer<'b> {
    fn from(buf: &'b [u8]) -> Self {
        ParseBuffer(buf, 0)
    }
}

impl<'b> fmt::LowerHex for ParseBuffer<'b> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> result::Result<(), fmt::Error> {
        write!(f, "ParseBuf::from(\"")?;
        for byte in self.0 {
            write!(f, "\\x{:02x}", byte)?;
        }
        write!(f, "\").as_bytes() at offset {}", self.1)
    }
}

/// Value of an enumerate type.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

impl ::std::fmt::Display for Variant {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Variant::U8(value) => write!(f, "{}", value),
            Variant::U16(value) => write!(f, "{}", value),
            Variant::U32(value) => write!(f, "{}", value),
            Variant::U64(value) => write!(f, "{}", value),
            Variant::I8(value) => write!(f, "{}", value),
            Variant::I16(value) => write!(f, "{}", value),
            Variant::I32(value) => write!(f, "{}", value),
            Variant::I64(value) => write!(f, "{}", value),
        }
    }
}

/// `RawString` refers to a `&[u8]` that physically resides somewhere inside a PDB data structure.
///
/// A `RawString` may not be valid UTF-8.
#[derive(Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct RawString<'b>(&'b [u8]);

impl fmt::Debug for RawString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "RawString({:?})", self.to_string())
    }
}

impl fmt::Display for RawString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
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
            let mut buf = ParseBuffer::from("hello\x00world\x00\x00\x01".as_bytes());

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
            let mut buf = ParseBuffer::from("\x05hello\x05world\x00\x01".as_bytes());

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
    }
}
