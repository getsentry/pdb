// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use std::borrow::Cow;
use std::fmt;
use std::io;
use std::ops::{Add, AddAssign, Sub};
use std::result;

use scroll::ctx::TryFromCtx;
use scroll::{self, Endian, Pread, LE};

use crate::tpi::constants;

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

    /// Type or Id not found.
    TypeNotFound(u32),

    /// Type or Id not indexed -- the requested type (`.0`) is larger than the maximum index covered
    /// by the `ItemFinder` (`.1`).
    TypeNotIndexed(u32, u32),

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

    /// A binary annotation was compressed incorrectly.
    InvalidCompressedAnnotation,

    /// An unknown binary annotation was encountered.
    UnknownBinaryAnnotation(u32),
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
            Error::InvalidCompressedAnnotation => "Invalid compressed annoation",
            Error::UnknownBinaryAnnotation(_) => "Unknown binary annotation",
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
            Error::UnknownBinaryAnnotation(num) => write!(f, "Unknown binary annotation {}", num),
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

/// Implements `Pread` using the inner type.
macro_rules! impl_pread {
    ($type:ty) => {
        impl<'a> TryFromCtx<'a, Endian> for $type {
            type Error = scroll::Error;
            type Size = usize;

            fn try_from_ctx(this: &'a [u8], le: Endian) -> scroll::Result<(Self, Self::Size)> {
                TryFromCtx::try_from_ctx(this, le).map(|(i, s)| (Self(i), s))
            }
        }
    };
}

/// Displays the type as hexadecimal number. Debug prints the type name around.
macro_rules! impl_hex_fmt {
    ($type:ty) => {
        impl fmt::Display for $type {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{:#x}", self.0)
            }
        }

        impl fmt::Debug for $type {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, concat!(stringify!($type), "({})"), self)
            }
        }
    };
}

/// Implements bidirectional conversion traits for the newtype.
macro_rules! impl_convert {
    ($type:ty, $inner:ty) => {
        impl From<$inner> for $type {
            fn from(offset: $inner) -> Self {
                Self(offset)
            }
        }

        impl From<$type> for $inner {
            fn from(string_ref: $type) -> Self {
                string_ref.0
            }
        }
    };
}

/// Declares that the given value represents `None`.
///
///  - `Type::none` and `Default::default` return the none value.
///  - `Type::is_some` and `Type::is_none` check for the none value.
macro_rules! impl_opt {
    ($type:ty, $none:literal) => {
        impl $type {
            /// Returns an index that points to no value.
            #[inline]
            pub const fn none() -> Self {
                Self($none)
            }

            /// Returns `true` if the index points to a valid value.
            #[inline]
            #[must_use]
            pub fn is_some(self) -> bool {
                self.0 != $none
            }

            /// Returns `true` if the index indicates the absence of a value.
            #[inline]
            #[must_use]
            pub fn is_none(self) -> bool {
                self.0 == $none
            }
        }

        impl Default for $type {
            #[inline]
            fn default() -> Self {
                Self::none()
            }
        }
    };
}

/// Implements common functionality for virtual addresses.
macro_rules! impl_va {
    ($type:ty) => {
        impl $type {
            /// Checked addition of an offset. Returns `None` if overflow occurred.
            pub fn checked_add(self, offset: u32) -> Option<Self> {
                Some(Self(self.0.checked_add(offset)?))
            }

            /// Checked computation of an offset between two addresses. Returns `None` if `other` is
            /// larger.
            pub fn checked_sub(self, other: Self) -> Option<u32> {
                self.0.checked_sub(other.0)
            }

            /// Saturating addition of an offset, clipped at the numeric bounds.
            pub fn saturating_add(self, offset: u32) -> Self {
                Self(self.0.saturating_add(offset))
            }

            /// Saturating computation of an offset between two addresses, clipped at zero.
            pub fn saturating_sub(self, other: Self) -> u32 {
                self.0.saturating_sub(other.0)
            }

            /// Wrapping (modular) addition of an offset.
            pub fn wrapping_add(self, offset: u32) -> Self {
                Self(self.0.wrapping_add(offset))
            }

            /// Wrapping (modular) computation of an offset between two addresses.
            pub fn wrapping_sub(self, other: Self) -> u32 {
                self.0.wrapping_sub(other.0)
            }
        }

        impl Add<u32> for $type {
            type Output = Self;

            /// Adds the given offset to this address.
            #[inline]
            fn add(mut self, offset: u32) -> Self {
                self.0 += offset;
                self
            }
        }

        impl AddAssign<u32> for $type {
            /// Adds the given offset to this address.
            #[inline]
            fn add_assign(&mut self, offset: u32) {
                self.0 += offset;
            }
        }

        impl Sub for $type {
            type Output = u32;

            fn sub(self, other: Self) -> Self::Output {
                self.0 - other.0
            }
        }

        impl_convert!($type, u32);
        impl_hex_fmt!($type);
    };
}

/// A Relative Virtual Address as it appears in a PE file.
///
/// RVAs are always relative to the image base address, as it is loaded into process memory. This
/// address is reported by debuggers in stack traces and may refer to symbols or instruction
/// pointers.
#[derive(Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Rva(pub u32);

impl_va!(Rva);

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

impl_va!(PdbInternalRva);
impl_pread!(PdbInternalRva);

/// Implements common functionality for section offsets.
macro_rules! impl_section_offset {
    ($type:ty) => {
        impl $type {
            /// Creates a new section offset.
            pub fn new(section: u16, offset: u32) -> Self {
                Self { offset, section }
            }

            /// Returns whether this section offset points to a valid section or into the void.
            pub fn is_valid(self) -> bool {
                self.section != 0
            }

            /// Checked addition of an offset. Returns `None` if overflow occurred.
            ///
            /// This does not check whether the offset is still valid within the given section. If
            /// the offset is out of bounds, the conversion to `Rva` will return `None`.
            pub fn checked_add(mut self, offset: u32) -> Option<Self> {
                self.offset = self.offset.checked_add(offset)?;
                Some(self)
            }

            /// Saturating addition of an offset, clipped at the numeric bounds.
            ///
            /// This does not check whether the offset is still valid within the given section. If
            /// the offset is out of bounds, the conversion to `Rva` will return `None`.
            pub fn saturating_add(mut self, offset: u32) -> Self {
                self.offset = self.offset.saturating_add(offset);
                self
            }

            /// Wrapping (modular) addition of an offset.
            ///
            /// This does not check whether the offset is still valid within the given section. If
            /// the offset is out of bounds, the conversion to `Rva` will return `None`.
            pub fn wrapping_add(mut self, offset: u32) -> Self {
                self.offset = self.offset.wrapping_add(offset);
                self
            }
        }

        impl Add<u32> for $type {
            type Output = Self;

            /// Adds the given offset to this section offset.
            ///
            /// This does not check whether the offset is still valid within the given section. If
            /// the offset is out of bounds, the conversion to `Rva` will return `None`.
            #[inline]
            fn add(mut self, offset: u32) -> Self {
                self.offset += offset;
                self
            }
        }

        impl AddAssign<u32> for $type {
            /// Adds the given offset to this section offset.
            ///
            /// This does not check whether the offset is still valid within the given section. If
            /// the offset is out of bounds, the conversion to `Rva` will return `None`.
            #[inline]
            fn add_assign(&mut self, offset: u32) {
                self.offset += offset;
            }
        }

        impl PartialOrd for $type {
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

        impl fmt::Debug for $type {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct(stringify!($type))
                    .field("section", &format_args!("{:#x}", self.section))
                    .field("offset", &format_args!("{:#x}", self.offset))
                    .finish()
            }
        }
    };
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

impl_section_offset!(SectionOffset);

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
#[derive(Clone, Copy, Default, Eq, Hash, PartialEq)]
pub struct PdbInternalSectionOffset {
    /// The memory offset relative from the start of the section's memory.
    pub offset: u32,

    /// The index of the section in the PDB's section headers list, incremented by `1`. A value of
    /// `0` indicates an invalid or missing reference.
    pub section: u16,
}

impl<'t> TryFromCtx<'t, Endian> for PdbInternalSectionOffset {
    type Error = scroll::Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], le: Endian) -> scroll::Result<(Self, Self::Size)> {
        let mut offset = 0;
        let data = PdbInternalSectionOffset {
            offset: this.gread_with(&mut offset, le)?,
            section: this.gread_with(&mut offset, le)?,
        };
        Ok((data, offset))
    }
}

impl_section_offset!(PdbInternalSectionOffset);

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
    /// Returns the MSF stream number, if this stream is not a NULL stream.
    #[inline]
    pub(crate) fn msf_number(self) -> Option<u32> {
        match self.0 {
            0xffff => None,
            index => Some(u32::from(index)),
        }
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

impl_opt!(StreamIndex, 0xffff);
impl_pread!(StreamIndex);

/// Index of a [`Type`] in the [`TypeInformation`] stream.
///
/// [`Type`]: type.Type.html
/// [`TypeInformation`]: type.TypeInformation.html
#[derive(Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TypeIndex(pub u32);

impl_convert!(TypeIndex, u32);
impl_hex_fmt!(TypeIndex);
impl_pread!(TypeIndex);

/// Index of an [`Id`] in [`IdInformation`] stream.
///
/// [`Id`]: type.Id.html
/// [`IdInformation`]: type.IdInformation.html
#[derive(Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct IdIndex(pub u32);

impl_convert!(IdIndex, u32);
impl_hex_fmt!(IdIndex);
impl_pread!(IdIndex);

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

impl_convert!(StringRef, u32);
impl_hex_fmt!(StringRef);
impl_pread!(StringRef);

/// Index of a file entry in the module.
///
/// Use the [`LineProgram`] to resolve information on the file from this offset.
///
/// [`LineProgram`]: struct.LineProgram.html
#[derive(Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FileIndex(pub u32);

impl_convert!(FileIndex, u32);
impl_hex_fmt!(FileIndex);
impl_pread!(FileIndex);

/// A reference into the symbol table of a module.
///
/// To retrieve the symbol referenced by this index, use [`ModuleInfo::symbols_at`]. When iterating,
/// use [`SymbolIter::seek`] to jump between symbols.
///
/// [`ModuleInfo::symbols_at`]: struct.ModuleInfo.html#method.symbols_at
/// [`SymbolIter::seek`]: struct.SymbolIter.html#method.seek
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SymbolIndex(pub u32);

impl_convert!(SymbolIndex, u32);
impl_hex_fmt!(SymbolIndex);
impl_pread!(SymbolIndex);

/// A register referred to by its number.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Register(pub u16);

impl_convert!(Register, u16);
impl_pread!(Register);

/// Provides little-endian access to a &[u8].
#[derive(Debug, Clone)]
pub(crate) struct ParseBuffer<'b>(&'b [u8], usize);

macro_rules! def_parse {
    ( $( ($n:ident, $t:ty) ),* $(,)* ) => {
        $(#[doc(hidden)]
          #[inline]
          #[allow(unused)]
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

    /// Seek to the given absolute position.
    #[inline]
    pub fn seek(&mut self, pos: usize) {
        self.1 = std::cmp::min(pos, self.0.len());
    }

    /// Truncates the buffer at the given absolute position.
    #[inline]
    pub fn truncate(&mut self, len: usize) -> Result<()> {
        if self.0.len() >= len {
            self.0 = &self.0[..len];
            Ok(())
        } else {
            Err(Error::UnexpectedEof)
        }
    }

    /// Align the current position to the next multiple of `alignment` bytes.
    #[inline]
    pub fn align(&mut self, alignment: usize) -> Result<()> {
        let diff = self.1 % alignment;
        if diff > 0 {
            if self.len() < (alignment - diff) {
                return Err(Error::UnexpectedEof);
            }
            self.1 += alignment - diff;
        }
        Ok(())
    }

    /// Parse an object that implements `Pread`.
    pub fn parse<T>(&mut self) -> Result<T>
    where
        T: TryFromCtx<'b, Endian, [u8], Size = usize>,
        T::Error: From<scroll::Error>,
        Error: From<T::Error>,
    {
        Ok(self.0.gread_with(&mut self.1, LE)?)
    }

    /// Parse an object that implements `Pread` with the given context.
    pub fn parse_with<T, C>(&mut self, ctx: C) -> Result<T>
    where
        T: TryFromCtx<'b, C, [u8], Size = usize>,
        T::Error: From<scroll::Error>,
        Error: From<T::Error>,
        C: Copy,
    {
        Ok(self.0.gread_with(&mut self.1, ctx)?)
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
    #[inline]
    pub fn parse_u8_pascal_string(&mut self) -> Result<RawString<'b>> {
        let length = self.parse_u8()? as usize;
        Ok(RawString::from(self.take(length)?))
    }

    /// Take n bytes from the input
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
#[allow(missing_docs)]
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

impl fmt::Display for Variant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
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

impl<'a> TryFromCtx<'a, Endian> for Variant {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'a [u8], le: Endian) -> Result<(Self, Self::Size)> {
        let mut offset = 0;

        let variant = match this.gread_with(&mut offset, le)? {
            value if value < constants::LF_NUMERIC => Variant::U16(value),
            constants::LF_CHAR => Variant::U8(this.gread_with(&mut offset, le)?),
            constants::LF_SHORT => Variant::I16(this.gread_with(&mut offset, le)?),
            constants::LF_LONG => Variant::I32(this.gread_with(&mut offset, le)?),
            constants::LF_QUADWORD => Variant::I64(this.gread_with(&mut offset, le)?),
            constants::LF_USHORT => Variant::U16(this.gread_with(&mut offset, le)?),
            constants::LF_ULONG => Variant::U32(this.gread_with(&mut offset, le)?),
            constants::LF_UQUADWORD => Variant::U64(this.gread_with(&mut offset, le)?),
            _ if cfg!(debug_assertions) => unreachable!(),
            other => return Err(Error::UnexpectedNumericPrefix(other)),
        };

        Ok((variant, offset))
    }
}

/// `RawString` refers to a `&[u8]` that physically resides somewhere inside a PDB data structure.
///
/// A `RawString` may not be valid UTF-8.
#[derive(Clone, Copy, Default, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
}
