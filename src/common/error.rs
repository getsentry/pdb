use core::fmt;

use crate::io::IoError;

/// An error that occurred while reading or parsing the PDB.
#[non_exhaustive]
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

    /// Invalid length or alignment of a stream.
    InvalidStreamLength(&'static str),

    /// An IO error occurred while reading from the data source.
    IoError(IoError),

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

    /// Type index is not a cross module reference.
    NotACrossModuleRef(u32),

    /// Cross module reference not found in imports.
    CrossModuleRefNotFound(u32),

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

#[cfg(feature = "std")]
impl From<std::io::Error> for Error {

    #[inline]
    fn from(err: std::io::Error) -> Self {
        Self::IoError(err.into())
    }
}

impl core::error::Error for Error {
    fn source(&self) -> Option<&(dyn core::error::Error + 'static)> {
        match self {
            Self::IoError(error) => Some(error),
            _ => None,
        }
    }
}

impl From<IoError> for crate::Error {
    fn from(err: IoError) -> Self {
        Error::IoError(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> ::core::result::Result<(), fmt::Error> {
        match self {
            Self::PageReferenceOutOfRange(p) => {
                write!(f, "MSF referred to page number ({}) out of range", p)
            }
            Self::InvalidPageSize(n) => write!(
                f,
                "The MSF header specifies an invalid page size ({} bytes)",
                n
            ),
            Self::StreamNotFound(s) => {
                write!(f, "The requested stream ({}) is not stored in this file", s)
            }
            Self::InvalidStreamLength(s) => write!(
                f,
                "{} stream has an invalid length or alignment for its records",
                s
            ),
            // Self::IoError(ref e) => write!(f, "IO error while reading PDB: {}", e),
            Self::UnimplementedFeature(feature) => {
                write!(f, "Unimplemented PDB feature: {}", feature)
            }
            Self::UnimplementedSymbolKind(kind) => write!(
                f,
                "Support for symbols of kind {:#06x} is not implemented",
                kind
            ),
            Self::InvalidTypeInformationHeader(reason) => {
                write!(f, "The type information header was invalid: {}", reason)
            }
            Self::TypeNotFound(type_index) => write!(f, "Type {} not found", type_index),
            Self::TypeNotIndexed(type_index, indexed_count) => write!(
                f,
                "Type {} not indexed (index covers {})",
                type_index, indexed_count
            ),
            Self::UnimplementedTypeKind(kind) => write!(
                f,
                "Support for types of kind {:#06x} is not implemented",
                kind
            ),
            Self::NotACrossModuleRef(index) => {
                write!(f, "Type {:#06x} is not a cross module reference", index)
            }
            Self::CrossModuleRefNotFound(index) => write!(
                f,
                "Cross module reference {:#06x} not found in imports",
                index
            ),
            Self::UnexpectedNumericPrefix(prefix) => write!(
                f,
                "Variable-length numeric parsing encountered an unexpected prefix ({:#06x}",
                prefix
            ),
            Self::UnimplementedDebugSubsection(kind) => write!(
                f,
                "Debug module subsection of kind {:#06x} is not implemented",
                kind
            ),
            Self::UnimplementedFileChecksumKind(kind) => {
                write!(f, "Unknown source file checksum kind {}", kind)
            }
            Self::InvalidFileChecksumOffset(offset) => {
                write!(f, "Invalid source file checksum offset {:#x}", offset)
            }
            Self::UnknownBinaryAnnotation(num) => write!(f, "Unknown binary annotation {}", num),
            _ => fmt::Debug::fmt(self, f),
        }
    }
}

impl From<scroll::Error> for Error {
    fn from(e: scroll::Error) -> Self {
        match e {
            scroll::Error::BadOffset(_) | scroll::Error::TooBig { .. } => Self::UnexpectedEof,
            _ => Self::ScrollError(e),
        }
    }
}

/// Result type used throughout the PDB crate.
pub type Result<T> = core::result::Result<T, Error>;