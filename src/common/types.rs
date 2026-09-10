#[cfg(feature = "alloc")]
use alloc::borrow::Cow;

#[cfg(feature = "alloc")]
use alloc::string::String;

use core::fmt;
use core::ops::{Add, AddAssign, Sub};

use scroll::ctx::TryFromCtx;
use scroll::{Endian, Pread, LE};

use crate::{Error, Result};

use crate::tpi::constants;

/// Implements `Pread` using the inner type.
macro_rules! impl_pread {
    ($type:ty) => {
        impl<'a> TryFromCtx<'a, Endian> for $type {
            type Error = scroll::Error;

            fn try_from_ctx(this: &'a [u8], le: Endian) -> scroll::Result<(Self, usize)> {
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
/// [`to_rva`](Self::to_rva).
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
            fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
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
/// stores [`PdbInternalSectionOffset`]s.
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
/// binary using [`to_rva`](PdbInternalSectionOffset::to_rva). Likewise, there is a conversion to [`SectionOffset`] in the actual address
/// space.
///
/// For binaries and their PDBs that have not been optimized, both address spaces are equal and the
/// offsets are interchangeable. The conversion operations are cheap no-ops in this case.
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

    fn try_from_ctx(this: &'t [u8], le: Endian) -> scroll::Result<(Self, usize)> {
        let mut offset = 0;
        let data = Self {
            offset: this.gread_with(&mut offset, le)?,
            section: this.gread_with(&mut offset, le)?,
        };
        Ok((data, offset))
    }
}

impl_section_offset!(PdbInternalSectionOffset);

/// Index of a PDB stream.
///
/// This index can either refer to a stream, or indicate the absence of a stream. Check
/// [`is_none`](Self::is_none) to see whether a stream should exist.
///
/// Use [`get`](Self::get) to load data for this stream.
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct StreamIndex(pub u16);

impl StreamIndex {
    /// Returns the MSF stream number, if this stream is not a NULL stream.
    #[inline]
    pub(crate) fn msf_number(self) -> Option<u32> {
        match self.0 {
            0xFFFF => None,
            index => Some(u32::from(index)),
        }
    }
}

impl fmt::Display for StreamIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.msf_number() {
            Some(number) => write!(f, "{}", number),
            None => write!(f, "None"),
        }
    }
}

impl fmt::Debug for StreamIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "StreamIndex({})", self)
    }
}

impl_opt!(StreamIndex, 0xFFFF);
impl_pread!(StreamIndex);

/// An index into either the [`TypeInformation`](crate::TypeInformation) or
/// [`IdInformation`](crate::IdInformation) stream.
pub trait ItemIndex:
    Copy + Default + fmt::Debug + fmt::Display + PartialEq + PartialOrd + From<u32> + Into<u32>
{
    /// Returns `true` if this is a cross module reference.
    ///
    /// When compiling with LTO, the compiler may reference types and ids across modules. In such
    /// cases, a lookup in the global streams will not succeed. Instead, the import must be resolved
    /// using cross module references:
    ///
    ///  1. Look up the index in [`CrossModuleImports`](crate::CrossModuleImports) of the current
    ///     module.
    ///  2. Use [`StringTable`](crate::StringTable) to resolve the name of the referenced module.
    ///  3. Find the [`Module`](crate::Module) with the same module name and load its
    ///     [`ModuleInfo`](crate::ModuleInfo).  Note that this comparison needs to be done
    ///     case-insensitively as the name in the DBI stream and name table are known to not
    ///     have matching cases.
    ///  4. Resolve the [`Local`] index into a global one using
    ///     [`CrossModuleExports`](crate::CrossModuleExports).
    ///
    /// Cross module references are specially formatted indexes with the most significant bit set to
    /// `1`. The remaining bits are divided into a module and index offset into the
    /// [`CrossModuleImports`](crate::CrossModuleImports) section.
    fn is_cross_module(self) -> bool {
        (self.into() & 0x8000_0000) != 0
    }
}

/// Index of [`TypeData`](crate::TypeData) in the [`TypeInformation`](crate::TypeInformation) stream.
///
/// If this index is a [cross module reference](ItemIndex::is_cross_module), it must be resolved
/// before lookup in the stream.
#[derive(Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TypeIndex(pub u32);

impl_convert!(TypeIndex, u32);
impl_hex_fmt!(TypeIndex);
impl_pread!(TypeIndex);

impl ItemIndex for TypeIndex {}

/// Index of an [`Id`](crate::Id) in [`IdInformation`](crate::IdInformation) stream.
///
/// If this index is a [cross module reference](ItemIndex::is_cross_module), it must be resolved
/// before lookup in the stream.
#[derive(Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct IdIndex(pub u32);

impl_convert!(IdIndex, u32);
impl_hex_fmt!(IdIndex);
impl_pread!(IdIndex);

impl ItemIndex for IdIndex {}

/// An [`ItemIndex`] that is local to a module.
///
/// This index is usually part of a [`CrossModuleRef`](crate::CrossModuleRef). It cannot be used to
/// query the [`TypeInformation`](crate::TypeInformation) or [`IdInformation`](crate::IdInformation)
/// streams directly. Instead, it must be looked up in the
/// [`CrossModuleImports`](crate::CrossModuleImports) of the module it belongs to in order to obtain
/// the global index.
///
/// See [`ItemIndex::is_cross_module`] for more information.
#[derive(Clone, Copy, Debug, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct Local<I: ItemIndex>(pub I);

impl<I> fmt::Display for Local<I>
where
    I: ItemIndex + fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A reference to a string in the string table.
///
/// This type stores an offset into the global string table of the PDB. To retrieve the string
/// value, use [`to_raw_string`](Self::to_raw_string), [`to_string_lossy`](Self::to_string_lossy) or
/// methods on [`StringTable`](crate::StringTable).
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct StringRef(pub u32);

impl_convert!(StringRef, u32);
impl_hex_fmt!(StringRef);
impl_pread!(StringRef);

/// Index of a file entry in the module.
///
/// Use the [`LineProgram`](crate::LineProgram) to resolve information on the file from this offset.
#[derive(Clone, Copy, Default, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct FileIndex(pub u32);

impl_convert!(FileIndex, u32);
impl_hex_fmt!(FileIndex);
impl_pread!(FileIndex);

/// A reference into the symbol table of a module.
///
/// To retrieve the symbol referenced by this index, use
/// [`ModuleInfo::symbols_at`](crate::ModuleInfo::symbols_at). When iterating, use
/// [`SymbolIter::seek`](crate::SymbolIter::seek) to jump between symbols.
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
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U8(value) => write!(f, "{}", value),
            Self::U16(value) => write!(f, "{}", value),
            Self::U32(value) => write!(f, "{}", value),
            Self::U64(value) => write!(f, "{}", value),
            Self::I8(value) => write!(f, "{}", value),
            Self::I16(value) => write!(f, "{}", value),
            Self::I32(value) => write!(f, "{}", value),
            Self::I64(value) => write!(f, "{}", value),
        }
    }
}

impl<'a> TryFromCtx<'a, Endian> for Variant {
    type Error = Error;

    fn try_from_ctx(this: &'a [u8], le: Endian) -> Result<(Self, usize)> {
        let mut offset = 0;

        let variant = match this.gread_with(&mut offset, le)? {
            value if value < constants::LF_NUMERIC => Self::U16(value),
            constants::LF_CHAR => Self::U8(this.gread_with(&mut offset, le)?),
            constants::LF_SHORT => Self::I16(this.gread_with(&mut offset, le)?),
            constants::LF_LONG => Self::I32(this.gread_with(&mut offset, le)?),
            constants::LF_QUADWORD => Self::I64(this.gread_with(&mut offset, le)?),
            constants::LF_USHORT => Self::U16(this.gread_with(&mut offset, le)?),
            constants::LF_ULONG => Self::U32(this.gread_with(&mut offset, le)?),
            constants::LF_UQUADWORD => Self::U64(this.gread_with(&mut offset, le)?),
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
        let str = core::str::from_utf8(self.0).map_err(|err| fmt::Error)?;
        write!(f, "RawString({:?})", str)
    }
}

impl fmt::Display for RawString<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = core::str::from_utf8(self.0).map_err(|err| fmt::Error)?;
        write!(f, "{}", str)
    }
}

impl<'b> RawString<'b> {
    /// Return the raw bytes of this string, as found in the PDB file.
    #[inline]
    pub const fn as_bytes(&self) -> &'b [u8] {
        self.0
    }

    /// Return the length of this string in bytes.
    #[inline]
    pub const fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns a boolean indicating if this string is empty.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.0.len() == 0
    }

    /// Returns a UTF-8 `String`, substituting in replacement characters as needed.
    ///
    /// This uses [`String::from_utf8_lossy`] and thus avoids copying in cases where the original
    /// string was valid UTF-8. This is the expected case for strings that appear in PDB files,
    /// since they are almost always composed of printable 7-bit ASCII characters.
    #[cfg(feature = "alloc")]
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