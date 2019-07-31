// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use std::fmt;

use scroll::{ctx::TryFromCtx, Endian, Pread, LE};

use crate::common::*;
use crate::msf::*;
use crate::FallibleIterator;

mod annotations;
mod constants;

use self::constants::*;

pub use self::annotations::*;

/// The raw type discriminator for `Symbols`.
pub type SymbolKind = u16;

/// A register referred to by its number.
pub type Register = u16;

/// A reference into the symbol table of a module.
///
/// To retrieve the symbol referenced by this index, use [`SymbolTable::iter_at`]. When iterating,
/// use [`SymbolIter::seek`] to jump between symbols.
///
/// [`SymbolTable::iter_at`]: struct.SymbolTable.html#method.iter_at
/// [`SymbolIter::seek`]: struct.SymbolIter.html#method.seek
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct SymbolIndex(pub u32);

impl From<u32> for SymbolIndex {
    fn from(offset: u32) -> Self {
        Self(offset)
    }
}

impl From<SymbolIndex> for u32 {
    fn from(string_ref: SymbolIndex) -> Self {
        string_ref.0
    }
}

impl fmt::Display for SymbolIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#010x}", self.0)
    }
}

impl fmt::Debug for SymbolIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SymbolIndex({})", self)
    }
}

impl<'a> TryFromCtx<'a, Endian> for SymbolIndex {
    type Error = scroll::Error;
    type Size = usize;

    fn try_from_ctx(this: &'a [u8], le: Endian) -> scroll::Result<(Self, Self::Size)> {
        u32::try_from_ctx(this, le).map(|(i, s)| (Self(i), s))
    }
}

/// PDB symbol tables contain names, locations, and metadata about functions, global/static data,
/// constants, data types, and more.
///
/// The `SymbolTable` holds a `SourceView` referencing the symbol table inside the PDB file. All the
/// data structures returned by a `SymbolTable` refer to that buffer.
///
/// # Example
///
/// ```
/// # use pdb::FallibleIterator;
/// #
/// # fn test() -> pdb::Result<usize> {
/// let file = std::fs::File::open("fixtures/self/foo.pdb")?;
/// let mut pdb = pdb::PDB::open(file)?;
///
/// let symbol_table = pdb.global_symbols()?;
/// let address_map = pdb.address_map()?;
///
/// # let mut count: usize = 0;
/// let mut symbols = symbol_table.iter();
/// while let Some(symbol) = symbols.next()? {
///     match symbol.parse() {
///         Ok(pdb::SymbolData::Public(data)) if data.function => {
///             // we found the location of a function!
///             let rva = data.offset.to_rva(&address_map).unwrap_or_default();
///             println!("{} is {}", rva, data.name);
///             # count += 1;
///         }
///         _ => {}
///     }
/// }
///
/// # Ok(count)
/// # }
/// # assert!(test().expect("test") > 2000);
/// ```
#[derive(Debug)]
pub struct SymbolTable<'s> {
    stream: Stream<'s>,
}

impl<'s> SymbolTable<'s> {
    /// Parses a symbol table from raw stream data.
    pub(crate) fn parse(stream: Stream<'s>) -> Result<Self> {
        Ok(SymbolTable { stream })
    }

    /// Returns an iterator that can traverse the symbol table in sequential order.
    pub fn iter(&self) -> SymbolIter<'_> {
        SymbolIter::new(self.stream.parse_buffer())
    }

    /// Returns an iterator over symbols starting at the given index.
    pub fn iter_at(&self, index: SymbolIndex) -> SymbolIter<'_> {
        let mut iter = self.iter();
        iter.seek(index);
        iter
    }
}

/// Represents a symbol from the symbol table.
///
/// A `Symbol` is represented internally as a `&[u8]`, and in general the bytes inside are not
/// inspected in any way before calling any of the accessor methods.
///
/// To avoid copying, `Symbol`s exist as references to data owned by the parent `SymbolTable`.
/// Therefore, a `Symbol` may not outlive its parent `SymbolTable`.
#[derive(Copy, Clone, PartialEq)]
pub struct Symbol<'t>(&'t [u8]);

impl<'t> Symbol<'t> {
    /// Returns the kind of symbol identified by this Symbol.
    #[inline]
    pub fn raw_kind(&self) -> SymbolKind {
        debug_assert!(self.0.len() >= 2);
        self.0.pread_with(0, LE).unwrap_or_default()
    }

    /// Returns the raw bytes of this symbol record, including the symbol type and extra data, but
    /// not including the preceding symbol length indicator.
    #[inline]
    pub fn raw_bytes(&self) -> &'t [u8] {
        self.0
    }

    /// Parse the symbol into the `SymbolData` it contains.
    #[inline]
    pub fn parse(&self) -> Result<SymbolData<'t>> {
        Ok(self.raw_bytes().pread_with(0, ())?)
    }

    /// Returns whether this symbol starts a scope.
    ///
    /// If `true`, this symbol has a `parent` and an `end` field, which contains the offset of the
    /// corrsponding end symbol.
    pub fn starts_scope(&self) -> bool {
        match self.raw_kind() {
            S_GPROC16 | S_GPROCMIPS | S_GPROCMIPS_ST | S_GPROCIA64 | S_GPROCIA64_ST | S_LPROC16
            | S_LPROC32_DPC | S_LPROCMIPS | S_LPROCMIPS_ST | S_LPROCIA64 | S_LPROCIA64_ST
            | S_LPROC32_DPC_ID | S_GPROCMIPS_ID | S_GPROCIA64_ID | S_BLOCK16 | S_BLOCK32
            | S_BLOCK32_ST | S_WITH16 | S_WITH32 | S_WITH32_ST | S_THUNK16 | S_THUNK32
            | S_THUNK32_ST | S_SEPCODE | S_GMANPROC | S_GMANPROC_ST | S_LMANPROC
            | S_LMANPROC_ST | S_INLINESITE | S_INLINESITE2 => true,
            _ => false,
        }
    }

    /// Returns whether this symbol declares the end of a scope.
    pub fn ends_scope(&self) -> bool {
        match self.raw_kind() {
            S_END | S_PROC_ID_END | S_INLINESITE_END => true,
            _ => false,
        }
    }
}

impl<'t> fmt::Debug for Symbol<'t> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Symbol{{ kind: 0x{:4x} [{} bytes] }}",
            self.raw_kind(),
            self.0.len()
        )
    }
}

// data types are defined at:
//   https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L3038
// constants defined at:
//   https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2735
// decoding reference:
//   https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/cvdump/dumpsym7.cpp#L264

fn parse_symbol_name<'t>(buf: &mut ParseBuffer<'t>, kind: SymbolKind) -> Result<RawString<'t>> {
    if kind < S_ST_MAX {
        // Pascal-style name
        buf.parse_u8_pascal_string()
    } else {
        // NUL-terminated name
        buf.parse_cstring()
    }
}

fn parse_optional_name<'t>(
    buf: &mut ParseBuffer<'t>,
    kind: SymbolKind,
) -> Result<Option<RawString<'t>>> {
    if kind < S_ST_MAX {
        // ST variants do not specify a name
        Ok(None)
    } else {
        // NUL-terminated name
        buf.parse_cstring().map(Some)
    }
}

/// `SymbolData` contains the information parsed from a symbol record.
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SymbolData<'t> {
    // S_END (0x0006)
    ScopeEnd,

    // S_REGISTER (0x1106) | S_REGISTER_ST (0x1001)
    RegisterVariable(RegisterVariableSymbol<'t>),

    // S_MANYREG (0x110a) | S_MANYREG_ST (0x1005)
    // S_MANYREG2 (0x1117) | S_MANYREG2_ST (0x1014)
    MultiRegisterVariable(MultiRegisterVariableSymbol<'t>),

    // S_PUB32 (0x110e) | S_PUB32_ST (0x1009)
    Public(PublicSymbol<'t>),

    //   S_LDATA32 (0x110c) | S_LDATA32_ST (0x1007)
    //   S_GDATA32 (0x110d) | S_GDATA32_ST (0x1008)
    //  S_LMANDATA (0x111c) | S_LMANDATA_ST (0x1020)
    //  S_GMANDATA (0x111d) | S_GMANDATA_ST (0x1021)
    Data(DataSymbol<'t>),

    //   S_PROCREF (0x1125) |  S_PROCREF_ST (0x0400)
    //  S_LPROCREF (0x1127) | S_LPROCREF_ST (0x0403)
    ProcedureReference(ProcedureReferenceSymbol<'t>),

    //   S_DATAREF (0x1126) |  S_DATAREF_ST (0x0401)
    DataReference(DataReferenceSymbol<'t>),

    // S_ANNOTATIONREF (0x1128)
    AnnotationReference(AnnotationReferenceSymbol<'t>),

    //  S_CONSTANT (0x1107) | S_CONSTANT_ST (0x1002)
    Constant(ConstantSymbol<'t>),

    //       S_UDT (0x1108) | S_UDT_ST (0x1003)
    UserDefinedType(UserDefinedTypeSymbol<'t>),

    // S_LTHREAD32 (0x1112) | S_LTHREAD32_ST (0x100e)
    // S_GTHREAD32 (0x1113) | S_GTHREAD32_ST (0x100f)
    ThreadStorage(ThreadStorageSymbol<'t>),

    // S_LPROC32 (0x110f) | S_LPROC32_ST (0x100a)
    // S_GPROC32 (0x1110) | S_GPROC32_ST (0x100b)
    // S_LPROC32_ID (0x1146) |
    // S_GPROC32_ID (0x1147) |
    // S_LPROC32_DPC (0x1155) |
    // S_LPROC32_DPC_ID (0x1156)
    Procedure(ProcedureSymbol<'t>),

    // S_PROC_ID_END (0x114f)
    ProcedureEnd,

    // S_INLINESITE (0x114d)
    InlineSite(InlineSiteSymbol<'t>),

    // S_INLINESITE_END (0x114e)
    InlineSiteEnd,

    // S_OBJNAME (0x1101) | S_OBJNAME_ST (0x0009)
    ObjName(ObjNameSymbol<'t>),

    // S_COMPILE2 (0x1116) | S_COMPILE2_ST (0x1013) | S_COMPILE3 (0x113c)
    ExtendedCompileFlags(ExtendedCompileFlagsSymbol<'t>),

    // S_UNAMESPACE (0x1124) | S_UNAMESPACE_ST (0x1029)
    UsingNamespace(UsingNamespaceSymbol<'t>),

    // S_LOCAL (0x113e)
    Local(LocalSymbol<'t>),

    // S_EXPORT (0x1138)
    Export(ExportSymbol<'t>),
}

impl<'t> SymbolData<'t> {
    /// Returns the name of this symbol if it has one.
    pub fn name(&self) -> Option<RawString<'t>> {
        match self {
            SymbolData::ScopeEnd => None,
            SymbolData::RegisterVariable(_) => None,
            SymbolData::MultiRegisterVariable(_) => None,
            SymbolData::Public(data) => Some(data.name),
            SymbolData::Data(data) => Some(data.name),
            SymbolData::ProcedureReference(data) => data.name,
            SymbolData::DataReference(data) => data.name,
            SymbolData::AnnotationReference(data) => Some(data.name),
            SymbolData::Constant(data) => Some(data.name),
            SymbolData::UserDefinedType(data) => Some(data.name),
            SymbolData::ThreadStorage(data) => Some(data.name),
            SymbolData::Procedure(data) => Some(data.name),
            SymbolData::ProcedureEnd => None,
            SymbolData::InlineSite(_) => None,
            SymbolData::InlineSiteEnd => None,
            SymbolData::ObjName(data) => Some(data.name),
            SymbolData::ExtendedCompileFlags(_) => None,
            SymbolData::UsingNamespace(data) => Some(data.name),
            SymbolData::Local(data) => Some(data.name),
            SymbolData::Export(data) => Some(data.name),
        }
    }
}

impl<'t> TryFromCtx<'t> for SymbolData<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], _ctx: ()) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);
        let kind = buf.parse()?;

        let symbol = match kind {
            S_END => SymbolData::ScopeEnd,
            S_REGISTER | S_REGISTER_ST => SymbolData::RegisterVariable(buf.parse_with(kind)?),
            S_MANYREG | S_MANYREG_ST | S_MANYREG2 | S_MANYREG2_ST => {
                SymbolData::MultiRegisterVariable(buf.parse_with(kind)?)
            }
            S_PUB32 | S_PUB32_ST => SymbolData::Public(buf.parse_with(kind)?),
            S_LDATA32 | S_LDATA32_ST | S_GDATA32 | S_GDATA32_ST | S_LMANDATA | S_LMANDATA_ST
            | S_GMANDATA | S_GMANDATA_ST => SymbolData::Data(buf.parse_with(kind)?),
            S_PROCREF | S_PROCREF_ST | S_LPROCREF | S_LPROCREF_ST => {
                SymbolData::ProcedureReference(buf.parse_with(kind)?)
            }
            S_DATAREF | S_DATAREF_ST => SymbolData::DataReference(buf.parse_with(kind)?),
            S_ANNOTATIONREF => SymbolData::AnnotationReference(buf.parse_with(kind)?),
            S_CONSTANT | S_CONSTANT_ST | S_MANCONSTANT => {
                SymbolData::Constant(buf.parse_with(kind)?)
            }
            S_UDT | S_UDT_ST | S_COBOLUDT | S_COBOLUDT_ST => {
                SymbolData::UserDefinedType(buf.parse_with(kind)?)
            }
            S_LTHREAD32 | S_LTHREAD32_ST | S_GTHREAD32 | S_GTHREAD32_ST => {
                SymbolData::ThreadStorage(buf.parse_with(kind)?)
            }
            S_LPROC32 | S_LPROC32_ST | S_GPROC32 | S_GPROC32_ST | S_LPROC32_ID | S_GPROC32_ID
            | S_LPROC32_DPC | S_LPROC32_DPC_ID => SymbolData::Procedure(buf.parse_with(kind)?),
            S_PROC_ID_END => SymbolData::ProcedureEnd,
            S_INLINESITE | S_INLINESITE2 => SymbolData::InlineSite(buf.parse_with(kind)?),
            S_INLINESITE_END => SymbolData::InlineSiteEnd,
            S_OBJNAME | S_OBJNAME_ST => SymbolData::ObjName(buf.parse_with(kind)?),
            S_COMPILE2 | S_COMPILE2_ST | S_COMPILE3 => {
                SymbolData::ExtendedCompileFlags(buf.parse_with(kind)?)
            }
            S_UNAMESPACE | S_UNAMESPACE_ST => SymbolData::UsingNamespace(buf.parse_with(kind)?),
            S_LOCAL => SymbolData::Local(buf.parse_with(kind)?),
            S_EXPORT => SymbolData::Export(buf.parse_with(kind)?),
            other => return Err(Error::UnimplementedSymbolKind(other)),
        };

        Ok((symbol, buf.pos()))
    }
}

/// A Register variable.
///
/// `S_REGISTER`, or `S_REGISTER_ST`
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RegisterVariableSymbol<'t> {
    pub type_index: TypeIndex,
    pub register: Register,
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for RegisterVariableSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = RegisterVariableSymbol {
            type_index: buf.parse()?,
            register: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// A Register variable spanning multiple registers.
///
/// `S_MANYREG`, `S_MANYREG_ST`, `S_MANYREG2`, or `S_MANYREG2_ST`
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MultiRegisterVariableSymbol<'t> {
    pub type_index: TypeIndex,
    /// Most significant register first.
    pub registers: Vec<(Register, RawString<'t>)>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for MultiRegisterVariableSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let type_index = buf.parse()?;
        let count = match kind {
            S_MANYREG2 | S_MANYREG2_ST => buf.parse::<u16>()?,
            _ => u16::from(buf.parse::<u8>()?),
        };

        let mut registers = Vec::with_capacity(count as usize);
        for _ in 0..count {
            registers.push((buf.parse()?, parse_symbol_name(&mut buf, kind)?));
        }

        let symbol = MultiRegisterVariableSymbol {
            type_index,
            registers,
        };

        Ok((symbol, buf.pos()))
    }
}

// CV_PUBSYMFLAGS_e
const CVPSF_CODE: u32 = 0x1;
const CVPSF_FUNCTION: u32 = 0x2;
const CVPSF_MANAGED: u32 = 0x4;
const CVPSF_MSIL: u32 = 0x8;

/// The information parsed from a symbol record with kind `S_PUB32` or `S_PUB32_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PublicSymbol<'t> {
    pub code: bool,
    pub function: bool,
    pub managed: bool,
    pub msil: bool,
    pub offset: PdbInternalSectionOffset,
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for PublicSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let flags = buf.parse::<u32>()?;
        let symbol = PublicSymbol {
            code: flags & CVPSF_CODE != 0,
            function: flags & CVPSF_FUNCTION != 0,
            managed: flags & CVPSF_MANAGED != 0,
            msil: flags & CVPSF_MSIL != 0,
            offset: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// The information parsed from a symbol record with kind
/// `S_LDATA32`, `S_LDATA32_ST`, `S_GDATA32`, `S_GDATA32_ST`,
/// `S_LMANDATA`, `S_LMANDATA_ST`, `S_GMANDATA`, or `S_GMANDATA_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DataSymbol<'t> {
    pub global: bool,
    pub managed: bool,
    pub type_index: TypeIndex,
    pub offset: PdbInternalSectionOffset,
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for DataSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let global = match kind {
            S_GDATA32 | S_GDATA32_ST | S_GMANDATA | S_GMANDATA_ST => true,
            _ => false,
        };
        let managed = match kind {
            S_LMANDATA | S_LMANDATA_ST | S_GMANDATA | S_GMANDATA_ST => true,
            _ => false,
        };

        let symbol = DataSymbol {
            global,
            managed,
            type_index: buf.parse()?,
            offset: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// The information parsed from a symbol record with kind
/// `S_PROCREF`, `S_PROCREF_ST`, `S_LPROCREF`, or `S_LPROCREF_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProcedureReferenceSymbol<'t> {
    pub global: bool,
    pub sum_name: u32,
    pub symbol_index: SymbolIndex,
    pub module: u16,
    pub name: Option<RawString<'t>>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ProcedureReferenceSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let global = match kind {
            S_PROCREF | S_PROCREF_ST => true,
            _ => false,
        };

        let symbol = ProcedureReferenceSymbol {
            global,
            sum_name: buf.parse()?,
            symbol_index: buf.parse()?,
            module: buf.parse()?,
            name: parse_optional_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// The information parsed from a symbol record with kind `S_DATAREF` or `S_DATAREF_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DataReferenceSymbol<'t> {
    pub sum_name: u32,
    pub symbol_index: SymbolIndex,
    pub module: u16,
    pub name: Option<RawString<'t>>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for DataReferenceSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = DataReferenceSymbol {
            sum_name: buf.parse()?,
            symbol_index: buf.parse()?,
            module: buf.parse()?,
            name: parse_optional_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// The information parsed from a symbol record with kind `S_ANNOTATIONREF`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AnnotationReferenceSymbol<'t> {
    pub sum_name: u32,
    pub symbol_index: SymbolIndex,
    pub module: u16,
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for AnnotationReferenceSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = AnnotationReferenceSymbol {
            sum_name: buf.parse()?,
            symbol_index: buf.parse()?,
            module: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// The information parsed from a symbol record with kind `S_CONSTANT`, or `S_CONSTANT_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ConstantSymbol<'t> {
    pub managed: bool,
    pub type_index: TypeIndex,
    pub value: Variant,
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ConstantSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = ConstantSymbol {
            managed: kind == S_MANCONSTANT,
            type_index: buf.parse()?,
            value: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// The information parsed from a symbol record with kind `S_UDT`, or `S_UDT_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct UserDefinedTypeSymbol<'t> {
    pub type_index: TypeIndex,
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for UserDefinedTypeSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = UserDefinedTypeSymbol {
            type_index: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// The information parsed from a symbol record with kind
/// `S_LTHREAD32`, `S_LTHREAD32_ST`, `S_GTHREAD32`, or `S_GTHREAD32_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ThreadStorageSymbol<'t> {
    pub global: bool,
    pub type_index: TypeIndex,
    pub offset: PdbInternalSectionOffset,
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ThreadStorageSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let global = match kind {
            S_GTHREAD32 | S_GTHREAD32_ST => true,
            _ => false,
        };

        let symbol = ThreadStorageSymbol {
            global,
            type_index: buf.parse()?,
            offset: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

// CV_PROCFLAGS:
const CV_PFLAG_NOFPO: u8 = 0x01;
const CV_PFLAG_INT: u8 = 0x02;
const CV_PFLAG_FAR: u8 = 0x04;
const CV_PFLAG_NEVER: u8 = 0x08;
const CV_PFLAG_NOTREACHED: u8 = 0x10;
const CV_PFLAG_CUST_CALL: u8 = 0x20;
const CV_PFLAG_NOINLINE: u8 = 0x40;
const CV_PFLAG_OPTDBGINFO: u8 = 0x80;

/// The information parsed from a CV_PROCFLAGS bit field
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProcedureFlags {
    pub nofpo: bool,
    pub int: bool,
    pub far: bool,
    pub never: bool,
    pub notreached: bool,
    pub cust_call: bool,
    pub noinline: bool,
    pub optdbginfo: bool,
}

impl<'t> TryFromCtx<'t, Endian> for ProcedureFlags {
    type Error = scroll::Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], le: Endian) -> scroll::Result<(Self, Self::Size)> {
        let (value, size) = u8::try_from_ctx(this, le)?;

        let flags = ProcedureFlags {
            nofpo: value & CV_PFLAG_NOFPO != 0,
            int: value & CV_PFLAG_INT != 0,
            far: value & CV_PFLAG_FAR != 0,
            never: value & CV_PFLAG_NEVER != 0,
            notreached: value & CV_PFLAG_NOTREACHED != 0,
            cust_call: value & CV_PFLAG_CUST_CALL != 0,
            noinline: value & CV_PFLAG_NOINLINE != 0,
            optdbginfo: value & CV_PFLAG_OPTDBGINFO != 0,
        };

        Ok((flags, size))
    }
}

/// The information parsed from a symbol record with kind
/// `S_GPROC32`, `S_GPROC32_ST`, `S_LPROC32`, `S_LPROC32_ST`
/// `S_GPROC32_ID`, `S_LPROC32_ID`, `S_LPROC32_DPC`, or `S_LPROC32_DPC_ID`
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProcedureSymbol<'t> {
    pub global: bool,
    pub parent: SymbolIndex,
    pub end: SymbolIndex,
    pub next: SymbolIndex,
    pub len: u32,
    pub dbg_start_offset: u32,
    pub dbg_end_offset: u32,
    pub type_index: TypeIndex,
    pub offset: PdbInternalSectionOffset,
    pub flags: ProcedureFlags,
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ProcedureSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let global = match kind {
            S_GPROC32 | S_GPROC32_ST | S_GPROC32_ID => true,
            _ => false,
        };

        let symbol = ProcedureSymbol {
            global,
            parent: buf.parse()?,
            end: buf.parse()?,
            next: buf.parse()?,
            len: buf.parse()?,
            dbg_start_offset: buf.parse()?,
            dbg_end_offset: buf.parse()?,
            type_index: buf.parse()?,
            offset: buf.parse()?,
            flags: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// The information parsed from a symbol record with kind
/// `S_INLINESITE` or `S_INLINESITE2`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct InlineSiteSymbol<'t> {
    pub parent: SymbolIndex,
    pub end: SymbolIndex,
    pub inlinee: ItemId,
    pub invocations: Option<u32>,
    pub annotations: BinaryAnnotations<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for InlineSiteSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = InlineSiteSymbol {
            parent: buf.parse()?,
            end: buf.parse()?,
            inlinee: buf.parse()?,
            invocations: match kind {
                S_INLINESITE2 => Some(buf.parse()?),
                _ => None,
            },
            annotations: BinaryAnnotations::new(buf.take(buf.len())?),
        };

        Ok((symbol, buf.pos()))
    }
}

/// The information parsed from a symbol record with kind
/// `S_OBJNAME`, or `S_OBJNAME_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ObjNameSymbol<'t> {
    pub signature: u32,
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ObjNameSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = ObjNameSymbol {
            signature: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// A version number refered to by `ExtendedCompileFlagsSymbol`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CompilerVersion {
    pub major: u16,
    pub minor: u16,
    pub build: u16,
    pub qfe: Option<u16>,
}

impl<'t> TryFromCtx<'t, bool> for CompilerVersion {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], has_qfe: bool) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let version = CompilerVersion {
            major: buf.parse()?,
            minor: buf.parse()?,
            build: buf.parse()?,
            qfe: if has_qfe { Some(buf.parse()?) } else { None },
        };

        Ok((version, buf.pos()))
    }
}

/// Compile flags declared in `ExtendedCompileFlagsSymbol`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ExtendedCompileFlags {
    /// Compiled for edit and continue.
    edit_and_continue: bool,
    /// Compiled without debugging info.
    no_debug_info: bool,
    /// Compiled with `LTCG`.
    link_time_codegen: bool,
    /// Compiled with `/bzalign`.
    no_data_align: bool,
    /// Managed code or data is present.
    managed: bool,
    /// Compiled with `/GS`.
    security_checks: bool,
    /// Compiled with `/hotpatch`.
    hot_patch: bool,
    /// Compiled with `CvtCIL`.
    cvtcil: bool,
    /// This is a MSIL .NET Module.
    msil_module: bool,
    /// Compiled with `/sdl`.
    sdl: bool,
    /// Compiled with `/ltcg:pgo` or `pgo:`.
    pgo: bool,
    /// This is a .exp module.
    exp_module: bool,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ExtendedCompileFlags {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let is_compile3 = kind == S_COMPILE3;

        let raw = this.pread_with::<u16>(0, LE)?;
        this.pread::<u8>(2)?; // unused

        let flags = ExtendedCompileFlags {
            edit_and_continue: raw & 1 != 0,
            no_debug_info: (raw >> 1) & 1 != 0,
            link_time_codegen: (raw >> 2) & 1 != 0,
            no_data_align: (raw >> 3) & 1 != 0,
            managed: (raw >> 4) & 1 != 0,
            security_checks: (raw >> 5) & 1 != 0,
            hot_patch: (raw >> 6) & 1 != 0,
            cvtcil: (raw >> 7) & 1 != 0,
            msil_module: (raw >> 8) & 1 != 0,
            sdl: (raw >> 9) & 1 != 0 && is_compile3,
            pgo: (raw >> 10) & 1 != 0 && is_compile3,
            exp_module: (raw >> 11) & 1 != 0 && is_compile3,
        };

        Ok((flags, 3))
    }
}

/// The information parsed from a symbol record with kind
/// `S_COMPILE2`, `S_COMPILE2_ST`, or `S_COMPILE3`
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ExtendedCompileFlagsSymbol<'t> {
    pub language: SourceLanguage,
    pub flags: ExtendedCompileFlags,
    pub cpu_type: CPUType,
    pub frontend_version: CompilerVersion,
    pub backend_version: CompilerVersion,
    pub version_string: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ExtendedCompileFlagsSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let has_qfe = kind == S_COMPILE3;
        let symbol = ExtendedCompileFlagsSymbol {
            language: buf.parse()?,
            flags: buf.parse_with(kind)?,
            cpu_type: buf.parse()?,
            frontend_version: buf.parse_with(has_qfe)?,
            backend_version: buf.parse_with(has_qfe)?,
            version_string: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// The information parsed from a symbol record with kind
/// `S_UNAMESPACE`, or `S_UNAMESPACE_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct UsingNamespaceSymbol<'t> {
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for UsingNamespaceSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = UsingNamespaceSymbol {
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

// CV_LVARFLAGS:
const CV_LVARFLAG_ISPARAM: u16 = 0x01;
const CV_LVARFLAG_ADDRTAKEN: u16 = 0x02;
const CV_LVARFLAG_COMPGENX: u16 = 0x04;
const CV_LVARFLAG_ISAGGREGATE: u16 = 0x08;
const CV_LVARFLAG_ISALIASED: u16 = 0x10;
const CV_LVARFLAG_ISALIAS: u16 = 0x20;
const CV_LVARFLAG_ISRETVALUE: u16 = 0x40;
const CV_LVARFLAG_ISOPTIMIZEDOUT: u16 = 0x80;
const CV_LVARFLAG_ISENREG_GLOB: u16 = 0x100;
const CV_LVARFLAG_ISENREG_STAT: u16 = 0x200;

/// The information parsed from a CV_LVARFLAGS bit field
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LocalVariableFlags {
    pub isparam: bool,        // Variable is a parameter
    pub addrtaken: bool,      // Address is taken
    pub compgenx: bool,       // Variable is compiler generated
    pub isaggregate: bool, // The symbol is splitted in temporaries, which are treated by compiler as independent entities
    pub isaliased: bool,   // Variable has multiple simultaneous lifetimes
    pub isalias: bool,     // Represents one of the multiple simultaneous lifetimes
    pub isretvalue: bool,  // Represents a function return value
    pub isoptimizedout: bool, // Variable has no lifetimes
    pub isenreg_glob: bool, // Variable is an enregistered global
    pub isenreg_stat: bool, // Variable is an enregistered static
}

impl<'t> TryFromCtx<'t, Endian> for LocalVariableFlags {
    type Error = scroll::Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], le: Endian) -> scroll::Result<(Self, Self::Size)> {
        let (value, size) = u16::try_from_ctx(this, le)?;

        let flags = LocalVariableFlags {
            isparam: value & CV_LVARFLAG_ISPARAM != 0,
            addrtaken: value & CV_LVARFLAG_ADDRTAKEN != 0,
            compgenx: value & CV_LVARFLAG_COMPGENX != 0,
            isaggregate: value & CV_LVARFLAG_ISAGGREGATE != 0,
            isaliased: value & CV_LVARFLAG_ISALIASED != 0,
            isalias: value & CV_LVARFLAG_ISALIAS != 0,
            isretvalue: value & CV_LVARFLAG_ISRETVALUE != 0,
            isoptimizedout: value & CV_LVARFLAG_ISOPTIMIZEDOUT != 0,
            isenreg_glob: value & CV_LVARFLAG_ISENREG_GLOB != 0,
            isenreg_stat: value & CV_LVARFLAG_ISENREG_STAT != 0,
        };

        Ok((flags, size))
    }
}

/// The information parsed from a symbol record with kind
/// `S_LOCAL`
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LocalSymbol<'t> {
    pub type_index: TypeIndex,
    pub flags: LocalVariableFlags,
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for LocalSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = LocalSymbol {
            type_index: buf.parse()?,
            flags: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

// https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L4456
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ExportSymbolFlags {
    pub constant: bool,
    pub data: bool,
    pub private: bool,
    pub no_name: bool,
    pub ordinal: bool,
    pub forwarder: bool,
}

impl<'t> TryFromCtx<'t, Endian> for ExportSymbolFlags {
    type Error = scroll::Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], le: Endian) -> scroll::Result<(Self, Self::Size)> {
        let (value, size) = u16::try_from_ctx(this, le)?;

        let flags = ExportSymbolFlags {
            constant: value & 0x01 != 0,
            data: value & 0x02 != 0,
            private: value & 0x04 != 0,
            no_name: value & 0x08 != 0,
            ordinal: value & 0x10 != 0,
            forwarder: value & 0x20 != 0,
        };

        Ok((flags, size))
    }
}

/// The information parsed from a symbol record with kind
/// `S_EXPORT`
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ExportSymbol<'t> {
    pub ordinal: u16,
    pub flags: ExportSymbolFlags,
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ExportSymbol<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = ExportSymbol {
            ordinal: buf.parse()?,
            flags: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// A `SymbolIter` iterates over a `SymbolTable`, producing `Symbol`s.
///
/// Symbol tables are represented internally as a series of records, each of which have a length, a
/// type, and a type-specific field layout. Iteration performance is therefore similar to a linked
/// list.
#[derive(Debug)]
pub struct SymbolIter<'t> {
    buf: ParseBuffer<'t>,
}

impl<'t> SymbolIter<'t> {
    pub(crate) fn new(buf: ParseBuffer<'t>) -> SymbolIter<'t> {
        SymbolIter { buf }
    }

    /// Move the iterator to the symbol referred to by `index`.
    ///
    /// This can be used to jump to the sibiling or parent of a symbol record.
    pub fn seek(&mut self, index: SymbolIndex) {
        // A symbol index of 0 referes to no symbol. Seek to the end of the iterator.
        let pos = match index.0 {
            0 => self.buf.pos() + self.buf.len(),
            pos => pos as usize,
        };

        self.buf.seek(pos);
    }

    /// Skip to the symbol referred to by `index`, returning the symbol.
    ///
    /// This can be used to jump to the sibiling or parent of a symbol record. Iteration continues
    /// after that symbol.
    ///
    /// Note that the symbol may be located **before** the originating symbol, for instance when
    /// jumping to the parent symbol. Take care not to enter an endless loop in this case.
    pub fn skip_to(&mut self, index: SymbolIndex) -> Result<Option<Symbol<'t>>> {
        self.seek(index);
        self.next()
    }
}

impl<'t> FallibleIterator for SymbolIter<'t> {
    type Item = Symbol<'t>;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        while !self.buf.is_empty() {
            // read the length of the next symbol
            let symbol_length = self.buf.parse::<u16>()? as usize;
            if symbol_length < 2 {
                // this can't be correct
                return Err(Error::SymbolTooShort);
            }

            // grab the symbol itself
            let data = self.buf.take(symbol_length)?;
            let symbol = Symbol(data);

            // skip over padding in the symbol table
            match symbol.raw_kind() {
                S_ALIGN | S_SKIP => continue,
                _ => return Ok(Some(symbol)),
            }
        }

        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    mod parsing {
        use crate::symbol::*;

        #[test]
        fn kind_110e() {
            let data = &[
                14, 17, 2, 0, 0, 0, 192, 85, 0, 0, 1, 0, 95, 95, 108, 111, 99, 97, 108, 95, 115,
                116, 100, 105, 111, 95, 112, 114, 105, 110, 116, 102, 95, 111, 112, 116, 105, 111,
                110, 115, 0, 0,
            ];

            let symbol = Symbol(data);
            assert_eq!(symbol.raw_kind(), 0x110e);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Public(PublicSymbol {
                    code: false,
                    function: true,
                    managed: false,
                    msil: false,
                    offset: PdbInternalSectionOffset {
                        offset: 21952,
                        section: 1
                    },
                    name: "__local_stdio_printf_options".into(),
                })
            );
        }

        #[test]
        fn kind_1125() {
            let data = &[
                37, 17, 0, 0, 0, 0, 108, 0, 0, 0, 1, 0, 66, 97, 122, 58, 58, 102, 95, 112, 117, 98,
                108, 105, 99, 0,
            ];
            let symbol = Symbol(data);
            assert_eq!(symbol.raw_kind(), 0x1125);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::ProcedureReference(ProcedureReferenceSymbol {
                    global: true,
                    sum_name: 0,
                    symbol_index: SymbolIndex(108),
                    module: 1,
                    name: Some("Baz::f_public".into()),
                })
            );
        }

        #[test]
        fn kind_1108() {
            let data = &[8, 17, 112, 6, 0, 0, 118, 97, 95, 108, 105, 115, 116, 0];
            let symbol = Symbol(data);
            assert_eq!(symbol.raw_kind(), 0x1108);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::UserDefinedType(UserDefinedTypeSymbol {
                    type_index: 1648,
                    name: "va_list".into(),
                })
            );
        }

        #[test]
        fn kind_1107() {
            let data = &[
                7, 17, 201, 18, 0, 0, 1, 0, 95, 95, 73, 83, 65, 95, 65, 86, 65, 73, 76, 65, 66, 76,
                69, 95, 83, 83, 69, 50, 0, 0,
            ];
            let symbol = Symbol(data);
            assert_eq!(symbol.raw_kind(), 0x1107);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Constant(ConstantSymbol {
                    managed: false,
                    type_index: 4809,
                    value: Variant::U16(1),
                    name: "__ISA_AVAILABLE_SSE2".into(),
                })
            );
        }

        #[test]
        fn kind_110d() {
            let data = &[
                13, 17, 116, 0, 0, 0, 16, 0, 0, 0, 3, 0, 95, 95, 105, 115, 97, 95, 97, 118, 97,
                105, 108, 97, 98, 108, 101, 0, 0, 0,
            ];
            let symbol = Symbol(data);
            assert_eq!(symbol.raw_kind(), 0x110d);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Data(DataSymbol {
                    global: true,
                    managed: false,
                    type_index: 116,
                    offset: PdbInternalSectionOffset {
                        offset: 16,
                        section: 3
                    },
                    name: "__isa_available".into(),
                })
            );
        }

        #[test]
        fn kind_110c() {
            let data = &[
                12, 17, 32, 0, 0, 0, 240, 36, 1, 0, 2, 0, 36, 120, 100, 97, 116, 97, 115, 121, 109,
                0,
            ];
            let symbol = Symbol(data);
            assert_eq!(symbol.raw_kind(), 0x110c);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Data(DataSymbol {
                    global: false,
                    managed: false,
                    type_index: 32,
                    offset: PdbInternalSectionOffset {
                        offset: 74992,
                        section: 2
                    },
                    name: "$xdatasym".into(),
                })
            );
        }

        #[test]
        fn kind_1127() {
            let data = &[
                39, 17, 0, 0, 0, 0, 128, 4, 0, 0, 182, 0, 99, 97, 112, 116, 117, 114, 101, 95, 99,
                117, 114, 114, 101, 110, 116, 95, 99, 111, 110, 116, 101, 120, 116, 0, 0, 0,
            ];
            let symbol = Symbol(data);
            assert_eq!(symbol.raw_kind(), 0x1127);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::ProcedureReference(ProcedureReferenceSymbol {
                    global: false,
                    sum_name: 0,
                    symbol_index: SymbolIndex(1152),
                    module: 182,
                    name: Some("capture_current_context".into()),
                })
            );
        }

        #[test]
        fn kind_1110() {
            let data = &[
                16, 17, 0, 0, 0, 0, 48, 2, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 7,
                16, 0, 0, 64, 85, 0, 0, 1, 0, 0, 66, 97, 122, 58, 58, 102, 95, 112, 114, 111, 116,
                101, 99, 116, 101, 100, 0,
            ];
            let symbol = Symbol(data);
            assert_eq!(symbol.raw_kind(), 0x1110);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Procedure(ProcedureSymbol {
                    global: true,
                    parent: SymbolIndex(0),
                    end: SymbolIndex(560),
                    next: SymbolIndex(0),
                    len: 6,
                    dbg_start_offset: 5,
                    dbg_end_offset: 5,
                    type_index: 4103,
                    offset: PdbInternalSectionOffset {
                        offset: 21824,
                        section: 1
                    },
                    flags: ProcedureFlags {
                        nofpo: false,
                        int: false,
                        far: false,
                        never: false,
                        notreached: false,
                        cust_call: false,
                        noinline: false,
                        optdbginfo: false
                    },
                    name: "Baz::f_protected".into(),
                })
            );
        }

        #[test]
        fn kind_110f() {
            let data = &[
                15, 17, 0, 0, 0, 0, 156, 1, 0, 0, 0, 0, 0, 0, 18, 0, 0, 0, 4, 0, 0, 0, 9, 0, 0, 0,
                128, 16, 0, 0, 196, 87, 0, 0, 1, 0, 128, 95, 95, 115, 99, 114, 116, 95, 99, 111,
                109, 109, 111, 110, 95, 109, 97, 105, 110, 0, 0, 0,
            ];
            let symbol = Symbol(data);
            assert_eq!(symbol.raw_kind(), 0x110f);
            assert_eq!(
                symbol.parse().expect("parse"),
                SymbolData::Procedure(ProcedureSymbol {
                    global: false,
                    parent: SymbolIndex(0),
                    end: SymbolIndex(412),
                    next: SymbolIndex(0),
                    len: 18,
                    dbg_start_offset: 4,
                    dbg_end_offset: 9,
                    type_index: 4224,
                    offset: PdbInternalSectionOffset {
                        offset: 22468,
                        section: 1
                    },
                    flags: ProcedureFlags {
                        nofpo: false,
                        int: false,
                        far: false,
                        never: false,
                        notreached: false,
                        cust_call: false,
                        noinline: false,
                        optdbginfo: true
                    },
                    name: "__scrt_common_main".into(),
                })
            );
        }
    }
}
