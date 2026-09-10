// Copyright 2026 PDB Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use core::fmt;

#[cfg(feature = "alloc")]
use alloc::vec::Vec;

use scroll::{ctx::TryFromCtx, Endian, Pread, LE};

use crate::{BinaryAnnotations, common::*, symbol::constants::*};

#[cfg(feature = "alloc")]
use crate::msf::*;

use crate::FallibleIterator;

/// The raw type discriminator for `Symbols`.
pub type SymbolKind = u16;

/// Represents a symbol from the symbol table.
///
/// A `Symbol` is represented internally as a `&[u8]`, and in general the bytes inside are not
/// inspected in any way before calling any of the accessor methods.
///
/// To avoid copying, `Symbol`s exist as references to data owned by the parent `SymbolTable`.
/// Therefore, a `Symbol` may not outlive its parent `SymbolTable`.
#[derive(Copy, Clone, PartialEq)]
pub struct Symbol<'t> {
    pub(crate) index: SymbolIndex,
    pub(crate) data: &'t [u8],
}

impl<'t> Symbol<'t> {
    /// The index of this symbol in the containing symbol stream.
    #[inline]
    pub fn index(&self) -> SymbolIndex {
        self.index
    }

    /// Returns the kind of symbol identified by this Symbol.
    #[inline]
    pub fn raw_kind(&self) -> SymbolKind {
        debug_assert!(self.data.len() >= 2);
        self.data.pread_with(0, LE).unwrap_or_default()
    }

    /// Returns the raw bytes of this symbol record, including the symbol type and extra data, but
    /// not including the preceding symbol length indicator.
    #[inline]
    pub fn raw_bytes(&self) -> &'t [u8] {
        self.data
    }

    /// Parse the symbol into the `SymbolData` it contains.
    #[inline]
    pub fn parse(&self) -> Result<SymbolData<'t>> {
        self.raw_bytes().pread_with(0, ())
    }

    /// Returns whether this symbol starts a scope.
    ///
    /// If `true`, this symbol has a `parent` and an `end` field, which contains the offset of the
    /// corrsponding end symbol.
    pub fn starts_scope(&self) -> bool {
        matches!(
            self.raw_kind(),
            S_GPROC16
                | S_GPROC32
                | S_GPROC32_ST
                | S_GPROCMIPS
                | S_GPROCMIPS_ST
                | S_GPROCIA64
                | S_GPROCIA64_ST
                | S_LPROC16
                | S_LPROC32
                | S_LPROC32_ST
                | S_LPROC32_DPC
                | S_LPROCMIPS
                | S_LPROCMIPS_ST
                | S_LPROCIA64
                | S_LPROCIA64_ST
                | S_LPROC32_DPC_ID
                | S_GPROC32_ID
                | S_GPROCMIPS_ID
                | S_GPROCIA64_ID
                | S_BLOCK16
                | S_BLOCK32
                | S_BLOCK32_ST
                | S_WITH16
                | S_WITH32
                | S_WITH32_ST
                | S_THUNK16
                | S_THUNK32
                | S_THUNK32_ST
                | S_SEPCODE
                | S_GMANPROC
                | S_GMANPROC_ST
                | S_LMANPROC
                | S_LMANPROC_ST
                | S_INLINESITE
                | S_INLINESITE2
        )
    }

    /// Returns whether this symbol declares the end of a scope.
    pub fn ends_scope(&self) -> bool {
        matches!(self.raw_kind(), S_END | S_PROC_ID_END | S_INLINESITE_END)
    }
}

impl<'t> fmt::Debug for Symbol<'t> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Symbol{{ kind: 0x{:x} [{} bytes] }}",
            self.raw_kind(),
            self.data.len()
        )
    }
}

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

fn parse_optional_index(buf: &mut ParseBuffer<'_>) -> Result<Option<SymbolIndex>> {
    Ok(match buf.parse()? {
        SymbolIndex(0) => None,
        index => Some(index),
    })
}

// data types are defined at:
//   https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L3038
// constants defined at:
//   https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2735
// decoding reference:
//   https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/cvdump/dumpsym7.cpp#L264

/// Information parsed from a [`Symbol`] record.
#[non_exhaustive]
#[derive(Clone, Debug, Eq, PartialEq)]
pub enum SymbolData<'t> {
    /// End of a scope, such as a procedure.
    ScopeEnd,
    /// Name of the object file of this module.
    ObjName(ObjNameSymbol<'t>),
    /// A Register variable.
    RegisterVariable(RegisterVariableSymbol<'t>),
    /// A constant value.
    Constant(ConstantSymbol<'t>),
    /// A user defined type.
    UserDefinedType(UserDefinedTypeSymbol<'t>),
    #[cfg(feature = "alloc")]
    /// A Register variable spanning multiple registers.
    MultiRegisterVariable(MultiRegisterVariableSymbol<'t>),
    /// Static data, such as a global variable.
    Data(DataSymbol<'t>),
    /// A public symbol with a mangled name.
    Public(PublicSymbol<'t>),
    /// A procedure, such as a function or method.
    Procedure(ProcedureSymbol<'t>),
    /// A thread local variable.
    ThreadStorage(ThreadStorageSymbol<'t>),
    /// Flags used to compile a module.
    CompileFlags(CompileFlagsSymbol<'t>),
    /// A using namespace directive.
    UsingNamespace(UsingNamespaceSymbol<'t>),
    /// Reference to a [`ProcedureSymbol`].
    ProcedureReference(ProcedureReferenceSymbol<'t>),
    /// Reference to an imported variable.
    DataReference(DataReferenceSymbol<'t>),
    /// Reference to an annotation.
    AnnotationReference(AnnotationReferenceSymbol<'t>),
    /// Trampoline thunk.
    Trampoline(TrampolineSymbol),
    /// An exported symbol.
    Export(ExportSymbol<'t>),
    /// A local symbol in optimized code.
    Local(LocalSymbol<'t>),
    /// Reference to build information.
    BuildInfo(BuildInfoSymbol),
    /// The callsite of an inlined function.
    InlineSite(InlineSiteSymbol<'t>),
    /// End of an inline callsite.
    InlineSiteEnd,
    /// End of a procedure.
    ProcedureEnd,
    /// A label.
    Label(LabelSymbol<'t>),
    /// A block.
    Block(BlockSymbol<'t>),
    /// Data allocated relative to a register.
    RegisterRelative(RegisterRelativeSymbol<'t>),
    /// A thunk.
    Thunk(ThunkSymbol<'t>),
    /// A block of separated code.
    SeparatedCode(SeparatedCodeSymbol),
}

impl<'t> SymbolData<'t> {
    /// Returns the name of this symbol if it has one.
    pub fn name(&self) -> Option<RawString<'t>> {
        match self {
            Self::ScopeEnd => None,
            Self::ObjName(data) => Some(data.name),
            Self::RegisterVariable(_) => None,
            Self::Constant(data) => Some(data.name),
            Self::UserDefinedType(data) => Some(data.name),
            #[cfg(feature = "alloc")]
            Self::MultiRegisterVariable(_) => None,
            Self::Data(data) => Some(data.name),
            Self::Public(data) => Some(data.name),
            Self::Procedure(data) => Some(data.name),
            Self::ThreadStorage(data) => Some(data.name),
            Self::CompileFlags(_) => None,
            Self::UsingNamespace(data) => Some(data.name),
            Self::ProcedureReference(data) => data.name,
            Self::DataReference(data) => data.name,
            Self::AnnotationReference(data) => Some(data.name),
            Self::Trampoline(_) => None,
            Self::Export(data) => Some(data.name),
            Self::Local(data) => Some(data.name),
            Self::InlineSite(_) => None,
            Self::BuildInfo(_) => None,
            Self::InlineSiteEnd => None,
            Self::ProcedureEnd => None,
            Self::Label(data) => Some(data.name),
            Self::Block(data) => Some(data.name),
            Self::RegisterRelative(data) => Some(data.name),
            Self::Thunk(data) => Some(data.name),
            Self::SeparatedCode(_) => None,
        }
    }
}

impl<'t> TryFromCtx<'t> for SymbolData<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], _ctx: ()) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);
        let kind = buf.parse()?;

        let symbol = match kind {
            S_END => SymbolData::ScopeEnd,
            S_OBJNAME | S_OBJNAME_ST => SymbolData::ObjName(buf.parse_with(kind)?),
            S_REGISTER | S_REGISTER_ST => SymbolData::RegisterVariable(buf.parse_with(kind)?),
            S_CONSTANT | S_CONSTANT_ST | S_MANCONSTANT => {
                SymbolData::Constant(buf.parse_with(kind)?)
            }
            S_UDT | S_UDT_ST | S_COBOLUDT | S_COBOLUDT_ST => {
                SymbolData::UserDefinedType(buf.parse_with(kind)?)
            }
            S_MANYREG | S_MANYREG_ST | S_MANYREG2 | S_MANYREG2_ST => {
                #[cfg(feature = "alloc")]
                {
                    SymbolData::MultiRegisterVariable(buf.parse_with(kind)?)
                }
                #[cfg(not(feature = "alloc"))]
                {
                    return Err(Error::UnimplementedFeature("MultiRegisterVariable requires alloc"));
                }
            }
            S_LDATA32 | S_LDATA32_ST | S_GDATA32 | S_GDATA32_ST | S_LMANDATA | S_LMANDATA_ST
            | S_GMANDATA | S_GMANDATA_ST => SymbolData::Data(buf.parse_with(kind)?),
            S_PUB32 | S_PUB32_ST => SymbolData::Public(buf.parse_with(kind)?),
            S_LPROC32 | S_LPROC32_ST | S_GPROC32 | S_GPROC32_ST | S_LPROC32_ID | S_GPROC32_ID
            | S_LPROC32_DPC | S_LPROC32_DPC_ID => SymbolData::Procedure(buf.parse_with(kind)?),
            S_LTHREAD32 | S_LTHREAD32_ST | S_GTHREAD32 | S_GTHREAD32_ST => {
                SymbolData::ThreadStorage(buf.parse_with(kind)?)
            }
            S_COMPILE2 | S_COMPILE2_ST | S_COMPILE3 => {
                SymbolData::CompileFlags(buf.parse_with(kind)?)
            }
            S_UNAMESPACE | S_UNAMESPACE_ST => SymbolData::UsingNamespace(buf.parse_with(kind)?),
            S_PROCREF | S_PROCREF_ST | S_LPROCREF | S_LPROCREF_ST => {
                SymbolData::ProcedureReference(buf.parse_with(kind)?)
            }
            S_TRAMPOLINE => Self::Trampoline(buf.parse_with(kind)?),
            S_DATAREF | S_DATAREF_ST => SymbolData::DataReference(buf.parse_with(kind)?),
            S_ANNOTATIONREF => SymbolData::AnnotationReference(buf.parse_with(kind)?),
            S_EXPORT => SymbolData::Export(buf.parse_with(kind)?),
            S_LOCAL => SymbolData::Local(buf.parse_with(kind)?),
            S_BUILDINFO => SymbolData::BuildInfo(buf.parse_with(kind)?),
            S_INLINESITE | S_INLINESITE2 => SymbolData::InlineSite(buf.parse_with(kind)?),
            S_INLINESITE_END => SymbolData::InlineSiteEnd,
            S_PROC_ID_END => SymbolData::ProcedureEnd,
            S_LABEL32 | S_LABEL32_ST => SymbolData::Label(buf.parse_with(kind)?),
            S_BLOCK32 | S_BLOCK32_ST => SymbolData::Block(buf.parse_with(kind)?),
            S_REGREL32 => SymbolData::RegisterRelative(buf.parse_with(kind)?),
            S_THUNK32 | S_THUNK32_ST => SymbolData::Thunk(buf.parse_with(kind)?),
            S_SEPCODE => SymbolData::SeparatedCode(buf.parse_with(kind)?),
            other => return Err(Error::UnimplementedSymbolKind(other)),
        };

        Ok((symbol, buf.pos()))
    }
}

/// A Register variable.
///
/// Symbol kind `S_REGISTER`, or `S_REGISTER_ST`
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RegisterVariableSymbol<'t> {
    /// Identifier of the variable type.
    pub type_index: TypeIndex,
    /// The register this variable is stored in.
    pub register: Register,
    /// Name of the variable.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for RegisterVariableSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
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
/// Symbol kind `S_MANYREG`, `S_MANYREG_ST`, `S_MANYREG2`, or `S_MANYREG2_ST`.
#[cfg(feature = "alloc")]
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MultiRegisterVariableSymbol<'t> {
    /// Identifier of the variable type.
    pub type_index: TypeIndex,
    /// Most significant register first.
    pub registers: Vec<(Register, RawString<'t>)>,
}

#[cfg(feature = "alloc")]
impl<'t> TryFromCtx<'t, SymbolKind> for MultiRegisterVariableSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
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

/// A public symbol with a mangled name.
///
/// Symbol kind `S_PUB32`, or `S_PUB32_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct PublicSymbol<'t> {
    /// The public symbol refers to executable code.
    pub code: bool,
    /// The public symbol is a function.
    pub function: bool,
    /// The symbol is in managed code (native or IL).
    pub managed: bool,
    /// The symbol is managed IL code.
    pub msil: bool,
    /// Start offset of the symbol.
    pub offset: PdbInternalSectionOffset,
    /// Mangled name of the symbol.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for PublicSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
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

/// Static data, such as a global variable.
///
/// Symbol kinds:
///  - `S_LDATA32` and `S_LDATA32_ST` for local unmanaged data
///  - `S_GDATA32` and `S_GDATA32_ST` for global unmanaged data
///  - `S_LMANDATA32` and `S_LMANDATA32_ST` for local managed data
///  - `S_GMANDATA32` and `S_GMANDATA32_ST` for global managed data
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DataSymbol<'t> {
    /// Whether this data is global or local.
    pub global: bool,
    /// Whether this data is managed or unmanaged.
    pub managed: bool,
    /// Type identifier of the type of data.
    pub type_index: TypeIndex,
    /// Code offset of the start of the data region.
    pub offset: PdbInternalSectionOffset,
    /// Name of the data variable.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for DataSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = DataSymbol {
            global: matches!(kind, S_GDATA32 | S_GDATA32_ST | S_GMANDATA | S_GMANDATA_ST),
            managed: matches!(
                kind,
                S_LMANDATA | S_LMANDATA_ST | S_GMANDATA | S_GMANDATA_ST
            ),
            type_index: buf.parse()?,
            offset: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// Reference to an imported procedure.
///
/// Symbol kind `S_PROCREF`, `S_PROCREF_ST`, `S_LPROCREF`, or `S_LPROCREF_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProcedureReferenceSymbol<'t> {
    /// Whether the referenced procedure is global or local.
    pub global: bool,
    /// SUC of the name.
    pub sum_name: u32,
    /// Symbol index of the referenced [`ProcedureSymbol`].
    ///
    /// Note that this symbol might be located in a different module.
    pub symbol_index: SymbolIndex,
    /// Index of the module in [`DebugInformation::modules`](crate::DebugInformation::modules)
    /// containing the actual symbol.
    pub module: Option<usize>,
    /// Name of the procedure reference.
    pub name: Option<RawString<'t>>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ProcedureReferenceSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = ProcedureReferenceSymbol {
            global: matches!(kind, S_PROCREF | S_PROCREF_ST),
            sum_name: buf.parse()?,
            symbol_index: buf.parse()?,
            module: buf.parse::<u16>()?.checked_sub(1).map(usize::from),
            name: parse_optional_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// Reference to an imported variable.
///
/// Symbol kind `S_DATAREF`, or `S_DATAREF_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct DataReferenceSymbol<'t> {
    /// SUC of the name.
    pub sum_name: u32,
    /// Symbol index of the referenced [`DataSymbol`].
    ///
    /// Note that this symbol might be located in a different module.
    pub symbol_index: SymbolIndex,
    /// Index of the module in [`DebugInformation::modules`](crate::DebugInformation::modules)
    /// containing the actual symbol.
    pub module: Option<usize>,
    /// Name of the data reference.
    pub name: Option<RawString<'t>>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for DataReferenceSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = DataReferenceSymbol {
            sum_name: buf.parse()?,
            symbol_index: buf.parse()?,
            module: buf.parse::<u16>()?.checked_sub(1).map(usize::from),
            name: parse_optional_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// Reference to an annotation.
///
/// Symbol kind `S_ANNOTATIONREF`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct AnnotationReferenceSymbol<'t> {
    /// SUC of the name.
    pub sum_name: u32,
    /// Symbol index of the referenced symbol.
    ///
    /// Note that this symbol might be located in a different module.
    pub symbol_index: SymbolIndex,
    /// Index of the module in [`DebugInformation::modules`](crate::DebugInformation::modules)
    /// containing the actual symbol.
    pub module: Option<usize>,
    /// Name of the annotation reference.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for AnnotationReferenceSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = AnnotationReferenceSymbol {
            sum_name: buf.parse()?,
            symbol_index: buf.parse()?,
            module: buf.parse::<u16>()?.checked_sub(1).map(usize::from),
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// Subtype of [`TrampolineSymbol`].
#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TrampolineType {
    /// An incremental thunk.
    Incremental,
    /// Branch island thunk.
    BranchIsland,
    /// An unknown thunk type.
    Unknown,
}

/// Trampoline thunk.
///
/// Symbol kind `S_TRAMPOLINE`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct TrampolineSymbol {
    /// Trampoline symbol subtype.
    pub tramp_type: TrampolineType,
    /// Code size of the thunk.
    pub size: u16,
    /// Code offset of the thunk.
    pub thunk: PdbInternalSectionOffset,
    /// Code offset of the thunk target.
    pub target: PdbInternalSectionOffset,
}

impl TryFromCtx<'_, SymbolKind> for TrampolineSymbol {
    type Error = Error;

    fn try_from_ctx(this: &'_ [u8], _kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let tramp_type = match buf.parse::<u16>()? {
            0x00 => TrampolineType::Incremental,
            0x01 => TrampolineType::BranchIsland,
            _ => TrampolineType::Unknown,
        };

        let size = buf.parse()?;
        let thunk_offset = buf.parse()?;
        let target_offset = buf.parse()?;
        let thunk_section = buf.parse()?;
        let target_section = buf.parse()?;

        let symbol = Self {
            tramp_type,
            size,
            thunk: PdbInternalSectionOffset::new(thunk_section, thunk_offset),
            target: PdbInternalSectionOffset::new(target_section, target_offset),
        };

        Ok((symbol, buf.pos()))
    }
}

/// A constant value.
///
/// Symbol kind `S_CONSTANT`, or `S_CONSTANT_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ConstantSymbol<'t> {
    /// Whether this constant has metadata type information.
    pub managed: bool,
    /// The type of this constant or metadata token.
    pub type_index: TypeIndex,
    /// The value of this constant.
    pub value: Variant,
    /// Name of the constant.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ConstantSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
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

/// A user defined type.
///
/// Symbol kind `S_UDT`, or `S_UDT_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct UserDefinedTypeSymbol<'t> {
    /// Identifier of the type.
    pub type_index: TypeIndex,
    /// Name of the type.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for UserDefinedTypeSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = UserDefinedTypeSymbol {
            type_index: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// A thread local variable.
///
/// Symbol kinds:
///  - `S_LTHREAD32`, `S_LTHREAD32_ST` for local thread storage.
///  - `S_GTHREAD32`, or `S_GTHREAD32_ST` for global thread storage.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ThreadStorageSymbol<'t> {
    /// Whether this is a global or local thread storage.
    pub global: bool,
    /// Identifier of the stored type.
    pub type_index: TypeIndex,
    /// Code offset of the thread local.
    pub offset: PdbInternalSectionOffset,
    /// Name of the thread local.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ThreadStorageSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = ThreadStorageSymbol {
            global: matches!(kind, S_GTHREAD32 | S_GTHREAD32_ST),
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

/// Flags of a [`ProcedureSymbol`].
#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProcedureFlags {
    /// Frame pointer is present (not omitted).
    pub nofpo: bool,
    /// Interrupt return.
    pub int: bool,
    /// Far return.
    pub far: bool,
    /// Procedure does not return.
    pub never: bool,
    /// Procedure is never called.
    pub notreached: bool,
    /// Custom calling convention.
    pub cust_call: bool,
    /// Marked as `noinline`.
    pub noinline: bool,
    /// Debug information for optimized code is present.
    pub optdbginfo: bool,
}

impl<'t> TryFromCtx<'t, Endian> for ProcedureFlags {
    type Error = scroll::Error;

    fn try_from_ctx(this: &'t [u8], le: Endian) -> scroll::Result<(Self, usize)> {
        let (value, size) = u8::try_from_ctx(this, le)?;

        let flags = Self {
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

/// A procedure, such as a function or method.
///
/// Symbol kinds:
///  - `S_GPROC32`, `S_GPROC32_ST` for global procedures
///  - `S_LPROC32`, `S_LPROC32_ST` for local procedures
///  - `S_LPROC32_DPC` for DPC procedures
///  - `S_GPROC32_ID`, `S_LPROC32_ID`, `S_LPROC32_DPC_ID` for procedures referencing types from the
///    ID stream rather than the Type stream.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ProcedureSymbol<'t> {
    /// Whether this is a global or local procedure.
    pub global: bool,
    /// Indicates Deferred Procedure Calls (DPC).
    pub dpc: bool,
    /// The parent scope that this procedure is nested in.
    pub parent: Option<SymbolIndex>,
    /// The end symbol of this procedure.
    pub end: SymbolIndex,
    /// The next procedure symbol.
    pub next: Option<SymbolIndex>,
    /// The length of the code block covered by this procedure.
    pub len: u32,
    /// Start offset of the procedure's body code, which marks the end of the prologue.
    pub dbg_start_offset: u32,
    /// End offset of the procedure's body code, which marks the start of the epilogue.
    pub dbg_end_offset: u32,
    /// Identifier of the procedure type.
    ///
    /// The type contains the complete signature, including parameters, modifiers and the return
    /// type.
    pub type_index: TypeIndex,
    /// Code offset of the start of this procedure.
    pub offset: PdbInternalSectionOffset,
    /// Detailed flags of this procedure.
    pub flags: ProcedureFlags,
    /// The full, demangled name of the procedure.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ProcedureSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = ProcedureSymbol {
            global: matches!(kind, S_GPROC32 | S_GPROC32_ST | S_GPROC32_ID),
            dpc: matches!(kind, S_LPROC32_DPC | S_LPROC32_DPC_ID),
            parent: parse_optional_index(&mut buf)?,
            end: buf.parse()?,
            next: parse_optional_index(&mut buf)?,
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

/// The callsite of an inlined function.
///
/// Symbol kind `S_INLINESITE`, or `S_INLINESITE2`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct InlineSiteSymbol<'t> {
    /// Index of the parent function.
    ///
    /// This might either be a [`ProcedureSymbol`] or another `InlineSiteSymbol`.
    pub parent: Option<SymbolIndex>,
    /// The end symbol of this callsite.
    pub end: SymbolIndex,
    /// Identifier of the type describing the inline function.
    pub inlinee: IdIndex,
    /// The total number of invocations of the inline function.
    pub invocations: Option<u32>,
    /// Binary annotations containing the line program of this call site.
    pub annotations: BinaryAnnotations<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for InlineSiteSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = InlineSiteSymbol {
            parent: parse_optional_index(&mut buf)?,
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

/// Reference to build information.
///
/// Symbol kind `S_BUILDINFO`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct BuildInfoSymbol {
    /// Index of the build information record.
    pub id: IdIndex,
}

impl<'t> TryFromCtx<'t, SymbolKind> for BuildInfoSymbol {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], _kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = Self { id: buf.parse()? };

        Ok((symbol, buf.pos()))
    }
}

/// Name of the object file of this module.
///
/// Symbol kind `S_OBJNAME`, or `S_OBJNAME_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ObjNameSymbol<'t> {
    /// Signature.
    pub signature: u32,
    /// Path to the object file.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ObjNameSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = ObjNameSymbol {
            signature: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// A version number refered to by `CompileFlagsSymbol`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CompilerVersion {
    /// The major version number.
    pub major: u16,
    /// The minor version number.
    pub minor: u16,
    /// The build (patch) version number.
    pub build: u16,
    /// The QFE (quick fix engineering) number.
    pub qfe: Option<u16>,
}

impl<'t> TryFromCtx<'t, bool> for CompilerVersion {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], has_qfe: bool) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let version = Self {
            major: buf.parse()?,
            minor: buf.parse()?,
            build: buf.parse()?,
            qfe: if has_qfe { Some(buf.parse()?) } else { None },
        };

        Ok((version, buf.pos()))
    }
}

/// Compile flags declared in `CompileFlagsSymbol`.
#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CompileFlags {
    /// Compiled for edit and continue.
    pub edit_and_continue: bool,
    /// Compiled without debugging info.
    pub no_debug_info: bool,
    /// Compiled with `LTCG`.
    pub link_time_codegen: bool,
    /// Compiled with `/bzalign`.
    pub no_data_align: bool,
    /// Managed code or data is present.
    pub managed: bool,
    /// Compiled with `/GS`.
    pub security_checks: bool,
    /// Compiled with `/hotpatch`.
    pub hot_patch: bool,
    /// Compiled with `CvtCIL`.
    pub cvtcil: bool,
    /// This is a MSIL .NET Module.
    pub msil_module: bool,
    /// Compiled with `/sdl`.
    pub sdl: bool,
    /// Compiled with `/ltcg:pgo` or `pgo:`.
    pub pgo: bool,
    /// This is a .exp module.
    pub exp_module: bool,
}

impl<'t> TryFromCtx<'t, SymbolKind> for CompileFlags {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let is_compile3 = kind == S_COMPILE3;

        let raw = this.pread_with::<u16>(0, LE)?;
        this.pread::<u8>(2)?; // unused

        let flags = Self {
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

/// Flags used to compile a module.
///
/// Symbol kind `S_COMPILE2`, `S_COMPILE2_ST`, or `S_COMPILE3`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct CompileFlagsSymbol<'t> {
    /// The source code language.
    pub language: SourceLanguage,
    /// Compiler flags.
    pub flags: CompileFlags,
    /// Machine type of the compilation target.
    pub cpu_type: CPUType,
    /// Version of the compiler frontend.
    pub frontend_version: CompilerVersion,
    /// Version of the compiler backend.
    pub backend_version: CompilerVersion,
    /// Display name of the compiler.
    pub version_string: RawString<'t>,
    // TODO: Command block for S_COMPILE2?
}

impl<'t> TryFromCtx<'t, SymbolKind> for CompileFlagsSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let has_qfe = kind == S_COMPILE3;
        let symbol = CompileFlagsSymbol {
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

/// A using namespace directive.
///
/// Symbol kind `S_UNAMESPACE`, or `S_UNAMESPACE_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct UsingNamespaceSymbol<'t> {
    /// The name of the imported namespace.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for UsingNamespaceSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
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

/// Flags for a [`LocalSymbol`].
#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LocalVariableFlags {
    /// Variable is a parameter.
    pub isparam: bool,
    /// Address is taken.
    pub addrtaken: bool,
    /// Variable is compiler generated.
    pub compgenx: bool,
    /// The symbol is splitted in temporaries, which are treated by compiler as independent
    /// entities.
    pub isaggregate: bool,
    /// Variable has multiple simultaneous lifetimes.
    pub isaliased: bool,
    /// Represents one of the multiple simultaneous lifetimes.
    pub isalias: bool,
    /// Represents a function return value.
    pub isretvalue: bool,
    /// Variable has no lifetimes.
    pub isoptimizedout: bool,
    /// Variable is an enregistered global.
    pub isenreg_glob: bool,
    /// Variable is an enregistered static.
    pub isenreg_stat: bool,
}

impl<'t> TryFromCtx<'t, Endian> for LocalVariableFlags {
    type Error = scroll::Error;

    fn try_from_ctx(this: &'t [u8], le: Endian) -> scroll::Result<(Self, usize)> {
        let (value, size) = u16::try_from_ctx(this, le)?;

        let flags = Self {
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

/// A local symbol in optimized code.
///
/// Symbol kind `S_LOCAL`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LocalSymbol<'t> {
    /// The type of the symbol.
    pub type_index: TypeIndex,
    /// Flags for this symbol.
    pub flags: LocalVariableFlags,
    /// Name of the symbol.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for LocalSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
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
/// Flags of an [`ExportSymbol`].
#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ExportSymbolFlags {
    /// An exported constant.
    pub constant: bool,
    /// Exported data (e.g. a static variable).
    pub data: bool,
    /// A private symbol.
    pub private: bool,
    /// A symbol with no name.
    pub no_name: bool,
    /// Ordinal was explicitly assigned.
    pub ordinal: bool,
    /// This is a forwarder.
    pub forwarder: bool,
}

impl<'t> TryFromCtx<'t, Endian> for ExportSymbolFlags {
    type Error = scroll::Error;

    fn try_from_ctx(this: &'t [u8], le: Endian) -> scroll::Result<(Self, usize)> {
        let (value, size) = u16::try_from_ctx(this, le)?;

        let flags = Self {
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

/// An exported symbol.
///
/// Symbol kind `S_EXPORT`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ExportSymbol<'t> {
    /// Ordinal of the symbol.
    pub ordinal: u16,
    /// Flags declaring the type of the exported symbol.
    pub flags: ExportSymbolFlags,
    /// The name of the exported symbol.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ExportSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = ExportSymbol {
            ordinal: buf.parse()?,
            flags: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// A label symbol.
///
/// Symbol kind `S_LABEL32`, `S_LABEL16`, or `S_LABEL32_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct LabelSymbol<'t> {
    /// Code offset of the start of this label.
    pub offset: PdbInternalSectionOffset,
    /// Detailed flags of this label.
    pub flags: ProcedureFlags,
    /// Name of the symbol.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for LabelSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = LabelSymbol {
            offset: buf.parse()?,
            flags: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// A block symbol.
///
/// Symbol kind `S_BLOCK32`, or `S_BLOCK32_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct BlockSymbol<'t> {
    /// The parent scope that this block is nested in.
    pub parent: SymbolIndex,
    /// The end symbol of this block.
    pub end: SymbolIndex,
    /// The length of the block.
    pub len: u32,
    /// Code offset of the start of this label.
    pub offset: PdbInternalSectionOffset,
    /// The block name.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for BlockSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = BlockSymbol {
            parent: buf.parse()?,
            end: buf.parse()?,
            len: buf.parse()?,
            offset: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// A register relative symbol.
///
/// The address of the variable is the value in the register + offset (e.g. %EBP + 8).
///
/// Symbol kind `S_REGREL32`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RegisterRelativeSymbol<'t> {
    /// The variable offset.
    pub offset: i32,
    /// The type of the variable.
    pub type_index: TypeIndex,
    /// The register this variable address is relative to.
    pub register: Register,
    /// The variable name.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for RegisterRelativeSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let symbol = RegisterRelativeSymbol {
            offset: buf.parse()?,
            type_index: buf.parse()?,
            register: buf.parse()?,
            name: parse_symbol_name(&mut buf, kind)?,
        };

        Ok((symbol, buf.pos()))
    }
}

/// Thunk adjustor
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ThunkAdjustor<'t> {
    delta: u16,
    target: RawString<'t>,
}

/// A thunk kind
#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum ThunkKind<'t> {
    /// Standard thunk
    NoType,
    /// "this" adjustor thunk with delta and target
    Adjustor(ThunkAdjustor<'t>),
    /// Virtual call thunk with table entry
    VCall(u16),
    /// pcode thunk
    PCode,
    /// thunk which loads the address to jump to via unknown means...
    Load,
    /// Unknown with ordinal value
    Unknown(u8),
}

/// A thunk symbol.
///
/// Symbol kind `S_THUNK32`, or `S_THUNK32_ST`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct ThunkSymbol<'t> {
    /// The parent scope that this thunk is nested in.
    pub parent: Option<SymbolIndex>,
    /// The end symbol of this thunk.
    pub end: SymbolIndex,
    /// The next symbol.
    pub next: Option<SymbolIndex>,
    /// Code offset of the start of this label.
    pub offset: PdbInternalSectionOffset,
    /// The length of the thunk.
    pub len: u16,
    /// The kind of the thunk.
    pub kind: ThunkKind<'t>,
    /// The thunk name.
    pub name: RawString<'t>,
}

impl<'t> TryFromCtx<'t, SymbolKind> for ThunkSymbol<'t> {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], kind: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let parent = parse_optional_index(&mut buf)?;
        let end = buf.parse()?;
        let next = parse_optional_index(&mut buf)?;
        let offset = buf.parse()?;
        let len = buf.parse()?;
        let ord = buf.parse::<u8>()?;
        let name = parse_symbol_name(&mut buf, kind)?;

        let kind = match ord {
            0 => ThunkKind::NoType,
            1 => ThunkKind::Adjustor(ThunkAdjustor {
                delta: buf.parse::<u16>()?,
                target: buf.parse_cstring()?,
            }),
            2 => ThunkKind::VCall(buf.parse::<u16>()?),
            3 => ThunkKind::PCode,
            4 => ThunkKind::Load,
            ord => ThunkKind::Unknown(ord),
        };

        let symbol = ThunkSymbol {
            parent,
            end,
            next,
            offset,
            len,
            kind,
            name,
        };

        Ok((symbol, buf.pos()))
    }
}

// CV_SEPCODEFLAGS:
const CV_SEPCODEFLAG_IS_LEXICAL_SCOPE: u32 = 0x01;
const CV_SEPCODEFLAG_RETURNS_TO_PARENT: u32 = 0x02;

/// Flags for a [`SeparatedCodeSymbol`].
#[non_exhaustive]
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SeparatedCodeFlags {
    /// S_SEPCODE doubles as lexical scope.
    pub islexicalscope: bool,
    /// code frag returns to parent.
    pub returnstoparent: bool,
}

impl<'t> TryFromCtx<'t, Endian> for SeparatedCodeFlags {
    type Error = scroll::Error;

    fn try_from_ctx(this: &'t [u8], le: Endian) -> scroll::Result<(Self, usize)> {
        let (value, size) = u32::try_from_ctx(this, le)?;

        let flags = Self {
            islexicalscope: value & CV_SEPCODEFLAG_IS_LEXICAL_SCOPE != 0,
            returnstoparent: value & CV_SEPCODEFLAG_RETURNS_TO_PARENT != 0,
        };

        Ok((flags, size))
    }
}

/// A separated code symbol.
///
/// Symbol kind `S_SEPCODE`.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct SeparatedCodeSymbol {
    /// The parent scope that this block is nested in.
    pub parent: SymbolIndex,
    /// The end symbol of this block.
    pub end: SymbolIndex,
    /// The length of the block.
    pub len: u32,
    /// Flags for this symbol
    pub flags: SeparatedCodeFlags,
    /// Code offset of the start of the separated code.
    pub offset: PdbInternalSectionOffset,
    /// Parent offset.
    pub parent_offset: PdbInternalSectionOffset,
}

impl<'t> TryFromCtx<'t, SymbolKind> for SeparatedCodeSymbol {
    type Error = Error;

    fn try_from_ctx(this: &'t [u8], _: SymbolKind) -> Result<(Self, usize)> {
        let mut buf = ParseBuffer::from(this);

        let parent = buf.parse()?;
        let end = buf.parse()?;
        let len = buf.parse()?;
        let flags = buf.parse()?;
        let offset = buf.parse()?;
        let parent_offset = buf.parse()?;
        let section = buf.parse()?;
        let parent_section = buf.parse()?;

        let symbol = Self {
            parent,
            end,
            len,
            flags,
            offset: PdbInternalSectionOffset { offset, section },
            parent_offset: PdbInternalSectionOffset {
                offset: parent_offset,
                section: parent_section,
            },
        };

        Ok((symbol, buf.pos()))
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
#[cfg(feature = "alloc")]
#[derive(Debug)]
pub struct SymbolTable<'s>(Stream<'s>);

#[cfg(feature = "alloc")]
impl<'s> SymbolTable<'s> {
    /// Parses a symbol table from raw stream data.
    pub(crate) fn new(stream: Stream<'s>) -> Self {
        SymbolTable(stream)
    }

    /// Returns an iterator that can traverse the symbol table in sequential order.
    pub fn iter(&self) -> SymbolIter<'_> {
        SymbolIter::new(self.0.parse_buffer())
    }

    /// Returns an iterator over symbols starting at the given index.
    pub fn iter_at(&self, index: SymbolIndex) -> SymbolIter<'_> {
        let mut iter = self.iter();
        iter.seek(index);
        iter
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
        self.buf.seek(index.0 as usize);
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
            let index = SymbolIndex(self.buf.pos() as u32);

            // read the length of the next symbol
            let symbol_length = self.buf.parse::<u16>()? as usize;
            if symbol_length < 2 {
                // this can't be correct
                return Err(Error::SymbolTooShort);
            }

            // grab the symbol itself
            let data = self.buf.take(symbol_length)?;
            let symbol = Symbol { index, data };

            // skip over padding in the symbol table
            match symbol.raw_kind() {
                S_ALIGN | S_SKIP => continue,
                _ => return Ok(Some(symbol)),
            }
        }

        Ok(None)
    }
}