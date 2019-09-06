use scroll::ctx::TryFromCtx;

use crate::common::*;
use crate::tpi::constants::*;

#[inline]
fn parse_optional_id_index<'t>(buf: &mut ParseBuffer<'t>) -> Result<Option<IdIndex>> {
    Ok(match buf.parse()? {
        IdIndex(0) => None,
        index => Some(index),
    })
}

#[inline]
fn parse_string<'t>(leaf: u16, buf: &mut ParseBuffer<'t>) -> Result<RawString<'t>> {
    if leaf > LF_ST_MAX {
        buf.parse_cstring()
    } else {
        buf.parse_u8_pascal_string()
    }
}

/// Encapsulates parsed data about an `Id`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IdData<'t> {
    /// Global function, usually inlined.
    Function(FunctionId<'t>),
    /// Member function, usually inlined.
    MemberFunction(MemberFunctionId<'t>),
    /// Tool, version and command line build information.
    BuildInfo(BuildInfoId),
    /// A list of substrings.
    StringList(StringListId),
    /// A string.
    String(StringId<'t>),
    /// Source and line of the definition of a User Defined Type (UDT).
    UserDefinedTypeSource(UserDefinedTypeSourceId),
}

impl<'t> IdData<'t> {}

impl<'t> TryFromCtx<'t, scroll::Endian> for IdData<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], _ctx: scroll::Endian) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);
        let leaf = buf.parse_u16()?;

        let data = match leaf {
            LF_FUNC_ID => IdData::Function(FunctionId {
                scope: parse_optional_id_index(&mut buf)?,
                function_type: buf.parse()?,
                name: parse_string(leaf, &mut buf)?,
            }),
            LF_MFUNC_ID => IdData::MemberFunction(MemberFunctionId {
                parent: buf.parse()?,
                function_type: buf.parse()?,
                name: parse_string(leaf, &mut buf)?,
            }),
            LF_BUILDINFO => IdData::BuildInfo({
                let count = buf.parse::<u16>()?;
                let mut arguments = Vec::with_capacity(count as usize);
                for _ in 0..count {
                    arguments.push(buf.parse()?);
                }
                BuildInfoId { arguments }
            }),
            LF_SUBSTR_LIST => IdData::StringList({
                let count = buf.parse::<u32>()?;
                let mut substrings = Vec::with_capacity(count as usize);
                for _ in 0..count {
                    substrings.push(buf.parse()?);
                }
                StringListId { substrings }
            }),
            LF_STRING_ID => IdData::String(StringId {
                substrings: parse_optional_id_index(&mut buf)?,
                name: parse_string(leaf, &mut buf)?,
            }),
            LF_UDT_SRC_LINE | LF_UDT_MOD_SRC_LINE => {
                let mut udt = UserDefinedTypeSourceId {
                    udt: buf.parse()?,
                    source_file: buf.parse()?,
                    line: buf.parse()?,
                    module: None,
                };

                if leaf == LF_UDT_MOD_SRC_LINE {
                    udt.module = Some(buf.parse()?);
                }

                IdData::UserDefinedTypeSource(udt)
            }
            _ => return Err(Error::UnimplementedTypeKind(leaf)),
        };

        Ok((data, buf.pos()))
    }
}

/// Global function, usually inlined.
///
/// This Id is usually referenced by [`InlineSiteSymbol`](struct.InlineSiteSymbol.html).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionId<'t> {
    /// Parent scope of this id.
    pub scope: Option<IdIndex>,
    /// Index of the function type declaration.
    pub function_type: TypeIndex,
    /// Name of the function.
    pub name: RawString<'t>,
}

/// Member function, usually inlined.
///
/// This Id is usually referenced by [`InlineSiteSymbol`](struct.InlineSiteSymbol.html).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemberFunctionId<'t> {
    /// Index of the parent type.
    pub parent: TypeIndex,
    /// Index of the member function type declaration.
    pub function_type: TypeIndex,
    /// Name of the member function.
    pub name: RawString<'t>,
}

/// Tool, version and command line build information.
///
/// This Id is usually referenced by [`BuildInfoSymbol`](struct.BuildInfoSymbol.html).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuildInfoId {
    /// Indexes of build arguments.
    pub arguments: Vec<IdIndex>,
}

/// A list of substrings.
///
/// This Id is usually referenced by [`StringId`](struct.StringId.html).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringListId {
    /// The list of substrings.
    pub substrings: Vec<TypeIndex>,
}

/// A string.
///
/// This Id is usually referenced by [`FunctionId`](struct.FunctionId.html) and contains the
/// full namespace of a function.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringId<'t> {
    /// Index of the list of substrings.
    pub substrings: Option<IdIndex>,
    /// The string.
    pub name: RawString<'t>,
}

/// Source and line of the definition of a User Defined Type (UDT).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UserDefinedTypeSourceId {
    /// Index of the UDT's type definition.
    pub udt: TypeIndex,
    /// Index of the source file name.
    pub source_file: IdIndex,
    /// Line number in the source file.
    pub line: u32,
    /// Module that contributes this UDT definition.
    ///
    /// If None, the UDT is declared in the same module.
    pub module: Option<u16>,
}
