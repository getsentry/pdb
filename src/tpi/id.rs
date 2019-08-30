use scroll::ctx::TryFromCtx;

use crate::common::*;
use crate::tpi::constants::*;

#[inline]
fn parse_optional_type_index<'t>(buf: &mut ParseBuffer<'t>) -> Result<Option<TypeIndex>> {
    let index = buf.parse()?;
    if index == TypeIndex(0) || index == TypeIndex(0xffff) {
        Ok(None)
    } else {
        Ok(Some(index))
    }
}

// TODO(ja): MOve these
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
    FunctionId(FunctionIdType<'t>),
    MemberFunctionId(MemberFunctionIdType<'t>),
    BuildInfoId(BuildInfoIdType),
    SubstringList(SubstringListType),
    StringId(StringIdType<'t>),
    UserDefinedTypeSource(UserDefinedTypeSourceType),
}

impl<'t> IdData<'t> {}

impl<'t> TryFromCtx<'t, scroll::Endian> for IdData<'t> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'t [u8], _ctx: scroll::Endian) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);
        let leaf = buf.parse_u16()?;

        let data = match leaf {
            LF_FUNC_ID => IdData::FunctionId(FunctionIdType {
                scope: parse_optional_id_index(&mut buf)?,
                function_type: buf.parse()?,
                name: parse_string(leaf, &mut buf)?,
            }),
            LF_MFUNC_ID => IdData::MemberFunctionId(MemberFunctionIdType {
                parent: parse_optional_type_index(&mut buf)?,
                function_type: buf.parse()?,
                name: parse_string(leaf, &mut buf)?,
            }),
            LF_BUILDINFO => IdData::BuildInfoId({
                let count = buf.parse::<u16>()?;
                let mut arguments = Vec::with_capacity(count as usize);
                for _ in 0..count {
                    arguments.push(buf.parse()?);
                }
                BuildInfoIdType { arguments }
            }),
            LF_SUBSTR_LIST => IdData::SubstringList({
                let count = buf.parse::<u32>()?;
                let mut substrings = Vec::with_capacity(count as usize);
                for _ in 0..count {
                    substrings.push(buf.parse()?);
                }
                SubstringListType { substrings }
            }),
            LF_STRING_ID => IdData::StringId(StringIdType {
                substrings: buf.parse()?,
                name: parse_string(leaf, &mut buf)?,
            }),
            LF_UDT_SRC_LINE | LF_UDT_MOD_SRC_LINE => {
                let mut udt = UserDefinedTypeSourceType {
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct FunctionIdType<'t> {
    /// Parent scope of this id.
    scope: Option<IdIndex>,
    /// Index of the function type declaration.
    function_type: TypeIndex,
    name: RawString<'t>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct MemberFunctionIdType<'t> {
    /// Index of the parent type.
    parent: Option<TypeIndex>,
    /// Index of the member function type declaration.
    function_type: TypeIndex,
    /// Name of the member function.
    name: RawString<'t>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BuildInfoIdType {
    /// Indexes of build arguments.
    arguments: Vec<IdIndex>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SubstringListType {
    /// The list of substrings.
    pub substrings: Vec<TypeIndex>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StringIdType<'t> {
    /// Index of the list of substrings.
    substrings: IdIndex,
    /// The string.
    name: RawString<'t>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct UserDefinedTypeSourceType {
    /// Index of the UDT's type definition.
    udt: TypeIndex,
    /// Index of the source file name.
    source_file: IdIndex,
    /// Line number in the source file.
    line: u32,
    /// Module that contributes this UDT definition.
    ///
    /// If None, the UDT is declared in the same module.
    module: Option<u16>,
}
