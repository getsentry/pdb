// Copyright 2026 PDB Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

#![allow(missing_docs)]

#[cfg(feature = "alloc")]
use alloc::vec::Vec;

use crate::common::*;
use crate::tpi::constants::*;
use crate::tpi::primitive::*;
use crate::tpi::types::*;
use crate::tpi::utils::*;

/// Encapsulates parsed data about a type record from the TPI stream.
///
/// This enum represents all possible type record variants that can appear in a PDB's
/// type information stream. Each variant corresponds to a specific CodeView leaf type.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeData<'t> {
     /// A built-in primitive type (int, float, char, etc.)
    Primitive(PrimitiveType),
    
    /// A class, struct, or interface type
    Class(ClassType<'t>),
    
    /// A non-static data member of a class/struct/union
    Member(MemberType<'t>),
    
    /// A member function (method) of a class
    MemberFunction(MemberFunctionType),
    
    /// A set of overloaded methods sharing the same name
    OverloadedMethod(OverloadedMethodType<'t>),
    
    /// A single method of a class
    Method(MethodType<'t>),
    
    /// A static data member of a class
    StaticMember(StaticMemberType<'t>),
    
    /// A nested type definition within another type
    Nested(NestedType<'t>),
    
    /// A base class from which another class derives
    BaseClass(BaseClassType),
    
    /// A virtual base class
    VirtualBaseClass(VirtualBaseClassType),
    
    /// A pointer to a virtual function table
    VirtualFunctionTablePointer(VirtualFunctionTablePointerType),
    
    /// A procedure/function type (signature only)
    Procedure(ProcedureType),
    
    /// A pointer type
    Pointer(PointerType),
    
    /// A type with modifiers (const, volatile, unaligned)
    Modifier(ModifierType),
    
    /// An enumeration type
    Enumeration(EnumerationType<'t>),
    
    /// An enumerator value within an enumeration
    Enumerate(EnumerateType<'t>),
    
    /// An array type
    Array(ArrayType),
    
    /// A union type
    Union(UnionType<'t>),
    
    /// A bitfield type
    Bitfield(BitfieldType),
    
    /// A list of fields/class members
    FieldList(FieldList<'t>),
    
    /// A list of arguments for a function
    ArgumentList(ArgumentList),
    
    /// A list of methods for a class
    MethodList(MethodList),
}

impl<'t> TypeData<'t> {

    /// Return the name of this TypeData, if any
    pub const fn name(&self) -> Option<RawString<'t>> {
        let name = match self {
            Self::Class(ClassType { ref name, .. })
            | Self::Member(MemberType { ref name, .. })
            | Self::OverloadedMethod(OverloadedMethodType { ref name, .. })
            | Self::StaticMember(StaticMemberType { ref name, .. })
            | Self::Nested(NestedType { ref name, .. })
            | Self::Enumeration(EnumerationType { ref name, .. })
            | Self::Enumerate(EnumerateType { ref name, .. })
            | Self::Union(UnionType { ref name, .. }) => name,
            _ => return None,
        };

        Some(*name)
    }

    /// Parse a type out of a `ParseBuffer`.
    pub(crate) fn parse(buf: &mut ParseBuffer<'t>) -> Result<Self> {
        let leaf = buf.parse_u16()?;

        match leaf {
            // Basic types
            // -----------

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1631-L1642
            LF_CLASS | LF_CLASS_ST | LF_STRUCTURE | LF_STRUCTURE_ST | LF_INTERFACE => Ok(TypeData::Class(ClassType::parse(buf, leaf)?)),

            // https://github.com/microsoft/microsoft-pdb/issues/50#issuecomment-737890766
            LF_STRUCTURE19 => {
                let mut class = ClassType {
                    kind: ClassKind::Struct,
                    properties: TypeProperties::new(buf.parse_u32()? as u16),
                    fields: parse_optional_type_index(buf)?,
                    derived_from: parse_optional_type_index(buf)?,
                    vtable_shape: parse_optional_type_index(buf)?,
                    count: buf.parse_u16()?,
                    size: parse_unsigned(buf)?,
                    name: parse_string(leaf, buf)?,
                    unique_name: None,
                };

                if class.properties.has_unique_name() {
                    class.unique_name = Some(parse_string(leaf, buf)?);
                }

                Ok(TypeData::Class(class))
            }

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2580-L2586
            LF_MEMBER | LF_MEMBER_ST => Ok(TypeData::Member(MemberType {
                attributes: FieldAttributes::new(buf.parse_u16()?),
                field_type: buf.parse()?,
                offset: parse_unsigned(buf)?,
                name: parse_string(leaf, buf)?,
            })),

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2699-L2714
            LF_NESTTYPE | LF_NESTTYPE_ST | LF_NESTTYPEEX | LF_NESTTYPEEX_ST => {
                // These structs differ in their use of the first 16 bits
                let raw_attr = match leaf {
                    LF_NESTTYPEEX | LF_NESTTYPEEX_ST => buf.parse_u16()?,
                    _ => {
                        // discard padding
                        buf.parse_u16()?;
                        // assume zero
                        0
                    }
                };

                Ok(TypeData::Nested(NestedType {
                    attributes: FieldAttributes::new(raw_attr),
                    nested_type: buf.parse()?,
                    name: parse_string(leaf, buf)?,
                }))
            }

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1801-L1811
            LF_MFUNCTION => Ok(TypeData::MemberFunction(MemberFunctionType {
                return_type: buf.parse()?,
                class_type: buf.parse()?,
                this_pointer_type: parse_optional_type_index(buf)?,
                attributes: FunctionAttributes::new(buf.parse_u16()?),
                parameter_count: buf.parse_u16()?,
                argument_list: buf.parse()?,
                this_adjustment: buf.parse_u32()?,
            })),

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2650-L2655
            LF_METHOD | LF_METHOD_ST => Ok(TypeData::OverloadedMethod(OverloadedMethodType {
                count: buf.parse_u16()?,
                method_list: buf.parse()?,
                name: parse_string(leaf, buf)?,
            })),

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2671-L2678
            LF_ONEMETHOD | LF_ONEMETHOD_ST => {
                let attr = FieldAttributes::new(buf.parse_u16()?);
                Ok(TypeData::Method(MethodType {
                    attributes: attr,
                    method_type: buf.parse()?,
                    vtable_offset: if attr.is_intro_virtual() {
                        Some(buf.parse_u32()?)
                    } else {
                        // yes, this is variable length
                        None
                    },
                    name: parse_string(leaf, buf)?,
                }))
            }

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2499-L2505
            LF_BCLASS | LF_BINTERFACE => Ok(TypeData::BaseClass(BaseClassType {
                kind: match leaf {
                    LF_BCLASS => ClassKind::Class,
                    LF_BINTERFACE => ClassKind::Interface,
                    _ => unreachable!(),
                },
                attributes: FieldAttributes::new(buf.parse_u16()?),
                base_class: buf.parse()?,
                offset: parse_unsigned(buf)? as u32,
            })),

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2615-L2619
            LF_VFUNCTAB => {
                // padding is supposed to be zero always, but… let's not check
                buf.parse_u16()?;
                Ok(TypeData::VirtualFunctionTablePointer(
                    VirtualFunctionTablePointerType {
                        table: buf.parse()?,
                    },
                ))
            }

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2599-L2604
            LF_STMEMBER | LF_STMEMBER_ST => Ok(TypeData::StaticMember(StaticMemberType {
                attributes: FieldAttributes::new(buf.parse_u16()?),
                field_type: buf.parse()?,
                name: parse_string(leaf, buf)?,
            })),

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1469-L1506
            LF_POINTER => {
                let underlying_type = buf.parse()?;
                let attributes = PointerAttributes::new(buf.parse()?);

                let containing_class = if attributes.pointer_to_member() {
                    Some(buf.parse()?)
                } else {
                    None
                };

                Ok(TypeData::Pointer(PointerType {
                    underlying_type,
                    attributes,
                    containing_class,
                }))
            }

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1775-L1782
            LF_PROCEDURE => Ok(TypeData::Procedure(ProcedureType {
                return_type: parse_optional_type_index(buf)?,
                attributes: FunctionAttributes::new(buf.parse_u16()?),
                parameter_count: buf.parse_u16()?,
                argument_list: buf.parse()?,
            })),

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1460-L1464
            LF_MODIFIER => {
                let type_index = buf.parse()?;

                // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1090-L1095
                let flags = buf.parse_u16()?;

                Ok(TypeData::Modifier(ModifierType {
                    underlying_type: type_index,
                    constant: (flags & 0x01) != 0,
                    volatile: (flags & 0x02) != 0,
                    unaligned: (flags & 0x04) != 0,
                }))
            }

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1752-L1759
            LF_ENUM | LF_ENUM_ST => {
                let mut enumeration = EnumerationType {
                    count: buf.parse_u16()?,
                    properties: TypeProperties::new(buf.parse_u16()?),
                    underlying_type: buf.parse()?,
                    fields: buf.parse()?,
                    name: parse_string(leaf, buf)?,
                    unique_name: None,
                };

                if enumeration.properties.has_unique_name() {
                    enumeration.unique_name = Some(parse_string(leaf, buf)?);
                }

                Ok(TypeData::Enumeration(enumeration))
            }

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2683-L2688
            LF_ENUMERATE | LF_ENUMERATE_ST => Ok(TypeData::Enumerate(EnumerateType {
                attributes: FieldAttributes::new(buf.parse_u16()?),
                value: buf.parse()?,
                name: parse_string(leaf, buf)?,
            })),

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1564-L1579
            LF_ARRAY | LF_ARRAY_ST | LF_STRIDED_ARRAY => {
                let element_type = buf.parse()?;
                let indexing_type = buf.parse()?;
                let stride: Option<u32> = if leaf == LF_STRIDED_ARRAY {
                    Some(buf.parse_u32()?)
                } else {
                    None
                };

                let mut dimensions: Vec<u32> = Vec::new();

                loop {
                    let dim = parse_unsigned(buf)?;
                    if dim > u64::from(u32::MAX) {
                        return Err(Error::UnimplementedFeature("u64 array sizes"));
                    }
                    dimensions.push(dim as u32);

                    if buf.is_empty() {
                        // shouldn't run out here
                        return Err(Error::UnexpectedEof);
                    }

                    if buf.peek_u8()? == 0x00 {
                        // end of dimensions
                        buf.parse_u8()?;
                        break;
                    }
                }

                parse_padding(buf)?;

                assert!(buf.is_empty());

                Ok(TypeData::Array(ArrayType {
                    element_type,
                    indexing_type,
                    stride,
                    dimensions,
                }))
            }

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1657-L1664
            LF_UNION | LF_UNION_ST => {
                let mut union = UnionType {
                    count: buf.parse_u16()?,
                    properties: TypeProperties::new(buf.parse_u16()?),
                    fields: buf.parse()?,
                    size: parse_unsigned(buf)?,
                    name: parse_string(leaf, buf)?,
                    unique_name: None,
                };

                if union.properties.has_unique_name() {
                    union.unique_name = Some(parse_string(leaf, buf)?);
                }

                Ok(TypeData::Union(union))
            }

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2164-L2170
            LF_BITFIELD => Ok(TypeData::Bitfield(BitfieldType {
                underlying_type: buf.parse()?,
                length: buf.parse_u8()?,
                position: buf.parse_u8()?,
            })),

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1819-L1823
            LF_VTSHAPE => {
                // TODO
                Err(Error::UnimplementedTypeKind(leaf))
            }

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1825-L1837
            LF_VFTABLE => {
                // TODO
                Err(Error::UnimplementedTypeKind(leaf))
            }

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2521-L2528
            LF_VBCLASS | LF_IVBCLASS => Ok(TypeData::VirtualBaseClass(VirtualBaseClassType {
                direct: leaf == LF_VBCLASS,
                attributes: FieldAttributes::new(buf.parse_u16()?),
                base_class: buf.parse()?,
                base_pointer: buf.parse()?,
                base_pointer_offset: parse_unsigned(buf)? as u32,
                virtual_base_offset: parse_unsigned(buf)? as u32,
            })),

            // List types
            // ----------

            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2112-L2115
            LF_FIELDLIST => {
                let mut fields: Vec<TypeData<'t>> = Vec::new();
                let mut continuation: Option<TypeIndex> = None;

                loop {
                    if buf.is_empty() {
                        break;
                    }

                    match buf.peek_u16()? {
                        LF_INDEX => {
                            // continuation record
                            // eat the leaf value
                            buf.parse_u16()?;

                            // parse the TypeIndex where we continue
                            continuation = Some(buf.parse()?);
                        }
                        _ => {
                            fields.push(Self::parse(buf)?);
                        }
                    }

                    // consume any padding
                    parse_padding(buf)?;
                }

                Ok(TypeData::FieldList(FieldList {
                    fields,
                    continuation,
                }))
            }

            LF_ARGLIST => Ok(TypeData::ArgumentList(ArgumentList::parse(buf)?)),
            LF_METHODLIST => Ok(TypeData::MethodList(MethodList::parse(buf)?)),

            _ => Err(Error::UnimplementedTypeKind(leaf)),
        }
    }
}

#[test]
fn kind_1609() {
    let data = &[
        9, 22, 0, 2, 0, 0, 22, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 6, 0, 72, 95, 115, 105, 122,
        101, 0, 46, 63, 65, 85, 72, 95, 115, 105, 122, 101, 64, 64, 0,
    ][..];

    assert_eq!(
        TypeData::parse(&mut ParseBuffer::from(data)).expect("parse"),
        TypeData::Class(ClassType {
            kind: ClassKind::Struct,
            count: 2,
            properties: TypeProperties::new(512),
            fields: Some(TypeIndex(0x1016)),
            derived_from: None,
            vtable_shape: None,
            size: 6,
            name: RawString::from("H_size"),
            unique_name: Some(RawString::from(".?AUH_size@@")),
        })
    );
}
