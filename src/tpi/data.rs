// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use crate::common::*;
use crate::tpi::constants::*;
use crate::tpi::primitive::*;

/// Encapsulates parsed data about a `Type`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeData<'t> {
    Primitive(PrimitiveType),
    Class(ClassType<'t>),
    Member(MemberType<'t>),
    MemberFunction(MemberFunctionType),
    OverloadedMethod(OverloadedMethodType<'t>),
    Method(MethodType<'t>),
    StaticMember(StaticMemberType<'t>),
    Nested(NestedType<'t>),
    BaseClass(BaseClassType),
    VirtualBaseClass(VirtualBaseClassType),
    VirtualFunctionTablePointer(VirtualFunctionTablePointerType),
    Procedure(ProcedureType),
    Pointer(PointerType),
    Modifier(ModifierType),
    Enumeration(EnumerationType<'t>),
    Enumerate(EnumerateType<'t>),
    Array(ArrayType),
    Union(UnionType<'t>),
    Bitfield(BitfieldType),
    FieldList(FieldList<'t>),
    ArgumentList(ArgumentList),
    MethodList(MethodList),
}

impl<'t> TypeData<'t> {
    /// Return the name of this TypeData, if any
    pub fn name(&self) -> Option<RawString<'t>> {
        let name = match *self {
            TypeData::Class(ClassType { ref name, .. })
            | TypeData::Member(MemberType { ref name, .. })
            | TypeData::OverloadedMethod(OverloadedMethodType { ref name, .. })
            | TypeData::StaticMember(StaticMemberType { ref name, .. })
            | TypeData::Nested(NestedType { ref name, .. })
            | TypeData::Enumeration(EnumerationType { ref name, .. })
            | TypeData::Enumerate(EnumerateType { ref name, .. })
            | TypeData::Union(UnionType { ref name, .. }) => name,
            _ => return None,
        };

        Some(*name)
    }
}

/// Parse a type out of a `ParseBuffer`.
pub(crate) fn parse_type_data<'t>(mut buf: &mut ParseBuffer<'t>) -> Result<TypeData<'t>> {
    let leaf = buf.parse_u16()?;

    match leaf {
        // Basic types
        // -----------

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1631-L1642
        LF_CLASS | LF_CLASS_ST | LF_STRUCTURE | LF_STRUCTURE_ST | LF_INTERFACE => {
            Ok(TypeData::Class(ClassType {
                kind: match leaf {
                    LF_CLASS | LF_CLASS_ST => ClassKind::Class,
                    LF_STRUCTURE | LF_STRUCTURE_ST => ClassKind::Struct,
                    LF_INTERFACE => ClassKind::Interface,
                    _ => unreachable!(),
                },
                count: buf.parse_u16()?,
                properties: TypeProperties(buf.parse_u16()?),
                fields: parse_optional_type_index(&mut buf)?,
                derived_from: parse_optional_type_index(&mut buf)?,
                vtable_shape: parse_optional_type_index(&mut buf)?,
                size: parse_unsigned(&mut buf)? as u16,
                name: parse_string(leaf, buf)?,
            }))
        }

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2580-L2586
        LF_MEMBER | LF_MEMBER_ST => Ok(TypeData::Member(MemberType {
            attributes: FieldAttributes(buf.parse_u16()?),
            field_type: buf.parse_u32()? as TypeIndex,
            offset: parse_unsigned(&mut buf)? as u16,
            name: parse_string(leaf, &mut buf)?,
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
                attributes: FieldAttributes(raw_attr),
                nested_type: buf.parse_u32()? as TypeIndex,
                name: parse_string(leaf, &mut buf)?,
            }))
        }

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1801-L1811
        LF_MFUNCTION => Ok(TypeData::MemberFunction(MemberFunctionType {
            return_type: buf.parse_u32()? as TypeIndex,
            class_type: buf.parse_u32()? as TypeIndex,
            this_pointer_type: parse_optional_type_index(&mut buf)?,
            attributes: FunctionAttributes(buf.parse_u16()?),
            parameter_count: buf.parse_u16()?,
            argument_list: buf.parse_u32()? as TypeIndex,
            this_adjustment: buf.parse_u32()?,
        })),

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2650-L2655
        LF_METHOD | LF_METHOD_ST => Ok(TypeData::OverloadedMethod(OverloadedMethodType {
            count: buf.parse_u16()?,
            method_list: buf.parse_u32()? as TypeIndex,
            name: parse_string(leaf, &mut buf)?,
        })),

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2671-L2678
        LF_ONEMETHOD | LF_ONEMETHOD_ST => {
            let attr = FieldAttributes(buf.parse_u16()?);
            Ok(TypeData::Method(MethodType {
                attributes: attr,
                method_type: buf.parse_u32()? as TypeIndex,
                vtable_offset: if attr.is_intro_virtual() {
                    Some(buf.parse_u32()? as u32)
                } else {
                    // yes, this is variable length
                    None
                },
                name: parse_string(leaf, &mut buf)?,
            }))
        }

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2499-L2505
        LF_BCLASS | LF_BINTERFACE => Ok(TypeData::BaseClass(BaseClassType {
            kind: match leaf {
                LF_BCLASS => ClassKind::Class,
                LF_BINTERFACE => ClassKind::Interface,
                _ => unreachable!(),
            },
            attributes: FieldAttributes(buf.parse_u16()?),
            base_class: buf.parse_u32()? as TypeIndex,
            offset: parse_unsigned(&mut buf)? as u32,
        })),

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2615-L2619
        LF_VFUNCTAB => {
            // padding is supposed to be zero always, but… let's not check
            buf.parse_u16()?;
            Ok(TypeData::VirtualFunctionTablePointer(
                VirtualFunctionTablePointerType {
                    table: buf.parse_u32()? as TypeIndex,
                },
            ))
        }

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2599-L2604
        LF_STMEMBER | LF_STMEMBER_ST => Ok(TypeData::StaticMember(StaticMemberType {
            attributes: FieldAttributes(buf.parse_u16()?),
            field_type: buf.parse_u32()? as TypeIndex,
            name: parse_string(leaf, &mut buf)?,
        })),

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1469-L1506
        LF_POINTER => Ok(TypeData::Pointer(PointerType {
            underlying_type: buf.parse_u32()? as TypeIndex,
            attributes: PointerAttributes(buf.parse_u32()?),
        })),

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1775-L1782
        LF_PROCEDURE => Ok(TypeData::Procedure(ProcedureType {
            return_type: parse_optional_type_index(&mut buf)?,
            attributes: FunctionAttributes(buf.parse_u16()?),
            parameter_count: buf.parse_u16()?,
            argument_list: buf.parse_u32()? as TypeIndex,
        })),

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1460-L1464
        LF_MODIFIER => {
            let type_index = buf.parse_u32()? as TypeIndex;

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
        LF_ENUM | LF_ENUM_ST => Ok(TypeData::Enumeration(EnumerationType {
            count: buf.parse_u16()?,
            properties: TypeProperties(buf.parse_u16()?),
            underlying_type: buf.parse_u32()? as TypeIndex,
            fields: buf.parse_u32()? as TypeIndex,
            name: parse_string(leaf, &mut buf)?,
        })),

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2683-L2688
        LF_ENUMERATE | LF_ENUMERATE_ST => Ok(TypeData::Enumerate(EnumerateType {
            attributes: FieldAttributes(buf.parse_u16()?),
            value: buf.parse()?,
            name: parse_string(leaf, &mut buf)?,
        })),

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1564-L1579
        LF_ARRAY | LF_ARRAY_ST | LF_STRIDED_ARRAY => {
            let element_type = buf.parse_u32()? as TypeIndex;
            let indexing_type = buf.parse_u32()? as TypeIndex;
            let stride: Option<u32> = if leaf == LF_STRIDED_ARRAY {
                Some(buf.parse_u32()?)
            } else {
                None
            };

            let mut dimensions: Vec<u32> = Vec::new();

            loop {
                let dim = parse_unsigned(&mut buf)?;
                if dim > u64::from(u32::max_value()) {
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

            // eat any padding
            parse_padding(&mut buf)?;

            //println!("array: {:x}", buf);
            //println!("dimensions: {:?}", dimensions);

            assert!(buf.is_empty());

            Ok(TypeData::Array(ArrayType {
                element_type,
                indexing_type,
                stride,
                dimensions,
            }))
        }

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1657-L1664
        LF_UNION | LF_UNION_ST => Ok(TypeData::Union(UnionType {
            count: buf.parse_u16()?,
            properties: TypeProperties(buf.parse_u16()?),
            fields: buf.parse_u32()? as TypeIndex,
            size: parse_unsigned(&mut buf)? as u32,
            name: parse_string(leaf, &mut buf)?,
        })),

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2164-L2170
        LF_BITFIELD => Ok(TypeData::Bitfield(BitfieldType {
            underlying_type: buf.parse_u32()? as TypeIndex,
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
            attributes: FieldAttributes(buf.parse_u16()?),
            base_class: buf.parse_u32()? as TypeIndex,
            base_pointer: buf.parse_u32()? as TypeIndex,
            base_pointer_offset: parse_unsigned(&mut buf)? as u32,
            virtual_base_offset: parse_unsigned(&mut buf)? as u32,
        })),

        // List types
        // ----------

        // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2112-L2115
        LF_FIELDLIST => {
            let mut fields: Vec<TypeData<'t>> = Vec::new();
            let mut continuation: Option<TypeIndex> = None;

            while !buf.is_empty() {
                match buf.peek_u16()? {
                    LF_INDEX => {
                        // continuation record
                        // eat the leaf value
                        buf.parse_u16()?;

                        // parse the TypeIndex where we continue
                        continuation = Some(buf.parse_u32()? as TypeIndex);
                    }
                    _ => {
                        // other type
                        // recurse because recursion is endless fun because [STACK OVERFLOW]
                        fields.push(parse_type_data(&mut buf)?);
                    }
                }

                // consume any padding
                parse_padding(&mut buf)?;
            }

            Ok(TypeData::FieldList(FieldList {
                fields,
                continuation,
            }))
        }

        LF_ARGLIST => {
            let count = buf.parse_u32()?;
            let mut arglist: Vec<TypeIndex> = Vec::with_capacity(count as usize);
            for _ in 0..count {
                arglist.push(buf.parse_u32()? as TypeIndex);
            }
            Ok(TypeData::ArgumentList(ArgumentList { arguments: arglist }))
        }

        LF_METHODLIST => {
            let mut methods: Vec<MethodListEntry> = Vec::new();

            while !buf.is_empty() {
                // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2131-L2136
                let attr = FieldAttributes(buf.parse_u16()?);
                buf.parse_u16()?; // padding

                methods.push(MethodListEntry {
                    attributes: attr,
                    method_type: buf.parse_u32()?,
                    vtable_offset: if attr.is_intro_virtual() {
                        Some(buf.parse_u32()?)
                    } else {
                        None
                    },
                });
            }

            Ok(TypeData::MethodList(MethodList { methods }))
        }

        _ => Err(Error::UnimplementedTypeKind(leaf)),
    }
}

#[inline]
fn parse_optional_type_index<'t>(buf: &mut ParseBuffer<'t>) -> Result<Option<TypeIndex>> {
    let index = buf.parse_u32()? as TypeIndex;
    if index == 0 || index == u32::from(u16::max_value()) {
        Ok(None)
    } else {
        Ok(Some(index))
    }
}

#[inline]
fn parse_string<'t>(leaf: u16, buf: &mut ParseBuffer<'t>) -> Result<RawString<'t>> {
    if leaf > LF_ST_MAX {
        buf.parse_cstring()
    } else {
        buf.parse_u8_pascal_string()
    }
}

#[inline]
fn parse_padding<'t>(buf: &mut ParseBuffer<'t>) -> Result<()> {
    while !buf.is_empty() && buf.peek_u8()? >= 0xf0 {
        let padding = buf.parse_u8()?;
        if padding > 0xf0 {
            // low four bits indicate amount of padding
            // (don't ask me what 0xf0 means, then)
            buf.take((padding & 0x0f) as usize - 1)?;
        }
    }
    Ok(())
}

// https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/pdbdump/pdbdump.cpp#L2417-L2456
fn parse_unsigned<'t>(buf: &mut ParseBuffer<'t>) -> Result<u64> {
    let leaf = buf.parse_u16()?;
    if leaf < LF_NUMERIC {
        // the u16 directly encodes a value
        return Ok(u64::from(leaf));
    }

    match leaf {
        LF_CHAR => Ok(u64::from(buf.parse_u8()?)),
        LF_USHORT => Ok(u64::from(buf.parse_u16()?)),
        LF_ULONG => Ok(u64::from(buf.parse_u32()?)),
        LF_UQUADWORD => Ok(buf.parse_u64()?),
        _ => {
            if cfg!(debug_assertions) {
                unreachable!();
            } else {
                Err(Error::UnexpectedNumericPrefix(leaf))
            }
        }
    }
}

/*
typedef struct CV_prop_t {
unsigned short  packed      :1;     // true if structure is packed
unsigned short  ctor        :1;     // true if constructors or destructors present
unsigned short  ovlops      :1;     // true if overloaded operators present
unsigned short  isnested    :1;     // true if this is a nested class
unsigned short  cnested     :1;     // true if this class contains nested types
unsigned short  opassign    :1;     // true if overloaded assignment (=)
unsigned short  opcast      :1;     // true if casting methods
unsigned short  fwdref      :1;     // true if forward reference (incomplete defn)
unsigned short  scoped      :1;     // scoped definition
unsigned short  hasuniquename :1;   // true if there is a decorated name following the regular name
unsigned short  sealed      :1;     // true if class cannot be used as a base class
unsigned short  hfa         :2;     // CV_HFA_e
unsigned short  intrinsic   :1;     // true if class is an intrinsic type (e.g. __m128d)
unsigned short  mocom       :2;     // CV_MOCOM_UDT_e
} CV_prop_t;
*/
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TypeProperties(u16);
impl TypeProperties {
    /// Indicates if a type is packed via `#pragma pack` or similar.
    pub fn packed(self) -> bool {
        self.0 & 0x0001 != 0
    }

    /// Indicates if a type has constructors or destructors.
    pub fn constructors(self) -> bool {
        self.0 & 0x0002 != 0
    }

    /// Indicates if a type has any overloaded operators.
    pub fn overloaded_operators(self) -> bool {
        self.0 & 0x0004 != 0
    }

    /// Indicates if a type is a nested type, e.g. a `union` defined inside a `class`.
    pub fn is_nested_type(self) -> bool {
        self.0 & 0x0008 != 0
    }

    /// Indicates if a type contains nested types.
    pub fn contains_nested_types(self) -> bool {
        self.0 & 0x0010 != 0
    }

    /// Indicates if a class has overloaded the assignment operator.
    pub fn overloaded_assignment(self) -> bool {
        self.0 & 0x0020 != 0
    }
    pub fn overloaded_casting(self) -> bool {
        self.0 & 0x0040 != 0
    }

    /// Indicates if a type is a forward reference, i.e. an incomplete Type that serves as a
    /// placeholder until a complete Type can be built. This is necessary for e.g. self-referential
    /// data structures, but other more common declaration/definition idioms can cause forward
    /// references too.
    pub fn forward_reference(self) -> bool {
        self.0 & 0x0080 != 0
    }

    pub fn scoped_definition(self) -> bool {
        self.0 & 0x0100 != 0
    }
    pub fn has_unique_name(self) -> bool {
        self.0 & 0x0200 != 0
    }
    pub fn sealed(self) -> bool {
        self.0 & 0x0400 != 0
    }
    pub fn hfa(self) -> u8 {
        ((self.0 & 0x1800) >> 11) as u8
    }
    pub fn intrinsic_type(self) -> bool {
        self.0 & 0x1000 != 0
    }
    pub fn mocom(self) -> u8 {
        ((self.0 & 0x6000) >> 14) as u8
    }
}

/*
typedef struct CV_fldattr_t {
    unsigned short  access      :2;     // access protection CV_access_t
    unsigned short  mprop       :3;     // method properties CV_methodprop_t
    unsigned short  pseudo      :1;     // compiler generated fcn and does not exist
    unsigned short  noinherit   :1;     // true if class cannot be inherited
    unsigned short  noconstruct :1;     // true if class cannot be constructed
    unsigned short  compgenx    :1;     // compiler generated fcn and does exist
    unsigned short  sealed      :1;     // true if method cannot be overridden
    unsigned short  unused      :6;     // unused
} CV_fldattr_t;

typedef enum CV_methodprop_e {
    CV_MTvanilla        = 0x00,
    CV_MTvirtual        = 0x01,
    CV_MTstatic         = 0x02,
    CV_MTfriend         = 0x03,
    CV_MTintro          = 0x04,
    CV_MTpurevirt       = 0x05,
    CV_MTpureintro      = 0x06
} CV_methodprop_e;

*/
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FieldAttributes(u16);
impl FieldAttributes {
    #[inline]
    pub fn access(self) -> u8 {
        (self.0 & 0x0003) as u8
    }
    #[inline]
    fn method_properties(self) -> u8 {
        ((self.0 & 0x001c) >> 2) as u8
    }

    #[inline]
    pub fn is_static(self) -> bool {
        self.method_properties() == 0x02
    }

    #[inline]
    pub fn is_virtual(self) -> bool {
        self.method_properties() == 0x01
    }

    #[inline]
    pub fn is_pure_virtual(self) -> bool {
        self.method_properties() == 0x05
    }

    #[inline]
    pub fn is_intro_virtual(self) -> bool {
        match self.method_properties() {
            0x04 | 0x06 => true,
            _ => false,
        }
    }

    // TODO
}

#[allow(unused)]
#[repr(u8)]
enum Access {
    None = 0x00,
    Private = 0x01,
    Protected = 0x02,
    Public = 0x03,
}

// CV_call_t and CV_funcattr_t are always found back to back
// Treat them as a combined u16
/*
typedef struct CV_funcattr_t {
    unsigned char  cxxreturnudt :1;  // true if C++ style ReturnUDT
    unsigned char  ctor         :1;  // true if func is an instance constructor
    unsigned char  ctorvbase    :1;  // true if func is an instance constructor of a class with virtual bases
    unsigned char  unused       :5;  // unused
} CV_funcattr_t;
*/
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FunctionAttributes(u16);
impl FunctionAttributes {
    pub fn calling_convention(self) -> u8 {
        (self.0 & 0xff) as u8
    }
    pub fn cxx_return_udt(self) -> bool {
        (self.0 & 0x0100) > 0
    }
    pub fn is_constructor(self) -> bool {
        (self.0 & 0x0200) > 0
    }
    pub fn is_constructor_with_virtual_bases(self) -> bool {
        (self.0 & 0x0400) > 0
    }
}

/*
struct lfPointerAttr {
    unsigned long   ptrtype     :5; // ordinal specifying pointer type (CV_ptrtype_e)
    unsigned long   ptrmode     :3; // ordinal specifying pointer mode (CV_ptrmode_e)
    unsigned long   isflat32    :1; // true if 0:32 pointer
    unsigned long   isvolatile  :1; // TRUE if volatile pointer
    unsigned long   isconst     :1; // TRUE if const pointer
    unsigned long   isunaligned :1; // TRUE if unaligned pointer
    unsigned long   isrestrict  :1; // TRUE if restricted pointer (allow agressive opts)
    unsigned long   size        :6; // size of pointer (in bytes)
    unsigned long   ismocom     :1; // TRUE if it is a MoCOM pointer (^ or %)
    unsigned long   islref      :1; // TRUE if it is this pointer of member function with & ref-qualifier
    unsigned long   isrref      :1; // TRUE if it is this pointer of member function with && ref-qualifier
    unsigned long   unused      :10;// pad out to 32-bits for following cv_typ_t's
} attr;

typedef enum CV_ptrtype_e {
    CV_PTR_NEAR         = 0x00, // 16 bit pointer
    CV_PTR_FAR          = 0x01, // 16:16 far pointer
    CV_PTR_HUGE         = 0x02, // 16:16 huge pointer
    CV_PTR_BASE_SEG     = 0x03, // based on segment
    CV_PTR_BASE_VAL     = 0x04, // based on value of base
    CV_PTR_BASE_SEGVAL  = 0x05, // based on segment value of base
    CV_PTR_BASE_ADDR    = 0x06, // based on address of base
    CV_PTR_BASE_SEGADDR = 0x07, // based on segment address of base
    CV_PTR_BASE_TYPE    = 0x08, // based on type
    CV_PTR_BASE_SELF    = 0x09, // based on self
    CV_PTR_NEAR32       = 0x0a, // 32 bit pointer
    CV_PTR_FAR32        = 0x0b, // 16:32 pointer
    CV_PTR_64           = 0x0c, // 64 bit pointer
    CV_PTR_UNUSEDPTR    = 0x0d  // first unused pointer type
} CV_ptrtype_e;

typedef enum CV_ptrmode_e {
    CV_PTR_MODE_PTR     = 0x00, // "normal" pointer
    CV_PTR_MODE_REF     = 0x01, // "old" reference
    CV_PTR_MODE_LVREF   = 0x01, // l-value reference
    CV_PTR_MODE_PMEM    = 0x02, // pointer to data member
    CV_PTR_MODE_PMFUNC  = 0x03, // pointer to member function
    CV_PTR_MODE_RVREF   = 0x04, // r-value reference
    CV_PTR_MODE_RESERVED= 0x05  // first unused pointer mode
} CV_ptrmode_e;

*/
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PointerAttributes(u32);
impl PointerAttributes {
    // TODO

    /// Indicates the type of pointer.
    pub fn pointer_type(self) -> u8 {
        (self.0 & 0x1f) as u8
    }

    /// Indicates if this pointer is `const`.
    pub fn is_const(self) -> bool {
        (self.0 & 0x40) != 0
    }

    /// Is this a C++ reference, as opposed to a C pointer?
    pub fn is_reference(self) -> bool {
        match (self.0 >> 5) & 0x07 {
            0x01 | 0x04 => true,
            _ => false,
        }
    }

    /// The size of the pointer in bytes.
    pub fn size(self) -> u8 {
        let size = ((self.0 >> 13) & 0x3f) as u8;
        if size != 0 {
            return size;
        }
        match self.pointer_type() {
            0x0a => 4,
            0x0c => 8,
            _ => 0,
        }
    }
}

/// The information parsed from a type record with kind
/// `LF_CLASS`, `LF_CLASS_ST`, `LF_STRUCTURE`, `LF_STRUCTURE_ST` or `LF_INTERFACE`.
// https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1631
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassType<'t> {
    pub kind: ClassKind,

    /// Count of number of elements in this class
    pub count: u16,
    pub properties: TypeProperties,

    /// Type index which describes the fields of this class
    pub fields: Option<TypeIndex>,

    /// Type index which describes the class from which this class is derived, if any
    pub derived_from: Option<TypeIndex>,

    /// Type index which describes the shape of the vtable for this class, if any
    pub vtable_shape: Option<TypeIndex>,

    pub size: u16,

    pub name: RawString<'t>,
}

/// Used by `ClassType` to distinguish class-like concepts.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ClassKind {
    Class,
    Struct,
    Interface,
}

/// The information parsed from a type record with kind `LF_MEMBER` or `LF_MEMBER_ST`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemberType<'t> {
    pub attributes: FieldAttributes,
    pub field_type: TypeIndex,
    pub offset: u16,
    pub name: RawString<'t>,
}

/// The information parsed from a type record with kind `LF_MFUNCTION`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct MemberFunctionType {
    pub return_type: TypeIndex,
    pub class_type: TypeIndex,
    pub this_pointer_type: Option<TypeIndex>,
    pub attributes: FunctionAttributes,
    pub parameter_count: u16,
    pub argument_list: TypeIndex,
    pub this_adjustment: u32,
}

/// The information parsed from a type record with kind `LF_METHOD` or `LF_METHOD_ST`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OverloadedMethodType<'t> {
    pub count: u16,
    pub method_list: TypeIndex,
    pub name: RawString<'t>,
}

/// The information parsed from a type record with kind `LF_ONEMETHOD` or `LF_ONEMETHOD_ST`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodType<'t> {
    pub attributes: FieldAttributes,
    pub method_type: TypeIndex,
    pub vtable_offset: Option<u32>,
    pub name: RawString<'t>,
}

/// The information parsed from a type record with kind `LF_STMEMBER` or `LF_STMEMBER_ST`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StaticMemberType<'t> {
    pub attributes: FieldAttributes,
    pub field_type: TypeIndex,
    pub name: RawString<'t>,
}

/// The information parsed from a type record with kind
/// `LF_NESTTYPE`, `LF_NESTTYPE_ST`, `LF_NESTTYPEEX`, or `LF_NESTTYPEEX_ST`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NestedType<'t> {
    pub attributes: FieldAttributes,
    pub nested_type: TypeIndex,
    pub name: RawString<'t>,
}

/// The information parsed from a type record with kind `LF_BCLASS` or `LF_BINTERFACE`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BaseClassType {
    pub kind: ClassKind,
    pub attributes: FieldAttributes,
    pub base_class: TypeIndex,

    /// Describes the offset of the base class within the class
    pub offset: u32,
}

/// The information parsed from a type record with kind `LF_VBCLASS` or `LF_IVBCLASS`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VirtualBaseClassType {
    pub direct: bool,
    pub attributes: FieldAttributes,
    pub base_class: TypeIndex,
    pub base_pointer: TypeIndex,

    pub base_pointer_offset: u32,
    pub virtual_base_offset: u32,
}

/// The information parsed from a type record with kind `LF_VFUNCTAB`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VirtualFunctionTablePointerType {
    pub table: TypeIndex,
}

/// The information parsed from a type record with kind `LF_PROCEDURE`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ProcedureType {
    pub return_type: Option<TypeIndex>,
    pub attributes: FunctionAttributes,
    pub parameter_count: u16,
    pub argument_list: TypeIndex,
}

/// The information parsed from a type record with kind `LF_POINTER`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct PointerType {
    pub underlying_type: TypeIndex,
    pub attributes: PointerAttributes,
}

/// The information parsed from a type record with kind `LF_MODIFIER`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct ModifierType {
    pub underlying_type: TypeIndex,
    pub constant: bool,
    pub volatile: bool,
    pub unaligned: bool,
}

/// The information parsed from a type record with kind `LF_ENUM` or `LF_ENUM_ST`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumerationType<'t> {
    pub count: u16,
    pub properties: TypeProperties,
    pub underlying_type: TypeIndex,
    pub fields: TypeIndex,
    pub name: RawString<'t>,
}

/// The information parsed from a type record with kind `LF_ENUMERATE` or `LF_ENUMERATE_ST`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumerateType<'t> {
    pub attributes: FieldAttributes,
    pub value: Variant,
    pub name: RawString<'t>,
}

/// The information parsed from a type record with kind
/// `LF_ARRAY`, `LF_ARRAY_ST` or `LF_STRIDED_ARRAY`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArrayType {
    pub element_type: TypeIndex,
    pub indexing_type: TypeIndex,
    pub stride: Option<u32>,

    /// Contains array dimensions as specified in the PDB. This is not what you expect:
    ///
    /// * Dimensions are specified in terms of byte sizes, not element counts.
    /// * Multidimensional arrays aggregate the lower dimensions into the sizes of the higher
    ///   dimensions.
    ///
    /// Thus a `float[4][4]` has `dimensions: [16, 64]`. Determining array dimensions in terms
    /// of element counts requires determining the size of the `element_type` and iteratively
    /// dividing.
    pub dimensions: Vec<u32>,
}

/// The information parsed from a type record with kind `LF_UNION` or `LF_UNION_ST`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnionType<'t> {
    pub count: u16,
    pub properties: TypeProperties,
    pub fields: TypeIndex,
    pub size: u32,
    pub name: RawString<'t>,
}

/// The information parsed from a type record with kind `LF_BITFIELD`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BitfieldType {
    pub underlying_type: TypeIndex,
    pub length: u8,
    pub position: u8,
}

/// The information parsed from a type record with kind `LF_FIELDLIST`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldList<'t> {
    pub fields: Vec<TypeData<'t>>,

    /// Sometimes fields can't all fit in a single FieldList, in which case the FieldList
    /// refers to another FieldList in a chain.
    pub continuation: Option<TypeIndex>,
}

/// The information parsed from a type record with kind `LF_ARGLIST`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArgumentList {
    pub arguments: Vec<TypeIndex>,
}

/// The information parsed from a type record with kind `LF_METHODLIST`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodList {
    pub methods: Vec<MethodListEntry>,
}

/// An entry in a `MethodList`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct MethodListEntry {
    pub attributes: FieldAttributes,
    pub method_type: TypeIndex,
    pub vtable_offset: Option<u32>,
}

/*
// arrays:
ParseBuf::from("\x03\x15\xa0\xdc\x0b\x00\x23\x00\x00\x00\x40\x00\x00\xf1").as_bytes(),
ParseBuf::from("\x03\x15\xa0\xdc\x0b\x00\x23\x00\x00\x00\x50\x00\x00\xf1").as_bytes(),
ParseBuf::from("\x03\x15\xa9\x12\x00\x00\x23\x00\x00\x00\x50\x02\x00\xf1").as_bytes(),
ParseBuf::from("\x03\x15\xac\x12\x00\x00\x23\x00\x00\x00\x6c\x00\x00\xf1").as_bytes(),
ParseBuf::from("\x03\x15\x14\x10\x00\x00\x23\x00\x00\x00\x80\x00\x00\xf1").as_bytes(),
ParseBuf::from("\x03\x15\x75\x00\x00\x00\x23\x00\x00\x00\x28\x00\x00\xf1").as_bytes(),
ParseBuf::from("\x03\x15\x14\x10\x00\x00\x23\x00\x00\x00\x70\x0e\x00\xf1").as_bytes(),
ParseBuf::from("\x03\x15\x31\x14\x00\x00\x23\x00\x00\x00\x04\x02\x00\xf1").as_bytes(),
ParseBuf::from("\x03\x15\x31\x14\x00\x00\x23\x00\x00\x00\x0e\x03\x00\xf1").as_bytes(),
ParseBuf::from("\x03\x15\x77\x13\x00\x00\x23\x00\x00\x00\x02\x80\xbd\xda\x00\xf3\xf2\xf1").as_bytes(),
ParseBuf::from("\x03\x15\xb7\x16\x00\x00\x23\x00\x00\x00\x28\x00\x00\xf1").as_bytes(),
ParseBuf::from("\x03\x15\x14\x10\x00\x00\x23\x00\x00\x00\x55\x00\x00\xf1").as_bytes(),
*/
