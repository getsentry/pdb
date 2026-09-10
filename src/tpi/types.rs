use core::ops::Deref;

use crate::{tpi::{constants::*, utils::*}, *};

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
    pub const fn new(value: u16) -> Self {
        Self(value)
    }

    /// Indicates if a type is packed via `#pragma pack` or similar.
    pub const fn packed(self) -> bool {
        self.0 & 0x0001 != 0
    }

    /// Indicates if a type has constructors or destructors.
    pub const fn constructors(self) -> bool {
        self.0 & 0x0002 != 0
    }

    /// Indicates if a type has any overloaded operators.
    pub const fn overloaded_operators(self) -> bool {
        self.0 & 0x0004 != 0
    }

    /// Indicates if a type is a nested type, e.g. a `union` defined inside a `class`.
    pub const fn is_nested_type(self) -> bool {
        self.0 & 0x0008 != 0
    }

    /// Indicates if a type contains nested types.
    pub const fn contains_nested_types(self) -> bool {
        self.0 & 0x0010 != 0
    }

    /// Indicates if a class has overloaded the assignment operator.
    pub const fn overloaded_assignment(self) -> bool {
        self.0 & 0x0020 != 0
    }

    pub const fn overloaded_casting(self) -> bool {
        self.0 & 0x0040 != 0
    }

    /// Indicates if a type is a forward reference, i.e. an incomplete Type that serves as a
    /// placeholder until a complete Type can be built. This is necessary for e.g. self-referential
    /// data structures, but other more common declaration/definition idioms can cause forward
    /// references too.
    pub const fn forward_reference(self) -> bool {
        self.0 & 0x0080 != 0
    }

    pub const fn scoped_definition(self) -> bool {
        self.0 & 0x0100 != 0
    }

    pub const fn has_unique_name(self) -> bool {
        self.0 & 0x0200 != 0
    }

    pub const fn sealed(self) -> bool {
        self.0 & 0x0400 != 0
    }

    pub const fn hfa(self) -> u8 {
        ((self.0 & 0x1800) >> 11) as u8
    }

    pub const fn intrinsic_type(self) -> bool {
        self.0 & 0x1000 != 0
    }

    pub const fn mocom(self) -> u8 {
        ((self.0 & 0x6000) >> 14) as u8
    }
}

/// Attribute bits for a class member (field or method).
///
/// This corresponds to the `CV_fldattr_t` bitfield used throughout CodeView
/// type records. It packs an access specifier, a method-property code, and
/// several boolean flags into a single `u16`.
///
/// # Bit layout
///
/// ```text
/// 15 14 13 12 11 10  9  8  7  6  5  4  3  2  1  0
/// └──── unused ────┘ se cg nc ni ps │ mprop │ access
/// ```
///
/// | Field | Bits | Meaning |
/// |-------|------|---------|
/// | `access` | 0-1 | Access specifier (see [`FieldAccess`]) |
/// | `mprop` | 2-4 | Method property (see [`FieldMethodProperty`]) |
/// | `pseudo` | 5 | Compiler-generated function that does not exist |
/// | `noinherit` | 6 | Class cannot be inherited from |
/// | `noconstruct` | 7 | Class cannot be constructed |
/// | `compgenx` | 8 | Compiler-generated function that *does* exist |
/// | `sealed` | 9 | Method cannot be overridden |
/// | `unused` | 10-15 | Reserved |
///
/// The raw field bits are only meaningful in context: for a data member,
/// only `access` and the flag bits apply; for a method, `mprop` is
/// meaningful as well.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FieldAttributes(u16);

impl FieldAttributes {
    
    pub const fn new(value: u16) -> Self {
        Self(value)
    }

    pub const fn access(self) -> u8 {
        (self.0 & 0x0003) as u8
    }

    #[inline]
    const fn method_properties(self) -> u8 {
        ((self.0 & 0x001c) >> 2) as u8
    }

    #[inline]
    pub const fn is_static(self) -> bool {
        self.method_properties() == 0x02
    }

    #[inline]
    pub const fn is_virtual(self) -> bool {
        self.method_properties() == 0x01
    }

    #[inline]
    pub const fn is_pure_virtual(self) -> bool {
        self.method_properties() == 0x05
    }

    #[inline]
    pub const fn is_intro_virtual(self) -> bool {
        matches!(self.method_properties(), 0x04 | 0x06)
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

    pub const fn new(value: u16) -> Self {
        Self(value)
    }

    pub const fn calling_convention(self) -> u8 {
        (self.0 & 0xff) as u8
    }

    pub const fn cxx_return_udt(self) -> bool {
        (self.0 & 0x0100) > 0
    }

    pub const fn is_constructor(self) -> bool {
        (self.0 & 0x0200) > 0
    }

    pub const fn is_constructor_with_virtual_bases(self) -> bool {
        (self.0 & 0x0400) > 0
    }
}

/// The kind of a `PointerType`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PointerKind {
    /// 16 bit pointer.
    Near16,
    /// 16:16 far pointer.
    Far16,
    /// 16:16 huge pointer.
    Huge16,
    /// Based on segment.
    BaseSeg,
    /// Based on value of base.
    BaseVal,
    /// Based on segment value of base.
    BaseSegVal,
    /// Based on address of base.
    BaseAddr,
    /// Based on segment address of base.
    BaseSegAddr,
    /// Based on type.
    BaseType,
    /// Based on self.
    BaseSelf,
    /// 32-bit pointer.
    Near32,
    /// 48-bit 16:32 pointer.
    Far32,
    /// 64-bit pointer.
    Ptr64,
}

/// The mode of a `PointerType`.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PointerMode {
    /// A regular pointer.
    Pointer,
    /// L-Value reference.
    LValueReference,
    /// Pointer to data member.
    Member,
    /// Pointer to member function.
    MemberFunction,
    /// R-Value reference.
    RValueReference,
}

/// Attribute bits for a pointer type, parsed from an `LF_POINTER` record.
///
/// This corresponds to the `lfPointerAttr` bitfield in CodeView. It packs a
/// pointer kind, a pointer mode, several qualifier flags, and the pointer's
/// size into a single `u32`.
///
/// # Bit layout
///
/// ```text
/// 31                22 21 20 19 18 17   16 15         6 5     3 2      0
/// └── unused (10) ───┘ lr lf mm ur cn vl f │   size    │ ptrmode │ ptrtype
/// ```
///
/// | Bits | Field | Meaning |
/// |------|-------|---------|
/// | 0-4  | `ptrtype` | Pointer kind (near, far, based, etc.) — see [`PointerKind`] |
/// | 5-7  | `ptrmode` | How the pointer is used (regular, reference, member) — see [`PointerMode`] |
/// | 8    | `isflat32` | 0:32 flat pointer |
/// | 9    | `isvolatile` | `volatile` qualifier |
/// | 10   | `isconst` | `const` qualifier |
/// | 11   | `isunaligned` | Unaligned pointer |
/// | 12   | `isrestrict` | `__restrict` (allows aggressive optimizations) |
/// | 13-18 | `size` | Pointer size in bytes (0 means "use the kind's natural size") |
/// | 19   | `ismocom` | MoCOM pointer (`^` or `%`) |
/// | 20   | `islref` | `&` ref-qualifier on a member function's `this` pointer |
/// | 21   | `isrref` | `&&` ref-qualifier on a member function's `this` pointer |
/// | 22-31 | unused | Padding, so the record is 32-bit aligned |
///
/// # Size semantics
///
/// The `size` field is only 6 bits, so it can only express values up to 63.
/// Historically, linkers wrote the actual byte size here when it fit. For
/// modern targets where the natural size is implied by the pointer kind
/// (near = 4, far = 4, 64-bit = 8), the field is often left as 0 and callers
/// must fall back on [`PointerAttributes::size`], which handles both cases.
///
/// # References
///
/// - [cvinfo.h, `lfPointerAttr`](https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1480)
/// - [LLVM: CodeView Pointer Records](https://llvm.org/docs/CodeView/CodeView.html)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PointerAttributes(u32);

impl PointerAttributes {

    pub const fn new(value: u32) -> Self {
        Self(value)
    }

    /// Indicates the type of pointer.
    pub const fn pointer_kind(self) -> PointerKind {
        match self.0 & 0x1f {
            0x00 => PointerKind::Near16,
            0x01 => PointerKind::Far16,
            0x02 => PointerKind::Huge16,
            0x03 => PointerKind::BaseSeg,
            0x04 => PointerKind::BaseVal,
            0x05 => PointerKind::BaseSegVal,
            0x06 => PointerKind::BaseAddr,
            0x07 => PointerKind::BaseSegAddr,
            0x08 => PointerKind::BaseType,
            0x09 => PointerKind::BaseSelf,
            0x0a => PointerKind::Near32,
            0x0b => PointerKind::Far32,
            0x0c => PointerKind::Ptr64,
            _ => unreachable!(),
        }
    }

    /// Returns the mode of this pointer.
    pub const fn pointer_mode(self) -> PointerMode {
        match (self.0 >> 5) & 0x7 {
            0x00 => PointerMode::Pointer,
            0x01 => PointerMode::LValueReference,
            0x02 => PointerMode::Member,
            0x03 => PointerMode::MemberFunction,
            0x04 => PointerMode::RValueReference,
            _ => unreachable!(),
        }
    }

    /// Returns `true` if this points to a member (either data or function).
    pub const fn pointer_to_member(self) -> bool {
        matches!(
            self.pointer_mode(),
            PointerMode::Member | PointerMode::MemberFunction
        )
    }

    /// Returns `true` if this is a flat `0:32` pointer.
    pub const fn is_flat_32(self) -> bool {
        (self.0 & 0x100) != 0
    }

    /// Returns `true` if this pointer is `volatile`.
    pub const fn is_volatile(self) -> bool {
        (self.0 & 0x200) != 0
    }

    /// Returns `true` if this pointer is `const`.
    pub const fn is_const(self) -> bool {
        (self.0 & 0x400) != 0
    }

    /// Returns `true` if this pointer is unaligned.
    pub fn is_unaligned(self) -> bool {
        (self.0 & 0x800) != 0
    }

    /// Returns `true` if this pointer is restricted (allow aggressive opts).
    pub const fn is_restrict(self) -> bool {
        (self.0 & 0x1000) != 0
    }

    /// Is this a C++ reference, as opposed to a C pointer?
    pub const fn is_reference(self) -> bool {
        matches!(
            self.pointer_mode(),
            PointerMode::LValueReference | PointerMode::RValueReference
        )
    }

    /// The size of the pointer in bytes.
    pub const fn size(self) -> u8 {
        let size = ((self.0 >> 13) & 0x3f) as u8;
        if size != 0 {
            return size;
        }

        match self.pointer_kind() {
            PointerKind::Near32 | PointerKind::Far32 => 4,
            PointerKind::Ptr64 => 8,
            _ => 0,
        }
    }

    /// Returns `true` if this is a MoCOM pointer (`^` or `%`).
    pub const fn is_mocom(self) -> bool {
        (self.0 & 0x40000) != 0
    }
}

/// A class, struct, or interface type parsed from a `LF_CLASS`, `LF_CLASS_ST`,
/// `LF_STRUCTURE`, `LF_STRUCTURE_ST`, or `LF_INTERFACE` record.
///
/// These five leaf kinds all describe the same underlying shape — a
/// user-defined type with members, an optional base class, and an optional
/// vtable — and differ only in how the source language declared them
/// (`class` vs `struct` vs COM `interface`) and how the name was encoded
/// (regular vs "ST" / "structured" variants).
/// 
/// # References
///
/// - [cvinfo.h, LF_CLASS and friends](https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L1631)
/// - [LLVM: CodeView Type Records — Classes](https://llvm.org/docs/CodeView/CodeView.html)
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassType<'t> {
    /// Whether this type was declared as a `class`, `struct`, or `interface`.
    ///
    /// This is derived from the leaf kind, not from any field in the record
    /// itself; the on-disk format is otherwise identical for all three.
    pub kind: ClassKind,

    /// Number of top-level elements in this class.
    ///
    /// This includes all entries in the associated field list: data members,
    /// methods, static members, nested types, and base classes. It does *not*
    /// include members inherited from base classes. If the field list is
    /// chained via [`FieldList::continuation`](crate::FieldList::continuation),
    /// this count covers the total across all chained lists.
    pub count: u16,

    /// Bitfield of properties describing the class.
    ///
    /// Use the accessors on [`TypeProperties`] to read individual flags
    /// (packed, scoped, sealed, has unique name, etc.) rather than inspecting
    /// the raw value.
    pub properties: TypeProperties,

    /// Type index of the `LF_FIELDLIST` describing this class's members.
    ///
    /// `None` for forward declarations and for classes with no members.
    pub fields: Option<TypeIndex>,

    /// Type index of the base class this class derives from, if any.
    ///
    /// This points at another `ClassType` record (or a modifier wrapping one).
    /// For multiple inheritance, only one base is recorded here; additional
    /// bases are represented as [`BaseClassType`](crate::BaseClassType)
    /// entries inside the field list.
    pub derived_from: Option<TypeIndex>,

    /// Type index describing the layout of this class's vtable, if any.
    ///
    /// Only present for polymorphic classes. The referenced record is
    /// typically an `LF_VTSHAPE` describing the vtable slot kinds (near,
    /// far, this-adjusting, etc.). You rarely need this unless you are
    /// reconstructing the vtable layout.
    pub vtable_shape: Option<TypeIndex>,

    /// Total size of this class in bytes, including base classes and padding.
    ///
    /// This is `0` for forward declarations and for classes whose size the
    /// compiler did not record.
    pub size: u64,

    /// Display name of the class, including any template parameters.
    ///
    /// This is the name as written in source, e.g. `MyClass<int>` or
    /// `std::vector<Foo>`. It may not be valid UTF-8; use
    /// [`RawString::to_string`] to decode it lossily.
    pub name: RawString<'t>,

    /// Mangled name of the class, if present.
    ///
    /// This is the linker-level name (e.g. `.?AVMyClass@@` on MSVC) used for
    /// symbol resolution. It is present only when the
    /// [`TypeProperties::has_unique_name`] flag is set on
    /// [`properties`](Self::properties).
    pub unique_name: Option<RawString<'t>>,
}

impl<'t> ClassType<'t> {
    pub fn parse(buf: &mut ParseBuffer<'t>, leaf: u16) -> Result<Self> {
        let mut class = Self {
            kind: match leaf {
                LF_CLASS | LF_CLASS_ST => ClassKind::Class,
                LF_STRUCTURE | LF_STRUCTURE_ST => ClassKind::Struct,
                LF_INTERFACE => ClassKind::Interface,
                _ => unreachable!(),
            },
            count: buf.parse_u16()?,
            properties: TypeProperties::new(buf.parse_u16()?),
            fields: parse_optional_type_index(buf)?,
            derived_from: parse_optional_type_index(buf)?,
            vtable_shape: parse_optional_type_index(buf)?,
            size: parse_unsigned(buf)?,
            name: parse_string(leaf, buf)?,
            unique_name: None,
        };

        if class.properties.has_unique_name() {
            class.unique_name = Some(parse_string(leaf, buf)?);
        }

        Ok(class)
    }
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
    pub offset: u64,
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
    pub containing_class: Option<TypeIndex>,
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
    pub unique_name: Option<RawString<'t>>,
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
#[cfg(feature = "alloc")]
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
    pub size: u64,
    pub name: RawString<'t>,
    pub unique_name: Option<RawString<'t>>,
}

/// The information parsed from a type record with kind `LF_BITFIELD`.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct BitfieldType {
    pub underlying_type: TypeIndex,
    pub length: u8,
    pub position: u8,
}

#[cfg(feature = "alloc")]
/// The information parsed from a type record with kind `LF_FIELDLIST`.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FieldList<'t> {
    pub fields: Vec<TypeData<'t>>,

    /// Sometimes fields can't all fit in a single FieldList, in which case the FieldList
    /// refers to another FieldList in a chain.
    pub continuation: Option<TypeIndex>,
}

/// The information parsed from a type record with kind `LF_ARGLIST`.
#[cfg(feature = "alloc")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ArgumentList(Vec<TypeIndex>);

#[cfg(feature = "alloc")]
impl Deref for ArgumentList {
    type Target = [TypeIndex];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(feature = "alloc")]
impl ArgumentList {
    pub const fn new(data: Vec<TypeIndex>) -> Self {
        Self(data)
    }

    pub fn parse(buf: &mut ParseBuffer<'_>) -> Result<Self> {
        let count = buf.parse_u32()?;
        let mut arglist: Vec<TypeIndex> = Vec::with_capacity(count as usize);
        
        for _ in 0..count {
            arglist.push(buf.parse()?);
        }

        Ok(Self(arglist))
    }
}


/// The information parsed from a type record with kind `LF_METHODLIST`.
#[cfg(feature = "alloc")]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MethodList(Vec<MethodListEntry>);

#[cfg(feature = "alloc")]
impl Deref for MethodList {
    type Target = [MethodListEntry];

    #[inline]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[cfg(feature = "alloc")]
impl MethodList {
    pub const fn new(data: Vec<MethodListEntry>) -> Self {
        Self(data)
    }

    pub fn parse(buf: &mut ParseBuffer<'_>) -> Result<Self> {
        let mut methods: Vec<MethodListEntry> = Vec::new();

        while !buf.is_empty() {
            // https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2131-L2136
            let attr = FieldAttributes(buf.parse_u16()?);
            buf.parse_u16()?; // padding

            let entry = MethodListEntry {
                attributes: attr,
                method_type: buf.parse()?,
                vtable_offset: if attr.is_intro_virtual() {
                    Some(buf.parse_u32()?)
                } else {
                    None
                },
            };
            methods.push(entry);
        }

        Ok(Self(methods))
    }
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


macro_rules! define_leaf_kinds {
    ($($variant:ident = $value:literal => $name:literal),* $(,)?) => {
        /// The CodeView leaf kind of a type or id record.
        ///
        /// Every TPI and IPI record starts with a `u16` leaf kind that
        /// determines how the rest of the record is laid out. This enum
        /// maps the leaf values recognized by CodeView to named variants,
        /// preserving anything unrecognized as [`LeafKind::Unknown`].
        ///
        /// # Legacy vs. modern leaves
        ///
        /// CodeView accumulated duplicate leaves over its lifetime. VC5 and
        /// earlier emitted 16-bit-type-index variants, distinguished by a
        /// suffix like `LF_MODIFIER_16t`. Modern compilers emit the
        /// 32-bit-type-index variants, whose values have the `0x1000` bit set
        /// (e.g. `LF_MODIFIER`). Both sets appear here. A few kinds
        /// (`LF_VTSHAPE`, `LF_COBOL1`, `LF_LABEL`, `LF_NULL`, `LF_NOTTRAN`,
        /// `LF_ENDPRECOMP`, `LF_REFSYM`) appear in only one form and are shared
        /// between the two eras.
        ///
        /// # Unknown leaves
        ///
        /// The enum is `#[non_exhaustive]` and includes an
        /// [`LeafKind::Unknown`] variant so that new or unrecognized leaves
        /// can be carried through the parser without loss. Use
        /// [`LeafKind::raw`] to recover the original `u16`.
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        #[non_exhaustive]
        pub enum LeafKind {
            $($variant,)*
            /// A leaf kind not recognized by this version of the crate.
            Unknown(u16),
        }

        impl LeafKind {
            /// Returns the raw `u16` value of this leaf kind as it appears on
            /// disk.
            pub const fn raw(self) -> u16 {
                match self {
                    $(Self::$variant => $value,)*
                    Self::Unknown(v) => v,
                }
            }
        }

        impl From<u16> for LeafKind {
            fn from(v: u16) -> Self {
                match v {
                    $($value => Self::$variant,)*
                    other => Self::Unknown(other),
                }
            }
        }

        impl From<LeafKind> for u16 {
            fn from(kind: LeafKind) -> Self {
                kind.raw()
            }
        }

        impl core::fmt::Display for LeafKind {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                match self {
                    $(Self::$variant => f.write_str($name),)*
                    Self::Unknown(v) => write!(f, "LF_UNKNOWN(0x{v:04x})"),
                }
            }
        }
    };
}

define_leaf_kinds! {
    // ===== Legacy 16-bit-type-index leaves (VC5 and earlier) =====
    Modifier16t      = 0x0001 => "LF_MODIFIER_16t",
    Pointer16t       = 0x0002 => "LF_POINTER_16t",
    Array16t         = 0x0003 => "LF_ARRAY_16t",
    Class16t         = 0x0004 => "LF_CLASS_16t",
    Structure16t     = 0x0005 => "LF_STRUCTURE_16t",
    Union16t         = 0x0006 => "LF_UNION_16t",
    Enum16t          = 0x0007 => "LF_ENUM_16t",
    Procedure16t     = 0x0008 => "LF_PROCEDURE_16t",
    Mfunction16t     = 0x0009 => "LF_MFUNCTION_16t",
    Vtshape          = 0x000a => "LF_VTSHAPE",
    Cobol016t        = 0x000b => "LF_COBOL0_16t",
    Cobol1           = 0x000c => "LF_COBOL1",
    Barray16t        = 0x000d => "LF_BARRAY_16t",
    Label            = 0x000e => "LF_LABEL",
    Null             = 0x000f => "LF_NULL",
    Nottran          = 0x0010 => "LF_NOTTRAN",
    Dimarray16t      = 0x0011 => "LF_DIMARRAY_16t",
    Vftpath16t       = 0x0012 => "LF_VFTPATH_16t",
    Precomp16t       = 0x0013 => "LF_PRECOMP_16t",
    Endprecomp       = 0x0014 => "LF_ENDPRECOMP",
    Oem16t           = 0x0015 => "LF_OEM_16t",
    TypeserverSt     = 0x0016 => "LF_TYPESERVER_ST",

    Skip16t          = 0x0200 => "LF_SKIP_16t",
    Arglist16t       = 0x0201 => "LF_ARGLIST_16t",
    Defarg16t        = 0x0202 => "LF_DEFARG_16t",
    List             = 0x0203 => "LF_LIST",
    Fieldlist16t     = 0x0204 => "LF_FIELDLIST_16t",
    Derived16t       = 0x0205 => "LF_DERIVED_16t",
    Bitfield16t      = 0x0206 => "LF_BITFIELD_16t",
    Methodlist16t    = 0x0207 => "LF_METHODLIST_16t",
    Dimconu16t       = 0x0208 => "LF_DIMCONU_16t",
    Dimconlu16t      = 0x0209 => "LF_DIMCONLU_16t",
    Dimvaru16t       = 0x020a => "LF_DIMVARU_16t",
    Dimvarlu16t      = 0x020b => "LF_DIMVARLU_16t",
    Refsym           = 0x020c => "LF_REFSYM",

    Bclass16t        = 0x0400 => "LF_BCLASS_16t",
    Vbclass16t       = 0x0401 => "LF_VBCLASS_16t",
    Ivbclass16t      = 0x0402 => "LF_IVBCLASS_16t",
    EnumerateSt      = 0x0403 => "LF_ENUMERATE_ST",
    Friendfcn16t     = 0x0404 => "LF_FRIENDFCN_16t",
    Index16t         = 0x0405 => "LF_INDEX_16t",
    Member16t        = 0x0406 => "LF_MEMBER_16t",
    Stmember16t      = 0x0407 => "LF_STMEMBER_16t",
    Method16t        = 0x0408 => "LF_METHOD_16t",
    Nesttype16t      = 0x0409 => "LF_NESTTYPE_16t",
    Vfunctab16t      = 0x040a => "LF_VFUNCTAB_16t",
    Friendcls16t     = 0x040b => "LF_FRIENDCLS_16t",
    Onemethod16t     = 0x040c => "LF_ONEMETHOD_16t",
    Vfuncoff16t      = 0x040d => "LF_VFUNCOFF_16t",

    // ===== Modern 32-bit-type-index leaves =====
    Modifier         = 0x1001 => "LF_MODIFIER",
    Pointer          = 0x1002 => "LF_POINTER",
    ArraySt          = 0x1003 => "LF_ARRAY_ST",
    ClassSt          = 0x1004 => "LF_CLASS_ST",
    StructureSt      = 0x1005 => "LF_STRUCTURE_ST",
    UnionSt          = 0x1006 => "LF_UNION_ST",
    EnumSt           = 0x1007 => "LF_ENUM_ST",
    Procedure        = 0x1008 => "LF_PROCEDURE",
    Mfunction        = 0x1009 => "LF_MFUNCTION",
    Cobol0           = 0x100a => "LF_COBOL0",
    Barray           = 0x100b => "LF_BARRAY",
    DimarraySt       = 0x100c => "LF_DIMARRAY_ST",
    Vftpath          = 0x100d => "LF_VFTPATH",
    PrecompSt        = 0x100e => "LF_PRECOMP_ST",
    Oem              = 0x100f => "LF_OEM",
    AliasSt          = 0x1010 => "LF_ALIAS_ST",
    Oem2             = 0x1011 => "LF_OEM2",

    Skip             = 0x1200 => "LF_SKIP",
    Arglist          = 0x1201 => "LF_ARGLIST",
    DefargSt         = 0x1202 => "LF_DEFARG_ST",
    Fieldlist        = 0x1203 => "LF_FIELDLIST",
    Derived          = 0x1204 => "LF_DERIVED",
    Bitfield         = 0x1205 => "LF_BITFIELD",
    Methodlist       = 0x1206 => "LF_METHODLIST",
    Dimconu          = 0x1207 => "LF_DIMCONU",
    Dimconlu         = 0x1208 => "LF_DIMCONLU",
    Dimvaru          = 0x1209 => "LF_DIMVARU",
    Dimvarlu         = 0x120a => "LF_DIMVARLU",

    Bclass           = 0x1400 => "LF_BCLASS",
    Vbclass          = 0x1401 => "LF_VBCLASS",
    Ivbclass         = 0x1402 => "LF_IVBCLASS",
    FriendfcnSt      = 0x1403 => "LF_FRIENDFCN_ST",
    Index            = 0x1404 => "LF_INDEX",
    MemberSt         = 0x1405 => "LF_MEMBER_ST",
    StmemberSt       = 0x1406 => "LF_STMEMBER_ST",
    MethodSt         = 0x1407 => "LF_METHOD_ST",
    NesttypeSt       = 0x1408 => "LF_NESTTYPE_ST",
    Vfunctab         = 0x1409 => "LF_VFUNCTAB",
    Friendcls        = 0x140a => "LF_FRIENDCLS",
    OnemethodSt      = 0x140b => "LF_ONEMETHOD_ST",
    Vfuncoff         = 0x140c => "LF_VFUNCOFF",
    NesttypeexSt     = 0x140d => "LF_NESTTYPEEX_ST",
    MembermodifySt   = 0x140e => "LF_MEMBERMODIFY_ST",
    ManagedSt        = 0x140f => "LF_MANAGED_ST",

    // ===== Modern leaves with SZ (null-terminated) names =====
    Typeserver       = 0x1501 => "LF_TYPESERVER",
    Enumerate        = 0x1502 => "LF_ENUMERATE",
    Array            = 0x1503 => "LF_ARRAY",
    Class            = 0x1504 => "LF_CLASS",
    Structure        = 0x1505 => "LF_STRUCTURE",
    Union            = 0x1506 => "LF_UNION",
    Enum             = 0x1507 => "LF_ENUM",
    Dimarray         = 0x1508 => "LF_DIMARRAY",
    Precomp          = 0x1509 => "LF_PRECOMP",
    Alias            = 0x150a => "LF_ALIAS",
    Defarg           = 0x150b => "LF_DEFARG",
    Friendfcn        = 0x150c => "LF_FRIENDFCN",
    Member           = 0x150d => "LF_MEMBER",
    Stmember         = 0x150e => "LF_STMEMBER",
    Method           = 0x150f => "LF_METHOD",
    Nesttype         = 0x1510 => "LF_NESTTYPE",
    Onemethod        = 0x1511 => "LF_ONEMETHOD",
    Nesttypeex       = 0x1512 => "LF_NESTTYPEEX",
    Membermodify     = 0x1513 => "LF_MEMBERMODIFY",
    Managed          = 0x1514 => "LF_MANAGED",
    Typeserver2      = 0x1515 => "LF_TYPESERVER2",
    StridedArray     = 0x1516 => "LF_STRIDED_ARRAY",
    Hlsl             = 0x1517 => "LF_HLSL",
    ModifierEx       = 0x1518 => "LF_MODIFIER_EX",
    Interface        = 0x1519 => "LF_INTERFACE",
    Binterface       = 0x151a => "LF_BINTERFACE",
    Vector           = 0x151b => "LF_VECTOR",
    Matrix           = 0x151c => "LF_MATRIX",
    Vftable          = 0x151d => "LF_VFTABLE",

    // ===== IPI (id) stream leaves =====
    FuncId           = 0x1601 => "LF_FUNC_ID",
    MfuncId          = 0x1602 => "LF_MFUNC_ID",
    Buildinfo        = 0x1603 => "LF_BUILDINFO",
    SubstrList       = 0x1604 => "LF_SUBSTR_LIST",
    StringId         = 0x1605 => "LF_STRING_ID",
    UdtSrcLine       = 0x1606 => "LF_UDT_SRC_LINE",
    UdtModSrcLine    = 0x1607 => "LF_UDT_MOD_SRC_LINE",
    Structure19      = 0x1609 => "LF_STRUCTURE19",
}

impl LeafKind {
    /// Returns `true` if this leaf kind is only emitted by VC5 and earlier.
    ///
    /// Modern compilers emit the corresponding 32-bit-type-index variants
    /// ([`Self::Modifier`], [`Self::Pointer`], [`Self::Class`], etc.) instead.
    /// The shared leaves that appear in both eras (`Vtshape`, `Cobol1`,
    /// `Label`, `Null`, `Nottran`, `Endprecomp`, `Refsym`) return `false`.
    pub const fn is_legacy(self) -> bool {
        match self {
            Self::Modifier16t
            | Self::Pointer16t
            | Self::Array16t
            | Self::Class16t
            | Self::Structure16t
            | Self::Union16t
            | Self::Enum16t
            | Self::Procedure16t
            | Self::Mfunction16t
            | Self::Cobol016t
            | Self::Barray16t
            | Self::Dimarray16t
            | Self::Vftpath16t
            | Self::Precomp16t
            | Self::Oem16t
            | Self::Skip16t
            | Self::Arglist16t
            | Self::Defarg16t
            | Self::Fieldlist16t
            | Self::Derived16t
            | Self::Bitfield16t
            | Self::Methodlist16t
            | Self::Dimconu16t
            | Self::Dimconlu16t
            | Self::Dimvaru16t
            | Self::Dimvarlu16t
            | Self::Bclass16t
            | Self::Vbclass16t
            | Self::Ivbclass16t
            | Self::Friendfcn16t
            | Self::Index16t
            | Self::Member16t
            | Self::Stmember16t
            | Self::Method16t
            | Self::Nesttype16t
            | Self::Vfunctab16t
            | Self::Friendcls16t
            | Self::Onemethod16t
            | Self::Vfuncoff16t => true,
            Self::Vtshape
            | Self::Cobol1
            | Self::Label
            | Self::Null
            | Self::Nottran
            | Self::Endprecomp
            | Self::Refsym
            | Self::TypeserverSt
            | Self::List
            | Self::EnumerateSt => false,
            Self::Modifier
            | Self::Pointer
            | Self::ArraySt
            | Self::ClassSt
            | Self::StructureSt
            | Self::UnionSt
            | Self::EnumSt
            | Self::Procedure
            | Self::Mfunction
            | Self::Cobol0
            | Self::Barray
            | Self::DimarraySt
            | Self::Vftpath
            | Self::PrecompSt
            | Self::Oem
            | Self::AliasSt
            | Self::Oem2
            | Self::Skip
            | Self::Arglist
            | Self::DefargSt
            | Self::Fieldlist
            | Self::Derived
            | Self::Bitfield
            | Self::Methodlist
            | Self::Dimconu
            | Self::Dimconlu
            | Self::Dimvaru
            | Self::Dimvarlu
            | Self::Bclass
            | Self::Vbclass
            | Self::Ivbclass
            | Self::FriendfcnSt
            | Self::Index
            | Self::MemberSt
            | Self::StmemberSt
            | Self::MethodSt
            | Self::NesttypeSt
            | Self::Vfunctab
            | Self::Friendcls
            | Self::OnemethodSt
            | Self::Vfuncoff
            | Self::NesttypeexSt
            | Self::MembermodifySt
            | Self::ManagedSt
            | Self::Typeserver
            | Self::Enumerate
            | Self::Array
            | Self::Class
            | Self::Structure
            | Self::Union
            | Self::Enum
            | Self::Dimarray
            | Self::Precomp
            | Self::Alias
            | Self::Defarg
            | Self::Friendfcn
            | Self::Member
            | Self::Stmember
            | Self::Method
            | Self::Nesttype
            | Self::Onemethod
            | Self::Nesttypeex
            | Self::Membermodify
            | Self::Managed
            | Self::Typeserver2
            | Self::StridedArray
            | Self::Hlsl
            | Self::ModifierEx
            | Self::Interface
            | Self::Binterface
            | Self::Vector
            | Self::Matrix
            | Self::Vftable
            | Self::FuncId
            | Self::MfuncId
            | Self::Buildinfo
            | Self::SubstrList
            | Self::StringId
            | Self::UdtSrcLine
            | Self::UdtModSrcLine
            | Self::Structure19 => false,
            // #[non_exhaustive] doesn't affect same-crate matches, but the
            // compiler still requires an arm for Unknown.
            Self::Unknown(_) => false,
        }
    }
}
