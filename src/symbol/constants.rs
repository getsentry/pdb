// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

// A list of known symbol kinds.
// from:
//  https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2735

#![allow(dead_code,non_upper_case_globals,non_camel_case_types)]

pub const S_COMPILE       : u16 = 0x0001;  // Compile flags symbol
pub const S_REGISTER_16t  : u16 = 0x0002;  // Register variable
pub const S_CONSTANT_16t  : u16 = 0x0003;  // constant symbol
pub const S_UDT_16t       : u16 = 0x0004;  // User defined type
pub const S_SSEARCH       : u16 = 0x0005;  // Start Search
pub const S_END           : u16 = 0x0006;  // Block procedure "with" or thunk end
pub const S_SKIP          : u16 = 0x0007;  // Reserve symbol space in $$Symbols table
pub const S_CVRESERVE     : u16 = 0x0008;  // Reserved symbol for CV internal use
pub const S_OBJNAME_ST    : u16 = 0x0009;  // path to object file name
pub const S_ENDARG        : u16 = 0x000a;  // end of argument/return list
pub const S_COBOLUDT_16t  : u16 = 0x000b;  // special UDT for cobol that does not symbol pack
pub const S_MANYREG_16t   : u16 = 0x000c;  // multiple register variable
pub const S_RETURN        : u16 = 0x000d;  // return description symbol
pub const S_ENTRYTHIS     : u16 = 0x000e;  // description of this pointer on entry

pub const S_BPREL16       : u16 = 0x0100;  // BP-relative
pub const S_LDATA16       : u16 = 0x0101;  // Module-local symbol
pub const S_GDATA16       : u16 = 0x0102;  // Global data symbol
pub const S_PUB16         : u16 = 0x0103;  // a public symbol
pub const S_LPROC16       : u16 = 0x0104;  // Local procedure start
pub const S_GPROC16       : u16 = 0x0105;  // Global procedure start
pub const S_THUNK16       : u16 = 0x0106;  // Thunk Start
pub const S_BLOCK16       : u16 = 0x0107;  // block start
pub const S_WITH16        : u16 = 0x0108;  // with start
pub const S_LABEL16       : u16 = 0x0109;  // code label
pub const S_CEXMODEL16    : u16 = 0x010a;  // change execution model
pub const S_VFTABLE16     : u16 = 0x010b;  // address of virtual function table
pub const S_REGREL16      : u16 = 0x010c;  // register relative address

pub const S_BPREL32_16t   : u16 = 0x0200;  // BP-relative
pub const S_LDATA32_16t   : u16 = 0x0201;  // Module-local symbol
pub const S_GDATA32_16t   : u16 = 0x0202;  // Global data symbol
pub const S_PUB32_16t     : u16 = 0x0203;  // a public symbol (CV internal reserved)
pub const S_LPROC32_16t   : u16 = 0x0204;  // Local procedure start
pub const S_GPROC32_16t   : u16 = 0x0205;  // Global procedure start
pub const S_THUNK32_ST    : u16 = 0x0206;  // Thunk Start
pub const S_BLOCK32_ST    : u16 = 0x0207;  // block start
pub const S_WITH32_ST     : u16 = 0x0208;  // with start
pub const S_LABEL32_ST    : u16 = 0x0209;  // code label
pub const S_CEXMODEL32    : u16 = 0x020a;  // change execution model
pub const S_VFTABLE32_16t : u16 = 0x020b;  // address of virtual function table
pub const S_REGREL32_16t  : u16 = 0x020c;  // register relative address
pub const S_LTHREAD32_16t : u16 = 0x020d;  // local thread storage
pub const S_GTHREAD32_16t : u16 = 0x020e;  // global thread storage
pub const S_SLINK32       : u16 = 0x020f;  // static link for MIPS EH implementation

pub const S_LPROCMIPS_16t : u16 = 0x0300;  // Local procedure start
pub const S_GPROCMIPS_16t : u16 = 0x0301;  // Global procedure start

// if these ref symbols have names following then the names are in ST format
pub const S_PROCREF_ST    : u16 = 0x0400;  // Reference to a procedure
pub const S_DATAREF_ST    : u16 = 0x0401;  // Reference to data
pub const S_ALIGN         : u16 = 0x0402;  // Used for page alignment of symbols

pub const S_LPROCREF_ST   : u16 = 0x0403;  // Local Reference to a procedure
pub const S_OEM           : u16 = 0x0404;  // OEM defined symbol

// sym records with 32-bit types embedded instead of 16-bit
// all have 0x1000 bit set for easy identification
// only do the 32-bit target versions since we don't really
// care about 16-bit ones anymore.
pub const S_TI16_MAX          : u16 = 0x1000;

pub const S_REGISTER_ST   : u16 = 0x1001;  // Register variable
pub const S_CONSTANT_ST   : u16 = 0x1002;  // constant symbol
pub const S_UDT_ST        : u16 = 0x1003;  // User defined type
pub const S_COBOLUDT_ST   : u16 = 0x1004;  // special UDT for cobol that does not symbol pack
pub const S_MANYREG_ST    : u16 = 0x1005;  // multiple register variable
pub const S_BPREL32_ST    : u16 = 0x1006;  // BP-relative
pub const S_LDATA32_ST    : u16 = 0x1007;  // Module-local symbol
pub const S_GDATA32_ST    : u16 = 0x1008;  // Global data symbol
pub const S_PUB32_ST      : u16 = 0x1009;  // a public symbol (CV internal reserved)
pub const S_LPROC32_ST    : u16 = 0x100a;  // Local procedure start
pub const S_GPROC32_ST    : u16 = 0x100b;  // Global procedure start
pub const S_VFTABLE32     : u16 = 0x100c;  // address of virtual function table
pub const S_REGREL32_ST   : u16 = 0x100d;  // register relative address
pub const S_LTHREAD32_ST  : u16 = 0x100e;  // local thread storage
pub const S_GTHREAD32_ST  : u16 = 0x100f;  // global thread storage

pub const S_LPROCMIPS_ST  : u16 = 0x1010;  // Local procedure start
pub const S_GPROCMIPS_ST  : u16 = 0x1011;  // Global procedure start

pub const S_FRAMEPROC     : u16 = 0x1012;  // extra frame and proc information
pub const S_COMPILE2_ST   : u16 = 0x1013;  // extended compile flags and info

// new symbols necessary for 16-bit enumerates of IA64 registers
// and IA64 specific symbols

pub const S_MANYREG2_ST   : u16 = 0x1014;  // multiple register variable
pub const S_LPROCIA64_ST  : u16 = 0x1015;  // Local procedure start (IA64)
pub const S_GPROCIA64_ST  : u16 = 0x1016;  // Global procedure start (IA64)

// Local symbols for IL
pub const S_LOCALSLOT_ST  : u16 = 0x1017;  // local IL sym with field for local slot index
pub const S_PARAMSLOT_ST  : u16 = 0x1018;  // local IL sym with field for parameter slot index

pub const S_ANNOTATION    : u16 = 0x1019;  // Annotation string literals

// symbols to support managed code debugging
pub const S_GMANPROC_ST   : u16 = 0x101a;  // Global proc
pub const S_LMANPROC_ST   : u16 = 0x101b;  // Local proc
pub const S_RESERVED1     : u16 = 0x101c;  // reserved
pub const S_RESERVED2     : u16 = 0x101d;  // reserved
pub const S_RESERVED3     : u16 = 0x101e;  // reserved
pub const S_RESERVED4     : u16 = 0x101f;  // reserved
pub const S_LMANDATA_ST   : u16 = 0x1020;
pub const S_GMANDATA_ST   : u16 = 0x1021;
pub const S_MANFRAMEREL_ST: u16 = 0x1022;
pub const S_MANREGISTER_ST: u16 = 0x1023;
pub const S_MANSLOT_ST    : u16 = 0x1024;
pub const S_MANMANYREG_ST : u16 = 0x1025;
pub const S_MANREGREL_ST  : u16 = 0x1026;
pub const S_MANMANYREG2_ST: u16 = 0x1027;
pub const S_MANTYPREF     : u16 = 0x1028;  // Index for type referenced by name from metadata
pub const S_UNAMESPACE_ST : u16 = 0x1029;  // Using namespace

// Symbols w/ SZ name fields. All name fields contain utf8 encoded strings.
pub const S_ST_MAX        : u16 = 0x1100;  // starting point for SZ name symbols

pub const S_OBJNAME       : u16 = 0x1101;  // path to object file name
pub const S_THUNK32       : u16 = 0x1102;  // Thunk Start
pub const S_BLOCK32       : u16 = 0x1103;  // block start
pub const S_WITH32        : u16 = 0x1104;  // with start
pub const S_LABEL32       : u16 = 0x1105;  // code label
pub const S_REGISTER      : u16 = 0x1106;  // Register variable
pub const S_CONSTANT      : u16 = 0x1107;  // constant symbol
pub const S_UDT           : u16 = 0x1108;  // User defined type
pub const S_COBOLUDT      : u16 = 0x1109;  // special UDT for cobol that does not symbol pack
pub const S_MANYREG       : u16 = 0x110a;  // multiple register variable
pub const S_BPREL32       : u16 = 0x110b;  // BP-relative
pub const S_LDATA32       : u16 = 0x110c;  // Module-local symbol
pub const S_GDATA32       : u16 = 0x110d;  // Global data symbol
pub const S_PUB32         : u16 = 0x110e;  // a public symbol (CV internal reserved)
pub const S_LPROC32       : u16 = 0x110f;  // Local procedure start
pub const S_GPROC32       : u16 = 0x1110;  // Global procedure start
pub const S_REGREL32      : u16 = 0x1111;  // register relative address
pub const S_LTHREAD32     : u16 = 0x1112;  // local thread storage
pub const S_GTHREAD32     : u16 = 0x1113;  // global thread storage

pub const S_LPROCMIPS     : u16 = 0x1114;  // Local procedure start
pub const S_GPROCMIPS     : u16 = 0x1115;  // Global procedure start
pub const S_COMPILE2      : u16 = 0x1116;  // extended compile flags and info
pub const S_MANYREG2      : u16 = 0x1117;  // multiple register variable
pub const S_LPROCIA64     : u16 = 0x1118;  // Local procedure start (IA64)
pub const S_GPROCIA64     : u16 = 0x1119;  // Global procedure start (IA64)
pub const S_LOCALSLOT     : u16 = 0x111a;  // local IL sym with field for local slot index
pub const S_PARAMSLOT     : u16 = 0x111b;  // local IL sym with field for parameter slot index

// symbols to support managed code debugging
pub const S_LMANDATA      : u16 = 0x111c;
pub const S_GMANDATA      : u16 = 0x111d;
pub const S_MANFRAMEREL   : u16 = 0x111e;
pub const S_MANREGISTER   : u16 = 0x111f;
pub const S_MANSLOT       : u16 = 0x1120;
pub const S_MANMANYREG    : u16 = 0x1121;
pub const S_MANREGREL     : u16 = 0x1122;
pub const S_MANMANYREG2   : u16 = 0x1123;
pub const S_UNAMESPACE    : u16 = 0x1124;  // Using namespace

// ref symbols with name fields
pub const S_PROCREF       : u16 = 0x1125;  // Reference to a procedure
pub const S_DATAREF       : u16 = 0x1126;  // Reference to data
pub const S_LPROCREF      : u16 = 0x1127;  // Local Reference to a procedure
pub const S_ANNOTATIONREF : u16 = 0x1128;  // Reference to an S_ANNOTATION symbol
pub const S_TOKENREF      : u16 = 0x1129;  // Reference to one of the many MANPROCSYM's

// continuation of managed symbols
pub const S_GMANPROC      : u16 = 0x112a;  // Global proc
pub const S_LMANPROC      : u16 = 0x112b;  // Local proc

// short light-weight thunks
pub const S_TRAMPOLINE    : u16 = 0x112c;  // trampoline thunks
pub const S_MANCONSTANT   : u16 = 0x112d;  // constants with metadata type info

// native attributed local/parms
pub const S_ATTR_FRAMEREL : u16 = 0x112e;  // relative to virtual frame ptr
pub const S_ATTR_REGISTER : u16 = 0x112f;  // stored in a register
pub const S_ATTR_REGREL   : u16 = 0x1130;  // relative to register (alternate frame ptr)
pub const S_ATTR_MANYREG  : u16 = 0x1131;  // stored in >1 register

// Separated code (from the compiler) support
pub const S_SEPCODE       : u16 = 0x1132;

pub const S_LOCAL_2005    : u16 = 0x1133;  // defines a local symbol in optimized code
pub const S_DEFRANGE_2005 : u16 = 0x1134;  // defines a single range of addresses in which symbol can be evaluated
pub const S_DEFRANGE2_2005 : u16 = 0x1135;  // defines ranges of addresses in which symbol can be evaluated

pub const S_SECTION       : u16 = 0x1136;  // A COFF section in a PE executable
pub const S_COFFGROUP     : u16 = 0x1137;  // A COFF group
pub const S_EXPORT        : u16 = 0x1138;  // A export

pub const S_CALLSITEINFO  : u16 = 0x1139;  // Indirect call site information
pub const S_FRAMECOOKIE   : u16 = 0x113a;  // Security cookie information

pub const S_DISCARDED     : u16 = 0x113b;  // Discarded by LINK /OPT:REF (experimental see richards)

pub const S_COMPILE3      : u16 = 0x113c;  // Replacement for S_COMPILE2
pub const S_ENVBLOCK      : u16 = 0x113d;  // Environment block split off from S_COMPILE2

pub const S_LOCAL                   : u16 = 0x113e;  // defines a local symbol in optimized code
pub const S_DEFRANGE                : u16 = 0x113f;  // defines a single range of addresses in which symbol can be evaluated
pub const S_DEFRANGE_SUBFIELD       : u16 = 0x1140;           // ranges for a subfield

pub const S_DEFRANGE_REGISTER               : u16 = 0x1141;           // ranges for en-registered symbol
pub const S_DEFRANGE_FRAMEPOINTER_REL       : u16 = 0x1142;   // range for stack symbol.
pub const S_DEFRANGE_SUBFIELD_REGISTER      : u16 = 0x1143;  // ranges for en-registered field of symbol
pub const S_DEFRANGE_FRAMEPOINTER_REL_FULL_SCOPE : u16 = 0x1144; // range for stack symbol span valid full scope of function body gap might apply.
pub const S_DEFRANGE_REGISTER_REL           : u16 = 0x1145; // range for symbol address as register + offset.

// S_PROC symbols that reference ID instead of type
pub const S_LPROC32_ID     : u16 = 0x1146;
pub const S_GPROC32_ID     : u16 = 0x1147;
pub const S_LPROCMIPS_ID   : u16 = 0x1148;
pub const S_GPROCMIPS_ID   : u16 = 0x1149;
pub const S_LPROCIA64_ID   : u16 = 0x114a;
pub const S_GPROCIA64_ID   : u16 = 0x114b;

pub const S_BUILDINFO      : u16 = 0x114c; // build information.
pub const S_INLINESITE     : u16 = 0x114d; // inlined function callsite.
pub const S_INLINESITE_END : u16 = 0x114e;
pub const S_PROC_ID_END    : u16 = 0x114f;

pub const S_DEFRANGE_HLSL  : u16 = 0x1150;
pub const S_GDATA_HLSL     : u16 = 0x1151;
pub const S_LDATA_HLSL     : u16 = 0x1152;

pub const S_FILESTATIC     : u16 = 0x1153;

pub const S_LOCAL_DPC_GROUPSHARED : u16 = 0x1154; // DPC groupshared variable
pub const S_LPROC32_DPC     : u16 = 0x1155; // DPC local procedure start
pub const S_LPROC32_DPC_ID : u16 = 0x1156;
pub const S_DEFRANGE_DPC_PTR_TAG : u16 = 0x1157; // DPC pointer tag definition range
pub const S_DPC_SYM_TAG_MAP : u16 = 0x1158; // DPC pointer tag value to symbol record map

pub const S_ARMSWITCHTABLE  : u16 = 0x1159;
pub const S_CALLEES         : u16 = 0x115a;
pub const S_CALLERS         : u16 = 0x115b;
pub const S_POGODATA        : u16 = 0x115c;
pub const S_INLINESITE2     : u16 = 0x115d;      // extended inline site information

pub const S_HEAPALLOCSITE   : u16 = 0x115e;    // heap allocation site

pub const S_MOD_TYPEREF     : u16 = 0x115f;      // only generated at link time

pub const S_REF_MINIPDB     : u16 = 0x1160;      // only generated at link time for mini PDB
pub const S_PDBMAP          : u16 = 0x1161;      // only generated at link time for mini PDB

pub const S_GDATA_HLSL32    : u16 = 0x1162;
pub const S_LDATA_HLSL32    : u16 = 0x1163;

pub const S_GDATA_HLSL32_EX : u16 = 0x1164;
pub const S_LDATA_HLSL32_EX : u16 = 0x1165;


/// These values correspond to the CV_CPU_TYPE_e enumeration, and are documented
/// here: https://msdn.microsoft.com/en-us/library/b2fc64ek.aspx
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum CPUType {
    Intel8080 = 0x0,
    Intel8086 = 0x1,
    Intel80286 = 0x2,
    Intel80386 = 0x3,
    Intel80486 = 0x4,
    Pentium = 0x5,
    PentiumPro = 0x6,
    Pentium3 = 0x7,
    MIPS = 0x10,
    MIPS16 = 0x11,
    MIPS32 = 0x12,
    MIPS64 = 0x13,
    MIPSI = 0x14,
    MIPSII = 0x15,
    MIPSIII = 0x16,
    MIPSIV = 0x17,
    MIPSV = 0x18,
    M68000 = 0x20,
    M68010 = 0x21,
    M68020 = 0x22,
    M68030 = 0x23,
    M68040 = 0x24,
    Alpha = 0x30,
    Alpha21164 = 0x31,
    Alpha21164A = 0x32,
    Alpha21264 = 0x33,
    Alpha21364 = 0x34,
    PPC601 = 0x40,
    PPC603 = 0x41,
    PPC604 = 0x42,
    PPC620 = 0x43,
    PPCFP = 0x44,
    PPCBE = 0x45,
    SH3 = 0x50,
    SH3E = 0x51,
    SH3DSP = 0x52,
    SH4 = 0x53,
    SHMedia = 0x54,
    ARM3 = 0x60,
    ARM4 = 0x61,
    ARM4T = 0x62,
    ARM5 = 0x63,
    ARM5T = 0x64,
    ARM6 = 0x65,
    ARM_XMAC = 0x66,
    ARM_WMMX = 0x67,
    ARM7 = 0x68,
    ARM64 = 0x69,
    Omni = 0x70,
    Ia64 = 0x80,
    Ia64_2 = 0x81,
    CEE = 0x90,
    AM33 = 0xa0,
    M32R = 0xb0,
    TriCore = 0xc0,
    X64 = 0xd0,
    EBC = 0xe0,
    Thumb = 0xf0,
    ARMNT = 0xf4,
    D3D11_Shader = 0x100,
}

impl ::std::fmt::Display for CPUType {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            CPUType::Intel8080 => write!(f, "Intel8080"),
            CPUType::Intel8086 => write!(f, "Intel8086"),
            CPUType::Intel80286 => write!(f, "Intel80286"),
            CPUType::Intel80386 => write!(f, "Intel80386"),
            CPUType::Intel80486 => write!(f, "Intel80486"),
            CPUType::Pentium => write!(f, "Pentium"),
            CPUType::PentiumPro => write!(f, "PentiumPro"),
            CPUType::Pentium3 => write!(f, "Pentium3"),
            CPUType::MIPS => write!(f, "MIPS"),
            CPUType::MIPS16 => write!(f, "MIPS16"),
            CPUType::MIPS32 => write!(f, "MIPS32"),
            CPUType::MIPS64 => write!(f, "MIPS64"),
            CPUType::MIPSI => write!(f, "MIPSI"),
            CPUType::MIPSII => write!(f, "MIPSII"),
            CPUType::MIPSIII => write!(f, "MIPSIII"),
            CPUType::MIPSIV => write!(f, "MIPSIV"),
            CPUType::MIPSV => write!(f, "MIPSV"),
            CPUType::M68000 => write!(f, "M68000"),
            CPUType::M68010 => write!(f, "M68010"),
            CPUType::M68020 => write!(f, "M68020"),
            CPUType::M68030 => write!(f, "M68030"),
            CPUType::M68040 => write!(f, "M68040"),
            CPUType::Alpha => write!(f, "Alpha"),
            CPUType::Alpha21164 => write!(f, "Alpha21164"),
            CPUType::Alpha21164A => write!(f, "Alpha21164A"),
            CPUType::Alpha21264 => write!(f, "Alpha21264"),
            CPUType::Alpha21364 => write!(f, "Alpha21364"),
            CPUType::PPC601 => write!(f, "PPC601"),
            CPUType::PPC603 => write!(f, "PPC603"),
            CPUType::PPC604 => write!(f, "PPC604"),
            CPUType::PPC620 => write!(f, "PPC620"),
            CPUType::PPCFP => write!(f, "PPCFP"),
            CPUType::PPCBE => write!(f, "PPCBE"),
            CPUType::SH3 => write!(f, "SH3"),
            CPUType::SH3E => write!(f, "SH3E"),
            CPUType::SH3DSP => write!(f, "SH3DSP"),
            CPUType::SH4 => write!(f, "SH4"),
            CPUType::SHMedia => write!(f, "SHMedia"),
            CPUType::ARM3 => write!(f, "ARM3"),
            CPUType::ARM4 => write!(f, "ARM4"),
            CPUType::ARM4T => write!(f, "ARM4T"),
            CPUType::ARM5 => write!(f, "ARM5"),
            CPUType::ARM5T => write!(f, "ARM5T"),
            CPUType::ARM6 => write!(f, "ARM6"),
            CPUType::ARM_XMAC => write!(f, "ARM_XMAC"),
            CPUType::ARM_WMMX => write!(f, "ARM_WMMX"),
            CPUType::ARM7 => write!(f, "ARM7"),
            CPUType::ARM64 => write!(f, "ARM64"),
            CPUType::Omni => write!(f, "Omni"),
            CPUType::Ia64 => write!(f, "Ia64"),
            CPUType::Ia64_2 => write!(f, "Ia64_2"),
            CPUType::CEE => write!(f, "CEE"),
            CPUType::AM33 => write!(f, "AM33"),
            CPUType::M32R => write!(f, "M32R"),
            CPUType::TriCore => write!(f, "TriCore"),
            CPUType::X64 => write!(f, "X64"),
            CPUType::EBC => write!(f, "EBC"),
            CPUType::Thumb => write!(f, "Thumb"),
            CPUType::ARMNT => write!(f, "ARMNT"),
            CPUType::D3D11_Shader => write!(f, "D3D11_Shader"),
        }
    }
}

impl From<u16> for CPUType {
    fn from(value: u16) -> Self {
        match value {
            0x0 => CPUType::Intel8080,
            0x1 => CPUType::Intel8086,
            0x2 => CPUType::Intel80286,
            0x3 => CPUType::Intel80386,
            0x4 => CPUType::Intel80486,
            0x5 => CPUType::Pentium,
            0x6 => CPUType::PentiumPro,
            0x7 => CPUType::Pentium3,
            0x10 => CPUType::MIPS,
            0x11 => CPUType::MIPS16,
            0x12 => CPUType::MIPS32,
            0x13 => CPUType::MIPS64,
            0x14 => CPUType::MIPSI,
            0x15 => CPUType::MIPSII,
            0x16 => CPUType::MIPSIII,
            0x17 => CPUType::MIPSIV,
            0x18 => CPUType::MIPSV,
            0x20 => CPUType::M68000,
            0x21 => CPUType::M68010,
            0x22 => CPUType::M68020,
            0x23 => CPUType::M68030,
            0x24 => CPUType::M68040,
            0x30 => CPUType::Alpha,
            0x31 => CPUType::Alpha21164,
            0x32 => CPUType::Alpha21164A,
            0x33 => CPUType::Alpha21264,
            0x34 => CPUType::Alpha21364,
            0x40 => CPUType::PPC601,
            0x41 => CPUType::PPC603,
            0x42 => CPUType::PPC604,
            0x43 => CPUType::PPC620,
            0x44 => CPUType::PPCFP,
            0x45 => CPUType::PPCBE,
            0x50 => CPUType::SH3,
            0x51 => CPUType::SH3E,
            0x52 => CPUType::SH3DSP,
            0x53 => CPUType::SH4,
            0x54 => CPUType::SHMedia,
            0x60 => CPUType::ARM3,
            0x61 => CPUType::ARM4,
            0x62 => CPUType::ARM4T,
            0x63 => CPUType::ARM5,
            0x64 => CPUType::ARM5T,
            0x65 => CPUType::ARM6,
            0x66 => CPUType::ARM_XMAC,
            0x67 => CPUType::ARM_WMMX,
            0x68 => CPUType::ARM7,
            0x69 => CPUType::ARM64,
            0x70 => CPUType::Omni,
            0x80 => CPUType::Ia64,
            0x81 => CPUType::Ia64_2,
            0x90 => CPUType::CEE,
            0xa0 => CPUType::AM33,
            0xb0 => CPUType::M32R,
            0xc0 => CPUType::TriCore,
            0xd0 => CPUType::X64,
            0xe0 => CPUType::EBC,
            0xf0 => CPUType::Thumb,
            0xf4 => CPUType::ARMNT,
            0x100 => CPUType::D3D11_Shader,
            _=> CPUType::Intel8080, // This enum doesn't have an unknown value, so we just force it to Intel8080 since it's 0x0.
        }
    }
}

/// These values correspond to the CV_CFL_LANG enumeration, and are documented
/// here: https://msdn.microsoft.com/en-us/library/bw3aekw6.aspx
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SourceLanguage {
    C = 0x00,
    Cpp = 0x01,
    Fortran = 0x02,
    Masm = 0x03,
    Pascal = 0x04,
    Basic = 0x05,
    Cobol = 0x06,
    Link = 0x07,
    Cvtres = 0x08,
    Cvtpgd = 0x09,
    CSharp = 0x0a,
    VB = 0x0b,
    ILAsm = 0x0c,
    Java = 0x0d,
    JScript = 0x0e,
    MSIL = 0x0f,
    HLSL = 0x10,

    /// The DMD compiler emits 'D' for the CV source language. Microsoft doesn't
    /// have an enumerator for it yet.
    D = 0x44,
}

impl ::std::fmt::Display for SourceLanguage {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            SourceLanguage::C => write!(f, "C"),
            SourceLanguage::Cpp => write!(f, "Cpp"),
            SourceLanguage::Fortran => write!(f, "Fortran"),
            SourceLanguage::Masm => write!(f, "Masm"),
            SourceLanguage::Pascal => write!(f, "Pascal"),
            SourceLanguage::Basic => write!(f, "Basic"),
            SourceLanguage::Cobol => write!(f, "Cobol"),
            SourceLanguage::Link => write!(f, "Link"),
            SourceLanguage::Cvtres => write!(f, "Cvtres"),
            SourceLanguage::Cvtpgd => write!(f, "Cvtpgd"),
            SourceLanguage::CSharp => write!(f, "CSharp"),
            SourceLanguage::VB => write!(f, "VB"),
            SourceLanguage::ILAsm => write!(f, "ILAsm"),
            SourceLanguage::Java => write!(f, "Java"),
            SourceLanguage::JScript => write!(f, "JScript"),
            SourceLanguage::MSIL => write!(f, "MSIL"),
            SourceLanguage::HLSL => write!(f, "HLSL"),
            SourceLanguage::D => write!(f, "D"),
        }
    }
}

impl From<u8> for SourceLanguage {
    fn from(value: u8) -> Self {
        match value {
            0x00 => SourceLanguage::C,
            0x01 => SourceLanguage::Cpp,
            0x02 => SourceLanguage::Fortran,
            0x03 => SourceLanguage::Masm,
            0x04 => SourceLanguage::Pascal,
            0x05 => SourceLanguage::Basic,
            0x06 => SourceLanguage::Cobol,
            0x07 => SourceLanguage::Link,
            0x08 => SourceLanguage::Cvtres,
            0x09 => SourceLanguage::Cvtpgd,
            0x0a => SourceLanguage::CSharp,
            0x0b => SourceLanguage::VB,
            0x0c => SourceLanguage::ILAsm,
            0x0d => SourceLanguage::Java,
            0x0e => SourceLanguage::JScript,
            0x0f => SourceLanguage::MSIL,
            0x10 => SourceLanguage::HLSL,
            0x44 => SourceLanguage::D,
            _ => SourceLanguage::Masm, // There is no unknown, so we just force to Masm as the default.
        }
    }
}
