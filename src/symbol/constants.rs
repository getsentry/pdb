// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

// A list of known symbol kinds.
// from:
//  https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2735

#![allow(dead_code,non_upper_case_globals)]

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

