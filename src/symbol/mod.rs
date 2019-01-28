// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use std::fmt;
use std::result;

use crate::common::*;
use crate::msf::*;
use crate::FallibleIterator;

mod constants;
use self::constants::*;

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
///         Ok(pdb::SymbolData::PublicSymbol(data)) if data.function => {
///             // we found the location of a function!
///             let rva = data.offset.to_rva(&address_map).unwrap_or_default();
///             println!("{} is {}", rva, symbol.name()?);
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
    pub fn raw_kind(&self) -> u16 {
        debug_assert!(self.0.len() >= 2);

        // assemble a little-endian u16
        u16::from(self.0[0]) | (u16::from(self.0[1]) << 8)
    }

    /// Returns the raw bytes of this symbol record, including the symbol type but not including
    /// the preceding symbol length indicator.
    pub fn raw_bytes(&self) -> &'t [u8] {
        self.0
    }

    /// Returns the size of the fixed-size fields for this kind of symbol. This permits other
    /// accessors to extract the fields independent from the names.
    fn data_length(&self) -> Result<usize> {
        let kind = self.raw_kind();

        let data_length = match kind {
            S_PUB32 | S_PUB32_ST => 10,

            S_LDATA32 | S_LDATA32_ST | S_GDATA32 | S_GDATA32_ST | S_LMANDATA | S_LMANDATA_ST
            | S_GMANDATA | S_GMANDATA_ST => 10,

            S_PROCREF | S_PROCREF_ST | S_LPROCREF | S_LPROCREF_ST | S_DATAREF | S_DATAREF_ST
            | S_ANNOTATIONREF => 10,

            S_CONSTANT | S_CONSTANT_ST => {
                let mut constant_size = 4;

                let mut buf = ParseBuffer::from(&self.0[2 + constant_size..]);
                constant_size += buf.get_variant_size();

                constant_size
            }

            S_UDT | S_UDT_ST => 4,

            S_LTHREAD32 | S_LTHREAD32_ST | S_GTHREAD32 | S_GTHREAD32_ST => 10,

            S_LPROC32 | S_LPROC32_ST | S_GPROC32 | S_GPROC32_ST | S_LPROC32_ID | S_GPROC32_ID
            | S_LPROC32_DPC | S_LPROC32_DPC_ID => 35,

            S_OBJNAME | S_OBJNAME_ST => 4,

            S_COMPILE3 => 22,

            S_UNAMESPACE | S_UNAMESPACE_ST => 0,

            _ => return Err(Error::UnimplementedSymbolKind(kind)),
        };

        if self.0.len() < data_length + 2 {
            return Err(Error::SymbolTooShort);
        }

        Ok(data_length)
    }

    /// Parse the symbol into the `SymbolData` it contains.
    #[inline]
    pub fn parse(&self) -> Result<SymbolData> {
        parse_symbol_data(self.raw_kind(), self.field_data()?)
    }

    /// Returns a slice containing the field information describing this symbol but not including
    /// its name.
    fn field_data(&self) -> Result<&'t [u8]> {
        let data_length = self.data_length()?;

        // we've already checked the length
        Ok(&self.0[2..(data_length + 2)])
    }

    /// Returns the name of the symbol. Note that the underlying buffer is owned by the
    /// `SymbolTable`.
    pub fn name(&self) -> Result<RawString<'t>> {
        // figure out how long the data is
        let data_length = self.data_length()?;

        // figure out where the name is
        let mut buf = ParseBuffer::from(&self.0[2 + data_length..]);

        // names come in two varieties:
        if self.raw_kind() < S_ST_MAX {
            // Pascal-style name
            let name = buf.parse_u8_pascal_string()?;
            Ok(name)
        } else {
            // NUL-terminated name
            let name = buf.parse_cstring()?;
            Ok(name)
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

// CV_PUBSYMFLAGS_e:
const CVPSF_CODE: u32 = 0x1;
const CVPSF_FUNCTION: u32 = 0x2;
const CVPSF_MANAGED: u32 = 0x4;
const CVPSF_MSIL: u32 = 0x8;

fn parse_symbol_data(kind: u16, data: &[u8]) -> Result<SymbolData> {
    let mut buf = ParseBuffer::from(data);

    match kind {
        S_PUB32 | S_PUB32_ST => {
            let flags = buf.parse_u32()?;
            Ok(SymbolData::PublicSymbol(PublicSymbol {
                code: flags & CVPSF_CODE != 0,
                function: flags & CVPSF_FUNCTION != 0,
                managed: flags & CVPSF_MANAGED != 0,
                msil: flags & CVPSF_MSIL != 0,
                offset: PdbInternalSectionOffset {
                    offset: buf.parse_u32()?,
                    section: buf.parse_u16()?,
                },
            }))
        }

        S_LDATA32 | S_LDATA32_ST | S_GDATA32 | S_GDATA32_ST | S_LMANDATA | S_LMANDATA_ST
        | S_GMANDATA | S_GMANDATA_ST => Ok(SymbolData::DataSymbol(DataSymbol {
            global: match kind {
                S_GDATA32 | S_GDATA32_ST | S_GMANDATA | S_GMANDATA_ST => true,
                _ => false,
            },
            managed: match kind {
                S_LMANDATA | S_LMANDATA_ST | S_GMANDATA | S_GMANDATA_ST => true,
                _ => false,
            },
            type_index: buf.parse_u32()?,
            offset: PdbInternalSectionOffset {
                offset: buf.parse_u32()?,
                section: buf.parse_u16()?,
            },
        })),

        S_PROCREF | S_PROCREF_ST | S_LPROCREF | S_LPROCREF_ST => {
            Ok(SymbolData::ProcedureReference(ProcedureReferenceSymbol {
                global: match kind {
                    S_PROCREF | S_PROCREF_ST => true,
                    _ => false,
                },
                sum_name: buf.parse_u32()?,
                symbol_index: buf.parse_u32()?,
                module: buf.parse_u16()?,
            }))
        }

        S_DATAREF | S_DATAREF_ST => Ok(SymbolData::DataReference(DataReferenceSymbol {
            sum_name: buf.parse_u32()?,
            symbol_index: buf.parse_u32()?,
            module: buf.parse_u16()?,
        })),

        S_ANNOTATIONREF => Ok(SymbolData::AnnotationReference(AnnotationReferenceSymbol {
            sum_name: buf.parse_u32()?,
            symbol_index: buf.parse_u32()?,
            module: buf.parse_u16()?,
        })),

        S_CONSTANT | S_CONSTANT_ST => Ok(SymbolData::Constant(ConstantSymbol {
            type_index: buf.parse_u32()?,
            value: buf.parse_variant()?,
        })),

        S_UDT | S_UDT_ST => Ok(SymbolData::UserDefinedType(UserDefinedTypeSymbol {
            type_index: buf.parse_u32()?,
        })),

        S_LTHREAD32 | S_LTHREAD32_ST | S_GTHREAD32 | S_GTHREAD32_ST => {
            Ok(SymbolData::ThreadStorage(ThreadStorageSymbol {
                global: match kind {
                    S_GTHREAD32 | S_GTHREAD32_ST => true,
                    _ => false,
                },
                type_index: buf.parse_u32()?,
                offset: PdbInternalSectionOffset {
                    offset: buf.parse_u32()?,
                    section: buf.parse_u16()?,
                },
            }))
        }

        S_LPROC32 | S_LPROC32_ST | S_GPROC32 | S_GPROC32_ST | S_LPROC32_ID | S_GPROC32_ID
        | S_LPROC32_DPC | S_LPROC32_DPC_ID => Ok(SymbolData::Procedure(ProcedureSymbol {
            global: match kind {
                S_GPROC32 | S_GPROC32_ST | S_GPROC32_ID => true,
                _ => false,
            },
            parent: buf.parse_u32()?,
            end: buf.parse_u32()?,
            next: buf.parse_u32()?,
            len: buf.parse_u32()?,
            dbg_start_offset: buf.parse_u32()?,
            dbg_end_offset: buf.parse_u32()?,
            type_index: buf.parse_u32()?,
            offset: PdbInternalSectionOffset {
                offset: buf.parse_u32()?,
                section: buf.parse_u16()?,
            },
            flags: ProcedureFlags::new(buf.parse_u8()?),
        })),

        S_OBJNAME | S_OBJNAME_ST => Ok(SymbolData::ObjName(ObjNameSymbol {
            signature: buf.parse_u32()?,
        })),

        S_COMPILE3 => Ok(SymbolData::Compile3(Compile3Symbol {
            language: buf.parse_u8()?.into(),
            flags: [buf.parse_u8()?, buf.parse_u8()?, buf.parse_u8()?],
            cpu_type: buf.parse_u16()?.into(),
            frontend_version: [
                buf.parse_u16()?,
                buf.parse_u16()?,
                buf.parse_u16()?,
                buf.parse_u16()?,
            ],
            backend_version: [
                buf.parse_u16()?,
                buf.parse_u16()?,
                buf.parse_u16()?,
                buf.parse_u16()?,
            ],
        })),

        S_UNAMESPACE | S_UNAMESPACE_ST => Ok(SymbolData::Namespace(NamespaceSymbol {})),

        _ => Err(Error::UnimplementedSymbolKind(kind)),
    }
}

/// `SymbolData` contains the information parsed from a symbol record.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum SymbolData {
    // S_PUB32 (0x110e) | S_PUB32_ST (0x1009)
    PublicSymbol(PublicSymbol),

    //   S_LDATA32 (0x110c) | S_LDATA32_ST (0x1007)
    //   S_GDATA32 (0x110d) | S_GDATA32_ST (0x1008)
    //  S_LMANDATA (0x111c) | S_LMANDATA_ST (0x1020)
    //  S_GMANDATA (0x111d) | S_GMANDATA_ST (0x1021)
    DataSymbol(DataSymbol),

    //   S_PROCREF (0x1125) |  S_PROCREF_ST (0x0400)
    //  S_LPROCREF (0x1127) | S_LPROCREF_ST (0x0403)
    ProcedureReference(ProcedureReferenceSymbol),

    //   S_DATAREF (0x1126) |  S_DATAREF_ST (0x0401)
    DataReference(DataReferenceSymbol),

    // S_ANNOTATIONREF (0x1128)
    AnnotationReference(AnnotationReferenceSymbol),

    //  S_CONSTANT (0x1107) | S_CONSTANT_ST (0x1002)
    Constant(ConstantSymbol),

    //       S_UDT (0x1108) | S_UDT_ST (0x1003)
    UserDefinedType(UserDefinedTypeSymbol),

    // S_LTHREAD32 (0x1112) | S_LTHREAD32_ST (0x100e)
    // S_GTHREAD32 (0x1113) | S_GTHREAD32_ST (0x100f)
    ThreadStorage(ThreadStorageSymbol),

    // S_LPROC32 (0x110f) | S_LPROC32_ST (0x100a)
    // S_GPROC32 (0x1110) | S_GPROC32_ST (0x100b)
    // S_LPROC32_ID (0x1146) |
    // S_GPROC32_ID (0x1147) |
    // S_LPROC32_DPC (0x1155) |
    // S_LPROC32_DPC_ID (0x1156)
    Procedure(ProcedureSymbol),

    // S_OBJNAME (0x1101) | S_OBJNAME_ST (0x0009)
    ObjName(ObjNameSymbol),

    // S_COMPILE3 (0x113c)
    Compile3(Compile3Symbol),

    // S_UNAMESPACE (0x1124) | S_UNAMESPACE_ST (0x1029)
    Namespace(NamespaceSymbol),
}

/// The information parsed from a symbol record with kind `S_PUB32` or `S_PUB32_ST`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct PublicSymbol {
    pub code: bool,
    pub function: bool,
    pub managed: bool,
    pub msil: bool,
    pub offset: PdbInternalSectionOffset,
}

/// The information parsed from a symbol record with kind
/// `S_LDATA32`, `S_LDATA32_ST`, `S_GDATA32`, `S_GDATA32_ST`,
/// `S_LMANDATA`, `S_LMANDATA_ST`, `S_GMANDATA`, or `S_GMANDATA_ST`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct DataSymbol {
    pub global: bool,
    pub managed: bool,
    pub type_index: TypeIndex,
    pub offset: PdbInternalSectionOffset,
}

/// The information parsed from a symbol record with kind
/// `S_PROCREF`, `S_PROCREF_ST`, `S_LPROCREF`, or `S_LPROCREF_ST`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ProcedureReferenceSymbol {
    pub global: bool,
    pub sum_name: u32,
    pub symbol_index: u32,
    pub module: u16,
}

/// The information parsed from a symbol record with kind `S_DATAREF` or `S_DATAREF_ST`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct DataReferenceSymbol {
    pub sum_name: u32,
    pub symbol_index: u32,
    pub module: u16,
}

/// The information parsed from a symbol record with kind `S_ANNOTATIONREF`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct AnnotationReferenceSymbol {
    pub sum_name: u32,
    pub symbol_index: u32,
    pub module: u16,
}

/// The information parsed from a symbol record with kind `S_CONSTANT`, or `S_CONSTANT_ST`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ConstantSymbol {
    pub type_index: TypeIndex,
    pub value: Variant,
}

/// The information parsed from a symbol record with kind `S_UDT`, or `S_UDT_ST`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct UserDefinedTypeSymbol {
    pub type_index: TypeIndex,
}

/// The information parsed from a symbol record with kind
/// `S_LTHREAD32`, `S_LTHREAD32_ST`, `S_GTHREAD32`, or `S_GTHREAD32_ST`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ThreadStorageSymbol {
    pub global: bool,
    pub type_index: TypeIndex,
    pub offset: PdbInternalSectionOffset,
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
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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

impl ProcedureFlags {
    fn new(flags: u8) -> Self {
        ProcedureFlags {
            nofpo: flags & CV_PFLAG_NOFPO != 0,
            int: flags & CV_PFLAG_INT != 0,
            far: flags & CV_PFLAG_FAR != 0,
            never: flags & CV_PFLAG_NEVER != 0,
            notreached: flags & CV_PFLAG_NOTREACHED != 0,
            cust_call: flags & CV_PFLAG_CUST_CALL != 0,
            noinline: flags & CV_PFLAG_NOINLINE != 0,
            optdbginfo: flags & CV_PFLAG_OPTDBGINFO != 0,
        }
    }
}

/// The information parsed from a symbol record with kind
/// `S_GPROC32`, `S_GPROC32_ST`, `S_LPROC32`, `S_LPROC32_ST`
/// `S_GPROC32_ID`, `S_LPROC32_ID`, `S_LPROC32_DPC`, or `S_LPROC32_DPC_ID`
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ProcedureSymbol {
    pub global: bool,
    pub parent: u32,
    pub end: u32,
    pub next: u32,
    pub len: u32,
    pub dbg_start_offset: u32,
    pub dbg_end_offset: u32,
    pub type_index: TypeIndex,
    pub offset: PdbInternalSectionOffset,
    pub flags: ProcedureFlags,
}

/// The information parsed from a symbol record with kind
/// `S_OBJNAME`, or `S_OBJNAME_ST`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct ObjNameSymbol {
    pub signature: u32,
}

/// The information parsed from a symbol record with kind
/// `S_COMPILE3`
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Compile3Symbol {
    pub language: SourceLanguage,
    pub flags: [u8; 3],
    pub cpu_type: CPUType,
    pub frontend_version: [u16; 4],
    pub backend_version: [u16; 4],
}

/// The information parsed from a symbol record with kind
/// `S_UNAMESPACE`, or `S_UNAMESPACE_ST`.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct NamespaceSymbol {}

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
}

impl<'t> FallibleIterator for SymbolIter<'t> {
    type Item = Symbol<'t>;
    type Error = Error;

    fn next(&mut self) -> result::Result<Option<Self::Item>, Self::Error> {
        // see if we're at EOF
        if self.buf.len() == 0 {
            return Ok(None);
        }

        // read the length of the next symbol
        let symbol_length = self.buf.parse_u16()? as usize;

        // validate
        if symbol_length < 2 {
            // this can't be correct
            return Err(Error::SymbolTooShort);
        }

        // grab the symbol itself
        let symbol = self.buf.take(symbol_length)?;

        // Done
        Ok(Some(Symbol(symbol)))
    }
}

#[cfg(test)]
mod tests {
    mod parsing {
        use crate::common::*;
        use crate::symbol::*;

        fn parse<'s>(buf: &'s [u8]) -> Result<(Symbol<'s>, SymbolData, String)> {
            let symbol = Symbol(buf);

            let data = symbol.parse()?;
            let name = symbol.name()?.to_string().into_owned();

            Ok((symbol, data, name))
        }

        #[test]
        fn kind_110e() {
            let buf = &[
                14, 17, 2, 0, 0, 0, 192, 85, 0, 0, 1, 0, 95, 95, 108, 111, 99, 97, 108, 95, 115,
                116, 100, 105, 111, 95, 112, 114, 105, 110, 116, 102, 95, 111, 112, 116, 105, 111,
                110, 115, 0, 0,
            ];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x110e);
            assert_eq!(
                data,
                SymbolData::PublicSymbol(PublicSymbol {
                    code: false,
                    function: true,
                    managed: false,
                    msil: false,
                    offset: PdbInternalSectionOffset {
                        offset: 21952,
                        section: 1
                    }
                })
            );
            assert_eq!(name, "__local_stdio_printf_options");
        }

        #[test]
        fn kind_1125() {
            let buf = &[
                37, 17, 0, 0, 0, 0, 108, 0, 0, 0, 1, 0, 66, 97, 122, 58, 58, 102, 95, 112, 117, 98,
                108, 105, 99, 0,
            ];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x1125);
            assert_eq!(
                data,
                SymbolData::ProcedureReference(ProcedureReferenceSymbol {
                    global: true,
                    sum_name: 0,
                    symbol_index: 108,
                    module: 1
                })
            );
            assert_eq!(name, "Baz::f_public");
        }

        #[test]
        fn kind_1108() {
            let buf = &[8, 17, 112, 6, 0, 0, 118, 97, 95, 108, 105, 115, 116, 0];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x1108);
            assert_eq!(
                data,
                SymbolData::UserDefinedType(UserDefinedTypeSymbol { type_index: 1648 })
            );
            assert_eq!(name, "va_list");
        }

        #[test]
        fn kind_1107() {
            let buf = &[
                7, 17, 201, 18, 0, 0, 1, 0, 95, 95, 73, 83, 65, 95, 65, 86, 65, 73, 76, 65, 66, 76,
                69, 95, 83, 83, 69, 50, 0, 0,
            ];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x1107);
            assert_eq!(
                data,
                SymbolData::Constant(ConstantSymbol {
                    type_index: 4809,
                    value: Variant::U16(1)
                })
            );
            assert_eq!(name, "__ISA_AVAILABLE_SSE2");
        }

        #[test]
        fn kind_110d() {
            let buf = &[
                13, 17, 116, 0, 0, 0, 16, 0, 0, 0, 3, 0, 95, 95, 105, 115, 97, 95, 97, 118, 97,
                105, 108, 97, 98, 108, 101, 0, 0, 0,
            ];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x110d);
            assert_eq!(
                data,
                SymbolData::DataSymbol(DataSymbol {
                    global: true,
                    managed: false,
                    type_index: 116,
                    offset: PdbInternalSectionOffset {
                        offset: 16,
                        section: 3
                    }
                })
            );
            assert_eq!(name, "__isa_available");
        }

        #[test]
        fn kind_110c() {
            let buf = &[
                12, 17, 32, 0, 0, 0, 240, 36, 1, 0, 2, 0, 36, 120, 100, 97, 116, 97, 115, 121, 109,
                0,
            ];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x110c);
            assert_eq!(
                data,
                SymbolData::DataSymbol(DataSymbol {
                    global: false,
                    managed: false,
                    type_index: 32,
                    offset: PdbInternalSectionOffset {
                        offset: 74992,
                        section: 2
                    }
                })
            );
            assert_eq!(name, "$xdatasym");
        }

        #[test]
        fn kind_1127() {
            let buf = &[
                39, 17, 0, 0, 0, 0, 128, 4, 0, 0, 182, 0, 99, 97, 112, 116, 117, 114, 101, 95, 99,
                117, 114, 114, 101, 110, 116, 95, 99, 111, 110, 116, 101, 120, 116, 0, 0, 0,
            ];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x1127);
            assert_eq!(
                data,
                SymbolData::ProcedureReference(ProcedureReferenceSymbol {
                    global: false,
                    sum_name: 0,
                    symbol_index: 1152,
                    module: 182
                })
            );
            assert_eq!(name, "capture_current_context");
        }

        #[test]
        fn kind_1110() {
            let buf = &[
                16, 17, 0, 0, 0, 0, 48, 2, 0, 0, 0, 0, 0, 0, 6, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 7,
                16, 0, 0, 64, 85, 0, 0, 1, 0, 0, 66, 97, 122, 58, 58, 102, 95, 112, 114, 111, 116,
                101, 99, 116, 101, 100, 0,
            ];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x1110);
            assert_eq!(
                data,
                SymbolData::Procedure(ProcedureSymbol {
                    global: true,
                    parent: 0,
                    end: 560,
                    next: 0,
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
                    }
                })
            );
            assert_eq!(name, "Baz::f_protected");
        }

        #[test]
        fn kind_110f() {
            let buf = &[
                15, 17, 0, 0, 0, 0, 156, 1, 0, 0, 0, 0, 0, 0, 18, 0, 0, 0, 4, 0, 0, 0, 9, 0, 0, 0,
                128, 16, 0, 0, 196, 87, 0, 0, 1, 0, 128, 95, 95, 115, 99, 114, 116, 95, 99, 111,
                109, 109, 111, 110, 95, 109, 97, 105, 110, 0, 0, 0,
            ];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x110f);
            assert_eq!(
                data,
                SymbolData::Procedure(ProcedureSymbol {
                    global: false,
                    parent: 0,
                    end: 412,
                    next: 0,
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
                    }
                })
            );
            assert_eq!(name, "__scrt_common_main");
        }
    }
}
