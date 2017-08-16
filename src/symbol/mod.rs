// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use std::fmt;
use std::result;
use FallibleIterator;

use common::*;
use msf::*;

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
///
/// # let mut count: usize = 0;
/// let mut symbols = symbol_table.iter();
/// while let Some(symbol) = symbols.next()? {
///     match symbol.parse() {
///     	Ok(pdb::SymbolData::PublicSymbol{
///     		function: true,
///     		segment,
///     		offset,
///     		..
///     	}) => {
///     		// we found the location of a function!
///     		println!("{:x}:{:08x} is {}", segment, offset, symbol.name()?);
///             # count += 1;
///     	}
///     	_ => {}
///     }
/// }
///
/// # Ok(count)
/// # }
/// # assert!(test().expect("test") > 2000);
/// ```
#[derive(Debug)]
pub struct SymbolTable<'t> {
    stream: Stream<'t>,
}

pub fn new_symbol_table(s: Stream) -> SymbolTable {
    SymbolTable{
        stream: s,
    }
}

impl<'t> SymbolTable<'t> {
    /// Returns an iterator that can traverse the symbol table in sequential order.
    pub fn iter(&self) -> SymbolIter {
        SymbolIter{
            buf: self.stream.parse_buffer(),
        }
    }
}

/// Represents a symbol from the symbol table.
///
/// A `Symbol` is represented internally as a `&[u8]`, and in general the bytes inside are not
/// inspected in any way before calling any of the accessor methods.
///
/// To avoid copying, `Symbol`s exist as references to data owned by the parent `SymbolTable`.
/// Therefore, a `Symbol` may not outlive its parent `SymbolTable`.
#[derive(Copy,Clone,PartialEq)]
pub struct Symbol<'t>(&'t [u8]);

impl<'t> Symbol<'t> {
    /// Returns the kind of symbol identified by this Symbol.
    #[inline]
    pub fn raw_kind(&self) -> u16 {
        debug_assert!(self.0.len() >= 2);

        // assemble a little-endian u16
        (self.0[0] as u16) | ((self.0[1] as u16) << 8)
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

            S_LDATA32 | S_LDATA32_ST |
            S_GDATA32 | S_GDATA32_ST |
            S_LMANDATA | S_LMANDATA_ST |
            S_GMANDATA | S_GMANDATA_ST => 10,

            S_PROCREF | S_PROCREF_ST  |
            S_LPROCREF | S_LPROCREF_ST |
            S_DATAREF | S_DATAREF_ST |
            S_ANNOTATIONREF => 10,

            S_CONSTANT | S_CONSTANT_ST => 6,

            S_UDT | S_UDT_ST => 4,

            S_LTHREAD32 | S_LTHREAD32_ST |
            S_GTHREAD32 | S_GTHREAD32_ST => 10,

            _ => return Err(Error::UnimplementedSymbolKind(kind))
        };

        if self.0.len() < data_length + 2 + 2 {
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
        Ok(&self.0[2..(data_length+2)])
    }

    /// Returns the name of the symbol. Note that the underlying buffer is owned by the
    /// `SymbolTable`.
    pub fn name(&self) -> Result<RawString<'t>> {
        // figure out how long the data is
        let data_length = self.data_length()?;

        // figure out where the name is
        let mut buf = ParseBuffer::from(&self.0[2 + data_length ..]);

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
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Symbol{{ kind: 0x{:4x} [{} bytes] }}", self.raw_kind(), self.0.len())
    }
}

// data types are defined at:
//   https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L3038
// constants defined at:
//   https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/include/cvinfo.h#L2735
// decoding reference:
//   https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/cvdump/dumpsym7.cpp#L264

// CV_PUBSYMFLAGS_e:
const CVPSF_CODE: u32 = 0x00000001;
const CVPSF_FUNCTION: u32 = 0x00000002;
const CVPSF_MANAGED: u32 = 0x00000004;
const CVPSF_MSIL: u32 = 0x00000008;

fn parse_symbol_data(kind: u16, data: &[u8]) -> Result<SymbolData> {
    let mut buf = ParseBuffer::from(data);

    match kind {
        S_PUB32 | S_PUB32_ST => {
            let flags = buf.parse_u32()?;
            Ok(SymbolData::PublicSymbol {
                code:       flags & CVPSF_CODE != 0,
                function:   flags & CVPSF_FUNCTION != 0,
                managed:    flags & CVPSF_MANAGED != 0,
                msil:       flags & CVPSF_MSIL != 0,
                offset:     buf.parse_u32()?,
                segment:    buf.parse_u16()?,
            })
        }

        S_LDATA32 | S_LDATA32_ST |
        S_GDATA32 | S_GDATA32_ST |
        S_LMANDATA | S_LMANDATA_ST |
        S_GMANDATA | S_GMANDATA_ST => {
            Ok(SymbolData::DataSymbol {
                global: match kind { S_GDATA32 | S_GDATA32_ST | S_GMANDATA | S_GMANDATA_ST => true, _ => false },
                managed: match kind { S_LMANDATA | S_LMANDATA_ST | S_GMANDATA | S_GMANDATA_ST => true, _ => false },
                type_index: buf.parse_u32()?,
                offset:     buf.parse_u32()?,
                segment:    buf.parse_u16()?,
            })
        }

         S_PROCREF | S_PROCREF_ST  |
        S_LPROCREF | S_LPROCREF_ST => {
             Ok(SymbolData::ProcedureReference {
                 global: match kind { S_PROCREF | S_PROCREF_ST => true, _ => false },
                 sum_name: buf.parse_u32()?,
                 symbol_index: buf.parse_u32()?,
                 module: buf.parse_u16()?,
             })
         },

        S_DATAREF | S_DATAREF_ST => {
            Ok(SymbolData::DataReference {
                sum_name: buf.parse_u32()?,
                symbol_index: buf.parse_u32()?,
                module: buf.parse_u16()?,
            })
        }

        S_ANNOTATIONREF => {
             Ok(SymbolData::AnnotationReference {
                 sum_name: buf.parse_u32()?,
                 symbol_index: buf.parse_u32()?,
                 module: buf.parse_u16()?,
             })
        }

        S_CONSTANT | S_CONSTANT_ST => {
            Ok(SymbolData::Constant {
                type_index: buf.parse_u32()?,
                value: buf.parse_u16()?,
            })
        }

        S_UDT | S_UDT_ST => {
            Ok(SymbolData::UserDefinedType {
                type_index: buf.parse_u32()?,
            })
        }

        S_LTHREAD32 | S_LTHREAD32_ST |
        S_GTHREAD32 | S_GTHREAD32_ST => {
            Ok(SymbolData::ThreadStorage {
                global: match kind { S_GTHREAD32 | S_GTHREAD32_ST => true, _ => false },
                type_index: buf.parse_u32()?,
                offset: buf.parse_u32()?,
                segment: buf.parse_u16()?,
            })
        }

        _ => Err(Error::UnimplementedSymbolKind(kind))
    }
}

/// `SymbolData` contains the information parsed from a symbol record.
#[derive(Debug,Copy,Clone,Eq,PartialEq)]
pub enum SymbolData {
    // S_PUB32 (0x110e) | S_PUB32_ST (0x1009)
    PublicSymbol { code: bool, function: bool, managed: bool, msil: bool, offset: u32, segment: u16 },

    //   S_LDATA32 (0x110c) | S_LDATA32_ST (0x1007)
    //   S_GDATA32 (0x110d) | S_GDATA32_ST (0x1008)
    //  S_LMANDATA (0x111c) | S_LMANDATA_ST (0x1020)
    //  S_GMANDATA (0x111d) | S_GMANDATA_ST (0x1021)
    DataSymbol { global: bool, managed: bool, type_index: TypeIndex, offset: u32, segment: u16 },

    //   S_PROCREF (0x1125) |  S_PROCREF_ST (0x0400)
    //   S_DATAREF (0x1126) |  S_DATAREF_ST (0x0401)
    //  S_LPROCREF (0x1127) | S_LPROCREF_ST (0x0403)
    // S_ANNOTATIONREF (0x1128)
    ProcedureReference { global: bool, sum_name: u32, symbol_index: u32, module: u16 },
    DataReference { sum_name: u32, symbol_index: u32, module: u16 },
    AnnotationReference { sum_name: u32, symbol_index: u32, module: u16 },

    //  S_CONSTANT (0x1107) | S_CONSTANT_ST (0x1002)
    Constant { type_index: TypeIndex, value: u16 },

    //       S_UDT (0x1108) | S_UDT_ST (0x1003)
    UserDefinedType { type_index: TypeIndex },

    // S_LTHREAD32 (0x1112) | S_LTHREAD32_ST (0x100e)
    // S_GTHREAD32 (0x1113) | S_GTHREAD32_ST (0x100f)
    ThreadStorage { global: bool, type_index: TypeIndex, offset: u32, segment: u16 },
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
        if symbol_length <= 2 {
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
        use common::*;
        use symbol::*;

        fn parse<'s>(buf: &'s [u8]) -> Result<(Symbol<'s>,SymbolData,String)> {
            let symbol = Symbol(buf);

            let data = symbol.parse()?;
            let name = symbol.name()?.to_string().into_owned();

            Ok((symbol, data, name))
        }

        #[test]
        fn kind_110e() {
            let buf = &[14, 17, 2, 0, 0, 0, 192, 85, 0, 0, 1, 0, 95, 95, 108, 111, 99, 97, 108, 95, 115, 116, 100, 105, 111, 95, 112, 114, 105, 110, 116, 102, 95, 111, 112, 116, 105, 111, 110, 115, 0, 0];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x110e);
            assert_eq!(data, SymbolData::PublicSymbol { code: false, function: true, managed: false, msil: false, offset: 21952, segment: 1 });
            assert_eq!(name, "__local_stdio_printf_options");
        }

        #[test]
        fn kind_1125() {
            let buf = &[37, 17, 0, 0, 0, 0, 108, 0, 0, 0, 1, 0, 66, 97, 122, 58, 58, 102, 95, 112, 117, 98, 108, 105, 99, 0];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x1125);
            assert_eq!(data, SymbolData::ProcedureReference { global: true, sum_name: 0, symbol_index: 108, module: 1 });
            assert_eq!(name, "Baz::f_public");
        }

        #[test]
        fn kind_1108() {
            let buf = &[8, 17, 112, 6, 0, 0, 118, 97, 95, 108, 105, 115, 116, 0];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x1108);
            assert_eq!(data, SymbolData::UserDefinedType { type_index: 1648 });
            assert_eq!(name, "va_list");
        }

        #[test]
        fn kind_1107() {
            let buf = &[7, 17, 201, 18, 0, 0, 1, 0, 95, 95, 73, 83, 65, 95, 65, 86, 65, 73, 76, 65, 66, 76, 69, 95, 83, 83, 69, 50, 0, 0];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x1107);
            assert_eq!(data, SymbolData::Constant { type_index: 4809, value: 1 });
            assert_eq!(name, "__ISA_AVAILABLE_SSE2");
        }

        #[test]
        fn kind_110d() {
            let buf = &[13, 17, 116, 0, 0, 0, 16, 0, 0, 0, 3, 0, 95, 95, 105, 115, 97, 95, 97, 118, 97, 105, 108, 97, 98, 108, 101, 0, 0, 0];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x110d);
            assert_eq!(data, SymbolData::DataSymbol { global: true, managed: false, type_index: 116, offset: 16, segment: 3 });
            assert_eq!(name, "__isa_available");
        }

        #[test]
        fn kind_110c() {
            let buf = &[12, 17, 32, 0, 0, 0, 240, 36, 1, 0, 2, 0, 36, 120, 100, 97, 116, 97, 115, 121, 109, 0];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x110c);
            assert_eq!(data, SymbolData::DataSymbol { global: false, managed: false, type_index: 32, offset: 74992, segment: 2 });
            assert_eq!(name, "$xdatasym");
        }

        #[test]
        fn kind_1127() {
            let buf = &[39, 17, 0, 0, 0, 0, 128, 4, 0, 0, 182, 0, 99, 97, 112, 116, 117, 114, 101, 95, 99, 117, 114, 114, 101, 110, 116, 95, 99, 111, 110, 116, 101, 120, 116, 0, 0, 0];
            let (symbol, data, name) = parse(buf).expect("parse");
            assert_eq!(symbol.raw_kind(), 0x1127);
            assert_eq!(data, SymbolData::ProcedureReference { global: false, sum_name: 0, symbol_index: 1152, module: 182 });
            assert_eq!(name, "capture_current_context");
        }
    }
}