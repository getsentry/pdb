// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.


//! The `pdb` create parses Microsoft PDB (Program Database) files. PDB files contain debugging
//! information produced by most compilers that target Windows, including information about symbols,
//! types, modules, and so on.
//!
//! # Usage
//!
//! PDB files are accessed via the [`pdb::PDB` object](struct.PDB.html).
//!
//! # Example
//!
//! ```
//! # use pdb::FallibleIterator;
//! #
//! # fn test() -> pdb::Result<usize> {
//! let file = std::fs::File::open("fixtures/self/foo.pdb")?;
//! let mut pdb = pdb::PDB::open(file)?;
//!
//! let symbol_table = pdb.global_symbols()?;
//!
//! # let mut count: usize = 0;
//! let mut symbols = symbol_table.iter();
//! while let Some(symbol) = symbols.next()? {
//!     match symbol.parse() {
//!     	Ok(pdb::SymbolData::PublicSymbol(data)) if data.function => {
//!     		// we found the location of a function!
//!     		println!("{:x}:{:08x} is {}", data.segment, data.offset, symbol.name()?);
//!             # count += 1;
//!     	}
//!     	_ => {}
//!     }
//! }
//!
//! # Ok(count)
//! # }
//! # assert!(test().expect("test") > 2000);
//! ```

extern crate fallible_iterator;
#[macro_use]
extern crate scroll;
extern crate uuid;

// modules
mod common;
mod dbi;
mod module_info;
mod msf;
mod pdb;
mod source;
mod string_table;
mod symbol;
mod tpi;
mod pdbi;

// exports
pub use common::{Error,Result,TypeIndex,RawString,Variant};
pub use dbi::{DebugInformation, Module, ModuleIter};
pub use module_info::ModuleInfo;
pub use pdbi::{NameIter, PDBInformation, StreamName, StreamNames};
pub use pdb::PDB;
pub use source::*;
pub use symbol::*;
pub use tpi::*;

// re-export FallibleIterator for convenience
#[doc(no_inline)]
pub use fallible_iterator::FallibleIterator;
