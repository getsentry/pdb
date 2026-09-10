// Copyright 2026 PDB Developers
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
//! PDB files are accessed via the [`pdb::PDB`] object.
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
//! let address_map = pdb.address_map()?;
//!
//! # let mut count: usize = 0;
//! let mut symbols = symbol_table.iter();
//! while let Some(symbol) = symbols.next()? {
//!     match symbol.parse() {
//!         Ok(pdb::SymbolData::Public(data)) if data.function => {
//!             // we found the location of a function!
//!             let rva = data.offset.to_rva(&address_map).unwrap_or_default();
//!             println!("{} is {}", rva, data.name);
//!             # count += 1;
//!         }
//!         _ => {}
//!     }
//! }
//!
//! # Ok(count)
//! # }
//! # assert!(test().expect("test") > 2000);
//! ```
#![cfg_attr(not(feature = "std"), no_std)]
#![cfg_attr(feature = "nightly", feature(core_io))]
#![cfg_attr(feature = "nightly", feature(alloc_io))]
#![warn(missing_docs)]
#![allow(unused)]

#[cfg(feature = "alloc")]
extern crate alloc;

// #[cfg(not(feature = "alloc"))]
pub mod noalloc;

mod constants;
mod common;

#[cfg(not(feature = "nightly"))]
mod io;

#[cfg(feature = "nightly")]
mod io {
    pub use core::io::*;
    pub use core::io::Error as IoError;
    pub use alloc::io::Read;
}

mod dbi;

#[cfg(feature = "alloc")]
mod framedata;

#[cfg(feature = "alloc")]
mod modi;

mod msf;

#[cfg(feature = "alloc")]
mod omap;

mod pdb;
mod pdbi;
mod pe;
mod source;
mod strings;
mod symbol;

mod tpi;

#[cfg(feature = "alloc")]
pub use crate::msf::*;

pub use crate::constants::*;
pub use crate::common::*;

pub use crate::dbi::*;

#[cfg(feature = "alloc")]
pub use crate::framedata::*;

#[cfg(feature = "alloc")]
pub use crate::modi::*;

#[cfg(feature = "alloc")]
pub use crate::omap::*;

pub use crate::pdb::*;
pub use crate::pdbi::*;
pub use crate::pe::*;
pub use crate::source::*;
pub use crate::strings::*;
pub use crate::symbol::*;

pub use crate::tpi::*;

// re-export FallibleIterator for convenience
#[doc(no_inline)]
pub use fallible_iterator::FallibleIterator;
