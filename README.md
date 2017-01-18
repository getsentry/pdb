pdb
===

This is a Rust library that parses Microsoft PDB (Program Database) files.
These files contain debugging information produced by most compilers that
target Windows, including information about symbols, types, modules, and so on.

The PDB format is not documented per sé, but Microsoft has [published
information](https://github.com/Microsoft/microsoft-pdb) in the form of C++
code relating to its use. The PDB format is full of... history, including
support for debugging 16-bit executables, COBOL user-defined types, and myriad
other features. `pdb` does not understand everything about the PDB format,
but it does cover enough to be useful for typical programs compiled today.

Design
---

`pdb`'s design objectives are similar to
[`gimli`](https://github.com/gimli-rs/gimli):

* `pdb` works with the original data as it's formatted on-disk as long as
  possible.

* `pdb` parses only what you ask.

* `pdb` can read PDBs anywhere. There's no dependency on Windows, on the
  [DIA SDK](https://msdn.microsoft.com/en-us/library/x93ctkx8.aspx), or on
  the target's native byte ordering.

Usage Example
---

```
extern crate pdb;

use pdb::FallibleIterator;
use std::fs::File;

fn main() {
    let file = std::fs::File::open("fixtures/self/foo.pdb")?;
    let mut pdb = pdb::PDB::open(file)?;
    
    let symbol_table = pdb.global_symbols()?;
    
    let mut symbols = symbol_table.iter();
    while let Some(symbol) = symbols.next()? {
        match symbol.parse() {
            Ok(pdb::SymbolData::PublicSymbol{function: true, segment, offset, ..}) => {
                // we found the location of a function
                println!("{:x}:{:08x} is {}", segment, offset, symbol.name()?);
            }
            _ => {}
        }
    }
}
```

Example Programs
---

Run with `cargo run --release --example <name>`:

* [`pdb_symbols`](examples/pdb_symbols.rs) is a toy program that prints the name and location of every function and
  data value defined in the symbol table.

* [`pdb2hpp`](examples/pdb2hpp.rs) is a somewhat larger program that prints an approximation of a C++ header file for
  a requested type given only a PDB.

License
---

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

Contribution
---

Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any
additional terms or conditions.


TODO
---

* Go from one-copy to zero-copy. The library works with data in-place for as
  long as possible, and it can in principle operate on a memory mapped PDB,
  but there's a catch.
  
  The underlying file format is laid out such that a single logical data
  stream (which `pdb` wants to view as a `&[u8]`) is actually  discontinuous
  on-disk. That shouldn't be a problem – operating systems let you memory map
  files in appropriate ways – except `memmap-rs` doesn't currently support
  mapping discontinuous segments into a continuous memory block.
   
  `pdb` today resorts to making a copy using `io::Seek` + `io::Read`, and
  then works on data in-place from there. If we had a way to [get
  discontinuous memory
  maps](https://github.com/danburkert/memmap-rs/issues/30), we could drop it
  in and eliminate that copy.

* Expose module information.

* Expose line number information.
