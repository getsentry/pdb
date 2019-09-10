use std::fs::File;

use pdb::{FallibleIterator, Result, PDB};

#[test]
fn test_symbol_depth() -> Result<()> {
    let file = File::open("fixtures/self/foo.pdb")?;
    let mut pdb = PDB::open(file)?;

    let dbi = pdb.debug_information()?;
    let mut modules = dbi.modules()?;

    while let Some(module) = modules.next()? {
        let module_info = match pdb.module_info(&module)? {
            Some(module_info) => module_info,
            None => continue,
        };

        let mut depth = 0isize;
        let mut symbols = module_info.symbols()?;
        while let Some(symbol) = symbols.next()? {
            if symbol.starts_scope() {
                depth += 1;
            } else if symbol.ends_scope() {
                depth -= 1;
            }

            // The most common case here will be that we forgot to add a raw kind to `starts_scope`.
            // PDBs seem to use `S_END` for most symbols with inline sites being the notable
            // exception. In case we forgot a start scope symbol, the depth will become negative.
            assert!(depth >= 0, "depth must not be negative");
        }
    }

    Ok(())
}
