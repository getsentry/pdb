use crate::common::*;
use crate::dbi::Module;
use crate::msf::Stream;
use crate::symbol::SymbolIter;
use std::mem;

/// The signature at the start of a module information stream.
const MODI_SIGNATURE: u32 = 4;

#[allow(dead_code)]
enum Lines {
    C11(usize),
    C13(usize),
}

/// This struct contains data about a single module from its module info stream.
///
/// The module info stream is where private symbols and line info is stored.
pub struct ModuleInfo<'m> {
    stream: Stream<'m>,
    symbols_size: usize,
    _lines: Lines,
}

impl<'m> ModuleInfo<'m> {
    /// Get an iterator over the private symbols of this module.
    pub fn symbols(&self) -> Result<SymbolIter<'_>> {
        let mut buf = self.stream.parse_buffer();
        buf.parse_u32()?;
        let symbols = buf.take(self.symbols_size - mem::size_of::<u32>())?;
        Ok(SymbolIter::new(symbols.into()))
    }
}

pub fn new_module_info<'s>(stream: Stream<'s>, module: &Module<'_>) -> Result<ModuleInfo<'s>> {
    let info = module.info();
    {
        let mut buf = stream.parse_buffer();
        if buf.parse_u32()? != MODI_SIGNATURE {
            return Err(Error::UnimplementedFeature(
                "Unsupported module info format",
            ));
        }
    }
    let lines = if info.lines_size > 0 {
        Lines::C11(info.lines_size as usize)
    } else {
        Lines::C13(info.c13_lines_size as usize)
    };
    let symbols_size = info.symbols_size as usize;
    Ok(ModuleInfo {
        stream,
        symbols_size,
        _lines: lines,
    })
}
