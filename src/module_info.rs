use common::*;
use dbi::Module;
use msf::Stream;
use std::mem;
use std::result;
use symbol::SymbolIter;
use FallibleIterator;


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
    lines_size: Lines,
}

impl<'m> ModuleInfo<'m> {
    /// Get an iterator over the private symbols of this module.
    pub fn symbols(&self) -> Result<SymbolIter> {
        let mut buf = self.stream.parse_buffer();
        buf.parse_u32()?;
        let symbols = buf.take(self.symbols_size - mem::size_of::<u32>())?;
        Ok(SymbolIter::new(symbols.into()))
    }

    pub fn lines(&self) -> Result<LineIter> {
       let mut buf = self.stream.parse_buffer();
        buf.parse_u32()?;
        let _symbols = buf.take(self.symbols_size - mem::size_of::<u32>())?;
        let lines = match self.lines_size {
            Lines::C13(size) => buf.take(size),
            _ => return Err(Error::LinesNotFound)
        }?;
        Ok(LineIter{section_iter: SectionIter::new(lines.into())})
    }
}

pub fn new_module_info<'s, 'm>(stream: Stream<'s>, module: &Module<'m>) -> Result<ModuleInfo<'s>> {
    let info = module.info();
    {
        let mut buf = stream.parse_buffer();
        if buf.parse_u32()? != MODI_SIGNATURE {
            return Err(Error::UnimplementedFeature("Unsupported module info format"));
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
        lines_size: lines,
    })
}


#[derive(Debug)]
pub struct SectionIter<'t> {
    buf: ParseBuffer<'t>,
    checksums: ParseBuffer<'t>
}

impl<'t> SectionIter<'t> {
    pub fn new(buf: ParseBuffer<'t>) -> SectionIter {
        static EMPTY: [u8; 0] = [0; 0];
        SectionIter { buf, checksums: ParseBuffer::from(&EMPTY[..]) }
    }
}

#[derive(Debug)]
pub struct LineIter<'t> {
    section_iter: SectionIter<'t>
}

#[derive(Debug)]
pub struct Section<'t> {
    buf: ParseBuffer<'t>,
    kind: u32,
}

pub const DEBUG_S_IGNORE               : u32 = 0x80000000;
pub const DEBUG_S_SYMBOLS              : u32 = 0xf1;
pub const DEBUG_S_LINES                : u32 = 0xf2;
pub const DEBUG_S_STRINGTABLE          : u32 = 0xf3;
pub const DEBUG_S_FILECHKSMS           : u32 = 0xf4;
pub const DEBUG_S_FRAMEDATA            : u32 = 0xf5;
pub const DEBUG_S_INLINEELINES         : u32 = 0xf6;
pub const DEBUG_S_CROSSSCOPEIMPORTS    : u32 = 0xf7;
pub const DEBUG_S_CROSSSCOPEEXPORTS    : u32 = 0xf8;
pub const DEBUG_S_IL_LINES             : u32 = 0xf9;
pub const DEBUG_S_FUNC_MDTOKEN_MAP     : u32 = 0xfa;
pub const DEBUG_S_TYPE_MDTOKEN_MAP     : u32 = 0xfb;
pub const DEBUG_S_MERGED_ASSEMBLYINPUT : u32 = 0xfc;
pub const DEBUG_S_COFF_SYMBOL_RVA      : u32 = 0xfd;

impl<'t> FallibleIterator for SectionIter<'t> {
    type Item = Section<'t>;
    type Error = Error;

    fn next(&mut self) -> result::Result<Option<Self::Item>, Self::Error> {
        // see if we're at EOF
        if self.buf.len() == 0 {
            return Ok(None);
        }

        let section_type = self.buf.parse_u32()?;
        let section_len = self.buf.parse_u32()? as usize;
        let section = self.buf.take(section_len)?;
        let parse: ParseBuffer = section.into();
        return Ok(Some(Section { kind: section_type, buf: parse }));
    }

}

#[derive(Debug, Pread)]
#[repr(C, packed)]
struct LineFragmentHeader {
    reloc_offset: u32,  // Code offset of line contribution.
    reloc_segment: u16, // Code segment of line contribution.
    flags: u16,      // See LineFlags enumeration.
    code_size: u32,     // Code size of this line contribution.
}

pub const CV_LINES_HAVE_COLUMNS: u16 = 0x0001;

#[derive(Debug, Pread)]
struct LineBlockFragmentHeader {
    name_index: u32,
    num_lines: u32,
    block_size: u32,
}

#[derive(Debug, Pread)]
pub struct LineNumberEntry {
    offset: u32,
    flags: u32
}

#[derive(Debug, Pread)]
struct ColumnNumberEntry {
    start_column: u16,
    end_column: u16
}

#[derive(Debug, Pread)]
struct ChecksumEntry {
    file_name_offset: u32,
    checksum_size: u8,
    checksum_kind: u8
}

#[derive(Debug)]
pub struct Line {
    pub file_name_index: u32,
    pub block_size: u32,
    pub lines: Vec<LineNumberEntry>
}

impl<'t> FallibleIterator for LineIter<'t> {
    type Item = Line;
    type Error = Error;

    fn next(&mut self) -> result::Result<Option<Self::Item>, Self::Error> {
        while let Some(sect) = self.section_iter.next()? {
            if sect.kind == DEBUG_S_LINES {
                let mut parse = sect.buf;
                let header: LineFragmentHeader = parse.parse()?;
                let block_header: LineBlockFragmentHeader = parse.parse()?;
                let have_columns = header.flags & CV_LINES_HAVE_COLUMNS != 0;

                let mut c = self.section_iter.checksums.clone();
                c.take(block_header.name_index as usize)?;
                let entry: ChecksumEntry = c.parse()?;
                let file_name_index = entry.file_name_offset;

                let mut lines = Vec::with_capacity(block_header.num_lines as usize);
                for _ in 0..block_header.num_lines {
                    let line: LineNumberEntry = parse.parse()?;
                    //println!("{:?}", line);
                    lines.push(line);
                }
                if have_columns {
                    for _ in 0..block_header.num_lines {
                        let column: ColumnNumberEntry = parse.parse()?;
                        //println!("{:?}", column);
                    }
                }

                return Ok(Some(Line{file_name_index, block_size: block_header.block_size, lines}));
            } else if sect.kind == DEBUG_S_FILECHKSMS {
                self.section_iter.checksums = sect.buf;
                /*let mut parse = sect.buf;
                while parse.len() > 0 {
                    let entry: ChecksumEntry = parse.parse()?;
                    println!("{:?}", entry);
                    parse.take(entry.checksum_size as usize)?;
                    self.section_iter.checksums.push(entry);
                    parse.align(4);
                }*/
            }
        }
        Ok(None)
    }
}

