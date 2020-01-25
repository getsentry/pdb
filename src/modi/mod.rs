use crate::common::*;
use crate::dbi::Module;
use crate::msf::Stream;
use crate::symbol::{InlineSiteSymbol, SymbolIter};
use crate::FallibleIterator;

mod c13;
mod constants;

#[derive(Clone, Copy, Debug)]
enum LinesSize {
    C11(usize),
    C13(usize),
}

/// This struct contains data about a single module from its module info stream.
///
/// The module info stream is where private symbols and line info is stored.
pub struct ModuleInfo<'s> {
    stream: Stream<'s>,
    symbols_size: usize,
    lines_size: LinesSize,
}

impl<'s> ModuleInfo<'s> {
    /// Parses a `ModuleInfo` from it's Module info stream data.
    pub(crate) fn parse(stream: Stream<'s>, module: &Module<'_>) -> Result<Self> {
        let info = module.info();

        let mut buf = stream.parse_buffer();
        if buf.parse_u32()? != constants::CV_SIGNATURE_C13 {
            return Err(Error::UnimplementedFeature(
                "Unsupported module info format",
            ));
        }

        let lines_size = if info.lines_size > 0 {
            LinesSize::C11(info.lines_size as usize)
        } else {
            LinesSize::C13(info.c13_lines_size as usize)
        };

        let symbols_size = info.symbols_size as usize;
        Ok(ModuleInfo {
            stream,
            symbols_size,
            lines_size,
        })
    }

    fn lines_data(&self, size: usize) -> &[u8] {
        let start = self.symbols_size as usize;
        &self.stream[start..start + size]
    }

    /// Get an iterator over the all symbols in this module.
    pub fn symbols(&self) -> Result<SymbolIter<'_>> {
        let mut buf = self.stream.parse_buffer();
        buf.truncate(self.symbols_size)?;
        buf.parse_u32()?;
        Ok(SymbolIter::new(buf))
    }

    /// Get an iterator over symbols starting at the given index.
    pub fn symbols_at(&self, index: SymbolIndex) -> Result<SymbolIter<'_>> {
        let mut iter = self.symbols()?;
        iter.seek(index);
        Ok(iter)
    }

    /// Returns a line program that gives access to file and line information in this module.
    pub fn line_program(&self) -> Result<LineProgram<'_>> {
        let inner = match self.lines_size {
            LinesSize::C11(_size) => return Err(Error::UnimplementedFeature("C11 line programs")),
            LinesSize::C13(size) => {
                LineProgramInner::C13(c13::C13LineProgram::parse(self.lines_data(size))?)
            }
        };

        Ok(LineProgram { inner })
    }

    /// Returns an iterator over all inlinees in this module.
    ///
    /// Inlinees are not guaranteed to be sorted. When requiring random access by `ItemId`, collect
    /// them into a mapping structure rather than reiterating multiple times.
    pub fn inlinees(&self) -> Result<InlineeIterator<'_>> {
        Ok(InlineeIterator(match self.lines_size {
            // C11 does not contain inlinee information.
            LinesSize::C11(_size) => Default::default(),
            LinesSize::C13(size) => c13::C13InlineeIterator::parse(self.lines_data(size))?,
        }))
    }
}

/// Checksum of a source file's contents.
#[derive(Clone, Debug)]
#[allow(missing_docs)]
pub enum FileChecksum<'a> {
    None,
    Md5(&'a [u8]),
    Sha1(&'a [u8]),
    Sha256(&'a [u8]),
}

impl PartialEq for FileChecksum<'_> {
    fn eq(&self, other: &Self) -> bool {
        // Manual implementation to allow for None != None.
        match (self, other) {
            (&FileChecksum::Md5(lhs), &FileChecksum::Md5(rhs)) => lhs == rhs,
            (&FileChecksum::Sha1(lhs), &FileChecksum::Sha1(rhs)) => lhs == rhs,
            (&FileChecksum::Sha256(lhs), &FileChecksum::Sha256(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

/// Information record on a source file.
#[derive(Clone, Debug, PartialEq)]
pub struct FileInfo<'a> {
    /// Reference to the file name in the [string table](struct.StringTable.html).
    pub name: StringRef,

    /// Checksum of the file contents.
    pub checksum: FileChecksum<'a>,
}

/// The kind of source construct a line info is referring to.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum LineInfoKind {
    /// A source code expression.
    Expression,
    /// A source code statement.
    Statement,
}

impl Default for LineInfoKind {
    fn default() -> Self {
        LineInfoKind::Statement
    }
}

/// Mapping of a source code offset to a source file location.
///
/// A line entry is always valid up to the subsequent entry.
#[derive(Clone, Debug, PartialEq)]
pub struct LineInfo {
    /// Source code offset.
    pub offset: PdbInternalSectionOffset,
    /// The optional length of the code.
    pub length: Option<u32>,
    /// Index of the source file in this module.
    pub file_index: FileIndex,
    /// Line number of the start of the covered range.
    pub line_start: u32,
    /// Line number of the end of the covered range.
    pub line_end: u32,
    /// Column number of the start of the covered range.
    ///
    /// This value is only present if column information is provided by the PDB. Even then, it is
    /// often zero.
    pub column_start: Option<u32>,
    /// Column number of the end of the covered range.
    ///
    /// This value is only present if column information is provided by the PDB. Even then, it is
    /// often zero.
    pub column_end: Option<u32>,
    /// Kind of this line information.
    pub kind: LineInfoKind,
}

enum LineProgramInner<'a> {
    C13(c13::C13LineProgram<'a>),
}

/// The `LineProgram` provides access to source line information for a module and its procedures.
pub struct LineProgram<'a> {
    inner: LineProgramInner<'a>,
}

impl<'a> LineProgram<'a> {
    /// Returns an iterator over all line information records of this module.
    ///
    /// Note that line records are not guaranteed to be ordered by source code offset. If a
    /// monotonic order by `PdbInternalSectionOffset` or `Rva` is required, the lines have to be
    /// sorted manually.
    pub fn lines(&self) -> LineIterator<'a> {
        match self.inner {
            LineProgramInner::C13(ref inner) => LineIterator {
                inner: LineIteratorInner::C13(inner.lines()),
            },
        }
    }

    /// Returns an iterator over all file records of this module.
    pub fn files(&self) -> FileIterator<'a> {
        match self.inner {
            LineProgramInner::C13(ref inner) => FileIterator {
                inner: FileIteratorInner::C13(inner.files()),
            },
        }
    }

    /// Returns an iterator over line records for the given section offset.
    ///
    /// This does not work with any arbitrary section offset. The iterator only returns lines if the
    /// section offset is the start of a line block. This is true for procedure or symbol offsets
    /// that specify line information. For all other offsets, the iterator will be empty.
    ///
    /// Note that line records are not guaranteed to be ordered by source code offset. If a
    /// monotonic order by `PdbInternalSectionOffset` or `Rva` is required, the lines have to be
    /// sorted manually.
    pub fn lines_at_offset(&self, offset: PdbInternalSectionOffset) -> LineIterator {
        match self.inner {
            LineProgramInner::C13(ref inner) => LineIterator {
                inner: LineIteratorInner::C13(inner.lines_at_offset(offset)),
            },
        }
    }

    /// Looks up file information for the specified file.
    pub fn get_file_info(&self, offset: FileIndex) -> Result<FileInfo<'a>> {
        match self.inner {
            LineProgramInner::C13(ref inner) => inner.get_file_info(offset),
        }
    }
}

#[derive(Clone, Debug)]
enum LineIteratorInner<'a> {
    C13(c13::C13LineIterator<'a>),
}

/// An iterator over line information records in a module.
#[derive(Clone, Debug)]
pub struct LineIterator<'a> {
    inner: LineIteratorInner<'a>,
}

impl Default for LineIterator<'_> {
    fn default() -> Self {
        LineIterator {
            inner: LineIteratorInner::C13(Default::default()),
        }
    }
}

impl<'a> FallibleIterator for LineIterator<'a> {
    type Item = LineInfo;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        match self.inner {
            LineIteratorInner::C13(ref mut inner) => inner.next(),
        }
    }
}

/// An inlined function that can evaluate to line information.
#[derive(Clone, Debug)]
pub struct Inlinee<'a>(c13::C13Inlinee<'a>);

impl<'a> Inlinee<'a> {
    /// The index of this inlinee in the `IdInformation` stream (IPI).
    pub fn index(&self) -> IdIndex {
        self.0.index()
    }

    /// Returns an iterator over line records for an inline site.
    ///
    /// Note that line records are not guaranteed to be ordered by source code offset. If a
    /// monotonic order by `PdbInternalSectionOffset` or `Rva` is required, the lines have to be
    /// sorted manually.
    pub fn lines(
        &self,
        parent_offset: PdbInternalSectionOffset,
        inline_site: &InlineSiteSymbol<'a>,
    ) -> InlineeLineIterator<'a> {
        InlineeLineIterator(self.0.lines(parent_offset, inline_site))
    }
}

/// An iterator over line information records in a module.
#[derive(Clone, Debug, Default)]
pub struct InlineeIterator<'a>(c13::C13InlineeIterator<'a>);

impl<'a> FallibleIterator for InlineeIterator<'a> {
    type Item = Inlinee<'a>;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        self.0.next().map(|opt| opt.map(Inlinee))
    }
}

/// An iterator over line information records in a module.
#[derive(Clone, Debug, Default)]
pub struct InlineeLineIterator<'a>(c13::C13InlineeLineIterator<'a>);

impl<'a> FallibleIterator for InlineeLineIterator<'a> {
    type Item = LineInfo;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        self.0.next()
    }
}

#[derive(Clone, Debug)]
enum FileIteratorInner<'a> {
    C13(c13::C13FileIterator<'a>),
}

/// An iterator over file records in a module.
#[derive(Clone, Debug)]
pub struct FileIterator<'a> {
    inner: FileIteratorInner<'a>,
}

impl Default for FileIterator<'_> {
    fn default() -> Self {
        FileIterator {
            inner: FileIteratorInner::C13(Default::default()),
        }
    }
}

impl<'a> FallibleIterator for FileIterator<'a> {
    type Item = FileInfo<'a>;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        match self.inner {
            FileIteratorInner::C13(ref mut inner) => inner.next(),
        }
    }
}
