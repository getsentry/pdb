use scroll::Pread;

use crate::common::*;
use crate::modi::{constants, FileChecksum, FileIndex, FileInfo, LineInfo, LineInfoKind};
use crate::FallibleIterator;

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(u32)]
#[allow(unused)]
enum DebugSubsectionKind {
    // Native
    Symbols = 0xf1,
    Lines = 0xf2,
    StringTable = 0xf3,
    FileChecksums = 0xf4,
    FrameData = 0xf5,
    InlineeLines = 0xf6,
    CrossScopeImports = 0xf7,
    CrossScopeExports = 0xf8,

    // .NET
    ILLines = 0xf9,
    FuncMDTokenMap = 0xfa,
    TypeMDTokenMap = 0xfb,
    MergedAssemblyInput = 0xfc,

    CoffSymbolRva = 0xfd,
}

impl DebugSubsectionKind {
    fn parse(value: u32) -> Result<Option<Self>> {
        if value >= 0xf1 && value <= 0xfd {
            Ok(Some(unsafe { std::mem::transmute(value) }))
        } else if value == constants::DEBUG_S_IGNORE {
            Ok(None)
        } else {
            Err(Error::UnimplementedDebugSubsection(value))
        }
    }
}

#[derive(Clone, Copy, Debug, Pread)]
struct DebugSubsectionHeader {
    /// The kind of this subsection.
    kind: u32,
    /// The length of this subsection in bytes, following the header.
    len: u32,
}

impl DebugSubsectionHeader {
    fn kind(self) -> Result<Option<DebugSubsectionKind>> {
        DebugSubsectionKind::parse(self.kind)
    }

    fn len(self) -> usize {
        self.len as usize
    }
}

#[derive(Clone, Copy, Debug)]
struct DebugSubsection<'a> {
    pub kind: DebugSubsectionKind,
    pub data: &'a [u8],
}

#[derive(Clone, Debug, Default)]
struct DebugSubsectionIterator<'a> {
    buf: ParseBuffer<'a>,
}

impl<'a> DebugSubsectionIterator<'a> {
    fn new(data: &'a [u8]) -> Self {
        DebugSubsectionIterator {
            buf: ParseBuffer::from(data),
        }
    }
}

impl<'a> FallibleIterator for DebugSubsectionIterator<'a> {
    type Item = DebugSubsection<'a>;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        while !self.buf.is_empty() {
            let header = self.buf.parse::<DebugSubsectionHeader>()?;
            let data = self.buf.take(header.len())?;
            let kind = match header.kind()? {
                Some(kind) => kind,
                None => continue,
            };

            return Ok(Some(DebugSubsection { kind, data }));
        }

        Ok(None)
    }
}

#[derive(Clone, Copy, Debug, Default, Pread)]
struct DebugLinesHeader {
    /// Section offset of this line contribution.
    offset: PdbInternalSectionOffset,
    /// See LineFlags enumeration.
    flags: u16,
    /// Code size of this line contribution.
    code_size: u32,
}

impl DebugLinesHeader {
    fn has_columns(self) -> bool {
        self.flags & constants::CV_LINES_HAVE_COLUMNS != 0
    }
}

#[derive(Clone, Copy, Debug, Default, Pread)]
struct DebugInlineesHeader {
    /// The signature of the inlinees
    signature: u32,
}

#[derive(Clone, Copy, Debug, Default, Pread)]
pub struct InlineeSourceLine {
    pub inlinee: ItemId,
    // This should be FileIndex
    pub file_id: u32,
    pub source_line_num: u32,
}

#[derive(Debug, Clone)]
struct DebugInlineesSubsection<'a> {
    header: DebugInlineesHeader,
    data: &'a [u8],
}

impl<'a> DebugInlineesSubsection<'a> {
    fn parse(data: &'a [u8]) -> Result<Self> {
        let mut buf = ParseBuffer::from(data);
        let header = buf.parse()?;
        let data = &data[buf.pos()..];
        Ok(DebugInlineesSubsection { header, data })
    }

    fn lines(&self) -> DebugInlineesSourceLineIterator<'a> {
        DebugInlineesSourceLineIterator {
            header: self.header,
            buf: ParseBuffer::from(self.data),
        }
    }
}

#[derive(Debug, Clone, Default)]
struct DebugInlineesSourceLineIterator<'a> {
    header: DebugInlineesHeader,
    buf: ParseBuffer<'a>,
}

impl<'a> FallibleIterator for DebugInlineesSourceLineIterator<'a> {
    type Item = InlineeSourceLine;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        if self.header.signature != constants::CV_INLINEE_SOURCE_LINE_SIGNATURE {
            return Ok(None);
        }
        if self.buf.is_empty() {
            Ok(None)
        } else {
            Ok(Some(self.buf.parse()?))
        }
    }
}

struct DebugLinesSubsection<'a> {
    header: DebugLinesHeader,
    data: &'a [u8],
}

impl<'a> DebugLinesSubsection<'a> {
    fn parse(data: &'a [u8]) -> Result<Self> {
        let mut buf = ParseBuffer::from(data);
        let header = buf.parse()?;
        let data = &data[buf.pos()..];
        Ok(DebugLinesSubsection { header, data })
    }

    fn blocks(&self) -> DebugLinesBlockIterator<'a> {
        DebugLinesBlockIterator {
            header: self.header,
            buf: ParseBuffer::from(self.data),
        }
    }
}

/// Marker instructions for a line offset.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
enum LineMarkerKind {
    /// A debugger should skip this address.
    DoNotStepOnto,
    /// A debugger should not step into this address.
    DoNotStepInto,
}

/// The raw line number entry in a PDB.
#[repr(C, packed)]
#[derive(Clone, Copy, Debug, Pread)]
struct LineNumberHeader {
    /// Offset to start of code bytes for line number.
    offset: u32,
    /// Combined information on the start line, end line and entry type:
    ///
    /// ```ignore
    /// unsigned long   linenumStart:24;  // line where statement/expression starts
    /// unsigned long   deltaLineEnd:7;   // delta to line where statement ends (optional)
    /// unsigned long   fStatement  :1;   // true if a statement line number, else an expression
    /// ```
    flags: u32,
}

/// A mapping of code section offsets to source line numbers.
#[derive(Clone, Debug)]
struct LineNumberEntry {
    /// Delta offset to the start of this line contribution (debug lines subsection).
    pub offset: u32,
    /// Start line number of the statement or expression.
    pub start_line: u32,
    /// End line number of the statement or expression.
    pub end_line: u32,
    /// The type of code construct this line entry refers to.
    pub kind: LineInfoKind,
}

/// Marker for debugging purposes.
#[derive(Clone, Debug)]
struct LineMarkerEntry {
    /// Delta offset to the start of this line contribution (debug lines subsection).
    pub offset: u32,
    /// The marker kind, hinting a debugger how to deal with code at this offset.
    pub kind: LineMarkerKind,
}

/// A parsed line entry.
#[derive(Clone, Debug)]
enum LineEntry {
    /// Declares a source line number.
    Number(LineNumberEntry),
    /// Declares a debugging marker.
    Marker(LineMarkerEntry),
}

impl LineNumberHeader {
    /// Parse this line number header into a line entry.
    pub fn parse(self) -> LineEntry {
        // The compiler generates special line numbers to hint the debugger. Separate these out so
        // that they are not confused with actual line number entries.
        let start_line = self.flags & 0x00ff_ffff;
        let marker = match start_line {
            0xfee_fee => Some(LineMarkerKind::DoNotStepOnto),
            0xf00_f00 => Some(LineMarkerKind::DoNotStepInto),
            _ => None,
        };

        if let Some(kind) = marker {
            return LineEntry::Marker(LineMarkerEntry {
                offset: self.offset,
                kind,
            });
        }

        // It has been observed in some PDBs that this does not store a delta to start_line but
        // actually just the truncated value of `end_line`. Therefore, prefer to use `end_line` and
        // compute the deta from `end_line` and `start_line`, if needed.
        let line_delta = self.flags & 0x7f00_0000 >> 24;

        // The line_delta contains the lower 7 bits of the end line number. We take all higher bits
        // from the start line and OR them with the lower delta bits. This combines to the full
        // original end line number.
        let high_start = start_line & !0x7f;
        let mut end_line = high_start | line_delta;

        // If the end line number is smaller than the start line, we have to assume an overflow.
        // The end line will most likely be within 128 lines from the start line. Thus, we account
        // for the overflow by adding 1 to the 8th bit.
        if end_line < start_line {
            end_line += 1 << 7;
        }

        let kind = if self.flags & 0x8000_0000 != 0 {
            LineInfoKind::Statement
        } else {
            LineInfoKind::Expression
        };

        LineEntry::Number(LineNumberEntry {
            offset: self.offset,
            start_line,
            end_line,
            kind,
        })
    }
}

#[derive(Clone, Debug, Default)]
struct DebugLinesIterator<'a> {
    block: DebugLinesBlockHeader,
    buf: ParseBuffer<'a>,
}

impl FallibleIterator for DebugLinesIterator<'_> {
    type Item = LineEntry;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        if self.buf.is_empty() {
            return Ok(None);
        }

        self.buf.parse().map(LineNumberHeader::parse).map(Some)
    }
}

#[derive(Clone, Copy, Debug, Default, Pread)]
#[repr(C, packed)]
struct ColumnNumberEntry {
    start_column: u16,
    end_column: u16,
}

#[derive(Clone, Debug, Default)]
struct DebugColumnsIterator<'a> {
    block: DebugLinesBlockHeader,
    buf: ParseBuffer<'a>,
}

impl FallibleIterator for DebugColumnsIterator<'_> {
    type Item = ColumnNumberEntry;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        if self.buf.is_empty() {
            return Ok(None);
        }

        self.buf.parse().map(Some)
    }
}

#[repr(C, packed)]
#[derive(Clone, Copy, Debug, Default, Pread)]
struct DebugLinesBlockHeader {
    /// Offset of the file checksum in the file checksums debug subsection.
    file_index: u32,

    /// Number of line entries in this block.
    ///
    /// If the debug lines subsection also contains column information (see `has_columns`), then the
    /// same number of column entries will be present after the line entries.
    num_lines: u32,

    /// Total byte size of this block, including following line and column entries.
    block_size: u32,
}

impl DebugLinesBlockHeader {
    /// The byte size of all line and column records combined.
    fn data_size(&self) -> usize {
        self.block_size as usize - std::mem::size_of::<Self>()
    }

    /// The byte size of all line number entries combined.
    fn line_size(&self) -> usize {
        self.num_lines as usize * std::mem::size_of::<LineNumberHeader>()
    }

    /// The byte size of all column number entries combined.
    fn column_size(&self, subsection: DebugLinesHeader) -> usize {
        if subsection.has_columns() {
            self.num_lines as usize * std::mem::size_of::<ColumnNumberEntry>()
        } else {
            0
        }
    }
}

#[derive(Clone, Debug)]
struct DebugLinesBlock<'a> {
    header: DebugLinesBlockHeader,
    line_data: &'a [u8],
    column_data: &'a [u8],
}

impl<'a> DebugLinesBlock<'a> {
    #[allow(unused)]
    fn file_index(&self) -> FileIndex {
        FileIndex(self.header.file_index)
    }

    fn lines(&self) -> DebugLinesIterator<'a> {
        DebugLinesIterator {
            block: self.header,
            buf: ParseBuffer::from(self.line_data),
        }
    }

    fn columns(&self) -> DebugColumnsIterator<'a> {
        DebugColumnsIterator {
            block: self.header,
            buf: ParseBuffer::from(self.line_data),
        }
    }
}

#[derive(Clone, Debug, Default)]
struct DebugLinesBlockIterator<'a> {
    header: DebugLinesHeader,
    buf: ParseBuffer<'a>,
}

impl<'a> FallibleIterator for DebugLinesBlockIterator<'a> {
    type Item = DebugLinesBlock<'a>;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        if self.buf.is_empty() {
            return Ok(None);
        }

        // The header is followed by a variable-size chunk of data, specified by `data_size`. Load
        // all of it at once to ensure we're not reading garbage in case there is more information
        // we do not yet understand.
        let header = self.buf.parse::<DebugLinesBlockHeader>()?;
        let data = self.buf.take(header.data_size())?;

        // The first data is a set of line entries, optionally followed by column entries. Load both
        // and discard eventual data that follows
        let (line_data, data) = data.split_at(header.line_size());
        let (column_data, remainder) = data.split_at(header.column_size(self.header));

        // In case the PDB format is extended with more information, we'd like to know here.
        debug_assert!(remainder.is_empty());

        Ok(Some(DebugLinesBlock {
            header,
            line_data,
            column_data,
        }))
    }
}

/// Possible representations of file checksums in the file checksums subsection.
#[repr(u8)]
#[allow(unused)]
#[derive(Clone, Copy, Debug, Eq, Ord, Hash, PartialEq, PartialOrd)]
enum FileChecksumKind {
    None = 0,
    Md5 = 1,
    Sha1 = 2,
    Sha256 = 3,
}

impl FileChecksumKind {
    /// Parses the checksum kind from its raw value.
    fn parse(value: u8) -> Result<Self> {
        if value <= 3 {
            Ok(unsafe { std::mem::transmute(value) })
        } else {
            Err(Error::UnimplementedFileChecksumKind(value))
        }
    }
}

/// Raw header of a single file checksum entry.
#[derive(Clone, Copy, Debug, Pread)]
struct FileChecksumHeader {
    name_offset: u32,
    checksum_size: u8,
    checksum_kind: u8,
}

/// A file checksum entry.
#[derive(Clone, Debug)]
struct FileChecksumEntry<'a> {
    /// Reference to the file name in the string table.
    name: StringRef,
    /// File checksum value.
    checksum: FileChecksum<'a>,
}

#[derive(Clone, Debug, Default)]
struct DebugFileChecksumsIterator<'a> {
    buf: ParseBuffer<'a>,
}

impl<'a> FallibleIterator for DebugFileChecksumsIterator<'a> {
    type Item = FileChecksumEntry<'a>;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        if self.buf.is_empty() {
            return Ok(None);
        }

        let header = self.buf.parse::<FileChecksumHeader>()?;
        let checksum_data = self.buf.take(header.checksum_size as usize)?;

        let checksum = match FileChecksumKind::parse(header.checksum_kind)? {
            FileChecksumKind::None => FileChecksum::None,
            FileChecksumKind::Md5 => FileChecksum::Md5(checksum_data),
            FileChecksumKind::Sha1 => FileChecksum::Sha1(checksum_data),
            FileChecksumKind::Sha256 => FileChecksum::Sha256(checksum_data),
        };

        self.buf.align(4)?;

        Ok(Some(FileChecksumEntry {
            name: StringRef(header.name_offset),
            checksum,
        }))
    }
}

#[derive(Clone, Debug, Default)]
struct DebugFileChecksumsSubsection<'a> {
    data: &'a [u8],
}

impl<'a> DebugFileChecksumsSubsection<'a> {
    /// Creates a new file checksums subsection.
    fn parse(data: &'a [u8]) -> Result<Self> {
        Ok(DebugFileChecksumsSubsection { data })
    }

    /// Returns an iterator over all file checksum entries.
    #[allow(unused)]
    fn entries(&self) -> Result<DebugFileChecksumsIterator<'a>> {
        self.entries_at_offset(FileIndex(0))
    }

    /// Returns an iterator over file checksum entries starting at the given offset.
    fn entries_at_offset(&self, offset: FileIndex) -> Result<DebugFileChecksumsIterator<'a>> {
        let mut buf = ParseBuffer::from(self.data);
        buf.take(offset.0 as usize)?;
        Ok(DebugFileChecksumsIterator { buf })
    }
}

#[derive(Clone, Debug, Default)]
pub struct C13LineIterator<'a> {
    /// Iterator over all subsections in the current module.
    sections: DebugSubsectionIterator<'a>,
    /// Iterator over all blocks in the current lines subsection.
    blocks: DebugLinesBlockIterator<'a>,
    /// Iterator over lines in the current block.
    lines: DebugLinesIterator<'a>,
    /// Iterator over optional columns in the current block.
    columns: DebugColumnsIterator<'a>,
}

impl<'a> FallibleIterator for C13LineIterator<'a> {
    type Item = LineInfo;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        loop {
            if let Some(entry) = self.lines.next()? {
                // A column entry is only returned if the debug lines subsection contains column
                // information. Otherwise, the columns iterator is empty. We can safely assume that
                // the number of line entries and column entries returned from the two iterators is
                // equivalent. If it were not, the creation of the block would already have failed.
                let column_entry = self.columns.next()?;

                // The high-level line iterator is only interested in actual line entries. It might
                // make sense to eventually fold markers at the same offset into the `LineInfo`
                // record.
                let line_entry = match entry {
                    LineEntry::Number(line_entry) => line_entry,
                    LineEntry::Marker(_) => continue,
                };

                let section_header = self.blocks.header;
                let block_header = self.lines.block;

                return Ok(Some(LineInfo {
                    offset: section_header.offset + line_entry.offset,
                    length: None,
                    file_index: FileIndex(block_header.file_index),
                    line_start: line_entry.start_line,
                    line_end: line_entry.end_line,
                    column_start: column_entry.map(|e| e.start_column),
                    column_end: column_entry.map(|e| e.end_column),
                    kind: line_entry.kind,
                }));
            }

            if let Some(block) = self.blocks.next()? {
                self.lines = block.lines();
                self.columns = block.columns();
                continue;
            }

            if let Some(section) = self.sections.next()? {
                if section.kind == DebugSubsectionKind::Lines {
                    let lines_section = DebugLinesSubsection::parse(section.data)?;
                    self.blocks = lines_section.blocks();
                }
                continue;
            }

            return Ok(None);
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct C13InlineeLineIterator<'a> {
    /// iterator over the inline source lines
    lines: DebugInlineesSourceLineIterator<'a>,
    /// Iterator over all subsections in the current module.
    sections: DebugSubsectionIterator<'a>,
}

impl<'a> FallibleIterator for C13InlineeLineIterator<'a> {
    type Item = InlineeSourceLine;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        loop {
            if let Some(line) = self.lines.next()? {
                return Ok(Some(line));
            }
            if let Some(section) = self.sections.next()? {
                if section.kind == DebugSubsectionKind::InlineeLines {
                    let inlinees_section = DebugInlineesSubsection::parse(section.data)?;
                    self.lines = inlinees_section.lines();
                }
                continue;
            } else {
                return Ok(None);
            }
        }
    }
}

#[derive(Clone, Debug, Default)]
pub struct C13FileIterator<'a> {
    checksums: DebugFileChecksumsIterator<'a>,
}

impl<'a> FallibleIterator for C13FileIterator<'a> {
    type Item = FileInfo<'a>;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        match self.checksums.next() {
            Ok(Some(entry)) => Ok(Some(FileInfo {
                name: entry.name,
                checksum: entry.checksum,
            })),
            Ok(None) => Ok(None),
            Err(error) => Err(error),
        }
    }
}

pub struct C13LineProgram<'a> {
    data: &'a [u8],
    file_checksums: DebugFileChecksumsSubsection<'a>,
}

impl<'a> C13LineProgram<'a> {
    pub(crate) fn parse(data: &'a [u8]) -> Result<Self> {
        let checksums_data = DebugSubsectionIterator::new(data)
            .find(|sec| sec.kind == DebugSubsectionKind::FileChecksums)?
            .map(|sec| sec.data);

        let file_checksums = match checksums_data {
            Some(d) => DebugFileChecksumsSubsection::parse(d)?,
            None => DebugFileChecksumsSubsection::default(),
        };

        Ok(C13LineProgram {
            data,
            file_checksums,
        })
    }

    pub(crate) fn lines(&self) -> C13LineIterator<'a> {
        C13LineIterator {
            sections: DebugSubsectionIterator::new(self.data),
            blocks: DebugLinesBlockIterator::default(),
            lines: DebugLinesIterator::default(),
            columns: DebugColumnsIterator::default(),
        }
    }

    pub(crate) fn lines_at_offset(&self, offset: PdbInternalSectionOffset) -> C13LineIterator<'a> {
        // Since we only care about the start offset of an entire debug lines subsection, we can
        // quickly advance to the first (and only) subsection that matches that offset. Since they
        // are non-overlapping and not empty, we can bail out at the first match.
        let section = DebugSubsectionIterator::new(self.data)
            .filter(|section| section.kind == DebugSubsectionKind::Lines)
            .and_then(|section| DebugLinesSubsection::parse(section.data))
            .find(|lines_section| lines_section.header.offset == offset);

        match section {
            Ok(Some(section)) => C13LineIterator {
                sections: DebugSubsectionIterator::default(),
                blocks: section.blocks(),
                lines: DebugLinesIterator::default(),
                columns: DebugColumnsIterator::default(),
            },
            _ => Default::default(),
        }
    }

    pub(crate) fn files(&self) -> C13FileIterator<'a> {
        C13FileIterator {
            checksums: self.file_checksums.entries().unwrap_or_default(),
        }
    }

    pub(crate) fn inlinee_lines(&self) -> C13InlineeLineIterator<'a> {
        C13InlineeLineIterator {
            sections: DebugSubsectionIterator::new(self.data),
            lines: Default::default(),
        }
    }

    pub(crate) fn get_file_info(&self, index: FileIndex) -> Result<FileInfo<'a>> {
        // The file index actually contains the byte offset value into the file_checksums
        // subsection. Therefore, treat it as the offset.
        let mut entries = self.file_checksums.entries_at_offset(index)?;
        let entry = entries
            .next()?
            .ok_or_else(|| Error::InvalidFileChecksumOffset(index.0))?;

        Ok(FileInfo {
            name: entry.name,
            checksum: entry.checksum,
        })
    }
}
