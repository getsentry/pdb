use scroll::{ctx::TryFromCtx, Pread};

use crate::common::*;
use crate::modi::{constants, FileChecksum, FileIndex, FileInfo, LineInfo, LineInfoKind};
use crate::symbol::{BinaryAnnotation, BinaryAnnotationsIter, InlineSiteSymbol};
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
struct DebugInlineeLinesHeader {
    /// The signature of the inlinees
    signature: u32,
}

impl DebugInlineeLinesHeader {
    pub fn has_extra_files(self) -> bool {
        self.signature == constants::CV_INLINEE_SOURCE_LINE_SIGNATURE_EX
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct InlineeSourceLine<'a> {
    pub inlinee: ItemId,
    pub file_id: FileIndex,
    pub line: u32,
    extra_files: &'a [u8],
}

impl<'a> InlineeSourceLine<'a> {
    // TODO: Implement extra files iterator when needed.
}

impl<'a> TryFromCtx<'a, DebugInlineeLinesHeader> for InlineeSourceLine<'a> {
    type Error = Error;
    type Size = usize;

    fn try_from_ctx(this: &'a [u8], header: DebugInlineeLinesHeader) -> Result<(Self, Self::Size)> {
        let mut buf = ParseBuffer::from(this);
        let inlinee = buf.parse()?;
        let file_id = buf.parse()?;
        let line = buf.parse()?;

        let extra_files = if header.has_extra_files() {
            let file_count = buf.parse::<u32>()? as usize;
            buf.take(file_count * std::mem::size_of::<u32>())?
        } else {
            &[]
        };

        let source_line = Self {
            inlinee,
            file_id,
            line,
            extra_files,
        };

        Ok((source_line, buf.pos()))
    }
}

#[derive(Debug, Clone, Default)]
struct DebugInlineeLinesIterator<'a> {
    header: DebugInlineeLinesHeader,
    buf: ParseBuffer<'a>,
}

impl<'a> FallibleIterator for DebugInlineeLinesIterator<'a> {
    type Item = InlineeSourceLine<'a>;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        if self.buf.is_empty() {
            Ok(None)
        } else {
            Ok(Some(self.buf.parse_with(self.header)?))
        }
    }
}

#[derive(Clone, Debug, Default)]
struct DebugInlineeLinesSubsection<'a> {
    header: DebugInlineeLinesHeader,
    data: &'a [u8],
}

impl<'a> DebugInlineeLinesSubsection<'a> {
    fn parse(data: &'a [u8]) -> Result<Self> {
        let mut buf = ParseBuffer::from(data);
        let header = buf.parse::<DebugInlineeLinesHeader>()?;

        Ok(DebugInlineeLinesSubsection {
            header,
            data: &data[buf.pos()..],
        })
    }

    /// Iterate through all inlinees.
    fn lines(&self) -> DebugInlineeLinesIterator<'a> {
        DebugInlineeLinesIterator {
            header: self.header,
            buf: ParseBuffer::from(self.data),
        }
    }

    /// Retrieve the inlinee source line for the given inlinee.
    fn find(&self, inlinee: ItemId) -> Result<Option<InlineeSourceLine<'a>>> {
        self.lines().find(|line| line.inlinee == inlinee)
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
                    length: None, // TODO(ja): Infer length from the next entry or the parent..?
                    file_index: FileIndex(block_header.file_index),
                    line_start: line_entry.start_line,
                    line_end: line_entry.end_line,
                    column_start: column_entry.map(|e| e.start_column.into()),
                    column_end: column_entry.map(|e| e.end_column.into()),
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
    annotations: BinaryAnnotationsIter<'a>,
    file_index: FileIndex,
    code_offset_base: u32,
    code_offset: PdbInternalSectionOffset,
    code_length: Option<u32>,
    line: u32,
    line_length: u32,
    col_start: Option<u32>,
    col_end: Option<u32>,
    line_kind: LineInfoKind,
    last_info: Option<LineInfo>,
}

impl<'a> C13InlineeLineIterator<'a> {
    fn new(
        parent_offset: PdbInternalSectionOffset,
        inline_site: &InlineSiteSymbol<'a>,
        inlinee_line: InlineeSourceLine<'a>,
    ) -> Self {
        C13InlineeLineIterator {
            annotations: inline_site.annotations.iter(),
            file_index: inlinee_line.file_id,
            code_offset_base: 0,
            code_offset: parent_offset,
            code_length: None,
            line: inlinee_line.line,
            line_length: 1,
            col_start: None,
            col_end: None,
            line_kind: LineInfoKind::Statement,
            last_info: None,
        }
    }
}

impl<'a> FallibleIterator for C13InlineeLineIterator<'a> {
    type Item = LineInfo;
    type Error = Error;

    fn next(&mut self) -> Result<Option<Self::Item>> {
        while let Some(op) = self.annotations.next()? {
            match op {
                BinaryAnnotation::CodeOffset(code_offset) => {
                    self.code_offset.offset = code_offset;
                }
                BinaryAnnotation::ChangeCodeOffsetBase(code_offset_base) => {
                    self.code_offset_base = code_offset_base;
                }
                BinaryAnnotation::ChangeCodeOffset(delta) => {
                    self.code_offset = self.code_offset.wrapping_add(delta);
                }
                BinaryAnnotation::ChangeCodeLength(code_length) => {
                    if let Some(ref mut last_info) = self.last_info {
                        if last_info.length.is_none() && last_info.kind == self.line_kind {
                            last_info.length = Some(code_length);
                        }
                    }

                    self.code_offset = self.code_offset.wrapping_add(code_length);
                }
                BinaryAnnotation::ChangeFile(file_index) => {
                    // NOTE: There seems to be a bug in VS2015-VS2019 compilers that generates
                    // invalid binary annotations when file changes are involved. This can be
                    // triggered by #including files directly into inline functions. The
                    // `ChangeFile` annotations are generated in the wrong spot or missing
                    // completely. This renders information on the file effectively useless in a lot
                    // of cases.
                    self.file_index = file_index;
                }
                BinaryAnnotation::ChangeLineOffset(delta) => {
                    self.line = (i64::from(self.line) + i64::from(delta)) as u32;
                }
                BinaryAnnotation::ChangeLineEndDelta(line_length) => {
                    self.line_length = line_length;
                }
                BinaryAnnotation::ChangeRangeKind(kind) => {
                    self.line_kind = match kind {
                        0 => LineInfoKind::Expression,
                        1 => LineInfoKind::Statement,
                        _ => self.line_kind,
                    };
                }
                BinaryAnnotation::ChangeColumnStart(col_start) => {
                    self.col_start = Some(col_start);
                }
                BinaryAnnotation::ChangeColumnEndDelta(delta) => {
                    self.col_end = self
                        .col_end
                        .map(|col_end| (i64::from(col_end) + i64::from(delta)) as u32)
                }
                BinaryAnnotation::ChangeCodeOffsetAndLineOffset(code_delta, line_delta) => {
                    self.code_offset += code_delta;
                    self.line = (i64::from(self.line) + i64::from(line_delta)) as u32;
                }
                BinaryAnnotation::ChangeCodeLengthAndCodeOffset(code_length, code_delta) => {
                    self.code_length = Some(code_length);
                    self.code_offset += code_delta;
                }
                BinaryAnnotation::ChangeColumnEnd(col_end) => {
                    self.col_end = Some(col_end);
                }
            }

            if !op.emits_line_info() {
                continue;
            }

            if let Some(ref mut last_info) = self.last_info {
                if last_info.length.is_none() && last_info.kind == self.line_kind {
                    last_info.length = Some(self.code_offset.offset - self.code_offset_base);
                }
            }

            let line_info = LineInfo {
                kind: self.line_kind,
                file_index: self.file_index,
                offset: self.code_offset + self.code_offset_base,
                length: self.code_length,
                line_start: self.line,
                line_end: self.line + self.line_length,
                column_start: self.col_start,
                column_end: self.col_end,
            };

            // Code length resets with every line record.
            self.code_length = None;

            // Finish the previous record and emit it. The current record is stored so that the
            // length can be inferred from subsequent operators or the next line info.
            if let Some(last_info) = std::mem::replace(&mut self.last_info, Some(line_info)) {
                return Ok(Some(last_info));
            }
        }

        Ok(self.last_info.take())
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
    inlinee_lines: DebugInlineeLinesSubsection<'a>,
}

impl<'a> C13LineProgram<'a> {
    pub(crate) fn parse(data: &'a [u8]) -> Result<Self> {
        let mut file_checksums = DebugFileChecksumsSubsection::default();
        let mut inlinee_lines = DebugInlineeLinesSubsection::default();

        let mut subsections = DebugSubsectionIterator::new(data);
        while let Some(sec) = subsections.next()? {
            match sec.kind {
                DebugSubsectionKind::FileChecksums => {
                    file_checksums = DebugFileChecksumsSubsection::parse(sec.data)?;
                }
                DebugSubsectionKind::InlineeLines => {
                    inlinee_lines = DebugInlineeLinesSubsection::parse(sec.data)?;
                }
                _ => {}
            }
        }

        Ok(C13LineProgram {
            data,
            file_checksums,
            inlinee_lines,
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

    pub(crate) fn inlinee_lines(
        &self,
        parent_offset: PdbInternalSectionOffset,
        inline_site: &InlineSiteSymbol<'a>,
    ) -> C13InlineeLineIterator<'a> {
        match self.inlinee_lines.find(inline_site.inlinee) {
            Ok(Some(inlinee_line)) => {
                C13InlineeLineIterator::new(parent_offset, inline_site, inlinee_line)
            }
            _ => C13InlineeLineIterator::default(),
        }
    }

    pub(crate) fn files(&self) -> C13FileIterator<'a> {
        C13FileIterator {
            checksums: self.file_checksums.entries().unwrap_or_default(),
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

#[cfg(test)]
mod tests {
    use super::*;

    use crate::symbol::{BinaryAnnotations, SymbolIndex};

    #[test]
    fn test_parse_inlinee_lines() {
        let data = &[
            0, 0, 0, 0, 254, 18, 0, 0, 104, 1, 0, 0, 24, 0, 0, 0, 253, 18, 0, 0, 104, 1, 0, 0, 28,
            0, 0, 0,
        ];

        let inlinee_lines = DebugInlineeLinesSubsection::parse(data).expect("parse inlinee lines");
        assert!(!inlinee_lines.header.has_extra_files());

        let lines: Vec<_> = inlinee_lines
            .lines()
            .collect()
            .expect("collect inlinee lines");

        let expected = [
            InlineeSourceLine {
                inlinee: 0x12FE,
                file_id: FileIndex(0x168),
                line: 24,
                extra_files: &[],
            },
            InlineeSourceLine {
                inlinee: 0x12FD,
                file_id: FileIndex(0x168),
                line: 28,
                extra_files: &[],
            },
        ];

        assert_eq!(lines, expected);
    }

    #[test]
    fn test_parse_inlinee_lines_with_files() {
        let data = &[
            1, 0, 0, 0, 235, 102, 9, 0, 232, 37, 0, 0, 19, 0, 0, 0, 1, 0, 0, 0, 216, 26, 0, 0, 240,
            163, 7, 0, 176, 44, 0, 0, 120, 0, 0, 0, 1, 0, 0, 0, 120, 3, 0, 0,
        ];

        let inlinee_lines = DebugInlineeLinesSubsection::parse(data).expect("parse inlinee lines");
        assert!(inlinee_lines.header.has_extra_files());

        let lines: Vec<_> = inlinee_lines
            .lines()
            .collect()
            .expect("collect inlinee lines");

        let expected = [
            InlineeSourceLine {
                inlinee: 0x966EB,
                file_id: FileIndex(0x25e8),
                line: 19,
                extra_files: &[216, 26, 0, 0],
            },
            InlineeSourceLine {
                inlinee: 0x7A3F0,
                file_id: FileIndex(0x2cb0),
                line: 120,
                extra_files: &[120, 3, 0, 0],
            },
        ];

        assert_eq!(lines, expected)
    }

    #[test]
    fn test_inlinee_lines() {
        // Obtained from a PDB compiling Breakpad's crash_generation_client.obj

        // S_GPROC32: [0001:00000120], Cb: 00000054
        //   S_INLINESITE: Parent: 0000009C, End: 00000318, Inlinee:             0x1173
        //     S_INLINESITE: Parent: 00000190, End: 000001EC, Inlinee:             0x1180
        //     BinaryAnnotations:    CodeLengthAndCodeOffset 2 3f  CodeLengthAndCodeOffset 3 9
        let inline_site = InlineSiteSymbol {
            parent: SymbolIndex(0x190),
            end: SymbolIndex(0x1ec),
            inlinee: 0x1180,
            invocations: None,
            annotations: BinaryAnnotations::new(&[12, 2, 63, 12, 3, 9, 0, 0]),
        };

        // Inline site from corresponding DEBUG_S_INLINEELINES subsection:
        let inlinee_line = InlineeSourceLine {
            inlinee: 0x1180,
            file_id: FileIndex(0x270),
            line: 341,
            extra_files: &[],
        };

        // Parent offset from procedure root:
        // S_GPROC32: [0001:00000120]
        let parent_offset = PdbInternalSectionOffset {
            offset: 0x120,
            section: 0x1,
        };

        let iter = C13InlineeLineIterator::new(parent_offset, &inline_site, inlinee_line);
        let lines: Vec<_> = iter.collect().expect("collect inlinee lines");

        let expected = [
            LineInfo {
                offset: PdbInternalSectionOffset {
                    section: 0x1,
                    offset: 0x015f,
                },
                length: Some(2),
                file_index: FileIndex(0x270),
                line_start: 341,
                line_end: 342,
                column_start: None,
                column_end: None,
                kind: LineInfoKind::Statement,
            },
            LineInfo {
                offset: PdbInternalSectionOffset {
                    section: 0x1,
                    offset: 0x0168,
                },
                length: Some(3),
                file_index: FileIndex(0x270),
                line_start: 341,
                line_end: 342,
                column_start: None,
                column_end: None,
                kind: LineInfoKind::Statement,
            },
        ];

        assert_eq!(lines, expected);
    }
}
