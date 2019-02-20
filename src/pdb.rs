// Copyright 2017 pdb Developers
//
// Licensed under the Apache License, Version 2.0, <LICENSE-APACHE or
// http://apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT or
// http://opensource.org/licenses/MIT>, at your option. This file may not be
// copied, modified, or distributed except according to those terms.

use dbi;
use module_info;
use msf;
use symbol;
use tpi;
use pdbi;

use common::*;
use dbi::{DebugInformation, Module};
use module_info::ModuleInfo;
use source::Source;
use msf::{MSF, Stream};
use symbol::SymbolTable;
use tpi::TypeInformation;
use pdbi::PDBInformation;
use pe::ImageSectionHeader;
use omap::OMAPTable;

/// Some streams have a fixed stream index.
/// http://llvm.org/docs/PDB/index.html

const PDB_STREAM: u32 = 1;
const TPI_STREAM: u32 = 2;
const DBI_STREAM: u32 = 3;
#[allow(dead_code)]
const IPI_STREAM: u32 = 4;

/// `PDB` provides access to the data within a PDB file.
///
/// A PDB file is internally a Multi-Stream File (MSF), composed of multiple independent
/// (and usually discontiguous) data streams on-disk. `PDB` provides lazy access to these data
/// structures, which means the `PDB` accessor methods usually cause disk accesses.
#[derive(Debug)]
pub struct PDB<'s, S> {
    /// `msf` provides access to the underlying data streams
    msf: Box<MSF<'s, S> + 's>,

    /// Memoize the `dbi::Header`, since it contains stream numbers we sometimes need
    dbi_header: Option<dbi::Header>,

    /// Memoize the `dbi::DBIExtraStreams`, since it too contains stream numbers we sometimes need
    dbi_extra_streams: Option<dbi::DBIExtraStreams>,
}

impl<'s, S: Source<'s> + 's> PDB<'s, S> {
    /// Create a new `PDB` for a `Source`.
    ///
    /// `open()` accesses enough of the source file to find the MSF stream table. This usually
    /// involves reading the header, a block near the end of the file, and finally the stream table
    /// itself. It does not access or validate any of the contents of the rest of the PDB.
    ///
    /// # Errors
    ///
    /// * `Error::UnimplementedFeature` if the PDB file predates ~2002
    /// * `Error::UnrecognizedFileFormat` if the `Source` does not appear to be a PDB file
    /// * `Error::IoError` if returned by the `Source`
    /// * `Error::PageReferenceOutOfRange`, `Error::InvalidPageSize` if the PDB file seems corrupt
    pub fn open(source: S) -> Result<PDB<'s, S>> {
        let msf = msf::open_msf(source)?;

        Ok(PDB{
            msf: msf,
            dbi_header: None,
            dbi_extra_streams: None,
        })
    }

    /// Retrieve the `PDBInformation` for this PDB.
    ///
    /// The `PDBInformation` object contains the GUID and age fields that can be used to verify
    /// that a PDB file matches a binary, as well as the stream indicies of named PDB streams.
    ///
    /// # Errors
    ///
    /// * `Error::StreamNotFound` if the PDB somehow does not contain the PDB information stream
    /// * `Error::IoError` if returned by the `Source`
    /// * `Error::PageReferenceOutOfRange` if the PDB file seems corrupt
    pub fn pdb_information(&mut self) -> Result<PDBInformation<'s>> {
        let stream: Stream = self.msf.get(PDB_STREAM, None)?;

        // Parse it
        let pdb_info = pdbi::new_pdb_information(stream)?;

        // Return
        Ok(pdb_info)
    }

    /// Retrieve the `TypeInformation` for this PDB.
    ///
    /// The `TypeInformation` object owns a `SourceView` for the type information ("TPI") stream.
    /// This is usually the single largest stream of the PDB file.
    ///
    /// # Errors
    ///
    /// * `Error::StreamNotFound` if the PDB somehow does not contain the type information stream
    /// * `Error::IoError` if returned by the `Source`
    /// * `Error::PageReferenceOutOfRange` if the PDB file seems corrupt
    /// * `Error::InvalidTypeInformationHeader` if the type information stream header was not
    ///   understood
    pub fn type_information(&mut self) -> Result<TypeInformation<'s>> {
        let stream: Stream = self.msf.get(TPI_STREAM, None)?;

        // Parse it
        let type_info = tpi::new_type_information(stream)?;

        // Return
        Ok(type_info)
    }

    /// Retrieve the `DebugInformation` for this PDB.
    ///
    /// The `DebugInformation` object owns a `SourceView` for the debug information ("DBI") stream.
    ///
    /// # Errors
    ///
    /// * `Error::StreamNotFound` if the PDB somehow does not contain a symbol records stream
    /// * `Error::IoError` if returned by the `Source`
    /// * `Error::PageReferenceOutOfRange` if the PDB file seems corrupt
    /// * `Error::UnimplementedFeature` if the debug information header predates ~1995
    pub fn debug_information(&mut self) -> Result<DebugInformation<'s>> {
        let stream: Stream = self.msf.get(DBI_STREAM, None)?;

        // Parse it
        let debug_info = dbi::DebugInformation::new(stream)?;

        // Grab its header, since we need that for unrelated operations
        self.dbi_header = Some(debug_info.get_header());

        // Return
        Ok(debug_info)
    }

    fn dbi_header(&mut self) -> Result<dbi::Header> {
        // see if we've already got a header
        if let Some(ref h) = self.dbi_header {
            return Ok(*h);
        }

        let header;

        {
            // get just the first little bit of the DBI stream
            let stream: Stream = self.msf.get(DBI_STREAM, Some(1024))?;
            let mut buf = stream.parse_buffer();
            header = dbi::parse_header(&mut buf)?;
        }

        self.dbi_header = Some(header);

        Ok(header)
    }

    /// Retrieve the global symbol table for this PDB.
    ///
    /// The `SymbolTable` object owns a `SourceView` for the symbol records stream. This is usually
    /// the second-largest stream of the PDB file.
    ///
    /// The debug information stream indicates which stream is the symbol records stream, so
    /// `global_symbols()` accesses the debug information stream to read the header unless
    /// `debug_information()` was called first.
    ///
    /// # Errors
    ///
    /// * `Error::StreamNotFound` if the PDB somehow does not contain a symbol records stream
    /// * `Error::IoError` if returned by the `Source`
    /// * `Error::PageReferenceOutOfRange` if the PDB file seems corrupt
    ///
    /// If `debug_information()` was not already called, `global_symbols()` will additionally read
    /// the debug information header, in which case it can also return:
    ///
    /// * `Error::StreamNotFound` if the PDB somehow does not contain a debug information stream
    /// * `Error::UnimplementedFeature` if the debug information header predates ~1995
    pub fn global_symbols(&mut self) -> Result<SymbolTable<'s>> {
        // the global symbol table is stored in a stream number described by the DBI header
        // so, start by getting the DBI header
        let dbi_header = self.dbi_header()?;

        // open the appropriate stream
        let stream: Stream = self.msf.get(dbi_header.symbol_records_stream as u32, None)?;

        Ok(symbol::new_symbol_table(stream))
    }

    /// Retrieve the module info stream for a specific `Module`.
    ///
    /// Some information for each module is stored in a separate stream per-module.
    /// `Module`s can be retrieved from the `PDB` by first calling [`debug_information`] to
    /// get the debug information stream, and then calling [`modules`] on that.
    ///
    /// # Errors
    ///
    /// * `Error::StreamNotFound` if the PDB does not contain this module info stream
    /// * `Error::IoError` if returned by the `Source`
    /// * `Error::PageReferenceOutOfRange` if the PDB file seems corrupt
    /// * `Error::UnimplementedFeature` if the module information stream is an unsupported
    ///   version
    ///
    /// # Example
    ///
    /// ```
    /// # use pdb::FallibleIterator;
    /// #
    /// # fn test() -> pdb::Result<()> {
    /// let file = std::fs::File::open("fixtures/self/foo.pdb")?;
    /// let mut pdb = pdb::PDB::open(file)?;
    /// let dbi = pdb.debug_information()?;
    /// let mut modules = dbi.modules()?;
    /// if let Some(module) = modules.next()? {
    ///     println!("module name: {}, object file name: {}",
    ///              module.module_name(), module.object_file_name());
    ///     let info = pdb.module_info(&module)?;
    ///     println!("contains {} symbols", info.symbols()?.count()?);
    /// }
    ///
    /// # Ok(())
    /// # }
    /// ```
    ///
    /// [`debug_information`]: #method.debug_information
    /// [`modules`]: struct.DebugInformation.html#method.modules
    pub fn module_info<'m>(&mut self, module: &Module<'m>) -> Result<ModuleInfo<'s>> {
        let res = {
            let stream: Stream = self.msf.get(module.info().stream as u32, None)?;
            module_info::new_module_info(stream, module)
        };
        res
    }

    /// Retrieve the executable's section headers, as stored inside this PDB.
    ///
    /// The debug information stream indicates which stream contains the section headers, so
    /// `sections()` accesses the debug information stream to read the header unless
    /// `debug_information()` was called first.
    ///
    /// # Errors
    ///
    /// * `Error::StreamNotFound` if the PDB somehow does not contain section headers
    /// * `Error::IoError` if returned by the `Source`
    /// * `Error::PageReferenceOutOfRange` if the PDB file seems corrupt
    /// * `Error::UnexpectedEof` if the section headers are truncated mid-record
    ///
    /// If `debug_information()` was not already called, `sections()` will additionally read
    /// the debug information header, in which case it can also return:
    ///
    /// * `Error::StreamNotFound` if the PDB somehow does not contain a debug information stream
    /// * `Error::UnimplementedFeature` if the debug information header predates ~1995
    pub fn sections(&mut self) -> Result<Option<Vec<ImageSectionHeader>>> {
        // Open the appropriate stream
        let stream_number = match self.extra_streams()?.section_headers() {
            Some(number) => number,
            None => return Ok(None),
        };

        // Parse
        let stream = self.msf.get(stream_number as u32, None)?;
        let mut buf = stream.parse_buffer();
        let mut headers = Vec::with_capacity(buf.len() / 40);
        while buf.len() > 0 {
            headers.push(ImageSectionHeader::parse(&mut buf)?);
        }

        Ok(Some(headers))
    }

    /// Retrieve the executable's original section headers, as stored inside this PDB.
    ///
    /// Offsets and addresses from these sections need to be rebased using `omap_from_src`.
    ///
    /// The debug information stream indicates which stream contains the section headers, so
    /// `sections()` accesses the debug information stream to read the header unless
    /// `debug_information()` was called first.
    ///
    /// # Errors
    ///
    /// * `Error::StreamNotFound` if the PDB somehow does not contain section headers
    /// * `Error::IoError` if returned by the `Source`
    /// * `Error::PageReferenceOutOfRange` if the PDB file seems corrupt
    /// * `Error::UnexpectedEof` if the section headers are truncated mid-record
    ///
    /// If `debug_information()` was not already called, `sections()` will additionally read
    /// the debug information header, in which case it can also return:
    ///
    /// * `Error::StreamNotFound` if the PDB somehow does not contain a debug information stream
    /// * `Error::UnimplementedFeature` if the debug information header predates ~1995
    pub fn original_sections(&mut self) -> Result<Option<Vec<ImageSectionHeader>>> {
        // Open the appropriate stream
        let stream_number = match self.extra_streams()?.original_section_headers() {
            Some(number) => number,
            None => return Ok(None),
        };

        // Parse
        let stream = self.msf.get(stream_number as u32, None)?;
        let mut buf = stream.parse_buffer();
        let mut headers = Vec::with_capacity(buf.len() / 40);
        while buf.len() > 0 {
            headers.push(ImageSectionHeader::parse(&mut buf)?);
        }

        Ok(Some(headers))
    }

    /// Retrieve the executable's OMAP lookup table for mapping section offsets to RVAs (relative
    /// virtual addresses).
    ///
    /// The debug information stream indicates which stream contains the section headers, so
    /// `omap_table()` accesses the debug information stream to read the header unless
    /// `debug_information()` was called first.
    ///
    /// # Errors
    ///
    /// * `Error::StreamNotFound` if the PDB somehow does not contain section headers
    /// * `Error::IoError` if returned by the `Source`
    /// * `Error::PageReferenceOutOfRange` if the PDB file seems corrupt
    /// * `Error::UnexpectedEof` if the section headers are truncated mid-record
    ///
    /// If `debug_information()` was not already called, `omap_table()` will additionally read
    /// the debug information header, in which case it can also return:
    ///
    /// * `Error::StreamNotFound` if the PDB somehow does not contain a debug information stream
    /// * `Error::UnimplementedFeature` if the debug information header predates ~1995
    pub fn omap_from_src(&mut self) -> Result<Option<OMAPTable<'s>>> {
        // Open the appropriate stream
        let stream_number = match self.extra_streams()?.omap_from_src().map(u32::from) {
            Some(number) => number,
            None => return Ok(None),
        };

        let stream = self.msf.get(stream_number as u32, None)?;
        let table = OMAPTable::parse(stream)?;
        Ok(Some(table))
    }

    /// Retrieve the executable's OMAP lookup table for RVAs (relative virtual addresses) to section
    /// offsets.
    ///
    /// The debug information stream indicates which stream contains the section headers, so
    /// `omap_table()` accesses the debug information stream to read the header unless
    /// `debug_information()` was called first.
    ///
    /// # Errors
    ///
    /// * `Error::StreamNotFound` if the PDB somehow does not contain section headers
    /// * `Error::IoError` if returned by the `Source`
    /// * `Error::PageReferenceOutOfRange` if the PDB file seems corrupt
    /// * `Error::UnexpectedEof` if the section headers are truncated mid-record
    ///
    /// If `debug_information()` was not already called, `omap_table()` will additionally read the
    /// debug information header, in which case it can also return:
    ///
    /// * `Error::StreamNotFound` if the PDB somehow does not contain a debug information stream
    /// * `Error::UnimplementedFeature` if the debug information header predates ~1995
    pub fn omap_to_src(&mut self) -> Result<Option<OMAPTable<'s>>> {
        // Open the appropriate stream
        let stream_number = match self.extra_streams()?.omap_to_src().map(u32::from) {
            Some(number) => number,
            None => return Ok(None),
        };

        let stream = self.msf.get(stream_number as u32, None)?;
        let table = OMAPTable::parse(stream)?;
        Ok(Some(table))
    }

    /// Build a map that translates relative section offsets to RVAs (relative virtual addresses).
    ///
    /// This reads `omap_from_src` and either `original_sections` or `sections` from this PDB and
    /// chooses internally which strategy to use for resolving RVAs. Consider to reuse this instance
    /// for multiple translations.
    ///
    /// # Errors
    ///
    /// * `Error::OmapNotFound` if an OMAP is required for translation but missing
    /// * `Error::StreamNotFound` if the PDB somehow does not contain section headers
    /// * `Error::IoError` if returned by the `Source`
    /// * `Error::PageReferenceOutOfRange` if the PDB file seems corrupt
    /// * `Error::UnexpectedEof` if the section headers are truncated mid-record
    ///
    /// If `debug_information()` was not already called, `omap_table()` will additionally read the
    /// debug information header, in which case it can also return:
    ///
    /// * `Error::StreamNotFound` if the PDB somehow does not contain a debug information stream
    /// * `Error::UnimplementedFeature` if the debug information header predates ~1995
    pub fn address_translator(&mut self) -> Result<AddressTranslator<'s>> {
        Ok(match self.original_sections()? {
            Some(sections) => {
                let omap = self.omap_from_src()?.ok_or_else(|| Error::AddressMapNotFound)?;
                AddressTranslator { sections, omap: Some(omap) }
            }
            None => AddressTranslator {
                sections: self.sections()?.ok_or_else(|| Error::AddressMapNotFound)?,
                omap: None,
            }
        })
    }

    /// Retrieve a stream by its index to read its contents as bytes.
    ///
    /// # Errors
    ///
    /// * `Error::StreamNotFound` if the PDB does not contain this module info stream
    /// * `Error::IoError` if returned by the `Source`
    /// * `Error::PageReferenceOutOfRange` if the PDB file seems corrupt
    ///
    /// # Example
    ///
    /// ```
    /// # fn test() -> pdb::Result<()> {
    /// let file = std::fs::File::open("fixtures/self/foo.pdb")?;
    /// let mut pdb = pdb::PDB::open(file)?;
    /// // This is the index of the "mystream" stream that was added using pdbstr.exe.
    /// let s = pdb.raw_stream(208)?;
    /// let mut buf = s.parse_buffer();
    /// let len = buf.len();
    /// let bytes = buf.take(len)?;
    /// assert_eq!(bytes, b"hello world\n");
    /// # Ok(())
    /// # }
    /// ```
    pub fn raw_stream(&mut self, stream: u32) -> Result<Stream<'s>> {
        self.msf.get(stream, None)
    }

    /// Loads the Optional Debug Header Stream, which contains offsets into extra streams.
    ///
    /// this stream is always returned, but its members are all optional depending on the data
    /// present in the PDB.
    ///
    /// The optional header begins at offset 0 immediately after the EC Substream ends.
    fn extra_streams(&mut self) -> Result<dbi::DBIExtraStreams> {
        if let Some(extra) = self.dbi_extra_streams {
            return Ok(extra);
        }

        // Parse and grab information on extra streams, since we might also need that
        let debug_info = self.debug_information()?;
        let extra = dbi::DBIExtraStreams::new(&debug_info)?;
        self.dbi_extra_streams = Some(extra);

        Ok(extra)
    }
}

/// A helper to resolve RVAs (relative virtual addresses) from segment offsets.
///
/// Addresses in PDBs are stored as offsets to segments, which refer to sections in the original PE.
/// For some binaries, these addresses are additionally reordered to optimize them for paging
/// reduction.
///
/// An instance of this translator can be obtained via `PDB::address_translator`.
pub struct AddressTranslator<'s> {
    sections: Vec<ImageSectionHeader>,
    omap: Option<OMAPTable<'s>>,
}

impl<'s> AddressTranslator<'s> {
    /// Lookup an address relative to the `image_base` from a segment and offset.
    ///
    /// An RVA of `0` may either indicate a location that does not exist in the original address
    /// space, a reference to a non-existent section, or simply the start of the image. Thus, take
    /// special care when zero is returned from this function.
    pub fn to_rva(&self, segment: u16, offset: u32) -> Option<u32> {
        if segment == 0 {
            return None;
        }

        let section = match self.sections.get(segment as usize - 1) {
            Some(section) => section,
            None => return None,
        };

        let address = section.virtual_address + offset;
        match self.omap {
            Some(ref omap) => omap.lookup(address),
            None => Some(address),
        }
    }
}
