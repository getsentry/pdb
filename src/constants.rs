/// Stream 0: the previous MSF stream directory.
///
/// This is the "old directory" from before the current stream directory
/// was written. It is only meaningful in Small MSF files; in Big MSF it
/// is typically a nil stream.
pub const OLD_DIRECTORY_STREAM: u32 = 0;

/// Stream 1: the PDB Information Stream.
///
/// Contains the PDB version, the GUID and age used to match the PDB to its
/// EXE, and the **named stream map** that maps names like `/names` and
/// `/src/headerblock` to stream indices.
///
/// See <https://llvm.org/docs/PDB/PdbStream.html>.
pub const PDB_STREAM: u32 = 1;

/// Stream 2: the Type Information (TPI) Stream.
///
/// Contains CodeView type records for the entire program, and the index of
/// the TPI hash stream used for fast lookup by name.
///
/// See <https://llvm.org/docs/PDB/TpiStream.html>.
pub const TPI_STREAM: u32 = 2;

/// Stream 3: the Debug Information (DBI) Stream.
///
/// Contains module/compiland info, section contributions, source file info,
/// FPO/PGO data, and the indices of the Public, Global, and per-module
/// streams.
///
/// See <https://llvm.org/docs/PDB/DbiStream.html>.
pub const DBI_STREAM: u32 = 3;

/// Size of a `NewDBIHdr` in bytes.
///
/// This is the fixed portion of the DBI stream that precedes the module list
/// and other substreams. It is stable across PDB versions that use the modern
/// DBI header layout (VC7 and later), regardless of whether the containing
/// MSF file is Small or Big.
///
/// Some older VC6-era PDBs use the pre-`NewDBIHdr` layout, which has a
/// different signature, field order, and size. Those are not parsed by this
/// crate and are rejected by [`DBIHeader::parse_buf`] before this length is
/// used.
pub const DBI_HEADER_LEN: usize = 64;

/// Stream 4: the Id Information (IPI) Stream.
///
/// Contains CodeView type records for "id" types (compiler-generated
/// types referenced by the DBI stream), and the index of the IPI hash stream.
///
/// See <https://llvm.org/docs/PDB/TpiStream.html>.
pub const IPI_STREAM: u32 = 4;