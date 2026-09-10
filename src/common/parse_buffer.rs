use core::fmt;

use crate::*;
use scroll::{ctx::TryFromCtx, Pread, Endian, LE};

/// Provides little-endian access to a &[u8].
#[derive(Debug, Default, Clone)]
pub struct ParseBuffer<'b>(&'b [u8], usize);

macro_rules! def_parse {
    ( $( ($n:ident, $t:ty) ),* $(,)* ) => {
        $(#[doc(hidden)]
          #[inline]
          #[allow(unused)]
          pub fn $n(&mut self) -> Result<$t> {
              self.parse()
          })*
    }
}

macro_rules! def_peek {
    ( $( ($n:ident, $t:ty) ),* $(,)* ) => {
        $(#[doc(hidden)]
          #[inline]
          pub fn $n(&mut self) -> Result<$t> {
              Ok(self.0.pread_with(self.1, LE)?)
          })*
    }
}

impl<'b> ParseBuffer<'b> {
    /// Return the remaining length of the buffer.
    #[inline]
    pub const fn len(&self) -> usize {
        self.0.len() - self.1
    }

    /// Determines whether this ParseBuffer has been consumed.
    #[inline]
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Return the position within the parent slice.
    #[inline]
    pub const fn pos(&self) -> usize {
        self.1
    }

    /// Seek to the given absolute position.
    #[inline]
    pub fn seek(&mut self, pos: usize) {
        self.1 = core::cmp::min(pos, self.0.len());
    }

    /// Truncates the buffer at the given absolute position.
    #[inline]
    pub fn truncate(&mut self, len: usize) -> Result<()> {
        if self.0.len() >= len {
            self.0 = &self.0[..len];
            Ok(())
        } else {
            Err(Error::UnexpectedEof)
        }
    }

    /// Align the current position to the next multiple of `alignment` bytes.
    #[inline]
    pub const fn align(&mut self, alignment: usize) -> Result<()> {
        let diff = self.1 % alignment;
        if diff > 0 {
            if self.len() < (alignment - diff) {
                return Err(Error::UnexpectedEof);
            }
            self.1 += alignment - diff;
        }
        Ok(())
    }

    /// Parse an object that implements `Pread`.
    pub fn parse<T>(&mut self) -> Result<T>
    where
        T: TryFromCtx<'b, Endian, [u8]>,
        T::Error: From<scroll::Error>,
        Error: From<T::Error>,
    {
        Ok(self.0.gread_with(&mut self.1, LE)?)
    }

    /// Parse an object that implements `Pread` with the given context.
    pub fn parse_with<T, C>(&mut self, ctx: C) -> Result<T>
    where
        T: TryFromCtx<'b, C, [u8]>,
        T::Error: From<scroll::Error>,
        Error: From<T::Error>,
        C: Copy,
    {
        Ok(self.0.gread_with(&mut self.1, ctx)?)
    }

    def_parse!(
        (parse_u8, u8),
        (parse_u16, u16),
        (parse_i16, i16),
        (parse_u32, u32),
        (parse_i32, i32),
        (parse_u64, u64),
        (parse_i64, i64),
    );

    def_peek!((peek_u8, u8), (peek_u16, u16),);

    /// Takes a fixed-size array of bytes from the input.
    ///
    /// Unlike [`Self::take`], this returns an owned `[u8; N]` rather than a
    /// borrowed `&[u8]`, which is convenient for fields with a known size at
    /// compile time (GUIDs, magic strings, etc.).
    ///
    /// # Errors
    ///
    /// Returns [`Error::UnexpectedEof`] if fewer than `N` bytes remain.
    #[inline]
    pub fn take_array<const N: usize>(&mut self) -> Result<[u8; N]> {
        let slice = self.take(N)?;
        let mut out = [0u8; N];
        out.copy_from_slice(slice);

        Ok(out)
    }

    /// Parse a NUL-terminated string from the input.
    #[inline]
    pub fn parse_cstring(&mut self) -> Result<RawString<'b>> {
        let input = &self.0[self.1..];
        let null_idx = input.iter().position(|ch| *ch == 0);

        if let Some(idx) = null_idx {
            self.1 += idx + 1;
            Ok(RawString::from(&input[..idx]))
        } else {
            Err(Error::UnexpectedEof)
        }
    }

    /// Parse a u8-length-prefixed string from the input.
    #[inline]
    pub fn parse_u8_pascal_string(&mut self) -> Result<RawString<'b>> {
        let length = self.parse_u8()? as usize;
        Ok(RawString::from(self.take(length)?))
    }

    /// Take n bytes from the input
    #[inline]
    pub fn take(&mut self, n: usize) -> Result<&'b [u8]> {
        let input = &self.0[self.1..];
        if input.len() >= n {
            self.1 += n;
            Ok(&input[..n])
        } else {
            Err(Error::UnexpectedEof)
        }
    }
}

impl<'b> From<&'b [u8]> for ParseBuffer<'b> {
    
    #[inline]
    fn from(buf: &'b [u8]) -> Self {
        ParseBuffer(buf, 0)
    }
}

impl<'b> fmt::LowerHex for ParseBuffer<'b> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> core::result::Result<(), fmt::Error> {
        write!(f, "ParseBuf::from(\"")?;
        for byte in self.0 {
            write!(f, "\\x{:02x}", byte)?;
        }
        write!(f, "\").as_bytes() at offset {}", self.1)
    }
}
