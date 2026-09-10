use core::{cmp, fmt, slice};

use crate::io::ErrorKind;

use crate::io::{IoError, Result};

#[cfg(feature = "alloc")]
pub use alloc::vec::Vec;

#[cfg(feature = "alloc")]
struct Guard<'a> {
    buf: &'a mut Vec<u8>,
    len: usize,
}

#[cfg(feature = "alloc")]
impl Drop for Guard<'_> {
    fn drop(&mut self) {
        unsafe {
            self.buf.set_len(self.len);
        }
    }
}

#[cfg(feature = "alloc")]
fn read_to_end<R: Read + ?Sized>(r: &mut R, buf: &mut Vec<u8>) -> Result<usize> {
    read_to_end_with_reservation(r, buf, |_| 32)
}

#[cfg(feature = "alloc")]
fn read_to_end_with_reservation<R, F>(
    r: &mut R,
    buf: &mut Vec<u8>,
    mut reservation_size: F,
) -> Result<usize>
where
    R: Read + ?Sized,
    F: FnMut(&R) -> usize,
{
    let start_len = buf.len();
    let mut g = Guard {
        len: buf.len(),
        buf,
    };
    loop {
        if g.len == g.buf.len() {
            unsafe {
    
                g.buf.reserve(reservation_size(r));
                let capacity = g.buf.capacity();
                g.buf.set_len(capacity);
                #[allow(deprecated)]
                r.initializer().initialize(&mut g.buf[g.len..]);
            }
        }

        let buf = &mut g.buf[g.len..];
        match r.read(buf) {
            Ok(0) => return Ok(g.len - start_len),
            Ok(n) => {

                assert!(n <= buf.len());
                g.len += n;
            }
            Err(ref e) if e.kind() == ErrorKind::Interrupted => {}
            Err(e) => return Err(e),
        }
    }
}

pub trait Read {
   
    fn read(&mut self, buf: &mut [u8]) -> Result<usize>;

    #[cfg(feature = "alloc")]
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize> {
        read_to_end(self, buf)
    }

  
    #[cfg(feature = "alloc")]
    fn read_to_string(&mut self, buf: &mut alloc::string::String) -> Result<usize> {
        use crate::io::{IoError, ErrorKind};

        let mut bytes = Vec::new();
        let n = self.read_to_end(&mut bytes)?;
        match core::str::from_utf8(&bytes) {
            Ok(s) => {
                buf.push_str(s);
                Ok(n)
            }
            Err(_) => Err(IoError::new(
                ErrorKind::InvalidData,
                "stream did not contain valid UTF-8",
            )),
        }
    }

    #[inline]
    unsafe fn initializer(&self) -> Initializer {
        Initializer::zeroing()
    }

    fn read_exact(&mut self, mut buf: &mut [u8]) -> Result<()> {
        while !buf.is_empty() {
            match self.read(buf) {
                Ok(0) => break,
                Ok(n) => {
                    let tmp = buf;
                    buf = &mut tmp[n..];
                }
                Err(ref e) if e.kind() == ErrorKind::Interrupted => {}
                Err(e) => return Err(e),
            }
        }
        if !buf.is_empty() {
            Err(IoError::new(
                ErrorKind::UnexpectedEof,
                "failed to fill whole buffer",
            ))
        } else {
            Ok(())
        }
    }

    fn by_ref(&mut self) -> &mut Self
    where
        Self: Sized,
    {
        self
    }

    fn bytes(self) -> Bytes<Self>
    where
        Self: Sized,
    {
        Bytes { inner: self }
    }

    fn chain<R: Read>(self, next: R) -> Chain<Self, R>
    where
        Self: Sized,
    {
        Chain {
            first: self,
            second: next,
            done_first: false,
        }
    }

  
    fn take(self, limit: u64) -> Take<Self>
    where
        Self: Sized,
    {
        Take { inner: self, limit }
    }
}

#[derive(Debug)]
pub struct Initializer(bool);

#[allow(deprecated)]
impl Initializer {
    /// Returns a new `Initializer` which will zero out buffers.
    #[inline]
    pub fn zeroing() -> Initializer {
        Initializer(true)
    }

    #[inline]
    pub unsafe fn nop() -> Initializer {
        Initializer(false)
    }

    #[inline]
    pub fn should_initialize(&self) -> bool {
        self.0
    }

    #[inline]
    pub fn initialize(&self, buf: &mut [u8]) {
        if self.should_initialize() {
            unsafe { core::ptr::write_bytes(buf.as_mut_ptr(), 0, buf.len()) }
        }
    }
}

pub trait Write {
   
    fn write(&mut self, buf: &[u8]) -> Result<usize>;

    fn flush(&mut self) -> Result<()>;

    fn write_all(&mut self, mut buf: &[u8]) -> Result<()> {
        while !buf.is_empty() {
            match self.write(buf) {
                Ok(0) => {
                    return Err(IoError::new(
                        ErrorKind::WriteZero,
                        "failed to write whole buffer",
                    ));
                }
                Ok(n) => buf = &buf[n..],
                Err(ref e) if e.kind() == ErrorKind::Interrupted => {}
                Err(e) => return Err(e),
            }
        }
        Ok(())
    }
   
    fn write_fmt(&mut self, fmt: fmt::Arguments<'_>) -> Result<()> {
  
        struct Adaptor<'a, T: ?Sized + 'a> {
            inner: &'a mut T,
            error: Result<()>,
        }

        impl<T: Write + ?Sized> fmt::Write for Adaptor<'_, T> {
            fn write_str(&mut self, s: &str) -> fmt::Result {
                match self.inner.write_all(s.as_bytes()) {
                    Ok(()) => Ok(()),
                    Err(e) => {
                        self.error = Err(e);
                        Err(fmt::Error)
                    }
                }
            }
        }

        let mut output = Adaptor {
            inner: self,
            error: Ok(()),
        };
        match fmt::write(&mut output, fmt) {
            Ok(()) => Ok(()),
            Err(..) => {
                // check if the error came from the underlying `Write` or not
                if output.error.is_err() {
                    output.error
                } else {
                    Err(IoError::new(ErrorKind::Other, "formatter error"))
                }
            }
        }
    }

    fn by_ref(&mut self) -> &mut Self
    where
        Self: Sized,
    {
        self
    }
}

pub trait Seek {

    fn seek(&mut self, pos: SeekFrom) -> Result<u64>;
}

#[derive(Copy, PartialEq, Eq, Clone, Debug)]
pub enum SeekFrom {
    Start(u64),
    End(i64),
    Current(i64),
}

#[derive(Debug)]
pub struct Bytes<R> {
    inner: R,
}

impl<R: Read> Iterator for Bytes<R> {
    type Item = Result<u8>;

    fn next(&mut self) -> Option<Result<u8>> {
        let mut byte = 0;
        loop {
            return match self.inner.read(slice::from_mut(&mut byte)) {
                Ok(0) => None,
                Ok(..) => Some(Ok(byte)),
                Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
                Err(e) => Some(Err(e)),
            };
        }
    }
}

pub trait BufRead: Read {
 
    fn fill_buf(&mut self) -> Result<&[u8]>;

    fn consume(&mut self, amt: usize);

    #[cfg(feature = "alloc")]
    fn read_until(&mut self, byte: u8, buf: &mut Vec<u8>) -> Result<usize> {
        read_until(self, byte, buf)
    }

    #[cfg(feature = "alloc")]
    fn read_line(&mut self, buf: &mut alloc::string::String) -> Result<usize> {
        let mut bytes = Vec::new();
        let n = self.read_until(b'\n', &mut bytes)?;
        match core::str::from_utf8(&bytes) {
            Ok(s) => {
                buf.push_str(s);
                Ok(n)
            }
            Err(_) => Err(IoError::new(
                ErrorKind::InvalidData,
                "stream did not contain valid UTF-8",
            )),
        }
    }
}

#[cfg(feature = "alloc")]
fn read_until<R: BufRead + ?Sized>(r: &mut R, delim: u8, buf: &mut Vec<u8>) -> Result<usize> {
    let mut total = 0;
    loop {
        let available = match r.fill_buf() {
            Ok(b) => b,
            Err(ref e) if e.kind() == ErrorKind::Interrupted => continue,
            Err(e) => return Err(e),
        };
        if available.is_empty() {
            return Ok(total);
        }

        let (done, used) = match memchr::memchr(delim, available) {
            Some(i) => {
                buf.extend_from_slice(&available[..=i]);
                (true, i + 1)
            }
            None => {
                buf.extend_from_slice(available);
                (false, available.len())
            }
        };
        r.consume(used);
        total += used;
        if done {
            return Ok(total);
        }
    }
}

pub struct Chain<T, U> {
    first: T,
    second: U,
    done_first: bool,
}

impl<T, U> Chain<T, U> {

    pub fn into_inner(self) -> (T, U) {
        (self.first, self.second)
    }

    pub fn get_ref(&self) -> (&T, &U) {
        (&self.first, &self.second)
    }

    pub fn get_mut(&mut self) -> (&mut T, &mut U) {
        (&mut self.first, &mut self.second)
    }
}

impl<T: fmt::Debug, U: fmt::Debug> fmt::Debug for Chain<T, U> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Chain")
            .field("t", &self.first)
            .field("u", &self.second)
            .finish()
    }
}

impl<T: Read, U: Read> Read for Chain<T, U> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        if !self.done_first {
            match self.first.read(buf)? {
                0 if !buf.is_empty() => self.done_first = true,
                n => return Ok(n),
            }
        }
        self.second.read(buf)
    }

    #[allow(deprecated)]
    unsafe fn initializer(&self) -> Initializer {
        let initializer = self.first.initializer();
        if initializer.should_initialize() {
            initializer
        } else {
            self.second.initializer()
        }
    }
}

impl<T: BufRead, U: BufRead> BufRead for Chain<T, U> {
    fn fill_buf(&mut self) -> Result<&[u8]> {
        if !self.done_first {
            match self.first.fill_buf()? {
                buf if buf.is_empty() => {
                    self.done_first = true;
                }
                buf => return Ok(buf),
            }
        }
        self.second.fill_buf()
    }

    fn consume(&mut self, amt: usize) {
        if !self.done_first {
            self.first.consume(amt)
        } else {
            self.second.consume(amt)
        }
    }
}

#[derive(Debug)]
pub struct Take<T> {
    inner: T,
    limit: u64,
}

impl<T> Take<T> {

    pub fn limit(&self) -> u64 {
        self.limit
    }

    pub fn set_limit(&mut self, limit: u64) {
        self.limit = limit;
    }

    pub fn into_inner(self) -> T {
        self.inner
    }

    pub fn get_ref(&self) -> &T {
        &self.inner
    }

    pub fn get_mut(&mut self) -> &mut T {
        &mut self.inner
    }
}

impl<T: Read> Read for Take<T> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {

        if self.limit == 0 {
            return Ok(0);
        }

        let max = cmp::min(buf.len() as u64, self.limit) as usize;
        let n = self.inner.read(&mut buf[..max])?;
        self.limit -= n as u64;
        Ok(n)
    }

    #[allow(deprecated)]
    unsafe fn initializer(&self) -> Initializer {
        self.inner.initializer()
    }

    #[cfg(feature = "alloc")]
    fn read_to_end(&mut self, buf: &mut Vec<u8>) -> Result<usize> {
        read_to_end_with_reservation(self, buf, |self_| cmp::min(self_.limit, 32) as usize)
    }
}

impl<T: BufRead> BufRead for Take<T> {
    fn fill_buf(&mut self) -> Result<&[u8]> {

        if self.limit == 0 {
            return Ok(&[]);
        }

        let buf = self.inner.fill_buf()?;
        let cap = cmp::min(buf.len() as u64, self.limit) as usize;
        Ok(&buf[..cap])
    }

    fn consume(&mut self, amt: usize) {
        let amt = cmp::min(amt as u64, self.limit) as usize;
        self.limit -= amt as u64;
        self.inner.consume(amt);
    }
}

#[cfg(feature = "std")]
impl crate::io::traits::Read for std::fs::File {
    
    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> std::result::Result<usize, crate::io::IoError> {
        use std::io::Read;
        std::io::Read::read(self, buf).map_err(|e| crate::io::IoError::from(e))
    }
    
    #[inline]
    fn read_exact(&mut self, buf: &mut [u8]) -> std::result::Result<(), crate::io::IoError> {
        use std::io::Read;
        std::io::Read::read_exact(self, buf).map_err(|e| crate::io::IoError::from(e))
    }
}

#[cfg(feature = "std")]
impl crate::io::traits::Seek for std::fs::File {
    fn seek(&mut self, pos: crate::io::SeekFrom) -> std::result::Result<u64, crate::io::IoError> {
        use std::io::Seek;
        let pos = match pos {
            crate::io::SeekFrom::Start(n) => std::io::SeekFrom::Start(n),
            crate::io::SeekFrom::End(n) => std::io::SeekFrom::End(n),
            crate::io::SeekFrom::Current(n) => std::io::SeekFrom::Current(n),
        };
        std::io::Seek::seek(self, pos).map_err(|e| crate::io::IoError::from(e))
    }
}

#[cfg(feature = "std")]
impl<T: AsRef<[u8]>> Read for std::io::Cursor<T> {

    #[inline]
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        use std::io::Read;
        std::io::Read::read(self, buf).map_err(|e| IoError::from(e))
    }

    #[inline]
    fn read_exact(&mut self, buf: &mut [u8]) -> Result<()> {
        use std::io::Read;
        std::io::Read::read_exact(self, buf).map_err(|e| IoError::from(e))
    }
}

#[cfg(feature = "std")]
impl<T: AsRef<[u8]>> Seek for std::io::Cursor<T> {

    #[inline]
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        use std::io::Seek;
        let pos = match pos {
            SeekFrom::Start(n) => std::io::SeekFrom::Start(n),
            SeekFrom::End(n) => std::io::SeekFrom::End(n),
            SeekFrom::Current(n) => std::io::SeekFrom::Current(n),
        };
        std::io::Seek::seek(self, pos).map_err(|e| IoError::from(e))
    }
}