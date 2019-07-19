use std::result;

use crate::common::*;
use crate::symbol::constants::*;
use crate::FallibleIterator;

/// An iterator over binary annotations used by `S_INLINESITE`
pub struct BinaryAnnotationsIter<'t> {
    buffer: &'t [u8],
}

impl<'t> BinaryAnnotationsIter<'t> {
    /// Initializes the iterator by parsing the buffer contents.
    pub fn new(buffer: &'t [u8]) -> BinaryAnnotationsIter<'t> {
        BinaryAnnotationsIter { buffer }
    }

    fn pop_front(&mut self) -> Result<u8> {
        let (first, rest) = self.buffer.split_first().ok_or(Error::UnexpectedEof)?;
        self.buffer = rest;
        Ok(*first)
    }

    fn get_compressed_annotation(&mut self) -> Result<u32> {
        let b1 = u32::from(self.pop_front()?);
        if (b1 & 0x80) == 0 {
            return Ok(b1);
        }
        let b2 = u32::from(self.pop_front()?);
        if (b1 & 0xc0) == 0x80 {
            return Ok((b1 & 0x3f) << 8 | b2);
        }
        let b3 = u32::from(self.pop_front()?);
        let b4 = u32::from(self.pop_front()?);
        if (b1 & 0xe0) == 0xc0 {
            return Ok(((b1 & 0x1f) << 24) | (b2 << 16) | (b3 << 8) | b4);
        }
        Err(Error::SymbolTooShort)
    }
}

fn decode_signed_operand(value: u32) -> i32 {
    if value & 1 != 0 {
        -((value >> 1) as i32)
    } else {
        (value >> 1) as i32
    }
}

impl<'t> FallibleIterator for BinaryAnnotationsIter<'t> {
    type Item = BinaryAnnotation;
    type Error = Error;

    fn next(&mut self) -> result::Result<Option<Self::Item>, Self::Error> {
        if self.buffer.is_empty() {
            return Ok(None);
        }
        Ok(Some(
            match BinaryAnnotationOpcode::from(self.get_compressed_annotation()?) {
                // invalid opcodes mark the end of the stream.
                BinaryAnnotationOpcode::Invalid => {
                    self.buffer = &[][..];
                    return Ok(None);
                }
                BinaryAnnotationOpcode::CodeOffset => {
                    BinaryAnnotation::CodeOffset(self.get_compressed_annotation()?)
                }
                BinaryAnnotationOpcode::ChangeCodeOffsetBase => {
                    BinaryAnnotation::ChangeCodeOffsetBase(self.get_compressed_annotation()?)
                }
                BinaryAnnotationOpcode::ChangeCodeOffset => {
                    BinaryAnnotation::ChangeCodeOffset(self.get_compressed_annotation()?)
                }
                BinaryAnnotationOpcode::ChangeCodeLength => {
                    BinaryAnnotation::ChangeCodeLength(self.get_compressed_annotation()?)
                }
                BinaryAnnotationOpcode::ChangeFile => {
                    BinaryAnnotation::ChangeFile(self.get_compressed_annotation()?)
                }
                BinaryAnnotationOpcode::ChangeLineOffset => BinaryAnnotation::ChangeLineOffset(
                    decode_signed_operand(self.get_compressed_annotation()?),
                ),
                BinaryAnnotationOpcode::ChangeLineEndDelta => {
                    BinaryAnnotation::ChangeLineEndDelta(self.get_compressed_annotation()?)
                }
                BinaryAnnotationOpcode::ChangeRangeKind => {
                    BinaryAnnotation::ChangeRangeKind(self.get_compressed_annotation()?)
                }
                BinaryAnnotationOpcode::ChangeColumnStart => {
                    BinaryAnnotation::ChangeColumnStart(self.get_compressed_annotation()?)
                }
                BinaryAnnotationOpcode::ChangeColumnEndDelta => {
                    BinaryAnnotation::ChangeColumnEndDelta(decode_signed_operand(
                        self.get_compressed_annotation()?,
                    ))
                }
                BinaryAnnotationOpcode::ChangeCodeOffsetAndLineOffset => {
                    let annotation = self.get_compressed_annotation()?;
                    BinaryAnnotation::ChangeCodeOffsetAndLineOffset(
                        decode_signed_operand(annotation >> 4),
                        decode_signed_operand(annotation & 0xf),
                    )
                }
                BinaryAnnotationOpcode::ChangeCodeLengthAndCodeOffset => {
                    BinaryAnnotation::ChangeCodeLengthAndCodeOffset(
                        self.get_compressed_annotation()?,
                        self.get_compressed_annotation()?,
                    )
                }
                BinaryAnnotationOpcode::ChangeColumnEnd => {
                    BinaryAnnotation::ChangeColumnEnd(self.get_compressed_annotation()?)
                }
            },
        ))
    }
}

/// Represents a parsed `BinaryAnnotation`.
///
/// Binary annotations are used by `S_INLINESITE` to encode opcodes for how to
/// evaluate the state changes for inline information.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum BinaryAnnotation {
    CodeOffset(u32),
    ChangeCodeOffsetBase(u32),
    ChangeCodeOffset(u32),
    ChangeCodeLength(u32),
    ChangeFile(u32),
    ChangeLineOffset(i32),
    ChangeLineEndDelta(u32),
    ChangeRangeKind(u32),
    ChangeColumnStart(u32),
    ChangeColumnEndDelta(i32),
    ChangeCodeOffsetAndLineOffset(i32, i32),
    ChangeCodeLengthAndCodeOffset(u32, u32),
    ChangeColumnEnd(u32),
}

#[test]
fn test_binary_annotation_iter() {
    let inp = b"\x0b\x03\x06\n\x03\x08\x06\x06\x03-\x06\x08\x03\x07\x0br\x06\x06\x0c\x03\x07\x06\x0f\x0c\x06\x05\x00\x00";
    let items = BinaryAnnotationsIter::new(inp).collect::<Vec<_>>().unwrap();
    assert_eq!(
        items,
        vec![
            BinaryAnnotation::ChangeCodeOffsetAndLineOffset(0, -1),
            BinaryAnnotation::ChangeLineOffset(5),
            BinaryAnnotation::ChangeCodeOffset(8),
            BinaryAnnotation::ChangeLineOffset(3),
            BinaryAnnotation::ChangeCodeOffset(45),
            BinaryAnnotation::ChangeLineOffset(4),
            BinaryAnnotation::ChangeCodeOffset(7),
            BinaryAnnotation::ChangeCodeOffsetAndLineOffset(-3, 1),
            BinaryAnnotation::ChangeLineOffset(3),
            BinaryAnnotation::ChangeCodeLengthAndCodeOffset(3, 7),
            BinaryAnnotation::ChangeLineOffset(-7),
            BinaryAnnotation::ChangeCodeLengthAndCodeOffset(6, 5)
        ]
    );

    let inp = b"\x03P\x06\x0e\x03\x0c\x06\x04\x032\x06\x06\x03T\x0b#\x0b\\\x0bC\x0b/\x06\x04\x0c-\t\x03;\x06\x1d\x0c\x05\x06\x00\x00";
    let items = BinaryAnnotationsIter::new(inp).collect::<Vec<_>>().unwrap();
    assert_eq!(
        items,
        vec![
            BinaryAnnotation::ChangeCodeOffset(80),
            BinaryAnnotation::ChangeLineOffset(7),
            BinaryAnnotation::ChangeCodeOffset(12),
            BinaryAnnotation::ChangeLineOffset(2),
            BinaryAnnotation::ChangeCodeOffset(50),
            BinaryAnnotation::ChangeLineOffset(3),
            BinaryAnnotation::ChangeCodeOffset(84),
            BinaryAnnotation::ChangeCodeOffsetAndLineOffset(1, -1),
            BinaryAnnotation::ChangeCodeOffsetAndLineOffset(-2, 6),
            BinaryAnnotation::ChangeCodeOffsetAndLineOffset(2, -1),
            BinaryAnnotation::ChangeCodeOffsetAndLineOffset(1, -7),
            BinaryAnnotation::ChangeLineOffset(2),
            BinaryAnnotation::ChangeCodeLengthAndCodeOffset(45, 9),
            BinaryAnnotation::ChangeCodeOffset(59),
            BinaryAnnotation::ChangeLineOffset(-14),
            BinaryAnnotation::ChangeCodeLengthAndCodeOffset(5, 6),
        ]
    );
}
