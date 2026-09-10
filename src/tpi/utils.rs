use crate::{*, tpi::constants::*};

#[inline]
pub fn parse_optional_type_index(buf: &mut ParseBuffer<'_>) -> Result<Option<TypeIndex>> {
    let index = buf.parse()?;
    if index == TypeIndex(0) || index == TypeIndex(0xffff) {
        Ok(None)
    } else {
        Ok(Some(index))
    }
}

#[inline]
pub fn parse_string<'t>(leaf: u16, buf: &mut ParseBuffer<'t>) -> Result<RawString<'t>> {
    if leaf > LF_ST_MAX {
        buf.parse_cstring()
    } else {
        buf.parse_u8_pascal_string()
    }
}

#[inline]
pub fn parse_padding(buf: &mut ParseBuffer<'_>) -> Result<()> {
    while !buf.is_empty() && buf.peek_u8()? >= 0xf0 {
        let padding = buf.parse_u8()?;
        if padding > 0xf0 {
            // low four bits indicate amount of padding
            // (don't ask me what 0xf0 means, then)
            buf.take((padding & 0x0f) as usize - 1)?;
        }
    }
    Ok(())
}


// https://github.com/Microsoft/microsoft-pdb/blob/082c5290e5aff028ae84e43affa8be717aa7af73/pdbdump/pdbdump.cpp#L2417-L2456
pub fn parse_unsigned(buf: &mut ParseBuffer<'_>) -> Result<u64> {
    let leaf = buf.parse_u16()?;
    if leaf < LF_NUMERIC {
        // the u16 directly encodes a value
        return Ok(u64::from(leaf));
    }

    match leaf {
        LF_CHAR => Ok(u64::from(buf.parse_u8()?)),
        LF_USHORT => Ok(u64::from(buf.parse_u16()?)),
        LF_ULONG => Ok(u64::from(buf.parse_u32()?)),
        LF_UQUADWORD => Ok(buf.parse_u64()?),
        _ => {
            if cfg!(debug_assertions) {
                unreachable!();
            } else {
                Err(Error::UnexpectedNumericPrefix(leaf))
            }
        }
    }
}
