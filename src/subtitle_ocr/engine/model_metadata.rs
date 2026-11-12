//! Byte-preserving edits to model-level ONNX metadata only.
//!
//! The caller must supply hash-audited, self-contained model bytes and preflight
//! the actual output tensor width before using the result. This helper neither
//! audits a model nor checks its tensors or external data.

use anyhow::{anyhow, bail, Result};

const MAX_MODEL_BYTES: usize = 32 * 1024 * 1024;
const MAX_GROUP_DEPTH: usize = 100;
const METADATA_PROPS: u32 = 14;

#[derive(Clone, Copy)]
struct Field {
    number: u32,
    wire: u8,
    // For length-delimited fields, these identify the length and payload.
    length_start: usize,
    data_start: usize,
    data_end: usize,
}

fn varint(bytes: &[u8], pos: &mut usize) -> Result<u64> {
    let mut value = 0u64;
    for i in 0..10 {
        let byte = *bytes.get(*pos).ok_or_else(|| anyhow!("truncated varint"))?;
        *pos += 1;
        if i == 9 && byte > 1 {
            bail!("varint overflow");
        }
        value |= u64::from(byte & 0x7f) << (7 * i);
        if byte & 0x80 == 0 {
            return Ok(value);
        }
    }
    bail!("varint overflow")
}

fn tag(bytes: &[u8], pos: &mut usize) -> Result<(u32, u8)> {
    let raw = varint(bytes, pos)?;
    let number = raw >> 3;
    if number == 0 || number > (1 << 29) - 1 {
        bail!("invalid field number");
    }
    let wire = (raw & 7) as u8;
    if wire > 5 {
        bail!("invalid wire type");
    }
    Ok((number as u32, wire))
}

fn advance(bytes: &[u8], pos: &mut usize, len: usize) -> Result<()> {
    *pos = pos
        .checked_add(len)
        .filter(|end| *end <= bytes.len())
        .ok_or_else(|| anyhow!("truncated field"))?;
    Ok(())
}

// Reads one field, including an entire group. A fixed stack bounds group nesting.
fn field(bytes: &[u8], pos: &mut usize) -> Result<Field> {
    let (number, wire) = tag(bytes, pos)?;
    if wire == 4 {
        bail!("unexpected end group");
    }
    let mut result = Field {
        number,
        wire,
        length_start: *pos,
        data_start: *pos,
        data_end: *pos,
    };
    if wire == 3 {
        let mut groups = [0u32; MAX_GROUP_DEPTH];
        groups[0] = number;
        let mut depth = 1usize;
        while depth != 0 {
            let (nested_number, nested_wire) = tag(bytes, pos)?;
            match nested_wire {
                3 => {
                    if depth == MAX_GROUP_DEPTH {
                        bail!("group nesting exceeds limit");
                    }
                    groups[depth] = nested_number;
                    depth += 1;
                }
                4 => {
                    if groups[depth - 1] != nested_number {
                        bail!("mismatched end group");
                    }
                    depth -= 1;
                }
                _ => skip_value(bytes, pos, nested_wire)?,
            }
        }
    } else if wire == 2 {
        let length =
            usize::try_from(varint(bytes, pos)?).map_err(|_| anyhow!("length overflow"))?;
        result.data_start = *pos;
        advance(bytes, pos, length)?;
        result.data_end = *pos;
    } else {
        skip_value(bytes, pos, wire)?;
    }
    Ok(result)
}

fn skip_value(bytes: &[u8], pos: &mut usize, wire: u8) -> Result<()> {
    match wire {
        0 => {
            varint(bytes, pos)?;
        }
        1 => advance(bytes, pos, 8)?,
        2 => {
            let length =
                usize::try_from(varint(bytes, pos)?).map_err(|_| anyhow!("length overflow"))?;
            advance(bytes, pos, length)?;
        }
        5 => advance(bytes, pos, 4)?,
        _ => bail!("invalid wire type"),
    }
    Ok(())
}

// Keep the original varint width, including any noncanonical padding.
fn write_length(bytes: &mut [u8], start: usize, end: usize, mut value: usize) {
    let width = end - start;
    for (i, byte) in bytes[start..end].iter_mut().enumerate() {
        *byte = (value as u8 & 0x7f) | if i + 1 < width { 0x80 } else { 0 };
        value >>= 7;
    }
}

/// Remove exactly one terminal LF from the unique model-level `character` value.
/// Missing or ambiguous metadata is rejected; a value without LF is unchanged.
pub(super) fn strip_character_metadata_lf(bytes: &[u8]) -> Result<Vec<u8>> {
    if bytes.len() > MAX_MODEL_BYTES {
        bail!("model exceeds size limit");
    }
    let mut pos = 0;
    let mut found = false;
    let mut edit = None;
    while pos < bytes.len() {
        let outer = field(bytes, &mut pos)?;
        if outer.number != METADATA_PROPS {
            continue;
        }
        if outer.wire != 2 {
            bail!("invalid metadata entry wire type");
        }
        let entry = &bytes[outer.data_start..outer.data_end];
        let mut entry_pos = 0;
        let mut key = None;
        let mut value = None;
        while entry_pos < entry.len() {
            let inner = field(entry, &mut entry_pos)?;
            if inner.number != 1 && inner.number != 2 {
                continue;
            }
            if inner.wire != 2 {
                bail!("invalid metadata string wire type");
            }
            let slot = if inner.number == 1 {
                &mut key
            } else {
                &mut value
            };
            if slot.replace(inner).is_some() {
                bail!("ambiguous metadata entry");
            }
            std::str::from_utf8(&entry[inner.data_start..inner.data_end])?;
        }
        if let Some(key) = key {
            if &entry[key.data_start..key.data_end] == b"character" {
                if found {
                    bail!("duplicate character metadata");
                }
                found = true;
                let value = value.ok_or_else(|| anyhow!("character metadata has no value"))?;
                if entry[value.data_start..value.data_end].last() == Some(&b'\n') {
                    edit = Some((outer, value));
                }
            }
        }
    }
    if !found {
        bail!("missing character metadata");
    }
    let mut output = Vec::new();
    output.try_reserve_exact(bytes.len())?;
    output.extend_from_slice(bytes);
    if let Some((outer, value)) = edit {
        write_length(
            &mut output,
            outer.length_start,
            outer.data_start,
            outer.data_end - outer.data_start - 1,
        );
        write_length(
            &mut output,
            outer.data_start + value.length_start,
            outer.data_start + value.data_start,
            value.data_end - value.data_start - 1,
        );
        output.remove(outer.data_start + value.data_end - 1);
    }
    Ok(output)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn enc(mut n: usize) -> Vec<u8> {
        let mut out = Vec::new();
        loop {
            let b = (n & 127) as u8;
            n >>= 7;
            out.push(b | if n != 0 { 128 } else { 0 });
            if n == 0 {
                return out;
            }
        }
    }

    fn bytes_field(number: usize, value: &[u8]) -> Vec<u8> {
        let mut out = enc(number * 8 + 2);
        out.extend(enc(value.len()));
        out.extend(value);
        out
    }

    fn metadata(key: &[u8], value: &[u8]) -> Vec<u8> {
        let mut entry = bytes_field(1, key);
        entry.extend(bytes_field(2, value));
        bytes_field(14, &entry)
    }

    #[test]
    fn preserves_everything_except_terminal_lf_and_lengths() {
        let prefix = [
            0x08, 0x81, 0x00, 0x11, 1, 2, 3, 4, 5, 6, 7, 8, 0x1d, 1, 2, 3, 4, 0x23, 0x2b, 0x30, 1,
            0x2c, 0x24,
        ];
        let other = metadata(b"other", b"unchanged\n");
        let input = [
            prefix.as_slice(),
            &other,
            &metadata(b"character", "λ\n\n \n\n".as_bytes()),
            &other,
        ]
        .concat();
        let expected = [
            prefix.as_slice(),
            &other,
            &metadata(b"character", "λ\n\n \n".as_bytes()),
            &other,
        ]
        .concat();
        assert_eq!(strip_character_metadata_lf(&input).unwrap(), expected);
        assert_eq!(
            strip_character_metadata_lf(&expected).unwrap(),
            [
                prefix.as_slice(),
                &other,
                &metadata(b"character", "λ\n\n ".as_bytes()),
                &other
            ]
            .concat()
        );
    }

    #[test]
    fn preserves_unknown_entry_fields_and_ignores_nested_metadata() {
        let nested = metadata(b"character", b"nested\n");
        let mut group = vec![0x23];
        group.extend(&nested);
        group.push(0x24);
        let mut before = bytes_field(3, b"unknown\n");
        before.extend(bytes_field(2, "λ\n".as_bytes()));
        before.extend([0x20, 0x81, 0x00]);
        before.extend(bytes_field(1, b"character"));
        let mut after = bytes_field(3, b"unknown\n");
        after.extend(bytes_field(2, "λ".as_bytes()));
        after.extend([0x20, 0x81, 0x00]);
        after.extend(bytes_field(1, b"character"));
        let input = [group.as_slice(), &bytes_field(14, &before)].concat();
        let expected = [group.as_slice(), &bytes_field(14, &after)].concat();
        assert_eq!(strip_character_metadata_lf(&input).unwrap(), expected);
    }

    #[test]
    fn unchanged_values_and_noncanonical_lengths() {
        for value in [b"".as_slice(), b"hello", "λ\n ".as_bytes()] {
            let input = metadata(b"character", value);
            assert_eq!(strip_character_metadata_lf(&input).unwrap(), input);
        }
        // Both length varints are intentionally noncanonical and retain their widths.
        let input = b"\x72\x8f\x00\x0a\x09character\x12\x81\x00\n";
        let expected = b"\x72\x8e\x00\x0a\x09character\x12\x80\x00";
        assert_eq!(strip_character_metadata_lf(input).unwrap(), expected);
    }

    #[test]
    fn length_boundary_is_preserved_without_reencoding_other_fields() {
        let mut entry = bytes_field(1, b"character");
        entry.extend([0x12, 0x80, 0x01]);
        entry.extend([b'a'; 127]);
        entry.push(b'\n');
        let input = bytes_field(14, &entry);
        let mut expected = input.clone();
        expected[1] -= 1;
        let value_length = 3 + 2 + 9 + 1;
        expected[value_length] = 0xff;
        expected[value_length + 1] = 0;
        expected.pop();
        assert_eq!(strip_character_metadata_lf(&input).unwrap(), expected);
    }

    #[test]
    fn rejects_missing_duplicate_and_ambiguous_entries() {
        let character = metadata(b"character", b"a\n");
        assert!(strip_character_metadata_lf(&metadata(b"other", b"x")).is_err());
        assert!(
            strip_character_metadata_lf(&[character.clone(), character.clone()].concat()).is_err()
        );
        let mut entry = bytes_field(1, b"character");
        entry.extend(bytes_field(2, b"a\n"));
        entry.extend(bytes_field(2, b"b\n"));
        assert!(strip_character_metadata_lf(&bytes_field(14, &entry)).is_err());
        assert!(
            strip_character_metadata_lf(&bytes_field(14, &bytes_field(1, b"character"))).is_err()
        );
        let mut input = character;
        input.extend([0x80]);
        assert!(strip_character_metadata_lf(&input).is_err());
    }

    #[test]
    fn rejects_malformed_wire_values_and_truncation() {
        let valid = metadata(b"character", b"x\n");
        let bad: &[&[u8]] = &[
            &[0],
            &[0x0e],
            &[0x0f],
            &[0x0c],
            &[0x0b],
            &[0x0b, 0x14],
            &[0x08, 0x80],
            &[0x09, 0, 0],
            &[0x0d, 0],
            &[0x0a, 2, 1],
            &[
                0x0a, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 2,
            ],
            &[
                0x08, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 2,
            ],
            &[0x70, 0],
            &[0x72, 2, 0x08, 0],
            &[0x72, 2, 0x0a, 2],
            &[0x72, 3, 0x0a, 1, 0xff],
        ];
        for suffix in bad {
            assert!(
                strip_character_metadata_lf(&[valid.as_slice(), suffix].concat()).is_err(),
                "{suffix:?}"
            );
        }
        for n in 0..valid.len() {
            assert!(
                strip_character_metadata_lf(&valid[..n]).is_err(),
                "prefix {n}"
            );
        }
    }

    #[test]
    fn limits_model_size_and_group_depth() {
        assert!(strip_character_metadata_lf(&vec![0; MAX_MODEL_BYTES + 1]).is_err());
        let valid = metadata(b"character", b"a");
        let mut groups = vec![0x0b; MAX_GROUP_DEPTH];
        groups.extend(vec![0x0c; MAX_GROUP_DEPTH]);
        assert!(strip_character_metadata_lf(&[valid.as_slice(), &groups].concat()).is_ok());
        groups.insert(0, 0x0b);
        groups.push(0x0c);
        assert!(strip_character_metadata_lf(&[valid.as_slice(), &groups].concat()).is_err());
    }
}
