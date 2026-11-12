//! Whole-line recognition for audited, self-contained models.
//!
//! The allowlist is pinned to bytes audited for external ONNX data, not to
//! filenames or model overrides.

use anyhow::{anyhow, bail, Context, Result};
use image::RgbImage;
use ort::{inputs, session::Session, tensor::TensorElementType, value::Tensor};
use paddle_ocr_rs::ocr_utils::OcrUtils;
use sha2::{Digest, Sha256};
use std::{
    collections::{BTreeMap, BTreeSet},
    ffi::OsStr,
    fs::OpenOptions,
    io::Read,
    path::Path,
};

use super::runtime::configure_ort_builder;

// Independently pinned: updating model provisioning must not approve a new model
// without repeating the self-contained ONNX schema and external-data audit.
const AUDITED_SHA256: [&str; 3] = [
    "ef7abd8bd3629ae57ea2c28b425c1bd258a871b93fd2fe7c433946ade9b5d9ea",
    "e8770c967605983d1570cdf5352041dfb68fa0c21664f49f47b155abd3e0e318",
    "640e323ac1a4da562eddd2a3963b4e1336686f898113983122d76d24dc3afcfe",
];
const MAX_MODEL_BYTES: u64 = 32 * 1024 * 1024;
const MAX_METADATA_BYTES: usize = 64 * 1024;
const MAX_CLASSES: usize = 4096;
const MAX_TIMESTEPS: usize = 4096;
const MAX_OUTPUT_ELEMENTS: usize = 4 * 1024 * 1024;
const MAX_TEXT_BYTES: usize = 1024 * 1024;
const MAX_CROP_SIDE: u32 = 4096;
const MAX_CROP_PIXELS: u32 = 4 * 1024 * 1024;
const MAX_RESIZED_WIDTH: u32 = 4096;
const VERTICAL_PADDING: u32 = 4;
const MAX_GUARD_CHARS: usize = 512;
const MAX_GUARD_BOUNDARIES: usize = 64;
const HEIGHT: u32 = 48;
const MEAN: [f32; 3] = [127.5; 3];
const NORM: [f32; 3] = [1.0 / 127.5; 3];

#[derive(Debug, PartialEq)]
pub(in crate::subtitle_ocr) struct RecognizedLine {
    pub(in crate::subtitle_ocr) text: String,
    pub(in crate::subtitle_ocr) score: Option<f32>,
}

#[derive(Debug, PartialEq)]
pub(in crate::subtitle_ocr) struct LineRecognition {
    pub(in crate::subtitle_ocr) original: RecognizedLine,
    pub(in crate::subtitle_ocr) corrected: RecognizedLine,
    pub(in crate::subtitle_ocr) class_width: usize,
}

struct Dictionaries {
    original: Vec<String>,
    corrected: Vec<String>,
}

impl Dictionaries {
    fn from_metadata(character: &str) -> Result<Self> {
        if character.len() > MAX_METADATA_BYTES {
            bail!("recognizer character metadata exceeds limit");
        }
        fn keys(raw: &str) -> Result<Vec<String>> {
            let count = raw
                .split('\n')
                .count()
                .checked_add(2)
                .ok_or_else(|| anyhow!("recognizer dictionary size overflow"))?;
            if count > MAX_CLASSES + 1 {
                bail!("recognizer dictionary exceeds class limit");
            }
            let mut keys = Vec::with_capacity(count);
            keys.push("#".to_owned());
            keys.extend(raw.split('\n').map(str::to_owned));
            keys.push(" ".to_owned());
            Ok(keys)
        }
        Ok(Self {
            original: keys(character)?,
            corrected: keys(character.strip_suffix('\n').unwrap_or(character))?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::subtitle_ocr) enum ExperimentalLineMapping {
    Original,
    Corrected,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::subtitle_ocr) enum BitmapLinePolicy {
    Auto,
    Off,
    Explicit(ExperimentalLineMapping),
}

fn parse_bitmap_line_policy(value: Option<&OsStr>) -> Result<BitmapLinePolicy> {
    let Some(value) = value else {
        return Ok(BitmapLinePolicy::Auto);
    };
    let value = value
        .to_str()
        .ok_or_else(|| anyhow!("DPN_OCR_EXPERIMENTAL_BITMAP_LINES must be UTF-8"))?;
    match value.trim().to_ascii_lowercase().as_str() {
        "" | "0" | "off" | "false" => Ok(BitmapLinePolicy::Off),
        "auto" => Ok(BitmapLinePolicy::Auto),
        "original" => Ok(BitmapLinePolicy::Explicit(
            ExperimentalLineMapping::Original,
        )),
        "corrected" => Ok(BitmapLinePolicy::Explicit(
            ExperimentalLineMapping::Corrected,
        )),
        _ => bail!("DPN_OCR_EXPERIMENTAL_BITMAP_LINES must be auto, original, corrected, or off"),
    }
}

pub(in crate::subtitle_ocr) fn bitmap_line_policy_from_env() -> Result<BitmapLinePolicy> {
    parse_bitmap_line_policy(std::env::var_os("DPN_OCR_EXPERIMENTAL_BITMAP_LINES").as_deref())
}

/// Lazily initializes only on a supported bitmap line, once per worker.
pub(in crate::subtitle_ocr) struct ExperimentalLineRecognizer {
    audited_model: Vec<u8>,
    mapping: ExperimentalLineMapping,
    recognizer: Option<WholeLineRecognizer>,
}

impl ExperimentalLineRecognizer {
    pub(in crate::subtitle_ocr) fn new(
        audited_model: Vec<u8>,
        mapping: ExperimentalLineMapping,
    ) -> Self {
        Self {
            audited_model,
            mapping,
            recognizer: None,
        }
    }

    pub(in crate::subtitle_ocr) fn recognize(&mut self, crop: &RgbImage) -> Result<RecognizedLine> {
        self.recognize_with_spacing_probe(crop, false)
    }

    pub(in crate::subtitle_ocr) fn recognize_with_spacing_probe(
        &mut self,
        crop: &RgbImage,
        allow_probe: bool,
    ) -> Result<RecognizedLine> {
        if self.recognizer.is_none() {
            self.recognizer = Some(WholeLineRecognizer::from_audited_bytes(
                &self.audited_model,
            )?);
        }
        let recognizer = self.recognizer.as_mut().expect("recognizer initialized");
        let result = if allow_probe && self.mapping == ExperimentalLineMapping::Corrected {
            recognizer.recognize_with_spacing_probe(crop)?
        } else {
            recognizer.recognize(crop)?
        };
        log::trace!(
            "Experimental bitmap-line OCR class width: {}",
            result.class_width
        );
        Ok(match self.mapping {
            ExperimentalLineMapping::Original => result.original,
            ExperimentalLineMapping::Corrected => result.corrected,
        })
    }
}

/// Own one instance per OCR worker; the supplied crop must already be oriented.
pub(in crate::subtitle_ocr) struct WholeLineRecognizer {
    session: Session,
    input_name: String,
    dictionaries: Dictionaries,
}

impl WholeLineRecognizer {
    #[cfg(test)]
    fn new(model_path: &Path) -> Result<Self> {
        Self::from_audited_bytes(&read_audited_model(model_path)?)
    }

    pub(super) fn preflight_audited_model(model: &[u8]) -> Result<Self> {
        let mut recognizer = Self::from_audited_bytes(model)?;
        let output_type = &recognizer.session.outputs[0].output_type;
        let shape = output_type
            .tensor_shape()
            .ok_or_else(|| anyhow!("recognizer output must be a tensor"))?;
        if output_type.tensor_type() != Some(TensorElementType::Float32)
            || shape.len() != 3
            || shape[2] != recognizer.dictionaries.corrected.len() as i64
        {
            bail!("recognizer declared class width does not match corrected dictionary");
        }
        // Check an actual output too; the fixed declared class axis constrains
        // subsequent detector crops, while decode_output validates this tensor.
        let probe = RgbImage::from_pixel(48, 48, image::Rgb([255, 255, 255]));
        recognizer.recognize(&probe)?;
        Ok(recognizer)
    }

    fn from_audited_bytes(model: &[u8]) -> Result<Self> {
        let builder = configure_ort_builder(Session::builder()?)?;
        let session = builder.commit_from_memory(model)?;
        if session.inputs.len() != 1 || session.outputs.len() != 1 {
            bail!("recognizer must have exactly one input and one output");
        }
        let input_name = session.inputs[0].name.clone();
        let character = session
            .metadata()?
            .custom("character")?
            .ok_or_else(|| anyhow!("recognizer has no character metadata"))?;
        let dictionaries = Dictionaries::from_metadata(&character)?;
        Ok(Self {
            session,
            input_name,
            dictionaries,
        })
    }

    /// Runs normal recognition with both dictionaries against the same scores.
    pub(in crate::subtitle_ocr) fn recognize(
        &mut self,
        crop: &RgbImage,
    ) -> Result<LineRecognition> {
        Ok(self.recognize_once(crop, false)?.0)
    }

    fn recognize_with_spacing_probe(&mut self, crop: &RgbImage) -> Result<LineRecognition> {
        let normal = self.recognize(crop)?;
        Ok(apply_padded_spacing_probe(normal, crop, |padded| {
            let (result, alignments) = self.recognize_once(padded, true)?;
            Ok((result, alignments.context("alignment requested")?))
        }))
    }

    fn recognize_once(
        &mut self,
        crop: &RgbImage,
        with_alignments: bool,
    ) -> Result<(LineRecognition, Option<SpaceAlignments>)> {
        let width = resized_width(crop.width(), crop.height())?;
        let resized =
            image::imageops::resize(crop, width, HEIGHT, image::imageops::FilterType::Triangle);
        let array = OcrUtils::substract_mean_normalize(&resized, &MEAN, &NORM);
        let tensor = Tensor::from_array(array)?;
        let outputs = self
            .session
            .run(inputs![self.input_name.clone() => tensor])?;
        let (_, output) = outputs
            .iter()
            .next()
            .ok_or_else(|| anyhow!("recognizer produced no output"))?;
        let (shape, data) = output.try_extract_tensor::<f32>()?;
        let result = decode_output(shape, data, &self.dictionaries)?;
        let alignments = if with_alignments {
            Some(ctc_space_alignments(shape, data, &self.dictionaries)?)
        } else {
            None
        };
        Ok((result, alignments))
    }
}

pub(super) fn read_bounded_model(path: &Path) -> Result<Vec<u8>> {
    let mut options = OpenOptions::new();
    options.read(true);
    // Opening an attacker-controlled FIFO must not hang a worker on Unix.
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.custom_flags(libc::O_NONBLOCK);
    }
    let file = options
        .open(path)
        .context("opening experimental recognizer model")?;
    let metadata = file.metadata()?;
    if !metadata.is_file() {
        bail!("experimental recognizer model must be a regular file");
    }
    if metadata.len() > MAX_MODEL_BYTES {
        bail!("experimental recognizer model exceeds size limit");
    }
    let mut bytes = Vec::new();
    file.take(MAX_MODEL_BYTES + 1).read_to_end(&mut bytes)?;
    if bytes.len() as u64 > MAX_MODEL_BYTES {
        bail!("experimental recognizer model exceeds size limit");
    }
    Ok(bytes)
}

pub(super) fn read_audited_model(path: &Path) -> Result<Vec<u8>> {
    let bytes = read_bounded_model(path)?;
    let digest = format!("{:x}", Sha256::digest(&bytes));
    if !AUDITED_SHA256.contains(&digest.as_str()) {
        bail!("experimental recognizer model is not in the audited allowlist");
    }
    Ok(bytes)
}

pub(in crate::subtitle_ocr) fn supports_crop_dimensions(width: u32, height: u32) -> bool {
    resized_width(width, height).is_ok()
}

fn padded_crop_dimensions(width: u32, height: u32) -> Result<(u32, u32)> {
    resized_width(width, height)?;
    let padded_height = height
        .checked_add(VERTICAL_PADDING * 2)
        .ok_or_else(|| anyhow!("whole-line padded height overflow"))?;
    Ok((padded_height, resized_width(width, padded_height)?))
}

fn pad_crop(crop: &RgbImage) -> Result<(RgbImage, u32)> {
    let (padded_height, resized_width) = padded_crop_dimensions(crop.width(), crop.height())?;
    let mut padded = RgbImage::from_pixel(crop.width(), padded_height, image::Rgb([255; 3]));
    image::imageops::replace(&mut padded, crop, 0, i64::from(VERTICAL_PADDING));
    Ok((padded, resized_width))
}

fn resized_width(width: u32, height: u32) -> Result<u32> {
    if width == 0
        || height == 0
        || width > MAX_CROP_SIDE
        || height > MAX_CROP_SIDE
        || width
            .checked_mul(height)
            .is_none_or(|n| n > MAX_CROP_PIXELS)
    {
        bail!("whole-line crop dimensions exceed limits");
    }
    // Match Paddle's preprocessing, including its truncation of fractional widths.
    let dst_width = (width as f32 * (HEIGHT as f32 / height as f32)) as u32;
    if dst_width == 0 || dst_width > MAX_RESIZED_WIDTH {
        bail!("whole-line resized width exceeds limits");
    }
    Ok(dst_width)
}

#[derive(Debug, Default)]
struct SpaceAlignments {
    corrected: BTreeMap<usize, usize>,
    timesteps: usize,
}

fn ctc_space_alignments(
    shape: &[i64],
    data: &[f32],
    dictionaries: &Dictionaries,
) -> Result<SpaceAlignments> {
    if shape.len() != 3 || shape[0] != 1 {
        bail!("recognizer output must have shape [1, time, classes]");
    }
    let time = usize::try_from(shape[1]).context("invalid recognizer time dimension")?;
    let width = usize::try_from(shape[2]).context("invalid recognizer class dimension")?;
    if time == 0
        || time > MAX_TIMESTEPS
        || width == 0
        || width > MAX_CLASSES
        || width != dictionaries.corrected.len()
        || time
            .checked_mul(width)
            .is_none_or(|n| n > MAX_OUTPUT_ELEMENTS || n != data.len())
    {
        bail!("recognizer alignment dimensions are invalid");
    }
    let mut runs = Vec::new();
    let mut previous = 0usize;
    let mut peak_step = 0usize;
    let mut peak_score = f32::MIN;
    for (step, row) in data.chunks_exact(width).enumerate() {
        let mut best_index = 0usize;
        let mut best_score = f32::MIN;
        for (index, &score) in row.iter().enumerate() {
            if !score.is_finite() {
                bail!("recognizer output contains a non-finite score");
            }
            if score > best_score {
                best_index = index;
                best_score = score;
            }
        }
        if best_index != previous {
            if previous != 0 {
                runs.push((previous, peak_step));
            }
            previous = best_index;
            peak_step = step;
            peak_score = best_score;
        } else if best_index != 0 && best_score > peak_score {
            peak_step = step;
            peak_score = best_score;
        }
    }
    if previous != 0 {
        runs.push((previous, peak_step));
    }
    let mut positions = BTreeMap::new();
    let mut glyph_index = 0usize;
    for (index, step) in runs {
        for ch in dictionaries.corrected[index].chars() {
            if ch == ' ' {
                positions.insert(glyph_index, step);
            } else if !ch.is_whitespace() {
                glyph_index += 1;
            }
        }
    }
    Ok(SpaceAlignments {
        corrected: positions,
        timesteps: time,
    })
}

fn guard_text_supported(text: &str) -> bool {
    let mut count = 0usize;
    for ch in text.chars() {
        if ch.is_whitespace() {
            if ch != ' ' {
                return false;
            }
        } else {
            count += 1;
            if count > MAX_GUARD_CHARS {
                return false;
            }
        }
    }
    count >= 2 && !text.contains([':', '-', '–', '—'])
}

fn spacing_layout(text: &str) -> Option<(Vec<char>, BTreeSet<usize>)> {
    if !guard_text_supported(text) {
        return None;
    }
    let mut glyphs = Vec::new();
    let mut boundaries = BTreeSet::new();
    for ch in text.chars() {
        if ch == ' ' {
            boundaries.insert(glyphs.len());
            if boundaries.len() > MAX_GUARD_BOUNDARIES {
                return None;
            }
        } else {
            glyphs.push(ch);
        }
    }
    Some((glyphs, boundaries))
}

fn apply_padded_spacing_probe(
    mut normal: LineRecognition,
    crop: &RgbImage,
    probe: impl FnOnce(&RgbImage) -> Result<(LineRecognition, SpaceAlignments)>,
) -> LineRecognition {
    if spacing_layout(&normal.corrected.text).is_none() {
        return normal;
    }
    let Ok((padded, _)) = pad_crop(crop) else {
        return normal;
    };
    let (alternative, alignments) = match probe(&padded) {
        Ok(result) => result,
        Err(err) => {
            log::debug!("Padded whole-line spacing probe abstained: {err:#}");
            return normal;
        }
    };
    normal.corrected.text = merge_padded_spacing(
        &normal.corrected.text,
        &alternative.corrected.text,
        &alignments.corrected,
        alignments.timesteps,
        crop,
    );
    normal
}

fn merge_padded_spacing(
    normal: &str,
    padded: &str,
    space_steps: &BTreeMap<usize, usize>,
    timesteps: usize,
    crop: &RgbImage,
) -> String {
    let Some((glyphs, original_boundaries)) = spacing_layout(normal) else {
        return normal.to_owned();
    };
    let Some((alternate_glyphs, alternate_boundaries)) = spacing_layout(padded) else {
        return normal.to_owned();
    };
    if glyphs != alternate_glyphs
        || !original_boundaries.is_subset(&alternate_boundaries)
        || timesteps == 0
        || timesteps > MAX_TIMESTEPS
        || resized_width(crop.width(), crop.height()).is_err()
        || crop.pixels().any(|p| p[0] != p[1] || p[1] != p[2])
    {
        return normal.to_owned();
    }
    let width = crop.width() as usize;
    let min_gap = (crop.height() as usize / 6).clamp(2, 8);
    let mut gaps = Vec::new();
    let mut x = 0usize;
    while x < width {
        if (0..crop.height()).any(|y| crop.get_pixel(x as u32, y)[0] < 245) {
            x += 1;
            continue;
        }
        let start = x;
        while x < width && (0..crop.height()).all(|y| crop.get_pixel(x as u32, y)[0] >= 245) {
            x += 1;
        }
        if start > 0 && x < width && x - start >= min_gap {
            gaps.push((start, x));
        }
    }
    let mut added = BTreeSet::new();
    for &boundary in alternate_boundaries.difference(&original_boundaries) {
        if boundary == 0
            || boundary >= glyphs.len()
            || !glyphs[boundary - 1].is_alphanumeric()
            || !glyphs[boundary].is_alphanumeric()
        {
            continue;
        }
        let Some(&step) = space_steps.get(&boundary) else {
            continue;
        };
        if step >= timesteps {
            continue;
        }
        let Some(numerator) = step
            .checked_mul(2)
            .and_then(|n| n.checked_add(1))
            .and_then(|n| n.checked_mul(width))
        else {
            continue;
        };
        let Some(denominator) = timesteps.checked_mul(2) else {
            continue;
        };
        let column = numerator / denominator;
        if gaps
            .iter()
            .any(|&(left, right)| left <= column && column < right)
        {
            added.insert(boundary);
        }
    }
    if added.is_empty() {
        return normal.to_owned();
    }
    let mut output = String::with_capacity(normal.len().saturating_add(added.len()));
    let mut position = 0usize;
    for ch in normal.chars() {
        if !ch.is_whitespace() {
            if added.contains(&position) {
                output.push(' ');
            }
            position += 1;
        }
        output.push(ch);
    }
    output
}

fn decode_output(
    shape: &[i64],
    data: &[f32],
    dictionaries: &Dictionaries,
) -> Result<LineRecognition> {
    if shape.len() != 3 || shape[0] != 1 {
        bail!("recognizer output must have shape [1, time, classes]");
    }
    let time = usize::try_from(shape[1]).context("invalid recognizer time dimension")?;
    let width = usize::try_from(shape[2]).context("invalid recognizer class dimension")?;
    if time == 0
        || time > MAX_TIMESTEPS
        || width == 0
        || width > MAX_CLASSES
        || width != dictionaries.corrected.len()
        || dictionaries.original.len() < width
    {
        bail!("recognizer output dimensions do not match the bounded dictionary");
    }
    let count = time
        .checked_mul(width)
        .ok_or_else(|| anyhow!("recognizer output size overflow"))?;
    if count > MAX_OUTPUT_ELEMENTS || data.len() != count {
        bail!("recognizer output size is invalid or exceeds limit");
    }
    let mut original = String::new();
    let mut corrected = String::new();
    let mut last_index = 0usize;
    let mut score_sum = 0.0f64;
    let mut score_count = 0usize;
    for row in data.chunks_exact(width) {
        let mut max_index = 0usize;
        let mut max_value = f32::MIN;
        for (index, &value) in row.iter().enumerate() {
            if !value.is_finite() {
                bail!("recognizer output contains a non-finite score");
            }
            if value > max_value {
                max_index = index;
                max_value = value;
            }
        }
        if max_index != 0 && max_index != last_index {
            append_bounded(&mut original, &dictionaries.original[max_index])?;
            append_bounded(&mut corrected, &dictionaries.corrected[max_index])?;
            score_sum += f64::from(max_value);
            score_count += 1;
        }
        last_index = max_index;
    }
    // Match Paddle's confidence calculation, including selected empty dictionary
    // entries, while representing an empty decoded line without a NaN score.
    let score = (score_count != 0).then(|| (score_sum / score_count as f64) as f32);
    let original_score = if original.is_empty() { None } else { score };
    let corrected_score = if corrected.is_empty() { None } else { score };
    Ok(LineRecognition {
        original: RecognizedLine {
            text: original,
            score: original_score,
        },
        corrected: RecognizedLine {
            text: corrected,
            score: corrected_score,
        },
        class_width: width,
    })
}

fn append_bounded(text: &mut String, token: &str) -> Result<()> {
    if text
        .len()
        .checked_add(token.len())
        .is_none_or(|n| n > MAX_TEXT_BYTES)
    {
        bail!("recognizer decoded text exceeds limit");
    }
    text.push_str(token);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::super::models::{PPOCR_V3_REC_MODEL, PPOCR_V4_REC_MODEL, PPOCR_V5_EN_REC_MODEL};
    use super::*;

    #[test]
    fn bitmap_line_policy_defaults_to_auto_and_supports_explicit_rollback() {
        assert_eq!(
            parse_bitmap_line_policy(None).unwrap(),
            BitmapLinePolicy::Auto
        );
        assert_eq!(
            parse_bitmap_line_policy(Some(OsStr::new("auto"))).unwrap(),
            BitmapLinePolicy::Auto
        );
        for value in ["", "0", "off", "false"] {
            assert_eq!(
                parse_bitmap_line_policy(Some(OsStr::new(value))).unwrap(),
                BitmapLinePolicy::Off
            );
        }
        assert_eq!(
            parse_bitmap_line_policy(Some(OsStr::new("original"))).unwrap(),
            BitmapLinePolicy::Explicit(ExperimentalLineMapping::Original)
        );
        assert_eq!(
            parse_bitmap_line_policy(Some(OsStr::new("corrected"))).unwrap(),
            BitmapLinePolicy::Explicit(ExperimentalLineMapping::Corrected)
        );
        assert!(parse_bitmap_line_policy(Some(OsStr::new("true"))).is_err());
        assert!(parse_bitmap_line_policy(Some(OsStr::new("typo"))).is_err());
    }

    #[test]
    fn dictionary_removes_only_one_lf_and_preserves_entries() {
        let d = Dictionaries::from_metadata("a\n \nλ\n\n").unwrap();
        assert_eq!(d.corrected, ["#", "a", " ", "λ", "", " "]);
        assert_eq!(d.original, ["#", "a", " ", "λ", "", "", " "]);
        let no_lf = Dictionaries::from_metadata("a\nλ").unwrap();
        assert_eq!(no_lf.original, no_lf.corrected);
        let crlf = Dictionaries::from_metadata("a\r\n").unwrap();
        assert_eq!(crlf.corrected, ["#", "a\r", " "]);
        assert!(Dictionaries::from_metadata(&"a".repeat(MAX_METADATA_BYTES + 1)).is_err());
        assert!(Dictionaries::from_metadata(&"\n".repeat(MAX_CLASSES + 1)).is_err());
    }

    #[test]
    fn output_safety_checks_actual_width_shape_and_scores() {
        let d = Dictionaries::from_metadata("a\n").unwrap();
        let good = [0.0, 0.9, 0.1];
        assert!(decode_output(&[1, 1, 3], &good, &d).is_ok());
        for shape in [
            &[1, 3][..],
            &[2, 1, 3],
            &[1, 0, 3],
            &[1, -1, 3],
            &[1, 1, 4],
            &[1, 1, -3],
            &[1, 4097, 3],
            &[1, 1, 4097],
            &[1, i64::MAX, i64::MAX],
        ] {
            assert!(decode_output(shape, &good, &d).is_err(), "{shape:?}");
        }
        assert!(decode_output(&[1, 1, 3], &good[..2], &d).is_err());
        assert!(decode_output(&[1, 1, 3], &[0.0, 0.9, 0.1, 0.0], &d).is_err());
        let wide = Dictionaries::from_metadata(&"a\n".repeat(MAX_CLASSES - 2)).unwrap();
        let err = decode_output(&[1, 1025, MAX_CLASSES as i64], &[], &wide).unwrap_err();
        assert!(err.to_string().contains("exceeds limit"));
        assert!(decode_output(&[1, 1, 3], &[0.0, f32::NAN, 1.0], &d).is_err());
        assert!(decode_output(&[1, 1, 3], &[0.0, f32::INFINITY, 1.0], &d).is_err());
    }

    #[test]
    fn ctc_decodes_both_versions_from_same_scores() {
        let d = Dictionaries::from_metadata("a\nλ\n").unwrap();
        // a, a, blank, a, last class (empty in original, space in corrected), λ.
        let indices = [1, 1, 0, 1, 3, 2];
        let mut data = vec![0.0; indices.len() * 4];
        let confidence = [0.8, 0.7, 0.9, 0.6, 0.4, 0.2];
        for (i, index) in indices.into_iter().enumerate() {
            data[i * 4 + index] = confidence[i];
        }
        let out = decode_output(&[1, 6, 4], &data, &d).unwrap();
        assert_eq!(out.original.text, "aaλ");
        assert_eq!(out.corrected.text, "aa λ");
        assert_eq!(out.original.score, Some(0.5));
        assert_eq!(out.corrected.score, Some(0.5));
        assert_eq!(out.class_width, 4);
        let empty = decode_output(&[1, 1, 4], &[1.0, 0.0, 0.0, 0.0], &d).unwrap();
        assert_eq!(empty.original.score, None);
        assert_eq!(empty.corrected.score, None);
        let empty_token = decode_output(&[1, 1, 4], &[0.0, 0.0, 0.0, 1.0], &d).unwrap();
        assert_eq!(empty_token.original.score, None);
        assert_eq!(empty_token.corrected.score, Some(1.0));
    }

    fn anonymous_gap() -> RgbImage {
        let mut image = RgbImage::from_pixel(100, 30, image::Rgb([255; 3]));
        for y in 0..30 {
            for x in (0..30).chain(50..100) {
                image.put_pixel(x, y, image::Rgb([0; 3]));
            }
        }
        image
    }

    #[test]
    fn padded_spacing_preserves_glyphs_and_existing_boundaries() {
        let crop = anonymous_gap();
        let steps = BTreeMap::from([(2, 39)]);
        assert_eq!(
            merge_padded_spacing("αβγδ", "αβ γδ", &steps, 100, &crop),
            "αβ γδ"
        );
        for (normal, padded) in [
            ("αβ  γδ", "αβγδ"),
            ("αβγδ", "αβ γχ"),
            ("αβ\nγδ", "αβ γδ"),
            ("12.34", "12. 34"),
            ("A.B", "A. B"),
            ("-AB", "- AB"),
            ("A:B", "A: B"),
            ("AB!", "AB !"),
            ("AB-CD", "AB- CD"),
        ] {
            assert_eq!(
                merge_padded_spacing(normal, padded, &steps, 100, &crop),
                normal
            );
        }
        assert_eq!(
            merge_padded_spacing("αβγδ", "αβ γδ", &steps, 0, &crop),
            "αβγδ"
        );
        assert_eq!(
            merge_padded_spacing(
                "αβγδ",
                "αβ γδ",
                &BTreeMap::from([(2, usize::MAX)]),
                100,
                &crop
            ),
            "αβγδ"
        );
        assert_eq!(
            merge_padded_spacing("αβγδ", "αβ γδ", &BTreeMap::from([(2, 10)]), 100, &crop),
            "αβγδ"
        );
        let mut colored = crop.clone();
        colored.put_pixel(0, 0, image::Rgb([0, 1, 2]));
        assert_eq!(
            merge_padded_spacing("αβγδ", "αβ γδ", &steps, 100, &colored),
            "αβγδ"
        );
        let mut bridged = crop;
        for y in 0..30 {
            bridged.put_pixel(30 + y % 20, y, image::Rgb([0; 3]));
        }
        assert_eq!(
            merge_padded_spacing("αβγδ", "αβ γδ", &steps, 100, &bridged),
            "αβγδ"
        );
        assert!(!guard_text_supported(&"a".repeat(MAX_GUARD_CHARS + 1)));
    }

    fn sample_recognition(text: &str) -> LineRecognition {
        LineRecognition {
            original: RecognizedLine {
                text: "original".into(),
                score: Some(0.7),
            },
            corrected: RecognizedLine {
                text: text.into(),
                score: Some(0.8),
            },
            class_width: 4,
        }
    }

    #[test]
    fn probe_failures_and_speaker_markers_keep_normal_result() {
        let crop = anonymous_gap();
        assert_eq!(
            apply_padded_spacing_probe(sample_recognition("ABCD"), &crop, |_| Err(anyhow!(
                "probe failed"
            ))),
            sample_recognition("ABCD")
        );
        assert_eq!(
            apply_padded_spacing_probe(
                sample_recognition("ABCD"),
                &RgbImage::new(0, 1),
                |_| panic!("invalid crop")
            ),
            sample_recognition("ABCD")
        );
        for text in ["- AB CD", "A: BC", "AB—CD"] {
            assert_eq!(
                apply_padded_spacing_probe(sample_recognition(text), &crop, |_| panic!(
                    "speaker marker"
                )),
                sample_recognition(text)
            );
        }
        let merged = apply_padded_spacing_probe(sample_recognition("ABCD"), &crop, |_| {
            Ok((
                sample_recognition("AB CD"),
                SpaceAlignments {
                    corrected: BTreeMap::from([(2, 39)]),
                    timesteps: 100,
                },
            ))
        });
        assert_eq!(merged.corrected.text, "AB CD");
        assert_eq!(merged.corrected.score, Some(0.8));
        assert_eq!(merged.original, sample_recognition("ABCD").original);
        assert_eq!(merged.class_width, 4);
    }

    #[test]
    fn ctc_space_alignment_uses_peak_and_correct_dictionary() {
        let d = Dictionaries::from_metadata("a\nb\n").unwrap();
        let indices = [1, 0, 3, 3, 0, 2];
        let mut data = vec![0.0; indices.len() * 4];
        for (i, ix) in indices.into_iter().enumerate() {
            data[i * 4 + ix] = if i == 3 { 0.95 } else { 0.8 };
        }
        let result = ctc_space_alignments(&[1, 6, 4], &data, &d).unwrap();
        assert_eq!(result.timesteps, 6);
        assert_eq!(result.corrected, BTreeMap::from([(1, 3)]));
        assert!(ctc_space_alignments(&[1, 6, 5], &data, &d).is_err());
        assert!(ctc_space_alignments(&[1, 6, 4], &data[..20], &d).is_err());
        data[0] = f32::NAN;
        assert!(ctc_space_alignments(&[1, 6, 4], &data, &d).is_err());
    }

    #[test]
    fn vertical_padding_is_white_and_preserves_source_pixels() {
        let mut crop = RgbImage::from_pixel(3, 2, image::Rgb([10, 20, 30]));
        crop.put_pixel(2, 1, image::Rgb([40, 50, 60]));
        let (padded, resized_width) = pad_crop(&crop).unwrap();
        assert_eq!(padded.dimensions(), (3, 10));
        assert_eq!(resized_width, 14);
        for (x, y, pixel) in padded.enumerate_pixels() {
            if (VERTICAL_PADDING..VERTICAL_PADDING + crop.height()).contains(&y) {
                assert_eq!(pixel, crop.get_pixel(x, y - VERTICAL_PADDING));
            } else {
                assert_eq!(*pixel, image::Rgb([255, 255, 255]));
            }
        }
    }

    #[test]
    fn padding_bounds_are_checked_before_allocation() {
        assert_eq!(padded_crop_dimensions(1024, 4088).unwrap(), (4096, 12));
        assert!(supports_crop_dimensions(1024, 4088));
        assert!(resized_width(1000, 18).is_ok());
        for (width, height) in [
            (0, 1),
            (1, 0),
            (1025, 4088),
            (128, 4089),
            (4097, 8),
            (u32::MAX, 1),
            (1, u32::MAX),
            (1, 100),
            (1000, 10),
            (4096, 1),
        ] {
            assert!(
                padded_crop_dimensions(width, height).is_err(),
                "{width}x{height}"
            );
            assert_eq!(
                supports_crop_dimensions(width, height),
                resized_width(width, height).is_ok(),
                "{width}x{height}"
            );
        }
        assert!(pad_crop(&RgbImage::new(0, 1)).is_err());
        assert!(pad_crop(&RgbImage::new(1, 0)).is_err());
    }

    #[test]
    fn bounds_and_allowlist_fail_closed() {
        assert_eq!(resized_width(100, 50).unwrap(), 96);
        assert!(supports_crop_dimensions(100, 50));
        assert!(!supports_crop_dimensions(3200, 12));
        for (w, h) in [
            (0, 1),
            (1, 0),
            (1, 4096),
            (4096, 1),
            (4097, 10),
            (4096, 4096),
        ] {
            assert!(resized_width(w, h).is_err(), "{w}x{h}");
        }
        let file = tempfile::NamedTempFile::new().unwrap();
        assert!(read_audited_model(file.path()).is_err());
        assert!(read_audited_model(file.path().parent().unwrap()).is_err());
        file.as_file().set_len(MAX_MODEL_BYTES + 1).unwrap();
        assert!(read_audited_model(file.path()).is_err());
        assert_eq!(
            AUDITED_SHA256,
            [
                PPOCR_V3_REC_MODEL.sha256.to_ascii_lowercase(),
                PPOCR_V4_REC_MODEL.sha256.to_ascii_lowercase(),
                PPOCR_V5_EN_REC_MODEL.sha256.to_ascii_lowercase()
            ]
        );
        let mut text = "a".repeat(MAX_TEXT_BYTES);
        assert!(append_bounded(&mut text, "x").is_err());
    }

    #[test]
    #[ignore = "requires audited model, ORT, DPN_OCR_FORCE_CPU=1 and DPN_OCR_REQUIRE_GPU=1"]
    fn conflicting_provider_policy_is_not_bypassed() {
        assert_eq!(std::env::var("DPN_OCR_FORCE_CPU").as_deref(), Ok("1"));
        assert_eq!(std::env::var("DPN_OCR_REQUIRE_GPU").as_deref(), Ok("1"));
        let path = std::env::var_os("DPN_TEST_AUDITED_REC_MODEL").unwrap();
        let result = WholeLineRecognizer::new(Path::new(&path));
        let error = match result {
            Ok(_) => panic!("conflicting provider policy unexpectedly succeeded"),
            Err(err) => err,
        };
        assert!(error.to_string().contains("DPN_OCR_REQUIRE_GPU=1"));
    }

    #[test]
    #[ignore = "requires audited and metadata-only test models plus a compatible ORT shared library"]
    fn corrected_dictionary_matches_metadata_only_model_on_anonymous_fixture() {
        use paddle_ocr_rs::{base_net::BaseNet, crnn_net::CrnnNet};

        let original_path = std::env::var_os("DPN_TEST_AUDITED_REC_MODEL").unwrap();
        let corrected_path = std::env::var_os("DPN_TEST_CORRECTED_REC_MODEL").unwrap();
        let mut recognizer = WholeLineRecognizer::new(Path::new(&original_path)).unwrap();
        let mut paddle = CrnnNet::new();
        paddle
            .init_model(
                Path::new(&corrected_path).to_str().unwrap(),
                0,
                Some(configure_ort_builder),
            )
            .unwrap();
        let image = image::open(
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests/golden_subs/strict_synthetic/compact_short_pairs.png"),
        )
        .unwrap()
        .to_rgb8();
        let actual = recognizer.recognize(&image).unwrap();
        let expected = paddle
            .get_text_lines(&[image], &std::collections::HashMap::new(), 0.0)
            .unwrap();
        assert_eq!(actual.corrected.text, expected[0].text);
        if let Some(score) = actual.corrected.score {
            assert!((score - expected[0].text_score).abs() < 1e-5);
        }
    }

    #[test]
    #[ignore = "requires DPN_TEST_AUDITED_REC_MODEL and a compatible ORT shared library"]
    fn in_memory_metadata_correction_matches_checked_decoder() {
        use paddle_ocr_rs::{base_net::BaseNet, crnn_net::CrnnNet};

        let path = std::env::var_os("DPN_TEST_AUDITED_REC_MODEL").unwrap();
        let original = read_audited_model(Path::new(&path)).unwrap();
        let corrected =
            super::super::model_metadata::strip_character_metadata_lf(&original).unwrap();
        let mut recognizer = WholeLineRecognizer::preflight_audited_model(&original).unwrap();
        let mut paddle = CrnnNet::new();
        paddle
            .init_model_from_memory(&corrected, 0, Some(configure_ort_builder))
            .unwrap();
        let image = image::open(
            Path::new(env!("CARGO_MANIFEST_DIR"))
                .join("tests/golden_subs/strict_synthetic/compact_short_pairs.png"),
        )
        .unwrap()
        .to_rgb8();
        let expected = recognizer.recognize(&image).unwrap();
        let actual = paddle
            .get_text_lines(&[image], &std::collections::HashMap::new(), 0.0)
            .unwrap();
        assert_eq!(actual[0].text, expected.corrected.text);
        if let Some(score) = expected.corrected.score {
            assert!((score - actual[0].text_score).abs() < 1e-5);
        }
    }

    #[test]
    #[ignore = "requires DPN_TEST_AUDITED_REC_MODEL and a compatible ORT shared library"]
    fn padded_layouts_match_in_memory_decoder_on_anonymous_images() {
        use paddle_ocr_rs::{base_net::BaseNet, crnn_net::CrnnNet};

        let path = std::env::var_os("DPN_TEST_AUDITED_REC_MODEL").unwrap();
        let original = read_audited_model(Path::new(&path)).unwrap();
        let corrected =
            super::super::model_metadata::strip_character_metadata_lf(&original).unwrap();
        let mut recognizer = WholeLineRecognizer::preflight_audited_model(&original).unwrap();
        let mut paddle = CrnnNet::new();
        paddle
            .init_model_from_memory(&corrected, 0, Some(configure_ort_builder))
            .unwrap();
        let fixture_dir =
            Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/golden_subs/strict_synthetic");
        let punctuation = image::open(fixture_dir.join("quotes_punctuation.png"))
            .unwrap()
            .to_rgb8();
        let plain = image::open(fixture_dir.join("compact_short_pairs.png"))
            .unwrap()
            .to_rgb8();
        let mut slanted = RgbImage::from_pixel(
            plain.width() + plain.height() / 4,
            plain.height(),
            image::Rgb([255; 3]),
        );
        for (x, y, pixel) in plain.enumerate_pixels() {
            slanted.put_pixel(x + (plain.height() - 1 - y) / 4, y, *pixel);
        }
        let mut speaker =
            RgbImage::from_pixel(plain.width() + 24, plain.height(), image::Rgb([255; 3]));
        image::imageops::replace(&mut speaker, &plain, 24, 0);
        for y in plain.height() / 2..plain.height() / 2 + 3 {
            for x in 5..19 {
                speaker.put_pixel(x, y, image::Rgb([0; 3]));
            }
        }
        for image in [punctuation, slanted, speaker] {
            let actual = recognizer.recognize(&image).unwrap();
            let guarded = recognizer.recognize_with_spacing_probe(&image).unwrap();
            assert_eq!(guarded.original, actual.original);
            assert_eq!(guarded.corrected.score, actual.corrected.score);
            assert_eq!(guarded.class_width, actual.class_width);
            let normal_glyphs: String = actual
                .corrected
                .text
                .chars()
                .filter(|c| *c != ' ')
                .collect();
            let guarded_glyphs: String = guarded
                .corrected
                .text
                .chars()
                .filter(|c| *c != ' ')
                .collect();
            assert_eq!(guarded_glyphs, normal_glyphs);
            if let (Some((_, before)), Some((_, after))) = (
                spacing_layout(&actual.corrected.text),
                spacing_layout(&guarded.corrected.text),
            ) {
                assert!(before.is_subset(&after));
            } else {
                assert_eq!(guarded.corrected.text, actual.corrected.text);
            }
            let expected = paddle
                .get_text_lines(&[image], &std::collections::HashMap::new(), 0.0)
                .unwrap();
            assert_eq!(actual.corrected.text, expected[0].text);
            if let Some(score) = actual.corrected.score {
                assert!((score - expected[0].text_score).abs() < 1e-5);
            }
        }
    }

    #[test]
    #[ignore = "requires DPN_TEST_AUDITED_REC_MODEL and a compatible ORT shared library"]
    fn audited_model_runs_on_anonymous_crop() {
        let path = std::env::var_os("DPN_TEST_AUDITED_REC_MODEL").unwrap();
        let bytes = read_audited_model(Path::new(&path)).unwrap();
        let mut recognizer = WholeLineRecognizer::preflight_audited_model(&bytes).unwrap();
        use paddle_ocr_rs::{base_net::BaseNet, crnn_net::CrnnNet};
        let mut paddle = CrnnNet::new();
        paddle
            .init_model(
                Path::new(&path).to_str().unwrap(),
                0,
                Some(configure_ort_builder),
            )
            .unwrap();
        for width in [96, 192] {
            let mut crop = RgbImage::from_pixel(width, 48, image::Rgb([255, 255, 255]));
            for (x, y, pixel) in crop.enumerate_pixels_mut() {
                if (12..36).contains(&y) && x % 15 < 4 {
                    *pixel = image::Rgb([0, 0, 0]);
                }
            }
            let result = recognizer.recognize(&crop).unwrap();
            let mut rollback =
                ExperimentalLineRecognizer::new(bytes.clone(), ExperimentalLineMapping::Original);
            assert_eq!(
                rollback.recognize_with_spacing_probe(&crop, true).unwrap(),
                result.original
            );
            assert_eq!(result.class_width, recognizer.dictionaries.corrected.len());
            let paddle_result = paddle
                .get_text_lines(&[crop], &std::collections::HashMap::new(), 0.0)
                .unwrap();
            assert_eq!(result.original.text, paddle_result[0].text);
            if let Some(score) = result.original.score {
                assert!((score - paddle_result[0].text_score).abs() < 1e-5);
            }
        }
    }
}
