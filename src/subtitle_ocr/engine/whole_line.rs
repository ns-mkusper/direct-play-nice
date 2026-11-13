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

use super::converters::OcrRecProfile;
use super::runtime::configure_ort_builder;

// Independently pinned: updating model provisioning must not approve a new model
// without repeating the self-contained ONNX schema and external-data audit.
const AUDITED_SHA256: [&str; 3] = [
    "ef7abd8bd3629ae57ea2c28b425c1bd258a871b93fd2fe7c433946ade9b5d9ea",
    "e8770c967605983d1570cdf5352041dfb68fa0c21664f49f47b155abd3e0e318",
    "640e323ac1a4da562eddd2a3963b4e1336686f898113983122d76d24dc3afcfe",
];
const AUDITED_LATIN_SHA256: [&str; 1] =
    ["8e6b29f8d68753ee536b465801c918ced32d0fde29566398f10ab6b88eadab48"];
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

/// Builds the CTC dictionary from the model's `character` metadata.
///
/// Paddle's metadata ends in an LF, which its own loader turns into an extra
/// empty class that takes the place of the final space class. Dropping one
/// terminal LF aligns the dictionary with the model's actual class width.
fn dictionary_from_metadata(character: &str) -> Result<Vec<String>> {
    if character.len() > MAX_METADATA_BYTES {
        bail!("recognizer character metadata exceeds limit");
    }
    let raw = character.strip_suffix('\n').unwrap_or(character);
    let mut keys = Vec::with_capacity(raw.split('\n').count() + 2);
    keys.push("#".to_owned());
    keys.extend(raw.split('\n').map(str::to_owned));
    keys.push(" ".to_owned());
    if keys.len() > MAX_CLASSES {
        bail!("recognizer dictionary exceeds class limit");
    }
    Ok(keys)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(in crate::subtitle_ocr) enum BitmapLinePolicy {
    /// Use direct-line OCR when the audited models are installed.
    Auto,
    Off,
    /// Fail initialization unless the audited models are installed.
    Required,
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
        "1" | "on" | "true" => Ok(BitmapLinePolicy::Required),
        _ => bail!("DPN_OCR_EXPERIMENTAL_BITMAP_LINES must be auto, on, or off"),
    }
}

pub(in crate::subtitle_ocr) fn bitmap_line_policy_from_env() -> Result<BitmapLinePolicy> {
    parse_bitmap_line_policy(std::env::var_os("DPN_OCR_EXPERIMENTAL_BITMAP_LINES").as_deref())
}

#[derive(Default)]
pub(in crate::subtitle_ocr) struct ProfileLineRecognizers {
    pub(super) english: Option<ExperimentalLineRecognizer>,
    pub(super) latin: Option<ExperimentalLineRecognizer>,
}

impl ProfileLineRecognizers {
    pub(super) fn get(&self, profile: OcrRecProfile) -> Option<&ExperimentalLineRecognizer> {
        match profile {
            OcrRecProfile::English => self.english.as_ref(),
            OcrRecProfile::Latin => self.latin.as_ref(),
            _ => None,
        }
    }

    pub(super) fn get_mut(
        &mut self,
        profile: OcrRecProfile,
    ) -> Option<&mut ExperimentalLineRecognizer> {
        match profile {
            OcrRecProfile::English => self.english.as_mut(),
            OcrRecProfile::Latin => self.latin.as_mut(),
            _ => None,
        }
    }
}

/// Lazily initializes only on a supported bitmap line, once per worker.
pub(in crate::subtitle_ocr) struct ExperimentalLineRecognizer {
    audited_model: Vec<u8>,
    recognizer: Option<WholeLineRecognizer>,
}

impl ExperimentalLineRecognizer {
    pub(in crate::subtitle_ocr) fn new(audited_model: Vec<u8>) -> Self {
        Self {
            audited_model,
            recognizer: None,
        }
    }

    /// Recognizes one line; `allow_probe` enables the padded spacing probe.
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
        if allow_probe {
            recognizer.recognize_with_spacing_probe(crop)
        } else {
            recognizer.recognize(crop)
        }
    }
}

/// Own one instance per OCR worker; the supplied crop must already be oriented.
pub(in crate::subtitle_ocr) struct WholeLineRecognizer {
    session: Session,
    input_name: String,
    dictionary: Vec<String>,
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
            || shape[2] != recognizer.dictionary.len() as i64
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
        let input_type = &session.inputs[0].input_type;
        let shape = input_type
            .tensor_shape()
            .ok_or_else(|| anyhow!("recognizer input must be a tensor"))?;
        if input_type.tensor_type() != Some(TensorElementType::Float32)
            || shape.len() != 4
            || (shape[0] != -1 && shape[0] != 1)
            || shape[1] != 3
            || (shape[2] != -1 && shape[2] != i64::from(HEIGHT))
        {
            bail!("recognizer input must support [1, 3, 48, width] float tensors");
        }
        let input_name = session.inputs[0].name.clone();
        let character = session
            .metadata()?
            .custom("character")?
            .ok_or_else(|| anyhow!("recognizer has no character metadata"))?;
        let dictionary = dictionary_from_metadata(&character)?;
        Ok(Self {
            session,
            input_name,
            dictionary,
        })
    }

    pub(in crate::subtitle_ocr) fn recognize(&mut self, crop: &RgbImage) -> Result<RecognizedLine> {
        Ok(self.recognize_once(crop, false)?.0)
    }

    fn recognize_with_spacing_probe(&mut self, crop: &RgbImage) -> Result<RecognizedLine> {
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
    ) -> Result<(RecognizedLine, Option<SpaceAlignments>)> {
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
        let result = decode_output(shape, data, &self.dictionary)?;
        let alignments = if with_alignments {
            Some(ctc_space_alignments(shape, data, &self.dictionary)?)
        } else {
            None
        };
        Ok((result, alignments))
    }
}

/// Trims blank columns beyond a margin of a quarter of the crop height.
///
/// PGS line crops span the whole rectangle, so a short centered line can sit
/// after hundreds of blank columns; the PP-OCRv5 Latin model then drops its
/// first glyphs (`-De hecho,` reads as `hecho,`).
pub(in crate::subtitle_ocr) fn trim_blank_columns(crop: &RgbImage) -> Option<RgbImage> {
    let ink = |x: u32| (0..crop.height()).any(|y| crop.get_pixel(x, y).0.iter().any(|&v| v < 245));
    let first = (0..crop.width()).find(|&x| ink(x))?;
    let last = (first..crop.width()).rev().find(|&x| ink(x))?;
    let margin = crop.height() / 4;
    let left = first.saturating_sub(margin);
    let right = last.saturating_add(margin + 1).min(crop.width());
    if left == 0 && right == crop.width() {
        return None;
    }
    Some(image::imageops::crop_imm(crop, left, 0, right - left, crop.height()).to_image())
}

/// Restores the space that always precedes a Spanish `¿`/`¡` inside a line.
///
/// The recognizer often merges the gap after a closing mark (`¡Vamos!¿Qué?`),
/// and the padded probe only restores spaces between alphanumerics. Openers at
/// the line start or after another opening mark are left alone.
pub(in crate::subtitle_ocr) fn space_before_inverted_openers(text: &str) -> String {
    let mut output = String::with_capacity(text.len() + 2);
    let mut previous: Option<char> = None;
    for ch in text.chars() {
        if matches!(ch, '¿' | '¡')
            && previous
                .is_some_and(|prev| !prev.is_whitespace() && !"¿¡\"'«“‘([{—–-".contains(prev))
        {
            output.push(' ');
        }
        output.push(ch);
        previous = Some(ch);
    }
    output
}

fn read_bounded_model(path: &Path) -> Result<Vec<u8>> {
    let mut options = OpenOptions::new();
    options.read(true);
    // Opening an attacker-controlled FIFO must not hang a worker on Unix.
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.custom_flags(libc::O_NONBLOCK);
    }
    let file = options.open(path).context("opening model")?;
    let metadata = file.metadata()?;
    if !metadata.is_file() {
        bail!("model must be a regular file");
    }
    if metadata.len() > MAX_MODEL_BYTES {
        bail!("model exceeds size limit");
    }
    let mut bytes = Vec::new();
    file.take(MAX_MODEL_BYTES + 1).read_to_end(&mut bytes)?;
    if bytes.len() as u64 > MAX_MODEL_BYTES {
        bail!("model exceeds size limit");
    }
    Ok(bytes)
}

/// Reads a bounded model and accepts it only if its SHA-256 is in `allowed`.
pub(super) fn read_pinned_model(path: &Path, allowed: &[&str]) -> Result<Vec<u8>> {
    let bytes = read_bounded_model(path)?;
    let digest = format!("{:x}", Sha256::digest(&bytes));
    if !allowed.contains(&digest.as_str()) {
        bail!("model is not in the audited allowlist");
    }
    Ok(bytes)
}

pub(super) fn read_audited_model_for_profile(
    path: &Path,
    profile: OcrRecProfile,
) -> Result<Vec<u8>> {
    let hashes: &[&str] = match profile {
        OcrRecProfile::English => &AUDITED_SHA256,
        OcrRecProfile::Latin => &AUDITED_LATIN_SHA256,
        _ => &[],
    };
    read_pinned_model(path, hashes).context("experimental recognizer model")
}

#[cfg(test)]
fn read_audited_model(path: &Path) -> Result<Vec<u8>> {
    read_audited_model_for_profile(path, OcrRecProfile::English)
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
    spaces: BTreeMap<usize, usize>,
    timesteps: usize,
}

/// Validates a `[1, time, classes]` recognizer output against the dictionary
/// and returns `(time, classes)`.
fn checked_output_dims(
    shape: &[i64],
    data: &[f32],
    dictionary: &[String],
) -> Result<(usize, usize)> {
    if shape.len() != 3 || shape[0] != 1 {
        bail!("recognizer output must have shape [1, time, classes]");
    }
    let time = usize::try_from(shape[1]).context("invalid recognizer time dimension")?;
    let width = usize::try_from(shape[2]).context("invalid recognizer class dimension")?;
    if time == 0
        || time > MAX_TIMESTEPS
        || width == 0
        || width > MAX_CLASSES
        || width != dictionary.len()
    {
        bail!("recognizer output dimensions do not match the bounded dictionary");
    }
    let count = time
        .checked_mul(width)
        .ok_or_else(|| anyhow!("recognizer output size overflow"))?;
    if count > MAX_OUTPUT_ELEMENTS || data.len() != count {
        bail!("recognizer output size is invalid or exceeds limit");
    }
    Ok((time, width))
}

/// Returns the first highest-scoring class of one timestep, rejecting non-finite scores.
fn best_class(row: &[f32]) -> Result<(usize, f32)> {
    let mut best = (0usize, f32::MIN);
    for (index, &score) in row.iter().enumerate() {
        if !score.is_finite() {
            bail!("recognizer output contains a non-finite score");
        }
        if score > best.1 {
            best = (index, score);
        }
    }
    Ok(best)
}

/// Maps each decoded space (by count of preceding glyphs) to the timestep where
/// its CTC run peaks.
fn ctc_space_alignments(
    shape: &[i64],
    data: &[f32],
    dictionary: &[String],
) -> Result<SpaceAlignments> {
    let (time, width) = checked_output_dims(shape, data, dictionary)?;
    let mut runs = Vec::new();
    let mut previous = 0usize;
    let mut peak_step = 0usize;
    let mut peak_score = f32::MIN;
    for (step, row) in data.chunks_exact(width).enumerate() {
        let (best_index, best_score) = best_class(row)?;
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
        for ch in dictionary[index].chars() {
            if ch == ' ' {
                positions.insert(glyph_index, step);
            } else if !ch.is_whitespace() {
                glyph_index += 1;
            }
        }
    }
    Ok(SpaceAlignments {
        spaces: positions,
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
    mut normal: RecognizedLine,
    crop: &RgbImage,
    probe: impl FnOnce(&RgbImage) -> Result<(RecognizedLine, SpaceAlignments)>,
) -> RecognizedLine {
    if spacing_layout(&normal.text).is_none() {
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
    normal.text = merge_padded_spacing(
        &normal.text,
        &alternative.text,
        &alignments.spaces,
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
    let blank_columns: Vec<bool> = (0..crop.width())
        .map(|x| (0..crop.height()).all(|y| crop.get_pixel(x, y)[0] >= 245))
        .collect();
    // Interior runs of blank columns wide enough to separate words.
    let mut gaps = Vec::new();
    let mut x = 0usize;
    while x < width {
        if !blank_columns[x] {
            x += 1;
            continue;
        }
        let start = x;
        while x < width && blank_columns[x] {
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

fn decode_output(shape: &[i64], data: &[f32], dictionary: &[String]) -> Result<RecognizedLine> {
    let (_, width) = checked_output_dims(shape, data, dictionary)?;
    let mut text = String::new();
    let mut last_index = 0usize;
    let mut score_sum = 0.0f64;
    let mut score_count = 0usize;
    for row in data.chunks_exact(width) {
        let (max_index, max_value) = best_class(row)?;
        if max_index != 0 && max_index != last_index {
            append_bounded(&mut text, &dictionary[max_index])?;
            score_sum += f64::from(max_value);
            score_count += 1;
        }
        last_index = max_index;
    }
    // Match Paddle's mean confidence, without a NaN score for an empty line.
    let score = (!text.is_empty()).then(|| (score_sum / score_count as f64) as f32);
    Ok(RecognizedLine { text, score })
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
    fn bitmap_line_policy_defaults_to_auto() {
        let parse = |value: Option<&str>| parse_bitmap_line_policy(value.map(OsStr::new));
        assert_eq!(parse(None).unwrap(), BitmapLinePolicy::Auto);
        assert_eq!(parse(Some("auto")).unwrap(), BitmapLinePolicy::Auto);
        for value in ["", "0", "off", "false"] {
            assert_eq!(parse(Some(value)).unwrap(), BitmapLinePolicy::Off);
        }
        for value in ["1", "on", "true"] {
            assert_eq!(parse(Some(value)).unwrap(), BitmapLinePolicy::Required);
        }
        assert!(parse(Some("typo")).is_err());
    }

    #[test]
    fn dictionary_removes_only_one_lf_and_preserves_entries() {
        let d = dictionary_from_metadata("a\n \nλ\n\n").unwrap();
        assert_eq!(d, ["#", "a", " ", "λ", "", " "]);
        assert_eq!(
            dictionary_from_metadata("a\nλ").unwrap(),
            ["#", "a", "λ", " "]
        );
        assert_eq!(
            dictionary_from_metadata("a\r\n").unwrap(),
            ["#", "a\r", " "]
        );
        assert!(dictionary_from_metadata(&"a".repeat(MAX_METADATA_BYTES + 1)).is_err());
        assert!(dictionary_from_metadata(&"\n".repeat(MAX_CLASSES + 1)).is_err());
    }

    #[test]
    fn output_safety_checks_actual_width_shape_and_scores() {
        let d = dictionary_from_metadata("a\n").unwrap();
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
        let wide = dictionary_from_metadata(&"a\n".repeat(MAX_CLASSES - 2)).unwrap();
        let err = decode_output(&[1, 1025, MAX_CLASSES as i64], &[], &wide).unwrap_err();
        assert!(err.to_string().contains("exceeds limit"));
        assert!(decode_output(&[1, 1, 3], &[0.0, f32::NAN, 1.0], &d).is_err());
        assert!(decode_output(&[1, 1, 3], &[0.0, f32::INFINITY, 1.0], &d).is_err());
    }

    #[test]
    fn ctc_decodes_space_class_and_mean_score() {
        let d = dictionary_from_metadata("a\nλ\n").unwrap();
        // a, a, blank, a, space (the class Paddle's loader maps to ""), λ.
        let indices = [1, 1, 0, 1, 3, 2];
        let mut data = vec![0.0; indices.len() * 4];
        let confidence = [0.8, 0.7, 0.9, 0.6, 0.4, 0.2];
        for (i, index) in indices.into_iter().enumerate() {
            data[i * 4 + index] = confidence[i];
        }
        let out = decode_output(&[1, 6, 4], &data, &d).unwrap();
        assert_eq!(out.text, "aa λ");
        assert_eq!(out.score, Some(0.5));
        let empty = decode_output(&[1, 1, 4], &[1.0, 0.0, 0.0, 0.0], &d).unwrap();
        assert_eq!(empty.score, None);
        let space = decode_output(&[1, 1, 4], &[0.0, 0.0, 0.0, 1.0], &d).unwrap();
        assert_eq!(space.score, Some(1.0));
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

    fn sample_recognition(text: &str) -> RecognizedLine {
        RecognizedLine {
            text: text.into(),
            score: Some(0.8),
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
                    spaces: BTreeMap::from([(2, 39)]),
                    timesteps: 100,
                },
            ))
        });
        assert_eq!(merged.text, "AB CD");
        assert_eq!(merged.score, Some(0.8));
    }

    #[test]
    fn ctc_space_alignment_uses_peak_and_correct_dictionary() {
        let d = dictionary_from_metadata("a\nb\n").unwrap();
        let indices = [1, 0, 3, 3, 0, 2];
        let mut data = vec![0.0; indices.len() * 4];
        for (i, ix) in indices.into_iter().enumerate() {
            data[i * 4 + ix] = if i == 3 { 0.95 } else { 0.8 };
        }
        let result = ctc_space_alignments(&[1, 6, 4], &data, &d).unwrap();
        assert_eq!(result.timesteps, 6);
        assert_eq!(result.spaces, BTreeMap::from([(1, 3)]));
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
        for profile in [
            OcrRecProfile::English,
            OcrRecProfile::Latin,
            OcrRecProfile::Multilingual,
        ] {
            assert!(read_audited_model_for_profile(file.path(), profile).is_err());
        }
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
    #[ignore = "requires DPN_TEST_AUDITED_LATIN_REC_MODEL and a compatible ORT shared library"]
    fn latin_model_dictionary_matches_output() {
        let path = std::env::var_os("DPN_TEST_AUDITED_LATIN_REC_MODEL").unwrap();
        let model = read_audited_model_for_profile(Path::new(&path), OcrRecProfile::Latin).unwrap();
        assert!(read_audited_model_for_profile(Path::new(&path), OcrRecProfile::English).is_err());
        let recognizer = WholeLineRecognizer::preflight_audited_model(&model).unwrap();
        assert_eq!(recognizer.dictionary.len(), 504);
        for glyph in ["à", "á", "ä", "é", "ñ", "¿", "¡"] {
            assert!(
                recognizer.dictionary.iter().any(|key| key == glyph),
                "missing {glyph}"
            );
        }
        assert_eq!(recognizer.dictionary.last().unwrap(), " ");
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
        assert_eq!(actual[0].text, expected.text);
        if let Some(score) = expected.score {
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
            assert_eq!(guarded.score, actual.score);
            let glyphs = |text: &str| text.replace(' ', "");
            assert_eq!(glyphs(&guarded.text), glyphs(&actual.text));
            if let (Some((_, before)), Some((_, after))) =
                (spacing_layout(&actual.text), spacing_layout(&guarded.text))
            {
                assert!(before.is_subset(&after));
            } else {
                assert_eq!(guarded.text, actual.text);
            }
            let expected = paddle
                .get_text_lines(&[image], &std::collections::HashMap::new(), 0.0)
                .unwrap();
            assert_eq!(actual.text, expected[0].text);
            if let Some(score) = actual.score {
                assert!((score - expected[0].text_score).abs() < 1e-5);
            }
        }
    }

    #[test]
    fn inverted_openers_get_a_preceding_space_only_inside_a_line() {
        for (input, expected) in [
            ("¡Vamos!¿Qué pasa?", "¡Vamos! ¿Qué pasa?"),
            ("Hola,¿qué tal?", "Hola, ¿qué tal?"),
            ("¿¡Qué!?", "¿¡Qué!?"),
            ("—¿Sí?", "—¿Sí?"),
            ("\"¡Ya!\"", "\"¡Ya!\""),
            ("Vamos! ¿Qué?", "Vamos! ¿Qué?"),
            ("No change.", "No change."),
        ] {
            assert_eq!(space_before_inverted_openers(input), expected, "{input}");
        }
    }

    #[test]
    fn blank_columns_are_trimmed_to_a_height_relative_margin() {
        let mut crop = RgbImage::from_pixel(100, 20, image::Rgb([255; 3]));
        crop.put_pixel(40, 10, image::Rgb([0; 3]));
        crop.put_pixel(60, 3, image::Rgb([244, 255, 255]));
        let trimmed = trim_blank_columns(&crop).unwrap();
        assert_eq!(trimmed.dimensions(), (31, 20));
        assert_eq!(trimmed.get_pixel(5, 10).0, [0; 3]);
        assert!(trim_blank_columns(&trimmed).is_none());
        assert!(trim_blank_columns(&RgbImage::from_pixel(9, 9, image::Rgb([255; 3]))).is_none());
    }
}
