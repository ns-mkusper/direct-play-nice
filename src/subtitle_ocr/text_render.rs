//! OCR text I/O adapters.
//!
//! Wraps Tesseract/external command execution and handles subtitle text
//! serialization to SRT/ASS.

use super::*;
use std::env;
pub(super) struct TesseractCandidate {
    pub(super) text: String,
    pub(super) psm: u8,
    pub(super) quality: f32,
}

pub(super) fn ppocr_engine_label(ocr_engine: OcrEngine) -> &'static str {
    match ocr_engine {
        OcrEngine::PpOcrV3 => "PP-OCRv3",
        OcrEngine::PpOcrV4 => "PP-OCRv4",
        _ => "PP-OCR",
    }
}

pub(super) fn run_tesseract(image_path: &Path, language: &str) -> Result<String> {
    Ok(run_tesseract_best_effort(image_path, language)?.text)
}

pub(super) fn run_tesseract_best_effort(
    image_path: &Path,
    language: &str,
) -> Result<TesseractCandidate> {
    let psm_modes: &[u8] = if language_uses_spaces(language) {
        &[6, 7]
    } else {
        &[6]
    };

    let mut best: Option<TesseractCandidate> = None;
    let mut last_error = None;

    for psm in psm_modes {
        match run_tesseract_with_psm(image_path, language, *psm) {
            Ok(text) if !text.is_empty() => {
                let quality = ocr_text_quality_score(&text, language);
                let candidate = TesseractCandidate {
                    text,
                    psm: *psm,
                    quality,
                };
                let should_replace = best
                    .as_ref()
                    .map(|current| candidate.quality > current.quality + 0.10)
                    .unwrap_or(true);
                if should_replace {
                    best = Some(candidate);
                }
            }
            Ok(_) => {}
            Err(err) => {
                last_error = Some(err);
            }
        }
    }

    if let Some(mut candidate) = best {
        candidate.text = recover_terminal_exclamation_with(
            &candidate.text,
            || run_tesseract_raw(image_path, language, 13),
            || run_tesseract_boxes(image_path, language, candidate.psm),
            || Ok(image::open(image_path)?.to_luma8()),
        );
        return Ok(candidate);
    }
    if let Some(err) = last_error {
        return Err(err);
    }
    Ok(TesseractCandidate {
        text: String::new(),
        psm: *psm_modes.first().unwrap_or(&6),
        quality: 0.0,
    })
}

fn run_tesseract_raw(image_path: &Path, language: &str, psm: u8) -> Result<String> {
    let output = Command::new("tesseract")
        .arg(image_path)
        .arg("stdout")
        .arg("-l")
        .arg(language)
        .arg("--oem")
        .arg("1")
        .arg("--psm")
        .arg(psm.to_string())
        .arg("-c")
        .arg("preserve_interword_spaces=1")
        .output()
        .with_context(|| {
            format!(
                "running tesseract on '{}' (lang={}, psm={})",
                image_path.display(),
                language,
                psm
            )
        })?;

    if !output.status.success() {
        bail!(
            "tesseract OCR failed for '{}' (lang={}, psm={}): {}",
            image_path.display(),
            language,
            psm,
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(normalize_utf8_text(&String::from_utf8_lossy(
        &output.stdout,
    )))
}

pub(super) fn run_tesseract_with_psm(image_path: &Path, language: &str, psm: u8) -> Result<String> {
    let text = run_tesseract_raw(image_path, language, psm)?;
    if !text.contains('"') || !text.contains(['“', '”']) {
        return Ok(text);
    }

    // Tesseract can assign different quote characters to identical straight
    // glyphs. Consult its character boxes and the source image before changing
    // anything; a character-level match also prevents transferring bad boxes.
    if let (Ok(boxes), Ok(image)) = (
        run_tesseract_boxes(image_path, language, psm),
        image::open(image_path),
    ) {
        return Ok(recover_straight_quote_from_boxes(
            &text,
            &boxes,
            &image.to_luma8(),
        ));
    }
    Ok(text)
}

fn tesseract_boxes_command(image_path: &Path, language: &str, psm: u8) -> Command {
    let mut command = Command::new("tesseract");
    command
        .arg(image_path)
        .arg("stdout")
        .arg("-l")
        .arg(language)
        .arg("--oem")
        .arg("1")
        .arg("--psm")
        .arg(psm.to_string())
        .arg("-c")
        .arg("preserve_interword_spaces=1")
        .arg("-c")
        .arg("tessedit_create_boxfile=1");
    command
}

fn run_tesseract_boxes(image_path: &Path, language: &str, psm: u8) -> Result<String> {
    let output = tesseract_boxes_command(image_path, language, psm).output()?;
    if !output.status.success() {
        bail!("tesseract character boxes failed");
    }
    Ok(String::from_utf8_lossy(&output.stdout).into_owned())
}

fn parse_tesseract_boxes(boxes: &str) -> Option<Vec<(char, [u32; 4])>> {
    boxes
        .lines()
        .map(|line| {
            let mut parts = line.split_whitespace();
            let mut chars = parts.next()?.chars();
            let ch = chars.next()?;
            if chars.next().is_some() {
                return None;
            }
            let bounds = [
                parts.next()?.parse().ok()?,
                parts.next()?.parse().ok()?,
                parts.next()?.parse().ok()?,
                parts.next()?.parse().ok()?,
            ];
            if parts.next()? != "0" || parts.next().is_some() {
                return None;
            }
            Some((ch, bounds))
        })
        .collect()
}

#[cfg(test)]
mod tesseract_boxes_tests {
    use super::*;
    use std::ffi::OsStr;
    use std::process::Output;

    #[test]
    fn command_requests_boxes_without_a_config_file() {
        let image_path = Path::new("synthetic image.pgm");
        let command = tesseract_boxes_command(image_path, "eng", 7);
        assert_eq!(command.get_program(), OsStr::new("tesseract"));
        assert_eq!(
            command.get_args().collect::<Vec<_>>(),
            [
                "synthetic image.pgm",
                "stdout",
                "-l",
                "eng",
                "--oem",
                "1",
                "--psm",
                "7",
                "-c",
                "preserve_interword_spaces=1",
                "-c",
                "tessedit_create_boxfile=1",
            ]
            .iter()
            .map(OsStr::new)
            .collect::<Vec<_>>()
        );
    }

    fn run_with_tessdata(mut command: Command, tessdata: &Path) -> Output {
        let output = command
            .env("TESSDATA_PREFIX", tessdata)
            .env("OMP_THREAD_LIMIT", "1")
            .output()
            .expect("run Tesseract");
        assert!(
            output.status.success(),
            "Tesseract failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        output
    }

    fn positional_makebox(image: &Path, psm: u8) -> Command {
        let mut command = Command::new("tesseract");
        command
            .arg(image)
            .args(["stdout", "-l", "eng", "--oem", "1", "--psm"])
            .arg(psm.to_string())
            .args(["-c", "preserve_interword_spaces=1", "makebox"]);
        command
    }

    #[test]
    #[ignore = "requires Tesseract and DPN_TEST_TESSDATA_DIR containing eng.traineddata"]
    fn config_free_child_produces_boxes_and_matches_standard_makebox() {
        let source = std::env::var_os("DPN_TEST_TESSDATA_DIR")
            .expect("set DPN_TEST_TESSDATA_DIR to the directory containing eng.traineddata");
        let source = Path::new(&source).join("eng.traineddata");
        let temp = tempfile::tempdir().expect("create isolated tessdata directories");
        let absent = temp.path().join("absent");
        let present = temp.path().join("present");
        for dir in [&absent, &present] {
            std::fs::create_dir(dir).expect("create tessdata directory");
            std::fs::copy(&source, dir.join("eng.traineddata")).expect("copy English traineddata");
        }
        std::fs::create_dir(present.join("configs")).expect("create configs directory");
        std::fs::write(
            present.join("configs/makebox"),
            b"tessedit_create_boxfile 1\n",
        )
        .expect("write standard makebox config");

        let image = Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("tests/golden_subs/strict_synthetic/title.png");
        let (width, height) = image::open(&image)
            .expect("load synthetic image")
            .to_luma8()
            .dimensions();
        for psm in [6, 7, 11, 13] {
            let without_config =
                run_with_tessdata(tesseract_boxes_command(&image, "eng", psm), &absent);
            let with_config =
                run_with_tessdata(tesseract_boxes_command(&image, "eng", psm), &present);
            let standard = run_with_tessdata(positional_makebox(&image, psm), &present);
            assert_eq!(without_config.stdout, with_config.stdout);
            assert_eq!(without_config.stdout, standard.stdout);
            let output = std::str::from_utf8(&without_config.stdout).expect("UTF-8 box output");
            let boxes = parse_tesseract_boxes(output).expect("parse box output");
            assert!(!boxes.is_empty(), "synthetic image should yield boxes");
            assert!(boxes.iter().all(|(_, [left, bottom, right, top])| {
                left < right && bottom < top && *right <= width && *top <= height
            }));

            let missing = run_with_tessdata(positional_makebox(&image, psm), &absent);
            let plain = std::str::from_utf8(&missing.stdout).expect("UTF-8 text output");
            assert!(!plain.is_empty());
            assert!(parse_tesseract_boxes(plain).is_none());
        }

        let blank = temp.path().join("blank.png");
        image::GrayImage::from_pixel(300, 80, image::Luma([255]))
            .save(&blank)
            .expect("save blank image");
        let output = run_with_tessdata(tesseract_boxes_command(&blank, "eng", 6), &absent);
        let boxes = std::str::from_utf8(&output.stdout).expect("UTF-8 output");
        assert_eq!(parse_tesseract_boxes(boxes), Some(Vec::new()));
    }
}

// Run at most one alternate recognition and one box extraction, only for the
// ambiguous single-line ending. Any failed or conflicting evidence leaves it intact.
fn recover_terminal_exclamation_with(
    text: &str,
    alternate: impl FnOnce() -> Result<String>,
    boxes: impl FnOnce() -> Result<String>,
    image: impl FnOnce() -> Result<image::GrayImage>,
) -> String {
    if text.contains(['\n', '\r']) {
        return text.to_string();
    }
    let Some(prefix) = text.strip_suffix("?\"") else {
        return text.to_string();
    };
    if !balanced_quote_context(prefix) {
        return text.to_string();
    }
    let Ok(alternate) = alternate() else {
        return text.to_string();
    };
    if alternate != format!("{prefix}?!") {
        return text.to_string();
    }
    let Ok(boxes) = boxes() else {
        return text.to_string();
    };
    let Ok(image) = image() else {
        return text.to_string();
    };
    if terminal_exclamation_mask(text, &boxes, &image) {
        return alternate;
    }
    text.to_string()
}

fn balanced_quote_context(text: &str) -> bool {
    let mut expected = Vec::new();
    let mut chars = text.chars().peekable();
    let mut previous: Option<char> = None;
    while let Some(ch) = chars.next() {
        // An apostrophe inside a word can also be an omitted quote boundary.
        if matches!(ch, '\'' | '’' | '＇')
            && previous.is_some_and(char::is_alphanumeric)
            && chars.peek().is_some_and(|next| next.is_alphanumeric())
        {
            return false;
        }
        if expected
            .last()
            .is_some_and(|end| *end == ch || (*end == '〞' && ch == '〟'))
        {
            expected.pop();
        } else {
            match ch {
                '"' | '\'' | '＂' | '＇' => expected.push(ch),
                '“' | '‟' => expected.push('”'),
                '‘' | '‛' => expected.push('’'),
                '„' => expected.push('“'),
                '‚' => expected.push('‘'),
                '«' => expected.push('»'),
                '‹' => expected.push('›'),
                '「' => expected.push('」'),
                '『' => expected.push('』'),
                '｢' => expected.push('｣'),
                '《' => expected.push('》'),
                '〈' => expected.push('〉'),
                '〝' => expected.push('〞'),
                '”' | '’' | '»' | '›' | '」' | '』' | '｣' | '》' | '〉' | '〞' | '〟' =>
                {
                    return false;
                }
                _ => {}
            }
        }
        previous = Some(ch);
    }
    expected.is_empty()
}

fn terminal_exclamation_mask(text: &str, boxes: &str, image: &image::GrayImage) -> bool {
    let Some(parsed) = parse_tesseract_boxes(boxes) else {
        return false;
    };
    if parsed.len() < 2
        || !text
            .chars()
            .filter(|c| !c.is_whitespace())
            .eq(parsed.iter().map(|(c, _)| *c))
        || parsed.iter().any(|(_, [left, bottom, right, top])| {
            left >= right || bottom >= top || *right > image.width() || *top > image.height()
        })
    {
        return false;
    }
    let ('?', [_, qbottom, qright, qtop]) = parsed[parsed.len() - 2] else {
        return false;
    };
    let ('"', [left, bottom, right, top]) = parsed[parsed.len() - 1] else {
        return false;
    };
    // A recognizer can omit a real closing glyph from both text and boxes.
    // Treat every nonwhite pixel beyond the terminal box as ambiguous.
    if (0..image.height()).any(|y| (right..image.width()).any(|x| image.get_pixel(x, y)[0] < 255)) {
        return false;
    }
    let width = right - left;
    let height = top - bottom;
    let question_height = qtop - qbottom;
    if width < 2
        || height < 6
        || u64::from(width) * u64::from(height) > 16_384
        || u64::from(height) < 2 * u64::from(width)
        || 5 * u64::from(height) < 4 * u64::from(question_height)
        || bottom.abs_diff(qbottom) > (question_height / 10).max(2)
        || qright > left
        || left - qright > question_height
    {
        return false;
    }

    // A clipped quote stroke may lie between the recognized boxes.
    if (0..image.height()).any(|y| (qright..left).any(|x| image.get_pixel(x, y)[0] < 255)) {
        return false;
    }

    // Require two solid, identically aligned rectangular bands, with no stray
    // ink. This deliberately abstains on partial or more complex shapes.
    let mut bands: Vec<(u32, u32)> = Vec::new();
    for y in image.height() - top..image.height() - bottom {
        let ink = (left..right)
            .filter(|x| image.get_pixel(*x, y)[0] < 128)
            .count() as u32;
        if ink != 0 && ink != width {
            return false;
        }
        let row = y - (image.height() - top);
        if ink == width {
            if let Some((_, end)) = bands.last_mut().filter(|(_, end)| *end == row) {
                *end += 1;
            } else {
                bands.push((row, row + 1));
                if bands.len() > 2 {
                    return false;
                }
            }
        }
    }
    if bands.len() != 2 || bands[0].0 != 0 || bands[1].1 != height {
        return false;
    }
    let stem = bands[0].1 - bands[0].0;
    let dot = bands[1].1 - bands[1].0;
    let gap = bands[1].0 - bands[0].1;
    dot >= 2
        && stem >= 2 * dot
        && gap > 0
        && gap <= height / 2
        && u64::from(width) * 2 >= u64::from(dot)
        && u64::from(width) <= 2 * u64::from(dot)
}

// These image-only checks are the frozen multi-threshold experiment. They do
// not establish what a mark means, and are not used by the default OCR path.
struct PunctuationRaster<'a> {
    pixels: &'a [u8],
    width: usize,
    height: usize,
}

#[derive(Clone, Copy)]
struct PunctuationInkBox {
    left: usize,
    right: usize,
    top: usize,
    bottom: usize,
}

impl<'a> PunctuationRaster<'a> {
    fn new(image: &'a image::GrayImage) -> Option<Self> {
        const MAX_SIDE: usize = 4096;
        const MAX_PIXELS: usize = 4_000_000;
        let width = usize::try_from(image.width()).ok()?;
        let height = usize::try_from(image.height()).ok()?;
        let area = width.checked_mul(height)?;
        if width == 0
            || height == 0
            || width > MAX_SIDE
            || height > MAX_SIDE
            || area > MAX_PIXELS
            || image.as_raw().len() != area
        {
            return None;
        }
        Some(Self {
            pixels: image.as_raw(),
            width,
            height,
        })
    }

    fn ink(&self, x: usize, y: usize, threshold: u8) -> bool {
        self.pixels[y * self.width + x] < threshold
    }

    fn column_runs(&self, top: usize, bottom: usize, threshold: u8) -> Vec<(usize, usize)> {
        let mut runs = Vec::new();
        let mut start = None;
        for x in 0..self.width {
            let occupied = (top..bottom).any(|y| self.ink(x, y, threshold));
            match (start, occupied) {
                (None, true) => start = Some(x),
                (Some(left), false) => {
                    runs.push((left, x));
                    start = None;
                }
                _ => {}
            }
        }
        if let Some(left) = start {
            runs.push((left, self.width));
        }
        runs
    }

    fn ink_box(&self, run: (usize, usize), threshold: u8) -> Option<PunctuationInkBox> {
        let mut top = self.height;
        let mut bottom = 0;
        for y in 0..self.height {
            if (run.0..run.1).any(|x| self.ink(x, y, threshold)) {
                top = top.min(y);
                bottom = y + 1;
            }
        }
        (top < bottom).then_some(PunctuationInkBox {
            left: run.0,
            right: run.1,
            top,
            bottom,
        })
    }

    fn single_8_connected_ink(&self, mark: PunctuationInkBox, threshold: u8) -> bool {
        const MAX_MARK_PIXELS: usize = 64 * 1024;
        let Some(width) = mark.right.checked_sub(mark.left) else {
            return false;
        };
        let Some(height) = mark.bottom.checked_sub(mark.top) else {
            return false;
        };
        let Some(area) = width.checked_mul(height) else {
            return false;
        };
        if width == 0
            || height == 0
            || mark.right > self.width
            || mark.bottom > self.height
            || area > MAX_MARK_PIXELS
        {
            return false;
        }
        let Some(first) =
            (0..area).find(|&i| self.ink(mark.left + i % width, mark.top + i / width, threshold))
        else {
            return false;
        };
        let mut visited = vec![false; area];
        let mut pending = vec![first];
        visited[first] = true;
        while let Some(i) = pending.pop() {
            let x = i % width;
            let y = i / width;
            for ny in y.saturating_sub(1)..=(y + 1).min(height - 1) {
                for nx in x.saturating_sub(1)..=(x + 1).min(width - 1) {
                    let next = ny * width + nx;
                    if !visited[next] && self.ink(mark.left + nx, mark.top + ny, threshold) {
                        visited[next] = true;
                        pending.push(next);
                    }
                }
            }
        }
        (0..area).all(|i| {
            visited[i] || !self.ink(mark.left + i % width, mark.top + i / width, threshold)
        })
    }

    fn equal_shapes(&self, a: PunctuationInkBox, b: PunctuationInkBox, threshold: u8) -> bool {
        let width = a.right - a.left;
        let height = a.bottom - a.top;
        width == b.right - b.left
            && height == b.bottom - b.top
            && (0..height).all(|dy| {
                (0..width).all(|dx| {
                    self.ink(a.left + dx, a.top + dy, threshold)
                        == self.ink(b.left + dx, b.top + dy, threshold)
                })
            })
    }
}

fn exact_three_dots_at_threshold(raster: &PunctuationRaster<'_>, threshold: u8) -> bool {
    let runs = raster.column_runs(0, raster.height, threshold);
    if runs.len() < 4 {
        return false;
    }
    let tail = &runs[runs.len() - 3..];
    let Some(first) = raster.ink_box(tail[0], threshold) else {
        return false;
    };
    let Some(second) = raster.ink_box(tail[1], threshold) else {
        return false;
    };
    let Some(third) = raster.ink_box(tail[2], threshold) else {
        return false;
    };
    let width = first.right - first.left;
    let height = first.bottom - first.top;
    if width.min(height) < 2
        || width.max(height) > 2 * width.min(height)
        || !raster.equal_shapes(first, second, threshold)
        || !raster.equal_shapes(first, third, threshold)
    {
        return false;
    }
    let gap1 = second.left - first.right;
    let gap2 = third.left - second.right;
    if gap1.min(gap2) < 1 || gap1.max(gap2) > 3 * width.max(height) {
        return false;
    }
    let lowest = first.bottom.max(second.bottom).max(third.bottom);
    let highest = first.bottom.min(second.bottom).min(third.bottom);
    if lowest - highest > 1 {
        return false;
    }

    // Count across the final dot band, including marks whose columns are joined
    // to earlier ink elsewhere in the image.
    let band_runs = raster.column_runs(third.top, third.bottom, threshold);
    let Some(&(last_left, last_right)) = band_runs.last() else {
        return false;
    };
    let template = PunctuationInkBox {
        left: last_left,
        right: last_right,
        top: third.top,
        bottom: third.bottom,
    };
    let mut count = 0;
    let mut next_left: Option<usize> = None;
    for &(left, right) in band_runs.iter().rev() {
        let run_width = right - left;
        if 4 * run_width < 3 * width || 4 * run_width > 5 * width {
            break;
        }
        if let Some(next) = next_left {
            let Some(gap) = next.checked_sub(right) else {
                return false;
            };
            if gap == 0 || gap > 3 * width.max(height) {
                break;
            }
        }
        let candidate = PunctuationInkBox {
            left,
            right,
            top: third.top,
            bottom: third.bottom,
        };
        if run_width != width || !raster.equal_shapes(candidate, template, threshold) {
            return false;
        }
        count += 1;
        next_left = Some(left);
    }
    count == 3
}

/// Returns whether the fixed pixel check finds exactly three terminal dots.
/// The caller must supply one text line; this does not establish punctuation.
pub(super) fn multi_threshold_exact_three_terminal_dots(image: &image::GrayImage) -> bool {
    let Some(raster) = PunctuationRaster::new(image) else {
        return false;
    };
    [80, 120, 160, 200]
        .into_iter()
        .all(|threshold| exact_three_dots_at_threshold(&raster, threshold))
}

fn comma_shape_at_threshold(raster: &PunctuationRaster<'_>, threshold: u8) -> bool {
    let runs = raster.column_runs(0, raster.height, threshold);
    if runs.len() < 2 {
        return false;
    }
    let Some(mark) = raster.ink_box(runs[runs.len() - 1], threshold) else {
        return false;
    };
    let gap = mark.left - runs[runs.len() - 2].1;
    let width = mark.right - mark.left;
    let height = mark.bottom - mark.top;
    if width.min(height) < 2
        || gap < 2
        || height * 5 < 6 * width
        || height > 3 * width
        || !raster.single_8_connected_ink(mark, threshold)
    {
        return false;
    }
    let mut top_sum = 0u64;
    let mut top_count = 0u64;
    let mut bottom_sum = 0u64;
    let mut bottom_count = 0u64;
    let midpoint = mark.top + height / 2;
    for y in mark.top..mark.bottom {
        for x in mark.left..mark.right {
            if raster.ink(x, y, threshold) {
                let (sum, count) = if y < midpoint {
                    (&mut top_sum, &mut top_count)
                } else {
                    (&mut bottom_sum, &mut bottom_count)
                };
                *sum += (x - mark.left) as u64;
                *count += 1;
            }
        }
    }
    if top_count == 0 || bottom_count == 0 {
        return false;
    }
    let shift = top_sum as f64 / top_count as f64 - bottom_sum as f64 / bottom_count as f64;
    shift >= 0.5f64.max(0.1 * width as f64)
}

/// Returns whether the fixed pixel check finds a connected terminal comma-like shape.
/// The caller must supply one text line; this does not establish punctuation.
pub(super) fn multi_threshold_terminal_comma_shape(image: &image::GrayImage) -> bool {
    let Some(raster) = PunctuationRaster::new(image) else {
        return false;
    };
    [80, 120, 160, 200]
        .into_iter()
        .all(|threshold| comma_shape_at_threshold(&raster, threshold))
}

#[cfg(test)]
mod multi_threshold_punctuation_tests {
    use super::*;

    fn blank() -> image::GrayImage {
        image::GrayImage::from_pixel(48, 18, image::Luma([255]))
    }

    fn fill(image: &mut image::GrayImage, left: u32, top: u32, width: u32, height: u32, level: u8) {
        for y in top..top + height {
            for x in left..left + width {
                image.put_pixel(x, y, image::Luma([level]));
            }
        }
    }

    fn dots(count: usize) -> image::GrayImage {
        let mut image = blank();
        fill(&mut image, 2, 2, 4, 8, 0);
        for index in 0..count {
            fill(&mut image, 10 + 4 * index as u32, 12, 2, 2, 0);
        }
        image
    }

    fn comma(detached: bool) -> image::GrayImage {
        let mut image = blank();
        fill(&mut image, 2, 2, 5, 9, 0);
        fill(&mut image, 21, 5, 2, 3, 0);
        fill(&mut image, 20, if detached { 9 } else { 8 }, 2, 3, 0);
        image
    }

    #[test]
    fn exact_three_rejects_two_four_and_trailing_noise() {
        assert!(multi_threshold_exact_three_terminal_dots(&dots(3)));
        assert!(!multi_threshold_exact_three_terminal_dots(&dots(2)));
        assert!(!multi_threshold_exact_three_terminal_dots(&dots(4)));
        let mut noise = dots(3);
        noise.put_pixel(26, 13, image::Luma([0]));
        assert!(!multi_threshold_exact_three_terminal_dots(&noise));
    }

    #[test]
    fn fourth_dot_joined_to_earlier_columns_is_still_counted() {
        let mut image = blank();
        fill(&mut image, 2, 2, 4, 8, 0);
        for left in [5, 9, 13, 17] {
            fill(&mut image, left, 12, 2, 2, 0);
        }
        assert!(!multi_threshold_exact_three_terminal_dots(&image));
    }

    #[test]
    fn a_similarly_sized_different_mark_abstains() {
        let mut image = blank();
        fill(&mut image, 2, 2, 3, 8, 0);
        fill(&mut image, 4, 12, 3, 4, 0);
        for left in [10, 16, 22] {
            fill(&mut image, left, 12, 4, 4, 0);
        }
        assert!(!multi_threshold_exact_three_terminal_dots(&image));
    }

    #[test]
    fn much_smaller_previous_mark_is_outside_the_fixed_dot_gate() {
        let mut image = blank();
        fill(&mut image, 2, 2, 3, 8, 0);
        fill(&mut image, 4, 13, 2, 2, 0);
        for left in [10, 16, 22] {
            fill(&mut image, left, 12, 4, 4, 0);
        }
        // This reproduces the frozen check; it does not certify the number of marks.
        assert!(multi_threshold_exact_three_terminal_dots(&image));
    }

    #[test]
    fn all_four_thresholds_must_agree_on_dot_shapes() {
        let mut image = blank();
        fill(&mut image, 2, 2, 3, 8, 0);
        for left in [10, 18, 26] {
            fill(&mut image, left + 1, 11, 2, 2, 0);
            for (dx, dy, level) in [(0, 0, 79), (3, 0, 119), (0, 3, 159), (3, 3, 199)] {
                image.put_pixel(left + dx, 10 + dy, image::Luma([level]));
            }
        }
        assert!(multi_threshold_exact_three_terminal_dots(&image));
        image.put_pixel(29, 13, image::Luma([255]));
        let raster = PunctuationRaster::new(&image).unwrap();
        assert!([80, 120, 160]
            .into_iter()
            .all(|threshold| exact_three_dots_at_threshold(&raster, threshold)));
        assert!(!exact_three_dots_at_threshold(&raster, 200));
        assert!(!multi_threshold_exact_three_terminal_dots(&image));
    }

    #[test]
    fn threshold_comparison_is_strict_at_the_boundary() {
        let mut image = dots(3);
        for left in [10, 14, 18] {
            fill(&mut image, left, 12, 2, 2, 79);
        }
        assert!(multi_threshold_exact_three_terminal_dots(&image));
        image.put_pixel(10, 12, image::Luma([80]));
        let raster = PunctuationRaster::new(&image).unwrap();
        assert!(!exact_three_dots_at_threshold(&raster, 80));
        assert!(exact_three_dots_at_threshold(&raster, 120));
        assert!(!multi_threshold_exact_three_terminal_dots(&image));
    }

    #[test]
    fn comma_shape_rejects_period_colon_and_rightward_tail() {
        assert!(multi_threshold_terminal_comma_shape(&comma(false)));
        let mut period = blank();
        fill(&mut period, 2, 2, 5, 9, 0);
        fill(&mut period, 20, 8, 3, 3, 0);
        assert!(!multi_threshold_terminal_comma_shape(&period));
        let mut colon = blank();
        fill(&mut colon, 2, 2, 5, 9, 0);
        fill(&mut colon, 20, 5, 2, 2, 0);
        fill(&mut colon, 20, 9, 2, 2, 0);
        assert!(!multi_threshold_terminal_comma_shape(&colon));
        let mut reverse = blank();
        fill(&mut reverse, 2, 2, 5, 9, 0);
        fill(&mut reverse, 20, 5, 2, 3, 0);
        fill(&mut reverse, 21, 8, 2, 3, 0);
        assert!(!multi_threshold_terminal_comma_shape(&reverse));
    }

    #[test]
    fn comma_requires_agreement_at_each_threshold() {
        let mut image = comma(false);
        image.put_pixel(30, 11, image::Luma([199]));
        let raster = PunctuationRaster::new(&image).unwrap();
        assert!([80, 120, 160]
            .into_iter()
            .all(|threshold| comma_shape_at_threshold(&raster, threshold)));
        assert!(!comma_shape_at_threshold(&raster, 200));
        assert!(!multi_threshold_terminal_comma_shape(&image));
    }

    #[test]
    fn disconnected_comma_like_ink_is_rejected() {
        let image = comma(true);
        let identical = image.clone();
        assert_eq!(image.as_raw(), identical.as_raw());
        assert!(!multi_threshold_terminal_comma_shape(&image));
        assert!(!multi_threshold_terminal_comma_shape(&identical));
    }

    #[test]
    fn comma_accepts_diagonal_eight_connected_ink() {
        let mut image = blank();
        fill(&mut image, 2, 2, 5, 9, 0);
        fill(&mut image, 21, 5, 2, 3, 0);
        fill(&mut image, 19, 8, 2, 3, 0);
        assert!(multi_threshold_terminal_comma_shape(&image));
    }

    #[test]
    fn comma_connectivity_must_hold_at_every_threshold() {
        let mut image = comma(true);
        image.put_pixel(21, 8, image::Luma([100]));
        let raster = PunctuationRaster::new(&image).unwrap();
        assert!(!comma_shape_at_threshold(&raster, 80));
        assert!([120, 160, 200]
            .into_iter()
            .all(|threshold| comma_shape_at_threshold(&raster, threshold)));
        assert!(!multi_threshold_terminal_comma_shape(&image));
    }

    #[test]
    fn connected_ink_search_is_bounded() {
        let allowed = image::GrayImage::from_pixel(256, 256, image::Luma([0]));
        let allowed_raster = PunctuationRaster::new(&allowed).unwrap();
        let allowed_mark = PunctuationInkBox {
            left: 0,
            right: 256,
            top: 0,
            bottom: 256,
        };
        assert!(allowed_raster.single_8_connected_ink(allowed_mark, 128));

        let oversized = image::GrayImage::from_pixel(257, 256, image::Luma([0]));
        let oversized_raster = PunctuationRaster::new(&oversized).unwrap();
        let oversized_mark = PunctuationInkBox {
            left: 0,
            right: 257,
            top: 0,
            bottom: 256,
        };
        assert!(!oversized_raster.single_8_connected_ink(oversized_mark, 128));
        for invalid in [
            PunctuationInkBox {
                left: 2,
                right: 1,
                top: 0,
                bottom: 1,
            },
            PunctuationInkBox {
                left: 0,
                right: 258,
                top: 0,
                bottom: 1,
            },
            PunctuationInkBox {
                left: 0,
                right: 1,
                top: 0,
                bottom: 257,
            },
        ] {
            assert!(!oversized_raster.single_8_connected_ink(invalid, 128));
        }
    }

    #[test]
    fn empty_tiny_and_oversized_images_fail_closed() {
        for image in [
            image::GrayImage::new(0, 0),
            image::GrayImage::new(1, 1),
            image::GrayImage::new(4097, 1),
            image::GrayImage::new(2001, 2000),
        ] {
            assert!(!multi_threshold_exact_three_terminal_dots(&image));
            assert!(!multi_threshold_terminal_comma_shape(&image));
        }
        assert!(PunctuationRaster::new(&image::GrayImage::new(4096, 1)).is_some());
        let extra = image::GrayImage::from_raw(2, 2, vec![0; 5]).unwrap();
        assert!(!multi_threshold_exact_three_terminal_dots(&extra));
        assert!(!multi_threshold_terminal_comma_shape(&extra));
    }
}

fn terminal_ellipsis_pixel_evidence(text: &str, boxes: &str, image: &image::GrayImage) -> bool {
    let Some(parsed) = parse_tesseract_boxes(boxes) else {
        return false;
    };
    if parsed.len() < 3
        || !text
            .chars()
            .filter(|c| !c.is_whitespace())
            .eq(parsed.iter().map(|(ch, _)| *ch))
        || !parsed[parsed.len() - 3..].iter().all(|(ch, _)| *ch == '.')
    {
        return false;
    }
    let (_, [_, bottom, right, top]) = parsed[parsed.len() - 1];
    if bottom >= top || top > image.height() || right > image.width() {
        return false;
    }
    let dot_height = top - bottom;
    let dot_boxes = &parsed[parsed.len() - 3..];
    if dot_boxes.iter().any(|(_, [left, lower, right, upper])| {
        left >= right
            || lower >= upper
            || *right > image.width()
            || *upper > image.height()
            || lower.abs_diff(bottom) > (dot_height / 4).max(2)
    }) || dot_boxes
        .windows(2)
        .any(|pair| pair[0].1[0] >= pair[1].1[0] || pair[0].1[2] > pair[1].1[2])
    {
        return false;
    }
    if dot_height < 2
        || u64::from(dot_height) * u64::from(image.width()) > 2_000_000
        || u64::from(image.width()) * u64::from(image.height()) > 4_000_000
    {
        return false;
    }
    let y_start = image.height() - top;
    let y_end = image.height() - bottom;
    let mut runs: Vec<(u32, u32, u32, u32)> = Vec::new();
    for x in 0..image.width() {
        let mut first = y_end;
        let mut last = y_start;
        for y in y_start..y_end {
            if image.get_pixel(x, y)[0] < 128 {
                first = first.min(y);
                last = last.max(y);
            }
        }
        if first == y_end {
            continue;
        }
        if let Some(run) = runs.last_mut().filter(|run| run.1 == x) {
            run.1 = x + 1;
            run.2 = run.2.min(first);
            run.3 = run.3.max(last);
        } else {
            runs.push((x, x + 1, first, last));
        }
    }
    if runs.len() < 3 {
        return false;
    }
    let dots = &runs[runs.len() - 3..];
    if dots.iter().any(|run| {
        !dot_boxes
            .iter()
            .any(|(_, [left, _, right, _])| run.0 < *right && *left < run.1)
    }) || dots[0].0 < dot_boxes[0].1[0]
        || dots[2].1 > dot_boxes[2].1[2]
    {
        return false;
    }
    // Do not count the bottom of a letter or a vertical stroke as a dot.
    // A blank row separates the current text line from any line above it.
    let mut line_top = y_start;
    while line_top > 0 && (0..image.width()).any(|x| image.get_pixel(x, line_top - 1)[0] < 128) {
        line_top -= 1;
    }
    if dots.iter().any(|run| {
        (run.0..run.1).any(|x| (line_top..y_start).any(|y| image.get_pixel(x, y)[0] < 128))
    }) || dots.windows(2).any(|pair| {
        (pair[0].1..pair[1].0).any(|x| (line_top..y_end).any(|y| image.get_pixel(x, y)[0] < 128))
    }) {
        return false;
    }
    let widths: Vec<_> = dots.iter().map(|run| run.1 - run.0).collect();
    let heights: Vec<_> = dots.iter().map(|run| run.3 - run.2 + 1).collect();
    let min_width = *widths.iter().min().unwrap();
    let max_width = *widths.iter().max().unwrap();
    let min_height = *heights.iter().min().unwrap();
    let max_height = *heights.iter().max().unwrap();
    let gap1 = dots[1].0 - dots[0].1;
    let gap2 = dots[2].0 - dots[1].1;
    min_width >= 2
        && min_height >= 2
        && u64::from(max_width) <= 2 * u64::from(dot_height)
        && max_height <= dot_height
        && u64::from(max_width) <= 2 * u64::from(min_width)
        && u64::from(max_height) <= 2 * u64::from(min_height)
        && dots.iter().all(|run| run.3.abs_diff(dots[0].3) <= 2)
        && gap1 > 0
        && gap2 > 0
        && u64::from(gap1.max(gap2)) <= 2 * u64::from(gap1.min(gap2))
}

fn terminal_period_pixel_evidence(text: &str, boxes: &str, image: &image::GrayImage) -> bool {
    let Some(parsed) = parse_tesseract_boxes(boxes) else {
        return false;
    };
    if u64::from(image.width()) * u64::from(image.height()) > 4_000_000
        || !text
            .chars()
            .filter(|ch| !ch.is_whitespace())
            .eq(parsed.iter().map(|(ch, _)| *ch))
        || parsed.iter().any(|(_, [left, bottom, right, top])| {
            left >= right || bottom >= top || *right > image.width() || *top > image.height()
        })
    {
        return false;
    }
    let mut offset = 0usize;
    let mut ranges = Vec::new();
    for line in text.lines() {
        let count = line.chars().filter(|ch| !ch.is_whitespace()).count();
        if count == 0 || offset + count > parsed.len() {
            return false;
        }
        let line_boxes = &parsed[offset..offset + count];
        let bottom = line_boxes.iter().map(|(_, b)| b[1]).min().unwrap();
        let top = line_boxes.iter().map(|(_, b)| b[3]).max().unwrap();
        ranges.push((offset, offset + count, bottom, top));
        offset += count;
    }
    if offset != parsed.len() || ranges.windows(2).any(|pair| pair[0].2 < pair[1].3) {
        return false;
    }
    let Some(&(start, end, bottom, top)) = ranges.last() else {
        return false;
    };
    if end - start < 4 || parsed[end - 1].0 != '.' || !parsed[end - 2].0.is_alphanumeric() {
        return false;
    }
    let mut baselines = Vec::new();
    let mut heights = Vec::new();
    for (ch, [_, lower, _, upper]) in &parsed[start..end - 1] {
        if ch.is_alphanumeric() {
            baselines.push(*lower);
            heights.push(*upper - *lower);
        }
    }
    if baselines.len() < 3 {
        return false;
    }
    baselines.sort_unstable();
    heights.sort_unstable();
    let baseline = baselines[baselines.len() / 2];
    let height = heights[heights.len() / 2];
    if height < 6 {
        return false;
    }
    let y0 = image.height() - top;
    let y1 = image.height() - bottom;
    let mut runs: Vec<(u32, u32, u32, u32, u32)> = Vec::new();
    for x in 0..image.width() {
        let mut first = y1;
        let mut last = y0;
        let mut count = 0;
        for y in y0..y1 {
            if image.get_pixel(x, y)[0] < 128 {
                first = first.min(y);
                last = last.max(y);
                count += 1;
            }
        }
        if count == 0 {
            continue;
        }
        if let Some(run) = runs.last_mut().filter(|run| run.1 == x) {
            run.1 = x + 1;
            run.2 = run.2.min(first);
            run.3 = run.3.max(last);
            run.4 += count;
        } else {
            runs.push((x, x + 1, first, last, count));
        }
    }
    if runs.len() < 2 {
        return false;
    }
    let dot = runs[runs.len() - 1];
    let previous = runs[runs.len() - 2];
    let [left, box_bottom, right, box_top] = parsed[end - 1].1;
    let width = dot.1 - dot.0;
    let dot_height = dot.3 - dot.2 + 1;
    let dot_bottom = image.height() - dot.3 - 1;
    let gap = dot.0 - previous.1;
    // Include a text-height margin to the left of the preceding run so that
    // clipped or detached comma tails below the detected line cause abstention.
    left <= dot.0
        && dot.1 <= right
        && box_bottom <= dot_bottom
        && box_top >= image.height() - dot.2
        && !(dot.1..image.width())
            .any(|x| (0..image.height()).any(|y| image.get_pixel(x, y)[0] < 245))
        && parsed[start..end - 1].iter().all(|(_, b)| b[2] <= dot.0)
        && width >= 2
        && dot_height >= 2
        && u64::from(width) <= 2 * u64::from(dot_height)
        && dot_height <= width
        && u64::from(dot_height) * 2 <= u64::from(height)
        && u64::from(dot.4) * 3 >= 2 * u64::from(width) * u64::from(dot_height)
        && period_shape_is_symmetric(image, dot.0, dot.1, dot.2, dot.3)
        && !(dot.0..dot.1).any(|x| (dot.3 + 1..y1).any(|y| image.get_pixel(x, y)[0] < 245))
        && dot_bottom.abs_diff(baseline) <= (height / 10).max(2)
        && gap >= 2
        && gap <= height
        && !(previous.1..dot.0).any(|x| (y0..y1).any(|y| image.get_pixel(x, y)[0] < 245))
        && !(previous.0.saturating_sub(height)..image.width())
            .any(|x| (y1..image.height()).any(|y| image.get_pixel(x, y)[0] < 245))
}

// Reject asymmetric tails, including tails inside a line extended by descenders.
fn period_shape_is_symmetric(
    image: &image::GrayImage,
    left: u32,
    right: u32,
    top: u32,
    bottom: u32,
) -> bool {
    let width = right - left;
    let height = bottom - top + 1;
    let mut mismatches = 0u64;
    let mut row_widths = Vec::with_capacity(height as usize);
    for y in 0..height {
        let mut first = None;
        let mut last = 0;
        let mut count = 0u32;
        for x in 0..width {
            let ink = image.get_pixel(left + x, top + y)[0] < 128;
            let opposite = image.get_pixel(right - 1 - x, bottom - y)[0] < 128;
            mismatches += u64::from(ink != opposite);
            if ink {
                first.get_or_insert(x);
                last = x;
                count += 1;
            }
        }
        if first.is_none_or(|first| last - first + 1 != count) {
            return false;
        }
        row_widths.push(count);
    }
    let area = u64::from(width) * u64::from(height);
    mismatches * 20 <= 3 * area
        && (area > 64 || mismatches == 0)
        && row_widths
            .iter()
            .zip(row_widths.iter().rev())
            .all(|(upper, lower)| upper.abs_diff(*lower) <= (width * 3 / 10).max(1))
}

// Only a clearly separate, downward-left slanting comma can pass. Character
// boxes locate the line; the punctuation's actual outline comes from the image.
fn line_final_comma_pixel_evidence(text: &str, boxes: &str, image: &image::GrayImage) -> bool {
    let Some(parsed) = parse_tesseract_boxes(boxes) else {
        return false;
    };
    let lines: Vec<_> = text
        .lines()
        .filter(|line| !line.trim().is_empty())
        .collect();
    if lines.len() != 2
        || !lines[0].trim_end().ends_with(',')
        || u64::from(image.width()) * u64::from(image.height()) > 4_000_000
        || !text
            .chars()
            .filter(|ch| !ch.is_whitespace())
            .eq(parsed.iter().map(|(ch, _)| *ch))
        || parsed
            .iter()
            .any(|(_, [l, b, r, t])| l >= r || b >= t || *r > image.width() || *t > image.height())
    {
        return false;
    }
    let first_len = lines[0].chars().filter(|ch| !ch.is_whitespace()).count();
    if first_len < 5
        || first_len >= parsed.len()
        || parsed[first_len - 1].0 != ','
        || !parsed[first_len - 2].0.is_ascii_alphanumeric()
        || matches!(
            parsed[first_len - 2].0.to_ascii_lowercase(),
            'g' | 'j' | 'p' | 'q' | 'y'
        )
    {
        return false;
    }
    let first = &parsed[..first_len];
    let second = &parsed[first_len..];
    let bottom = first.iter().map(|(_, b)| b[1]).min().unwrap();
    let second_top = second.iter().map(|(_, b)| b[3]).max().unwrap();
    if bottom <= second_top + 4 {
        return false;
    }
    let mut baselines = Vec::new();
    let mut heights = Vec::new();
    for (ch, [_, lower, _, upper]) in &first[..first_len - 1] {
        if ch.is_ascii_alphanumeric()
            && !matches!(ch.to_ascii_lowercase(), 'g' | 'j' | 'p' | 'q' | 'y')
        {
            baselines.push(*lower);
            heights.push(upper - lower);
        }
    }
    if baselines.len() < 4 {
        return false;
    }
    baselines.sort_unstable();
    heights.sort_unstable();
    let baseline = baselines[baselines.len() / 2];
    let height = heights[heights.len() / 2];
    if height < 12
        || baselines
            .iter()
            .filter(|b| b.abs_diff(baseline) <= (height / 10).max(2))
            .count()
            < 4
    {
        return false;
    }
    let y_end = image.height() - (bottom + second_top) / 2;
    let mut runs: Vec<(u32, u32, u32, u32)> = Vec::new();
    for x in 0..image.width() {
        let mut top = y_end;
        let mut bottom = 0;
        for y in 0..y_end {
            if image.get_pixel(x, y)[0] < 245 {
                top = top.min(y);
                bottom = bottom.max(y);
            }
        }
        if top == y_end {
            continue;
        }
        if let Some(run) = runs.last_mut().filter(|run| run.1 == x) {
            run.1 = x + 1;
            run.2 = run.2.min(top);
            run.3 = run.3.max(bottom);
        } else {
            runs.push((x, x + 1, top, bottom));
        }
    }
    if runs.len() < 2 {
        return false;
    }
    let comma = runs[runs.len() - 1];
    let previous = runs[runs.len() - 2];
    let [cl, cb, cr, ct] = first[first_len - 1].1;
    let [pl, _, pr, _] = first[first_len - 2].1;
    let width = comma.1 - comma.0;
    let shape_height = comma.3 - comma.2 + 1;
    let comma_bottom = image.height() - comma.3 - 1;
    if cl > comma.0
        || cr < comma.1
        || cb > comma_bottom
        || ct < image.height() - comma.2
        || first[..first_len - 1].iter().any(|(_, b)| b[2] > comma.0)
        || previous.0 >= pr
        || previous.1 <= pl
        || previous.1 > comma.0
        || (image.height() - previous.3 - 1).abs_diff(baseline) > (height / 10).max(2)
        || comma.0 - previous.1 < 2
        || comma.0 - previous.1 > height / 2
        || width < 4
        || shape_height < 6
        || shape_height * 5 < width * 6
        || shape_height > height
        || comma_bottom >= baseline
        || baseline - comma_bottom < (height / 10).max(2)
        || baseline - comma_bottom > height / 3
        || image.height() - comma.2 <= baseline + height / 5
    {
        return false;
    }
    // Require one connected, contiguous stroke with a clear leftward tail.
    let mut upper_x = 0u64;
    let mut upper_n = 0u64;
    let mut lower_x = 0u64;
    let mut lower_n = 0u64;
    let mut previous_row: Option<(u32, u32)> = None;
    let mut ink = 0u64;
    for y in comma.2..=comma.3 {
        let xs: Vec<_> = (comma.0..comma.1)
            .filter(|&x| image.get_pixel(x, y)[0] < 128)
            .collect();
        if xs.is_empty() {
            return false;
        }
        let (left, right) = (*xs.first().unwrap(), *xs.last().unwrap());
        if right - left + 1 != xs.len() as u32
            || previous_row.is_some_and(|(l, r)| left > r + 1 || l > right + 1)
        {
            return false;
        }
        previous_row = Some((left, right));
        ink += xs.len() as u64;
        for x in xs {
            if y < comma.2 + shape_height / 4 {
                upper_x += u64::from(x);
                upper_n += 1;
            }
            if y >= comma.3 + 1 - shape_height / 4 {
                lower_x += u64::from(x);
                lower_n += 1;
            }
        }
    }
    upper_n > 0
        && lower_n > 0
        && upper_n * 4 >= lower_n * 5
        && ink * 3 >= u64::from(width) * u64::from(shape_height)
        && upper_x * lower_n > lower_x * upper_n + u64::from((width / 5).max(2)) * upper_n * lower_n
}

pub(super) fn tesseract_line_final_comma_evidence(
    image_path: &Path,
    language: &str,
    text: &str,
) -> bool {
    match (
        run_tesseract_boxes(image_path, language, 6),
        image::open(image_path),
    ) {
        (Ok(boxes), Ok(image)) => line_final_comma_pixel_evidence(text, &boxes, &image.to_luma8()),
        _ => false,
    }
}

pub(super) fn tesseract_terminal_period_evidence(
    image_path: &Path,
    language: &str,
    psm: u8,
    text: &str,
) -> bool {
    match (
        run_tesseract_boxes(image_path, language, psm),
        image::open(image_path),
    ) {
        (Ok(boxes), Ok(image)) => terminal_period_pixel_evidence(text, &boxes, &image.to_luma8()),
        _ => false,
    }
}

fn comma_spacing_pixel_evidence(
    text: &str,
    boxes: &str,
    image: &image::GrayImage,
) -> HashSet<usize> {
    let mut supported = HashSet::new();
    let Some(parsed) = parse_tesseract_boxes(boxes) else {
        return supported;
    };
    if !text
        .chars()
        .filter(|ch| !ch.is_whitespace())
        .eq(parsed.iter().map(|(ch, _)| *ch))
        || u64::from(image.width()) * u64::from(image.height()) > 4_000_000
    {
        return supported;
    }
    let mut line_ranges = Vec::new();
    let mut offset = 0usize;
    for line in text.lines() {
        let length = line.chars().filter(|ch| !ch.is_whitespace()).count();
        if length == 0 {
            return supported;
        }
        let line_boxes = &parsed[offset..offset + length];
        let bottom = line_boxes
            .iter()
            .map(|(_, bounds)| bounds[1])
            .min()
            .unwrap();
        let top = line_boxes
            .iter()
            .map(|(_, bounds)| bounds[3])
            .max()
            .unwrap();
        if bottom >= top || top > image.height() {
            return supported;
        }
        line_ranges.push((bottom, top));
        offset += length;
    }
    // Tesseract uses bottom-origin coordinates, so upper lines have larger y.
    if line_ranges.windows(2).any(|pair| pair[0].0 < pair[1].1) {
        return supported;
    }
    let box_has_ink = |left: u32, bottom: u32, right: u32, top: u32| {
        (left..right).any(|x| {
            (image.height() - top..image.height() - bottom).any(|y| image.get_pixel(x, y)[0] < 245)
        })
    };
    let mut index = 0usize;
    for line in text.lines() {
        let mut previous = None;
        let mut whitespace = false;
        for ch in line.chars() {
            if ch.is_whitespace() {
                whitespace = true;
                continue;
            }
            if whitespace && previous == Some(',') && ch.is_alphabetic() && index > 0 {
                let [comma_left, comma_bottom, comma_right, comma_top] = parsed[index - 1].1;
                let [next_left, next_bottom, next_right, next_top] = parsed[index].1;
                if comma_left < comma_right
                    && comma_bottom < comma_top
                    && next_bottom < next_top
                    && comma_top <= image.height()
                    && next_top <= image.height()
                    && comma_right < next_left
                    && next_left < next_right
                    && next_right <= image.width()
                    && box_has_ink(comma_left, comma_bottom, comma_right, comma_top)
                    && box_has_ink(next_left, next_bottom, next_right, next_top)
                {
                    let gap = next_left - comma_right;
                    let height = next_top - next_bottom;
                    let overlap = comma_top
                        .min(next_top)
                        .saturating_sub(comma_bottom.max(next_bottom));
                    if gap >= (height / 4).max(2)
                        && u64::from(overlap) * 3 >= u64::from(height)
                        && !(comma_right..next_left).any(|x| {
                            (image.height() - comma_top.max(next_top)
                                ..image.height() - comma_bottom.min(next_bottom))
                                .any(|y| image.get_pixel(x, y)[0] < 245)
                        })
                    {
                        supported.insert(index);
                    }
                }
            }
            index += 1;
            previous = Some(ch);
            whitespace = false;
        }
    }
    supported
}

pub(super) fn tesseract_terminal_ellipsis_evidence(
    image_path: &Path,
    language: &str,
    psm: u8,
    text: &str,
) -> Option<HashSet<usize>> {
    let boxes = run_tesseract_boxes(image_path, language, psm).ok()?;
    let image = image::open(image_path).ok()?.to_luma8();
    terminal_ellipsis_pixel_evidence(text, &boxes, &image)
        .then(|| comma_spacing_pixel_evidence(text, &boxes, &image))
}

fn recover_straight_quote_from_boxes(text: &str, boxes: &str, image: &image::GrayImage) -> String {
    if text.contains(['\n', '\r']) {
        return text.to_string();
    }
    let Some(parsed) = parse_tesseract_boxes(boxes) else {
        return text.to_string();
    };
    if !text
        .chars()
        .filter(|c| !c.is_whitespace())
        .eq(parsed.iter().map(|(c, _)| *c))
    {
        return text.to_string();
    }
    let quotes: Vec<_> = parsed
        .iter()
        .filter(|(ch, _)| matches!(ch, '"' | '“' | '”'))
        .collect();
    if quotes.len() != 2 || quotes.iter().filter(|(ch, _)| *ch == '"').count() != 1 {
        return text.to_string();
    }
    let Some(first) = straight_quote_mask(image, quotes[0].1) else {
        return text.to_string();
    };
    let Some(second) = straight_quote_mask(image, quotes[1].1) else {
        return text.to_string();
    };
    if first != second {
        return text.to_string();
    }
    text.chars()
        .map(|ch| if matches!(ch, '“' | '”') { '"' } else { ch })
        .collect()
}

// A straight double quote has the same two vertical strokes in every row.
// Trim box padding, then require matching masks for both recognized glyphs.
fn straight_quote_mask(
    image: &image::GrayImage,
    [left, bottom, right, top]: [u32; 4],
) -> Option<(usize, Vec<bool>)> {
    if left >= right || bottom >= top || right > image.width() || top > image.height() {
        return None;
    }
    let rows: Vec<Vec<bool>> = (image.height() - top..image.height() - bottom)
        .map(|y| {
            (left..right)
                .map(|x| image.get_pixel(x, y)[0] < 128)
                .collect()
        })
        .collect();
    let first = rows.first()?;
    let start = first.iter().position(|ink| *ink)?;
    let end = first.iter().rposition(|ink| *ink)? + 1;
    let mask = &first[start..end];
    if rows.len() < 3
        || rows.iter().any(|row| {
            row[start..end] != *mask
                || row[..start].iter().any(|v| *v)
                || row[end..].iter().any(|v| *v)
        })
    {
        return None;
    }
    let runs = mask
        .iter()
        .enumerate()
        .filter(|(i, v)| **v && (*i == 0 || !mask[*i - 1]))
        .count();
    if runs != 2 {
        return None;
    }
    // Include height so an unrelated taller glyph cannot match.
    let mut result = Vec::with_capacity(mask.len() * rows.len());
    for _ in &rows {
        result.extend_from_slice(mask);
    }
    Some((mask.len(), result))
}

pub(super) fn run_external_ocr_command(
    image_path: &Path,
    language: &str,
    ocr_external_command: &str,
) -> Result<String> {
    let argv = parse_external_ocr_argv(ocr_external_command)?;
    let mut cmd = Command::new(&argv[0]);
    cmd.args(&argv[1..]);

    let output = cmd
        .env("DPN_OCR_IMAGE", image_path)
        .env("DPN_OCR_LANGUAGE", language)
        .output()
        .with_context(|| {
            format!(
                "running OCR external command on '{}' with DPN_OCR_LANGUAGE={}",
                image_path.display(),
                language
            )
        })?;

    if !output.status.success() {
        bail!(
            "OCR external command failed for '{}': {}",
            image_path.display(),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(normalize_utf8_text(&String::from_utf8_lossy(
        &output.stdout,
    )))
}

pub(super) fn parse_external_ocr_argv(ocr_external_command: &str) -> Result<Vec<String>> {
    let argv = shlex::split(ocr_external_command).ok_or_else(|| {
        anyhow!("Invalid --ocr-external-command value: could not parse command/arguments safely")
    })?;
    if argv.is_empty() {
        bail!("--ocr-external-command must include a program name");
    }
    Ok(argv)
}

pub(super) fn rect_to_pgm(
    rect: &ffi::AVSubtitleRect,
    ocr_engine: OcrEngine,
) -> Option<(Vec<u8>, bool)> {
    if rect.w <= 0 || rect.h <= 0 || rect.data[0].is_null() {
        return None;
    }

    let width = rect.w as usize;
    let height = rect.h as usize;
    let stride = rect.linesize[0].max(0) as usize;
    if stride < width {
        return None;
    }

    let pixels = unsafe { std::slice::from_raw_parts(rect.data[0], stride * height) };

    let palette = if !rect.data[1].is_null() {
        Some(unsafe { std::slice::from_raw_parts(rect.data[1], 256 * 4) })
    } else {
        None
    };

    let mut raster = Vec::with_capacity(width * height);
    let mut has_visible_pixels = false;
    let mut strong_foreground_pixels = 0usize;
    let ai_mode = matches!(ocr_engine, OcrEngine::PpOcrV3 | OcrEngine::PpOcrV4);

    for y in 0..height {
        let row = &pixels[y * stride..(y * stride + width)];
        for &idx in row {
            let value = if let Some(pal) = palette {
                let base = (idx as usize) * 4;
                if base + 3 >= pal.len() {
                    255u8
                } else {
                    // Palette layout is RGBA for these subtitle codecs in FFmpeg.
                    let r = pal[base] as u16;
                    let g = pal[base + 1] as u16;
                    let b = pal[base + 2] as u16;
                    let a = pal[base + 3] as u16;

                    if a > 16 {
                        has_visible_pixels = true;
                        let luma = ((77 * r + 150 * g + 29 * b) >> 8) as u8;
                        if ai_mode {
                            // Preserve grayscale detail for PP-OCR while emphasizing bright, opaque glyph cores.
                            let ink = ((luma as u16 * a + 127) / 255) as u8;
                            let value = 255u8.saturating_sub(ink);
                            if value < 220 {
                                strong_foreground_pixels += 1;
                            }
                            value
                        } else {
                            // Tesseract path: binarized foreground with mild antialias.
                            if luma >= 160 {
                                strong_foreground_pixels += 1;
                                0u8
                            } else if luma >= 95 {
                                strong_foreground_pixels += 1;
                                64u8
                            } else {
                                255u8
                            }
                        }
                    } else {
                        255u8
                    }
                }
            } else if idx > 0 {
                has_visible_pixels = true;
                strong_foreground_pixels += 1;
                0u8
            } else {
                255u8
            };
            raster.push(value);
        }
    }

    // Fallback: if luma filtering removed too much, use alpha-only occupancy mask.
    if has_visible_pixels && strong_foreground_pixels == 0 {
        raster.clear();
        for y in 0..height {
            let row = &pixels[y * stride..(y * stride + width)];
            for &idx in row {
                let value = if let Some(pal) = palette {
                    let base = (idx as usize) * 4;
                    if base + 3 < pal.len() && pal[base + 3] > 16 {
                        0u8
                    } else {
                        255u8
                    }
                } else if idx > 0 {
                    0u8
                } else {
                    255u8
                };
                raster.push(value);
            }
        }
    }

    let (final_raster, final_w, final_h) = if ai_mode {
        let scale = env::var("DPN_OCR_PGM_SCALE")
            .ok()
            .and_then(|v| v.trim().parse::<usize>().ok())
            .filter(|v| (1..=6).contains(v))
            .unwrap_or(2);
        upscale_grayscale_nearest(&raster, width, height, scale)
    } else {
        (raster, width, height)
    };
    let header = format!("P5\n{} {}\n255\n", final_w, final_h);
    let mut out = Vec::with_capacity(final_raster.len() + header.len());
    out.extend_from_slice(header.as_bytes());
    out.extend_from_slice(&final_raster);
    Some((out, has_visible_pixels))
}

pub(super) fn upscale_grayscale_nearest(
    raster: &[u8],
    width: usize,
    height: usize,
    factor: usize,
) -> (Vec<u8>, usize, usize) {
    if factor <= 1 || width == 0 || height == 0 {
        return (raster.to_vec(), width, height);
    }

    let out_w = width * factor;
    let out_h = height * factor;
    let mut out = vec![255u8; out_w * out_h];
    for y in 0..height {
        for x in 0..width {
            let value = raster[y * width + x];
            let base_y = y * factor;
            let base_x = x * factor;
            for dy in 0..factor {
                let out_row = (base_y + dy) * out_w;
                for dx in 0..factor {
                    out[out_row + base_x + dx] = value;
                }
            }
        }
    }
    (out, out_w, out_h)
}

fn pgm_header_len(pgm: &[u8]) -> Option<(usize, usize, usize)> {
    if !pgm.starts_with(b"P5") {
        return None;
    }
    let mut i = 2usize;
    let mut tokens = Vec::new();
    while i < pgm.len() && tokens.len() < 3 {
        while i < pgm.len() && pgm[i].is_ascii_whitespace() {
            i += 1;
        }
        if i >= pgm.len() {
            return None;
        }
        let start = i;
        while i < pgm.len() && !pgm[i].is_ascii_whitespace() {
            i += 1;
        }
        tokens.push(
            std::str::from_utf8(&pgm[start..i])
                .ok()?
                .parse::<usize>()
                .ok()?,
        );
    }
    if i < pgm.len() && pgm[i].is_ascii_whitespace() {
        i += 1;
    }
    if tokens.len() != 3 || tokens[2] != 255 {
        return None;
    }
    Some((tokens[0], tokens[1], i))
}

fn crop_pgm(
    pixels: &[u8],
    width: usize,
    left: usize,
    top: usize,
    right: usize,
    bottom: usize,
) -> Vec<u8> {
    let crop_w = right.saturating_sub(left).max(1);
    let crop_h = bottom.saturating_sub(top).max(1);
    let mut out = format!(
        "P5
{} {}
255
",
        crop_w, crop_h
    )
    .into_bytes();
    for y in top..bottom {
        let row = y * width;
        out.extend_from_slice(&pixels[row + left..row + right]);
    }
    out
}

#[derive(Clone, Copy)]
struct PgsSpatialInkGroup {
    left: usize,
    right: usize,
    top: usize,
    bottom: usize,
    count: usize,
}

fn overlapping_spatial_bands(
    pixels: &[u8],
    stride: usize,
    width: usize,
    top: usize,
    bottom: usize,
    is_visible: impl Fn(u8) -> bool,
) -> bool {
    let height = bottom - top;
    let mut columns = vec![(height, 0usize, 0usize); width];
    for y in top..bottom {
        for (column, &index) in columns
            .iter_mut()
            .zip(&pixels[y * stride..y * stride + width])
        {
            if is_visible(index) {
                column.0 = column.0.min(y - top);
                column.1 = y - top + 1;
                column.2 += 1;
            }
        }
    }

    let merge_gap = (height / 4).max(1);
    let mut groups: Vec<PgsSpatialInkGroup> = Vec::new();
    for (x, &(column_top, column_bottom, count)) in columns.iter().enumerate() {
        if count == 0 {
            continue;
        }
        match groups.last_mut() {
            Some(group) if x - group.right <= merge_gap => {
                group.right = x + 1;
                group.top = group.top.min(column_top);
                group.bottom = group.bottom.max(column_bottom);
                group.count += count;
            }
            _ => groups.push(PgsSpatialInkGroup {
                left: x,
                right: x + 1,
                top: column_top,
                bottom: column_bottom,
                count,
            }),
        }
    }

    let mut previous: Option<PgsSpatialInkGroup> = None;
    for group in groups {
        let group_height = group.bottom - group.top;
        if group_height < height.div_ceil(2).max(6)
            || group.right - group.left < group_height.div_ceil(3).max(3)
            || group.count < (2 * group_height).max(12)
        {
            continue;
        }
        if let Some(first) = previous {
            let shorter = (first.bottom - first.top).min(group_height);
            let top_shift = first.top.abs_diff(group.top);
            let bottom_shift = first.bottom.abs_diff(group.bottom);
            let edge_shift = shorter.div_ceil(4).max(2);
            let same_direction = (first.top < group.top && first.bottom < group.bottom)
                || (first.top > group.top && first.bottom > group.bottom);
            if first.top.max(group.top) < first.bottom.min(group.bottom)
                && group.left - first.right >= shorter.max(4)
                && same_direction
                && top_shift >= edge_shift
                && bottom_shift >= edge_shift
                && top_shift + bottom_shift >= shorter
            {
                return true;
            }
        }
        previous = Some(group);
    }
    false
}

/// Partition an FFmpeg bitmap into full-width, PGM-local line bands.
///
/// A continuous ink band is retained only when the bounded ambiguity checks
/// pass; those checks cannot prove it contains one text line. Abstention requests
/// detector fallback, which may still merge lines. Horizontal ink bridges can
/// conceal the spatial separation and evade this check.
///
/// # Safety
///
/// A non-null `rect.data[0]` must point to one initialized, readable allocation
/// containing at least `rect.linesize[0] * rect.h` bytes when the stride and
/// height are positive. A non-null `rect.data[1]` must point to one initialized,
/// readable allocation containing at least `4 * rect.nb_colors` bytes when
/// the color count is positive. Both buffers must remain alive and unmodified
/// for the duration of this call. For FFmpeg-owned buffers, keep the owning
/// subtitle alive and prevent FFmpeg from freeing or mutating it during the call.
/// Null pointers are permitted: a null index pointer is rejected, and a null
/// palette uses the nonpalette convention.
pub(super) unsafe fn split_pgs_bitmap_lines(
    rect: &ffi::AVSubtitleRect,
    pgm: &[u8],
) -> Option<Vec<(Vec<u8>, OcrBoundingBox)>> {
    const MAX_SOURCE_SIDE: usize = 4096;
    const MAX_SOURCE_PIXELS: usize = 4 * 1024 * 1024;
    const MAX_PGM_PIXELS: usize = 16 * 1024 * 1024;
    const MAX_CROP_PIXELS: usize = 4 * 1024 * 1024;
    const MAX_CROPS: usize = 16;

    if rect.type_ != ffi::SUBTITLE_BITMAP || rect.w <= 0 || rect.h <= 0 || rect.data[0].is_null() {
        return None;
    }
    let width = usize::try_from(rect.w).ok()?;
    let height = usize::try_from(rect.h).ok()?;
    let stride = usize::try_from(rect.linesize[0]).ok()?;
    if stride < width || !pgm.get(2).is_some_and(u8::is_ascii_whitespace) {
        return None;
    }
    let (pgm_width, pgm_height, header_len) = pgm_header_len(pgm)?;
    if header_len <= 2 || !pgm.get(header_len - 1).is_some_and(u8::is_ascii_whitespace) {
        return None;
    }
    let raster_len = pgm_width.checked_mul(pgm_height)?;
    if pgm_width == 0
        || pgm_height == 0
        || header_len.checked_add(raster_len)? != pgm.len()
        || pgm_width % width != 0
        || pgm_height % height != 0
    {
        return None;
    }
    let scale = pgm_width / width;
    let pixel_count = width.checked_mul(height)?;
    if scale == 0
        || pgm_height / height != scale
        || width > MAX_SOURCE_SIDE
        || height > MAX_SOURCE_SIDE
        || pixel_count > MAX_SOURCE_PIXELS
        || raster_len > MAX_PGM_PIXELS
        || pgm_width > MAX_SOURCE_SIDE
    {
        return None;
    }
    let right = i32::try_from(pgm_width).ok()?;
    i32::try_from(pgm_height).ok()?;
    let source_len = stride.checked_mul(height)?;
    if source_len > isize::MAX as usize || source_len > MAX_SOURCE_PIXELS * 4 {
        return None;
    }
    (rect.data[0] as usize).checked_add(source_len)?;
    // SAFETY: the caller guarantees an initialized index allocation covering
    // the declared stride and height; the length arithmetic was checked.
    let pixels = unsafe { std::slice::from_raw_parts(rect.data[0], source_len) };
    let palette = if rect.data[1].is_null() {
        None
    } else {
        let colors = usize::try_from(rect.nb_colors).ok()?;
        if colors == 0 || colors > 256 {
            return None;
        }
        let palette_len = colors.checked_mul(4)?;
        (rect.data[1] as usize).checked_add(palette_len)?;
        // SAFETY: the caller guarantees that the declared palette entries are readable.
        Some(unsafe { std::slice::from_raw_parts(rect.data[1], palette_len) })
    };

    let mut row_counts = vec![0usize; height];
    let mut visible_pixels = 0usize;
    for (y, count) in row_counts.iter_mut().enumerate() {
        for &index in &pixels[y * stride..y * stride + width] {
            let visible = match palette {
                Some(palette) => *palette.get(usize::from(index) * 4 + 3)? != 0,
                None => index != 0,
            };
            if visible {
                *count += 1;
                visible_pixels += 1;
            }
        }
    }
    if visible_pixels == 0 || visible_pixels == pixel_count {
        return None;
    }

    let is_visible = |index: u8| match palette {
        Some(palette) => palette[usize::from(index) * 4 + 3] != 0,
        None => index != 0,
    };
    let mut runs: Vec<(usize, usize)> = Vec::new();
    let mut y = 0;
    while y < height {
        if row_counts[y] == 0 {
            y += 1;
            continue;
        }
        let start = y;
        while y < height && row_counts[y] != 0 {
            y += 1;
        }
        runs.push((start, y));
    }
    // Two substantial runs with only a tiny blank gap may be distinct lines.
    if runs.windows(2).any(|pair| {
        pair[0].1 - pair[0].0 >= 4 && pair[1].1 - pair[1].0 >= 4 && pair[1].0 - pair[0].1 <= 2
    }) {
        return None;
    }
    for &(start, end) in &runs {
        if end - start < 8 {
            continue;
        }
        // Nearly uniform row masks can represent touching bands just as easily
        // as a tall glyph. Find a majority candidate without storing masks;
        // the source pixel and dimension limits bound all row comparisons.
        let same_mask = |a: usize, b: usize| {
            row_counts[a] == row_counts[b]
                && pixels[a * stride..a * stride + width]
                    .iter()
                    .zip(&pixels[b * stride..b * stride + width])
                    .all(|(&x, &y)| is_visible(x) == is_visible(y))
        };
        let (mut reference, mut votes) = (start, 0usize);
        for row in start..end {
            if votes == 0 {
                reference = row;
                votes = 1;
            } else if same_mask(reference, row) {
                votes += 1;
            } else {
                votes -= 1;
            }
        }
        // Allow at most one eighth of the rows (minimum two) to differ by at
        // most two visible pixels each; verify every row against the candidate.
        let allowed_noisy_rows = ((end - start) / 8).max(2);
        let first = &pixels[reference * stride..reference * stride + width];
        let mut noisy_rows = 0;
        let almost_flat = (start..end).all(|row| {
            if row_counts[row].abs_diff(row_counts[reference]) > 2 {
                return false;
            }
            let differences = first
                .iter()
                .zip(&pixels[row * stride..row * stride + width])
                .filter(|&(&a, &b)| is_visible(a) != is_visible(b))
                .take(3)
                .count();
            if differences > 2 {
                return false;
            }
            if differences != 0 {
                noisy_rows += 1;
            }
            noisy_rows <= allowed_noisy_rows
        });
        if almost_flat {
            return None;
        }
        // A missing or very narrow connection between substantial sections
        // can be two touching rows of text rather than one line.
        for row in start + 3..end - 4 {
            let upper = &pixels[row * stride..row * stride + width];
            let lower = &pixels[(row + 1) * stride..(row + 1) * stride + width];
            let connections = (0..width)
                .filter(|&x| {
                    is_visible(upper[x])
                        && lower[x.saturating_sub(1)..=(x + 1).min(width - 1)]
                            .iter()
                            .any(|&index| is_visible(index))
                })
                .count();
            let smaller_row = row_counts[row].min(row_counts[row + 1]);
            if connections == 0 || (smaller_row >= 4 && connections <= (smaller_row / 8).max(1)) {
                return None;
            }
        }
        // A narrow occupied bridge between dense sections is also ambiguous.
        let peak = *row_counts[start..end].iter().max()?;
        if peak >= 8 {
            let dense = peak.div_ceil(2);
            let valley = (peak / 8).max(1);
            let (mut dense_run, mut before, mut bridged, mut after) =
                (0usize, false, false, 0usize);
            for &count in &row_counts[start..end] {
                if before && count <= valley {
                    bridged = true;
                    after = 0;
                } else if bridged {
                    after = if count >= dense { after + 1 } else { 0 };
                    if after >= 4 {
                        return None;
                    }
                }
                if !bridged {
                    dense_run = if count >= dense { dense_run + 1 } else { 0 };
                    before |= dense_run >= 4;
                }
            }
        }
    }
    let cores: Vec<usize> = runs
        .iter()
        .enumerate()
        .filter_map(|(i, &(start, end))| (end - start >= 4).then_some(i))
        .collect();
    if cores.is_empty() || cores.len() > MAX_CROPS {
        return None;
    }
    let mut spans: Vec<(usize, usize)> = cores.iter().map(|&i| runs[i]).collect();
    for &(start, end) in runs.iter().filter(|&&(start, end)| end - start < 4) {
        let mut nearest: Option<(usize, usize)> = None;
        let mut tied = false;
        let insertion = cores.partition_point(|&i| runs[i].0 < start);
        // A farther core can have a larger attachment limit, so inspect every
        // core within the maximum limit on each side, not just the nearest.
        let before = (0..insertion)
            .rev()
            .take_while(|&position| start.saturating_sub(runs[cores[position]].1) <= 8);
        let after = (insertion..cores.len())
            .take_while(|&position| runs[cores[position]].0.saturating_sub(end) <= 8);
        for position in before.chain(after) {
            let (core_start, core_end) = runs[cores[position]];
            let gap = core_start
                .saturating_sub(end)
                .max(start.saturating_sub(core_end));
            let limit = ((core_end - core_start) / 2).clamp(4, 8);
            if gap > limit {
                continue;
            }
            match nearest {
                None => nearest = Some((gap, position)),
                Some((best, _)) if gap < best => {
                    nearest = Some((gap, position));
                    tied = false;
                }
                Some((best, _)) if gap == best => tied = true,
                _ => {}
            }
        }
        let (_, position) = nearest?;
        if tied {
            return None;
        }
        spans[position].0 = spans[position].0.min(start);
        spans[position].1 = spans[position].1.max(end);
    }
    spans.sort_unstable();
    if spans
        .windows(2)
        .any(|pair| pair[0].1 > pair[1].0 || pair[1].0 - pair[0].1 <= 2)
    {
        return None;
    }
    let mut span_index = 0;
    for (row, &count) in row_counts.iter().enumerate() {
        while span_index < spans.len() && spans[span_index].1 <= row {
            span_index += 1;
        }
        if count != 0
            && (span_index == spans.len()
                || row < spans[span_index].0
                || row >= spans[span_index].1)
        {
            return None;
        }
    }

    // Spans are disjoint, so these column scans remain bounded by the source
    // pixel limit and require only O(width) temporary storage per span.
    if spans.iter().any(|&(top, bottom)| {
        overlapping_spatial_bands(pixels, stride, width, top, bottom, is_visible)
    }) {
        return None;
    }

    // Reject oversized crops before allocating any of their output buffers.
    for &(top, bottom) in &spans {
        let scaled_top = top.checked_mul(scale)?;
        let scaled_bottom = bottom.checked_mul(scale)?;
        let crop_height = scaled_bottom.checked_sub(scaled_top)?;
        if crop_height == 0
            || crop_height > MAX_SOURCE_SIDE
            || pgm_width.checked_mul(crop_height)? > MAX_CROP_PIXELS
        {
            return None;
        }
    }
    let raster = &pgm[header_len..];
    let mut crops = Vec::with_capacity(spans.len());
    for (top, bottom) in spans {
        let top = top.checked_mul(scale)?;
        let bottom = bottom.checked_mul(scale)?;
        crops.push((
            crop_pgm(raster, pgm_width, 0, top, pgm_width, bottom),
            OcrBoundingBox {
                left: 0,
                right,
                top: i32::try_from(top).ok()?,
                bottom: i32::try_from(bottom).ok()?,
            },
        ));
    }
    Some(crops)
}

#[cfg(test)]
mod pgs_bitmap_line_tests {
    use super::*;

    struct Bitmap {
        rect: ffi::AVSubtitleRect,
        pixels: Vec<u8>,
        palette: Option<Vec<u8>>,
        pgm: Vec<u8>,
        width: usize,
        height: usize,
        stride: usize,
        scale: usize,
    }

    impl Bitmap {
        fn new(width: usize, height: usize, stride: usize, scale: usize, palette: bool) -> Self {
            assert!(width > 0 && height > 0 && stride >= width && scale > 0);
            let mut pixels = vec![0; stride * height];
            let mut colors = palette.then(|| {
                let mut values = vec![0; 257 * 4];
                values[4..7].fill(255);
                values[7] = 255;
                values[11] = 1;
                values
            });
            let mut rect: ffi::AVSubtitleRect = unsafe { std::mem::zeroed() };
            rect.type_ = ffi::SUBTITLE_BITMAP;
            rect.x = 123;
            rect.y = 456;
            rect.w = width as i32;
            rect.h = height as i32;
            rect.linesize[0] = stride as i32;
            rect.data[0] = pixels.as_mut_ptr();
            if let Some(palette) = &mut colors {
                rect.nb_colors = 256;
                rect.data[1] = palette.as_mut_ptr();
            }
            let pgm_width = width * scale;
            let pgm_height = height * scale;
            let mut pgm = format!("P5\n{} {}\n255\n", pgm_width, pgm_height).into_bytes();
            pgm.extend((0..pgm_width * pgm_height).map(|i| (i % 251) as u8));
            Self {
                rect,
                pixels,
                palette: colors,
                pgm,
                width,
                height,
                stride,
                scale,
            }
        }

        fn split(&self, pgm: &[u8]) -> Option<Vec<(Vec<u8>, OcrBoundingBox)>> {
            if !self.rect.data[0].is_null() {
                assert_eq!(self.rect.data[0], self.pixels.as_ptr() as *mut u8);
                if self.rect.linesize[0] > 0 && self.rect.h > 0 {
                    let len = (self.rect.linesize[0] as usize)
                        .checked_mul(self.rect.h as usize)
                        .expect("fixture dimensions fit usize");
                    assert!(len <= self.pixels.len());
                }
            }
            if !self.rect.data[1].is_null() {
                let palette = self.palette.as_ref().unwrap();
                assert_eq!(self.rect.data[1], palette.as_ptr() as *mut u8);
                if self.rect.nb_colors > 0 {
                    assert!(self.rect.nb_colors as usize * 4 <= palette.len());
                }
            }
            // SAFETY: the fixture owns both backing buffers, checks the declared
            // lengths above, and keeps them alive and immutable during this call.
            unsafe { split_pgs_bitmap_lines(&self.rect, pgm) }
        }

        fn ink(&mut self, top: usize, bottom: usize, left: usize, right: usize, index: u8) {
            for y in top..bottom {
                self.pixels[y * self.stride + left..y * self.stride + right].fill(index);
            }
        }

        fn assert_bands(&self, expected: &[(usize, usize)]) {
            let crops = self.split(&self.pgm).expect("line bands");
            assert_eq!(crops.len(), expected.len());
            let (full_width, full_height, header) = pgm_header_len(&self.pgm).unwrap();
            assert_eq!(
                (full_width, full_height),
                (self.width * self.scale, self.height * self.scale)
            );
            let raster = &self.pgm[header..];
            for ((crop, bbox), &(top, bottom)) in crops.iter().zip(expected) {
                let top = top * self.scale;
                let bottom = bottom * self.scale;
                assert_eq!(
                    (bbox.left, bbox.top, bbox.right, bbox.bottom),
                    (0, top as i32, full_width as i32, bottom as i32)
                );
                let (crop_width, crop_height, crop_header) = pgm_header_len(crop).unwrap();
                assert_eq!((crop_width, crop_height), (full_width, bottom - top));
                assert_eq!(
                    &crop[crop_header..],
                    &raster[top * full_width..bottom * full_width]
                );
            }
            for y in 0..self.height {
                let visible = self.pixels[y * self.stride..y * self.stride + self.width]
                    .iter()
                    .any(|&index| match &self.palette {
                        Some(palette) => palette[usize::from(index) * 4 + 3] != 0,
                        None => index != 0,
                    });
                if visible {
                    assert_eq!(
                        expected
                            .iter()
                            .filter(|&&(top, bottom)| top <= y && y < bottom)
                            .count(),
                        1
                    );
                }
            }
        }
    }

    #[test]
    fn multiline_detached_marks_and_two_speakers_preserve_pixels() {
        let mut image = Bitmap::new(40, 32, 40, 2, true);
        image.ink(1, 2, 5, 7, 1);
        image.ink(4, 10, 2, 15, 1);
        image.ink(4, 10, 23, 38, 1);
        image.ink(18, 19, 30, 31, 2);
        image.ink(21, 27, 3, 37, 1);
        image.assert_bands(&[(1, 10), (18, 27)]);
    }

    #[test]
    fn short_dense_line_uses_scaled_pgm_local_bbox() {
        let mut image = Bitmap::new(12, 8, 12, 3, true);
        image.ink(1, 7, 3, 9, 1);
        image.assert_bands(&[(1, 7)]);
    }

    #[test]
    fn overlapping_spatial_bands_abstain_with_or_without_specks() {
        for palette in [false, true] {
            for scale in [1, 2, 3] {
                for reverse in [false, true] {
                    let mut image = Bitmap::new(50, 25, 52, scale, palette);
                    let (first, second) = if reverse { (9, 2) } else { (2, 9) };
                    image.ink(first, first + 12, 2, 12, 1);
                    image.ink(second, second + 12, 30, 40, 1);
                    assert!(image.split(&image.pgm).is_none());
                    image.ink(10, 11, 21, 22, 2);
                    assert!(image.split(&image.pgm).is_none());
                }
            }
        }
    }

    #[test]
    fn spatial_band_guard_preserves_mild_stagger_and_short_gaps() {
        for reverse in [false, true] {
            let mut image = Bitmap::new(50, 25, 50, 2, true);
            let (first, second) = if reverse { (4, 2) } else { (2, 4) };
            image.ink(first, first + 12, 2, 12, 1);
            image.ink(second, second + 12, 30, 40, 1);
            image.assert_bands(&[(2, 16)]);
        }
        for (gap, shift, abstains) in [(11, 6, false), (12, 5, false), (12, 6, true)] {
            let mut image = Bitmap::new(50, 25, 50, 2, true);
            image.ink(2, 14, 2, 12, 1);
            image.ink(2 + shift, 14 + shift, 12 + gap, 22 + gap, 1);
            if abstains {
                assert!(image.split(&image.pgm).is_none());
            } else {
                image.assert_bands(&[(2, 14 + shift)]);
            }
        }
    }

    #[test]
    fn spatial_band_guard_preserves_anonymous_text_glyphs() {
        const A: [u8; 7] = [
            0b01110, 0b10001, 0b10001, 0b11111, 0b10001, 0b10001, 0b10001,
        ];
        const I: [u8; 7] = [
            0b11111, 0b00100, 0b00100, 0b00100, 0b00100, 0b00100, 0b11111,
        ];
        for offset in [0, 2] {
            let mut image = Bitmap::new(60, 24, 60, 2, true);
            for (left, top, glyph) in [(2, 2, A), (32, 2 + offset, I)] {
                for (y, row) in glyph.into_iter().enumerate() {
                    for x in 0..5 {
                        if row & (1 << (4 - x)) != 0 {
                            image.ink(
                                top + y * 2,
                                top + y * 2 + 2,
                                left + x * 2,
                                left + x * 2 + 2,
                                1,
                            );
                        }
                    }
                }
            }
            image.assert_bands(&[(2, 16 + offset)]);
        }
    }

    #[test]
    fn spatial_band_guard_ignores_small_marks_and_connected_bridges() {
        let mut small = Bitmap::new(50, 25, 50, 2, false);
        small.ink(2, 14, 2, 12, 1);
        small.ink(9, 11, 30, 32, 1);
        assert!(!overlapping_spatial_bands(
            &small.pixels,
            small.stride,
            small.width,
            2,
            14,
            |index| index != 0
        ));

        let mut bridged = Bitmap::new(50, 25, 50, 2, true);
        bridged.ink(2, 14, 2, 12, 1);
        bridged.ink(9, 21, 30, 40, 1);
        bridged.ink(10, 11, 12, 30, 1);
        // The horizontal bridge hides the separation; acceptance is not proof
        // that the pixels are one text line.
        bridged.assert_bands(&[(2, 21)]);
    }

    #[test]
    fn spatial_band_guard_is_bounded_at_source_limits() {
        let (width, height) = (4096, 1024);
        let pixels = vec![1u8; width * height];
        assert!(!overlapping_spatial_bands(
            &pixels,
            width,
            width,
            0,
            height,
            |index| index != 0
        ));
        let mut sparse = vec![0u8; width * 6];
        for row in sparse.chunks_mut(width) {
            for x in (0..width).step_by(3) {
                row[x] = 1;
            }
        }
        assert!(!overlapping_spatial_bands(
            &sparse,
            width,
            width,
            0,
            6,
            |index| index != 0
        ));
    }

    #[test]
    fn overlapping_spatial_band_abstention_uses_detector_fallback() {
        use super::super::ocr_pipeline::{extract_subtitle_lines, CueBuildParams};

        #[derive(Default)]
        struct Mock {
            detector_calls: usize,
            direct_calls: usize,
        }
        impl SubtitleConverter for Mock {
            fn supports_bitmap_line_recognition(&self, _: &str) -> bool {
                true
            }

            fn recognize_bitmap_line(
                &mut self,
                _: &image::RgbImage,
                _: &str,
            ) -> Result<Option<OcrLine>> {
                self.direct_calls += 1;
                Ok(None)
            }

            fn extract_lines(&mut self, _: &Path, _: &str) -> Result<OcrOutput> {
                self.detector_calls += 1;
                Ok(OcrOutput {
                    lines: vec![OcrLine {
                        text: "The sky is clear!".to_owned(),
                        bbox: None,
                        score: Some(1.0),
                        color: None,
                        italic: false,
                    }],
                    ..OcrOutput::default()
                })
            }
        }

        let mut multiple = Bitmap::new(50, 40, 50, 2, true);
        multiple.ink(2, 8, 2, 15, 1);
        multiple.ink(12, 24, 2, 12, 1);
        multiple.ink(19, 31, 30, 40, 1);
        assert!(multiple.split(&multiple.pgm).is_none());

        let mut image = Bitmap::new(50, 25, 50, 2, true);
        image.ink(2, 14, 2, 12, 1);
        image.ink(9, 21, 30, 40, 1);
        let mut rect_ptr = &mut image.rect as *mut ffi::AVSubtitleRect;
        let mut subtitle: ffi::AVSubtitle = unsafe { std::mem::zeroed() };
        subtitle.num_rects = 1;
        subtitle.rects = &mut rect_ptr;
        let temp = tempfile::tempdir().unwrap();
        let params = CueBuildParams {
            language: "eng",
            stream_index: 0,
            stream_codec_id: ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
            packet_seq: 0,
            work_dir: temp.path(),
            ocr_format: OcrFormat::Srt,
            video_dimensions: None,
            ocr_engine: OcrEngine::PpOcrV3,
        };
        let mut mock = Mock::default();
        let (lines, visible) = extract_subtitle_lines(
            &subtitle,
            &params,
            0,
            &mut mock,
            &mut OcrQualityBaseline::default(),
        )
        .unwrap();
        assert!(visible);
        assert_eq!(mock.detector_calls, 1);
        assert_eq!(mock.direct_calls, 0);
        assert_eq!(lines.len(), 1);
    }

    #[test]
    fn close_substantial_runs_abstain_and_wider_gaps_split() {
        for gap in [1, 2] {
            let mut image = Bitmap::new(18, 24, 18, 2, true);
            image.ink(2, 8, 2, 14, 1);
            image.ink(8 + gap, 14 + gap, 2, 14, 1);
            assert!(image.split(&image.pgm).is_none());
        }
        let mut gaps = Bitmap::new(18, 24, 18, 2, true);
        gaps.ink(2, 6, 2, 14, 1);
        gaps.ink(10, 14, 2, 14, 1);
        gaps.ink(18, 22, 2, 14, 1);
        gaps.assert_bands(&[(2, 6), (10, 14), (18, 22)]);
    }

    #[test]
    fn touching_and_narrowly_bridged_speakers_abstain() {
        let mut touching = Bitmap::new(18, 20, 18, 1, true);
        touching.ink(2, 10, 2, 8, 1);
        touching.ink(10, 17, 8, 14, 1);
        assert!(touching.split(&touching.pgm).is_none());

        let mut aligned = Bitmap::new(18, 20, 18, 1, true);
        aligned.ink(2, 10, 2, 14, 1);
        aligned.ink(10, 17, 2, 14, 1);
        assert!(aligned.split(&aligned.pgm).is_none());

        let mut disconnected = Bitmap::new(30, 24, 30, 1, true);
        disconnected.ink(2, 10, 2, 10, 1);
        disconnected.ink(10, 19, 17, 25, 1);
        assert!(disconnected.split(&disconnected.pgm).is_none());

        let mut bridged = Bitmap::new(30, 24, 30, 1, true);
        bridged.ink(2, 10, 3, 19, 1);
        bridged.ink(10, 11, 9, 10, 1);
        bridged.ink(11, 19, 3, 19, 1);
        assert!(bridged.split(&bridged.pgm).is_none());
    }

    #[test]
    fn touching_bands_with_one_noisy_row_abstain() {
        for row in [2, 5, 17] {
            for noise_width in [1, 2] {
                let mut image = Bitmap::new(20, 20, 20, 2, true);
                image.ink(2, 10, 2, 14, 1);
                image.ink(10, 18, 2, 14, 1);
                image.ink(row, row + 1, 14, 14 + noise_width, 2);
                assert!(image.split(&image.pgm).is_none());

                let mut missing = Bitmap::new(20, 20, 20, 2, true);
                missing.ink(2, 10, 2, 14, 1);
                missing.ink(10, 18, 2, 14, 1);
                missing.ink(row, row + 1, 2, 2 + noise_width, 0);
                assert!(missing.split(&missing.pgm).is_none());
            }
        }
    }

    #[test]
    fn touching_bands_with_multiple_noisy_rows_abstain() {
        let mut previous_counterexample = Bitmap::new(20, 20, 20, 2, true);
        previous_counterexample.ink(2, 10, 2, 14, 1);
        previous_counterexample.ink(10, 18, 2, 14, 1);
        previous_counterexample.ink(5, 6, 14, 15, 1);
        previous_counterexample.ink(12, 13, 14, 15, 1);
        assert!(previous_counterexample
            .split(&previous_counterexample.pgm)
            .is_none());

        let mut initial_noise = Bitmap::new(20, 36, 20, 2, true);
        initial_noise.ink(2, 18, 2, 14, 1);
        initial_noise.ink(18, 34, 2, 14, 1);
        initial_noise.ink(2, 3, 14, 15, 2);
        initial_noise.ink(3, 4, 15, 16, 2);
        initial_noise.ink(4, 5, 2, 3, 0);
        initial_noise.ink(25, 26, 14, 16, 2);
        assert!(initial_noise.split(&initial_noise.pgm).is_none());

        for rows in [&[2, 10][..], &[2, 5, 14][..], &[2, 5, 14, 25][..]] {
            let mut protruding = Bitmap::new(20, 36, 20, 2, true);
            protruding.ink(2, 18, 2, 14, 1);
            protruding.ink(18, 34, 2, 14, 1);
            for &row in rows {
                protruding.ink(row, row + 1, 14, 16, 2);
            }
            assert!(protruding.split(&protruding.pgm).is_none());

            let mut missing = Bitmap::new(20, 36, 20, 2, true);
            missing.ink(2, 18, 2, 14, 1);
            missing.ink(18, 34, 2, 14, 1);
            for &row in rows {
                missing.ink(row, row + 1, 2, 4, 0);
            }
            assert!(missing.split(&missing.pgm).is_none());
        }
    }

    #[test]
    fn tall_connected_single_line_is_retained() {
        let mut image = Bitmap::new(30, 30, 30, 2, true);
        image.ink(2, 26, 5, 10, 1);
        image.ink(2, 26, 15, 20, 1);
        image.ink(11, 14, 10, 15, 1);
        image.assert_bands(&[(2, 26)]);
    }

    #[test]
    fn isolated_noise_ties_empty_solid_and_no_core_abstain() {
        let mut image = Bitmap::new(16, 40, 16, 1, true);
        assert!(image.split(&image.pgm).is_none());
        image.ink(0, 40, 0, 16, 1);
        assert!(image.split(&image.pgm).is_none());
        image.pixels.fill(0);
        image.ink(2, 4, 2, 5, 1);
        assert!(image.split(&image.pgm).is_none());
        image.pixels.fill(0);
        image.ink(2, 3, 0, 1, 1);
        image.ink(25, 35, 2, 14, 1);
        image.ink(30, 31, 0, 2, 1);
        image.ink(30, 31, 14, 15, 1);
        assert!(image.split(&image.pgm).is_none());
        image.pixels.fill(0);
        image.ink(5, 15, 2, 14, 1);
        image.ink(7, 8, 0, 2, 1);
        image.ink(7, 8, 14, 15, 1);
        image.ink(20, 21, 2, 3, 1);
        image.ink(26, 36, 2, 14, 1);
        image.ink(28, 29, 0, 2, 1);
        image.ink(28, 29, 14, 15, 1);
        assert!(image.split(&image.pgm).is_none());
    }

    #[test]
    fn low_alpha_and_attached_noise_are_included_even_when_pgm_is_white() {
        let mut image = Bitmap::new(16, 22, 16, 2, true);
        image.ink(2, 3, 0, 1, 2);
        image.ink(5, 15, 3, 14, 2);
        image.ink(9, 10, 0, 3, 2);
        image.ink(20, 21, 3, 4, 3); // Palette entry three is transparent.
        let (_, _, header) = pgm_header_len(&image.pgm).unwrap();
        image.pgm[header..].fill(255);
        image.assert_bands(&[(2, 15)]);
    }

    #[test]
    fn farther_core_can_be_the_only_eligible_mark_attachment() {
        let mut image = Bitmap::new(16, 40, 16, 1, true);
        image.ink(2, 6, 1, 14, 1);
        image.ink(11, 12, 1, 2, 1);
        image.ink(18, 36, 1, 14, 1);
        image.ink(26, 27, 0, 1, 1);
        image.ink(26, 27, 14, 16, 1);
        image.assert_bands(&[(2, 6), (11, 36)]);
    }

    #[test]
    fn nonpalette_and_padded_stride_follow_index_visibility() {
        let mut image = Bitmap::new(12, 14, 17, 2, false);
        for y in 0..image.height {
            image.pixels[y * image.stride + image.width..(y + 1) * image.stride].fill(1);
        }
        image.ink(3, 9, 2, 8, 1);
        image.assert_bands(&[(3, 9)]);
    }

    #[test]
    fn production_pgm_is_cropped_without_rerendering() {
        let mut image = Bitmap::new(16, 22, 16, 2, true);
        image.ink(2, 3, 2, 4, 1);
        image.ink(5, 15, 2, 13, 1);
        image.ink(9, 10, 13, 16, 1);
        let (pgm, visible) = rect_to_pgm(&image.rect, OcrEngine::PpOcrV3).unwrap();
        assert!(visible);
        let (width, height, _) = pgm_header_len(&pgm).unwrap();
        assert_eq!(width % image.width, 0);
        assert_eq!(height % image.height, 0);
        image.scale = width / image.width;
        assert_eq!(height / image.height, image.scale);
        image.pgm = pgm;
        image.assert_bands(&[(2, 15)]);
    }

    #[test]
    fn whitespace_valued_first_raster_byte_is_not_parsed_as_header() {
        let mut image = Bitmap::new(8, 10, 8, 1, true);
        image.ink(0, 6, 1, 7, 1);
        let (_, _, header) = pgm_header_len(&image.pgm).unwrap();
        image.pgm[header] = b'\n';
        image.assert_bands(&[(0, 6)]);
    }

    #[test]
    fn malformed_pgm_and_inconsistent_dimensions_abstain() {
        let mut image = Bitmap::new(8, 8, 8, 1, true);
        image.ink(1, 7, 1, 7, 1);
        for pgm in [
            b"".as_slice(),
            b"P2\n8 8\n255\n",
            b"P58 8\n255\n",
            b"P5\n8 8\n65535\n",
            b"P5\n0 8\n255\n",
            b"P5\n999999999999999999999999999999 2\n255\n",
            b"P5\n8 8\n255",
        ] {
            assert!(image.split(pgm).is_none());
        }
        let overflow = format!("P5\n{} {}\n255\n", usize::MAX, usize::MAX);
        assert!(image.split(overflow.as_bytes()).is_none());
        assert!(image.split(&image.pgm[..image.pgm.len() - 1]).is_none());
        let mut extra = image.pgm.clone();
        extra.push(0);
        assert!(image.split(&extra).is_none());
        let unequal_scale = [b"P5\n16 8\n255\n".as_slice(), &[0; 128]].concat();
        assert!(image.split(&unequal_scale).is_none());
        let noninteger_scale = [b"P5\n9 8\n255\n".as_slice(), &[0; 72]].concat();
        assert!(image.split(&noninteger_scale).is_none());
    }

    #[test]
    fn crop_count_and_area_limits_abstain() {
        let mut at_limit = Bitmap::new(8, 145, 8, 1, true);
        let mut bands = Vec::new();
        for index in 0..16 {
            let top = 1 + index * 9;
            at_limit.ink(top, top + 4, 1, 7, 1);
            bands.push((top, top + 4));
        }
        at_limit.assert_bands(&bands);
        let mut too_many = Bitmap::new(8, 155, 8, 1, true);
        for index in 0..17 {
            let top = 1 + index * 9;
            too_many.ink(top, top + 4, 1, 7, 1);
        }
        assert!(too_many.split(&too_many.pgm).is_none());

        let mut source_too_large = Bitmap::new(2049, 2048, 2049, 1, true);
        source_too_large.ink(2, 18, 2, 20, 1);
        source_too_large.ink(9, 10, 20, 22, 1);
        assert!(source_too_large.split(&source_too_large.pgm).is_none());

        let mut crop_too_large = Bitmap::new(1024, 1025, 1024, 3, true);
        crop_too_large.ink(1, 1025, 50, 1000, 1);
        crop_too_large.ink(400, 401, 20, 30, 1);
        assert!(crop_too_large.split(&crop_too_large.pgm).is_none());
    }

    #[test]
    fn invalid_rectangles_strides_pointers_and_palette_abstain() {
        let mut image = Bitmap::new(8, 8, 8, 1, true);
        image.ink(1, 7, 1, 7, 1);
        image.rect.linesize[0] = 7;
        assert!(image.split(&image.pgm).is_none());
        image.rect.linesize[0] = -1;
        assert!(image.split(&image.pgm).is_none());
        let oversized_stride = (16 * 1024 * 1024 / 8) + 1;
        image.pixels.resize(oversized_stride * 8, 0);
        image.rect.data[0] = image.pixels.as_mut_ptr();
        image.rect.linesize[0] = oversized_stride as i32;
        assert!(image.split(&image.pgm).is_none());
        image.rect.linesize[0] = 8;
        image.rect.w = 0;
        assert!(image.split(&image.pgm).is_none());
        image.rect.w = i32::MAX;
        image.rect.h = i32::MAX;
        image.rect.linesize[0] = i32::MAX;
        image.rect.data[0] = std::ptr::null_mut();
        assert!(image.split(&image.pgm).is_none());
        image.rect.w = 8;
        image.rect.h = 8;
        image.rect.linesize[0] = 8;
        image.rect.data[0] = image.pixels.as_mut_ptr();
        image.rect.type_ = ffi::SUBTITLE_TEXT;
        assert!(image.split(&image.pgm).is_none());
        image.rect.type_ = ffi::SUBTITLE_BITMAP;
        let pixels = image.rect.data[0];
        image.rect.data[0] = std::ptr::null_mut();
        assert!(image.split(&image.pgm).is_none());
        image.rect.data[0] = pixels;
        image.rect.nb_colors = 0;
        assert!(image.split(&image.pgm).is_none());
        image.rect.nb_colors = 257;
        assert!(image.split(&image.pgm).is_none());
        image.rect.nb_colors = 1;
        assert!(image.split(&image.pgm).is_none());
    }
}

pub(super) fn split_pgm_into_word_crops_with_gap(
    pgm: &[u8],
    gap_override: Option<usize>,
) -> Vec<(Vec<u8>, OcrBoundingBox)> {
    let Some((width, height, header_len)) = pgm_header_len(pgm) else {
        return Vec::new();
    };
    let pixels = &pgm[header_len..];
    if width == 0 || height == 0 || pixels.len() < width * height {
        return Vec::new();
    }

    let has_ink = |x: usize, y: usize| pixels[y * width + x] < 245;
    let row_has_ink = |y: usize| (0..width).any(|x| has_ink(x, y));

    let mut bands = Vec::new();
    let mut y = 0usize;
    while y < height {
        while y < height && !row_has_ink(y) {
            y += 1;
        }
        if y >= height {
            break;
        }
        let start = y;
        while y < height && row_has_ink(y) {
            y += 1;
        }
        let end = y;
        if end.saturating_sub(start) >= 4 {
            bands.push((start, end));
        }
    }

    let mut crops = Vec::new();
    for (band_top, band_bottom) in bands {
        let band_h = band_bottom - band_top;
        let col_has_ink = |x: usize| (band_top..band_bottom).any(|yy| has_ink(x, yy));
        let min_gap = gap_override
            .or_else(|| {
                env::var("DPN_OCR_WORD_GAP_COLUMNS")
                    .ok()
                    .and_then(|v| v.trim().parse::<usize>().ok())
                    .filter(|v| (1..=32).contains(v))
            })
            .unwrap_or_else(|| (band_h / 6).clamp(2, 8));
        let mut spans = Vec::new();
        let mut x = 0usize;
        while x < width {
            while x < width && !col_has_ink(x) {
                x += 1;
            }
            if x >= width {
                break;
            }
            let start = x;
            let mut last_ink = x;
            let mut blank_run = 0usize;
            while x < width {
                if col_has_ink(x) {
                    last_ink = x;
                    blank_run = 0;
                } else {
                    blank_run += 1;
                    if blank_run >= min_gap {
                        break;
                    }
                }
                x += 1;
            }
            let end = last_ink + 1;
            if end > start + 1 {
                spans.push((start, end));
            }
        }
        // If no real word gaps were found, do not return a single full-line crop.
        if spans.len() < 2 {
            continue;
        }
        for (left, right) in spans {
            let pad_x = env::var("DPN_OCR_WORD_PAD_X")
                .ok()
                .and_then(|v| v.trim().parse::<usize>().ok())
                .filter(|v| (0..=32).contains(v))
                .unwrap_or(2);
            let pad_y = env::var("DPN_OCR_WORD_PAD_Y")
                .ok()
                .and_then(|v| v.trim().parse::<usize>().ok())
                .filter(|v| (0..=24).contains(v))
                .unwrap_or(2);
            let l = left.saturating_sub(pad_x);
            let r = (right + pad_x).min(width);
            let t = band_top.saturating_sub(pad_y);
            let b = (band_bottom + pad_y).min(height);
            let crop = crop_pgm(pixels, width, l, t, r, b);
            crops.push((
                crop,
                OcrBoundingBox {
                    left: l as i32,
                    right: r as i32,
                    top: t as i32,
                    bottom: b as i32,
                },
            ));
        }
    }
    crops
}

pub(super) fn dominant_color_from_rect(rect: &ffi::AVSubtitleRect) -> Option<(u8, u8, u8)> {
    if rect.w <= 0 || rect.h <= 0 || rect.data[0].is_null() || rect.data[1].is_null() {
        return None;
    }

    let width = rect.w as usize;
    let height = rect.h as usize;
    let stride = rect.linesize[0].max(0) as usize;
    if stride < width {
        return None;
    }

    let pixels = unsafe { std::slice::from_raw_parts(rect.data[0], stride * height) };
    let palette = unsafe { std::slice::from_raw_parts(rect.data[1], 256 * 4) };

    let mut counts = [0u32; 256];
    for y in 0..height {
        let row = &pixels[y * stride..(y * stride + width)];
        for &idx in row {
            let base = (idx as usize) * 4;
            if base + 3 >= palette.len() {
                continue;
            }
            let a = palette[base + 3];
            if a > 16 {
                counts[idx as usize] += 1;
            }
        }
    }

    let mut best_idx = None;
    let mut best_count = 0u32;
    for (idx, count) in counts.iter().enumerate() {
        if *count > best_count {
            best_count = *count;
            best_idx = Some(idx);
        }
    }

    let idx = best_idx?;
    if best_count == 0 {
        return None;
    }
    let base = idx * 4;
    if base + 2 >= palette.len() {
        return None;
    }
    let r = palette[base];
    let g = palette[base + 1];
    let b = palette[base + 2];
    Some((r, g, b))
}

pub(super) fn write_srt(path: &Path, cues: &[SubtitleCue]) -> Result<()> {
    let mut body = String::new();

    for (i, cue) in cues.iter().enumerate() {
        body.push_str(&(i + 1).to_string());
        body.push('\n');
        body.push_str(&format!(
            "{} --> {}\n",
            format_srt_timestamp(cue.start_ms),
            format_srt_timestamp(cue.end_ms)
        ));
        body.push_str(&cue.text);
        body.push_str("\n\n");
    }

    fs::write(path, body.as_bytes()).with_context(|| format!("writing '{}'", path.display()))?;
    Ok(())
}

pub(super) fn write_ass(
    path: &Path,
    cues: &[SubtitleCue],
    video_dimensions: Option<(u32, u32)>,
) -> Result<()> {
    let (play_res_x, play_res_y) = video_dimensions.unwrap_or((1920, 1080));
    let mut body = String::new();
    body.push_str("[Script Info]\n");
    body.push_str("ScriptType: v4.00+\n");
    body.push_str(&format!("PlayResX: {}\n", play_res_x));
    body.push_str(&format!("PlayResY: {}\n", play_res_y));
    body.push_str("WrapStyle: 2\n");
    body.push_str("ScaledBorderAndShadow: yes\n");
    body.push('\n');
    body.push_str("[V4+ Styles]\n");
    body.push_str("Format: Name, Fontname, Fontsize, PrimaryColour, SecondaryColour, OutlineColour, BackColour, Bold, Italic, Underline, StrikeOut, ScaleX, ScaleY, Spacing, Angle, BorderStyle, Outline, Shadow, Alignment, MarginL, MarginR, MarginV, Encoding\n");
    body.push_str("Style: Default,Arial,48,&H00FFFFFF,&H000000FF,&H00000000,&H64000000,0,0,0,0,100,100,0,0,1,2,0,2,10,10,10,1\n");
    body.push('\n');
    body.push_str("[Events]\n");
    body.push_str(
        "Format: Layer, Start, End, Style, Name, MarginL, MarginR, MarginV, Effect, Text\n",
    );

    for cue in cues {
        body.push_str(&format!(
            "Dialogue: 0,{},{},Default,,0,0,0,,{}\n",
            format_ass_timestamp(cue.start_ms),
            format_ass_timestamp(cue.end_ms),
            cue.text
        ));
    }

    fs::write(path, body.as_bytes()).with_context(|| format!("writing '{}'", path.display()))?;
    Ok(())
}

pub(super) fn sanitize_cues(cues: &mut Vec<SubtitleCue>, format: OcrFormat) {
    cues.sort_by_key(|cue| cue.start_ms);

    if matches!(format, OcrFormat::Srt) {
        for i in 0..cues.len().saturating_sub(1) {
            if cues[i].end_ms > cues[i + 1].start_ms {
                cues[i].end_ms = cues[i + 1].start_ms;
            }
        }
    }

    cues.retain(|cue| cue.end_ms > cue.start_ms && !cue.text.trim().is_empty());
}

pub(super) fn format_srt_timestamp(total_ms: i64) -> String {
    let ms = total_ms.max(0);
    let hours = ms / 3_600_000;
    let minutes = (ms % 3_600_000) / 60_000;
    let seconds = (ms % 60_000) / 1_000;
    let millis = ms % 1_000;
    format!("{:02}:{:02}:{:02},{:03}", hours, minutes, seconds, millis)
}

pub(super) fn format_ass_timestamp(total_ms: i64) -> String {
    let ms = total_ms.max(0);
    let hours = ms / 3_600_000;
    let minutes = (ms % 3_600_000) / 60_000;
    let seconds = (ms % 60_000) / 1_000;
    let centis = (ms % 1_000) / 10;
    format!("{}:{:02}:{:02}.{:02}", hours, minutes, seconds, centis)
}

pub(super) fn normalize_utf8_text(input: &str) -> String {
    input
        .replace('\r', "")
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

pub(super) fn strip_ass_formatting(ass: &str) -> String {
    let text_payload = ass.rsplit_once(',').map(|(_, rhs)| rhs).unwrap_or(ass);
    let mut out = String::new();
    let mut in_tag = false;
    for ch in text_payload.chars() {
        match ch {
            '{' => in_tag = true,
            '}' => in_tag = false,
            _ if !in_tag => out.push(ch),
            _ => {}
        }
    }
    normalize_utf8_text(&out.replace("\\N", "\n"))
}

pub(super) fn ass_position_from_bbox(
    bbox: &OcrBoundingBox,
    video_dimensions: Option<(u32, u32)>,
) -> (i32, i32) {
    let mut x = (bbox.left + bbox.right) / 2;
    let mut y = (bbox.top + bbox.bottom) / 2;
    if let Some((width, height)) = video_dimensions {
        let max_x = width.saturating_sub(1) as i32;
        let max_y = height.saturating_sub(1) as i32;
        x = x.clamp(0, max_x);
        y = y.clamp(0, max_y);
    }
    (x, y)
}

pub(super) fn format_ass_text_with_style(
    text: &str,
    pos: Option<(i32, i32)>,
    color: Option<(u8, u8, u8)>,
    italic: bool,
) -> String {
    let mut tags = String::new();
    if let Some((x, y)) = pos {
        tags.push_str(&format!("\\pos({},{})", x, y));
    }
    if let Some((r, g, b)) = color {
        tags.push_str(&format!("\\c{}", ass_color_from_rgb(r, g, b)));
    }
    if italic {
        tags.push_str("\\i1");
    }
    if tags.is_empty() {
        ass_escape(text)
    } else {
        format!("{{{}}}{}", tags, ass_escape(text))
    }
}

pub(super) fn ass_escape(text: &str) -> String {
    let replaced = text.replace('\r', "");
    let replaced = replaced.replace('\\', "\\\\");
    let replaced = replaced.replace('{', "\\{");
    let replaced = replaced.replace('}', "\\}");
    replaced.replace('\n', "\\N")
}

pub(super) fn ass_color_from_rgb(r: u8, g: u8, b: u8) -> String {
    format!("&H{:02X}{:02X}{:02X}&", b, g, r)
}

pub(super) fn parse_ass_color(text: &str) -> Option<(u8, u8, u8)> {
    let marker = "\\c&H";
    let start = text.find(marker)?;
    let after = &text[start + marker.len()..];
    let end = after.find('&')?;
    let hex = &after[..end];
    if hex.len() != 6 {
        return None;
    }
    let b = u8::from_str_radix(&hex[0..2], 16).ok()?;
    let g = u8::from_str_radix(&hex[2..4], 16).ok()?;
    let r = u8::from_str_radix(&hex[4..6], 16).ok()?;
    Some((r, g, b))
}

#[cfg(test)]
pub(super) fn intersection_over_union(a: &OcrBoundingBox, b: &OcrBoundingBox) -> f32 {
    let x_left = a.left.max(b.left);
    let y_top = a.top.max(b.top);
    let x_right = a.right.min(b.right);
    let y_bottom = a.bottom.min(b.bottom);

    let inter_width = (x_right - x_left).max(0) as f32;
    let inter_height = (y_bottom - y_top).max(0) as f32;
    let inter_area = inter_width * inter_height;

    let area_a = ((a.right - a.left).max(0) as f32) * ((a.bottom - a.top).max(0) as f32);
    let area_b = ((b.right - b.left).max(0) as f32) * ((b.bottom - b.top).max(0) as f32);

    if area_a <= 0.0 || area_b <= 0.0 {
        return 0.0;
    }

    inter_area / (area_a + area_b - inter_area)
}

#[cfg(test)]
pub(super) fn rgb_distance(a: (u8, u8, u8), b: (u8, u8, u8)) -> f32 {
    let dr = a.0 as f32 - b.0 as f32;
    let dg = a.1 as f32 - b.1 as f32;
    let db = a.2 as f32 - b.2 as f32;
    (dr * dr + dg * dg + db * db).sqrt()
}

#[cfg(test)]
#[allow(clippy::needless_range_loop)]
pub(super) fn word_error_rate(expected: &str, actual: &str) -> f32 {
    let expected_words: Vec<&str> = expected.split_whitespace().collect();
    let actual_words: Vec<&str> = actual.split_whitespace().collect();
    if expected_words.is_empty() {
        return if actual_words.is_empty() { 0.0 } else { 1.0 };
    }

    let m = expected_words.len();
    let n = actual_words.len();
    let mut dp = vec![vec![0usize; n + 1]; m + 1];
    for i in 0..=m {
        dp[i][0] = i;
    }
    for j in 0..=n {
        dp[0][j] = j;
    }
    for i in 1..=m {
        for j in 1..=n {
            let cost = if expected_words[i - 1] == actual_words[j - 1] {
                0
            } else {
                1
            };
            dp[i][j] = std::cmp::min(
                std::cmp::min(dp[i - 1][j] + 1, dp[i][j - 1] + 1),
                dp[i - 1][j - 1] + cost,
            );
        }
    }
    dp[m][n] as f32 / expected_words.len() as f32
}

pub(super) fn bounding_box_from_points(
    points: &[paddle_ocr_rs::ocr_result::Point],
) -> Option<OcrBoundingBox> {
    if points.is_empty() {
        return None;
    }
    let mut left = i32::MAX;
    let mut right = i32::MIN;
    let mut top = i32::MAX;
    let mut bottom = i32::MIN;
    for point in points {
        let x = point.x as i32;
        let y = point.y as i32;
        left = left.min(x);
        right = right.max(x);
        top = top.min(y);
        bottom = bottom.max(y);
    }
    Some(OcrBoundingBox {
        left,
        right,
        top,
        bottom,
    })
}

pub(super) fn offset_bbox(bbox: &mut OcrBoundingBox, offset_x: i32, offset_y: i32) {
    bbox.left += offset_x;
    bbox.right += offset_x;
    bbox.top += offset_y;
    bbox.bottom += offset_y;
}

pub(super) fn sort_ocr_lines(a: &OcrLine, b: &OcrLine) -> std::cmp::Ordering {
    match (&a.bbox, &b.bbox) {
        (Some(a_box), Some(b_box)) => {
            let ay = a_box.top;
            let by = b_box.top;
            if ay == by {
                a_box.left.cmp(&b_box.left)
            } else {
                ay.cmp(&by)
            }
        }
        _ => std::cmp::Ordering::Equal,
    }
}

// This guard never transfers alternate OCR text or whitespace into the output.
// Detector polygons and character boxes are checked against the same source image.
pub(super) struct OverlappingSeam {
    pub(super) original_glyphs: String,
    pub(super) corrected_glyphs: String,
    pub(super) corrected_text: String,
    seam_index: usize,
    polygons: [Vec<paddle_ocr_rs::ocr_result::Point>; 2],
}

pub(super) fn seam_glyph_stream(text: &str) -> Option<String> {
    if text.is_empty()
        || text
            .chars()
            .any(|c| (c.is_whitespace() && c != ' ') || c.is_control())
    {
        return None;
    }
    let glyphs: String = text.chars().filter(|c| *c != ' ').collect();
    (!glyphs.is_empty()).then_some(glyphs)
}

// Match the foreground threshold used when preparing PP-OCR subtitle images.
const SEAM_FOREGROUND_THRESHOLD: u8 = 220;
const SEAM_CORE_THRESHOLD: u8 = 128;
const SEAM_COMPONENT_PIXEL_LIMIT: usize = 65_536;

fn bounded_seam_image(image: &image::RgbImage) -> bool {
    let (w, h) = image.dimensions();
    w > 0 && h > 0 && w <= 4096 && h <= 2048 && u64::from(w) * u64::from(h) <= 4_194_304
}

fn valid_seam_polygon(points: &[paddle_ocr_rs::ocr_result::Point], w: u32, h: u32) -> bool {
    // Detector unclip may extend a valid polygon beyond the input canvas.
    // Keep coordinates bounded, but evaluate coverage only on actual pixels.
    let Some(max_x) = w.checked_add((w / 4).max(32)) else {
        return false;
    };
    let Some(max_y) = h.checked_add((h / 4).max(32)) else {
        return false;
    };
    if points.len() != 4 || points.iter().any(|p| p.x > max_x || p.y > max_y) {
        return false;
    }
    let mut sign = 0i64;
    for i in 0..4 {
        let a = points[i];
        let b = points[(i + 1) % 4];
        let c = points[(i + 2) % 4];
        let cross = (i64::from(b.x) - i64::from(a.x)) * (i64::from(c.y) - i64::from(b.y))
            - (i64::from(b.y) - i64::from(a.y)) * (i64::from(c.x) - i64::from(b.x));
        if cross == 0 || (sign != 0 && cross.signum() != sign) {
            return false;
        }
        sign = cross.signum();
    }
    true
}

fn seam_polygon_contains(points: &[paddle_ocr_rs::ocr_result::Point], x: u32, y: u32) -> bool {
    seam_polygon_contains_with_boundary(points, x, y, true)
}

fn seam_polygon_contains_with_boundary(
    points: &[paddle_ocr_rs::ocr_result::Point],
    x: u32,
    y: u32,
    include_boundary: bool,
) -> bool {
    let mut positive = false;
    let mut negative = false;
    for i in 0..4 {
        let a = points[i];
        let b = points[(i + 1) % 4];
        let cross = (i64::from(b.x) - i64::from(a.x)) * (i64::from(y) - i64::from(a.y))
            - (i64::from(b.y) - i64::from(a.y)) * (i64::from(x) - i64::from(a.x));
        if cross == 0 && !include_boundary {
            return false;
        }
        positive |= cross > 0;
        negative |= cross < 0;
    }
    !(positive && negative)
}

pub(super) fn prepare_overlapping_seam(
    blocks: &[paddle_ocr_rs::ocr_result::TextBlock],
    merged: &[OcrLine],
    image: &image::RgbImage,
) -> Option<OverlappingSeam> {
    if blocks.len() != 2 || merged.len() != 1 || !bounded_seam_image(image) {
        return None;
    }
    let (w, h) = image.dimensions();
    if blocks.iter().any(|b| {
        b.text.trim() != b.text
            || seam_glyph_stream(&b.text).is_none()
            || !valid_seam_polygon(&b.box_points, w, h)
    }) {
        return None;
    }
    let mut ordered = [&blocks[0], &blocks[1]];
    ordered.sort_by_key(|b| b.box_points.iter().map(|p| p.x).min().unwrap_or(0));
    let left = ordered[0];
    let right = ordered[1];
    let left_box = bounding_box_from_points(&left.box_points)?;
    let right_box = bounding_box_from_points(&right.box_points)?;
    if left_box.left >= right_box.left
        || left_box.right <= right_box.left
        || left_box.bottom <= right_box.top
        || right_box.bottom <= left_box.top
    {
        return None;
    }
    let seam_char = left.text.chars().last()?;
    if seam_char == ' ' || right.text.chars().next()? != seam_char {
        return None;
    }
    let left_chars: Vec<char> = left.text.chars().collect();
    let right_chars: Vec<char> = right.text.chars().collect();
    if left_chars.len() >= 2
        && right_chars.len() >= 2
        && left_chars[left_chars.len() - 2..] == right_chars[..2]
    {
        return None;
    }
    let original = format!("{}{}", left.text, right.text);
    if merged[0].text != original {
        return None;
    }
    let original_glyphs = seam_glyph_stream(&original)?;
    let seam_index = seam_glyph_stream(&left.text)?
        .chars()
        .count()
        .checked_sub(1)?;
    let mut corrected_text = original.clone();
    corrected_text.drain(left.text.len()..left.text.len() + seam_char.len_utf8());
    let corrected_glyphs = seam_glyph_stream(&corrected_text)?;
    Some(OverlappingSeam {
        original_glyphs,
        corrected_glyphs,
        corrected_text,
        seam_index,
        polygons: [left.box_points.clone(), right.box_points.clone()],
    })
}

pub(super) fn apply_overlapping_seam(line: &mut OcrLine, seam: &OverlappingSeam) {
    line.text = seam.corrected_text.clone();
}

pub(super) fn full_line_ink_coverage(
    image: &image::RgbImage,
    points: &[paddle_ocr_rs::ocr_result::Point],
) -> bool {
    if !bounded_seam_image(image) || !valid_seam_polygon(points, image.width(), image.height()) {
        return false;
    }
    let gray = image::imageops::grayscale(image);
    let mut total = 0u64;
    let mut covered = 0u64;
    for (x, y, pixel) in gray.enumerate_pixels() {
        if pixel[0] < SEAM_FOREGROUND_THRESHOLD {
            total += 1;
            covered += u64::from(seam_polygon_contains(points, x, y));
        }
    }
    total > 0 && covered * 100 >= total * 99
}

// Both modes must independently read the proposed glyphs and align every box.
// Errors, unavailable Tesseract, and conflicting boxes all abstain.
pub(super) fn tesseract_seam_evidence(
    image_path: &Path,
    image: &image::RgbImage,
    seam: &OverlappingSeam,
) -> bool {
    tesseract_seam_evidence_with(image, seam, |psm| {
        Some((
            run_tesseract_raw(image_path, "eng", psm).ok()?,
            run_tesseract_boxes(image_path, "eng", psm).ok()?,
        ))
    })
}

fn tesseract_seam_evidence_with(
    image: &image::RgbImage,
    seam: &OverlappingSeam,
    mut read: impl FnMut(u8) -> Option<(String, String)>,
) -> bool {
    let mut seam_box = None;
    for psm in [6, 7] {
        let Some((text, raw_boxes)) = read(psm) else {
            return false;
        };
        if seam_glyph_stream(&text).as_deref() != Some(seam.corrected_glyphs.as_str()) {
            return false;
        }
        let Some(boxes) = parse_tesseract_boxes(&raw_boxes) else {
            return false;
        };
        if boxes.iter().map(|(c, _)| *c).collect::<String>() != seam.corrected_glyphs
            || boxes.iter().any(|(_, [l, b, r, t])| {
                l >= r || b >= t || *r > image.width() || *t > image.height()
            })
            || seam.seam_index == 0
            || seam.seam_index + 1 >= boxes.len()
        {
            return false;
        }
        let current = boxes[seam.seam_index].1;
        if seam_box.is_some_and(|previous| previous != current) {
            return false;
        }
        let prev = boxes[seam.seam_index - 1].1;
        let next = boxes[seam.seam_index + 1].1;
        if !(prev[2] <= current[0] && current[2] <= next[0] && prev[2] < next[0]) {
            return false;
        }
        if !seam_pixel_evidence(image, seam, current, prev[2], next) {
            return false;
        }
        seam_box = Some(current);
    }
    seam_box.is_some()
}

fn seam_pixel_evidence(
    image: &image::RgbImage,
    seam: &OverlappingSeam,
    char_box: [u32; 4],
    left: u32,
    next_box: [u32; 4],
) -> bool {
    let right = next_box[0];
    if !bounded_seam_image(image) || left >= right || right > image.width() {
        return false;
    }
    let [cl, cb, cr, ct] = char_box;
    if cl >= cr || cb >= ct || cr > image.width() || ct > image.height() {
        return false;
    }
    let gray = image::imageops::grayscale(image);
    let width = (right - left) as usize;
    let height = image.height() as usize;
    let mut seen = vec![false; width * height];
    let mut component_count = 0;
    let mut neighbor_fringe_count = 0;
    let mut component_pixels = 0u64;
    let mut char_pixels = 0u64;
    let mut overlap_pixels = 0u64;
    for y in 0..image.height() {
        for x in left..right {
            let index = y as usize * width + (x - left) as usize;
            if gray.get_pixel(x, y)[0] >= SEAM_FOREGROUND_THRESHOLD || seen[index] {
                continue;
            }
            seen[index] = true;
            let mut stack = vec![(x, y)];
            let mut pixels = Vec::new();
            while let Some((px, py)) = stack.pop() {
                pixels.push((px, py));
                if pixels.len() > SEAM_COMPONENT_PIXEL_LIMIT {
                    return false;
                }
                for dy in -1i64..=1 {
                    for dx in -1i64..=1 {
                        let nx = i64::from(px) + dx;
                        let ny = i64::from(py) + dy;
                        if nx < i64::from(left)
                            || nx >= i64::from(right)
                            || ny < 0
                            || ny >= i64::from(image.height())
                        {
                            continue;
                        }
                        let (nx, ny) = (nx as u32, ny as u32);
                        let ni = ny as usize * width + (nx - left) as usize;
                        if !seen[ni] && gray.get_pixel(nx, ny)[0] < SEAM_FOREGROUND_THRESHOLD {
                            seen[ni] = true;
                            stack.push((nx, ny));
                        }
                    }
                }
            }
            let top = image.height() - ct;
            let bottom = image.height() - cb;
            if pixels.iter().any(|&(px, py)| {
                px < cl.saturating_sub(2)
                    || px >= cr.saturating_add(2)
                    || py < top.saturating_sub(2)
                    || py >= bottom.saturating_add(2)
            }) {
                neighbor_fringe_count += 1;
                if neighbor_fringe_count > 1 || !is_neighbor_fringe(&gray, &pixels, next_box) {
                    return false;
                }
                continue;
            }
            component_count += 1;
            if component_count > 1 {
                return false;
            }
            for (px, py) in pixels {
                if gray.get_pixel(px, py)[0] < SEAM_CORE_THRESHOLD {
                    component_pixels += 1;
                    if px >= cl && px < cr && py >= top && py < bottom {
                        char_pixels += 1;
                        if seam_polygon_contains_with_boundary(&seam.polygons[0], px, py, false)
                            && seam_polygon_contains_with_boundary(&seam.polygons[1], px, py, false)
                        {
                            overlap_pixels += 1;
                        }
                    }
                }
            }
        }
    }
    component_count == 1
        && component_pixels > 0
        && char_pixels * 100 >= component_pixels * 98
        && overlap_pixels * 2 >= component_pixels
}

// Ignore only a weak strip fragment connected to a tightly bounded neighbor core.
fn is_neighbor_fringe(gray: &image::GrayImage, fringe: &[(u32, u32)], next_box: [u32; 4]) -> bool {
    let [left, bottom, right, top] = next_box;
    if fringe.is_empty()
        || left >= right
        || bottom >= top
        || right > gray.width()
        || top > gray.height()
    {
        return false;
    }
    let top = gray.height() - top;
    let bottom = gray.height() - bottom;
    let min_x = left.saturating_sub(2);
    let max_x = right.saturating_add(2).min(gray.width());
    let min_y = top.saturating_sub(2);
    let max_y = bottom.saturating_add(2).min(gray.height());
    if fringe.iter().any(|&(x, y)| {
        x < min_x
            || x >= left
            || y < min_y
            || y >= max_y
            || gray.get_pixel(x, y)[0] < SEAM_CORE_THRESHOLD
    }) {
        return false;
    }
    let width = (max_x - min_x) as usize;
    let height = (max_y - min_y) as usize;
    if width * height > SEAM_COMPONENT_PIXEL_LIMIT {
        return false;
    }
    let mut seen = vec![false; width * height];
    let mut stack = vec![fringe[0]];
    seen[(fringe[0].1 - min_y) as usize * width + (fringe[0].0 - min_x) as usize] = true;
    let mut core_pixels = 0usize;
    while let Some((x, y)) = stack.pop() {
        if gray.get_pixel(x, y)[0] < SEAM_CORE_THRESHOLD {
            if x < left || x >= right || y < top || y >= bottom {
                return false;
            }
            core_pixels += 1;
        }
        for dy in -1i64..=1 {
            for dx in -1i64..=1 {
                let nx = i64::from(x) + dx;
                let ny = i64::from(y) + dy;
                if nx < 0
                    || ny < 0
                    || nx >= i64::from(gray.width())
                    || ny >= i64::from(gray.height())
                {
                    continue;
                }
                let (nx, ny) = (nx as u32, ny as u32);
                if gray.get_pixel(nx, ny)[0] >= SEAM_FOREGROUND_THRESHOLD {
                    continue;
                }
                if nx < min_x || nx >= max_x || ny < min_y || ny >= max_y {
                    return false;
                }
                let index = (ny - min_y) as usize * width + (nx - min_x) as usize;
                if !seen[index] {
                    seen[index] = true;
                    stack.push((nx, ny));
                }
            }
        }
    }
    core_pixels >= 4
        && core_pixels >= fringe.len()
        && fringe
            .iter()
            .all(|&(x, y)| seen[(y - min_y) as usize * width + (x - min_x) as usize])
}

pub(super) fn merge_ocr_lines_with_spacing(lines: Vec<OcrLine>) -> Vec<OcrLine> {
    merge_ocr_lines(lines, false)
}

pub(super) fn merge_ocr_word_crops(lines: Vec<OcrLine>) -> Vec<OcrLine> {
    merge_ocr_lines(lines, true)
}

fn merge_ocr_lines(lines: Vec<OcrLine>, separated_words: bool) -> Vec<OcrLine> {
    if lines.is_empty() {
        return Vec::new();
    }

    let mut with_bbox = Vec::new();
    let mut without_bbox = Vec::new();
    for line in lines {
        if line.bbox.is_some() {
            with_bbox.push(line);
        } else {
            without_bbox.push(line);
        }
    }

    if with_bbox.is_empty() {
        return without_bbox;
    }

    with_bbox.sort_by(sort_ocr_lines);

    struct LineGroup {
        items: Vec<OcrLine>,
        center_y: f32,
        avg_height: f32,
        bbox: OcrBoundingBox,
        score_sum: f32,
        score_count: usize,
    }

    let mut groups: Vec<LineGroup> = Vec::new();

    for line in with_bbox {
        let Some(bbox) = line.bbox.clone() else {
            continue;
        };
        let score = line.score;
        let height = (bbox.bottom - bbox.top).max(1) as f32;
        let center_y = (bbox.top + bbox.bottom) as f32 / 2.0;

        let mut matched = None;
        for (idx, group) in groups.iter().enumerate() {
            let threshold = (group.avg_height * 0.6).max(4.0);
            if (center_y - group.center_y).abs() <= threshold {
                matched = Some(idx);
                break;
            }
        }

        if let Some(idx) = matched {
            let group = &mut groups[idx];
            group.items.push(line);
            let count = group.items.len() as f32;
            group.center_y = (group.center_y * (count - 1.0) + center_y) / count;
            group.avg_height = (group.avg_height * (count - 1.0) + height) / count;
            group.bbox.left = group.bbox.left.min(bbox.left);
            group.bbox.right = group.bbox.right.max(bbox.right);
            group.bbox.top = group.bbox.top.min(bbox.top);
            group.bbox.bottom = group.bbox.bottom.max(bbox.bottom);
            if let Some(score) = score {
                group.score_sum += score;
                group.score_count += 1;
            }
        } else {
            let mut score_sum = 0.0;
            let mut score_count = 0;
            if let Some(score) = score {
                score_sum = score;
                score_count = 1;
            }
            groups.push(LineGroup {
                items: vec![line],
                center_y,
                avg_height: height,
                bbox: bbox.clone(),
                score_sum,
                score_count,
            });
        }
    }

    let mut merged = Vec::new();
    for mut group in groups {
        group
            .items
            .sort_by(|a, b| match (a.bbox.as_ref(), b.bbox.as_ref()) {
                (Some(a_box), Some(b_box)) => a_box.left.cmp(&b_box.left),
                (Some(_), None) => std::cmp::Ordering::Less,
                (None, Some(_)) => std::cmp::Ordering::Greater,
                (None, None) => std::cmp::Ordering::Equal,
            });

        let avg_height = group.avg_height.max(1.0);
        let space_threshold = (avg_height * 0.25).max(2.0);
        let mut text = String::new();
        let mut prev_right: Option<i32> = None;

        for item in group.items {
            let Some(bbox) = item.bbox.as_ref() else {
                continue;
            };
            if let Some(prev) = prev_right {
                let gap = bbox.left - prev;
                if separated_words || (gap as f32) > space_threshold {
                    text.push(' ');
                }
            }
            text.push_str(item.text.trim());
            prev_right = Some(bbox.right);
        }

        let text = normalize_utf8_text(&text);
        if text.is_empty() {
            continue;
        }

        let score = if group.score_count > 0 {
            Some(group.score_sum / group.score_count as f32)
        } else {
            None
        };

        merged.push(OcrLine {
            text,
            bbox: Some(group.bbox),
            score,
            color: None,
            italic: false,
        });
    }

    merged.extend(without_bbox);
    merged.sort_by(sort_ocr_lines);
    merged
}

pub(super) fn load_image(path: &Path) -> Result<image::RgbImage> {
    let img = image::open(path).with_context(|| format!("loading image '{}'", path.display()))?;
    Ok(img.to_rgb8())
}

#[cfg(test)]
mod overlapping_seam_tests {
    use super::*;
    use paddle_ocr_rs::ocr_result::{Point, TextBlock};

    fn rect(l: u32, t: u32, r: u32, b: u32) -> Vec<Point> {
        vec![
            Point { x: l, y: t },
            Point { x: r, y: t },
            Point { x: r, y: b },
            Point { x: l, y: b },
        ]
    }

    fn block(text: &str, points: Vec<Point>) -> TextBlock {
        TextBlock {
            box_points: points,
            box_score: 0.9,
            angle_index: 0,
            angle_score: 0.0,
            text: text.into(),
            text_score: 0.8,
        }
    }

    fn setup() -> (image::RgbImage, Vec<TextBlock>, Vec<OcrLine>) {
        let mut img = image::RgbImage::from_pixel(24, 10, image::Rgb([255, 255, 255]));
        for x in [2, 10, 20] {
            img.put_pixel(x, 4, image::Rgb([0, 0, 0]));
        }
        let blocks = vec![
            block("A Q", rect(0, 0, 14, 9)),
            block("Q B", rect(8, 0, 24, 9)),
        ];
        let lines = blocks
            .iter()
            .map(|b| OcrLine {
                text: b.text.clone(),
                bbox: bounding_box_from_points(&b.box_points),
                score: Some(b.text_score),
                color: None,
                italic: false,
            })
            .collect();
        (img, blocks, merge_ocr_lines_with_spacing(lines))
    }

    fn boxes() -> String {
        "A 1 2 4 8 0\nQ 10 2 12 8 0\nB 19 2 22 8 0\n".into()
    }

    #[test]
    fn candidate_preserves_spacing_and_metadata() {
        let (img, blocks, mut lines) = setup();
        let seam = prepare_overlapping_seam(&blocks, &lines, &img).unwrap();
        assert_eq!(seam.original_glyphs, "AQQB");
        assert_eq!(seam.corrected_glyphs, "AQB");
        assert_eq!(seam_glyph_stream("A  Q B").as_deref(), Some("AQB"));
        assert_ne!(seam_glyph_stream("A Q Q B").unwrap(), seam.corrected_glyphs);
        assert!(seam_glyph_stream("A\nQ B").is_none());
        assert!(seam_glyph_stream("A\tQ B").is_none());
        lines[0].color = Some((1, 2, 3));
        lines[0].italic = true;
        let before = lines[0].clone();
        apply_overlapping_seam(&mut lines[0], &seam);
        assert_eq!(lines[0].text, "A Q B");
        let a = lines[0].bbox.as_ref().unwrap();
        let b = before.bbox.as_ref().unwrap();
        assert_eq!(
            (a.left, a.top, a.right, a.bottom),
            (b.left, b.top, b.right, b.bottom)
        );
        assert_eq!(lines[0].score, before.score);
        assert_eq!(lines[0].color, before.color);
        assert_eq!(lines[0].italic, before.italic);
    }

    #[test]
    fn eligibility_rejects_unsafe_text_multiline_and_invalid_polygons() {
        let (img, mut blocks, lines) = setup();
        assert!(prepare_overlapping_seam(&blocks, &lines, &img).is_some());
        blocks[1].text = "Q\nB".into();
        assert!(prepare_overlapping_seam(&blocks, &lines, &img).is_none());
        blocks[1].text = "Q B".into();
        assert!(
            prepare_overlapping_seam(&blocks, &[lines[0].clone(), lines[0].clone()], &img)
                .is_none()
        );
        blocks[1].box_points = rect(8, 0, 57, 9);
        assert!(prepare_overlapping_seam(&blocks, &lines, &img).is_none());
        assert!(prepare_overlapping_seam(&blocks[..1], &lines, &img).is_none());
    }

    #[test]
    fn coverage_requires_nonempty_ink_and_at_least_99_percent() {
        let mut img = image::RgbImage::from_pixel(100, 2, image::Rgb([255, 255, 255]));
        assert!(!full_line_ink_coverage(&img, &rect(0, 0, 98, 1)));
        for x in 0..100 {
            img.put_pixel(x, 0, image::Rgb([0, 0, 0]));
        }
        assert!(full_line_ink_coverage(&img, &rect(0, 0, 98, 1)));
        assert!(!full_line_ink_coverage(&img, &rect(0, 0, 97, 1)));
        assert!(full_line_ink_coverage(&img, &rect(0, 0, 101, 1)));
        assert!(!full_line_ink_coverage(&img, &rect(0, 0, 133, 1)));
        assert!(!full_line_ink_coverage(&img, &[]));
    }

    #[test]
    fn tesseract_requires_both_modes_exact_glyphs_boxes_and_one_component() {
        let (mut img, blocks, lines) = setup();
        let seam = prepare_overlapping_seam(&blocks, &lines, &img).unwrap();
        assert!(tesseract_seam_evidence_with(&img, &seam, |_| Some((
            "A Q B".into(),
            boxes()
        ))));
        assert!(!tesseract_seam_evidence_with(&img, &seam, |_| None));
        assert!(!tesseract_seam_evidence_with(&img, &seam, |psm| {
            if psm == 6 {
                Some(("A Q B".into(), boxes()))
            } else {
                None
            }
        }));
        assert!(!tesseract_seam_evidence_with(&img, &seam, |_| Some((
            "A Q Q B".into(),
            boxes()
        ))));
        assert!(!tesseract_seam_evidence_with(&img, &seam, |_| Some((
            "A Q B".into(),
            boxes().replace("Q 10", "X 10")
        ))));
        assert!(!tesseract_seam_evidence_with(&img, &seam, |psm| {
            let b = if psm == 6 {
                boxes()
            } else {
                boxes().replace("Q 10", "Q 9")
            };
            Some(("A Q B".into(), b))
        }));
        for shade in [0, 200] {
            img.put_pixel(15, 4, image::Rgb([shade, shade, shade]));
            assert!(!tesseract_seam_evidence_with(&img, &seam, |_| Some((
                "A Q B".into(),
                boxes()
            ))));
        }

        let (mut halo, blocks, lines) = setup();
        let seam = prepare_overlapping_seam(&blocks, &lines, &halo).unwrap();
        for x in 11..=12 {
            halo.put_pixel(x, 4, image::Rgb([200, 200, 200]));
        }
        assert!(tesseract_seam_evidence_with(&halo, &seam, |_| Some((
            "A Q B".into(),
            boxes()
        ))));
        halo.put_pixel(14, 4, image::Rgb([200, 200, 200]));
        halo.put_pixel(13, 4, image::Rgb([200, 200, 200]));
        assert!(!tesseract_seam_evidence_with(&halo, &seam, |_| Some((
            "A Q B".into(),
            boxes()
        ))));

        let (mut connected, blocks, lines) = setup();
        let seam = prepare_overlapping_seam(&blocks, &lines, &connected).unwrap();
        for x in 11..18 {
            connected.put_pixel(x, 4, image::Rgb([0, 0, 0]));
        }
        assert!(!tesseract_seam_evidence_with(&connected, &seam, |_| Some(
            ("A Q B".into(), boxes())
        )));

        let (img, blocks, lines) = setup();
        let mut seam = prepare_overlapping_seam(&blocks, &lines, &img).unwrap();
        seam.polygons = [rect(0, 0, 10, 9), rect(10, 0, 24, 9)];
        assert!(!tesseract_seam_evidence_with(&img, &seam, |_| Some((
            "A Q B".into(),
            boxes()
        ))));
    }

    #[test]
    fn neighbor_fringe_requires_a_bounded_connected_neighbor_core() {
        let (mut image, blocks, lines) = setup();
        let seam = prepare_overlapping_seam(&blocks, &lines, &image).unwrap();
        for x in 19..21 {
            for y in 3..5 {
                image.put_pixel(x, y, image::Rgb([0, 0, 0]));
            }
        }
        for x in 17..19 {
            for y in 3..5 {
                image.put_pixel(x, y, image::Rgb([200, 200, 200]));
            }
        }
        let evidence = |image: &image::RgbImage| {
            tesseract_seam_evidence_with(image, &seam, |_| Some(("A Q B".into(), boxes())))
        };
        assert!(evidence(&image));

        let mut disconnected = image.clone();
        for y in 3..5 {
            disconnected.put_pixel(18, y, image::Rgb([255, 255, 255]));
        }
        assert!(!evidence(&disconnected));

        let mut dark_fringe = image.clone();
        dark_fringe.put_pixel(17, 3, image::Rgb([0, 0, 0]));
        assert!(!evidence(&dark_fringe));

        let mut wider_fringe = image.clone();
        wider_fringe.put_pixel(16, 3, image::Rgb([200, 200, 200]));
        assert!(!evidence(&wider_fringe));

        let mut bridge = image.clone();
        for x in 11..17 {
            bridge.put_pixel(x, 4, image::Rgb([200, 200, 200]));
        }
        assert!(!evidence(&bridge));

        let mut tiny_core = image.clone();
        for (x, y) in [(19, 3), (20, 3), (20, 4)] {
            tiny_core.put_pixel(x, y, image::Rgb([255, 255, 255]));
        }
        assert!(!evidence(&tiny_core));

        let mut outside_core = image.clone();
        for x in 21..23 {
            outside_core.put_pixel(x, 4, image::Rgb([0, 0, 0]));
        }
        assert!(!evidence(&outside_core));

        let mut large = image::GrayImage::from_pixel(512, 512, image::Luma([255]));
        large.put_pixel(0, 212, image::Luma([200]));
        assert!(!is_neighbor_fringe(&large, &[(0, 212)], [1, 1, 400, 300]));
    }
}

#[cfg(test)]
mod quote_tests {
    use super::*;

    fn image_with_quotes(curved: bool) -> image::GrayImage {
        let mut image = image::GrayImage::from_pixel(20, 8, image::Luma([255]));
        for y in 1..6 {
            for x in [2, 4, 12, 14] {
                image.put_pixel(x, y, image::Luma([0]));
            }
        }
        if curved {
            image.put_pixel(14, 5, image::Luma([255]));
            image.put_pixel(15, 5, image::Luma([0]));
        }
        image
    }

    fn punctuation_image() -> image::GrayImage {
        let mut image = image::GrayImage::from_pixel(40, 30, image::Luma([255]));
        for y in (7..19).chain(23..27) {
            for x in 24..28 {
                image.put_pixel(x, y, image::Luma([0]));
            }
        }
        image
    }

    const PUNCTUATION_BOXES: &str = "A 1 3 8 23 0\n? 10 3 22 23 0\n\" 24 3 28 23 0\n";

    fn repair(text: &str, alternate: &str, boxes: &str, image: &image::GrayImage) -> String {
        recover_terminal_exclamation_with(
            text,
            || Ok(alternate.to_string()),
            || Ok(boxes.to_string()),
            || Ok(image.clone()),
        )
    }

    #[test]
    fn line_final_comma_requires_isolated_slanting_ink_below_baseline() {
        let boxes = "A 10 90 22 130 0\nB 30 90 42 130 0\nC 50 90 62 130 0\nD 70 90 82 130 0\n, 86 78 102 100 0\nX 10 20 22 40 0\nY 30 20 42 40 0\n";
        let mut image = image::GrayImage::from_pixel(140, 140, image::Luma([255]));
        for x0 in [10, 30, 50, 70] {
            for x in x0..x0 + 12 {
                for y in 10..50 {
                    image.put_pixel(x, y, image::Luma([0]));
                }
            }
        }
        for x0 in [10, 30] {
            for x in x0..x0 + 12 {
                for y in 100..120 {
                    image.put_pixel(x, y, image::Luma([0]));
                }
            }
        }
        for y in 40..62 {
            let start = 94 - (y - 40) / 4;
            let width = if y >= 57 { 5 } else { 8 };
            for x in start..start + width {
                image.put_pixel(x, y, image::Luma([0]));
            }
        }
        assert!(line_final_comma_pixel_evidence("ABCD,\nXY", boxes, &image));
        assert!(!line_final_comma_pixel_evidence("ABCE,\nXY", boxes, &image));
        assert!(!line_final_comma_pixel_evidence("ABC,D\nXY", boxes, &image));
        let mut period = image.clone();
        for x in 84..105 {
            for y in 35..65 {
                period.put_pixel(x, y, image::Luma([255]));
            }
        }
        for x in 92..100 {
            for y in 43..51 {
                period.put_pixel(x, y, image::Luma([0]));
            }
        }
        assert!(!line_final_comma_pixel_evidence(
            "ABCD,\nXY",
            boxes,
            &period
        ));
        let mut connected = image.clone();
        for x in 82..94 {
            connected.put_pixel(x, 49, image::Luma([0]));
        }
        assert!(!line_final_comma_pixel_evidence(
            "ABCD,\nXY",
            boxes,
            &connected
        ));
        let mut noise = image.clone();
        noise.put_pixel(110, 42, image::Luma([0]));
        assert!(!line_final_comma_pixel_evidence("ABCD,\nXY", boxes, &noise));
        let mut detached = image.clone();
        for x in 86..103 {
            detached.put_pixel(x, 54, image::Luma([255]));
        }
        assert!(!line_final_comma_pixel_evidence(
            "ABCD,\nXY",
            boxes,
            &detached
        ));
        let mut vertical = image.clone();
        for x in 84..105 {
            for y in 35..65 {
                vertical.put_pixel(x, y, image::Luma([255]));
            }
        }
        for x in 91..99 {
            for y in 40..62 {
                vertical.put_pixel(x, y, image::Luma([0]));
            }
        }
        assert!(!line_final_comma_pixel_evidence(
            "ABCD,\nXY",
            boxes,
            &vertical
        ));
        let mut slash = image.clone();
        for x in 84..105 {
            for y in 35..65 {
                slash.put_pixel(x, y, image::Luma([255]));
            }
        }
        for y in 40..62 {
            let start = 94 - (y - 40) / 4;
            for x in start..start + 8 {
                slash.put_pixel(x, y, image::Luma([0]));
            }
        }
        assert!(!line_final_comma_pixel_evidence("ABCD,\nXY", boxes, &slash));
        let descender_boxes = boxes.replace("D 70 90 82 130 0", "g 70 90 82 130 0");
        assert!(!line_final_comma_pixel_evidence(
            "ABCg,\nXY",
            &descender_boxes,
            &image
        ));
    }

    #[test]
    fn comma_spacing_requires_matching_boxes_and_clear_gap() {
        let boxes = "A 1 2 5 15 0\n, 6 2 8 8 0\nB 14 2 20 15 0\n";
        let mut image = image::GrayImage::from_pixel(30, 20, image::Luma([255]));
        for y in 5..18 {
            image.put_pixel(2, y, image::Luma([0]));
            image.put_pixel(15, y, image::Luma([0]));
        }
        for y in 12..18 {
            image.put_pixel(7, y, image::Luma([0]));
        }
        assert_eq!(
            comma_spacing_pixel_evidence("A, B", boxes, &image),
            HashSet::from([2])
        );
        assert!(comma_spacing_pixel_evidence("A,B", boxes, &image).is_empty());
        assert!(comma_spacing_pixel_evidence("A, C", boxes, &image).is_empty());
        assert!(comma_spacing_pixel_evidence("A,\nB", boxes, &image).is_empty());
        let invalid = "A 1 2 5 15 0\n, 9 2 8 8 0\nB 14 2 20 15 0\n";
        assert!(comma_spacing_pixel_evidence("A, B", invalid, &image).is_empty());
        let blank = image::GrayImage::from_pixel(30, 20, image::Luma([255]));
        assert!(comma_spacing_pixel_evidence("A, B", boxes, &blank).is_empty());
        let swapped_lines = "A 1 2 5 8 0\n, 6 2 8 8 0\nB 14 2 20 8 0\nA 1 10 5 17 0\n, 6 10 8 17 0\nB 14 10 20 17 0\n";
        assert!(comma_spacing_pixel_evidence("A, B\nA, B", swapped_lines, &image).is_empty());
        image.put_pixel(10, 15, image::Luma([0]));
        assert!(comma_spacing_pixel_evidence("A, B", boxes, &image).is_empty());
    }

    #[test]
    fn terminal_period_requires_separate_baseline_ink_and_matching_boxes() {
        let boxes = "A 2 6 7 22 0\nB 10 6 15 22 0\nC 18 6 23 22 0\n. 21 6 31 22 0\n";
        let mut image = image::GrayImage::from_pixel(36, 28, image::Luma([255]));
        for x in [2..7, 10..15, 18..23].into_iter().flatten() {
            for y in 6..22 {
                image.put_pixel(x, y, image::Luma([0]));
            }
        }
        for x in 27..31 {
            for y in 18..22 {
                image.put_pixel(x, y, image::Luma([0]));
            }
        }
        assert!(terminal_period_pixel_evidence("ABC.", boxes, &image));
        for (text, bad_boxes) in [
            ("ABD.", boxes),
            ("ABC?", boxes),
            ("ABC..", boxes),
            (
                "ABC.\n",
                "A 2 6 7 22 0\nB 10 6 15 22 0\nC 18 6 23 22 0\n. 31 6 27 22 0\n",
            ),
            (
                "ABC.",
                "A 2 6 7 22 0\nB 10 6 15 22 0\nC 18 6 23 22 0\n. 21 6 37 22 0\n",
            ),
            ("A\nBC.", boxes),
            (
                "ABC.",
                "A 2 6 7 22 0\nB 10 6 15 22 0\nC 18 6 29 22 0\n. 21 6 31 22 0\n",
            ),
        ] {
            assert!(!terminal_period_pixel_evidence(text, bad_boxes, &image));
        }
        let mut missing = image.clone();
        for x in 27..31 {
            for y in 18..22 {
                missing.put_pixel(x, y, image::Luma([255]));
            }
        }
        assert!(!terminal_period_pixel_evidence("ABC.", boxes, &missing));
        let mut high = missing.clone();
        for x in 27..31 {
            for y in 7..11 {
                high.put_pixel(x, y, image::Luma([0]));
            }
        }
        assert!(!terminal_period_pixel_evidence("ABC.", boxes, &high));
        let mut tall = image.clone();
        for y in 10..18 {
            tall.put_pixel(28, y, image::Luma([0]));
        }
        assert!(!terminal_period_pixel_evidence("ABC.", boxes, &tall));
        let mut sparse = image.clone();
        for (x, y) in [(27, 18), (28, 18), (29, 18), (27, 19), (28, 19), (27, 20)] {
            sparse.put_pixel(x, y, image::Luma([255]));
        }
        assert!(!terminal_period_pixel_evidence("ABC.", boxes, &sparse));
        let mut gap_ink = image.clone();
        gap_ink.put_pixel(25, 12, image::Luma([200]));
        assert!(!terminal_period_pixel_evidence("ABC.", boxes, &gap_ink));
        let mut trailing = image.clone();
        trailing.put_pixel(34, 2, image::Luma([0]));
        assert!(!terminal_period_pixel_evidence("ABC.", boxes, &trailing));
        // A clipped comma tail can extend below every recognized line box.
        let mut comma_tail = image.clone();
        comma_tail.put_pixel(28, 22, image::Luma([0]));
        comma_tail.put_pixel(27, 23, image::Luma([200]));
        assert!(!terminal_period_pixel_evidence("ABC.", boxes, &comma_tail));
        let mut detached_tail = image.clone();
        detached_tail.put_pixel(5, 26, image::Luma([200]));
        assert!(!terminal_period_pixel_evidence(
            "ABC.",
            boxes,
            &detached_tail
        ));

        let descender_boxes = "A 2 6 7 22 0\nB 10 2 15 22 0\nC 18 6 23 22 0\n. 21 2 31 22 0\n";
        let mut descender = image.clone();
        for x in 10..12 {
            for y in 22..26 {
                descender.put_pixel(x, y, image::Luma([0]));
            }
        }
        assert!(terminal_period_pixel_evidence(
            "ABC.",
            descender_boxes,
            &descender
        ));
        let mut inner_tail = descender.clone();
        for x in 28..30 {
            inner_tail.put_pixel(x, 22, image::Luma([0]));
        }
        assert!(!terminal_period_pixel_evidence(
            "ABC.",
            descender_boxes,
            &inner_tail
        ));
        let mut full_width_tail = descender.clone();
        for x in 27..31 {
            full_width_tail.put_pixel(x, 22, image::Luma([0]));
        }
        assert!(!terminal_period_pixel_evidence(
            "ABC.",
            descender_boxes,
            &full_width_tail
        ));
        let mut asymmetric = descender.clone();
        for x in [27, 30] {
            asymmetric.put_pixel(x, 21, image::Luma([255]));
        }
        assert!(!terminal_period_pixel_evidence(
            "ABC.",
            descender_boxes,
            &asymmetric
        ));
        let mut asymmetric_one_pixel = descender.clone();
        asymmetric_one_pixel.put_pixel(27, 21, image::Luma([255]));
        assert!(!terminal_period_pixel_evidence(
            "ABC.",
            descender_boxes,
            &asymmetric_one_pixel
        ));
        let mut punctured = image::GrayImage::from_pixel(12, 12, image::Luma([0]));
        punctured.put_pixel(3, 2, image::Luma([255]));
        punctured.put_pixel(8, 9, image::Luma([255]));
        assert!(!period_shape_is_symmetric(&punctured, 0, 12, 0, 11));
        let mut faint_tail = descender.clone();
        faint_tail.put_pixel(28, 23, image::Luma([200]));
        assert!(!terminal_period_pixel_evidence(
            "ABC.",
            descender_boxes,
            &faint_tail
        ));

        let multiline_boxes = "X 2 30 7 46 0\nY 10 30 15 46 0\nA 2 6 7 22 0\nB 10 6 15 22 0\nC 18 6 23 22 0\n. 21 6 31 22 0\n";
        let mut multiline = image::GrayImage::from_pixel(36, 52, image::Luma([255]));
        for x in [2..7, 10..15].into_iter().flatten() {
            for y in 6..22 {
                multiline.put_pixel(x, y, image::Luma([0]));
            }
        }
        for (x, y, pixel) in image.enumerate_pixels() {
            multiline.put_pixel(x, y + 24, *pixel);
        }
        assert!(terminal_period_pixel_evidence(
            "XY\nABC.",
            multiline_boxes,
            &multiline
        ));
        assert!(!terminal_period_pixel_evidence(
            "X\nYABC.",
            multiline_boxes,
            &multiline
        ));
        assert!(!terminal_period_pixel_evidence(
            "ABC.\nXY",
            multiline_boxes,
            &multiline
        ));
        assert!(!terminal_period_pixel_evidence(
            "XY\n\nABC.",
            multiline_boxes,
            &multiline
        ));
    }

    #[test]
    fn terminal_ellipsis_requires_three_separate_baseline_dots() {
        let boxes = "A 2 2 10 15 0\n. 16 2 19 5 0\n. 23 2 26 5 0\n. 30 2 33 5 0\n";
        let mut image = image::GrayImage::from_pixel(40, 20, image::Luma([255]));
        for x in [16..19, 23..26, 30..33].into_iter().flatten() {
            for y in 15..18 {
                image.put_pixel(x, y, image::Luma([0]));
            }
        }
        // A separate character above the baseline establishes the text line.
        for y in 5..18 {
            image.put_pixel(4, y, image::Luma([0]));
        }
        assert!(terminal_ellipsis_pixel_evidence("A...", boxes, &image));
        // Some recognizers shift one dot box and combine two others.
        let shifted = "A 2 2 10 15 0\n. 11 2 15 5 0\n. 16 2 19 5 0\n. 23 2 33 5 0\n";
        assert!(terminal_ellipsis_pixel_evidence("A...", shifted, &image));
        // A recognizer can also overlap the first two box estimates.
        let overlapping = "A 2 2 10 15 0\n. 11 2 18 5 0\n. 16 2 19 5 0\n. 23 2 33 5 0\n";
        assert!(terminal_ellipsis_pixel_evidence(
            "A...",
            overlapping,
            &image
        ));
        assert!(!terminal_ellipsis_pixel_evidence("B...", boxes, &image));
        assert!(!terminal_ellipsis_pixel_evidence("A..", boxes, &image));
        let misplaced = "A 2 2 10 15 0\n. 11 2 14 5 0\n. 13 2 15 5 0\n. 14 2 15 5 0\n";
        assert!(!terminal_ellipsis_pixel_evidence("A...", misplaced, &image));
        let mut interposed_ink = image.clone();
        interposed_ink.put_pixel(28, 13, image::Luma([0]));
        assert!(!terminal_ellipsis_pixel_evidence(
            "A...",
            boxes,
            &interposed_ink
        ));
        let mut two_dots = image.clone();
        for x in 16..19 {
            for y in 15..18 {
                two_dots.put_pixel(x, y, image::Luma([255]));
            }
        }
        assert!(!terminal_ellipsis_pixel_evidence("A...", boxes, &two_dots));
        two_dots.put_pixel(17, 17, image::Luma([0]));
        assert!(!terminal_ellipsis_pixel_evidence("A...", boxes, &two_dots));
        // The bottom of a narrow letter is not an independent punctuation dot.
        for y in 6..15 {
            image.put_pixel(17, y, image::Luma([0]));
        }
        assert!(!terminal_ellipsis_pixel_evidence("A...", boxes, &image));
    }

    #[test]
    fn terminal_exclamation_requires_exact_alternate_and_matching_boxes() {
        let image = punctuation_image();
        assert_eq!(repair("A?\"", "A?!", PUNCTUATION_BOXES, &image), "A?!");
        assert_eq!(repair("A ?\"", "A ?!", PUNCTUATION_BOXES, &image), "A ?!");
        for alternate in ["B?!", "A ?!", "A?", "A?.", "A?!X", "A?\n!"] {
            assert_eq!(repair("A?\"", alternate, PUNCTUATION_BOXES, &image), "A?\"");
        }
        for boxes in [
            "",
            "bad",
            "A 1 3 8 23 0\n? 10 3 22 23 0\n",
            "B 1 3 8 23 0\n? 10 3 22 23 0\n\" 24 3 28 23 0\n",
            "A 1 3 8 23 0\n? 10 3 22 23 0\n\" 24 3 80 23 0\n",
            "A 1 3 8 23 0\n? 10 3 22 23 0\n\" 24 3 28 23 1\n",
        ] {
            assert_eq!(repair("A?\"", "A?!", boxes, &image), "A?\"");
        }
    }

    #[test]
    fn terminal_exclamation_abstains_for_open_or_mismatched_quotes() {
        use std::cell::Cell;
        for prefix in [
            "\"A",
            "'A",
            "“A",
            "‘A",
            "„A",
            "‚A",
            "«A",
            "‹A",
            "「A",
            "『A",
            "｢A",
            "《A",
            "〈A",
            "〝A",
            "“A»",
            "A”",
            "l'amour",
            "l’amour",
        ] {
            assert!(
                !balanced_quote_context(prefix),
                "unbalanced prefix {prefix:?}"
            );
            let calls = Cell::new(0);
            let text = format!("{prefix}?\"");
            let result = recover_terminal_exclamation_with(
                &text,
                || {
                    calls.set(calls.get() + 1);
                    Ok(format!("{prefix}?!"))
                },
                || {
                    calls.set(calls.get() + 1);
                    Ok(PUNCTUATION_BOXES.to_string())
                },
                || {
                    calls.set(calls.get() + 1);
                    Ok(punctuation_image())
                },
            );
            assert_eq!(result, text);
            assert_eq!(calls.get(), 0);
        }
    }

    #[test]
    fn terminal_exclamation_accepts_balanced_multilingual_quote_context() {
        for prefix in [
            "A",
            "\"A\"",
            "'A'",
            "“A”",
            "‘A’",
            "„A“",
            "‚A‘",
            "«A»",
            "‹A›",
            "「A」",
            "『A』",
            "｢A｣",
            "《A》",
            "〈A〉",
            "〝A〞",
            "〝A〟",
            "「“A”」",
        ] {
            assert!(balanced_quote_context(prefix), "balanced prefix {prefix:?}");
        }
        let image = punctuation_image();
        for (open, close) in [('"', '"'), ('“', '”'), ('«', '»'), ('「', '」')] {
            let text = format!("{open}A{close}?\"");
            let alternate = format!("{open}A{close}?!");
            let boxes = format!("{open} 0 3 1 23 0\nA 1 3 8 23 0\n{close} 8 3 9 23 0\n? 10 3 22 23 0\n\" 24 3 28 23 0\n");
            assert_eq!(repair(&text, &alternate, &boxes, &image), alternate);
        }
    }

    #[test]
    fn terminal_exclamation_rejects_ink_after_the_terminal_box() {
        let image = punctuation_image();
        assert_eq!(repair("A?\"", "A?!", PUNCTUATION_BOXES, &image), "A?!");
        let mut omitted_quote = image.clone();
        for y in 7..13 {
            for x in [30, 33] {
                omitted_quote.put_pixel(x, y, image::Luma([0]));
            }
        }
        assert_eq!(
            repair("A?\"", "A?!", PUNCTUATION_BOXES, &omitted_quote),
            "A?\""
        );
        for (x, y, value) in [(28, 0, 0), (39, 29, 0), (30, 20, 254)] {
            let mut outside = image.clone();
            outside.put_pixel(x, y, image::Luma([value]));
            assert_eq!(repair("A?\"", "A?!", PUNCTUATION_BOXES, &outside), "A?\"");
        }
    }

    #[test]
    fn terminal_exclamation_rejects_ink_between_the_character_boxes() {
        let image = punctuation_image();
        assert_eq!(repair("A?\"", "A?!", PUNCTUATION_BOXES, &image), "A?!");

        // The final box can contain one quote stroke and a stray dot, while
        // Tesseract omits a second stroke just outside that box.
        let mut omitted_stroke = image.clone();
        for y in 7..19 {
            omitted_stroke.put_pixel(22, y, image::Luma([0]));
        }
        assert_eq!(
            repair("A?\"", "A?!", PUNCTUATION_BOXES, &omitted_stroke),
            "A?\""
        );
        for (x, y, value) in [(22, 0, 0), (23, 29, 0), (22, 20, 254)] {
            let mut outside = image.clone();
            outside.put_pixel(x, y, image::Luma([value]));
            assert_eq!(repair("A?\"", "A?!", PUNCTUATION_BOXES, &outside), "A?\"");
        }

        let touching_boxes = "A 1 3 8 23 0\n? 10 3 24 23 0\n\" 24 3 28 23 0\n";
        assert_eq!(repair("A?\"", "A?!", touching_boxes, &image), "A?!");
    }

    #[test]
    fn terminal_exclamation_rejects_quotes_and_noisy_or_incomplete_shapes() {
        let image = punctuation_image();
        let mut quote = image.clone();
        for y in 7..27 {
            for x in 24..28 {
                quote.put_pixel(x, y, image::Luma([255]));
            }
        }
        for y in 7..13 {
            for x in [24, 27] {
                quote.put_pixel(x, y, image::Luma([0]));
            }
        }
        let mut missing_dot = image.clone();
        for y in 23..27 {
            for x in 24..28 {
                missing_dot.put_pixel(x, y, image::Luma([255]));
            }
        }
        let mut joined = image.clone();
        for y in 19..23 {
            for x in 24..28 {
                joined.put_pixel(x, y, image::Luma([0]));
            }
        }
        let mut noise = image.clone();
        noise.put_pixel(25, 21, image::Luma([0]));
        let mut connected_quote_and_dot = image.clone();
        for y in 8..19 {
            for x in 25..27 {
                connected_quote_and_dot.put_pixel(x, y, image::Luma([255]));
            }
        }
        for negative in [quote, missing_dot, joined, noise, connected_quote_and_dot] {
            assert_eq!(repair("A?\"", "A?!", PUNCTUATION_BOXES, &negative), "A?\"");
        }
    }

    #[test]
    fn terminal_exclamation_abstains_on_multiline_and_subprocess_failure() {
        use std::cell::Cell;
        let calls = Cell::new(0);
        let unchanged = recover_terminal_exclamation_with(
            "A\nB?\"",
            || {
                calls.set(calls.get() + 1);
                Ok("A\nB?!".to_string())
            },
            || {
                calls.set(calls.get() + 1);
                Ok(String::new())
            },
            || {
                calls.set(calls.get() + 1);
                Ok(punctuation_image())
            },
        );
        assert_eq!(unchanged, "A\nB?\"");
        assert_eq!(calls.get(), 0);

        let unchanged = recover_terminal_exclamation_with(
            "A?!",
            || {
                calls.set(calls.get() + 1);
                Ok(String::new())
            },
            || {
                calls.set(calls.get() + 1);
                Ok(String::new())
            },
            || {
                calls.set(calls.get() + 1);
                Ok(punctuation_image())
            },
        );
        assert_eq!(unchanged, "A?!");
        assert_eq!(calls.get(), 0);

        let unchanged = recover_terminal_exclamation_with(
            "A?\"",
            || {
                std::process::Command::new("/nonexistent/dpn-ocr-test-command").output()?;
                Ok("A?!".to_string())
            },
            || {
                calls.set(calls.get() + 1);
                Ok(String::new())
            },
            || {
                calls.set(calls.get() + 1);
                Ok(punctuation_image())
            },
        );
        assert_eq!(unchanged, "A?\"");
        assert_eq!(calls.get(), 0);

        let unchanged = recover_terminal_exclamation_with(
            "A?\"",
            || {
                calls.set(calls.get() + 1);
                Ok("A?!".to_string())
            },
            || {
                calls.set(calls.get() + 1);
                bail!("box subprocess failed")
            },
            || {
                calls.set(calls.get() + 1);
                Ok(punctuation_image())
            },
        );
        assert_eq!(unchanged, "A?\"");
        assert_eq!(calls.get(), 2);

        let unchanged = recover_terminal_exclamation_with(
            "A?\"",
            || Ok("A?!".to_string()),
            || Ok(PUNCTUATION_BOXES.to_string()),
            || bail!("image unavailable"),
        );
        assert_eq!(unchanged, "A?\"");
    }

    #[test]
    fn straight_quote_recovery_requires_matching_source_glyphs() {
        let boxes = "\" 2 2 5 7 0\nA 8 2 9 7 0\n” 12 2 15 7 0\n";
        let straight = image_with_quotes(false);
        assert_eq!(
            recover_straight_quote_from_boxes("\"A”", boxes, &straight),
            "\"A\""
        );
        assert_eq!(
            recover_straight_quote_from_boxes("\"B”", boxes, &straight),
            "\"B”"
        );
        assert_eq!(
            recover_straight_quote_from_boxes("\"A”", "bad", &straight),
            "\"A”"
        );
        let curved = image_with_quotes(true);
        assert_eq!(
            recover_straight_quote_from_boxes("\"A”", boxes, &curved),
            "\"A”"
        );
        let curly_boxes = "“ 2 2 5 7 0\nA 8 2 9 7 0\n” 12 2 16 7 0\n";
        assert_eq!(
            recover_straight_quote_from_boxes("“A”", curly_boxes, &curved),
            "“A”"
        );
        let mixed_boxes = "\" 2 2 5 7 0\nA 8 2 9 7 0\n” 12 2 16 7 0\n";
        assert_eq!(
            recover_straight_quote_from_boxes("\"A”", mixed_boxes, &curved),
            "\"A”"
        );
        let invalid_boxes = "\" 2 2 5 7 0\nA 8 2 9 7 0\n” 12 2 100 7 0\n";
        assert_eq!(
            recover_straight_quote_from_boxes("\"A”", invalid_boxes, &straight),
            "\"A”"
        );
    }
}
