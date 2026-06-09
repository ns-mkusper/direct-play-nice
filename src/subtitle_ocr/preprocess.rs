//! Optional OCR image preprocessing for rendered subtitle bitmaps.
//!
//! The OCR pipeline renders every subtitle rectangle to a grayscale PGM (P5) image first. This
//! module preserves that interface so downstream engines, temporary files, and word-segmentation
//! recovery continue to operate on the same `.pgm` path regardless of preprocessing mode.

use anyhow::{bail, Result};

use crate::OcrPreprocess;

pub(super) fn preprocess_ocr_pgm(pgm: &[u8], mode: OcrPreprocess) -> Result<Vec<u8>> {
    match mode {
        OcrPreprocess::None => Ok(pgm.to_vec()),
        OcrPreprocess::OpenCvBasic | OcrPreprocess::OpenCvSubtitle => {
            preprocess_ocr_pgm_with_opencv(pgm, mode)
        }
    }
}

#[cfg(not(feature = "opencv-preprocess"))]
fn preprocess_ocr_pgm_with_opencv(_pgm: &[u8], mode: OcrPreprocess) -> Result<Vec<u8>> {
    bail!(
        "--ocr-preprocess={} requires building direct-play-nice with --features opencv-preprocess",
        mode_label(mode)
    )
}

#[cfg(feature = "opencv-preprocess")]
fn preprocess_ocr_pgm_with_opencv(pgm: &[u8], mode: OcrPreprocess) -> Result<Vec<u8>> {
    use anyhow::Context;
    use opencv::core::{self, Mat, Point, Size};
    use opencv::imgproc;
    use opencv::prelude::*;

    let image = parse_pgm_p5(pgm)?;
    let src_1d = Mat::from_slice(image.pixels).context("creating OpenCV Mat from PGM pixels")?;
    let src = src_1d
        .reshape(1, image.height as i32)
        .context("reshaping OpenCV PGM Mat")?;

    let mut output = Mat::default();
    match mode {
        OcrPreprocess::OpenCvBasic => {
            let mut blurred = Mat::default();
            imgproc::median_blur(&src, &mut blurred, 3).context("OpenCV median blur")?;
            imgproc::threshold(
                &blurred,
                &mut output,
                0.0,
                255.0,
                imgproc::THRESH_BINARY | imgproc::THRESH_OTSU,
            )
            .context("OpenCV Otsu threshold")?;
        }
        OcrPreprocess::OpenCvSubtitle => {
            let mut blurred = Mat::default();
            imgproc::gaussian_blur(
                &src,
                &mut blurred,
                Size::new(3, 3),
                0.0,
                0.0,
                core::BORDER_DEFAULT,
            )
            .context("OpenCV Gaussian blur")?;
            let block_size = adaptive_block_size(image.width, image.height);
            let mut thresholded = Mat::default();
            imgproc::adaptive_threshold(
                &blurred,
                &mut thresholded,
                255.0,
                imgproc::ADAPTIVE_THRESH_GAUSSIAN_C,
                imgproc::THRESH_BINARY,
                block_size,
                7.0,
            )
            .context("OpenCV adaptive threshold")?;
            let kernel = imgproc::get_structuring_element(
                imgproc::MORPH_RECT,
                Size::new(2, 2),
                Point::new(-1, -1),
            )
            .context("OpenCV morphology kernel")?;
            imgproc::morphology_ex(
                &thresholded,
                &mut output,
                imgproc::MORPH_CLOSE,
                &kernel,
                Point::new(-1, -1),
                1,
                core::BORDER_CONSTANT,
                imgproc::morphology_default_border_value()?,
            )
            .context("OpenCV morphology close")?;
        }
        OcrPreprocess::None => unreachable!("OpenCV preprocessing is not used for no-op mode"),
    }

    let bytes = output
        .data_bytes()
        .context("reading OpenCV output pixels")?;
    let expected_len = image
        .width
        .checked_mul(image.height)
        .ok_or_else(|| anyhow::anyhow!("PGM dimensions overflow"))?;
    if bytes.len() != expected_len {
        bail!(
            "OpenCV preprocessing returned {} bytes for {}x{} PGM (expected {})",
            bytes.len(),
            image.width,
            image.height,
            expected_len
        );
    }

    Ok(write_pgm_p5(image.width, image.height, bytes))
}

#[cfg(feature = "opencv-preprocess")]
fn adaptive_block_size(width: usize, height: usize) -> i32 {
    let min_dim = width.min(height).max(3);
    let mut block = (min_dim / 8).clamp(3, 31) as i32;
    if block % 2 == 0 {
        block += 1;
    }
    block
}

#[cfg(not(feature = "opencv-preprocess"))]
fn mode_label(mode: OcrPreprocess) -> &'static str {
    match mode {
        OcrPreprocess::None => "none",
        OcrPreprocess::OpenCvBasic => "open-cv-basic",
        OcrPreprocess::OpenCvSubtitle => "open-cv-subtitle",
    }
}

#[cfg(any(feature = "opencv-preprocess", test))]
struct PgmImage<'a> {
    width: usize,
    height: usize,
    pixels: &'a [u8],
}

#[cfg(any(feature = "opencv-preprocess", test))]
fn parse_pgm_p5(pgm: &[u8]) -> Result<PgmImage<'_>> {
    use anyhow::Context;

    let mut cursor = 0usize;
    let magic = next_pgm_token(pgm, &mut cursor).context("PGM missing magic header")?;
    if magic != b"P5" {
        bail!("OCR preprocessing expected binary PGM/P5 input");
    }
    let width = parse_usize_token(next_pgm_token(pgm, &mut cursor).context("PGM missing width")?)?;
    let height =
        parse_usize_token(next_pgm_token(pgm, &mut cursor).context("PGM missing height")?)?;
    let maxval =
        parse_usize_token(next_pgm_token(pgm, &mut cursor).context("PGM missing maxval")?)?;
    if width == 0 || height == 0 {
        bail!("PGM dimensions must be non-zero");
    }
    if maxval != 255 {
        bail!("OCR preprocessing expected 8-bit PGM maxval 255, got {maxval}");
    }
    if cursor >= pgm.len() || !pgm[cursor].is_ascii_whitespace() {
        bail!("PGM header must be followed by one whitespace byte before pixels");
    }
    cursor += 1;
    let expected_len = width
        .checked_mul(height)
        .ok_or_else(|| anyhow::anyhow!("PGM dimensions overflow"))?;
    let pixels = pgm
        .get(cursor..cursor + expected_len)
        .ok_or_else(|| anyhow::anyhow!("PGM pixel payload is shorter than declared dimensions"))?;
    if pgm.len() != cursor + expected_len {
        bail!("PGM contains trailing data after pixel payload");
    }
    Ok(PgmImage {
        width,
        height,
        pixels,
    })
}

#[cfg(any(feature = "opencv-preprocess", test))]
fn next_pgm_token<'a>(pgm: &'a [u8], cursor: &mut usize) -> Option<&'a [u8]> {
    loop {
        while *cursor < pgm.len() && pgm[*cursor].is_ascii_whitespace() {
            *cursor += 1;
        }
        if *cursor >= pgm.len() {
            return None;
        }
        if pgm[*cursor] != b'#' {
            break;
        }
        while *cursor < pgm.len() && pgm[*cursor] != b'\n' {
            *cursor += 1;
        }
    }

    let start = *cursor;
    while *cursor < pgm.len() && !pgm[*cursor].is_ascii_whitespace() {
        *cursor += 1;
    }
    Some(&pgm[start..*cursor])
}

#[cfg(any(feature = "opencv-preprocess", test))]
fn parse_usize_token(token: &[u8]) -> Result<usize> {
    use anyhow::Context;

    let text = std::str::from_utf8(token).context("PGM header token is not UTF-8")?;
    text.parse::<usize>()
        .with_context(|| format!("invalid PGM integer token '{text}'"))
}

#[cfg(feature = "opencv-preprocess")]
fn write_pgm_p5(width: usize, height: usize, pixels: &[u8]) -> Vec<u8> {
    let mut output = format!("P5\n{width} {height}\n255\n").into_bytes();
    output.extend_from_slice(pixels);
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn none_preprocess_preserves_pgm_bytes() {
        let pgm = b"P5\n2 2\n255\n\x00\x7f\x80\xff";
        assert_eq!(preprocess_ocr_pgm(pgm, OcrPreprocess::None).unwrap(), pgm);
    }

    #[test]
    fn parses_pgm_with_comment() {
        let pgm = b"P5\n# generated\n2 1\n255\n\x01\x02";
        let parsed = parse_pgm_p5(pgm).unwrap();
        assert_eq!(parsed.width, 2);
        assert_eq!(parsed.height, 1);
        assert_eq!(parsed.pixels, &[1, 2]);
    }

    #[cfg(not(feature = "opencv-preprocess"))]
    #[test]
    fn opencv_mode_requires_feature_when_not_compiled_in() {
        let pgm = b"P5\n1 1\n255\n\xff";
        let err = preprocess_ocr_pgm(pgm, OcrPreprocess::OpenCvBasic).unwrap_err();
        assert!(err.to_string().contains("--features opencv-preprocess"));
    }
}
