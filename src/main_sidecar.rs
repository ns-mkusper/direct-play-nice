use anyhow::{Context, Result};
use log::{debug, info, warn};
use std::collections::HashMap;
use std::ffi::CStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};

use crate::{subtitle_ocr, OcrEngine, OcrFormat, SubMode};

/// Stores data for OcrSidecarRequest.
pub(super) struct OcrSidecarRequest<'a> {
    pub(super) input_file: &'a CStr,
    pub(super) mux_source_file: &'a CStr,
    pub(super) output_file: &'a CStr,
    pub(super) sub_mode: SubMode,
    pub(super) default_ocr_language: Option<&'a str>,
    pub(super) ocr_engine: OcrEngine,
    pub(super) ocr_format: OcrFormat,
    pub(super) ocr_external_command: Option<&'a str>,
    pub(super) ocr_write_srt_sidecar: bool,
}

/// Executes the post process ocr subtitles routine.
pub(super) fn post_process_ocr_subtitles(request: OcrSidecarRequest<'_>) -> Result<()> {
    let OcrSidecarRequest {
        input_file,
        mux_source_file,
        output_file,
        sub_mode,
        default_ocr_language,
        ocr_engine,
        ocr_format,
        ocr_external_command,
        ocr_write_srt_sidecar,
    } = request;

    if matches!(sub_mode, SubMode::Skip) {
        return Ok(());
    }

    let ocr_work_dir = OcrWorkDir::create()?;

    let tracks = subtitle_ocr::convert_bitmap_subtitles(
        input_file,
        ocr_work_dir.path(),
        sub_mode,
        default_ocr_language,
        ocr_engine,
        ocr_format,
        ocr_external_command,
    )?;
    subtitle_ocr::mux_text_tracks_from(mux_source_file, output_file, &tracks)?;
    write_ocr_srt_sidecars(output_file, &tracks, ocr_write_srt_sidecar)?;

    Ok(())
}

/// Stores data for OcrWorkDir.
struct OcrWorkDir {
    path: PathBuf,
}

static OCR_WORK_DIR_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Implements behavior for `OcrWorkDir`.
impl OcrWorkDir {
    /// Executes the create routine.
    fn create() -> Result<Self> {
        let mut path = std::env::temp_dir();
        let now = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .map(|d| d.as_millis())
            .unwrap_or(0);
        let nonce = OCR_WORK_DIR_COUNTER.fetch_add(1, Ordering::Relaxed);
        path.push(format!(
            "direct-play-nice-ocr-{}-{}-{}",
            std::process::id(),
            now,
            nonce
        ));
        fs::create_dir(&path)
            .with_context(|| format!("creating OCR temporary directory '{}'", path.display()))?;
        Ok(Self { path })
    }

    /// Executes the path routine.
    fn path(&self) -> &Path {
        &self.path
    }
}

/// Implements behavior for `OcrWorkDir`.
impl Drop for OcrWorkDir {
    /// Executes the drop routine.
    fn drop(&mut self) {
        if let Err(err) = fs::remove_dir_all(&self.path) {
            debug!(
                "Failed to clean OCR temporary directory '{}': {}",
                self.path.display(),
                err
            );
        }
    }
}

/// Executes the sanitize sidecar language routine.
fn sanitize_sidecar_language(language: &str) -> String {
    let normalized: String = language
        .trim()
        .to_ascii_lowercase()
        .chars()
        .filter(|ch| ch.is_ascii_alphanumeric() || *ch == '-' || *ch == '_')
        .collect();
    if normalized.is_empty() {
        "und".to_string()
    } else {
        normalized
    }
}

/// Executes the sidecar path for track routine.
pub(super) fn sidecar_path_for_track(
    output_path: &Path,
    language: &str,
    language_occurrence: usize,
) -> PathBuf {
    let parent = output_path.parent().unwrap_or_else(|| Path::new("."));
    let stem = output_path
        .file_stem()
        .and_then(|value| value.to_str())
        .unwrap_or("output");
    let suffix = if language_occurrence <= 1 {
        format!("{stem}.{language}.srt")
    } else {
        format!("{stem}.{language}.{language_occurrence}.srt")
    };
    parent.join(suffix)
}

/// Executes the write ocr srt sidecars routine.
pub(super) fn write_ocr_srt_sidecars(
    output_file: &CStr,
    tracks: &[subtitle_ocr::OcrSubtitleTrack],
    enabled: bool,
) -> Result<()> {
    if !enabled || tracks.is_empty() {
        return Ok(());
    }

    let output_path = PathBuf::from(output_file.to_string_lossy().into_owned());
    let mut language_counts: HashMap<String, usize> = HashMap::new();

    for track in tracks {
        if !matches!(track.format, OcrFormat::Srt) {
            warn!(
                "Skipping OCR sidecar write for '{}' because format is {:?} (SRT required).",
                track.subtitle_path.display(),
                track.format
            );
            continue;
        }

        let language = sanitize_sidecar_language(&track.language);
        let occurrence = {
            let count = language_counts.entry(language.clone()).or_insert(0);
            *count += 1;
            *count
        };
        let sidecar_path = sidecar_path_for_track(&output_path, &language, occurrence);
        fs::copy(&track.subtitle_path, &sidecar_path).with_context(|| {
            format!(
                "writing OCR sidecar '{}' from '{}'",
                sidecar_path.display(),
                track.subtitle_path.display()
            )
        })?;
        info!(
            "Wrote OCR sidecar subtitle '{}' (language={}, source='{}').",
            sidecar_path.display(),
            language,
            track.subtitle_path.display()
        );
    }

    Ok(())
}
