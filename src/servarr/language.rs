//! Language requirement checks for Sonarr/Radarr-triggered runs.
//!
//! This module intentionally only inspects the already-imported media file.
//! When requirements are not met, the API layer may ask Servarr for manual-search
//! results and only grab a specific replacement when the release metadata proves
//! the desired languages are available.

use anyhow::{Context, Result};
use log::info;
use rsmpeg::avformat::{AVFormatContextInput, AVFormatContextOutput};
use rsmpeg::avutil::AVDictionary;
use rsmpeg::ffi;
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::ffi::CString;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Default, Serialize, Deserialize, PartialEq, Eq)]
pub struct LanguageRequirements {
    pub enabled: bool,
    pub audio: Vec<String>,
    pub subtitles: Vec<String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct UntaggedRetagOptions {
    pub audio_language: Option<String>,
    pub subtitle_language: Option<String>,
    pub dry_run: bool,
}

impl UntaggedRetagOptions {
    pub fn enabled(&self) -> bool {
        self.audio_language.is_some() || self.subtitle_language.is_some()
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct UntaggedRetagReport {
    pub audio_streams: usize,
    pub subtitle_streams: usize,
    pub dry_run: bool,
}

impl UntaggedRetagReport {
    pub fn changed(&self) -> bool {
        self.audio_streams > 0 || self.subtitle_streams > 0
    }
}

impl LanguageRequirements {
    pub fn is_effective(&self) -> bool {
        self.enabled && (!self.audio.is_empty() || !self.subtitles.is_empty())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct LanguageCheckReport {
    pub present_audio: Vec<String>,
    pub present_subtitles: Vec<String>,
    pub missing_audio: Vec<String>,
    pub missing_subtitles: Vec<String>,
}

impl LanguageCheckReport {
    pub fn satisfied(&self) -> bool {
        self.missing_audio.is_empty() && self.missing_subtitles.is_empty()
    }

    pub fn describe_missing(&self) -> String {
        let mut parts = Vec::new();
        if !self.missing_audio.is_empty() {
            parts.push(format!("audio [{}]", self.missing_audio.join(", ")));
        }
        if !self.missing_subtitles.is_empty() {
            parts.push(format!("subtitles [{}]", self.missing_subtitles.join(", ")));
        }
        parts.join("; ")
    }
}

pub fn parse_language_list(raw: Option<&str>) -> Vec<String> {
    parse_language_tokens(raw.unwrap_or(""))
}

pub fn parse_language_tokens(raw: &str) -> Vec<String> {
    raw.split([',', '/', ';', '|', ' '])
        .map(str::trim)
        .filter(|entry| !entry.is_empty())
        .filter_map(normalize_language_tag)
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect()
}

pub fn report_from_present(
    present_audio: Vec<String>,
    present_subtitles: Vec<String>,
    requirements: &LanguageRequirements,
) -> LanguageCheckReport {
    check_sets(present_audio, present_subtitles, requirements)
}

pub fn check_file(path: &Path, requirements: &LanguageRequirements) -> Result<LanguageCheckReport> {
    let path_cstr = path_to_cstr(path)?;
    let ictx = AVFormatContextInput::open(path_cstr.as_c_str())?;
    let mut audio = BTreeSet::new();
    let mut subtitles = BTreeSet::new();

    for stream in ictx.streams() {
        let cp = stream.codecpar();
        let language = stream
            .metadata()
            .as_deref()
            .and_then(extract_language_tag_from_metadata)
            .and_then(|tag| normalize_language_tag(&tag));
        let Some(language) = language else {
            continue;
        };
        match cp.codec_type {
            ffi::AVMEDIA_TYPE_AUDIO => {
                audio.insert(language);
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                subtitles.insert(language);
            }
            _ => {}
        }
    }

    Ok(check_sets(
        audio.into_iter().collect(),
        subtitles.into_iter().collect(),
        requirements,
    ))
}

pub fn retag_unknown_streams(
    path: &Path,
    requirements: &LanguageRequirements,
    options: &UntaggedRetagOptions,
) -> Result<UntaggedRetagReport> {
    if !options.enabled() {
        return Ok(UntaggedRetagReport::default());
    }

    let initial_report = check_file(path, requirements)?;
    let should_tag_audio =
        options.audio_language.is_some() && !initial_report.missing_audio.is_empty();
    let should_tag_subtitles =
        options.subtitle_language.is_some() && !initial_report.missing_subtitles.is_empty();
    if !should_tag_audio && !should_tag_subtitles {
        return Ok(UntaggedRetagReport::default());
    }

    let path_cstr = path_to_cstr(path)?;
    let input_ctx = AVFormatContextInput::open(path_cstr.as_c_str())?;
    let mut report = UntaggedRetagReport {
        dry_run: options.dry_run,
        ..Default::default()
    };

    for stream in input_ctx.streams() {
        let cp = stream.codecpar();
        if stream_has_known_language(stream.metadata().as_deref()) {
            continue;
        }
        match cp.codec_type {
            ffi::AVMEDIA_TYPE_AUDIO if should_tag_audio => report.audio_streams += 1,
            ffi::AVMEDIA_TYPE_SUBTITLE if should_tag_subtitles => report.subtitle_streams += 1,
            _ => {}
        }
    }

    if !report.changed() || options.dry_run {
        return Ok(report);
    }

    drop(input_ctx);
    remux_with_retagged_unknown_streams(path, options, should_tag_audio, should_tag_subtitles)?;
    info!(
        "Retagged {} unknown audio stream(s) and {} unknown subtitle stream(s) in '{}'.",
        report.audio_streams,
        report.subtitle_streams,
        path.display()
    );
    Ok(report)
}

fn remux_with_retagged_unknown_streams(
    path: &Path,
    options: &UntaggedRetagOptions,
    should_tag_audio: bool,
    should_tag_subtitles: bool,
) -> Result<()> {
    let path_cstr = path_to_cstr(path)?;
    let mut input_ctx = AVFormatContextInput::open(path_cstr.as_c_str())?;
    let tmp_path = retag_temp_path(path);
    let tmp_cstr = CString::new(tmp_path.to_string_lossy().to_string())
        .context("retag temp path has interior NUL")?;
    let mut output_ctx = AVFormatContextOutput::create(tmp_cstr.as_c_str())?;
    let is_mkv = path
        .extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| matches!(ext.to_ascii_lowercase().as_str(), "mkv" | "mka" | "mks"));

    let mut stream_index_map = Vec::with_capacity(input_ctx.streams().len());
    for stream in input_ctx.streams() {
        let mut out_stream = output_ctx.new_stream();
        out_stream.set_time_base(stream.time_base);
        let mut codecpar = stream.codecpar().clone();
        if is_mkv {
            unsafe { (*codecpar.as_mut_ptr()).codec_tag = 0 };
        }
        out_stream.set_codecpar(codecpar);

        let cp = stream.codecpar();
        let metadata = if !stream_has_known_language(stream.metadata().as_deref()) {
            match cp.codec_type {
                ffi::AVMEDIA_TYPE_AUDIO if should_tag_audio => set_language_metadata(
                    stream.metadata().as_deref().cloned(),
                    options.audio_language.as_deref(),
                ),
                ffi::AVMEDIA_TYPE_SUBTITLE if should_tag_subtitles => set_language_metadata(
                    stream.metadata().as_deref().cloned(),
                    options.subtitle_language.as_deref(),
                ),
                _ => stream.metadata().as_deref().cloned(),
            }
        } else {
            stream.metadata().as_deref().cloned()
        };
        out_stream.set_metadata(metadata);
        stream_index_map.push(out_stream.index);
    }

    output_ctx
        .write_header(&mut None)
        .context("failed to write retagged output header")?;

    loop {
        let mut packet = match input_ctx.read_packet()? {
            Some(packet) => packet,
            None => break,
        };
        let input_index = packet.stream_index as usize;
        let output_index = stream_index_map
            .get(input_index)
            .copied()
            .with_context(|| format!("stream index {input_index} not mapped during retag remux"))?;
        let input_time_base = input_ctx.streams()[input_index].time_base;
        let output_time_base = output_ctx.streams()[output_index as usize].time_base;
        packet.set_stream_index(output_index);
        packet.rescale_ts(input_time_base, output_time_base);
        output_ctx.interleaved_write_frame(&mut packet)?;
    }

    output_ctx.write_trailer()?;
    drop(output_ctx);
    drop(input_ctx);
    replace_with_temp(&tmp_path, path, "retagging unknown stream languages")
}

fn stream_has_known_language(metadata: Option<&AVDictionary>) -> bool {
    metadata
        .and_then(extract_language_tag_from_metadata)
        .and_then(|tag| normalize_language_tag(&tag))
        .is_some()
}

fn set_language_metadata(
    metadata: Option<AVDictionary>,
    language: Option<&str>,
) -> Option<AVDictionary> {
    let language = language?;
    let key = CString::new("language").ok()?;
    let value = CString::new(language).ok()?;
    Some(
        metadata
            .map(|dict| dict.set(&key, &value, 0))
            .unwrap_or_else(|| AVDictionary::new(&key, &value, 0)),
    )
}

fn retag_temp_path(path: &Path) -> PathBuf {
    let extension = path
        .extension()
        .and_then(|ext| ext.to_str())
        .unwrap_or("tmp");
    let stem = path
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("media");
    path.with_file_name(format!("{stem}.dpn-retag.tmp.{extension}"))
}

fn replace_with_temp(tmp_path: &Path, path: &Path, context: &str) -> Result<()> {
    #[cfg(windows)]
    if path.exists() {
        fs::remove_file(path)
            .with_context(|| format!("removing '{}' before {context}", path.display()))?;
    }
    fs::rename(tmp_path, path)
        .with_context(|| format!("replacing '{}' after {context}", path.display()))
}

fn check_sets(
    present_audio: Vec<String>,
    present_subtitles: Vec<String>,
    requirements: &LanguageRequirements,
) -> LanguageCheckReport {
    let audio_set = present_audio.iter().cloned().collect::<BTreeSet<_>>();
    let subtitle_set = present_subtitles.iter().cloned().collect::<BTreeSet<_>>();
    let missing_audio = requirements
        .audio
        .iter()
        .filter(|lang| !audio_set.contains(*lang))
        .cloned()
        .collect();
    let missing_subtitles = requirements
        .subtitles
        .iter()
        .filter(|lang| !subtitle_set.contains(*lang))
        .cloned()
        .collect();

    LanguageCheckReport {
        present_audio,
        present_subtitles,
        missing_audio,
        missing_subtitles,
    }
}

fn path_to_cstr(path: &Path) -> Result<std::ffi::CString> {
    let raw = path.to_string_lossy();
    Ok(std::ffi::CString::new(raw.as_bytes())?)
}

fn extract_language_tag_from_metadata(dict: &AVDictionary) -> Option<String> {
    for entry in dict.iter() {
        if entry
            .key()
            .to_string_lossy()
            .eq_ignore_ascii_case("language")
        {
            let value = entry.value().to_string_lossy().trim().to_string();
            if !value.is_empty() {
                return Some(value);
            }
        }
    }
    None
}

pub(super) fn normalize_language_tag(input: &str) -> Option<String> {
    let primary = input
        .trim()
        .to_ascii_lowercase()
        .split(['-', '_'])
        .next()
        .unwrap_or("")
        .to_string();
    if primary.is_empty() || primary == "und" || primary == "unknown" {
        return None;
    }

    let normalized = match primary.as_str() {
        "en" | "eng" | "english" => "eng",
        "fr" | "fre" | "fra" | "french" => "fra",
        "es" | "spa" | "spanish" => "spa",
        "de" | "ger" | "deu" | "german" => "deu",
        "it" | "ita" | "italian" => "ita",
        "pt" | "por" | "portuguese" => "por",
        "ja" | "jpn" | "japanese" => "jpn",
        "ko" | "kor" | "korean" => "kor",
        "zh" | "zho" | "chi" | "chinese" => "zho",
        "ru" | "rus" | "russian" => "rus",
        "nl" => "nld",
        "pl" => "pol",
        "sv" => "swe",
        "no" | "nb" => "nor",
        "da" => "dan",
        "fi" => "fin",
        "cs" => "ces",
        "cz" | "cze" => "ces",
        "el" | "gre" => "ell",
        "he" => "heb",
        "hi" => "hin",
        "ar" => "ara",
        "tr" => "tur",
        "th" => "tha",
        "vi" => "vie",
        "id" => "ind",
        "uk" => "ukr",
        _ => primary.as_str(),
    };

    Some(normalized.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_language_list_normalizes_and_deduplicates() {
        assert_eq!(
            parse_language_list(Some("en, eng, fr-FR, spa, und, ")),
            vec!["eng", "fra", "spa"]
        );
    }

    #[test]
    fn retag_unknown_streams_dry_run_reports_unknown_audio_without_writing() {
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".into()],
            subtitles: Vec::new(),
        };
        let options = UntaggedRetagOptions {
            audio_language: Some("eng".to_string()),
            subtitle_language: None,
            dry_run: true,
        };
        let path = Path::new("/definitely/missing/file.mp4");
        assert!(retag_unknown_streams(path, &requirements, &options).is_err());
    }

    #[cfg(feature = "ffmpeg-cli-tests")]
    fn ffmpeg_present() -> bool {
        std::process::Command::new("ffmpeg")
            .arg("-version")
            .output()
            .is_ok_and(|out| out.status.success())
    }

    #[cfg(feature = "ffmpeg-cli-tests")]
    fn make_audio_fixture(path: &Path, language: &str) {
        let status = std::process::Command::new("ffmpeg")
            .args([
                "-hide_banner",
                "-loglevel",
                "error",
                "-y",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=1000:duration=0.2",
                "-c:a",
                "aac",
                "-metadata:s:a:0",
                &format!("language={language}"),
            ])
            .arg(path)
            .status()
            .expect("run ffmpeg audio fixture");
        assert!(status.success(), "ffmpeg audio fixture failed");
    }

    #[cfg(feature = "ffmpeg-cli-tests")]
    #[test]
    fn retag_unknown_streams_sets_unknown_audio_language() {
        if !ffmpeg_present() {
            eprintln!(
                "skipping retag_unknown_streams_sets_unknown_audio_language: ffmpeg not found"
            );
            return;
        }
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join("audio.mp4");
        make_audio_fixture(&path, "und");
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".into()],
            subtitles: Vec::new(),
        };
        assert_eq!(
            check_file(&path, &requirements).unwrap().missing_audio,
            vec!["eng"]
        );
        let report = retag_unknown_streams(
            &path,
            &requirements,
            &UntaggedRetagOptions {
                audio_language: Some("eng".to_string()),
                subtitle_language: None,
                dry_run: false,
            },
        )
        .unwrap();
        assert_eq!(report.audio_streams, 1);
        assert!(check_file(&path, &requirements).unwrap().satisfied());
    }

    #[cfg(feature = "ffmpeg-cli-tests")]
    #[test]
    fn retag_unknown_streams_does_not_overwrite_known_audio_language() {
        if !ffmpeg_present() {
            eprintln!("skipping retag_unknown_streams_does_not_overwrite_known_audio_language: ffmpeg not found");
            return;
        }
        let tmp = tempfile::tempdir().unwrap();
        let path = tmp.path().join("audio.mp4");
        make_audio_fixture(&path, "jpn");
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".into()],
            subtitles: Vec::new(),
        };
        let report = retag_unknown_streams(
            &path,
            &requirements,
            &UntaggedRetagOptions {
                audio_language: Some("eng".to_string()),
                subtitle_language: None,
                dry_run: false,
            },
        )
        .unwrap();
        assert!(!report.changed());
        let check = check_file(&path, &requirements).unwrap();
        assert_eq!(check.present_audio, vec!["jpn"]);
        assert_eq!(check.missing_audio, vec!["eng"]);
    }

    #[test]
    fn check_sets_reports_missing_required_languages() {
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".into(), "jpn".into()],
            subtitles: vec!["eng".into(), "spa".into()],
        };

        let report = check_sets(
            vec!["eng".into()],
            vec!["eng".into(), "fra".into()],
            &requirements,
        );

        assert!(!report.satisfied());
        assert_eq!(report.missing_audio, vec!["jpn"]);
        assert_eq!(report.missing_subtitles, vec!["spa"]);
        assert_eq!(report.describe_missing(), "audio [jpn]; subtitles [spa]");
    }
}
