//! Language requirement checks for Sonarr/Radarr-triggered runs.
//!
//! Conversion checks inspect only embedded streams. Audit checks may also use
//! conservatively matched sidecars. Tags and coverage are evidence, not proof
//! of language, quality, or complete dialogue.
//! When requirements are not met, the API layer may ask Servarr for manual-search
//! results and only grab a specific replacement when the release metadata proves
//! the desired languages are available.

#[cfg(unix)]
use anyhow::Context;
use anyhow::{bail, Result};
use log::info;
#[cfg(unix)]
use log::warn;
use rsmpeg::avformat::AVFormatContextInput;
#[cfg(unix)]
use rsmpeg::avformat::{AVFormatContextOutput, AVIOContextContainer, AVIOContextCustom};
use rsmpeg::avutil::AVDictionary;
#[cfg(unix)]
use rsmpeg::avutil::AVMem;
use rsmpeg::ffi;
use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::ffi::CString;
use std::io::Read;
use std::path::Path;
#[cfg(unix)]
use std::path::PathBuf;

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
    let mut tags = BTreeSet::new();
    for group in raw
        .split([',', '/', ';', '|'])
        .map(str::trim)
        .filter(|group| !group.is_empty())
    {
        if let Some(tag) = canonical_arr_language(group) {
            tags.insert(tag.to_owned());
        } else {
            tags.extend(group.split_whitespace().filter_map(normalize_language_tag));
        }
    }
    tags.into_iter().collect()
}

pub fn report_from_present(
    present_audio: Vec<String>,
    present_subtitles: Vec<String>,
    requirements: &LanguageRequirements,
) -> LanguageCheckReport {
    check_sets(present_audio, present_subtitles, requirements)
}

pub fn check_file(path: &Path, requirements: &LanguageRequirements) -> Result<LanguageCheckReport> {
    check_file_impl(path, requirements, false)
}

/// Audit-only check: matching sidecars do not necessarily survive conversion.
pub(super) fn check_file_with_sidecars(
    path: &Path,
    requirements: &LanguageRequirements,
) -> Result<LanguageCheckReport> {
    check_file_impl(path, requirements, true)
}

fn check_file_impl(
    path: &Path,
    requirements: &LanguageRequirements,
    include_sidecars: bool,
) -> Result<LanguageCheckReport> {
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
            ffi::AVMEDIA_TYPE_SUBTITLE
                if !subtitle_track_is_partial(stream.disposition, stream.metadata().as_deref()) =>
            {
                subtitles.insert(language);
            }
            _ => {}
        }
    }

    if include_sidecars && !requirements.subtitles.is_empty() {
        let duration_ms = u64::try_from(ictx.duration).ok().map(|us| us / 1000);
        subtitles.extend(sidecar_languages(path, duration_ms));
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
    retag_unknown_streams_impl(path, requirements, options, false)
}

pub(super) fn retag_unknown_streams_for_original(
    path: &Path,
    requirements: &LanguageRequirements,
    options: &UntaggedRetagOptions,
) -> Result<UntaggedRetagReport> {
    retag_unknown_streams_impl(path, requirements, options, true)
}

fn retag_unknown_streams_impl(
    path: &Path,
    requirements: &LanguageRequirements,
    options: &UntaggedRetagOptions,
    only_inferred_audio_missing: bool,
) -> Result<UntaggedRetagReport> {
    if !options.enabled() {
        return Ok(UntaggedRetagReport::default());
    }

    let initial_report = check_file(path, requirements)?;
    let inferred = options
        .audio_language
        .as_deref()
        .and_then(normalize_language_tag);
    let should_tag_audio = options.audio_language.is_some()
        && if only_inferred_audio_missing {
            inferred.as_ref().is_some_and(|tag| {
                initial_report
                    .missing_audio
                    .iter()
                    .any(|missing| normalize_language_tag(missing).as_ref() == Some(tag))
            })
        } else {
            !initial_report.missing_audio.is_empty()
        };
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
            ffi::AVMEDIA_TYPE_SUBTITLE
                if should_tag_subtitles
                    && !subtitle_track_is_partial(
                        stream.disposition,
                        stream.metadata().as_deref(),
                    ) =>
            {
                report.subtitle_streams += 1;
            }
            _ => {}
        }
    }

    if report.changed() && input_ctx.nb_chapters != 0 {
        bail!("cannot safely retag a file with chapters; original left unchanged");
    }
    if report.changed() && options.dry_run {
        #[cfg(unix)]
        secure_retag::check_path(path)?;
        #[cfg(not(unix))]
        bail!("safe retagging is unsupported on this platform");
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

#[cfg(not(unix))]
fn remux_with_retagged_unknown_streams(
    _path: &Path,
    _options: &UntaggedRetagOptions,
    _should_tag_audio: bool,
    _should_tag_subtitles: bool,
) -> Result<()> {
    bail!("safe retagging is unsupported on this platform")
}

#[cfg(unix)]
fn remux_with_retagged_unknown_streams(
    path: &Path,
    options: &UntaggedRetagOptions,
    should_tag_audio: bool,
    should_tag_subtitles: bool,
) -> Result<()> {
    remux_with_retagged_unknown_streams_hook(
        path,
        options,
        should_tag_audio,
        should_tag_subtitles,
        |_| Ok(()),
    )
}

#[cfg(unix)]
fn remux_with_retagged_unknown_streams_hook(
    path: &Path,
    options: &UntaggedRetagOptions,
    should_tag_audio: bool,
    should_tag_subtitles: bool,
    before_output_open: impl FnOnce(&Path) -> Result<()>,
) -> Result<()> {
    let mut stage = secure_retag::Stage::reserve(path)?;
    let path_cstr = path_to_cstr(stage.source_path())?;
    let mut input_ctx = AVFormatContextInput::open(path_cstr.as_c_str())?;
    // rsmpeg has no supported chapter-copy API. Refuse before creating output.
    if input_ctx.nb_chapters != 0 {
        bail!("cannot safely retag a file with chapters; original left unchanged");
    }
    let expected_metadata = container_metadata(input_ctx.as_ptr())?;
    let tmp_cstr = path_to_cstr(stage.stage_path())?;
    stage.verify()?;
    before_output_open(stage.stage_path())?;
    let io = retag_output_io(stage.output_file()?);
    let mut output_ctx = AVFormatContextOutput::builder()
        .filename(tmp_cstr.as_c_str())
        .io_context(AVIOContextContainer::Custom(io))
        .build()?;
    if output_ctx.oformat().flags & ffi::AVFMT_NOFILE as i32 != 0 {
        bail!("retag output format does not support descriptor-bound staging");
    }
    let format = output_ctx.oformat();
    let format_name = format.name().to_bytes();
    if !matches!(
        format_name,
        b"matroska" | b"webm" | b"mp4" | b"mov" | b"ipod"
    ) {
        bail!("retag output muxer is not supported for descriptor-bound staging");
    }
    stage.verify()?;
    // Avoid substituting this build's encoder identifier for existing metadata.
    unsafe {
        (*output_ctx.as_mut_ptr()).flags |= ffi::AVFMT_FLAG_BITEXACT as i32;
    }
    // FFmpeg owns and frees the copied dictionary with the output context.
    let result = unsafe {
        ffi::av_dict_copy(
            &mut (*output_ctx.as_mut_ptr()).metadata,
            (*input_ctx.as_ptr()).metadata,
            ffi::AV_DICT_MULTIKEY as i32,
        )
    };
    if result < 0 {
        bail!("failed to copy container metadata (FFmpeg error {result})");
    }
    warn!(
        "Retag remux does not preserve filesystem ACLs or extended attributes for '{}'.",
        path.display()
    );
    let is_mkv = path
        .extension()
        .and_then(|ext| ext.to_str())
        .is_some_and(|ext| matches!(ext.to_ascii_lowercase().as_str(), "mkv" | "mka" | "mks"));

    let mut stream_index_map = Vec::with_capacity(input_ctx.streams().len());
    for stream in input_ctx.streams() {
        let mut out_stream = output_ctx.new_stream();
        out_stream.set_time_base(stream.time_base);
        out_stream.set_disposition(stream.disposition);
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
                ffi::AVMEDIA_TYPE_SUBTITLE
                    if should_tag_subtitles
                        && !subtitle_track_is_partial(
                            stream.disposition,
                            stream.metadata().as_deref(),
                        ) =>
                {
                    set_language_metadata(
                        stream.metadata().as_deref().cloned(),
                        options.subtitle_language.as_deref(),
                    )
                }
                _ => stream.metadata().as_deref().cloned(),
            }
        } else {
            stream.metadata().as_deref().cloned()
        };
        out_stream.set_metadata(metadata);
        stream_index_map.push(out_stream.index);
    }

    // The normal faststart option can reopen the pathname; never use it with
    // descriptor-bound staging. Mux directly to the held output descriptor.
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
    stage.verify()?;
    let output = AVFormatContextInput::open(tmp_cstr.as_c_str())
        .context("failed to verify retagged container")?;
    let mut actual_metadata = container_metadata(output.as_ptr())?;
    for entry in expected_metadata {
        let Some(index) = actual_metadata.iter().position(|actual| *actual == entry) else {
            bail!(
                "retagged container did not preserve metadata key '{}'; original left unchanged",
                String::from_utf8_lossy(&entry.0)
            );
        };
        actual_metadata.remove(index);
    }
    drop(output);
    stage.apply_source_permissions()?;
    stage.promote()
}

#[cfg(unix)]
fn retag_output_io(file: std::fs::File) -> AVIOContextCustom {
    use std::io::{Seek, SeekFrom, Write};
    use std::sync::{Arc, Mutex};

    let file = Arc::new(Mutex::new(file));
    let writer = Arc::clone(&file);
    AVIOContextCustom::alloc_context(
        AVMem::new(32 * 1024),
        true,
        Vec::new(),
        None,
        Some(Box::new(move |_, bytes| {
            let Ok(mut file) = writer.lock() else {
                return -libc::EIO;
            };
            match file.write_all(bytes) {
                Ok(()) => bytes.len() as i32,
                Err(_) => -libc::EIO,
            }
        })),
        Some(Box::new(move |_, offset, whence| {
            let Ok(mut file) = file.lock() else {
                return i64::from(-libc::EIO);
            };
            if whence & ffi::AVSEEK_SIZE as i32 != 0 {
                return file
                    .metadata()
                    .ok()
                    .and_then(|m| i64::try_from(m.len()).ok())
                    .unwrap_or(i64::from(-libc::EIO));
            }
            let whence = whence & !(ffi::AVSEEK_FORCE as i32);
            let position = match whence {
                libc::SEEK_SET => match u64::try_from(offset) {
                    Ok(offset) => SeekFrom::Start(offset),
                    Err(_) => return i64::from(-libc::EINVAL),
                },
                libc::SEEK_CUR => SeekFrom::Current(offset),
                libc::SEEK_END => SeekFrom::End(offset),
                _ => return i64::from(-libc::EINVAL),
            };
            file.seek(position)
                .ok()
                .and_then(|n| i64::try_from(n).ok())
                .unwrap_or(i64::from(-libc::EIO))
        })),
    )
}

// Values are compared byte-for-byte; FFmpeg metadata keys are case-insensitive.
#[cfg(unix)]
fn container_metadata(context: *const ffi::AVFormatContext) -> Result<Vec<(Vec<u8>, Vec<u8>)>> {
    let dictionary = unsafe { (*context).metadata };
    let mut previous = std::ptr::null();
    let mut entries = Vec::new();
    loop {
        let entry = unsafe { ffi::av_dict_iterate(dictionary, previous) };
        if entry.is_null() {
            break;
        }
        let (key, value) = unsafe { ((*entry).key, (*entry).value) };
        if key.is_null() || value.is_null() {
            bail!("invalid container metadata");
        }
        let key = unsafe { std::ffi::CStr::from_ptr(key) }
            .to_bytes()
            .to_ascii_lowercase();
        let value = unsafe { std::ffi::CStr::from_ptr(value) }
            .to_bytes()
            .to_vec();
        entries.push((key, value));
        previous = entry;
    }
    Ok(entries)
}

fn subtitle_track_is_partial(disposition: i32, metadata: Option<&AVDictionary>) -> bool {
    disposition & ffi::AV_DISPOSITION_FORCED as i32 != 0
        || metadata.is_some_and(subtitle_title_is_partial)
}

fn subtitle_title_is_partial(metadata: &AVDictionary) -> bool {
    metadata.iter().any(|entry| {
        entry.key().to_string_lossy().eq_ignore_ascii_case("title")
            && partial_subtitle_label(&entry.value().to_string_lossy())
    })
}

fn partial_subtitle_label(label: &str) -> bool {
    let words: Vec<_> = label
        .split(|c: char| !c.is_ascii_alphanumeric())
        .filter(|word| !word.is_empty())
        .collect();
    let has = |term: &str| words.iter().any(|word| word.eq_ignore_ascii_case(term));
    if has("forced") {
        return true;
    }
    let signs_or_songs = has("signs") || has("songs");
    let only = has("only") || has("just");
    let no_dialogue = words.windows(2).any(|pair| {
        (pair[0].eq_ignore_ascii_case("no") || pair[0].eq_ignore_ascii_case("without"))
            && pair[1].eq_ignore_ascii_case("dialogue")
    });
    (signs_or_songs && (only || no_dialogue || !has("dialogue")))
        || words.windows(2).any(|pair| {
            (pair[0].eq_ignore_ascii_case("sign") || pair[0].eq_ignore_ascii_case("song"))
                && (pair[1].eq_ignore_ascii_case("only") || pair[1].eq_ignore_ascii_case("just"))
        })
}

// Unlike stream metadata, a filename must use an explicitly recognised language:
// normalize_language_tag intentionally passes through arbitrary unknown tokens.
fn sidecar_language(tag: &str) -> Option<String> {
    // A bare .hi is also a common hearing-impaired marker, so it is ambiguous.
    if tag.eq_ignore_ascii_case("hi") {
        return None;
    }
    let mut components = tag.split(['-', '_']);
    let primary = components.next()?.to_ascii_lowercase();
    if !matches!(
        primary.as_str(),
        "en" | "eng"
            | "english"
            | "fr"
            | "fre"
            | "fra"
            | "french"
            | "es"
            | "spa"
            | "spanish"
            | "de"
            | "ger"
            | "deu"
            | "german"
            | "it"
            | "ita"
            | "italian"
            | "pt"
            | "por"
            | "portuguese"
            | "ja"
            | "jpn"
            | "japanese"
            | "ko"
            | "kor"
            | "korean"
            | "zh"
            | "zho"
            | "chi"
            | "chinese"
            | "ru"
            | "rus"
            | "russian"
            | "nl"
            | "nld"
            | "dut"
            | "pl"
            | "pol"
            | "sv"
            | "swe"
            | "no"
            | "nb"
            | "nor"
            | "nn"
            | "nob"
            | "nno"
            | "da"
            | "dan"
            | "fi"
            | "fin"
            | "cs"
            | "cz"
            | "cze"
            | "ces"
            | "el"
            | "gre"
            | "ell"
            | "he"
            | "heb"
            | "hi"
            | "hin"
            | "ar"
            | "ara"
            | "tr"
            | "tur"
            | "th"
            | "tha"
            | "vi"
            | "vie"
            | "id"
            | "ind"
            | "uk"
            | "ukr"
    ) || components.any(|part| {
        !((part.len() == 2 || part.len() == 4) && part.bytes().all(|b| b.is_ascii_alphabetic())
            || part.len() == 3 && part.bytes().all(|b| b.is_ascii_digit()))
    }) {
        return None;
    }
    normalize_language_tag(&primary)
}

// Require an exact media stem, known language and qualifiers. Prefix-only
// matching could associate another episode or unrelated version of a film.
fn sidecar_languages(media: &Path, duration_ms: Option<u64>) -> BTreeSet<String> {
    let mut languages = BTreeSet::new();
    let Some(duration_ms) = duration_ms.filter(|d| (10_000..=86_400_000).contains(d)) else {
        return languages;
    };
    let Some(stem) = media.file_stem().and_then(|s| s.to_str()) else {
        return languages;
    };
    let directory = media
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or(Path::new("."));
    let Ok(entries) = std::fs::read_dir(directory) else {
        return languages;
    };
    for entry in entries.flatten() {
        let name = entry.file_name();
        let Some(name) = name.to_str() else { continue };
        let Some(prefix) = name.get(..stem.len()) else {
            continue;
        };
        #[cfg(windows)]
        let same_stem = prefix.eq_ignore_ascii_case(stem);
        #[cfg(not(windows))]
        let same_stem = prefix == stem;
        if !same_stem {
            continue;
        }
        let Some(suffix) = name.get(stem.len()..).and_then(|s| s.strip_prefix('.')) else {
            continue;
        };
        let mut parts: Vec<_> = suffix.split('.').collect();
        let Some(extension) = parts.pop() else {
            continue;
        };
        if !extension.eq_ignore_ascii_case("srt") && !extension.eq_ignore_ascii_case("vtt") {
            continue;
        }
        let Some(language) = parts.first().and_then(|part| sidecar_language(part)) else {
            continue;
        };
        if partial_subtitle_label(suffix)
            || suffix
                .split(|c: char| !c.is_ascii_alphanumeric())
                .any(|word| word.eq_ignore_ascii_case("sign") || word.eq_ignore_ascii_case("song"))
            || !parts[1..].iter().all(|part| {
                ["sdh", "cc", "hi", "default"]
                    .iter()
                    .any(|label| part.eq_ignore_ascii_case(label))
                    || (!part.is_empty() && part.bytes().all(|b| b.is_ascii_digit()))
            })
        {
            continue;
        }
        if valid_text_sidecar(&entry.path(), extension, duration_ms, &language) {
            languages.insert(language);
        }
    }
    languages
}

// The gate is deliberately bounded and conservative. It cannot prove language,
// accuracy or complete dialogue; sparse dialogue can fail and signs can pass.
fn valid_text_sidecar(path: &Path, extension: &str, duration_ms: u64, language: &str) -> bool {
    const MAX_BYTES: u64 = 8 * 1024 * 1024;
    if !(10_000..=86_400_000).contains(&duration_ms)
        || !std::fs::symlink_metadata(path)
            .is_ok_and(|m| m.is_file() && m.len() > 0 && m.len() <= MAX_BYTES)
    {
        return false;
    }
    let mut options = std::fs::OpenOptions::new();
    options.read(true);
    #[cfg(unix)]
    {
        use std::os::unix::fs::OpenOptionsExt;
        options.custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK);
    }
    #[cfg(windows)]
    {
        use std::os::windows::fs::OpenOptionsExt;
        const FILE_FLAG_OPEN_REPARSE_POINT: u32 = 0x0020_0000;
        options.custom_flags(FILE_FLAG_OPEN_REPARSE_POINT);
    }
    let Ok(file) = options.open(path) else {
        return false;
    };
    if !file
        .metadata()
        .is_ok_and(|m| m.is_file() && m.len() > 0 && m.len() <= MAX_BYTES)
    {
        return false;
    }
    let mut bytes = Vec::new();
    if file.take(MAX_BYTES + 1).read_to_end(&mut bytes).is_err() || bytes.len() as u64 > MAX_BYTES {
        return false;
    }
    let Ok(text) = std::str::from_utf8(&bytes) else {
        return false;
    };
    let lines: Vec<_> = text
        .trim_start_matches('\u{feff}')
        .lines()
        .map(str::trim)
        .collect();
    let vtt = extension.eq_ignore_ascii_case("vtt");
    let lines = if vtt {
        if !lines.first().is_some_and(|line| {
            *line == "WEBVTT" || line.starts_with("WEBVTT ") || line.starts_with("WEBVTT\t")
        }) || lines.get(1).is_none_or(|line| !line.is_empty())
        {
            return false;
        }
        &lines[2..]
    } else {
        &lines[..]
    };
    let mut cue_count = 0u64;
    let mut substantive_count = 0u64;
    let mut unique_text = BTreeSet::new();
    let mut quarters = [0u64; 4];
    let mut last_start = None;
    let mut last_end = 0u64;
    let mut visible_ms = 0u64;
    for block in lines.split(|line| line.is_empty()) {
        let Some(first) = block.first() else { continue };
        if vtt
            && (*first == "NOTE"
                || first.starts_with("NOTE ")
                || *first == "STYLE"
                || *first == "REGION")
        {
            continue;
        }
        let timing_index = if cue_timing(first, vtt).is_some() {
            0
        } else if (vtt || (!first.is_empty() && first.bytes().all(|b| b.is_ascii_digit())))
            && block
                .get(1)
                .is_some_and(|line| cue_timing(line, vtt).is_some())
        {
            1
        } else {
            return false;
        };
        if block
            .get(timing_index + 1)
            .is_none_or(|line| line.is_empty())
            || block[timing_index + 1..]
                .iter()
                .any(|line| line.contains("-->"))
        {
            return false;
        }
        let Some((start, end)) = cue_timing(block[timing_index], vtt) else {
            return false;
        };
        if end > duration_ms.saturating_add(1_000)
            || start >= duration_ms
            || last_start.is_some_and(|previous| start <= previous)
            || start.saturating_sub(last_end) > (duration_ms / 4).min(120_000)
        {
            return false;
        }
        let end = end.min(duration_ms);
        let midpoint = start + (end - start) / 2;
        quarters[((midpoint * 4 / duration_ms) as usize).min(3)] += 1;
        visible_ms += end.saturating_sub(start.max(last_end)).min(10_000);
        last_start = Some(start);
        last_end = last_end.max(end);
        cue_count += 1;
        let text = block[timing_index + 1..].join(" ");
        let mut visible = String::new();
        let mut in_tag = false;
        for ch in text.chars() {
            match ch {
                '<' => in_tag = true,
                '>' if in_tag => in_tag = false,
                _ if !in_tag => visible.push(ch),
                _ => {}
            }
        }
        if in_tag {
            return false;
        }
        if matches!(language, "jpn" | "zho") {
            // Japanese and Chinese commonly have no whitespace between words.
            // Count visible script characters without inferring the language.
            let alphabetic = visible.chars().filter(|ch| ch.is_alphabetic()).count();
            let cjk = visible.chars().filter(|ch| is_cjk_character(*ch)).count();
            if alphabetic >= 4 && cjk >= 3 {
                substantive_count += 1;
                unique_text.insert(
                    visible
                        .chars()
                        .filter(|ch| ch.is_alphabetic())
                        .collect::<String>(),
                );
            }
        } else {
            let words = visible
                .split(|c: char| !c.is_alphabetic())
                .filter(|s| s.chars().count() >= 2);
            if words.clone().count() >= 2
                && words
                    .clone()
                    .map(|word| word.chars().count())
                    .sum::<usize>()
                    >= 8
            {
                substantive_count += 1;
                unique_text.insert(words.collect::<Vec<_>>().join(" ").to_lowercase());
            }
        }
    }
    // At least 8 cues and one per 15 seconds, 75% substantive, four distinct
    // texts, each quarter represented, no gap over 25% or two minutes, and 5%
    // non-overlapping visible time (with at most ten seconds credited per cue).
    let min_cues = 8.max(duration_ms.div_ceil(15_000));
    cue_count >= min_cues
        && substantive_count * 4 >= cue_count * 3
        && unique_text.len() >= 4
        && quarters.iter().all(|count| *count >= 2.max(min_cues / 8))
        && duration_ms.saturating_sub(last_end) <= (duration_ms / 4).min(120_000)
        && visible_ms >= duration_ms.div_ceil(20)
}

fn is_cjk_character(ch: char) -> bool {
    matches!(ch, '\u{3040}'..='\u{30ff}' | '\u{31f0}'..='\u{31ff}' | '\u{3400}'..='\u{4dbf}'
        | '\u{4e00}'..='\u{9fff}' | '\u{f900}'..='\u{faff}' | '\u{ff66}'..='\u{ff9f}'
        | '\u{20000}'..='\u{2fa1f}')
}

fn cue_timing(line: &str, vtt: bool) -> Option<(u64, u64)> {
    let (start, tail) = line.split_once("-->")?;
    let mut parts = tail.split_whitespace();
    let end = parts.next()?;
    if parts.any(|setting| {
        !vtt || !setting.split_once(':').is_some_and(|(name, value)| {
            matches!(
                name,
                "line" | "position" | "size" | "align" | "vertical" | "region"
            ) && !value.is_empty()
        })
    }) {
        return None;
    }
    let start = cue_timestamp(start.trim(), vtt)?;
    let end = cue_timestamp(end, vtt)?;
    (end > start).then_some((start, end))
}

fn cue_timestamp(value: &str, vtt: bool) -> Option<u64> {
    let (whole, millis) = value.split_once(if vtt { '.' } else { ',' })?;
    if millis.len() != 3 || !millis.bytes().all(|b| b.is_ascii_digit()) {
        return None;
    }
    let parts: Vec<_> = whole.split(':').collect();
    if !(parts.len() == 3 || (vtt && parts.len() == 2))
        || parts
            .iter()
            .any(|p| p.is_empty() || !p.bytes().all(|b| b.is_ascii_digit()))
    {
        return None;
    }
    let seconds = parts[parts.len() - 1];
    let minutes = parts[parts.len() - 2];
    if seconds.len() != 2 || minutes.len() != 2 {
        return None;
    }
    let seconds: u64 = seconds.parse().ok()?;
    let minutes: u64 = minutes.parse().ok()?;
    if seconds >= 60 || minutes >= 60 {
        return None;
    }
    let hours: u64 = if parts.len() == 3 {
        if parts[0].len() < 2 {
            return None;
        }
        parts[0].parse().ok()?
    } else {
        0
    };
    hours
        .checked_mul(3600)?
        .checked_add(minutes * 60 + seconds)?
        .checked_mul(1000)?
        .checked_add(millis.parse().ok()?)
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

#[cfg(unix)]
mod secure_retag {
    use super::retag_temp_path;
    use anyhow::{bail, Context, Result};
    use std::ffi::CString;
    use std::fs::{File, Metadata};
    use std::os::fd::{AsRawFd, FromRawFd};
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
    use std::path::{Component, Path, PathBuf};

    #[derive(Clone, Copy, PartialEq, Eq)]
    struct Snapshot {
        dev: u64,
        ino: u64,
        size: u64,
        mode: u32,
        uid: u32,
        gid: u32,
        links: u64,
        mtime: i64,
        mtime_nsec: i64,
        ctime: i64,
        ctime_nsec: i64,
    }

    impl Snapshot {
        fn of(metadata: &Metadata) -> Self {
            Self {
                dev: metadata.dev(),
                ino: metadata.ino(),
                size: metadata.len(),
                mode: metadata.mode(),
                uid: metadata.uid(),
                gid: metadata.gid(),
                links: metadata.nlink(),
                mtime: metadata.mtime(),
                mtime_nsec: metadata.mtime_nsec(),
                ctime: metadata.ctime(),
                ctime_nsec: metadata.ctime_nsec(),
            }
        }
    }

    struct Source {
        path: PathBuf,
        parent_path: PathBuf,
        parent: File,
        name: CString,
        file: File,
        snapshot: Snapshot,
    }

    pub(super) struct Stage {
        source: Source,
        path: PathBuf,
        name: CString,
        file: File,
        identity: (u64, u64),
        source_permissions_applied: bool,
        promoted: bool,
    }

    fn open_at(parent: &File, name: &CString, flags: i32, mode: libc::mode_t) -> Result<File> {
        let fd = unsafe {
            libc::openat(
                parent.as_raw_fd(),
                name.as_ptr(),
                flags | libc::O_NOFOLLOW | libc::O_CLOEXEC,
                mode,
            )
        };
        if fd < 0 {
            return Err(std::io::Error::last_os_error().into());
        }
        Ok(unsafe { File::from_raw_fd(fd) })
    }

    fn trusted_directory(path: &Path) -> Result<File> {
        let mut directory = std::fs::OpenOptions::new()
            .read(true)
            .custom_flags(libc::O_DIRECTORY | libc::O_NOFOLLOW)
            .open("/")?;
        let uid = unsafe { libc::geteuid() };
        let check = |metadata: &Metadata| -> Result<()> {
            if !metadata.is_dir()
                || (metadata.uid() != 0 && metadata.uid() != uid)
                || metadata.mode() & 0o022 != 0
            {
                bail!("retag media directory has an untrusted or writable ancestor");
            }
            Ok(())
        };
        check(&directory.metadata()?)?;
        for component in path.components() {
            match component {
                Component::RootDir => {}
                Component::Normal(name) => {
                    let name = CString::new(name.as_bytes())?;
                    directory = open_at(
                        &directory,
                        &name,
                        libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NONBLOCK,
                        0,
                    )
                    .context("opening trusted retag directory")?;
                    check(&directory.metadata()?)?;
                }
                _ => bail!("unsafe retag directory component"),
            }
        }
        Ok(directory)
    }

    fn open_source(path: &Path) -> Result<Source> {
        if path.as_os_str().is_empty()
            || path
                .components()
                .any(|component| component == Component::ParentDir)
        {
            bail!("unsafe retag source path");
        }
        let absolute = if path.is_absolute() {
            path.to_path_buf()
        } else {
            std::env::current_dir()?.join(path)
        };
        let mut normalized = PathBuf::new();
        for component in absolute.components() {
            normalized.push(component.as_os_str());
        }
        if normalized.to_str().is_none() {
            bail!("retag source path must be valid UTF-8");
        }
        let parent_path = normalized
            .parent()
            .context("retag source has no parent")?
            .to_path_buf();
        let parent = trusted_directory(&parent_path)?;
        let name = CString::new(
            normalized
                .file_name()
                .context("retag source has no filename")?
                .as_bytes(),
        )?;
        let file = open_at(&parent, &name, libc::O_RDONLY | libc::O_NONBLOCK, 0)
            .context("opening retag source without following symlinks")?;
        let metadata = file.metadata()?;
        if !metadata.is_file()
            || metadata.uid() != unsafe { libc::geteuid() }
            || metadata.nlink() != 1
            || metadata.mode() & 0o022 != 0
        {
            bail!("retag source must be an owned, unaliased regular file not writable by others");
        }
        let snapshot = Snapshot::of(&metadata);
        Ok(Source {
            path: normalized,
            parent_path,
            parent,
            name,
            file,
            snapshot,
        })
    }

    impl Source {
        fn verify(&self) -> Result<()> {
            let parent = trusted_directory(&self.parent_path)?;
            let current_parent = parent.metadata()?;
            let original_parent = self.parent.metadata()?;
            if (current_parent.dev(), current_parent.ino())
                != (original_parent.dev(), original_parent.ino())
            {
                bail!("retag media directory changed");
            }
            let current = open_at(
                &self.parent,
                &self.name,
                libc::O_RDONLY | libc::O_NONBLOCK,
                0,
            )?;
            if Snapshot::of(&self.file.metadata()?) != self.snapshot
                || Snapshot::of(&current.metadata()?) != self.snapshot
            {
                bail!("retag source changed; original not replaced");
            }
            Ok(())
        }
    }

    pub(super) fn check_path(path: &Path) -> Result<()> {
        open_source(path)?.verify()
    }

    impl Stage {
        pub(super) fn reserve(path: &Path) -> Result<Self> {
            let source = open_source(path)?;
            source.verify()?;
            let stage_path = retag_temp_path(&source.path);
            let name = CString::new(
                stage_path
                    .file_name()
                    .context("retag staging filename missing")?
                    .as_bytes(),
            )?;
            let file = open_at(
                &source.parent,
                &name,
                libc::O_RDWR | libc::O_CREAT | libc::O_EXCL,
                0o600,
            )
            .context("reserving retag staging file without overwrite")?;
            let metadata = file.metadata()?;
            let stage = Self {
                source,
                path: stage_path,
                name,
                file,
                identity: (metadata.dev(), metadata.ino()),
                source_permissions_applied: false,
                promoted: false,
            };
            if unsafe { libc::fchmod(stage.file.as_raw_fd(), 0o600) } != 0 {
                return Err(std::io::Error::last_os_error())
                    .context("setting private retag staging permissions");
            }
            stage.verify()?;
            Ok(stage)
        }

        pub(super) fn source_path(&self) -> &Path {
            &self.source.path
        }
        pub(super) fn stage_path(&self) -> &Path {
            &self.path
        }

        pub(super) fn output_file(&self) -> Result<File> {
            self.file
                .try_clone()
                .context("duplicating retag staging descriptor")
        }

        pub(super) fn verify(&self) -> Result<()> {
            self.source.verify()?;
            let current = open_at(
                &self.source.parent,
                &self.name,
                libc::O_RDONLY | libc::O_NONBLOCK,
                0,
            )
            .context("examining retag staging file")?;
            let held = self.file.metadata()?;
            let path = current.metadata()?;
            if !held.is_file()
                || !path.is_file()
                || held.uid() != unsafe { libc::geteuid() }
                || held.nlink() != 1
                || path.nlink() != 1
                || (held.dev(), held.ino()) != self.identity
                || (path.dev(), path.ino()) != self.identity
                || held.mode() & 0o022 != 0
                || (!self.source_permissions_applied && held.mode() & 0o077 != 0)
            {
                bail!("retag staging file changed or became unsafe");
            }
            Ok(())
        }

        pub(super) fn apply_source_permissions(&mut self) -> Result<()> {
            self.verify()?;
            let metadata = self.file.metadata()?;
            if (metadata.uid() != self.source.snapshot.uid
                || metadata.gid() != self.source.snapshot.gid)
                && unsafe {
                    libc::fchown(
                        self.file.as_raw_fd(),
                        self.source.snapshot.uid,
                        self.source.snapshot.gid,
                    )
                } != 0
            {
                return Err(std::io::Error::last_os_error()).context("preserving retag ownership");
            }
            if unsafe {
                libc::fchmod(
                    self.file.as_raw_fd(),
                    (self.source.snapshot.mode & 0o7777) as libc::mode_t,
                )
            } != 0
            {
                return Err(std::io::Error::last_os_error()).context("preserving retag mode");
            }
            let metadata = self.file.metadata()?;
            if metadata.uid() != self.source.snapshot.uid
                || metadata.gid() != self.source.snapshot.gid
                || metadata.mode() & 0o7777 != self.source.snapshot.mode & 0o7777
            {
                bail!("failed to preserve source ownership or mode");
            }
            self.source_permissions_applied = true;
            self.file.sync_all().context("syncing retag staging file")?;
            self.verify()
        }

        pub(super) fn promote(&mut self) -> Result<()> {
            self.verify()?;
            // Only the verified original source is the intended destination.
            let rc = unsafe {
                libc::renameat(
                    self.source.parent.as_raw_fd(),
                    self.name.as_ptr(),
                    self.source.parent.as_raw_fd(),
                    self.source.name.as_ptr(),
                )
            };
            if rc != 0 {
                return Err(std::io::Error::last_os_error())
                    .context("replacing verified retag source");
            }
            self.promoted = true;
            Ok(())
        }
    }

    impl Drop for Stage {
        fn drop(&mut self) {
            if self.promoted {
                return;
            }
            if let Ok(current) = open_at(
                &self.source.parent,
                &self.name,
                libc::O_RDONLY | libc::O_NONBLOCK,
                0,
            ) {
                if current.metadata().is_ok_and(|metadata| {
                    metadata.is_file() && (metadata.dev(), metadata.ino()) == self.identity
                }) {
                    unsafe {
                        libc::unlinkat(self.source.parent.as_raw_fd(), self.name.as_ptr(), 0);
                    }
                }
            }
        }
    }
}

#[cfg(unix)]
fn retag_temp_path(path: &Path) -> PathBuf {
    crate::staging::path_for(path, "retag")
}

fn check_sets(
    present_audio: Vec<String>,
    present_subtitles: Vec<String>,
    requirements: &LanguageRequirements,
) -> LanguageCheckReport {
    let audio_set = present_audio
        .iter()
        .filter_map(|language| normalize_language_tag(language))
        .collect::<BTreeSet<_>>();
    let subtitle_set = present_subtitles
        .iter()
        .filter_map(|language| normalize_language_tag(language))
        .collect::<BTreeSet<_>>();
    let missing_audio = requirements
        .audio
        .iter()
        .filter(|lang| {
            normalize_language_tag(lang).is_none_or(|language| !audio_set.contains(&language))
        })
        .cloned()
        .collect();
    let missing_subtitles = requirements
        .subtitles
        .iter()
        .filter(|lang| {
            normalize_language_tag(lang).is_none_or(|language| !subtitle_set.contains(&language))
        })
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

pub(super) fn canonical_arr_language(name: &str) -> Option<&'static str> {
    Some(match name.trim().to_ascii_lowercase().as_str() {
        "english" => "eng",
        "french" => "fra",
        "spanish" | "spanish (latino)" => "spa",
        "german" => "deu",
        "italian" => "ita",
        "danish" => "dan",
        "dutch" | "flemish" => "nld",
        "japanese" => "jpn",
        "icelandic" => "isl",
        "chinese" => "zho",
        "russian" => "rus",
        "polish" => "pol",
        "vietnamese" => "vie",
        "swedish" => "swe",
        "norwegian" => "nor",
        "finnish" => "fin",
        "turkish" => "tur",
        "portuguese" | "portuguese (brazil)" => "por",
        "greek" => "ell",
        "korean" => "kor",
        "hungarian" => "hun",
        "hebrew" => "heb",
        "lithuanian" => "lit",
        "czech" => "ces",
        "arabic" => "ara",
        "hindi" => "hin",
        "romanian" => "ron",
        "thai" => "tha",
        "bulgarian" => "bul",
        "ukrainian" => "ukr",
        "persian" => "fas",
        "bengali" => "ben",
        "slovak" => "slk",
        "latvian" => "lav",
        "catalan" => "cat",
        "croatian" => "hrv",
        "serbian" => "srp",
        "bosnian" => "bos",
        "estonian" => "est",
        "tamil" => "tam",
        "indonesian" => "ind",
        "telugu" => "tel",
        "macedonian" => "mkd",
        "slovenian" => "slv",
        "malayalam" => "mal",
        "kannada" => "kan",
        "albanian" => "sqi",
        "afrikaans" => "afr",
        "marathi" => "mar",
        "tagalog" => "tgl",
        "urdu" => "urd",
        "romansh" => "roh",
        "mongolian" => "mon",
        "georgian" => "kat",
        "irish" => "gle",
        "azerbaijani" => "aze",
        "uzbek" => "uzb",
        "malay" => "msa",
        _ => return None,
    })
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

    if let Some(tag) = canonical_arr_language(&primary) {
        return Some(tag.to_string());
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
        "nl" | "dut" => "nld",
        "pl" | "polish" => "pol",
        "sv" | "swedish" => "swe",
        "no" | "nb" | "nn" | "nob" | "nno" => "nor",
        "da" | "danish" => "dan",
        "fi" | "finnish" => "fin",
        "cs" => "ces",
        "cz" | "cze" => "ces",
        "el" | "gre" => "ell",
        "he" => "heb",
        "hi" => "hin",
        "ar" => "ara",
        "tr" => "tur",
        "th" => "tha",
        "vi" => "vie",
        "is" | "ice" => "isl",
        "ro" | "rum" => "ron",
        "sk" | "slo" => "slk",
        "mk" | "mac" => "mkd",
        "ka" | "geo" => "kat",
        "sq" | "alb" => "sqi",
        "ms" | "may" => "msa",
        "az" => "aze",
        "uz" => "uzb",
        "ur" => "urd",
        "ga" => "gle",
        "bn" => "ben",
        "ta" => "tam",
        "te" => "tel",
        "kn" => "kan",
        "ml" => "mal",
        "bg" => "bul",
        "hu" => "hun",
        "lt" => "lit",
        "lv" => "lav",
        "et" => "est",
        "ca" => "cat",
        "hr" => "hrv",
        "sr" => "srp",
        "bs" => "bos",
        "sl" => "slv",
        "af" => "afr",
        "mr" => "mar",
        "tl" => "tgl",
        "mn" => "mon",
        "rm" => "roh",
        "id" | "indonesian" => "ind",
        "fa" | "fas" | "per" | "persian" => "fas",
        "uk" => "ukr",
        _ => primary.as_str(),
    };

    Some(normalized.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    const SHORT_SRT: &str = "1\n00:00:01,000 --> 00:00:02,000\nHello.\n";
    const DURATION_MS: u64 = 30_000;

    fn fixture(starts: &[u32], text: &[&str]) -> String {
        starts
            .iter()
            .enumerate()
            .map(|(index, start)| {
                format!(
                    "{}\n00:00:{:02},000 --> 00:00:{:02},000\n{}\n\n",
                    index + 1,
                    start,
                    start + 2,
                    text[index % text.len()]
                )
            })
            .collect()
    }

    fn dialogue(starts: &[u32]) -> String {
        fixture(
            starts,
            &[
                "Are you coming with us?",
                "I will meet you outside.",
                "We should leave before dark.",
                "The train is already waiting.",
                "Please bring your coat along.",
                "That sounds like a good idea.",
                "There is a better way home.",
                "Thank you for helping me today.",
            ],
        )
    }

    fn complete_fixture() -> String {
        dialogue(&[1, 4, 8, 11, 16, 19, 23, 27])
    }

    #[test]
    fn subtitle_titles_distinguish_forced_and_signs_only() {
        for title in [
            "English Forced",
            "English - Signs & Songs",
            "SONGS ONLY",
            "Sign-only",
            "Full signs only",
            "Full Signs & Songs",
            "Signs and songs without dialogue",
        ] {
            assert!(partial_subtitle_label(title), "{title}");
        }
        for title in [
            "English",
            "English SDH",
            "Unforced English",
            "Full dialogue including signs and songs",
            "English sign language",
        ] {
            assert!(!partial_subtitle_label(title), "{title}");
        }
    }

    #[test]
    fn sidecars_require_exact_stem_and_known_language() {
        let dir = tempfile::tempdir().unwrap();
        let media = dir.path().join("Show.S01E01.mkv");
        for name in [
            "Show.S01E02.en.srt",
            "Show.S01E01.en.S01E02.srt",
            "Show.S01E01.en-S01E02.srt",
            "Show.S01E01.srt",
            "Show.S01E01.und.srt",
            "Show.S01E01.xyz.srt",
            "Show.S01E01.en.forced.srt",
            "Show.S01E01.en.signs.srt",
            "Show.S01E01.en-Songs.srt",
            "Show.S01E01.eng.extra.srt",
        ] {
            std::fs::write(dir.path().join(name), complete_fixture()).unwrap();
        }
        assert!(sidecar_languages(&media, Some(DURATION_MS)).is_empty());
        std::fs::write(
            dir.path().join("Show.S01E01.en-US.sdh.srt"),
            complete_fixture(),
        )
        .unwrap();
        std::fs::write(dir.path().join("Show.S01E01.es.srt"), complete_fixture()).unwrap();
        assert_eq!(
            sidecar_languages(&media, Some(DURATION_MS)),
            BTreeSet::from(["eng".into(), "spa".into()])
        );
        assert!(sidecar_languages(&media, None).is_empty());
        assert_eq!(sidecar_language("fr-FR"), Some("fra".into()));
        assert_eq!(sidecar_language("und"), None);
        assert_eq!(sidecar_language("unknown"), None);
        assert_eq!(sidecar_language("hi"), None);
        assert_eq!(sidecar_language("hin"), Some("hin".into()));
        #[cfg(unix)]
        {
            std::fs::remove_file(dir.path().join("Show.S01E01.en-US.sdh.srt")).unwrap();
            std::fs::remove_file(dir.path().join("Show.S01E01.es.srt")).unwrap();
            std::fs::write(dir.path().join("show.S01E01.en.srt"), complete_fixture()).unwrap();
            assert!(sidecar_languages(&media, Some(DURATION_MS)).is_empty());
        }
    }

    #[test]
    fn sidecars_need_valid_distributed_substantive_cues() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("test.en.srt");
        for content in [
            "".to_string(),
            "not a subtitle".to_string(),
            SHORT_SRT.into(),
            dialogue(&[1, 27]),
            dialogue(&[1, 2, 3, 4, 5, 6, 7, 8]),
            dialogue(&[1, 2, 8, 9, 20, 21, 27, 28]),
            fixture(&[1, 4, 8, 11, 16, 19, 23, 27], &["SIGN"]),
            fixture(
                &[1, 4, 8, 11, 16, 19, 23, 27],
                &["Door number 1", "Door number 2"],
            ),
            complete_fixture().replace("00:00:03,000", "00:99:03,000"),
            complete_fixture().replace("00:00:03,000", "00:00:00,000"),
            complete_fixture().replace("00:00:29,000", "00:05:29,000"),
            complete_fixture().replace("00:00:03,000", "00:00:03,000 junk"),
            format!("{}\ngarbage", complete_fixture()),
            format!("{}9\n00:00:29,000 --> 00:00:30,000\n", complete_fixture()),
            format!(
                "{}9\n00:00:29,000 --> 00:00:30,000\n  \n",
                complete_fixture()
            ),
        ] {
            std::fs::write(&path, &content).unwrap();
            assert!(
                !valid_text_sidecar(&path, "srt", DURATION_MS, "eng"),
                "accepted {content:?}"
            );
        }
        std::fs::write(&path, complete_fixture()).unwrap();
        assert!(valid_text_sidecar(&path, "srt", DURATION_MS, "eng"));
        assert!(!valid_text_sidecar(&path, "srt", 120_000, "eng"));
        assert!(!valid_text_sidecar(&path, "srt", 0, "eng"));
        let vtt = format!(
            "\u{feff}WEBVTT\r\n\r\n{}",
            complete_fixture().replace(',', ".").replace('\n', "\r\n")
        );
        std::fs::write(&path, vtt).unwrap();
        assert!(valid_text_sidecar(&path, "vtt", DURATION_MS, "eng"));
        assert!(cue_timing("00:01.000 --> 00:02.000 align:start", true).is_some());
        assert!(cue_timing("00:01.000 --> 00:02.000 --> 00:03.000", true).is_none());
        std::fs::write(&path, [0xff, 0xfe]).unwrap();
        assert!(!valid_text_sidecar(&path, "srt", DURATION_MS, "eng"));
    }

    #[test]
    fn unspaced_japanese_and_chinese_sidecars_pass_only_the_coverage_gate() {
        let dir = tempfile::tempdir().unwrap();
        let media = dir.path().join("episode.mkv");
        let starts = [1, 4, 8, 11, 16, 19, 23, 27];
        let japanese = [
            "今日は天気がいいですね",
            "明日は一緒に出かけましょう",
            "駅の前で待っていますね",
            "この道をまっすぐ進んでください",
            "大切な話がありますから",
            "みんなで帰りましょうね",
            "お手伝いしてくれてありがとう",
            "また明日ここで会いましょう",
        ];
        let chinese = [
            "今天的天气真的很好",
            "明天我们一起出去吧",
            "我会在车站前面等你",
            "请沿着这条路一直走",
            "我有一件重要的事情",
            "我们大家一起回家吧",
            "谢谢你今天帮助了我",
            "明天在这里再见面吧",
        ];
        std::fs::write(
            dir.path().join("episode.ja.srt"),
            fixture(&starts, &japanese),
        )
        .unwrap();
        std::fs::write(
            dir.path().join("episode.zh.srt"),
            fixture(&starts, &chinese),
        )
        .unwrap();
        assert_eq!(
            sidecar_languages(&media, Some(DURATION_MS)),
            BTreeSet::from(["jpn".into(), "zho".into()])
        );
        let short_japanese = [
            "そうですね",
            "分かりました",
            "一緒に行こう",
            "ありがとうね",
            "また明日ね",
            "大丈夫ですよ",
            "気をつけてね",
            "少し待ってね",
        ];
        std::fs::write(
            dir.path().join("episode.ja.srt"),
            fixture(&starts, &short_japanese),
        )
        .unwrap();
        assert_eq!(
            sidecar_languages(&media, Some(DURATION_MS)),
            BTreeSet::from(["jpn".into(), "zho".into()])
        );
        std::fs::write(
            dir.path().join("episode.ja.srt"),
            fixture(&[1, 27], &japanese),
        )
        .unwrap();
        assert_eq!(
            sidecar_languages(&media, Some(DURATION_MS)),
            BTreeSet::from(["zho".into()])
        );
        std::fs::write(
            dir.path().join("episode.ja.srt"),
            fixture(&starts, &["日本語の看板です"]),
        )
        .unwrap();
        assert_eq!(
            sidecar_languages(&media, Some(DURATION_MS)),
            BTreeSet::from(["zho".into()])
        );
    }

    #[cfg(unix)]
    #[test]
    fn symlink_sidecars_are_not_evidence() {
        let dir = tempfile::tempdir().unwrap();
        let target = dir.path().join("target.srt");
        let link = dir.path().join("video.en.srt");
        std::fs::write(&target, complete_fixture()).unwrap();
        std::os::unix::fs::symlink(&target, &link).unwrap();
        assert!(!valid_text_sidecar(&link, "srt", DURATION_MS, "eng"));
        assert!(sidecar_languages(&dir.path().join("video.mkv"), Some(DURATION_MS)).is_empty());
    }

    #[cfg(feature = "ffmpeg-cli-tests")]
    #[test]
    fn file_checks_keep_sidecars_out_of_conversion() {
        if !ffmpeg_present() {
            return;
        }
        let dir = tempfile::tempdir().unwrap();
        let source = dir.path().join("source.srt");
        std::fs::write(&source, SHORT_SRT).unwrap();
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec![],
            subtitles: vec!["eng".into()],
        };
        for (index, disposition, title, language, expected) in [
            (0, "forced", "English", "eng", false),
            (1, "0", "English Signs & Songs", "eng", false),
            (2, "0", "English", "eng", true),
            (3, "0", "English Forced", "eng", false),
            (4, "0", "English", "und", false),
            (
                5,
                "0",
                "Full dialogue including signs and songs",
                "eng",
                true,
            ),
        ] {
            let media = dir.path().join(format!("video{index}.mkv"));
            let status = std::process::Command::new("ffmpeg")
                .args([
                    "-hide_banner",
                    "-loglevel",
                    "error",
                    "-y",
                    "-f",
                    "lavfi",
                    "-i",
                    "sine=frequency=1000:duration=30",
                    "-i",
                ])
                .arg(&source)
                .args([
                    "-c:a",
                    "aac",
                    "-c:s",
                    "srt",
                    "-metadata:s:s:0",
                    &format!("language={language}"),
                    "-metadata:s:s:0",
                    &format!("title={title}"),
                    "-disposition:s:0",
                    disposition,
                ])
                .arg(&media)
                .status()
                .unwrap();
            assert!(status.success());
            let original = std::fs::read(&media).unwrap();
            let embedded = check_file(&media, &requirements).unwrap();
            assert_eq!(embedded.satisfied(), expected, "case {index}");
            assert_eq!(
                check_file_with_sidecars(&media, &requirements).unwrap(),
                embedded
            );
            if index == 0 {
                let sidecar = dir.path().join("video0.eng.srt");
                std::fs::write(&sidecar, SHORT_SRT).unwrap();
                assert!(!check_file_with_sidecars(&media, &requirements)
                    .unwrap()
                    .satisfied());
                std::fs::write(&sidecar, complete_fixture()).unwrap();
                let audit = check_file_with_sidecars(&media, &requirements).unwrap();
                assert!(audit.satisfied());
                assert_eq!(audit.present_subtitles, vec!["eng"]);
                assert_eq!(
                    check_file(&media, &requirements).unwrap().missing_subtitles,
                    vec!["eng"]
                );
                std::fs::write(dir.path().join("video0.es.srt"), complete_fixture()).unwrap();
                let both = LanguageRequirements {
                    subtitles: vec!["eng".into(), "spa".into()],
                    ..requirements.clone()
                };
                assert!(check_file_with_sidecars(&media, &both).unwrap().satisfied());
                assert_eq!(
                    check_file(&media, &both).unwrap().missing_subtitles,
                    vec!["eng", "spa"]
                );
                let no_subtitles = LanguageRequirements {
                    subtitles: vec![],
                    ..requirements.clone()
                };
                assert_eq!(
                    check_file(&media, &no_subtitles).unwrap(),
                    check_file_with_sidecars(&media, &no_subtitles).unwrap()
                );
            }
            assert_eq!(std::fs::read(&media).unwrap(), original);
        }
    }

    #[cfg(all(feature = "ffmpeg-cli-tests", unix))]
    fn secure_retag_tempdir() -> tempfile::TempDir {
        tempfile::tempdir_in(
            std::env::var_os("HOME").expect("HOME required for trusted retag tests"),
        )
        .unwrap()
    }

    #[cfg(all(feature = "ffmpeg-cli-tests", unix))]
    fn make_retag_subtitle_fixture(path: &Path, include_dialogue: bool, forced_language: &str) {
        let srt = path.parent().unwrap().join("fixture.srt");
        std::fs::write(&srt, SHORT_SRT).unwrap();
        let mut command = std::process::Command::new("ffmpeg");
        command.args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=1000:duration=1",
        ]);
        for _ in 0..if include_dialogue { 3 } else { 2 } {
            command.arg("-i").arg(&srt);
        }
        command.args(["-map", "0:a", "-map", "1:0", "-map", "2:0"]);
        if include_dialogue {
            command.args(["-map", "3:0"]);
        }
        command.args([
            "-c:a",
            "aac",
            "-fflags",
            "+bitexact",
            "-c:s",
            "srt",
            "-metadata:s:a:0",
            "language=und",
            "-disposition:a:0",
            "default",
            "-metadata:s:s:0",
            &format!("language={forced_language}"),
            "-metadata:s:s:0",
            "title=English",
            "-disposition:s:0",
            "forced+default",
            "-metadata:s:s:1",
            "language=und",
            "-metadata:s:s:1",
            "title=Signs & Songs",
            "-disposition:s:1",
            "0",
        ]);
        if include_dialogue {
            command.args([
                "-metadata:s:s:2",
                "language=und",
                "-metadata:s:s:2",
                "title=Full Dialogue",
                "-disposition:s:2",
                "hearing_impaired",
            ]);
        }
        let status = command
            .arg(path)
            .status()
            .expect("run ffmpeg subtitle fixture");
        assert!(status.success());
    }

    #[cfg(all(feature = "ffmpeg-cli-tests", unix))]
    fn stream_languages_and_dispositions(path: &Path) -> Vec<(i32, i32, Option<String>)> {
        let path = path_to_cstr(path).unwrap();
        let ctx = AVFormatContextInput::open(path.as_c_str()).unwrap();
        ctx.streams()
            .iter()
            .map(|stream| {
                let lang = stream
                    .metadata()
                    .as_deref()
                    .and_then(extract_language_tag_from_metadata)
                    .and_then(|tag| normalize_language_tag(&tag));
                (stream.codecpar().codec_type, stream.disposition, lang)
            })
            .collect()
    }

    #[cfg(all(feature = "ffmpeg-cli-tests", unix))]
    #[test]
    fn retag_rejects_unsafe_directories_and_writable_sources() {
        use std::os::unix::fs::{symlink, PermissionsExt};
        if !ffmpeg_present() {
            return;
        }
        let tmp = secure_retag_tempdir();
        let directory = tmp.path().join("shared");
        std::fs::create_dir(&directory).unwrap();
        let media = directory.join("audio.mp4");
        make_audio_fixture(&media, "und");
        let original = std::fs::read(&media).unwrap();
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".into()],
            subtitles: vec![],
        };
        let options = UntaggedRetagOptions {
            audio_language: Some("eng".into()),
            ..Default::default()
        };

        std::fs::set_permissions(&directory, std::fs::Permissions::from_mode(0o777)).unwrap();
        assert!(retag_unknown_streams(&media, &requirements, &options).is_err());
        assert!(!retag_temp_path(&media).exists());
        std::fs::set_permissions(&directory, std::fs::Permissions::from_mode(0o700)).unwrap();
        let child = directory.join("private");
        std::fs::create_dir(&child).unwrap();
        std::fs::set_permissions(&child, std::fs::Permissions::from_mode(0o700)).unwrap();
        let nested_media = child.join("nested.mp4");
        make_audio_fixture(&nested_media, "und");
        std::fs::set_permissions(&directory, std::fs::Permissions::from_mode(0o775)).unwrap();
        assert!(retag_unknown_streams(&nested_media, &requirements, &options).is_err());
        assert!(!retag_temp_path(&nested_media).exists());
        std::fs::set_permissions(&directory, std::fs::Permissions::from_mode(0o700)).unwrap();
        std::fs::set_permissions(&media, std::fs::Permissions::from_mode(0o664)).unwrap();
        assert!(retag_unknown_streams(&media, &requirements, &options).is_err());
        assert!(!retag_temp_path(&media).exists());
        std::fs::set_permissions(&media, std::fs::Permissions::from_mode(0o600)).unwrap();
        let link = directory.join("link.mp4");
        symlink(&media, &link).unwrap();
        assert!(retag_unknown_streams(&link, &requirements, &options).is_err());
        assert_eq!(std::fs::read(&media).unwrap(), original);
    }

    #[cfg(all(feature = "ffmpeg-cli-tests", unix))]
    #[test]
    fn retag_preserves_source_mode_and_cleans_its_failed_stage() {
        use std::os::unix::fs::{MetadataExt, PermissionsExt};
        if !ffmpeg_present() {
            return;
        }
        let tmp = secure_retag_tempdir();
        let media = tmp.path().join("audio.mp4");
        make_audio_fixture(&media, "und");
        std::fs::set_permissions(&media, std::fs::Permissions::from_mode(0o640)).unwrap();
        let before = std::fs::metadata(&media).unwrap();
        let original = std::fs::read(&media).unwrap();
        let options = UntaggedRetagOptions {
            audio_language: Some("eng".into()),
            ..Default::default()
        };
        let unsafe_mode =
            remux_with_retagged_unknown_streams_hook(&media, &options, true, false, |staged| {
                std::fs::set_permissions(staged, std::fs::Permissions::from_mode(0o644))?;
                Ok(())
            });
        assert!(unsafe_mode.is_err());
        assert!(!retag_temp_path(&media).exists());
        assert_eq!(std::fs::read(&media).unwrap(), original);
        let failure =
            remux_with_retagged_unknown_streams_hook(&media, &options, true, false, |staged| {
                assert_eq!(std::fs::metadata(staged)?.mode() & 0o7777, 0o600);
                bail!("simulated retag failure")
            });
        assert!(failure.is_err());
        assert!(!retag_temp_path(&media).exists());
        assert_eq!(std::fs::read(&media).unwrap(), original);
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".into()],
            subtitles: vec![],
        };
        assert_eq!(
            retag_unknown_streams(&media, &requirements, &options)
                .unwrap()
                .audio_streams,
            1
        );
        let after = std::fs::metadata(&media).unwrap();
        assert_eq!(after.mode() & 0o7777, before.mode() & 0o7777);
        assert_eq!((after.uid(), after.gid()), (before.uid(), before.gid()));
        assert!(!retag_temp_path(&media).exists());
    }

    #[cfg(all(feature = "ffmpeg-cli-tests", unix))]
    #[test]
    fn descriptor_bound_output_does_not_follow_a_swapped_path() {
        use std::os::unix::fs::symlink;
        if !ffmpeg_present() {
            return;
        }
        let tmp = secure_retag_tempdir();
        let media = tmp.path().join("audio.mp4");
        let victim = tmp.path().join("victim");
        let moved = tmp.path().join("moved-stage");
        make_audio_fixture(&media, "und");
        std::fs::write(&victim, b"keep victim").unwrap();
        let stage = secure_retag::Stage::reserve(&media).unwrap();
        let mut io = retag_output_io(stage.output_file().unwrap());
        std::fs::rename(stage.stage_path(), &moved).unwrap();
        symlink(&victim, stage.stage_path()).unwrap();
        let data = b"descriptor-bound";
        unsafe {
            ffi::avio_write(io.as_mut_ptr(), data.as_ptr(), data.len() as i32);
            ffi::avio_flush(io.as_mut_ptr());
        }
        drop(io);
        assert_eq!(std::fs::read(&victim).unwrap(), b"keep victim");
        assert_eq!(std::fs::read(&moved).unwrap(), data);
        assert!(stage.verify().is_err());
    }

    #[cfg(all(feature = "ffmpeg-cli-tests", unix))]
    #[test]
    fn retag_rejects_staging_symlink_swap_without_touching_other_files() {
        use std::os::unix::fs::symlink;
        if !ffmpeg_present() {
            return;
        }
        let tmp = secure_retag_tempdir();
        let media = tmp.path().join("audio.mp4");
        let victim = tmp.path().join("victim");
        let moved = tmp.path().join("moved-stage");
        make_audio_fixture(&media, "und");
        std::fs::write(&victim, b"keep victim").unwrap();
        let original = std::fs::read(&media).unwrap();
        let options = UntaggedRetagOptions {
            audio_language: Some("eng".into()),
            ..Default::default()
        };
        let result =
            remux_with_retagged_unknown_streams_hook(&media, &options, true, false, |staged| {
                std::fs::rename(staged, &moved)?;
                symlink(&victim, staged)?;
                Ok(())
            });
        assert!(result.is_err());
        assert_eq!(std::fs::read(&victim).unwrap(), b"keep victim");
        assert_eq!(std::fs::read(&media).unwrap(), original);
        assert!(std::fs::symlink_metadata(retag_temp_path(&media))
            .unwrap()
            .file_type()
            .is_symlink());
        assert!(moved.is_file());
    }

    #[cfg(all(feature = "ffmpeg-cli-tests", unix))]
    #[test]
    fn retag_leaves_replaced_staging_file_untouched() {
        if !ffmpeg_present() {
            return;
        }
        let tmp = secure_retag_tempdir();
        let media = tmp.path().join("audio.mp4");
        let moved = tmp.path().join("moved-stage");
        make_audio_fixture(&media, "und");
        let original = std::fs::read(&media).unwrap();
        let options = UntaggedRetagOptions {
            audio_language: Some("eng".into()),
            ..Default::default()
        };
        let result =
            remux_with_retagged_unknown_streams_hook(&media, &options, true, false, |staged| {
                std::fs::rename(staged, &moved)?;
                std::fs::write(staged, b"foreign staging file")?;
                Ok(())
            });
        assert!(result.is_err());
        assert_eq!(
            std::fs::read(retag_temp_path(&media)).unwrap(),
            b"foreign staging file"
        );
        assert_eq!(std::fs::read(&media).unwrap(), original);
        assert!(moved.is_file());
    }

    #[cfg(all(feature = "ffmpeg-cli-tests", unix))]
    #[test]
    fn retag_rejects_source_swap_and_preserves_unexpected_destination() {
        if !ffmpeg_present() {
            return;
        }
        let tmp = secure_retag_tempdir();
        let media = tmp.path().join("audio.mp4");
        let moved = tmp.path().join("original.mp4");
        make_audio_fixture(&media, "und");
        let original = std::fs::read(&media).unwrap();
        let options = UntaggedRetagOptions {
            audio_language: Some("eng".into()),
            ..Default::default()
        };
        let result =
            remux_with_retagged_unknown_streams_hook(&media, &options, true, false, |_| {
                std::fs::rename(&media, &moved)?;
                std::fs::write(&media, b"replacement file")?;
                Ok(())
            });
        assert!(result.is_err());
        assert_eq!(std::fs::read(&media).unwrap(), b"replacement file");
        assert_eq!(std::fs::read(&moved).unwrap(), original);
        assert!(!retag_temp_path(&media).exists());
    }

    #[cfg(all(feature = "ffmpeg-cli-tests", unix))]
    #[test]
    fn subtitle_retag_preserves_dispositions_and_skips_partial_unknown_tracks() {
        if !ffmpeg_present() {
            return;
        }
        let tmp = secure_retag_tempdir();
        let media = tmp.path().join("subtitle.mkv");
        make_retag_subtitle_fixture(&media, true, "und");
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec![],
            subtitles: vec!["eng".into()],
        };
        let before = stream_languages_and_dispositions(&media);
        assert_eq!(before.len(), 4);
        assert_ne!(before[1].1 & ffi::AV_DISPOSITION_FORCED as i32, 0);
        assert_ne!(before[3].1 & ffi::AV_DISPOSITION_HEARING_IMPAIRED as i32, 0);
        assert!(!check_file(&media, &requirements).unwrap().satisfied());
        let options = UntaggedRetagOptions {
            audio_language: None,
            subtitle_language: Some("eng".into()),
            dry_run: false,
        };
        let before_bytes = std::fs::read(&media).unwrap();
        let dry_run = retag_unknown_streams(
            &media,
            &requirements,
            &UntaggedRetagOptions {
                dry_run: true,
                ..options.clone()
            },
        )
        .unwrap();
        assert_eq!(dry_run.subtitle_streams, 1);
        assert_eq!(std::fs::read(&media).unwrap(), before_bytes);
        let report = retag_unknown_streams(&media, &requirements, &options).unwrap();
        assert_eq!(report.subtitle_streams, 1);
        let after = stream_languages_and_dispositions(&media);
        assert_eq!(
            before.iter().map(|s| s.1).collect::<Vec<_>>(),
            after.iter().map(|s| s.1).collect::<Vec<_>>()
        );
        assert_eq!(after[1].2, None);
        assert_eq!(after[2].2, None);
        let ctx_path = path_to_cstr(&media).unwrap();
        let ctx = AVFormatContextInput::open(ctx_path.as_c_str()).unwrap();
        assert!(subtitle_track_is_partial(
            ctx.streams()[2].disposition,
            ctx.streams()[2].metadata().as_deref()
        ));
        assert_eq!(after[3].2.as_deref(), Some("eng"));
        assert!(check_file(&media, &requirements).unwrap().satisfied());

        let partial_only = tmp.path().join("partial.mkv");
        make_retag_subtitle_fixture(&partial_only, false, "und");
        let original = std::fs::read(&partial_only).unwrap();
        let report = retag_unknown_streams(&partial_only, &requirements, &options).unwrap();
        assert!(!report.changed());
        assert_eq!(std::fs::read(&partial_only).unwrap(), original);
        assert_eq!(
            check_file(&partial_only, &requirements)
                .unwrap()
                .missing_subtitles,
            vec!["eng"]
        );
    }

    #[cfg(all(feature = "ffmpeg-cli-tests", unix))]
    #[test]
    fn audio_only_retag_preserves_all_subtitle_dispositions() {
        if !ffmpeg_present() {
            return;
        }
        let tmp = secure_retag_tempdir();
        let media = tmp.path().join("audio.mkv");
        make_retag_subtitle_fixture(&media, true, "eng");
        let before = stream_languages_and_dispositions(&media);
        assert_ne!(before[1].1 & ffi::AV_DISPOSITION_FORCED as i32, 0);
        assert_eq!(before[1].2.as_deref(), Some("eng"));
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".into()],
            subtitles: vec!["eng".into()],
        };
        let report = retag_unknown_streams(
            &media,
            &requirements,
            &UntaggedRetagOptions {
                audio_language: Some("eng".into()),
                subtitle_language: None,
                dry_run: false,
            },
        )
        .unwrap();
        assert_eq!(report.audio_streams, 1);
        assert_eq!(report.subtitle_streams, 0);
        let after = stream_languages_and_dispositions(&media);
        assert_eq!(
            before.iter().map(|s| s.1).collect::<Vec<_>>(),
            after.iter().map(|s| s.1).collect::<Vec<_>>()
        );
        assert_eq!(after[0].2.as_deref(), Some("eng"));
        assert_eq!(after[1].2.as_deref(), Some("eng"));
        assert!(after[2..].iter().all(|stream| stream.2.is_none()));
        assert_eq!(
            check_file(&media, &requirements).unwrap().missing_subtitles,
            vec!["eng"]
        );
    }

    #[cfg(all(feature = "ffmpeg-cli-tests", unix))]
    fn make_metadata_fixture(path: &Path, chapters: bool, bitexact: bool) {
        let mut command = std::process::Command::new("ffmpeg");
        command.args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=500:duration=1",
        ]);
        if chapters {
            let file = path.parent().unwrap().join("chapters.ffmeta");
            std::fs::write(&file, ";FFMETADATA1\ntitle=Fixture Title\ncomment=Fixture Comment\n[CHAPTER]\nTIMEBASE=1/1000\nSTART=0\nEND=900\ntitle=Opening\n").unwrap();
            command.args(["-f", "ffmetadata", "-i"]).arg(&file);
            command.args(["-map", "0:a", "-map_metadata", "1", "-map_chapters", "1"]);
        } else {
            command.args([
                "-metadata",
                "title=Fixture Title",
                "-metadata",
                "comment=Fixture Comment",
            ]);
        }
        if bitexact {
            command.args(["-fflags", "+bitexact"]);
        }
        let status = command
            .args(["-c:a", "aac", "-metadata:s:a:0", "language=und"])
            .arg(path)
            .status()
            .unwrap();
        assert!(status.success());
    }

    #[cfg(all(feature = "ffmpeg-cli-tests", unix))]
    #[test]
    fn retag_preserves_container_metadata_or_fails_closed_for_chapters() {
        if !ffmpeg_present() {
            return;
        }
        let tmp = secure_retag_tempdir();
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".into()],
            subtitles: vec![],
        };
        let options = UntaggedRetagOptions {
            audio_language: Some("eng".into()),
            subtitle_language: None,
            dry_run: false,
        };

        let media = tmp.path().join("metadata.mkv");
        make_metadata_fixture(&media, false, true);
        let path = path_to_cstr(&media).unwrap();
        let input = AVFormatContextInput::open(path.as_c_str()).unwrap();
        let before = container_metadata(input.as_ptr()).unwrap();
        assert!(before.contains(&(b"title".to_vec(), b"Fixture Title".to_vec())));
        assert!(before.contains(&(b"comment".to_vec(), b"Fixture Comment".to_vec())));
        drop(input);
        assert_eq!(
            retag_unknown_streams(&media, &requirements, &options)
                .unwrap()
                .audio_streams,
            1
        );
        let output = AVFormatContextInput::open(path.as_c_str()).unwrap();
        let after = container_metadata(output.as_ptr()).unwrap();
        assert!(before.iter().all(|entry| after.contains(entry)));

        let chaptered = tmp.path().join("chaptered.mkv");
        make_metadata_fixture(&chaptered, true, true);
        let chapter_path = path_to_cstr(&chaptered).unwrap();
        let input = AVFormatContextInput::open(chapter_path.as_c_str()).unwrap();
        assert!(input.nb_chapters > 0);
        assert!(container_metadata(input.as_ptr())
            .unwrap()
            .contains(&(b"title".to_vec(), b"Fixture Title".to_vec())));
        drop(input);
        let bytes = std::fs::read(&chaptered).unwrap();
        let dry_run = UntaggedRetagOptions {
            dry_run: true,
            ..options.clone()
        };
        assert!(retag_unknown_streams(&chaptered, &requirements, &dry_run).is_err());
        let error = retag_unknown_streams(&chaptered, &requirements, &options).unwrap_err();
        assert!(format!("{error:#}").contains("chapters"));
        assert_eq!(std::fs::read(&chaptered).unwrap(), bytes);
        assert!(!retag_temp_path(&chaptered).exists());

        let unsupported = tmp.path().join("encoder.mkv");
        make_metadata_fixture(&unsupported, false, false);
        let bytes = std::fs::read(&unsupported).unwrap();
        let error = retag_unknown_streams(&unsupported, &requirements, &options).unwrap_err();
        assert!(format!("{error:#}").contains("metadata"));
        assert_eq!(std::fs::read(&unsupported).unwrap(), bytes);
        // Failed verification removes only the staging inode created by this run.
        let staged = retag_temp_path(&unsupported);
        assert!(!staged.exists());
        std::fs::write(&staged, b"pre-existing file").unwrap();
        let error = retag_unknown_streams(&unsupported, &requirements, &options).unwrap_err();
        assert!(format!("{error:#}").contains("without overwrite"));
        assert_eq!(std::fs::read(&staged).unwrap(), b"pre-existing file");
        assert_eq!(std::fs::read(&unsupported).unwrap(), bytes);
    }

    #[test]
    fn original_audio_aliases_match_without_changing_requirements() {
        assert_eq!(normalize_language_tag("dut").as_deref(), Some("nld"));
        assert_eq!(normalize_language_tag("nob").as_deref(), Some("nor"));
        assert_eq!(normalize_language_tag("nno").as_deref(), Some("nor"));
        assert_eq!(sidecar_language("dut").as_deref(), Some("nld"));
        assert_eq!(sidecar_language("nob").as_deref(), Some("nor"));
        assert_eq!(sidecar_language("nno").as_deref(), Some("nor"));
        assert_eq!(
            parse_language_list(Some("Portuguese (Brazil), Spanish (Latino), dut nob")),
            ["nld", "nor", "por", "spa"]
        );
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["dut".into(), "nob".into()],
            subtitles: vec![],
        };
        let report = check_sets(vec!["nld".into(), "nno".into()], vec![], &requirements);
        assert!(report.satisfied());
        assert_eq!(requirements.audio, ["dut", "nob"]);
    }

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
                "-fflags",
                "+bitexact",
                "-metadata:s:a:0",
                &format!("language={language}"),
            ])
            .arg(path)
            .status()
            .expect("run ffmpeg audio fixture");
        assert!(status.success(), "ffmpeg audio fixture failed");
    }

    #[cfg(all(feature = "ffmpeg-cli-tests", unix))]
    #[test]
    fn retag_unknown_streams_sets_unknown_audio_language() {
        if !ffmpeg_present() {
            eprintln!(
                "skipping retag_unknown_streams_sets_unknown_audio_language: ffmpeg not found"
            );
            return;
        }
        let tmp = secure_retag_tempdir();
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
