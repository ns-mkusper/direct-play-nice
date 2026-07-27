//! Stream setup helpers used before the packet-processing loop starts.
//!
//! These routines classify non-playable streams, create decoder contexts, and
//! initialize progress tracking. Keeping them outside the main conversion
//! function makes the high-level pipeline easier to audit.

use anyhow::Result;
use log::{info, warn};
use rsmpeg::avcodec::{AVCodecContext, AVCodecRef};
use rsmpeg::avformat::{AVFormatContextInput, AVStreamRef};
use rsmpeg::ffi;
use std::collections::HashSet;

use crate::ffmpeg_utils::ProgressTracker;
use crate::transcoder::ffmpeg_ext::{codec_name, input_duration_us, stream_disposition};
use crate::transcoder::helpers::enable_strict_decode_failure;

pub(crate) const TRACK_BLOAT_AUDIO_THRESHOLD: usize = 4;
pub(crate) const TRACK_BLOAT_SUBTITLE_THRESHOLD: usize = 8;
pub(crate) const TRACK_BLOAT_MAX_AUDIO: usize = 2;
pub(crate) const TRACK_BLOAT_MAX_SUBTITLES: usize = 1;

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub(crate) struct TrackHygienePlan {
    pub(crate) bloated: bool,
    pub(crate) kept_streams: HashSet<i32>,
    pub(crate) audio_count: usize,
    pub(crate) subtitle_count: usize,
}

impl TrackHygienePlan {
    pub(crate) fn keeps(&self, stream_index: i32) -> bool {
        !self.bloated || self.kept_streams.contains(&stream_index)
    }

    pub(crate) fn reason(&self) -> Option<String> {
        if !self.bloated {
            return None;
        }
        Some(format!(
            "input has bloated audio/subtitle track layout ({} audio, {} subtitle streams); remux needed to keep direct-play startup responsive",
            self.audio_count, self.subtitle_count
        ))
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct TrackCandidate {
    index: i32,
    language: Option<String>,
    default: bool,
    forced: bool,
}

/// Creates progress tracking from container duration while tolerating files
/// where the demuxer cannot determine duration up front.
pub(crate) fn create_progress_tracker(
    input_format_context: &mut AVFormatContextInput,
) -> Option<ProgressTracker> {
    let mut container_duration_us = input_duration_us(input_format_context);
    if container_duration_us <= 0 {
        // Keep progress reporting alive for containers with missing/invalid duration.
        container_duration_us = 1;
    }
    Some(ProgressTracker::new(container_duration_us))
}

/// Filters streams that are metadata or muxer side data rather than playable
/// audio/video/subtitle content for direct-play outputs.
pub(crate) fn should_skip_auxiliary_stream(
    stream: &AVStreamRef<'_>,
    input_codec_type: ffi::AVMediaType,
) -> bool {
    if input_codec_type == ffi::AVMEDIA_TYPE_ATTACHMENT {
        warn!(
            "Skipping attachment stream {} ({}).",
            stream.index,
            codec_name(stream.codecpar().codec_id)
        );
        return true;
    }

    if input_codec_type == ffi::AVMEDIA_TYPE_DATA {
        warn!(
            "Skipping data stream {} ({}).",
            stream.index,
            codec_name(stream.codecpar().codec_id)
        );
        return true;
    }

    if (stream_disposition(stream) & ffi::AV_DISPOSITION_ATTACHED_PIC as i32) != 0 {
        info!(
            "Skipping attached-picture stream {} ({}).",
            stream.index,
            codec_name(stream.codecpar().codec_id)
        );
        return true;
    }

    false
}

pub(crate) fn plan_track_hygiene(input_ctx: &AVFormatContextInput) -> TrackHygienePlan {
    let mut audio = Vec::new();
    let mut subtitles = Vec::new();

    for stream in input_ctx.streams() {
        let candidate = TrackCandidate {
            index: stream.index,
            language: stream_language(stream),
            default: (stream_disposition(stream) & ffi::AV_DISPOSITION_DEFAULT as i32) != 0,
            forced: (stream_disposition(stream) & ffi::AV_DISPOSITION_FORCED as i32) != 0,
        };
        match stream.codecpar().codec_type {
            ffi::AVMEDIA_TYPE_AUDIO => audio.push(candidate),
            ffi::AVMEDIA_TYPE_SUBTITLE => subtitles.push(candidate),
            _ => {}
        }
    }

    let bloated = audio.len() > TRACK_BLOAT_AUDIO_THRESHOLD
        || subtitles.len() > TRACK_BLOAT_SUBTITLE_THRESHOLD;
    if !bloated {
        return TrackHygienePlan {
            bloated,
            audio_count: audio.len(),
            subtitle_count: subtitles.len(),
            kept_streams: HashSet::new(),
        };
    }

    let mut kept_streams = HashSet::new();
    keep_ranked_tracks(&audio, TRACK_BLOAT_MAX_AUDIO, &mut kept_streams);
    keep_ranked_tracks(&subtitles, TRACK_BLOAT_MAX_SUBTITLES, &mut kept_streams);

    TrackHygienePlan {
        bloated,
        kept_streams,
        audio_count: audio.len(),
        subtitle_count: subtitles.len(),
    }
}

fn keep_ranked_tracks(
    candidates: &[TrackCandidate],
    limit: usize,
    kept_streams: &mut HashSet<i32>,
) {
    if limit == 0 {
        return;
    }

    let mut ranked = candidates.to_vec();
    ranked.sort_by_key(track_rank);
    for candidate in ranked.into_iter().take(limit) {
        kept_streams.insert(candidate.index);
    }
}

fn track_rank(candidate: &TrackCandidate) -> (u8, u8, i32) {
    let language_rank = match candidate.language.as_deref() {
        Some("jpn") => 0,
        Some("eng") => 1,
        Some(_) => 2,
        None => 3,
    };
    let disposition_rank = if candidate.default {
        0
    } else if candidate.forced {
        1
    } else {
        2
    };
    (language_rank, disposition_rank, candidate.index)
}

fn stream_language(stream: &AVStreamRef<'_>) -> Option<String> {
    let metadata = stream.metadata()?;
    for entry in metadata.iter() {
        if entry
            .key()
            .to_string_lossy()
            .eq_ignore_ascii_case("language")
        {
            let language = entry.value().to_string_lossy().trim().to_ascii_lowercase();
            if !language.is_empty() && language != "und" {
                return Some(language);
            }
        }
    }
    None
}

/// Builds a decoder context from an input stream and applies the strict decode
/// policy used by the conversion pipeline.
pub(crate) fn new_decode_context(
    decoder: &AVCodecRef<'_>,
    stream: &AVStreamRef<'_>,
) -> Result<AVCodecContext> {
    let mut decode_context = AVCodecContext::new(decoder);
    decode_context.apply_codecpar(&stream.codecpar())?;
    enable_strict_decode_failure(&mut decode_context);
    decode_context.set_time_base(stream.time_base);
    if let Some(framerate) = stream.guess_framerate() {
        decode_context.set_framerate(framerate);
    }
    Ok(decode_context)
}
