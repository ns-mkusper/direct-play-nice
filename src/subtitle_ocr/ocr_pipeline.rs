//! Per-stream OCR pipeline execution.
//!
//! This module handles bitmap subtitle packet decode, rectangle extraction,
//! OCR invocation, and cue assembly for a single subtitle stream.

use super::text_processing::ambiguous_short_single_token;
use super::*;
pub(super) fn discover_candidates(
    input_file: &CStr,
    sub_mode: SubMode,
    stream_filter: &OcrStreamFilter,
) -> Result<Vec<SubtitleCandidate>> {
    let ictx = AVFormatContextInput::open(input_file)?;
    let mut out = Vec::new();
    let skipped = stream_filter.skipped(&subtitle_stream_facts(&ictx));

    for stream in ictx.streams() {
        let cp = stream.codecpar();
        if cp.codec_type != ffi::AVMEDIA_TYPE_SUBTITLE {
            continue;
        }
        if !is_image_based_subtitle(cp.codec_id) {
            continue;
        }
        match skipped.get(&stream.index) {
            Some(OcrSkipReason::Redundant) => {
                info!(
                    "Skipping OCR for bitmap subtitle stream {}: a text subtitle stream already covers its language (--no-skip-redundant-ocr to OCR it anyway).",
                    stream.index
                );
                continue;
            }
            Some(OcrSkipReason::LanguageNotListed) => {
                info!(
                    "Skipping OCR for bitmap subtitle stream {}: its language is not in --ocr-languages; the track is not carried into the output.",
                    stream.index
                );
                continue;
            }
            None => {}
        }

        let language_tag = stream
            .metadata()
            .as_deref()
            .and_then(extract_language_tag_from_metadata);

        if matches!(sub_mode, SubMode::Auto | SubMode::Force) {
            out.push(SubtitleCandidate {
                stream_index: stream.index,
                language_tag,
            });
        }
    }

    Ok(out)
}

pub(super) fn probe_video_dimensions(input_file: &CStr) -> Option<(u32, u32)> {
    let ictx = AVFormatContextInput::open(input_file).ok()?;
    for stream in ictx.streams() {
        let cp = stream.codecpar();
        if cp.codec_type == ffi::AVMEDIA_TYPE_VIDEO && cp.width > 0 && cp.height > 0 {
            return Some((cp.width as u32, cp.height as u32));
        }
    }
    None
}

#[derive(Debug, Default, Clone)]
pub(super) struct OcrQualityBaseline {
    samples: usize,
    quality_sum: f32,
    confidence_sum: f32,
}

impl OcrQualityBaseline {
    const WINDOW_MS: i64 = 3 * 60 * 1_000;

    pub(super) fn observe(&mut self, quality: f32, confidence: f32, timestamp_ms: i64) {
        if !(0..=Self::WINDOW_MS).contains(&timestamp_ms) {
            return;
        }
        if !quality.is_finite() || !confidence.is_finite() {
            return;
        }
        if !(0.0..=1.0).contains(&quality) || !(0.0..=1.0).contains(&confidence) {
            return;
        }
        self.samples += 1;
        self.quality_sum += quality;
        self.confidence_sum += confidence;
    }

    fn avg_quality(&self) -> Option<f32> {
        if self.samples == 0 {
            None
        } else {
            Some(self.quality_sum / self.samples as f32)
        }
    }

    fn avg_confidence(&self) -> Option<f32> {
        if self.samples == 0 {
            None
        } else {
            Some(self.confidence_sum / self.samples as f32)
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub(super) struct OcrFallbackThresholds {
    pub(super) quality: f32,
    pub(super) confidence: f32,
}

/// Immutable request parameters required to OCR one subtitle stream.
pub(super) struct OcrStreamRequest<'a> {
    pub(super) input_path: &'a str,
    pub(super) stream_index: i32,
    pub(super) language: &'a str,
    pub(super) work_dir: &'a Path,
    pub(super) ocr_format: OcrFormat,
    pub(super) video_dimensions: Option<(u32, u32)>,
    pub(super) ocr_engine: OcrEngine,
}

/// Per-packet/per-subtitle context used when turning decoded subtitle data into cues.
pub(super) struct CueBuildParams<'a> {
    pub(super) language: &'a str,
    pub(super) stream_index: i32,
    pub(super) stream_codec_id: ffi::AVCodecID,
    pub(super) packet_seq: usize,
    pub(super) work_dir: &'a Path,
    pub(super) ocr_format: OcrFormat,
    pub(super) video_dimensions: Option<(u32, u32)>,
    pub(super) ocr_engine: OcrEngine,
}

#[derive(Debug)]
pub(super) struct OcrDecodeOutcome {
    pub(super) cues: Vec<SubtitleCue>,
    pub(super) stream_codec_id: ffi::AVCodecID,
    pub(super) subtitle_packet_count: usize,
    pub(super) decoded_subtitle_count: usize,
    pub(super) decoded_rect_count: usize,
    pub(super) decoded_image_rect_count: usize,
}

pub(super) fn ocr_single_stream(
    request: &OcrStreamRequest<'_>,
    engine: &mut dyn SubtitleConverter,
) -> Result<Vec<SubtitleCue>> {
    let mut outcome = ocr_single_stream_once(request, engine)?;

    if should_retry_bitmap_ocr_with_external_remux(&outcome) {
        match normalize_bitmap_subtitle_stream_for_ocr(request) {
            Ok(Some(normalized_path)) => {
                let normalized_input = normalized_path.to_string_lossy().into_owned();
                let retry_request = OcrStreamRequest {
                    input_path: &normalized_input,
                    stream_index: 0,
                    language: request.language,
                    work_dir: request.work_dir,
                    ocr_format: request.ocr_format,
                    video_dimensions: request.video_dimensions,
                    ocr_engine: request.ocr_engine,
                };
                let retry = ocr_single_stream_once(&retry_request, engine)?;
                if !retry.cues.is_empty() {
                    info!(
                        "OCR subtitle stream {} recovered after ffmpeg subtitle remux fallback ({} cues)",
                        request.stream_index,
                        retry.cues.len()
                    );
                    return Ok(retry.cues);
                }
                warn!(
                    "OCR subtitle stream {} ffmpeg subtitle remux fallback produced no cues; decoded_subtitles={}, decoded_rects={}, decoded_image_rects={}",
                    request.stream_index,
                    retry.decoded_subtitle_count,
                    retry.decoded_rect_count,
                    retry.decoded_image_rect_count
                );
                outcome = retry;
            }
            Ok(None) => {}
            Err(err) => {
                warn!(
                    "OCR subtitle stream {} ffmpeg subtitle remux fallback failed: {err:#}",
                    request.stream_index
                );
            }
        }
    }

    if outcome.cues.is_empty()
        && outcome.subtitle_packet_count > 0
        && is_image_based_subtitle(outcome.stream_codec_id)
    {
        bail!(
            "OCR produced no cues for bitmap subtitle stream {} ({}) despite reading {} subtitle packet(s); decoded_subtitles={}, decoded_rects={}, decoded_image_rects={}, video_dimensions={:?}. This usually means the bitmap subtitle decoder could not derive a valid canvas/rectangle layout.",
            request.stream_index,
            codec_name(outcome.stream_codec_id),
            outcome.subtitle_packet_count,
            outcome.decoded_subtitle_count,
            outcome.decoded_rect_count,
            outcome.decoded_image_rect_count,
            request.video_dimensions
        );
    }

    Ok(outcome.cues)
}

fn ocr_single_stream_once(
    request: &OcrStreamRequest<'_>,
    engine: &mut dyn SubtitleConverter,
) -> Result<OcrDecodeOutcome> {
    let input_cstr = CString::new(request.input_path).context("input path has interior NUL")?;
    let mut ictx = AVFormatContextInput::open(input_cstr.as_c_str())?;

    let (stream_time_base, stream_codec_id, stream_width, stream_height) = ictx
        .streams()
        .iter()
        .find(|st| st.index == request.stream_index)
        .map(|st| {
            let cp = st.codecpar();
            (st.time_base, cp.codec_id, cp.width, cp.height)
        })
        .ok_or_else(|| anyhow!("subtitle stream {} not found", request.stream_index))?;

    let decoder = AVCodec::find_decoder(stream_codec_id).ok_or_else(|| {
        anyhow!(
            "decoder unavailable for subtitle stream {} ({})",
            request.stream_index,
            codec_name(stream_codec_id)
        )
    })?;

    let mut decode_context = AVCodecContext::new(&decoder);
    let mut applied_codecpar = false;
    for st in ictx.streams() {
        if st.index == request.stream_index {
            decode_context.apply_codecpar(&st.codecpar())?;
            applied_codecpar = true;
            break;
        }
    }
    if !applied_codecpar {
        bail!(
            "subtitle stream {} codec parameters unavailable",
            request.stream_index
        );
    }
    decode_context.set_time_base(stream_time_base);
    crate::transcoder::helpers::ensure_decoder_pkt_time_base(&mut decode_context, stream_time_base);
    apply_bitmap_subtitle_canvas_fallback(
        &mut decode_context,
        stream_codec_id,
        stream_width,
        stream_height,
        request.video_dimensions,
        request.stream_index,
    );
    decode_context.open(None)?;

    let mut cues = Vec::new();
    let mut pgs_timing = PgsCueTiming::default();
    let mut subtitle_packet_count: usize = 0;
    let mut decoded_subtitle_count: usize = 0;
    let mut decoded_rect_count: usize = 0;
    let mut decoded_image_rect_count: usize = 0;
    let mut quality_baseline = OcrQualityBaseline::default();
    let mut cue_params = CueBuildParams {
        language: request.language,
        stream_index: request.stream_index,
        stream_codec_id,
        packet_seq: 0,
        work_dir: request.work_dir,
        ocr_format: request.ocr_format,
        video_dimensions: request.video_dimensions,
        ocr_engine: request.ocr_engine,
    };
    let mut append_decoded = |subtitle: *const ffi::AVSubtitle,
                              fallback_start_ms: Option<i64>,
                              fallback_dur_ms: i64,
                              engine: &mut dyn SubtitleConverter|
     -> Result<()> {
        decoded_subtitle_count += 1;
        let (rect_count, image_rect_count) = subtitle_rect_counts(subtitle);
        decoded_rect_count += rect_count;
        decoded_image_rect_count += image_rect_count;
        append_subtitle_cues(
            subtitle,
            fallback_start_ms,
            fallback_dur_ms,
            &cue_params,
            engine,
            &mut quality_baseline,
            &mut cues,
            &mut pgs_timing,
        )?;
        cue_params.packet_seq += 1;
        Ok(())
    };

    while let Some(mut packet) = ictx.read_packet()? {
        if packet.stream_index != request.stream_index {
            continue;
        }
        subtitle_packet_count += 1;

        let src_pts = packet.pts;
        let src_dur = packet.duration;

        packet.rescale_ts(stream_time_base, decode_context.time_base);

        if let Some(subtitle) = decode_context.decode_subtitle(Some(&mut packet))? {
            let fallback_start_ms = timestamp_to_ms(src_pts, stream_time_base);
            let fallback_dur_ms = timestamp_to_ms(src_dur, stream_time_base)
                .unwrap_or(0)
                .max(0);
            append_decoded(
                subtitle.as_ptr(),
                fallback_start_ms,
                fallback_dur_ms,
                engine,
            )?;
        }
    }

    // Drain subtitles buffered inside the decoder.
    while let Some(subtitle) = decode_context.decode_subtitle(None)? {
        append_decoded(subtitle.as_ptr(), None, 0, engine)?;
    }

    pgs_timing.finish(&mut cues);
    sanitize_cues(&mut cues, request.ocr_format);

    Ok(OcrDecodeOutcome {
        cues,
        stream_codec_id,
        subtitle_packet_count,
        decoded_subtitle_count,
        decoded_rect_count,
        decoded_image_rect_count,
    })
}

pub(super) fn should_retry_bitmap_ocr_with_external_remux(outcome: &OcrDecodeOutcome) -> bool {
    outcome.cues.is_empty()
        && outcome.subtitle_packet_count > 0
        && outcome.decoded_subtitle_count == 0
        && is_image_based_subtitle(outcome.stream_codec_id)
}

pub(super) fn normalize_bitmap_subtitle_stream_for_ocr(
    request: &OcrStreamRequest<'_>,
) -> Result<Option<PathBuf>> {
    let ffmpeg = env::var("DPN_FFMPEG_BINARY").unwrap_or_else(|_| "ffmpeg".to_string());
    let output_path = request
        .work_dir
        .join(format!("ocr-s{}-normalized.mks", request.stream_index));
    let map_arg = format!("0:{}", request.stream_index);

    info!(
        "Retrying OCR subtitle stream {} via ffmpeg subtitle remux fallback",
        request.stream_index
    );
    let output = Command::new(&ffmpeg)
        .arg("-hide_banner")
        .arg("-loglevel")
        .arg("error")
        .arg("-y")
        .arg("-i")
        .arg(request.input_path)
        .arg("-map")
        .arg(&map_arg)
        .arg("-c")
        .arg("copy")
        .arg("-f")
        .arg("matroska")
        .arg(&output_path)
        .output()
        .with_context(|| {
            format!(
                "running ffmpeg subtitle remux fallback for stream {}",
                request.stream_index
            )
        })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        let stderr = stderr.trim();
        if stderr.is_empty() {
            bail!(
                "ffmpeg subtitle remux fallback failed for stream {} with status {}",
                request.stream_index,
                output.status
            );
        }
        bail!(
            "ffmpeg subtitle remux fallback failed for stream {} with status {}: {}",
            request.stream_index,
            output.status,
            stderr
        );
    }

    let metadata = fs::metadata(&output_path).with_context(|| {
        format!(
            "ffmpeg subtitle remux fallback did not create {}",
            output_path.display()
        )
    })?;
    if metadata.len() == 0 {
        warn!(
            "ffmpeg subtitle remux fallback produced an empty file for stream {}",
            request.stream_index
        );
        return Ok(None);
    }

    Ok(Some(output_path))
}

pub(super) fn apply_bitmap_subtitle_canvas_fallback(
    decode_context: &mut AVCodecContext,
    codec_id: ffi::AVCodecID,
    stream_width: i32,
    stream_height: i32,
    video_dimensions: Option<(u32, u32)>,
    stream_index: i32,
) {
    if !is_image_based_subtitle(codec_id) {
        return;
    }
    if decode_context.width > 0 && decode_context.height > 0 {
        return;
    }

    let Some((video_width, video_height)) = video_dimensions else {
        return;
    };
    if video_width == 0 || video_height == 0 {
        return;
    }

    let width = stream_width.max(video_width as i32);
    let height = stream_height.max(video_height as i32);
    if width <= 0 || height <= 0 {
        return;
    }

    unsafe {
        let ctx = decode_context.as_mut_ptr();
        (*ctx).width = width;
        (*ctx).height = height;
        (*ctx).coded_width = width;
        (*ctx).coded_height = height;
    }
    info!(
        "Applying bitmap subtitle canvas fallback for stream {} ({}): {}x{} from video dimensions {:?}",
        stream_index,
        codec_name(codec_id),
        width,
        height,
        video_dimensions
    );
}

pub(super) fn subtitle_rect_counts(subtitle: *const ffi::AVSubtitle) -> (usize, usize) {
    if subtitle.is_null() {
        return (0, 0);
    }
    let sub = unsafe { &*subtitle };
    if sub.num_rects == 0 || sub.rects.is_null() {
        return (0, 0);
    }

    let mut image_rects = 0usize;
    for i in 0..sub.num_rects {
        let rect_ptr = unsafe { *sub.rects.add(i as usize) };
        if rect_ptr.is_null() {
            continue;
        }
        let rect = unsafe { &*rect_ptr };
        if rect.type_ == ffi::SUBTITLE_BITMAP {
            image_rects += 1;
        }
    }

    (sub.num_rects as usize, image_rects)
}

/// Event base time: the decoder's microsecond PTS, else the packet timestamp.
fn subtitle_base_ms(subtitle: &ffi::AVSubtitle, fallback_start_ms: Option<i64>) -> Option<i64> {
    if subtitle.pts != ffi::AV_NOPTS_VALUE {
        Some(subtitle.pts / 1000)
    } else {
        fallback_start_ms
    }
}

fn subtitle_start_ms(subtitle: &ffi::AVSubtitle, fallback_start_ms: Option<i64>) -> Option<i64> {
    let base_ms = subtitle_base_ms(subtitle, fallback_start_ms)?;
    Some(
        base_ms
            .saturating_add(subtitle.start_display_time as i64)
            .max(0),
    )
}

/// Bounds PGS cues by the next display set, since PGS events often omit an end time.
#[derive(Default)]
struct PgsCueTiming {
    /// Cue indices of the open event, whether it lacks an end time, and its
    /// packet duration for use when no later event closes it.
    active: Option<(std::ops::Range<usize>, bool, i64)>,
}

impl PgsCueTiming {
    fn finish_at(&mut self, cues: &mut [SubtitleCue], event_start_ms: Option<i64>) {
        let Some((range, indefinite, fallback_duration_ms)) = self.active.take() else {
            return;
        };
        for cue in &mut cues[range] {
            let boundary = event_start_ms.filter(|&start| start >= cue.start_ms);
            if indefinite {
                // Match the existing one-second minimum while bounding unclosed PGS events.
                cue.end_ms = boundary.unwrap_or_else(|| {
                    cue.start_ms
                        .saturating_add(fallback_duration_ms.clamp(1_000, 10_000))
                });
            } else if let Some(boundary) = boundary {
                cue.end_ms = cue.end_ms.min(boundary);
            }
        }
    }

    fn finish(&mut self, cues: &mut [SubtitleCue]) {
        self.finish_at(cues, None);
    }

    fn append(
        &mut self,
        cues: &mut Vec<SubtitleCue>,
        mut new_cues: Vec<SubtitleCue>,
        start_ms: Option<i64>,
        indefinite: bool,
        fallback_duration_ms: i64,
    ) {
        self.finish_at(cues, start_ms);
        let start = cues.len();
        cues.append(&mut new_cues);
        self.active = Some((start..cues.len(), indefinite, fallback_duration_ms));
    }
}

#[allow(clippy::too_many_arguments)]
fn append_subtitle_cues(
    subtitle: *const ffi::AVSubtitle,
    fallback_start_ms: Option<i64>,
    fallback_duration_ms: i64,
    params: &CueBuildParams<'_>,
    engine: &mut dyn SubtitleConverter,
    quality_baseline: &mut OcrQualityBaseline,
    cues: &mut Vec<SubtitleCue>,
    pgs_timing: &mut PgsCueTiming,
) -> Result<()> {
    let new_cues = subtitle_to_cues(
        subtitle,
        fallback_start_ms,
        fallback_duration_ms,
        params,
        engine,
        quality_baseline,
    )?;
    if params.stream_codec_id == ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE && !subtitle.is_null() {
        let sub = unsafe { &*subtitle };
        pgs_timing.append(
            cues,
            new_cues,
            subtitle_start_ms(sub, fallback_start_ms),
            sub.end_display_time == u32::MAX || sub.end_display_time <= sub.start_display_time,
            fallback_duration_ms,
        );
    } else {
        cues.extend(new_cues);
    }
    Ok(())
}

fn subtitle_to_cues(
    subtitle: *const ffi::AVSubtitle,
    fallback_start_ms: Option<i64>,
    fallback_duration_ms: i64,
    params: &CueBuildParams<'_>,
    engine: &mut dyn SubtitleConverter,
    quality_baseline: &mut OcrQualityBaseline,
) -> Result<Vec<SubtitleCue>> {
    if subtitle.is_null() {
        return Ok(Vec::new());
    }

    let sub = unsafe { &*subtitle };
    let base_ms = subtitle_base_ms(sub, fallback_start_ms).unwrap_or(0);
    let start_ms = subtitle_start_ms(sub, fallback_start_ms)
        .unwrap_or_else(|| (sub.start_display_time as i64).max(0));
    let mut end_ms = base_ms
        .saturating_add(sub.end_display_time as i64)
        .max(start_ms);
    if end_ms <= start_ms {
        end_ms = start_ms.saturating_add(fallback_duration_ms.max(1_000));
    }

    let (lines, had_imagery) =
        extract_subtitle_lines(sub, params, start_ms, engine, quality_baseline)?;
    if had_imagery && lines.is_empty() {
        warn!(
            "OCR produced empty text for subtitle stream {} at {} ms",
            params.stream_index, start_ms
        );
    }

    let mut cues = Vec::new();
    match params.ocr_format {
        OcrFormat::Srt => {
            let merged = lines
                .iter()
                .map(|line| line.text.as_str())
                .filter(|text| !text.is_empty())
                .collect::<Vec<_>>()
                .join("\n");
            if merged.is_empty() {
                return Ok(Vec::new());
            }
            cues.push(SubtitleCue {
                start_ms,
                end_ms,
                text: merged,
            });
        }
        OcrFormat::Ass => {
            let mut unpositioned = Vec::new();
            for line in lines {
                if line.text.is_empty() {
                    continue;
                }
                if let Some(bbox) = line.bbox {
                    let (pos_x, pos_y) = ass_position_from_bbox(&bbox, params.video_dimensions);
                    let ass_text = format_ass_text_with_style(
                        &line.text,
                        Some((pos_x, pos_y)),
                        line.color,
                        line.italic,
                    );
                    cues.push(SubtitleCue {
                        start_ms,
                        end_ms,
                        text: ass_text,
                    });
                } else {
                    unpositioned.push(line.text);
                }
            }
            if !unpositioned.is_empty() {
                let merged = unpositioned.join("\n");
                let ass_text = format_ass_text_with_style(&merged, None, None, false);
                cues.push(SubtitleCue {
                    start_ms,
                    end_ms,
                    text: ass_text,
                });
            }
        }
    }

    Ok(cues)
}

/// Lines recognized from one subtitle rectangle. Bitmap rectangles carry their
/// vertical extent so SRT output can restore top-to-bottom order.
#[derive(Default)]
struct SubtitleRectLines {
    vertical_bounds: Option<(i64, i64)>,
    lines: Vec<OcrLine>,
}

fn order_srt_rect_groups(groups: &mut [SubtitleRectLines]) {
    // Rectangles without reliable geometry are barriers: never move another
    // group across them. Overlap components preserve decoder order, including
    // transitive overlaps, while disjoint components have a clear vertical order.
    for run in groups.split_mut(|group| group.vertical_bounds.is_none()) {
        let bounds = |i: usize| run[i].vertical_bounds.unwrap();
        let mut order: Vec<usize> = (0..run.len()).collect();
        order.sort_by_key(|&i| bounds(i).0);
        let mut start = 0;
        while start < order.len() {
            let mut end = start + 1;
            let mut bottom = bounds(order[start]).1;
            while end < order.len() {
                let (top, next_bottom) = bounds(order[end]);
                if top > bottom {
                    break;
                }
                bottom = bottom.max(next_bottom);
                end += 1;
            }
            order[start..end].sort_unstable();
            start = end;
        }
        let reordered: Vec<_> = order.iter().map(|&i| std::mem::take(&mut run[i])).collect();
        for (slot, group) in run.iter_mut().zip(reordered) {
            *slot = group;
        }
    }
}

fn unpositioned_text_group(
    text: &str,
    color: Option<(u8, u8, u8)>,
    italic: bool,
) -> Option<SubtitleRectLines> {
    let text = normalize_utf8_text(text);
    if text.is_empty() {
        return None;
    }
    Some(SubtitleRectLines {
        vertical_bounds: None,
        lines: vec![OcrLine {
            text,
            bbox: None,
            score: None,
            color,
            italic,
        }],
    })
}

fn training_export_requested() -> bool {
    env::var_os("DPN_OCR_TRAINING_MANIFEST").is_some()
}

fn word_segmentation_enabled() -> bool {
    env::var("DPN_OCR_WORD_SEGMENTATION")
        .map(|v| {
            !matches!(
                v.trim().to_ascii_lowercase().as_str(),
                "0" | "false" | "no" | "off"
            )
        })
        .unwrap_or(true)
}

fn remove_intermediate(path: &Path) {
    if !keep_ocr_intermediates() {
        let _ = fs::remove_file(path);
    }
}

fn may_use_bitmap_lines(
    params: &CueBuildParams<'_>,
    engine: &dyn SubtitleConverter,
    training_requested: bool,
    force_tesseract_non_english: bool,
) -> bool {
    // Direct lines do not retain detector training metadata or run the forced
    // Tesseract fallback, so use the detector path for either policy.
    !training_requested
        && !(force_tesseract_non_english
            && !is_english_language(params.language)
            && language_uses_spaces(params.language))
        && params.stream_codec_id == ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE
        && matches!(params.ocr_engine, OcrEngine::PpOcrV3 | OcrEngine::PpOcrV4)
        && matches!(params.ocr_format, OcrFormat::Srt)
        && engine.supports_bitmap_line_recognition(params.language)
}

pub(super) fn extract_subtitle_lines(
    subtitle: &ffi::AVSubtitle,
    params: &CueBuildParams<'_>,
    subtitle_start_ms: i64,
    engine: &mut dyn SubtitleConverter,
    quality_baseline: &mut OcrQualityBaseline,
) -> Result<(Vec<OcrLine>, bool)> {
    if subtitle.num_rects == 0 || subtitle.rects.is_null() {
        return Ok((Vec::new(), false));
    }

    let mut groups = Vec::new();
    let mut had_imagery = false;
    for i in 0..subtitle.num_rects {
        let rect_ptr = unsafe { *subtitle.rects.add(i as usize) };
        if rect_ptr.is_null() {
            continue;
        }
        let rect = unsafe { &*rect_ptr };

        if rect.type_ == ffi::SUBTITLE_TEXT && !rect.text.is_null() {
            let text = unsafe { CStr::from_ptr(rect.text) }.to_string_lossy();
            groups.extend(unpositioned_text_group(text.trim(), None, false));
            continue;
        }

        if rect.type_ == ffi::SUBTITLE_ASS && !rect.ass.is_null() {
            let ass = unsafe { CStr::from_ptr(rect.ass) }.to_string_lossy();
            let ass = ass.trim();
            groups.extend(unpositioned_text_group(
                &strip_ass_formatting(ass),
                parse_ass_color(ass),
                ass.contains("\\i1"),
            ));
            continue;
        }

        let Some((pgm, has_visible_pixels)) = rect_to_pgm(rect, params.ocr_engine) else {
            continue;
        };
        had_imagery |= has_visible_pixels;
        if !has_visible_pixels {
            continue;
        }

        let pgm_path = params.work_dir.join(format!(
            "ocr-s{}-p{}-r{}.pgm",
            params.stream_index, params.packet_seq, i
        ));
        fs::write(&pgm_path, &pgm)
            .with_context(|| format!("writing OCR frame {}", pgm_path.display()))?;
        let bitmap = BitmapRect {
            rect,
            index: i,
            pgm: &pgm,
            pgm_path: &pgm_path,
            color: dominant_color_from_rect(rect),
        };

        let lines =
            match recognize_rect_as_bitmap_lines(engine, &bitmap, params, subtitle.num_rects)? {
                Some(lines) => lines,
                None => recognize_rect_with_detector(
                    engine,
                    &bitmap,
                    params,
                    subtitle_start_ms,
                    quality_baseline,
                )?,
            };
        remove_intermediate(&pgm_path);
        if !lines.is_empty() {
            groups.push(SubtitleRectLines {
                vertical_bounds: Some((i64::from(rect.y), i64::from(rect.y) + i64::from(rect.h))),
                lines,
            });
        }
    }

    if matches!(params.ocr_format, OcrFormat::Srt) {
        order_srt_rect_groups(&mut groups);
    }
    let lines = groups.into_iter().flat_map(|group| group.lines).collect();
    Ok((lines, had_imagery))
}

/// One visible bitmap rectangle, rendered to a PGM work image for OCR.
struct BitmapRect<'a> {
    rect: &'a ffi::AVSubtitleRect,
    index: u32,
    pgm: &'a [u8],
    pgm_path: &'a Path,
    color: Option<(u8, u8, u8)>,
}

/// Recognizes eligible PGS rectangles line by line without the text detector.
/// Returns `None` when the direct route is unavailable or abstains for any line.
fn recognize_rect_as_bitmap_lines(
    engine: &mut dyn SubtitleConverter,
    bitmap: &BitmapRect<'_>,
    params: &CueBuildParams<'_>,
    cue_rect_count: u32,
) -> Result<Option<Vec<OcrLine>>> {
    if !may_use_bitmap_lines(
        params,
        engine,
        training_export_requested(),
        force_tesseract_non_english_enabled(),
    ) {
        return Ok(None);
    }
    let language = params.language;
    let lines = 'direct: {
        // SAFETY: the decoder keeps this bitmap's index and palette buffers
        // valid until subtitle processing completes, and the cropper checks
        // the declared stride and dimensions before reading them.
        let Some(crops) = (unsafe { split_pgs_bitmap_lines(bitmap.rect, bitmap.pgm) }) else {
            break 'direct None;
        };
        // An extreme number of bands is more likely noise or unsupported geometry.
        if crops.is_empty() || crops.len() > 64 {
            break 'direct None;
        }
        let context = if cue_rect_count == 1 && crops.len() == 1 {
            BitmapLineContext::SingleRectangleSingleBand
        } else {
            BitmapLineContext::UnknownOrMultiple
        };
        let recover_punctuation = !disable_tesseract_quality_fallback();
        let mut lines = Vec::with_capacity(crops.len());
        for (crop, _) in crops {
            let image = image::load_from_memory_with_format(&crop, image::ImageFormat::Pnm)
                .context("decoding experimental bitmap-line crop")?
                .to_rgb8();
            let Some(mut line) = engine.recognize_bitmap_line(&image, language, context)? else {
                break 'direct None;
            };
            line.text = line.text.trim().to_owned();
            if line.text.is_empty() {
                break 'direct None;
            }
            if recover_punctuation {
                let gray = image::imageops::grayscale(&image);
                recover_direct_line_punctuation_with(&mut line.text, &gray, || {
                    direct_punctuation_tesseract_attempts(bitmap.pgm_path, &crop, language)
                });
            }
            // This route is SRT-only, and scaled crop boxes are not display
            // coordinates, so lines carry no box.
            line.bbox = None;
            line.color = line.color.or(bitmap.color);
            lines.push(line);
        }
        Some(lines)
    };
    let Some(lines) = lines else {
        debug!(
            "Experimental bitmap-line OCR abstained for stream {} packet {} rect {}; using detector",
            params.stream_index, params.packet_seq, bitmap.index
        );
        return Ok(None);
    };
    debug!(
        "Experimental bitmap-line OCR processed {} lines for stream {} packet {} rect {}",
        lines.len(),
        params.stream_index,
        params.packet_seq,
        bitmap.index
    );
    Ok(Some(lines))
}

/// Runs the detector-based recognizer, then applies the spacing, quality, and
/// punctuation recovery passes that cross-check PP-OCR against Tesseract.
fn recognize_rect_with_detector(
    engine: &mut dyn SubtitleConverter,
    bitmap: &BitmapRect<'_>,
    params: &CueBuildParams<'_>,
    subtitle_start_ms: i64,
    quality_baseline: &mut OcrQualityBaseline,
) -> Result<Vec<OcrLine>> {
    let ocr_engine = params.ocr_engine;
    let language = params.language;
    let pgm_path = bitmap.pgm_path;
    let tesseract_fallback_enabled = matches!(ocr_engine, OcrEngine::PpOcrV4 | OcrEngine::PpOcrV3)
        && !disable_tesseract_quality_fallback();

    let mut output = engine.extract_lines(pgm_path, language)?;
    let raw_lines = output.lines.clone();
    let discarded = prune_impossible_geometry(&mut output.lines, language);
    if discarded > 0 {
        debug!(
            "{} geometry pruning: discarded {} impossible OCR boxes for subtitle stream {} packet {} rect {}",
            ppocr_engine_label(ocr_engine),
            discarded,
            params.stream_index,
            params.packet_seq,
            bitmap.index
        );
    }
    let raw_quality = ocr_text_quality_score(&lines_text_for_quality(&output.lines), language);
    let raw_confidence = ppocr_average_confidence(&output.lines).unwrap_or(0.0);
    quality_baseline.observe(raw_quality, raw_confidence, subtitle_start_ms);
    let force_tesseract_non_english = force_tesseract_non_english_enabled()
        && !is_english_language(language)
        && language_uses_spaces(language);
    let mut tesseract_attempt = None;

    let mut restored_spacing = false;
    // Forced Tesseract replaces the text, so do not transfer its spacing.
    if tesseract_fallback_enabled
        && !force_tesseract_non_english
        && language_uses_spaces(language)
        && (ppocr_spacing_needs_fallback(&output.lines)
            || postprocessed_text_needs_quality_fallback(&output, language))
    {
        if let Some(restored) =
            restore_spacing_from_tesseract(&mut output, pgm_path, language, &mut tesseract_attempt)
        {
            restored_spacing = restored;
            info!(
                "{} spacing recovery: restored word boundaries using matching Tesseract text for subtitle stream {} packet {} rect {}",
                ppocr_engine_label(ocr_engine), params.stream_index, params.packet_seq, bitmap.index
            );
        }
    }

    // Both checks intentionally use the output before word segmentation.
    let thresholds = quality_fallback_thresholds(quality_baseline);
    let quality_fallback_requested = ppocr_needs_quality_fallback(&output.lines, language);
    let preserve_single_token =
        output.lines.len() == 1 && ambiguous_short_single_token(&output.lines[0].text);
    if !restored_spacing
        && !preserve_single_token
        && ppocr_spacing_needs_fallback(&output.lines)
        && output_needs_spacing_after_postprocess(&output, language)
        && word_segmentation_enabled()
    {
        let raw_postprocess_quality = output_quality_after_postprocess(&output, language);
        if let Some(segmented) = try_ppocr_word_segmentation_recovery(
            engine,
            bitmap.pgm,
            pgm_path,
            language,
            bitmap.color,
            &output,
        )? {
            let segmented_quality = output_quality_after_postprocess(&segmented, language);
            if segmented_quality > raw_postprocess_quality + 0.03 {
                info!(
                    "{} spacing recovery: split subtitle stream {} packet {} rect {} into {} PP-OCR word crops (segmented_score={:.2}, ppocr_score={:.2})",
                    ppocr_engine_label(ocr_engine),
                    params.stream_index,
                    params.packet_seq,
                    bitmap.index,
                    segmented.lines.len(),
                    segmented_quality,
                    raw_postprocess_quality
                );
                output = segmented;
            }
        }
    }

    let spacing_fallback_requested = output_needs_spacing_after_postprocess(&output, language);
    let scores = PpOcrScores::of(&output, language);
    // Severe PP-OCR glue can be consistent for an entire subtitle track, so a
    // dynamic baseline trained on the same bad output will not catch it. Try
    // Tesseract immediately when PP-OCR word segmentation cannot recover spaces
    // for a space-using language.
    let should_try_quality_fallback = spacing_fallback_requested
        || quality_fallback_requested
        || postprocessed_text_needs_quality_fallback(&output, language)
        || thresholds.is_some_and(|thresholds| {
            scores.quality < thresholds.quality || scores.confidence < thresholds.confidence
        });
    let replaced_with_tesseract = !restored_spacing
        && tesseract_fallback_enabled
        && language_uses_spaces(language)
        && (force_tesseract_non_english
            || (should_try_quality_fallback && scores.confidence < 0.30))
        && try_tesseract_quality_fallback(
            &mut output,
            bitmap,
            params,
            force_tesseract_non_english,
            spacing_fallback_requested,
            &scores,
            &mut tesseract_attempt,
        );

    let training_output = training_export_requested().then(|| OcrOutput {
        lines: output.lines.clone(),
        seam_corrected: output.seam_corrected,
    });
    let mut rect_lines = Vec::new();
    for mut line in output.lines {
        if let Some(bbox) = line.bbox.as_mut() {
            offset_bbox(bbox, bitmap.rect.x, bitmap.rect.y);
        }
        if !restored_spacing {
            line.text = postprocess_ocr_text(&line.text, language);
        }
        if line.color.is_none() {
            line.color = bitmap.color;
        }
        if !line.text.is_empty() {
            rect_lines.push(line);
        }
    }
    let recovered_punctuation = tesseract_fallback_enabled
        && !replaced_with_tesseract
        && !force_tesseract_non_english
        && recover_detector_punctuation(
            &mut rect_lines,
            &raw_lines,
            pgm_path,
            params,
            &mut tesseract_attempt,
        );
    export_training_sample_unless_recovered(
        training_output.as_ref(),
        recovered_punctuation,
        |output| export_ocr_training_sample(pgm_path, output, language),
    )?;
    Ok(rect_lines)
}

/// Copies word boundaries from a Tesseract reading that agrees with PP-OCR's
/// glyphs. Returns `None` when nothing was transferred, otherwise whether the
/// result no longer needs a spacing fallback.
fn restore_spacing_from_tesseract(
    output: &mut OcrOutput,
    pgm_path: &Path,
    language: &str,
    tesseract_attempt: &mut Option<Result<TesseractCandidate>>,
) -> Option<bool> {
    let fallback_language = resolve_tesseract_fallback_language(language)?;
    let candidate = tesseract_attempt
        .get_or_insert_with(|| run_tesseract_best_effort(pgm_path, &fallback_language))
        .as_ref()
        .ok()?;
    let mut spacing_lines = output.lines.clone();
    if spacing_lines.len() > 1 {
        if let Ok(alternate) = run_tesseract_with_psm(pgm_path, &fallback_language, 11) {
            recover_line_terminal_punctuation(&mut spacing_lines, &candidate.text, &alternate);
        }
    }
    let transferred = transfer_agreed_spacing(&spacing_lines, &candidate.text)?;
    output.lines = apply_transferred_spacing(&spacing_lines, &transferred, language);
    Some(!output_needs_spacing_after_postprocess(output, language))
}

/// PP-OCR text and scores that a Tesseract candidate must beat.
struct PpOcrScores {
    text: String,
    quality: f32,
    postprocessed_text: String,
    postprocessed_quality: f32,
    confidence: f32,
}

impl PpOcrScores {
    fn of(output: &OcrOutput, language: &str) -> Self {
        let text = lines_text_for_quality(&output.lines);
        let postprocessed_text = postprocess_ocr_text(&text, language);
        Self {
            quality: ocr_text_quality_score(&text, language),
            postprocessed_quality: ocr_text_quality_score(&postprocessed_text, language),
            confidence: ppocr_average_confidence(&output.lines).unwrap_or(0.0),
            text,
            postprocessed_text,
        }
    }
}

/// Replaces low-confidence PP-OCR output with Tesseract when it is clearly
/// better, or unconditionally when non-English Tesseract is forced. Returns
/// whether the output was replaced.
fn try_tesseract_quality_fallback(
    output: &mut OcrOutput,
    bitmap: &BitmapRect<'_>,
    params: &CueBuildParams<'_>,
    force_tesseract_non_english: bool,
    spacing_fallback_requested: bool,
    ppocr: &PpOcrScores,
    tesseract_attempt: &mut Option<Result<TesseractCandidate>>,
) -> bool {
    let engine_label = ppocr_engine_label(params.ocr_engine);
    let Some(fallback_language) = resolve_tesseract_fallback_language(params.language) else {
        warn!(
            "{} quality fallback skipped (no Tesseract languages available) for subtitle stream {} packet {} rect {}",
            engine_label, params.stream_index, params.packet_seq, bitmap.index
        );
        return false;
    };
    let candidate = match tesseract_attempt
        .get_or_insert_with(|| run_tesseract_best_effort(bitmap.pgm_path, &fallback_language))
    {
        Ok(candidate) if !candidate.text.is_empty() => candidate,
        Ok(_) => return false,
        Err(err) => {
            warn!(
                "{} quality fallback failed for subtitle stream {} packet {} rect {} (lang={}): {}",
                engine_label,
                params.stream_index,
                params.packet_seq,
                bitmap.index,
                fallback_language,
                err
            );
            return false;
        }
    };
    // PP-OCR has language/profile routing for English, Latin, CJK, Japanese,
    // Korean, and multilingual recognizers. Keep Tesseract as a low-confidence
    // rescue path instead of replacing high-confidence PP-OCR output by language.
    let postprocess_repaired_spacing = ppocr.postprocessed_text != ppocr.text
        && ppocr.postprocessed_text.split_whitespace().count() > 1;
    let should_replace = if force_tesseract_non_english {
        true
    } else if postprocess_repaired_spacing || spacing_fallback_requested {
        // For glued subtitle text, keep PP-OCR and let DPN's word
        // segmentation/post-processing repair spacing. VobSub outlines can make
        // Tesseract confidently emit worse text (e.g. "That was" -> "The wes"),
        // so do not replace on spacing failures alone.
        false
    } else {
        // Prefer Tesseract only when it beats PP-OCR after DPN's own post-processing.
        candidate.text.split_whitespace().count() > 1
            && ppocr.confidence < 0.30
            && candidate.quality
                >= ppocr.postprocessed_quality + tesseract_quality_fallback_min_gain()
    };
    if !should_replace {
        debug!(
            "{} quality fallback skipped (insufficient gain): keeping model output for subtitle stream {} packet {} rect {} (tess_score={:.2}, ppocr_score={:.2}, ppocr_conf={:.2})",
            engine_label,
            params.stream_index,
            params.packet_seq,
            bitmap.index,
            candidate.quality,
            ppocr.quality,
            ppocr.confidence
        );
        return false;
    }

    output.lines = vec![OcrLine {
        text: candidate.text.clone(),
        bbox: union_bbox(&output.lines),
        score: None,
        color: None,
        italic: false,
    }];
    let (kind, scope) = if force_tesseract_non_english {
        ("language", "non-English ")
    } else {
        ("quality", "")
    };
    info!(
        "{} {} fallback: using Tesseract({}) psm={} for {}subtitle stream {} packet {} rect {} (tess_score={:.2}, ppocr_score={:.2}, ppocr_conf={:.2})",
        engine_label,
        kind,
        fallback_language,
        candidate.psm,
        scope,
        params.stream_index,
        params.packet_seq,
        bitmap.index,
        candidate.quality,
        ppocr.quality,
        ppocr.confidence
    );
    true
}

fn union_bbox(lines: &[OcrLine]) -> Option<OcrBoundingBox> {
    lines
        .iter()
        .filter_map(|line| line.bbox.clone())
        .reduce(|a, b| OcrBoundingBox {
            left: a.left.min(b.left),
            top: a.top.min(b.top),
            right: a.right.max(b.right),
            bottom: a.bottom.max(b.bottom),
        })
}

/// Adds punctuation PP-OCR dropped when Tesseract corroborates every other
/// glyph. Returns whether a recovered mark makes the line unsuitable as a
/// training label.
fn recover_detector_punctuation(
    lines: &mut [OcrLine],
    raw_lines: &[OcrLine],
    pgm_path: &Path,
    params: &CueBuildParams<'_>,
    tesseract_attempt: &mut Option<Result<TesseractCandidate>>,
) -> bool {
    let language = params.language;
    let is_srt = matches!(params.ocr_format, OcrFormat::Srt);
    let recovered_line_comma = is_srt
        && is_english_language(language)
        && lines.len() == 2
        && lines[0]
            .text
            .trim_end()
            .chars()
            .last()
            .is_some_and(|ch| ch.is_ascii_alphanumeric())
        && raw_and_final_glyphs_match(raw_lines, lines)
        && recover_two_line_comma(lines, pgm_path, language);

    if !language_uses_spaces(language)
        || !needs_terminal_punctuation_candidate(lines)
        || (!is_srt && lines_text_for_quality(lines).trim_end().ends_with(".."))
    {
        return recovered_line_comma;
    }
    let Some(fallback_language) = resolve_tesseract_fallback_language(language) else {
        return recovered_line_comma;
    };
    let Ok(candidate) = tesseract_attempt
        .get_or_insert_with(|| run_tesseract_best_effort(pgm_path, &fallback_language))
    else {
        return recovered_line_comma;
    };
    let recovered_terminal = if missing_terminal_ellipsis_candidate(lines, &candidate.text) {
        is_srt && recover_terminal_ellipsis(lines, pgm_path, &fallback_language, candidate)
    } else if recover_terminal_punctuation(lines, &candidate.text) {
        true
    } else if quoted_candidate_preserves_glyphs(lines, &candidate.text) {
        let alternate_psm = if candidate.psm == 6 { 7 } else { 6 };
        run_tesseract_with_psm(pgm_path, &fallback_language, alternate_psm)
            .is_ok_and(|alternate| recover_balanced_quotes(lines, &candidate.text, &alternate))
    } else {
        is_srt
            && recover_ambiguous_terminal_period_with(
                raw_lines,
                lines,
                candidate,
                || run_tesseract_with_psm(pgm_path, &fallback_language, 6),
                |text| tesseract_terminal_period_evidence(pgm_path, &fallback_language, 6, text),
            )
    };
    recovered_line_comma || recovered_terminal
}

fn recover_two_line_comma(lines: &mut [OcrLine], pgm_path: &Path, language: &str) -> bool {
    let Some(language) = resolve_tesseract_fallback_language(language) else {
        return false;
    };
    let Ok(candidate) = run_tesseract_with_psm(pgm_path, &language, 6) else {
        return false;
    };
    if !first_line_comma_agrees(lines, &candidate) {
        return false;
    }
    let Ok(alternate) = run_tesseract_with_psm(pgm_path, &language, 11) else {
        return false;
    };
    recover_first_line_comma_with(lines, &candidate, &alternate, || {
        tesseract_line_final_comma_evidence(pgm_path, &language, &candidate)
    })
}

fn recover_terminal_ellipsis(
    lines: &mut [OcrLine],
    pgm_path: &Path,
    language: &str,
    candidate: &TesseractCandidate,
) -> bool {
    let Some(comma_boundaries) =
        tesseract_terminal_ellipsis_evidence(pgm_path, language, candidate.psm, &candidate.text)
    else {
        return false;
    };
    if !append_terminal_mark(lines, '.') {
        return false;
    }
    transfer_verified_comma_spacing(lines, &candidate.text, &comma_boundaries);
    true
}

fn recover_direct_line_punctuation_with(
    text: &mut String,
    image: &image::GrayImage,
    recognize: impl FnOnce() -> Option<(String, String)>,
) -> bool {
    if text.contains(['\n', '\r']) || text.chars().filter(|ch| ch.is_alphanumeric()).count() < 3 {
        return false;
    }
    let proposal = if text.ends_with("..") && !text.ends_with("...") {
        if !multi_threshold_exact_three_terminal_dots(image) {
            return false;
        }
        format!("{text}.")
    } else if text.ends_with('.') && !text.ends_with("..") {
        if !multi_threshold_terminal_comma_shape(image) {
            return false;
        }
        format!("{},", &text[..text.len() - 1])
    } else {
        return false;
    };
    let Some((psm6, psm7)) = recognize() else {
        return false;
    };
    let proposed_glyphs = without_whitespace(&proposal);
    if [psm6, psm7].iter().any(|candidate| {
        candidate.contains(['\n', '\r']) || without_whitespace(candidate) != proposed_glyphs
    }) {
        return false;
    }
    *text = proposal;
    true
}

fn direct_punctuation_tesseract_attempts(
    work_image_path: &Path,
    pgm: &[u8],
    language: &str,
) -> Option<(String, String)> {
    direct_punctuation_tesseract_attempts_with(
        work_image_path,
        pgm,
        language,
        resolve_tesseract_fallback_language,
        run_tesseract_with_psm,
    )
}

fn direct_punctuation_tesseract_attempts_with(
    work_image_path: &Path,
    pgm: &[u8],
    language: &str,
    resolve_language: impl FnOnce(&str) -> Option<String>,
    mut recognize: impl FnMut(&Path, &str, u8) -> Result<String>,
) -> Option<(String, String)> {
    use std::io::Write;
    use std::sync::atomic::AtomicU64;

    static NEXT_CROP: AtomicU64 = AtomicU64::new(0);
    let language = resolve_language(language)?;
    for _ in 0..8 {
        let sequence = NEXT_CROP.fetch_add(1, Ordering::Relaxed);
        let path =
            work_image_path.with_extension(format!("direct-{}-{sequence}.pgm", std::process::id()));
        let Ok(mut file) = fs::OpenOptions::new()
            .write(true)
            .create_new(true)
            .open(&path)
        else {
            continue;
        };
        let written = file.write_all(pgm).is_ok();
        drop(file);
        let result = if written {
            recognize(&path, &language, 6)
                .ok()
                .zip(recognize(&path, &language, 7).ok())
        } else {
            None
        };
        if !written || !keep_ocr_intermediates() {
            let _ = fs::remove_file(&path);
        }
        return result;
    }
    None
}

fn export_training_sample_unless_recovered(
    output: Option<&OcrOutput>,
    recovered_punctuation: bool,
    export: impl FnOnce(&OcrOutput) -> Result<()>,
) -> Result<()> {
    match output {
        Some(output) if !recovered_punctuation && !output.seam_corrected => export(output),
        _ => Ok(()),
    }
}

fn export_ocr_training_sample(pgm_path: &Path, output: &OcrOutput, language: &str) -> Result<()> {
    let Some(manifest_path) = env::var_os("DPN_OCR_TRAINING_MANIFEST").map(PathBuf::from) else {
        return Ok(());
    };
    let image_dir = env::var_os("DPN_OCR_TRAINING_IMAGE_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            manifest_path
                .parent()
                .map(|parent| parent.join("images"))
                .unwrap_or_else(|| PathBuf::from("images"))
        });
    fs::create_dir_all(&image_dir)
        .with_context(|| format!("creating OCR training image dir '{}'", image_dir.display()))?;
    if let Some(parent) = manifest_path.parent() {
        fs::create_dir_all(parent).with_context(|| {
            format!("creating OCR training manifest dir '{}'", parent.display())
        })?;
    }

    let pgm = fs::read(pgm_path)
        .with_context(|| format!("reading OCR training image '{}'", pgm_path.display()))?;
    let mode = env::var("DPN_OCR_TRAINING_SAMPLE_MODE")
        .unwrap_or_else(|_| "line".to_string())
        .trim()
        .to_ascii_lowercase();
    let mut samples = Vec::new();
    if matches!(mode.as_str(), "line" | "lines" | "both") {
        samples.extend(exportable_line_training_samples(&pgm, output, language));
    }
    if samples.is_empty() || mode == "rect" || mode == "both" {
        let processed = postprocessed_line_texts(output, language).join(" ");
        if is_exportable_training_label(&processed, language) {
            samples.push((processed, pgm.clone(), "rect".to_string()));
        }
    }
    if samples.is_empty() {
        return Ok(());
    }

    let mut manifest = fs::OpenOptions::new()
        .create(true)
        .append(true)
        .open(&manifest_path)
        .with_context(|| {
            format!(
                "opening OCR training manifest '{}'",
                manifest_path.display()
            )
        })?;
    for (label, image, suffix) in samples {
        write_ocr_training_sample(
            pgm_path,
            &manifest_path,
            &image_dir,
            &mut manifest,
            &label,
            &image,
            &suffix,
        )?;
    }
    Ok(())
}

fn exportable_line_training_samples(
    pgm: &[u8],
    output: &OcrOutput,
    language: &str,
) -> Vec<(String, Vec<u8>, String)> {
    let Some((width, height, header_len)) = parse_pgm_header(pgm) else {
        return Vec::new();
    };
    let pixels = &pgm[header_len..];
    if pixels.len() < width * height {
        return Vec::new();
    }
    output
        .lines
        .iter()
        .enumerate()
        .filter_map(|(index, line)| {
            let label = postprocess_ocr_text(&line.text, language)
                .trim()
                .to_string();
            if !is_exportable_training_label(&label, language) {
                return None;
            }
            let image = line
                .bbox
                .as_ref()
                .and_then(|bbox| crop_pgm_to_bbox(pixels, width, height, bbox))
                .unwrap_or_else(|| pgm.to_vec());
            Some((label, image, format!("line{index}")))
        })
        .collect()
}

fn is_exportable_training_label(text: &str, language: &str) -> bool {
    let processed = text.trim();
    if processed.is_empty() || ocr_text_quality_score(processed, language) < 0.72 {
        return false;
    }
    // Reject labels that still contain a glued run of words.
    !ascii_alpha_tokens(processed)
        .any(|token| token.chars().filter(|ch| ch.is_ascii_alphabetic()).count() >= 14)
}

fn write_ocr_training_sample(
    pgm_path: &Path,
    manifest_path: &Path,
    image_dir: &Path,
    manifest: &mut fs::File,
    label: &str,
    image: &[u8],
    suffix: &str,
) -> Result<()> {
    let stem = pgm_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("ocr");
    let label = label.replace(['\n', '\t'], " ");
    let filename = format!("{}-{}-{}.pgm", stem, suffix, stable_label_hash(&label));
    let out_path = image_dir.join(&filename);
    fs::write(&out_path, image)
        .with_context(|| format!("writing OCR training image '{}'", out_path.display()))?;
    let rel_path = out_path
        .strip_prefix(manifest_path.parent().unwrap_or_else(|| Path::new(".")))
        .unwrap_or(&out_path)
        .to_string_lossy()
        .replace('\\', "/");
    let line = format!("{rel_path}\t{label}\n");
    use std::io::Write as _;
    manifest.write_all(line.as_bytes()).with_context(|| {
        format!(
            "writing OCR training manifest '{}'",
            manifest_path.display()
        )
    })?;
    Ok(())
}

fn parse_pgm_header(pgm: &[u8]) -> Option<(usize, usize, usize)> {
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
        tokens.push(std::str::from_utf8(&pgm[start..i]).ok()?.parse().ok()?);
    }
    if i < pgm.len() && pgm[i].is_ascii_whitespace() {
        i += 1;
    }
    if tokens.len() != 3 || tokens[2] != 255 {
        return None;
    }
    Some((tokens[0], tokens[1], i))
}

fn crop_pgm_to_bbox(
    pixels: &[u8],
    width: usize,
    height: usize,
    bbox: &OcrBoundingBox,
) -> Option<Vec<u8>> {
    let pad_x = 3usize;
    let pad_y = 2usize;
    let left = bbox.left.max(0) as usize;
    let top = bbox.top.max(0) as usize;
    let right = bbox.right.max(0) as usize;
    let bottom = bbox.bottom.max(0) as usize;
    if right <= left || bottom <= top || left >= width || top >= height {
        return None;
    }
    let left = left.saturating_sub(pad_x);
    let top = top.saturating_sub(pad_y);
    let right = (right + pad_x).min(width);
    let bottom = (bottom + pad_y).min(height);
    if right <= left || bottom <= top {
        return None;
    }
    let crop_w = right - left;
    let crop_h = bottom - top;
    let mut out = format!("P5\n{crop_w} {crop_h}\n255\n").into_bytes();
    for y in top..bottom {
        let row = y * width;
        out.extend_from_slice(&pixels[row + left..row + right]);
    }
    Some(out)
}

fn stable_label_hash(text: &str) -> u64 {
    let mut hash = 0xcbf29ce484222325u64;
    for byte in text.as_bytes() {
        hash ^= *byte as u64;
        hash = hash.wrapping_mul(0x100000001b3);
    }
    hash
}

fn keep_ocr_intermediates() -> bool {
    env_flag_enabled("DPN_OCR_KEEP_INTERMEDIATES")
}

/// Text long enough to trust a recognizer's terminal mark: it ends in a glyph
/// with no mark, or in a two-dot ellipsis that may have lost its third dot.
fn needs_terminal_punctuation_candidate(lines: &[OcrLine]) -> bool {
    let text = lines_text_for_quality(lines);
    let len = text.chars().filter(|ch| !ch.is_whitespace()).count();
    let text = text.trim_end();
    len >= 8 && ends_with_alphanumeric(text)
        || len >= 3 && text.ends_with("..") && !text.ends_with("...")
}

// Only add a terminal mark when both recognizers agree on every other glyph.
// In particular, never transfer punctuation across a conflicting apostrophe,
// quote, or other pre-existing punctuation mark.
fn recover_terminal_punctuation(lines: &mut [OcrLine], candidate: &str) -> bool {
    let text = lines_text_for_quality(lines);
    if !needs_terminal_punctuation_candidate(lines) || !ends_with_alphanumeric(&text) {
        return false;
    }
    let mut proposed = without_whitespace(candidate);
    let Some(mark @ ('.' | '?' | '!')) = proposed.pop() else {
        return false;
    };
    if without_whitespace(&text) != proposed {
        return false;
    }
    append_terminal_mark(lines, mark)
}

fn recover_ambiguous_terminal_period_with(
    raw_lines: &[OcrLine],
    final_lines: &mut [OcrLine],
    candidate: &TesseractCandidate,
    alternate: impl FnOnce() -> Result<String>,
    pixel_evidence: impl FnOnce(&str) -> bool,
) -> bool {
    if !ambiguous_terminal_period_input_valid(raw_lines)
        || !ambiguous_terminal_period_input_valid(final_lines)
        || !raw_and_final_glyphs_match(raw_lines, final_lines)
    {
        return false;
    }
    let text = if candidate.psm == 6 {
        candidate.text.clone()
    } else if let Ok(text) = alternate() {
        text
    } else {
        return false;
    };
    if !ambiguous_terminal_period_raw_candidate(raw_lines, &text)
        || !ambiguous_terminal_period_candidate(final_lines, &text)
        || !pixel_evidence(&text)
    {
        return false;
    }
    append_terminal_mark(final_lines, '.')
}

fn raw_and_final_glyphs_match(raw: &[OcrLine], final_lines: &[OcrLine]) -> bool {
    let glyphs = |lines: &[OcrLine]| {
        lines
            .iter()
            .map(|line| line.text.trim().replace(' ', ""))
            .collect::<Vec<_>>()
            .join("\n")
    };
    glyphs(raw) == glyphs(final_lines)
}

// PP-OCR can emit a line without spaces before spacing recovery. Compare its
// glyphs and line boundaries, then separately check the final output's spacing.
fn ambiguous_terminal_period_raw_candidate(lines: &[OcrLine], candidate: &str) -> bool {
    let compact_lines: Vec<_> = lines
        .iter()
        .map(|line| {
            let mut compact = line.clone();
            compact.text.retain(|ch| ch != ' ');
            compact
        })
        .collect();
    ambiguous_terminal_period_candidate(&compact_lines, &candidate.replace(' ', ""))
}

fn ambiguous_terminal_period_input_valid(lines: &[OcrLine]) -> bool {
    needs_terminal_punctuation_candidate(lines)
        && ends_with_alphanumeric(&lines_text_for_quality(lines))
        && lines.iter().any(|line| line.text.contains(['I', '|']))
        && !lines
            .iter()
            .any(|line| line.text.trim().is_empty() || has_irregular_line_breaks(&line.text))
}

/// Carriage returns, or leading, trailing, or blank lines.
fn has_irregular_line_breaks(text: &str) -> bool {
    text.contains('\r')
        || text.starts_with('\n')
        || text.ends_with('\n')
        || text.lines().any(|line| line.trim().is_empty())
}

// Permit only a single I/| disagreement, and the space Tesseract can insert
// immediately after its pipe. Never transfer the glyph or its spacing.
fn ambiguous_terminal_period_candidate(lines: &[OcrLine], candidate: &str) -> bool {
    if !ambiguous_terminal_period_input_valid(lines) || has_irregular_line_breaks(candidate) {
        return false;
    }
    // Valid input has no blank lines, so this keeps every line boundary.
    let original = joined_line_text(lines);
    let Some(proposed) = candidate.trim().strip_suffix('.') else {
        return false;
    };
    if !original.chars().last().is_some_and(char::is_alphanumeric) {
        return false;
    }
    let mut original = original.chars().peekable();
    let mut proposed = proposed.chars().peekable();
    let mut mismatch = false;
    loop {
        match (original.next(), proposed.next()) {
            (None, None) => return mismatch,
            (Some(a), Some(b)) if a == b => {}
            (Some(a), Some(b)) if !mismatch && matches!((a, b), ('I', '|') | ('|', 'I')) => {
                mismatch = true;
                if b == '|'
                    && original.peek().is_some_and(|c| !c.is_whitespace())
                    && proposed.peek() == Some(&' ')
                {
                    proposed.next();
                }
            }
            _ => return false,
        }
    }
}

fn missing_terminal_ellipsis_candidate(lines: &[OcrLine], candidate: &str) -> bool {
    let original = compact_lines(&joined_line_text(lines)).join("\n");
    let proposed = compact_lines(candidate).join("\n");
    original.ends_with("..")
        && !original.ends_with("...")
        && proposed.strip_suffix('.') == Some(original.as_str())
}

fn transfer_verified_comma_spacing(
    lines: &mut [OcrLine],
    candidate: &str,
    supported: &HashSet<usize>,
) -> bool {
    if compact_lines(&joined_line_text(lines)) != compact_lines(candidate) {
        return false;
    }
    let mut comma_boundaries = HashSet::new();
    let mut index = 0usize;
    for line in candidate.lines() {
        let mut previous = None;
        let mut whitespace = false;
        for ch in line.chars() {
            if ch.is_whitespace() {
                whitespace = true;
            } else {
                if whitespace
                    && previous == Some(',')
                    && ch.is_alphabetic()
                    && supported.contains(&index)
                {
                    comma_boundaries.insert(index);
                }
                index += 1;
                previous = Some(ch);
                whitespace = false;
            }
        }
    }
    let mut changed = false;
    let mut index = 0usize;
    for line in lines {
        let mut result = String::with_capacity(line.text.len());
        let mut previous = None;
        for ch in line.text.chars() {
            if !ch.is_whitespace() {
                if comma_boundaries.contains(&index) && previous == Some(',') {
                    result.push(' ');
                    changed = true;
                }
                index += 1;
            }
            result.push(ch);
            previous = Some(ch);
        }
        line.text = result;
    }
    changed
}

fn append_terminal_mark(lines: &mut [OcrLine], mark: char) -> bool {
    let Some(line) = lines
        .iter_mut()
        .rev()
        .find(|line| !line.text.trim().is_empty())
    else {
        return false;
    };
    insert_before_trailing_whitespace(&mut line.text, mark);
    true
}

fn first_line_comma_agrees(lines: &[OcrLine], candidate: &str) -> bool {
    if lines.len() != 2 || lines.iter().any(|line| line.text.contains(['\n', '\r'])) {
        return false;
    }
    let candidate = non_blank_lines(candidate);
    if candidate.len() != 2 {
        return false;
    }
    let first = without_whitespace(&lines[0].text);
    let second = without_whitespace(&lines[1].text);
    first.len() >= 4
        && !second.is_empty()
        && first
            .chars()
            .last()
            .is_some_and(|ch| ch.is_ascii_alphanumeric())
        && without_whitespace(candidate[0]).strip_suffix(',') == Some(first.as_str())
        && without_whitespace(candidate[1]) == second
}

fn recover_first_line_comma_with(
    lines: &mut [OcrLine],
    candidate: &str,
    alternate: &str,
    evidence: impl FnOnce() -> bool,
) -> bool {
    if !first_line_comma_agrees(lines, candidate)
        || !first_line_comma_agrees(lines, alternate)
        || !evidence()
    {
        return false;
    }
    insert_before_trailing_whitespace(&mut lines[0].text, ',');
    true
}

// Recover only a missing line-final mark when two independent page segmentation
// modes agree on all glyphs for every line. Never move a line or replace a glyph.
fn recover_line_terminal_punctuation(
    lines: &mut [OcrLine],
    candidate: &str,
    alternate: &str,
) -> bool {
    let candidates = non_blank_lines(candidate);
    let alternates = non_blank_lines(alternate);
    if lines.len() < 2 || candidates.len() != lines.len() || alternates.len() != lines.len() {
        return false;
    }
    let mut marks = Vec::with_capacity(lines.len());
    for ((line, candidate), alternate) in lines.iter().zip(candidates).zip(alternates) {
        let original = without_whitespace(&line.text);
        let proposed = without_whitespace(candidate);
        if proposed != without_whitespace(alternate) || original.is_empty() {
            return false;
        }
        if original == proposed {
            marks.push(None);
        } else if let Some(mark @ ('.' | '?' | '!')) = proposed.chars().last() {
            if proposed.strip_suffix(mark) != Some(original.as_str())
                || !original.chars().last().is_some_and(char::is_alphanumeric)
            {
                return false;
            }
            marks.push(Some(mark));
        } else {
            return false;
        }
    }
    let mut changed = false;
    for (line, mark) in lines.iter_mut().zip(marks) {
        if let Some(mark) = mark {
            insert_before_trailing_whitespace(&mut line.text, mark);
            changed = true;
        }
    }
    changed
}

fn quoted_candidate_preserves_glyphs(lines: &[OcrLine], candidate: &str) -> bool {
    if lines.len() != 1 || lines[0].text.contains(['"', '\n', '\r']) {
        return false;
    }
    let original = without_whitespace(&lines[0].text);
    let proposed = without_whitespace(candidate);
    original.chars().count() >= 8
        && proposed.strip_prefix('"').and_then(|s| s.strip_suffix('"')) == Some(original.as_str())
}

// A second page segmentation mode must independently see the same enclosing
// quotes; neither recognizer may change, delete, or insert any other glyph.
fn recover_balanced_quotes(lines: &mut [OcrLine], candidate: &str, alternate: &str) -> bool {
    if !quoted_candidate_preserves_glyphs(lines, candidate)
        || !quoted_candidate_preserves_glyphs(lines, alternate)
    {
        return false;
    }
    let line = &mut lines[0].text;
    let start = line.len() - line.trim_start().len();
    let end = line.trim_end().len();
    line.insert(end, '"');
    line.insert(start, '"');
    true
}

fn crop_recovery_preserves_characters(original: &OcrOutput, candidate: &OcrOutput) -> bool {
    let original_text = lines_text_for_quality(&original.lines);
    let candidate_text = lines_text_for_quality(&candidate.lines);
    let (original_chars, original_boundaries) = text_and_boundaries(&original_text);
    let (candidate_chars, candidate_boundaries) = text_and_boundaries(&candidate_text);
    !original_chars.is_empty()
        && original_chars == candidate_chars
        && original_boundaries.is_subset(&candidate_boundaries)
        && line_break_positions(original) == line_break_positions(candidate)
}

fn line_break_positions(output: &OcrOutput) -> Vec<usize> {
    let mut positions = Vec::new();
    let mut count = 0;
    for ch in joined_line_text(&output.lines).chars() {
        if ch == '\n' {
            positions.push(count);
        } else if !ch.is_whitespace() {
            count += 1;
        }
    }
    positions
}

fn transfer_agreed_spacing(lines: &[OcrLine], candidate: &str) -> Option<String> {
    let original = joined_line_text(lines);
    // A short, already recognized single word can also be a tightly spaced
    // phrase. A second recognizer's spaces do not distinguish those cases.
    if ambiguous_short_single_token(&original) {
        return None;
    }
    let (original_chars, original_boundaries) = text_and_boundaries(&original);
    let (candidate_chars, candidate_boundaries) = text_and_boundaries(candidate);
    let left: Vec<char> = original_chars.chars().collect();
    let right: Vec<char> = candidate_chars.chars().collect();
    if left.is_empty() {
        return None;
    }
    let compatible = |a: char, b: char| a == b || matches!((a, b), ('I', '|') | ('l', '|'));
    if !spacing_candidate_preserves_characters(&original, candidate)
        && (left.len() != right.len()
            || !original_boundaries.is_subset(&candidate_boundaries)
            || !left.iter().zip(&right).all(|(&a, &b)| compatible(a, b)))
    {
        return transfer_anchored_spacing(&original, candidate);
    }
    let added: HashSet<_> = candidate_boundaries
        .difference(&original_boundaries)
        .copied()
        .collect();
    if added.is_empty()
        || added.iter().any(|&i| {
            i == 0 || i >= left.len() || (left[i - 1] != right[i - 1] && left[i] != right[i])
        })
    {
        return None;
    }
    Some(insert_word_boundaries(&original, &added, &mut 0))
}

fn apply_transferred_spacing(lines: &[OcrLine], transferred: &str, language: &str) -> Vec<OcrLine> {
    let (_, original_boundaries) = text_and_boundaries(&joined_line_text(lines));
    let (_, transferred_boundaries) = text_and_boundaries(transferred);
    let added: HashSet<_> = transferred_boundaries
        .difference(&original_boundaries)
        .copied()
        .collect();
    let mut index = 0;
    lines
        .iter()
        .cloned()
        .map(|mut line| {
            let repaired = insert_word_boundaries(&line.text, &added, &mut index);
            let has_glued_words = repaired.split_whitespace().any(|word| {
                word.chars()
                    .filter(|ch| ch.is_alphabetic() || *ch == '\'')
                    .count()
                    >= 9
                    && word.chars().any(|ch| ch == '\'')
                    || word.chars().filter(|ch| ch.is_alphabetic()).count() >= 12
            });
            let processed = postprocess_ocr_text(&repaired, language);
            if has_glued_words
                && !repaired.contains(['\n', '\r'])
                && processed
                    .chars()
                    .filter(|ch| !ch.is_whitespace())
                    .eq(repaired.chars().filter(|ch| !ch.is_whitespace()))
                && preserves_existing_boundaries(&repaired, &processed)
            {
                line.text = processed;
            } else {
                line.text = repaired;
            }
            line
        })
        .collect()
}

// Use only uniquely matching character runs around each proposed boundary. The
// whole-text distance guard prevents an unrelated candidate from contributing a
// coincidental local match; neither substitutions nor punctuation are copied.
fn transfer_anchored_spacing(original: &str, candidate: &str) -> Option<String> {
    const ANCHOR: usize = 4;
    const MAX_CHARS: usize = 512;
    let (original_chars, original_boundaries) = text_and_boundaries(original);
    let (candidate_chars, candidate_boundaries) = text_and_boundaries(candidate);
    let left: Vec<char> = original_chars.chars().collect();
    let right: Vec<char> = candidate_chars.chars().collect();
    if left.len() < ANCHOR * 2 || left.len() > MAX_CHARS || right.len() > MAX_CHARS {
        return None;
    }

    let mut previous: Vec<usize> = (0..=right.len()).collect();
    for (i, a) in left.iter().enumerate() {
        let mut current = vec![i + 1; right.len() + 1];
        for (j, b) in right.iter().enumerate() {
            current[j + 1] = (previous[j + 1] + 1)
                .min(current[j] + 1)
                .min(previous[j] + usize::from(a != b));
        }
        previous = current;
    }
    if previous[right.len()] > (left.len().max(right.len()) / 10).max(2) {
        return None;
    }

    let mut added = HashSet::new();
    for boundary in candidate_boundaries {
        if boundary < ANCHOR || boundary + ANCHOR > right.len() {
            continue;
        }
        let anchor = &right[boundary - ANCHOR..boundary + ANCHOR];
        if !anchor[ANCHOR - 1].is_alphabetic() || !anchor[ANCHOR].is_alphabetic() {
            continue;
        }
        if right
            .windows(anchor.len())
            .filter(|window| *window == anchor)
            .count()
            != 1
        {
            continue;
        }
        let mut matches = left
            .windows(anchor.len())
            .enumerate()
            .filter_map(|(i, window)| (window == anchor).then_some(i + ANCHOR));
        if let (Some(index), None) = (matches.next(), matches.next()) {
            if !original_boundaries.contains(&index) {
                added.insert(index);
            }
        }
    }
    if added.is_empty() {
        return None;
    }
    Some(insert_word_boundaries(original, &added, &mut 0))
}

pub(super) fn spacing_candidate_preserves_characters(original: &str, candidate: &str) -> bool {
    let (original_chars, original_boundaries) = text_and_boundaries(original);
    let (candidate_chars, candidate_boundaries) = text_and_boundaries(candidate);
    !original_chars.is_empty()
        && original_chars == candidate_chars
        && original_boundaries.is_subset(&candidate_boundaries)
        && candidate_boundaries.len() > original_boundaries.len()
}

fn preserves_existing_boundaries(original: &str, candidate: &str) -> bool {
    let (_, original_boundaries) = text_and_boundaries(original);
    let (_, candidate_boundaries) = text_and_boundaries(candidate);
    original_boundaries.is_subset(&candidate_boundaries)
}

fn non_blank_lines(text: &str) -> Vec<&str> {
    text.lines()
        .filter(|line| !line.trim().is_empty())
        .collect()
}

fn without_whitespace(text: &str) -> String {
    text.chars().filter(|ch| !ch.is_whitespace()).collect()
}

fn compact_lines(text: &str) -> Vec<String> {
    text.lines().map(without_whitespace).collect()
}

/// Trimmed, non-empty line texts joined by newlines.
fn joined_line_text(lines: &[OcrLine]) -> String {
    lines
        .iter()
        .map(|line| line.text.trim())
        .filter(|text| !text.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

fn ends_with_alphanumeric(text: &str) -> bool {
    text.trim_end()
        .chars()
        .last()
        .is_some_and(char::is_alphanumeric)
}

fn insert_before_trailing_whitespace(text: &mut String, mark: char) {
    let end = text.trim_end().len();
    text.insert(end, mark);
}

/// Copies `text`, adding a space before each glyph whose index is in
/// `boundaries`. Glyph indices count non-whitespace characters starting at
/// `*index`, which is advanced so multi-line text can share one numbering.
fn insert_word_boundaries(text: &str, boundaries: &HashSet<usize>, index: &mut usize) -> String {
    let mut result = String::with_capacity(text.len() + boundaries.len());
    for ch in text.chars() {
        if !ch.is_whitespace() {
            if boundaries.contains(index) {
                result.push(' ');
            }
            *index += 1;
        }
        result.push(ch);
    }
    result
}

/// Whitespace-free glyphs of `text` and the glyph indices preceded by whitespace.
fn text_and_boundaries(text: &str) -> (String, HashSet<usize>) {
    let mut chars = String::new();
    let mut boundaries = HashSet::new();
    let mut count = 0;
    let mut whitespace = false;
    for ch in text.chars() {
        if ch.is_whitespace() {
            whitespace = count > 0;
        } else {
            if whitespace {
                boundaries.insert(count);
            }
            whitespace = false;
            chars.push(ch);
            count += 1;
        }
    }
    (chars, boundaries)
}

/// Post-processed, trimmed, non-empty text of each output line.
fn postprocessed_line_texts(output: &OcrOutput, language: &str) -> Vec<String> {
    output
        .lines
        .iter()
        .map(|line| {
            postprocess_ocr_text(&line.text, language)
                .trim()
                .to_string()
        })
        .filter(|text| !text.is_empty())
        .collect()
}

fn output_quality_after_postprocess(output: &OcrOutput, language: &str) -> f32 {
    let processed = postprocessed_line_texts(output, language).join(" ");
    ocr_text_quality_score(&processed, language)
}

fn output_needs_spacing_after_postprocess(output: &OcrOutput, language: &str) -> bool {
    let lines = output
        .lines
        .iter()
        .map(|line| OcrLine {
            text: postprocess_ocr_text(&line.text, language),
            bbox: line.bbox.clone(),
            ..*line
        })
        .collect::<Vec<_>>();
    ppocr_spacing_needs_fallback(&lines) || ppocr_needs_quality_fallback(&lines, language)
}

fn postprocessed_text_needs_quality_fallback(output: &OcrOutput, language: &str) -> bool {
    if !language_uses_spaces(language) {
        return false;
    }
    let processed = postprocessed_line_texts(output, language);
    processed.iter().any(|text| {
        let alpha_chars = text.chars().filter(|ch| ch.is_ascii_alphabetic()).count();
        let spaces = text.chars().filter(|ch| ch.is_whitespace()).count();
        let space_rate = spaces as f32 / text.len().max(1) as f32;
        let text_len = text.chars().count();
        let alpha_ratio = alpha_chars as f32 / text_len.max(1) as f32;
        let glued = alpha_chars >= 18 && space_rate < 0.06;
        let low_information_garbage = text_len <= 12 && alpha_chars <= 4 && alpha_ratio < 0.75;
        let suspicious_token = ascii_alpha_tokens(text).any(|token| {
            token.chars().filter(|ch| ch.is_ascii_alphabetic()).count() >= 12
                || token
                    .as_bytes()
                    .windows(2)
                    .any(|pair| pair[0].is_ascii_lowercase() && pair[1].is_ascii_uppercase())
        });
        glued
            || low_information_garbage
            || (text.split_whitespace().count() <= 2
                && ocr_text_quality_score(text, language) < 0.70)
            || suspicious_token
    })
}

/// Runs of ASCII letters and apostrophes, split at every other character.
fn ascii_alpha_tokens(text: &str) -> impl Iterator<Item = &str> {
    text.split(|ch: char| !ch.is_ascii_alphabetic() && ch != '\'')
        .filter(|part| !part.is_empty())
}

fn try_ppocr_word_segmentation_recovery(
    engine: &mut dyn SubtitleConverter,
    pgm: &[u8],
    pgm_path: &Path,
    language: &str,
    color: Option<(u8, u8, u8)>,
    original: &OcrOutput,
) -> Result<Option<OcrOutput>> {
    let candidate_gaps = [None, Some(5usize), Some(8usize)];
    let mut best: Option<(f32, OcrOutput)> = None;
    for gap in candidate_gaps {
        let Some(candidate) =
            recognize_ppocr_word_crops(engine, pgm, pgm_path, language, color, gap)?
        else {
            continue;
        };
        let quality = output_quality_after_postprocess(&candidate, language);
        consider_crop_candidate(&mut best, original, candidate, quality);
    }
    Ok(best.map(|(_, output)| output))
}

fn consider_crop_candidate(
    best: &mut Option<(f32, OcrOutput)>,
    original: &OcrOutput,
    mut candidate: OcrOutput,
    quality: f32,
) {
    if !crop_recovery_preserves_characters(original, &candidate) {
        return;
    }
    if best
        .as_ref()
        .map(|(best_quality, _)| quality > *best_quality + 0.001)
        .unwrap_or(true)
    {
        candidate.seam_corrected |= original.seam_corrected;
        *best = Some((quality, candidate));
    }
}

fn recognize_ppocr_word_crops(
    engine: &mut dyn SubtitleConverter,
    pgm: &[u8],
    pgm_path: &Path,
    language: &str,
    color: Option<(u8, u8, u8)>,
    gap_override: Option<usize>,
) -> Result<Option<OcrOutput>> {
    let crops = split_pgm_into_word_crops_with_gap(pgm, gap_override);
    if crops.len() < 2 || crops.len() > 80 {
        return Ok(None);
    }
    let mut lines = Vec::new();
    let mut seam_corrected = false;
    for (idx, (crop, bbox)) in crops.into_iter().enumerate() {
        let crop_path = pgm_path.with_file_name(format!(
            "{}-word-{}.pgm",
            pgm_path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("ocr"),
            idx
        ));
        fs::write(&crop_path, crop)
            .with_context(|| format!("writing OCR word crop {}", crop_path.display()))?;
        let word_output = engine.extract_lines(&crop_path, language)?;
        remove_intermediate(&crop_path);
        let text = normalize_utf8_text(&lines_text_for_quality(&word_output.lines));
        if text.is_empty() {
            continue;
        }
        seam_corrected |= word_output.seam_corrected;
        let mut bbox = bbox;
        let score = ppocr_average_confidence(&word_output.lines);
        if let Some(first_bbox) = word_output.lines.iter().find_map(|line| line.bbox.as_ref()) {
            // Keep the coarse crop position. Internal PP-OCR boxes are relative to the crop.
            bbox.top += first_bbox.top;
            bbox.bottom = bbox.top + (first_bbox.bottom - first_bbox.top).max(1);
        }
        lines.push(OcrLine {
            text,
            bbox: Some(bbox),
            score,
            color,
            italic: false,
        });
    }
    if lines.len() < 2 {
        return Ok(None);
    }
    Ok(Some(OcrOutput {
        lines: merge_ocr_word_crops(lines),
        seam_corrected,
    }))
}

pub(super) fn force_tesseract_non_english_enabled() -> bool {
    let enabled = env_flag_enabled("DPN_OCR_FORCE_TESS_NON_ENGLISH");
    if enabled && !FORCE_TESS_NON_ENGLISH_LOGGED.swap(true, Ordering::Relaxed) {
        warn!(
            "DPN_OCR_FORCE_TESS_NON_ENGLISH=1 set; non-English subtitle streams will prefer Tesseract over PP-OCR."
        );
    }
    enabled
}

pub(super) fn tesseract_quality_fallback_min_gain() -> f32 {
    match env::var("DPN_OCR_TESS_FALLBACK_MIN_GAIN") {
        Ok(v) => match v.trim().parse::<f32>() {
            Ok(x) if x.is_finite() && (0.0..=0.5).contains(&x) => x,
            _ => {
                warn!(
                    "Ignoring invalid DPN_OCR_TESS_FALLBACK_MIN_GAIN='{}'; using default 0.08",
                    v
                );
                0.08
            }
        },
        Err(_) => 0.08,
    }
}

pub(super) fn quality_fallback_thresholds(
    baseline: &OcrQualityBaseline,
) -> Option<OcrFallbackThresholds> {
    const BASELINE_MIN_SAMPLES: usize = 12;
    const RELATIVE_DROP: f32 = 0.15;

    if baseline.samples < BASELINE_MIN_SAMPLES {
        return None;
    }

    let dynamic_quality = baseline.avg_quality()?.mul_add(1.0 - RELATIVE_DROP, 0.0);
    let dynamic_confidence = baseline.avg_confidence()?.mul_add(1.0 - RELATIVE_DROP, 0.0);

    Some(OcrFallbackThresholds {
        quality: dynamic_quality.clamp(0.0, 1.0),
        confidence: dynamic_confidence.clamp(0.0, 1.0),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    struct NoOcr;

    impl SubtitleConverter for NoOcr {
        fn extract_lines(&mut self, _: &Path, _: &str) -> Result<OcrOutput> {
            panic!("text and clear events must not invoke OCR")
        }
    }

    fn line(text: &str) -> OcrLine {
        OcrLine {
            text: text.into(),
            bbox: None,
            score: None,
            color: None,
            italic: false,
        }
    }

    fn text_output(text: &str) -> OcrOutput {
        OcrOutput {
            lines: vec![line(text)],
            ..OcrOutput::default()
        }
    }

    /// A line with metadata that recovery passes must preserve.
    fn styled_line(text: &str) -> OcrLine {
        OcrLine {
            score: Some(0.9),
            italic: true,
            ..line(text)
        }
    }

    fn cue(start_ms: i64, end_ms: i64, text: &str) -> SubtitleCue {
        SubtitleCue {
            start_ms,
            end_ms,
            text: text.to_owned(),
        }
    }

    fn subtitle(pts_ms: Option<i64>, start_offset: u32, end_offset: u32) -> ffi::AVSubtitle {
        let mut sub: ffi::AVSubtitle = unsafe { std::mem::zeroed() };
        sub.pts = pts_ms.map_or(ffi::AV_NOPTS_VALUE, |pts| pts * 1_000);
        sub.start_display_time = start_offset;
        sub.end_display_time = end_offset;
        sub
    }

    fn cue_params<'a>(
        language: &'a str,
        stream_codec_id: ffi::AVCodecID,
        ocr_format: OcrFormat,
        ocr_engine: OcrEngine,
        work_dir: &'a Path,
    ) -> CueBuildParams<'a> {
        CueBuildParams {
            language,
            stream_index: 0,
            stream_codec_id,
            packet_seq: 0,
            work_dir,
            ocr_format,
            video_dimensions: None,
            ocr_engine,
        }
    }

    fn process_event(
        sub: &ffi::AVSubtitle,
        fallback_start: Option<i64>,
        fallback_duration: i64,
        codec: ffi::AVCodecID,
        format: OcrFormat,
        cues: &mut Vec<SubtitleCue>,
        timing: &mut PgsCueTiming,
    ) {
        let params = cue_params("eng", codec, format, OcrEngine::External, Path::new("."));
        append_subtitle_cues(
            sub,
            fallback_start,
            fallback_duration,
            &params,
            &mut NoOcr,
            &mut OcrQualityBaseline::default(),
            cues,
            timing,
        )
        .unwrap();
    }

    fn process_text_event(
        pts_ms: i64,
        end_offset: u32,
        text: &str,
        codec: ffi::AVCodecID,
        format: OcrFormat,
        cues: &mut Vec<SubtitleCue>,
        timing: &mut PgsCueTiming,
    ) {
        let text = CString::new(text).unwrap();
        let mut rect: ffi::AVSubtitleRect = unsafe { std::mem::zeroed() };
        rect.type_ = ffi::SUBTITLE_TEXT;
        rect.text = text.as_ptr().cast_mut();
        let mut rect_ptr = &mut rect as *mut _;
        let mut sub = subtitle(Some(pts_ms), 0, end_offset);
        sub.num_rects = 1;
        sub.rects = &mut rect_ptr;
        process_event(&sub, None, 0, codec, format, cues, timing);
    }

    fn rect_group(bounds: Option<(i64, i64)>, texts: &[&str]) -> SubtitleRectLines {
        SubtitleRectLines {
            vertical_bounds: bounds,
            lines: texts.iter().map(|text| line(text)).collect(),
        }
    }

    fn group_texts(groups: &[SubtitleRectLines]) -> Vec<Vec<&str>> {
        groups
            .iter()
            .map(|group| group.lines.iter().map(|line| line.text.as_str()).collect())
            .collect()
    }

    #[test]
    fn srt_rectangle_order_preserves_multiline_groups() {
        let mut groups = vec![
            rect_group(Some((100, 120)), &["lower first", "lower second"]),
            rect_group(Some((0, 20)), &["upper first", "upper second"]),
            rect_group(Some((50, 70)), &["middle"]),
        ];
        order_srt_rect_groups(&mut groups);
        assert_eq!(
            group_texts(&groups),
            [
                vec!["upper first", "upper second"],
                vec!["middle"],
                vec!["lower first", "lower second"],
            ]
        );
    }

    #[test]
    fn srt_rectangle_order_preserves_overlaps_and_unknown_geometry() {
        let mut groups = vec![
            rect_group(Some((12, 20)), &["overlap first"]),
            rect_group(Some((0, 15)), &["overlap second"]),
            rect_group(Some((0, 15)), &["same row"]),
            rect_group(Some((20, 30)), &["touching"]),
            rect_group(None, &["unknown"]),
            rect_group(Some((100, 110)), &["after lower"]),
            rect_group(Some((50, 60)), &["after upper"]),
        ];
        order_srt_rect_groups(&mut groups);
        assert_eq!(
            group_texts(&groups),
            [
                vec!["overlap first"],
                vec!["overlap second"],
                vec!["same row"],
                vec!["touching"],
                vec!["unknown"],
                vec!["after upper"],
                vec!["after lower"],
            ]
        );
    }

    #[test]
    fn srt_rectangle_order_preserves_transitive_overlap_for_every_input_order() {
        // A overlaps B and B overlaps C, even though A and C are disjoint.
        let intervals = [(0, 10), (8, 18), (16, 26)];
        for permutation in [
            [0, 1, 2],
            [0, 2, 1],
            [1, 0, 2],
            [1, 2, 0],
            [2, 0, 1],
            [2, 1, 0],
        ] {
            let labels = ["A", "B", "C"];
            let mut groups = vec![rect_group(Some((40, 50)), &["lower"])];
            groups.extend(
                permutation
                    .iter()
                    .map(|&i| rect_group(Some(intervals[i]), &[labels[i]])),
            );
            groups.push(rect_group(Some((-20, -10)), &["upper"]));
            order_srt_rect_groups(&mut groups);
            assert_eq!(groups[0].lines[0].text, "upper");
            assert_eq!(groups[4].lines[0].text, "lower");
            for (group, &index) in groups[1..4].iter().zip(&permutation) {
                assert_eq!(group.lines[0].text, labels[index]);
            }
        }
    }

    struct DirectBitmapMock {
        enabled: bool,
        responses: std::collections::VecDeque<Option<String>>,
        seen_sizes: Vec<(u32, u32)>,
        detector_calls: usize,
    }

    impl SubtitleConverter for DirectBitmapMock {
        fn extract_lines(&mut self, _: &Path, _: &str) -> Result<OcrOutput> {
            self.detector_calls += 1;
            Ok(OcrOutput {
                lines: vec![OcrLine {
                    text: "Stable text.".to_owned(),
                    bbox: None,
                    score: Some(1.0),
                    color: None,
                    italic: false,
                }],
                ..OcrOutput::default()
            })
        }

        fn supports_bitmap_line_recognition(&self, _: &str) -> bool {
            self.enabled
        }

        fn recognize_bitmap_line(
            &mut self,
            image: &image::RgbImage,
            _: &str,
            _: BitmapLineContext,
        ) -> Result<Option<OcrLine>> {
            self.seen_sizes.push(image.dimensions());
            Ok(self.responses.pop_front().flatten().map(|text| OcrLine {
                text,
                bbox: None,
                score: Some(0.9),
                color: None,
                italic: false,
            }))
        }
    }

    struct ContextBitmapMock {
        contexts: Vec<BitmapLineContext>,
    }

    impl SubtitleConverter for ContextBitmapMock {
        fn extract_lines(&mut self, _: &Path, _: &str) -> Result<OcrOutput> {
            Ok(OcrOutput::default())
        }
        fn supports_bitmap_line_recognition(&self, language: &str) -> bool {
            is_english_language(language)
        }
        fn recognize_bitmap_line(
            &mut self,
            _: &image::RgbImage,
            _: &str,
            context: BitmapLineContext,
        ) -> Result<Option<OcrLine>> {
            self.contexts.push(context);
            Ok(Some(OcrLine {
                text: "AB".into(),
                bbox: None,
                score: Some(0.9),
                color: None,
                italic: false,
            }))
        }
    }

    /// A paletted bitmap rectangle whose index 1 is opaque white.
    fn bitmap_rect(
        width: usize,
        height: usize,
        pixels: &mut [u8],
        palette: &mut [u8],
    ) -> ffi::AVSubtitleRect {
        palette[4..8].fill(255);
        let mut rect: ffi::AVSubtitleRect = unsafe { std::mem::zeroed() };
        rect.type_ = ffi::SUBTITLE_BITMAP;
        rect.w = width as i32;
        rect.h = height as i32;
        rect.nb_colors = 256;
        rect.linesize[0] = width as i32;
        rect.data[0] = pixels.as_mut_ptr();
        rect.data[1] = palette.as_mut_ptr();
        rect
    }

    #[test]
    fn bitmap_line_context_requires_one_rectangle_and_one_band() {
        let width = 16usize;
        let height = 25usize;
        let mut pixels = vec![0u8; width * height];
        for y in 2..8 {
            pixels[y * width + 2..y * width + 14].fill(1);
        }
        let mut palette = vec![0u8; 256 * 4];
        let rect = bitmap_rect(width, height, &mut pixels, &mut palette);
        let temp = tempfile::tempdir().unwrap();
        let params = cue_params(
            "eng",
            ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
            OcrFormat::Srt,
            OcrEngine::PpOcrV3,
            temp.path(),
        );
        let pgm_path = temp.path().join("unused.pgm");
        let mut mock = ContextBitmapMock {
            contexts: Vec::new(),
        };
        let recognize = |mock: &mut ContextBitmapMock, cue_rect_count| {
            let (pgm, _) = rect_to_pgm(&rect, OcrEngine::PpOcrV3).unwrap();
            let bitmap = BitmapRect {
                rect: &rect,
                index: 0,
                pgm: &pgm,
                pgm_path: &pgm_path,
                color: None,
            };
            recognize_rect_as_bitmap_lines(mock, &bitmap, &params, cue_rect_count)
                .unwrap()
                .is_some()
        };
        for (count, expected) in [
            (1, BitmapLineContext::SingleRectangleSingleBand),
            (2, BitmapLineContext::UnknownOrMultiple),
        ] {
            assert!(recognize(&mut mock, count));
            assert_eq!(mock.contexts, [expected]);
            mock.contexts.clear();
        }
        for y in 15..21 {
            pixels[y * width + 2..y * width + 14].fill(1);
        }
        assert!(recognize(&mut mock, 1));
        assert_eq!(mock.contexts, [BitmapLineContext::UnknownOrMultiple; 2]);
    }

    fn punctuation_test_image(dots: usize, comma: bool) -> image::GrayImage {
        let mut image = image::GrayImage::from_pixel(48, 18, image::Luma([255]));
        let mut fill = |left, top, width, height| {
            for y in top..top + height {
                for x in left..left + width {
                    image.put_pixel(x, y, image::Luma([0]));
                }
            }
        };
        fill(2, 2, 4, 8);
        if comma {
            fill(21, 5, 2, 3);
            fill(20, 8, 2, 3);
        } else {
            for index in 0..dots as u32 {
                fill(10 + 4 * index, 12, 2, 2);
            }
        }
        image
    }

    #[test]
    fn direct_punctuation_requires_shape_and_two_agreeing_recognizers() {
        let three = punctuation_test_image(3, false);
        let mut text = "abc..".to_owned();
        assert!(recover_direct_line_punctuation_with(
            &mut text,
            &three,
            || { Some(("abc...".into(), "a bc...".into())) }
        ));
        assert_eq!(text, "abc...");

        for image in [
            punctuation_test_image(2, false),
            punctuation_test_image(4, false),
            {
                let mut noisy = three.clone();
                noisy.put_pixel(26, 13, image::Luma([0]));
                noisy
            },
        ] {
            let mut text = "abc..".to_owned();
            assert!(!recover_direct_line_punctuation_with(
                &mut text,
                &image,
                || { panic!("ambiguous shape must not run recognition") }
            ));
            assert_eq!(text, "abc..");
        }
        for attempts in [
            Some(("abc...".into(), "abc..".into())),
            Some(("Abc...".into(), "Abc...".into())),
            Some(("abd...".into(), "abd...".into())),
            Some(("abc...\nxyz".into(), "abc...\nxyz".into())),
            None,
        ] {
            let mut text = "abc..".to_owned();
            assert!(!recover_direct_line_punctuation_with(
                &mut text,
                &three,
                || attempts
            ));
            assert_eq!(text, "abc..");
        }
        for input in ["ab..", "- A: abc..\n- B: def..", "abc...", "abc!"] {
            let mut text = input.to_owned();
            assert!(!recover_direct_line_punctuation_with(
                &mut text,
                &three,
                || { panic!("ineligible text must not run recognition") }
            ));
            assert_eq!(text, input);
        }
    }

    #[test]
    fn direct_punctuation_preserves_latin_glyphs_and_requires_exact_agreement() {
        let image = punctuation_test_image(3, false);
        let mut text = "Élève déjà..".to_owned();
        assert!(recover_direct_line_punctuation_with(
            &mut text,
            &image,
            || { Some(("Élève déjà...".into(), "Élève déjà...".into())) }
        ));
        assert_eq!(text, "Élève déjà...");

        for candidate in ["Eleve déjà...", "Élève deja...", "Éle\u{300}ve déjà..."] {
            let mut text = "Élève déjà..".to_owned();
            assert!(!recover_direct_line_punctuation_with(
                &mut text,
                &image,
                || { Some(("Élève déjà...".into(), candidate.into())) }
            ));
            assert_eq!(text, "Élève déjà..");
        }
        let mut text = "Español.".to_owned();
        assert!(recover_direct_line_punctuation_with(
            &mut text,
            &punctuation_test_image(0, true),
            || Some(("Español,".into(), "Español,".into()))
        ));
        assert_eq!(text, "Español,");
    }

    #[test]
    fn direct_punctuation_uses_matching_available_tessdata_for_both_psms() {
        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("work.pgm");
        let available = ["eng".to_owned(), "fra".to_owned()]
            .into_iter()
            .collect::<HashSet<_>>();
        let mut calls = Vec::new();
        let attempts = direct_punctuation_tesseract_attempts_with(
            &path,
            b"crop",
            "fre",
            |language| resolve_tesseract_fallback_language_with_available(language, &available),
            |crop_path, language, psm| {
                assert_eq!(fs::read(crop_path).unwrap(), b"crop");
                calls.push((language.to_owned(), psm));
                Ok("Élève...".into())
            },
        );
        assert_eq!(attempts, Some(("Élève...".into(), "Élève...".into())));
        assert_eq!(calls, [("fra".to_owned(), 6), ("fra".to_owned(), 7)]);

        for language in ["spa", "xyz"] {
            assert!(direct_punctuation_tesseract_attempts_with(
                &path,
                b"crop",
                language,
                |language| resolve_tesseract_fallback_language_with_available(language, &available),
                |_, _, _| panic!("missing tessdata must not run recognition"),
            )
            .is_none());
        }
    }

    #[test]
    fn direct_punctuation_comma_requires_both_shape_and_full_text_agreement() {
        let comma = punctuation_test_image(0, true);
        let mut text = "- A: abc.".to_owned();
        assert!(recover_direct_line_punctuation_with(
            &mut text,
            &comma,
            || { Some(("- A: abc,".into(), "- A: abc,".into())) }
        ));
        assert_eq!(text, "- A: abc,");
        let mut text = "- B: def.".to_owned();
        assert!(!recover_direct_line_punctuation_with(
            &mut text,
            &comma,
            || { Some(("- B: def,".into(), "- B: def.".into())) }
        ));
        assert_eq!(text, "- B: def.");
        let mut text = "abc.".to_owned();
        assert!(!recover_direct_line_punctuation_with(
            &mut text,
            &punctuation_test_image(1, false),
            || panic!("period shape must not run recognition")
        ));
        assert_eq!(text, "abc.");
    }

    #[test]
    fn bitmap_line_route_respects_engine_capability_and_policies() {
        let temp = tempfile::tempdir().unwrap();
        let params = cue_params(
            "eng",
            ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
            OcrFormat::Srt,
            OcrEngine::PpOcrV3,
            temp.path(),
        );
        let engine = DirectBitmapMock {
            enabled: true,
            responses: Default::default(),
            seen_sizes: Vec::new(),
            detector_calls: 0,
        };
        for language in ["eng", "fra", "spa", "xyz", "sr-Latn"] {
            let params = CueBuildParams { language, ..params };
            assert!(
                may_use_bitmap_lines(&params, &engine, false, false),
                "{language}"
            );
            assert!(
                !may_use_bitmap_lines(&params, &engine, true, false),
                "{language}"
            );
        }
        // Match the detector's existing forced-fallback language policy exactly.
        for (language, expected) in [("eng", true), ("fra", false), ("spa", false), ("xyz", true)] {
            let params = CueBuildParams { language, ..params };
            assert_eq!(
                may_use_bitmap_lines(&params, &engine, false, true),
                expected,
                "{language}"
            );
        }
        let disabled = DirectBitmapMock {
            enabled: false,
            ..engine
        };
        assert!(!may_use_bitmap_lines(&params, &disabled, false, false));
    }

    #[test]
    fn experimental_bitmap_lines_use_raw_lines_and_fallback_as_a_whole() {
        let width = 16usize;
        let height = 25usize;
        let mut pixels = vec![0u8; width * height];
        for y in (2..8).chain(15..21) {
            pixels[y * width + 2..y * width + 14].fill(1);
        }
        let mut palette = vec![0u8; 256 * 4];
        let mut rect = bitmap_rect(width, height, &mut pixels, &mut palette);
        rect.x = 9;
        rect.y = 11;
        let mut rect_ptr = &mut rect as *mut ffi::AVSubtitleRect;
        let mut sub = subtitle(Some(1_000), 0, 2_000);
        sub.num_rects = 1;
        sub.rects = &mut rect_ptr;
        let temp = tempfile::tempdir().unwrap();
        let params = cue_params(
            "eng",
            ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
            OcrFormat::Srt,
            OcrEngine::PpOcrV3,
            temp.path(),
        );
        let mut direct = DirectBitmapMock {
            enabled: true,
            responses: [
                Some("  - Red: i can.  ".to_owned()),
                Some(" - Blue: Yes! ".to_owned()),
            ]
            .into(),
            seen_sizes: Vec::new(),
            detector_calls: 0,
        };
        assert!(may_use_bitmap_lines(&params, &direct, false, false));
        assert!(!may_use_bitmap_lines(&params, &direct, true, false));
        assert!(may_use_bitmap_lines(
            &CueBuildParams {
                language: "fra",
                ..params
            },
            &direct,
            false,
            false
        ));
        assert!(!may_use_bitmap_lines(
            &CueBuildParams {
                stream_codec_id: ffi::AV_CODEC_ID_DVD_SUBTITLE,
                ..params
            },
            &direct,
            false,
            false
        ));
        assert!(!may_use_bitmap_lines(
            &CueBuildParams {
                ocr_format: OcrFormat::Ass,
                ..params
            },
            &direct,
            false,
            false
        ));
        let (lines, visible) = extract_subtitle_lines(
            &sub,
            &params,
            1_000,
            &mut direct,
            &mut OcrQualityBaseline::default(),
        )
        .unwrap();
        assert!(visible);
        assert_eq!(
            lines
                .iter()
                .map(|line| line.text.as_str())
                .collect::<Vec<_>>(),
            ["- Red: i can.", "- Blue: Yes!"]
        );
        assert_eq!(direct.seen_sizes.len(), 2);
        assert_eq!(direct.detector_calls, 0);
        assert!(lines.iter().all(|line| line.bbox.is_none()));

        for (language, text) in [("fra", "Élève déjà"), ("xyz", "Crème brûlée")] {
            let mut latin = DirectBitmapMock {
                enabled: true,
                responses: [Some(text.to_owned()), Some(text.to_owned())].into(),
                seen_sizes: Vec::new(),
                detector_calls: 0,
            };
            let latin_params = CueBuildParams { language, ..params };
            let (lines, _) = extract_subtitle_lines(
                &sub,
                &latin_params,
                1_000,
                &mut latin,
                &mut OcrQualityBaseline::default(),
            )
            .unwrap();
            assert_eq!(latin.detector_calls, 0, "{language}");
            assert_eq!(
                lines
                    .iter()
                    .map(|line| line.text.as_str())
                    .collect::<Vec<_>>(),
                [text, text]
            );
        }

        let mut partial = DirectBitmapMock {
            enabled: true,
            responses: [Some("partial".to_owned()), None].into(),
            seen_sizes: Vec::new(),
            detector_calls: 0,
        };
        let (lines, _) = extract_subtitle_lines(
            &sub,
            &params,
            1_000,
            &mut partial,
            &mut OcrQualityBaseline::default(),
        )
        .unwrap();
        assert_eq!(partial.detector_calls, 1);
        assert_eq!(
            lines
                .iter()
                .map(|line| line.text.as_str())
                .collect::<Vec<_>>(),
            ["Stable text."]
        );

        let dvd_params = CueBuildParams {
            stream_codec_id: ffi::AV_CODEC_ID_DVD_SUBTITLE,
            ..params
        };
        let mut dvd = DirectBitmapMock {
            enabled: true,
            responses: [Some("should not be used".to_owned())].into(),
            seen_sizes: Vec::new(),
            detector_calls: 0,
        };
        let (dvd_lines, _) = extract_subtitle_lines(
            &sub,
            &dvd_params,
            1_000,
            &mut dvd,
            &mut OcrQualityBaseline::default(),
        )
        .unwrap();
        assert_eq!(dvd.detector_calls, 1);
        assert!(dvd.seen_sizes.is_empty());
        assert_eq!(dvd_lines[0].text, "Stable text.");

        let disabled = DirectBitmapMock {
            enabled: false,
            responses: std::collections::VecDeque::new(),
            seen_sizes: Vec::new(),
            detector_calls: 0,
        };
        assert!(!may_use_bitmap_lines(&params, &disabled, false, false));
    }

    struct RectangleOcr {
        outputs: std::collections::VecDeque<OcrOutput>,
    }

    impl SubtitleConverter for RectangleOcr {
        fn extract_lines(&mut self, _: &Path, _: &str) -> Result<OcrOutput> {
            Ok(self.outputs.pop_front().expect("one output per rectangle"))
        }
    }

    #[test]
    fn rectangle_order_is_srt_only_and_ass_keeps_line_metadata() {
        let temp = tempfile::tempdir().unwrap();
        let mut pixels = [1u8; 16];
        let mut lower: ffi::AVSubtitleRect = unsafe { std::mem::zeroed() };
        lower.type_ = ffi::SUBTITLE_BITMAP;
        lower.x = 10;
        lower.y = 100;
        lower.w = 4;
        lower.h = 4;
        lower.linesize[0] = 4;
        lower.data[0] = pixels.as_mut_ptr();
        let mut upper: ffi::AVSubtitleRect = unsafe { std::mem::zeroed() };
        upper.type_ = ffi::SUBTITLE_BITMAP;
        upper.x = 20;
        upper.y = 10;
        upper.w = 4;
        upper.h = 4;
        upper.linesize[0] = 4;
        upper.data[0] = pixels.as_mut_ptr();
        let mut rects = [&mut lower as *mut _, &mut upper as *mut _];
        let mut sub = subtitle(Some(1000), 0, 1000);
        sub.num_rects = rects.len() as u32;
        sub.rects = rects.as_mut_ptr();
        let make_line = |text: &str, color, italic| OcrLine {
            text: text.into(),
            bbox: Some(OcrBoundingBox {
                left: 0,
                top: 0,
                right: 4,
                bottom: 4,
            }),
            score: Some(0.8),
            color: Some(color),
            italic,
        };
        for format in [OcrFormat::Srt, OcrFormat::Ass] {
            let mut engine = RectangleOcr {
                outputs: [
                    OcrOutput {
                        lines: vec![
                            make_line("lower one", (1, 2, 3), true),
                            make_line("lower two", (1, 2, 3), true),
                        ],
                        ..OcrOutput::default()
                    },
                    OcrOutput {
                        lines: vec![make_line("upper", (4, 5, 6), false)],
                        ..OcrOutput::default()
                    },
                ]
                .into(),
            };
            let params = cue_params(
                "fra",
                ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
                format,
                OcrEngine::External,
                temp.path(),
            );
            let cues = subtitle_to_cues(
                &sub,
                None,
                0,
                &params,
                &mut engine,
                &mut OcrQualityBaseline::default(),
            )
            .unwrap();
            match format {
                OcrFormat::Srt => {
                    assert_eq!(cues.len(), 1);
                    assert_eq!(cues[0].text, "upper\nlower one\nlower two");
                }
                OcrFormat::Ass => {
                    assert_eq!(cues.len(), 3);
                    assert_eq!(cues[0].text, "{\\pos(12,102)\\c&H030201&\\i1}lower one");
                    assert_eq!(cues[1].text, "{\\pos(12,102)\\c&H030201&\\i1}lower two");
                    assert_eq!(cues[2].text, "{\\pos(22,12)\\c&H060504&}upper");
                }
            }
        }
    }

    #[test]
    fn terminal_punctuation_recovery_requires_exact_glyph_agreement() {
        let line = styled_line;

        let mut lines = vec![line("A small"), line("example  ")];
        assert!(recover_terminal_punctuation(&mut lines, "Asmall example!"));
        assert_eq!(lines[0].text, "A small");
        assert_eq!(lines[1].text, "example!  ");
        assert!(lines[1].italic);
        assert_eq!(lines[1].score, Some(0.9));

        for (original, candidate) in [
            ("A small example", "A small examples."),
            ("A small example", "A smalI example."),
            ("A small example", "A small example..."),
            ("A small example?", "A small example!"),
            ("A small example", "A small example,"),
            ("A small example", "A small example"),
            ("A short", "A short."),
            ("Some 100", "Some 100."),
            ("", "A small example."),
        ] {
            let mut lines = vec![line(original)];
            assert!(!recover_terminal_punctuation(&mut lines, candidate));
            assert_eq!(lines[0].text, original);
        }
    }

    #[test]
    fn training_export_skips_exact_punctuation_recovery() {
        use std::cell::Cell;
        let original = OcrOutput {
            lines: vec![OcrLine {
                text: "Some simple words".to_string(),
                bbox: None,
                score: Some(0.9),
                color: None,
                italic: false,
            }],
            ..OcrOutput::default()
        };
        let exports = Cell::new(0);
        for mark in ['.', '?', '!'] {
            let mut lines = original.lines.clone();
            let recovered =
                recover_terminal_punctuation(&mut lines, &format!("Some simple words{mark}"));
            assert!(recovered);
            export_training_sample_unless_recovered(Some(&original), recovered, |_| {
                exports.set(exports.get() + 1);
                Ok(())
            })
            .unwrap();
        }
        assert_eq!(exports.get(), 0);
        let mut lines = original.lines.clone();
        let recovered = recover_terminal_punctuation(&mut lines, "Some altered words.");
        assert!(!recovered);
        export_training_sample_unless_recovered(Some(&original), recovered, |_| {
            exports.set(exports.get() + 1);
            Ok(())
        })
        .unwrap();
        assert_eq!(exports.get(), 1);
        export_training_sample_unless_recovered(None, false, |_| {
            panic!("no sample should not invoke export")
        })
        .unwrap();
        assert!(
            export_training_sample_unless_recovered(Some(&original), false, |_| {
                Err(anyhow!("export failure"))
            })
            .is_err()
        );
    }

    #[test]
    fn ambiguous_terminal_period_requires_matching_raw_lines_and_glyphs() {
        let line = styled_line;
        let mut lines = vec![line("Iwalk home"), line("before dusk  ")];
        assert!(ambiguous_terminal_period_candidate(
            &lines,
            "| walk home\nbefore dusk."
        ));
        assert!(append_terminal_mark(&mut lines, '.'));
        assert_eq!(lines[0].text, "Iwalk home");
        assert_eq!(lines[1].text, "before dusk.  ");
        assert!(lines[1].italic);
        assert_eq!(lines[1].score, Some(0.9));
        assert!(!ambiguous_terminal_period_candidate(
            &lines,
            "| walk home\nbefore dusk."
        ));

        let raw = vec![line("Iwalkhome"), line("beforedusk")];
        assert!(raw_and_final_glyphs_match(
            &raw,
            &[line("Iwalk home"), line("before dusk")]
        ));
        assert!(!raw_and_final_glyphs_match(
            &raw,
            &[line("|walk home"), line("before dusk")]
        ));
        assert!(!raw_and_final_glyphs_match(
            &raw,
            &[line("Iwalk"), line("home before dusk")]
        ));
        assert!(ambiguous_terminal_period_raw_candidate(
            &raw,
            "| walk home\nbefore dusk."
        ));
        assert!(!ambiguous_terminal_period_raw_candidate(
            &raw,
            "| walk\nhome before dusk."
        ));
        assert!(!ambiguous_terminal_period_raw_candidate(
            &raw,
            "| walk home\nbefore dust."
        ));
        let lines = vec![line("Iwalk home"), line("before dusk")];
        for candidate in [
            "| walk home\nbefore dusk",
            "| walk home\nbefore dusk?",
            "| walk home\nbefore dusk!",
            "| walk home\nbefore dusk..",
            "| walk home before dusk.",
            "| walk\nhome before dusk.",
            "| walk home\n\nbefore dusk.",
            "| walk home\r\nbefore dusk.",
            "| walk home\nbefore dusK.",
            "| walk home\nbefore dust.",
            "|  walk home\nbefore dusk.",
            "| walk home\nbefore  dusk.",
            "| walk home\nbefore dusk .",
            "Iwalk home\nbefore dusk.",
            "| wa|k home\nbefore dusk.",
        ] {
            assert!(
                !ambiguous_terminal_period_candidate(&lines, candidate),
                "{candidate:?}"
            );
        }
        assert!(ambiguous_terminal_period_candidate(
            &[line("|walk home"), line("before dusk")],
            "Iwalk home\nbefore dusk."
        ));
        assert!(!ambiguous_terminal_period_candidate(
            &[line("Iwalk home"), line(""), line("before dusk")],
            "| walk home\nbefore dusk."
        ));
        assert!(!ambiguous_terminal_period_candidate(
            &[line("I go")],
            "| go."
        ));
    }

    #[test]
    fn ambiguous_period_checks_inputs_before_alternate_and_pixel_work() {
        use std::cell::Cell;
        let line = styled_line;
        let candidate = |psm, text: &str| TesseractCandidate {
            text: text.to_string(),
            psm,
            quality: 0.9,
        };
        let raw = vec![line("Iwalkhome"), line("beforedusk")];
        let mut changed = vec![line("Iwalk home"), line("before dawn")];
        assert!(!recover_ambiguous_terminal_period_with(
            &raw,
            &mut changed,
            &candidate(7, "unrelated"),
            || panic!("alternate must not run for changed glyphs"),
            |_| panic!("pixel evidence must not run for changed glyphs"),
        ));
        let mut blank = vec![line("Iwalk home"), line(""), line("before dusk")];
        assert!(!recover_ambiguous_terminal_period_with(
            &raw,
            &mut blank,
            &candidate(7, "unrelated"),
            || panic!("alternate must not run for invalid line structure"),
            |_| panic!("pixel evidence must not run for invalid line structure"),
        ));
        let ellipsis_raw = vec![line("Iwait..")];
        let mut ellipsis = vec![line("Iwait..")];
        assert!(!recover_ambiguous_terminal_period_with(
            &ellipsis_raw,
            &mut ellipsis,
            &candidate(7, "unrelated"),
            || panic!("alternate must not run for an existing terminal mark"),
            |_| panic!("pixel evidence must not run for an existing terminal mark"),
        ));
        let mut lines = vec![line("Iwalk home"), line("before dusk")];
        assert!(!recover_ambiguous_terminal_period_with(
            &raw,
            &mut lines,
            &candidate(6, "| walk home\nbefore dust."),
            || panic!("the selected PSM 6 must be reused"),
            |_| panic!("pixel evidence must not run for a mismatched candidate"),
        ));
        assert!(!recover_ambiguous_terminal_period_with(
            &raw,
            &mut lines,
            &candidate(7, "unrelated"),
            || Err(anyhow!("alternate unavailable")),
            |_| panic!("pixel evidence must not run after a failed alternate"),
        ));
        assert!(!recover_ambiguous_terminal_period_with(
            &raw,
            &mut lines,
            &candidate(6, "| walk home\nbefore dusk."),
            || panic!("the selected PSM 6 must be reused"),
            |_| false,
        ));
        assert_eq!(lines[1].text, "before dusk");
        let calls = Cell::new(0);
        assert!(recover_ambiguous_terminal_period_with(
            &raw,
            &mut lines,
            &candidate(7, "unrelated"),
            || {
                calls.set(calls.get() + 1);
                Ok("| walk home\nbefore dusk.".to_string())
            },
            |_| true,
        ));
        assert_eq!(calls.get(), 1);
        assert_eq!(lines[0].text, "Iwalk home");
        assert_eq!(lines[1].text, "before dusk.");
        assert_eq!(lines[1].score, Some(0.9));
        assert!(lines[1].italic);
    }

    #[test]
    fn missing_terminal_ellipsis_requires_exact_existing_glyphs() {
        let line = styled_line;
        let mut lines = vec![line("Waiting..  ")];
        assert!(needs_terminal_punctuation_candidate(&lines));
        assert!(missing_terminal_ellipsis_candidate(&lines, "Waiting..."));
        assert!(!recover_terminal_punctuation(&mut lines, "Waiting..."));
        for candidate in [
            "Waitlng...",
            "Waiting..",
            "Waiting....",
            "Waiting!...",
            "Waiting.\n..",
        ] {
            assert!(!missing_terminal_ellipsis_candidate(&lines, candidate));
        }
        assert!(append_terminal_mark(&mut lines, '.'));
        assert_eq!(lines[0].text, "Waiting...  ");
        assert!(lines[0].italic);
        assert_eq!(lines[0].score, Some(0.9));
        assert!(!needs_terminal_punctuation_candidate(&lines));
        let short = vec![line("No..")];
        assert!(needs_terminal_punctuation_candidate(&short));
        assert!(missing_terminal_ellipsis_candidate(&short, "No..."));
    }

    #[test]
    fn comma_spacing_transfer_preserves_glyphs_and_line_layout() {
        let line = styled_line;
        let mut lines = vec![line("Well,then"), line("we can wait...")];
        assert!(transfer_verified_comma_spacing(
            &mut lines,
            "Well, then\nwe can wait...",
            &HashSet::from([5])
        ));
        assert_eq!(lines[0].text, "Well, then");
        assert_eq!(lines[1].text, "we can wait...");
        assert!(lines[0].italic);
        assert_eq!(lines[0].score, Some(0.9));
        assert!(!transfer_verified_comma_spacing(
            &mut lines,
            "Well, then\nwe can wait...",
            &HashSet::from([5])
        ));
        let mut original = vec![line("Well,then"), line("we can wait...")];
        assert!(!transfer_verified_comma_spacing(
            &mut original,
            "Well, then\nwe can wait...",
            &HashSet::new()
        ));
        assert_eq!(original[0].text, "Well,then");
        for candidate in [
            "Well, thEn\nwe can wait...",
            "Well, then we can wait...",
            "Well, then\nwe can wait!",
        ] {
            let mut original = vec![line("Well,then"), line("we can wait...")];
            assert!(!transfer_verified_comma_spacing(
                &mut original,
                candidate,
                &HashSet::from([5])
            ));
            assert_eq!(original[0].text, "Well,then");
        }
    }

    #[test]
    fn first_line_comma_requires_exact_glyphs_both_psms_and_pixels() {
        let line = |text: &str| OcrLine {
            text: text.into(),
            bbox: Some(OcrBoundingBox {
                left: 1,
                top: 2,
                right: 30,
                bottom: 20,
            }),
            score: Some(0.9),
            color: Some((4, 5, 6)),
            italic: true,
        };
        let original = vec![line("AB CD  "), line("XY Z")];
        let mut actual = original.clone();
        assert!(recover_first_line_comma_with(
            &mut actual,
            "ABCD,\nXYZ",
            "AB CD,\nXY Z",
            || true
        ));
        assert_eq!(actual[0].text, "AB CD,  ");
        assert_eq!(actual[1].text, original[1].text);
        for (actual, original) in actual.iter().zip(&original) {
            assert_eq!(actual.score, original.score);
            assert_eq!(actual.color, original.color);
            assert_eq!(actual.italic, original.italic);
            let actual = actual.bbox.as_ref().unwrap();
            let original = original.bbox.as_ref().unwrap();
            assert_eq!(
                (actual.left, actual.top, actual.right, actual.bottom),
                (original.left, original.top, original.right, original.bottom)
            );
        }
        let training = OcrOutput {
            lines: original.clone(),
            ..OcrOutput::default()
        };
        export_training_sample_unless_recovered(Some(&training), true, |_| {
            panic!("stale label exported")
        })
        .unwrap();
        for (first, second) in [
            ("ABCD,\nXYZ", "ABCD\nXYZ"),
            ("ABCD,\nXYZ", "ABCE,\nXYZ"),
            ("ABCD,\nXYZ", "ABCD,\nXYQ"),
            ("ABCD.\nXYZ", "ABCD.\nXYZ"),
            ("ABCD, XYZ", "ABCD, XYZ"),
        ] {
            let mut actual = original.clone();
            assert!(!recover_first_line_comma_with(
                &mut actual,
                first,
                second,
                || panic!("must not inspect pixels")
            ));
            assert_eq!(actual[0].text, original[0].text);
        }
        let mut actual = original.clone();
        assert!(!recover_first_line_comma_with(
            &mut actual,
            "ABCD,\nXYZ",
            "ABCD,\nXYZ",
            || false
        ));
        assert_eq!(actual[0].text, original[0].text);
    }

    #[test]
    fn multiline_punctuation_requires_matching_lines_and_independent_agreement() {
        fn lines() -> Vec<OcrLine> {
            ["Aquietplace", "Wearehome."].map(line).to_vec()
        }
        let mut original = lines();
        assert!(recover_line_terminal_punctuation(
            &mut original,
            "A quiet place.\nWe are home.",
            "A quiet place.\n\nWe are home."
        ));
        assert_eq!(original[0].text, "Aquietplace.");
        assert_eq!(
            transfer_agreed_spacing(&original, "A quiet place.\nWe are home.").as_deref(),
            Some("A quiet place.\nWe are home.")
        );
        for (candidate, alternate) in [
            (
                "A quiet place.\nWe are home.",
                "A quiet place!\nWe are home.",
            ),
            (
                "A quiet places.\nWe are home.",
                "A quiet places.\nWe are home.",
            ),
            ("A quiet place. We are home.", "A quiet place. We are home."),
            (
                "A quiet place.\nWe are home!",
                "A quiet place.\nWe are home!",
            ),
        ] {
            let mut original = lines();
            assert!(!recover_line_terminal_punctuation(
                &mut original,
                candidate,
                alternate
            ));
            assert_eq!(original[0].text, "Aquietplace");
        }
    }

    #[test]
    fn balanced_quote_recovery_requires_two_matching_recognitions() {
        let mut lines = vec![line("  Quiet harbor  ")];
        assert!(recover_balanced_quotes(
            &mut lines,
            "\"Quiet harbor\"",
            "\"Quiet harbor\""
        ));
        assert_eq!(lines[0].text, "  \"Quiet harbor\"  ");
        for (original, candidate, alternate) in [
            ("Quiet harbor", "\"Quiet harbor\"", "Quiet harbor"),
            ("Quiet harbor", "\"Quiet harbor\"", "\"Quiet harbour\""),
            ("Quiet harbor", "\"Quiet harbor 2\"", "\"Quiet harbor 2\""),
            ("Quiet harbor", "\"Quiet harbor.\"", "\"Quiet harbor.\""),
            ("Quiet \"harbor\"", "\"Quiet harbor\"", "\"Quiet harbor\""),
            ("Quiet", "\"Quiet\"", "\"Quiet\""),
        ] {
            let mut lines = vec![line(original)];
            assert!(!recover_balanced_quotes(&mut lines, candidate, alternate));
            assert_eq!(lines[0].text, original);
        }
        let mut lines = vec![line("Quiet"), line("harbor")];
        assert!(!recover_balanced_quotes(
            &mut lines,
            "\"Quiet harbor\"",
            "\"Quiet harbor\""
        ));
    }

    #[test]
    fn spacing_transfer_abstains_for_an_ambiguous_short_single_token() {
        for (original, candidate) in [("inform", "in form"), ("forall", "for all")] {
            let lines = vec![OcrLine {
                text: original.to_string(),
                bbox: None,
                score: Some(0.9),
                color: None,
                italic: false,
            }];
            assert!(transfer_agreed_spacing(&lines, candidate).is_none());
        }
    }

    #[test]
    fn spacing_recovery_preserves_original_line_layout_and_metadata() {
        let lines = vec![
            OcrLine {
                text: "  Bluebird ".into(),
                bbox: Some(OcrBoundingBox {
                    left: 1,
                    top: 2,
                    right: 30,
                    bottom: 10,
                }),
                score: Some(0.8),
                color: Some((1, 2, 3)),
                italic: true,
            },
            OcrLine {
                text: "greentree".into(),
                bbox: Some(OcrBoundingBox {
                    left: 5,
                    top: 15,
                    right: 40,
                    bottom: 25,
                }),
                score: Some(0.9),
                color: Some((4, 5, 6)),
                italic: false,
            },
        ];
        for candidate in [
            "Blue bird green tree",
            "Blue\nbird green tree",
            "Blue bird green\ntree",
        ] {
            let transferred = transfer_agreed_spacing(&lines, candidate).unwrap();
            assert_eq!(transferred, "Blue bird\ngreen tree");
            let repaired = apply_transferred_spacing(&lines, &transferred, "eng");
            assert_eq!(repaired.len(), 2);
            assert_eq!(repaired[0].text, "  Blue bird ");
            assert_eq!(repaired[1].text, "green tree");
            for (actual, original) in repaired.iter().zip(&lines) {
                let a = actual.bbox.as_ref().unwrap();
                let b = original.bbox.as_ref().unwrap();
                assert_eq!(
                    (a.left, a.top, a.right, a.bottom),
                    (b.left, b.top, b.right, b.bottom)
                );
                assert_eq!(actual.score, original.score);
                assert_eq!(actual.color, original.color);
                assert_eq!(actual.italic, original.italic);
            }
        }
    }

    #[test]
    fn spacing_recovery_preserves_internal_line_breaks() {
        let lines = vec![line("Bluebird\ngreentree")];
        let transferred = transfer_agreed_spacing(&lines, "Blue bird green tree").unwrap();
        assert_eq!(transferred, "Blue bird\ngreen tree");
        assert_eq!(
            apply_transferred_spacing(&lines, &transferred, "eng")[0].text,
            transferred
        );
    }

    #[test]
    fn transfer_spacing_preserves_ambiguous_glyphs_and_lines() {
        let lines = vec![line("Iwanttheletter"), line("sentbytomorrow.")];
        assert_eq!(
            transfer_agreed_spacing(&lines, "| want the letter\nsent by tomorrow.").as_deref(),
            Some("I want the letter\nsent by tomorrow.")
        );
        assert!(transfer_agreed_spacing(&lines, "| want the letter\nsent by today.").is_none());
        let repaired = transfer_agreed_spacing(&lines, "| want the letter\nsent by tomorrow!")
            .expect("matching anchors can recover spacing despite punctuation differences");
        assert!(repaired.ends_with('.'));
        assert_eq!(
            text_and_boundaries(&repaired).0,
            text_and_boundaries("Iwanttheletter\nsentbytomorrow.").0
        );
    }

    #[test]
    fn anchored_spacing_uses_sample_evidence_without_copying_glyphs() {
        let original = "we found a note on the\nwindowsillbeforetheevening";
        let candidate = "we found a note on the windowsill before the evening.";
        let repaired = transfer_anchored_spacing(original, candidate).unwrap();
        assert_eq!(
            repaired,
            "we found a note on the\nwindowsill before the evening"
        );
        assert_eq!(
            text_and_boundaries(original).0,
            text_and_boundaries(&repaired).0
        );
        assert!(preserves_existing_boundaries(original, &repaired));

        let original = "Well, well... I should send thelettersbeforetomorrow.";
        let candidate = "Well, well... | should send the letters before tomorrow.";
        let repaired = transfer_anchored_spacing(original, candidate).unwrap();
        assert!(repaired.contains("letters before tomorrow"));
        assert!(repaired.contains("I should"));
        assert_eq!(
            text_and_boundaries(original).0,
            text_and_boundaries(&repaired).0
        );
    }

    #[test]
    fn anchored_spacing_rejects_unrelated_or_ambiguous_evidence() {
        assert!(
            transfer_anchored_spacing("Thedoorisopenandready", "A different line entirely")
                .is_none()
        );
        assert!(transfer_anchored_spacing("abcdabcdabcdabcd", "abcd abcd abcd abcd!").is_none());
        assert!(transfer_anchored_spacing("short", "sh ort!").is_none());
        assert!(transfer_anchored_spacing(&"a".repeat(513), &"a".repeat(513)).is_none());
    }

    #[test]
    fn crop_recovery_rejects_lost_or_altered_characters() {
        let output = text_output;
        let original = output("Thedoorisopen.\nPleasewait.");
        assert!(crop_recovery_preserves_characters(
            &original,
            &output("The door is open.\nPlease wait.")
        ));
        assert!(!crop_recovery_preserves_characters(
            &original,
            &output("The d o or is open.\nPlease wai t")
        ));
        assert!(!crop_recovery_preserves_characters(
            &original,
            &output("The door is open.")
        ));
        assert!(!crop_recovery_preserves_characters(
            &original,
            &output("The door is opened.\nPlease wait.")
        ));
        assert!(!crop_recovery_preserves_characters(
            &original,
            &output("The door is open. Please wait.")
        ));
        assert!(!crop_recovery_preserves_characters(
            &original,
            &output("The door is\nopen. Please wait.")
        ));
        let split_lines = OcrOutput {
            lines: vec![line("The door is open."), line("Please wait.")],
            ..OcrOutput::default()
        };
        assert!(crop_recovery_preserves_characters(&original, &split_lines));
    }

    #[test]
    fn crop_selection_filters_before_ranking() {
        let output = text_output;
        let original = output("Thedoorisopen.");
        let invalid = output("This is a fluent but different sentence.");
        let valid = output("The door is open.");
        let mut best = None;
        consider_crop_candidate(&mut best, &original, invalid, 0.99);
        consider_crop_candidate(&mut best, &original, valid, 0.70);
        assert_eq!(best.unwrap().1.lines[0].text, "The door is open.");
    }

    #[test]
    fn crop_seam_provenance_follows_selected_nonempty_outputs() {
        fn output(text: &str, seam_corrected: bool) -> OcrOutput {
            OcrOutput {
                lines: if text.is_empty() {
                    Vec::new()
                } else {
                    vec![line(text)]
                },
                seam_corrected,
            }
        }

        let temp = tempfile::tempdir().unwrap();
        let path = temp.path().join("rect.pgm");
        let mut pgm = b"P5\n32 8\n255\n".to_vec();
        let mut pixels = vec![255u8; 32 * 8];
        for y in 2..6 {
            for x in [2..4, 14..16, 26..28].into_iter().flatten() {
                pixels[y * 32 + x] = 0;
            }
        }
        pgm.extend_from_slice(&pixels);

        for (primary_flag, first_flag, second_flag, empty_flag, expected) in [
            (true, false, false, false, true),
            (false, true, false, false, true),
            (false, false, true, false, true),
            (false, false, false, true, false),
            (false, false, false, false, false),
        ] {
            let mut engine = RectangleOcr {
                outputs: [
                    output("A", first_flag),
                    output("B", second_flag),
                    output("", empty_flag),
                ]
                .into(),
            };
            let candidate =
                recognize_ppocr_word_crops(&mut engine, &pgm, &path, "eng", None, Some(5))
                    .unwrap()
                    .unwrap();
            let primary = output("AB", primary_flag);
            let mut best = None;
            consider_crop_candidate(&mut best, &primary, candidate, 0.8);
            let selected = best.unwrap().1;
            assert_eq!(selected.lines[0].text, "A B");
            assert_eq!(selected.seam_corrected, expected);
        }

        let primary = output("AB", false);
        let mut best = None;
        consider_crop_candidate(&mut best, &primary, output("X Y", true), 0.99);
        consider_crop_candidate(&mut best, &primary, output("A B", false), 0.8);
        consider_crop_candidate(&mut best, &primary, output("A B", true), 0.7);
        assert!(!best.unwrap().1.seam_corrected);

        let mut best = None;
        consider_crop_candidate(&mut best, &primary, output("A B", true), 0.7);
        consider_crop_candidate(&mut best, &primary, output("A B", false), 0.8);
        assert!(!best.unwrap().1.seam_corrected);
    }

    #[test]
    fn training_export_skips_seam_corrections_without_leaking_to_later_output() {
        use std::cell::Cell;

        let corrected = OcrOutput {
            seam_corrected: true,
            ..OcrOutput::default()
        };
        let ordinary = OcrOutput::default();
        let exports = Cell::new(0);
        export_training_sample_unless_recovered(Some(&corrected), false, |_| {
            panic!("corrected output must not be exported")
        })
        .unwrap();
        export_training_sample_unless_recovered(Some(&ordinary), true, |_| {
            panic!("punctuation recovery must not be exported")
        })
        .unwrap();
        export_training_sample_unless_recovered(Some(&ordinary), false, |_| {
            exports.set(exports.get() + 1);
            Ok(())
        })
        .unwrap();
        assert_eq!(exports.get(), 1);
        assert!(
            export_training_sample_unless_recovered(Some(&ordinary), false, |_| {
                Err(anyhow!("export failure"))
            })
            .is_err()
        );
        export_training_sample_unless_recovered(Some(&corrected), false, |_| {
            Err(anyhow!("must not be called"))
        })
        .unwrap();
    }

    #[test]
    fn pgs_clear_events_preserve_gaps_in_srt_and_ass() {
        for format in [OcrFormat::Srt, OcrFormat::Ass] {
            let mut cues = Vec::new();
            let mut timing = PgsCueTiming::default();
            for (start, clear) in [(317, 2652), (3186, 8124)] {
                process_text_event(
                    start,
                    u32::MAX,
                    "line",
                    ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
                    format,
                    &mut cues,
                    &mut timing,
                );
                process_event(
                    &subtitle(Some(clear), 0, u32::MAX),
                    None,
                    0,
                    ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
                    format,
                    &mut cues,
                    &mut timing,
                );
            }
            process_text_event(
                9326,
                u32::MAX,
                "last",
                ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
                format,
                &mut cues,
                &mut timing,
            );
            timing.finish(&mut cues);
            sanitize_cues(&mut cues, format);
            assert_eq!(
                cues.iter()
                    .map(|c| (c.start_ms, c.end_ms))
                    .collect::<Vec<_>>(),
                [(317, 2652), (3186, 8124), (9326, 10326)]
            );
        }
    }

    #[test]
    fn pgs_closes_all_ass_cues_on_replacement_even_when_ocr_is_empty() {
        let mut cues = Vec::new();
        let mut timing = PgsCueTiming::default();
        timing.append(
            &mut cues,
            vec![cue(317, i64::MAX, "one"), cue(317, i64::MAX, "two")],
            Some(317),
            true,
            0,
        );
        timing.append(&mut cues, Vec::new(), Some(2652), true, 0);
        timing.finish(&mut cues);
        sanitize_cues(&mut cues, OcrFormat::Ass);
        assert_eq!(cues.len(), 2);
        assert!(cues.iter().all(|c| c.end_ms == 2652));
    }

    #[test]
    fn pgs_replacement_and_finite_durations_are_handled() {
        let mut cues = Vec::new();
        let mut timing = PgsCueTiming::default();
        process_text_event(
            1000,
            u32::MAX,
            "one",
            ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
            OcrFormat::Ass,
            &mut cues,
            &mut timing,
        );
        process_text_event(
            2500,
            800,
            "two",
            ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
            OcrFormat::Ass,
            &mut cues,
            &mut timing,
        );
        process_text_event(
            4000,
            3000,
            "three",
            ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
            OcrFormat::Ass,
            &mut cues,
            &mut timing,
        );
        process_event(
            &subtitle(Some(5000), 0, u32::MAX),
            None,
            0,
            ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
            OcrFormat::Ass,
            &mut cues,
            &mut timing,
        );
        timing.finish(&mut cues);
        assert_eq!(
            cues.iter()
                .map(|c| (c.start_ms, c.end_ms))
                .collect::<Vec<_>>(),
            [(1000, 2500), (2500, 3300), (4000, 5000)]
        );
    }

    #[test]
    fn pgs_missing_or_non_monotonic_boundary_uses_finite_fallback() {
        let mut cues = Vec::new();
        let mut timing = PgsCueTiming::default();
        process_event(
            &subtitle(Some(10), 0, u32::MAX),
            None,
            0,
            ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
            OcrFormat::Srt,
            &mut cues,
            &mut timing,
        );
        timing.append(
            &mut cues,
            vec![cue(1000, i64::MAX, "one")],
            Some(1000),
            true,
            2400,
        );
        process_event(
            &subtitle(None, 0, u32::MAX),
            None,
            0,
            ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
            OcrFormat::Srt,
            &mut cues,
            &mut timing,
        );
        assert_eq!(cues[0].end_ms, 3400);
        timing.append(
            &mut cues,
            vec![cue(5000, i64::MAX, "two")],
            Some(5000),
            true,
            0,
        );
        process_event(
            &subtitle(Some(4500), 0, u32::MAX),
            None,
            0,
            ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
            OcrFormat::Srt,
            &mut cues,
            &mut timing,
        );
        assert_eq!(cues[1].end_ms, 6000);
    }

    #[test]
    fn pgs_same_timestamp_clears_and_unclosed_fallback_is_bounded() {
        let mut cues = Vec::new();
        let mut timing = PgsCueTiming::default();
        timing.append(
            &mut cues,
            vec![cue(1000, i64::MAX, "one")],
            Some(1000),
            true,
            0,
        );
        timing.append(&mut cues, Vec::new(), Some(1000), true, 0);
        assert_eq!(cues[0].end_ms, 1000);
        timing.append(
            &mut cues,
            vec![cue(2000, i64::MAX, "two")],
            Some(2000),
            true,
            999_999,
        );
        timing.finish(&mut cues);
        assert_eq!(cues[1].end_ms, 12_000);
        sanitize_cues(&mut cues, OcrFormat::Ass);
        assert_eq!(cues.len(), 1);
    }

    #[test]
    fn pgs_timestamps_use_subtitle_pts_then_packet_fallback_and_display_offset() {
        assert_eq!(
            subtitle_start_ms(&subtitle(Some(1000), 125, u32::MAX), Some(2000)),
            Some(1125)
        );
        assert_eq!(
            subtitle_start_ms(&subtitle(None, 125, u32::MAX), Some(2000)),
            Some(2125)
        );
        assert_eq!(
            subtitle_start_ms(&subtitle(None, 125, u32::MAX), None),
            None
        );
        let mut cues = Vec::new();
        let mut timing = PgsCueTiming::default();
        timing.append(
            &mut cues,
            vec![cue(1000, i64::MAX, "one")],
            Some(1000),
            true,
            0,
        );
        process_event(
            &subtitle(None, 125, u32::MAX),
            Some(2000),
            0,
            ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE,
            OcrFormat::Ass,
            &mut cues,
            &mut timing,
        );
        assert_eq!(cues[0].end_ms, 2125);
    }

    #[test]
    fn non_pgs_events_keep_existing_timing() {
        let mut cues = Vec::new();
        let mut timing = PgsCueTiming::default();
        process_text_event(
            1000,
            5000,
            "one",
            ffi::AV_CODEC_ID_DVD_SUBTITLE,
            OcrFormat::Ass,
            &mut cues,
            &mut timing,
        );
        process_event(
            &subtitle(Some(2000), 0, 0),
            None,
            0,
            ffi::AV_CODEC_ID_DVD_SUBTITLE,
            OcrFormat::Ass,
            &mut cues,
            &mut timing,
        );
        timing.finish(&mut cues);
        assert_eq!(cues[0].end_ms, 6000);
    }

    #[test]
    fn postprocessed_quality_fallback_flags_glued_space_language_text() {
        let output = text_output;
        assert!(!postprocessed_text_needs_quality_fallback(
            &output("I will see you tomorrow."),
            "eng"
        ));
        // Probabilistic deglue (another test may enable it) would split the
        // glued text before the check sees it.
        assert!(
            crate::subtitle_ocr::text_processing::with_probabilistic_deglue(false, || {
                postprocessed_text_needs_quality_fallback(
                    &output("Iwillseeyoutomorrowmorning"),
                    "eng",
                )
            })
        );
        assert!(!postprocessed_text_needs_quality_fallback(
            &output("Iwillseeyoutomorrowmorning"),
            "jpn"
        ));
        assert!(!postprocessed_text_needs_quality_fallback(
            &output("   "),
            "eng"
        ));
    }
}
