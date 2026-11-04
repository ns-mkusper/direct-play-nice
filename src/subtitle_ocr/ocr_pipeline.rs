//! Per-stream OCR pipeline execution.
//!
//! This module handles bitmap subtitle packet decode, rectangle extraction,
//! OCR invocation, and cue assembly for a single subtitle stream.

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
    let mut packet_seq: usize = 0;
    let mut subtitle_packet_count: usize = 0;
    let mut decoded_subtitle_count: usize = 0;
    let mut decoded_rect_count: usize = 0;
    let mut decoded_image_rect_count: usize = 0;
    let mut quality_baseline = OcrQualityBaseline::default();

    loop {
        let mut packet = match ictx.read_packet()? {
            Some(pkt) => pkt,
            None => break,
        };

        if packet.stream_index != request.stream_index {
            continue;
        }
        subtitle_packet_count += 1;

        let src_pts = packet.pts;
        let src_dur = packet.duration;

        packet.rescale_ts(stream_time_base, decode_context.time_base);

        if let Some(subtitle) = decode_context.decode_subtitle(Some(&mut packet))? {
            decoded_subtitle_count += 1;
            let (rect_count, image_rect_count) = subtitle_rect_counts(subtitle.as_ptr());
            decoded_rect_count += rect_count;
            decoded_image_rect_count += image_rect_count;
            let fallback_start_ms = timestamp_to_ms(src_pts, stream_time_base);
            let fallback_dur_ms = timestamp_to_ms(src_dur, stream_time_base)
                .unwrap_or(0)
                .max(0);
            let cue_params = CueBuildParams {
                language: request.language,
                stream_index: request.stream_index,
                packet_seq,
                work_dir: request.work_dir,
                ocr_format: request.ocr_format,
                video_dimensions: request.video_dimensions,
                ocr_engine: request.ocr_engine,
            };
            append_subtitle_cues(
                subtitle.as_ptr(),
                fallback_start_ms,
                fallback_dur_ms,
                stream_codec_id,
                &cue_params,
                engine,
                &mut quality_baseline,
                &mut cues,
                &mut pgs_timing,
            )?;
            packet_seq += 1;
        }
    }

    loop {
        let Some(subtitle) = decode_context.decode_subtitle(None)? else {
            break;
        };
        decoded_subtitle_count += 1;
        let (rect_count, image_rect_count) = subtitle_rect_counts(subtitle.as_ptr());
        decoded_rect_count += rect_count;
        decoded_image_rect_count += image_rect_count;
        let cue_params = CueBuildParams {
            language: request.language,
            stream_index: request.stream_index,
            packet_seq,
            work_dir: request.work_dir,
            ocr_format: request.ocr_format,
            video_dimensions: request.video_dimensions,
            ocr_engine: request.ocr_engine,
        };
        append_subtitle_cues(
            subtitle.as_ptr(),
            None,
            0,
            stream_codec_id,
            &cue_params,
            engine,
            &mut quality_baseline,
            &mut cues,
            &mut pgs_timing,
        )?;
        packet_seq += 1;
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

fn subtitle_start_ms(subtitle: &ffi::AVSubtitle, fallback_start_ms: Option<i64>) -> Option<i64> {
    let base_ms = if subtitle.pts != ffi::AV_NOPTS_VALUE {
        Some(subtitle.pts / 1000)
    } else {
        fallback_start_ms
    }?;
    Some(
        base_ms
            .saturating_add(subtitle.start_display_time as i64)
            .max(0),
    )
}

#[derive(Default)]
struct PgsCueTiming {
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
    stream_codec_id: ffi::AVCodecID,
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
    if stream_codec_id == ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE && !subtitle.is_null() {
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

    let mut end_ms;
    let sub = unsafe { &*subtitle };

    let base_ms = if sub.pts != ffi::AV_NOPTS_VALUE {
        sub.pts / 1000
    } else {
        fallback_start_ms.unwrap_or(0)
    };

    let start_ms = subtitle_start_ms(sub, fallback_start_ms)
        .unwrap_or_else(|| (sub.start_display_time as i64).max(0));
    end_ms = base_ms
        .saturating_add(sub.end_display_time as i64)
        .max(start_ms);

    if end_ms <= start_ms {
        let dur = fallback_duration_ms.max(1_000);
        end_ms = start_ms.saturating_add(dur);
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

pub(super) fn extract_subtitle_lines(
    subtitle: &ffi::AVSubtitle,
    params: &CueBuildParams<'_>,
    subtitle_start_ms: i64,
    engine: &mut dyn SubtitleConverter,
    quality_baseline: &mut OcrQualityBaseline,
) -> Result<(Vec<OcrLine>, bool)> {
    let mut lines = Vec::new();
    let mut had_imagery = false;
    let ocr_engine = params.ocr_engine;

    if subtitle.num_rects == 0 || subtitle.rects.is_null() {
        return Ok((Vec::new(), false));
    }

    for i in 0..subtitle.num_rects {
        let rect_ptr = unsafe { *subtitle.rects.add(i as usize) };
        if rect_ptr.is_null() {
            continue;
        }
        let rect = unsafe { &*rect_ptr };

        if rect.type_ == ffi::SUBTITLE_TEXT && !rect.text.is_null() {
            let txt = unsafe { CStr::from_ptr(rect.text) }
                .to_string_lossy()
                .trim()
                .to_string();
            let txt = normalize_utf8_text(&txt);
            if !txt.is_empty() {
                lines.push(OcrLine {
                    text: txt,
                    bbox: None,
                    score: None,
                    color: None,
                    italic: false,
                });
            }
            continue;
        }

        if rect.type_ == ffi::SUBTITLE_ASS && !rect.ass.is_null() {
            let txt = unsafe { CStr::from_ptr(rect.ass) }
                .to_string_lossy()
                .trim()
                .to_string();
            let italic = txt.contains("\\i1");
            let color = parse_ass_color(&txt);
            let txt = normalize_utf8_text(&strip_ass_formatting(&txt));
            if !txt.is_empty() {
                lines.push(OcrLine {
                    text: txt,
                    bbox: None,
                    score: None,
                    color,
                    italic,
                });
            }
            continue;
        }

        let Some((pgm, has_visible_pixels)) = rect_to_pgm(rect, ocr_engine) else {
            continue;
        };
        had_imagery = had_imagery || has_visible_pixels;
        if !has_visible_pixels {
            continue;
        }

        let rect_color = dominant_color_from_rect(rect);
        let pgm_path = params.work_dir.join(format!(
            "ocr-s{}-p{}-r{}.pgm",
            params.stream_index, params.packet_seq, i
        ));
        fs::write(&pgm_path, &pgm)
            .with_context(|| format!("writing OCR frame {}", pgm_path.display()))?;

        let mut output = engine.extract_lines(&pgm_path, params.language)?;
        let discarded = prune_impossible_geometry(&mut output.lines, params.language);
        if discarded > 0 {
            debug!(
                "{} geometry pruning: discarded {} impossible OCR boxes for subtitle stream {} packet {} rect {}",
                ppocr_engine_label(ocr_engine),
                discarded,
                params.stream_index,
                params.packet_seq,
                i
            );
        }
        let raw_quality =
            ocr_text_quality_score(&lines_text_for_quality(&output.lines), params.language);
        let raw_confidence = ppocr_average_confidence(&output.lines).unwrap_or(0.0);
        quality_baseline.observe(raw_quality, raw_confidence, subtitle_start_ms);
        let force_tesseract_non_english = force_tesseract_non_english_enabled()
            && !is_english_language(params.language)
            && language_uses_spaces(params.language);
        let mut tesseract_attempt = None;
        let mut restored_spacing = false;
        let mut replaced_with_tesseract = false;
        if matches!(ocr_engine, OcrEngine::PpOcrV4 | OcrEngine::PpOcrV3)
            && language_uses_spaces(params.language)
            && (ppocr_spacing_needs_fallback(&output.lines)
                || postprocessed_text_needs_quality_fallback(&output, params.language))
            && !disable_tesseract_quality_fallback()
        {
            if let Some(fallback_language) = resolve_tesseract_fallback_language(params.language) {
                if let Ok(candidate) = cached_tesseract_attempt(&mut tesseract_attempt, || {
                    run_tesseract_best_effort(&pgm_path, &fallback_language)
                }) {
                    if let Some(transferred) = transfer_spacing_unless_forced(
                        &output.lines,
                        &candidate.text,
                        force_tesseract_non_english,
                    ) {
                        let bbox = output
                            .lines
                            .iter()
                            .filter_map(|line| line.bbox.as_ref())
                            .fold(None, |acc: Option<OcrBoundingBox>, b| match acc {
                                Some(mut current) => {
                                    current.left = current.left.min(b.left);
                                    current.right = current.right.max(b.right);
                                    current.top = current.top.min(b.top);
                                    current.bottom = current.bottom.max(b.bottom);
                                    Some(current)
                                }
                                None => Some(b.clone()),
                            });
                        let mut repaired_lines = Vec::new();
                        for candidate_line in transferred.lines() {
                            let has_glued_words = candidate_line.split_whitespace().any(|word| {
                                word.chars()
                                    .filter(|ch| ch.is_alphabetic() || *ch == '\'')
                                    .count()
                                    >= 9
                                    && word.chars().any(|ch| ch == '\'')
                                    || word.chars().filter(|ch| ch.is_alphabetic()).count() >= 12
                            });
                            let processed = postprocess_ocr_text(candidate_line, params.language);
                            if has_glued_words
                                && processed
                                    .chars()
                                    .filter(|ch| !ch.is_whitespace())
                                    .eq(candidate_line.chars().filter(|ch| !ch.is_whitespace()))
                                && preserves_existing_boundaries(candidate_line, &processed)
                            {
                                repaired_lines.push(processed);
                            } else {
                                repaired_lines.push(candidate_line.to_string());
                            }
                        }
                        output.lines = vec![OcrLine {
                            text: repaired_lines.join("\n"),
                            bbox,
                            score: None,
                            color: rect_color,
                            italic: false,
                        }];
                        restored_spacing =
                            !output_needs_spacing_after_postprocess(&output, params.language);
                        info!(
                            "{} spacing recovery: restored word boundaries using matching Tesseract text for subtitle stream {} packet {} rect {}",
                            ppocr_engine_label(ocr_engine), params.stream_index, params.packet_seq, i
                        );
                    }
                }
            }
        }
        let thresholds = quality_fallback_thresholds(quality_baseline);
        let spacing_fallback_requested = ppocr_spacing_needs_fallback(&output.lines);
        let quality_fallback_requested =
            ppocr_needs_quality_fallback(&output.lines, params.language);
        let raw_postprocess_quality = output_quality_after_postprocess(&output, params.language);
        let raw_postprocess_needs_spacing_fallback =
            output_needs_spacing_after_postprocess(&output, params.language);
        let word_segmentation_enabled = env::var("DPN_OCR_WORD_SEGMENTATION")
            .ok()
            .map(|v| {
                !matches!(
                    v.trim().to_ascii_lowercase().as_str(),
                    "0" | "false" | "no" | "off"
                )
            })
            .unwrap_or(true);
        if !restored_spacing
            && spacing_fallback_requested
            && raw_postprocess_needs_spacing_fallback
            && word_segmentation_enabled
        {
            if let Some(segmented) = try_ppocr_word_segmentation_recovery(
                engine,
                &pgm,
                &pgm_path,
                params.language,
                rect_color,
            )? {
                let segmented_quality =
                    output_quality_after_postprocess(&segmented, params.language);
                if segmented_quality > raw_postprocess_quality + 0.03
                    && crop_recovery_preserves_characters(&output, &segmented)
                {
                    info!(
                        "{} spacing recovery: split subtitle stream {} packet {} rect {} into {} PP-OCR word crops (segmented_score={:.2}, ppocr_score={:.2})",
                        ppocr_engine_label(ocr_engine),
                        params.stream_index,
                        params.packet_seq,
                        i,
                        segmented.lines.len(),
                        segmented_quality,
                        raw_postprocess_quality
                    );
                    output = segmented;
                }
            }
        }
        let spacing_fallback_requested =
            output_needs_spacing_after_postprocess(&output, params.language);
        let ppocr_text = lines_text_for_quality(&output.lines);
        let ppocr_quality = ocr_text_quality_score(&ppocr_text, params.language);
        let ppocr_postprocessed_text = postprocess_ocr_text(&ppocr_text, params.language);
        let ppocr_postprocessed_quality =
            ocr_text_quality_score(&ppocr_postprocessed_text, params.language);
        let ppocr_confidence = ppocr_average_confidence(&output.lines).unwrap_or(0.0);
        let residual_quality_fallback_requested = quality_fallback_requested
            || postprocessed_text_needs_quality_fallback(&output, params.language);
        let should_try_quality_fallback = if spacing_fallback_requested {
            // Severe PP-OCR glue can be consistent for an entire subtitle track,
            // so a dynamic baseline trained on the same bad output will not catch
            // it. Try Tesseract immediately when PP-OCR word segmentation cannot
            // recover spaces for a space-using language.
            true
        } else if residual_quality_fallback_requested {
            true
        } else {
            thresholds.is_some_and(|thresholds| {
                ppocr_quality < thresholds.quality || ppocr_confidence < thresholds.confidence
            })
        };
        if !restored_spacing
            && matches!(params.ocr_engine, OcrEngine::PpOcrV4 | OcrEngine::PpOcrV3)
            && language_uses_spaces(params.language)
            && !disable_tesseract_quality_fallback()
            && (force_tesseract_non_english
                || (should_try_quality_fallback && ppocr_confidence < 0.30))
        {
            if let Some(fallback_language) = resolve_tesseract_fallback_language(params.language) {
                match cached_tesseract_attempt(&mut tesseract_attempt, || {
                    run_tesseract_best_effort(&pgm_path, &fallback_language)
                }) {
                    Ok(candidate) if !candidate.text.is_empty() => {
                        // PP-OCR has language/profile routing for English, Latin,
                        // CJK, Japanese, Korean, and multilingual recognizers. Keep
                        // Tesseract as a low-confidence rescue path instead of
                        // replacing high-confidence PP-OCR output by language.
                        let candidate_has_spaces = candidate.text.split_whitespace().count() > 1;
                        let postprocess_repaired_spacing = ppocr_postprocessed_text != ppocr_text
                            && ppocr_postprocessed_text.split_whitespace().count() > 1;
                        let should_replace_with_tesseract = if force_tesseract_non_english {
                            true
                        } else if postprocess_repaired_spacing || spacing_fallback_requested {
                            // For glued subtitle text, keep PP-OCR and let DPN's word
                            // segmentation/post-processing repair spacing. VobSub
                            // outlines can make Tesseract confidently emit worse text
                            // (e.g. "That was" -> "The wes"), so do not replace on
                            // spacing failures alone.
                            false
                        } else {
                            // Prefer Tesseract only when it beats PP-OCR after DPN's
                            // own post-processing.
                            let min_gain = tesseract_quality_fallback_min_gain();
                            candidate_has_spaces
                                && ppocr_confidence < 0.30
                                && candidate.quality >= ppocr_postprocessed_quality + min_gain
                        };
                        if should_replace_with_tesseract {
                            replaced_with_tesseract = true;
                            let bbox: Option<OcrBoundingBox> = output
                                .lines
                                .iter()
                                .filter_map(|line| line.bbox.as_ref())
                                .fold(None, |acc, b| match acc {
                                    Some(mut current) => {
                                        current.left = current.left.min(b.left);
                                        current.right = current.right.max(b.right);
                                        current.top = current.top.min(b.top);
                                        current.bottom = current.bottom.max(b.bottom);
                                        Some(current)
                                    }
                                    None => Some(b.clone()),
                                });
                            output.lines = vec![OcrLine {
                                text: candidate.text.clone(),
                                bbox,
                                score: None,
                                color: None,
                                italic: false,
                            }];
                            if force_tesseract_non_english {
                                info!(
                                    "{} language fallback: using Tesseract({}) psm={} for non-English subtitle stream {} packet {} rect {} (tess_score={:.2}, ppocr_score={:.2}, ppocr_conf={:.2})",
                                    ppocr_engine_label(ocr_engine),
                                    fallback_language,
                                    candidate.psm,
                                    params.stream_index,
                                    params.packet_seq,
                                    i,
                                    candidate.quality,
                                    ppocr_quality,
                                    ppocr_confidence
                                );
                            } else {
                                info!(
                                    "{} quality fallback: using Tesseract({}) psm={} for subtitle stream {} packet {} rect {} (tess_score={:.2}, ppocr_score={:.2}, ppocr_conf={:.2})",
                                    ppocr_engine_label(ocr_engine),
                                    fallback_language,
                                    candidate.psm,
                                    params.stream_index,
                                    params.packet_seq,
                                    i,
                                    candidate.quality,
                                    ppocr_quality,
                                    ppocr_confidence
                                );
                            }
                        } else {
                            debug!(
                                "{} quality fallback skipped (insufficient gain): keeping model output for subtitle stream {} packet {} rect {} (tess_score={:.2}, ppocr_score={:.2}, ppocr_conf={:.2})",
                                ppocr_engine_label(ocr_engine),
                                params.stream_index,
                                params.packet_seq,
                                i,
                                candidate.quality,
                                ppocr_quality,
                                ppocr_confidence
                            );
                        }
                    }
                    Ok(_) => {}
                    Err(err) => {
                        warn!(
                            "{} quality fallback failed for subtitle stream {} packet {} rect {} (lang={}): {}",
                            ppocr_engine_label(ocr_engine),
                            params.stream_index,
                            params.packet_seq,
                            i,
                            fallback_language,
                            err
                        );
                    }
                }
            } else {
                warn!(
                    "{} quality fallback skipped (no Tesseract languages available) for subtitle stream {} packet {} rect {}",
                    ppocr_engine_label(ocr_engine),
                    params.stream_index,
                    params.packet_seq,
                    i
                );
            }
        }
        export_ocr_training_sample(&pgm_path, &output, params.language)?;
        let mut rect_lines = Vec::new();
        for mut line in output.lines {
            if let Some(bbox) = line.bbox.as_mut() {
                offset_bbox(bbox, rect.x, rect.y);
            }
            if !restored_spacing {
                line.text = postprocess_ocr_text(&line.text, params.language);
            }
            if line.color.is_none() {
                line.color = rect_color;
            }
            if !line.text.is_empty() {
                rect_lines.push(line);
            }
        }
        if !replaced_with_tesseract
            && !force_tesseract_non_english
            && matches!(ocr_engine, OcrEngine::PpOcrV4 | OcrEngine::PpOcrV3)
            && language_uses_spaces(params.language)
            && !disable_tesseract_quality_fallback()
            && needs_terminal_punctuation_candidate(&rect_lines)
        {
            if let Some(fallback_language) = resolve_tesseract_fallback_language(params.language) {
                if let Ok(candidate) = cached_tesseract_attempt(&mut tesseract_attempt, || {
                    run_tesseract_best_effort(&pgm_path, &fallback_language)
                }) {
                    if !recover_terminal_punctuation(&mut rect_lines, &candidate.text)
                        && quoted_candidate_preserves_glyphs(&rect_lines, &candidate.text)
                    {
                        let alternate_psm = if candidate.psm == 6 { 7 } else { 6 };
                        if let Ok(alternate) =
                            run_tesseract_with_psm(&pgm_path, &fallback_language, alternate_psm)
                        {
                            recover_balanced_quotes(&mut rect_lines, &candidate.text, &alternate);
                        }
                    }
                }
            }
        }
        if !keep_ocr_intermediates() {
            let _ = fs::remove_file(&pgm_path);
        }
        lines.extend(rect_lines);
    }

    Ok((lines, had_imagery))
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
        let processed = output
            .lines
            .iter()
            .map(|line| postprocess_ocr_text(&line.text, language))
            .map(|text| text.trim().to_string())
            .filter(|text| !text.is_empty())
            .collect::<Vec<_>>()
            .join(" ");
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
    let alpha_tokens = processed
        .split_whitespace()
        .flat_map(|part| part.split(|ch: char| !ch.is_ascii_alphabetic() && ch != '\''))
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>();
    !alpha_tokens
        .iter()
        .any(|tok| tok.chars().filter(|ch| ch.is_ascii_alphabetic()).count() >= 14)
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
    env::var("DPN_OCR_KEEP_INTERMEDIATES")
        .ok()
        .map(|v| {
            matches!(
                v.trim().to_ascii_lowercase().as_str(),
                "1" | "true" | "yes" | "on"
            )
        })
        .unwrap_or(false)
}

fn needs_terminal_punctuation_candidate(lines: &[OcrLine]) -> bool {
    let text = lines_text_for_quality(lines);
    text.chars().filter(|ch| !ch.is_whitespace()).count() >= 8
        && text
            .trim_end()
            .chars()
            .last()
            .is_some_and(char::is_alphanumeric)
}

// Only add a terminal mark when both recognizers agree on every other glyph.
// In particular, never transfer punctuation across a conflicting apostrophe,
// quote, or other pre-existing punctuation mark.
fn recover_terminal_punctuation(lines: &mut [OcrLine], candidate: &str) -> bool {
    if !needs_terminal_punctuation_candidate(lines) {
        return false;
    }
    let original: String = lines_text_for_quality(lines)
        .chars()
        .filter(|ch| !ch.is_whitespace())
        .collect();
    let mut proposed: Vec<char> = candidate.chars().filter(|ch| !ch.is_whitespace()).collect();
    let Some(mark @ ('.' | '?' | '!')) = proposed.pop() else {
        return false;
    };
    if !original.chars().eq(proposed) {
        return false;
    }
    if let Some(line) = lines
        .iter_mut()
        .rev()
        .find(|line| !line.text.trim().is_empty())
    {
        let end = line.text.trim_end().len();
        line.text.insert(end, mark);
        return true;
    }
    false
}

fn quoted_candidate_preserves_glyphs(lines: &[OcrLine], candidate: &str) -> bool {
    if lines.len() != 1 || lines[0].text.contains(['"', '\n', '\r']) {
        return false;
    }
    let original: String = lines[0]
        .text
        .chars()
        .filter(|ch| !ch.is_whitespace())
        .collect();
    let proposed: String = candidate.chars().filter(|ch| !ch.is_whitespace()).collect();
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
}

fn cached_tesseract_attempt(
    attempt: &mut Option<Result<TesseractCandidate>>,
    run: impl FnOnce() -> Result<TesseractCandidate>,
) -> &Result<TesseractCandidate> {
    attempt.get_or_insert_with(run)
}

fn transfer_spacing_unless_forced(
    lines: &[OcrLine],
    candidate: &str,
    force_tesseract: bool,
) -> Option<String> {
    if force_tesseract {
        None
    } else {
        transfer_agreed_spacing(lines, candidate)
    }
}

fn transfer_agreed_spacing(lines: &[OcrLine], candidate: &str) -> Option<String> {
    let flattened = lines_text_for_quality(lines);
    if spacing_candidate_preserves_characters(&flattened, candidate) {
        return Some(candidate.to_string());
    }
    let original = lines
        .iter()
        .map(|line| line.text.trim())
        .filter(|text| !text.is_empty())
        .collect::<Vec<_>>()
        .join("\n");
    let (original_chars, original_boundaries) = text_and_boundaries(&original);
    let (candidate_chars, candidate_boundaries) = text_and_boundaries(candidate);
    let left: Vec<char> = original_chars.chars().collect();
    let right: Vec<char> = candidate_chars.chars().collect();
    if left.is_empty() {
        return None;
    }
    let compatible = |a: char, b: char| a == b || matches!((a, b), ('I', '|') | ('l', '|'));
    if left.len() != right.len()
        || !original_boundaries.is_subset(&candidate_boundaries)
        || !left.iter().zip(&right).all(|(&a, &b)| compatible(a, b))
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
    let mut result = String::new();
    let mut index = 0;
    for ch in original.chars() {
        if !ch.is_whitespace() {
            if added.contains(&index) {
                result.push(' ');
            }
            index += 1;
        }
        result.push(ch);
    }
    Some(result)
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
    let mut result = String::new();
    let mut index = 0;
    for ch in original.chars() {
        if !ch.is_whitespace() {
            if added.contains(&index) {
                result.push(' ');
            }
            index += 1;
        }
        result.push(ch);
    }
    Some(result)
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

fn output_quality_after_postprocess(output: &OcrOutput, language: &str) -> f32 {
    let processed = output
        .lines
        .iter()
        .map(|line| postprocess_ocr_text(&line.text, language))
        .map(|text| text.trim().to_string())
        .filter(|text| !text.is_empty())
        .collect::<Vec<_>>()
        .join(" ");
    ocr_text_quality_score(&processed, language)
}

fn output_needs_spacing_after_postprocess(output: &OcrOutput, language: &str) -> bool {
    let lines = output
        .lines
        .iter()
        .map(|line| OcrLine {
            text: postprocess_ocr_text(&line.text, language),
            bbox: line.bbox.clone(),
            score: line.score,
            color: line.color,
            italic: line.italic,
        })
        .collect::<Vec<_>>();
    ppocr_spacing_needs_fallback(&lines) || ppocr_needs_quality_fallback(&lines, language)
}

fn postprocessed_text_needs_quality_fallback(output: &OcrOutput, language: &str) -> bool {
    if !language_uses_spaces(language) {
        return false;
    }
    let processed = output
        .lines
        .iter()
        .map(|line| postprocess_ocr_text(&line.text, language))
        .map(|text| text.trim().to_string())
        .filter(|text| !text.is_empty())
        .collect::<Vec<_>>();
    if processed.is_empty() {
        return false;
    }
    let mut suspicious = 0usize;
    for text in &processed {
        let alpha_tokens = text
            .split_whitespace()
            .flat_map(|part| part.split(|ch: char| !ch.is_ascii_alphabetic() && ch != '\''))
            .filter(|part| !part.is_empty())
            .collect::<Vec<_>>();
        let long_count = alpha_tokens
            .iter()
            .filter(|tok| tok.chars().filter(|ch| ch.is_ascii_alphabetic()).count() >= 12)
            .count();
        let camel_count = alpha_tokens
            .iter()
            .filter(|tok| {
                tok.as_bytes()
                    .windows(2)
                    .any(|pair| pair[0].is_ascii_lowercase() && pair[1].is_ascii_uppercase())
            })
            .count();
        let alpha_chars = text.chars().filter(|ch| ch.is_ascii_alphabetic()).count();
        let spaces = text.chars().filter(|ch| ch.is_whitespace()).count();
        let space_rate = spaces as f32 / text.len().max(1) as f32;
        let token_count = text.split_whitespace().count();
        let text_len = text.chars().count();
        let alpha_ratio = alpha_chars as f32 / text_len.max(1) as f32;
        let has_low_information_garbage = text_len <= 12 && alpha_chars <= 4 && alpha_ratio < 0.75;
        if alpha_chars >= 18 && space_rate < 0.06 {
            suspicious += 1;
        }
        if has_low_information_garbage
            || (token_count <= 2 && ocr_text_quality_score(text, language) < 0.70)
        {
            suspicious += 1;
        }
        if long_count >= 1 || camel_count >= 1 {
            suspicious += 1;
        }
    }
    suspicious > 0
}

fn try_ppocr_word_segmentation_recovery(
    engine: &mut dyn SubtitleConverter,
    pgm: &[u8],
    pgm_path: &Path,
    language: &str,
    color: Option<(u8, u8, u8)>,
) -> Result<Option<OcrOutput>> {
    let candidate_gaps = [None, Some(5usize), Some(8usize)];
    let mut best: Option<(f32, OcrOutput)> = None;
    for gap in candidate_gaps {
        let Some(candidate) =
            recognize_ppocr_word_crops(engine, pgm, pgm_path, language, color, gap)?
        else {
            continue;
        };
        let text = lines_text_for_quality(&candidate.lines);
        let quality = ocr_text_quality_score(&text, language);
        let replace = best
            .as_ref()
            .map(|(best_quality, _)| quality > *best_quality + 0.001)
            .unwrap_or(true);
        if replace {
            best = Some((quality, candidate));
        }
    }
    Ok(best.map(|(_, output)| output))
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
        if !keep_ocr_intermediates() {
            let _ = fs::remove_file(&crop_path);
        }
        let text = normalize_utf8_text(&lines_text_for_quality(&word_output.lines));
        if text.is_empty() {
            continue;
        }
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
    }))
}

pub(super) fn force_tesseract_non_english_enabled() -> bool {
    let enabled = env::var("DPN_OCR_FORCE_TESS_NON_ENGLISH")
        .ok()
        .map(|v| {
            let x = v.trim().to_ascii_lowercase();
            matches!(x.as_str(), "1" | "true" | "yes" | "on")
        })
        .unwrap_or(false);
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
mod pgs_timing_tests {
    use super::*;

    struct NoOcr;

    impl SubtitleConverter for NoOcr {
        fn extract_lines(&mut self, _: &Path, _: &str) -> Result<OcrOutput> {
            panic!("text and clear events must not invoke OCR")
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

    fn process_event(
        sub: &ffi::AVSubtitle,
        fallback_start: Option<i64>,
        fallback_duration: i64,
        codec: ffi::AVCodecID,
        format: OcrFormat,
        cues: &mut Vec<SubtitleCue>,
        timing: &mut PgsCueTiming,
    ) {
        let params = CueBuildParams {
            language: "eng",
            stream_index: 0,
            packet_seq: 0,
            work_dir: Path::new("."),
            ocr_format: format,
            video_dimensions: None,
            ocr_engine: OcrEngine::External,
        };
        append_subtitle_cues(
            sub,
            fallback_start,
            fallback_duration,
            codec,
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

    #[test]
    fn terminal_punctuation_recovery_requires_exact_glyph_agreement() {
        fn line(text: &str) -> OcrLine {
            OcrLine {
                text: text.to_string(),
                bbox: None,
                score: Some(0.9),
                color: None,
                italic: true,
            }
        }

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
    fn balanced_quote_recovery_requires_two_matching_recognitions() {
        fn line(text: &str) -> OcrLine {
            OcrLine {
                text: text.to_string(),
                bbox: None,
                score: None,
                color: None,
                italic: false,
            }
        }
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
    fn forced_tesseract_takes_precedence_over_agreed_spacing() {
        let lines = vec![OcrLine {
            text: "Bonjourtoutlemonde".to_string(),
            bbox: None,
            score: Some(0.9),
            color: None,
            italic: false,
        }];
        let candidate = "Bonjour tout le monde";
        assert_eq!(
            transfer_spacing_unless_forced(&lines, candidate, false).as_deref(),
            Some(candidate)
        );
        assert!(transfer_spacing_unless_forced(&lines, candidate, true).is_none());
    }

    #[test]
    fn tesseract_attempt_is_reused_for_success_empty_and_failure() {
        for result in [Some("Bonjour tout le monde"), Some(""), None] {
            let mut attempt = None;
            let mut calls = 0;
            let first = cached_tesseract_attempt(&mut attempt, || {
                calls += 1;
                match result {
                    Some(text) => Ok(TesseractCandidate {
                        text: text.to_string(),
                        psm: 6,
                        quality: 0.8,
                    }),
                    None => Err(anyhow!("recognition failed")),
                }
            });
            match result {
                Some(text) => assert_eq!(first.as_ref().unwrap().text, text),
                None => assert_eq!(
                    first.as_ref().err().unwrap().to_string(),
                    "recognition failed"
                ),
            }
            let second = cached_tesseract_attempt(&mut attempt, || {
                calls += 1;
                panic!("must reuse the first result")
            });
            match result {
                Some(text) => assert_eq!(second.as_ref().unwrap().text, text),
                None => assert_eq!(
                    second.as_ref().err().unwrap().to_string(),
                    "recognition failed"
                ),
            }
            assert_eq!(calls, 1);
        }
    }

    #[test]
    fn transfer_spacing_preserves_ambiguous_glyphs_and_lines() {
        fn line(text: &str) -> OcrLine {
            OcrLine {
                text: text.to_owned(),
                bbox: None,
                score: None,
                color: None,
                italic: false,
            }
        }
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
        fn output(text: &str) -> OcrOutput {
            OcrOutput {
                lines: vec![OcrLine {
                    text: text.to_owned(),
                    bbox: None,
                    score: None,
                    color: None,
                    italic: false,
                }],
            }
        }
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
}
