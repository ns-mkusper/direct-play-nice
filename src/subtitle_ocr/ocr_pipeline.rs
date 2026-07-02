//! Per-stream OCR pipeline execution.
//!
//! This module handles bitmap subtitle packet decode, rectangle extraction,
//! OCR invocation, and cue assembly for a single subtitle stream.

use super::*;
pub(super) fn discover_candidates(
    input_file: &CStr,
    sub_mode: SubMode,
) -> Result<Vec<SubtitleCandidate>> {
    let ictx = AVFormatContextInput::open(input_file)?;
    let mut out = Vec::new();

    for stream in ictx.streams() {
        let cp = stream.codecpar();
        if cp.codec_type != ffi::AVMEDIA_TYPE_SUBTITLE {
            continue;
        }
        if !is_image_based_subtitle(cp.codec_id) {
            continue;
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
    pub(super) ocr_preprocess: OcrPreprocess,
    pub(super) video_dimensions: Option<(u32, u32)>,
    pub(super) ocr_engine: OcrEngine,
}

/// Per-packet/per-subtitle context used when turning decoded subtitle data into cues.
struct CueBuildParams<'a> {
    language: &'a str,
    stream_index: i32,
    packet_seq: usize,
    work_dir: &'a Path,
    ocr_format: OcrFormat,
    ocr_preprocess: OcrPreprocess,
    video_dimensions: Option<(u32, u32)>,
    ocr_engine: OcrEngine,
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
            let fallback_start_ms = timestamp_to_ms(src_pts, stream_time_base).unwrap_or(0);
            let fallback_dur_ms = timestamp_to_ms(src_dur, stream_time_base)
                .unwrap_or(0)
                .max(0);
            let cue_params = CueBuildParams {
                language: request.language,
                stream_index: request.stream_index,
                packet_seq,
                work_dir: request.work_dir,
                ocr_format: request.ocr_format,
                ocr_preprocess: request.ocr_preprocess,
                video_dimensions: request.video_dimensions,
                ocr_engine: request.ocr_engine,
            };
            let mut new_cues = subtitle_to_cues(
                subtitle.as_ptr(),
                fallback_start_ms,
                fallback_dur_ms,
                &cue_params,
                engine,
                &mut quality_baseline,
            )?;
            cues.append(&mut new_cues);
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
            ocr_preprocess: request.ocr_preprocess,
            video_dimensions: request.video_dimensions,
            ocr_engine: request.ocr_engine,
        };
        let mut new_cues = subtitle_to_cues(
            subtitle.as_ptr(),
            0,
            0,
            &cue_params,
            engine,
            &mut quality_baseline,
        )?;
        cues.append(&mut new_cues);
        packet_seq += 1;
    }

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

fn subtitle_to_cues(
    subtitle: *const ffi::AVSubtitle,
    fallback_start_ms: i64,
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
        fallback_start_ms
    };

    let start_ms = base_ms.saturating_add(sub.start_display_time as i64).max(0);
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

fn extract_subtitle_lines(
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

        let Some((mut pgm, has_visible_pixels)) = rect_to_pgm(rect, ocr_engine) else {
            continue;
        };
        had_imagery = had_imagery || has_visible_pixels;
        if !has_visible_pixels {
            continue;
        }

        if !matches!(
            params.ocr_preprocess,
            OcrPreprocess::OpenCv5CudaBasic | OcrPreprocess::OpenCv5CudaSubtitle
        ) {
            pgm = preprocess_ocr_pgm(&pgm, params.ocr_preprocess).with_context(|| {
                format!(
                    "preprocessing OCR frame for subtitle stream {} packet {} rect {}",
                    params.stream_index, params.packet_seq, i
                )
            })?;
        }

        let rect_color = dominant_color_from_rect(rect);
        let mut pgm_path = params.work_dir.join(format!(
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
        let force_tesseract_non_english = force_tesseract_non_english_enabled()
            && !is_english_language(params.language)
            && language_uses_spaces(params.language);
        let spacing_fallback_requested = ppocr_spacing_needs_fallback(&output.lines);
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
        if spacing_fallback_requested
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
                if segmented_quality > raw_postprocess_quality + 0.03 {
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
        let mut spacing_fallback_requested =
            output_needs_spacing_after_postprocess(&output, params.language);
        let postprocess_quality = output_quality_after_postprocess(&output, params.language);
        let quality_rescue_requested = postprocess_quality < 0.60
            && (ppocr_needs_quality_fallback(&output.lines, params.language)
                || postprocessed_text_needs_quality_fallback(&output, params.language));
        if (spacing_fallback_requested || quality_rescue_requested)
            && matches!(
                params.ocr_preprocess,
                OcrPreprocess::OpenCv5CudaBasic | OcrPreprocess::OpenCv5CudaSubtitle
            )
        {
            let rescue_reason = match (spacing_fallback_requested, quality_rescue_requested) {
                (true, true) => "spacing+quality",
                (true, false) => "spacing",
                (false, true) => "quality",
                (false, false) => "unknown",
            };
            if let Some(rescued) = try_opencv_cuda_ocr_rescue(
                engine,
                &pgm,
                &pgm_path,
                params.ocr_preprocess,
                params.language,
                ocr_engine,
                params.stream_index,
                params.packet_seq,
                i,
                rescue_reason,
                &output,
            )? {
                output = rescued.output;
                pgm_path = rescued.path;
                spacing_fallback_requested =
                    output_needs_spacing_after_postprocess(&output, params.language);
            }
        }
        let ppocr_text = lines_text_for_quality(&output.lines);
        let ppocr_quality = ocr_text_quality_score(&ppocr_text, params.language);
        let ppocr_confidence = ppocr_average_confidence(&output.lines).unwrap_or(0.0);
        quality_baseline.observe(ppocr_quality, ppocr_confidence, subtitle_start_ms);
        let thresholds = quality_fallback_thresholds(quality_baseline);
        let residual_quality_fallback_requested =
            ppocr_needs_quality_fallback(&output.lines, params.language)
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
        if matches!(params.ocr_engine, OcrEngine::PpOcrV4 | OcrEngine::PpOcrV3)
            && language_uses_spaces(params.language)
            && !disable_tesseract_quality_fallback()
            && (force_tesseract_non_english || should_try_quality_fallback)
        {
            if let Some(fallback_language) = resolve_tesseract_fallback_language(params.language) {
                match run_tesseract_best_effort(&pgm_path, &fallback_language) {
                    Ok(candidate) if !candidate.text.is_empty() => {
                        // For non-English streams, prefer language-specific Tesseract
                        // because the bundled PP-OCR recognizer is English-focused.
                        let candidate_has_spaces = candidate.text.split_whitespace().count() > 1;
                        let should_replace_with_tesseract = if force_tesseract_non_english {
                            true
                        } else if spacing_fallback_requested && candidate_has_spaces {
                            // Prefer a spaced Tesseract result when PP-OCR glues a
                            // space-using subtitle line into one long token. The
                            // generic quality score intentionally penalizes glue
                            // lightly for proper nouns, so do not require the
                            // normal min-gain threshold here.
                            candidate.quality + 0.10 >= ppocr_quality
                        } else {
                            let min_gain = tesseract_quality_fallback_min_gain();
                            candidate.quality >= ppocr_quality + min_gain
                                || (ppocr_confidence < 0.70
                                    && candidate.quality + 0.03 >= ppocr_quality)
                        };
                        if should_replace_with_tesseract {
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
                                text: candidate.text,
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
        if !keep_ocr_intermediates() {
            let _ = fs::remove_file(&pgm_path);
        }
        for mut line in output.lines {
            if let Some(bbox) = line.bbox.as_mut() {
                offset_bbox(bbox, rect.x, rect.y);
            }
            line.text = postprocess_ocr_text(&line.text, params.language);
            if line.color.is_none() {
                line.color = rect_color;
            }
            if !line.text.is_empty() {
                lines.push(line);
            }
        }
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

struct OcrPreprocessRescue {
    output: OcrOutput,
    path: PathBuf,
}

fn try_opencv_cuda_ocr_rescue(
    engine: &mut dyn SubtitleConverter,
    original_pgm: &[u8],
    original_pgm_path: &Path,
    mode: OcrPreprocess,
    language: &str,
    ocr_engine: OcrEngine,
    stream_index: i32,
    packet_seq: usize,
    rect_index: u32,
    rescue_reason: &str,
    baseline: &OcrOutput,
) -> Result<Option<OcrPreprocessRescue>> {
    debug_assert!(matches!(
        mode,
        OcrPreprocess::OpenCv5CudaBasic | OcrPreprocess::OpenCv5CudaSubtitle
    ));

    let baseline_quality = output_quality_after_postprocess(baseline, language);
    let baseline_spacing_bad = output_needs_spacing_after_postprocess(baseline, language);
    let processed_pgm = preprocess_ocr_pgm(original_pgm, mode).with_context(|| {
        format!(
            "running OpenCV 5 CUDA OCR rescue for subtitle stream {stream_index} packet {packet_seq} rect {rect_index}"
        )
    })?;
    let rescue_path = opencv_cuda_rescue_path(original_pgm_path);
    fs::write(&rescue_path, &processed_pgm).with_context(|| {
        format!(
            "writing OpenCV 5 CUDA OCR rescue frame {}",
            rescue_path.display()
        )
    })?;

    let mut candidate = engine.extract_lines(&rescue_path, language)?;
    let discarded = prune_impossible_geometry(&mut candidate.lines, language);
    if discarded > 0 {
        debug!(
            "{} OpenCV 5 CUDA rescue geometry pruning: discarded {} impossible OCR boxes for subtitle stream {} packet {} rect {}",
            ppocr_engine_label(ocr_engine),
            discarded,
            stream_index,
            packet_seq,
            rect_index
        );
    }

    let candidate_quality = output_quality_after_postprocess(&candidate, language);
    let candidate_spacing_bad = output_needs_spacing_after_postprocess(&candidate, language);
    let candidate_empty = candidate
        .lines
        .iter()
        .map(|line| postprocess_ocr_text(&line.text, language))
        .all(|text| text.trim().is_empty());
    let candidate_confidence = ppocr_average_confidence(&candidate.lines).unwrap_or(0.0);
    let use_candidate = should_use_opencv_cuda_rescue(
        baseline_quality,
        baseline_spacing_bad,
        candidate_quality,
        candidate_spacing_bad,
        candidate_empty,
    );

    if use_candidate {
        info!(
            "{} OpenCV 5 CUDA rescue: using preprocessed OCR for subtitle stream {} packet {} rect {} (reason={}, candidate_score={:.2}, baseline_score={:.2}, candidate_conf={:.2})",
            ppocr_engine_label(ocr_engine),
            stream_index,
            packet_seq,
            rect_index,
            rescue_reason,
            candidate_quality,
            baseline_quality,
            candidate_confidence
        );
        if !keep_ocr_intermediates() {
            let _ = fs::remove_file(original_pgm_path);
        }
        Ok(Some(OcrPreprocessRescue {
            output: candidate,
            path: rescue_path,
        }))
    } else {
        info!(
            "{} OpenCV 5 CUDA rescue rejected for subtitle stream {} packet {} rect {} (reason={}, candidate_score={:.2}, baseline_score={:.2}, candidate_spacing_bad={}, baseline_spacing_bad={}, candidate_conf={:.2})",
            ppocr_engine_label(ocr_engine),
            stream_index,
            packet_seq,
            rect_index,
            rescue_reason,
            candidate_quality,
            baseline_quality,
            candidate_spacing_bad,
            baseline_spacing_bad,
            candidate_confidence
        );
        if !keep_ocr_intermediates() {
            let _ = fs::remove_file(&rescue_path);
        }
        Ok(None)
    }
}

fn opencv_cuda_rescue_path(original_pgm_path: &Path) -> PathBuf {
    let stem = original_pgm_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("ocr");
    original_pgm_path.with_file_name(format!("{stem}-opencv5-cuda.pgm"))
}

fn should_use_opencv_cuda_rescue(
    baseline_quality: f32,
    baseline_spacing_bad: bool,
    candidate_quality: f32,
    candidate_spacing_bad: bool,
    candidate_empty: bool,
) -> bool {
    if candidate_empty || !baseline_quality.is_finite() || !candidate_quality.is_finite() {
        return false;
    }
    if baseline_spacing_bad
        && !candidate_spacing_bad
        && candidate_quality + 0.03 >= baseline_quality
    {
        return true;
    }
    candidate_quality >= baseline_quality + 0.08
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
        lines: merge_ocr_lines_with_spacing(lines),
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
mod tests {
    use super::*;

    #[test]
    fn opencv_cuda_rescue_accepts_spacing_fix_without_quality_loss() {
        assert!(should_use_opencv_cuda_rescue(
            0.82, true, 0.80, false, false
        ));
    }

    #[test]
    fn opencv_cuda_rescue_rejects_empty_or_worse_candidate() {
        assert!(!should_use_opencv_cuda_rescue(
            0.82, true, 0.79, false, true
        ));
        assert!(!should_use_opencv_cuda_rescue(
            0.82, true, 0.78, false, false
        ));
        assert!(!should_use_opencv_cuda_rescue(
            0.82, false, 0.86, true, false
        ));
    }

    #[test]
    fn opencv_cuda_rescue_accepts_large_quality_gain() {
        assert!(should_use_opencv_cuda_rescue(
            0.70, false, 0.78, true, false
        ));
    }
}
