fn discover_candidates(input_file: &CStr, sub_mode: SubMode) -> Result<Vec<SubtitleCandidate>> {
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

fn probe_video_dimensions(input_file: &CStr) -> Option<(u32, u32)> {
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
struct OcrQualityBaseline {
    samples: usize,
    quality_sum: f32,
    confidence_sum: f32,
}

impl OcrQualityBaseline {
    const WINDOW_MS: i64 = 3 * 60 * 1_000;

    fn observe(&mut self, quality: f32, confidence: f32, timestamp_ms: i64) {
        if timestamp_ms < 0 || timestamp_ms > Self::WINDOW_MS {
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
struct OcrFallbackThresholds {
    quality: f32,
    confidence: f32,
}

#[allow(clippy::too_many_arguments)]
fn ocr_single_stream(
    input_path: &str,
    stream_index: i32,
    language: &str,
    work_dir: &Path,
    ocr_format: OcrFormat,
    video_dimensions: Option<(u32, u32)>,
    ocr_engine: OcrEngine,
    engine: &mut dyn SubtitleConverter,
) -> Result<Vec<SubtitleCue>> {
    let input_cstr = CString::new(input_path).context("input path has interior NUL")?;
    let mut ictx = AVFormatContextInput::open(input_cstr.as_c_str())?;

    let (stream_time_base, stream_codec_id) = ictx
        .streams()
        .iter()
        .find(|st| st.index == stream_index)
        .map(|st| (st.time_base, st.codecpar().codec_id))
        .ok_or_else(|| anyhow!("subtitle stream {} not found", stream_index))?;

    let decoder = AVCodec::find_decoder(stream_codec_id).ok_or_else(|| {
        anyhow!(
            "decoder unavailable for subtitle stream {} ({})",
            stream_index,
            codec_name(stream_codec_id)
        )
    })?;

    let mut decode_context = AVCodecContext::new(&decoder);
    let mut applied_codecpar = false;
    for st in ictx.streams() {
        if st.index == stream_index {
            decode_context.apply_codecpar(&st.codecpar())?;
            applied_codecpar = true;
            break;
        }
    }
    if !applied_codecpar {
        bail!(
            "subtitle stream {} codec parameters unavailable",
            stream_index
        );
    }
    decode_context.set_time_base(stream_time_base);
    decode_context.open(None)?;

    let mut cues = Vec::new();
    let mut packet_seq: usize = 0;
    let mut quality_baseline = OcrQualityBaseline::default();

    loop {
        let mut packet = match ictx.read_packet()? {
            Some(pkt) => pkt,
            None => break,
        };

        if packet.stream_index != stream_index {
            continue;
        }

        let src_pts = packet.pts;
        let src_dur = packet.duration;

        packet.rescale_ts(stream_time_base, decode_context.time_base);

        if let Some(subtitle) = decode_context.decode_subtitle(Some(&mut packet))? {
            let fallback_start_ms = timestamp_to_ms(src_pts, stream_time_base).unwrap_or(0);
            let fallback_dur_ms = timestamp_to_ms(src_dur, stream_time_base)
                .unwrap_or(0)
                .max(0);
            let mut new_cues = subtitle_to_cues(
                subtitle.as_ptr(),
                fallback_start_ms,
                fallback_dur_ms,
                language,
                stream_index,
                packet_seq,
                work_dir,
                ocr_format,
                video_dimensions,
                ocr_engine,
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
        let mut new_cues = subtitle_to_cues(
            subtitle.as_ptr(),
            0,
            0,
            language,
            stream_index,
            packet_seq,
            work_dir,
            ocr_format,
            video_dimensions,
            ocr_engine,
            engine,
            &mut quality_baseline,
        )?;
        cues.append(&mut new_cues);
        packet_seq += 1;
    }

    sanitize_cues(&mut cues, ocr_format);

    Ok(cues)
}

#[allow(clippy::too_many_arguments)]
fn subtitle_to_cues(
    subtitle: *const ffi::AVSubtitle,
    fallback_start_ms: i64,
    fallback_duration_ms: i64,
    language: &str,
    stream_index: i32,
    packet_seq: usize,
    work_dir: &Path,
    ocr_format: OcrFormat,
    video_dimensions: Option<(u32, u32)>,
    ocr_engine: OcrEngine,
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

    let (lines, had_imagery) = extract_subtitle_lines(
        sub,
        language,
        start_ms,
        stream_index,
        packet_seq,
        work_dir,
        ocr_engine,
        engine,
        quality_baseline,
    )?;
    if had_imagery && lines.is_empty() {
        warn!(
            "OCR produced empty text for subtitle stream {} at {} ms",
            stream_index, start_ms
        );
    }

    let mut cues = Vec::new();
    match ocr_format {
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
                    let (pos_x, pos_y) = ass_position_from_bbox(&bbox, video_dimensions);
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
    language: &str,
    subtitle_start_ms: i64,
    stream_index: i32,
    packet_seq: usize,
    work_dir: &Path,
    ocr_engine: OcrEngine,
    engine: &mut dyn SubtitleConverter,
    quality_baseline: &mut OcrQualityBaseline,
) -> Result<(Vec<OcrLine>, bool)> {
    let mut lines = Vec::new();
    let mut had_imagery = false;

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
        let pgm_path = work_dir.join(format!("ocr-s{}-p{}-r{}.pgm", stream_index, packet_seq, i));
        fs::write(&pgm_path, pgm)
            .with_context(|| format!("writing OCR frame {}", pgm_path.display()))?;

        let mut output = engine.extract_lines(&pgm_path, language)?;
        let discarded = prune_impossible_geometry(&mut output.lines, language);
        if discarded > 0 {
            debug!(
                "{} geometry pruning: discarded {} impossible OCR boxes for subtitle stream {} packet {} rect {}",
                ppocr_engine_label(ocr_engine),
                discarded,
                stream_index,
                packet_seq,
                i
            );
        }
        let ppocr_text = lines_text_for_quality(&output.lines);
        let ppocr_quality = ocr_text_quality_score(&ppocr_text, language);
        let ppocr_confidence = ppocr_average_confidence(&output.lines).unwrap_or(0.0);
        quality_baseline.observe(ppocr_quality, ppocr_confidence, subtitle_start_ms);
        let thresholds = quality_fallback_thresholds(quality_baseline);
        let force_tesseract_non_english = force_tesseract_non_english_enabled()
            && !is_english_language(language)
            && language_uses_spaces(language);
        let quality_fallback_requested = ppocr_needs_quality_fallback(&output.lines, language);
        let should_try_quality_fallback = quality_fallback_requested
            && thresholds.is_some_and(|thresholds| {
                ppocr_quality < thresholds.quality || ppocr_confidence < thresholds.confidence
            });
        if matches!(ocr_engine, OcrEngine::PpOcrV4 | OcrEngine::PpOcrV3)
            && language_uses_spaces(language)
            && !disable_tesseract_quality_fallback()
            && (force_tesseract_non_english || should_try_quality_fallback)
        {
            if let Some(fallback_language) = resolve_tesseract_fallback_language(language) {
                match run_tesseract_best_effort(&pgm_path, &fallback_language) {
                    Ok(candidate) if !candidate.text.is_empty() => {
                        // For non-English streams, prefer language-specific Tesseract
                        // because the bundled PP-OCR recognizer is English-focused.
                        let should_replace_with_tesseract = if force_tesseract_non_english {
                            true
                        } else {
                            let min_gain = tesseract_quality_fallback_min_gain();
                            candidate.quality >= ppocr_quality + min_gain
                                || (ppocr_confidence < 0.70
                                    && candidate.quality + 0.03 >= ppocr_quality)
                        };
                        if should_replace_with_tesseract
                        {
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
                                    stream_index,
                                    packet_seq,
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
                                    stream_index,
                                    packet_seq,
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
                                stream_index,
                                packet_seq,
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
                            stream_index,
                            packet_seq,
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
                    stream_index,
                    packet_seq,
                    i
                );
            }
        }
        let _ = fs::remove_file(&pgm_path);
        for mut line in output.lines {
            if let Some(bbox) = line.bbox.as_mut() {
                offset_bbox(bbox, rect.x, rect.y);
            }
            line.text = postprocess_ocr_text(&line.text, language);
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

fn force_tesseract_non_english_enabled() -> bool {
    env::var("DPN_OCR_FORCE_TESS_NON_ENGLISH")
        .ok()
        .map(|v| {
            let x = v.trim().to_ascii_lowercase();
            matches!(x.as_str(), "1" | "true" | "yes" | "on")
        })
        .unwrap_or(false)
}

fn tesseract_quality_fallback_min_gain() -> f32 {
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

fn quality_fallback_thresholds(baseline: &OcrQualityBaseline) -> Option<OcrFallbackThresholds> {
    const BASELINE_MIN_SAMPLES: usize = 12;
    const RELATIVE_DROP: f32 = 0.15;

    if baseline.samples < BASELINE_MIN_SAMPLES {
        return None;
    }

    let dynamic_quality = baseline.avg_quality()?.mul_add(1.0 - RELATIVE_DROP, 0.0);
    let dynamic_confidence = baseline
        .avg_confidence()?
        .mul_add(1.0 - RELATIVE_DROP, 0.0);

    Some(OcrFallbackThresholds {
        quality: dynamic_quality.clamp(0.0, 1.0),
        confidence: dynamic_confidence.clamp(0.0, 1.0),
    })
}
