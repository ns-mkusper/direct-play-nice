use super::*;
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

    if let Some(candidate) = best {
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

pub(super) fn run_tesseract_with_psm(image_path: &Path, language: &str, psm: u8) -> Result<String> {
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
        upscale_grayscale_nearest(&raster, width, height, 2)
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

pub(super) fn merge_ocr_lines_with_spacing(lines: Vec<OcrLine>) -> Vec<OcrLine> {
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
                if (gap as f32) > space_threshold {
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
