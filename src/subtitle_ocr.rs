use anyhow::{anyhow, bail, Context, Result};
use log::{debug, info, warn};
use rsmpeg::avcodec::{AVCodec, AVCodecContext};
use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;
use std::collections::HashSet;
use std::env;
use std::ffi::{CStr, CString};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use crate::SubMode;

#[derive(Debug, Clone)]
pub struct OcrSubtitleTrack {
    pub language: String,
    pub srt_path: PathBuf,
}

#[derive(Debug, Clone)]
struct SubtitleCandidate {
    stream_index: i32,
    language_tag: Option<String>,
}

#[derive(Debug, Clone)]
struct SubtitleCue {
    start_ms: i64,
    end_ms: i64,
    text: String,
}

pub fn convert_bitmap_subtitles_to_srt(
    input_file: &CStr,
    work_dir: &Path,
    sub_mode: SubMode,
    default_language: Option<&str>,
) -> Result<Vec<OcrSubtitleTrack>> {
    if matches!(sub_mode, SubMode::Skip) {
        return Ok(Vec::new());
    }

    let candidates = discover_candidates(input_file, sub_mode)?;
    if candidates.is_empty() {
        return Ok(Vec::new());
    }

    let available_langs = list_tesseract_languages().context(
        "Failed to query Tesseract language packs. Install `tesseract-ocr` and required traineddata files.",
    )?;

    let input_path = input_file
        .to_str()
        .map_err(|_| anyhow!("Input path must be valid UTF-8 for OCR side pass"))?
        .to_string();
    let system_language = detect_system_ocr_language();

    let mut tracks = Vec::with_capacity(candidates.len());
    for candidate in candidates {
        let resolved_lang = resolve_ocr_language(
            candidate.language_tag.as_deref(),
            default_language,
            system_language.as_deref(),
            &available_langs,
        );
        let srt_path = work_dir.join(format!("stream-{}.srt", candidate.stream_index));
        let cue_count = ocr_single_stream(
            &input_path,
            candidate.stream_index,
            &resolved_lang,
            &srt_path,
            work_dir,
        )?;

        info!(
            "OCR subtitle stream {} -> '{}' ({} cues)",
            candidate.stream_index,
            srt_path.display(),
            cue_count
        );

        tracks.push(OcrSubtitleTrack {
            language: resolved_lang,
            srt_path,
        });
    }

    Ok(tracks)
}

pub fn mux_srt_tracks_into_mp4(output_file: &CStr, tracks: &[OcrSubtitleTrack]) -> Result<()> {
    if tracks.is_empty() {
        return Ok(());
    }

    ensure_ffmpeg_binary()?;

    let output_path = PathBuf::from(output_file.to_string_lossy().into_owned());
    let subtitle_offset = count_subtitle_streams(output_file)?;
    let tmp_out = output_path.with_extension("ocr.tmp.mp4");

    let mut cmd = Command::new("ffmpeg");
    cmd.arg("-y").arg("-i").arg(&output_path);
    for track in tracks {
        cmd.arg("-i").arg(&track.srt_path);
    }

    cmd.arg("-map")
        .arg("0:v?")
        .arg("-map")
        .arg("0:a?")
        .arg("-map")
        .arg("0:s?");

    for (i, _) in tracks.iter().enumerate() {
        cmd.arg("-map").arg(format!("{}:0", i + 1));
    }

    cmd.arg("-c:v")
        .arg("copy")
        .arg("-c:a")
        .arg("copy")
        .arg("-c:s")
        .arg("mov_text");

    for (i, track) in tracks.iter().enumerate() {
        cmd.arg(format!("-metadata:s:s:{}", subtitle_offset + i))
            .arg(format!("language={}", track.language));
    }

    cmd.arg(&tmp_out);

    let output = cmd
        .output()
        .context("failed to run ffmpeg subtitle remux")?;
    if !output.status.success() {
        bail!(
            "ffmpeg subtitle remux failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    fs::rename(&tmp_out, &output_path)
        .with_context(|| format!("replacing '{}' after subtitle remux", output_path.display()))?;

    Ok(())
}

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

fn ocr_single_stream(
    input_path: &str,
    stream_index: i32,
    language: &str,
    srt_path: &Path,
    work_dir: &Path,
) -> Result<usize> {
    let input_cstr = CString::new(input_path).context("input path has interior NUL")?;
    let mut ictx = AVFormatContextInput::open(input_cstr.as_c_str())?;

    let (stream_time_base, stream_codec_id) = ictx
        .streams()
        .into_iter()
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
            if let Some(cue) = subtitle_to_cue(
                subtitle.as_ptr(),
                fallback_start_ms,
                fallback_dur_ms,
                language,
                stream_index,
                packet_seq,
                work_dir,
            )? {
                cues.push(cue);
            }
            packet_seq += 1;
        }
    }

    loop {
        let Some(subtitle) = decode_context.decode_subtitle(None)? else {
            break;
        };
        if let Some(cue) = subtitle_to_cue(
            subtitle.as_ptr(),
            0,
            0,
            language,
            stream_index,
            packet_seq,
            work_dir,
        )? {
            cues.push(cue);
        }
        packet_seq += 1;
    }

    sanitize_cue_overlaps(&mut cues);
    write_srt(srt_path, &cues)?;

    Ok(cues.len())
}

fn subtitle_to_cue(
    subtitle: *const ffi::AVSubtitle,
    fallback_start_ms: i64,
    fallback_duration_ms: i64,
    language: &str,
    stream_index: i32,
    packet_seq: usize,
    work_dir: &Path,
) -> Result<Option<SubtitleCue>> {
    if subtitle.is_null() {
        return Ok(None);
    }

    let start_ms;
    let mut end_ms;
    let sub = unsafe { &*subtitle };

    let base_ms = if sub.pts != ffi::AV_NOPTS_VALUE {
        sub.pts / 1000
    } else {
        fallback_start_ms
    };

    start_ms = base_ms.saturating_add(sub.start_display_time as i64).max(0);
    end_ms = base_ms
        .saturating_add(sub.end_display_time as i64)
        .max(start_ms);

    if end_ms <= start_ms {
        let dur = fallback_duration_ms.max(1_000);
        end_ms = start_ms.saturating_add(dur);
    }

    let (text, had_imagery) =
        extract_subtitle_text(sub, language, stream_index, packet_seq, work_dir)?;
    if had_imagery && text.is_empty() {
        warn!(
            "OCR produced empty text for subtitle stream {} at {} ms",
            stream_index, start_ms
        );
    }
    if text.is_empty() {
        return Ok(None);
    }

    Ok(Some(SubtitleCue {
        start_ms,
        end_ms,
        text,
    }))
}

fn extract_subtitle_text(
    subtitle: &ffi::AVSubtitle,
    language: &str,
    stream_index: i32,
    packet_seq: usize,
    work_dir: &Path,
) -> Result<(String, bool)> {
    let mut lines = Vec::new();
    let mut had_imagery = false;

    if subtitle.num_rects == 0 || subtitle.rects.is_null() {
        return Ok((String::new(), false));
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
            if !txt.is_empty() {
                lines.push(txt);
            }
            continue;
        }

        if rect.type_ == ffi::SUBTITLE_ASS && !rect.ass.is_null() {
            let txt = unsafe { CStr::from_ptr(rect.ass) }
                .to_string_lossy()
                .trim()
                .to_string();
            if !txt.is_empty() {
                lines.push(strip_ass_formatting(&txt));
            }
            continue;
        }

        let Some((pgm, has_visible_pixels)) = rect_to_pgm(rect) else {
            continue;
        };
        had_imagery = had_imagery || has_visible_pixels;
        if !has_visible_pixels {
            continue;
        }

        let pgm_path = work_dir.join(format!("ocr-s{}-p{}-r{}.pgm", stream_index, packet_seq, i));
        fs::write(&pgm_path, pgm)
            .with_context(|| format!("writing OCR frame {}", pgm_path.display()))?;

        let text = run_tesseract(&pgm_path, language)?;
        let _ = fs::remove_file(&pgm_path);
        if !text.is_empty() {
            lines.push(text);
        }
    }

    let merged = lines
        .into_iter()
        .map(|line| normalize_utf8_text(&line))
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n");

    Ok((merged, had_imagery))
}

fn run_tesseract(image_path: &Path, language: &str) -> Result<String> {
    let output = Command::new("tesseract")
        .arg(image_path)
        .arg("stdout")
        .arg("-l")
        .arg(language)
        .arg("--psm")
        .arg("6")
        .output()
        .with_context(|| format!("running tesseract on '{}'", image_path.display()))?;

    if !output.status.success() {
        bail!(
            "tesseract OCR failed for '{}': {}",
            image_path.display(),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    Ok(normalize_utf8_text(&String::from_utf8_lossy(
        &output.stdout,
    )))
}

fn rect_to_pgm(rect: &ffi::AVSubtitleRect) -> Option<(Vec<u8>, bool)> {
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

    let mut out = Vec::with_capacity(width * height + 64);
    out.extend_from_slice(format!("P5\n{} {}\n255\n", width, height).as_bytes());

    let mut has_visible_pixels = false;

    for y in 0..height {
        let row = &pixels[y * stride..(y * stride + width)];
        for &idx in row {
            let value = if let Some(pal) = palette {
                let base = (idx as usize) * 4;
                let p0 = pal[base] as i32;
                let p1 = pal[base + 1] as i32;
                let p2 = pal[base + 2] as i32;
                let p3 = pal[base + 3] as i32;

                // Treat the strongest channel as alpha-like visibility signal and produce
                // a high-contrast mask for OCR.
                let alpha = p0.max(p1).max(p2).max(p3);
                if alpha > 24 {
                    has_visible_pixels = true;
                    255u8
                } else {
                    0u8
                }
            } else if idx > 0 {
                has_visible_pixels = true;
                255u8
            } else {
                0u8
            };
            out.push(value);
        }
    }

    Some((out, has_visible_pixels))
}

fn write_srt(path: &Path, cues: &[SubtitleCue]) -> Result<()> {
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

fn sanitize_cue_overlaps(cues: &mut Vec<SubtitleCue>) {
    cues.sort_by_key(|cue| cue.start_ms);

    for i in 0..cues.len().saturating_sub(1) {
        if cues[i].end_ms > cues[i + 1].start_ms {
            cues[i].end_ms = cues[i + 1].start_ms;
        }
    }

    cues.retain(|cue| cue.end_ms > cue.start_ms && !cue.text.trim().is_empty());
}

fn format_srt_timestamp(total_ms: i64) -> String {
    let ms = total_ms.max(0);
    let hours = ms / 3_600_000;
    let minutes = (ms % 3_600_000) / 60_000;
    let seconds = (ms % 60_000) / 1_000;
    let millis = ms % 1_000;
    format!("{:02}:{:02}:{:02},{:03}", hours, minutes, seconds, millis)
}

fn normalize_utf8_text(input: &str) -> String {
    input
        .replace('\r', "")
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .collect::<Vec<_>>()
        .join("\n")
}

fn strip_ass_formatting(ass: &str) -> String {
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

fn timestamp_to_ms(value: i64, time_base: ffi::AVRational) -> Option<i64> {
    if value == ffi::AV_NOPTS_VALUE || time_base.num <= 0 || time_base.den <= 0 {
        return None;
    }
    Some(unsafe { ffi::av_rescale_q(value, time_base, ffi::AVRational { num: 1, den: 1000 }) })
}

fn extract_language_tag_from_metadata(dict: &rsmpeg::avutil::AVDictionary) -> Option<String> {
    for entry in dict.iter() {
        if entry
            .key()
            .to_string_lossy()
            .eq_ignore_ascii_case("language")
        {
            let v = entry.value().to_string_lossy().trim().to_string();
            if !v.is_empty() {
                return Some(v);
            }
        }
    }
    None
}

fn resolve_ocr_language(
    tag: Option<&str>,
    default_lang: Option<&str>,
    system_lang: Option<&str>,
    available: &HashSet<String>,
) -> String {
    let mapped = tag
        .and_then(map_language_tag_to_tesseract)
        .filter(|code| available.contains(code));

    if let Some(code) = mapped {
        return code;
    }

    if let Some(configured) = default_lang
        .and_then(map_language_tag_to_tesseract)
        .filter(|code| available.contains(code))
    {
        return configured;
    }

    if let Some(system) = system_lang
        .and_then(map_language_tag_to_tesseract)
        .filter(|code| available.contains(code))
    {
        return system;
    }

    if available.contains("eng") {
        return "eng".to_string();
    }

    available
        .iter()
        .next()
        .cloned()
        .unwrap_or_else(|| "eng".to_string())
}

fn detect_system_ocr_language() -> Option<String> {
    for var in ["LC_ALL", "LC_MESSAGES", "LANG"] {
        if let Some(raw) = env::var_os(var) {
            let val = raw.to_string_lossy().trim().to_string();
            if val.is_empty() {
                continue;
            }
            let normalized = val
                .split('.')
                .next()
                .unwrap_or(&val)
                .split('@')
                .next()
                .unwrap_or(&val)
                .trim()
                .to_string();
            if !normalized.is_empty() {
                return Some(normalized);
            }
        }
    }
    None
}

fn map_language_tag_to_tesseract(input: &str) -> Option<String> {
    let normalized = input.trim().to_ascii_lowercase();
    if normalized.is_empty() {
        return None;
    }

    let primary = normalized
        .split(['-', '_'])
        .next()
        .unwrap_or(&normalized)
        .trim();

    let mapped = match primary {
        "en" | "eng" => "eng",
        "es" | "spa" => "spa",
        "fr" | "fra" | "fre" => "fra",
        "de" | "deu" | "ger" => "deu",
        "it" | "ita" => "ita",
        "pt" | "por" => "por",
        "nl" | "nld" | "dut" => "nld",
        "sv" | "swe" => "swe",
        "no" | "nor" => "nor",
        "da" | "dan" => "dan",
        "fi" | "fin" => "fin",
        "pl" | "pol" => "pol",
        "cs" | "ces" | "cze" => "ces",
        "hu" | "hun" => "hun",
        "ro" | "ron" | "rum" => "ron",
        "tr" | "tur" => "tur",
        "el" | "ell" | "gre" => "ell",
        "ru" | "rus" => "rus",
        "uk" | "ukr" => "ukr",
        "ar" | "ara" => "ara",
        "he" | "heb" => "heb",
        "hi" | "hin" => "hin",
        "th" | "tha" => "tha",
        "vi" | "vie" => "vie",
        "id" | "ind" => "ind",
        "ja" | "jpn" => "jpn",
        "ko" | "kor" => "kor",
        "zh" | "zho" | "chi" => "chi_sim",
        _ => primary,
    };

    Some(mapped.to_string())
}

fn list_tesseract_languages() -> Result<HashSet<String>> {
    let output = Command::new("tesseract")
        .arg("--list-langs")
        .output()
        .context("failed to run tesseract --list-langs")?;

    if !output.status.success() {
        bail!(
            "tesseract --list-langs failed: {}",
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let stdout = String::from_utf8_lossy(&output.stdout);
    let langs = stdout
        .lines()
        .map(str::trim)
        .filter(|line| !line.is_empty())
        .filter(|line| {
            !line
                .to_ascii_lowercase()
                .starts_with("list of available languages")
        })
        .map(|line| line.to_string())
        .collect::<HashSet<_>>();

    if langs.is_empty() {
        bail!("tesseract reports no installed OCR languages")
    }

    debug!("Detected {} Tesseract language packs", langs.len());
    Ok(langs)
}

fn ensure_ffmpeg_binary() -> Result<()> {
    let output = Command::new("ffmpeg")
        .arg("-version")
        .output()
        .context("failed to execute ffmpeg")?;
    if !output.status.success() {
        bail!("ffmpeg is required for subtitle remuxing");
    }
    Ok(())
}

fn count_subtitle_streams(path: &CStr) -> Result<usize> {
    let ictx = AVFormatContextInput::open(path)?;
    Ok(ictx
        .streams()
        .into_iter()
        .filter(|st| st.codecpar().codec_type == ffi::AVMEDIA_TYPE_SUBTITLE)
        .count())
}

fn codec_name(codec_id: ffi::AVCodecID) -> String {
    unsafe {
        CStr::from_ptr(ffi::avcodec_get_name(codec_id))
            .to_string_lossy()
            .into_owned()
    }
}

fn is_image_based_subtitle(codec_id: ffi::AVCodecID) -> bool {
    matches!(
        codec_id,
        ffi::AV_CODEC_ID_HDMV_PGS_SUBTITLE
            | ffi::AV_CODEC_ID_DVD_SUBTITLE
            | ffi::AV_CODEC_ID_DVB_SUBTITLE
            | ffi::AV_CODEC_ID_XSUB
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn overlap_sanitization_truncates_earlier_block() {
        let mut cues = vec![
            SubtitleCue {
                start_ms: 1000,
                end_ms: 3000,
                text: "a".to_string(),
            },
            SubtitleCue {
                start_ms: 2500,
                end_ms: 4000,
                text: "b".to_string(),
            },
        ];
        sanitize_cue_overlaps(&mut cues);
        assert_eq!(cues[0].end_ms, 2500);
        assert_eq!(cues[1].start_ms, 2500);
    }

    #[test]
    fn srt_timestamp_formats_correctly() {
        assert_eq!(format_srt_timestamp(0), "00:00:00,000");
        assert_eq!(format_srt_timestamp(3_723_004), "01:02:03,004");
    }

    #[test]
    fn language_mapping_handles_iso_tags() {
        assert_eq!(map_language_tag_to_tesseract("en"), Some("eng".to_string()));
        assert_eq!(
            map_language_tag_to_tesseract("eng"),
            Some("eng".to_string())
        );
        assert_eq!(
            map_language_tag_to_tesseract("pt-BR"),
            Some("por".to_string())
        );
        assert_eq!(map_language_tag_to_tesseract(""), None);
    }

    #[test]
    fn resolve_language_prefers_stream_metadata_then_config_then_system_then_english() {
        let available = ["eng", "spa", "fra"]
            .into_iter()
            .map(|s| s.to_string())
            .collect::<HashSet<_>>();

        assert_eq!(
            resolve_ocr_language(Some("spa"), Some("eng"), Some("fr_FR.UTF-8"), &available),
            "spa"
        );
        assert_eq!(
            resolve_ocr_language(None, Some("fra"), Some("en_US.UTF-8"), &available),
            "fra"
        );
        assert_eq!(
            resolve_ocr_language(None, None, Some("fr_FR.UTF-8"), &available),
            "fra"
        );
        assert_eq!(
            resolve_ocr_language(None, None, Some("zz_ZZ"), &available),
            "eng"
        );
    }
}
