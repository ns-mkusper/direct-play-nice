//! Subtitle remuxing utilities that merge OCR-generated text tracks back into output containers.
//!
//! Converts generated text subtitle files into container subtitle streams and
//! preserves language/default metadata in the output container.

use super::*;
use crate::ffmpeg_utils::write_container_header;
pub(super) struct PendingPacket {
    ts: i64,
    packet: AVPacket,
}

pub(super) struct SubtitleMuxer {
    input_ctx: AVFormatContextInput,
    input_stream_index: usize,
    input_time_base: ffi::AVRational,
    decode_ctx: AVCodecContext,
    encode_ctx: AVCodecContext,
    output_stream_index: i32,
    last_written_dts: Option<i64>,
}

impl SubtitleMuxer {
    fn collect_packets(&mut self, output_time_base: ffi::AVRational) -> Result<Vec<PendingPacket>> {
        let mut out = Vec::new();
        loop {
            let mut packet = match self.input_ctx.read_packet()? {
                Some(pkt) => pkt,
                None => break,
            };
            if packet.stream_index != self.input_stream_index as i32 {
                continue;
            }

            packet.rescale_ts(self.input_time_base, self.decode_ctx.time_base);

            if let Some(subtitle) = self.decode_ctx.decode_subtitle(Some(&mut packet))? {
                if let Some(encoded) = encode_subtitle_packet(
                    &mut self.encode_ctx,
                    &subtitle,
                    &packet,
                    self.output_stream_index,
                    output_time_base,
                    &mut self.last_written_dts,
                )? {
                    let ts = packet_ts(&encoded, output_time_base);
                    out.push(PendingPacket {
                        ts,
                        packet: encoded,
                    });
                }
            }
        }
        Ok(out)
    }
}

// Remuxing writes to a temp path first so partial outputs never clobber the
// destination. Windows cannot rename over an existing file, so remove it
// explicitly there before moving the temp file into place.
fn replace_output_file(tmp_out: &Path, output_path: &Path, context: &str) -> Result<()> {
    crate::staging::promote(tmp_out, output_path, context)
}

fn validate_matroska_copied_streams(
    input: &AVFormatContextInput,
    output_path: &CStr,
) -> Result<()> {
    let output =
        AVFormatContextInput::open(output_path).context("reopening Matroska remux output")?;
    crate::ffmpeg_utils::validate_matroska_metadata(input, &output)?;
    let mut indices = HashMap::<ffi::AVMediaType, usize>::new();
    for source in input.streams() {
        let source_codec = source.codecpar();
        let media_type = source_codec.codec_type;
        let ordinal = indices.entry(media_type).or_default();
        let output_streams = output.streams();
        let copied = output_streams
            .iter()
            .filter(|stream| stream.codecpar().codec_type == media_type)
            .nth(*ordinal)
            .ok_or_else(|| anyhow!("Matroska remux lost stream {}", source.index))?;
        *ordinal += 1;
        let copied_codec = copied.codecpar();
        let source_disposition = unsafe { (*source.as_ptr()).disposition };
        let copied_disposition = unsafe { (*copied.as_ptr()).disposition };
        if source_codec.codec_id != copied_codec.codec_id
            || source_disposition != copied_disposition
        {
            bail!(
                "Matroska remux cannot preserve codec or disposition of stream {}",
                source.index
            );
        }
        if media_type == ffi::AVMEDIA_TYPE_ATTACHMENT {
            if source_codec.extradata_size <= 0
                || source_codec.extradata.is_null()
                || copied_codec.extradata_size != source_codec.extradata_size
                || copied_codec.extradata.is_null()
            {
                bail!(
                    "Matroska remux cannot preserve attachment data for stream {}",
                    source.index
                );
            }
            // Codec parameter buffers are owned by the open input contexts.
            let length = source_codec.extradata_size as usize;
            let same = unsafe {
                std::slice::from_raw_parts(source_codec.extradata, length)
                    == std::slice::from_raw_parts(copied_codec.extradata, length)
            };
            if !same {
                bail!(
                    "Matroska remux changed attachment data for stream {}",
                    source.index
                );
            }
            for key in [cstr::cstr!("filename"), cstr::cstr!("mimetype")] {
                let value = |stream: &rsmpeg::avformat::AVStreamRef<'_>| {
                    stream.metadata().and_then(|metadata| {
                        metadata
                            .get(key, None, 0)
                            .map(|entry| entry.value().to_bytes().to_vec())
                    })
                };
                if value(source) != value(copied) {
                    bail!(
                        "Matroska remux changed attachment metadata for stream {}",
                        source.index
                    );
                }
            }
        }
    }
    Ok(())
}

pub fn remux_copy_streams(input_file: &CStr, output_file: &CStr) -> Result<()> {
    let output_path = PathBuf::from(output_file.to_string_lossy().into_owned());
    let output_extension = output_path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_ascii_lowercase())
        .unwrap_or_default();
    let is_mkv = matches!(output_extension.as_str(), "mkv" | "mka" | "mks");
    let tmp_out = crate::staging::path_for(&output_path, "ocr");

    let mut input_ctx = AVFormatContextInput::open(input_file)?;
    let tmp_cstr = CString::new(tmp_out.to_string_lossy().to_string())
        .context("output path has interior NUL")?;
    let mut output_ctx = AVFormatContextOutput::create(tmp_cstr.as_c_str())?;
    if is_mkv {
        crate::ffmpeg_utils::copy_matroska_metadata(&input_ctx, &mut output_ctx)?;
        // Keep copied A/V and newly generated OCR cues on the same timeline.
        unsafe {
            (*output_ctx.as_mut_ptr()).avoid_negative_ts = ffi::AVFMT_AVOID_NEG_TS_DISABLED as i32;
        }
    }

    let mut stream_index_map = Vec::with_capacity(input_ctx.streams().len());
    for stream in input_ctx.streams() {
        let mut out_stream = output_ctx.new_stream();
        out_stream.set_time_base(stream.time_base);
        let mut codecpar = stream.codecpar().clone();
        if is_mkv {
            unsafe {
                (*codecpar.as_mut_ptr()).codec_tag = 0;
            }
        }
        out_stream.set_codecpar(codecpar);
        out_stream.set_metadata(stream.metadata().as_deref().cloned());
        // Both streams are live and owned by their respective format contexts.
        unsafe {
            (*out_stream.as_mut_ptr()).disposition = (*stream.as_ptr()).disposition;
        }
        stream_index_map.push(out_stream.index);
    }

    write_container_header(&mut output_ctx)
        .context("failed to write output header for subtitle remux")?;

    loop {
        let mut packet = match input_ctx.read_packet()? {
            Some(pkt) => pkt,
            None => break,
        };
        let input_index = packet.stream_index as usize;
        let output_index = stream_index_map
            .get(input_index)
            .copied()
            .ok_or_else(|| anyhow!("stream index {} not mapped", input_index))?;

        let input_time_base = input_ctx.streams()[input_index].time_base;
        let output_time_base = output_ctx.streams()[output_index as usize].time_base;

        packet.set_stream_index(output_index);
        packet.rescale_ts(input_time_base, output_time_base);
        output_ctx.interleaved_write_frame(&mut packet)?;
    }

    output_ctx.write_trailer()?;
    drop(output_ctx);
    if is_mkv {
        if let Err(err) = validate_matroska_copied_streams(&input_ctx, tmp_cstr.as_c_str()) {
            crate::staging::discard(&tmp_out);
            return Err(err);
        }
    }
    // Input and destination can be the same file; close it before replacement.
    drop(input_ctx);

    replace_output_file(&tmp_out, &output_path, "container remux")?;

    Ok(())
}

pub fn mux_text_tracks_from(
    input_file: &CStr,
    output_file: &CStr,
    tracks: &[OcrSubtitleTrack],
) -> Result<()> {
    if tracks.is_empty() {
        return Ok(());
    }

    let output_path = PathBuf::from(output_file.to_string_lossy().into_owned());
    let output_extension = output_path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_ascii_lowercase())
        .unwrap_or_default();
    let tmp_out = crate::staging::path_for(&output_path, "ocr");

    let is_mp4 = matches!(output_extension.as_str(), "mp4" | "m4v");
    let is_mkv = matches!(output_extension.as_str(), "mkv" | "mka" | "mks");
    if is_mp4
        && tracks
            .iter()
            .any(|track| matches!(track.format, OcrFormat::Ass))
    {
        warn!(
            "ASS OCR output is being remuxed into MP4; formatting will be downgraded to mov_text"
        );
    }

    let mut input_ctx = AVFormatContextInput::open(input_file)?;
    let tmp_cstr = CString::new(tmp_out.to_string_lossy().to_string())
        .context("output path has interior NUL")?;
    let mut output_ctx = AVFormatContextOutput::create(tmp_cstr.as_c_str())?;
    if is_mkv {
        crate::ffmpeg_utils::copy_matroska_metadata(&input_ctx, &mut output_ctx)?;
        // Keep copied A/V and newly generated OCR cues on the same timeline.
        unsafe {
            (*output_ctx.as_mut_ptr()).avoid_negative_ts = ffi::AVFMT_AVOID_NEG_TS_DISABLED as i32;
        }
    }

    let mut stream_index_map = Vec::with_capacity(input_ctx.streams().len());
    for stream in input_ctx.streams() {
        let mut out_stream = output_ctx.new_stream();
        out_stream.set_time_base(stream.time_base);
        let mut codecpar = stream.codecpar().clone();
        if is_mkv {
            unsafe {
                (*codecpar.as_mut_ptr()).codec_tag = 0;
            }
        }
        out_stream.set_codecpar(codecpar);
        out_stream.set_metadata(stream.metadata().as_deref().cloned());
        // Both streams are live and owned by their respective format contexts.
        unsafe {
            (*out_stream.as_mut_ptr()).disposition = (*stream.as_ptr()).disposition;
        }
        stream_index_map.push(out_stream.index);
    }

    let mut subtitle_muxers = Vec::with_capacity(tracks.len());
    for track in tracks {
        subtitle_muxers.push(build_subtitle_muxer(
            track,
            &mut output_ctx,
            is_mp4,
            is_mkv,
        )?);
    }

    write_container_header(&mut output_ctx)
        .context("failed to write output header for subtitle remux")?;

    let mut pending = Vec::new();
    for muxer in subtitle_muxers.iter_mut() {
        let output_time_base = output_ctx.streams()[muxer.output_stream_index as usize].time_base;
        pending.extend(muxer.collect_packets(output_time_base)?);
    }
    pending.sort_by(|a, b| {
        a.ts.cmp(&b.ts)
            .then_with(|| a.packet.stream_index.cmp(&b.packet.stream_index))
    });

    let mut next_sub = 0usize;
    loop {
        let mut packet = match input_ctx.read_packet()? {
            Some(pkt) => pkt,
            None => break,
        };
        let input_index = packet.stream_index as usize;
        let output_index = stream_index_map
            .get(input_index)
            .copied()
            .ok_or_else(|| anyhow!("stream index {} not mapped", input_index))?;

        let input_time_base = input_ctx.streams()[input_index].time_base;
        let output_time_base = output_ctx.streams()[output_index as usize].time_base;

        packet.set_stream_index(output_index);
        packet.rescale_ts(input_time_base, output_time_base);

        let packet_ts = packet_ts(&packet, output_time_base);
        while next_sub < pending.len() && pending[next_sub].ts <= packet_ts {
            output_ctx.interleaved_write_frame(&mut pending[next_sub].packet)?;
            next_sub += 1;
        }

        output_ctx.interleaved_write_frame(&mut packet)?;
    }

    while next_sub < pending.len() {
        output_ctx.interleaved_write_frame(&mut pending[next_sub].packet)?;
        next_sub += 1;
    }

    output_ctx.write_trailer()?;
    drop(output_ctx);
    if is_mkv {
        if let Err(err) = validate_matroska_copied_streams(&input_ctx, tmp_cstr.as_c_str()) {
            crate::staging::discard(&tmp_out);
            return Err(err);
        }
    }
    // The converted input is often also the destination on the MP4 OCR path.
    drop(input_ctx);

    replace_output_file(&tmp_out, &output_path, "subtitle remux")?;

    Ok(())
}

pub(super) fn build_subtitle_muxer(
    track: &OcrSubtitleTrack,
    output_ctx: &mut AVFormatContextOutput,
    is_mp4: bool,
    is_mkv: bool,
) -> Result<SubtitleMuxer> {
    let input_cstr = CString::new(track.subtitle_path.to_string_lossy().to_string())
        .context("subtitle path has interior NUL")?;
    let input_ctx = AVFormatContextInput::open(input_cstr.as_c_str())?;

    let mut input_stream_index = None;
    for (idx, stream) in input_ctx.streams().iter().enumerate() {
        if stream.codecpar().codec_type == ffi::AVMEDIA_TYPE_SUBTITLE {
            input_stream_index = Some(idx);
            break;
        }
    }
    let input_stream_index =
        input_stream_index.ok_or_else(|| anyhow!("subtitle input has no subtitle stream"))?;
    let input_stream = &input_ctx.streams()[input_stream_index];
    let input_time_base = input_stream.time_base;

    let decoder = AVCodec::find_decoder(input_stream.codecpar().codec_id).ok_or_else(|| {
        anyhow!(
            "decoder unavailable for OCR subtitle input ({})",
            codec_name(input_stream.codecpar().codec_id)
        )
    })?;

    let mut decode_ctx = AVCodecContext::new(&decoder);
    decode_ctx.apply_codecpar(&input_stream.codecpar())?;
    decode_ctx.set_time_base(input_time_base);
    decode_ctx.open(None)?;

    let output_codec_id = select_subtitle_codec_id(track.format, is_mp4, is_mkv);
    let encoder = AVCodec::find_encoder(output_codec_id).ok_or_else(|| {
        anyhow!(
            "encoder unavailable for OCR subtitle output ({})",
            codec_name(output_codec_id)
        )
    })?;

    let mut encode_ctx = AVCodecContext::new(&encoder);
    let mut output_stream = output_ctx.new_stream();
    if let Some(metadata) = build_language_metadata(&track.language) {
        output_stream.set_metadata(Some(metadata));
    }

    set_subtitle_codec_par(&mut decode_ctx, &mut encode_ctx);
    encode_ctx.open(None)?;
    output_stream.set_codecpar(encode_ctx.extract_codecpar());
    output_stream.set_time_base(encode_ctx.time_base);

    Ok(SubtitleMuxer {
        input_ctx,
        input_stream_index,
        input_time_base,
        decode_ctx,
        encode_ctx,
        output_stream_index: output_stream.index,
        last_written_dts: None,
    })
}

pub(super) fn select_subtitle_codec_id(
    format: OcrFormat,
    is_mp4: bool,
    is_mkv: bool,
) -> ffi::AVCodecID {
    if is_mp4 {
        ffi::AV_CODEC_ID_MOV_TEXT
    } else if is_mkv {
        match format {
            OcrFormat::Ass => ffi::AV_CODEC_ID_ASS,
            OcrFormat::Srt => ffi::AV_CODEC_ID_SUBRIP,
        }
    } else {
        ffi::AV_CODEC_ID_MOV_TEXT
    }
}

pub(super) fn build_language_metadata(language: &str) -> Option<AVDictionary> {
    let key = CString::new("language").ok()?;
    let value = CString::new(language).ok()?;
    Some(AVDictionary::new(&key, &value, 0))
}

pub(super) fn set_subtitle_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
) {
    encode_context.set_time_base(decode_context.time_base);

    if decode_context.subtitle_header_size > 0 {
        let mut new_subtitle_header = vec![0u8; decode_context.subtitle_header_size as usize];
        new_subtitle_header.copy_from_slice(unsafe {
            std::slice::from_raw_parts(
                decode_context.subtitle_header,
                decode_context.subtitle_header_size as usize,
            )
        });

        unsafe {
            (*encode_context.as_mut_ptr()).subtitle_header =
                ffi::av_mallocz(new_subtitle_header.len()) as *mut _;
            (*encode_context.as_mut_ptr()).subtitle_header_size = new_subtitle_header.len() as i32;
            std::ptr::copy_nonoverlapping(
                new_subtitle_header.as_ptr(),
                (*encode_context.as_mut_ptr()).subtitle_header,
                new_subtitle_header.len(),
            );
        }
    }
}

pub(super) fn encode_subtitle_packet(
    encode_context: &mut AVCodecContext,
    subtitle: &rsmpeg::avcodec::AVSubtitle,
    packet: &AVPacket,
    output_stream_index: i32,
    output_time_base: ffi::AVRational,
    last_written_dts: &mut Option<i64>,
) -> Result<Option<AVPacket>> {
    const MAX_SUBTITLE_PACKET_SIZE: usize = 32 * 1024;
    let mut subtitle_buffer = vec![0u8; MAX_SUBTITLE_PACKET_SIZE];
    encode_context.encode_subtitle(subtitle, &mut subtitle_buffer)?;

    let encoded_size = subtitle_buffer
        .iter()
        .rposition(|&x| x != 0)
        .map(|pos| pos + 1)
        .unwrap_or(0);

    if encoded_size == 0 {
        return Ok(None);
    }

    let mut encoded_packet = AVPacket::new();
    unsafe {
        ffi::av_new_packet(encoded_packet.as_mut_ptr(), encoded_size as i32);
        std::ptr::copy_nonoverlapping(
            subtitle_buffer.as_ptr(),
            (*encoded_packet.as_mut_ptr()).data,
            encoded_size,
        );
    }

    let mut pts = if subtitle.pts != ffi::AV_NOPTS_VALUE {
        subtitle.pts
    } else {
        packet.pts
    };
    let mut dts = if packet.dts != ffi::AV_NOPTS_VALUE {
        packet.dts
    } else {
        pts
    };

    if pts == ffi::AV_NOPTS_VALUE {
        pts = last_written_dts.map(|prev| prev + 1).unwrap_or(0);
    }
    if dts == ffi::AV_NOPTS_VALUE {
        dts = pts;
    }

    encoded_packet.set_stream_index(output_stream_index);
    encoded_packet.set_pts(pts);
    encoded_packet.set_dts(dts);
    let mut duration = packet.duration;
    if duration == ffi::AV_NOPTS_VALUE || duration <= 0 {
        let display_ms = i64::from(subtitle.end_display_time)
            .saturating_sub(i64::from(subtitle.start_display_time));
        if display_ms > 0 {
            duration =
                unsafe { ffi::av_rescale_q(display_ms, ra(1, 1_000), encode_context.time_base) };
        }
    }
    if duration <= 0 || duration == ffi::AV_NOPTS_VALUE {
        duration = 1;
    }
    encoded_packet.set_duration(duration);
    encoded_packet.set_flags(packet.flags);

    encoded_packet.rescale_ts(encode_context.time_base, output_time_base);

    let packet_dts = encoded_packet.dts;
    if let Some(prev_dts) = *last_written_dts {
        if packet_dts <= prev_dts {
            let adjusted = prev_dts + 1;
            encoded_packet.set_dts(adjusted);
            if encoded_packet.pts < adjusted {
                encoded_packet.set_pts(adjusted);
            }
        }
    }
    *last_written_dts = Some(encoded_packet.dts);

    Ok(Some(encoded_packet))
}

pub(super) fn packet_ts(packet: &AVPacket, time_base: ffi::AVRational) -> i64 {
    let ts = if packet.pts != ffi::AV_NOPTS_VALUE {
        packet.pts
    } else if packet.dts != ffi::AV_NOPTS_VALUE {
        packet.dts
    } else {
        return 0;
    };
    unsafe { ffi::av_rescale_q(ts, time_base, ra(1, ffi::AV_TIME_BASE as i32)) }
}

#[cfg(test)]
mod tests {
    use super::replace_output_file;
    #[cfg(feature = "ffmpeg-cli-tests")]
    use super::{mux_text_tracks_from, remux_copy_streams, validate_matroska_copied_streams};
    #[cfg(feature = "ffmpeg-cli-tests")]
    use crate::subtitle_ocr::OcrSubtitleTrack;
    #[cfg(feature = "ffmpeg-cli-tests")]
    use crate::OcrFormat;
    #[cfg(feature = "ffmpeg-cli-tests")]
    use rsmpeg::{avformat::AVFormatContextInput, ffi};
    #[cfg(feature = "ffmpeg-cli-tests")]
    use std::ffi::CString;
    use std::fs;
    #[cfg(feature = "ffmpeg-cli-tests")]
    use std::io::Write;
    #[cfg(feature = "ffmpeg-cli-tests")]
    use std::path::Path;
    #[cfg(feature = "ffmpeg-cli-tests")]
    use std::process::Command;

    #[cfg(feature = "ffmpeg-cli-tests")]
    fn ensure_ffmpeg_present() {
        let out = Command::new("ffmpeg").arg("-version").output();
        match out {
            Ok(o) if o.status.success() => {}
            _ => panic!("ffmpeg CLI not found. Install ffmpeg and ensure it is on PATH."),
        }
    }

    #[cfg(feature = "ffmpeg-cli-tests")]
    fn write_test_video(path: &Path) {
        let status = Command::new("ffmpeg")
            .args([
                "-hide_banner",
                "-loglevel",
                "error",
                "-y",
                "-f",
                "lavfi",
                "-i",
                "testsrc=size=96x64:rate=10:duration=1",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=1000:sample_rate=44100:duration=1",
                "-pix_fmt",
                "yuv420p",
                "-c:v",
                "mpeg4",
                "-c:a",
                "mp2",
                "-shortest",
                path.to_str().expect("path utf8"),
            ])
            .status()
            .expect("run ffmpeg test video generation");
        assert!(status.success(), "ffmpeg test video generation failed");
    }

    #[cfg(feature = "ffmpeg-cli-tests")]
    fn write_srt(path: &Path) {
        let mut file = fs::File::create(path).expect("create srt");
        writeln!(
            file,
            "1\n00:00:00,000 --> 00:00:00,800\nreplacement subtitle\n"
        )
        .expect("write srt");
    }

    #[cfg(feature = "ffmpeg-cli-tests")]
    fn cstring_path(path: &Path) -> CString {
        CString::new(path.to_string_lossy().to_string()).expect("path cstring")
    }

    #[cfg(feature = "ffmpeg-cli-tests")]
    fn preserved_subtitle_and_attachment(path: &Path) -> (i32, Vec<u8>) {
        let context = AVFormatContextInput::open(cstring_path(path).as_c_str()).unwrap();
        let streams = context.streams();
        let subtitle = streams
            .iter()
            .find(|stream| stream.codecpar().codec_type == ffi::AVMEDIA_TYPE_SUBTITLE)
            .unwrap();
        let attachment = streams
            .iter()
            .find(|stream| stream.codecpar().codec_type == ffi::AVMEDIA_TYPE_ATTACHMENT)
            .unwrap();
        let disposition = unsafe { (*subtitle.as_ptr()).disposition };
        let codec = attachment.codecpar();
        assert!(codec.extradata_size > 0 && !codec.extradata.is_null());
        let bytes =
            unsafe { std::slice::from_raw_parts(codec.extradata, codec.extradata_size as usize) }
                .to_vec();
        (disposition, bytes)
    }

    #[cfg(feature = "ffmpeg-cli-tests")]
    #[test]
    fn remux_preserves_subtitle_disposition_and_font_attachment() {
        ensure_ffmpeg_present();
        let tmp = tempfile::tempdir().unwrap();
        let video = tmp.path().join("video.mkv");
        let input = tmp.path().join("input.mkv");
        let subtitles = tmp.path().join("input.srt");
        let ocr = tmp.path().join("ocr.srt");
        let font = tmp.path().join("font.ttf");
        let chapters = tmp.path().join("chapters.ffmeta");
        let copied = tmp.path().join("copied.mkv");
        let with_ocr = tmp.path().join("with-ocr.mkv");
        write_test_video(&video);
        write_srt(&subtitles);
        write_srt(&ocr);
        fs::write(&font, b"binary font attachment\0\x01\xff").unwrap();
        fs::write(&chapters, ";FFMETADATA1\ntitle=Episode Title\nartist=Series Artist\n[CHAPTER]\nTIMEBASE=1/1000\nSTART=125\nEND=500\ntitle=Opening\n[CHAPTER]\nTIMEBASE=1/1000\nSTART=500\nEND=975\ntitle=Episode\n").unwrap();
        let result = Command::new("ffmpeg")
            .args(["-hide_banner", "-loglevel", "error", "-y", "-i"])
            .arg(&video)
            .arg("-i")
            .arg(&subtitles)
            .arg("-i")
            .arg(&chapters)
            .args([
                "-map_metadata",
                "2",
                "-map_chapters",
                "2",
                "-map",
                "0",
                "-map",
                "1:s",
                "-c",
                "copy",
                "-c:s",
                "srt",
                "-disposition:s:0",
                "default+forced",
                "-attach",
            ])
            .arg(&font)
            .args(["-metadata:s:t", "mimetype=application/x-truetype-font"])
            .arg(&input)
            .output()
            .unwrap();
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
        let expected = preserved_subtitle_and_attachment(&input);
        let source = AVFormatContextInput::open(cstring_path(&input).as_c_str()).unwrap();
        assert_eq!(source.nb_chapters, 2);
        assert_ne!(expected.0 & ffi::AV_DISPOSITION_DEFAULT as i32, 0);
        assert_ne!(expected.0 & ffi::AV_DISPOSITION_FORCED as i32, 0);
        assert_eq!(expected.1, fs::read(&font).unwrap());
        remux_copy_streams(
            cstring_path(&input).as_c_str(),
            cstring_path(&copied).as_c_str(),
        )
        .unwrap();
        assert_eq!(preserved_subtitle_and_attachment(&copied), expected);
        let copied_context = AVFormatContextInput::open(cstring_path(&copied).as_c_str()).unwrap();
        crate::ffmpeg_utils::validate_matroska_metadata(&source, &copied_context).unwrap();
        let mut altered = AVFormatContextInput::open(cstring_path(&copied).as_c_str()).unwrap();
        unsafe {
            let chapter = *(*altered.as_mut_ptr()).chapters;
            (*chapter).start += 1;
            assert!(format!(
                "{:#}",
                crate::ffmpeg_utils::validate_matroska_metadata(&source, &altered).unwrap_err()
            )
            .contains("timing"));
            (*chapter).start -= 1;
            assert_eq!(
                ffi::av_dict_set(
                    &mut (*chapter).metadata,
                    cstr::cstr!("title").as_ptr(),
                    cstr::cstr!("Wrong title").as_ptr(),
                    0
                ),
                0
            );
        }
        assert!(format!(
            "{:#}",
            crate::ffmpeg_utils::validate_matroska_metadata(&source, &altered).unwrap_err()
        )
        .contains("chapter 0 metadata"));
        let mut altered_format =
            AVFormatContextInput::open(cstring_path(&copied).as_c_str()).unwrap();
        unsafe {
            assert_eq!(
                ffi::av_dict_set(
                    &mut (*altered_format.as_mut_ptr()).metadata,
                    cstr::cstr!("title").as_ptr(),
                    cstr::cstr!("Wrong episode").as_ptr(),
                    0
                ),
                0
            );
        }
        assert!(format!(
            "{:#}",
            crate::ffmpeg_utils::validate_matroska_metadata(&source, &altered_format).unwrap_err()
        )
        .contains("format metadata"));
        let track = [OcrSubtitleTrack {
            language: "spa".into(),
            subtitle_path: ocr,
            format: OcrFormat::Srt,
        }];
        mux_text_tracks_from(
            cstring_path(&input).as_c_str(),
            cstring_path(&with_ocr).as_c_str(),
            &track,
        )
        .unwrap();
        assert_eq!(preserved_subtitle_and_attachment(&with_ocr), expected);
        let ocr_context = AVFormatContextInput::open(cstring_path(&with_ocr).as_c_str()).unwrap();
        crate::ffmpeg_utils::validate_matroska_metadata(&source, &ocr_context).unwrap();
        let no_chapters = tmp.path().join("no-chapters.mkv");
        let status = Command::new("ffmpeg")
            .args(["-hide_banner", "-loglevel", "error", "-y", "-i"])
            .arg(&input)
            .args(["-map", "0", "-map_chapters", "-1", "-c", "copy"])
            .arg(&no_chapters)
            .status()
            .unwrap();
        assert!(status.success());
        let error =
            validate_matroska_copied_streams(&source, cstring_path(&no_chapters).as_c_str())
                .unwrap_err();
        assert!(format!("{error:#}").contains("chapter count"));

        let lost_flags = tmp.path().join("lost-flags.mkv");
        let result = Command::new("ffmpeg")
            .args(["-hide_banner", "-loglevel", "error", "-y", "-i"])
            .arg(&input)
            .args(["-map", "0", "-c", "copy", "-disposition:s:0", "0"])
            .arg(&lost_flags)
            .output()
            .unwrap();
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
        assert_eq!(
            preserved_subtitle_and_attachment(&lost_flags).0 & ffi::AV_DISPOSITION_FORCED as i32,
            0
        );
        let input_context = AVFormatContextInput::open(cstring_path(&input).as_c_str()).unwrap();
        assert!(validate_matroska_copied_streams(
            &input_context,
            cstring_path(&lost_flags).as_c_str()
        )
        .is_err());
        let lost_font = tmp.path().join("lost-font.mkv");
        let result = Command::new("ffmpeg")
            .args(["-hide_banner", "-loglevel", "error", "-y", "-i"])
            .arg(&input)
            .args(["-map", "0:v", "-map", "0:a", "-map", "0:s", "-c", "copy"])
            .arg(&lost_font)
            .output()
            .unwrap();
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
        assert!(validate_matroska_copied_streams(
            &input_context,
            cstring_path(&lost_font).as_c_str()
        )
        .is_err());
        drop(input_context);

        // MP4 cannot represent this Matroska attachment; failure must not
        // overwrite either the original or an existing destination.
        let unsupported = tmp.path().join("unsupported.mp4");
        fs::write(&unsupported, b"existing output").unwrap();
        let font_only = tmp.path().join("font-only.mkv");
        let result = Command::new("ffmpeg")
            .args(["-hide_banner", "-loglevel", "error", "-y", "-i"])
            .arg(&video)
            .args(["-map", "0", "-c", "copy", "-attach"])
            .arg(&font)
            .args(["-metadata:s:t", "mimetype=application/x-truetype-font"])
            .arg(&font_only)
            .output()
            .unwrap();
        assert!(
            result.status.success(),
            "{}",
            String::from_utf8_lossy(&result.stderr)
        );
        let original = fs::read(&font_only).unwrap();
        assert!(remux_copy_streams(
            cstring_path(&font_only).as_c_str(),
            cstring_path(&unsupported).as_c_str()
        )
        .is_err());
        assert_eq!(fs::read(&unsupported).unwrap(), b"existing output");
        assert!(mux_text_tracks_from(
            cstring_path(&font_only).as_c_str(),
            cstring_path(&unsupported).as_c_str(),
            &track,
        )
        .is_err());
        assert_eq!(fs::read(&unsupported).unwrap(), b"existing output");
        assert_eq!(fs::read(&font_only).unwrap(), original);
    }

    #[test]
    fn replace_output_file_overwrites_existing_destination() {
        let tmp = tempfile::tempdir().expect("create tempdir");
        let tmp_out = tmp.path().join("out.tmp");
        let output = tmp.path().join("out.mp4");

        fs::write(&output, b"old").expect("write existing output");
        fs::write(&tmp_out, b"new").expect("write temp output");

        replace_output_file(&tmp_out, &output, "unit test").expect("replace output");

        assert_eq!(fs::read(&output).expect("read replaced output"), b"new");
        assert!(
            !tmp_out.exists(),
            "temporary output should be moved into place"
        );
    }

    #[cfg(feature = "ffmpeg-cli-tests")]
    #[test]
    fn remux_copy_streams_replaces_existing_destination() {
        ensure_ffmpeg_present();
        let tmp = tempfile::tempdir().expect("create tempdir");
        let input = tmp.path().join("input.mkv");
        let output = tmp.path().join("output.mkv");
        write_test_video(&input);
        fs::write(&output, b"stale output").expect("write stale output");

        let input_cstr = cstring_path(&input);
        let output_cstr = cstring_path(&output);
        remux_copy_streams(input_cstr.as_c_str(), output_cstr.as_c_str())
            .expect("remux should replace stale output");

        assert_ne!(
            fs::read(&output).expect("read output"),
            b"stale output",
            "remux output was not replaced"
        );
        let output_cstr = cstring_path(&output);
        AVFormatContextInput::open(output_cstr.as_c_str()).expect("open replaced remux output");
    }

    #[cfg(feature = "ffmpeg-cli-tests")]
    #[test]
    fn remux_copy_streams_replaces_input_in_place() {
        ensure_ffmpeg_present();
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("input.mkv");
        write_test_video(&path);
        let input = cstring_path(&path);

        remux_copy_streams(input.as_c_str(), input.as_c_str())
            .expect("in-place remux should close the input before replacing it");
        let output = AVFormatContextInput::open(input.as_c_str()).expect("open remuxed output");
        assert!(output
            .streams()
            .iter()
            .any(|stream| { stream.codecpar().codec_type == ffi::AVMEDIA_TYPE_VIDEO }));
    }

    #[cfg(feature = "ffmpeg-cli-tests")]
    #[test]
    fn mux_text_tracks_from_replaces_input_in_place() {
        ensure_ffmpeg_present();
        let tmp = tempfile::tempdir().expect("create tempdir");
        let path = tmp.path().join("input.mkv");
        let subtitles = tmp.path().join("ocr.srt");
        write_test_video(&path);
        write_srt(&subtitles);
        let input = cstring_path(&path);
        let tracks = [OcrSubtitleTrack {
            language: "eng".to_string(),
            subtitle_path: subtitles,
            format: OcrFormat::Srt,
        }];

        mux_text_tracks_from(input.as_c_str(), input.as_c_str(), &tracks)
            .expect("in-place subtitle mux should close the input before replacing it");
        let output = AVFormatContextInput::open(input.as_c_str()).expect("open muxed output");
        assert!(output
            .streams()
            .iter()
            .any(|stream| { stream.codecpar().codec_type == ffi::AVMEDIA_TYPE_VIDEO }));
        assert!(output
            .streams()
            .iter()
            .any(|stream| { stream.codecpar().codec_type == ffi::AVMEDIA_TYPE_SUBTITLE }));
    }

    #[cfg(feature = "ffmpeg-cli-tests")]
    #[test]
    fn mux_text_tracks_from_replaces_existing_destination() {
        ensure_ffmpeg_present();
        let tmp = tempfile::tempdir().expect("create tempdir");
        let input = tmp.path().join("input.mkv");
        let subtitles = tmp.path().join("ocr.srt");
        let output = tmp.path().join("output.mkv");
        write_test_video(&input);
        write_srt(&subtitles);
        fs::write(&output, b"stale output").expect("write stale output");

        let input_cstr = cstring_path(&input);
        let output_cstr = cstring_path(&output);
        let tracks = [OcrSubtitleTrack {
            language: "eng".to_string(),
            subtitle_path: subtitles,
            format: OcrFormat::Srt,
        }];
        mux_text_tracks_from(input_cstr.as_c_str(), output_cstr.as_c_str(), &tracks)
            .expect("subtitle mux should replace stale output");

        assert_ne!(
            fs::read(&output).expect("read output"),
            b"stale output",
            "subtitle mux output was not replaced"
        );
        let output_cstr = cstring_path(&output);
        let output_ctx =
            AVFormatContextInput::open(output_cstr.as_c_str()).expect("open replaced mux output");
        assert!(
            output_ctx
                .streams()
                .iter()
                .any(|stream| stream.codecpar().codec_type == ffi::AVMEDIA_TYPE_SUBTITLE),
            "expected muxed output to include a subtitle stream"
        );
    }
}
