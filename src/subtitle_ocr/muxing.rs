//! Subtitle remux helpers for OCR outputs.
//!
//! Converts generated text subtitle files into container subtitle streams and
//! preserves language/default metadata in the output container.

use super::*;
/// Stores data for PendingPacket.
pub(super) struct PendingPacket {
    ts: i64,
    packet: AVPacket,
}

/// Stores data for SubtitleMuxer.
pub(super) struct SubtitleMuxer {
    input_ctx: AVFormatContextInput,
    input_stream_index: usize,
    input_time_base: ffi::AVRational,
    decode_ctx: AVCodecContext,
    encode_ctx: AVCodecContext,
    output_stream_index: i32,
    last_written_dts: Option<i64>,
}

/// Implements behavior for `SubtitleMuxer`.
impl SubtitleMuxer {
    /// Executes the collect packets routine.
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

/// Executes the remux copy streams routine.
pub fn remux_copy_streams(input_file: &CStr, output_file: &CStr) -> Result<()> {
    let output_path = PathBuf::from(output_file.to_string_lossy().into_owned());
    let output_extension = output_path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_ascii_lowercase())
        .unwrap_or_default();
    let is_mkv = matches!(output_extension.as_str(), "mkv" | "mka" | "mks");
    let stem = output_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("output");
    let tmp_out = if output_extension.is_empty() {
        output_path.with_extension("ocr.tmp")
    } else {
        output_path.with_file_name(format!("{stem}.ocr.tmp.{output_extension}"))
    };

    let mut input_ctx = AVFormatContextInput::open(input_file)?;
    let tmp_cstr = CString::new(tmp_out.to_string_lossy().to_string())
        .context("output path has interior NUL")?;
    let mut output_ctx = AVFormatContextOutput::create(tmp_cstr.as_c_str())?;

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
        stream_index_map.push(out_stream.index);
    }

    output_ctx
        .write_header(&mut None)
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

    fs::rename(&tmp_out, &output_path).with_context(|| {
        format!(
            "replacing '{}' after container remux",
            output_path.display()
        )
    })?;

    Ok(())
}

/// Executes the mux text tracks from routine.
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
    let stem = output_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("output");
    let tmp_out = if output_extension.is_empty() {
        output_path.with_extension("ocr.tmp")
    } else {
        output_path.with_file_name(format!("{stem}.ocr.tmp.{output_extension}"))
    };

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

    output_ctx
        .write_header(&mut None)
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

    fs::rename(&tmp_out, &output_path)
        .with_context(|| format!("replacing '{}' after subtitle remux", output_path.display()))?;

    Ok(())
}

/// Executes the build subtitle muxer routine.
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

/// Executes the select subtitle codec id routine.
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

/// Executes the build language metadata routine.
pub(super) fn build_language_metadata(language: &str) -> Option<AVDictionary> {
    let key = CString::new("language").ok()?;
    let value = CString::new(language).ok()?;
    Some(AVDictionary::new(&key, &value, 0))
}

/// Executes the set subtitle codec par routine.
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

/// Executes the encode subtitle packet routine.
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

/// Executes the packet ts routine.
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
