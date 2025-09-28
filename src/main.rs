use anyhow::{anyhow, bail, Context, Result};
use clap::{value_parser, Parser, ValueEnum};
use log::{debug, error, warn};
use rsmpeg::avcodec::{AVCodec, AVCodecContext, AVPacket};
use rsmpeg::avformat::{AVFormatContextInput, AVFormatContextOutput, AVStreamMut, AVStreamRef};
use rsmpeg::avutil::{ra, AVAudioFifo, AVChannelLayout, AVFrame, AVSamples};
use rsmpeg::error::RsmpegError;
use rsmpeg::ffi::{self};
use rsmpeg::swresample::SwrContext;
use rsmpeg::swscale::SwsContext;
use std::path::PathBuf;
use std::{
    ffi::{CStr, CString},
    sync::atomic::{AtomicI64, Ordering},
};
use streaming_devices::{H264Level, H264Profile, Resolution, StreamingDevice};

mod config;
mod gpu;
mod streaming_devices;
use gpu::{find_hw_encoder, gather_probe_json, print_probe, print_probe_codecs, HwAccel};

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
enum VideoQuality {
    /// Leave video resolution/bitrate untouched.
    #[value(
        name = "match-source",
        alias = "source",
        alias = "auto",
        alias = "original",
        alias = "input"
    )]
    MatchSource,
    /// 360p (SD) profile – ~1.2 Mbps target bitrate.
    #[value(name = "360p", alias = "sd", alias = "sd360", alias = "low")]
    P360,
    /// 480p (SD+) profile – ~2.5 Mbps target bitrate.
    #[value(name = "480p", alias = "sd480", alias = "dvd", alias = "standard")]
    P480,
    /// 720p (HD) profile – ~5 Mbps target bitrate.
    #[value(name = "720p", alias = "hd", alias = "hd-ready", alias = "1280x720")]
    P720,
    /// 1080p (Full HD) profile – ~8 Mbps target bitrate.
    #[value(name = "1080p", alias = "full-hd", alias = "fhd", alias = "1920x1080")]
    P1080,
    /// 1440p (Quad HD) profile – ~16 Mbps target bitrate.
    #[value(name = "1440p", alias = "qhd", alias = "2k", alias = "2560x1440")]
    P1440,
    /// 2160p (Ultra HD / 4K) profile – ~35 Mbps target bitrate.
    #[value(name = "2160p", alias = "uhd", alias = "4k", alias = "3840x2160")]
    P2160,
}

impl VideoQuality {
    fn targets(self) -> (Option<(u32, u32)>, Option<i64>) {
        match self {
            VideoQuality::MatchSource => (None, None),
            VideoQuality::P360 => (Some((640, 360)), Some(1_200_000)),
            VideoQuality::P480 => (Some((854, 480)), Some(2_500_000)),
            VideoQuality::P720 => (Some((1280, 720)), Some(5_000_000)),
            VideoQuality::P1080 => (Some((1920, 1080)), Some(8_000_000)),
            VideoQuality::P1440 => (Some((2560, 1440)), Some(16_000_000)),
            VideoQuality::P2160 => (Some((3840, 2160)), Some(35_000_000)),
        }
    }
}

impl std::fmt::Display for VideoQuality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let label = match self {
            VideoQuality::MatchSource => "match-source",
            VideoQuality::P360 => "360p",
            VideoQuality::P480 => "480p",
            VideoQuality::P720 => "720p",
            VideoQuality::P1080 => "1080p",
            VideoQuality::P1440 => "1440p",
            VideoQuality::P2160 => "2160p",
        };
        write!(f, "{}", label)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ValueEnum)]
enum AudioQuality {
    /// Leave audio bitrate untouched.
    #[value(
        name = "match-source",
        alias = "source",
        alias = "auto",
        alias = "original"
    )]
    MatchSource,
    /// 320 kbps (very high) AAC target bitrate.
    #[value(name = "320k", alias = "very-high", alias = "studio")]
    K320,
    /// 256 kbps (high) AAC target bitrate.
    #[value(name = "256k", alias = "high", alias = "itunes")]
    K256,
    /// 224 kbps AAC target bitrate.
    #[value(name = "224k", alias = "broadcast")]
    K224,
    /// 192 kbps (standard) AAC target bitrate.
    #[value(name = "192k", alias = "standard", alias = "cd")]
    K192,
    /// 160 kbps AAC target bitrate.
    #[value(name = "160k", alias = "medium-high")]
    K160,
    /// 128 kbps (medium) AAC target bitrate.
    #[value(name = "128k", alias = "medium", alias = "default")]
    K128,
    /// 96 kbps (low) AAC target bitrate.
    #[value(name = "96k", alias = "low", alias = "speech")]
    K96,
}

impl AudioQuality {
    fn bitrate(self) -> Option<i64> {
        match self {
            AudioQuality::MatchSource => None,
            AudioQuality::K320 => Some(320_000),
            AudioQuality::K256 => Some(256_000),
            AudioQuality::K224 => Some(224_000),
            AudioQuality::K192 => Some(192_000),
            AudioQuality::K160 => Some(160_000),
            AudioQuality::K128 => Some(128_000),
            AudioQuality::K96 => Some(96_000),
        }
    }
}

impl std::fmt::Display for AudioQuality {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let label = match self {
            AudioQuality::MatchSource => "match-source",
            AudioQuality::K320 => "320k",
            AudioQuality::K256 => "256k",
            AudioQuality::K224 => "224k",
            AudioQuality::K192 => "192k",
            AudioQuality::K160 => "160k",
            AudioQuality::K128 => "128k",
            AudioQuality::K96 => "96k",
        };
        write!(f, "{}", label)
    }
}

#[derive(Copy, Clone, Debug, Default)]
struct QualityLimits {
    max_video_dimensions: Option<(u32, u32)>,
    max_video_bitrate: Option<i64>,
    max_audio_bitrate: Option<i64>,
}

impl QualityLimits {
    fn apply_video_quality(&mut self, video_quality: VideoQuality) {
        let (dimensions, bitrate) = video_quality.targets();
        self.max_video_dimensions = dimensions;
        self.max_video_bitrate = bitrate;
    }

    fn apply_audio_quality(&mut self, audio_quality: AudioQuality) {
        self.max_audio_bitrate = audio_quality.bitrate();
    }
}

fn derive_target_bitrate(source: i64, limit: Option<i64>) -> Option<i64> {
    match (limit, source) {
        (Some(limit), source_value) if limit > 0 => {
            if source_value > 0 {
                Some(std::cmp::min(source_value, limit))
            } else {
                Some(limit)
            }
        }
        (None, source_value) if source_value > 0 => Some(source_value),
        _ => None,
    }
}

fn resolution_to_dimensions(resolution: Resolution) -> (u32, u32) {
    match resolution {
        Resolution::Resolution480p => (640, 480),
        Resolution::Resolution720p => (1280, 720),
        Resolution::Resolution1080p => (1920, 1080),
        Resolution::Resolution1440p => (2560, 1440),
        Resolution::Resolution2160p => (3840, 2160),
    }
}

fn clamp_dimensions(
    source_width: i32,
    source_height: i32,
    device_cap: (u32, u32),
    quality_cap: Option<(u32, u32)>,
) -> (i32, i32) {
    if source_width <= 0 || source_height <= 0 {
        return (source_width.max(1), source_height.max(1));
    }

    let mut max_width = device_cap.0.max(2);
    let mut max_height = device_cap.1.max(2);

    if let Some((quality_width, quality_height)) = quality_cap {
        max_width = max_width.min(quality_width.max(2));
        max_height = max_height.min(quality_height.max(2));
    }

    if (source_width as u32) <= max_width && (source_height as u32) <= max_height {
        return (source_width, source_height);
    }

    let width_ratio = max_width as f64 / source_width as f64;
    let height_ratio = max_height as f64 / source_height as f64;
    let scale = width_ratio.min(height_ratio);

    let mut target_width = (source_width as f64 * scale).round() as i32;
    let mut target_height = (source_height as f64 * scale).round() as i32;

    if target_width < 2 {
        target_width = 2;
    }
    if target_height < 2 {
        target_height = 2;
    }

    if source_width >= 2 {
        target_width = target_width.min(source_width);
    } else {
        target_width = source_width;
    }

    if source_height >= 2 {
        target_height = target_height.min(source_height);
    } else {
        target_height = source_height;
    }

    if target_width % 2 != 0 {
        target_width = (target_width - 1).max(2);
    }
    if target_height % 2 != 0 {
        target_height = (target_height - 1).max(2);
    }

    (target_width, target_height)
}
// TODO: switch to enum to allow for different modes
// see: https://github.com/clap-rs/clap/discussions/3711#discussioncomment-2717657
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum StreamingDeviceSelection {
    All,
    Model(&'static streaming_devices::StreamingDevice),
}

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// List of StreamingDevice models, or "all" (default) to target every supported device
    #[arg(
        short,
        long,
        value_delimiter = ',',
        value_name = "STREAMING_DEVICE",
        value_parser = Args::parse_device_selection
    )]
    streaming_devices: Option<Vec<StreamingDeviceSelection>>,

    /// Path to the configuration file
    #[arg(short, long, value_parser = value_parser!(PathBuf))]
    config_file: Option<PathBuf>,

    /// Target video quality profile (defaults to match the source resolution/bitrate)
    #[arg(long, value_enum, default_value_t = VideoQuality::MatchSource)]
    video_quality: VideoQuality,

    /// Target audio quality profile (defaults to match the source bitrate)
    #[arg(long, value_enum, default_value_t = AudioQuality::MatchSource)]
    audio_quality: AudioQuality,

    /// Maximum video bitrate (e.g. 8M, 4800k, 5.5mbps)
    #[arg(long, value_parser = Args::parse_bitrate)]
    max_video_bitrate: Option<i64>,

    /// Maximum audio bitrate (e.g. 320k, 0.2M)
    #[arg(long, value_parser = Args::parse_bitrate)]
    max_audio_bitrate: Option<i64>,

    /// Video file to convert (required unless probing)
    #[arg(
        value_parser = Args::parse_cstring,
        required_unless_present_any = ["probe_hw", "probe_codecs"]
    )]
    input_file: Option<CString>,

    /// Our output direct-play-compatible video file (required unless probing)
    #[arg(
        value_parser = Args::parse_cstring,
        required_unless_present_any = ["probe_hw", "probe_codecs"]
    )]
    output_file: Option<CString>,

    /// Hardware acceleration preference (auto tries GPU encoders if available)
    #[arg(long, value_enum, default_value_t = HwAccel::Auto)]
    hw_accel: HwAccel,

    /// Print available HW devices/encoders and exit
    #[arg(long, default_value_t = false)]
    probe_hw: bool,

    /// Print all FFmpeg encoders/decoders and exit
    #[arg(long, default_value_t = false)]
    probe_codecs: bool,

    /// Filter probes to video codecs only
    #[arg(long, default_value_t = false)]
    only_video: bool,

    /// Filter probes to hardware-capable codecs only
    #[arg(long, default_value_t = false)]
    only_hw: bool,

    /// Emit probe output as JSON (machine-readable)
    #[arg(long, default_value_t = false)]
    probe_json: bool,
}

impl Args {
    fn parse_cstring(s: &str) -> Result<CString, String> {
        CString::new(s).map_err(|e| format!("Invalid CString: {}", e))
    }

    fn parse_bitrate(input: &str) -> Result<i64, String> {
        let trimmed = input.trim();
        if trimmed.is_empty() {
            return Err("Bitrate value cannot be empty".to_string());
        }

        let lower = trimmed.to_ascii_lowercase().replace(' ', "");
        let mut split_idx = lower.len();
        for (idx, ch) in lower.char_indices() {
            if !(ch.is_ascii_digit() || ch == '.' || ch == ',' || ch == '_') {
                split_idx = idx;
                break;
            }
        }

        let (number_str, suffix) = lower.split_at(split_idx);
        if number_str.is_empty() {
            return Err(format!(
                "Failed to parse bitrate '{}': missing number",
                input
            ));
        }

        let numeric = number_str.replace(',', "").replace('_', "");
        let value: f64 = numeric
            .parse()
            .map_err(|_| format!("Failed to parse bitrate '{}': invalid number", input))?;

        let mut normalized_suffix = suffix.trim().to_string();
        for trailing in ["/s", "ps", "bps", "bits", "bit"] {
            if normalized_suffix.ends_with(trailing) {
                let new_len = normalized_suffix.len() - trailing.len();
                normalized_suffix.truncate(new_len);
            }
        }
        normalized_suffix = normalized_suffix.trim().to_string();

        let multiplier = match normalized_suffix.as_str() {
            "" | "b" => 1i64,
            "k" | "kb" | "kbit" => 1_000i64,
            "m" | "mb" | "mbit" => 1_000_000i64,
            "g" | "gb" | "gbit" => 1_000_000_000i64,
            other => {
                return Err(format!(
                    "Failed to parse bitrate '{}': unsupported suffix '{}'. Use plain numbers or k/m/g suffixes.",
                    input, other
                ));
            }
        };

        let bits_per_second = (value * multiplier as f64).round() as i64;
        if bits_per_second <= 0 {
            return Err(format!(
                "Failed to parse bitrate '{}': value must be positive",
                input
            ));
        }

        Ok(bits_per_second)
    }

    fn parse_device_selection(input: &str) -> Result<StreamingDeviceSelection, String> {
        let normalized = input.trim();
        if normalized.is_empty() {
            return Err("Streaming device value cannot be empty".to_string());
        }

        if normalized.eq_ignore_ascii_case("all") {
            return Ok(StreamingDeviceSelection::All);
        }

        streaming_devices::STREAMING_DEVICES
            .iter()
            .find(|device| device.model.eq_ignore_ascii_case(normalized))
            .map(StreamingDeviceSelection::Model)
            .ok_or_else(|| {
                let available = streaming_devices::STREAMING_DEVICES
                    .iter()
                    .map(|device| device.model)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!(
                    "Provided streaming device model '{}' not found. Valid values: all, {}",
                    input, available
                )
            })
    }
}

pub enum StreamExtras {
    Some((SwrContext, AVAudioFifo)),
    None,
}

pub struct StreamProcessingContext {
    decode_context: AVCodecContext,
    encode_context: AVCodecContext,
    stream_index: i32,
    media_type: ffi::AVMediaType,
    frame_buffer: Option<AVAudioFifo>, // TODO: Support video stream buffers too?
    resample_context: Option<SwrContext>,
    pts: AtomicI64,
    #[allow(dead_code)]
    hw_device_ctx: Option<*mut ffi::AVBufferRef>,
}

impl std::fmt::Debug for StreamProcessingContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StreamProcessingContext")
            .field("stream_index", &self.stream_index)
            .field("media_type", &self.media_type)
            .field("pts", &self.pts)
            .finish()
    }
}

fn init_audio_resampler(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
) -> Result<SwrContext> {
    let mut resample_context = SwrContext::new(
        &encode_context.ch_layout,
        encode_context.sample_fmt,
        encode_context.sample_rate,
        &decode_context.ch_layout,
        decode_context.sample_fmt,
        decode_context.sample_rate,
    )
    .context("Could not allocate resample context")?;
    resample_context
        .init()
        .context("Could not open resample context")?;
    Ok(resample_context)
}

fn add_samples_to_fifo(
    fifo: &mut AVAudioFifo,
    samples_buffer: &AVSamples,
    frame_size: i32,
) -> Result<()> {
    fifo.realloc(fifo.size() + frame_size);
    unsafe { fifo.write(samples_buffer.audio_data.as_ptr(), frame_size) }
        .context("Could not write data to FIFO")?;
    Ok(())
}

fn init_output_audio_frame(
    nb_samples: i32,
    ch_layout: ffi::AVChannelLayout,
    sample_fmt: i32,
    sample_rate: i32,
) -> Result<AVFrame> {
    let mut frame = AVFrame::new();
    frame.set_nb_samples(nb_samples);
    frame.set_ch_layout(ch_layout);
    frame.set_format(sample_fmt);
    frame.set_sample_rate(sample_rate);

    frame
        .get_buffer(0)
        .context("Could not allocate output frame samples")?;

    Ok(frame)
}

fn encode_and_write_frame(
    encode_context: &mut AVCodecContext,
    output_format_context: &mut AVFormatContextOutput,
    stream_index: usize,
    frame: Option<AVFrame>,
) -> Result<()> {
    encode_context
        .send_frame(frame.as_ref())
        .context("Failed to send frame!")?;

    loop {
        let mut packet = match encode_context.receive_packet() {
            Ok(packet) => packet,
            Err(RsmpegError::EncoderDrainError) | Err(RsmpegError::EncoderFlushedError) => {
                break;
            }
            Err(e) => bail!(e),
        };

        packet.set_stream_index(stream_index as i32);
        packet.rescale_ts(
            encode_context.time_base,
            output_format_context
                .streams()
                .get(stream_index)
                .context("Failed to get stream")?
                .time_base,
        );

        output_format_context
            .write_frame(&mut packet)
            .context("Could not write frame")?;
    }

    Ok(())
}

fn process_video_stream(
    stream_processing_context: &mut StreamProcessingContext,
    input_stream: &AVStreamRef,
    output_format_context: &mut AVFormatContextOutput,
    packet: &mut AVPacket,
) -> Result<()> {
    packet.rescale_ts(
        input_stream.time_base,
        stream_processing_context.decode_context.time_base,
    );

    stream_processing_context
        .encode_context
        .open(None)
        .context("Error opening video encoding context")?;

    match stream_processing_context
        .decode_context
        .send_packet(Some(packet))
    {
        Ok(_) | Err(RsmpegError::DecoderFlushedError) => {}
        Err(e) => {
            bail!("Packet failed to send to decoder: {}", e); // TODO: reasonable to end here on error?
        }
    }

    loop {
        let frame = match stream_processing_context.decode_context.receive_frame() {
            Ok(frame) => {
                error!("Successfully processed frame!");
                frame
            }
            Err(RsmpegError::DecoderDrainError) | Err(RsmpegError::DecoderFlushedError) => {
                break;
            }
            Err(e) => {
                error!("Decoder receive frame error: {}", e);
                break;
            }
        };

        let mut new_frame = AVFrame::new();
        new_frame.set_width(stream_processing_context.encode_context.width);
        new_frame.set_height(stream_processing_context.encode_context.height);
        new_frame.set_format(ffi::AV_PIX_FMT_YUV420P);
        new_frame.alloc_buffer().context("Error allocating ")?;

        let mut sws_context = SwsContext::get_context(
            stream_processing_context.decode_context.width,
            stream_processing_context.decode_context.height,
            stream_processing_context.decode_context.pix_fmt,
            stream_processing_context.encode_context.width,
            stream_processing_context.encode_context.height,
            stream_processing_context.encode_context.pix_fmt,
            ffi::SWS_FAST_BILINEAR | ffi::SWS_ACCURATE_RND,
            None,
            None,
            None,
        )
        .context("Failed to create a swscale context.")?;

        new_frame.set_pts(frame.best_effort_timestamp);

        sws_context
            .scale_frame(
                &frame,
                0,
                stream_processing_context.decode_context.height,
                &mut new_frame,
            )
            .context("Failed to scale frame.")?;

        encode_and_write_frame(
            &mut stream_processing_context.encode_context,
            output_format_context,
            packet.stream_index as usize,
            Some(new_frame),
        )?;
    }

    Ok(())
}

fn process_audio_stream(
    stream_processing_context: &mut StreamProcessingContext,
    input_stream: &AVStreamRef,
    output_format_context: &mut AVFormatContextOutput,
    packet: &mut AVPacket,
) -> Result<()> {
    // TODO: ensure audio streams have the same metadata
    // based on https://github.com/larksuite/rsmpeg/blob/master/tests/ffmpeg_examples/transcode_aac.rs
    packet.rescale_ts(
        input_stream.time_base,
        stream_processing_context.decode_context.time_base,
    );

    stream_processing_context
        .encode_context
        .open(None)
        .context("Error opening audio encoding context")?;

    let Some(fifo) = stream_processing_context.frame_buffer.as_mut() else {
        panic!("Failed to get Audio FIFO buffer!");
    };

    match stream_processing_context
        .decode_context
        .send_packet(Some(packet))
    {
        Ok(_) | Err(RsmpegError::DecoderFlushedError) => {}
        Err(e) => {
            bail!("Packet failed to send to decoder: {}", e); // TODO: reasonable to end here on error?
        }
    }

    loop {
        let frame = match stream_processing_context.decode_context.receive_frame() {
            Ok(frame) => frame,
            Err(RsmpegError::DecoderDrainError) | Err(RsmpegError::DecoderFlushedError) => {
                error!("Cannot read from drianed/flushed Decoder.");
                break;
            }
            Err(e) => {
                error!("Decoder receive frame error: {}", e);
                break;
            }
        };

        let output_frame_size = stream_processing_context.encode_context.frame_size; // TODO: Why is this 0 in some cases and how to handle that?

        debug!("OUTPUT FRAME SIZE: {}", output_frame_size);

        let mut output_samples = AVSamples::new(
            stream_processing_context
                .encode_context
                .ch_layout
                .nb_channels,
            frame.nb_samples,
            stream_processing_context.encode_context.sample_fmt,
            0,
        )
        .context("Create samples buffer failed.")?;

        match &mut stream_processing_context.resample_context {
            Some(resampler) => unsafe {
                resampler
                    .convert(
                        output_samples.audio_data.as_mut_ptr(),
                        output_samples.nb_samples,
                        frame.extended_data as *const _,
                        frame.nb_samples,
                    )
                    .context("Could not convert input samples")?;
            },
            None => {}
        }

        add_samples_to_fifo(fifo, &output_samples, frame.nb_samples)?;

        debug!("FIFO SIZE: {}", fifo.size());
        debug!(
            "AUDIO STREAM INDEX: {}",
            stream_processing_context.stream_index
        );
        while fifo.size() >= output_frame_size {
            load_encode_and_write(
                fifo,
                output_format_context,
                &mut stream_processing_context.encode_context,
                stream_processing_context.stream_index,
                &mut stream_processing_context.pts,
            )?;
        }
    }

    Ok(())
}

fn process_subtitle_stream(
    stream_processing_context: &mut StreamProcessingContext,
    input_stream: &AVStreamRef,
    output_format_context: &mut AVFormatContextOutput,
    packet: &mut AVPacket,
) -> Result<()> {
    packet.rescale_ts(
        input_stream.time_base,
        stream_processing_context.decode_context.time_base,
    );

    stream_processing_context
        .encode_context
        .open(None)
        .context("Could not open subitle encoder context")?;

    match stream_processing_context
        .decode_context
        .decode_subtitle(Some(packet))
    {
        Ok(sub) => {
            if let Some(s) = sub {
                // TODO: Find the max size of subtitle data in a single packet
                const MAX_SUBTITLE_PACKET_SIZE: usize = 32 * 1024; // 32KB
                let mut subtitle_buffer = vec![0u8; MAX_SUBTITLE_PACKET_SIZE];
                stream_processing_context
                    .encode_context
                    .encode_subtitle(&s, &mut subtitle_buffer)?;

                let encoded_size = subtitle_buffer
                    .iter()
                    .rposition(|&x| x != 0)
                    .map(|pos| pos + 1)
                    .unwrap_or(0);

                // Create a new packet for the encoded subtitle
                let mut encoded_packet = AVPacket::new();
                unsafe {
                    (*encoded_packet.as_mut_ptr()).data = subtitle_buffer.as_mut_ptr();
                    (*encoded_packet.as_mut_ptr()).size = encoded_size as i32;
                }
                encoded_packet.set_stream_index(stream_processing_context.stream_index);
                encoded_packet.set_pts(packet.pts);
                encoded_packet.set_dts(packet.dts);
                encoded_packet.set_duration(packet.duration);
                encoded_packet.set_flags(packet.flags);

                encoded_packet.rescale_ts(
                    stream_processing_context.decode_context.time_base,
                    output_format_context.streams()
                        [stream_processing_context.stream_index as usize]
                        .time_base,
                );

                output_format_context
                    .interleaved_write_frame(&mut encoded_packet)
                    .context("Could not write subtitle packet")?;
            }
        }
        Err(rsmpeg::error::RsmpegError::DecoderDrainError) => {
            error!("Error: The decoder has been fully drained, no more subtitles to decode. Continuing...");
        }
        Err(rsmpeg::error::RsmpegError::DecoderFlushedError) => {
            error!(
                "Error: The decoder has been flushed, no more subtitles to decode. Continuing..."
            );
        }
        Err(e) => {
            error!("Error decoding subtitle: {}", e);
        }
    }

    Ok(())
}

fn load_encode_and_write(
    fifo: &mut AVAudioFifo,
    output_format_context: &mut AVFormatContextOutput,
    encode_context: &mut AVCodecContext,
    stream_index: i32,
    pts: &mut AtomicI64,
) -> Result<()> {
    let frame_size = fifo.size().min(encode_context.frame_size); // TODO: should be encode_context.frame_size but it reads 0

    let mut frame = init_output_audio_frame(
        frame_size,
        encode_context.ch_layout().clone().into_inner(),
        encode_context.sample_fmt,
        encode_context.sample_rate,
    )
    .context("Failed to initialize audio frame.")?;

    if unsafe {
        let read_frame_size = fifo.read(frame.data_mut().as_mut_ptr(), frame_size)?;
        debug!("Read audio frame size (bytes): {}", read_frame_size);
        read_frame_size
    } < frame_size
    {
        bail!("Could not read data from FIFO");
    }

    frame.set_pts(pts.fetch_add(frame.nb_samples as i64, Ordering::Relaxed));

    encode_and_write_frame(
        encode_context,
        output_format_context,
        stream_index as usize,
        Some(frame),
    )
    .context("Error encoding audio frame!")?;

    Ok(())
}

fn set_video_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
    output_stream: &mut AVStreamMut,
    h264_profile: H264Profile, // TODO: handle cases somewhere when target video codec is NOT h264
    h264_level: H264Level,
    quality_limits: &QualityLimits,
    device_max_resolution: Resolution,
) {
    encode_context.set_sample_rate(decode_context.sample_rate);
    let device_cap = resolution_to_dimensions(device_max_resolution);
    let (target_width, target_height) = clamp_dimensions(
        decode_context.width,
        decode_context.height,
        device_cap,
        quality_limits.max_video_dimensions,
    );

    if target_width != decode_context.width || target_height != decode_context.height {
        debug!(
            "Scaling video from {}x{} to {}x{}",
            decode_context.width, decode_context.height, target_width, target_height
        );
    }

    encode_context.set_width(target_width);
    encode_context.set_height(target_height);
    encode_context.set_time_base(decode_context.time_base);
    encode_context.set_pix_fmt(ffi::AV_PIX_FMT_YUV420P); // TODO: downgrade more intelligently?
    encode_context.set_max_b_frames(decode_context.max_b_frames);
    if let Some(bit_rate) =
        derive_target_bitrate(decode_context.bit_rate, quality_limits.max_video_bitrate)
    {
        debug!("Video bitrate target set to {} bps", bit_rate);
        encode_context.set_bit_rate(bit_rate);
    } else {
        debug!("Video bitrate target not set; using encoder default");
    }
    encode_context.set_gop_size(decode_context.gop_size);
    encode_context.set_sample_aspect_ratio(decode_context.sample_aspect_ratio);
    // TODO: find a safe way to do this
    unsafe {
        (*encode_context.as_mut_ptr()).profile = h264_profile as i32;
        (*encode_context.as_mut_ptr()).level = h264_level as i32;
    }
    output_stream.set_codecpar(encode_context.extract_codecpar());
}

fn set_audio_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
    output_stream: &mut AVStreamMut,
    quality_limits: &QualityLimits,
) {
    // TODO: Read input to determine output audio codec params
    let encoder = AVCodec::find_encoder(ffi::AV_CODEC_ID_AAC).expect("Could not find AAC encoder");
    encode_context
        .set_ch_layout(AVChannelLayout::from_nb_channels(decode_context.channels).into_inner());
    // The input file's sample rate is used to avoid a sample rate conversion.
    encode_context.set_sample_rate(decode_context.sample_rate);
    encode_context.set_sample_fmt(encoder.sample_fmts().unwrap()[0]); // TODO: Are we actually getting the sample rate we want?
    if let Some(bit_rate) =
        derive_target_bitrate(decode_context.bit_rate, quality_limits.max_audio_bitrate)
    {
        debug!("Audio bitrate target set to {} bps", bit_rate);
        encode_context.set_bit_rate(bit_rate);
    } else {
        debug!("Audio bitrate target not set; using encoder default");
    }

    output_stream.set_codecpar(encode_context.extract_codecpar());
    output_stream.set_time_base(ra(1, decode_context.sample_rate)); // use high-precision time base
}

fn set_subtitle_codec_par(
    decode_context: &mut AVCodecContext,
    encode_context: &mut AVCodecContext,
    output_stream: &mut AVStreamMut,
) {
    // Set subtitle encoder parameters based on the input subtitle stream
    encode_context.set_time_base(decode_context.time_base);

    if decode_context.subtitle_header_size > 0 {
        let mut new_subtitle_header = vec![0u8; decode_context.subtitle_header_size as usize];
        new_subtitle_header.copy_from_slice(unsafe {
            std::slice::from_raw_parts(
                decode_context.subtitle_header,
                decode_context.subtitle_header_size as usize,
            )
        });

        // TODO: find safe way to do this
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

    output_stream.set_codecpar(encode_context.extract_codecpar());
}

/// Takes input video files and outputs direct-play-compatible video files
fn convert_video_file(
    input_file: &CStr,
    output_file: &CStr,
    target_video_codec: ffi::AVCodecID,
    target_audio_codec: ffi::AVCodecID,
    min_h264_profile: H264Profile,
    min_h264_level: H264Level,
    _min_fps: u32,
    device_max_resolution: Resolution,
    quality_limits: &QualityLimits,
    hw_accel: HwAccel,
) -> Result<(), anyhow::Error> {
    let mut input_format_context = AVFormatContextInput::open(input_file, None, &mut None)?;
    input_format_context.dump(0, input_file)?;

    let mut output_format_context = AVFormatContextOutput::create(output_file, None)?;

    let mut stream_contexts: Vec<StreamProcessingContext> = Vec::new();

    for stream in input_format_context.streams() {
        // TODO: ID streams either unsupported in output container type or without a supported decoder and skip them, producing a warning for each skipped.
        let input_codec_type = stream.codecpar().codec_type;
        // TODO: implement support for attachments
        if input_codec_type == ffi::AVMEDIA_TYPE_ATTACHMENT {
            warn!("Warning: Input file contains attachment streams, which may not be handled correctly. Skipping...");
            continue;
        }

        let mut output_stream = output_format_context.new_stream();

        let input_stream_codecpar = stream.codecpar();
        let input_codec_id = input_stream_codecpar.codec_id;
        let decoder = AVCodec::find_decoder(input_codec_id)
            .with_context(|| anyhow!("Decoder not found for stream {}.", output_stream.index))?;
        let mut decode_context = AVCodecContext::new(&decoder);
        decode_context.apply_codecpar(&input_stream_codecpar)?;
        decode_context.set_time_base(stream.time_base); // TODO: needed?
        if let Some(framerate) = stream.guess_framerate() {
            decode_context.set_framerate(framerate);
        }
        decode_context.open(None)?;

        let mut encode_context: AVCodecContext;
        let media_type: ffi::AVMediaType;
        let mut frame_buffer: Option<AVAudioFifo> = None;
        let mut resample_context: Option<SwrContext> = None;
        let mut hw_device_ctx_ptr: Option<*mut ffi::AVBufferRef> = None;

        match decode_context.codec_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                output_stream.set_metadata(stream.metadata().as_deref().cloned());
                // Prefer HW encoder when available and requested
                let (maybe_hw_encoder, maybe_hw_dev) =
                    find_hw_encoder(target_video_codec, hw_accel);
                let encoder = maybe_hw_encoder
                    .or_else(|| AVCodec::find_encoder(target_video_codec))
                    .expect("Could not find H264 encoder");
                encode_context = AVCodecContext::new(&encoder);
                // Attach hardware device if present (must be set before open())
                if let Some(buf) = maybe_hw_dev {
                    unsafe {
                        (*encode_context.as_mut_ptr()).hw_device_ctx = ffi::av_buffer_ref(buf);
                    }
                    hw_device_ctx_ptr = Some(maybe_hw_dev.unwrap());
                }
                media_type = ffi::AVMEDIA_TYPE_VIDEO;

                set_video_codec_par(
                    &mut decode_context,
                    &mut encode_context,
                    &mut output_stream,
                    min_h264_profile,
                    min_h264_level,
                    quality_limits,
                    device_max_resolution,
                );
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                output_stream.set_metadata(stream.metadata().as_deref().cloned());
                let encoder =
                    AVCodec::find_encoder(target_audio_codec).expect("Could not find AAC encoder");

                encode_context = AVCodecContext::new(&encoder);
                media_type = ffi::AVMEDIA_TYPE_AUDIO;

                set_audio_codec_par(
                    &mut decode_context,
                    &mut encode_context,
                    &mut output_stream,
                    quality_limits,
                );
                // Initialize the resampler to be able to convert audio sample formats.
                resample_context =
                    init_audio_resampler(&mut decode_context, &mut encode_context).ok();

                // Initialize the FIFO buffer to store audio samples to be encoded.
                frame_buffer = Some(AVAudioFifo::new(
                    encode_context.sample_fmt,
                    encode_context.ch_layout.nb_channels,
                    1,
                ));
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                // TODO: handle cases with no metadata?
                // TODO: always copy metadata or only in some cases?
                output_stream.set_metadata(stream.metadata().as_deref().cloned());

                let encoder = AVCodec::find_encoder(ffi::AV_CODEC_ID_MOV_TEXT)
                    .expect("Could not find MOV_TEXT encoder");
                encode_context = AVCodecContext::new(&encoder);
                media_type = ffi::AVMEDIA_TYPE_SUBTITLE;
                set_subtitle_codec_par(
                    &mut decode_context,
                    &mut encode_context,
                    &mut output_stream,
                );
            }
            // TODO: Handle metadata streams
            unsupported_type => {
                debug!(
                    "Encountered unsupported stream type ({}). Not setting up Codec.",
                    unsupported_type
                );
                continue;
            }
        }

        let stream_process_context = StreamProcessingContext {
            decode_context,
            encode_context,
            stream_index: output_stream.index,
            media_type,
            frame_buffer,
            resample_context,
            pts: AtomicI64::new(0),
            hw_device_ctx: hw_device_ctx_ptr,
        };

        stream_contexts.push(stream_process_context);
    }

    // Write the header of the output file container.
    output_format_context
        .write_header(&mut None)
        .context("Error writing output file header")?;

    // Demux streams
    loop {
        let mut packet = match input_format_context.read_packet()? {
            Some(x) => x,
            None => break,
        };

        let Some(stream_processing_context) = stream_contexts
            .iter_mut()
            .find(|context| context.stream_index == packet.stream_index)
        else {
            // TODO: handle missing matching streams
            break;
        };

        let input_stream: &rsmpeg::avformat::AVStreamRef<'_> =
            &input_format_context.streams()[packet.stream_index as usize];
        match stream_processing_context.media_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                process_video_stream(
                    stream_processing_context,
                    input_stream,
                    &mut output_format_context,
                    &mut packet,
                )?;
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                process_audio_stream(
                    stream_processing_context,
                    input_stream,
                    &mut output_format_context,
                    &mut packet,
                )?;
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {
                process_subtitle_stream(
                    stream_processing_context,
                    input_stream,
                    &mut output_format_context,
                    &mut packet,
                )?;
            }

            unsupported_type => {
                debug!(
                    "Encountered unsupported stream type ({}). Not setting up Codec.",
                    unsupported_type
                );
            }
        }
    }

    // After processing all packets, flush each encoder
    for mut context in stream_contexts {
        match context.media_type {
            ffi::AVMEDIA_TYPE_VIDEO => {
                encode_and_write_frame(
                    &mut context.encode_context,
                    &mut output_format_context,
                    context.stream_index as usize,
                    None,
                )
                .context("Failed to flush video encoder.")?;
                if let Some(mut dev) = context.hw_device_ctx {
                    unsafe {
                        ffi::av_buffer_unref(&mut dev);
                    }
                }
            }
            ffi::AVMEDIA_TYPE_AUDIO => {
                encode_and_write_frame(
                    &mut context.encode_context,
                    &mut output_format_context,
                    context.stream_index as usize,
                    None,
                )
                .context("Failed to flush audio encoder.")?;
            }
            ffi::AVMEDIA_TYPE_SUBTITLE => {}
            unsupported_type => {
                debug!(
                    "Encountered unsupported stream type ({}). Not flushing.",
                    unsupported_type
                );
            }
        }
    }

    output_format_context.write_trailer()?;

    Ok(())
}

fn main() -> Result<()> {
    // FFMPEG TRACE LOGGING
    // unsafe {
    //     ffi::av_log_set_level(ffi::AV_LOG_TRACE as i32);
    // }

    let mut args = Args::parse();

    if args.probe_hw || args.probe_codecs {
        if args.probe_json {
            let summary = gather_probe_json(
                args.only_video,
                args.only_hw,
                args.probe_hw,
                args.probe_codecs,
            );
            println!("{}", serde_json::to_string_pretty(&summary).unwrap());
            return Ok(());
        }
        if args.probe_hw {
            print_probe();
        }
        if args.probe_codecs {
            print_probe_codecs(args.only_video, args.only_hw);
        }
        return Ok(());
    }
    // TODO: implement config file
    if args.config_file.is_some() {
        eprintln!("Error: The --config-file option is not implemented yet.");
        std::process::exit(1);
    }

    let mut quality_limits = QualityLimits::default();
    quality_limits.apply_video_quality(args.video_quality);
    quality_limits.apply_audio_quality(args.audio_quality);
    if let Some(video_cap) = args.max_video_bitrate {
        quality_limits.max_video_bitrate = Some(video_cap);
    }
    if let Some(audio_cap) = args.max_audio_bitrate {
        quality_limits.max_audio_bitrate = Some(audio_cap);
    }

    debug!(
        "Video quality {}, audio quality {}, caps: resolution={:?}, video={:?} bps, audio={:?} bps",
        args.video_quality,
        args.audio_quality,
        quality_limits.max_video_dimensions,
        quality_limits.max_video_bitrate,
        quality_limits.max_audio_bitrate
    );

    // TODO: add reading of config file
    // let config_file = args.config_file.unwrap();

    // let config_streaming_devices = config::parse_config_from_toml(config_file).unwrap();

    let selections = args
        .streaming_devices
        .take()
        .unwrap_or_else(|| vec![StreamingDeviceSelection::All]);

    let mut streaming_devices: Vec<&StreamingDevice> = if selections
        .iter()
        .any(|selection| matches!(selection, StreamingDeviceSelection::All))
    {
        streaming_devices::STREAMING_DEVICES.iter().collect()
    } else {
        selections
            .into_iter()
            .filter_map(|selection| match selection {
                StreamingDeviceSelection::Model(device) => Some(device),
                StreamingDeviceSelection::All => None,
            })
            .collect()
    };

    streaming_devices.sort_by_key(|device| device.model);
    streaming_devices.dedup_by_key(|device| device.model);

    if streaming_devices.is_empty() {
        bail!("No streaming devices resolved from CLI arguments.");
    }

    let common_video_codec = StreamingDevice::get_common_video_codec(&streaming_devices)?;
    let common_audio_codec = StreamingDevice::get_common_audio_codec(&streaming_devices)?;
    let min_h264_profile = StreamingDevice::get_min_h264_profile(&streaming_devices)?;
    let min_h264_level = StreamingDevice::get_min_h264_level(&streaming_devices)?;
    let min_fps = StreamingDevice::get_min_fps(&streaming_devices)?;
    let min_resolution = StreamingDevice::get_min_resolution(&streaming_devices)?;

    // TODO: Check if video file is already compatible and skip if it is

    let input_file = args
        .input_file
        .as_ref()
        .map(|s| s.as_c_str())
        .expect("INPUT_FILE is required unless using --probe-* flags");
    let output_file = args
        .output_file
        .as_ref()
        .map(|s| s.as_c_str())
        .expect("OUTPUT_FILE is required unless using --probe-* flags");

    convert_video_file(
        input_file,
        output_file,
        common_video_codec,
        common_audio_codec,
        min_h264_profile,
        min_h264_level,
        min_fps,
        min_resolution,
        &quality_limits,
        args.hw_accel,
    )
}
