//! CLI argument types and shared FFmpeg stream-processing helpers.

use anyhow::{bail, Context, Result};
use clap::{value_parser, Parser};
use rsmpeg::avcodec::AVCodecContext;
use rsmpeg::avformat::AVFormatContextOutput;
use rsmpeg::avutil::{AVAudioFifo, AVFrame, AVSamples};
use rsmpeg::error::RsmpegError;
use rsmpeg::ffi;
use rsmpeg::swresample::SwrContext;
use std::ffi::CString;
use std::path::PathBuf;
use std::sync::atomic::AtomicI64;

use crate::devices::{self, DeviceFamily};
use crate::gpu::HwAccel;
use crate::transcoder::{AudioQuality, VideoCodecPreference, VideoQuality};
use crate::types::{
    OcrEngine, OcrFormat, OutputFormat, PrimaryVideoCriteria, StreamsFilter, SubMode,
    UnsupportedVideoPolicy,
};

pub(crate) use crate::cli::progress::ProgressTracker;

// TODO: switch to enum to allow for different modes
// see: https://github.com/clap-rs/clap/discussions/3711#discussioncomment-2717657
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
/// Normalized target-device selector used by CLI parsing and config overrides.
///
/// A single CLI token may resolve to either a concrete model, a family, or the
/// sentinel `All` value.
pub(crate) enum StreamingDeviceSelection {
    All,
    Family(DeviceFamily),
    Model(&'static devices::StreamingDevice),
}

#[derive(Parser, Clone)]
#[command(author, version, about, long_about = None)]
pub(crate) struct Args {
    /// Target device family/model, or "all" (default). Examples: chromecast, roku, apple_tv, fire_tv.
    #[arg(
        short = 'd',
        visible_short_alias = 's',
        long = "device",
        visible_alias = "streaming-devices",
        value_delimiter = ',',
        value_name = "DEVICE",
        value_parser = Args::parse_device_selection
    )]
    pub(crate) streaming_devices: Option<Vec<StreamingDeviceSelection>>,

    /// Path to the configuration file
    #[arg(short, long, value_parser = value_parser!(PathBuf))]
    pub(crate) config_file: Option<PathBuf>,

    /// Target video quality profile (defaults to match the source resolution/bitrate)
    #[arg(
        long,
        value_enum,
        default_value_t = VideoQuality::MatchSource,
        id = "video_quality"
    )]
    pub(crate) video_quality: VideoQuality,

    /// Target video codec preference (auto selects the intersection of device support)
    #[arg(
        long,
        value_enum,
        default_value_t = VideoCodecPreference::Auto,
        id = "video_codec"
    )]
    pub(crate) video_codec: VideoCodecPreference,

    /// Target audio quality profile (defaults to match the source bitrate)
    #[arg(
        long,
        value_enum,
        default_value_t = AudioQuality::MatchSource,
        id = "audio_quality"
    )]
    pub(crate) audio_quality: AudioQuality,

    /// Maximum video bitrate (e.g. 8M, 4800k, 5.5mbps)
    #[arg(long, value_parser = Args::parse_bitrate, id = "max_video_bitrate")]
    pub(crate) max_video_bitrate: Option<i64>,

    /// Maximum audio bitrate (e.g. 320k, 0.2M)
    #[arg(long, value_parser = Args::parse_bitrate, id = "max_audio_bitrate")]
    pub(crate) max_audio_bitrate: Option<i64>,

    /// Video file to convert (required unless probing)
    #[arg(value_parser = Args::parse_cstring)]
    pub(crate) input_file: Option<CString>,

    /// Our output direct-play-compatible video file (required unless probing)
    #[arg(value_parser = Args::parse_cstring)]
    pub(crate) output_file: Option<CString>,

    /// Policy for unsupported/extra video streams: convert|ignore|fail
    #[arg(
        long = "unsupported-video-policy",
        value_enum,
        default_value_t = UnsupportedVideoPolicy::Ignore,
        id = "unsupported_video_policy"
    )]
    pub(crate) unsupported_video_policy: UnsupportedVideoPolicy,

    /// Override the auto-selected primary video stream by index (0-based)
    #[arg(long = "primary-video-stream-index", id = "primary_video_stream_index")]
    pub(crate) primary_video_stream_index: Option<usize>,

    /// Criteria for auto-selecting the primary video stream
    #[arg(
        long = "primary-video-criteria",
        value_enum,
        default_value_t = PrimaryVideoCriteria::Resolution,
        id = "primary_video_criteria"
    )]
    pub(crate) primary_video_criteria: PrimaryVideoCriteria,

    /// Hardware acceleration preference (auto tries GPU encoders if available)
    #[arg(long, value_enum, default_value_t = HwAccel::Auto, id = "hw_accel")]
    pub(crate) hw_accel: HwAccel,

    /// Print detailed info about all streams in the input and exit
    #[arg(long, default_value_t = false)]
    pub(crate) probe_streams: bool,

    /// Print available HW devices/encoders and exit
    #[arg(long, default_value_t = false)]
    pub(crate) probe_hw: bool,

    /// Print all FFmpeg encoders/decoders and exit
    #[arg(long, default_value_t = false)]
    pub(crate) probe_codecs: bool,

    /// Filter probes to video codecs only
    #[arg(long, default_value_t = false)]
    pub(crate) only_video: bool,

    /// Filter probes to hardware-capable codecs only
    #[arg(long, default_value_t = false)]
    pub(crate) only_hw: bool,

    /// Emit probe output as JSON (machine-readable)
    #[arg(long, default_value_t = false)]
    pub(crate) probe_json: bool,

    /// Output format for probe results: text|json
    #[arg(long, value_enum, default_value_t = OutputFormat::Text)]
    pub(crate) output: OutputFormat,

    /// Filter streams for --probe-streams: all|video|audio|subtitle
    #[arg(long = "streams-filter", value_enum, default_value_t = StreamsFilter::All)]
    pub(crate) streams_filter: StreamsFilter,

    /// Evaluate OCR accuracy against fixture PNG+JSON pairs and exit (ground-truth CER/WER report).
    #[arg(
        long = "probe-ocr-fixtures",
        value_parser = value_parser!(PathBuf),
        id = "probe_ocr_fixtures"
    )]
    pub(crate) probe_ocr_fixtures: Option<PathBuf>,

    /// File extension to use when replacing Sonarr/Radarr media (default: mp4). Use 'match-input' to keep the original extension.
    #[arg(
        long = "servarr-output-extension",
        value_name = "EXTENSION",
        default_value = "mp4",
        id = "servarr_output_extension"
    )]
    pub(crate) servarr_output_extension: String,

    /// Suffix to append before the extension when replacing Sonarr/Radarr media (e.g. '.fixed')
    #[arg(
        long = "servarr-output-suffix",
        default_value = "",
        id = "servarr_output_suffix"
    )]
    pub(crate) servarr_output_suffix: String,

    /// Subtitle handling mode: auto converts bitmap subs via OCR, force processes all subtitle streams as text, skip disables subtitle processing.
    #[arg(
        long = "sub-mode",
        value_enum,
        default_value_t = SubMode::Auto,
        id = "sub_mode"
    )]
    pub(crate) sub_mode: SubMode,

    /// Default Tesseract language code used when subtitle stream language is missing (e.g. eng, spa, jpn).
    #[arg(long = "ocr-default-language", id = "ocr_default_language")]
    pub(crate) ocr_default_language: Option<String>,

    /// OCR backend to use for bitmap subtitle conversion.
    #[arg(
        long = "ocr-engine",
        value_enum,
        default_value_t = OcrEngine::Auto,
        id = "ocr_engine"
    )]
    pub(crate) ocr_engine: OcrEngine,

    /// OCR output format: srt (text only) or ass (positioned, colored).
    #[arg(
        long = "ocr-format",
        value_enum,
        default_value_t = OcrFormat::Srt,
        id = "ocr_format"
    )]
    pub(crate) ocr_format: OcrFormat,

    /// Executable and arguments used when --ocr-engine=external. Values are parsed without a shell. The command receives DPN_OCR_IMAGE and DPN_OCR_LANGUAGE env vars and must print recognized text to stdout.
    #[arg(
        long = "ocr-external-command",
        id = "ocr_external_command",
        hide = true
    )]
    pub(crate) ocr_external_command: Option<String>,

    /// Also write OCR subtitle sidecar .srt files next to the output file.
    #[arg(
        long = "ocr-write-srt-sidecar",
        default_value_t = false,
        id = "ocr_write_srt_sidecar"
    )]
    pub(crate) ocr_write_srt_sidecar: bool,

    /// Skip H.264 profile/level verification after transcode (troubleshooting for non-standard streams).
    #[arg(
        long = "skip-codec-check",
        default_value_t = false,
        id = "skip_codec_check"
    )]
    pub(crate) skip_codec_check: bool,

    /// Delete the source file after a successful conversion for direct CLI runs.
    /// In Sonarr/Radarr integration mode, successful replacement always removes the original while failures restore it.
    #[arg(
        long = "delete-source",
        value_name = "BOOL",
        num_args = 0..=1,
        default_missing_value = "true",
        value_parser = clap::builder::BoolishValueParser::new(),
        id = "delete_source"
    )]
    pub(crate) delete_source: Option<bool>,

    /// Trigger a Plex library refresh for the output directory after a successful conversion
    #[arg(long = "plex-refresh", default_value_t = false)]
    pub(crate) plex_refresh: bool,

    /// Base URL for the Plex server when using --plex-refresh (defaults to http://127.0.0.1:32400)
    #[arg(long = "plex-url")]
    pub(crate) plex_url: Option<String>,

    /// Plex API token used with --plex-refresh
    #[arg(long = "plex-token")]
    pub(crate) plex_token: Option<String>,
}

impl Args {
    pub(crate) fn parse_cstring(s: &str) -> Result<CString, String> {
        CString::new(s).map_err(|e| format!("Invalid CString: {}", e))
    }

    pub(crate) fn parse_bitrate(input: &str) -> Result<i64, String> {
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

        let numeric = number_str.replace([',', '_'], "");
        let value: f64 = numeric
            .parse()
            .map_err(|_| format!("Failed to parse bitrate '{}': invalid number", input))?;

        // Accept common human input variants like "8mbps", "8 mbit", or "8M/s".
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

    pub(crate) fn parse_device_selection(input: &str) -> Result<StreamingDeviceSelection, String> {
        let normalized = input.trim();
        if normalized.is_empty() {
            return Err("Streaming device value cannot be empty".to_string());
        }

        if normalized.eq_ignore_ascii_case("all") {
            return Ok(StreamingDeviceSelection::All);
        }

        if let Some(family) = DeviceFamily::from_identifier(normalized) {
            return Ok(StreamingDeviceSelection::Family(family));
        }

        devices::find_by_model(normalized)
            .map(StreamingDeviceSelection::Model)
            .ok_or_else(|| {
                let families = [
                    DeviceFamily::Chromecast,
                    DeviceFamily::Roku,
                    DeviceFamily::AppleTv,
                    DeviceFamily::FireTv,
                ]
                .iter()
                .map(|f| f.as_str())
                .collect::<Vec<_>>()
                .join(", ");
                let models = devices::supported_model_ids().join(", ");
                format!(
                    "Provided device '{}' not found. Valid values: all, device families [{}], and model ids [{}]",
                    input, families, models
                )
            })
    }
}

#[allow(dead_code)]
pub(crate) enum StreamExtras {
    Some((SwrContext, AVAudioFifo)),
    None,
}

/// Per-stream state carried across decode/encode/mux iterations.
///
/// `skip_stream` lets setup code keep stream ordering aligned with FFmpeg stream
/// indexes while disabling actual processing for selected streams.
pub(crate) struct StreamProcessingContext {
    pub(crate) decode_context: AVCodecContext,
    pub(crate) encode_context: AVCodecContext,
    pub(crate) stream_index: i32,
    pub(crate) media_type: ffi::AVMediaType,
    pub(crate) frame_buffer: Option<AVAudioFifo>, // TODO: Support video stream buffers too?
    pub(crate) resample_context: Option<SwrContext>,
    pub(crate) pts: AtomicI64,
    pub(crate) last_written_dts: Option<i64>,
    pub(crate) skip_stream: bool,
    #[allow(dead_code)]
    pub(crate) hw_device_ctx: Option<*mut ffi::AVBufferRef>,
    pub(crate) decoder_name: String,
}

impl std::fmt::Debug for StreamProcessingContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("StreamProcessingContext")
            .field("stream_index", &self.stream_index)
            .field("media_type", &self.media_type)
            .field("pts", &self.pts)
            .field("last_written_dts", &self.last_written_dts)
            .field("skip_stream", &self.skip_stream)
            .field("decoder_name", &self.decoder_name)
            .finish()
    }
}

pub(crate) fn init_audio_resampler(
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

pub(crate) fn add_samples_to_fifo(
    fifo: &mut AVAudioFifo,
    samples_buffer: &AVSamples,
    frame_size: i32,
) -> Result<()> {
    fifo.realloc(fifo.size() + frame_size);
    unsafe { fifo.write(samples_buffer.audio_data.as_ptr(), frame_size) }
        .context("Could not write data to FIFO")?;
    Ok(())
}

pub(crate) fn init_output_audio_frame(
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

pub(crate) fn encode_and_write_frame(
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
            Err(e) if is_eagain_error(&e) => {
                // Some FFmpeg/rsmpeg combinations surface EAGAIN as ReceivePacketError(-11).
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

pub(crate) fn is_eagain_error(err: &RsmpegError) -> bool {
    let raw = err.raw_error().unwrap_or_default();
    raw == ffi::AVERROR(ffi::EAGAIN) || raw == -(ffi::EAGAIN as i32)
}
