# direct-play-nice

[![crates.io](https://img.shields.io/crates/v/direct_play_nice.svg)](https://crates.io/crates/direct_play_nice)
[![docs.rs](https://docs.rs/direct_play_nice/badge.svg)](https://docs.rs/direct_play_nice)
[![CI](https://github.com/ns-mkusper/direct-play-nice/actions/workflows/ci.yml/badge.svg)](https://github.com/ns-mkusper/direct-play-nice/actions/workflows/ci.yml)

`direct-play-nice` is a CLI tool that converts video files to profiles more
likely to Direct Play across common streaming devices.

## What Is Direct Play?

Direct Play means the client can play the original media file as-is, without
server-side video transcoding. In practice, this is usually the lowest-load,
highest-quality playback path for media servers.

Official references:

- Plex: <https://support.plex.tv/articles/200250387-streaming-media-direct-play-and-direct-stream/>
- Jellyfin codec support (goal is Direct Play): <https://jellyfin.org/docs/general/clients/codec-support>
- Emby playback methods: <https://emby.media/support/articles/DirectPlay-Stream-Transcoding.html>

## Quick Install

```bash
cargo install direct_play_nice
```

## Quick Start

Convert one file using the default multi-device profile:

```bash
direct_play_nice input.mkv output.mp4
```

Target specific device families:

```bash
direct_play_nice --device chromecast,roku input.mkv output.mp4
```

Probe local hardware/codec capabilities:

```bash
direct_play_nice --probe-hw --probe-codecs --only-video --only-hw --probe-json
```

## Common Usage Patterns

Convert one file with defaults:

```bash
direct_play_nice input.mkv output.mp4
```

Target a device-family intersection:

```bash
direct_play_nice --device chromecast,roku input.mkv output.mp4
```

Cap bitrate for bandwidth-constrained clients:

```bash
direct_play_nice --max-video-bitrate 8M --max-audio-bitrate 192k input.mkv output.mp4
```

Inspect input streams before conversion:

```bash
direct_play_nice --probe-streams input.mkv
direct_play_nice --probe-streams --output json input.mkv
```

Probe hardware and hardware-capable codecs:

```bash
direct_play_nice --probe-hw --probe-codecs --only-video --only-hw --probe-json
```

## Config File Examples

Load a config file with `--config-file /path/to/config.toml`, or place it at:

- `$XDG_CONFIG_HOME/direct-play-nice/config.toml`
- `~/.config/direct-play-nice/config.toml`

General CLI profile example:

```toml
streaming_devices = ["chromecast", "roku"]
video_quality = "1080p"
video_codec = "h264"
audio_quality = "192k"
max_video_bitrate = "8M"
max_audio_bitrate = "192k"
hw_accel = "auto"
unsupported_video_policy = "ignore"
primary_video_criteria = "resolution"
sub_mode = "auto"
ocr_engine = "auto"
ocr_format = "srt"
ocr_write_srt_sidecar = false
skip_codec_check = false
delete_source = false

[plex]
refresh = false
url = "http://127.0.0.1:32400"
token = ""
```

Sonarr/Radarr-focused example:

```toml
streaming_devices = "all"
servarr_output_extension = "mp4"
servarr_output_suffix = ".fixed"
video_codec = "h264"
video_quality = "1080p"
audio_quality = "192k"
hw_accel = "auto"
sub_mode = "auto"
ocr_engine = "pp-ocr-v4"
ocr_format = "srt"
ocr_write_srt_sidecar = false
skip_codec_check = false
```

## `--help` Output

```text
CLI program that converts video files to direct-play-compatible formats.

Usage: direct_play_nice [OPTIONS] [INPUT_FILE] [OUTPUT_FILE]

Arguments:
  [INPUT_FILE]
          Video file to convert (required unless probing)

  [OUTPUT_FILE]
          Our output direct-play-compatible video file (required unless probing)

Options:
  -d, --device <DEVICE>
          Target device family/model, or "all" (default). Examples: chromecast, roku, apple_tv, fire_tv

          [aliases: -s, --streaming-devices]

  -c, --config-file <CONFIG_FILE>
          Path to the configuration file

      --video-quality <video_quality>
          Target video quality profile (defaults to match the source resolution/bitrate)

          Possible values:
          - match-source: Leave video resolution/bitrate untouched
          - 360p:         360p (SD) profile – ~1.2 Mbps target bitrate
          - 480p:         480p (SD+) profile – ~2.5 Mbps target bitrate
          - 720p:         720p (HD) profile – ~5 Mbps target bitrate
          - 1080p:        1080p (Full HD) profile – ~8 Mbps target bitrate
          - 1440p:        1440p (Quad HD) profile – ~16 Mbps target bitrate
          - 2160p:        2160p (Ultra HD / 4K) profile – ~35 Mbps target bitrate

          [default: match-source]

      --video-codec <video_codec>
          Target video codec preference (auto selects the intersection of device support)

          [default: auto]
          [possible values: auto, h264, hevc]

      --audio-quality <audio_quality>
          Target audio quality profile (defaults to match the source bitrate)

          Possible values:
          - match-source: Leave audio bitrate untouched
          - 320k:         320 kbps (very high) AAC target bitrate
          - 256k:         256 kbps (high) AAC target bitrate
          - 224k:         224 kbps AAC target bitrate
          - 192k:         192 kbps (standard) AAC target bitrate
          - 160k:         160 kbps AAC target bitrate
          - 128k:         128 kbps (medium) AAC target bitrate
          - 96k:          96 kbps (low) AAC target bitrate

          [default: match-source]

      --max-video-bitrate <max_video_bitrate>
          Maximum video bitrate (e.g. 8M, 4800k, 5.5mbps)

      --max-audio-bitrate <max_audio_bitrate>
          Maximum audio bitrate (e.g. 320k, 0.2M)

      --unsupported-video-policy <unsupported_video_policy>
          Policy for unsupported/extra video streams: convert|ignore|fail

          [default: ignore]
          [possible values: convert, ignore, fail]

      --primary-video-stream-index <primary_video_stream_index>
          Override the auto-selected primary video stream by index (0-based)

      --primary-video-criteria <primary_video_criteria>
          Criteria for auto-selecting the primary video stream

          [default: resolution]
          [possible values: resolution, bitrate, fps]

      --hw-accel <hw_accel>
          Hardware acceleration preference (auto tries GPU encoders if available)

          Possible values:
          - auto:         Auto-select hardware acceleration (prefers CUDA path in current logic)
          - none:         Disable hardware acceleration
          - nvenc:        NVIDIA NVENC-based acceleration
          - vaapi:        VAAPI-based acceleration
          - qsv:          Intel Quick Sync Video acceleration
          - videotoolbox: Apple VideoToolbox acceleration
          - amf:          AMD AMF acceleration

          [default: auto]

      --probe-streams
          Print detailed info about all streams in the input and exit

      --probe-hw
          Print available HW devices/encoders and exit

      --probe-codecs
          Print all FFmpeg encoders/decoders and exit

      --only-video
          Filter probes to video codecs only

      --only-hw
          Filter probes to hardware-capable codecs only

      --probe-json
          Emit probe output as JSON (machine-readable)

      --output <OUTPUT>
          Output format for probe results: text|json

          [default: text]
          [possible values: text, json]

      --streams-filter <STREAMS_FILTER>
          Filter streams for --probe-streams: all|video|audio|subtitle

          [default: all]
          [possible values: all, video, audio, subtitle]

      --servarr-output-extension <EXTENSION>
          File extension to use when replacing Sonarr/Radarr media (default: mp4). Use 'match-input' to keep the original extension

          [default: mp4]

      --servarr-output-suffix <servarr_output_suffix>
          Suffix to append before the extension when replacing Sonarr/Radarr media (e.g. '.fixed')

          [default: ]

      --sub-mode <sub_mode>
          Subtitle handling mode: auto converts bitmap subs via OCR, force processes all subtitle streams as text, skip disables subtitle processing

          [default: auto]
          [possible values: auto, force, skip]

      --ocr-default-language <ocr_default_language>
          Default Tesseract language code used when subtitle stream language is missing (e.g. eng, spa, jpn)

      --ocr-engine <ocr_engine>
          OCR backend to use for bitmap subtitle conversion

          [default: auto]
          [possible values: auto, tesseract, pp-ocr-v3, pp-ocr-v4, external]

      --ocr-format <ocr_format>
          OCR output format: srt (text only) or ass (positioned, colored)

          [default: srt]
          [possible values: srt, ass]

      --ocr-write-srt-sidecar
          Also write OCR subtitle sidecar .srt files next to the output file

      --skip-codec-check
          Skip H.264 profile/level verification after transcode (troubleshooting for non-standard streams)

      --delete-source [<BOOL>]
          Delete the source file after a successful conversion for direct CLI runs. In Sonarr/Radarr integration mode, successful replacement always removes the original while failures restore it

          [possible values: true, false]

      --plex-refresh
          Trigger a Plex library refresh for the output directory after a successful conversion

      --plex-url <PLEX_URL>
          Base URL for the Plex server when using --plex-refresh (defaults to http://127.0.0.1:32400)

      --plex-token <PLEX_TOKEN>
          Plex API token used with --plex-refresh

  -h, --help
          Print help (see a summary with '-h')

  -V, --version
          Print version
```

## Supported Devices

For the full model matrix and constraints, see
[SUPPORTED_DEVICES.md](SUPPORTED_DEVICES.md).

## Documentation

For advanced usage, full CLI reference, OCR setup, Sonarr/Radarr integration,
and troubleshooting, read the manual:

- [direct-play-nice Book (mdBook)](docs/src/index.md)

For Rust API docs (library internals used by the CLI):

```bash
cargo doc --no-deps
```

## License

GPL-3.0-only
