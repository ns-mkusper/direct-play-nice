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

## GPU Acceleration

`direct_play_nice` supports GPU acceleration in two places:

- Bitmap subtitle OCR (PGS/VobSub/DVD) via ONNX Runtime providers
- H.264/HEVC hardware transcoding via FFmpeg hardware encoders

Project-specific behavior:

- `--ocr-engine auto` prefers `pp-ocr-v4` on modern GPU stacks
- legacy NVIDIA (Maxwell-class / compute capability `<= 5`) auto-selects
  `pp-ocr-v3` for better stability
- OCR benchmark evidence in this repo shows full-movie OCR at `87.62 FPS`
  (`3.65x` realtime) on a self-hosted Linux GPU run
  ([OCR benchmark report](benches/OCR_BENCHMARK.md))

Official compatibility and architecture references are collected in the manual:
[Hardware Acceleration](docs/src/hardware-acceleration.md).

## Sonarr Download Hook Example

Use Sonarr `Settings -> Connect -> Custom Script` and enable the script on the
`On Download` event. Point it to the `direct_play_nice` binary with a config
file:

```bash
/path/to/direct_play_nice --config-file /path/to/direct-play-nice-sonarr.toml
```

Example `direct-play-nice-sonarr.toml`:

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

![Running as a custom script in Sonarr][sonarr-script-img]

## Supported Devices

For the full model matrix and constraints, see
[SUPPORTED_DEVICES.md](SUPPORTED_DEVICES.md).

## Documentation

For advanced usage, read the manual:

- [direct-play-nice Book (mdBook)](docs/src/index.md)
- AI OCR for bitmap subtitles (setup + runtime notes): [Subtitle OCR](docs/src/subtitle-ocr.md)
- GPU acceleration and supported architecture references: [Hardware Acceleration](docs/src/hardware-acceleration.md)
- Plex auto-refresh workflow: [Plex Refresh](docs/src/plex-refresh.md)
- Arr custom-script operation: [Sonarr/Radarr Integration](docs/src/servarr.md)
- Hardware probing and diagnostics: [Probe and Debug](docs/src/probe-and-debug.md)

For Rust API docs (library internals used by the CLI):

```bash
cargo doc --no-deps
```

## License

GPL-3.0-only

[sonarr-script-img]: media/readme/sonarr-add-custom-script.png
