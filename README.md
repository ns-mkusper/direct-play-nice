# direct-play-nice

[![crates.io](https://img.shields.io/crates/v/direct_play_nice.svg)](https://crates.io/crates/direct_play_nice)
[![docs.rs](https://docs.rs/direct_play_nice/badge.svg)](https://docs.rs/direct_play_nice)
[![CI](https://github.com/ns-mkusper/direct-play-nice/actions/workflows/ci.yml/badge.svg)](https://github.com/ns-mkusper/direct-play-nice/actions/workflows/ci.yml)

`direct-play-nice` is a cross-platform CLI tool that converts video files
to profiles more likely to Direct Play across common streaming devices.

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
[Hardware Acceleration](https://ns-mkusper.github.io/direct-play-nice/hardware-acceleration.html).

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
# Output validation is enabled by default; keep these explicit in Servarr mode
# if you want config-visible safety settings.
validate_output = true
visual_validate_output = true
visual_quality_report = false
visual_scan_frames = 120
visual_sample_interval = 15
visual_failure_ratio = 0.60

# Optional: require imported media to contain English audio.
# Start with dry-run while tuning candidate policy and custom formats.
servarr_language_check = true
servarr_language_dry_run = true
servarr_language_candidate_policy = "custom-format-or-title"
required_audio_languages = "eng"
# Leave subtitle requirements empty unless subtitle completeness is a goal.
required_subtitle_languages = ""
# Optional: for trusted English-native libraries, retag untagged audio before
# deciding the file is missing English audio.
servarr_untagged_audio_language = "eng"
```

To catch delayed dubs/subs that arrive after the first import, run the same
binary periodically with `--servarr-language-audit`. Use
`--servarr-language-audit-scope inventory` for a Sonarr current-library sweep.
Before applying broad language replacements, follow the
[Safe language upgrade runbook](https://ns-mkusper.github.io/direct-play-nice/servarr.html#safe-language-upgrade-runbook)
in the Sonarr/Radarr manual.

![Running as a custom script in Sonarr][sonarr-script-img]

## Supported Devices

For the full model matrix and constraints, see
[SUPPORTED_DEVICES.md](SUPPORTED_DEVICES.md).

## Documentation

For advanced usage, read the manual:

- [direct-play-nice Book (mdBook)](https://ns-mkusper.github.io/direct-play-nice/)
- AI OCR for bitmap subtitles (setup + runtime notes): [Subtitle OCR](https://ns-mkusper.github.io/direct-play-nice/subtitle-ocr.html)
- GPU acceleration and supported architecture references: [Hardware Acceleration](https://ns-mkusper.github.io/direct-play-nice/hardware-acceleration.html)
- Plex auto-refresh workflow: [Plex Refresh](https://ns-mkusper.github.io/direct-play-nice/plex-refresh.html)
- Arr custom-script operation: [Sonarr/Radarr Integration](https://ns-mkusper.github.io/direct-play-nice/servarr.html)
- Hardware probing and diagnostics: [Probe and Debug](https://ns-mkusper.github.io/direct-play-nice/probe-and-debug.html)

For Rust API docs (library internals used by the CLI):

```bash
cargo doc --no-deps
```

## License

GPL-3.0-only

[sonarr-script-img]: media/readme/sonarr-add-custom-script.png
