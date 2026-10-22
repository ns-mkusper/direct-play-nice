# direct-play-nice

[![crates.io](https://img.shields.io/crates/v/direct_play_nice.svg)](https://crates.io/crates/direct_play_nice)
[![docs.rs](https://docs.rs/direct_play_nice/badge.svg)](https://docs.rs/direct_play_nice)
[![CI](https://github.com/ns-mkusper/direct-play-nice/actions/workflows/ci.yml/badge.svg)](https://github.com/ns-mkusper/direct-play-nice/actions/workflows/ci.yml)

`direct-play-nice` is a cross-platform CLI tool that converts video files
to profiles more likely to Direct Play across common streaming devices.

The overall goal is to fill in common gaps between existing FOSS media server
software to give a fully automated, performant, and reliable hands-free
streaming server for both admins and users.

## What Is Direct Play?

Direct Play means the client can play the original media file as-is, without
server-side video transcoding. In practice, this is usually the lowest-load,
highest-quality playback path for media servers.

Official references:

- Plex: <https://support.plex.tv/articles/200250387-streaming-media-direct-play-and-direct-stream/>
- Jellyfin codec support (goal is Direct Play): <https://jellyfin.org/docs/general/clients/codec-support>
- Emby playback methods: <https://emby.media/support/articles/DirectPlay-Stream-Transcoding.html>

## Install

```bash
# Docker
docker run --rm -v /path/to/media:/media \
  ghcr.io/ns-mkusper/direct-play-nice:latest /media/input.mkv /media/output.mp4

# Linux / macOS binary
curl --proto '=https' --tlsv1.2 -LsSf https://github.com/ns-mkusper/direct-play-nice/releases/latest/download/direct_play_nice-installer.sh | sh
```

Windows, archives, `cargo install`, GPU containers, and the ONNX Runtime
library needed for subtitle OCR are covered in
[Installation](https://ns-mkusper.github.io/direct-play-nice/installation.html).

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

The input is never modified. Every output is written to a
`.direct-play-nice.tmp` file next to its final path and promoted by rename, so
a failed or killed run leaves the original in place and never a half-written
output. Add `--dry-run` to print the plan without writing anything. Details:
[File safety](https://ns-mkusper.github.io/direct-play-nice/getting-started.html#file-safety).

## Sonarr/Radarr Hook

Add `direct_play_nice --config-file /path/to/config.toml` as a Custom Script
in Sonarr or Radarr with `On Import` and `On Upgrade` enabled. Each import is
converted and swapped in atomically; failures leave the original untouched.
Start with `dry_run = true` to see the planned paths. Setup, config example,
language checks, and the replacement policy are in the
[Sonarr/Radarr manual](https://ns-mkusper.github.io/direct-play-nice/servarr.html).

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
