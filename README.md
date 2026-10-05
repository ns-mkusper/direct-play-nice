# direct-play-nice

[![crates.io](https://img.shields.io/crates/v/direct_play_nice.svg)](https://crates.io/crates/direct_play_nice)
[![docs.rs](https://docs.rs/direct_play_nice/badge.svg)](https://docs.rs/direct_play_nice)
[![CI](https://github.com/ns-mkusper/direct-play-nice/actions/workflows/ci.yml/badge.svg)](https://github.com/ns-mkusper/direct-play-nice/actions/workflows/ci.yml)

`direct-play-nice` is a cross-platform CLI tool that converts video files
to profiles more likely to Direct Play across common streaming devices.

The overall goal is to fill in common gaps between existing FOSS media server
software to give a fully automated, performant, and reliable hands-free
streaming server for both admins and users.

```mermaid
flowchart LR
    DL[Download client] --> ARR[Sonarr / Radarr import]
    ARR -- "Download event<br/>(custom script)" --> DPN[direct-play-nice]
    DPN -- "replaces file with a<br/>Direct Play profile" --> LIB[(Media library)]
    LIB --> SRV[Plex / Jellyfin / Emby]
    SRV -- "Direct Play,<br/>no server transcode" --> DEV[Chromecast, Roku,<br/>Apple TV, Fire TV]
```

## What Is Direct Play?

Direct Play means the client can play the original media file as-is, without
server-side video transcoding. In practice, this is usually the lowest-load,
highest-quality playback path for media servers.

Official references:

- Plex: <https://support.plex.tv/articles/200250387-streaming-media-direct-play-and-direct-stream/>
- Jellyfin codec support (goal is Direct Play): <https://jellyfin.org/docs/general/clients/codec-support>
- Emby playback methods: <https://emby.media/support/articles/DirectPlay-Stream-Transcoding.html>

## Quick Install

### Docker

```bash
docker run --rm -v /path/to/media:/media \
  ghcr.io/ns-mkusper/direct-play-nice:latest /media/input.mkv /media/output.mp4
```

Add `--gpus all` for NVENC (requires the [NVIDIA Container Toolkit](https://docs.nvidia.com/datacenter/cloud-native/container-toolkit/latest/install-guide.html))
or `--device /dev/dri` for VA-API. OCR models download on first use; mount
`-v dpn-config:/config` to persist them.

### Pre-built binaries

Installer script (Linux x86_64/aarch64, macOS):

```bash
curl --proto '=https' --tlsv1.2 -LsSf https://github.com/ns-mkusper/direct-play-nice/releases/latest/download/direct_play_nice-installer.sh | sh
```

Windows (PowerShell):

```powershell
powershell -ExecutionPolicy Bypass -c "irm https://github.com/ns-mkusper/direct-play-nice/releases/latest/download/direct_play_nice-installer.ps1 | iex"
```

Or download an archive from [GitHub Releases](https://github.com/ns-mkusper/direct-play-nice/releases)
and drop the binary into `/usr/local/bin`. Subtitle OCR needs the ONNX Runtime
shared library at runtime — see [the OCR docs](https://ns-mkusper.github.io/direct-play-nice/subtitle-ocr.html).

### From crates.io (developers)

Requires a Rust toolchain; FFmpeg is built and statically linked via vcpkg.

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

The input is never modified. Sonarr/Radarr replacements go through a temp file
and an atomic rename, so a failed or killed run leaves the original in place.
Add `--dry-run` to print the plan without writing anything. Details:
[File safety](https://ns-mkusper.github.io/direct-play-nice/getting-started.html#file-safety).

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
