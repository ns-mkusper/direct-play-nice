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
