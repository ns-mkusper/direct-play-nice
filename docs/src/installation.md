# Installation

## Docker

Multi-arch images (amd64, arm64) are published to GHCR with FFmpeg statically
linked, ONNX Runtime, and VA-API drivers included:

```bash
docker run --rm -v /path/to/media:/media \
  ghcr.io/ns-mkusper/direct-play-nice:latest /media/input.mkv /media/output.mp4
```

- `--gpus all` enables NVENC via the NVIDIA Container Toolkit.
- `--device /dev/dri` enables VA-API (Intel/AMD).
- OCR models download to `/config/models` on first use; mount a volume at
  `/config` to persist them.

Tags: `latest` (stable releases), `X.Y.Z` (each release), `edge` (main).

## Prebuilt binaries

Each GitHub release publishes platform archives and checksums built by
`cargo-dist`:

- `direct_play_nice-aarch64-apple-darwin.tar.xz`
- `direct_play_nice-x86_64-apple-darwin.tar.xz`
- `direct_play_nice-x86_64-unknown-linux-gnu.tar.xz`
- `direct_play_nice-aarch64-unknown-linux-gnu.tar.xz`
- `direct_play_nice-x86_64-pc-windows-msvc.zip`

The release also includes shell and PowerShell installers.

## From crates.io

```bash
cargo install direct_play_nice
```

## From source

```bash
git clone https://github.com/ns-mkusper/direct-play-nice.git
cd direct-play-nice
cargo build --release
```

Binary path:

```text
target/release/direct_play_nice
```
