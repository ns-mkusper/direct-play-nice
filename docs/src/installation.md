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

Linux (x86_64, aarch64) and macOS:

```bash
curl --proto '=https' --tlsv1.2 -LsSf https://github.com/ns-mkusper/direct-play-nice/releases/latest/download/direct_play_nice-installer.sh | sh
```

Windows (PowerShell):

```powershell
powershell -ExecutionPolicy Bypass -c "irm https://github.com/ns-mkusper/direct-play-nice/releases/latest/download/direct_play_nice-installer.ps1 | iex"
```

To install from an archive instead, put the binary on your `PATH`, for example
in `/usr/local/bin`. Subtitle OCR needs the ONNX Runtime shared library at run
time; see [Subtitle OCR](subtitle-ocr.md).

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
