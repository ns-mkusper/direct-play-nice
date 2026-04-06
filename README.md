# direct-play-nice

A CLI utility to convert video files to Direct-Play-compatible (the single best
optimization for your home video streaming server) formats.

## Purpose

This tool was created to convert video files to formats that satisfy Direct Play
requirements for all video files used by your video streaming server and their
streaming devices (eg.
[Chromcast](https://developers.google.com/cast/docs/media)). It is intended to
be added to a video-downloading service (ie. with
[Sonarr](https://wiki.servarr.com/sonarr/custom-scripts) or
[Radarr](https://wiki.servarr.com/radarr/custom-scripts)) as an optimization
feature and seamless to the end user (ie. Direct Play selected by default).

## Features

- Converts any video file supported by [FFmpeg](https://www.ffmpeg.org/)
- Satisfies Direct Play requirements for [Plex](https://www.plex.tv/),
  [Jellyfin](https://jellyfin.org/), etc.
- Can be used standalone or as a Custom Script Connection with
  [Sonarr](https://wiki.servarr.com/sonarr/custom-scripts),
  [Radarr](https://wiki.servarr.com/radarr/custom-scripts), etc.
- Allows for achieving [Direct Play][direct-play] with all videos in a
  way seamless to the end user.
- Offers intuitive quality presets (match-source, 360p through 2160p + audio
  caps) so you can keep output sizes in check for fast starts on Plex and other
  direct-play servers.
- Targets every supported streaming device by default, or narrow the profile
  with `--device`.

### Supported Streaming Devices

See [SUPPORTED_DEVICES.md](SUPPORTED_DEVICES.md) for the full up-to-date
matrix of:

- model IDs
- family/group targets (`chromecast`, `roku`, `apple_tv`, `fire_tv`)
- container, codec, and bitrate constraints
- official source links for each ecosystem

## Usage

This program can be run standlone directly on the CLI or as a Custom Script
Connection with [Sonarr](https://wiki.servarr.com/sonarr/custom-scripts) or
[Radarr](https://wiki.servarr.com/radarr/custom-scripts).

### CLI

```bash
Usage: direct_play_nice [OPTIONS] [INPUT_FILE] [OUTPUT_FILE]

Arguments:
  [INPUT_FILE]   Video file to convert (required unless probing)
  [OUTPUT_FILE]  Our output direct-play-compatible video file (required unless probing)

Options:
  -d, --device <DEVICE>
          Target device family/model, or 'all' (default). Examples:
          chromecast, roku, apple_tv, fire_tv
          [aliases: -s, --streaming-devices]
  -c, --config-file <CONFIG_FILE>
          Path to the configuration file
      --video-quality <VIDEO_QUALITY>
          Target video quality profile (defaults to match the source)
          [default: match-source] [possible values: match-source, 360p, 480p,
          720p, 1080p, 1440p, 2160p]
      --audio-quality <AUDIO_QUALITY>
          Target audio quality profile (defaults to match the source)
          [default: match-source] [possible values: match-source, 320k, 256k,
          224k, 192k, 160k, 128k, 96k]
      --max-video-bitrate <MAX_VIDEO_BITRATE>
          Maximum video bitrate (e.g. 8M, 4800k, 5.5mbps)
      --max-audio-bitrate <MAX_AUDIO_BITRATE>
          Maximum audio bitrate (e.g. 320k, 0.2M)
      --skip-codec-check
          Skip H.264 profile/level verification after transcode (troubleshooting for non-standard streams)
      --unsupported-video-policy <POLICY>
          How to handle unsupported/extra video streams: convert|ignore|fail
          (default: ignore)
      --primary-video-stream-index <INDEX>
          Override auto selection of primary video stream (0-based)
      --primary-video-criteria <CRITERIA>
          Auto-pick criteria for primary video: resolution|bitrate|fps
          (default: resolution)
      --probe-streams
          Print detailed info for all streams in the input and exit
      --streams-filter <FILTER>
          Filter streams in probe: all|video|audio|subtitle (default: all)
      --output <FORMAT>
          Output format for probe results: text|json (default: text)
      --hw-accel <HW_ACCEL>
          Hardware acceleration: auto|none|nvenc|vaapi|qsv|videotoolbox|amf
          (default: auto)
      --probe-hw
          Print available HW devices/encoders and exit
      --probe-codecs
          Print all FFmpeg encoders/decoders and exit
      --only-video
          Probe filter: only show video codecs
      --only-hw
          Probe filter: only show hardware-capable codecs
      --probe-json
          Output probe results as JSON
  -h, --help
          Print help
  -V, --version
          Print version
```

Notes:

- In probe modes (`--probe-hw`, `--probe-codecs`, `--probe-streams`), the
  positional `<INPUT_FILE> <OUTPUT_FILE>` are not required (except
  `--probe-streams`, which requires `<INPUT_FILE>`).
- `--device all` has the same effect as omitting `--device`: the tool computes a
  direct-play profile compatible across all known devices.
- Combine `--servarr-output-extension` and `--servarr-output-suffix` to control
  how Sonarr/Radarr replacements are named. When run from Sonarr, the CLI
  defaults to creating `Episode.fixed.<ext>` (with the extension derived from
  the conversion output).
- Use `--delete-source` if you want the original input removed after a
  successful conversion in direct CLI runs.
- In Sonarr/Radarr mode, successful replacement removes the original
  automatically, while failures restore the original from backup.
- The binary self-throttles: no more than two conversions run at once across all
  processes. Additional invocations wait until a slot is free.
- Tune concurrency via environment variables: set `DIRECT_PLAY_NICE_MAX_JOBS`
  for a global cap, or `DIRECT_PLAY_NICE_JOBS_PER_GPU` to control how many
  simultaneous encodes run on each detected GPU (default: two per NVIDIA/AMD
  device; NVIDIA is detected via `nvidia-smi`, AMD via `rocm-smi` on Linux or
  PowerShell on Windows). Machines without supported detection fall back to a
  single shared queue; set
  `DIRECT_PLAY_NICE_MAX_JOBS` manually if you want more parallelism on AMD or
  CPU-only hosts.

#### Device selection

Use family targets such as `--device chromecast`, `--device roku`,
`--device apple_tv`, `--device fire_tv`, or leave unset / use `--device all`
to target every supported device.

For exact model IDs, see [SUPPORTED_DEVICES.md](SUPPORTED_DEVICES.md).

### Examples

Convert for all devices (same as omitting `--device`):

```bash
direct_play_nice --device all input.mkv out.mp4
```

Convert for specific devices (intersection of capabilities):

```bash
direct_play_nice --device chromecast,roku input.mkv out.mp4
```

Probe hardware and codec availability (JSON):

```bash
direct_play_nice --probe-hw --probe-codecs --only-video --only-hw --probe-json
```

Probe input streams (text and JSON):

```bash
direct_play_nice --probe-streams input.mkv
direct_play_nice --probe-streams --output json input.mkv
```

### Subtitle OCR

Bitmap subtitles (PGS/VobSub/DVD) are not compatible with MP4 direct‑play.
When the output container is MP4, `direct_play_nice` can OCR those bitmap
streams into text subtitles.

Defaults:

- `--sub-mode auto` (default): only bitmap subtitle streams are
  OCR‑converted; text subtitles are preserved when possible.
- `--ocr-engine auto` (default): prefers PP‑OCRv4 when a GPU execution
  provider is available; otherwise falls back to Tesseract.
- `--ocr-format srt` (default): emits simple text subtitles (SRT/MOV_TEXT).
- If no bitmap subtitles are present, no OCR pass is performed.

Enable/override behavior:

- `--sub-mode=skip` disables all subtitle processing (no OCR, no subtitle
  output).
- `--sub-mode=force` keeps subtitle processing enabled even if you usually
  skip it.
- `--ocr-default-language <lang>` sets a fallback language code (e.g.
  `eng`, `spa`) when a subtitle stream is missing language metadata.
- `--ocr-format=ass` emits positioned/colored ASS. For MP4 outputs, ASS is
  downgraded to `mov_text`; use an MKV output if you want to preserve full
  ASS styling.
- `--ocr-write-srt-sidecar` optionally writes `.srt` sidecars next to the
  output file. Default is embedded-only subtitle output.

ONNX OCR engines (PP‑OCR):

- `--ocr-engine=pp-ocr-v4` uses the PP‑OCRv4 ONNX pipeline with execution
  provider fallback: CUDA → DirectML → CoreML → CPU.
- `--ocr-engine=pp-ocr-v3` uses the PP‑OCRv3 ONNX pipeline. This is useful
  for older GPUs where newer model/runtime combos are unstable.
- GPU execution providers require the matching runtime libraries (CUDA
  toolkit + cuDNN for NVIDIA, DirectML on Windows, CoreML on macOS).
  Missing runtimes fall back to CPU automatically.
- On Linux, the CUDA EP must match the `libonnxruntime.so` build. Run
  `./check_gpu_env.sh` (or `ldd` against the ONNX Runtime `.so`) to
  confirm which `libcudnn.so.*` is required and that it is discoverable
  via `LD_LIBRARY_PATH`/`ldconfig`.
- For containers, install the NVIDIA Container Toolkit and expose the
  CUDA/cuDNN libraries to the container (`--gpus all` or equivalent).
- `DPN_OCR_REQUIRE_GPU=1` forces GPU execution providers (fail fast if
  unavailable).
- `DPN_OCR_FORCE_CPU=1` disables GPU execution providers and forces
  CPU-only OCR.
- Models auto‑download into `models/` next to the executable, or
  `~/.config/direct-play-nice/models/` on Linux (override with
  `DPN_OCR_MODEL_DIR`).
- To swap models, drop replacement `.onnx` files into the model directory
  with the same filenames. v4 defaults:
  `ch_PP-OCRv4_det_infer.onnx`, `ch_ppocr_mobile_v2.0_cls_infer.onnx`,
  `en_PP-OCRv4_rec_infer.onnx`. v3 defaults:
  `ch_PP-OCRv3_det_infer.onnx`, `ch_ppocr_mobile_v2.0_cls_train.onnx`,
  `en_PP-OCRv3_rec_infer.onnx`.

Linux GPU runtime notes:

- Install matching NVIDIA driver, CUDA runtime, cuDNN, and ONNX Runtime
  CUDA provider versions.
- Keep CUDA/cuDNN/onnxruntime packages aligned. Partial upgrades can break
  provider loading.
- If your runtime libraries are in a non-standard path, set
  `ORT_DYLIB_PATH=/path/to/libonnxruntime.so`.
- For modern GPUs, `--ocr-engine auto` or `--ocr-engine pp-ocr-v4` is the
  preferred path.
- For legacy NVIDIA GPUs, `--ocr-engine pp-ocr-v3` can be more stable than
  v4 on older driver/runtime combinations.
- If GPU must be used, set `DPN_OCR_REQUIRE_GPU=1` to fail fast when no GPU
  execution provider is available.

Config file equivalents:

```toml
sub_mode = "auto"           # auto | force | skip
ocr_default_language = "eng"
ocr_engine = "auto"         # auto | tesseract | ppocrv3 | ppocrv4 | external
ocr_format = "srt"          # srt | ass
ocr_write_srt_sidecar = false
ocr_external_command = "python3 /opt/ocr/run.py"
```

### Sonarr / Radarr

When running via Sonarr, Radarr, etc you can use this program to convert each
downloaded video file to a Direct-Play-compatible format by adding it as a
Custom Script Connection (`Settings >> Connect >> Custom Script`).

The binary now auto-detects Sonarr/Radarr custom-script invocations:

- `sonarr_eventtype=Download` or `radarr_eventtype=Download` triggers an
  in-place transcode of the file referenced by the corresponding
  `$sonarr_episodefile_path` / `$radarr_moviefile_path` environment variable.
- All other event types (e.g. `Test`, `Grab`, `Rename`, etc.) exit cleanly
  without requiring any CLI arguments.
- By default the script promotes the converted output to an `.mp4` beside the
  original file (which is removed once the conversion completes). Override this
  by passing `--servarr-output-extension match-input` to keep the original
  container or any custom extension (e.g. `--servarr-output-extension mkv`).

Example Sonarr command:

```bash
/path/to/direct_play_nice --video-quality match-source --audio-quality match-source
```

Example Radarr command keeping the source container:

```bash
/path/to/direct_play_nice --servarr-output-extension match-input
```

Example Sonarr command that keeps all defaults (resulting in
`Episode.fixed.mp4` after conversion):

```bash
/path/to/direct_play_nice
```

Autopilot wrapper for Sonarr/Cron (GPU OCR + config file defaults):

```bash
#!/usr/bin/env bash
set -euo pipefail

# Optional: pinned ONNX runtime bundle.
RUNTIME_DIR=/opt/direct-play-nice/ort116-runtime
export ORT_DYLIB_PATH="$RUNTIME_DIR/lib/libonnxruntime.so"
export LD_LIBRARY_PATH="$RUNTIME_DIR/lib:/usr/lib:/opt/cuda/lib64"

# Keep cross-user lock files out of sticky /tmp.
export DIRECT_PLAY_NICE_LOCK_DIR=/var/lib/direct-play-nice/locks

# Maxwell-safe default (override if needed).
export DPN_OCR_SKIP_CLS=1

exec /usr/local/bin/direct_play_nice \
  --config-file /etc/direct_play_nice/config.toml \
  --video-quality match-source \
  --audio-quality match-source \
  "$@"
```

Notes:

- `ORT_DYLIB_PATH` is only needed when `libonnxruntime.so` is not on the
  default library path (or when multiple versions are installed and you want
  to pin one).
- `DPN_OCR_REQUIRE_GPU=1` is optional. Set it only if you want strict fail-fast
  behavior when GPU OCR libraries are missing.
- Keep the wrapper as the stable entry point for Sonarr/Radarr. Do not call a
  second binary path directly unless you also set `ORT_DYLIB_PATH` and
  `LD_LIBRARY_PATH`.
- For repeatable deployments, keep a manifest of runtime URLs and checksums
  next to the pinned runtime directory (example:
  `/opt/direct-play-nice/ort116-runtime/STACK_MANIFEST.txt`).

Example `/etc/direct_play_nice/config.toml` for unattended runs:

```toml
skip_codec_check = true
sub_mode = "auto"
ocr_engine = "ppocrv3" # recommended default for legacy NVIDIA GPUs
ocr_format = "srt"
ocr_write_srt_sidecar = false
```

> Tip: Sonarr/Radarr will see the new filename on their next library scan. If
> you convert to `.mp4`, Plex/Jellyfin can immediately direct play the result.

### Plex Refresh

Avoid the “Plex dance” by letting `direct_play_nice` trigger a targeted Plex
library refresh after a successful conversion. Supply a Plex token (and
optionally a custom server URL) via CLI flags or environment variables:

- `--plex-refresh` enables the behaviour for the current invocation. It is also
  activated automatically when both a Plex URL and token are provided through
  environment variables.
- `--plex-token <TOKEN>` or `DIRECT_PLAY_NICE_PLEX_TOKEN` / `PLEX_TOKEN`
  authenticate the refresh request.
- `--plex-url <http://host:port>` or `DIRECT_PLAY_NICE_PLEX_URL` override the
  Plex base URL (defaults to `http://127.0.0.1:32400`).
- Set `DIRECT_PLAY_NICE_PLEX_REFRESH=true` in the environment to make automatic
  refreshes the default for CLI runs.
- Prefer to store long-lived settings in `config.toml` under
  `$XDG_CONFIG_HOME/direct-play-nice` (fallback `~/.config/direct-play-nice`) or
  point at a custom file with either `--config-file` or the
  `DIRECT_PLAY_NICE_CONFIG` environment variable.
- Need a Plex token? Follow Plex’s official guide on locating your
  `X-Plex-Token` in their
  [support article][plex-token-support].

The tool looks up the Plex library section that contains the converted file
and invokes the server’s refresh endpoint for that directory, eliminating the
need to manually move files in and out of the library.

Example `~/.config/direct-play-nice/config.toml` (remember to quote strings):

```toml
streaming_devices = ["all"]
video_quality = "match-source"
audio_quality = "match-source"
max_video_bitrate = "2M"
max_audio_bitrate = "160k"
hw_accel = "auto"
unsupported_video_policy = "ignore"
servarr_output_extension = "mp4"
servarr_output_suffix = ".fixed"
delete_source = false

[plex]
refresh = true
url = "http://localhost:32400"
token = "PLEX-TOKEN-HERE"
```

Only the `[plex]` section is consumed today; the top-level keys mirror CLI
flags so you can keep preferred defaults documented alongside your Plex
credentials.

![Running as a custom script in Sonarr][sonarr-script-img]

<!-- markdownlint-disable MD013 -->
[direct-play]: https://support.plex.tv/articles/200250387-streaming-media-direct-play-and-direct-stream/
[plex-token-support]: https://support.plex.tv/articles/204059436-finding-an-authentication-token-x-plex-token/
<!-- markdownlint-enable MD013 -->
[sonarr-script-img]: media/readme/sonarr-add-custom-script.png

### Quality Controls

By default the CLI preserves source quality for both video and audio. To shrink
files ahead of Plex / Jellyfin direct play, mix and match:

- `--video-quality match-source|360p|480p|720p|1080p|1440p|2160p` applies
  intuitive resolution caps and matching H.264 bitrate ceilings. For example,
  `--video-quality 720p` clamps to 1280×720 and ~5 Mbps, while
  `--video-quality 2160p` scales to 3840×2160 at ~35 Mbps.
- `--audio-quality match-source|320k|256k|224k|192k|160k|128k|96k` sets AAC
  bitrate ceilings to well-known streaming tiers (e.g. `--audio-quality 192k`
  for a "standard" profile).
- Familiar aliases such as `--video-quality 4k`, `--video-quality full-hd`, or
  `--audio-quality high` map onto the presets above.

Need something custom? Use `--max-video-bitrate` and/or `--max-audio-bitrate` to
override the presets with any value such as `4800k`, `6M`, or `12.5mbps`.

### Troubleshooting

- H.264 profile/level verification can fail on non-standard streams or older
  hardware encoders. Use `--skip-codec-check` to bypass the verification step
  (or set `skip_codec_check = true` in `config.toml`) so the conversion can
  proceed without forcing a fallback.

## Building

This project relies on [FFmpeg](https://www.ffmpeg.org/) and
[rsmpeg](https://github.com/larksuite/rsmpeg) and builds on Mac, Linux and
Windows:

```bash
cargo install cargo-vcpkg
cargo vcpkg build
cargo build
```

After running `cargo vcpkg build` once, point `VCPKG_ROOT` at the shared
installation so builds and CI reuse the same FFmpeg toolchain (this repo
defaults to `/opt/vcpkg` on Unix and `C:\\src\\vcpkg` on Windows):

```bash
export VCPKG_ROOT=/opt/vcpkg
```

The repo defaults to `/opt/vcpkg` on Unix-like platforms and `C:\\src\\vcpkg` on
Windows; override this environment variable if your setup differs. Our
`Cargo.toml` pins vcpkg to commit `21012a516c9e5fa547baf212f2d937cd8d15dcb5`
which includes FFmpeg 8—if you reuse an existing checkout, make sure it is
checked out to that revision:

```powershell
# Windows PowerShell
git -C $env:VCPKG_ROOT fetch https://github.com/cqundefine/vcpkg.git
git -C $env:VCPKG_ROOT checkout 21012a516c9e5fa547baf212f2d937cd8d15dcb5
```

```bash
# macOS/Linux
git -C "$VCPKG_ROOT" fetch https://github.com/cqundefine/vcpkg.git
git -C "$VCPKG_ROOT" checkout 21012a516c9e5fa547baf212f2d937cd8d15dcb5
```

## Tests

```bash
# Builds assume `/opt/vcpkg`; override VCPKG_ROOT if you keep vcpkg elsewhere.
# export VCPKG_ROOT=/opt/vcpkg
cargo test
```

For explicit parallel execution across profiles/devices, run:

```bash
./scripts/test-concurrent.sh
```

Integration tests that synthesize media depend on the `ffmpeg` CLI. They are
gated behind the `ffmpeg-cli-tests` feature and are run in CI only. To run
them locally:

```bash
VCPKG_ROOT=/opt/vcpkg cargo test --features ffmpeg-cli-tests
```

### Optional: NVENC regression suite

The NVENC matrix exercises the hardware encoder across multiple device/bitrate
profiles. Because it depends on NVIDIA hardware (with working `h264_nvenc`
support) it is opt-in. Enable it with:

```bash
ENABLE_NVENC_TESTS=1 cargo test nvenc_matrix -- --test-threads=1
```

The existing single-case NVENC integration test also participates when the same
environment variable is set.

### Optional: direnv

If you use `direnv`, add the following to `.envrc` so shells pick up the shared
vcpkg install:

```sh
export VCPKG_ROOT=/opt/vcpkg
export RUST_LOG=${RUST_LOG:-WARN}
```

Then run `direnv allow` once in the repo.

## Release Automation

Versioning and release publication are automated on merges to `main`:

- `.github/workflows/cd.yml` runs `release-plz update` first (to bump
  `Cargo.toml`/`Cargo.lock` and `CHANGELOG.md`), commits those release metadata
  updates, then runs `release-plz release` to publish tags/releases.
- `release-plz.toml` controls release behavior, and `cliff.toml` controls how
  commits are grouped/rendered into changelog/release notes.
- `scripts/generate_release_notes.sh` generates a `RELEASE_NOTES.md` preview in
  CI, uploaded as an artifact before the release step.

### Version bump rules

By default, `release-plz` follows Conventional Commit semantics:

- `feat:` -> minor bump
- `fix:` (and non-breaking maintenance changes) -> patch bump
- `!` marker or `BREAKING CHANGE` footer -> major bump

So bump control lives in:

- commit messages in merged PRs
- `release-plz.toml` (release behavior)
- `cliff.toml` (what changes are included/displayed in notes)

## Contributions / Support

If you run into any issues while using this software or want to add a feature or
bug fix feel free to raise an issue.
