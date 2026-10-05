# direct-play-nice

[![crates.io](https://img.shields.io/crates/v/direct_play_nice.svg)](https://crates.io/crates/direct_play_nice)
[![docs.rs](https://docs.rs/direct_play_nice/badge.svg)](https://docs.rs/direct_play_nice)
[![CI](https://github.com/ns-mkusper/direct-play-nice/actions/workflows/ci.yml/badge.svg)](https://github.com/ns-mkusper/direct-play-nice/actions/workflows/ci.yml)

`direct-play-nice` is a cross-platform CLI tool that converts video files
to profiles more likely to Direct Play across common streaming devices.

It fills the gap in the usual Sonarr/Radarr plus Plex, Jellyfin, or Emby
setup: the Arr apps fetch media and the server plays it, but nothing in
between makes sure every file plays on every device without a server-side
transcode. Hooked into the Arr import pipeline, `direct-play-nice` closes that
gap, so the library stays fully automated and playback stays fast.

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

## How Files Are Replaced

`direct_play_nice` never writes into the input file. Every mode reads the
source, writes a new file, and only then decides what happens to the source.

**Direct conversion** (`direct_play_nice input.mkv output.mp4`):

- The output is written straight to the output path. An MKV output is first
  built as `<output stem>.conv.mp4` next to it, then remuxed into the final
  file, and the intermediate is removed.
- A failed or interrupted run leaves the input untouched. It can leave a
  partial file at the output path, plus the `.conv.mp4` intermediate if the
  process was killed. Delete those and rerun.
- `--delete-source` removes the input only after the output exists and passes
  output validation.

**Sonarr/Radarr mode** (custom-script `Download` events):

- The converted file is written to a temporary path next to the final one:
  `<final>.direct-play-nice.tmp.<ext>`. The original is not touched while the
  transcode runs.
- On success the original is renamed to `<input>.direct-play-nice.bak.<ext>`,
  the temporary file is renamed to the final path, and the backup is deleted.
  Both steps are renames on the same filesystem, so the final path only ever
  holds a complete file.
- On a transcode or validation failure the temporary file is removed and the
  original stays in place.
- If the process is killed mid-transcode, the original stays in place and the
  temporary file is left behind. Delete it. If the process dies between the
  two renames, the original is still present under the `.bak` name; rename it
  back by hand.
- Inputs that already satisfy the target devices are left alone and no output
  is produced.

**Dry run.** Pass `--dry-run` (or set `dry_run = true` in the config file) to
see the plan for a file without writing, renaming, or deleting anything. It
works for direct conversion, Sonarr/Radarr `Download` events, and the language
audit, where it also turns on `servarr_language_dry_run`:

```bash
direct_play_nice --dry-run input.mkv output.mp4
```

```text
Dry run: no file will be written, renamed, or deleted.
  Mode:            direct conversion
  Input:           /media/input.mkv
  Action:          transcode
                   - video codec mpeg4 is not compatible with required H.264
                   - no audio stream with compatible codec AAC found
  Output:          /media/output.mp4
  Source:          left untouched
  Target devices:  Chromecast (1st gen), Roku Ultra
  Codecs:          video H.264, audio AAC, container mp4
  Subtitle OCR:    enabled
```

In Sonarr/Radarr mode the report also lists the temporary and backup paths.
Add `--output json` for a machine-readable report.

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

## Sonarr/Radarr Import and Upgrade Hook Example

Use `Settings -> Connect -> Custom Script` in Sonarr or Radarr. Enable both
`On Import` (called `On Download` in some versions) and `On Upgrade` so new files
and replacements both run DPN. Both subscriptions send a `Download` event;
upgrades also set `sonarr_isupgrade` or `radarr_isupgrade` to `True`.

Enabling only the initial-import hook lets upgrades replace converted media
without running DPN again. This hook setting is separate from allowing quality
upgrades in a quality profile. See the manual's
[hook setup and verification](https://ns-mkusper.github.io/direct-play-nice/servarr.html#hook-setup-and-verification)
for checks and existing-library limitations.

Point the script to the `direct_play_nice` binary with a service-specific config
file (Sonarr shown here; use the Radarr config for Radarr):

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

# Optional: print the replacement plan and exit without writing anything.
# Useful on a first run against a real library; remove once the paths look right.
# dry_run = true

# Optional: require imported media to contain English audio.
# Start with dry-run while tuning candidate policy and custom formats.
# dry_run = true above also implies this setting.
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
`--servarr-language-audit-scope latest-missing` for a pass that spends the
search budget on the newest aired/released non-compliant items first, or
`--servarr-language-audit-scope inventory` for a full current-library sweep.
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
