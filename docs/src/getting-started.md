# Getting Started

## First conversion

```bash
direct_play_nice input.mkv output.mp4
```

## Set up a default config file

If you run this tool often, create a config once and keep day-to-day commands
short.

By default, direct-play-nice reads:

- `$XDG_CONFIG_HOME/direct-play-nice/config.toml` (when `XDG_CONFIG_HOME` is set)
- `~/.config/direct-play-nice/config.toml`

Create the file:

```bash
mkdir -p ~/.config/direct-play-nice
cat > ~/.config/direct-play-nice/config.toml <<'EOF'
streaming_devices = "all"
video_quality = "match-source"
video_codec = "auto"
audio_quality = "192k"
hw_accel = "auto"
unsupported_video_policy = "ignore"
sub_mode = "auto"
ocr_default_language = "eng"
servarr_output_extension = "mp4"
servarr_output_suffix = ".fixed"

[plex]
refresh = false
EOF
```

### Why these defaults are sane

- `streaming_devices = "all"` keeps output compatible across all built-in
  device profiles.
- `video_quality = "match-source"` avoids unnecessary downscaling by default.
- `video_codec = "auto"` lets the tool pick the safest codec intersection.
- `audio_quality = "192k"` is a practical bitrate for broad AAC compatibility.
- `hw_accel = "auto"` uses hardware encoding when available and falls back to
  software when not.
- `unsupported_video_policy = "ignore"` skips extra video streams that can
  break muxing in common container/player paths.
- `sub_mode = "auto"` only OCRs bitmap subtitles when needed.
- `ocr_default_language = "eng"` gives OCR a stable fallback language.
- `servarr_output_extension = "mp4"` targets the most portable container for
  direct play.
- `servarr_output_suffix = ".fixed"` makes replaced files easy to identify
  during rollout.

### Override order

When the same option appears in multiple places, priority is:

1. CLI flags (highest)
2. `--config <path>`
3. `DIRECT_PLAY_NICE_CONFIG=<path>`
4. Default config location above

## Device targeting

Use `--device` to narrow compatibility constraints:

```bash
direct_play_nice --device chromecast input.mkv output.mp4
direct_play_nice --device chromecast,roku input.mkv output.mp4
```

`--device all` (or omitting `--device`) computes a profile compatible across all
built-in device definitions.

## Inspect an input before converting

```bash
direct_play_nice --probe-streams input.mkv
direct_play_nice --probe-streams --output json input.mkv
```

## File safety

The input file is only ever read. What happens to the output depends on mode:

- **Direct conversion** writes straight to the output path. An MKV output is
  first built as `<output stem>.conv.mp4` next to it, remuxed into the final
  file, and the intermediate is removed. A failed or interrupted run leaves the
  input untouched but can leave a partial output (and the `.conv.mp4`
  intermediate if the process was killed); delete those and rerun.
  `--delete-source` removes the input only after output validation passes.
- **Sonarr/Radarr mode** stages through a temporary file, parks the original
  under a `.bak` name, promotes by rename, then deletes the backup. See the
  [replacement policy](./servarr.html#replacement-policy).
- Inputs that already satisfy the target devices are left alone and no output
  is produced.

## See the plan without writing anything

```bash
direct_play_nice --dry-run input.mkv output.mp4
direct_play_nice --dry-run --output json input.mkv output.mp4
```

The report names the action (skip, transcode, or remux with OCR subtitles),
the reasons, every path the real run would create, and what happens to the
source. Nothing is written, renamed, or deleted. The same flag works inside a
Sonarr/Radarr `Download` event; see
[Sonarr/Radarr Integration](./servarr.html#dry-run).
