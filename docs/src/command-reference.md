# Command Reference

## Synopsis

```bash
direct_play_nice [OPTIONS] [INPUT_FILE] [OUTPUT_FILE]
```

## Positional arguments

- `[INPUT_FILE]` video file to convert (required unless probing)
- `[OUTPUT_FILE]` output media file (required unless probing)

## Device and quality options

- `-d, --device <DEVICE>` target family/model or `all`
- `--video-quality <video_quality>` quality preset
- `--video-codec <video_codec>` `auto|h264|hevc`
- `--audio-quality <audio_quality>` quality preset
- `--max-video-bitrate <max_video_bitrate>` explicit video cap
- `--max-audio-bitrate <max_audio_bitrate>` explicit audio cap
- `--resize-quality <resize_quality>` `fast-bilinear|bilinear|bicubic|lanczos|spline`
- `--resize-backend <resize_backend>` `auto|software|cuda`

## Stream and compatibility controls

- `--unsupported-video-policy <unsupported_video_policy>` `convert|ignore|fail`
- `--primary-video-stream-index <primary_video_stream_index>`
- `--primary-video-criteria <primary_video_criteria>` `resolution|bitrate|fps`
- `--skip-codec-check`
- `--validate-output` reopen the completed output and fail if expected A/V
  codecs or stream hygiene checks do not pass

## Hardware controls

- `--hw-accel <hw_accel>` `auto|none|nvenc|vaapi|qsv|videotoolbox|amf`

## Probe modes

- `--probe-streams`
- `--probe-hw`
- `--probe-codecs`
- `--probe-ocr-fixtures <PATH>` evaluate OCR accuracy against fixture PNG+JSON pairs
- `--only-video`
- `--only-hw`
- `--probe-json`
- `--output <OUTPUT>` `text|json`
- `--streams-filter <STREAMS_FILTER>` `all|video|audio|subtitle`

## Sonarr/Radarr options

- `--servarr-output-extension <EXTENSION>` (`match-input` supported)
- `--servarr-output-suffix <servarr_output_suffix>`
- `--delete-source [<BOOL>]`

## Subtitle OCR options

- `--sub-mode <sub_mode>` `auto|force|skip`
- `--subtitle-failure-policy <subtitle_failure_policy>` `skip-stream|fail`
- `--ocr-default-language <ocr_default_language>`
- `--ocr-engine <ocr_engine>` `auto|tesseract|pp-ocr-v3|pp-ocr-v4|external`
- `--ocr-format <ocr_format>` `srt|ass`
- `--ocr-write-srt-sidecar`

## Failure policy

- Extra video streams follow `--unsupported-video-policy`: `ignore` drops them,
  `fail` aborts, and `convert` attempts to include them when the output
  container supports it.
- Attachments, data streams, and attached pictures are treated as metadata and
  skipped for direct-play outputs.
- Bitmap subtitles are handled by the OCR side pass unless `--sub-mode=skip`;
  text subtitles are converted to MP4-compatible timed text when included.
- Subtitle decode/encode timestamp failures follow
  `--subtitle-failure-policy`: `skip-stream` warns and disables only that
  subtitle stream, while `fail` aborts conversion.
- Audio conversion setup is fail-fast: if FFmpeg cannot initialize the required
  resampler, conversion aborts rather than writing suspect audio.
- Hardware encoder/profile failures may retry with a safer software encoder
  path; decoder bitstream failures remain hard failures.

## Plex options

- `--plex-refresh`
- `--plex-url <PLEX_URL>`
- `--plex-token <PLEX_TOKEN>`

## Full generated help

For the exact, version-specific clap output:

```bash
direct_play_nice --help
```
