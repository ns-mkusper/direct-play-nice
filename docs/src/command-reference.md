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

## Stream and compatibility controls

- `--unsupported-video-policy <unsupported_video_policy>` `convert|ignore|fail`
- `--primary-video-stream-index <primary_video_stream_index>`
- `--primary-video-criteria <primary_video_criteria>` `resolution|bitrate|fps`
- `--skip-codec-check`

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
- `--ocr-default-language <ocr_default_language>`
- `--ocr-engine <ocr_engine>` `auto|tesseract|pp-ocr-v3|pp-ocr-v4|external`
- `--ocr-format <ocr_format>` `srt|ass`
- `--ocr-write-srt-sidecar`

## Plex options

- `--plex-refresh`
- `--plex-url <PLEX_URL>`
- `--plex-token <PLEX_TOKEN>`

## Full generated help

For the exact, version-specific clap output:

```bash
direct_play_nice --help
```
