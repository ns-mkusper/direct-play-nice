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
  codecs, stream hygiene, or temporal checks fail (enabled by default)
- `--no-validate-output` disable post-conversion output validation
- `--visual-validate-output` decode sampled output frames and fail on obvious
  visual corruption such as repeated green-screen frames (enabled by default)
- `--no-visual-validate-output` disable sampled visual validation while keeping
  structural validation enabled
- `--visual-quality-report` log sampled luma/chroma statistics for
  troubleshooting scaling or visual-corruption issues
- `--visual-scan-frames <N>` set how many decoded video frames visual
  validation scans before deciding the output is safe enough to promote
- `--visual-sample-interval <N>` inspect every Nth decoded frame during visual
  validation; default is 15
- `--visual-failure-ratio <R>` set the sampled-frame fraction that must look
  corrupt before visual validation fails; default is 0.60

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
- `--servarr-language-audit` run a periodic Sonarr/Radarr audit for
  delayed language upgrades
- `--servarr-language-audit-scope <SCOPE>` choose `history`, `inventory`, or
  `latest-missing` audit source
- `--servarr-language-audit-lookback-days <DAYS>` recent import window for
  history audit mode
- `--servarr-language-audit-max-searches <N>` cap release searches per audit run
- `--servarr-language-audit-no-candidate-cooldown-days <D>` skip items
  with a recent no-candidate audit result for `D` days; `0` disables this
- `--servarr-language-audit-latest-missing-no-candidate-cooldown-days <D>`
  override the no-candidate cooldown for `latest-missing` audits
- `--servarr-language-audit-episode-ids <IDS>` comma-separated Sonarr episode
  IDs to audit instead of the full scope
- `--servarr-language-check` enable pre-conversion language checks for Arr
  downloads
- `--required-audio-languages <LANGS>` comma-separated ISO-639 tags such as
  `eng,jpn`
- `--required-subtitle-languages <LANGS>` comma-separated ISO-639 tags such as
  `eng,spa`
- `--servarr-api-url <URL>` Sonarr/Radarr base URL for mismatch replacement
  checks
- `--servarr-api-key <KEY>` Sonarr/Radarr API key for mismatch replacement checks
- `--servarr-language-dry-run` evaluate candidates without grabbing or
  blocklisting
- `--servarr-untagged-audio-language <LANG>` opt-in language tag to apply to
  untagged audio streams before redownload decisions
- `--servarr-untagged-subtitle-language <LANG>` opt-in language tag to apply to
  untagged subtitle streams before redownload decisions
- `--servarr-language-candidate-policy <POLICY>`
  `strict|custom-format|custom-format-or-title|title-guess`
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
