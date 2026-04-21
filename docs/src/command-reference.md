# Command Reference

## Synopsis

```bash
direct_play_nice [OPTIONS] [INPUT_FILE] [OUTPUT_FILE]
```

## Core options

- `--device <DEVICE>` target family/model set (`chromecast`, `roku`, `apple_tv`, `fire_tv`, or comma-separated intersection)
- `--video-quality <PROFILE>` one of `match-source`, `360p`, `480p`, `720p`, `1080p`, `1440p`, `2160p`
- `--audio-quality <PROFILE>` one of `match-source`, `320k`, `256k`, `224k`, `192k`, `160k`, `128k`, `96k`
- `--max-video-bitrate <RATE>` cap video bitrate (for example `8M`)
- `--max-audio-bitrate <RATE>` cap audio bitrate (for example `320k`)
- `--hw-accel <MODE>` one of `auto`, `none`, `nvenc`, `vaapi`, `qsv`, `videotoolbox`, `amf`

## Probe options

- `--probe-hw` report detected hardware backends/encoders
- `--probe-codecs` list FFmpeg encoder/decoder inventory
- `--probe-streams` inspect stream metadata for an input file
- `--only-video` filter probe output to video codecs
- `--only-hw` filter probe output to hardware-capable codecs
- `--probe-json` emit probe output as JSON
- `--output <FORMAT>` `text` or `json` for stream probe output

## Stream-selection options

- `--unsupported-video-policy <POLICY>` `convert|ignore|fail`
- `--primary-video-stream-index <INDEX>` manual primary stream override
- `--primary-video-criteria <CRITERIA>` `resolution|bitrate|fps`

## Full help

Run:

```bash
direct_play_nice --help
```
