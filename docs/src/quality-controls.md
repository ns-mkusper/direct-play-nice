# Quality Controls

By default, the CLI preserves source quality (`match-source`) for both video
and audio.

## Video quality presets

`--video-quality` supports:

- `match-source`
- `360p`
- `480p`
- `720p`
- `1080p`
- `1440p`
- `2160p`

These presets apply resolution caps and target bitrate ranges suitable for
common direct-play scenarios.

## Audio quality presets

`--audio-quality` supports:

- `match-source`
- `320k`
- `256k`
- `224k`
- `192k`
- `160k`
- `128k`
- `96k`

## Custom bitrate overrides

Use these for explicit control:

- `--max-video-bitrate <RATE>` (for example `4800k`, `6M`, `12.5mbps`)
- `--max-audio-bitrate <RATE>`

When overrides are provided, they constrain the selected quality profile.
