# Supported Devices

`direct-play-nice` supports selecting either device families (for example `--device roku`) or specific model ids (for example `--device roku_ultra`).

When `--device all` is used (or omitted), the tool computes a strict least-common-denominator target that is compatible across every device listed below.

## Chromecast Family

Source: https://developers.google.com/cast/docs/media

| Model ID | Device | Containers | Video Codecs | Audio Codecs | Max Resolution | Max FPS | Max Video Bitrate | Max Audio Bitrate |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `chromecast_1st_gen` | Chromecast (1st gen) | MP4 | H.264 | AAC | 1080p | 30 | 20 Mbps | 320 kbps |
| `chromecast_2nd_gen` | Chromecast (2nd gen) | MP4 | H.264 | AAC | 1080p | 60 | 20 Mbps | 320 kbps |
| `chromecast_3rd_gen` | Chromecast (3rd gen) | MP4 | H.264, VP8 | AAC | 1080p | 60 | 25 Mbps | 320 kbps |
| `chromecast_ultra` | Chromecast Ultra | MP4 | H.264, VP8 | AAC | 2160p | 60 | 35 Mbps | 320 kbps |
| `chromecast_google_tv` | Chromecast with Google TV | MP4 | H.264, HEVC, VP9 | AAC | 2160p | 60 | 45 Mbps | 320 kbps |
| `google_tv_streamer` | Google TV Streamer | MP4 | H.264, HEVC, VP9 | AAC | 2160p | 60 | 45 Mbps | 320 kbps |
| `nest_hub` | Nest Hub | MP4 | H.264, VP9 | AAC | 720p | 60 | 12 Mbps | 256 kbps |
| `nest_hub_max` | Nest Hub Max | MP4 | H.264, VP9 | AAC | 720p | 30 | 12 Mbps | 256 kbps |

## Roku Family

Source: https://developer.roku.com/docs/specs/media/streaming-specifications.md

| Model ID | Device | Containers | Video Codecs | Audio Codecs | Max Resolution | Max FPS | Max Video Bitrate | Max Audio Bitrate |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `roku_express` | Roku Express | MP4, MKV | H.264, HEVC | AAC, AC3, EAC3, DTS | 1080p | 60 | 20 Mbps | 640 kbps |
| `roku_streaming_stick_4k` | Roku Streaming Stick 4K | MP4, MKV | H.264, HEVC | AAC, AC3, EAC3, DTS | 2160p | 60 | 40 Mbps | 1536 kbps |
| `roku_ultra` | Roku Ultra | MP4, MKV | H.264, HEVC | AAC, AC3, EAC3, DTS | 2160p | 60 | 40 Mbps | 1536 kbps |

## Apple TV Family

Sources:
- https://developer.apple.com/library/archive/technotes/tn2429/_index.html
- tvOS media format documentation

| Model ID | Device | Containers | Video Codecs | Audio Codecs | Max Resolution | Max FPS | Max Video Bitrate | Max Audio Bitrate |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `apple_tv_hd` | Apple TV HD | MP4, M4V, MOV | H.264 | AAC, AC3, EAC3 | 1080p | 60 | 20 Mbps | 640 kbps |
| `apple_tv_4k_1st_gen` | Apple TV 4K (1st gen) | MP4, M4V, MOV | H.264, HEVC | AAC, AC3, EAC3 | 2160p | 60 | 35 Mbps | 768 kbps |
| `apple_tv_4k_2nd_gen` | Apple TV 4K (2nd gen) | MP4, M4V, MOV | H.264, HEVC | AAC, AC3, EAC3 | 2160p | 60 | 40 Mbps | 768 kbps |
| `apple_tv_4k_3rd_gen` | Apple TV 4K (3rd gen) | MP4, M4V, MOV | H.264, HEVC | AAC, AC3, EAC3 | 2160p | 60 | 40 Mbps | 768 kbps |

## Fire TV Family

Source: https://developer.amazon.com/docs/device-specs/device-specifications-fire-tv-streaming-media-player.html

| Model ID | Device | Containers | Video Codecs | Audio Codecs | Max Resolution | Max FPS | Max Video Bitrate | Max Audio Bitrate |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `fire_tv_stick_4k` | Fire TV Stick 4K | MP4, MKV | H.264, HEVC, AV1 | AAC, AC3, EAC3, FLAC | 2160p | 60 | 35 Mbps | 1024 kbps |
| `fire_tv_stick_4k_max` | Fire TV Stick 4K Max | MP4, MKV | H.264, HEVC, AV1 | AAC, AC3, EAC3, FLAC | 2160p | 60 | 40 Mbps | 1024 kbps |
| `fire_tv_cube_3rd_gen` | Fire TV Cube (3rd gen) | MP4, MKV | H.264, HEVC, AV1 | AAC, AC3, EAC3, FLAC | 2160p | 60 | 40 Mbps | 1024 kbps |
