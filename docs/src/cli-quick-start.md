# CLI Quick Start

## Basic conversion

```bash
direct_play_nice input.mkv output.mp4
```

## Target specific devices

```bash
direct_play_nice --device chromecast,roku input.mkv output.mp4
```

## Probe hardware and codecs

```bash
direct_play_nice --probe-hw --probe-codecs --only-video --only-hw --probe-json
```

## Subtitle OCR defaults

By default, bitmap subtitle tracks are OCR-converted when needed for
direct-play-compatible output.
