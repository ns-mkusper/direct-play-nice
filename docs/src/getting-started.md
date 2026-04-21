# Getting Started

## First conversion

```bash
direct_play_nice input.mkv output.mp4
```

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
