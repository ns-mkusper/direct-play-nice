# Probe and Debug

Use probe flags before converting large batches:

```bash
direct_play_nice --probe-hw
direct_play_nice --probe-codecs --only-video
direct_play_nice --probe-streams input.mkv
```

JSON output can be enabled for machine-readable inspection:

```bash
direct_play_nice --probe-hw --probe-codecs --probe-json
```
