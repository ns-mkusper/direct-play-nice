# Troubleshooting

## `--probe-hw` shows no usable hardware encoders

- confirm FFmpeg build includes your target encoder (`h264_nvenc`, `h264_qsv`, etc.)
- run `--probe-codecs --only-video --only-hw`
- set `--hw-accel none` as a fallback path while debugging

## OCR falls back to CPU unexpectedly

- verify runtime libraries for your platform/provider are installed
- use `DPN_OCR_REQUIRE_GPU=1` to fail fast instead of silently falling back
- try `--ocr-engine pp-ocr-v3` on older GPUs/runtime stacks

## Output not directly playable on a target client

- verify target selection (`--device`)
- inspect stream details with `--probe-streams`
- set explicit bitrate/quality limits to match endpoint constraints
- compare against [SUPPORTED_DEVICES.md](../../SUPPORTED_DEVICES.md)

## Remote playback buffers despite compatible codecs

Check container layout as well as bitrate. Subtitle timestamps can be correct
while their bytes are stored far from the corresponding audio/video packets,
causing repeated remote range requests. MP4 fast-start only moves the index; it
does not by itself repair poorly interleaved packet data.

Newly written seekable MP4 outputs use interleaved packet writes and fast-start
finalization. Existing compatible files may still take the skip path, so this
change does not retroactively rewrite a library. Preserve the original when
repairing a file, and verify the final client's actual playback mode, startup,
and seeking before replacing more media.
