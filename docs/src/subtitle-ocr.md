# Subtitle OCR

Bitmap subtitle formats (PGS/VobSub/DVD) are not directly compatible with MP4
Direct Play in many client stacks. `direct_play_nice` can OCR bitmap subtitles
into text tracks.

## Defaults

- `--sub-mode auto`
- `--ocr-engine auto`
- `--ocr-format srt`

## Common overrides

- `--sub-mode skip` disable subtitle processing
- `--sub-mode force` force subtitle processing
- `--ocr-engine pp-ocr-v4` force PP-OCR v4 pipeline
- `--ocr-engine pp-ocr-v3` fallback for older GPU/runtime combinations
- `--ocr-format ass` request ASS (may be downgraded in MP4)
- `--ocr-write-srt-sidecar` write `.srt` sidecars in addition to embedded output

## GPU behavior

The OCR runtime attempts provider fallback when available (for example CUDA,
DirectML, CoreML, then CPU). You can force behavior with:

- `DPN_OCR_REQUIRE_GPU=1`
- `DPN_OCR_FORCE_CPU=1`

## Model location

Models are downloaded to a default model directory unless `DPN_OCR_MODEL_DIR`
is set.
