# OCR Engine Architecture

This page is the implementation guide for engineers working in
`src/subtitle_ocr/`.

## Goal

The OCR subsystem converts bitmap subtitle streams (PGS/VobSub/DVD) into text
subtitles that can be embedded or muxed for Direct Play compatibility.

Primary outcomes:

- preserve timing
- preserve readable text quality
- keep language metadata stable
- avoid hard failures by falling back when possible

## Pipeline Overview

1. Stream discovery: find bitmap subtitle streams to OCR.
2. Frame extraction: decode subtitle packets and render bitmap rectangles.
3. OCR inference: run PP-OCR/Tesseract/external engine per subtitle image.
4. Quality guards: geometry/quality checks and optional Tesseract fallback.
5. Serialization: write `.srt`/`.ass`.
6. Muxing: embed generated text tracks back into container.

## Module Map

- `engine/mod.rs`: orchestration entrypoint (`convert_bitmap_subtitles`).
- `engine/types.rs`: shared OCR domain types (`OcrLine`, `SubtitleCue`, tasks).
- `engine/factory.rs`: engine selection and `auto` fallback policy.
- `engine/providers.rs`: ONNX provider policy (CUDA/DirectML/CoreML/CPU).
- `engine/runtime.rs`: ORT + PP-OCR model/session initialization.
- `engine/models.rs`: model directory, download/provision, checksum checks.
- `engine/converters.rs`: converter implementations and language->rec-profile routing.
- `engine/workers.rs`: worker planning, multi-worker execution, output finalize.
- `ocr_pipeline.rs`: subtitle frame decode/extract + OCR pass control flow.
- `text_processing.rs`: quality scoring, cleanup, fallback triggers.
- `text_render.rs`: OCR text rendering helpers and subtitle writers.
- `muxing.rs`: remux generated subtitle tracks into output media.
- `language.rs`: language normalization and Tesseract language resolution.

## Where To Change What

- New OCR engine/provider behavior: `engine/factory.rs`, `engine/providers.rs`.
- Model changes/download logic: `engine/models.rs`.
- Recognition profile routing or language policy: `engine/converters.rs`.
- Parallel execution/GPU worker assignment: `engine/workers.rs`.
- OCR quality heuristics/fallback thresholds: `text_processing.rs`.
- Subtitle output formatting rules: `text_render.rs`, `muxing.rs`.

## Recognition Profiles

PP-OCR is split into recognizer profiles:

- `English`
- `Latin`
- `Japanese`
- `Korean`
- `Cjk`

Routing is implemented in `engine/converters.rs`:

- strong mappings for Japanese/Korean/CJK stay explicit
- BCP-47 script tags drive routing when present (`Latn`, `Hant`, `Cyrl`, ...)
- known non-Latin tags route to `English` fallback
- unknown tags default to `Latin`
- optional runtime override:
  `DPN_OCR_REC_PROFILE_OVERRIDES=lang=profile,lang2=profile`

Example:

```bash
export DPN_OCR_REC_PROFILE_OVERRIDES="rus=english,sr-Latn=latin,spa=latin"
```

## Runtime Controls

Most important env switches:

- `DPN_OCR_REQUIRE_GPU=1`: fail if GPU provider is unavailable.
- `DPN_OCR_FORCE_CPU=1`: force CPU execution providers.
- `DPN_OCR_MODEL_DIR=/path`: custom model directory.
- `DPN_OCR_MAX_JOBS`, `DPN_OCR_JOBS_PER_GPU`: worker concurrency.
- `DPN_OCR_DISABLE_TESS_FALLBACK=1`: disable quality fallback to Tesseract.

## Testing Strategy

- Unit tests: `src/subtitle_ocr/tests.rs` for routing, workers, quality heuristics.
- Fixture accuracy: `--probe-ocr-fixtures` path.
- Full workflow: CLI integration tests in `tests/`.
- Performance evidence: benchmark artifacts in `benches/` and server run logs.
