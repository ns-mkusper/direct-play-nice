# Architecture

`direct-play-nice` is organized around a CLI-first runtime with reusable library helpers.

## High-level flow

1. `src/main.rs` parses CLI/config and dispatches to `transcoder::run`.
2. `src/transcoder/app_entry.rs` handles probe-only exits and integration prep.
3. `src/transcoder/app_convert.rs` runs stream assessment + conversion pipeline.
4. `src/transcoder/pipeline_*` modules execute decode/encode/remux stages.
5. `src/main_retry.rs` handles hardware fallback and retry policy.
6. `src/main_sidecar.rs` handles OCR sidecar generation.

## Module boundaries

- `src/transcoder/`: transcode orchestration, codec setup, stream processing, diagnostics.
- `src/subtitle_ocr/`: OCR extraction, language resolution, text rendering/muxing.
- `src/devices/`: device capability profiles and compatibility resolution.
- `src/gpu.rs`: hardware probing and encoder/device discovery.
- `src/servarr.rs`: Sonarr/Radarr integration path/env handling.
- `src/types.rs`: shared CLI/config enums and helper transforms.

## Dependency direction

1. CLI/config layer (`main`, `ffmpeg_utils`, `config`) depends on domain/runtime modules.
2. Runtime modules (`transcoder`, `subtitle_ocr`) depend on low-level IO/FFmpeg/ORT crates.
3. Device/profile logic stays pure where possible and should not depend on CLI parsing.

## Guardrails

1. Prefer explicit imports over broad wildcard re-exports.
2. Keep unsafe code isolated and documented with `SAFETY:` invariants.
3. Favor typed enums/newtypes over stringly decision APIs.
4. Keep large modules split by responsibility (planning vs execution vs diagnostics).
5. Maintain CI gates: `fmt`, `clippy -D warnings`, `doc -D warnings`, tests, secret scan.
