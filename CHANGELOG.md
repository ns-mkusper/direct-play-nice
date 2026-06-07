# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

## [1.1.0-beta.3] - 2026-06-01

### 🛠️ Beta 3 Fixes

- Fixed bitmap-subtitle OCR canvas seeding so bitmap subtitles render more
  reliably before OCR.
- Fixed Servarr handling for direct-play-compatible files so already-compatible
  imports are detected and skipped correctly.
- Fixed Servarr output filename quality tags to reflect the requested output
  quality instead of stale or source quality metadata.

### 🧪 Beta 3 Validation

- Added regression coverage for subtitle encode overflow and unsupported
  subtitle-character handling, covering both skip-stream completion and
  fail-policy failure behavior.
- Validated post-merge GPU OCR, spacing regression, NVENC transcode, and resize
  benchmark results before cutting this release candidate.

## [1.1.0-beta.2] - 2026-05-27

### 🛠️ Fixed

- Added OCR quality fallback handling for severely glued bitmap-subtitle text so
  PP-OCR failures can recover at cue level instead of emitting unreadable text.
- Added generic OCR word-fragment normalization for common recognition artifacts
  without media-specific replacement tables.
- Added Sonarr parity for completed pending language-upgrade force imports so
  quality-downgrade dub replacements can be manually imported after DPN verifies
  the pending file satisfies the configured language policy.
- Skipped Sonarr multi-episode and season-pack release results for automatic
  language redownloads unless the release maps to the single requested episode.

### 🧪 Validation and Tooling

- Added OCR text-quality metrics and a post-merge spacing regression benchmark
  gate to catch low-space-rate and long-token regressions.
- Added strict SRT reference comparison tooling for CER/WER validation of OCR
  recognizer candidates.
- Added gated OCR training data export and recognizer training utilities for
  continued GPU OCR improvements.

### 📚 Documentation

- Documented the bitmap-subtitle OCR flow with a Mermaid diagram covering
  PP-OCR, generic normalization, word/phrase recovery, and safety fallback.

## [1.1.0-beta.1] - 2026-05-21

### ⚙️ Benchmark Reliability

- Restored Maxwell-compatible OCR GPU benchmark execution on the Plex benchmark
  runner by using the conservative ORT 1.16 / cuDNN8 runtime profile.
- Kept CUDA safety brakes enabled with heuristic convolution search so required
  GPU OCR validation completes without falling back to CPU.
- Added manual dispatch support for the post-merge benchmark workflow so release
  candidates can be validated on `plex-bench` before merging.

### 🧪 Validation

- Validated the fixed post-merge benchmark path on `plex-bench` with GPU OCR
  required, both CUDA devices active, CPU fallback skipped, and all bitmap
  subtitle OCR streams emitted.

## [1.0.0-beta.1] - 2026-05-21

### 📌 1.0 Beta 1 Highlights

- Added opt-in Sonarr/Radarr language compliance checks that inspect imported
  media for required audio languages before conversion or replacement.
- Added safe, dry-run-first replacement workflows that grab verified language
  candidates, blocklist superseded history items, and avoid blind redownloads.
- Added periodic history and Sonarr inventory audits for delayed dubs and
  current-library backlog cleanup, including focused episode-ID batches.
- Added Radarr completed-pending force-import handling for language-better
  replacements that Radarr would otherwise leave blocked by quality hierarchy.
- Added opt-in stream-copy retagging for trusted untagged audio/subtitle streams
  so English-native libraries with `und` metadata can be fixed without
  redownloading.

### 🧪 Language Validation

- Added mocked Sonarr/Radarr API coverage for release selection, inventory
  audits, targeted audits, dry-runs, no-candidate behavior, and Radarr
  force-import side effects.
- Added FFmpeg-backed retagging tests for unknown audio language metadata and
  safeguards that explicit non-English language tags are not overwritten.

## [0.2.0-beta.1] - 2026-05-21

### 📌 Beta 4 Highlights

- Added high-quality deterministic FFmpeg resizing for direct-play constraints,
  with Lanczos as the default resize kernel and lower-cost kernels still
  available for speed-focused workflows.
- Added CUDA `scale_cuda` resizing for compatible Linux CUDA/NVENC paths, plus
  `--resize-backend auto|software|cuda` to control backend selection.
- Preserved direct-play safety by keeping output dimensions bounded by source,
  device, and quality caps while normalizing encoder-safe dimensions.

### 🧪 Benchmarks and Validation

- Added full-reference resize benchmarking with elapsed time, FPS, realtime
  factor, output size, VMAF when available, and PSNR/SSIM fallback metrics.
- Integrated software and CUDA resize candidates into the post-merge benchmark
  workflow for Plex benchmark validation.
- Validated full-video 4K → 480p downscales: default Lanczos delivered about a
  40% overall measured quality improvement versus the old fast-bilinear path,
  with the expected software CPU runtime tradeoff.

### 🧱 Reliability and Test Coverage

- Added FFmpeg CLI integration coverage for downscale and no-upscale behavior.
- Hardened resize, subtitle, and delete-source tests against CI/runtime
  variability across Linux, macOS, Windows, and nightly Rust.
- Documented resize quality/backend controls and external vcpkg build notes.

## [0.1.0-beta.3] - 2026-05-04

### ⚙️ Release Validation

- Added a release-readiness check that requires concrete `CHANGELOG.md` notes
  for the exact `Cargo.toml` version before release-impacting PRs can merge.
- Reused the GitHub release-note extraction rules in PR validation so missing,
  placeholder-only, or malformed changelog sections fail before tags are cut.
- Documented that release PRs must include matching changelog notes alongside
  version bumps.

## [0.1.0-beta.2] - 2026-04-29

### 📌 Beta 2 Highlights

- Published cargo-dist release binaries for Apple Silicon macOS, Intel macOS,
  Linux, and Windows.
- Added workflow-dispatch release reruns by tag so binary publishing can be
  recovered without creating a new source release.
- Added PR CI coverage for macOS Intel, macOS Apple Silicon, and Windows MSVC.

### ⚙️ Release Pipeline

- Pinned cargo-dist target runner labels to the current GitHub-hosted runner
  fleet, including `macos-15`, `macos-15-intel`, `windows-2022`, and
  `ubuntu-22.04`.
- Split release vcpkg caches by target and reset partial restored caches before
  building to avoid stale or malformed vcpkg workspaces.
- Skipped release-workflow benchmarks only for manual binary reruns while
  keeping the post-merge benchmark workflow as the release gate.
- Made the release benchmark path explicitly build and validate the expected
  release binary before running benchmark jobs.

### 🧱 Platform Linkage

- Added macOS framework link flags required by FFmpeg and VideoToolbox release
  builds.
- Aligned Windows CI and release builds on the `x64-windows-static` vcpkg
  triplet and static MSVC CRT.
- Passed Windows system library link flags through release workflow `RUSTFLAGS`
  so static FFmpeg Schannel and Media Foundation objects link under cargo-dist.

## [0.1.0-beta.1] - 2026-04-26

### 📌 Beta 1 Highlights

- Landed a large OCR/transcoding refactor focused on clearer module boundaries,
  better reuse, and lower maintenance overhead across the core pipeline.
- Removed vendored third-party OCR source trees from the repository and moved to
  upstream crate and runtime consumption, reducing repo size and vendoring risk.
- Added stronger benchmark and fixture-evaluation coverage for OCR quality,
  fallback behavior, and GPU execution health.
- Hardened benchmark runner orchestration so post-merge release validation is
  more deterministic on the self-hosted Plex benchmark runner.

### 🧠 OCR & Language Pipeline

- Added fixture-driven OCR evaluation (`--probe-ocr-fixtures`) with truth-backed
  CER/WER/similarity reporting for `hybrid`, `strict-gain`, and `pure-onnx`
  modes.
- Expanded OCR benchmark diagnostics with explicit fallback counts/rates and
  averaged confidence/score signals to make quality regressions visible.
- Improved fallback policy transparency by logging effective non-English
  Tesseract fallback settings and warning when forced fallback policy is enabled.

### 🧱 Architecture & Code Quality

- Split and clarified core OCR/transcoder responsibilities across dedicated
  modules (engine setup/core/pipeline/muxing/render and related entrypoints).
- Reworked broad import patterns and low-signal inline documentation toward
  clearer contracts and easier refactors.
- Performed style and cleanup passes to reduce duplication, improve naming and
  readability, and tighten idiomatic Rust usage under strict `clippy`.

### ⚙️ CI, Benchmarks, and Release Flow

- Added benchmark-runner smoke validation workflow to verify dependency/toolchain
  readiness and benchmark source availability before release-critical merges.
- Kept strict verification gates (`fmt`, strict `clippy`, docs with warnings as
  errors, targeted CLI/OCR tests, concurrency scripts) aligned with refactors.
- Confirmed post-merge benchmark execution on the self-hosted runner with
  hardware-accelerated transcode and multi-GPU OCR telemetry artifacts.

## [0.1.0-alpha.5] - 2026-04-21

### 📌 Release Highlights

- Merged major refactor and CI hardening work from recent mainline changes.
- Added strict rustdoc coverage checks for public library APIs.
- Added and validated an mdBook manual with dedicated chapters for:
  installation, command reference, quality controls, OCR/GPU setup, Plex
  refresh, Sonarr/Radarr integration, troubleshooting, and build/test flows.
- Added GitHub Pages deployment workflow for publishing the mdBook on pushes to
  `main`, and retained PR-time docs validation (`markdownlint`, link checks,
  mdBook build).
- Improved release-note generation to capture all commit styles (including
  unconventional subjects) since the previous tag.
- Refreshed README to be a concise landing page with Direct Play definition
  links and handoff to the full manual.

## [0.1.0-alpha.4] - 2026-04-07

### 📌 Release Delta Summary

- Fixed 30x duration inflation from incorrect FPS detection.
- Pinned vcpkg SHA to restore reproducible dependency builds.
- Fixed all-devices mode skipping conversion without configuration file.
- Added OCR subtitle conversion modes for flexible subtitle workflows.
- Improved OCR accuracy and unblocked broken main deployment pipeline.
- Added Roku, Apple TV, and Fire TV device support.
- Prevented corrupted output by failing fast on decoder errors.
- Added CI regression test coverage for FFmpeg corruption scenarios.

## [0.1.0-alpha.3] - 2025-11-06

### 🛠 Fixes

- Rescaled video frame timestamps during re-encode so Chromecast direct-play
  exports remain within the 200 ms duration tolerance.

### 🧪 Tests

- Synchronized configuration-loading tests with a global environment mutex to
  prevent nightly-only race conditions.

### 🧹 Maintenance

- Updated CLI integration suites to use `cargo::cargo_bin!` for compatibility
  with custom Cargo build directories.

## [0.1.0-alpha.1] - 2024-09-01

### 🚀 Features

- Handle multiple devices ([#6](https://github.com/ns-mkusper/direct-play-nice/pull/6))

### ⚙️ Miscellaneous Tasks

- Get release workflow working ([#12](https://github.com/ns-mkusper/direct-play-nice/pull/12))

<!-- generated by git-cliff -->
