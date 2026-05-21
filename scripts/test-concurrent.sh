#!/usr/bin/env bash
set -euo pipefail

export VCPKG_ROOT="${VCPKG_ROOT:-/workspace/target/vcpkg}"
threads="$(nproc)"
if rustc --version | grep -q 'nightly'; then
  threads=1
fi

# Explicitly run unit/bin/integration tests with maximum parallel test threads.
cargo test --lib --bins --tests -- --test-threads="${threads}"

# Execute ffmpeg-backed integration tests end-to-end (not just compile checks).
# Nightly still compiles them, but stable/beta provide runtime coverage; this
# keeps nightly focused on compiler compatibility instead of FFmpeg runtime noise.
if rustc --version | grep -q 'nightly'; then
  cargo test --features ffmpeg-cli-tests --no-run
else
  cargo test --features ffmpeg-cli-tests -- --test-threads="${threads}"
fi
