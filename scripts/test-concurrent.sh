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
cargo test --features ffmpeg-cli-tests -- --test-threads="${threads}"
