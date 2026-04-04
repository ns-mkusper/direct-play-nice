#!/usr/bin/env bash
set -euo pipefail

export VCPKG_ROOT="${VCPKG_ROOT:-/workspace/target/vcpkg}"
threads="$(nproc)"

# Explicitly run unit/bin/integration tests with maximum parallel test threads.
cargo test --lib --bins --tests -- --test-threads="${threads}"
