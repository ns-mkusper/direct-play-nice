# Build and Test

## Build from source

```bash
cargo install cargo-vcpkg
cargo vcpkg build
cargo build
```

If your vcpkg checkout is in a non-default location, set `VCPKG_ROOT`.

## External vcpkg host notes

Some long-lived Linux hosts keep a shared vcpkg checkout outside the repo, for
example under `/home/$USER/vcpkg`. On those hosts, use that checkout explicitly:

```bash
source "$HOME/.cargo/env"
export VCPKG_ROOT=/home/$USER/vcpkg
export VCPKGRS_TRIPLET=x64-linux
export LD_LIBRARY_PATH="$VCPKG_ROOT/installed/x64-linux/lib:${LD_LIBRARY_PATH:-}"
cargo build --release
```

If `rsmpeg` fails with missing FFmpeg struct fields such as `AVFormatContext.pb`,
`AVFormatContext.streams`, or `AVBitStreamFilter.name`, bindgen likely generated
opaque FFmpeg structs for the local headers. Reuse the bundled FFmpeg 8 bindings
from `rusty_ffmpeg` while still linking against the host vcpkg libraries:

```bash
export FFMPEG_BINDING_PATH="$(
  find "$HOME/.cargo/registry/src" \
    -path '*/rusty_ffmpeg-0.16.7+ffmpeg.8/src/binding.rs' \
    -print -quit
)"

cargo build --release
```

This keeps source builds reproducible on hosts whose clang/bindgen combination
does not expose FFmpeg internals consistently.

## Run tests

```bash
cargo test
```

Run integration tests requiring ffmpeg CLI:

```bash
VCPKG_ROOT=/opt/vcpkg cargo test --features ffmpeg-cli-tests
```

## Optional NVENC regression suite

```bash
ENABLE_NVENC_TESTS=1 cargo test nvenc_matrix -- --test-threads=1
```

## Optional direnv setup

```sh
export VCPKG_ROOT=/opt/vcpkg
export RUST_LOG=${RUST_LOG:-WARN}
```

## Quality gates

Run the same strict checks used in CI before opening a PR:

```bash
cargo fmt --all -- --check
cargo clippy --all-targets --all-features -- -D warnings
RUSTDOCFLAGS="-D warnings" cargo doc --no-deps --document-private-items
cargo test --no-run
```
