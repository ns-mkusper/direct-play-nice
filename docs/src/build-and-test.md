# Build and Test

## Build from source

```bash
cargo install cargo-vcpkg
cargo vcpkg build
cargo build
```

If your vcpkg checkout is in a non-default location, set `VCPKG_ROOT`.

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
