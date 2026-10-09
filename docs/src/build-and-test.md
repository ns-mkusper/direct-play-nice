# Build and Test

## Build from source

```bash
cargo install cargo-vcpkg
export VCPKG_OVERLAY_PORTS="$PWD/vcpkg-overlays/ports"
cargo vcpkg build
cargo build
```

`vcpkg-overlays/ports` carries local copies of upstream vcpkg ports that need a
different source URL; today that is x264, which upstream fetches from a GitLab
host that answers archive downloads with an anti-bot page. CI and the Docker
image set `VCPKG_OVERLAY_PORTS` the same way.

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

## FFmpeg layout and quality baselines

`tests/ffmpeg_baseline.rs` converts generated fixtures and compares the result
with snapshots in `tests/baselines/`: container format, head `moov`, the stream
table with profile, level, dimensions, languages and dispositions, packet counts
and timing per stream, subtitle cue text and timing, how far subtitle payloads
trail the audio/video already written, and video PSNR against the source. The
inspection uses only the `ffprobe` and `ffmpeg` CLIs, so the snapshots judge
output the same way before and after a bindings or FFmpeg version change.

```bash
cargo test --features ffmpeg-cli-tests --test ffmpeg_baseline
```

When an output change is intended, regenerate the snapshots and review the
diff before committing:

```bash
DPN_UPDATE_BASELINES=1 cargo test --features ffmpeg-cli-tests --test ffmpeg_baseline
```

## Real Sonarr/Radarr imports in kind

The Servarr end-to-end suite in `tests/e2e/servarr`
uses pinned Arr containers and a PR-built DPN binary in a disposable kind cluster.
It tests real import and upgrade notifications, disabled-hook controls, source
safety, hardlinks, and file tracking after rescans without live media or indexers.
Run it with Docker, kind, kubectl, and Python installed:

```bash
bash tests/e2e/servarr/run.sh
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
