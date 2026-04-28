# Installation

## Prebuilt binaries

Each GitHub release publishes platform archives and checksums built by
`cargo-dist`:

- `direct_play_nice-aarch64-apple-darwin.tar.xz`
- `direct_play_nice-x86_64-apple-darwin.tar.xz`
- `direct_play_nice-x86_64-unknown-linux-gnu.tar.xz`
- `direct_play_nice-x86_64-pc-windows-msvc.zip`

The release also includes shell and PowerShell installers.

## From crates.io

```bash
cargo install direct_play_nice
```

## From source

```bash
git clone https://github.com/ns-mkusper/direct-play-nice.git
cd direct-play-nice
cargo build --release
```

Binary path:

```text
target/release/direct_play_nice
```
