# direct-play-nice

A CLI utility to convert video files to Direct-Play-compatible (the single best optimization for your home video streaming server) formats for your video streaming server.

## Purpose

This tool was created to convert video files to formats that satisfy Direct Play requirements (eg. Chromcast) for all users of your video streaming server. It's intended to be paired with [Sonarr](https://wiki.servarr.com/sonarr/custom-scripts), [Radarr](https://wiki.servarr.com/radarr/custom-scripts), etc. as a Custom Script.

## Features

- Convert any video file supported by [FFmpeg](https://www.ffmpeg.org/).

## Contributions / Support

If you run into any issues while using this software or want to add a feature or bug fix feel free to raise an issue.

## Usage

``` bash
Usage: direct_play_nice.exe [OPTIONS] <INPUT_FILE> <OUTPUT_FILE>

Arguments:
  <INPUT_FILE>   Video file to convert
  <OUTPUT_FILE>  Our output direct-play-compatible video file

Options:
  -s, --streaming-devices <STREAMING_DEVICES>  List of StreamingDevice
  -c, --config-file <CONFIG_FILE>              Path to the configuration file
  -h, --help                                   Print help
  -V, --version                                Print version
```

## Building

This project relies on [FFmpeg](https://www.ffmpeg.org/) and [rsmpeg](https://github.com/larksuite/rsmpeg) and builds on Mac, Linux and Windows:

```bash
cargo install cargo-vcpkg
cargo vcpkg --verbose build
cargo build --verbose
```
