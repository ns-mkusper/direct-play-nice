#![cfg(feature = "ffmpeg-cli-tests")]

//! Ensure that specifying `-s all` produces a direct-play compatible output.

#[path = "common/mod.rs"]
mod common;

use assert_cmd::prelude::*;
use predicates::str;
use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;
use std::ffi::CString;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn cli_all_devices_selector_converts_to_direct_play() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, _in_ms) = common::gen_odd_width_input(&tmp);
    let output = tmp.path().join("out_all.mp4");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.arg("-s")
        .arg("all")
        .arg("--hw-accel")
        .arg("none")
        .arg(&input)
        .arg(&output);
    cmd.env("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/sbin");
    cmd.assert().success().stdout(str::is_empty());
    assert!(output.exists(), "output file was not created");

    let output_cstr = CString::new(output.to_string_lossy().to_string())?;
    let octx = AVFormatContextInput::open(output_cstr.as_c_str())?;

    let mut saw_v = false;
    let mut pix_fmt = -1i32;
    for st in octx.streams() {
        let par = st.codecpar();
        if par.codec_type == ffi::AVMEDIA_TYPE_VIDEO {
            saw_v = true;
            assert_eq!(par.codec_id, ffi::AV_CODEC_ID_H264, "video must be H.264");
            pix_fmt = par.format;
        } else if par.codec_type == ffi::AVMEDIA_TYPE_AUDIO {
            assert_eq!(par.codec_id, ffi::AV_CODEC_ID_AAC, "audio must be AAC");
        }
    }

    assert!(saw_v, "missing required video stream");
    assert_eq!(pix_fmt, ffi::AV_PIX_FMT_YUV420P, "pix fmt must be yuv420p");

    Ok(())
}
