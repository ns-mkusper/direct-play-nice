#![cfg(feature = "ffmpeg-cli-tests")]

use assert_cmd::prelude::*;
use predicates::str;
use std::ffi::CString;
use std::process::Command;
use tempfile::TempDir;

mod common;

use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;

#[test]
fn cli_sub_mode_skip_drops_all_subtitle_streams() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, _dur_ms) = common::gen_problem_input(&tmp);
    let output = tmp.path().join("out_no_subs.mp4");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.arg("--sub-mode")
        .arg("skip")
        .arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg(&input)
        .arg(&output);
    cmd.assert().success().stdout(str::is_empty());

    let output_cstr = CString::new(output.to_string_lossy().to_string())?;
    let octx = AVFormatContextInput::open(output_cstr.as_c_str())?;

    let subtitle_count = octx
        .streams()
        .iter()
        .filter(|st| st.codecpar().codec_type == ffi::AVMEDIA_TYPE_SUBTITLE)
        .count();

    assert_eq!(subtitle_count, 0, "expected no subtitle streams in output");

    Ok(())
}
