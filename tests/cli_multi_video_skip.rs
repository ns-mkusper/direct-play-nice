#![cfg(feature = "ffmpeg-cli-tests")]

#[path = "common/mod.rs"]
mod common;

use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;
use std::ffi::CString;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn cli_ignores_secondary_video_stream_without_misrouting_audio_packets(
) -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, in_dur_ms) = common::gen_multi_video_input(&tmp);
    let output = tmp.path().join("out.mp4");

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg("--hw-accel")
        .arg("none")
        .arg("--sub-mode")
        .arg("skip")
        .arg("--unsupported-video-policy")
        .arg("ignore")
        .arg(&input)
        .arg(&output);
    common::assert_cli_success(cmd);

    let output_cstr = CString::new(output.to_string_lossy().to_string()).unwrap();
    let octx = AVFormatContextInput::open(output_cstr.as_c_str())?;

    let mut video_streams = 0usize;
    let mut audio_streams = 0usize;
    for st in octx.streams() {
        let par = st.codecpar();
        match par.codec_type {
            t if t == ffi::AVMEDIA_TYPE_VIDEO => {
                video_streams += 1;
                assert_eq!(par.codec_id, ffi::AV_CODEC_ID_H264);
            }
            t if t == ffi::AVMEDIA_TYPE_AUDIO => {
                audio_streams += 1;
                assert_eq!(par.codec_id, ffi::AV_CODEC_ID_AAC);
            }
            _ => {}
        }
    }

    assert_eq!(video_streams, 1, "expected exactly one output video stream");
    assert_eq!(audio_streams, 1, "expected exactly one output audio stream");

    let out_dur_ms = common::probe_duration_ms(&output);
    let diff = out_dur_ms.abs_diff(in_dur_ms);
    assert!(
        diff <= 250,
        "duration drift too large: in={}ms out={}ms",
        in_dur_ms,
        out_dur_ms
    );

    Ok(())
}
