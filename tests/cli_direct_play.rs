//! Integration test: ensure CLI converts a problematic video
//! into a Chromecast direct‑play compatible MP4 without
//! breaking duration or stream mappings.

#[path = "common/mod.rs"]
mod common;

use rsmpeg::avformat::AVFormatContextInput;
use rsmpeg::ffi;
use std::ffi::CString;
use std::process::Command;
use tempfile::TempDir;

#[test]
fn cli_produces_chromecast_direct_play_mp4() -> Result<(), Box<dyn std::error::Error>> {
    // Fail fast if ffmpeg CLI is not present
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, in_dur_ms) = common::gen_problem_input(&tmp);
    let output = tmp.path().join("out.mp4");

    // Run the CLI for all Chromecast models (intersection guarantees
    // output is direct-play-compatible across all of them).
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg(&input)
        .arg(&output);
    common::assert_cli_success(cmd);

    assert!(output.exists(), "output file was not created");

    // Validate via rsmpeg
    let output_cstr = CString::new(output.to_string_lossy().to_string()).unwrap();
    let octx = AVFormatContextInput::open(output_cstr.as_c_str())?;

    let mut saw_v = false;
    let mut saw_a = false;
    let mut saw_s = false;
    let mut width = 0i32;
    let mut height = 0i32;
    let mut fps_num = 0i32;
    let mut fps_den = 1i32;
    let mut level = 0i32;
    let mut pix_fmt = -1i32;

    for st in octx.streams() {
        let par = st.codecpar();
        match par.codec_type {
            t if t == ffi::AVMEDIA_TYPE_VIDEO => {
                saw_v = true;
                assert_eq!(par.codec_id, ffi::AV_CODEC_ID_H264, "video must be H.264");
                width = par.width;
                height = par.height;
                level = par.level;
                pix_fmt = par.format;
                let rate = st.avg_frame_rate;
                fps_num = rate.num;
                fps_den = rate.den;
            }
            t if t == ffi::AVMEDIA_TYPE_AUDIO => {
                saw_a = true;
                assert_eq!(par.codec_id, ffi::AV_CODEC_ID_AAC, "audio must be AAC");
            }
            t if t == ffi::AVMEDIA_TYPE_SUBTITLE => {
                saw_s = true;
                assert_eq!(
                    par.codec_id,
                    ffi::AV_CODEC_ID_MOV_TEXT,
                    "subs must be MOV_TEXT"
                );
            }
            _ => {}
        }
    }

    assert!(
        saw_v && saw_a && saw_s,
        "missing one or more required streams"
    );

    // Chromecast (1st gen) bounds
    assert!(
        width as u32 <= 1920 && height as u32 <= 1080,
        "resolution too high"
    );
    // Accept any profile but ensure level <= 4.1 (41)
    assert!(level <= 41, "H.264 level too high: {}", level);
    // yuv420p pixel format
    assert_eq!(pix_fmt, ffi::AV_PIX_FMT_YUV420P, "pix fmt must be yuv420p");
    // fps <= 30
    if fps_den != 0 {
        // guard
        let fps = (fps_num as f64) / (fps_den as f64);
        assert!(fps <= 30.01, "fps too high: {}", fps);
    }

    // Duration close to input (within 200ms)
    let out_dur_ms = common::probe_duration_ms(&output);
    let diff = if out_dur_ms > in_dur_ms {
        out_dur_ms - in_dur_ms
    } else {
        in_dur_ms - out_dur_ms
    };
    assert!(
        diff <= 200,
        "duration drift too large: in={}ms out={}ms",
        in_dur_ms,
        out_dur_ms
    );

    Ok(())
}
