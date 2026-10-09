#![cfg(feature = "ffmpeg-cli-tests")]

//! Physical MP4 layout regressions: faststart alone does not make subtitle
//! samples interleaved. Inspect nonempty MOV_TEXT payloads and their byte offsets,
//! not demux order (which FFmpeg can reconstruct from a badly laid-out file).

mod common;

use direct_play_nice::ff::AVFormatContextInput;
use ffmpeg_next::sys as ffi;
use std::ffi::CString;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

type TestResult = Result<(), Box<dyn std::error::Error>>;

const LANGUAGES: [&str; 17] = [
    "eng", "pol", "fra", "deu", "spa", "ita", "por", "nld", "swe", "nor", "dan", "fin", "ces",
    "hun", "ron", "jpn", "kor",
];
const AUDIO_LANGUAGES: [&str; 2] = ["eng", "pol"];

fn ffmpeg() -> Command {
    let mut cmd = Command::new("ffmpeg");
    cmd.args(["-hide_banner", "-loglevel", "error", "-nostdin", "-y"]);
    cmd
}

fn successful(cmd: &mut Command) -> String {
    let out = cmd.output().expect("execute fixture/CLI command");
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        out.status.success(),
        "{cmd:?} failed: {}\n{stderr}",
        out.status
    );
    stderr
}

fn cue_text(track: usize, late: bool) -> String {
    format!(
        "track {track:02} {} payload",
        if late { "late" } else { "early" }
    )
}

fn has_late_cue(track: usize, sparse: bool) -> bool {
    !sparse || track.is_multiple_of(3)
}

/// Four seconds, 40 tiny software video frames, two distinct mono audio streams.
/// MPEG4/MP2 forces the actual transcode path rather than direct-play skipping.
/// Sparse tracks stop after their first cue; no bitmap source or OCR is needed.
fn fixture(tmp: &TempDir, subtitle_count: usize, sparse: bool) -> PathBuf {
    common::ensure_ffmpeg_present();
    let av = tmp.path().join("av.mkv");
    successful(
        ffmpeg()
            .args([
                "-f",
                "lavfi",
                "-i",
                "testsrc2=size=160x120:rate=10:duration=4",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=440:sample_rate=48000:duration=4",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=880:sample_rate=48000:duration=4",
                "-map",
                "0:v:0",
                "-map",
                "1:a:0",
                "-map",
                "2:a:0",
                "-c:v",
                "mpeg4",
                "-threads:v",
                "1",
                "-pix_fmt",
                "yuv420p",
                "-c:a",
                "mp2",
                "-b:a",
                "64k",
                "-metadata:s:a:0",
                "language=eng",
                "-metadata:s:a:1",
                "language=pol",
            ])
            .arg(&av),
    );
    if subtitle_count == 0 {
        return av;
    }

    let input = tmp.path().join("text_tracks.mkv");
    let mut mux = ffmpeg();
    mux.arg("-i").arg(&av);
    for track in 0..subtitle_count {
        let path = tmp.path().join(format!("track_{track:02}.srt"));
        let mut text = format!(
            "1\n00:00:00,250 --> 00:00:00,750\n{}\n\n",
            cue_text(track, false)
        );
        if has_late_cue(track, sparse) {
            text.push_str(&format!(
                "2\n00:00:03,250 --> 00:00:03,750\n{}\n\n",
                cue_text(track, true)
            ));
        }
        fs::write(&path, text).unwrap();
        mux.arg("-i").arg(path);
    }
    mux.args(["-map", "0:v:0", "-map", "0:a"]);
    for track in 0..subtitle_count {
        mux.args(["-map", &format!("{}:s:0", track + 1)]);
        mux.args([
            &format!("-metadata:s:s:{track}"),
            &format!("language={}", LANGUAGES[track % LANGUAGES.len()]),
        ]);
    }
    successful(mux.args(["-c", "copy", "-c:s", "srt"]).arg(&input));
    input
}

fn convert(input: &Path, output: &Path, device: &str, skip_subtitles: bool) -> String {
    // An explicit empty TOML file takes precedence over environment/default
    // config discovery; the CLI does not expose a --no-config switch.
    let config = output
        .parent()
        .expect("fixture directory")
        .join("config.toml");
    fs::write(&config, "").expect("write isolated empty config");
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("RUST_LOG", "info")
        .env_remove("DIRECT_PLAY_NICE_CONFIG")
        .env_remove("DIRECT_PLAY_NICE_CONFIG_FILE")
        .arg("--config-file")
        .arg(&config)
        .args([
            "--skip-codec-check",
            "--hw-accel",
            "none",
            "--video-codec",
            "h264",
            "--device",
            device,
        ])
        .arg(input)
        .arg(output);
    if skip_subtitles {
        cmd.args(["--sub-mode", "skip"]);
    }
    let stderr = successful(&mut cmd);
    assert!(
        output.is_file(),
        "conversion did not create {}: {stderr}",
        output.display()
    );
    assert!(
        !stderr.contains("OCR progress:"),
        "text-only fixture unexpectedly ran OCR: {stderr}"
    );
    stderr
}

#[derive(Debug)]
struct Packet {
    pts_seconds: f64,
    duration_seconds: f64,
    pos: i64,
    text: Option<String>,
}

#[derive(Debug)]
struct Track {
    kind: ffi::AVMediaType,
    codec: ffi::AVCodecID,
    language: String,
    packets: Vec<Packet>,
}

fn inspect(path: &Path) -> (f64, Vec<Track>) {
    let path = CString::new(path.to_string_lossy().as_bytes()).unwrap();
    let mut input = AVFormatContextInput::open(&path).expect("open media");
    let duration = input.duration as f64 / ffi::AV_TIME_BASE as f64;
    let language_key = CString::new("language").unwrap();
    let mut tracks: Vec<Track> = input
        .streams()
        .iter()
        .map(|stream| Track {
            kind: stream.codecpar().codec_type,
            codec: stream.codecpar().codec_id,
            language: stream
                .metadata()
                .and_then(|metadata| {
                    metadata
                        .get(&language_key, None, 0)
                        .map(|entry| entry.value().to_string_lossy().into_owned())
                })
                .unwrap_or_default(),
            packets: Vec::new(),
        })
        .collect();
    while let Some(packet) = input.read_packet().expect("read packet") {
        let index = packet.stream_index as usize;
        let time_base = input.streams()[index].time_base;
        let seconds_per_tick = time_base.num as f64 / time_base.den as f64;
        assert_ne!(
            packet.pts,
            ffi::AV_NOPTS_VALUE,
            "missing PTS in stream {index}"
        );
        assert!(packet.pos >= 0, "missing physical offset in stream {index}");
        let text = if tracks[index].codec == ffi::AVCodecID::AV_CODEC_ID_MOV_TEXT {
            assert!(
                packet.size >= 2 && !packet.data.is_null(),
                "invalid MOV_TEXT packet"
            );
            // MOV_TEXT begins with a big-endian text length. Empty gap/end samples
            // can precede all A/V even in broken files, so they must not pass this test.
            // SAFETY: the live AVPacket owns `size` readable bytes at non-null data.
            let data = unsafe { std::slice::from_raw_parts(packet.data, packet.size as usize) };
            let len = u16::from_be_bytes([data[0], data[1]]) as usize;
            assert!(len + 2 <= data.len(), "truncated MOV_TEXT payload");
            if len == 0 {
                None
            } else {
                Some(
                    std::str::from_utf8(&data[2..2 + len])
                        .expect("UTF-8 subtitle")
                        .to_owned(),
                )
            }
        } else {
            None
        };
        tracks[index].packets.push(Packet {
            pts_seconds: packet.pts as f64 * seconds_per_tick,
            duration_seconds: packet.duration as f64 * seconds_per_tick,
            pos: packet.pos,
            text,
        });
    }
    (duration, tracks)
}

/// Walk top-level ISO BMFF boxes; searching for the string "moov" can match
/// bytes inside media payloads and does not prove that the index is at the head.
fn assert_head_moov(path: &Path) {
    let data = fs::read(path).unwrap();
    let mut offset = 0usize;
    let mut moov = None;
    let mut mdat = None;
    while offset < data.len() {
        assert!(data.len() - offset >= 8, "truncated box header");
        let size32 = u32::from_be_bytes(data[offset..offset + 4].try_into().unwrap());
        let (size, header) = match size32 {
            0 => (data.len() - offset, 8),
            1 => {
                assert!(data.len() - offset >= 16, "truncated extended box header");
                (
                    usize::try_from(u64::from_be_bytes(
                        data[offset + 8..offset + 16].try_into().unwrap(),
                    ))
                    .unwrap(),
                    16,
                )
            }
            n => (n as usize, 8),
        };
        assert!(
            size >= header && size <= data.len() - offset,
            "invalid box size"
        );
        match &data[offset + 4..offset + 8] {
            b"moov" => {
                assert!(moov.is_none(), "multiple moov boxes");
                moov = Some(offset);
            }
            b"mdat" => {
                mdat.get_or_insert(offset);
            }
            _ => {}
        }
        offset += size;
    }
    assert!(
        moov.expect("moov box") < mdat.expect("mdat box"),
        "MP4 moov must precede media data"
    );
}

fn assert_av(tracks: &[Track]) {
    let videos: Vec<_> = tracks
        .iter()
        .filter(|t| t.kind == ffi::AVMediaType::AVMEDIA_TYPE_VIDEO)
        .collect();
    let audios: Vec<_> = tracks
        .iter()
        .filter(|t| t.kind == ffi::AVMediaType::AVMEDIA_TYPE_AUDIO)
        .collect();
    assert_eq!(videos.len(), 1, "video track count");
    assert_eq!(audios.len(), AUDIO_LANGUAGES.len(), "audio track count");
    assert_eq!(videos[0].codec, ffi::AVCodecID::AV_CODEC_ID_H264);
    assert!(videos[0].packets.len() >= 39, "video frames lost");
    for (audio, language) in audios.iter().zip(AUDIO_LANGUAGES) {
        assert_eq!(audio.codec, ffi::AVCodecID::AV_CODEC_ID_AAC);
        assert_eq!(audio.language, language);
        assert!(
            audio.packets.len() >= 100,
            "audio packets lost for {language}"
        );
    }
    for track in videos.into_iter().chain(audios) {
        let end = track
            .packets
            .iter()
            .map(|p| p.pts_seconds + p.duration_seconds)
            .fold(0.0, f64::max);
        assert!(
            (end - 4.0).abs() < 0.25,
            "A/V stream duration drift: {end}s"
        );
    }
}

fn mp4_case(subtitle_count: usize, sparse: bool) -> TestResult {
    let tmp = TempDir::new()?;
    let input = fixture(&tmp, subtitle_count, sparse);
    let output = tmp.path().join("out.mp4");
    let stderr = convert(&input, &output, "chromecast_1st_gen", false);
    assert!(
        stderr.contains("No bitmap subtitle tracks required OCR"),
        "must exercise text-only transcode without an OCR remux: {stderr}"
    );
    let (duration, tracks) = inspect(&output);
    assert_eq!(tracks.len(), 3 + subtitle_count, "stream count changed");
    assert_av(&tracks);
    assert!(
        (duration - 4.0).abs() < 0.25,
        "container duration drift: {duration}s"
    );
    assert!(
        (duration * 1000.0 - common::probe_duration_ms(&input) as f64).abs() < 250.0,
        "input/output duration drift"
    );
    let subtitles: Vec<_> = tracks
        .iter()
        .filter(|t| t.kind == ffi::AVMediaType::AVMEDIA_TYPE_SUBTITLE)
        .collect();
    assert_eq!(subtitles.len(), subtitle_count);
    for (index, subtitle) in subtitles.iter().enumerate() {
        assert_eq!(subtitle.codec, ffi::AVCodecID::AV_CODEC_ID_MOV_TEXT);
        assert_eq!(subtitle.language, LANGUAGES[index % LANGUAGES.len()]);
        let nonempty: Vec<_> = subtitle
            .packets
            .iter()
            .filter(|p| p.text.is_some())
            .collect();
        assert_eq!(
            nonempty.len(),
            if has_late_cue(index, sparse) { 2 } else { 1 },
            "cue count in subtitle {index}"
        );
        for (cue, packet) in nonempty.iter().enumerate() {
            assert_eq!(
                packet.text.as_deref(),
                Some(cue_text(index, cue == 1).as_str())
            );
            let expected_pts = if cue == 0 { 0.25 } else { 3.25 };
            assert!(
                (packet.pts_seconds - expected_pts).abs() < 0.1,
                "subtitle {index} cue {cue} timing: {packet:?}"
            );
            assert!(
                (packet.duration_seconds - 0.5).abs() < 0.1,
                "subtitle {index} cue {cue} duration: {packet:?}"
            );
        }
        let early = nonempty[0];
        for (av_index, av) in tracks.iter().enumerate().filter(|(_, t)| {
            matches!(
                t.kind,
                ffi::AVMediaType::AVMEDIA_TYPE_VIDEO | ffi::AVMediaType::AVMEDIA_TYPE_AUDIO
            )
        }) {
            let late_pos = av
                .packets
                .iter()
                .filter(|p| p.pts_seconds >= 3.0)
                .map(|p| p.pos)
                .min()
                .expect("late A/V packet");
            assert!(early.pos < late_pos,
                "subtitle {index} early nonempty payload at byte {} is after late A/V stream {av_index} at byte {late_pos}; head moov alone is insufficient (mixed mux APIs leave subtitle data at EOF)", early.pos);
        }
    }
    // Keep this after payload checks so the historical mixed-write bug fails on
    // interleaving, not merely the independent missing-faststart regression.
    assert_head_moov(&output);
    Ok(())
}

#[test]
fn text_subtitle_payload_is_interleaved_with_video_and_both_audio_tracks() -> TestResult {
    mp4_case(1, false)
}

#[test]
fn seventeen_text_tracks_preserve_languages_cues_and_physical_interleaving() -> TestResult {
    mp4_case(17, false)
}

#[test]
fn thirty_four_sparse_text_tracks_are_not_buffered_to_end_of_file() -> TestResult {
    mp4_case(34, true)
}

#[test]
fn matroska_copy_remux_does_not_receive_mp4_only_mux_options() -> TestResult {
    let tmp = TempDir::new()?;
    let input = fixture(&tmp, 0, false);
    let output = tmp.path().join("out.mkv");
    // The public MKV route transcodes via a temporary MP4, then copy-remuxes.
    // Avoid MOV_TEXT here: Matroska does not support that subtitle codec.
    convert(&input, &output, "roku_ultra", true);
    assert_eq!(
        &fs::read(&output)?[..4],
        &[0x1a, 0x45, 0xdf, 0xa3],
        "Matroska EBML header"
    );
    let (duration, tracks) = inspect(&output);
    assert_eq!(tracks.len(), 3);
    assert_av(&tracks);
    assert!(
        (duration - 4.0).abs() < 0.25,
        "Matroska duration drift: {duration}s"
    );
    Ok(())
}
