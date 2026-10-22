#![cfg(feature = "ffmpeg-cli-tests")]

//! Binding-independent regression baselines for the FFmpeg layer.
//!
//! Each scenario converts a generated fixture, inspects the result with the
//! `ffprobe` and `ffmpeg` CLIs only, and compares the observed layout against a
//! snapshot in `tests/baselines/`. The inspection deliberately avoids the
//! crate's own FFmpeg bindings so the same oracle can judge output before and
//! after a bindings or FFmpeg version change.
//!
//! Regenerate snapshots with `DPN_UPDATE_BASELINES=1`.

mod common;

use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

type TestResult = Result<(), Box<dyn std::error::Error>>;

const UPDATE_ENV: &str = "DPN_UPDATE_BASELINES";
const DURATION_TOLERANCE_S: f64 = 0.25;
const PTS_TOLERANCE_S: f64 = 0.1;
const AUDIO_PACKET_TOLERANCE: i64 = 2;
const PSNR_TOLERANCE_DB: f64 = 1.0;
const FPS_TOLERANCE: f64 = 0.1;
/// Hard cap on how far behind the audio/video a subtitle payload may be written.
const MAX_SUBTITLE_LAG_S: f64 = 2.0;
const SUBTITLE_LAG_TOLERANCE_S: f64 = 0.5;

// ---------------------------------------------------------------------------
// Snapshot model
// ---------------------------------------------------------------------------

#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct Snapshot {
    scenario: String,
    container: Container,
    streams: Vec<Stream>,
    subtitles: Vec<SubtitleTrack>,
    /// Largest distance, in seconds of audio/video time already written, that
    /// any nonempty subtitle payload trails its own timestamp. Mixed muxing
    /// APIs push this to the whole file length.
    subtitle_max_lag_s: Option<f64>,
    /// Average PSNR of the output video against the (rescaled) source.
    video_psnr_db: Option<f64>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct Container {
    format: String,
    duration_s: f64,
    /// `None` for formats without ISO BMFF boxes.
    moov_before_mdat: Option<bool>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct Stream {
    index: usize,
    kind: String,
    codec: String,
    language: Option<String>,
    default: bool,
    forced: bool,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    profile: Option<String>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    level: Option<i64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    width: Option<u64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    height: Option<u64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pix_fmt: Option<String>,
    /// Packets per second of stream time, derived from packet timing rather
    /// than ffprobe's `avg_frame_rate` heuristic, which differs across versions.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    fps: Option<f64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    sample_rate: Option<u64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    channels: Option<u64>,
    packets: i64,
    first_pts_s: f64,
    last_end_s: f64,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct SubtitleTrack {
    stream: usize,
    cues: Vec<Cue>,
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
struct Cue {
    start_s: f64,
    end_s: f64,
    text: String,
}

// ---------------------------------------------------------------------------
// CLI helpers
// ---------------------------------------------------------------------------

fn ffmpeg() -> Command {
    let mut cmd = Command::new("ffmpeg");
    cmd.args(["-hide_banner", "-loglevel", "error", "-nostdin", "-y"]);
    cmd
}

fn run(cmd: &mut Command) -> (String, String) {
    let out = cmd.output().expect("execute command");
    let stdout = String::from_utf8_lossy(&out.stdout).into_owned();
    let stderr = String::from_utf8_lossy(&out.stderr).into_owned();
    assert!(
        out.status.success(),
        "{cmd:?} failed: {}\n{stderr}",
        out.status
    );
    (stdout, stderr)
}

fn round3(value: f64) -> f64 {
    (value * 1000.0).round() / 1000.0
}

fn convert(input: &Path, output: &Path, device: &str, extra: &[&str]) -> String {
    let config = output
        .parent()
        .expect("output directory")
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
        .args(extra)
        .arg(input)
        .arg(output);
    let (_, stderr) = run(&mut cmd);
    assert!(
        output.is_file(),
        "conversion did not create {}: {stderr}",
        output.display()
    );
    stderr
}

// ---------------------------------------------------------------------------
// Inspection (ffprobe / ffmpeg CLI only)
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
struct ProbedPacket {
    stream: usize,
    pts_s: f64,
    end_s: f64,
    pos: i64,
    size: i64,
}

fn probe(path: &Path) -> (serde_json::Value, Vec<ProbedPacket>) {
    let (stdout, _) = run(Command::new("ffprobe").args([
        "-v",
        "error",
        "-of",
        "json",
        "-show_format",
        "-show_streams",
        "-show_packets",
        path.to_str().expect("utf8 path"),
    ]));
    let doc: serde_json::Value = serde_json::from_str(&stdout).expect("ffprobe json");
    let packets = doc["packets"]
        .as_array()
        .expect("packets array")
        .iter()
        .map(|p| {
            let pts_s = p["pts_time"]
                .as_str()
                .and_then(|s| s.parse::<f64>().ok())
                .unwrap_or_else(|| panic!("packet without pts_time: {p}"));
            let duration_s = p["duration_time"]
                .as_str()
                .and_then(|s| s.parse::<f64>().ok())
                .unwrap_or(0.0);
            ProbedPacket {
                stream: p["stream_index"].as_u64().expect("stream_index") as usize,
                pts_s,
                end_s: pts_s + duration_s,
                pos: p["pos"]
                    .as_str()
                    .and_then(|s| s.parse::<i64>().ok())
                    .unwrap_or(-1),
                size: p["size"]
                    .as_str()
                    .and_then(|s| s.parse::<i64>().ok())
                    .unwrap_or(0),
            }
        })
        .collect();
    (doc, packets)
}

fn extract_cues(path: &Path, subtitle_ordinal: usize) -> Vec<Cue> {
    let (stdout, _) = run(ffmpeg().arg("-i").arg(path).args([
        "-map",
        &format!("0:s:{subtitle_ordinal}"),
        "-f",
        "srt",
        "-",
    ]));
    parse_srt(&stdout)
}

fn parse_srt_time(value: &str) -> f64 {
    let (hms, millis) = value.trim().split_once(',').expect("srt time");
    let mut parts = hms
        .split(':')
        .map(|p| p.parse::<f64>().expect("srt time part"));
    let h = parts.next().unwrap();
    let m = parts.next().unwrap();
    let s = parts.next().unwrap();
    h * 3600.0 + m * 60.0 + s + millis.parse::<f64>().expect("srt millis") / 1000.0
}

fn parse_srt(text: &str) -> Vec<Cue> {
    let mut cues = Vec::new();
    for block in text.replace("\r\n", "\n").split("\n\n") {
        let mut lines = block.lines().filter(|l| !l.trim().is_empty());
        let Some(_index) = lines.next() else { continue };
        let Some(timing) = lines.next() else { continue };
        let (start, end) = timing.split_once("-->").expect("srt timing line");
        let body: Vec<&str> = lines.collect();
        cues.push(Cue {
            start_s: round3(parse_srt_time(start)),
            end_s: round3(parse_srt_time(end)),
            text: body.join("\n"),
        });
    }
    cues
}

/// Walk top-level ISO BMFF boxes and report whether `moov` precedes `mdat`.
fn moov_before_mdat(path: &Path) -> bool {
    let data = fs::read(path).expect("read output");
    let mut offset = 0usize;
    let (mut moov, mut mdat) = (None, None);
    while offset < data.len() {
        assert!(data.len() - offset >= 8, "truncated box header");
        let size32 = u32::from_be_bytes(data[offset..offset + 4].try_into().unwrap());
        let (size, header) = match size32 {
            0 => (data.len() - offset, 8),
            1 => (
                usize::try_from(u64::from_be_bytes(
                    data[offset + 8..offset + 16].try_into().unwrap(),
                ))
                .unwrap(),
                16,
            ),
            n => (n as usize, 8),
        };
        assert!(
            size >= header && size <= data.len() - offset,
            "invalid box size"
        );
        match &data[offset + 4..offset + 8] {
            b"moov" => moov = Some(offset),
            b"mdat" => {
                mdat.get_or_insert(offset);
            }
            _ => {}
        }
        offset += size;
    }
    moov.expect("moov box") < mdat.expect("mdat box")
}

fn video_psnr(output: &Path, source: &Path, width: u64, height: u64) -> f64 {
    let (_, stderr) = run(Command::new("ffmpeg")
        .args(["-hide_banner", "-nostdin"])
        .arg("-i")
        .arg(output)
        .arg("-i")
        .arg(source)
        .args([
            "-lavfi",
            &format!(
                "[0:v:0]setpts=PTS-STARTPTS[out];[1:v:0]scale={width}:{height}:flags=bicubic,format=yuv420p,setpts=PTS-STARTPTS[ref];[out][ref]psnr"
            ),
            "-f",
            "null",
            "-",
        ]));
    let line = stderr
        .lines()
        .find(|l| l.contains("PSNR") && l.contains("average:"))
        .unwrap_or_else(|| panic!("no PSNR summary in ffmpeg output:\n{stderr}"));
    let average = line
        .split_whitespace()
        .find_map(|tok| tok.strip_prefix("average:"))
        .expect("average token");
    match average {
        "inf" => 99.0,
        v => round3(v.parse::<f64>().expect("psnr value")),
    }
}

fn observe(scenario: &str, output: &Path, source: &Path) -> Snapshot {
    let (doc, packets) = probe(output);
    let format = doc["format"]["format_name"]
        .as_str()
        .expect("format_name")
        .to_owned();
    let duration_s = doc["format"]["duration"]
        .as_str()
        .and_then(|s| s.parse::<f64>().ok())
        .expect("format duration");
    let is_mp4 = format
        .split(',')
        .any(|n| matches!(n, "mov" | "mp4" | "m4a" | "ipod"));

    let mut streams = Vec::new();
    let mut subtitles = Vec::new();
    let mut subtitle_ordinal = 0usize;
    let mut video_dims = None;
    for s in doc["streams"].as_array().expect("streams") {
        let index = s["index"].as_u64().expect("index") as usize;
        let kind = s["codec_type"].as_str().expect("codec_type").to_owned();
        let own: Vec<&ProbedPacket> = packets.iter().filter(|p| p.stream == index).collect();
        let first_pts_s = own.iter().map(|p| p.pts_s).fold(f64::INFINITY, f64::min);
        let last_end_s = own
            .iter()
            .map(|p| p.end_s)
            .fold(f64::NEG_INFINITY, f64::max);
        let tags = &s["tags"];
        let disposition = &s["disposition"];
        let num = |key: &str| s[key].as_u64().or_else(|| s[key].as_str()?.parse().ok());
        let stream = Stream {
            index,
            kind: kind.clone(),
            codec: s["codec_name"].as_str().unwrap_or("").to_owned(),
            language: tags["language"].as_str().map(str::to_owned),
            default: disposition["default"].as_i64() == Some(1),
            forced: disposition["forced"].as_i64() == Some(1),
            profile: s["profile"].as_str().map(str::to_owned),
            level: s["level"].as_i64(),
            width: num("width"),
            height: num("height"),
            pix_fmt: s["pix_fmt"].as_str().map(str::to_owned),
            fps: (kind == "video" && own.len() > 1 && last_end_s > first_pts_s)
                .then(|| round3(own.len() as f64 / (last_end_s - first_pts_s))),
            sample_rate: num("sample_rate"),
            channels: num("channels"),
            packets: own.len() as i64,
            first_pts_s: round3(first_pts_s),
            last_end_s: round3(last_end_s),
        };
        if kind == "video" && video_dims.is_none() {
            video_dims = Some((stream.width.unwrap(), stream.height.unwrap()));
        }
        if kind == "subtitle" {
            subtitles.push(SubtitleTrack {
                stream: index,
                cues: extract_cues(output, subtitle_ordinal),
            });
            subtitle_ordinal += 1;
        }
        streams.push(stream);
    }

    // Subtitle lag: for each nonempty subtitle payload, how much audio/video
    // time had already been written to disk before it.
    let av: Vec<&ProbedPacket> = packets
        .iter()
        .filter(|p| {
            streams
                .iter()
                .any(|s| s.index == p.stream && matches!(s.kind.as_str(), "video" | "audio"))
        })
        .collect();
    let mut max_lag: Option<f64> = None;
    for sub in &streams {
        if sub.kind != "subtitle" {
            continue;
        }
        let empty_size = if sub.codec == "mov_text" { 2 } else { 0 };
        for p in packets
            .iter()
            .filter(|p| p.stream == sub.index && p.size > empty_size)
        {
            assert!(p.pos >= 0, "subtitle packet without byte offset");
            let written_before = av
                .iter()
                .filter(|a| a.pos >= 0 && a.pos < p.pos)
                .map(|a| a.pts_s)
                .fold(f64::NEG_INFINITY, f64::max);
            let lag = (written_before - p.pts_s).max(0.0);
            max_lag = Some(max_lag.map_or(lag, |m| m.max(lag)));
        }
    }

    let video_psnr_db = video_dims.map(|(w, h)| video_psnr(output, source, w, h));

    Snapshot {
        scenario: scenario.to_owned(),
        container: Container {
            format,
            duration_s: round3(duration_s),
            moov_before_mdat: is_mp4.then(|| moov_before_mdat(output)),
        },
        streams,
        subtitles,
        subtitle_max_lag_s: max_lag.map(round3),
        video_psnr_db,
    }
}

// ---------------------------------------------------------------------------
// Comparison
// ---------------------------------------------------------------------------

fn within(a: f64, b: f64, tol: f64) -> bool {
    (a - b).abs() <= tol
}

fn diff(expected: &Snapshot, actual: &Snapshot) -> Vec<String> {
    let mut out = Vec::new();
    let mut check = |ok: bool, msg: String| {
        if !ok {
            out.push(msg);
        }
    };
    let e = &expected.container;
    let a = &actual.container;
    check(
        e.format == a.format,
        format!("container format {} -> {}", e.format, a.format),
    );
    check(
        within(e.duration_s, a.duration_s, DURATION_TOLERANCE_S),
        format!("container duration {}s -> {}s", e.duration_s, a.duration_s),
    );
    check(
        e.moov_before_mdat == a.moov_before_mdat,
        format!(
            "moov before mdat {:?} -> {:?}",
            e.moov_before_mdat, a.moov_before_mdat
        ),
    );
    check(
        expected.streams.len() == actual.streams.len(),
        format!(
            "stream count {} -> {}",
            expected.streams.len(),
            actual.streams.len()
        ),
    );
    for (e, a) in expected.streams.iter().zip(&actual.streams) {
        let id = format!("stream {} ({} {})", e.index, e.kind, e.codec);
        let exact = [
            ("kind", e.kind != a.kind),
            ("codec", e.codec != a.codec),
            ("language", e.language != a.language),
            ("default", e.default != a.default),
            ("forced", e.forced != a.forced),
            ("profile", e.profile != a.profile),
            ("level", e.level != a.level),
            ("width", e.width != a.width),
            ("height", e.height != a.height),
            ("pix_fmt", e.pix_fmt != a.pix_fmt),
            ("sample_rate", e.sample_rate != a.sample_rate),
            ("channels", e.channels != a.channels),
        ];
        for (field, changed) in exact {
            check(
                !changed,
                format!("{id}: {field} changed\n  expected {e:?}\n  actual   {a:?}"),
            );
        }
        check(
            match (e.fps, a.fps) {
                (Some(e), Some(a)) => within(e, a, FPS_TOLERANCE),
                (e, a) => e == a,
            },
            format!("{id}: fps {:?} -> {:?}", e.fps, a.fps),
        );
        let packet_tolerance = if e.kind == "audio" {
            AUDIO_PACKET_TOLERANCE
        } else {
            0
        };
        check(
            (e.packets - a.packets).abs() <= packet_tolerance,
            format!("{id}: packets {} -> {}", e.packets, a.packets),
        );
        check(
            within(e.first_pts_s, a.first_pts_s, PTS_TOLERANCE_S),
            format!("{id}: first pts {}s -> {}s", e.first_pts_s, a.first_pts_s),
        );
        check(
            within(e.last_end_s, a.last_end_s, DURATION_TOLERANCE_S),
            format!("{id}: last end {}s -> {}s", e.last_end_s, a.last_end_s),
        );
    }
    check(
        expected.subtitles.len() == actual.subtitles.len(),
        format!(
            "subtitle track count {} -> {}",
            expected.subtitles.len(),
            actual.subtitles.len()
        ),
    );
    for (e, a) in expected.subtitles.iter().zip(&actual.subtitles) {
        check(
            e.cues.len() == a.cues.len(),
            format!(
                "subtitle stream {}: cue count {} -> {}",
                e.stream,
                e.cues.len(),
                a.cues.len()
            ),
        );
        for (ec, ac) in e.cues.iter().zip(&a.cues) {
            check(
                ec.text == ac.text
                    && within(ec.start_s, ac.start_s, PTS_TOLERANCE_S)
                    && within(ec.end_s, ac.end_s, PTS_TOLERANCE_S),
                format!("subtitle stream {}: cue {ec:?} -> {ac:?}", e.stream),
            );
        }
    }
    match (expected.subtitle_max_lag_s, actual.subtitle_max_lag_s) {
        (Some(e), Some(a)) => check(
            a <= e + SUBTITLE_LAG_TOLERANCE_S && a <= MAX_SUBTITLE_LAG_S,
            format!("subtitle payload lag {e}s -> {a}s"),
        ),
        (e, a) => check(e == a, format!("subtitle lag presence {e:?} -> {a:?}")),
    }
    match (expected.video_psnr_db, actual.video_psnr_db) {
        (Some(e), Some(a)) => check(
            a >= e - PSNR_TOLERANCE_DB,
            format!("video PSNR vs source {e} dB -> {a} dB"),
        ),
        (e, a) => check(e == a, format!("video PSNR presence {e:?} -> {a:?}")),
    }
    out
}

fn baseline_path(scenario: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/baselines")
        .join(format!("{scenario}.json"))
}

fn assert_baseline(scenario: &str, output: &Path, source: &Path) -> TestResult {
    let actual = observe(scenario, output, source);
    let path = baseline_path(scenario);
    if std::env::var_os(UPDATE_ENV).is_some() {
        fs::create_dir_all(path.parent().unwrap())?;
        fs::write(&path, serde_json::to_string_pretty(&actual)? + "\n")?;
        eprintln!("wrote {}", path.display());
        return Ok(());
    }
    let expected: Snapshot = serde_json::from_str(&fs::read_to_string(&path).map_err(|e| {
        format!(
            "missing baseline {} ({e}); run with {UPDATE_ENV}=1 to create it",
            path.display()
        )
    })?)?;
    let differences = diff(&expected, &actual);
    assert!(
        differences.is_empty(),
        "baseline `{scenario}` drifted ({} differences). If the new output is intended, rerun with {UPDATE_ENV}=1 and review the snapshot diff.\n- {}\n\nobserved:\n{}",
        differences.len(),
        differences.join("\n- "),
        serde_json::to_string_pretty(&actual)?
    );
    Ok(())
}

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

/// Four seconds of software video, two mono audio languages, and three text
/// subtitle tracks (the third sparse). MPEG-4/MP2 forces a real transcode.
fn gen_text_subtitle_input(tmp: &TempDir) -> PathBuf {
    common::ensure_ffmpeg_present();
    let av = tmp.path().join("av.mkv");
    run(ffmpeg()
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
        .arg(&av));
    let languages = ["eng", "pol", "fra"];
    let mut mux = ffmpeg();
    mux.arg("-i").arg(&av);
    for (track, _) in languages.iter().enumerate() {
        let path = tmp.path().join(format!("track_{track}.srt"));
        let mut text = format!("1\n00:00:00,250 --> 00:00:00,750\ntrack {track} early payload\n\n");
        if track != 2 {
            text.push_str(&format!(
                "2\n00:00:03,250 --> 00:00:03,750\ntrack {track} late payload\n\n"
            ));
        }
        fs::write(&path, text).unwrap();
        mux.arg("-i").arg(path);
    }
    mux.args(["-map", "0:v:0", "-map", "0:a"]);
    for (track, language) in languages.iter().enumerate() {
        mux.args(["-map", &format!("{}:s:0", track + 1)]);
        mux.args([
            &format!("-metadata:s:s:{track}"),
            &format!("language={language}"),
        ]);
    }
    let input = tmp.path().join("text_tracks.mkv");
    run(mux.args(["-c", "copy", "-c:s", "srt"]).arg(&input));
    input
}

// ---------------------------------------------------------------------------
// Scenarios
// ---------------------------------------------------------------------------

/// Transcode with text subtitles into MP4: stream table, cue timing, payload
/// interleaving, and head `moov`.
#[test]
fn baseline_text_subtitles_mp4() -> TestResult {
    let tmp = TempDir::new()?;
    let input = gen_text_subtitle_input(&tmp);
    let output = tmp.path().join("out.mp4");
    convert(&input, &output, "chromecast_1st_gen", &[]);
    assert_baseline("text_subtitles_mp4", &output, &input)
}

/// The public Matroska route: transcode via MP4 then copy-remux.
#[test]
fn baseline_matroska_remux() -> TestResult {
    let tmp = TempDir::new()?;
    let input = gen_text_subtitle_input(&tmp);
    let output = tmp.path().join("out.mkv");
    convert(&input, &output, "roku_ultra", &["--sub-mode", "skip"]);
    assert_baseline("matroska_remux", &output, &input)
}

/// Odd-width 1080p source downscaled for a 720p device.
#[test]
fn baseline_odd_width_downscale_720p() -> TestResult {
    let tmp = TempDir::new()?;
    let (input, _) = common::gen_odd_width_input(&tmp);
    let output = tmp.path().join("out.mp4");
    convert(&input, &output, "nest_hub", &[]);
    assert_baseline("odd_width_downscale_720p", &output, &input)
}

/// H.264 High@L5.2 with 6-channel E-AC-3: level downgrade and audio transcode.
#[test]
fn baseline_h264_high_level_eac3() -> TestResult {
    let tmp = TempDir::new()?;
    let input = common::gen_h264_high_input(&tmp);
    let output = tmp.path().join("out.mp4");
    convert(&input, &output, "chromecast_1st_gen", &[]);
    assert_baseline("h264_high_level_eac3", &output, &input)
}

/// Two video streams with the extra one dropped by policy.
#[test]
fn baseline_multi_video_ignore_extra() -> TestResult {
    let tmp = TempDir::new()?;
    let (input, _) = common::gen_multi_video_input(&tmp);
    let output = tmp.path().join("out.mp4");
    convert(
        &input,
        &output,
        "chromecast_1st_gen",
        &["--unsupported-video-policy", "ignore"],
    );
    assert_baseline("multi_video_ignore_extra", &output, &input)
}

#[test]
fn srt_parser_handles_multiline_cues() {
    let cues = parse_srt("1\n00:00:00,250 --> 00:00:00,750\nline one\nline two\n\n2\n00:01:02,000 --> 00:01:03,500\nx\n");
    assert_eq!(cues.len(), 2);
    assert_eq!(cues[0].text, "line one\nline two");
    assert_eq!(cues[0].start_s, 0.25);
    assert_eq!(cues[1].start_s, 62.0);
    assert_eq!(cues[1].end_s, 63.5);
}

#[test]
fn diff_flags_layout_and_quality_regressions() {
    let good: Snapshot = serde_json::from_str(
        &fs::read_to_string(baseline_path("text_subtitles_mp4")).expect("checked-in baseline"),
    )
    .expect("baseline parses");
    let mut bad: Snapshot = serde_json::from_str(&serde_json::to_string(&good).unwrap()).unwrap();
    // The historical mixed-writer bug: index at the tail and subtitle payloads
    // deferred to the end of the file, with codecs and timestamps all intact.
    bad.container.moov_before_mdat = Some(false);
    bad.subtitle_max_lag_s = Some(good.container.duration_s);
    bad.video_psnr_db = good.video_psnr_db.map(|p| p - PSNR_TOLERANCE_DB - 0.5);
    let differences = diff(&good, &bad);
    assert!(
        differences.iter().any(|d| d.contains("moov before mdat")),
        "{differences:?}"
    );
    assert!(
        differences
            .iter()
            .any(|d| d.contains("subtitle payload lag")),
        "{differences:?}"
    );
    assert!(
        differences.iter().any(|d| d.contains("video PSNR")),
        "{differences:?}"
    );
    assert!(diff(&good, &good).is_empty());
}
