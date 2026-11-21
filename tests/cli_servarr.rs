#![cfg(feature = "ffmpeg-cli-tests")]

#[path = "common/mod.rs"]
mod common;

use std::fs;
use std::fs::OpenOptions;
use std::io::{Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::process::Command;
use tempfile::TempDir;

fn append_suffix(path: &Path, suffix: &str) -> PathBuf {
    let filename = path
        .file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| String::from("file"));
    let new_name = match filename.rfind('.') {
        Some(idx) => {
            let (stem, ext) = filename.split_at(idx);
            format!("{}{}{}", stem, suffix, ext)
        }
        None => format!("{}{}", filename, suffix),
    };
    match path.parent() {
        Some(parent) => parent.join(new_name),
        None => PathBuf::from(new_name),
    }
}

fn gen_direct_play_mp4(path: &Path) {
    assert!(
        Command::new("ffmpeg")
            .args([
                "-hide_banner",
                "-loglevel",
                "error",
                "-y",
                "-f",
                "lavfi",
                "-i",
                "testsrc=size=640x360:rate=30:duration=2",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=1000:sample_rate=48000:duration=2",
                "-c:v",
                "libx264",
                "-b:v",
                "700k",
                "-minrate",
                "700k",
                "-maxrate",
                "700k",
                "-bufsize",
                "1400k",
                "-profile:v",
                "high",
                "-level:v",
                "4.1",
                "-pix_fmt",
                "yuv420p",
                "-c:a",
                "aac",
                "-b:a",
                "96k",
                "-movflags",
                "+faststart",
                path.to_string_lossy().as_ref(),
            ])
            .status()
            .expect("invoke ffmpeg")
            .success(),
        "ffmpeg failed to generate direct-play sample"
    );
}

fn gen_explicit_language_input(path: &Path, language: &str) {
    assert!(Command::new("ffmpeg")
        .args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc2=size=160x120:rate=25:duration=1",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=1000:sample_rate=48000:duration=1",
            "-c:v",
            "mpeg2video",
            "-c:a",
            "flac",
            "-metadata:s:a:0",
            &format!("language={language}"),
            "-shortest",
        ])
        .arg(path)
        .status()
        .expect("invoke ffmpeg")
        .success());
}

fn gen_explicit_dual_language_input(path: &Path) {
    assert!(Command::new("ffmpeg")
        .args([
            "-hide_banner",
            "-loglevel",
            "error",
            "-y",
            "-f",
            "lavfi",
            "-i",
            "testsrc2=size=160x120:rate=25:duration=1",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=1000:sample_rate=48000:duration=1",
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=500:sample_rate=48000:duration=1",
            "-map",
            "0:v:0",
            "-map",
            "1:a:0",
            "-map",
            "2:a:0",
            "-c:v",
            "mpeg2video",
            "-c:a",
            "flac",
            "-metadata:s:a:0",
            "language=eng",
            "-metadata:s:a:1",
            "language=jpn",
            "-shortest"
        ])
        .arg(path)
        .status()
        .expect("invoke ffmpeg")
        .success());
}

fn mock_explicit_target(
    server: &mut mockito::Server,
    kind: &str,
    input: &Path,
    original: serde_json::Value,
) -> Vec<mockito::Mock> {
    use serde_json::json;
    match kind {
        "sonarr" => vec![
            server
                .mock("GET", "/api/v3/episode/7")
                .with_body(json!({"id":7,"seriesId":9,"episodeFileId":3}).to_string())
                .create(),
            server
                .mock("GET", "/api/v3/series/9")
                .with_body(
                    json!({"id":9,"title":"Example","originalLanguage":original}).to_string(),
                )
                .create(),
            server
                .mock("GET", "/api/v3/episodefile/3")
                .with_body(json!({"id":3,"path":input}).to_string())
                .create(),
        ],
        "radarr" => vec![
            server
                .mock("GET", "/api/v3/movie/7")
                .with_body(json!({"id":7,"movieFileId":3,"originalLanguage":original}).to_string())
                .create(),
            server
                .mock("GET", "/api/v3/moviefile/3")
                .with_body(json!({"id":3,"path":input}).to_string())
                .create(),
        ],
        _ => unreachable!(),
    }
}

fn explicit_download_command(tmp: &TempDir, kind: &str, input: &Path, output: &Path) -> Command {
    let config = tmp.path().join("empty.toml");
    fs::write(&config, "").unwrap();
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env_clear()
        .env("PATH", std::env::var_os("PATH").unwrap())
        .env("HOME", tmp.path())
        .env("DIRECT_PLAY_NICE_LOCK_DIR", tmp.path().join("locks"))
        .env("RUST_LOG", "info")
        .env(format!("{kind}_eventtype"), "Download")
        .env(
            format!(
                "{kind}_{}_path",
                if kind == "sonarr" {
                    "episodefile"
                } else {
                    "moviefile"
                }
            ),
            input,
        )
        .arg("--config-file")
        .arg(config)
        .arg(input)
        .arg(output);
    if let Some(value) = std::env::var_os("LD_LIBRARY_PATH") {
        cmd.env("LD_LIBRARY_PATH", value);
    }
    cmd
}

#[test]
fn explicit_download_enabled_is_rejected_and_disabled_uses_requested_output() {
    for kind in ["sonarr", "radarr"] {
        for (enabled, keep_original) in [(false, true), (false, false), (true, true), (true, false)]
        {
            let tmp = TempDir::new().unwrap();
            let input = tmp.path().join("input.mkv");
            let output = tmp.path().join("explicit output ü + 1.mp4");
            gen_explicit_language_input(&input, if enabled { "eng" } else { "jpn" });
            let original = fs::read(&input).unwrap();
            let mut cmd = explicit_download_command(&tmp, kind, &input, &output);
            cmd.arg("--required-audio-languages").arg("eng");
            if !keep_original {
                cmd.args(["--servarr-keep-present-original-audio", "false"]);
            }
            if enabled {
                cmd.arg("--servarr-language-check");
                cmd.env_remove(if kind == "sonarr" {
                    "sonarr_episodefile_path"
                } else {
                    "radarr_moviefile_path"
                });
            }
            let result = cmd.output().unwrap();
            assert_eq!(
                result.status.success(),
                !enabled,
                "{kind} enabled={enabled} keep_original={keep_original}: {}",
                String::from_utf8_lossy(&result.stderr)
            );
            assert_eq!(output.exists(), !enabled, "{kind} enabled={enabled}");
            assert_eq!(fs::read(&input).unwrap(), original);
            if enabled {
                assert!(String::from_utf8_lossy(&result.stderr)
                    .contains("remove both paths to use the event-resolved paths"));
            }
        }
    }
}

#[test]
fn explicit_download_rejects_before_probe_retag_or_api_for_cli_and_config() {
    use mockito::Matcher;
    for kind in ["sonarr", "radarr"] {
        for (via_config, probe) in [(false, false), (true, false), (false, true)] {
            let tmp = TempDir::new().unwrap();
            let input = tmp.path().join("input.mkv");
            let output = tmp.path().join("output.mp4");
            let cache = tmp.path().join("language-cache.json");
            fs::write(&input, b"not a media file; must remain untouched").unwrap();
            fs::write(&output, b"existing output; must remain untouched").unwrap();
            let mut server = mockito::Server::new();
            let no_get = server.mock("GET", Matcher::Any).expect(0).create();
            let no_post = server.mock("POST", Matcher::Any).expect(0).create();
            let mut cmd = explicit_download_command(&tmp, kind, &input, &output);
            if via_config {
                fs::write(
                    tmp.path().join("empty.toml"),
                    "servarr_language_check = true\n",
                )
                .unwrap();
            } else {
                cmd.arg("--servarr-language-check");
            }
            if probe {
                cmd.arg("--probe-streams");
            }
            cmd.env("DIRECT_PLAY_NICE_LANGUAGE_CACHE", &cache)
                .env(
                    if kind == "sonarr" {
                        "sonarr_episode_id"
                    } else {
                        "radarr_movie_id"
                    },
                    "7",
                )
                .args([
                    "--required-audio-languages",
                    "eng",
                    "--required-subtitle-languages",
                    "eng",
                    "--servarr-preserve-original-audio",
                    "--servarr-untagged-audio-language",
                    "jpn",
                    "--servarr-untagged-subtitle-language",
                    "eng",
                    "--sub-mode",
                    "skip",
                    "--delete-source",
                    "--servarr-api-url",
                    &server.url(),
                    "--servarr-api-key",
                    "test-key",
                ]);
            let result = cmd.output().unwrap();
            assert!(
                !result.status.success(),
                "{kind} via_config={via_config} probe={probe}"
            );
            assert!(String::from_utf8_lossy(&result.stderr)
                .contains("remove both paths to use the event-resolved paths"));
            assert_eq!(
                fs::read(&input).unwrap(),
                b"not a media file; must remain untouched"
            );
            assert_eq!(
                fs::read(&output).unwrap(),
                b"existing output; must remain untouched"
            );
            assert!(!cache.exists());
            no_get.assert();
            no_post.assert();
        }
    }
}

#[test]
fn event_resolved_download_still_checks_languages_in_dry_run() {
    for kind in ["sonarr", "radarr"] {
        let tmp = TempDir::new().unwrap();
        let input = tmp.path().join("input.mkv");
        let config = tmp.path().join("empty.toml");
        fs::write(&config, "").unwrap();
        gen_explicit_language_input(&input, "eng");
        let original = fs::read(&input).unwrap();
        let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
        cmd.env_clear()
            .env("PATH", std::env::var_os("PATH").unwrap())
            .env("HOME", tmp.path())
            .env("DIRECT_PLAY_NICE_LOCK_DIR", tmp.path().join("locks"))
            .env(format!("{kind}_eventtype"), "Download")
            .env(
                if kind == "sonarr" {
                    "sonarr_episodefile_path"
                } else {
                    "radarr_moviefile_path"
                },
                &input,
            )
            .arg("--config-file")
            .arg(&config)
            .args([
                "--dry-run",
                "--servarr-language-check",
                "--required-audio-languages",
                "eng",
            ]);
        if let Some(value) = std::env::var_os("LD_LIBRARY_PATH") {
            cmd.env("LD_LIBRARY_PATH", value);
        }
        let result = cmd.output().unwrap();
        assert!(
            result.status.success(),
            "{kind}: {}",
            String::from_utf8_lossy(&result.stderr)
        );
        assert!(String::from_utf8_lossy(&result.stdout).contains(input.to_str().unwrap()));
        assert_eq!(fs::read(&input).unwrap(), original);
    }
}

#[test]
fn explicit_download_after_double_dash_is_rejected() {
    let tmp = TempDir::new().unwrap();
    let input = tmp.path().join("input with spaces.mkv");
    let output = tmp.path().join("chosen output.mp4");
    let config = tmp.path().join("empty.toml");
    fs::write(&config, "").unwrap();
    gen_explicit_language_input(&input, "eng");
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env_clear()
        .env("PATH", std::env::var_os("PATH").unwrap())
        .env("HOME", tmp.path())
        .env("DIRECT_PLAY_NICE_LOCK_DIR", tmp.path().join("locks"))
        .env("sonarr_eventtype", "Download")
        .args([
            "--config-file",
            config.to_str().unwrap(),
            "--servarr-language-check",
            "--required-audio-languages",
            "eng",
            "--",
        ])
        .arg(&input)
        .arg(&output);
    if let Some(value) = std::env::var_os("LD_LIBRARY_PATH") {
        cmd.env("LD_LIBRARY_PATH", value);
    }
    let result = cmd.output().unwrap();
    assert!(
        !result.status.success(),
        "{}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert!(String::from_utf8_lossy(&result.stderr)
        .contains("remove both paths to use the event-resolved paths"));
    assert!(!output.exists());
    assert!(input.exists());
}

#[test]
fn explicit_download_opt_out_is_still_rejected_when_language_check_enabled() {
    for kind in ["sonarr", "radarr"] {
        let tmp = TempDir::new().unwrap();
        let input = tmp.path().join("input.mkv");
        let output = tmp.path().join("requested-output.mp4");
        gen_explicit_language_input(&input, "jpn");
        let original = fs::read(&input).unwrap();
        let mut cmd = explicit_download_command(&tmp, kind, &input, &output);
        let result = cmd
            .args([
                "--servarr-language-check",
                "--required-audio-languages",
                "eng",
                "--servarr-keep-present-original-audio",
                "false",
            ])
            .output()
            .unwrap();
        assert!(
            !result.status.success(),
            "{kind}: {}",
            String::from_utf8_lossy(&result.stderr)
        );
        assert!(String::from_utf8_lossy(&result.stderr)
            .contains("remove both paths to use the event-resolved paths"));
        assert!(!output.exists());
        assert_eq!(fs::read(&input).unwrap(), original);
    }
}

#[test]
fn explicit_download_rejected_without_searching_for_a_release() {
    use mockito::Matcher;
    use serde_json::json;
    for kind in ["sonarr", "radarr"] {
        let tmp = TempDir::new().unwrap();
        let input = tmp.path().join("input.mkv");
        let output = tmp.path().join("requested-output.mp4");
        gen_explicit_language_input(&input, "jpn");
        let original_bytes = fs::read(&input).unwrap();
        let mut server = mockito::Server::new();
        let _target =
            mock_explicit_target(&mut server, kind, &input, json!({"id":8,"name":"Japanese"}));
        let release = json!({"title":"Example S01E01 English", "seriesTitle":"Example",
            "mappedSeriesId":9,"mappedEpisodeInfo":[{"id":7}],"rejected":false,
            "guid":"replacement","languages":[{"name":"English"}]});
        let search = server
            .mock("GET", "/api/v3/release")
            .match_query(Matcher::Any)
            .with_body(json!([release]).to_string())
            .expect(0)
            .create();
        let no_post = server.mock("POST", Matcher::Any).expect(0).create();
        let mut cmd = explicit_download_command(&tmp, kind, &input, &output);
        cmd.env(
            if kind == "sonarr" {
                "sonarr_episode_id"
            } else {
                "radarr_movie_id"
            },
            "7",
        )
        .args([
            "--servarr-language-check",
            "--required-audio-languages",
            "eng",
            "--servarr-language-dry-run",
            "--servarr-api-url",
            &server.url(),
            "--servarr-api-key",
            "test-key",
        ]);
        let result = cmd.output().unwrap();
        assert!(
            !result.status.success(),
            "{kind}: {}",
            String::from_utf8_lossy(&result.stderr)
        );
        assert!(String::from_utf8_lossy(&result.stderr)
            .contains("remove both paths to use the event-resolved paths"));
        assert!(!output.exists());
        assert_eq!(fs::read(&input).unwrap(), original_bytes);
        search.assert();
        no_post.assert();
    }
}

#[test]
fn explicit_download_dry_run_is_rejected_without_searching() {
    use mockito::Matcher;
    use serde_json::json;
    for kind in ["sonarr", "radarr"] {
        let tmp = TempDir::new().unwrap();
        let input = tmp.path().join("input.mkv");
        let output = tmp.path().join("requested-output.mp4");
        gen_explicit_language_input(&input, "jpn");
        let original_bytes = fs::read(&input).unwrap();
        let mut server = mockito::Server::new();
        let _target =
            mock_explicit_target(&mut server, kind, &input, json!({"id":8,"name":"Japanese"}));
        let release = json!({"title":"Example S01E01 Dual Audio", "seriesTitle":"Example",
            "mappedSeriesId":9,"mappedEpisodeInfo":[{"id":7}],"rejected":false,
            "guid":"replacement","languages":[{"name":"English"},{"name":"Japanese"}]});
        let search = server
            .mock("GET", "/api/v3/release")
            .match_query(Matcher::Any)
            .with_body(json!([release]).to_string())
            .expect(0)
            .create();
        let _history = server
            .mock("GET", "/api/v3/history")
            .match_query(Matcher::Any)
            .with_body(json!({"records":[],"totalRecords":0}).to_string())
            .create();
        let _blocklist = server
            .mock("GET", "/api/v3/blocklist")
            .match_query(Matcher::Any)
            .with_body(json!({"records":[],"totalRecords":0}).to_string())
            .create();
        let no_post = server.mock("POST", Matcher::Any).expect(0).create();
        let mut cmd = explicit_download_command(&tmp, kind, &input, &output);
        cmd.env(
            if kind == "sonarr" {
                "sonarr_episode_id"
            } else {
                "radarr_movie_id"
            },
            "7",
        )
        .args([
            "--servarr-language-check",
            "--required-audio-languages",
            "eng",
            "--servarr-language-dry-run",
            "--servarr-api-url",
            &server.url(),
            "--servarr-api-key",
            "test-key",
        ]);
        let result = cmd.output().unwrap();
        assert!(
            !result.status.success(),
            "{kind}: {}",
            String::from_utf8_lossy(&result.stderr)
        );
        assert!(String::from_utf8_lossy(&result.stderr)
            .contains("remove both paths to use the event-resolved paths"));
        assert!(!output.exists());
        assert_eq!(fs::read(&input).unwrap(), original_bytes);
        search.assert();
        no_post.assert();
    }
}

#[test]
fn explicit_download_strict_policy_rejects_before_conversion() {
    use mockito::Matcher;
    use serde_json::json;
    for kind in ["sonarr", "radarr"] {
        for compliant in [false, true] {
            let tmp = TempDir::new().unwrap();
            let input = tmp.path().join("input.mkv");
            let output = tmp.path().join("requested-output.mp4");
            if compliant {
                gen_explicit_dual_language_input(&input);
            } else {
                gen_explicit_language_input(&input, "eng");
            }
            let original_bytes = fs::read(&input).unwrap();
            let mut server = mockito::Server::new();
            let _target =
                mock_explicit_target(&mut server, kind, &input, json!({"id":8,"name":"Japanese"}));
            let no_search = server
                .mock("GET", "/api/v3/release")
                .match_query(Matcher::Any)
                .expect(0)
                .create();
            let no_post = server.mock("POST", Matcher::Any).expect(0).create();
            let mut cmd = explicit_download_command(&tmp, kind, &input, &output);
            cmd.env(
                if kind == "sonarr" {
                    "sonarr_episode_id"
                } else {
                    "radarr_movie_id"
                },
                "7",
            )
            .args([
                "--servarr-language-check",
                "--required-audio-languages",
                "eng",
                "--servarr-preserve-original-audio",
                "--servarr-api-url",
                &server.url(),
                "--servarr-api-key",
                "test-key",
            ]);
            let result = cmd.output().unwrap();
            assert!(
                !result.status.success(),
                "{kind} compliant={compliant}: {}",
                String::from_utf8_lossy(&result.stderr)
            );
            assert!(!output.exists());
            assert_eq!(fs::read(&input).unwrap(), original_bytes);
            assert!(String::from_utf8_lossy(&result.stderr)
                .contains("remove both paths to use the event-resolved paths"));
            no_search.assert();
            no_post.assert();
        }
    }
}

#[test]
fn explicit_download_is_rejected_even_with_missing_or_conflicting_identity() {
    use mockito::Matcher;
    use serde_json::json;
    for kind in ["sonarr", "radarr"] {
        for mismatch in [false, true] {
            let tmp = TempDir::new().unwrap();
            let input = tmp.path().join("input.mkv");
            let other = tmp.path().join("other.mkv");
            let output = tmp.path().join("requested-output.mp4");
            gen_explicit_language_input(&input, "jpn");
            fs::write(&other, b"other file").unwrap();
            let original_bytes = fs::read(&input).unwrap();
            let mut server = mockito::Server::new();
            let _target =
                mock_explicit_target(&mut server, kind, &input, json!({"id":0,"name":"Unknown"}));
            let no_search = server
                .mock("GET", "/api/v3/release")
                .match_query(Matcher::Any)
                .expect(0)
                .create();
            let no_post = server.mock("POST", Matcher::Any).expect(0).create();
            let mut cmd = explicit_download_command(&tmp, kind, &input, &output);
            if mismatch {
                cmd.env(
                    if kind == "sonarr" {
                        "sonarr_episodefile_path"
                    } else {
                        "radarr_moviefile_path"
                    },
                    &other,
                );
            }
            cmd.env(
                if kind == "sonarr" {
                    "sonarr_episode_id"
                } else {
                    "radarr_movie_id"
                },
                "7",
            )
            .args([
                "--servarr-language-check",
                "--required-audio-languages",
                "eng",
                "--servarr-language-dry-run",
                "--servarr-api-url",
                &server.url(),
                "--servarr-api-key",
                "test-key",
            ]);
            let result = cmd.output().unwrap();
            assert!(
                !result.status.success(),
                "{kind} mismatch={mismatch}: {}",
                String::from_utf8_lossy(&result.stderr)
            );
            assert!(String::from_utf8_lossy(&result.stderr)
                .contains("remove both paths to use the event-resolved paths"));
            assert!(!output.exists());
            assert_eq!(fs::read(&input).unwrap(), original_bytes);
            no_search.assert();
            no_post.assert();
        }
    }
}

#[test]
fn explicit_download_is_rejected_before_arr_current_file_lookup() {
    use mockito::Matcher;
    use serde_json::json;
    for kind in ["sonarr", "radarr"] {
        let tmp = TempDir::new().unwrap();
        let input = tmp.path().join("input.mkv");
        let current = tmp.path().join("different-current.mkv");
        let output = tmp.path().join("requested-output.mp4");
        gen_explicit_language_input(&input, "jpn");
        gen_explicit_language_input(&current, "jpn");
        let mut server = mockito::Server::new();
        let _target = mock_explicit_target(
            &mut server,
            kind,
            &current,
            json!({"id":8,"name":"Japanese"}),
        );
        let no_search = server
            .mock("GET", "/api/v3/release")
            .match_query(Matcher::Any)
            .expect(0)
            .create();
        let no_post = server.mock("POST", Matcher::Any).expect(0).create();
        let mut cmd = explicit_download_command(&tmp, kind, &input, &output);
        cmd.env(
            if kind == "sonarr" {
                "sonarr_episode_id"
            } else {
                "radarr_movie_id"
            },
            "7",
        )
        .args([
            "--servarr-language-check",
            "--required-audio-languages",
            "eng",
            "--servarr-language-dry-run",
            "--servarr-api-url",
            &server.url(),
            "--servarr-api-key",
            "test-key",
        ]);
        let result = cmd.output().unwrap();
        assert!(
            !result.status.success(),
            "{kind}: {}",
            String::from_utf8_lossy(&result.stderr)
        );
        assert!(String::from_utf8_lossy(&result.stderr)
            .contains("remove both paths to use the event-resolved paths"));
        assert!(!output.exists());
        no_search.assert();
        no_post.assert();
    }
}

#[test]
fn explicit_download_never_overwrites_the_supplied_existing_output() {
    for kind in ["sonarr", "radarr"] {
        for enabled in [false, true] {
            let tmp = TempDir::new().unwrap();
            let input = tmp.path().join("input.mkv");
            let output = tmp.path().join("requested-output.mp4");
            gen_explicit_language_input(&input, "eng");
            let original = fs::read(&input).unwrap();
            fs::write(&output, b"existing output").unwrap();
            let mut cmd = explicit_download_command(&tmp, kind, &input, &output);
            if enabled {
                cmd.args([
                    "--servarr-language-check",
                    "--required-audio-languages",
                    "eng",
                ]);
            }
            let result = cmd.output().unwrap();
            assert!(
                !result.status.success(),
                "{kind} enabled={enabled}: {}",
                String::from_utf8_lossy(&result.stderr)
            );
            assert_eq!(fs::read(&input).unwrap(), original);
            assert_eq!(fs::read(&output).unwrap(), b"existing output");
        }
    }
}

#[test]
fn language_proof_cannot_be_combined_with_probe_mode() -> Result<(), Box<dyn std::error::Error>> {
    let tmp = TempDir::new()?;
    let config = tmp.path().join("empty.toml");
    fs::write(&config, "")?;
    let output = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .env_remove("DIRECT_PLAY_NICE_CONFIG")
        .args([
            "--config-file",
            config.to_str().unwrap(),
            "--servarr-language-audit",
            "--servarr-language-proof-title",
            "Verified release",
            "--probe-codecs",
        ])
        .output()?;
    assert!(!output.status.success());
    assert!(
        String::from_utf8_lossy(&output.stderr)
            .contains("language release proof options cannot be combined with probe modes"),
        "unexpected stderr: {}",
        String::from_utf8_lossy(&output.stderr)
    );
    Ok(())
}

#[cfg(target_os = "linux")]
#[test]
fn sonarr_proof_import_requires_explicit_filesystem_attestation_in_production_binary() {
    use mockito::Matcher;
    use serde_json::json;
    use sha2::{Digest, Sha256};
    use std::os::unix::fs::{MetadataExt, PermissionsExt};
    use std::sync::{
        atomic::{AtomicUsize, Ordering},
        Arc,
    };

    let parent = tempfile::tempdir_in(std::env::current_dir().unwrap()).unwrap();
    let library = parent.path().join("library");
    let downloads = parent.path().join("downloads");
    let recovery = parent.path().join("recovery");
    let cache = parent.path().join("cache");
    for dir in [&library, &downloads, &recovery, &cache] {
        fs::create_dir(dir).unwrap();
    }
    fs::set_permissions(&recovery, fs::Permissions::from_mode(0o700)).unwrap();
    let original = library.join("old.mkv");
    let download = downloads.join("download.mkv");
    for (path, language) in [(&original, "jpn"), (&download, "eng")] {
        assert!(Command::new("ffmpeg")
            .args([
                "-hide_banner",
                "-loglevel",
                "error",
                "-y",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=1000:duration=0.2",
                "-metadata:s:a:0",
                &format!("language={language}"),
                "-c:a",
                "aac"
            ])
            .arg(path)
            .status()
            .unwrap()
            .success());
    }
    let recovery_dir = recovery.join(".dpn-recovery-test");
    fs::create_dir(&recovery_dir).unwrap();
    fs::set_permissions(&recovery_dir, fs::Permissions::from_mode(0o700)).unwrap();
    let blob = recovery_dir.join("0000.blob");
    fs::copy(&original, &blob).unwrap();
    fs::set_permissions(&blob, fs::Permissions::from_mode(0o600)).unwrap();
    let source_meta = fs::metadata(&original).unwrap();
    let media_hash = format!("{:x}", Sha256::digest(fs::read(&original).unwrap()));
    let manifest = recovery_dir.join("manifest.json");
    let contents = serde_json::to_vec(&json!({"version":1,"files":[{
        "original_path":original,"backup_file":"0000.blob","size":source_meta.len(),"sha256":media_hash,
        "original_metadata":{"mode":source_meta.mode(),"uid":source_meta.uid(),"gid":source_meta.gid(),
        "atime_secs":source_meta.atime(),"atime_nsec":source_meta.atime_nsec(),"mtime_secs":source_meta.mtime(),"mtime_nsec":source_meta.mtime_nsec(),
        "ctime_secs":source_meta.ctime(),"ctime_nsec":source_meta.ctime_nsec()}}]})).unwrap();
    fs::write(&manifest, &contents).unwrap();
    fs::set_permissions(&manifest, fs::Permissions::from_mode(0o600)).unwrap();
    let root_meta = fs::metadata(&recovery).unwrap();
    let dir_meta = fs::metadata(&recovery_dir).unwrap();
    let manifest_meta = fs::metadata(&manifest).unwrap();
    let blob_meta = fs::metadata(&blob).unwrap();
    let lock_name = format!(
        ".dpn-target-{:x}.lock",
        Sha256::digest(original.to_str().unwrap().to_lowercase().as_bytes())
    );
    let lock = recovery.join(lock_name);
    fs::write(&lock, b"").unwrap();
    fs::set_permissions(&lock, fs::Permissions::from_mode(0o600)).unwrap();
    let proof_hash = format!("{:x}", Sha256::digest(fs::read(&download).unwrap()));
    let checkpoint = json!({"version":1,"manifest_sha256":format!("{:x}", Sha256::digest(&contents)),"manifest_size":contents.len(),
        "recovery_root":recovery,"manifest_path":manifest,"media_path":original,"root_device":root_meta.dev(),"root_inode":root_meta.ino(),
        "directory_device":dir_meta.dev(),"directory_inode":dir_meta.ino(),"manifest_device":manifest_meta.dev(),"manifest_inode":manifest_meta.ino(),
        "blobs":[{"device":blob_meta.dev(),"inode":blob_meta.ino()}],"sidecars":[]});
    let pending_path = cache.join("servarr-language-pending-grabs.json");
    let grabbed_at = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    let date = Command::new("date")
        .args(["-u", "+%Y-%m-%dT%H:%M:%SZ"])
        .output()
        .unwrap();
    assert!(date.status.success());
    let history_date = String::from_utf8(date.stdout).unwrap().trim().to_owned();
    let pending = json!([{"kind":"Sonarr","target_noun":"episode","target_id":77,"release_title":"Example Show S01E01",
        "guid":"proof-guid","download_id":null,"grabbed_at_unix":grabbed_at,"requirements":{"enabled":true,"audio":["eng"],"subtitles":[]},
        "recovery_manifest":manifest,"proof":{"sha256":proof_hash,"series_id":9,"indexer_id":3,"recovery_checkpoint":checkpoint,
        "previous_file":{"file_id":555,"path":original,"unverified_history_id":null}}}]);
    fs::write(&pending_path, serde_json::to_vec(&pending).unwrap()).unwrap();
    let pending_before = fs::read(&pending_path).unwrap();
    let config = parent.path().join("empty.toml");
    fs::write(&config, "").unwrap();
    let mut server = mockito::Server::new();
    let posts = Arc::new(AtomicUsize::new(0));
    let episode_state = posts.clone();
    let _episode = server
        .mock("GET", "/api/v3/episode/77")
        .with_chunked_body(move |writer| {
            let file_id = if episode_state.load(Ordering::SeqCst) == 0 {
                555
            } else {
                556
            };
            write!(
                writer,
                "{}",
                json!({"id":77,"seriesId":9,"episodeFileId":file_id})
            )
        })
        .create();
    let episodes = server
        .mock("GET", "/api/v3/episode")
        .match_query(Matcher::Exact("episodeFileId=555".into()))
        .with_body(json!([{"id":77,"seriesId":9,"episodeFileId":555}]).to_string())
        .expect_at_least(8)
        .create();
    let no_series = server.mock("GET", "/api/v3/series").expect(0).create();
    let _root = server
        .mock("GET", "/api/v3/rootfolder")
        .with_body(json!([{"path":library}]).to_string())
        .create();
    let _file = server
        .mock("GET", "/api/v3/episodefile/555")
        .with_body(json!({"id":555,"path":original}).to_string())
        .create();
    let _imported_file = server
        .mock("GET", "/api/v3/episodefile/556")
        .with_body(json!({"id":556,"path":download}).to_string())
        .create();
    let _queue = server.mock("GET", "/api/v3/queue").match_query(Matcher::Any).with_body(json!({"totalRecords":1,"records":[{"id":90,"episodeId":77,"seriesId":9,
        "downloadId":"download-1","outputPath":downloads,"title":"Example Show S01E01","status":"completed","trackedDownloadState":"importBlocked",
        "trackedDownloadStatus":"warning","statusMessages":[{"messages":["Existing file meets cutoff: Unknown"]}]}]}).to_string()).create();
    let history_state = posts.clone();
    let imported_path = download.clone();
    let _history = server.mock("GET", "/api/v3/history").match_query(Matcher::Any)
        .with_chunked_body(move |writer| {
            let mut records = vec![json!({"id":101,"episodeId":77,"seriesId":9,"eventType":"grabbed",
                "sourceTitle":"Example Show S01E01","date":history_date,"downloadId":"download-1","data":{"guid":"proof-guid","indexerId":3}})];
            if history_state.load(Ordering::SeqCst) > 0 {
                records.push(json!({"id":102,"episodeId":77,"seriesId":9,"eventType":"downloadFolderImported",
                    "date":history_date,"downloadId":"download-1","data":{"fileId":"556","importedPath":imported_path}}));
            }
            write!(writer, "{}", json!({"totalRecords":records.len(),"records":records}))
        }).create();
    let _manual = server
        .mock("GET", "/api/v3/manualimport")
        .match_query(Matcher::Any)
        .with_body(
            json!([{"path":download,"episodes":[{"id":77}],"seriesId":9,"downloadId":"download-1",
        "quality":{"quality":{"id":4}},"languages":[{"id":1,"name":"English"}],"rejections":[]}])
            .to_string(),
        )
        .create();
    let observed = posts.clone();
    let post = server.mock("POST", "/api/v3/command").match_body(Matcher::PartialJson(json!({"name":"ManualImport","importMode":"copy","files":[{"episodeIds":[77],"downloadId":"download-1"}]})))
        .with_status(201).with_chunked_body(move |writer| { observed.fetch_add(1, Ordering::SeqCst); write!(writer, "{{\"id\":9}}") }).expect(1).create();
    let _command = server
        .mock("GET", "/api/v3/command/9")
        .with_body(json!({"id":9,"status":"completed"}).to_string())
        .create();
    let no_delete = server.mock("DELETE", Matcher::Any).expect(0).create();
    let no_grab = server.mock("POST", "/api/v3/release").expect(0).create();
    let run = |attestation: Option<&str>, dry: bool| {
        let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
        cmd.env_clear()
            .env("PATH", std::env::var_os("PATH").unwrap())
            .env("HOME", parent.path())
            .env(
                "DIRECT_PLAY_NICE_LANGUAGE_CACHE",
                cache.join("language.json"),
            )
            .env("DPN_SERVARR_PROOF_ADDITIONAL_ROOTS", "[]")
            .env("RUST_LOG", "info")
            .args([
                "--config-file",
                config.to_str().unwrap(),
                "--servarr-language-audit",
                "--servarr-language-audit-episode-ids",
                "77",
                "--servarr-language-audit-max-searches",
                "1",
                "--required-audio-languages",
                "eng",
                "--servarr-keep-present-original-audio",
                "false",
                "--servarr-api-url",
                &server.url(),
                "--servarr-api-key",
                "test-key",
                "--servarr-language-recovery-root",
                recovery.to_str().unwrap(),
            ]);
        if let Some(value) = std::env::var_os("LD_LIBRARY_PATH") {
            cmd.env("LD_LIBRARY_PATH", value);
        }
        if let Some(value) = attestation {
            cmd.env("DPN_SONARR_PROOF_IMPORT_FILESYSTEM_ATTESTATION", value);
        }
        if dry {
            cmd.arg("--servarr-language-dry-run");
        }
        cmd.output().unwrap()
    };
    for invalid in [
        None,
        Some(""),
        Some("yes"),
        Some("I_VERIFIED_SONARR_AND_DPN_FILE_AND_OUTPUT_FOLDER_IDENTITIES:78:90"),
        Some("I_VERIFIED_SONARR_AND_DPN_FILE_AND_OUTPUT_FOLDER_IDENTITIES:77:91"),
        Some("I_VERIFIED_SONARR_AND_DPN_FILE_AND_OUTPUT_FOLDER_IDENTITIES:77:90 "),
    ] {
        for dry in [true, false] {
            let output = run(invalid, dry);
            assert!(
                !output.status.success(),
                "unexpected success for {invalid:?}"
            );
            assert!(
                String::from_utf8_lossy(&output.stderr)
                    .contains("DPN_SONARR_PROOF_IMPORT_FILESYSTEM_ATTESTATION"),
                "{}",
                String::from_utf8_lossy(&output.stderr)
            );
            assert_eq!(posts.load(Ordering::SeqCst), 0);
            assert_eq!(fs::read(&pending_path).unwrap(), pending_before);
        }
    }
    let confirmation = "I_VERIFIED_SONARR_AND_DPN_FILE_AND_OUTPUT_FOLDER_IDENTITIES:77:90";
    let dry = run(Some(confirmation), true);
    assert!(
        dry.status.success(),
        "{}",
        String::from_utf8_lossy(&dry.stderr)
    );
    assert!(String::from_utf8_lossy(&dry.stderr).contains("Operator attests"));
    assert_eq!(posts.load(Ordering::SeqCst), 0);
    let apply = run(Some(confirmation), false);
    assert!(
        apply.status.success(),
        "{}",
        String::from_utf8_lossy(&apply.stderr)
    );
    assert_eq!(posts.load(Ordering::SeqCst), 1);
    let bound: serde_json::Value =
        serde_json::from_slice(&fs::read(&pending_path).unwrap()).unwrap();
    assert_eq!(bound[0]["download_id"], "download-1");
    assert_eq!(bound[0]["proof"], pending[0]["proof"]);
    assert_eq!(
        bound[0]["recovery_manifest"],
        pending[0]["recovery_manifest"]
    );
    let intents: serde_json::Value = serde_json::from_slice(
        &fs::read(cache.join("servarr-language-grab-intents.json")).unwrap(),
    )
    .unwrap();
    assert!(intents.as_object().unwrap().is_empty());
    post.assert();
    episodes.assert();
    no_series.assert();
    no_delete.assert();
    no_grab.assert();
}

#[test]
fn sonarr_test_event_short_circuits() -> Result<(), Box<dyn std::error::Error>> {
    let tmp = TempDir::new()?;
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", tmp.path())
        .env("sonarr_eventtype", "Test");
    common::assert_cli_success(cmd);
    Ok(())
}

#[test]
fn sonarr_grab_event_skips_conversion() -> Result<(), Box<dyn std::error::Error>> {
    let tmp = TempDir::new()?;
    let input = tmp.path().join("sample.mkv");
    fs::write(&input, b"dummy")?;
    let before_len = fs::metadata(&input)?.len();
    let temp_path = append_suffix(&input, ".direct-play-nice.tmp");
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("sonarr_eventtype", "Grab")
        .env("sonarr_episodefile_path", &input);

    common::assert_cli_success(cmd);

    assert!(input.exists(), "original file should remain in place");
    assert_eq!(before_len, fs::metadata(&input)?.len(), "file size changed");
    assert!(
        !temp_path.exists(),
        "no temporary output should be produced for Grab events"
    );
    assert!(
        !backup_path.exists(),
        "no backup file should be produced for Grab events"
    );

    Ok(())
}

#[test]
fn sonarr_download_skips_already_direct_play_without_replacement(
) -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let input = tmp.path().join("already_direct_play.mp4");
    gen_direct_play_mp4(&input);

    let final_path = input.with_file_name("already_direct_play.fixed.mp4");
    let temp_path = append_suffix(&final_path, ".direct-play-nice.tmp");
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");

    let config_path = tmp.path().join("direct-play-nice.toml");
    fs::write(&config_path, "")?;

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let output = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"))
        .env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", &input)
        .env("sonarr_series_title", "Compat Test")
        .arg("--config-file")
        .arg(&config_path)
        .arg("-s")
        .arg("chromecast_1st_gen,chromecast_2nd_gen,chromecast_ultra")
        .arg("--video-quality")
        .arg("480p")
        .arg("--audio-quality")
        .arg("128k")
        .arg("--max-video-bitrate")
        .arg("1M")
        .output()?;

    assert!(
        output.status.success(),
        "expected already-compatible Sonarr file to be skipped successfully; stderr:
{}",
        String::from_utf8_lossy(&output.stderr)
    );
    let stderr = String::from_utf8_lossy(&output.stderr);
    assert!(
        stderr.contains("Input is direct-play compatible"),
        "stderr did not include direct-play skip message:
{stderr}"
    );

    assert!(input.exists(), "original file should remain in place");
    assert!(
        !final_path.exists(),
        "skip path should not promote a replacement output"
    );
    assert!(
        !temp_path.exists(),
        "skip path should not leave a temporary output"
    );
    assert!(
        !backup_path.exists(),
        "skip path should not create a source backup"
    );

    Ok(())
}

#[test]
fn sonarr_download_converts_and_replaces() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, original_duration) = common::gen_problem_input(&tmp);
    let final_path = input.with_file_name(format!(
        "{}.fixed.mp4",
        input.file_stem().and_then(|s| s.to_str()).unwrap()
    ));
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");

    if final_path.exists() {
        fs::remove_file(&final_path)?;
    }
    if backup_path.exists() {
        fs::remove_file(&backup_path)?;
    }

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", &input)
        .env("sonarr_series_title", "Example Series");

    common::assert_cli_success(cmd);

    assert!(final_path.exists(), "converted file was not promoted");
    assert!(
        !input.exists(),
        "original file still present after conversion"
    );
    assert!(!backup_path.exists(), "backup file should be cleaned up");

    let final_duration = common::probe_duration_ms(&final_path);
    let delta = (final_duration as i64 - original_duration as i64).abs();
    assert!(delta <= 100, "unexpected duration delta: {} ms", delta);

    Ok(())
}

#[test]
fn sonarr_upgrade_download_flag_converts() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, original_duration) = common::gen_problem_input(&tmp);
    let final_path = input.with_file_name(format!(
        "{}.fixed.mp4",
        input.file_stem().and_then(|s| s.to_str()).unwrap()
    ));
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");

    if final_path.exists() {
        fs::remove_file(&final_path)?;
    }
    if backup_path.exists() {
        fs::remove_file(&backup_path)?;
    }

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", &input)
        .env("sonarr_series_title", "Example Series")
        .env("sonarr_isupgrade", "True");

    common::assert_cli_success(cmd);

    assert!(final_path.exists(), "converted file was not promoted");
    assert!(
        !input.exists(),
        "original file still present after conversion"
    );
    assert!(!backup_path.exists(), "backup file should be cleaned up");

    let final_duration = common::probe_duration_ms(&final_path);
    let delta = (final_duration as i64 - original_duration as i64).abs();
    assert!(delta <= 100, "unexpected duration delta: {} ms", delta);

    Ok(())
}

#[test]
fn sonarr_download_uses_config_for_plex_refresh() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, original_duration) = common::gen_problem_input(&tmp);
    let final_path = input.with_file_name(format!(
        "{}.fixed.mp4",
        input.file_stem().and_then(|s| s.to_str()).unwrap()
    ));
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");

    if final_path.exists() {
        fs::remove_file(&final_path)?;
    }
    if backup_path.exists() {
        fs::remove_file(&backup_path)?;
    }

    let config_path = tmp.path().join("direct-play-nice.toml");
    fs::write(
        &config_path,
        r#"[plex]
refresh = true
url = "http://127.0.0.1:9"
token = "test-token"
"#,
    )?;

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", &input)
        .env("sonarr_series_title", "Example Series")
        .arg("--config-file")
        .arg(&config_path);

    common::assert_cli_success(cmd);

    assert!(final_path.exists(), "converted file was not promoted");
    assert!(
        !input.exists(),
        "original file still present after conversion"
    );
    assert!(!backup_path.exists(), "backup file should be cleaned up");

    let final_duration = common::probe_duration_ms(&final_path);
    let delta = (final_duration as i64 - original_duration as i64).abs();
    assert!(delta <= 100, "unexpected duration delta: {} ms", delta);

    Ok(())
}

#[test]
fn sonarr_download_handles_multiple_paths() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;

    let (input, original_duration) = common::gen_problem_input(&tmp);
    let ep1 = input.with_file_name("episode1.mkv");
    fs::rename(&input, &ep1)?;

    let (second_input, original_duration2) = common::gen_problem_input(&tmp);
    let ep2 = second_input.with_file_name("episode2.mkv");
    fs::rename(&second_input, &ep2)?;

    let out1 = ep1.with_file_name("episode1.fixed.mp4");
    let out2 = ep2.with_file_name("episode2.fixed.mp4");
    let backup1 = append_suffix(&ep1, ".direct-play-nice.bak");
    let backup2 = append_suffix(&ep2, ".direct-play-nice.bak");

    for path in [&out1, &out2, &backup1, &backup2] {
        if path.exists() {
            fs::remove_file(path)?;
        }
    }

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", "")
        .env(
            "sonarr_episodefile_paths",
            format!("{}|{}", ep1.to_string_lossy(), ep2.to_string_lossy()),
        )
        .env("sonarr_series_title", "Example Series Batch");

    common::assert_cli_success(cmd);

    assert!(out1.exists(), "first episode output missing");
    assert!(out2.exists(), "second episode output missing");
    assert!(!ep1.exists(), "first source should be removed");
    assert!(!ep2.exists(), "second source should be removed");
    assert!(!backup1.exists(), "first backup should be cleaned up");
    assert!(!backup2.exists(), "second backup should be cleaned up");

    let final_duration1 = common::probe_duration_ms(&out1);
    let final_duration2 = common::probe_duration_ms(&out2);
    let delta1 = (final_duration1 as i64 - original_duration as i64).abs();
    let delta2 = (final_duration2 as i64 - original_duration2 as i64).abs();
    assert!(
        delta1 <= 100,
        "unexpected duration delta for ep1: {} ms",
        delta1
    );
    assert!(
        delta2 <= 100,
        "unexpected duration delta for ep2: {} ms",
        delta2
    );

    Ok(())
}

#[test]
fn radarr_download_with_match_input_extension_replaces_in_place(
) -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let (input, original_duration) = common::gen_problem_input(&tmp);
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");

    if backup_path.exists() {
        fs::remove_file(&backup_path)?;
    }

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("radarr_eventtype", "Download")
        .env("radarr_moviefile_path", &input)
        .env("radarr_movie_title", "Example Movie");

    common::assert_cli_success(cmd);

    let final_path = input.with_extension("mp4");

    assert!(final_path.exists(), "converted file was not promoted");
    assert!(
        !input.exists(),
        "original file should be removed after conversion"
    );
    assert!(!backup_path.exists(), "backup file should not persist");

    let final_duration = common::probe_duration_ms(&final_path);
    let delta = (final_duration as i64 - original_duration as i64).abs();
    assert!(delta <= 100, "unexpected duration delta: {} ms", delta);

    Ok(())
}

// Use an explicit config and isolated environment: these tests must never pick
// up a real service's Plex token, language policy, or imported media paths.
fn radarr_upgrade_command(tmp: &TempDir, input: &Path) -> Command {
    let config = tmp.path().join("upgrade.toml");
    fs::write(
        &config,
        r#"streaming_devices = "all"
hw_accel = "none"
video_quality = "480p"
max_video_bitrate = "1M"
servarr_output_extension = "mp4"
servarr_output_suffix = ".fixed"
delete_source = true
"#,
    )
    .unwrap();
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    for (key, _) in std::env::vars_os() {
        let name = key.to_string_lossy().to_ascii_lowercase();
        if name.starts_with("sonarr_")
            || name.starts_with("radarr_")
            || name.starts_with("direct_play_nice_")
            || matches!(name.as_str(), "plex_url" | "plex_token")
        {
            cmd.env_remove(key);
        }
    }
    cmd.env("HOME", tmp.path())
        .env("XDG_CONFIG_HOME", tmp.path())
        .env("XDG_CACHE_HOME", tmp.path())
        .env("DIRECT_PLAY_NICE_LOCK_DIR", tmp.path().join("locks"))
        .env("radarr_eventtype", "Download")
        .env("radarr_isupgrade", "True")
        .env("radarr_moviefile_path", input)
        .env("radarr_movie_title", "Synthetic upgrade")
        .args(["--config-file", config.to_str().unwrap()]);
    cmd
}

#[test]
fn radarr_upgrade_download_converts_and_removes_library_source(
) -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let (input, duration) = common::gen_problem_input(&tmp);
    let output = tmp.path().join("input.fixed.mp4");
    let backup = append_suffix(&input, ".direct-play-nice.bak");
    let temp = append_suffix(&output, ".direct-play-nice.tmp");
    // A separate download-client copy must not be removed by library replacement.
    let download = tmp.path().join("download.mkv");
    fs::copy(&input, &download)?;
    let download_bytes = fs::read(&download)?;
    let mut cmd = radarr_upgrade_command(&tmp, &input);
    cmd.env("radarr_moviefile_sourcepath", &download);
    common::assert_cli_success(cmd);

    assert!(output.is_file(), "upgrade output should be promoted");
    assert!(!input.exists(), "replaced library original must be removed");
    assert!(!backup.exists(), "original backup must be removed");
    assert!(!temp.exists(), "conversion temp file must be promoted");
    assert_eq!(fs::read(&download)?, download_bytes);
    assert!(common::probe_duration_ms(&output).abs_diff(duration) <= 100);
    // Decode the complete short synthetic output, not just its container header.
    assert!(Command::new("ffmpeg")
        .args(["-v", "error", "-xerror", "-i"])
        .arg(&output)
        .args(["-f", "null", "-"])
        .status()?
        .success());
    Ok(())
}

#[test]
fn radarr_upgrade_invalid_input_preserves_source() -> Result<(), Box<dyn std::error::Error>> {
    let tmp = TempDir::new()?;
    let input = tmp.path().join("input.mkv");
    let bytes = b"not a media file";
    fs::write(&input, bytes)?;
    let output = tmp.path().join("input.fixed.mp4");
    let result = radarr_upgrade_command(&tmp, &input).output()?;
    assert!(!result.status.success(), "invalid input must not succeed");
    assert_eq!(fs::read(&input)?, bytes);
    assert!(!output.exists());
    assert!(!append_suffix(&input, ".direct-play-nice.bak").exists());
    assert!(!append_suffix(&output, ".direct-play-nice.tmp").exists());
    Ok(())
}

#[test]
fn radarr_upgrade_promotion_failure_restores_source() -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();
    let tmp = TempDir::new()?;
    let (input, _) = common::gen_problem_input(&tmp);
    let original_bytes = fs::read(&input)?;
    let output = tmp.path().join("input.fixed.mp4");
    // Make promotion fail after conversion without relying on Unix permissions.
    fs::create_dir(&output)?;
    let sentinel = output.join("keep.txt");
    fs::write(&sentinel, b"untouched")?;
    let result = radarr_upgrade_command(&tmp, &input).output()?;
    assert!(!result.status.success(), "blocked promotion must fail");
    assert!(
        String::from_utf8_lossy(&result.stderr).contains("could not promote"),
        "must exercise promotion rollback, not an earlier failure: {}",
        String::from_utf8_lossy(&result.stderr)
    );
    assert_eq!(fs::read(&input)?, original_bytes);
    assert_eq!(fs::read(&sentinel)?, b"untouched");
    assert!(!append_suffix(&input, ".direct-play-nice.bak").exists());
    Ok(())
}

#[test]
fn radarr_test_event_short_circuits() -> Result<(), Box<dyn std::error::Error>> {
    let tmp = TempDir::new()?;
    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    cmd.env("DIRECT_PLAY_NICE_LOCK_DIR", tmp.path())
        .env("radarr_eventtype", "Test");
    common::assert_cli_success(cmd);
    Ok(())
}

#[test]
fn sonarr_download_failure_restores_original_and_cleans_temp_files(
) -> Result<(), Box<dyn std::error::Error>> {
    common::ensure_ffmpeg_present();

    let tmp = TempDir::new()?;
    let input = tmp.path().join("episode_corrupt.mkv");
    let temp_path = append_suffix(&input, ".direct-play-nice.tmp");
    let backup_path = append_suffix(&input, ".direct-play-nice.bak");
    let final_path = input.with_file_name("episode_corrupt.fixed.mp4");

    assert!(
        Command::new("ffmpeg")
            .args([
                "-y",
                "-f",
                "lavfi",
                "-i",
                "testsrc2=size=1280x720:rate=24:duration=12",
                "-f",
                "lavfi",
                "-i",
                "sine=frequency=1000:sample_rate=48000:duration=12",
                "-c:v",
                "libx265",
                "-preset",
                "medium",
                "-x265-params",
                "log-level=error",
                "-c:a",
                "aac",
                input.to_string_lossy().as_ref(),
            ])
            .status()
            .expect("generate hevc source")
            .success(),
        "ffmpeg failed to generate source"
    );

    let mut f = OpenOptions::new().read(true).write(true).open(&input)?;
    let len = f.metadata()?.len();
    f.seek(SeekFrom::Start(len / 2))?;
    f.write_all(&vec![0xAAu8; 64 * 1024])?;
    f.flush()?;

    let lock_dir = tmp.path().join("locks");
    fs::create_dir_all(&lock_dir)?;
    // Software decode: hardware decoders conceal the corruption on GPU hosts.
    let config = tmp.path().join("software-decode.toml");
    fs::write(&config, "hw_accel = \"none\"\n")?;

    let mut cmd = Command::new(assert_cmd::cargo::cargo_bin!("direct_play_nice"));
    let status = cmd
        .env("DIRECT_PLAY_NICE_LOCK_DIR", &lock_dir)
        .env("DIRECT_PLAY_NICE_CONFIG", &config)
        .env("sonarr_eventtype", "Download")
        .env("sonarr_episodefile_path", &input)
        .env("sonarr_series_title", "Example Series")
        .status()?;

    assert!(
        !status.success(),
        "expected conversion to fail on corrupt input"
    );
    assert!(
        input.exists(),
        "original source should remain after failure"
    );
    assert!(
        !temp_path.exists(),
        "temporary conversion file should be removed after failure"
    );
    assert!(
        !backup_path.exists(),
        "backup should not remain after failure"
    );
    assert!(
        !final_path.exists(),
        "final promoted output should not exist after failure"
    );

    Ok(())
}
