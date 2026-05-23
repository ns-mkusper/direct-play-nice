//! OCR subsystem tests covering engine selection, parsing behavior, and subtitle text-processing outcomes.

use crate::subtitle_ocr::*;
use mockito::Server;
use std::fs::File;
use std::io::Write;
use std::ptr;
use std::sync::{Mutex, MutexGuard, OnceLock};
use strsim::jaro_winkler;
use tempfile::TempDir;

fn env_lock() -> MutexGuard<'static, ()> {
    static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
    LOCK.get_or_init(|| Mutex::new(()))
        .lock()
        .unwrap_or_else(|poisoned| poisoned.into_inner())
}

fn normalize_text_for_word_similarity(input: &str) -> String {
    input
        .to_uppercase()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

fn normalize_text_for_char_similarity(input: &str) -> String {
    input
        .to_uppercase()
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect()
}

#[allow(clippy::needless_range_loop)]
fn char_error_rate(expected: &str, actual: &str) -> f32 {
    let expected_chars: Vec<char> = expected.chars().collect();
    let actual_chars: Vec<char> = actual.chars().collect();
    if expected_chars.is_empty() {
        return if actual_chars.is_empty() { 0.0 } else { 1.0 };
    }

    let m = expected_chars.len();
    let n = actual_chars.len();
    let mut dp = vec![vec![0usize; n + 1]; m + 1];
    for i in 0..=m {
        dp[i][0] = i;
    }
    for j in 0..=n {
        dp[0][j] = j;
    }
    for i in 1..=m {
        for j in 1..=n {
            let cost = if expected_chars[i - 1] == actual_chars[j - 1] {
                0
            } else {
                1
            };
            dp[i][j] = std::cmp::min(
                std::cmp::min(dp[i - 1][j] + 1, dp[i][j - 1] + 1),
                dp[i - 1][j - 1] + cost,
            );
        }
    }
    dp[m][n] as f32 / expected_chars.len() as f32
}

#[test]
fn external_ocr_command_parser_requires_program_name() {
    assert!(parse_external_ocr_argv("").is_err());
    assert!(parse_external_ocr_argv("   ").is_err());
}

#[test]
fn external_ocr_command_parser_splits_args_without_shell() {
    let argv = parse_external_ocr_argv("python3 /opt/ocr/run.py --mode fast")
        .expect("parser should accept executable + args");
    assert_eq!(argv[0], "python3");
    assert_eq!(argv[1], "/opt/ocr/run.py");
    assert_eq!(argv[2], "--mode");
    assert_eq!(argv[3], "fast");
}

#[test]
fn overlap_sanitization_truncates_earlier_block() {
    let mut cues = vec![
        SubtitleCue {
            start_ms: 1000,
            end_ms: 3000,
            text: "a".to_string(),
        },
        SubtitleCue {
            start_ms: 2500,
            end_ms: 4000,
            text: "b".to_string(),
        },
    ];
    sanitize_cues(&mut cues, OcrFormat::Srt);
    assert_eq!(cues[0].end_ms, 2500);
    assert_eq!(cues[1].start_ms, 2500);
}

#[test]
fn srt_timestamp_formats_correctly() {
    assert_eq!(format_srt_timestamp(0), "00:00:00,000");
    assert_eq!(format_srt_timestamp(3_723_004), "01:02:03,004");
}

#[test]
fn language_mapping_handles_iso_tags() {
    assert_eq!(map_language_tag_to_tesseract("en"), Some("eng".to_string()));
    assert_eq!(
        map_language_tag_to_tesseract("eng"),
        Some("eng".to_string())
    );
    assert_eq!(
        map_language_tag_to_tesseract("pt-BR"),
        Some("por".to_string())
    );
    assert_eq!(map_language_tag_to_tesseract(""), None);
}

#[test]
fn resolve_language_prefers_stream_metadata_then_config_then_system_then_english() {
    let available = ["eng", "spa", "fra"]
        .iter()
        .map(|s| (*s).to_string())
        .collect::<HashSet<_>>();

    assert_eq!(
        resolve_ocr_language(
            Some("spa"),
            Some("eng"),
            Some("fr_FR.UTF-8"),
            &available,
            OcrEngine::Tesseract
        ),
        "spa"
    );
    assert_eq!(
        resolve_ocr_language(
            None,
            Some("fra"),
            Some("en_US.UTF-8"),
            &available,
            OcrEngine::Tesseract
        ),
        "fra"
    );
    assert_eq!(
        resolve_ocr_language(
            None,
            None,
            Some("fr_FR.UTF-8"),
            &available,
            OcrEngine::Tesseract
        ),
        "fra"
    );
    assert_eq!(
        resolve_ocr_language(None, None, Some("zz_ZZ"), &available, OcrEngine::Tesseract),
        "eng"
    );
}

#[test]
fn resolve_language_ai_prefers_tags_without_lang_list() {
    let available = HashSet::<String>::new();

    assert_eq!(
        resolve_ocr_language(Some("jpn"), None, None, &available, OcrEngine::PpOcrV4),
        "jpn"
    );
    assert_eq!(
        resolve_ocr_language(Some("eng"), None, None, &available, OcrEngine::PpOcrV3),
        "eng"
    );
    assert_eq!(
        resolve_ocr_language(None, None, None, &available, OcrEngine::External),
        "eng"
    );
}

#[test]
fn non_english_fallback_requires_matching_tesseract_pack() {
    let available = ["eng"].iter().map(|s| (*s).to_string()).collect();
    assert_eq!(
        resolve_tesseract_fallback_language_with_available("spa", &available),
        None
    );
    assert_eq!(
        resolve_tesseract_fallback_language_with_available("fre", &available),
        None
    );
}

#[test]
fn english_fallback_prefers_eng_when_available() {
    let available = ["eng"].iter().map(|s| (*s).to_string()).collect();
    assert_eq!(
        resolve_tesseract_fallback_language_with_available("eng", &available),
        Some("eng".to_string())
    );
}

#[test]
fn fallback_uses_mapped_language_code_when_available() {
    let available = ["fra", "spa"].iter().map(|s| (*s).to_string()).collect();
    assert_eq!(
        resolve_tesseract_fallback_language_with_available("fre", &available),
        Some("fra".to_string())
    );
    assert_eq!(
        resolve_tesseract_fallback_language_with_available("es", &available),
        Some("spa".to_string())
    );
}

#[test]
fn rec_profile_routing_prefers_english_for_eng_and_latin_for_romance_langs() {
    assert_eq!(rec_profile_for_language("eng"), OcrRecProfile::English);
    assert_eq!(rec_profile_for_language("en"), OcrRecProfile::English);
    assert_eq!(rec_profile_for_language("fra"), OcrRecProfile::Latin);
    assert_eq!(rec_profile_for_language("fre"), OcrRecProfile::Latin);
    assert_eq!(rec_profile_for_language("es"), OcrRecProfile::Latin);
    assert_eq!(rec_profile_for_language("spa"), OcrRecProfile::Latin);
}

#[test]
fn rec_profile_routing_handles_dedicated_and_non_latin_codes() {
    assert_eq!(rec_profile_for_language("jpn"), OcrRecProfile::Japanese);
    assert_eq!(rec_profile_for_language("ja"), OcrRecProfile::Japanese);
    assert_eq!(rec_profile_for_language("kor"), OcrRecProfile::Korean);
    assert_eq!(rec_profile_for_language("ko"), OcrRecProfile::Korean);
    assert_eq!(rec_profile_for_language("zho"), OcrRecProfile::Cjk);
    assert_eq!(rec_profile_for_language("rus"), OcrRecProfile::Multilingual);
    assert_eq!(rec_profile_for_language("ara"), OcrRecProfile::Multilingual);
    assert_eq!(rec_profile_for_language("zzz"), OcrRecProfile::Latin);
}

#[test]
fn rec_profile_routing_respects_latin_script_hint() {
    assert_eq!(rec_profile_for_language("sr-Latn"), OcrRecProfile::Latin);
}

#[test]
fn rec_profile_routing_uses_script_subtags_when_present() {
    assert_eq!(
        rec_profile_for_language("sr-Cyrl"),
        OcrRecProfile::Multilingual
    );
    assert_eq!(rec_profile_for_language("zh-Hant"), OcrRecProfile::Cjk);
    assert_eq!(rec_profile_for_language("ko-Hang"), OcrRecProfile::Korean);
}

#[test]
fn rec_profile_routing_matrix_covers_common_language_families() {
    let cases = [
        ("eng", OcrRecProfile::English),
        ("en", OcrRecProfile::English),
        ("fra", OcrRecProfile::Latin),
        ("deu", OcrRecProfile::Latin),
        ("ita", OcrRecProfile::Latin),
        ("por", OcrRecProfile::Latin),
        ("jpn", OcrRecProfile::Japanese),
        ("kor", OcrRecProfile::Korean),
        ("zho", OcrRecProfile::Cjk),
        ("chi_sim", OcrRecProfile::Cjk),
        ("chi_tra", OcrRecProfile::Cjk),
        ("rus", OcrRecProfile::Multilingual),
        ("ukr", OcrRecProfile::Multilingual),
        ("ara", OcrRecProfile::Multilingual),
        ("fas", OcrRecProfile::Multilingual),
        ("heb", OcrRecProfile::Multilingual),
        ("ell", OcrRecProfile::Multilingual),
        ("hin", OcrRecProfile::Multilingual),
        ("ben", OcrRecProfile::Multilingual),
        ("tam", OcrRecProfile::Multilingual),
        ("tha", OcrRecProfile::Multilingual),
        ("lao", OcrRecProfile::Multilingual),
        ("khm", OcrRecProfile::Multilingual),
        ("mya", OcrRecProfile::Multilingual),
        ("amh", OcrRecProfile::Multilingual),
        ("dzo", OcrRecProfile::Multilingual),
        ("zzz", OcrRecProfile::Latin),
    ];
    for (language, expected) in cases {
        assert_eq!(
            rec_profile_for_language(language),
            expected,
            "unexpected routing for language {language}"
        );
    }
}

#[test]
fn rec_profile_routing_matrix_covers_script_tag_variants() {
    let cases = [
        ("sr-Latn", OcrRecProfile::Latin),
        ("sr-Cyrl", OcrRecProfile::Multilingual),
        ("fa-Arab", OcrRecProfile::Multilingual),
        ("el-Grek", OcrRecProfile::Multilingual),
        ("he-Hebr", OcrRecProfile::Multilingual),
        ("zh-Hans", OcrRecProfile::Cjk),
        ("zh-Hant", OcrRecProfile::Cjk),
        ("ja-Jpan", OcrRecProfile::Japanese),
        ("ko-Kore", OcrRecProfile::Korean),
    ];
    for (language, expected) in cases {
        assert_eq!(
            rec_profile_for_language(language),
            expected,
            "unexpected script routing for language tag {language}"
        );
    }
}

#[test]
fn rec_profile_custom_routing_override_precedence_is_deterministic() {
    let manifest = r#"
default_profile = "english"

[language_profiles]
rus = "multilingual"

[script_profiles]
cyrl = "multilingual"

[likely_scripts]
rus = "Cyrl"
"#;
    let profile =
        rec_profile_for_language_with_test_config("rus", Some(manifest), Some("rus=latin"), None);
    assert_eq!(profile, OcrRecProfile::Latin);
}

#[test]
fn rec_profile_custom_routing_language_profile_beats_script_profile() {
    let manifest = r#"
default_profile = "english"

[language_profiles]
sr = "latin"

[script_profiles]
cyrl = "multilingual"
"#;
    let profile = rec_profile_for_language_with_test_config("sr-Cyrl", Some(manifest), None, None);
    assert_eq!(profile, OcrRecProfile::Latin);
}

#[test]
fn rec_profile_custom_routing_uses_script_hints_when_no_tag_or_alias_match() {
    let manifest = r#"
default_profile = "latin"

[script_profiles]
arab = "multilingual"
"#;
    let profile =
        rec_profile_for_language_with_test_config("arb", Some(manifest), None, Some("arb=Arab"));
    assert_eq!(profile, OcrRecProfile::Multilingual);
}

#[test]
fn rec_profile_custom_routing_uses_tesseract_alias_in_language_lookup() {
    let manifest = r#"
default_profile = "english"

[language_profiles]
fra = "cjk"
"#;
    let profile = rec_profile_for_language_with_test_config("fre", Some(manifest), None, None);
    assert_eq!(profile, OcrRecProfile::Cjk);
}

#[test]
fn rec_profile_custom_routing_tolerates_case_and_whitespace_in_manifest() {
    let manifest = r#"
default_profile = " LATIN "

[language_profiles]
" RU " = " MULTILINGUAL "

[script_profiles]
" CyRL " = " MULTILINGUAL "
"#;
    let profile = rec_profile_for_language_with_test_config("ru", Some(manifest), None, None);
    assert_eq!(profile, OcrRecProfile::Multilingual);
}

#[test]
fn rec_profile_custom_routing_invalid_manifest_falls_back_to_default() {
    let profile = rec_profile_for_language_with_test_config("zzz", Some("not-toml"), None, None);
    assert_eq!(profile, OcrRecProfile::Latin);
}

#[test]
fn rec_profile_custom_routing_ignores_invalid_override_entries() {
    let profile = rec_profile_for_language_with_test_config(
        "rus",
        None,
        Some("rus=not-a-profile,ara=multi"),
        None,
    );
    assert_eq!(profile, OcrRecProfile::Multilingual);
}

#[test]
fn plan_workers_ppocr_caps_by_gpu_capacity() {
    let plan =
        plan_ocr_workers_with_inputs(OcrEngine::PpOcrV3, 8, 32, None, 2, true, vec![1, 0, 1]);
    assert_eq!(plan.worker_count, 4);
    assert_eq!(plan.device_ids, vec![0, 1]);
}

#[test]
fn plan_workers_ppocr_no_detected_devices_falls_back_to_one() {
    let plan =
        plan_ocr_workers_with_inputs(OcrEngine::PpOcrV4, 6, 16, Some(8), 1, true, Vec::new());
    assert_eq!(plan.worker_count, 1);
    assert!(plan.device_ids.is_empty());
}

#[test]
fn plan_workers_non_ppocr_ignores_gpu_device_pool() {
    let plan =
        plan_ocr_workers_with_inputs(OcrEngine::Tesseract, 5, 64, Some(3), 4, true, vec![0, 1]);
    assert_eq!(plan.worker_count, 3);
    assert!(plan.device_ids.is_empty());
}

#[test]
fn parse_cuda_device_list_deduplicates_and_ignores_invalid_entries() {
    let parsed = parse_cuda_device_list("2,abc,1,2, ,0");
    assert_eq!(parsed, vec![0, 1, 2]);
}

#[test]
fn worker_batches_shard_streams_across_cuda_devices() {
    let tasks = (0..4usize)
        .map(|i| OcrTask {
            order: i,
            stream_index: i as i32,
            language: "eng".to_string(),
            subtitle_path: PathBuf::from(format!("stream-{}.srt", i)),
        })
        .collect::<Vec<_>>();
    let batches = build_ocr_worker_batches(tasks, 4, &[0, 1]);
    assert_eq!(batches.len(), 4);
    assert_eq!(batches[0].assigned_device, Some(0));
    assert_eq!(batches[1].assigned_device, Some(1));
    assert_eq!(batches[2].assigned_device, Some(0));
    assert_eq!(batches[3].assigned_device, Some(1));
    assert_eq!(batches[0].tasks[0].stream_index, 0);
    assert_eq!(batches[1].tasks[0].stream_index, 1);
    assert_eq!(batches[2].tasks[0].stream_index, 2);
    assert_eq!(batches[3].tasks[0].stream_index, 3);
}

#[test]
fn worker_batches_round_robin_when_tasks_exceed_workers() {
    let tasks = (0..5usize)
        .map(|i| OcrTask {
            order: i,
            stream_index: i as i32,
            language: "eng".to_string(),
            subtitle_path: PathBuf::from(format!("stream-{}.srt", i)),
        })
        .collect::<Vec<_>>();
    let batches = build_ocr_worker_batches(tasks, 2, &[0, 1]);
    let w0_streams = batches[0]
        .tasks
        .iter()
        .map(|task| task.stream_index)
        .collect::<Vec<_>>();
    let w1_streams = batches[1]
        .tasks
        .iter()
        .map(|task| task.stream_index)
        .collect::<Vec<_>>();
    assert_eq!(w0_streams, vec![0, 2, 4]);
    assert_eq!(w1_streams, vec![1, 3]);
}

#[test]
fn bounding_box_to_ass_position() {
    let bbox = OcrBoundingBox {
        left: 80,
        right: 120,
        top: 790,
        bottom: 810,
    };
    let (x, y) = ass_position_from_bbox(&bbox, Some((1920, 1080)));
    assert_eq!((x, y), (100, 800));
    let formatted = format_ass_text_with_style("Hello Bitch", Some((x, y)), None, false);
    assert!(formatted.contains("{\\pos(100,800)}"));
}

#[test]
fn color_extraction_to_ass_hex() {
    let color = ass_color_from_rgb(255, 0, 0);
    assert_eq!(color, "&H0000FF&");
    let formatted = format_ass_text_with_style("Red", None, Some((255, 0, 0)), false);
    assert!(formatted.contains("\\c&H0000FF&"));
}

#[test]
fn dominant_color_from_rect_prefers_visible_palette() {
    let mut pixels = vec![1u8; 4];
    let mut palette = vec![0u8; 256 * 4];
    palette[4] = 255; // R
    palette[5] = 0; // G
    palette[6] = 0; // B
    palette[7] = 255; // A

    let mut rect: ffi::AVSubtitleRect = unsafe { std::mem::zeroed() };
    rect.w = 2;
    rect.h = 2;
    rect.linesize[0] = 2;
    rect.data[0] = pixels.as_mut_ptr();
    rect.data[1] = palette.as_mut_ptr();
    rect.type_ = ffi::SUBTITLE_BITMAP;
    rect.x = 0;
    rect.y = 0;
    rect.nb_colors = 256;
    rect.flags = 0;
    for i in 2..rect.data.len() {
        rect.data[i] = ptr::null_mut();
    }

    let color = dominant_color_from_rect(&rect).expect("expected color");
    assert_eq!(color, (255, 0, 0));
}

#[test]
fn test_model_downloads_successfully() {
    let mut server = Server::new();
    let body = b"dummy-onnx-model";
    let hash = to_hex_lower(&Sha256::digest(body));
    let mock = server
        .mock("GET", "/model.onnx")
        .with_status(200)
        .with_body(body.as_slice())
        .create();

    let tmp = TempDir::new().unwrap();
    let path = tmp.path().join("model.onnx");
    download_model_with_values(
        &path,
        &format!("{}/model.onnx", server.url()),
        &hash,
        "model.onnx",
    )
    .expect("download should succeed");

    mock.assert();
    let metadata = fs::metadata(&path).unwrap();
    assert_eq!(metadata.len(), body.len() as u64);
}

#[test]
fn test_skips_download_if_cached() {
    let mut server = Server::new();
    let _mock = server
        .mock("GET", "/cached.onnx")
        .expect(0)
        .with_status(200)
        .with_body("should-not-be-downloaded")
        .create();

    let tmp = TempDir::new().unwrap();
    let path = tmp.path().join("cached.onnx");
    let mut file = File::create(&path).unwrap();
    file.write_all(b"cached").unwrap();

    let result = ensure_model_file_with_values(
        tmp.path(),
        "cached.onnx",
        &format!("{}/cached.onnx", server.url()),
        "deadbeef",
    );
    assert!(result.is_ok());
    _mock.assert();
}

#[test]
fn test_handles_corrupted_download() {
    let mut server = Server::new();
    let body = b"partial-data-should-fail";
    let hash = to_hex_lower(&Sha256::digest(body));
    let mock = server
        .mock("GET", "/corrupt.onnx")
        .with_status(200)
        .with_chunked_body(|writer| {
            writer.write_all(&body[..8])?;
            Err(std::io::Error::new(
                std::io::ErrorKind::UnexpectedEof,
                "simulated drop",
            ))
        })
        .create();

    let tmp = TempDir::new().unwrap();
    let path = tmp.path().join("corrupt.onnx");
    let result = download_model_with_values(
        &path,
        &format!("{}/corrupt.onnx", server.url()),
        &hash,
        "corrupt.onnx",
    );
    assert!(result.is_err());
    mock.assert();
    assert!(!path.with_extension("download").exists());
}

#[test]
fn test_downloader_handles_404() {
    let mut server = Server::new();
    let mock = server
        .mock("GET", "/missing.onnx")
        .with_status(404)
        .create();

    let tmp = TempDir::new().unwrap();
    let path = tmp.path().join("missing.onnx");
    let result = download_model_with_values(
        &path,
        &format!("{}/missing.onnx", server.url()),
        "deadbeef",
        "missing.onnx",
    );
    assert!(result.is_err());
    mock.assert();
    assert!(!path.exists());
    assert!(!path.with_extension("download").exists());
}

#[test]
fn test_optional_rec_model_auto_provision_downloads_when_missing() {
    let mut server = Server::new();
    let body = b"auto-provision-rec-model";
    let hash = to_hex_lower(&Sha256::digest(body));
    let mock = server
        .mock("GET", "/optional_rec.onnx")
        .with_status(200)
        .with_body(body.as_slice())
        .create();

    let tmp = TempDir::new().unwrap();
    let spec = ModelSpec {
        filename: "optional_rec.onnx",
        url: Box::leak(format!("{}/optional_rec.onnx", server.url()).into_boxed_str()),
        sha256: Box::leak(hash.to_ascii_uppercase().into_boxed_str()),
    };

    let resolved = resolve_optional_rec_model_with_candidates(
        "DPN_TEST_DO_NOT_SET_OPTIONAL_REC",
        tmp.path(),
        &["missing_candidate.onnx"],
        &spec,
        "test",
        PpOcrVariant::V4,
    )
    .expect("resolver should not error");
    let resolved = resolved.expect("expected auto-provisioned rec model path");
    assert_eq!(resolved.file_name().unwrap(), "optional_rec.onnx");
    assert!(resolved.is_file());
    mock.assert();
}

#[test]
fn test_optional_rec_model_env_override_wins_over_auto_provision() {
    let env_key = "DPN_TEST_REC_MODEL_OVERRIDE";
    let tmp = TempDir::new().unwrap();
    let manual = tmp.path().join("manual_rec.onnx");
    let mut f = File::create(&manual).unwrap();
    f.write_all(b"manual").unwrap();

    std::env::set_var(env_key, &manual);
    let resolved = resolve_optional_rec_model_with_candidates(
        env_key,
        tmp.path(),
        &["missing_candidate.onnx"],
        &ModelSpec {
            filename: "unused.onnx",
            url: "http://127.0.0.1:9/unused.onnx",
            sha256: "DEADBEEF",
        },
        "test",
        PpOcrVariant::V3,
    )
    .expect("resolver should not error");
    std::env::remove_var(env_key);

    let resolved = resolved.expect("expected env override path");
    assert_eq!(resolved, manual);
}

#[test]
fn test_optional_rec_model_auto_provision_failure_returns_none() {
    let mut server = Server::new();
    let mock = server
        .mock("GET", "/missing_optional_rec.onnx")
        .with_status(404)
        .create();

    let tmp = TempDir::new().unwrap();
    let spec = ModelSpec {
        filename: "missing_optional_rec.onnx",
        url: Box::leak(format!("{}/missing_optional_rec.onnx", server.url()).into_boxed_str()),
        sha256: "DEADBEEF",
    };

    let resolved = resolve_optional_rec_model_with_candidates(
        "DPN_TEST_DO_NOT_SET_OPTIONAL_REC_FAIL",
        tmp.path(),
        &["still_missing.onnx"],
        &spec,
        "test",
        PpOcrVariant::V4,
    )
    .expect("resolver should not hard-fail when optional provisioning fails");
    assert!(resolved.is_none());
    mock.assert();
}

#[test]
fn test_optional_multilingual_rec_model_prefers_local_file() {
    let _guard = env_lock();
    std::env::remove_var("DPN_OCR_REC_MULTILINGUAL_MODEL");
    let tmp = TempDir::new().unwrap();
    let local = tmp.path().join("multilingual_PP-OCRv4_rec_infer.onnx");
    let mut f = File::create(&local).unwrap();
    f.write_all(b"local-multi").unwrap();

    let resolved = resolve_optional_multilingual_rec_model(tmp.path(), PpOcrVariant::V4)
        .expect("resolver should not error");
    let resolved = resolved.expect("expected local multilingual rec model");
    assert_eq!(resolved, local);
}

#[test]
fn test_optional_multilingual_rec_model_env_override_wins() {
    let _guard = env_lock();
    let env_key = "DPN_OCR_REC_MULTILINGUAL_MODEL";
    let tmp = TempDir::new().unwrap();
    let manual = tmp.path().join("manual_multi.onnx");
    let mut f = File::create(&manual).unwrap();
    f.write_all(b"manual-multi").unwrap();

    std::env::set_var(env_key, &manual);
    let resolved = resolve_optional_multilingual_rec_model(tmp.path(), PpOcrVariant::V3)
        .expect("resolver should not error");
    std::env::remove_var(env_key);

    let resolved = resolved.expect("expected env override model");
    assert_eq!(resolved, manual);
}

#[test]
fn test_optional_multilingual_rec_model_env_override_missing_path_errors() {
    let _guard = env_lock();
    let env_key = "DPN_OCR_REC_MULTILINGUAL_MODEL";
    let tmp = TempDir::new().unwrap();
    std::env::set_var(env_key, tmp.path().join("does-not-exist.onnx"));
    let err = resolve_optional_multilingual_rec_model(tmp.path(), PpOcrVariant::V4)
        .expect_err("expected missing override path to error");
    std::env::remove_var(env_key);
    assert!(err
        .to_string()
        .contains("DPN_OCR_REC_MULTILINGUAL_MODEL is set but file does not exist"));
}

#[test]
fn test_optional_multilingual_rec_model_prefers_variant_specific_candidate() {
    let _guard = env_lock();
    std::env::remove_var("DPN_OCR_REC_MULTILINGUAL_MODEL");
    let tmp = TempDir::new().unwrap();
    let v3 = tmp.path().join("multilingual_PP-OCRv3_rec_infer.onnx");
    let v4 = tmp.path().join("multilingual_PP-OCRv4_rec_infer.onnx");
    File::create(&v3).unwrap();
    File::create(&v4).unwrap();

    let resolved_v4 = resolve_optional_multilingual_rec_model(tmp.path(), PpOcrVariant::V4)
        .expect("resolver should not error")
        .expect("expected multilingual model");
    let resolved_v3 = resolve_optional_multilingual_rec_model(tmp.path(), PpOcrVariant::V3)
        .expect("resolver should not error")
        .expect("expected multilingual model");

    assert_eq!(resolved_v4, v4);
    assert_eq!(resolved_v3, v3);
}

#[test]
fn test_optional_multilingual_rec_model_ignores_non_rec_or_non_onnx_files() {
    let _guard = env_lock();
    std::env::remove_var("DPN_OCR_REC_MULTILINGUAL_MODEL");
    let tmp = TempDir::new().unwrap();
    File::create(tmp.path().join("multilingual_PP-OCRv4_det_infer.onnx")).unwrap();
    File::create(tmp.path().join("multilingual_PP-OCRv4_rec_infer.txt")).unwrap();
    let resolved = resolve_optional_multilingual_rec_model(tmp.path(), PpOcrVariant::V4)
        .expect("resolver should not error");
    assert!(resolved.is_none());
}

#[test]
fn test_optional_multilingual_rec_model_accepts_non_latin_dedicated_rec_fallback() {
    let _guard = env_lock();
    std::env::remove_var("DPN_OCR_REC_MULTILINGUAL_MODEL");
    let tmp = TempDir::new().unwrap();
    let greek = tmp.path().join("greek_PP-OCRv4_rec_infer.onnx");
    File::create(&greek).unwrap();
    let resolved = resolve_optional_multilingual_rec_model(tmp.path(), PpOcrVariant::V4)
        .expect("resolver should not error")
        .expect("expected non-latin rec fallback");
    assert_eq!(resolved, greek);
}

fn is_skippable_ort_runtime_error(msg: &str) -> bool {
    let lower = msg.to_ascii_lowercase();
    // CI runners often have an unexpected ORT runtime on PATH/LD path.
    // Skip these environment-specific failures so OCR logic tests remain stable.
    (lower.contains("libonnxruntime.so") && lower.contains("cannot open shared object file"))
        || (lower.contains("libonnxruntime.dylib") && lower.contains("no such file"))
        || (lower.contains("onnxruntime.dll")
            && lower.contains("is not compatible with the onnx runtime binary found"))
        || lower.contains("expected getversionstring")
}

#[test]
fn test_onnx_session_initializes_with_fallbacks() {
    let init_result = std::panic::catch_unwind(init_ort_environment);
    match init_result {
        Ok(Ok(_)) => {}
        Ok(Err(err)) => {
            let msg = err.to_string();
            if is_skippable_ort_runtime_error(&msg) {
                eprintln!(
                    "Skipping ORT init fallback test due to environment ORT runtime issue: {}",
                    msg
                );
                return;
            }
            panic!("Failed to initialize ORT environment: {}", msg);
        }
        Err(payload) => {
            let panic_msg = if let Some(msg) = payload.downcast_ref::<&str>() {
                *msg
            } else if let Some(msg) = payload.downcast_ref::<String>() {
                msg.as_str()
            } else {
                "unknown panic payload"
            };
            if is_skippable_ort_runtime_error(panic_msg) {
                eprintln!(
                    "Skipping ORT init fallback test due to environment ORT runtime issue: {}",
                    panic_msg
                );
                return;
            }
            std::panic::resume_unwind(payload);
        }
    }
    assert!(
        ORT_ENV_INIT.get().is_some(),
        "ORT environment not initialized"
    );
}

#[test]
fn test_gpu_requirement_env_gate() {
    if std::env::var("DPN_OCR_REQUIRE_GPU").ok().as_deref() != Some("1") {
        return;
    }
    let selection =
        build_execution_providers().expect("GPU execution providers required but unavailable");
    assert!(
        selection.providers.len() > 1,
        "Expected at least one GPU execution provider plus CPU"
    );
    assert!(selection.gpu_available, "GPU availability was not detected");
}

#[test]
fn test_provider_selection_prefers_cuda_when_available() {
    let (kinds, gpu_available) = select_execution_provider_plan(false, true, true, true).unwrap();
    assert!(gpu_available);
    assert_eq!(kinds.first(), Some(&ExecutionProviderKind::Cuda));
    assert_eq!(kinds.last(), Some(&ExecutionProviderKind::Cpu));
}

#[test]
fn test_provider_selection_requires_gpu_flag() {
    let err = select_execution_provider_plan(true, false, false, false)
        .expect_err("Expected error when requiring GPU without providers");
    assert!(
        err.to_string().contains("No GPU execution providers"),
        "Unexpected error: {}",
        err
    );
}

#[test]
fn test_auto_engine_prefers_ppocr_with_gpu() {
    assert_eq!(
        auto_engine_preference_with_capability(true, false),
        OcrEngine::PpOcrV4
    );
}

#[test]
fn test_auto_engine_prefers_ppocr_v3_on_legacy_gpu() {
    assert_eq!(
        auto_engine_preference_with_capability(true, true),
        OcrEngine::PpOcrV3
    );
}

#[test]
fn test_auto_engine_prefers_tesseract_without_gpu() {
    assert_eq!(
        auto_engine_preference_with_capability(false, false),
        OcrEngine::Tesseract
    );
}

#[test]
fn test_text_similarity_wer_like_threshold() {
    let expected = "THIS OCR QUALITY TEST USES MANY WORDS TO ALLOW SMALL ERRORS WITHOUT FAILING STRICT THRESHOLDS IN CI RUNS TODAY ALWAYS FOR STABILITY CHECKS EACH TIME";
    let actual = "THIS OCR QUALITY TEST USES MANY WORDS TO ALLOW SMALL ERRORS WITHOUT FAILING STRICT THRESHOLDS IN CI RUNS TODAY ALWAYS FOR STABIL1TY CHECKS EACH TIME";
    let expected_words = normalize_text_for_word_similarity(expected);
    let actual_words = normalize_text_for_word_similarity(actual);
    let wer = word_error_rate(&expected_words, &actual_words);
    let similarity = 1.0 - wer;
    assert!(
        similarity > 0.95,
        "WER similarity too low: {} ({} vs {})",
        similarity,
        expected,
        actual
    );
    let expected_chars = normalize_text_for_char_similarity(expected);
    let actual_chars = normalize_text_for_char_similarity(actual);
    let cer = char_error_rate(&expected_chars, &actual_chars);
    let cer_similarity = 1.0 - cer;
    assert!(
        cer_similarity > 0.95,
        "CER similarity too low: {} ({} vs {})",
        cer_similarity,
        expected,
        actual
    );
    let jw = jaro_winkler(expected, actual);
    assert!(jw > 0.90, "Jaro-Winkler too low: {}", jw);
}

#[test]
fn test_spatial_iou_threshold() {
    let a = OcrBoundingBox {
        left: 100,
        right: 200,
        top: 100,
        bottom: 150,
    };
    let b = OcrBoundingBox {
        left: 103,
        right: 198,
        top: 101,
        bottom: 149,
    };
    let iou = intersection_over_union(&a, &b);
    assert!(iou > 0.90, "IoU too low: {}", iou);
}

#[test]
fn test_ppocr_spacing_inserts_space_for_gap() {
    let lines = vec![
        OcrLine {
            text: "By".to_string(),
            bbox: Some(OcrBoundingBox {
                left: 0,
                right: 10,
                top: 0,
                bottom: 10,
            }),
            score: Some(0.9),
            color: None,
            italic: false,
        },
        OcrLine {
            text: "this".to_string(),
            bbox: Some(OcrBoundingBox {
                left: 14,
                right: 30,
                top: 0,
                bottom: 10,
            }),
            score: Some(0.9),
            color: None,
            italic: false,
        },
    ];

    let merged = merge_ocr_lines_with_spacing(lines);
    assert_eq!(merged.len(), 1);
    assert_eq!(merged[0].text, "By this");
}

#[test]
fn test_ppocr_spacing_keeps_compact_tokens() {
    let lines = vec![
        OcrLine {
            text: "By".to_string(),
            bbox: Some(OcrBoundingBox {
                left: 0,
                right: 10,
                top: 0,
                bottom: 10,
            }),
            score: Some(0.9),
            color: None,
            italic: false,
        },
        OcrLine {
            text: "this".to_string(),
            bbox: Some(OcrBoundingBox {
                left: 11,
                right: 30,
                top: 0,
                bottom: 10,
            }),
            score: Some(0.9),
            color: None,
            italic: false,
        },
    ];

    let merged = merge_ocr_lines_with_spacing(lines);
    assert_eq!(merged.len(), 1);
    assert_eq!(merged[0].text, "Bythis");
}

#[test]
fn test_ppocr_spacing_fallback_detection() {
    let lines = vec![OcrLine {
        text: "BythistimeIobserved".to_string(),
        bbox: Some(OcrBoundingBox {
            left: 0,
            right: 100,
            top: 0,
            bottom: 10,
        }),
        score: Some(0.9),
        color: None,
        italic: false,
    }];
    assert!(ppocr_spacing_needs_fallback(&lines));
}

#[test]
fn test_quality_score_penalizes_noise() {
    let clean = "By this time, I observed that the rain had stopped.";
    let noisy = "Bythistime,I0bserved @&| the rain had st0pped";
    let clean_score = ocr_text_quality_score(clean, "eng");
    let noisy_score = ocr_text_quality_score(noisy, "eng");
    assert!(
        clean_score > noisy_score,
        "expected clean score ({clean_score}) > noisy score ({noisy_score})"
    );
}

#[test]
fn test_quality_fallback_detection_triggers_on_noisy_text() {
    let lines = vec![OcrLine {
        text: "BythistimeI0bserved@&|".to_string(),
        bbox: Some(OcrBoundingBox {
            left: 0,
            right: 100,
            top: 0,
            bottom: 10,
        }),
        score: Some(0.95),
        color: None,
        italic: false,
    }];
    assert!(ppocr_needs_quality_fallback(&lines, "eng"));
}

#[test]
fn test_quality_fallback_detection_avoids_good_english_text() {
    let lines = vec![OcrLine {
        text: "By this time, I observed the village from afar.".to_string(),
        bbox: Some(OcrBoundingBox {
            left: 0,
            right: 200,
            top: 0,
            bottom: 12,
        }),
        score: Some(0.93),
        color: None,
        italic: false,
    }];
    assert!(!ppocr_needs_quality_fallback(&lines, "eng"));
}

#[test]
fn test_ppocr_average_confidence_is_area_weighted() {
    let lines = vec![
        OcrLine {
            text: "tiny".to_string(),
            bbox: Some(OcrBoundingBox {
                left: 0,
                right: 10,
                top: 0,
                bottom: 10,
            }),
            score: Some(1.0),
            color: None,
            italic: false,
        },
        OcrLine {
            text: "large".to_string(),
            bbox: Some(OcrBoundingBox {
                left: 0,
                right: 200,
                top: 0,
                bottom: 60,
            }),
            score: Some(0.5),
            color: None,
            italic: false,
        },
    ];
    let avg = ppocr_average_confidence(&lines).expect("expected confidence");
    assert!(
        avg < 0.55,
        "expected weighted confidence to be dominated by larger text area, got {avg}"
    );
}

#[test]
fn test_quality_fallback_detection_triggers_on_impossible_geometry() {
    let lines = vec![OcrLine {
        text: "ThisIsClearlyHorizontalSubtitleText".to_string(),
        bbox: Some(OcrBoundingBox {
            left: 0,
            right: 8,
            top: 0,
            bottom: 80,
        }),
        score: Some(0.96),
        color: None,
        italic: false,
    }];
    assert!(ppocr_needs_quality_fallback(&lines, "eng"));
}

#[test]
fn test_prune_impossible_geometry_discards_vertical_hallucinations() {
    let mut lines = vec![
        OcrLine {
            text: "Hallucinated".to_string(),
            bbox: Some(OcrBoundingBox {
                left: 0,
                right: 8,
                top: 0,
                bottom: 80,
            }),
            score: Some(0.95),
            color: None,
            italic: false,
        },
        OcrLine {
            text: "Valid subtitle".to_string(),
            bbox: Some(OcrBoundingBox {
                left: 0,
                right: 120,
                top: 0,
                bottom: 18,
            }),
            score: Some(0.95),
            color: None,
            italic: false,
        },
    ];
    let discarded = prune_impossible_geometry(&mut lines, "eng");
    assert_eq!(discarded, 1);
    assert_eq!(lines.len(), 1);
    assert_eq!(lines[0].text, "Valid subtitle");
}

#[test]
fn test_quality_thresholds_adapt_to_stream_baseline() {
    let mut baseline = OcrQualityBaseline::default();
    for _ in 0..16 {
        baseline.observe(0.80, 0.90, 120_000);
    }
    let thresholds = quality_fallback_thresholds(&baseline).expect("baseline should be ready");
    assert!(
        thresholds.quality > 0.67 && thresholds.quality < 0.69,
        "unexpected quality threshold {}",
        thresholds.quality
    );
    assert!(
        thresholds.confidence > 0.75 && thresholds.confidence < 0.77,
        "unexpected confidence threshold {}",
        thresholds.confidence
    );
}

#[test]
fn test_quality_thresholds_ignore_samples_after_baseline_window() {
    let mut baseline = OcrQualityBaseline::default();
    for _ in 0..16 {
        baseline.observe(0.82, 0.88, 210_000);
    }
    assert!(
        quality_fallback_thresholds(&baseline).is_none(),
        "expected no thresholds when all samples are outside baseline window"
    );
}

#[test]
fn test_postprocess_english_deglues_common_tokens_algorithmically() {
    assert_eq!(
        split_glued_ascii_token("Ibegpardon").as_deref(),
        Some("I beg pardon")
    );
    assert_eq!(
        split_glued_ascii_token("Ihavenot").as_deref(),
        Some("I have not")
    );
    assert_eq!(
        split_glued_ascii_token("Ishall").as_deref(),
        Some("I shall")
    );
}

#[test]
fn test_split_glued_token_dp_segmentation() {
    assert_eq!(
        split_glued_ascii_token("Ibegpardon").as_deref(),
        Some("I beg pardon")
    );
    assert_eq!(
        split_glued_ascii_token("Standdown").as_deref(),
        Some("Stand down")
    );
    assert_eq!(split_glued_ascii_token("Tonight"), None);
}

#[test]
fn test_postprocess_non_english_passthrough() {
    let src = "Notonlyme";
    let got = postprocess_ocr_text(src, "jpn");
    assert_eq!(got, "Notonlyme");
}

#[test]
fn test_color_distance_threshold() {
    let mut pixels = vec![1u8; 4];
    let mut palette = vec![0u8; 256 * 4];
    palette[4] = 255;
    palette[5] = 0;
    palette[6] = 0;
    palette[7] = 255;

    let mut rect: ffi::AVSubtitleRect = unsafe { std::mem::zeroed() };
    rect.w = 2;
    rect.h = 2;
    rect.linesize[0] = 2;
    rect.data[0] = pixels.as_mut_ptr();
    rect.data[1] = palette.as_mut_ptr();
    rect.type_ = ffi::SUBTITLE_BITMAP;
    rect.x = 0;
    rect.y = 0;
    rect.nb_colors = 256;
    rect.flags = 0;
    for i in 2..rect.data.len() {
        rect.data[i] = ptr::null_mut();
    }

    let color = dominant_color_from_rect(&rect).expect("expected color");
    let distance = rgb_distance(color, (255, 0, 0));
    assert!(distance < 15.0, "distance too high: {}", distance);
}

#[test]
fn test_golden_dataset_quality() {
    if std::env::var("DPN_OCR_GOLDEN").ok().as_deref() != Some("1") {
        eprintln!("Skipping golden OCR quality test (set DPN_OCR_GOLDEN=1 to enable).");
        return;
    }

    let dataset_dir = PathBuf::from("tests/golden_subs");
    if !dataset_dir.is_dir() {
        eprintln!("Golden dataset directory not found; skipping.");
        return;
    }

    let model_dir = match resolve_model_dir() {
        Ok(dir) => dir,
        Err(err) => {
            eprintln!("Model dir unavailable: {err}. Skipping.");
            return;
        }
    };

    let engine_result =
        std::panic::catch_unwind(|| PpOcrEngine::new(&model_dir, PpOcrVariant::V4, false));
    let mut engine = match engine_result {
        Ok(Ok(engine)) => engine,
        Ok(Err(err)) => {
            eprintln!("OCR engine unavailable: {err}. Skipping.");
            return;
        }
        Err(payload) => {
            let panic_msg = if let Some(msg) = payload.downcast_ref::<&str>() {
                *msg
            } else if let Some(msg) = payload.downcast_ref::<String>() {
                msg.as_str()
            } else {
                "unknown panic payload"
            };
            if is_skippable_ort_runtime_error(panic_msg) {
                eprintln!(
                    "Skipping golden OCR quality test due to environment ORT runtime issue: {}",
                    panic_msg
                );
                return;
            }
            std::panic::resume_unwind(payload);
        }
    };

    #[derive(serde::Deserialize)]
    struct GoldenExpected {
        expected_text: String,
        expected_bbox: [i32; 4],
        expected_color_rgb: [u8; 3],
        is_italic: Option<bool>,
    }

    let mut found = 0usize;
    for entry in fs::read_dir(&dataset_dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("png") {
            continue;
        }
        let json_path = path.with_extension("json");
        if !json_path.exists() {
            continue;
        }
        found += 1;
        let json = fs::read_to_string(&json_path).unwrap();
        let expected: GoldenExpected = serde_json::from_str(&json).unwrap();

        let output = engine.extract_lines(&path, "eng").unwrap();
        let Some(line) = output.lines.first() else {
            panic!("No OCR output for {:?}", path);
        };

        let expected_text = expected.expected_text.trim();
        let actual_text = line.text.trim();
        let expected_words = normalize_text_for_word_similarity(expected_text);
        let actual_words = normalize_text_for_word_similarity(actual_text);
        let wer = word_error_rate(&expected_words, &actual_words);
        let expected_chars = normalize_text_for_char_similarity(expected_text);
        let actual_chars = normalize_text_for_char_similarity(actual_text);
        let cer = char_error_rate(&expected_chars, &actual_chars);
        let similarity = (1.0 - wer).max(1.0 - cer);
        assert!(
            similarity > 0.95,
            "Text similarity too low for {:?}: {} (wer {:.3}, cer {:.3})",
            path,
            similarity,
            wer,
            cer
        );

        let expected_bbox = OcrBoundingBox {
            left: expected.expected_bbox[0],
            top: expected.expected_bbox[1],
            right: expected.expected_bbox[2],
            bottom: expected.expected_bbox[3],
        };
        let Some(actual_bbox) = line.bbox.as_ref() else {
            panic!("Missing bounding box for {:?}", path);
        };
        let iou = intersection_over_union(&expected_bbox, actual_bbox);
        assert!(iou > 0.90, "IoU too low for {:?}: {}", path, iou);

        let expected_color = (
            expected.expected_color_rgb[0],
            expected.expected_color_rgb[1],
            expected.expected_color_rgb[2],
        );
        if let Some(actual_color) = line.color {
            let dist = rgb_distance(actual_color, expected_color);
            assert!(
                dist < 15.0,
                "Color distance too high for {:?}: {}",
                path,
                dist
            );
        }

        if let Some(italic) = expected.is_italic {
            assert_eq!(line.italic, italic, "Italic mismatch for {:?}", path);
        }
    }

    if found == 0 {
        eprintln!("Golden dataset empty; skipping.");
    }
}

#[test]
fn test_multilang_prerendered_fixture_accuracy_and_performance() {
    if std::env::var("DPN_OCR_MULTILANG_FIXTURES").ok().as_deref() != Some("1") {
        eprintln!(
            "Skipping multilingual fixture OCR test (set DPN_OCR_MULTILANG_FIXTURES=1 to enable)."
        );
        return;
    }

    let dataset_dir = PathBuf::from("tests/golden_subs/multilang");
    if !dataset_dir.is_dir() {
        eprintln!("Multilingual fixture directory not found; skipping.");
        return;
    }

    let model_dir = match resolve_model_dir() {
        Ok(dir) => dir,
        Err(err) => {
            eprintln!("Model dir unavailable: {err}. Skipping.");
            return;
        }
    };

    let engine_result =
        std::panic::catch_unwind(|| PpOcrEngine::new(&model_dir, PpOcrVariant::V3, false));
    let mut engine = match engine_result {
        Ok(Ok(engine)) => engine,
        Ok(Err(err)) => {
            eprintln!("OCR engine unavailable: {err}. Skipping.");
            return;
        }
        Err(payload) => {
            let panic_msg = if let Some(msg) = payload.downcast_ref::<&str>() {
                *msg
            } else if let Some(msg) = payload.downcast_ref::<String>() {
                msg.as_str()
            } else {
                "unknown panic payload"
            };
            if is_skippable_ort_runtime_error(panic_msg) {
                eprintln!(
                    "Skipping multilingual fixture OCR test due to environment ORT runtime issue: {}",
                    panic_msg
                );
                return;
            }
            std::panic::resume_unwind(payload);
        }
    };

    #[derive(serde::Deserialize)]
    struct MultiLangExpected {
        language: String,
        expected_text: String,
        min_similarity: f32,
        max_infer_ms: u64,
    }

    let mut fixture_count = 0usize;
    for entry in fs::read_dir(&dataset_dir).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        if path.extension().and_then(|e| e.to_str()) != Some("png") {
            continue;
        }
        let json_path = path.with_extension("json");
        if !json_path.exists() {
            continue;
        }
        fixture_count += 1;
        let json = fs::read_to_string(&json_path).unwrap();
        let expected: MultiLangExpected = serde_json::from_str(&json).unwrap();

        let start = std::time::Instant::now();
        let output = engine.extract_lines(&path, &expected.language).unwrap();
        let elapsed_ms = start.elapsed().as_millis() as u64;
        assert!(
            elapsed_ms <= expected.max_infer_ms,
            "Fixture {:?} took too long: {}ms > {}ms",
            path,
            elapsed_ms,
            expected.max_infer_ms
        );

        let actual_text = output
            .lines
            .iter()
            .map(|line| line.text.trim())
            .filter(|line| !line.is_empty())
            .collect::<Vec<_>>()
            .join(" ");
        assert!(
            !actual_text.is_empty(),
            "No OCR output text for fixture {:?}",
            path
        );

        let expected_chars = normalize_text_for_char_similarity(&expected.expected_text);
        let actual_chars = normalize_text_for_char_similarity(&actual_text);
        let cer = char_error_rate(&expected_chars, &actual_chars);
        let char_similarity = (1.0 - cer).clamp(0.0, 1.0);

        let expected_words = normalize_text_for_word_similarity(&expected.expected_text);
        let actual_words = normalize_text_for_word_similarity(&actual_text);
        let wer = word_error_rate(&expected_words, &actual_words);
        let word_similarity = (1.0 - wer).clamp(0.0, 1.0);

        let similarity = char_similarity.max(word_similarity);
        assert!(
            similarity >= expected.min_similarity,
            "Low OCR similarity for {:?}: got {:.3}, want >= {:.3}; expected='{}' actual='{}'",
            path,
            similarity,
            expected.min_similarity,
            expected.expected_text,
            actual_text
        );
    }

    if fixture_count == 0 {
        eprintln!("Multilingual fixtures are empty; skipping.");
    }
}

#[test]
#[ignore]
fn test_manual_ppocr_v3_single_image_probe() {
    if std::env::var("DPN_OCR_MANUAL_PPOCR_V3").ok().as_deref() != Some("1") {
        eprintln!("Skipping manual PP-OCRv3 probe (set DPN_OCR_MANUAL_PPOCR_V3=1 to enable).");
        return;
    }

    let model_dir = resolve_model_dir().expect("resolve model dir");
    let gpu_available = init_ort_environment().expect("init ORT environment");
    eprintln!(
        "ORT environment initialized; gpu_available={}",
        gpu_available
    );

    let mut engine = init_ppocr_engine(&model_dir, require_gpu(), PpOcrVariant::V3)
        .expect("init PP-OCRv3 engine");

    let tmp_dir = tempfile::tempdir().expect("create temp dir");
    let image_path = tmp_dir.path().join("manual_probe.png");
    let img = image::RgbImage::from_pixel(1280, 720, image::Rgb([0, 0, 0]));
    img.save(&image_path).expect("save probe image");

    let output = engine
        .extract_lines(&image_path, "eng")
        .expect("run PP-OCRv3 inference");
    let (sum_conf, count_conf) = output
        .lines
        .iter()
        .filter_map(|line| line.score)
        .fold((0.0f32, 0usize), |(sum, count), score| {
            (sum + score, count + 1)
        });
    let avg_conf = if count_conf > 0 {
        sum_conf / count_conf as f32
    } else {
        0.0
    };
    eprintln!(
        "PP-OCRv3 probe completed: lines={}, avg_conf={:.4}",
        output.lines.len(),
        avg_conf
    );
}
