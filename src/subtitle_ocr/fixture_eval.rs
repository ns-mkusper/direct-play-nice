//! Fixture-based OCR evaluation utilities.
//!
//! Provides repeatable accuracy scoring against golden OCR fixtures for
//! regression detection and tuning comparisons.

use super::*;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum FixtureEvalMode {
    Hybrid,
    StrictGain,
    PureOnnx,
}

impl FixtureEvalMode {
    fn name(self) -> &'static str {
        match self {
            FixtureEvalMode::Hybrid => "hybrid",
            FixtureEvalMode::StrictGain => "strict-gain",
            FixtureEvalMode::PureOnnx => "pure-onnx",
        }
    }

    fn min_gain(self) -> Option<f32> {
        match self {
            FixtureEvalMode::Hybrid => Some(0.08),
            FixtureEvalMode::StrictGain => Some(0.12),
            FixtureEvalMode::PureOnnx => None,
        }
    }
}

#[derive(Debug, serde::Deserialize)]
pub(super) struct FixtureExpected {
    expected_text: String,
    language: Option<String>,
    min_similarity: Option<f32>,
}

#[derive(Debug)]
pub(super) struct FixtureSpec {
    name: String,
    language: String,
    expected_text: String,
    min_similarity: f32,
    image_path: PathBuf,
}

#[derive(Debug, Clone, serde::Serialize)]
pub(crate) struct FixtureResult {
    pub name: String,
    pub language: String,
    pub expected_text: String,
    pub actual_text: String,
    pub infer_ms: u64,
    pub cer: f32,
    pub wer: f32,
    pub strict_cer: f32,
    pub similarity: f32,
    pub min_similarity: f32,
    pub meets_threshold: bool,
    pub used_tesseract: bool,
    pub ppocr_quality: f32,
    pub ppocr_confidence: f32,
    pub tesseract_quality: Option<f32>,
}

#[derive(Debug, Clone, serde::Serialize)]
pub(crate) struct FixtureLanguageSummary {
    pub language: String,
    pub fixture_count: usize,
    pub avg_cer: f32,
    pub avg_wer: f32,
    pub avg_strict_cer: f32,
    pub avg_similarity: f32,
    pub pass_rate: f32,
}

#[derive(Debug, Clone, serde::Serialize)]
pub(crate) struct FixtureModeSummary {
    pub mode: String,
    pub fixture_count: usize,
    pub avg_cer: f32,
    pub avg_wer: f32,
    pub avg_strict_cer: f32,
    pub avg_similarity: f32,
    pub pass_rate: f32,
    pub avg_infer_ms: f32,
    pub tesseract_replacement_count: usize,
    pub per_language: Vec<FixtureLanguageSummary>,
    pub fixtures: Vec<FixtureResult>,
}

#[derive(Debug, Clone, serde::Serialize)]
pub(crate) struct OcrFixtureEvalReport {
    pub fixture_dir: String,
    pub engine: String,
    pub modes: Vec<FixtureModeSummary>,
}

pub(crate) fn evaluate_ocr_fixture_accuracy(
    fixture_dir: &Path,
    ocr_engine: OcrEngine,
) -> Result<OcrFixtureEvalReport> {
    if !fixture_dir.is_dir() {
        bail!("fixture directory not found: '{}'", fixture_dir.display());
    }

    let fixtures = load_fixture_specs(fixture_dir)?;
    if fixtures.is_empty() {
        bail!(
            "no fixture PNG+JSON pairs found under '{}'",
            fixture_dir.display()
        );
    }

    let variant = resolve_eval_variant(ocr_engine)?;
    let model_dir = resolve_model_dir()?;

    let mut mode_summaries = Vec::new();
    for mode in [
        FixtureEvalMode::Hybrid,
        FixtureEvalMode::StrictGain,
        FixtureEvalMode::PureOnnx,
    ] {
        let mut engine = init_ppocr_engine_safe(&model_dir, variant)?;
        let mut results = Vec::with_capacity(fixtures.len());
        for spec in &fixtures {
            results.push(evaluate_fixture(&mut engine, spec, mode)?);
        }
        mode_summaries.push(summarize_mode(mode, results));
    }

    Ok(OcrFixtureEvalReport {
        fixture_dir: fixture_dir.display().to_string(),
        engine: variant.label().to_string(),
        modes: mode_summaries,
    })
}

pub(crate) fn render_ocr_fixture_report_markdown(report: &OcrFixtureEvalReport) -> String {
    let mut lines = Vec::new();
    lines.push("# OCR Fixture Accuracy Report".to_string());
    lines.push(String::new());
    lines.push(format!("- Fixture dir: `{}`", report.fixture_dir));
    lines.push(format!("- Engine: `{}`", report.engine));
    lines.push(String::new());
    lines.push("CER ignores case and whitespace; WER ignores case and whitespace differences between words. Strict CER compares all Unicode characters, including case, punctuation, and whitespace, in expected and postprocessed actual text. Each error rate divides edit distance by the reference length and may exceed 1; an empty reference scores 0 for empty output and 1 otherwise. Similarity is the legacy best of CER and WER similarities, capped by strict CER similarity (each clamped to 0–1). A score of 1 requires exact text equality.".to_string());
    lines.push("OCR output is postprocessed before scoring; PP-OCR lines are joined with spaces, so original line breaks may not survive in actual text.".to_string());
    lines.push(String::new());
    lines.push("## Mode Summary".to_string());
    lines.push(String::new());
    lines.push(
        "| Mode | Fixtures | Avg CER | Avg WER | Avg Strict CER | Avg Similarity | Pass Rate | Avg Infer ms | Tesseract Replacements |"
            .to_string(),
    );
    lines.push("| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |".to_string());
    for mode in &report.modes {
        lines.push(format!(
            "| {} | {} | {:.4} | {:.4} | {:.4} | {:.4} | {:.2}% | {:.2} | {} |",
            mode.mode,
            mode.fixture_count,
            mode.avg_cer,
            mode.avg_wer,
            mode.avg_strict_cer,
            mode.avg_similarity,
            mode.pass_rate * 100.0,
            mode.avg_infer_ms,
            mode.tesseract_replacement_count
        ));
    }

    for mode in &report.modes {
        lines.push(String::new());
        lines.push(format!("## Mode: {}", mode.mode));
        lines.push(String::new());
        lines.push(
            "| Language | Fixtures | Avg CER | Avg WER | Avg Strict CER | Avg Similarity | Pass Rate |".to_string(),
        );
        lines.push("| --- | ---: | ---: | ---: | ---: | ---: | ---: |".to_string());
        for lang in &mode.per_language {
            lines.push(format!(
                "| {} | {} | {:.4} | {:.4} | {:.4} | {:.4} | {:.2}% |",
                lang.language,
                lang.fixture_count,
                lang.avg_cer,
                lang.avg_wer,
                lang.avg_strict_cer,
                lang.avg_similarity,
                lang.pass_rate * 100.0
            ));
        }
    }

    lines.push(String::new());
    lines.join("\n")
}

pub(super) fn resolve_eval_variant(ocr_engine: OcrEngine) -> Result<PpOcrVariant> {
    match ocr_engine {
        OcrEngine::PpOcrV3 => Ok(PpOcrVariant::V3),
        OcrEngine::PpOcrV4 => Ok(PpOcrVariant::V4),
        OcrEngine::Auto => Ok(if prefer_ppocr_v3_for_legacy_nvidia() {
            PpOcrVariant::V3
        } else {
            PpOcrVariant::V4
        }),
        other => bail!(
            "--probe-ocr-fixtures requires an ONNX OCR engine (auto|pp-ocr-v3|pp-ocr-v4); got {:?}",
            other
        ),
    }
}

pub(super) fn init_ppocr_engine_safe(
    model_dir: &Path,
    variant: PpOcrVariant,
) -> Result<PpOcrEngine> {
    let init_result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        init_ppocr_engine(model_dir, require_gpu(), variant)
    }));
    match init_result {
        Ok(Ok(engine)) => Ok(engine),
        Ok(Err(err)) => Err(err),
        Err(payload) => {
            bail!(
                "{} fixture probe failed due to runtime panic: {}",
                variant.label(),
                panic_payload_to_string(payload)
            );
        }
    }
}

pub(super) fn panic_payload_to_string(payload: Box<dyn std::any::Any + Send>) -> String {
    if let Some(msg) = payload.downcast_ref::<&str>() {
        (*msg).to_string()
    } else if let Some(msg) = payload.downcast_ref::<String>() {
        msg.clone()
    } else {
        "unknown panic payload".to_string()
    }
}

pub(super) fn load_fixture_specs(fixture_dir: &Path) -> Result<Vec<FixtureSpec>> {
    let mut fixtures = Vec::new();
    for entry in fs::read_dir(fixture_dir)
        .with_context(|| format!("reading fixture directory '{}'", fixture_dir.display()))?
    {
        let entry = entry?;
        let path = entry.path();
        if path.extension().and_then(|x| x.to_str()) != Some("png") {
            continue;
        }
        let json_path = path.with_extension("json");
        if !json_path.is_file() {
            continue;
        }
        let body = fs::read_to_string(&json_path)
            .with_context(|| format!("reading fixture metadata '{}'", json_path.display()))?;
        let expected: FixtureExpected = serde_json::from_str(&body)
            .with_context(|| format!("parsing fixture metadata '{}'", json_path.display()))?;
        let name = path
            .file_stem()
            .and_then(|x| x.to_str())
            .ok_or_else(|| anyhow!("invalid UTF-8 fixture file name '{}'", path.display()))?
            .to_string();
        fixtures.push(FixtureSpec {
            name,
            language: expected.language.unwrap_or_else(|| "eng".to_string()),
            expected_text: expected.expected_text,
            min_similarity: expected.min_similarity.unwrap_or(0.0).clamp(0.0, 1.0),
            image_path: path,
        });
    }
    fixtures.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(fixtures)
}

pub(super) fn evaluate_fixture(
    engine: &mut PpOcrEngine,
    spec: &FixtureSpec,
    mode: FixtureEvalMode,
) -> Result<FixtureResult> {
    let started = std::time::Instant::now();
    let output = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        engine.extract_lines(&spec.image_path, &spec.language)
    }));
    let mut output = match output {
        Ok(Ok(output)) => output,
        Ok(Err(err)) => return Err(err),
        Err(payload) => {
            bail!(
                "OCR fixture '{}' panicked during inference: {}",
                spec.name,
                panic_payload_to_string(payload)
            );
        }
    };
    let infer_ms = started.elapsed().as_millis() as u64;

    let _ = prune_impossible_geometry(&mut output.lines, &spec.language);
    let ppocr_text = lines_text_for_quality(&output.lines);
    let ppocr_quality = ocr_text_quality_score(&ppocr_text, &spec.language);
    let ppocr_postprocessed_text = postprocess_ocr_text(&ppocr_text, &spec.language);
    let ppocr_postprocessed_quality =
        ocr_text_quality_score(&ppocr_postprocessed_text, &spec.language);
    let ppocr_confidence = ppocr_average_confidence(&output.lines).unwrap_or(0.0);

    let mut actual_text = ppocr_text.clone();
    let mut used_tesseract = false;
    let mut tesseract_quality = None;

    if let Some(min_gain) = mode.min_gain() {
        let should_try_fallback = language_uses_spaces(&spec.language)
            && ppocr_needs_quality_fallback(&output.lines, &spec.language)
            && ppocr_confidence < 0.30;
        if should_try_fallback {
            if let Some(fallback_language) = resolve_tesseract_fallback_language(&spec.language) {
                if let Ok(candidate) =
                    run_tesseract_best_effort(&spec.image_path, &fallback_language)
                {
                    if !candidate.text.is_empty() {
                        let postprocess_repaired_spacing = ppocr_postprocessed_text != ppocr_text
                            && ppocr_postprocessed_text.split_whitespace().count() > 1;
                        let spacing_fallback_requested =
                            ppocr_spacing_needs_fallback(&output.lines);
                        let replace = !postprocess_repaired_spacing
                            && !spacing_fallback_requested
                            && ppocr_confidence < 0.30
                            && candidate.quality >= ppocr_postprocessed_quality + min_gain;
                        tesseract_quality = Some(candidate.quality);
                        if replace {
                            actual_text = normalize_utf8_text(&candidate.text);
                            used_tesseract = true;
                        }
                    }
                }
            }
        }
    }

    actual_text = postprocess_ocr_text(&actual_text, &spec.language);
    actual_text = normalize_utf8_text(&actual_text);

    let metrics = fixture_text_metrics(&spec.expected_text, &actual_text, spec.min_similarity);

    Ok(FixtureResult {
        name: spec.name.clone(),
        language: spec.language.clone(),
        expected_text: spec.expected_text.clone(),
        actual_text,
        infer_ms,
        cer: metrics.cer,
        wer: metrics.wer,
        strict_cer: metrics.strict_cer,
        similarity: metrics.similarity,
        min_similarity: spec.min_similarity,
        meets_threshold: metrics.meets_threshold,
        used_tesseract,
        ppocr_quality,
        ppocr_confidence,
        tesseract_quality,
    })
}

pub(super) fn summarize_mode(
    mode: FixtureEvalMode,
    fixtures: Vec<FixtureResult>,
) -> FixtureModeSummary {
    let fixture_count = fixtures.len();
    let avg_cer = avg(fixtures.iter().map(|x| x.cer));
    let avg_wer = avg(fixtures.iter().map(|x| x.wer));
    let avg_strict_cer = avg(fixtures.iter().map(|x| x.strict_cer));
    let avg_similarity = avg(fixtures.iter().map(|x| x.similarity));
    let pass_rate = avg(fixtures
        .iter()
        .map(|x| if x.meets_threshold { 1.0 } else { 0.0 }));
    let avg_infer_ms = avg(fixtures.iter().map(|x| x.infer_ms as f32));
    let tesseract_replacement_count = fixtures.iter().filter(|x| x.used_tesseract).count();

    let mut by_language: std::collections::BTreeMap<String, Vec<&FixtureResult>> =
        std::collections::BTreeMap::new();
    for fixture in &fixtures {
        by_language
            .entry(fixture.language.clone())
            .or_default()
            .push(fixture);
    }

    let mut per_language = Vec::new();
    for (language, items) in by_language {
        per_language.push(FixtureLanguageSummary {
            language,
            fixture_count: items.len(),
            avg_cer: avg(items.iter().map(|x| x.cer)),
            avg_wer: avg(items.iter().map(|x| x.wer)),
            avg_strict_cer: avg(items.iter().map(|x| x.strict_cer)),
            avg_similarity: avg(items.iter().map(|x| x.similarity)),
            pass_rate: avg(items
                .iter()
                .map(|x| if x.meets_threshold { 1.0 } else { 0.0 })),
        });
    }

    FixtureModeSummary {
        mode: mode.name().to_string(),
        fixture_count,
        avg_cer,
        avg_wer,
        avg_strict_cer,
        avg_similarity,
        pass_rate,
        avg_infer_ms,
        tesseract_replacement_count,
        per_language,
        fixtures,
    }
}

pub(super) fn avg<I>(iter: I) -> f32
where
    I: Iterator<Item = f32>,
{
    let mut sum = 0.0f32;
    let mut count = 0usize;
    for value in iter {
        sum += value;
        count += 1;
    }
    if count == 0 {
        0.0
    } else {
        sum / count as f32
    }
}

#[derive(Debug)]
struct FixtureTextMetrics {
    cer: f32,
    wer: f32,
    strict_cer: f32,
    similarity: f32,
    meets_threshold: bool,
}

fn fixture_text_metrics(expected: &str, actual: &str, min_similarity: f32) -> FixtureTextMetrics {
    let cer = char_error_rate_eval(
        &normalize_text_for_char_similarity(expected),
        &normalize_text_for_char_similarity(actual),
    );
    let wer = word_error_rate_eval(
        &normalize_text_for_word_similarity(expected),
        &normalize_text_for_word_similarity(actual),
    );
    let strict_cer = char_error_rate_eval(expected, actual);
    let legacy_similarity = (1.0 - cer).max(1.0 - wer).clamp(0.0, 1.0);
    let mut similarity = legacy_similarity.min((1.0 - strict_cer).clamp(0.0, 1.0));
    // A nonzero error can round away for very long strings. Keep 1.0 exact.
    if expected != actual && similarity == 1.0 {
        similarity = f32::from_bits(1.0f32.to_bits() - 1);
    }
    FixtureTextMetrics {
        cer,
        wer,
        strict_cer,
        similarity,
        meets_threshold: similarity >= min_similarity,
    }
}

pub(super) fn normalize_text_for_word_similarity(input: &str) -> String {
    input
        .to_uppercase()
        .split_whitespace()
        .collect::<Vec<_>>()
        .join(" ")
}

pub(super) fn normalize_text_for_char_similarity(input: &str) -> String {
    input
        .to_uppercase()
        .chars()
        .filter(|c| !c.is_whitespace())
        .collect()
}

pub(super) fn word_error_rate_eval(expected: &str, actual: &str) -> f32 {
    let expected_words: Vec<&str> = expected.split_whitespace().collect();
    let actual_words: Vec<&str> = actual.split_whitespace().collect();
    edit_error_rate(&expected_words, &actual_words)
}

pub(super) fn char_error_rate_eval(expected: &str, actual: &str) -> f32 {
    let expected_chars: Vec<char> = expected.chars().collect();
    let actual_chars: Vec<char> = actual.chars().collect();
    edit_error_rate(&expected_chars, &actual_chars)
}

/// Levenshtein distance divided by the reference length, using two rolling
/// rows. An empty reference scores 0 for empty output and 1 otherwise.
fn edit_error_rate<T: PartialEq>(expected: &[T], actual: &[T]) -> f32 {
    if expected.is_empty() {
        return if actual.is_empty() { 0.0 } else { 1.0 };
    }

    let mut previous: Vec<usize> = (0..=actual.len()).collect();
    let mut current = vec![0; actual.len() + 1];
    for (i, expected_item) in expected.iter().enumerate() {
        current[0] = i + 1;
        for (j, actual_item) in actual.iter().enumerate() {
            let cost = usize::from(expected_item != actual_item);
            current[j + 1] = (previous[j + 1] + 1)
                .min(current[j] + 1)
                .min(previous[j] + cost);
        }
        std::mem::swap(&mut previous, &mut current);
    }
    previous[actual.len()] as f32 / expected.len() as f32
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_close(actual: f32, expected: f32) {
        assert!((actual - expected).abs() < 1e-6, "{actual} != {expected}");
    }

    #[test]
    fn strict_metrics_distinguish_spacing_case_punctuation_and_line_breaks() {
        let cases = [
            ("Hello world", "Helloworld", 0.0, 1.0, 1.0 / 11.0),
            ("Hello world", "hello world", 0.0, 0.0, 1.0 / 11.0),
            ("Hello, world!", "Hello world!", 1.0 / 12.0, 0.5, 1.0 / 13.0),
            ("Hello\nworld", "Hello world", 0.0, 0.0, 1.0 / 11.0),
            ("Hello  world", "Hello world", 0.0, 0.0, 1.0 / 12.0),
        ];
        for (expected, actual, cer, wer, strict_cer) in cases {
            let metrics = fixture_text_metrics(expected, actual, 1.0);
            assert_close(metrics.cer, cer);
            assert_close(metrics.wer, wer);
            assert_close(metrics.strict_cer, strict_cer);
            assert_close(
                metrics.similarity,
                (1.0 - cer).max(1.0 - wer).min(1.0 - strict_cer),
            );
            assert!(!metrics.meets_threshold, "{expected:?} vs {actual:?}");
        }
    }

    #[test]
    fn strict_metrics_handle_long_whitespace_without_a_quadratic_matrix() {
        let expected = " ".repeat(8_000);
        let actual = " ".repeat(7_999);
        assert_close(char_error_rate_eval(&expected, &actual), 1.0 / 8_000.0);
    }

    #[test]
    fn exact_match_and_threshold_boundaries() {
        for text in ["", "Hello,\nworld!", "你好 世界"] {
            let metrics = fixture_text_metrics(text, text, 1.0);
            assert_close(metrics.cer, 0.0);
            assert_close(metrics.wer, 0.0);
            assert_close(metrics.strict_cer, 0.0);
            assert_eq!(metrics.similarity, 1.0);
            assert!(metrics.meets_threshold);
        }
        let score = fixture_text_metrics("Hello world", "Helloworld", 0.0).similarity;
        assert!(fixture_text_metrics("Hello world", "Helloworld", score).meets_threshold);
        assert!(!fixture_text_metrics("Hello world", "Helloworld", score + 0.001).meets_threshold);
    }

    #[test]
    fn legacy_similarity_remains_a_cap_when_strict_cer_is_more_lenient() {
        let metrics = fixture_text_metrics("Hello, world!", "Hello world!", 0.0);
        assert_close(metrics.similarity, 1.0 - metrics.cer);
        assert!(metrics.similarity < 1.0 - metrics.strict_cer);
    }

    #[test]
    fn empty_reference_and_large_insertion_error_are_bounded() {
        for (expected, actual, strict_cer) in [("", " ", 1.0), ("a", "abcd", 3.0)] {
            let metrics = fixture_text_metrics(expected, actual, 1.0);
            assert_close(metrics.strict_cer, strict_cer);
            assert_eq!(metrics.similarity, 0.0);
            assert!(!metrics.meets_threshold);
        }
    }

    #[test]
    fn summaries_and_reports_expose_strict_metric() {
        let metrics = fixture_text_metrics("Hello world", "Helloworld", 1.0);
        let result = FixtureResult {
            name: "example".into(),
            language: "eng".into(),
            expected_text: "Hello world".into(),
            actual_text: "Helloworld".into(),
            infer_ms: 1,
            cer: metrics.cer,
            wer: metrics.wer,
            strict_cer: metrics.strict_cer,
            similarity: metrics.similarity,
            min_similarity: 1.0,
            meets_threshold: metrics.meets_threshold,
            used_tesseract: false,
            ppocr_quality: 0.0,
            ppocr_confidence: 0.0,
            tesseract_quality: None,
        };
        let summary = summarize_mode(FixtureEvalMode::PureOnnx, vec![result]);
        assert_close(summary.avg_strict_cer, 1.0 / 11.0);
        assert_close(summary.per_language[0].avg_strict_cer, 1.0 / 11.0);
        assert_eq!(summary.pass_rate, 0.0);
        let report = OcrFixtureEvalReport {
            fixture_dir: "fixtures".into(),
            engine: "test".into(),
            modes: vec![summary],
        };
        let json = serde_json::to_value(&report).unwrap();
        assert!(json["modes"][0]["fixtures"][0]["strict_cer"].is_number());
        assert!(json["modes"][0]["avg_strict_cer"].is_number());
        assert!(json["modes"][0]["per_language"][0]["avg_strict_cer"].is_number());
        let markdown = render_ocr_fixture_report_markdown(&report);
        assert!(markdown.contains("Avg Strict CER"));
        assert!(markdown.contains("including case, punctuation, and whitespace"));
    }
}
