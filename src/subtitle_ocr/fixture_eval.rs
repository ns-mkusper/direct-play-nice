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
    pub avg_similarity: f32,
    pub pass_rate: f32,
}

#[derive(Debug, Clone, serde::Serialize)]
pub(crate) struct FixtureModeSummary {
    pub mode: String,
    pub fixture_count: usize,
    pub avg_cer: f32,
    pub avg_wer: f32,
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
    lines.push("## Mode Summary".to_string());
    lines.push(String::new());
    lines.push(
        "| Mode | Fixtures | Avg CER | Avg WER | Avg Similarity | Pass Rate | Avg Infer ms | Tesseract Replacements |"
            .to_string(),
    );
    lines.push("| --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: |".to_string());
    for mode in &report.modes {
        lines.push(format!(
            "| {} | {} | {:.4} | {:.4} | {:.4} | {:.2}% | {:.2} | {} |",
            mode.mode,
            mode.fixture_count,
            mode.avg_cer,
            mode.avg_wer,
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
            "| Language | Fixtures | Avg CER | Avg WER | Avg Similarity | Pass Rate |".to_string(),
        );
        lines.push("| --- | ---: | ---: | ---: | ---: | ---: |".to_string());
        for lang in &mode.per_language {
            lines.push(format!(
                "| {} | {} | {:.4} | {:.4} | {:.4} | {:.2}% |",
                lang.language,
                lang.fixture_count,
                lang.avg_cer,
                lang.avg_wer,
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

    let expected_word = normalize_text_for_word_similarity(&spec.expected_text);
    let actual_word = normalize_text_for_word_similarity(&actual_text);
    let wer = word_error_rate_eval(&expected_word, &actual_word);

    let expected_char = normalize_text_for_char_similarity(&spec.expected_text);
    let actual_char = normalize_text_for_char_similarity(&actual_text);
    let cer = char_error_rate_eval(&expected_char, &actual_char);
    let similarity = (1.0 - cer).max(1.0 - wer).clamp(0.0, 1.0);
    let meets_threshold = similarity >= spec.min_similarity;

    Ok(FixtureResult {
        name: spec.name.clone(),
        language: spec.language.clone(),
        expected_text: spec.expected_text.clone(),
        actual_text,
        infer_ms,
        cer,
        wer,
        similarity,
        min_similarity: spec.min_similarity,
        meets_threshold,
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
    if expected_words.is_empty() {
        return if actual_words.is_empty() { 0.0 } else { 1.0 };
    }

    let m = expected_words.len();
    let n = actual_words.len();
    let mut dp = vec![vec![0usize; n + 1]; m + 1];
    for (i, row) in dp.iter_mut().enumerate().take(m + 1) {
        row[0] = i;
    }
    for (j, cell) in dp[0].iter_mut().enumerate().take(n + 1) {
        *cell = j;
    }
    for i in 1..=m {
        for j in 1..=n {
            let cost = if expected_words[i - 1] == actual_words[j - 1] {
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
    dp[m][n] as f32 / expected_words.len() as f32
}

pub(super) fn char_error_rate_eval(expected: &str, actual: &str) -> f32 {
    let expected_chars: Vec<char> = expected.chars().collect();
    let actual_chars: Vec<char> = actual.chars().collect();
    if expected_chars.is_empty() {
        return if actual_chars.is_empty() { 0.0 } else { 1.0 };
    }

    let m = expected_chars.len();
    let n = actual_chars.len();
    let mut dp = vec![vec![0usize; n + 1]; m + 1];
    for (i, row) in dp.iter_mut().enumerate().take(m + 1) {
        row[0] = i;
    }
    for (j, cell) in dp[0].iter_mut().enumerate().take(n + 1) {
        *cell = j;
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
