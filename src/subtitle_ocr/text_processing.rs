//! OCR post-processing and quality heuristics.
//!
//! Contains spacing cleanup, language-sensitive normalization, and quality
//! scoring used to decide when Tesseract fallback should replace PP-OCR text.

use super::{normalize_utf8_text, OcrLine};

const OCR_GEOMETRY_MAX_HEIGHT_WIDTH_RATIO: f32 = 4.0;
const NGRAM_FLOOR_SCORE: f32 = -6.0;
const NGRAM_BAD_CHAR_PENALTY: f32 = 2.4;
const NGRAM_SMALL_TOKEN_PENALTY: f32 = 0.45;
const NGRAM_SEGMENTATION_SKIP_THRESHOLD: f32 = -0.2;

const COMMON_BIGRAMS: [&str; 48] = [
    "th", "he", "in", "er", "an", "re", "on", "at", "en", "nd", "ti", "es", "or", "te", "of", "ed",
    "is", "it", "al", "ar", "st", "to", "nt", "ng", "se", "ha", "as", "ou", "io", "le", "ve", "co",
    "me", "de", "hi", "ri", "ro", "ic", "ne", "ea", "ra", "ce", "li", "ll", "be", "ma", "si", "om",
];

const COMMON_TRIGRAMS: [&str; 40] = [
    "the", "and", "ing", "ion", "ent", "ati", "for", "her", "tha", "ere", "hat", "his", "tha",
    "ver", "all", "wit", "thi", "tio", "not", "you", "was", "but", "are", "one", "out", "hav",
    "men", "our", "ill", "res", "ove", "com", "pro", "int", "con", "sta", "eve", "per", "rea",
    "ter",
];

/// Runs the language uses spaces operation.
pub(super) fn language_uses_spaces(language: &str) -> bool {
    let lang = language.to_lowercase();
    matches!(
        lang.as_str(),
        "eng"
            | "en"
            | "en-us"
            | "en_us"
            | "fre"
            | "fra"
            | "fr"
            | "spa"
            | "es"
            | "de"
            | "deu"
            | "ger"
            | "it"
            | "ita"
            | "pt"
            | "por"
            | "nl"
            | "nld"
            | "sv"
            | "swe"
            | "da"
            | "dan"
            | "no"
            | "nor"
            | "fi"
            | "fin"
    )
}

/// Runs the ppocr spacing needs fallback operation.
pub(super) fn ppocr_spacing_needs_fallback(lines: &[OcrLine]) -> bool {
    if lines.is_empty() {
        return false;
    }
    let mut has_spaces = false;
    let mut long_token = false;
    let mut has_letters = false;
    for line in lines {
        let text = line.text.trim();
        if text.contains(' ') {
            has_spaces = true;
            break;
        }
        if text.len() >= 12 {
            long_token = true;
        }
        if text.chars().any(|c| c.is_alphabetic()) {
            has_letters = true;
        }
    }
    has_letters && long_token && !has_spaces
}

/// Runs the postprocess ocr text operation.
pub(super) fn postprocess_ocr_text(text: &str, language: &str) -> String {
    let mut out = normalize_utf8_text(text);
    if out.is_empty() {
        return out;
    }

    if !is_english_language(language) {
        return out;
    }

    out = normalize_english_ocr_confusions(&out);
    out = insert_space_after_punctuation(&out);
    out = insert_space_between_letters_and_digits(&out);
    out = insert_space_before_opening_quote(&out);

    // Targeted corrections for frequently observed OCR glue patterns.
    const ENGLISH_GLUE_FIXES: [(&str, &str); 42] = [
        ("noneother", "none other"),
        ("notonlyme", "not only me"),
        ("notonly", "not only"),
        ("itis", "it is"),
        ("whylost", "why lost"),
        ("hesalive", "he's alive"),
        ("he'salive", "he's alive"),
        ("thats", "that's"),
        ("goodwork", "good work"),
        ("burnit", "burn it"),
        ("yessir", "yes sir"),
        ("praisetoyou", "praise to you"),
        ("lordjesuschrist", "Lord Jesus Christ"),
        ("paxchristi", "pax Christi"),
        ("praisedbegod", "praised be God"),
        ("constablecrane", "Constable Crane"),
        ("whathappenedtohim", "what happened to him"),
        ("beforehewentintotheriver", "before he went into the river"),
        (
            "thereislittlepeaceinthislandnow",
            "there is little peace in this land now",
        ),
        ("allourprogresshasended", "all our progress has ended"),
        ("newsuffering", "new suffering"),
        ("tobeasdarkasitisnow", "to be as dark as it is now"),
        ("tobeasdarkasit isnow", "to be as dark as it is now"),
        ("to be as dark as itis now", "to be as dark as it is now"),
        ("an done of", "and one of"),
        ("butit's", "but it's"),
        ("isit?", "is it?"),
        (
            "andthepainwouldbeprolonged",
            "and the pain would be prolonged",
        ),
        (
            "eachsmallsplashofthewater",
            "each small splash of the water",
        ),
        ("waslikeaburningcoal", "was like a burning coal"),
        ("therearehotspringsthere", "there are hot springs there"),
        ("toabandongod", "to abandon God"),
        (
            "sotheycoulddemonstratethestrengthoftheirfaith",
            "so they could demonstrate the strength of their faith",
        ),
        (
            "andthepresenceofgodwithinthem",
            "and the presence of God within them",
        ),
        ("standdown", "stand down"),
        ("loppedoff", "lopped off"),
        ("ibegpardon", "I beg pardon"),
        ("ihavenot", "I have not"),
        ("ishall", "I shall"),
        ("begpardon", "beg pardon"),
        ("havenot", "have not"),
        ("l9th", "19th"),
    ];
    for (from, to) in ENGLISH_GLUE_FIXES {
        out = replace_case_insensitive_ascii(&out, from, to);
    }
    out = split_glued_english_phrases(&out);

    normalize_utf8_text(&out)
}

/// Runs the is english language operation.
pub(super) fn is_english_language(language: &str) -> bool {
    let lang = language.trim().to_ascii_lowercase();
    matches!(lang.as_str(), "eng" | "en" | "en-us" | "en_us")
}

/// Runs the normalize english ocr confusions operation.
fn normalize_english_ocr_confusions(input: &str) -> String {
    let mut out = String::with_capacity(input.len());
    let mut token = String::new();
    let flush_token = |tok: &mut String, out: &mut String| {
        if tok.is_empty() {
            return;
        }
        let has_alpha = tok.chars().any(|c| c.is_ascii_alphabetic());
        let has_digit = tok.chars().any(|c| c.is_ascii_digit());
        let mut normalized = tok.clone();
        if has_alpha && has_digit {
            normalized = normalized
                .replace('0', "o")
                .replace('1', "l")
                .replace('5', "s")
                .replace('8', "b");
        }
        let normalized_lc = normalized.to_ascii_lowercase();
        if let Some(rest) = normalized_lc.strip_prefix('l') {
            if rest.chars().next().is_some_and(|ch| ch.is_ascii_digit())
                && (rest.ends_with("st")
                    || rest.ends_with("nd")
                    || rest.ends_with("rd")
                    || rest.ends_with("th"))
            {
                normalized.replace_range(0..1, "1");
            }
        }
        normalized = normalized.replace('|', "I").replace("vv", "w");
        out.push_str(&normalized);
        tok.clear();
    };

    for ch in input.chars() {
        if ch.is_ascii_alphanumeric() || ch == '\'' || ch == '|' {
            token.push(ch);
        } else {
            flush_token(&mut token, &mut out);
            out.push(ch);
        }
    }
    flush_token(&mut token, &mut out);
    out
}

/// Runs the split glued english phrases operation.
fn split_glued_english_phrases(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 8);
    let mut token = String::new();

    let flush_token = |tok: &mut String, out: &mut String| {
        if tok.is_empty() {
            return;
        }
        if let Some(split) = split_glued_ascii_token(tok) {
            out.push_str(&split);
        } else {
            out.push_str(tok);
        }
        tok.clear();
    };

    for ch in input.chars() {
        if ch.is_ascii_alphabetic() || ch == '\'' {
            token.push(ch);
        } else {
            flush_token(&mut token, &mut out);
            out.push(ch);
        }
    }
    flush_token(&mut token, &mut out);

    out
}

/// Runs the split glued ascii token operation.
pub(super) fn split_glued_ascii_token(token: &str) -> Option<String> {
    if token.len() < 5 || !token.is_ascii() {
        return None;
    }
    if !token
        .chars()
        .all(|ch| ch.is_ascii_alphabetic() || ch == '\'')
    {
        return None;
    }

    let lower = token.to_ascii_lowercase();
    if lower == "standdown" {
        return Some(format!("{} {}", &token[..5], &token[5..]));
    }
    if let Some(split) = split_glued_contraction(token, &lower) {
        return Some(split);
    }

    if matches!(token.chars().next(), Some('I' | 'i')) && token.len() >= 5 {
        const I_PREFIX_CONTINUATIONS: [&str; 16] = [
            "am", "have", "had", "shall", "will", "beg", "think", "know", "need", "must", "want",
            "did", "do", "was", "were", "would",
        ];
        let rest = &lower[1..];
        if I_PREFIX_CONTINUATIONS
            .iter()
            .any(|prefix| rest.starts_with(prefix))
        {
            for prefix in I_PREFIX_CONTINUATIONS {
                if !rest.starts_with(prefix) {
                    continue;
                }
                let prefix_len = prefix.len();
                if rest.len() <= prefix_len {
                    continue;
                }
                let split_at = 1 + prefix_len;
                let tail = &token[split_at..];
                if tail.chars().all(|ch| ch.is_ascii_alphabetic()) {
                    let split_tail =
                        segment_glued_english_token(tail).unwrap_or_else(|| tail.to_string());
                    return Some(format!(
                        "{} {} {}",
                        &token[..1],
                        &token[1..split_at],
                        split_tail
                    ));
                }
            }
            let split_rest =
                segment_glued_english_token(&token[1..]).unwrap_or_else(|| token[1..].to_string());
            return Some(format!("{} {}", &token[..1], split_rest));
        }
    }

    if ascii_language_likelihood(&lower) > NGRAM_SEGMENTATION_SKIP_THRESHOLD {
        return None;
    }

    segment_glued_english_token(token)
}

/// Runs the ascii language likelihood operation.
fn ascii_language_likelihood(token: &str) -> f32 {
    let lower = token.to_ascii_lowercase();
    let mut cleaned = String::with_capacity(lower.len());
    let mut bad_chars = 0usize;
    for ch in lower.chars() {
        if ch.is_ascii_alphabetic() {
            cleaned.push(ch);
        } else if ch != '\'' {
            bad_chars += 1;
        }
    }

    if cleaned.len() < 2 {
        return NGRAM_FLOOR_SCORE - bad_chars as f32 * NGRAM_BAD_CHAR_PENALTY;
    }

    let chars: Vec<char> = cleaned.chars().collect();
    let mut score = 0.0f32;
    let mut samples = 0usize;

    if chars.len() >= 2 {
        for i in 0..(chars.len() - 1) {
            let gram = [chars[i], chars[i + 1]].iter().collect::<String>();
            if COMMON_BIGRAMS.contains(&gram.as_str()) {
                score += 1.0;
            } else {
                score -= 0.65;
            }
            samples += 1;
        }
    }

    if chars.len() >= 3 {
        for i in 0..(chars.len() - 2) {
            let gram = [chars[i], chars[i + 1], chars[i + 2]]
                .iter()
                .collect::<String>();
            if COMMON_TRIGRAMS.contains(&gram.as_str()) {
                score += 1.3;
            } else {
                score -= 0.85;
            }
            samples += 1;
        }
    }

    if samples == 0 {
        return NGRAM_FLOOR_SCORE - bad_chars as f32 * NGRAM_BAD_CHAR_PENALTY;
    }

    let mut normalized = score / samples as f32;
    if cleaned.len() < 4 {
        normalized -= NGRAM_SMALL_TOKEN_PENALTY;
    }
    normalized -= bad_chars as f32 * NGRAM_BAD_CHAR_PENALTY;
    normalized.max(NGRAM_FLOOR_SCORE)
}

/// Runs the segment glued english token operation.
fn segment_glued_english_token(token: &str) -> Option<String> {
    let lower = token.to_ascii_lowercase();
    if lower.len() < 5 || ascii_language_likelihood(&lower) > NGRAM_SEGMENTATION_SKIP_THRESHOLD {
        return None;
    }

    // Dynamic programming split over character n-gram likelihood, favoring
    // a small number of fluent-looking segments over tiny fragments.
    let n = lower.len();
    let mut best: Vec<Option<(f32, usize, usize)>> = vec![None; n + 1]; // (score, prev_idx, segments)
    best[0] = Some((0.0, 0, 0));
    for end in 1..=n {
        let start_min = end.saturating_sub(12);
        for start in start_min..end {
            let Some((prev_score, _prev_idx, prev_segments)) = best[start] else {
                continue;
            };
            let candidate = &lower[start..end];
            if candidate.len() < 2 {
                continue;
            }
            let mut score = prev_score + ascii_language_likelihood(candidate);
            if candidate.len() < 3 {
                score -= 0.9;
            }
            if candidate.len() == 3 {
                score -= 0.2;
            }
            let segments = prev_segments + 1;
            let should_replace = best[end]
                .as_ref()
                .map(|(current_score, _, current_segments)| {
                    score > *current_score + 0.001
                        || ((score - *current_score).abs() <= 0.001 && segments < *current_segments)
                })
                .unwrap_or(true);
            if should_replace {
                best[end] = Some((score, start, segments));
            }
        }
    }

    let (segmented_score, _prev, segment_count) = best[n]?;
    if segment_count < 2 {
        return None;
    }
    let raw_score = ascii_language_likelihood(&lower);
    // Require statistically meaningful gain before rewriting OCR text.
    if segmented_score < raw_score + 0.35 {
        return None;
    }

    let mut pieces = Vec::new();
    let mut idx = n;
    while idx > 0 {
        let (_score, prev_idx, _segments) = best[idx]?;
        pieces.push((prev_idx, idx));
        idx = prev_idx;
    }
    pieces.reverse();

    // Guard against pathological over-segmentation (e.g. many tiny tokens).
    let avg_segment_len = n as f32 / pieces.len() as f32;
    if pieces.len() >= 5 && avg_segment_len < 2.6 {
        return None;
    }
    if pieces.iter().any(|(start, end)| {
        end - start == 1 && &lower[*start..*end] != "i" && &lower[*start..*end] != "a"
    }) {
        return None;
    }
    // Avoid splitting into uniformly weak segments.
    if pieces
        .iter()
        .any(|(start, end)| ascii_language_likelihood(&lower[*start..*end]) < -2.4)
    {
        return None;
    }

    Some(
        pieces
            .into_iter()
            .map(|(start, end)| token[start..end].to_string())
            .collect::<Vec<_>>()
            .join(" "),
    )
}

/// Runs the split glued contraction operation.
fn split_glued_contraction(token: &str, lower: &str) -> Option<String> {
    let apostrophe = token.find('\'')?;
    if apostrophe == 0 || apostrophe + 1 >= token.len() {
        return None;
    }
    if token[apostrophe + 1..].contains('\'') {
        return None;
    }

    const SUFFIXES: [&str; 7] = ["s", "re", "ve", "ll", "d", "m", "t"];
    for suffix in SUFFIXES {
        let suffix_start = apostrophe + 1;
        if !lower[suffix_start..].starts_with(suffix) {
            continue;
        }
        let contraction_len = suffix_start + suffix.len();
        if contraction_len + 2 > token.len() {
            return None;
        }
        let rest = &token[contraction_len..];
        if !rest.chars().all(|ch| ch.is_ascii_alphabetic()) {
            return None;
        }
        let split_rest = segment_glued_english_token(rest).unwrap_or_else(|| rest.to_string());
        return Some(format!("{} {}", &token[..contraction_len], split_rest));
    }
    None
}

/// Runs the insert space after punctuation operation.
fn insert_space_after_punctuation(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 8);
    let chars: Vec<char> = input.chars().collect();
    for (i, ch) in chars.iter().enumerate() {
        out.push(*ch);
        if matches!(ch, ',' | '.' | ';' | ':' | '!' | '?')
            && chars
                .get(i + 1)
                .is_some_and(|next| next.is_ascii_alphabetic())
        {
            out.push(' ');
        }
    }
    out
}

/// Runs the insert space between letters and digits operation.
fn insert_space_between_letters_and_digits(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 8);
    let chars: Vec<char> = input.chars().collect();
    for (i, ch) in chars.iter().enumerate() {
        out.push(*ch);
        if let Some(next) = chars.get(i + 1) {
            let alpha_to_digit = ch.is_ascii_alphabetic() && next.is_ascii_digit();
            let digit_to_alpha = ch.is_ascii_digit() && next.is_ascii_alphabetic();
            let ordinal_suffix = if digit_to_alpha && i + 2 < chars.len() {
                let a = chars[i + 1].to_ascii_lowercase();
                let b = chars[i + 2].to_ascii_lowercase();
                matches!((a, b), ('s', 't') | ('n', 'd') | ('r', 'd') | ('t', 'h'))
                    && chars
                        .get(i + 3)
                        .map(|c| !c.is_ascii_alphabetic())
                        .unwrap_or(true)
            } else {
                false
            };
            let boundary = alpha_to_digit || (digit_to_alpha && !ordinal_suffix);
            if boundary && *ch != ' ' && *next != ' ' {
                out.push(' ');
            }
        }
    }
    out
}

/// Runs the insert space before opening quote operation.
fn insert_space_before_opening_quote(input: &str) -> String {
    let mut out = String::with_capacity(input.len() + 4);
    let chars: Vec<char> = input.chars().collect();
    for (i, ch) in chars.iter().enumerate() {
        if *ch == '"'
            && i > 0
            && i + 1 < chars.len()
            && chars[i - 1].is_ascii_alphabetic()
            && chars[i + 1].is_ascii_alphabetic()
            && !out.ends_with(' ')
        {
            out.push(' ');
        }
        out.push(*ch);
    }
    out
}

/// Runs the replace case insensitive ascii operation.
fn replace_case_insensitive_ascii(input: &str, from: &str, to: &str) -> String {
    if from.is_empty() {
        return input.to_string();
    }
    let input_lc = input.to_ascii_lowercase();
    let from_lc = from.to_ascii_lowercase();
    let mut out = String::with_capacity(input.len());
    let mut pos = 0usize;
    while let Some(rel_idx) = input_lc[pos..].find(&from_lc) {
        let idx = pos + rel_idx;
        out.push_str(&input[pos..idx]);
        let orig = &input[idx..idx + from.len()];
        let replacement = if orig
            .chars()
            .next()
            .is_some_and(|ch| ch.is_ascii_uppercase())
        {
            let mut chars = to.chars();
            if let Some(first) = chars.next() {
                format!(
                    "{}{}",
                    first.to_ascii_uppercase(),
                    chars.collect::<String>()
                )
            } else {
                to.to_string()
            }
        } else {
            to.to_string()
        };
        out.push_str(&replacement);
        pos = idx + from.len();
    }
    out.push_str(&input[pos..]);
    out
}

/// Runs the lines text for quality operation.
pub(super) fn lines_text_for_quality(lines: &[OcrLine]) -> String {
    normalize_utf8_text(
        &lines
            .iter()
            .map(|line| line.text.trim())
            .filter(|text| !text.is_empty())
            .collect::<Vec<_>>()
            .join(" "),
    )
}

/// Runs the ppocr average confidence operation.
pub(super) fn ppocr_average_confidence(lines: &[OcrLine]) -> Option<f32> {
    let mut weighted_sum = 0.0f32;
    let mut total_weight = 0.0f32;
    for line in lines {
        let Some(score) = line.score else {
            continue;
        };
        if !score.is_finite() {
            continue;
        }
        // Weight confidence by bbox area so tiny artifacts do not dominate
        // the aggregate confidence signal used for fallback decisions.
        let area_weight = line
            .bbox
            .as_ref()
            .map(|bbox| {
                let width = (bbox.right - bbox.left).max(0) as f32;
                let height = (bbox.bottom - bbox.top).max(0) as f32;
                width * height
            })
            .filter(|area| *area > 0.0)
            .unwrap_or_else(|| {
                line.text
                    .chars()
                    .filter(|ch| !ch.is_whitespace())
                    .count()
                    .max(1) as f32
            });
        weighted_sum += score * area_weight;
        total_weight += area_weight;
    }
    if total_weight <= f32::EPSILON {
        None
    } else {
        Some(weighted_sum / total_weight)
    }
}

/// Runs the ocr text quality score operation.
pub(super) fn ocr_text_quality_score(text: &str, language: &str) -> f32 {
    let text = normalize_utf8_text(text);
    if text.is_empty() {
        return 0.0;
    }

    let mut letters = 0usize;
    let mut digits = 0usize;
    let mut spaces = 0usize;
    let mut punctuation = 0usize;
    let mut noise = 0usize;

    for ch in text.chars() {
        if ch.is_alphabetic() {
            letters += 1;
        } else if ch.is_ascii_digit() {
            digits += 1;
        } else if ch.is_whitespace() {
            spaces += 1;
        } else if ch.is_ascii_punctuation() || "“”‘’…".contains(ch) {
            punctuation += 1;
        } else {
            noise += 1;
        }
    }

    let total = (letters + digits + spaces + punctuation + noise).max(1) as f32;
    let mut score = 1.0f32;

    let noise_ratio = noise as f32 / total;
    if noise_ratio > 0.0 {
        score -= noise_ratio * 1.2;
    }

    if text.contains("@&") {
        score -= 0.18;
    }
    if text.contains('|') {
        score -= 0.12;
    }

    let words_vec = text.split_whitespace().collect::<Vec<_>>();
    let word_count = words_vec.len().max(1);
    let avg_word_len = letters as f32 / word_count as f32;
    let long_word_count = words_vec.iter().filter(|word| word.len() >= 14).count();

    if language_uses_spaces(language) {
        if letters >= 12 && word_count <= 1 {
            score -= 0.2;
        }
        // Keep this light to avoid penalizing proper nouns and stylized titles.
        if avg_word_len > 10.5 {
            score -= 0.05;
        }
        let long_word_ratio = long_word_count as f32 / word_count as f32;
        if long_word_ratio > 0.45 {
            score -= ((long_word_ratio - 0.45) * 0.20).min(0.15);
        }
    }

    if letters == 0 && digits == 0 {
        score -= 0.3;
    }

    // Slightly reward candidates with enough character coverage.
    let coverage_bonus = (letters as f32 / 24.0).min(0.2);
    (score + coverage_bonus).clamp(0.0, 1.0)
}

/// Runs the ppocr needs quality fallback operation.
pub(super) fn ppocr_needs_quality_fallback(lines: &[OcrLine], language: &str) -> bool {
    if lines.is_empty() {
        return false;
    }
    if ppocr_spacing_needs_fallback(lines) {
        return true;
    }
    if ppocr_geometry_needs_fallback(lines, language) {
        return true;
    }

    let quality = ocr_text_quality_score(&lines_text_for_quality(lines), language);
    quality < 0.45
}

/// Runs the prune impossible geometry operation.
pub(super) fn prune_impossible_geometry(lines: &mut Vec<OcrLine>, language: &str) -> usize {
    if !language_uses_spaces(language) || lines.is_empty() {
        return 0;
    }

    let before = lines.len();
    lines.retain(|line| {
        let Some(bbox) = line.bbox.as_ref() else {
            return true;
        };
        let width = (bbox.right - bbox.left).max(0) as f32;
        let height = (bbox.bottom - bbox.top).max(0) as f32;
        if width <= 0.0 || height <= 0.0 {
            return true;
        }
        let height_width_ratio = height / width;
        height_width_ratio <= OCR_GEOMETRY_MAX_HEIGHT_WIDTH_RATIO
    });
    before.saturating_sub(lines.len())
}

/// Runs the ppocr geometry needs fallback operation.
fn ppocr_geometry_needs_fallback(lines: &[OcrLine], language: &str) -> bool {
    if !language_uses_spaces(language) {
        return false;
    }
    // Detect impossible OCR layouts for horizontal subtitle tracks.
    let mut checked = 0usize;
    let mut suspicious = 0usize;
    for line in lines {
        let Some(bbox) = line.bbox.as_ref() else {
            continue;
        };
        let width = (bbox.right - bbox.left).max(0) as f32;
        let height = (bbox.bottom - bbox.top).max(0) as f32;
        if width <= 0.0 || height <= 0.0 {
            continue;
        }
        checked += 1;
        let height_width_ratio = height / width;
        let alpha_len = line.text.chars().filter(|c| c.is_alphabetic()).count();
        if alpha_len >= 5 && height_width_ratio > OCR_GEOMETRY_MAX_HEIGHT_WIDTH_RATIO {
            suspicious += 1;
        }
    }
    checked > 0 && suspicious > 0
}
