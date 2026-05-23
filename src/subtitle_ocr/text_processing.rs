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

pub(super) fn ppocr_spacing_needs_fallback(lines: &[OcrLine]) -> bool {
    if lines.is_empty() {
        return false;
    }
    let mut has_letters = false;
    let mut has_any_space = false;
    let mut long_alpha_tokens = 0usize;
    let mut very_long_alpha_token = false;

    for line in lines {
        let text = line.text.trim();
        has_any_space |= text.contains(' ');
        has_letters |= text.chars().any(|c| c.is_alphabetic());
        for token in text
            .split_whitespace()
            .flat_map(|part| part.split(|ch: char| !ch.is_alphabetic() && ch != '\''))
        {
            let alpha_len = token.chars().filter(|ch| ch.is_alphabetic()).count();
            if alpha_len >= 14 {
                long_alpha_tokens += 1;
            }
            if alpha_len >= 20 {
                very_long_alpha_token = true;
            }
        }
        // A no-space line with many letters is the classic PP-OCR subtitle glue failure.
        if !text.contains(' ') && text.chars().filter(|c| c.is_alphabetic()).count() >= 12 {
            long_alpha_tokens += 1;
        }
    }

    has_letters
        && (!has_any_space && long_alpha_tokens > 0
            || very_long_alpha_token
            || long_alpha_tokens >= 2)
}

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

    out = split_glued_english_phrases(&out);

    normalize_utf8_text(&out)
}

pub(super) fn is_english_language(language: &str) -> bool {
    let lang = language.trim().to_ascii_lowercase();
    matches!(lang.as_str(), "eng" | "en" | "en-us" | "en_us")
}

fn normalize_mixed_case_ocr_token(token: &str) -> String {
    let chars: Vec<char> = token.chars().collect();
    if chars.len() < 3 || !chars.iter().any(|ch| ch.is_ascii_lowercase()) {
        return token.to_string();
    }
    let mut out = String::with_capacity(token.len());
    for (idx, ch) in chars.iter().copied().enumerate() {
        let prev = idx.checked_sub(1).and_then(|i| chars.get(i)).copied();
        let next = chars.get(idx + 1).copied();
        let surrounded_by_lower = prev.is_some_and(|c| c.is_ascii_lowercase())
            && next.is_some_and(|c| c.is_ascii_lowercase());
        let after_lower = prev.is_some_and(|c| c.is_ascii_lowercase());
        let replacement = match ch {
            // Common OCR confusions from subtitle crops: uppercase I appears
            // inside lower-case words where the recognizer meant i/l.
            'I' if surrounded_by_lower => {
                if matches!(prev, Some('u' | 'o')) && matches!(next, Some('d')) {
                    'l'
                } else {
                    'i'
                }
            }
            'I' if after_lower && next.is_none() => 'i',
            'Q' if surrounded_by_lower || after_lower => 'u',
            'O' if surrounded_by_lower || after_lower => 'o',
            'N' if after_lower && next.is_none() => 'n',
            _ => ch,
        };
        out.push(replacement);
    }
    out
}

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
        normalized = normalize_mixed_case_ocr_token(&normalized);
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

pub(super) fn split_glued_ascii_token(token: &str) -> Option<String> {
    if token.len() < 4 || !token.is_ascii() {
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
    if let Some(split) = split_camelcase_proper_noun_suffix(token) {
        return Some(split);
    }
    if let Some(split) = segment_glued_english_token_with_dictionary(token) {
        return Some(split);
    }
    if let Some(split) = split_glued_contraction(token, &lower) {
        return Some(split);
    }

    if matches!(token.chars().next(), Some('I' | 'i')) && token.len() >= 5 {
        const I_PREFIX_CONTINUATIONS: [&str; 16] = [
            "am", "have", "had", "shall", "will", "beg", "think", "know", "need", "must", "want",
            "did", "do", "was", "were", "would", "hear",
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

fn split_camelcase_proper_noun_suffix(token: &str) -> Option<String> {
    if token.len() < 6 || !token.is_ascii() {
        return None;
    }
    let bytes = token.as_bytes();
    for idx in 2..bytes.len().saturating_sub(1) {
        if bytes[idx - 1].is_ascii_lowercase() && bytes[idx].is_ascii_uppercase() {
            let prefix = &token[..idx];
            let suffix = &token[idx..];
            if suffix.len() < 2 || !suffix.chars().all(|ch| ch.is_ascii_alphabetic()) {
                continue;
            }
            let suffix_has_lower = suffix.chars().skip(1).any(|ch| ch.is_ascii_lowercase());
            if !suffix_has_lower {
                continue;
            }
            let split_prefix = segment_glued_english_token_with_dictionary(prefix)
                .or_else(|| split_glued_contraction(prefix, &prefix.to_ascii_lowercase()))
                .or_else(|| segment_glued_english_token(prefix));
            if let Some(split_prefix) = split_prefix {
                return Some(format!("{} {}", split_prefix, suffix));
            }
        }
    }
    None
}

fn segment_glued_english_token_with_dictionary(token: &str) -> Option<String> {
    const WORDS: &[&str] = &[
        "a",
        "about",
        "after",
        "again",
        "all",
        "always",
        "am",
        "an",
        "and",
        "any",
        "anyone",
        "are",
        "around",
        "as",
        "at",
        "bad",
        "be",
        "because",
        "been",
        "before",
        "being",
        "belong",
        "beneath",
        "besides",
        "blue",
        "alive",
        "along",
        "answers",
        "beyond",
        "coward",
        "dead",
        "desert's",
        "different",
        "faces",
        "impression",
        "matters",
        "right",
        "stop",
        "wonder",
        "won't",
        "bones",
        "boy",
        "breathe",
        "bring",
        "but",
        "by",
        "called",
        "can",
        "can't",
        "cannot",
        "care",
        "city",
        "come",
        "decided",
        "desert",
        "or",
        "didn't",
        "do",
        "does",
        "doesn't",
        "doing",
        "don't",
        "down",
        "dream",
        "eat",
        "end",
        "even",
        "every",
        "everyone",
        "fall",
        "fine",
        "find",
        "flower",
        "for",
        "forget",
        "friend",
        "from",
        "get",
        "give",
        "starting",
        "given",
        "go",
        "going",
        "gonna",
        "good",
        "got",
        "have",
        "haven't",
        "he",
        "he's",
        "hearing",
        "here",
        "hey",
        "him",
        "his",
        "howl",
        "humans",
        "i",
        "i'd",
        "i'll",
        "i'm",
        "i've",
        "if",
        "in",
        "into",
        "is",
        "it",
        "it's",
        "jerk",
        "kind",
        "know",
        "land",
        "leaving",
        "let",
        "let's",
        "lie",
        "like",
        "likes",
        "little",
        "live",
        "look",
        "looking",
        "matter",
        "me",
        "mean",
        "meeting",
        "mind",
        "much",
        "my",
        "no",
        "not",
        "nothing",
        "now",
        "of",
        "off",
        "okay",
        "on",
        "one",
        "paradise",
        "people",
        "place",
        "seem",
        "ive",
        "please",
        "positive",
        "put",
        "really",
        "remember",
        "remind",
        "rocks",
        "sand",
        "say",
        "see",
        "seems",
        "self",
        "sniveling",
        "so",
        "somebody",
        "stay",
        "staying",
        "still",
        "sure",
        "take",
        "takes",
        "that",
        "that's",
        "the",
        "then",
        "there",
        "there's",
        "these",
        "they",
        "thing",
        "thinks",
        "this",
        "those",
        "to",
        "too",
        "up",
        "us",
        "used",
        "voices",
        "want",
        "wanted",
        "was",
        "we",
        "went",
        "what",
        "what's",
        "when",
        "where",
        "whether",
        "who",
        "who's",
        "why",
        "will",
        "able",
        "actually",
        "advance",
        "advanced",
        "ain't",
        "almost",
        "anywhere",
        "appears",
        "attacked",
        "bears",
        "beast",
        "believe",
        "big",
        "call",
        "calling",
        "came",
        "cold",
        "coldly",
        "crossing",
        "curl",
        "day",
        "days",
        "death",
        "direction",
        "dog",
        "dome",
        "driven",
        "dumbass",
        "earth",
        "either",
        "ends",
        "enough",
        "extinct",
        "fairy",
        "fairytale",
        "fairytales",
        "far",
        "fit",
        "following",
        "garbage",
        "grandpa",
        "hate",
        "heard",
        "hit",
        "hurry",
        "inside",
        "just",
        "keeps",
        "last",
        "make",
        "man",
        "mountains",
        "nobles",
        "nobody",
        "ought",
        "pagan",
        "pass",
        "passing",
        "plenty",
        "provisions",
        "pulled",
        "rush",
        "same",
        "security",
        "ship",
        "should",
        "sleep",
        "someone",
        "sound",
        "squad",
        "standby",
        "stunt",
        "such",
        "surprise",
        "surprised",
        "telling",
        "through",
        "tight",
        "today",
        "tomorrow",
        "truth",
        "two",
        "voice",
        "walk",
        "way",
        "week",
        "worth",
        "years",
        "away",
        "bastard",
        "bleeding",
        "checkpoint",
        "chen",
        "coming",
        "doubt",
        "em",
        "fair",
        "fight",
        "half",
        "how",
        "ions",
        "kill",
        "killing",
        "living",
        "long",
        "looks",
        "maybe",
        "mountain",
        "need",
        "pride",
        "protecting",
        "provis",
        "pull",
        "quick",
        "road",
        "rotten",
        "rules",
        "said",
        "says",
        "sedo",
        "seen",
        "servants",
        "shouldn't",
        "somewhere",
        "spite",
        "talk",
        "tell",
        "them",
        "throwing",
        "using",
        "wound",
        "more",
        "than",
        "build",
        "building",
        "allergies",
        "begins",
        "improving",
        "lifestyle",
        "style",
        "weird",
        "aviation",
        "bureau",
        "sign",
        "air",
        "nasty",
        "scent",
        "heading",
        "unnatural",
        "some",
        "across",
        "appeared",
        "chem",
        "couldn't",
        "darcia",
        "dogs",
        "door",
        "gel",
        "guard",
        "guards",
        "hige",
        "leara",
        "lord",
        "lost",
        "lyek",
        "maiden",
        "means",
        "messed",
        "might",
        "myself",
        "never",
        "noble",
        "out",
        "point",
        "quent",
        "quite",
        "something",
        "they've",
        "time",
        "wrong",
        "yaiden",
        "hear",
        "ooyears",
        "appear",
        "only",
        "someone's",
        "c'm",
        "another",
        "with",
        "within",
        "without",
        "wolf",
        "wolves",
        "world",
        "worry",
        "would",
        "you",
        "you'd",
        "you'll",
        "you're",
        "you've",
        "your",
        "yourself",
    ];
    let lower = token.to_ascii_lowercase();
    let n = lower.len();
    if n < 4 {
        return None;
    }
    let mut best: Vec<Option<(i32, usize, usize)>> = vec![None; n + 1];
    best[0] = Some((0, 0, 0)); // score, prev, segments
    for end in 1..=n {
        let start_min = end.saturating_sub(16);
        for start in start_min..end {
            let Some((prev_score, _, prev_segments)) = best[start] else {
                continue;
            };
            let piece = &lower[start..end];
            if !WORDS.contains(&piece) {
                continue;
            }
            let len = end - start;
            let mut score = prev_score + (len as i32 * 10) - 8;
            if len <= 2 {
                score -= 6;
            }
            if matches!(
                piece,
                "if" | "it" | "in" | "on" | "of" | "to" | "me" | "we" | "he" | "as" | "is"
            ) {
                score += 4;
            }
            if len >= 4 {
                score += 6;
            }
            if piece.contains('\'') {
                score += 4;
            }
            let segments = prev_segments + 1;
            let replace = best[end]
                .as_ref()
                .map(|(cur, _, cur_segments)| {
                    score > *cur || (score == *cur && segments < *cur_segments)
                })
                .unwrap_or(true);
            if replace {
                best[end] = Some((score, start, segments));
            }
        }
    }
    let (_score, _, segments) = best[n]?;
    if segments < 2 {
        return None;
    }
    let mut ranges = Vec::new();
    let mut idx = n;
    while idx > 0 {
        let (_score, prev, _segments) = best[idx]?;
        ranges.push((prev, idx));
        idx = prev;
    }
    ranges.reverse();
    if ranges.iter().any(|(start, end)| {
        end - start == 1 && &lower[*start..*end] != "i" && &lower[*start..*end] != "a"
    }) {
        return None;
    }
    Some(
        ranges
            .into_iter()
            .map(|(start, end)| token[start..end].to_string())
            .collect::<Vec<_>>()
            .join(" "),
    )
}

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
            score -= 0.35;
        }
        if avg_word_len > 10.5 {
            score -= ((avg_word_len - 10.5) * 0.035).min(0.22);
        }
        let long_word_ratio = long_word_count as f32 / word_count as f32;
        if long_word_ratio > 0.12 {
            score -= ((long_word_ratio - 0.12) * 0.65).min(0.35);
        }

        let space_rate = spaces as f32 / total;
        if letters >= 80 && space_rate < 0.08 {
            score -= ((0.08 - space_rate) * 3.0).min(0.25);
        }

        let mut ascii_tokens = 0usize;
        let mut invalid_short_tokens = 0usize;
        let mut camelcase_tokens = 0usize;
        const COMMON_SHORT_WORDS: &[&str] = &[
            "a", "i", "am", "an", "as", "at", "be", "by", "do", "go", "he", "if", "in", "is", "it",
            "me", "my", "no", "of", "on", "or", "so", "to", "up", "us", "we",
        ];
        for word in &words_vec {
            let trimmed = word.trim_matches(|ch: char| !ch.is_ascii_alphabetic() && ch != '\'');
            if trimmed.is_empty() || !trimmed.is_ascii() {
                continue;
            }
            ascii_tokens += 1;
            let lower = trimmed.to_ascii_lowercase();
            if lower.len() <= 2 && !COMMON_SHORT_WORDS.contains(&lower.as_str()) {
                invalid_short_tokens += 1;
            }
            if trimmed
                .as_bytes()
                .windows(2)
                .any(|pair| pair[0].is_ascii_lowercase() && pair[1].is_ascii_uppercase())
            {
                camelcase_tokens += 1;
            }
        }
        if ascii_tokens > 0 {
            let invalid_short_ratio = invalid_short_tokens as f32 / ascii_tokens as f32;
            if invalid_short_ratio > 0.06 {
                score -= ((invalid_short_ratio - 0.06) * 1.2).min(0.30);
            }
            let camelcase_ratio = camelcase_tokens as f32 / ascii_tokens as f32;
            if camelcase_ratio > 0.02 {
                score -= ((camelcase_ratio - 0.02) * 1.0).min(0.20);
            }
        }
    }

    if letters == 0 && digits == 0 {
        score -= 0.3;
    }

    // Slightly reward candidates with enough character coverage.
    let coverage_bonus = (letters as f32 / 24.0).min(0.2);
    (score + coverage_bonus).clamp(0.0, 1.0)
}

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::subtitle_ocr::OcrLine;

    fn ocr_line(text: &str) -> OcrLine {
        OcrLine {
            text: text.to_string(),
            bbox: None,
            score: Some(0.95),
            color: None,
            italic: false,
        }
    }

    #[test]
    fn spacing_fallback_flags_long_spaceless_english_tokens() {
        let lines = vec![
            ocr_line("Whereareyou?"),
            ocr_line("Whatyoullfindbeyondthoserocks"),
        ];
        assert!(ppocr_spacing_needs_fallback(&lines));
        assert!(ppocr_needs_quality_fallback(&lines, "eng"));
    }

    #[test]
    fn spacing_fallback_does_not_flag_short_single_word_cues() {
        let lines = vec![ocr_line("Kiba!"), ocr_line("Hello")];
        assert!(!ppocr_spacing_needs_fallback(&lines));
    }

    #[test]
    fn camelcase_split_preserves_proper_noun_suffixes() {
        assert_eq!(
            split_glued_ascii_token("goingtoParadise"),
            Some("going to Paradise".to_string())
        );
        assert_eq!(
            split_glued_ascii_token("positiveaboutAlice"),
            Some("positive about Alice".to_string())
        );
    }

    #[test]
    fn dictionary_split_handles_common_glued_phrases() {
        assert_eq!(
            split_glued_ascii_token("Whatyou'll"),
            Some("What you'll".to_string())
        );
        assert_eq!(
            split_glued_ascii_token("findbeyondthoserocks"),
            Some("find beyond those rocks".to_string())
        );
        assert_eq!(
            split_glued_ascii_token("lookingforhimwon't"),
            Some("looking for him won't".to_string())
        );
        assert_eq!(
            split_glued_ascii_token("doanygood"),
            Some("do any good".to_string())
        );
    }
}
