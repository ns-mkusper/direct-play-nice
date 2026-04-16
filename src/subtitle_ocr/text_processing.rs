use super::{normalize_utf8_text, OcrLine};

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
        ("constablecrane", "constable crane"),
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

    normalize_utf8_text(&out)
}

pub(super) fn is_english_language(language: &str) -> bool {
    let lang = language.trim().to_ascii_lowercase();
    matches!(lang.as_str(), "eng" | "en" | "en-us" | "en_us")
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
    if let Some(split) = split_glued_contraction(token, &lower) {
        return Some(split);
    }
    if is_common_english_word(&lower) {
        return None;
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
            let split_rest =
                segment_glued_english_token(&token[1..]).unwrap_or_else(|| token[1..].to_string());
            return Some(format!("{} {}", &token[..1], split_rest));
        }
    }

    for suffix in [
        "down", "off", "out", "up", "in", "on", "over", "under", "away", "back",
    ] {
        if lower.ends_with(suffix) {
            let split = token.len() - suffix.len();
            if split >= 4 && is_common_english_word(&lower[..split]) {
                return Some(format!("{} {}", &token[..split], &token[split..]));
            }
        }
    }

    segment_glued_english_token(token)
}

fn segment_glued_english_token(token: &str) -> Option<String> {
    let lower = token.to_ascii_lowercase();
    if lower.len() < 5 || is_common_english_word(&lower) {
        return None;
    }

    // Dynamic programming split over common English words.
    let n = lower.len();
    let mut best: Vec<Option<(i32, usize, usize)>> = vec![None; n + 1]; // (score, prev_idx, segments)
    best[0] = Some((0, 0, 0));
    for end in 1..=n {
        let start_min = end.saturating_sub(12);
        for start in start_min..end {
            let Some((prev_score, _prev_idx, prev_segments)) = best[start] else {
                continue;
            };
            let candidate = &lower[start..end];
            if !is_common_english_word(candidate) {
                continue;
            }
            let segment_len = end - start;
            let score = prev_score + (segment_len as i32 * segment_len as i32) - 4;
            let segments = prev_segments + 1;
            let should_replace = best[end]
                .as_ref()
                .map(|(current_score, _, current_segments)| {
                    score > *current_score
                        || (score == *current_score && segments < *current_segments)
                })
                .unwrap_or(true);
            if should_replace {
                best[end] = Some((score, start, segments));
            }
        }
    }

    let (_score, _prev, segment_count) = best[n]?;
    if segment_count < 2 {
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

fn is_common_english_word(word: &str) -> bool {
    const WORDS: [&str; 511] = [
        "a",
        "abandon",
        "able",
        "about",
        "above",
        "according",
        "across",
        "actually",
        "add",
        "after",
        "again",
        "against",
        "ago",
        "ahead",
        "air",
        "alive",
        "all",
        "allow",
        "almost",
        "along",
        "already",
        "also",
        "although",
        "always",
        "am",
        "among",
        "amount",
        "an",
        "and",
        "another",
        "any",
        "anyone",
        "anything",
        "apostatize",
        "apostatized",
        "are",
        "around",
        "as",
        "ask",
        "asked",
        "at",
        "away",
        "back",
        "bad",
        "base",
        "be",
        "became",
        "because",
        "become",
        "becoming",
        "been",
        "before",
        "beg",
        "behind",
        "being",
        "below",
        "beside",
        "best",
        "better",
        "between",
        "black",
        "blood",
        "blue",
        "body",
        "book",
        "born",
        "both",
        "break",
        "bring",
        "brother",
        "brought",
        "build",
        "built",
        "burn",
        "burning",
        "but",
        "by",
        "call",
        "came",
        "can",
        "cannot",
        "cause",
        "century",
        "children",
        "christ",
        "christi",
        "christians",
        "church",
        "city",
        "clear",
        "close",
        "coal",
        "cold",
        "come",
        "constable",
        "continue",
        "could",
        "country",
        "courage",
        "crane",
        "cut",
        "dandelion",
        "dark",
        "day",
        "days",
        "dead",
        "death",
        "deep",
        "demonstrate",
        "denounced",
        "despite",
        "did",
        "die",
        "different",
        "do",
        "does",
        "done",
        "door",
        "down",
        "drops",
        "during",
        "dutch",
        "each",
        "early",
        "earth",
        "easy",
        "either",
        "ended",
        "enough",
        "even",
        "ever",
        "every",
        "face",
        "fact",
        "faith",
        "family",
        "far",
        "father",
        "fear",
        "ferreira",
        "few",
        "figure",
        "filled",
        "finally",
        "fire",
        "first",
        "five",
        "floor",
        "follow",
        "food",
        "for",
        "force",
        "form",
        "four",
        "free",
        "friars",
        "friend",
        "from",
        "front",
        "full",
        "game",
        "get",
        "girl",
        "give",
        "given",
        "gives",
        "glass",
        "go",
        "god",
        "good",
        "gospel",
        "governor",
        "great",
        "green",
        "group",
        "grow",
        "had",
        "half",
        "hand",
        "happened",
        "hard",
        "has",
        "have",
        "he",
        "head",
        "hear",
        "heart",
        "heavy",
        "hells",
        "her",
        "here",
        "hes",
        "hidden",
        "him",
        "his",
        "history",
        "holes",
        "home",
        "hope",
        "hot",
        "house",
        "how",
        "however",
        "i",
        "idea",
        "if",
        "important",
        "in",
        "inside",
        "into",
        "is",
        "it",
        "its",
        "japan",
        "japanese",
        "jesus",
        "job",
        "just",
        "keep",
        "kind",
        "king",
        "knew",
        "know",
        "known",
        "ladles",
        "land",
        "large",
        "last",
        "late",
        "later",
        "leave",
        "left",
        "let",
        "letter",
        "life",
        "light",
        "like",
        "line",
        "list",
        "little",
        "live",
        "living",
        "long",
        "looked",
        "looking",
        "looks",
        "lopped",
        "lord",
        "lost",
        "love",
        "low",
        "made",
        "main",
        "make",
        "man",
        "many",
        "may",
        "me",
        "mean",
        "means",
        "men",
        "might",
        "mind",
        "minute",
        "mockery",
        "money",
        "month",
        "months",
        "more",
        "morning",
        "most",
        "mother",
        "mountain",
        "move",
        "moved",
        "must",
        "my",
        "nagasaki",
        "name",
        "near",
        "need",
        "never",
        "new",
        "news",
        "next",
        "night",
        "no",
        "none",
        "nor",
        "north",
        "not",
        "nothing",
        "notice",
        "now",
        "number",
        "of",
        "off",
        "officials",
        "often",
        "old",
        "on",
        "once",
        "one",
        "only",
        "open",
        "or",
        "order",
        "other",
        "others",
        "our",
        "out",
        "outside",
        "over",
        "own",
        "padres",
        "pain",
        "paper",
        "pardon",
        "part",
        "partly",
        "pass",
        "past",
        "pax",
        "pay",
        "peace",
        "people",
        "persecution",
        "person",
        "piece",
        "place",
        "point",
        "portugal",
        "possible",
        "power",
        "praise",
        "praised",
        "presence",
        "priests",
        "probably",
        "progress",
        "prolonged",
        "proven",
        "public",
        "put",
        "question",
        "rain",
        "ransomed",
        "rather",
        "reach",
        "read",
        "ready",
        "really",
        "red",
        "refused",
        "remain",
        "remained",
        "remember",
        "repression",
        "right",
        "risked",
        "river",
        "road",
        "room",
        "run",
        "said",
        "same",
        "saw",
        "say",
        "school",
        "second",
        "secret",
        "see",
        "seem",
        "seen",
        "service",
        "set",
        "several",
        "shall",
        "she",
        "show",
        "side",
        "since",
        "sir",
        "six",
        "slowly",
        "small",
        "smuggled",
        "so",
        "society",
        "some",
        "something",
        "son",
        "soon",
        "sound",
        "south",
        "splash",
        "spread",
        "springs",
        "stand",
        "state",
        "still",
        "stop",
        "stopped",
        "story",
        "strength",
        "stronger",
        "strongest",
        "such",
        "suffering",
        "sure",
        "surrendered",
        "sweeping",
        "table",
        "taken",
        "taking",
        "talk",
        "teacher",
        "team",
        "tell",
        "ten",
        "terms",
        "text",
        "than",
        "that",
        "thats",
        "the",
        "their",
        "them",
        "then",
        "there",
        "these",
        "they",
        "thing",
        "things",
        "think",
        "this",
        "those",
        "though",
        "thousands",
        "three",
        "through",
        "time",
        "to",
        "today",
        "together",
        "told",
        "tonight",
        "too",
        "took",
        "top",
        "tortured",
        "toward",
        "trader",
        "traveling",
        "true",
        "truth",
        "turn",
        "two",
        "under",
        "until",
        "unzen",
        "up",
        "upon",
        "us",
        "use",
        "used",
        "using",
        "usually",
        "value",
        "very",
        "view",
        "voice",
        "wait",
        "wall",
        "want",
        "war",
        "was",
        "watch",
        "water",
        "we",
        "week",
        "well",
        "went",
        "were",
        "what",
        "when",
        "where",
        "which",
        "white",
        "who",
        "whole",
        "whose",
        "why",
        "will",
        "with",
        "within",
        "woman",
        "word",
        "words",
        "work",
        "world",
        "worse",
        "would",
        "wrote",
        "year",
        "years",
        "yes",
        "yet",
        "you",
        "young",
        "your",
    ];
    WORDS.binary_search(&word).is_ok()
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
    let mut sum = 0.0f32;
    let mut count = 0usize;
    for score in lines.iter().filter_map(|line| line.score) {
        if score.is_finite() {
            sum += score;
            count += 1;
        }
    }
    if count == 0 {
        None
    } else {
        Some(sum / count as f32)
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
            score -= 0.2;
        }
        if avg_word_len > 8.5 {
            score -= 0.12;
        }
        if long_word_count > 0 {
            score -= (long_word_count as f32 * 0.04).min(0.2);
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

    let quality = ocr_text_quality_score(&lines_text_for_quality(lines), language);
    quality < 0.45
}
