//! Case-insensitive environment variable lookup and parsing helpers.

use std::env;

/// Parses common truthy/falsey env string values into a boolean.
pub(super) fn parse_boolish(value: String) -> Option<bool> {
    match value.to_ascii_lowercase().as_str() {
        "true" | "1" | "yes" | "y" => Some(true),
        "false" | "0" | "no" | "n" => Some(false),
        _ => None,
    }
}

/// Looks up an environment variable using case-insensitive key matching.
pub(super) fn get_env_ignore_case(key: &str) -> Option<String> {
    if let Ok(val) = env::var(key) {
        return Some(val);
    }

    let target = key.to_ascii_lowercase();
    for (k, v) in env::vars() {
        if k.to_ascii_lowercase() == target {
            return Some(v);
        }
    }
    None
}

/// Formats captured environment key/value pairs for diagnostics.
pub(super) fn format_env_snapshot(entries: &[(String, String)]) -> String {
    if entries.is_empty() {
        return "<none>".to_string();
    }

    entries
        .iter()
        .map(|(k, v)| format!("{}={}", k, v))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Returns the first non-empty value among the provided environment keys.
pub(super) fn first_non_empty(keys: &[&str]) -> Option<String> {
    for key in keys {
        if let Some(val) = get_env_ignore_case(key) {
            let trimmed = val.trim();
            if !trimmed.is_empty() {
                return Some(trimmed.to_string());
            }
        }
    }
    None
}

/// Parses a `|`-delimited environment variable into non-empty path entries.
pub(super) fn get_env_paths(keys: &[&str]) -> Option<Vec<String>> {
    for key in keys {
        if let Some(val) = get_env_ignore_case(key) {
            let mut entries = Vec::new();
            for part in val.split('|') {
                let trimmed = part.trim();
                if !trimmed.is_empty() {
                    entries.push(trimmed.to_string());
                }
            }
            if !entries.is_empty() {
                return Some(entries);
            }
        }
    }
    None
}
