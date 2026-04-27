//! Module for path policy.

use anyhow::{anyhow, bail, Context, Result};
use std::ffi::CString;
use std::path::{Path, PathBuf};

/// Builds the destination file path using suffix/extension policy options.
pub(super) fn resolve_output_path(
    input_path: &Path,
    desired_ext: &str,
    desired_suffix: &str,
) -> Result<PathBuf> {
    let parent = input_path.parent();
    let stem = input_path
        .file_stem()
        .and_then(|s| s.to_str())
        .ok_or_else(|| {
            anyhow!(
                "Input file name is not valid UTF-8: {}",
                input_path.display()
            )
        })?;

    let suffix = normalize_suffix(desired_suffix);

    let final_extension = if desired_ext.eq_ignore_ascii_case("match-input")
        || desired_ext.eq_ignore_ascii_case("same")
    {
        input_path
            .extension()
            .and_then(|ext| ext.to_str())
            .map(|s| s.to_string())
            .unwrap_or_default()
    } else {
        let trimmed = desired_ext.trim().trim_start_matches('.');
        if trimmed.is_empty() {
            bail!(
                "Invalid --servarr-output-extension '{}': must not be empty.",
                desired_ext
            );
        }
        trimmed.to_string()
    };

    let mut filename = String::from(stem);
    if let Some(sfx) = suffix.as_ref() {
        filename.push_str(sfx);
    }
    if !final_extension.is_empty() {
        filename.push('.');
        filename.push_str(&final_extension);
    }

    let new_path = match parent {
        Some(dir) => dir.join(filename),
        None => PathBuf::from(filename),
    };

    Ok(new_path)
}

/// Normalizes a suffix value to either `None` or a dot-prefixed string.
pub(super) fn normalize_suffix(raw: &str) -> Option<String> {
    let trimmed = raw.trim();
    if trimmed.is_empty() {
        return None;
    }
    if trimmed.starts_with('.') {
        Some(trimmed.to_string())
    } else {
        Some(format!(".{}", trimmed))
    }
}

/// Appends a suffix before the extension while preserving parent directory.
pub(super) fn append_suffix(path: &Path, suffix: &str) -> PathBuf {
    let parent = path.parent();
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

    match parent {
        Some(dir) => dir.join(new_name),
        None => PathBuf::from(new_name),
    }
}

/// Converts a Rust path to a `CString` for FFI calls.
pub(super) fn path_to_cstring(path: &Path) -> Result<CString> {
    let path_str = path
        .to_str()
        .ok_or_else(|| anyhow!("Path contains invalid UTF-8: {}", path.display()))?;
    CString::new(path_str.as_bytes()).context("Failed to convert path to CString")
}
