//! Shared temporary-file naming and atomic replacement helpers.

use anyhow::{Context, Result};
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

/// Stable namespace used for files which are safe to remove after an interrupted run.
pub const NAMESPACE: &str = ".direct-play-nice";

/// Builds a sibling staging path while preserving the destination extension.
pub fn path_for(final_path: &Path, tag: &str) -> PathBuf {
    let filename = final_path
        .file_name()
        .map(|name| name.to_string_lossy().into_owned())
        .unwrap_or_else(|| String::from("output"));
    let new_name = match filename.rfind('.') {
        Some(index) => {
            let (stem, extension) = filename.split_at(index);
            format!("{stem}{NAMESPACE}.{tag}{extension}")
        }
        None => format!("{filename}{NAMESPACE}.{tag}"),
    };
    final_path
        .parent()
        .map(|parent| parent.join(&new_name))
        .unwrap_or_else(|| PathBuf::from(new_name))
}

/// Builds a hidden unique temporary path in the destination directory.
pub fn unique_path(path: &Path) -> PathBuf {
    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    let filename = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("output");
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    parent.join(format!(".{filename}.{}.{}.tmp", std::process::id(), nanos))
}

/// Promotes a completed sibling staging file into place.
pub fn promote(staged: &Path, final_path: &Path, context: &str) -> Result<()> {
    #[cfg(windows)]
    if final_path.exists() {
        fs::remove_file(final_path)
            .with_context(|| format!("removing '{}' before {context}", final_path.display()))?;
    }
    fs::rename(staged, final_path).with_context(|| {
        format!(
            "promoting '{}' to '{}' after {context}",
            staged.display(),
            final_path.display()
        )
    })
}

/// Removes a partial staging file if it exists.
pub fn discard(path: &Path) {
    if let Err(error) = fs::remove_file(path) {
        if error.kind() != std::io::ErrorKind::NotFound {
            log::warn!(
                "failed to remove partial staging file '{}': {error}",
                path.display()
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn path_for_preserves_extension_and_parent() {
        let path = Path::new("/media/show/episode.mkv");
        assert_eq!(
            path_for(path, "tmp"),
            PathBuf::from("/media/show/episode.direct-play-nice.tmp.mkv")
        );
    }

    #[test]
    fn path_for_handles_extensionless_files() {
        assert_eq!(
            path_for(Path::new("episode"), "ocr"),
            PathBuf::from("episode.direct-play-nice.ocr")
        );
    }
}
