//! Best-effort cache for DPN's own Servarr language assessment state.

use super::language::{LanguageCheckReport, LanguageRequirements};
use super::IntegrationKind;
use anyhow::{Context, Result};
use log::warn;
use serde::{Deserialize, Serialize};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::time::{SystemTime, UNIX_EPOCH};

const CACHE_ENV_VAR: &str = "DIRECT_PLAY_NICE_LANGUAGE_CACHE";

#[derive(Debug, Clone, Serialize, Deserialize)]
struct CacheRecord {
    path: String,
    kind: String,
    checked_at_unix: u64,
    requirements: LanguageRequirements,
    report: LanguageCheckReport,
}

pub fn record_assessment(
    kind: IntegrationKind,
    path: &Path,
    requirements: &LanguageRequirements,
    report: &LanguageCheckReport,
) -> Result<()> {
    let Some(cache_path) = resolve_cache_path() else {
        return Ok(());
    };
    if let Some(parent) = cache_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("creating language cache directory '{}'", parent.display()))?;
    }

    let mut records = read_records(&cache_path)?;
    let path_string = path.to_string_lossy().into_owned();
    records.retain(|record| record.path != path_string);
    records.push(CacheRecord {
        path: path_string,
        kind: format!("{:?}", kind),
        checked_at_unix: SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap_or_default()
            .as_secs(),
        requirements: requirements.clone(),
        report: report.clone(),
    });
    records.sort_by(|a, b| a.path.cmp(&b.path));
    let tmp_path = cache_path.with_extension("json.tmp");
    fs::write(&tmp_path, serde_json::to_vec_pretty(&records)?)
        .with_context(|| format!("writing language cache '{}'", tmp_path.display()))?;
    fs::rename(&tmp_path, &cache_path).with_context(|| {
        format!(
            "renaming language cache '{}' to '{}'",
            tmp_path.display(),
            cache_path.display()
        )
    })?;
    Ok(())
}

fn read_records(path: &Path) -> Result<Vec<CacheRecord>> {
    match fs::read_to_string(path) {
        Ok(contents) => match serde_json::from_str(&contents) {
            Ok(records) => Ok(records),
            Err(err) => {
                warn!(
                    "Ignoring corrupt language cache '{}': {}",
                    path.display(),
                    err
                );
                Ok(Vec::new())
            }
        },
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(Vec::new()),
        Err(err) => {
            Err(err).with_context(|| format!("reading language cache '{}'", path.display()))
        }
    }
}

fn resolve_cache_path() -> Option<PathBuf> {
    if let Some(path) = env::var_os(CACHE_ENV_VAR) {
        return Some(PathBuf::from(path));
    }
    if let Some(cache_home) = env::var_os("XDG_CACHE_HOME") {
        return Some(
            PathBuf::from(cache_home)
                .join("direct-play-nice")
                .join("servarr-language-cache.json"),
        );
    }
    env::var_os("HOME").map(|home| {
        PathBuf::from(home)
            .join(".cache")
            .join("direct-play-nice")
            .join("servarr-language-cache.json")
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::{Mutex, MutexGuard, OnceLock};
    use tempfile::TempDir;

    fn env_lock() -> MutexGuard<'static, ()> {
        static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        LOCK.get_or_init(|| Mutex::new(())).lock().unwrap()
    }

    #[test]
    fn ignores_corrupt_cache_file() {
        let _guard = env_lock();
        let tmp = TempDir::new().unwrap();
        let cache_path = tmp.path().join("cache.json");
        fs::write(&cache_path, "not json").unwrap();
        env::set_var(CACHE_ENV_VAR, &cache_path);

        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["jpn".to_string()],
            subtitles: vec!["eng".to_string()],
        };
        let report = LanguageCheckReport {
            present_audio: vec!["jpn".to_string()],
            present_subtitles: vec!["eng".to_string()],
            missing_audio: Vec::new(),
            missing_subtitles: Vec::new(),
        };

        record_assessment(
            IntegrationKind::Sonarr,
            Path::new("/media/show.mkv"),
            &requirements,
            &report,
        )
        .unwrap();
        assert!(fs::read_to_string(&cache_path)
            .unwrap()
            .contains("/media/show.mkv"));

        env::remove_var(CACHE_ENV_VAR);
    }

    #[test]
    fn records_assessment_to_configured_cache_path() {
        let _guard = env_lock();
        let tmp = TempDir::new().unwrap();
        let cache_path = tmp.path().join("cache.json");
        env::set_var(CACHE_ENV_VAR, &cache_path);

        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["jpn".to_string()],
            subtitles: vec!["eng".to_string()],
        };
        let report = LanguageCheckReport {
            present_audio: vec!["jpn".to_string()],
            present_subtitles: vec!["eng".to_string()],
            missing_audio: Vec::new(),
            missing_subtitles: Vec::new(),
        };

        record_assessment(
            IntegrationKind::Sonarr,
            Path::new("/media/show.mkv"),
            &requirements,
            &report,
        )
        .unwrap();

        let contents = fs::read_to_string(&cache_path).unwrap();
        assert!(contents.contains("/media/show.mkv"));
        assert!(contents.contains("jpn"));
        assert!(contents.contains("eng"));

        env::remove_var(CACHE_ENV_VAR);
    }
}
