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

#[derive(Debug, Clone, Serialize, Deserialize)]
struct NoCandidateRecord {
    key: String,
    kind: String,
    target_noun: String,
    target_id: i64,
    checked_at_unix: u64,
    requirements: LanguageRequirements,
    reason: String,
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
    record_assessment_at_path(&cache_path, kind, path, requirements, report)
}

pub fn no_candidate_cooldown_remaining_days(
    kind: IntegrationKind,
    target_noun: &str,
    target_id: i64,
    requirements: &LanguageRequirements,
    cooldown_days: u32,
) -> Result<Option<u32>> {
    if cooldown_days == 0 {
        return Ok(None);
    }
    let Some(cache_path) = resolve_no_candidate_cache_path() else {
        return Ok(None);
    };
    let records = read_no_candidate_records(&cache_path)?;
    let key = no_candidate_key(kind, target_noun, target_id);
    let now = now_unix_secs();
    let cooldown_secs = u64::from(cooldown_days) * 86_400;
    Ok(records
        .into_iter()
        .find(|record| record.key == key && record.requirements == *requirements)
        .and_then(|record| {
            let elapsed = now.saturating_sub(record.checked_at_unix);
            if elapsed >= cooldown_secs {
                None
            } else {
                let remaining_secs = cooldown_secs - elapsed;
                Some(((remaining_secs + 86_399) / 86_400) as u32)
            }
        }))
}

pub fn record_no_candidate(
    kind: IntegrationKind,
    target_noun: &str,
    target_id: i64,
    requirements: &LanguageRequirements,
    reason: &str,
) -> Result<()> {
    let Some(cache_path) = resolve_no_candidate_cache_path() else {
        return Ok(());
    };
    if let Some(parent) = cache_path.parent() {
        fs::create_dir_all(parent).with_context(|| {
            format!(
                "creating language no-candidate cache directory '{}'",
                parent.display()
            )
        })?;
    }

    let key = no_candidate_key(kind, target_noun, target_id);
    let mut records = read_no_candidate_records(&cache_path)?;
    records.retain(|record| record.key != key);
    records.push(NoCandidateRecord {
        key,
        kind: format!("{:?}", kind),
        target_noun: target_noun.to_string(),
        target_id,
        checked_at_unix: now_unix_secs(),
        requirements: requirements.clone(),
        reason: reason.to_string(),
    });
    records.sort_by(|a, b| a.key.cmp(&b.key));
    let tmp_path = cache_path.with_extension("json.tmp");
    fs::write(&tmp_path, serde_json::to_vec_pretty(&records)?).with_context(|| {
        format!(
            "writing language no-candidate cache '{}'",
            tmp_path.display()
        )
    })?;
    fs::rename(&tmp_path, &cache_path).with_context(|| {
        format!(
            "renaming language no-candidate cache '{}' to '{}'",
            tmp_path.display(),
            cache_path.display()
        )
    })?;
    Ok(())
}

fn record_assessment_at_path(
    cache_path: &Path,
    kind: IntegrationKind,
    path: &Path,
    requirements: &LanguageRequirements,
    report: &LanguageCheckReport,
) -> Result<()> {
    if let Some(parent) = cache_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("creating language cache directory '{}'", parent.display()))?;
    }

    let mut records = read_records(cache_path)?;
    let path_string = path.to_string_lossy().into_owned();
    records.retain(|record| record.path != path_string);
    records.push(CacheRecord {
        path: path_string,
        kind: format!("{:?}", kind),
        checked_at_unix: now_unix_secs(),
        requirements: requirements.clone(),
        report: report.clone(),
    });
    records.sort_by(|a, b| a.path.cmp(&b.path));
    let tmp_path = cache_path.with_extension("json.tmp");
    fs::write(&tmp_path, serde_json::to_vec_pretty(&records)?)
        .with_context(|| format!("writing language cache '{}'", tmp_path.display()))?;
    fs::rename(&tmp_path, cache_path).with_context(|| {
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

fn read_no_candidate_records(path: &Path) -> Result<Vec<NoCandidateRecord>> {
    match fs::read_to_string(path) {
        Ok(contents) => match serde_json::from_str(&contents) {
            Ok(records) => Ok(records),
            Err(err) => {
                warn!(
                    "Ignoring corrupt language no-candidate cache '{}': {}",
                    path.display(),
                    err
                );
                Ok(Vec::new())
            }
        },
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(Vec::new()),
        Err(err) => Err(err)
            .with_context(|| format!("reading language no-candidate cache '{}'", path.display())),
    }
}

fn resolve_no_candidate_cache_path() -> Option<PathBuf> {
    if let Some(path) = env::var_os("DIRECT_PLAY_NICE_LANGUAGE_NO_CANDIDATE_CACHE") {
        return Some(PathBuf::from(path));
    }
    resolve_cache_path().map(|path| path.with_file_name("servarr-language-no-candidate-cache.json"))
}

fn no_candidate_key(kind: IntegrationKind, target_noun: &str, target_id: i64) -> String {
    format!("{:?}:{}:{}", kind, target_noun, target_id)
}

fn now_unix_secs() -> u64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs()
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
    use tempfile::TempDir;

    #[test]
    fn ignores_corrupt_cache_file() {
        let tmp = TempDir::new().unwrap();
        let cache_path = tmp.path().join("cache.json");
        fs::write(&cache_path, "not json").unwrap();

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

        record_assessment_at_path(
            &cache_path,
            IntegrationKind::Sonarr,
            Path::new("/media/show.mkv"),
            &requirements,
            &report,
        )
        .unwrap();
        assert!(fs::read_to_string(&cache_path)
            .unwrap()
            .contains("/media/show.mkv"));
    }

    #[test]
    fn records_assessment_to_configured_cache_path() {
        let tmp = TempDir::new().unwrap();
        let cache_path = tmp.path().join("cache.json");

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

        record_assessment_at_path(
            &cache_path,
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
    }

    #[test]
    fn records_and_reads_no_candidate_cooldown() {
        let tmp = TempDir::new().unwrap();
        let cache_path = tmp.path().join("no-candidates.json");
        std::env::set_var("DIRECT_PLAY_NICE_LANGUAGE_NO_CANDIDATE_CACHE", &cache_path);

        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string()],
            subtitles: Vec::new(),
        };

        record_no_candidate(
            IntegrationKind::Sonarr,
            "episode",
            42,
            &requirements,
            "no approved release",
        )
        .unwrap();

        let remaining = no_candidate_cooldown_remaining_days(
            IntegrationKind::Sonarr,
            "episode",
            42,
            &requirements,
            14,
        )
        .unwrap();
        assert_eq!(remaining, Some(14));

        let different_requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string(), "jpn".to_string()],
            subtitles: Vec::new(),
        };
        assert_eq!(
            no_candidate_cooldown_remaining_days(
                IntegrationKind::Sonarr,
                "episode",
                42,
                &different_requirements,
                14,
            )
            .unwrap(),
            None
        );

        std::env::remove_var("DIRECT_PLAY_NICE_LANGUAGE_NO_CANDIDATE_CACHE");
    }
}
