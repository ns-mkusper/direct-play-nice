//! Best-effort cache for DPN's own Servarr language assessment state.

use super::language::{LanguageCheckReport, LanguageRequirements};
use super::IntegrationKind;
use anyhow::{Context, Result};
use fs2::FileExt;
use log::warn;
use serde::{Deserialize, Serialize};
use std::env;
use std::fs;
use std::fs::OpenOptions;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::time::{SystemTime, UNIX_EPOCH};

static CACHE_WRITE_MUTEX: Mutex<()> = Mutex::new(());

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
                Some(remaining_secs.div_ceil(86_400) as u32)
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
    record_no_candidate_at_path(
        &cache_path,
        kind,
        target_noun,
        target_id,
        requirements,
        reason,
    )
}

fn record_no_candidate_at_path(
    cache_path: &Path,
    kind: IntegrationKind,
    target_noun: &str,
    target_id: i64,
    requirements: &LanguageRequirements,
    reason: &str,
) -> Result<()> {
    with_cache_lock(cache_path, "language no-candidate cache", || {
        let key = no_candidate_key(kind, target_noun, target_id);
        let mut records = read_no_candidate_records(cache_path)?;
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
        write_json_atomically(cache_path, "language no-candidate cache", &records)
    })
}

fn record_assessment_at_path(
    cache_path: &Path,
    kind: IntegrationKind,
    path: &Path,
    requirements: &LanguageRequirements,
    report: &LanguageCheckReport,
) -> Result<()> {
    with_cache_lock(cache_path, "language cache", || {
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
        write_json_atomically(cache_path, "language cache", &records)
    })
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

fn with_cache_lock<T>(cache_path: &Path, label: &str, f: impl FnOnce() -> Result<T>) -> Result<T> {
    if let Some(parent) = cache_path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("creating {label} directory '{}'", parent.display()))?;
    }

    let _process_guard = CACHE_WRITE_MUTEX
        .lock()
        .unwrap_or_else(|err| err.into_inner());
    let lock_path = cache_path.with_extension("json.lock");
    let lock_file = OpenOptions::new()
        .create(true)
        .read(true)
        .write(true)
        .truncate(false)
        .open(&lock_path)
        .with_context(|| format!("opening {label} lock '{}'", lock_path.display()))?;
    lock_file
        .lock_exclusive()
        .with_context(|| format!("locking {label} lock '{}'", lock_path.display()))?;

    f()
}

fn write_json_atomically<T: Serialize>(cache_path: &Path, label: &str, value: &T) -> Result<()> {
    let tmp_path = unique_tmp_path(cache_path);
    fs::write(&tmp_path, serde_json::to_vec_pretty(value)?)
        .with_context(|| format!("writing {label} '{}'", tmp_path.display()))?;
    fs::rename(&tmp_path, cache_path).with_context(|| {
        let _ = fs::remove_file(&tmp_path);
        format!(
            "renaming {label} '{}' to '{}'",
            tmp_path.display(),
            cache_path.display()
        )
    })?;
    Ok(())
}

fn unique_tmp_path(cache_path: &Path) -> PathBuf {
    let parent = cache_path.parent().unwrap_or_else(|| Path::new("."));
    let filename = cache_path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("cache.json");
    let nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap_or_default()
        .as_nanos();
    parent.join(format!(".{filename}.{}.{}.tmp", std::process::id(), nanos))
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
    fn concurrent_assessment_writes_keep_valid_json_and_all_records() {
        let tmp = TempDir::new().unwrap();
        let cache_path = tmp.path().join("cache.json");
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string()],
            subtitles: Vec::new(),
        };
        let report = LanguageCheckReport {
            present_audio: vec!["eng".to_string()],
            present_subtitles: Vec::new(),
            missing_audio: Vec::new(),
            missing_subtitles: Vec::new(),
        };

        std::thread::scope(|scope| {
            for i in 0..32 {
                let cache_path = cache_path.clone();
                let requirements = requirements.clone();
                let report = report.clone();
                scope.spawn(move || {
                    record_assessment_at_path(
                        &cache_path,
                        IntegrationKind::Sonarr,
                        Path::new(&format!("/media/show-{i}.mkv")),
                        &requirements,
                        &report,
                    )
                    .unwrap();
                });
            }
        });

        let records = read_records(&cache_path).unwrap();
        assert_eq!(records.len(), 32);
        for i in 0..32 {
            assert!(records
                .iter()
                .any(|record| record.path == format!("/media/show-{i}.mkv")));
        }
        assert!(fs::read_dir(tmp.path())
            .unwrap()
            .filter_map(|entry| entry.ok())
            .all(|entry| !entry.file_name().to_string_lossy().ends_with(".tmp")));
    }

    #[test]
    fn concurrent_no_candidate_writes_keep_valid_json_and_all_records() {
        let tmp = TempDir::new().unwrap();
        let cache_path = tmp.path().join("no-candidates.json");
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string()],
            subtitles: Vec::new(),
        };

        std::thread::scope(|scope| {
            for i in 0..32 {
                let cache_path = cache_path.clone();
                let requirements = requirements.clone();
                scope.spawn(move || {
                    record_no_candidate_at_path(
                        &cache_path,
                        IntegrationKind::Sonarr,
                        "episode",
                        i,
                        &requirements,
                        "no approved release",
                    )
                    .unwrap();
                });
            }
        });

        let records = read_no_candidate_records(&cache_path).unwrap();
        assert_eq!(records.len(), 32);
        for i in 0..32 {
            assert!(records.iter().any(|record| record.target_id == i));
        }
        assert!(fs::read_dir(tmp.path())
            .unwrap()
            .filter_map(|entry| entry.ok())
            .all(|entry| !entry.file_name().to_string_lossy().ends_with(".tmp")));
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
