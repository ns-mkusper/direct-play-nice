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
use std::sync::atomic::{AtomicBool, AtomicU32, Ordering};
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
    /// Consecutive no-candidate results; records from older versions count as one.
    #[serde(default)]
    misses: u32,
}

/// A replacement the language audit grabbed, kept until a later audit run
/// has checked the imported file. The audit and the Arr import hooks run as
/// different users, so the audit verifies its own grabs.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PendingGrab {
    pub kind: String,
    pub target_noun: String,
    pub target_id: i64,
    pub release_title: String,
    #[serde(default)]
    pub guid: Option<String>,
    #[serde(default)]
    pub download_id: Option<String>,
    pub grabbed_at_unix: u64,
    pub requirements: LanguageRequirements,
}

impl PendingGrab {
    pub fn new(
        kind: IntegrationKind,
        target_noun: &str,
        target_id: i64,
        release_title: &str,
        guid: Option<String>,
        requirements: &LanguageRequirements,
    ) -> Self {
        Self {
            kind: format!("{:?}", kind),
            target_noun: target_noun.to_string(),
            target_id,
            release_title: release_title.to_string(),
            guid,
            download_id: None,
            grabbed_at_unix: now_unix_secs(),
            requirements: requirements.clone(),
        }
    }

    fn is_for(&self, kind: IntegrationKind, target_noun: &str, target_id: i64) -> bool {
        self.kind == format!("{:?}", kind)
            && self.target_noun == target_noun
            && self.target_id == target_id
    }
}

/// Process-wide switch that turns every cache write into a no-op. Set once by `--dry-run`.
static WRITES_DISABLED: AtomicBool = AtomicBool::new(false);

/// Stops this process from writing the language cache. Reads keep working.
pub fn disable_writes() {
    WRITES_DISABLED.store(true, Ordering::SeqCst);
}

fn writes_disabled() -> bool {
    WRITES_DISABLED.load(Ordering::SeqCst)
}

/// Upper bound for the doubling no-candidate cooldown. Zero keeps the cooldown
/// at its base length. Set once from `--servarr-language-audit-no-candidate-max-cooldown-days`.
static NO_CANDIDATE_MAX_COOLDOWN_DAYS: AtomicU32 = AtomicU32::new(0);

/// Lets repeated no-candidate results double the cooldown, up to `days`.
pub fn set_no_candidate_max_cooldown_days(days: u32) {
    NO_CANDIDATE_MAX_COOLDOWN_DAYS.store(days, Ordering::SeqCst);
}

/// Cooldown after `misses` consecutive no-candidate results: `base_days`
/// doubled for each miss after the first, capped at `max_days` (never below
/// `base_days`).
fn no_candidate_cooldown_days(base_days: u32, misses: u32, max_days: u32) -> u32 {
    let doublings = misses.saturating_sub(1).min(16);
    base_days
        .saturating_mul(1 << doublings)
        .min(max_days.max(base_days))
}

pub fn record_assessment(
    kind: IntegrationKind,
    path: &Path,
    requirements: &LanguageRequirements,
    report: &LanguageCheckReport,
) -> Result<()> {
    if writes_disabled() {
        return Ok(());
    }
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
    let max_days = NO_CANDIDATE_MAX_COOLDOWN_DAYS.load(Ordering::SeqCst);
    Ok(records
        .into_iter()
        .find(|record| record.key == key && record.requirements == *requirements)
        .and_then(|record| {
            let days = no_candidate_cooldown_days(cooldown_days, record.misses, max_days);
            let cooldown_secs = u64::from(days) * 86_400;
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
    if writes_disabled() {
        return Ok(());
    }
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

/// Removes the no-candidate cooldown of a target, so the next search is not
/// skipped.
pub fn clear_no_candidate(kind: IntegrationKind, target_noun: &str, target_id: i64) -> Result<()> {
    if writes_disabled() {
        return Ok(());
    }
    let Some(cache_path) = resolve_no_candidate_cache_path() else {
        return Ok(());
    };
    with_cache_lock(&cache_path, "language no-candidate cache", || {
        let key = no_candidate_key(kind, target_noun, target_id);
        let mut records = read_no_candidate_records(&cache_path)?;
        let before = records.len();
        records.retain(|record| record.key != key);
        if records.len() == before {
            return Ok(());
        }
        write_json_atomically(&cache_path, "language no-candidate cache", &records)
    })
}

/// Remembers a replacement grabbed by the audit; replaces any earlier
/// pending grab of the same target.
pub fn record_pending_grab(grab: PendingGrab) -> Result<()> {
    if writes_disabled() {
        return Ok(());
    }
    let Some(cache_path) = resolve_pending_grab_cache_path() else {
        return Ok(());
    };
    with_cache_lock(&cache_path, "language pending-grab cache", || {
        let mut records = read_pending_grabs(&cache_path)?;
        records.retain(|record| {
            record.kind != grab.kind
                || record.target_noun != grab.target_noun
                || record.target_id != grab.target_id
        });
        records.push(grab);
        write_json_atomically(&cache_path, "language pending-grab cache", &records)
    })
}

/// Pending grabs of `kind`, oldest first.
pub fn pending_grabs(kind: IntegrationKind) -> Result<Vec<PendingGrab>> {
    let Some(cache_path) = resolve_pending_grab_cache_path() else {
        return Ok(Vec::new());
    };
    let kind = format!("{:?}", kind);
    let mut records = read_pending_grabs(&cache_path)?;
    records.retain(|record| record.kind == kind);
    records.sort_by_key(|record| record.grabbed_at_unix);
    Ok(records)
}

/// Drops the pending grab of a target if it is still the one recorded at
/// `grabbed_at_unix` (a newer grab of the same target is kept).
pub fn remove_pending_grab(
    kind: IntegrationKind,
    target_noun: &str,
    target_id: i64,
    grabbed_at_unix: u64,
) -> Result<()> {
    if writes_disabled() {
        return Ok(());
    }
    let Some(cache_path) = resolve_pending_grab_cache_path() else {
        return Ok(());
    };
    with_cache_lock(&cache_path, "language pending-grab cache", || {
        let mut records = read_pending_grabs(&cache_path)?;
        records.retain(|record| {
            !(record.is_for(kind, target_noun, target_id)
                && record.grabbed_at_unix == grabbed_at_unix)
        });
        write_json_atomically(&cache_path, "language pending-grab cache", &records)
    })
}

/// Reads pending grabs, skipping entries this version cannot parse instead of
/// discarding the whole file.
fn read_pending_grabs(path: &Path) -> Result<Vec<PendingGrab>> {
    let contents = match fs::read_to_string(path) {
        Ok(contents) => contents,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(err) => {
            return Err(err).with_context(|| {
                format!("reading language pending-grab cache '{}'", path.display())
            })
        }
    };
    let entries = match serde_json::from_str::<Vec<serde_json::Value>>(&contents) {
        Ok(entries) => entries,
        Err(err) => {
            warn!(
                "Ignoring corrupt language pending-grab cache '{}': {}",
                path.display(),
                err
            );
            return Ok(Vec::new());
        }
    };
    Ok(entries
        .into_iter()
        .filter_map(|entry| match serde_json::from_value(entry) {
            Ok(record) => Some(record),
            Err(err) => {
                warn!(
                    "Ignoring unreadable entry in language pending-grab cache '{}': {}",
                    path.display(),
                    err
                );
                None
            }
        })
        .collect())
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
        let previous_misses = records
            .iter()
            .find(|record| record.key == key && record.requirements == *requirements)
            .map_or(0, |record| record.misses.max(1));
        records.retain(|record| record.key != key);
        records.push(NoCandidateRecord {
            key,
            kind: format!("{:?}", kind),
            target_noun: target_noun.to_string(),
            target_id,
            checked_at_unix: now_unix_secs(),
            requirements: requirements.clone(),
            reason: reason.to_string(),
            misses: previous_misses.saturating_add(1),
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
    if let Err(error) = crate::staging::promote(&tmp_path, cache_path, label) {
        crate::staging::discard(&tmp_path);
        return Err(error);
    }
    Ok(())
}

fn unique_tmp_path(cache_path: &Path) -> PathBuf {
    crate::staging::unique_path(cache_path)
}

#[cfg(test)]
thread_local! {
    /// Per-test cache locations. Tests run on their own threads, so audits
    /// under test never read or write the real cache or each other's state.
    static TEST_CACHE_PATHS: std::cell::RefCell<Option<(PathBuf, PathBuf)>> =
        const { std::cell::RefCell::new(None) };
}

/// Points this test thread's pending-grab and no-candidate caches at the
/// given files.
#[cfg(test)]
pub fn use_test_cache_paths(pending_grabs: PathBuf, no_candidates: PathBuf) {
    TEST_CACHE_PATHS.with(|paths| *paths.borrow_mut() = Some((pending_grabs, no_candidates)));
}

#[cfg(test)]
fn test_cache_paths() -> Option<(PathBuf, PathBuf)> {
    TEST_CACHE_PATHS.with(|paths| paths.borrow().clone())
}

fn resolve_pending_grab_cache_path() -> Option<PathBuf> {
    #[cfg(test)]
    return test_cache_paths().map(|(pending, _)| pending);
    #[cfg(not(test))]
    resolve_cache_path().map(|path| path.with_file_name("servarr-language-pending-grabs.json"))
}

fn resolve_no_candidate_cache_path() -> Option<PathBuf> {
    #[cfg(test)]
    if let Some((_, no_candidates)) = test_cache_paths() {
        return Some(no_candidates);
    }
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

    #[test]
    fn no_candidate_cooldown_doubles_up_to_the_cap() {
        assert_eq!(no_candidate_cooldown_days(2, 0, 14), 2);
        assert_eq!(no_candidate_cooldown_days(2, 1, 14), 2);
        assert_eq!(no_candidate_cooldown_days(2, 2, 14), 4);
        assert_eq!(no_candidate_cooldown_days(2, 3, 14), 8);
        assert_eq!(no_candidate_cooldown_days(2, 4, 14), 14);
        assert_eq!(no_candidate_cooldown_days(2, u32::MAX, 14), 14);
        // No cap, or a cap below the base, keeps the base cooldown.
        assert_eq!(no_candidate_cooldown_days(2, 5, 0), 2);
        assert_eq!(no_candidate_cooldown_days(14, 5, 7), 14);
    }

    #[test]
    fn repeated_no_candidate_results_count_consecutive_misses() {
        let tmp = TempDir::new().unwrap();
        let cache_path = tmp.path().join("no-candidates.json");
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string()],
            subtitles: Vec::new(),
        };
        let record = |requirements: &LanguageRequirements| {
            record_no_candidate_at_path(
                &cache_path,
                IntegrationKind::Radarr,
                "movie",
                7,
                requirements,
                "no approved release",
            )
            .unwrap();
            read_no_candidate_records(&cache_path).unwrap()[0].misses
        };

        assert_eq!(record(&requirements), 1);
        assert_eq!(record(&requirements), 2);
        assert_eq!(record(&requirements), 3);
        // New requirements start a fresh streak.
        let different_requirements = LanguageRequirements {
            audio: vec!["jpn".to_string()],
            ..requirements.clone()
        };
        assert_eq!(record(&different_requirements), 1);

        // Records written before misses were tracked count as one miss.
        fs::write(
            &cache_path,
            serde_json::json!([{
                "key": "Radarr:movie:7",
                "kind": "Radarr",
                "target_noun": "movie",
                "target_id": 7,
                "checked_at_unix": 10,
                "requirements": requirements,
                "reason": "no approved release"
            }])
            .to_string(),
        )
        .unwrap();
        assert_eq!(read_no_candidate_records(&cache_path).unwrap()[0].misses, 0);
        assert_eq!(record(&requirements), 2);
    }

    #[test]
    fn pending_grabs_skip_unreadable_entries() {
        let tmp = TempDir::new().unwrap();
        let pending_path = tmp.path().join("pending-grabs.json");
        use_test_cache_paths(pending_path.clone(), tmp.path().join("no-candidates.json"));
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string()],
            subtitles: Vec::new(),
        };
        // Entries from other versions may lack fields or carry extra ones.
        fs::write(
            &pending_path,
            serde_json::json!([
                { "kind": "Sonarr", "target_id": 1 },
                {
                    "kind": "Sonarr",
                    "target_noun": "episode",
                    "target_id": 2,
                    "release_title": "Show S01E02",
                    "grabbed_at_unix": 10,
                    "requirements": requirements,
                    "added_by_newer_version": true
                }
            ])
            .to_string(),
        )
        .unwrap();

        let pending = pending_grabs(IntegrationKind::Sonarr).unwrap();
        assert_eq!(pending.len(), 1);
        assert_eq!(pending[0].target_id, 2);
        assert_eq!(pending[0].guid, None);
        assert!(pending_grabs(IntegrationKind::Radarr).unwrap().is_empty());

        fs::write(&pending_path, "not json").unwrap();
        assert!(pending_grabs(IntegrationKind::Sonarr).unwrap().is_empty());
    }
}
