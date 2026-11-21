//! Best-effort cache for DPN's own Servarr language assessment state.

use super::language::{LanguageCheckReport, LanguageRequirements};
use super::IntegrationKind;
use anyhow::{Context, Result};
use fs2::FileExt;
use log::warn;
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::env;
use std::fs;
use std::fs::{File, OpenOptions};
use std::io::Write;
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
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub present_original_audio: Option<PresentOriginalAudio>,
    #[serde(default)]
    pub recovery_manifest: Option<PathBuf>,
    #[serde(default)]
    pub proof: Option<PendingProof>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PresentOriginalAudio {
    pub language: String,
    pub present: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PendingProof {
    pub sha256: String,
    pub series_id: i64,
    pub indexer_id: i64,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub recovery_checkpoint: Option<super::recovery::RecoveryCheckpoint>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub previous_file: Option<PreviousFileIdentity>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PreviousFileIdentity {
    pub file_id: i64,
    pub path: PathBuf,
    // The most recent history ID is not proven to identify this file.
    pub unverified_history_id: Option<i64>,
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
            present_original_audio: None,
            recovery_manifest: None,
            proof: None,
        }
    }

    fn is_for(&self, kind: IntegrationKind, target_noun: &str, target_id: i64) -> bool {
        self.kind == format!("{:?}", kind)
            && self.target_noun == target_noun
            && self.target_id == target_id
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct GrabIntent {
    pub kind: String,
    pub target_noun: String,
    pub target_id: i64,
    pub release_title: String,
    pub guid: Option<String>,
    #[serde(default)]
    pub indexer_id: Option<i64>,
    pub created_at_unix: u64,
    pub recovery_manifest: Option<PathBuf>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub proof: Option<IntentProof>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub proof_import: Option<ProofImportIntent>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
#[serde(deny_unknown_fields)]
pub struct ProofImportIntent {
    pub queue_id: i64,
    pub download_id: String,
    pub path: PathBuf,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub output_path: Option<PathBuf>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub output_device: Option<u64>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub output_inode: Option<u64>,
    pub device: u64,
    pub inode: u64,
    pub size: u64,
    pub mtime: i64,
    pub mtime_nsec: i64,
    pub ctime: i64,
    pub ctime_nsec: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct IntentProof {
    pub identity: PendingProof,
    pub requirements: LanguageRequirements,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub present_original_audio: Option<PresentOriginalAudio>,
}

impl GrabIntent {
    pub fn new(
        kind: IntegrationKind,
        noun: &str,
        id: i64,
        title: &str,
        guid: Option<String>,
        indexer_id: Option<i64>,
        recovery_manifest: Option<PathBuf>,
    ) -> Self {
        Self {
            kind: format!("{:?}", kind),
            target_noun: noun.to_string(),
            target_id: id,
            release_title: title.to_string(),
            guid,
            indexer_id,
            created_at_unix: now_unix_secs(),
            recovery_manifest,
            proof: None,
            proof_import: None,
        }
    }
}

fn intent_path() -> Result<PathBuf> {
    resolve_search_cache_path()
        .map(|path| path.with_file_name("servarr-language-grab-intents.json"))
        .ok_or_else(|| anyhow::anyhow!("no persistent grab-intent path is available"))
}

fn valid_proof_checkpoint(proof: &PendingProof, manifest: Option<&Path>) -> bool {
    let Some(checkpoint) = proof.recovery_checkpoint.as_ref() else {
        return true;
    };
    checkpoint.is_valid()
        && manifest == Some(checkpoint.manifest_path.as_path())
        && proof
            .previous_file
            .as_ref()
            .is_some_and(|previous| previous.path == checkpoint.media_path)
        && checkpoint.manifest_path.parent().and_then(Path::parent)
            == Some(checkpoint.recovery_root.as_path())
}

fn valid_grab_intent(key: &str, intent: &GrabIntent) -> bool {
    matches!(
        (intent.kind.as_str(), intent.target_noun.as_str()),
        ("Sonarr", "episode") | ("Radarr", "movie")
    ) && intent.target_id > 0
        && !intent.release_title.trim().is_empty()
        && intent.created_at_unix != 0
        && key
            == format!(
                "{}:{}:{}",
                intent.kind, intent.target_noun, intent.target_id
            )
        && intent.proof_import.as_ref().is_none_or(|import| {
            intent
                .proof
                .as_ref()
                .is_some_and(|proof| proof.identity.recovery_checkpoint.is_some())
                && import.queue_id > 0
                && import.inode > 0
                && import.mtime_nsec >= 0
                && import.mtime_nsec < 1_000_000_000
                && import.ctime_nsec >= 0
                && import.ctime_nsec < 1_000_000_000
                && !import.download_id.trim().is_empty()
                && import.path.is_absolute()
                && !import
                    .path
                    .components()
                    .any(|part| part == std::path::Component::ParentDir)
                && match (
                    &import.output_path,
                    import.output_device,
                    import.output_inode,
                ) {
                    (None, None, None) => true,
                    (Some(output), Some(_), Some(inode)) => {
                        inode > 0
                            && output.is_absolute()
                            && !output
                                .components()
                                .any(|part| part == std::path::Component::ParentDir)
                            && import.path.parent() == Some(output.as_path())
                    }
                    _ => false,
                }
        })
        && intent.proof.as_ref().is_none_or(|proof| {
            intent.kind == "Sonarr"
                && intent.target_noun == "episode"
                && intent
                    .guid
                    .as_deref()
                    .is_some_and(|guid| !guid.trim().is_empty())
                && intent
                    .recovery_manifest
                    .as_ref()
                    .is_some_and(|path| !path.as_os_str().is_empty())
                && intent.indexer_id == Some(proof.identity.indexer_id)
                && proof.identity.indexer_id > 0
                && proof.identity.series_id > 0
                && proof.identity.sha256.len() == 64
                && proof.identity.sha256.bytes().all(|b| b.is_ascii_hexdigit())
                && valid_proof_checkpoint(&proof.identity, intent.recovery_manifest.as_deref())
                && proof
                    .identity
                    .previous_file
                    .as_ref()
                    .is_none_or(|previous| {
                        previous.file_id > 0
                            && !previous.path.as_os_str().is_empty()
                            && previous.unverified_history_id.is_none_or(|id| id > 0)
                    })
                && proof.requirements.is_effective()
        })
}

fn read_intents(path: &Path) -> Result<BTreeMap<String, GrabIntent>> {
    match fs::read_to_string(path) {
        Ok(contents) => {
            let intents: BTreeMap<String, GrabIntent> = serde_json::from_str(&contents)
                .with_context(|| format!("parsing grab-intent ledger '{}'", path.display()))?;
            for (key, intent) in &intents {
                if !valid_grab_intent(key, intent) {
                    anyhow::bail!("invalid grab intent in ledger '{}'", path.display());
                }
            }
            Ok(intents)
        }
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(BTreeMap::new()),
        Err(err) => {
            Err(err).with_context(|| format!("reading grab-intent ledger '{}'", path.display()))
        }
    }
}

pub fn get_grab_intent(kind: IntegrationKind, noun: &str, id: i64) -> Result<Option<GrabIntent>> {
    Ok(read_intents(&intent_path()?)?
        .get(&no_candidate_key(kind, noun, id))
        .cloned())
}

pub fn has_grab_intent(kind: IntegrationKind, noun: &str, id: i64) -> Result<bool> {
    Ok(read_intents(&intent_path()?)?.contains_key(&no_candidate_key(kind, noun, id)))
}

pub struct ProofWorkGuard {
    _file: File,
    #[cfg(unix)]
    owner_pid: libc::pid_t,
}

#[cfg(unix)]
impl ProofWorkGuard {
    fn unlock_if_owner(&self) -> std::io::Result<()> {
        if unsafe { libc::getpid() } != self.owner_pid {
            return Ok(());
        }
        loop {
            match FileExt::unlock(&self._file) {
                Err(err) if err.kind() == std::io::ErrorKind::Interrupted => continue,
                result => return result,
            }
        }
    }
}

#[cfg(unix)]
impl Drop for ProofWorkGuard {
    fn drop(&mut self) {
        // Closing alone leaves flock held while a forked child retains the descriptor.
        if let Err(err) = self.unlock_if_owner() {
            log::warn!("failed to release proof-work lock: {err}");
        }
    }
}

/// Serializes cooperative Sonarr proof state transitions with destructive queue work.
/// Acquire this before any per-ledger cache lock; never acquire it while holding one.
pub fn acquire_proof_work_guard() -> Result<ProofWorkGuard> {
    let path = intent_path()?.with_file_name("servarr-language-proof-work.lock");
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)?;
    }
    let file = OpenOptions::new()
        .create(true)
        .read(true)
        .write(true)
        .truncate(false)
        .open(&path)
        .with_context(|| format!("opening proof-work lock '{}'", path.display()))?;
    file.try_lock_exclusive()
        .context("proof or queue work is already in progress")?;
    Ok(ProofWorkGuard {
        _file: file,
        #[cfg(unix)]
        owner_pid: unsafe { libc::getpid() },
    })
}

/// Queue cleanup cannot safely attribute an unknown/shared download while proof work is pending.
pub fn has_proof_recovery_work(kind: IntegrationKind) -> Result<bool> {
    let pending = pending_grabs_strict(kind)?;
    let intents = read_intents(&intent_path()?)?;
    Ok(pending.iter().any(|grab| grab.proof.is_some())
        || intents
            .values()
            .any(|intent| intent.kind == format!("{:?}", kind) && intent.proof.is_some()))
}

/// Persist the confirmed download binding before clearing the proof import intent.
pub fn finish_proof_import(
    kind: IntegrationKind,
    expected: &PendingGrab,
    intent: &GrabIntent,
) -> Result<()> {
    let _guard = acquire_proof_work_guard()?;
    let import = intent
        .proof_import
        .as_ref()
        .context("intent is not a proof import")?;
    if get_grab_intent(kind, &intent.target_noun, intent.target_id)?.as_ref() != Some(intent)
        || !expected.is_for(kind, &intent.target_noun, intent.target_id)
        || expected.release_title != intent.release_title
        || expected.guid != intent.guid
        || expected.recovery_manifest != intent.recovery_manifest
        || intent.proof.as_ref().is_none_or(|proof| {
            expected.proof.as_ref() != Some(&proof.identity)
                || expected.requirements != proof.requirements
                || expected.present_original_audio != proof.present_original_audio
        })
    {
        anyhow::bail!("proof import intent or pending target changed");
    }
    let path = resolve_pending_grab_cache_path().context("no pending-grab cache path")?;
    with_cache_lock(&path, "language pending-grab cache", || {
        let mut records = read_pending_grabs_strict(&path)?;
        let current = records
            .iter_mut()
            .find(|record| record.is_for(kind, &intent.target_noun, intent.target_id))
            .context("proof pending record disappeared")?;
        let mut bound = expected.clone();
        if bound
            .download_id
            .as_deref()
            .is_some_and(|id| !id.eq_ignore_ascii_case(&import.download_id))
        {
            anyhow::bail!("proof pending download conflicts with intent");
        }
        bound.download_id = Some(import.download_id.clone());
        if current != expected && current != &bound {
            anyhow::bail!("proof pending record changed before download binding");
        }
        *current = bound;
        // A previous rename may have succeeded before a directory sync failed.
        // Re-sync even an already-bound record before clearing the intent.
        write_intents_durably(&path, &records)?;
        Ok(())
    })?;
    #[cfg(all(test, unix))]
    durable_write_fault("after-pending")?;
    remove_grab_intent_inner(kind, intent)
}

pub fn create_grab_intent(kind: IntegrationKind, intent: &GrabIntent) -> Result<()> {
    let _guard = if intent.proof.is_some() {
        Some(acquire_proof_work_guard()?)
    } else {
        None
    };
    create_grab_intent_inner(kind, intent)
}

fn create_grab_intent_inner(kind: IntegrationKind, intent: &GrabIntent) -> Result<()> {
    let path = intent_path()?;
    let key = no_candidate_key(kind, &intent.target_noun, intent.target_id);
    if !valid_grab_intent(&key, intent) {
        anyhow::bail!("refusing to persist invalid grab intent");
    }
    with_cache_lock(&path, "grab-intent ledger", || {
        let mut intents = read_intents(&path)?;
        if intents.contains_key(&key) {
            anyhow::bail!(
                "unresolved grab intent for {} {} requires review",
                intent.target_noun,
                intent.target_id
            );
        }
        intents.insert(key, intent.clone());
        write_intents_durably(&path, &intents)
    })
}

pub fn remove_grab_intent(kind: IntegrationKind, intent: &GrabIntent) -> Result<()> {
    let _guard = if intent.proof.is_some() {
        Some(acquire_proof_work_guard()?)
    } else {
        None
    };
    remove_grab_intent_inner(kind, intent)
}

fn remove_grab_intent_inner(kind: IntegrationKind, intent: &GrabIntent) -> Result<()> {
    let path = intent_path()?;
    let key = no_candidate_key(kind, &intent.target_noun, intent.target_id);
    with_cache_lock(&path, "grab-intent ledger", || {
        let mut intents = read_intents(&path)?;
        if intents.get(&key) != Some(intent) {
            anyhow::bail!("grab intent changed before completion");
        }
        intents.remove(&key);
        write_intents_durably(&path, &intents)
    })
}

#[cfg(all(test, unix))]
thread_local! {
    static DURABLE_WRITE_FAULT: std::cell::Cell<Option<&'static str>> = const { std::cell::Cell::new(None) };
}

#[cfg(all(test, unix))]
pub(super) fn set_durable_write_fault(phase: Option<&'static str>) {
    DURABLE_WRITE_FAULT.with(|fault| fault.set(phase));
}

#[cfg(all(test, unix))]
fn durable_write_fault(phase: &str) -> Result<()> {
    if DURABLE_WRITE_FAULT.with(|fault| fault.get() == Some(phase)) {
        anyhow::bail!("injected durable-write failure at {phase}");
    }
    Ok(())
}

#[cfg(unix)]
fn write_intents_durably<T: Serialize>(path: &Path, intents: &T) -> Result<()> {
    let tmp = unique_tmp_path(path);
    let result = (|| -> Result<()> {
        let mut file = OpenOptions::new().write(true).create_new(true).open(&tmp)?;
        file.write_all(&serde_json::to_vec_pretty(intents)?)?;
        file.sync_all()?;
        #[cfg(test)]
        durable_write_fault("before-rename")?;
        fs::rename(&tmp, path)?;
        #[cfg(test)]
        durable_write_fault("after-rename")?;
        let absolute = if path.is_absolute() {
            path.to_path_buf()
        } else {
            std::env::current_dir()?.join(path)
        };
        let mut parent = absolute.parent().unwrap_or_else(|| Path::new("."));
        loop {
            File::open(parent)?.sync_all()?;
            match parent
                .parent()
                .filter(|next| !next.as_os_str().is_empty() && *next != parent)
            {
                Some(next) => parent = next,
                None => break,
            }
        }
        Ok(())
    })();
    if result.is_err() && tmp.exists() {
        crate::staging::discard(&tmp);
    }
    result.with_context(|| format!("writing durable grab-intent ledger '{}'", path.display()))
}

#[cfg(windows)]
fn write_intents_durably<T: Serialize>(path: &Path, intents: &T) -> Result<()> {
    use std::os::windows::ffi::OsStrExt;
    #[link(name = "kernel32")]
    extern "system" {
        fn MoveFileExW(existing: *const u16, replacement: *const u16, flags: u32) -> i32;
    }
    const MOVEFILE_REPLACE_EXISTING: u32 = 1;
    const MOVEFILE_WRITE_THROUGH: u32 = 8;
    let tmp = unique_tmp_path(path);
    let result = (|| -> Result<()> {
        let mut file = OpenOptions::new().write(true).create_new(true).open(&tmp)?;
        file.write_all(&serde_json::to_vec_pretty(intents)?)?;
        file.sync_all()?;
        drop(file);
        let source: Vec<u16> = tmp.as_os_str().encode_wide().chain(Some(0)).collect();
        let dest: Vec<u16> = path.as_os_str().encode_wide().chain(Some(0)).collect();
        // MoveFileExW replaces the destination without first deleting it.
        if unsafe {
            MoveFileExW(
                source.as_ptr(),
                dest.as_ptr(),
                MOVEFILE_REPLACE_EXISTING | MOVEFILE_WRITE_THROUGH,
            )
        } == 0
        {
            return Err(std::io::Error::last_os_error()).context("replacing Windows safety state");
        }
        Ok(())
    })();
    if result.is_err() && tmp.exists() {
        crate::staging::discard(&tmp);
    }
    if result.is_ok() {
        warn!("Windows safety state uses a flushed replacement without Unix directory-sync guarantees; recovery mode is unavailable.");
    }
    result
}

#[cfg(not(any(unix, windows)))]
fn write_intents_durably<T: Serialize>(_path: &Path, _intents: &T) -> Result<()> {
    anyhow::bail!("durable grab-intent ledger is unsupported on this platform")
}

/// Process-wide switch that turns every cache write into a no-op. Set once by `--dry-run`.
static WRITES_DISABLED: AtomicBool = AtomicBool::new(false);

/// Stops this process from writing the language cache. Reads keep working.
pub fn disable_writes() {
    WRITES_DISABLED.store(true, Ordering::SeqCst);
}

pub(crate) fn writes_disabled() -> bool {
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

/// Days left on a target's no-candidate cooldown. With `backoff`, repeated
/// misses lengthen it (see [`set_no_candidate_max_cooldown_days`]); without,
/// it stays at `cooldown_days`.
pub fn no_candidate_cooldown_remaining_days(
    kind: IntegrationKind,
    target_noun: &str,
    target_id: i64,
    requirements: &LanguageRequirements,
    cooldown_days: u32,
    backoff: bool,
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
    let max_days = if backoff {
        NO_CANDIDATE_MAX_COOLDOWN_DAYS.load(Ordering::SeqCst)
    } else {
        cooldown_days
    };
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

#[derive(Debug, Clone, Serialize, Deserialize)]
struct PreflightAttempt {
    attempted_at: u64,
    retry_after: u64,
}

const PREFLIGHT_RETRY_SECS: u64 = 5 * 60;
const PREFLIGHT_LEASE_SECS: u64 = 60 * 60;

pub struct PreflightClaim {
    pub token: u64,
    _target_lock: Option<File>,
}

fn preflight_lock_contended(err: &std::io::Error) -> bool {
    matches!(
        (err.raw_os_error(), fs2::lock_contended_error().raw_os_error()),
        (Some(actual), Some(expected)) if actual == expected
    )
}

/// Claims a target before its import-history lookup. A failed lookup remains on
/// a short cooldown; an abandoned claim expires. A per-target file lock keeps
/// an active claim exclusive even when its lease expires.
pub fn claim_preflight(
    kind: IntegrationKind,
    noun: &str,
    id: i64,
    dry_run: bool,
) -> Result<Option<PreflightClaim>> {
    let Some(path) = resolve_preflight_cache_path() else {
        if dry_run || writes_disabled() {
            return Ok(Some(PreflightClaim {
                token: 0,
                _target_lock: None,
            }));
        }
        return Err(anyhow::anyhow!(
            "no persistent language preflight cache path is available"
        ));
    };
    if dry_run || writes_disabled() {
        let records = read_preflight_attempts(&path)?;
        let key = no_candidate_key(kind, noun, id);
        return Ok(records
            .get(&key)
            .is_none_or(|record| record.retry_after <= now_unix_secs())
            .then_some(PreflightClaim {
                token: 0,
                _target_lock: None,
            }));
    }
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("creating preflight lock directory '{}'", parent.display()))?;
    }
    if !matches!(
        (kind, noun),
        (IntegrationKind::Sonarr, "episode") | (IntegrationKind::Radarr, "movie")
    ) {
        return Err(anyhow::anyhow!("invalid preflight target"));
    }
    let lock_path = path.with_file_name(format!(
        "servarr-language-preflight-{kind:?}-{noun}-{id}.lock"
    ));
    let lock = OpenOptions::new()
        .create(true)
        .read(true)
        .write(true)
        .truncate(false)
        .open(&lock_path)
        .with_context(|| format!("opening preflight target lock '{}'", lock_path.display()))?;
    match FileExt::try_lock_exclusive(&lock) {
        Ok(()) => {}
        Err(err) if preflight_lock_contended(&err) => return Ok(None),
        Err(err) => {
            return Err(err)
                .with_context(|| format!("locking preflight target '{}'", lock_path.display()))
        }
    }
    with_cache_lock(&path, "language preflight cache", || {
        let mut records = read_preflight_attempts(&path)?;
        let key = no_candidate_key(kind, noun, id);
        let now = now_unix_secs();
        if records
            .get(&key)
            .is_some_and(|record| record.retry_after > now)
        {
            return Ok(None);
        }
        let token = records
            .values()
            .map(|record| record.attempted_at)
            .max()
            .unwrap_or_default()
            .saturating_add(1)
            .max(now);
        records.insert(
            key,
            PreflightAttempt {
                attempted_at: token,
                retry_after: now.saturating_add(PREFLIGHT_LEASE_SECS),
            },
        );
        write_json_atomically(&path, "language preflight cache", &records)?;
        Ok(Some(PreflightClaim {
            token,
            _target_lock: Some(lock),
        }))
    })
}

pub fn finish_preflight(
    kind: IntegrationKind,
    noun: &str,
    id: i64,
    token: u64,
    success: bool,
) -> Result<()> {
    if writes_disabled() || token == 0 {
        return Ok(());
    }
    let Some(path) = resolve_preflight_cache_path() else {
        return Ok(());
    };
    with_cache_lock(&path, "language preflight cache", || {
        let mut records = read_preflight_attempts(&path)?;
        let key = no_candidate_key(kind, noun, id);
        if let Some(record) = records
            .get_mut(&key)
            .filter(|record| record.attempted_at == token)
        {
            record.retry_after = if success {
                0
            } else {
                now_unix_secs().saturating_add(PREFLIGHT_RETRY_SECS)
            };
            write_json_atomically(&path, "language preflight cache", &records)?;
        }
        Ok(())
    })
}

pub fn preflight_attempts(kind: IntegrationKind, noun: &str) -> Result<HashMap<i64, u64>> {
    let Some(path) = resolve_preflight_cache_path() else {
        return Ok(HashMap::new());
    };
    let prefix = format!("{:?}:{}:", kind, noun);
    Ok(read_preflight_attempts(&path)?
        .into_iter()
        .filter_map(|(key, record)| {
            Some((
                key.strip_prefix(&prefix)?.parse().ok()?,
                record.attempted_at,
            ))
        })
        .collect())
}

fn read_preflight_attempts(path: &Path) -> Result<BTreeMap<String, PreflightAttempt>> {
    match fs::read_to_string(path) {
        Ok(contents) => serde_json::from_str(&contents)
            .with_context(|| format!("parsing language preflight cache '{}'", path.display())),
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(BTreeMap::new()),
        Err(err) => Err(err)
            .with_context(|| format!("reading language preflight cache '{}'", path.display())),
    }
}

/// Remembers that the audit searched releases for a target, so later runs
/// can start with the targets that have waited longest.
pub fn record_search(kind: IntegrationKind, target_noun: &str, target_id: i64) -> Result<()> {
    if writes_disabled() {
        return Ok(());
    }
    let Some(cache_path) = resolve_search_cache_path() else {
        return Err(anyhow::anyhow!(
            "no persistent language search cache path is available"
        ));
    };
    with_cache_lock(&cache_path, "language search cache", || {
        let mut searches = read_searches(&cache_path)?;
        searches.insert(
            no_candidate_key(kind, target_noun, target_id),
            now_unix_secs(),
        );
        write_json_atomically(&cache_path, "language search cache", &searches)
    })
}

/// When the audit last searched each `target_noun` of `kind`, by target id.
pub fn last_searches(kind: IntegrationKind, target_noun: &str) -> Result<HashMap<i64, u64>> {
    let Some(cache_path) = resolve_search_cache_path() else {
        return Ok(HashMap::new());
    };
    let prefix = format!("{:?}:{}:", kind, target_noun);
    Ok(read_searches(&cache_path)?
        .into_iter()
        .filter_map(|(key, searched_at)| {
            let id = key.strip_prefix(&prefix)?.parse().ok()?;
            Some((id, searched_at))
        })
        .collect())
}

/// Remembers a replacement grabbed by the audit; replaces any earlier
/// pending grab of the same target.
pub fn record_pending_grab(grab: PendingGrab) -> Result<()> {
    if writes_disabled() {
        return Ok(());
    }
    let _guard = acquire_proof_work_guard()?;
    let cache_path = resolve_pending_grab_cache_path().ok_or_else(|| {
        anyhow::anyhow!("no persistent language pending-grab cache path is available")
    })?;
    with_cache_lock(&cache_path, "language pending-grab cache", || {
        let mut records = read_pending_grabs_strict(&cache_path)?;
        if records.iter().any(|record| {
            record.kind == grab.kind
                && record.target_noun == grab.target_noun
                && record.target_id == grab.target_id
                && record.proof.is_some()
                && record != &grab
        }) {
            anyhow::bail!("refusing to replace an existing proof pending record");
        }
        records.retain(|record| {
            record.kind != grab.kind
                || record.target_noun != grab.target_noun
                || record.target_id != grab.target_id
        });
        records.push(grab);
        write_intents_durably(&cache_path, &records)
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

pub fn pending_grabs_strict(kind: IntegrationKind) -> Result<Vec<PendingGrab>> {
    let path = resolve_pending_grab_cache_path().ok_or_else(|| {
        anyhow::anyhow!("no persistent language pending-grab cache path is available")
    })?;
    let mut records = read_pending_grabs_strict(&path)?;
    records.retain(|record| record.kind == format!("{:?}", kind));
    records.sort_by_key(|record| record.grabbed_at_unix);
    Ok(records)
}

fn read_pending_grabs_strict(path: &Path) -> Result<Vec<PendingGrab>> {
    let contents = match fs::read_to_string(path) {
        Ok(contents) => contents,
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(Vec::new()),
        Err(err) => {
            return Err(err).with_context(|| {
                format!("reading language pending-grab cache '{}'", path.display())
            })
        }
    };
    let records: Vec<PendingGrab> = serde_json::from_str(&contents)
        .with_context(|| format!("parsing language pending-grab cache '{}'", path.display()))?;
    let mut targets = std::collections::BTreeSet::new();
    for record in &records {
        if !matches!(
            (record.kind.as_str(), record.target_noun.as_str()),
            ("Sonarr", "episode") | ("Radarr", "movie")
        ) || record.target_id <= 0
            || record.release_title.trim().is_empty()
            || record.grabbed_at_unix == 0
            || record.proof.as_ref().is_some_and(|proof| {
                record.kind != "Sonarr"
                    || !record.requirements.is_effective()
                    || proof.series_id <= 0
                    || proof.indexer_id <= 0
                    || proof.sha256.len() != 64
                    || !proof.sha256.bytes().all(|b| b.is_ascii_hexdigit())
                    || !valid_proof_checkpoint(proof, record.recovery_manifest.as_deref())
                    || proof.previous_file.as_ref().is_some_and(|previous| {
                        previous.file_id <= 0
                            || previous.path.as_os_str().is_empty()
                            || previous.unverified_history_id.is_some_and(|id| id <= 0)
                    })
                    || record
                        .guid
                        .as_deref()
                        .is_none_or(|guid| guid.trim().is_empty())
            })
            || !targets.insert((&record.kind, &record.target_noun, record.target_id))
        {
            anyhow::bail!("invalid pending grab in cache '{}'", path.display());
        }
    }
    Ok(records)
}

/// Reads pending grabs strictly for an audit safety decision. An unreadable
/// entry cannot safely be attributed to a different target, so it is an error.
pub fn has_pending_grab_for_target(
    kind: IntegrationKind,
    target_noun: &str,
    target_id: i64,
) -> Result<bool> {
    Ok(pending_grabs_strict(kind)?
        .iter()
        .any(|record| record.is_for(kind, target_noun, target_id)))
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
    let _guard = acquire_proof_work_guard()?;
    let cache_path = resolve_pending_grab_cache_path().ok_or_else(|| {
        anyhow::anyhow!("no persistent language pending-grab cache path is available")
    })?;
    with_cache_lock(&cache_path, "language pending-grab cache", || {
        let mut records = read_pending_grabs_strict(&cache_path)?;
        if records.iter().any(|record| {
            record.is_for(kind, target_noun, target_id)
                && record.grabbed_at_unix == grabbed_at_unix
                && record.proof.is_some()
        }) && has_grab_intent(kind, target_noun, target_id)?
        {
            anyhow::bail!("cannot remove proof pending while an intent exists");
        }
        records.retain(|record| {
            !(record.is_for(kind, target_noun, target_id)
                && record.grabbed_at_unix == grabbed_at_unix)
        });
        write_intents_durably(&cache_path, &records)
    })
}

/// Removes only the exact verified pending record, under the cache lock.
pub fn remove_pending_grab_exact(kind: IntegrationKind, expected: &PendingGrab) -> Result<bool> {
    if writes_disabled() {
        return Ok(false);
    }
    let _guard = acquire_proof_work_guard()?;
    let cache_path = resolve_pending_grab_cache_path().ok_or_else(|| {
        anyhow::anyhow!("no persistent language pending-grab cache path is available")
    })?;
    with_cache_lock(&cache_path, "language pending-grab cache", || {
        let mut records = read_pending_grabs_strict(&cache_path)?;
        let Some(index) = records.iter().position(|record| {
            record.is_for(kind, &expected.target_noun, expected.target_id) && record == expected
        }) else {
            return Ok(false);
        };
        if expected.proof.is_some()
            && has_grab_intent(kind, &expected.target_noun, expected.target_id)?
        {
            anyhow::bail!("cannot remove proof pending while an intent exists");
        }
        records.remove(index);
        write_intents_durably(&cache_path, &records)?;
        Ok(true)
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
        Ok(contents) => serde_json::from_str(&contents)
            .with_context(|| format!("parsing language no-candidate cache '{}'", path.display())),
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(Vec::new()),
        Err(err) => Err(err)
            .with_context(|| format!("reading language no-candidate cache '{}'", path.display())),
    }
}

fn read_searches(path: &Path) -> Result<BTreeMap<String, u64>> {
    match fs::read_to_string(path) {
        Ok(contents) => serde_json::from_str(&contents)
            .with_context(|| format!("parsing language search cache '{}'", path.display())),
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => Ok(BTreeMap::new()),
        Err(err) => {
            Err(err).with_context(|| format!("reading language search cache '{}'", path.display()))
        }
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
    static DEFAULT_TEST_CACHE_DIR: tempfile::TempDir = tempfile::TempDir::new().expect("test cache directory");
}

/// Points this test thread's pending-grab and no-candidate caches at the
/// given files.
#[cfg(test)]
pub fn use_test_cache_paths(pending_grabs: PathBuf, no_candidates: PathBuf) {
    TEST_CACHE_PATHS.with(|paths| *paths.borrow_mut() = Some((pending_grabs, no_candidates)));
}

#[cfg(test)]
pub fn ensure_test_cache_paths() {
    if test_cache_paths().is_none() {
        DEFAULT_TEST_CACHE_DIR.with(|dir| {
            use_test_cache_paths(
                dir.path().join("pending.json"),
                dir.path().join("no-candidates.json"),
            );
        });
    }
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

fn resolve_search_cache_path() -> Option<PathBuf> {
    #[cfg(test)]
    return test_cache_paths()
        .map(|(_, no_candidates)| no_candidates.with_file_name("servarr-language-searches.json"));
    #[cfg(not(test))]
    resolve_cache_path().map(|path| path.with_file_name("servarr-language-searches.json"))
}

fn resolve_preflight_cache_path() -> Option<PathBuf> {
    resolve_search_cache_path().map(|path| path.with_file_name("servarr-language-preflights.json"))
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
    #[cfg(windows)]
    if let Some(cache_home) = env::var_os("LOCALAPPDATA") {
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

    #[cfg(any(unix, windows))]
    #[test]
    fn grab_intent_round_trip_and_corruption_fail_closed() {
        let dir = TempDir::new().unwrap();
        use_test_cache_paths(
            dir.path().join("pending.json"),
            dir.path().join("no-candidates.json"),
        );
        let intent = GrabIntent::new(
            IntegrationKind::Sonarr,
            "episode",
            77,
            "release",
            Some("guid".into()),
            Some(5),
            Some(dir.path().join("manifest.json")),
        );
        assert!(!has_grab_intent(IntegrationKind::Sonarr, "episode", 77).unwrap());
        create_grab_intent(IntegrationKind::Sonarr, &intent).unwrap();
        assert!(has_grab_intent(IntegrationKind::Sonarr, "episode", 77).unwrap());
        assert!(create_grab_intent(IntegrationKind::Sonarr, &intent).is_err());
        assert_eq!(
            read_intents(&intent_path().unwrap()).unwrap()["Sonarr:episode:77"],
            intent
        );
        remove_grab_intent(IntegrationKind::Sonarr, &intent).unwrap();
        assert!(!has_grab_intent(IntegrationKind::Sonarr, "episode", 77).unwrap());
        fs::write(intent_path().unwrap(), "broken").unwrap();
        assert!(create_grab_intent(IntegrationKind::Sonarr, &intent).is_err());
        assert!(has_grab_intent(IntegrationKind::Sonarr, "episode", 77).is_err());
    }

    #[cfg(any(unix, windows))]
    #[test]
    fn proof_intent_legacy_roundtrip_and_malformed_data_fail_closed() {
        let dir = TempDir::new().unwrap();
        use_test_cache_paths(
            dir.path().join("pending.json"),
            dir.path().join("no-candidates.json"),
        );
        let mut intent = GrabIntent::new(
            IntegrationKind::Sonarr,
            "episode",
            77,
            "release",
            Some("guid".into()),
            Some(5),
            Some(dir.path().join("manifest.json")),
        );
        let legacy = serde_json::to_value(&intent).unwrap();
        assert!(legacy.get("proof").is_none());
        assert_eq!(
            serde_json::from_value::<GrabIntent>(legacy).unwrap(),
            intent
        );
        intent.proof = Some(IntentProof {
            identity: PendingProof {
                sha256: "a".repeat(64),
                series_id: 9,
                indexer_id: 5,
                recovery_checkpoint: None,
                previous_file: None,
            },
            requirements: LanguageRequirements {
                enabled: true,
                audio: vec!["eng".into()],
                subtitles: vec!["eng".into()],
            },
            present_original_audio: None,
        });
        let legacy_proof = serde_json::to_value(&intent).unwrap();
        assert!(legacy_proof["proof"]["identity"]
            .get("recovery_checkpoint")
            .is_none());
        assert_eq!(
            serde_json::from_value::<GrabIntent>(legacy_proof).unwrap(),
            intent
        );
        intent.recovery_manifest = Some(dir.path().join(".dpn-recovery-test/manifest.json"));
        intent.proof.as_mut().unwrap().identity.previous_file = Some(PreviousFileIdentity {
            file_id: 55,
            path: dir.path().join("media.mkv"),
            unverified_history_id: None,
        });
        intent.proof.as_mut().unwrap().identity.recovery_checkpoint =
            Some(super::super::recovery::RecoveryCheckpoint {
                version: 1,
                manifest_sha256: "b".repeat(64),
                manifest_size: 123,
                recovery_root: dir.path().to_path_buf(),
                manifest_path: dir.path().join(".dpn-recovery-test/manifest.json"),
                media_path: dir.path().join("media.mkv"),
                root_device: 1,
                root_inode: 2,
                directory_device: 1,
                directory_inode: 3,
                manifest_device: 1,
                manifest_inode: 4,
                sidecars: None,
                blobs: vec![super::super::recovery::RecoveryBlobIdentity {
                    device: 1,
                    inode: 5,
                }],
            });
        let mut invalid = intent.clone();
        invalid
            .proof
            .as_mut()
            .unwrap()
            .identity
            .recovery_checkpoint
            .as_mut()
            .unwrap()
            .media_path = dir.path().join("other.mkv");
        assert!(create_grab_intent(IntegrationKind::Sonarr, &invalid).is_err());
        invalid = intent.clone();
        invalid
            .proof
            .as_mut()
            .unwrap()
            .identity
            .recovery_checkpoint
            .as_mut()
            .unwrap()
            .blobs[0]
            .inode = 0;
        assert!(create_grab_intent(IntegrationKind::Sonarr, &invalid).is_err());
        invalid = intent.clone();
        invalid.proof.as_mut().unwrap().identity.sha256 = "invalid".into();
        assert!(create_grab_intent(IntegrationKind::Sonarr, &invalid).is_err());
        assert!(!has_grab_intent(IntegrationKind::Sonarr, "episode", 77).unwrap());
        create_grab_intent(IntegrationKind::Sonarr, &intent).unwrap();
        assert_eq!(
            get_grab_intent(IntegrationKind::Sonarr, "episode", 77).unwrap(),
            Some(intent.clone())
        );
        let mut pending = PendingGrab::new(
            IntegrationKind::Sonarr,
            "episode",
            77,
            "release",
            Some("guid".into()),
            &intent.proof.as_ref().unwrap().requirements,
        );
        pending.recovery_manifest = intent.recovery_manifest.clone();
        pending.proof = Some(intent.proof.as_ref().unwrap().identity.clone());
        record_pending_grab(pending.clone()).unwrap();
        assert_eq!(
            pending_grabs_strict(IntegrationKind::Sonarr).unwrap(),
            vec![pending]
        );
        let path = intent_path().unwrap();
        let mut raw: serde_json::Value =
            serde_json::from_str(&fs::read_to_string(&path).unwrap()).unwrap();
        raw["Sonarr:episode:77"]["proof"]["identity"]["sha256"] = serde_json::json!("invalid");
        fs::write(&path, serde_json::to_string(&raw).unwrap()).unwrap();
        assert!(has_grab_intent(IntegrationKind::Sonarr, "episode", 77).is_err());
        assert!(get_grab_intent(IntegrationKind::Sonarr, "episode", 77).is_err());
    }

    #[test]
    fn preflight_claim_requires_a_cache_path() {
        TEST_CACHE_PATHS.with(|paths| *paths.borrow_mut() = None);
        assert!(claim_preflight(IntegrationKind::Sonarr, "episode", 77, false).is_err());
        assert!(
            claim_preflight(IntegrationKind::Sonarr, "episode", 77, true)
                .unwrap()
                .is_some()
        );
    }

    #[test]
    fn corrupt_preflight_cache_does_not_discard_claims() {
        let dir = TempDir::new().unwrap();
        use_test_cache_paths(
            dir.path().join("pending.json"),
            dir.path().join("no-candidates.json"),
        );
        let path = resolve_preflight_cache_path().unwrap();
        fs::write(&path, "not json").unwrap();
        assert!(claim_preflight(IntegrationKind::Sonarr, "episode", 77, false).is_err());
        assert_eq!(fs::read_to_string(path).unwrap(), "not json");
    }

    #[test]
    fn preflight_lock_contention_is_platform_specific() {
        let dir = TempDir::new().unwrap();
        let path = dir.path().join("target.lock");
        let first = File::create(&path).unwrap();
        let second = OpenOptions::new()
            .read(true)
            .write(true)
            .open(&path)
            .unwrap();
        FileExt::try_lock_exclusive(&first).unwrap();
        let err = FileExt::try_lock_exclusive(&second).unwrap_err();
        assert!(preflight_lock_contended(&err));
        assert_eq!(
            err.raw_os_error(),
            fs2::lock_contended_error().raw_os_error()
        );
        assert!(!preflight_lock_contended(&std::io::Error::new(
            std::io::ErrorKind::PermissionDenied,
            "unrelated error",
        )));
        assert!(!preflight_lock_contended(&std::io::Error::new(
            std::io::ErrorKind::WouldBlock,
            "no operating system error",
        )));
        #[cfg(windows)]
        assert!(preflight_lock_contended(
            &std::io::Error::from_raw_os_error(33)
        ));
        drop(first);
        FileExt::try_lock_exclusive(&second).unwrap();
    }

    #[test]
    fn preflight_target_lock_open_failure_is_not_contention() {
        let dir = TempDir::new().unwrap();
        use_test_cache_paths(
            dir.path().join("pending.json"),
            dir.path().join("no-candidates.json"),
        );
        let path = resolve_preflight_cache_path().unwrap();
        let lock_path = path.with_file_name("servarr-language-preflight-Sonarr-episode-77.lock");
        fs::create_dir(&lock_path).unwrap();
        let err = claim_preflight(IntegrationKind::Sonarr, "episode", 77, false)
            .err()
            .expect("opening a directory as a lock must fail");
        assert!(err.to_string().contains("opening preflight target lock"));
        assert!(!path.exists());
    }

    #[test]
    fn preflight_claim_is_shared_and_failed_attempts_back_off() {
        let dir = TempDir::new().unwrap();
        let pending = dir.path().join("pending.json");
        let no_candidates = dir.path().join("no-candidates.json");
        let barrier = std::sync::Arc::new(std::sync::Barrier::new(8));
        let workers: Vec<_> = (0..8)
            .map(|_| {
                let pending = pending.clone();
                let no_candidates = no_candidates.clone();
                let barrier = barrier.clone();
                std::thread::spawn(move || {
                    use_test_cache_paths(pending, no_candidates);
                    barrier.wait();
                    claim_preflight(IntegrationKind::Sonarr, "episode", 77, false).unwrap()
                })
            })
            .collect();
        let mut claims: Vec<_> = workers
            .into_iter()
            .filter_map(|worker| worker.join().unwrap())
            .collect();
        assert_eq!(claims.len(), 1);
        use_test_cache_paths(pending, no_candidates);
        let first = claims.pop().unwrap();
        let first_token = first.token;
        let path = resolve_preflight_cache_path().unwrap();
        let mut records = read_preflight_attempts(&path).unwrap();
        records.get_mut("Sonarr:episode:77").unwrap().retry_after = 0;
        write_json_atomically(&path, "language preflight cache", &records).unwrap();
        assert!(
            claim_preflight(IntegrationKind::Sonarr, "episode", 77, false)
                .unwrap()
                .is_none()
        );
        finish_preflight(IntegrationKind::Sonarr, "episode", 77, first_token, false).unwrap();
        drop(first);
        assert!(
            claim_preflight(IntegrationKind::Sonarr, "episode", 77, false)
                .unwrap()
                .is_none()
        );
        let attempts = preflight_attempts(IntegrationKind::Sonarr, "episode").unwrap();
        assert_eq!(attempts.get(&77), Some(&first_token));
        let mut records = read_preflight_attempts(&path).unwrap();
        records.get_mut("Sonarr:episode:77").unwrap().retry_after = 0;
        write_json_atomically(&path, "language preflight cache", &records).unwrap();
        let newer = claim_preflight(IntegrationKind::Sonarr, "episode", 77, false)
            .unwrap()
            .unwrap();
        assert!(newer.token > first_token);
        finish_preflight(IntegrationKind::Sonarr, "episode", 77, first_token, true).unwrap();
        assert!(
            claim_preflight(IntegrationKind::Sonarr, "episode", 77, false)
                .unwrap()
                .is_none()
        );
        finish_preflight(IntegrationKind::Sonarr, "episode", 77, newer.token, true).unwrap();
        drop(newer);
        assert!(
            claim_preflight(IntegrationKind::Sonarr, "episode", 77, false)
                .unwrap()
                .is_some()
        );
    }

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
            true,
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
                true,
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

    #[cfg(unix)]
    #[test]
    #[ignore = "child process for cooperative proof-work lock test"]
    fn proof_work_guard_child() {
        let Some(dir) = std::env::var_os("DPN_TEST_PROOF_WORK_DIR") else {
            return;
        };
        let dir = PathBuf::from(dir);
        use_test_cache_paths(dir.join("pending.json"), dir.join("no-candidates.json"));
        let available = acquire_proof_work_guard().is_ok();
        assert_eq!(
            available,
            std::env::var("DPN_TEST_PROOF_WORK_AVAILABLE").unwrap() == "yes"
        );
    }

    #[cfg(unix)]
    #[test]
    fn proof_work_guard_is_exclusive_across_processes() {
        let tmp = TempDir::new().unwrap();
        use_test_cache_paths(
            tmp.path().join("pending.json"),
            tmp.path().join("no-candidates.json"),
        );
        let run_child = |available: &str| {
            std::process::Command::new(std::env::current_exe().unwrap())
                .args([
                    "--exact",
                    "servarr::cache::tests::proof_work_guard_child",
                    "--ignored",
                ])
                .env("DPN_TEST_PROOF_WORK_DIR", tmp.path())
                .env("DPN_TEST_PROOF_WORK_AVAILABLE", available)
                .status()
                .unwrap()
                .success()
        };
        let guard = acquire_proof_work_guard().unwrap();
        assert!(run_child("no"));
        drop(guard);
        assert!(run_child("yes"));
    }

    #[cfg(unix)]
    #[test]
    fn proof_work_guard_releases_when_fork_child_keeps_descriptor() {
        const CHILD: &str = "DPN_PROOF_WORK_FORK_TEST";
        if std::env::var_os(CHILD).is_none() {
            let status = std::process::Command::new(std::env::current_exe().unwrap())
                .args([
                    "--exact",
                    "servarr::cache::tests::proof_work_guard_releases_when_fork_child_keeps_descriptor",
                    "--nocapture",
                ])
                .env(CHILD, "1")
                .status()
                .unwrap();
            assert!(status.success());
            return;
        }

        use std::io::Read;
        use std::os::fd::FromRawFd;
        let dir = TempDir::new().unwrap();
        use_test_cache_paths(
            dir.path().join("pending.json"),
            dir.path().join("no-candidates.json"),
        );
        let requirements = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".into()],
            subtitles: vec![],
        };
        let proof = PendingProof {
            sha256: "a".repeat(64),
            series_id: 9,
            indexer_id: 3,
            recovery_checkpoint: None,
            previous_file: None,
        };
        let mut intent = GrabIntent::new(
            IntegrationKind::Sonarr,
            "episode",
            77,
            "proof release",
            Some("guid".into()),
            Some(3),
            Some(dir.path().join("manifest.json")),
        );
        intent.proof = Some(IntentProof {
            identity: proof.clone(),
            requirements: requirements.clone(),
            present_original_audio: None,
        });
        create_grab_intent(IntegrationKind::Sonarr, &intent).unwrap();
        let mut pending = PendingGrab::new(
            IntegrationKind::Sonarr,
            "episode",
            77,
            "proof release",
            Some("guid".into()),
            &requirements,
        );
        pending.recovery_manifest = intent.recovery_manifest.clone();
        pending.proof = Some(proof);
        let guard = acquire_proof_work_guard().unwrap();
        let mut ready = [0; 2];
        let mut release = [0; 2];
        assert_eq!(unsafe { libc::pipe(ready.as_mut_ptr()) }, 0);
        assert_eq!(unsafe { libc::pipe(release.as_mut_ptr()) }, 0);
        let pid = unsafe { libc::fork() };
        assert!(pid >= 0);
        if pid == 0 {
            unsafe {
                libc::close(ready[0]);
                libc::close(release[1]);
                // Exercise Drop's release path without freeing allocations after fork.
                if guard.unlock_if_owner().is_err() {
                    libc::_exit(3);
                }
                let byte = [1_u8];
                if libc::write(ready[1], byte.as_ptr().cast(), 1) != 1 {
                    libc::_exit(1);
                }
                let mut byte = [0_u8];
                let read = libc::read(release[0], byte.as_mut_ptr().cast(), 1);
                libc::_exit(if read == 1 { 0 } else { 2 });
            }
        }
        unsafe {
            libc::close(ready[1]);
            libc::close(release[0]);
        }
        let mut ready = unsafe { File::from_raw_fd(ready[0]) };
        let mut release = unsafe { File::from_raw_fd(release[1]) };
        let mut byte = [0_u8];
        ready.read_exact(&mut byte).unwrap();
        let blocked_while_held = record_pending_grab(pending.clone());
        drop(guard);
        let after_drop = record_pending_grab(pending.clone());
        let mut status = 0;
        let child_still_alive = unsafe { libc::waitpid(pid, &mut status, libc::WNOHANG) } == 0;
        release.write_all(&[1]).unwrap();
        assert_eq!(unsafe { libc::waitpid(pid, &mut status, 0) }, pid);
        assert!(libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0);
        let contention = blocked_while_held.err().and_then(|err| {
            err.root_cause()
                .downcast_ref::<std::io::Error>()
                .and_then(std::io::Error::raw_os_error)
        });
        assert_eq!(contention, fs2::lock_contended_error().raw_os_error());
        assert!(contention.is_some());
        assert!(child_still_alive);
        assert!(
            after_drop.is_ok(),
            "proof lock remained held: {after_drop:?}"
        );
        assert_eq!(
            pending_grabs_strict(IntegrationKind::Sonarr).unwrap(),
            vec![pending]
        );
        assert_eq!(
            get_grab_intent(IntegrationKind::Sonarr, "episode", 77).unwrap(),
            Some(intent)
        );
    }

    #[cfg(unix)]
    #[test]
    fn pending_removal_rewrites_durably_and_preserves_other_records_on_failure() {
        for (exact, phase) in [
            (false, "before-rename"),
            (false, "after-rename"),
            (true, "before-rename"),
            (true, "after-rename"),
        ] {
            let tmp = TempDir::new().unwrap();
            use_test_cache_paths(
                tmp.path().join("pending.json"),
                tmp.path().join("no-candidates.json"),
            );
            let requirements = LanguageRequirements {
                enabled: true,
                audio: vec!["eng".into()],
                subtitles: vec![],
            };
            let mut target = PendingGrab::new(
                IntegrationKind::Sonarr,
                "episode",
                2,
                "target",
                Some("a".into()),
                &requirements,
            );
            target.grabbed_at_unix = 10;
            let mut other = target.clone();
            other.target_id = 3;
            record_pending_grab(target.clone()).unwrap();
            record_pending_grab(other.clone()).unwrap();
            set_durable_write_fault(Some(phase));
            let result = if exact {
                remove_pending_grab_exact(IntegrationKind::Sonarr, &target).map(|_| ())
            } else {
                remove_pending_grab(
                    IntegrationKind::Sonarr,
                    "episode",
                    2,
                    target.grabbed_at_unix,
                )
            };
            set_durable_write_fault(None);
            assert!(result.is_err());
            let records = pending_grabs_strict(IntegrationKind::Sonarr).unwrap();
            assert!(records.contains(&other));
            assert_eq!(records.contains(&target), phase == "before-rename");
        }
    }

    #[test]
    fn exact_pending_removal_preserves_changed_record_at_same_timestamp() {
        let tmp = TempDir::new().unwrap();
        use_test_cache_paths(
            tmp.path().join("pending-grabs.json"),
            tmp.path().join("no-candidates.json"),
        );
        let mut original = PendingGrab::new(
            IntegrationKind::Sonarr,
            "episode",
            2,
            "Original",
            Some("guid-a".into()),
            &LanguageRequirements {
                enabled: true,
                audio: vec!["eng".into()],
                subtitles: vec![],
            },
        );
        original.grabbed_at_unix = 10;
        let mut changed = original.clone();
        changed.guid = Some("guid-b".into());
        record_pending_grab(changed.clone()).unwrap();
        assert!(!remove_pending_grab_exact(IntegrationKind::Sonarr, &original).unwrap());
        assert_eq!(
            pending_grabs_strict(IntegrationKind::Sonarr).unwrap(),
            vec![changed.clone()]
        );
        assert!(remove_pending_grab_exact(IntegrationKind::Sonarr, &changed).unwrap());
        assert!(pending_grabs_strict(IntegrationKind::Sonarr)
            .unwrap()
            .is_empty());
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
        assert!(has_pending_grab_for_target(IntegrationKind::Sonarr, "episode", 2).is_err());
        let original = fs::read_to_string(&pending_path).unwrap();
        assert!(record_pending_grab(pending[0].clone()).is_err());
        assert!(remove_pending_grab(IntegrationKind::Sonarr, "episode", 2, 10).is_err());
        assert_eq!(fs::read_to_string(&pending_path).unwrap(), original);

        fs::write(&pending_path, "not json").unwrap();
        assert!(pending_grabs(IntegrationKind::Sonarr).unwrap().is_empty());
        assert!(has_pending_grab_for_target(IntegrationKind::Sonarr, "episode", 2).is_err());
    }
}
