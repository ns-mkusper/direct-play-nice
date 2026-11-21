//! Explicit, verified recovery copies for imported media and its subtitle sidecars.
//!
//! A successful result is a durable manifest, not permission to delete or replace
//! the original. A caller must hold the same exclusive target lock across this
//! backup and the subsequent replacement/import action, until that action ends.
//! This is cooperative only: Arr and other writers that do not honor the lock
//! can still change the source; this function cannot make them safe.
//!
//! Recovery admission serializes backups using this root. It counts all regular
//! file sizes under the root against a 1 TiB logical quota, limits an individual
//! backup to 512 GiB, and reserves the greater of 5 GiB or 5% of filesystem
//! capacity. Independent writers and other recovery roots are not coordinated.
//! Bind-mounted aliases can evade path-based source/root containment checks.
//! Recovery copies record mode, owner and timestamps, but do not capture or
//! restore ACLs, extended attributes, or other filesystem-specific metadata.
//! The root is excluded only from the source directory, not from an entire
//! media library; operators must place it outside every managed library.

use anyhow::Result;
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// A durable integrity checkpoint, not authentication against a ledger writer.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct RecoveryCheckpoint {
    pub version: u8,
    pub manifest_sha256: String,
    pub manifest_size: u64,
    pub recovery_root: PathBuf,
    pub manifest_path: PathBuf,
    pub media_path: PathBuf,
    pub root_device: u64,
    pub root_inode: u64,
    pub directory_device: u64,
    pub directory_inode: u64,
    pub manifest_device: u64,
    pub manifest_inode: u64,
    pub blobs: Vec<RecoveryBlobIdentity>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub sidecars: Option<Vec<RecoverySidecarIdentity>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct RecoveryBlobIdentity {
    pub device: u64,
    pub inode: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub(super) struct RecoverySidecarIdentity {
    pub path: PathBuf,
    pub device: u64,
    pub inode: u64,
    pub size: u64,
    pub mode: u32,
    pub uid: u32,
    pub gid: u32,
    pub mtime: i64,
    pub mtime_nsec: i64,
    pub ctime: i64,
    pub ctime_nsec: i64,
    pub links: u64,
}

impl RecoveryCheckpoint {
    pub(super) fn is_valid(&self) -> bool {
        self.version == 1
            && self.manifest_size > 0
            && self.manifest_size <= 64 * 1024 * 1024
            && self.manifest_sha256.len() == 64
            && self
                .manifest_sha256
                .bytes()
                .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
            && self.root_inode != 0
            && self.recovery_root.is_absolute()
            && self.manifest_path.is_absolute()
            && self.media_path.is_absolute()
            && self.directory_inode != 0
            && self.manifest_inode != 0
            && self.directory_device == self.root_device
            && self.manifest_device == self.root_device
            && !self.blobs.is_empty()
            && self.blobs.len() <= 4097
            && self
                .blobs
                .iter()
                .all(|blob| blob.inode != 0 && blob.device == self.root_device)
            && self
                .blobs
                .iter()
                .map(|blob| (blob.device, blob.inode))
                .collect::<std::collections::BTreeSet<_>>()
                .len()
                == self.blobs.len()
    }
}

pub(super) use imp::RecoveryTargetLock;

/// Acquire the DPN target lock. Every cooperating operation must use the same
/// source path and recovery root and retain the guard until its action ends.
pub(super) fn acquire_recovery_lock(
    source: &Path,
    recovery_root: &Path,
) -> Result<RecoveryTargetLock> {
    imp::acquire(source, recovery_root)
}

/// Acquire an existing target lock without creating or modifying a file.
pub(super) fn acquire_existing_recovery_lock(
    source: &Path,
    recovery_root: &Path,
) -> Result<RecoveryTargetLock> {
    imp::acquire_existing(source, recovery_root)
}

/// Back up the current media and matching sidecars before a destructive operation.
/// The existing recovery root must be private and outside the media directory.
/// The caller must retain the matching target lock until the replacement/import
/// completes. Advisory locks cannot constrain Arr or other uncooperative writers.
pub(super) fn backup_current_file(
    source: &Path,
    recovery_root: &Path,
    lock: &RecoveryTargetLock,
) -> Result<PathBuf> {
    imp::backup_locked(source, recovery_root, lock)
}

/// Capture the published backup under its existing target lock.
pub(super) fn capture_checkpoint(
    source: &Path,
    root: &Path,
    manifest: &Path,
    lock: &RecoveryTargetLock,
) -> Result<RecoveryCheckpoint> {
    imp::capture_checkpoint(source, root, manifest, lock)
}

/// Read-only validation. Callers coordinating an action must also hold the target lock.
pub(super) fn verify_checkpoint(
    source: &Path,
    root: &Path,
    manifest: &Path,
    checkpoint: Option<&RecoveryCheckpoint>,
) -> Result<()> {
    imp::verify_checkpoint(source, root, manifest, checkpoint)
}

/// Verify the current source still matches its checkpointed recovery copy.
pub(super) fn verify_original_locked(
    source: &Path,
    root: &Path,
    manifest: &Path,
    checkpoint: Option<&RecoveryCheckpoint>,
    lock: &RecoveryTargetLock,
) -> Result<()> {
    imp::verify_original_locked(source, root, manifest, checkpoint, lock)
}

// Directory fsync and reliable no-follow/identity checks are required. Fail
// closed on platforms where this implementation cannot establish those facts.
#[cfg(not(unix))]
mod imp {
    use super::RecoveryCheckpoint;
    use anyhow::{bail, Result};
    use std::path::{Path, PathBuf};

    pub struct RecoveryTargetLock;

    pub(super) fn acquire(_source: &Path, _root: &Path) -> Result<RecoveryTargetLock> {
        bail!("verified recovery backups are not supported on this platform")
    }

    pub(super) fn acquire_existing(_source: &Path, _root: &Path) -> Result<RecoveryTargetLock> {
        bail!("verified recovery locks are not supported on this platform")
    }

    pub(super) fn backup_locked(
        _source: &Path,
        _root: &Path,
        _lock: &RecoveryTargetLock,
    ) -> Result<PathBuf> {
        bail!("verified recovery backups are not supported on this platform")
    }

    pub(super) fn capture_checkpoint(
        _source: &Path,
        _root: &Path,
        _manifest: &Path,
        _lock: &RecoveryTargetLock,
    ) -> Result<RecoveryCheckpoint> {
        bail!("verified recovery checkpoints are not supported on this platform")
    }

    pub(super) fn verify_checkpoint(
        _source: &Path,
        _root: &Path,
        _manifest: &Path,
        _checkpoint: Option<&RecoveryCheckpoint>,
    ) -> Result<()> {
        bail!("verified recovery checkpoints are not supported on this platform")
    }

    pub(super) fn verify_original_locked(
        _source: &Path,
        _root: &Path,
        _manifest: &Path,
        _checkpoint: Option<&RecoveryCheckpoint>,
        _lock: &RecoveryTargetLock,
    ) -> Result<()> {
        bail!("verified recovery checkpoints are not supported on this platform")
    }

    #[cfg(test)]
    #[test]
    fn unsupported_platform_fails_closed() {
        assert!(acquire(Path::new("source"), Path::new("root")).is_err());
        assert!(capture_checkpoint(
            Path::new("source"),
            Path::new("root"),
            Path::new("manifest"),
            &RecoveryTargetLock
        )
        .is_err());
        assert!(verify_checkpoint(
            Path::new("source"),
            Path::new("root"),
            Path::new("manifest"),
            None
        )
        .is_err());
    }
}

#[cfg(unix)]
mod imp {
    use super::{RecoveryBlobIdentity, RecoveryCheckpoint};
    use anyhow::{anyhow, bail, Context, Result};
    use fs2::FileExt;
    use serde::{Deserialize, Serialize};
    use sha2::{Digest, Sha256};
    use std::cell::RefCell;
    use std::ffi::{CStr, CString, OsStr};
    use std::fs::{self, File, Metadata, OpenOptions};
    use std::io::{Read, Seek, SeekFrom, Write};
    use std::os::fd::{AsRawFd, FromRawFd, IntoRawFd};
    use std::os::unix::ffi::OsStrExt;
    use std::os::unix::fs::{MetadataExt, OpenOptionsExt};
    use std::path::{Component, Path, PathBuf};
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::time::{SystemTime, UNIX_EPOCH};

    static NEXT_ID: AtomicU64 = AtomicU64::new(0);
    const MAX_SIDECARS: usize = 4096;
    // Bound disk use and work even if a source grows while it is being read.
    const MAX_BACKUP_BYTES: u64 = 512 * 1024 * 1024 * 1024;
    const MAX_ROOT_BYTES: u128 = 1024 * 1024 * 1024 * 1024;
    const MIN_SPACE_RESERVE_BYTES: u128 = 5 * 1024 * 1024 * 1024;
    const MAX_QUOTA_ENTRIES: usize = 100_000;
    const MAX_QUOTA_DEPTH: usize = 32;

    #[derive(Serialize, Deserialize)]
    #[serde(deny_unknown_fields)]
    struct Manifest {
        version: u8,
        files: Vec<ManifestFile>,
    }

    #[derive(Serialize, Deserialize)]
    #[serde(deny_unknown_fields)]
    struct ManifestFile {
        original_path: PathBuf,
        backup_file: String,
        size: u64,
        sha256: String,
        original_metadata: ManifestMetadata,
    }

    // Recorded for manual recovery only; this module does not restore metadata.
    #[derive(Clone, Serialize, Deserialize)]
    #[serde(deny_unknown_fields)]
    struct ManifestMetadata {
        mode: u32,
        uid: u32,
        gid: u32,
        atime_secs: i64,
        atime_nsec: i64,
        mtime_secs: i64,
        mtime_nsec: i64,
        ctime_secs: i64,
        ctime_nsec: i64,
    }

    impl ManifestMetadata {
        fn of(metadata: &Metadata) -> Self {
            Self {
                mode: metadata.mode(),
                uid: metadata.uid(),
                gid: metadata.gid(),
                atime_secs: metadata.atime(),
                atime_nsec: metadata.atime_nsec(),
                mtime_secs: metadata.mtime(),
                mtime_nsec: metadata.mtime_nsec(),
                ctime_secs: metadata.ctime(),
                ctime_nsec: metadata.ctime_nsec(),
            }
        }
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct Snapshot {
        dev: u64,
        ino: u64,
        size: u64,
        mode: u32,
        uid: u32,
        gid: u32,
        mtime: i64,
        mtime_nsec: i64,
        ctime: i64,
        ctime_nsec: i64,
        links: u64,
    }

    impl Snapshot {
        fn of(metadata: &Metadata) -> Self {
            Self {
                dev: metadata.dev(),
                ino: metadata.ino(),
                size: metadata.len(),
                mode: metadata.mode(),
                uid: metadata.uid(),
                gid: metadata.gid(),
                mtime: metadata.mtime(),
                mtime_nsec: metadata.mtime_nsec(),
                ctime: metadata.ctime(),
                ctime_nsec: metadata.ctime_nsec(),
                links: metadata.nlink(),
            }
        }
    }

    fn sidecar_identity(path: &Path, snapshot: Snapshot) -> super::RecoverySidecarIdentity {
        super::RecoverySidecarIdentity {
            path: path.to_path_buf(),
            device: snapshot.dev,
            inode: snapshot.ino,
            size: snapshot.size,
            mode: snapshot.mode,
            uid: snapshot.uid,
            gid: snapshot.gid,
            mtime: snapshot.mtime,
            mtime_nsec: snapshot.mtime_nsec,
            ctime: snapshot.ctime,
            ctime_nsec: snapshot.ctime_nsec,
            links: snapshot.links,
        }
    }

    struct Original {
        path: PathBuf,
        file: File,
        snapshot: Snapshot,
        original_metadata: ManifestMetadata,
        digest: String,
    }

    // All recovery writes and cleanup are relative to held directory descriptors.
    // Unexpected or replaced entries are never removed.
    struct IncompleteBackup {
        root: File,
        directory: File,
        directory_name: CString,
        dir_identity: (u64, u64),
        files: Vec<(CString, (u64, u64))>,
        committed: bool,
    }

    impl Drop for IncompleteBackup {
        fn drop(&mut self) {
            if self.committed {
                return;
            }
            if !entry_identity(&self.root, &self.directory_name)
                .is_some_and(|(id, mode)| id == self.dir_identity && mode == libc::S_IFDIR)
            {
                return;
            }
            for (name, identity) in self.files.iter().rev() {
                if entry_identity(&self.directory, name)
                    .is_some_and(|(id, mode)| id == *identity && mode == libc::S_IFREG)
                {
                    // The directory is private and held open. Never recurse.
                    unsafe {
                        libc::unlinkat(self.directory.as_raw_fd(), name.as_ptr(), 0);
                    }
                }
            }
            let _ = self.directory.sync_all();
            unsafe {
                libc::unlinkat(
                    self.root.as_raw_fd(),
                    self.directory_name.as_ptr(),
                    libc::AT_REMOVEDIR,
                );
            }
            let _ = self.root.sync_all();
        }
    }

    pub struct RecoveryTargetLock {
        file: File,
        source: PathBuf,
        root: PathBuf,
        root_identity: (u64, u64),
        name: CString,
        checkpoint: RefCell<Option<RecoveryCheckpoint>>,
    }

    pub(super) fn acquire(source: &Path, root: &Path) -> Result<RecoveryTargetLock> {
        acquire_impl(source, root, true)
    }

    pub(super) fn acquire_existing(source: &Path, root: &Path) -> Result<RecoveryTargetLock> {
        acquire_impl(source, root, false)
    }

    fn acquire_impl(source: &Path, root: &Path, create: bool) -> Result<RecoveryTargetLock> {
        let source = absolute_no_aliases(source)?;
        let root = absolute_no_aliases(root)?;
        let media_dir = source.parent().context("source has no media directory")?;
        if root.starts_with(media_dir) {
            bail!("recovery root must be outside the source media directory");
        }
        let original = open_original(&source)?;
        checked_original_metadata(&original, &source)?;
        let root_handle = open_protected_root(&root)?;
        let media_identity = identity(&open_directory(media_dir)?.metadata()?);
        for ancestor in root.ancestors() {
            if identity(&fs::symlink_metadata(ancestor)?) == media_identity {
                bail!("recovery root aliases the source media directory");
            }
        }
        let root_identity = identity(&root_handle.metadata()?);
        let source_str = source.to_str().context("source path is not UTF-8")?;
        // Over-lock case variants even on case-sensitive hosts; the volume may
        // actually resolve them to the same target.
        let lock_key = source_str.to_lowercase();
        let name = CString::new(format!(
            ".dpn-target-{:x}.lock",
            Sha256::digest(lock_key.as_bytes())
        ))?;
        let flags = if create {
            libc::O_RDWR | libc::O_CREAT
        } else {
            libc::O_RDONLY | libc::O_NONBLOCK
        };
        let file =
            open_at(&root_handle, &name, flags, 0o600).context("opening recovery target lock")?;
        let metadata = file.metadata()?;
        if !metadata.is_file()
            || metadata.uid() != unsafe { libc::geteuid() }
            || metadata.mode() & 0o077 != 0
            || metadata.nlink() != 1
            || entry_identity(&root_handle, &name) != Some((identity(&metadata), libc::S_IFREG))
        {
            bail!("recovery target lock is not private and unaliased");
        }
        file.try_lock_exclusive()
            .context("acquiring recovery target lock")?;
        if entry_identity(&root_handle, &name) != Some((identity(&file.metadata()?), libc::S_IFREG))
        {
            bail!("recovery target lock was replaced");
        }
        if create {
            root_handle
                .sync_all()
                .context("syncing recovery lock directory")?;
        }
        Ok(RecoveryTargetLock {
            file,
            source,
            root,
            root_identity,
            name,
            checkpoint: RefCell::new(None),
        })
    }

    fn validate_target_lock(source: &Path, root: &Path, lock: &RecoveryTargetLock) -> Result<()> {
        let source = absolute_no_aliases(source)?;
        let root = absolute_no_aliases(root)?;
        let root_handle = open_protected_root(&root)?;
        let metadata = lock.file.metadata()?;
        if source != lock.source
            || root != lock.root
            || identity(&root_handle.metadata()?) != lock.root_identity
            || !metadata.is_file()
            || metadata.nlink() != 1
            || metadata.uid() != unsafe { libc::geteuid() }
            || metadata.mode() & 0o077 != 0
            || entry_identity(&root_handle, &lock.name)
                != Some((identity(&metadata), libc::S_IFREG))
        {
            bail!("recovery lock does not match the source and protected root");
        }
        Ok(())
    }

    pub(super) fn backup_locked(
        source: &Path,
        root: &Path,
        lock: &RecoveryTargetLock,
    ) -> Result<PathBuf> {
        validate_target_lock(source, root, lock)?;
        let source = absolute_no_aliases(source)?;
        let root = absolute_no_aliases(root)?;
        log::warn!(
            "Recovery root '{}' is checked only against source directory '{}'; ensure it is outside every managed library.",
            root.display(),
            source.parent().unwrap_or(Path::new(".")).display()
        );
        *lock.checkpoint.borrow_mut() = None;
        let (manifest, checkpoint) =
            backup_with_hooks_checkpoint(&source, &root, || Ok(()), || Ok(()))?;
        *lock.checkpoint.borrow_mut() = Some(checkpoint);
        Ok(manifest)
    }

    const MAX_MANIFEST_BYTES: u64 = 64 * 1024 * 1024;

    struct CheckpointFiles {
        root: File,
        directory: File,
        manifest: File,
        directory_name: CString,
    }

    fn open_checkpoint_files(root: &Path, manifest: &Path) -> Result<CheckpointFiles> {
        let root_path = absolute_no_aliases(root)?;
        let relative = manifest
            .strip_prefix(&root_path)
            .context("manifest is outside recovery root")?;
        let mut components = relative.components();
        let directory_name = match (components.next(), components.next(), components.next()) {
            (Some(Component::Normal(dir)), Some(Component::Normal(file)), None)
                if dir.as_bytes().starts_with(b".dpn-recovery-")
                    && file == OsStr::new("manifest.json") =>
            {
                CString::new(dir.as_bytes())?
            }
            _ => bail!("invalid recovery manifest path"),
        };
        let root_handle = open_protected_root(&root_path)?;
        let directory = open_at(
            &root_handle,
            &directory_name,
            libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NONBLOCK,
            0,
        )?;
        let dir_metadata = directory.metadata()?;
        if !dir_metadata.is_dir()
            || dir_metadata.uid() != unsafe { libc::geteuid() }
            || dir_metadata.mode() & 0o077 != 0
            || dir_metadata.dev() != root_handle.metadata()?.dev()
            || entry_identity(&root_handle, &directory_name)
                != Some((identity(&dir_metadata), libc::S_IFDIR))
        {
            bail!("recovery directory is not private or has changed");
        }
        let manifest_name = CString::new("manifest.json")?;
        let manifest_file = open_at(
            &directory,
            &manifest_name,
            libc::O_RDONLY | libc::O_NONBLOCK,
            0,
        )?;
        let meta = manifest_file.metadata()?;
        if !meta.is_file()
            || meta.uid() != unsafe { libc::geteuid() }
            || meta.mode() & 0o077 != 0
            || meta.nlink() != 1
            || meta.len() == 0
            || meta.len() > MAX_MANIFEST_BYTES
            || entry_identity(&directory, &manifest_name) != Some((identity(&meta), libc::S_IFREG))
        {
            bail!("recovery manifest is not a bounded, private regular file");
        }
        Ok(CheckpointFiles {
            root: root_handle,
            directory,
            manifest: manifest_file,
            directory_name,
        })
    }

    fn read_checkpoint_manifest(files: &mut CheckpointFiles) -> Result<(Vec<u8>, Snapshot)> {
        let before = files.manifest.metadata()?;
        if !before.is_file()
            || before.uid() != unsafe { libc::geteuid() }
            || before.mode() & 0o077 != 0
            || before.nlink() != 1
            || before.len() == 0
            || before.len() > MAX_MANIFEST_BYTES
        {
            bail!("recovery manifest is not a bounded, private regular file");
        }
        files.manifest.seek(SeekFrom::Start(0))?;
        let mut bytes = Vec::new();
        (&mut files.manifest)
            .take(MAX_MANIFEST_BYTES + 1)
            .read_to_end(&mut bytes)?;
        if bytes.len() as u64 != before.len()
            || Snapshot::of(&files.manifest.metadata()?) != Snapshot::of(&before)
            || entry_identity(&files.directory, &CString::new("manifest.json")?)
                != Some((identity(&before), libc::S_IFREG))
        {
            bail!("recovery manifest changed while reading");
        }
        Ok((bytes, Snapshot::of(&before)))
    }

    fn verify_checkpoint_inner(
        source: &Path,
        root: &Path,
        manifest: &Path,
        checkpoint: &RecoveryCheckpoint,
    ) -> Result<Manifest> {
        let root = absolute_no_aliases(root)?;
        if !checkpoint.is_valid()
            || checkpoint.recovery_root != root
            || checkpoint.manifest_path != manifest
            || checkpoint.media_path != source
        {
            bail!("recovery checkpoint does not match its paths or has invalid fields");
        }
        let mut files = open_checkpoint_files(&root, manifest)?;
        let root_identity = identity(&files.root.metadata()?);
        let dir_metadata = files.directory.metadata()?;
        if !dir_metadata.is_dir()
            || dir_metadata.uid() != unsafe { libc::geteuid() }
            || dir_metadata.mode() & 0o077 != 0
            || dir_metadata.dev() != root_identity.0
        {
            bail!("recovery directory changed or is not private");
        }
        let dir_snapshot = Snapshot::of(&dir_metadata);
        let dir_identity = (dir_snapshot.dev, dir_snapshot.ino);
        let manifest_identity = identity(&files.manifest.metadata()?);
        if root_identity != (checkpoint.root_device, checkpoint.root_inode)
            || dir_identity != (checkpoint.directory_device, checkpoint.directory_inode)
            || manifest_identity != (checkpoint.manifest_device, checkpoint.manifest_inode)
        {
            bail!("recovery checkpoint identity changed");
        }
        let (bytes, manifest_snapshot) = read_checkpoint_manifest(&mut files)?;
        if bytes.len() as u64 != checkpoint.manifest_size
            || format!("{:x}", Sha256::digest(&bytes)) != checkpoint.manifest_sha256
        {
            bail!("recovery manifest does not match its checkpoint");
        }
        let parsed: Manifest =
            serde_json::from_slice(&bytes).context("parsing recovery manifest")?;
        if parsed.version != 1
            || parsed.files.is_empty()
            || parsed.files.len() > MAX_SIDECARS + 1
            || parsed.files.len() != checkpoint.blobs.len()
            || parsed.files[0].original_path != source
            || !source.is_absolute()
            || source.components().any(|part| part == Component::ParentDir)
        {
            bail!("recovery manifest has an invalid version, file count, or media path");
        }
        let mut expected_names = std::collections::BTreeSet::new();
        expected_names.insert(b"manifest.json".to_vec());
        for index in 0..parsed.files.len() {
            expected_names.insert(format!("{index:04}.blob").into_bytes());
        }
        let actual_names: std::collections::BTreeSet<Vec<u8>> =
            read_directory_names(&files.directory)?
                .into_iter()
                .map(|name| name.into_bytes())
                .collect();
        if actual_names != expected_names {
            bail!("recovery directory contains missing or unexpected entries");
        }
        let parent = source.parent().context("media path has no parent")?;
        let stem = source
            .file_stem()
            .and_then(OsStr::to_str)
            .filter(|s| !s.is_empty())
            .context("invalid media stem")?;
        let mut total = 0u64;
        let mut checked = Vec::with_capacity(parsed.files.len());
        let mut previous_sidecar: Option<&Path> = None;
        for (index, item) in parsed.files.iter().enumerate() {
            if item.backup_file != format!("{index:04}.blob")
                || item.size > MAX_BACKUP_BYTES
                || item.sha256.len() != 64
                || !item
                    .sha256
                    .bytes()
                    .all(|b| b.is_ascii_digit() || (b'a'..=b'f').contains(&b))
                || !item.original_path.is_absolute()
                || item
                    .original_path
                    .components()
                    .any(|c| c == Component::ParentDir)
            {
                bail!("invalid recovery manifest entry");
            }
            if index > 0 {
                let name = item
                    .original_path
                    .file_name()
                    .and_then(OsStr::to_str)
                    .context("invalid sidecar name")?;
                if item.original_path.parent() != Some(parent)
                    || !matches_sidecar(name, stem)?
                    || previous_sidecar
                        .is_some_and(|previous| previous >= item.original_path.as_path())
                {
                    bail!("invalid or duplicate recovery sidecar path");
                }
                previous_sidecar = Some(&item.original_path);
            }
            total = total
                .checked_add(item.size)
                .context("recovery size overflow")?;
            if total > MAX_BACKUP_BYTES {
                bail!("recovery backup exceeds size limit");
            }
            let name = CString::new(item.backup_file.as_str())?;
            let mut blob = open_at(
                &files.directory,
                &name,
                libc::O_RDONLY | libc::O_NONBLOCK,
                0,
            )?;
            let meta = blob.metadata()?;
            if !meta.is_file()
                || meta.uid() != unsafe { libc::geteuid() }
                || meta.mode() & 0o077 != 0
                || meta.nlink() != 1
                || meta.len() != item.size
                || identity(&meta)
                    != (
                        checkpoint.blobs[index].device,
                        checkpoint.blobs[index].inode,
                    )
                || entry_identity(&files.directory, &name) != Some((identity(&meta), libc::S_IFREG))
            {
                bail!("recovery blob is missing, untrusted, or changed");
            }
            let snapshot = Snapshot::of(&meta);
            let (_, digest) = hash_reader(&mut blob, item.size)?;
            if digest != item.sha256
                || Snapshot::of(&blob.metadata()?) != snapshot
                || entry_identity(&files.directory, &name) != Some((identity(&meta), libc::S_IFREG))
            {
                bail!("recovery blob failed verification");
            }
            checked.push((name, snapshot));
        }
        for (name, snapshot) in checked {
            let blob = open_at(
                &files.directory,
                &name,
                libc::O_RDONLY | libc::O_NONBLOCK,
                0,
            )?;
            if Snapshot::of(&blob.metadata()?) != snapshot
                || entry_identity(&files.directory, &name)
                    != Some(((snapshot.dev, snapshot.ino), libc::S_IFREG))
            {
                bail!("recovery blob changed during verification");
            }
        }
        let reopened_root = open_protected_root(&root)?;
        if identity(&reopened_root.metadata()?) != root_identity
            || Snapshot::of(&files.directory.metadata()?) != dir_snapshot
            || entry_identity(&files.root, &files.directory_name)
                != Some((dir_identity, libc::S_IFDIR))
            || Snapshot::of(&files.manifest.metadata()?) != manifest_snapshot
            || entry_identity(&files.directory, &CString::new("manifest.json")?)
                != Some((manifest_identity, libc::S_IFREG))
        {
            bail!("recovery evidence changed during verification");
        }
        Ok(parsed)
    }

    pub(super) fn verify_checkpoint(
        source: &Path,
        root: &Path,
        manifest: &Path,
        checkpoint: Option<&RecoveryCheckpoint>,
    ) -> Result<()> {
        let checkpoint = checkpoint
            .context("legacy proof has no recovery checkpoint; manual review required")?;
        verify_checkpoint_inner(source, root, manifest, checkpoint).map(|_| ())
    }

    pub(super) fn verify_original_locked(
        source: &Path,
        root: &Path,
        manifest: &Path,
        checkpoint: Option<&RecoveryCheckpoint>,
        lock: &RecoveryTargetLock,
    ) -> Result<()> {
        validate_target_lock(source, root, lock)?;
        let checkpoint = checkpoint
            .context("legacy proof has no recovery checkpoint; manual review required")?;
        let parsed = verify_checkpoint_inner(source, root, manifest, checkpoint)?;
        let expected_sidecars: Vec<_> = parsed
            .files
            .iter()
            .skip(1)
            .map(|item| item.original_path.clone())
            .collect();
        if find_sidecars(source)? != expected_sidecars {
            bail!("current subtitle sidecars differ from the recovery checkpoint");
        }
        let recorded_sidecars = checkpoint.sidecars.as_deref().unwrap_or(&[]);
        if recorded_sidecars.len() != expected_sidecars.len() {
            bail!("recovery checkpoint lacks current subtitle sidecar identities");
        }
        let mut checked_sidecars = Vec::new();
        for (item, recorded) in parsed.files.iter().skip(1).zip(recorded_sidecars) {
            let mut file = open_original(&item.original_path)?;
            let snapshot = checked_original_metadata(&file, &item.original_path)?;
            if recorded != &sidecar_identity(&item.original_path, snapshot)
                || snapshot.size != item.size
                || hash_reader(&mut file, snapshot.size)?.1 != item.sha256
                || checked_original_metadata(&file, &item.original_path)? != snapshot
            {
                bail!("current subtitle sidecar differs from the recovery checkpoint");
            }
            checked_sidecars.push((file, item.original_path.clone(), snapshot));
        }
        let mut original = open_original(source)?;
        let snapshot = checked_original_metadata(&original, source)?;
        if snapshot.size != parsed.files[0].size {
            bail!("current media no longer matches its recovery copy");
        }
        let (_, digest) = hash_reader(&mut original, snapshot.size)?;
        if digest != parsed.files[0].sha256
            || checked_original_metadata(&original, source)? != snapshot
        {
            bail!("current media no longer matches its recovery copy");
        }
        verify_checkpoint_inner(source, root, manifest, checkpoint)?;
        if checked_original_metadata(&original, source)? != snapshot
            || find_sidecars(source)? != expected_sidecars
        {
            bail!("current media or subtitle sidecars changed during recovery verification");
        }
        for (file, path, snapshot) in &checked_sidecars {
            if checked_original_metadata(file, path)? != *snapshot {
                bail!("current subtitle sidecar changed during recovery verification");
            }
        }
        validate_target_lock(source, root, lock)
    }

    pub(super) fn capture_checkpoint(
        source: &Path,
        root: &Path,
        manifest: &Path,
        lock: &RecoveryTargetLock,
    ) -> Result<RecoveryCheckpoint> {
        validate_target_lock(source, root, lock)?;
        let source = absolute_no_aliases(source)?;
        let root = absolute_no_aliases(root)?;
        let checkpoint = lock
            .checkpoint
            .borrow()
            .clone()
            .context("recovery lock has no published backup checkpoint")?;
        let parsed = verify_checkpoint_inner(&source, &root, manifest, &checkpoint)?;
        let mut original = open_original(&source)?;
        let snapshot = checked_original_metadata(&original, &source)?;
        if snapshot.size != parsed.files[0].size {
            bail!("current media no longer matches its recovery copy");
        }
        let (_, digest) = hash_reader(&mut original, snapshot.size)?;
        if digest != parsed.files[0].sha256
            || checked_original_metadata(&original, &source)? != snapshot
        {
            bail!("current media no longer matches its recovery copy");
        }
        verify_checkpoint_inner(&source, &root, manifest, &checkpoint)?;
        if checked_original_metadata(&original, &source)? != snapshot {
            bail!("current media changed during checkpoint capture");
        }
        validate_target_lock(&source, &root, lock)?;
        Ok(checkpoint)
    }

    #[cfg(test)]
    fn backup(source: &Path, root: &Path) -> Result<PathBuf> {
        backup_with_hook(source, root, || Ok(()))
    }

    #[cfg(test)]
    fn backup_with_hook(
        source: &Path,
        root: &Path,
        before_final_verification: impl FnOnce() -> Result<()>,
    ) -> Result<PathBuf> {
        backup_with_hooks(source, root, before_final_verification, || Ok(()))
    }

    #[cfg(test)]
    fn backup_with_hooks(
        source: &Path,
        root: &Path,
        before_final_verification: impl FnOnce() -> Result<()>,
        after_publication: impl FnOnce() -> Result<()>,
    ) -> Result<PathBuf> {
        backup_with_hooks_checkpoint(source, root, before_final_verification, after_publication)
            .map(|(path, _)| path)
    }

    fn backup_with_hooks_checkpoint(
        source: &Path,
        root: &Path,
        before_final_verification: impl FnOnce() -> Result<()>,
        after_publication: impl FnOnce() -> Result<()>,
    ) -> Result<(PathBuf, RecoveryCheckpoint)> {
        let source = absolute_no_aliases(source)?;
        let root = absolute_no_aliases(root)?;
        let media_dir = source.parent().context("source has no media directory")?;
        if root.starts_with(media_dir) {
            bail!("recovery root must be outside the source media directory");
        }
        // Keep directory handles open and recheck identity before publishing.
        let media_dir_handle = open_directory(media_dir)?;
        let root_handle = open_protected_root(&root)?;
        let media_identity = identity(&media_dir_handle.metadata()?);
        let root_identity = identity(&root_handle.metadata()?);
        for ancestor in root.ancestors() {
            if identity(&fs::symlink_metadata(ancestor)?) == media_identity {
                bail!("recovery root aliases the source media directory");
            }
        }
        let sidecars = find_sidecars(&source)?;
        let mut paths = vec![source.clone()];
        paths.extend(sidecars.iter().cloned());
        let mut sources = Vec::with_capacity(paths.len());
        for path in paths {
            let file = open_original(&path)?;
            let metadata = file.metadata()?;
            let snapshot = checked_original_metadata(&file, &path)?;
            if Snapshot::of(&metadata) != snapshot {
                bail!(
                    "source metadata changed before backup: '{}'",
                    path.display()
                );
            }
            sources.push(Original {
                path,
                file,
                snapshot,
                original_metadata: ManifestMetadata::of(&metadata),
                digest: String::new(),
            });
        }

        let total = sources.iter().try_fold(0u64, |sum, original| {
            sum.checked_add(original.snapshot.size)
                .ok_or_else(|| anyhow!("backup size overflow"))
        })?;
        // A root-wide lock serializes quota and free-space admission through all
        // writes. It coordinates only processes using this same recovery root.
        let admission = acquire_admission_lock(&root_handle)?;
        let used = recovery_root_usage(&root, &root_handle)?;
        check_budget(&root_handle, total, used)?;
        let (directory_name, directory_handle) = create_private_directory(&root_handle)?;
        let directory = root.join(OsStr::from_bytes(directory_name.as_bytes()));
        let mut guard = IncompleteBackup {
            root: root_handle.try_clone()?,
            dir_identity: identity(&directory_handle.metadata()?),
            directory: directory_handle,
            directory_name,
            files: Vec::new(),
            committed: false,
        };
        verify_private_directory(&guard)?;
        // Persist the newly created directory before writing its contents.
        root_handle.sync_all().context("syncing recovery root")?;
        let mut files = Vec::with_capacity(sources.len());
        for (index, original) in sources.iter_mut().enumerate() {
            let backup_file = format!("{index:04}.blob");
            let path = directory.join(&backup_file);
            let mut output = create_owned_file(&backup_file, &mut guard)?;
            let (size, digest) =
                copy_and_hash(&mut original.file, &mut output, original.snapshot.size)?;
            if size != original.snapshot.size {
                bail!(
                    "source size changed while copying '{}'; backup aborted",
                    original.path.display()
                );
            }
            ensure_original_unchanged(original)?;
            output
                .sync_all()
                .with_context(|| format!("syncing '{}'", path.display()))?;
            output.seek(SeekFrom::Start(0))?;
            let (copied_size, copied_digest) = hash_reader(&mut output, size)?;
            if copied_size != size || copied_digest != digest {
                bail!(
                    "recovery copy verification failed for '{}'",
                    original.path.display()
                );
            }
            original.digest = digest.clone();
            files.push(ManifestFile {
                original_path: original.path.clone(),
                backup_file,
                size,
                sha256: digest,
                original_metadata: original.original_metadata.clone(),
            });
        }

        for (file, (name, expected_identity)) in files.iter().zip(&guard.files) {
            verify_copy(&guard, name, *expected_identity, file.size, &file.sha256)?;
        }
        verify_private_directory(&guard)?;
        let manifest_bytes = serde_json::to_vec_pretty(&Manifest { version: 1, files })?;
        let mut manifest = create_owned_file(".manifest.pending", &mut guard)?;
        manifest.write_all(&manifest_bytes)?;
        manifest.write_all(b"\n")?;
        manifest
            .sync_all()
            .context("syncing pending recovery manifest")?;
        guard
            .directory
            .sync_all()
            .context("syncing recovery data and pending manifest")?;
        root_handle.sync_all().context("syncing recovery root")?;

        before_final_verification()?;
        if find_sidecars(&source)? != sidecars {
            bail!("subtitle sidecars changed during backup");
        }
        verify_directory(media_dir, &media_dir_handle, media_identity)?;
        absolute_no_aliases(media_dir)?;
        let reopened_root = open_protected_root(&root)?;
        if identity(&reopened_root.metadata()?) != root_identity {
            bail!("recovery root changed");
        }
        verify_private_directory(&guard)?;
        for (file, (name, expected_identity)) in sources.iter().zip(&guard.files) {
            verify_copy(
                &guard,
                name,
                *expected_identity,
                file.snapshot.size,
                &file.digest,
            )?;
        }
        verify_admission_lock(&root_handle, &admission)?;
        // Hash sources after the potentially long copy verification and as close
        // as possible to publication. The caller's shared lock must remain held.
        for original in &mut sources {
            ensure_original_unchanged(original)?;
            original.file.seek(SeekFrom::Start(0))?;
            let (size, digest) = hash_reader(&mut original.file, original.snapshot.size)?;
            if size != original.snapshot.size || digest != original.digest {
                bail!(
                    "source content changed during backup: '{}'",
                    original.path.display()
                );
            }
            ensure_original_unchanged(original)?;
        }
        if find_sidecars(&source)? != sidecars {
            bail!("subtitle sidecars changed during backup");
        }
        for original in &sources {
            ensure_original_unchanged(original)?;
        }
        verify_private_directory(&guard)?;
        let pending = CString::new(".manifest.pending")?;
        let final_name = CString::new("manifest.json")?;
        let pending_metadata = manifest.metadata()?;
        let pending_identity = identity(&pending_metadata);
        let mut expected_manifest = manifest_bytes;
        expected_manifest.push(b'\n');
        manifest.seek(SeekFrom::Start(0))?;
        let (size, hash) = hash_reader(&mut manifest, expected_manifest.len() as u64)?;
        if size != expected_manifest.len() as u64
            || hash != format!("{:x}", Sha256::digest(&expected_manifest))
            || pending_metadata.len() != expected_manifest.len() as u64
            || pending_metadata.uid() != unsafe { libc::geteuid() }
            || pending_metadata.mode() & 0o077 != 0
            || pending_metadata.nlink() != 1
            || Snapshot::of(&manifest.metadata()?) != Snapshot::of(&pending_metadata)
            || entry_identity(&guard.directory, &pending) != Some((pending_identity, libc::S_IFREG))
        {
            bail!("pending recovery manifest changed before publication");
        }
        let checkpoint = RecoveryCheckpoint {
            version: 1,
            manifest_sha256: hash,
            manifest_size: size,
            recovery_root: root.clone(),
            manifest_path: directory.join("manifest.json"),
            media_path: source.clone(),
            root_device: root_identity.0,
            root_inode: root_identity.1,
            directory_device: guard.dir_identity.0,
            directory_inode: guard.dir_identity.1,
            manifest_device: pending_identity.0,
            manifest_inode: pending_identity.1,
            sidecars: Some(
                sources
                    .iter()
                    .skip(1)
                    .map(|source| sidecar_identity(&source.path, source.snapshot))
                    .collect(),
            ),
            blobs: guard
                .files
                .iter()
                .take(sources.len())
                .map(|(_, (device, inode))| RecoveryBlobIdentity {
                    device: *device,
                    inode: *inode,
                })
                .collect(),
        };
        // linkat is atomic and does not replace an existing marker. Both names
        // refer only to our synced manifest, never to the source media.
        let rc = unsafe {
            libc::linkat(
                guard.directory.as_raw_fd(),
                pending.as_ptr(),
                guard.directory.as_raw_fd(),
                final_name.as_ptr(),
                0,
            )
        };
        if rc != 0 {
            return Err(std::io::Error::last_os_error()).context("publishing recovery manifest");
        }
        guard.files.push((final_name, pending_identity));
        after_publication()?;
        let rc = unsafe { libc::unlinkat(guard.directory.as_raw_fd(), pending.as_ptr(), 0) };
        if rc != 0 {
            return Err(std::io::Error::last_os_error())
                .context("removing pending recovery manifest");
        }
        guard
            .directory
            .sync_all()
            .context("syncing published recovery manifest")?;
        verify_private_directory(&guard)?;
        guard.committed = true;
        Ok((directory.join("manifest.json"), checkpoint))
    }

    fn identity(metadata: &Metadata) -> (u64, u64) {
        (metadata.dev(), metadata.ino())
    }

    fn absolute_no_aliases(path: &Path) -> Result<PathBuf> {
        if path.as_os_str().is_empty() || path.components().any(|c| c == Component::ParentDir) {
            bail!("empty paths and parent-directory aliases are not allowed");
        }
        let absolute = if path.is_absolute() {
            path.to_path_buf()
        } else {
            std::env::current_dir()?.join(path)
        };
        if absolute.to_str().is_none() {
            bail!("recovery paths must be valid UTF-8");
        }
        let mut part = PathBuf::new();
        for component in absolute.components() {
            part.push(component.as_os_str());
            let metadata = fs::symlink_metadata(&part)
                .with_context(|| format!("examining '{}'", part.display()))?;
            if metadata.file_type().is_symlink() {
                bail!(
                    "symlink path is not safe for recovery: '{}'",
                    part.display()
                );
            }
        }
        // Components remove harmless `.` aliases; use this same normalized path
        // for both the lock key and the backup to prevent duplicate target locks.
        Ok(part)
    }

    fn open_directory(path: &Path) -> Result<File> {
        let file = OpenOptions::new()
            .read(true)
            .custom_flags(libc::O_DIRECTORY | libc::O_NOFOLLOW | libc::O_NONBLOCK)
            .open(path)
            .with_context(|| format!("opening directory '{}'", path.display()))?;
        if !file.metadata()?.is_dir() {
            bail!("not a directory: '{}'", path.display());
        }
        Ok(file)
    }

    fn verify_directory(path: &Path, handle: &File, expected: (u64, u64)) -> Result<()> {
        let reopened = open_directory(path)?;
        if identity(&handle.metadata()?) != expected || identity(&reopened.metadata()?) != expected
        {
            bail!("directory changed during recovery: '{}'", path.display());
        }
        Ok(())
    }

    fn open_original(path: &Path) -> Result<File> {
        let file = OpenOptions::new()
            .read(true)
            .custom_flags(libc::O_NOFOLLOW | libc::O_NONBLOCK)
            .open(path)
            .with_context(|| format!("opening original '{}'", path.display()))?;
        checked_original_metadata(&file, path)?;
        Ok(file)
    }

    fn checked_original_metadata(file: &File, path: &Path) -> Result<Snapshot> {
        let metadata = file.metadata()?;
        let path_metadata = fs::symlink_metadata(path)?;
        if !metadata.is_file()
            || !path_metadata.is_file()
            || metadata.nlink() != 1
            || Snapshot::of(&metadata) != Snapshot::of(&path_metadata)
        {
            bail!(
                "source is not an unaliased, stable regular file: '{}'",
                path.display()
            );
        }
        Ok(Snapshot::of(&metadata))
    }

    fn ensure_original_unchanged(original: &Original) -> Result<()> {
        if checked_original_metadata(&original.file, &original.path)? != original.snapshot {
            bail!(
                "source changed during recovery: '{}'",
                original.path.display()
            );
        }
        Ok(())
    }

    // libc uses different device/inode field types across Unix targets. Match
    // MetadataExt's u64 identity, including its signed-device conversion on macOS.
    #[cfg(all(target_os = "linux", target_pointer_width = "64"))]
    fn stat_identity(stat: &libc::stat) -> (u64, u64) {
        (stat.st_dev, stat.st_ino)
    }

    #[cfg(target_os = "macos")]
    fn stat_identity(stat: &libc::stat) -> (u64, u64) {
        (stat.st_dev as u64, stat.st_ino)
    }

    #[cfg(not(any(
        all(target_os = "linux", target_pointer_width = "64"),
        target_os = "macos"
    )))]
    #[allow(clippy::unnecessary_cast)]
    fn stat_identity(stat: &libc::stat) -> (u64, u64) {
        (stat.st_dev as u64, stat.st_ino as u64)
    }

    fn entry_identity(dir: &File, name: &CString) -> Option<((u64, u64), libc::mode_t)> {
        let mut stat = std::mem::MaybeUninit::<libc::stat>::uninit();
        let rc = unsafe {
            libc::fstatat(
                dir.as_raw_fd(),
                name.as_ptr(),
                stat.as_mut_ptr(),
                libc::AT_SYMLINK_NOFOLLOW,
            )
        };
        if rc != 0 {
            return None;
        }
        let stat = unsafe { stat.assume_init() };
        Some((stat_identity(&stat), stat.st_mode & libc::S_IFMT))
    }

    #[allow(clippy::unnecessary_cast)]
    fn open_at(dir: &File, name: &CString, flags: i32, mode: libc::mode_t) -> Result<File> {
        let fd = unsafe {
            libc::openat(
                dir.as_raw_fd(),
                name.as_ptr(),
                flags | libc::O_CLOEXEC | libc::O_NOFOLLOW,
                mode as libc::c_uint,
            )
        };
        if fd < 0 {
            return Err(std::io::Error::last_os_error().into());
        }
        Ok(unsafe { File::from_raw_fd(fd) })
    }

    // Walk from the filesystem root, rejecting symlinks and untrusted writable
    // ancestors. The final recovery root must belong to this effective user and
    // grant no permissions to group or others.
    fn open_protected_root(path: &Path) -> Result<File> {
        let mut dir = open_directory(Path::new("/"))?;
        let uid = unsafe { libc::geteuid() };
        let check_parent = |m: &Metadata| -> Result<()> {
            if !m.is_dir() || (m.uid() != 0 && m.uid() != uid) || m.mode() & 0o022 != 0 {
                bail!("recovery root has an untrusted or writable ancestor");
            }
            Ok(())
        };
        check_parent(&dir.metadata()?)?;
        for component in path.components() {
            match component {
                Component::RootDir => continue,
                Component::Normal(name) => {
                    let name = CString::new(name.as_bytes())?;
                    dir = open_at(
                        &dir,
                        &name,
                        libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NONBLOCK,
                        0,
                    )
                    .context("opening protected recovery root component")?;
                    check_parent(&dir.metadata()?)?;
                }
                _ => bail!("unsafe recovery root component"),
            }
        }
        let metadata = dir.metadata()?;
        if metadata.uid() != uid || metadata.mode() & 0o077 != 0 {
            bail!(
                "recovery root must be owned by the effective user and private (0700 or stricter)"
            );
        }
        Ok(dir)
    }

    fn verify_private_directory(guard: &IncompleteBackup) -> Result<()> {
        let metadata = guard.directory.metadata()?;
        if !metadata.is_dir()
            || metadata.uid() != unsafe { libc::geteuid() }
            || metadata.mode() & 0o077 != 0
            || identity(&metadata) != guard.dir_identity
            || !entry_identity(&guard.root, &guard.directory_name)
                .is_some_and(|(id, mode)| id == guard.dir_identity && mode == libc::S_IFDIR)
        {
            bail!("private recovery directory changed or became accessible");
        }
        Ok(())
    }

    fn create_private_directory(root: &File) -> Result<(CString, File)> {
        let time = SystemTime::now().duration_since(UNIX_EPOCH)?.as_nanos();
        for _ in 0..128 {
            let counter = NEXT_ID.fetch_add(1, Ordering::Relaxed);
            let name = CString::new(format!(
                ".dpn-recovery-{:x}-{time:x}-{counter:x}",
                std::process::id()
            ))?;
            let rc = unsafe { libc::mkdirat(root.as_raw_fd(), name.as_ptr(), 0o700) };
            if rc != 0 {
                let error = std::io::Error::last_os_error();
                if error.kind() == std::io::ErrorKind::AlreadyExists {
                    continue;
                }
                return Err(error).context("creating private recovery directory");
            }
            let directory = open_at(
                root,
                &name,
                libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NONBLOCK,
                0,
            )?;
            let m = directory.metadata()?;
            if !m.is_dir() || m.uid() != unsafe { libc::geteuid() } || m.mode() & 0o077 != 0 {
                bail!("created recovery directory is not private and owned by the effective user");
            }
            return Ok((name, directory));
        }
        bail!("could not allocate a unique recovery directory")
    }

    fn create_owned_file(name: &str, guard: &mut IncompleteBackup) -> Result<File> {
        let name = CString::new(name)?;
        let file = open_at(
            &guard.directory,
            &name,
            libc::O_RDWR | libc::O_CREAT | libc::O_EXCL,
            0o600,
        )
        .context("creating recovery file")?;
        let metadata = file.metadata()?;
        let file_identity = identity(&metadata);
        guard.files.push((name, file_identity));
        if !metadata.is_file()
            || metadata.uid() != unsafe { libc::geteuid() }
            || metadata.mode() & 0o077 != 0
            || metadata.nlink() != 1
        {
            bail!("created recovery file is not private and unaliased");
        }
        Ok(file)
    }

    fn verify_copy(
        guard: &IncompleteBackup,
        name: &CString,
        expected: (u64, u64),
        expected_size: u64,
        expected_hash: &str,
    ) -> Result<()> {
        let mut file = open_at(&guard.directory, name, libc::O_RDONLY | libc::O_NONBLOCK, 0)?;
        let before = file.metadata()?;
        if !before.is_file()
            || before.uid() != unsafe { libc::geteuid() }
            || before.mode() & 0o077 != 0
            || before.nlink() != 1
            || identity(&before) != expected
            || before.len() != expected_size
            || entry_identity(&guard.directory, name) != Some((expected, libc::S_IFREG))
        {
            bail!("recovery copy was replaced");
        }
        let (size, hash) = hash_reader(&mut file, expected_size)?;
        if size != expected_size
            || hash != expected_hash
            || Snapshot::of(&file.metadata()?) != Snapshot::of(&before)
            || entry_identity(&guard.directory, name) != Some((expected, libc::S_IFREG))
        {
            bail!("recovery copy changed during verification");
        }
        Ok(())
    }

    struct AdmissionLock {
        file: File,
        name: CString,
        identity: (u64, u64),
    }

    fn acquire_admission_lock(root: &File) -> Result<AdmissionLock> {
        let name = CString::new(".dpn-space-admission.lock")?;
        let file = open_at(root, &name, libc::O_RDWR | libc::O_CREAT, 0o600)
            .context("opening recovery admission lock")?;
        let identity = identity(&file.metadata()?);
        let lock = AdmissionLock {
            file,
            name,
            identity,
        };
        verify_admission_lock(root, &lock)?;
        lock.file
            .try_lock_exclusive()
            .context("recovery admission lock is busy")?;
        verify_admission_lock(root, &lock)?;
        root.sync_all().context("syncing recovery admission lock")?;
        Ok(lock)
    }

    fn verify_admission_lock(root: &File, lock: &AdmissionLock) -> Result<()> {
        let metadata = lock.file.metadata()?;
        if !metadata.is_file()
            || metadata.uid() != unsafe { libc::geteuid() }
            || metadata.mode() & 0o077 != 0
            || metadata.nlink() != 1
            || identity(&metadata) != lock.identity
            || entry_identity(root, &lock.name) != Some((lock.identity, libc::S_IFREG))
        {
            bail!("recovery admission lock changed or is not private");
        }
        Ok(())
    }

    // Count all existing logical sizes. Pending work stores only bounded paths;
    // each directory is reopened from the held root fd, then closed after use.
    fn recovery_root_usage(root: &Path, root_handle: &File) -> Result<u128> {
        let root_metadata = root_handle.metadata()?;
        let uid = unsafe { libc::geteuid() };
        let mut stack: Vec<Vec<(CString, (u64, u64))>> = vec![Vec::new()];
        let mut entries = 0usize;
        let mut used = 0u128;
        while let Some(components) = stack.pop() {
            if components.len() > MAX_QUOTA_DEPTH {
                bail!("recovery quota directory depth exceeded");
            }
            let mut directory = root_handle.try_clone()?;
            for (name, expected) in &components {
                let child = open_at(
                    &directory,
                    name,
                    libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NONBLOCK,
                    0,
                )?;
                let metadata = child.metadata()?;
                if identity(&metadata) != *expected
                    || metadata.dev() != root_metadata.dev()
                    || metadata.uid() != uid
                    || metadata.mode() & 0o022 != 0
                {
                    bail!("recovery quota directory is untrusted or changed");
                }
                directory = child;
            }
            let metadata = directory.metadata()?;
            if !metadata.is_dir()
                || metadata.dev() != root_metadata.dev()
                || metadata.uid() != uid
                || metadata.mode() & 0o022 != 0
            {
                bail!("recovery quota directory is untrusted or changed");
            }
            for name in read_directory_names(&directory)? {
                entries += 1;
                if entries > MAX_QUOTA_ENTRIES {
                    bail!("recovery quota entry limit exceeded");
                }
                let meta = stat_at(&directory, &name)?;
                if meta.st_uid != uid || stat_identity(&meta).0 != root_metadata.dev() {
                    bail!("recovery quota entry is untrusted or on another filesystem");
                }
                match meta.st_mode & libc::S_IFMT {
                    libc::S_IFREG => {
                        if meta.st_size < 0 {
                            bail!("invalid recovery file size");
                        }
                        used = used
                            .checked_add(meta.st_size as u128)
                            .context("recovery quota overflow")?;
                        if used > MAX_ROOT_BYTES {
                            bail!("recovery root quota exceeded");
                        }
                    }
                    libc::S_IFDIR => {
                        if components.len() >= MAX_QUOTA_DEPTH {
                            bail!("recovery quota directory depth exceeded");
                        }
                        let mut child_path = components.clone();
                        child_path.push((name, stat_identity(&meta)));
                        stack.push(child_path);
                    }
                    _ => bail!("unsupported recovery quota entry"),
                }
            }
        }
        let reopened = open_protected_root(root)?;
        if identity(&reopened.metadata()?) != identity(&root_metadata) {
            bail!("recovery root changed during quota scan");
        }
        Ok(used)
    }

    fn stat_at(directory: &File, name: &CString) -> Result<libc::stat> {
        let mut value = std::mem::MaybeUninit::<libc::stat>::uninit();
        if unsafe {
            libc::fstatat(
                directory.as_raw_fd(),
                name.as_ptr(),
                value.as_mut_ptr(),
                libc::AT_SYMLINK_NOFOLLOW,
            )
        } != 0
        {
            return Err(std::io::Error::last_os_error()).context("examining recovery quota entry");
        }
        Ok(unsafe { value.assume_init() })
    }

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    fn read_directory_names(directory: &File) -> Result<Vec<CString>> {
        struct Directory(*mut libc::DIR);
        impl Drop for Directory {
            fn drop(&mut self) {
                unsafe {
                    libc::closedir(self.0);
                }
            }
        }
        let dot = CString::new(".")?;
        let file = open_at(
            directory,
            &dot,
            libc::O_RDONLY | libc::O_DIRECTORY | libc::O_NONBLOCK,
            0,
        )?;
        let fd = file.into_raw_fd();
        let ptr = unsafe { libc::fdopendir(fd) };
        if ptr.is_null() {
            let error = std::io::Error::last_os_error();
            unsafe {
                libc::close(fd);
            }
            return Err(error).context("opening recovery quota directory stream");
        }
        let stream = Directory(ptr);
        let mut names = Vec::new();
        loop {
            #[cfg(target_os = "linux")]
            unsafe {
                *libc::__errno_location() = 0;
            }
            #[cfg(target_os = "macos")]
            unsafe {
                *libc::__error() = 0;
            }
            let entry = unsafe { libc::readdir(stream.0) };
            if entry.is_null() {
                let error = std::io::Error::last_os_error();
                if error.raw_os_error().unwrap_or(0) != 0 {
                    return Err(error).context("reading recovery quota directory");
                }
                break;
            }
            let name = unsafe { CStr::from_ptr((*entry).d_name.as_ptr()) };
            if name.to_bytes() == b"." || name.to_bytes() == b".." {
                continue;
            }
            if names.len() >= MAX_QUOTA_ENTRIES {
                bail!("recovery quota entry limit exceeded");
            }
            names.push(name.to_owned());
        }
        Ok(names)
    }

    #[cfg(not(any(target_os = "linux", target_os = "macos")))]
    fn read_directory_names(_directory: &File) -> Result<Vec<CString>> {
        bail!("safe recovery quota enumeration is unsupported on this platform")
    }

    fn check_budget(root: &File, total: u64, used: u128) -> Result<()> {
        let mut stat = std::mem::MaybeUninit::<libc::statvfs>::uninit();
        if unsafe { libc::fstatvfs(root.as_raw_fd(), stat.as_mut_ptr()) } != 0 {
            return Err(std::io::Error::last_os_error()).context("checking recovery free space");
        }
        let stat = unsafe { stat.assume_init() };
        let available = (stat.f_bavail as u128).saturating_mul(stat.f_frsize as u128);
        let capacity = (stat.f_blocks as u128).saturating_mul(stat.f_frsize as u128);
        if stat.f_frsize == 0 {
            bail!("cannot determine recovery free space");
        }
        validate_budget(total, used, available, capacity)
    }

    fn validate_budget(total: u64, used: u128, available: u128, capacity: u128) -> Result<()> {
        if total > MAX_BACKUP_BYTES {
            bail!("backup exceeds the 512 GiB limit");
        }
        if used
            .checked_add(u128::from(total))
            .is_none_or(|sum| sum > MAX_ROOT_BYTES)
        {
            bail!("recovery root quota exceeded");
        }
        if capacity == 0 || available > capacity {
            bail!("invalid recovery filesystem capacity");
        }
        let reserve = MIN_SPACE_RESERVE_BYTES.max(capacity.saturating_add(19) / 20);
        if available < u128::from(total) + reserve {
            bail!("insufficient recovery free space including reserve");
        }
        Ok(())
    }

    fn copy_and_hash(input: &mut File, output: &mut File, expected: u64) -> Result<(u64, String)> {
        let mut hash = Sha256::new();
        let mut size = 0u64;
        let mut buffer = [0u8; 128 * 1024];
        while size < expected {
            let count = (expected - size).min(buffer.len() as u64) as usize;
            input.read_exact(&mut buffer[..count])?;
            output.write_all(&buffer[..count])?;
            hash.update(&buffer[..count]);
            size += count as u64;
        }
        Ok((size, format!("{:x}", hash.finalize())))
    }

    fn hash_reader(input: &mut File, expected: u64) -> Result<(u64, String)> {
        let mut hash = Sha256::new();
        let mut size = 0u64;
        let mut buffer = [0u8; 128 * 1024];
        while size < expected {
            let count = (expected - size).min(buffer.len() as u64) as usize;
            input.read_exact(&mut buffer[..count])?;
            hash.update(&buffer[..count]);
            size += count as u64;
        }
        Ok((size, format!("{:x}", hash.finalize())))
    }

    fn find_sidecars(source: &Path) -> Result<Vec<PathBuf>> {
        let stem = source
            .file_stem()
            .and_then(OsStr::to_str)
            .context("source stem is not UTF-8")?;
        if stem.is_empty() {
            bail!("source has an empty stem");
        }
        let mut result = Vec::new();
        for entry in fs::read_dir(source.parent().context("missing source directory")?)? {
            let entry = entry?;
            let name = entry.file_name();
            let Some(name) = name.to_str() else {
                // A non-UTF-8 candidate cannot be represented in the manifest.
                use std::os::unix::ffi::OsStrExt;
                let mut prefix = stem.as_bytes().to_vec();
                prefix.push(b'.');
                let bytes = name.as_bytes();
                if bytes
                    .get(..prefix.len())
                    .is_some_and(|head| head.eq_ignore_ascii_case(&prefix))
                {
                    bail!("non-UTF-8 sidecar candidate");
                }
                continue;
            };
            if entry.path() != source && matches_sidecar(name, stem)? {
                result.push(entry.path());
                if result.len() > MAX_SIDECARS {
                    bail!("too many matching sidecars");
                }
            }
        }
        result.sort();
        Ok(result)
    }

    fn matches_sidecar(name: &str, stem: &str) -> Result<bool> {
        let Some(suffix) = name.strip_prefix(stem).and_then(|s| s.strip_prefix('.')) else {
            // Even on a case-sensitive host, the underlying volume may fold
            // case. Never silently skip a plausible differently cased file.
            let lower_name = name.to_lowercase();
            let lower_prefix = format!("{}.", stem.to_lowercase());
            if let Some(case_suffix) = lower_name.strip_prefix(&lower_prefix) {
                let extension = case_suffix.rsplit_once('.').map(|(_, ext)| ext);
                if subtitle_extension(name).is_some()
                    || extension.is_some_and(|ext| !known_non_subtitle_extension(ext))
                {
                    bail!("case-ambiguous subtitle sidecar: '{name}'");
                }
            }
            return Ok(false);
        };
        let mut parts: Vec<_> = suffix.split('.').collect();
        let Some(extension) = parts.pop() else {
            return Ok(false);
        };
        if subtitle_extension(name).is_none() {
            let plausible_label = parts.first().is_some_and(|part| {
                let primary = part.split(['-', '_']).next().unwrap_or("");
                (2..=3).contains(&primary.len()) && primary.bytes().all(|b| b.is_ascii_alphabetic())
                    || [
                        "english",
                        "portuguese",
                        "spanish",
                        "french",
                        "german",
                        "japanese",
                        "forced",
                        "full",
                        "sdh",
                    ]
                    .iter()
                    .any(|label| primary.eq_ignore_ascii_case(label))
            });
            if plausible_label && !known_non_subtitle_extension(extension) {
                bail!("unsupported plausible subtitle sidecar: '{name}'");
            }
            return Ok(false);
        }
        if ![
            "srt", "vtt", "ass", "ssa", "sub", "idx", "sup", "ttml", "sbv",
        ]
        .iter()
        .any(|ext| extension.eq_ignore_ascii_case(ext))
        {
            bail!("unsupported plausible subtitle sidecar: '{name}'");
        }
        if parts.is_empty() {
            return Ok(true);
        }
        let mut has_label = false;
        let mut has_language = false;
        for part in parts {
            if [
                "forced", "sign", "signs", "song", "songs", "sdh", "cc", "hi", "default", "full",
                "complete",
            ]
            .iter()
            .any(|q| part.eq_ignore_ascii_case(q))
            {
                has_label = true;
                continue;
            }
            if !part.is_empty() && part.bytes().all(|b| b.is_ascii_digit()) {
                continue;
            }
            let mut language_parts = part.split(['-', '_']);
            let primary = language_parts.next().unwrap_or("");
            if has_language
                || !((2..=3).contains(&primary.len())
                    && primary.bytes().all(|b| b.is_ascii_alphabetic())
                    || [
                        "english",
                        "french",
                        "spanish",
                        "german",
                        "japanese",
                        "portuguese",
                        "italian",
                        "korean",
                        "chinese",
                        "russian",
                        "polish",
                        "dutch",
                    ]
                    .iter()
                    .any(|l| primary.eq_ignore_ascii_case(l)))
                || language_parts.any(|part| {
                    !((part.len() == 2 || part.len() == 4)
                        && part.bytes().all(|b| b.is_ascii_alphabetic())
                        || part.len() == 3 && part.bytes().all(|b| b.is_ascii_digit()))
                })
            {
                bail!("unsupported plausible subtitle sidecar: '{name}'");
            }
            has_language = true;
            has_label = true;
        }
        if !has_label {
            bail!("unsupported plausible subtitle sidecar: '{name}'");
        }
        Ok(true)
    }

    fn known_non_subtitle_extension(extension: &str) -> bool {
        [
            "mkv", "mp4", "m4v", "avi", "mov", "webm", "ts", "m2ts", "mp3", "flac", "m4a", "aac",
            "jpg", "jpeg", "png", "webp", "nfo",
        ]
        .iter()
        .any(|ext| extension.eq_ignore_ascii_case(ext))
    }

    fn subtitle_extension(name: &str) -> Option<&str> {
        let extension = name.rsplit_once('.')?.1;
        [
            "srt", "vtt", "ass", "ssa", "sub", "idx", "sup", "ttml", "sbv", "smi", "stl", "scc",
            "dfxp", "usf", "lrc", "txt", "xml",
        ]
        .iter()
        .any(|ext| extension.eq_ignore_ascii_case(ext))
        .then_some(extension)
    }

    #[cfg(test)]
    mod tests {
        use super::*;
        use std::os::unix::fs::{symlink, PermissionsExt};

        fn setup() -> (tempfile::TempDir, PathBuf, PathBuf) {
            let home =
                std::env::var_os("HOME").expect("HOME required for protected test directory");
            let temp = tempfile::tempdir_in(home).unwrap();
            let media = temp.path().join("media");
            let root = temp.path().join("recovery");
            fs::create_dir(&media).unwrap();
            fs::create_dir(&root).unwrap();
            fs::set_permissions(&root, fs::Permissions::from_mode(0o700)).unwrap();
            let source = media.join("Show.S01E01.mkv");
            fs::write(&source, b"original media").unwrap();
            (temp, source, root)
        }

        fn checkpoint_fixture() -> (
            tempfile::TempDir,
            PathBuf,
            PathBuf,
            PathBuf,
            RecoveryCheckpoint,
            RecoveryTargetLock,
        ) {
            let (temp, source, root) = setup();
            fs::write(source.with_file_name("Show.S01E01.eng.srt"), b"subtitles").unwrap();
            let lock = super::super::acquire_recovery_lock(&source, &root).unwrap();
            let manifest = super::super::backup_current_file(&source, &root, &lock).unwrap();
            let checkpoint =
                super::super::capture_checkpoint(&source, &root, &manifest, &lock).unwrap();
            (temp, source, root, manifest, checkpoint, lock)
        }

        #[test]
        fn original_verification_requires_unchanged_checkpointed_sidecars() {
            for case in [
                "unchanged",
                "new",
                "changed",
                "missing",
                "replaced",
                "legacy",
            ] {
                let (_temp, source, root, manifest, mut checkpoint, lock) = checkpoint_fixture();
                let sidecar = source.with_file_name("Show.S01E01.eng.srt");
                match case {
                    "new" => {
                        fs::write(source.with_file_name("Show.S01E01.jpn.srt"), b"extra").unwrap()
                    }
                    "changed" => fs::write(&sidecar, b"different").unwrap(),
                    "missing" => fs::remove_file(&sidecar).unwrap(),
                    "replaced" => {
                        let replacement = sidecar.with_extension("replacement");
                        fs::write(&replacement, b"subtitles").unwrap();
                        fs::rename(replacement, &sidecar).unwrap();
                    }
                    "legacy" => checkpoint.sidecars = None,
                    _ => {}
                }
                let result = super::super::verify_original_locked(
                    &source,
                    &root,
                    &manifest,
                    Some(&checkpoint),
                    &lock,
                );
                assert_eq!(result.is_ok(), case == "unchanged", "{case}: {result:?}");
            }
        }

        #[test]
        fn existing_recovery_lock_is_read_only_and_exclusive() {
            let (_temp, source, root) = setup();
            assert!(super::super::acquire_existing_recovery_lock(&source, &root).is_err());
            assert_eq!(fs::read_dir(&root).unwrap().count(), 0);
            let lock = super::super::acquire_recovery_lock(&source, &root).unwrap();
            let manifest = super::super::backup_current_file(&source, &root, &lock).unwrap();
            let checkpoint =
                super::super::capture_checkpoint(&source, &root, &manifest, &lock).unwrap();
            assert!(super::super::acquire_existing_recovery_lock(&source, &root).is_err());
            drop(lock);
            let mut before = read_directory_names(&open_protected_root(&root).unwrap()).unwrap();
            before.sort();
            let existing = super::super::acquire_existing_recovery_lock(&source, &root).unwrap();
            assert!(super::super::verify_original_locked(
                &source,
                &root,
                &manifest,
                Some(&checkpoint),
                &existing
            )
            .is_ok());
            assert!(super::super::acquire_existing_recovery_lock(&source, &root).is_err());
            fs::write(&source, b"changed media!").unwrap();
            assert!(super::super::verify_original_locked(
                &source,
                &root,
                &manifest,
                Some(&checkpoint),
                &existing
            )
            .is_err());
            drop(existing);
            let mut after = read_directory_names(&open_protected_root(&root).unwrap()).unwrap();
            after.sort();
            assert_eq!(before, after);
        }

        #[test]
        fn checkpoint_verifies_all_blobs_and_legacy_needs_review() {
            let (_temp, source, root, manifest, checkpoint, _lock) = checkpoint_fixture();
            assert!(
                super::super::verify_checkpoint(&source, &root, &manifest, Some(&checkpoint))
                    .is_ok()
            );
            assert!(super::super::verify_checkpoint(&source, &root, &manifest, None).is_err());
            let value = serde_json::to_vec(&checkpoint).unwrap();
            assert_eq!(
                serde_json::from_slice::<RecoveryCheckpoint>(&value).unwrap(),
                checkpoint
            );
            fs::write(manifest.parent().unwrap().join("0001.blob"), b"corrupted").unwrap();
            assert!(
                super::super::verify_checkpoint(&source, &root, &manifest, Some(&checkpoint))
                    .is_err()
            );
        }

        #[test]
        fn checkpoint_rejects_unlisted_recovery_blobs() {
            let (_temp, source, root, manifest, checkpoint, _lock) = checkpoint_fixture();
            fs::write(manifest.parent().unwrap().join("0002.blob"), b"unlisted").unwrap();
            assert!(
                super::super::verify_checkpoint(&source, &root, &manifest, Some(&checkpoint))
                    .is_err()
            );
        }

        #[test]
        fn checkpoint_rejects_corruption_missing_and_symlinked_blobs() {
            for case in ["media", "missing", "symlink", "same_bytes_replacement"] {
                let (_temp, source, root, manifest, checkpoint, _lock) = checkpoint_fixture();
                let blob = manifest.parent().unwrap().join("0000.blob");
                match case {
                    "media" => fs::write(&blob, b"corrupted media").unwrap(),
                    "missing" => fs::remove_file(&blob).unwrap(),
                    "symlink" => {
                        fs::remove_file(&blob).unwrap();
                        symlink(&source, &blob).unwrap();
                    }
                    "same_bytes_replacement" => {
                        let bytes = fs::read(&blob).unwrap();
                        let original = blob.with_file_name("original");
                        fs::rename(&blob, &original).unwrap();
                        fs::write(&blob, bytes).unwrap();
                        fs::set_permissions(&blob, fs::Permissions::from_mode(0o600)).unwrap();
                        fs::remove_file(original).unwrap();
                    }
                    _ => unreachable!(),
                }
                assert!(
                    super::super::verify_checkpoint(&source, &root, &manifest, Some(&checkpoint))
                        .is_err(),
                    "{case}"
                );
            }
        }

        #[test]
        fn checkpoint_rejects_manifest_changes_and_replacement() {
            for case in ["corrupt", "replace", "symlink"] {
                let (_temp, source, root, manifest, checkpoint, _lock) = checkpoint_fixture();
                let original = fs::read(&manifest).unwrap();
                match case {
                    "corrupt" => fs::write(&manifest, b"{}").unwrap(),
                    "replace" => {
                        let old = manifest.with_file_name("old");
                        fs::rename(&manifest, old).unwrap();
                        fs::write(&manifest, original).unwrap();
                        fs::set_permissions(&manifest, fs::Permissions::from_mode(0o600)).unwrap();
                    }
                    "symlink" => {
                        let old = manifest.with_file_name("old");
                        fs::rename(&manifest, &old).unwrap();
                        symlink(old, &manifest).unwrap();
                    }
                    _ => unreachable!(),
                }
                assert!(
                    super::super::verify_checkpoint(&source, &root, &manifest, Some(&checkpoint))
                        .is_err(),
                    "{case}"
                );
            }
        }

        #[test]
        fn checkpoint_rejects_changed_root_directory_and_paths() {
            let (temp, source, root, manifest, checkpoint, _lock) = checkpoint_fixture();
            let other_root = temp.path().join("other");
            fs::create_dir(&other_root).unwrap();
            fs::set_permissions(&other_root, fs::Permissions::from_mode(0o700)).unwrap();
            assert!(super::super::verify_checkpoint(
                &source,
                &other_root,
                &manifest,
                Some(&checkpoint)
            )
            .is_err());
            assert!(super::super::verify_checkpoint(
                &source.with_file_name("other.mkv"),
                &root,
                &manifest,
                Some(&checkpoint)
            )
            .is_err());
            assert!(super::super::verify_checkpoint(
                &source,
                &root,
                &manifest.with_file_name("other.json"),
                Some(&checkpoint)
            )
            .is_err());
            let dir = manifest.parent().unwrap();
            let saved = root.join("saved-directory");
            fs::rename(dir, &saved).unwrap();
            symlink(&saved, dir).unwrap();
            assert!(
                super::super::verify_checkpoint(&source, &root, &manifest, Some(&checkpoint))
                    .is_err()
            );
        }

        #[test]
        fn checkpoint_rejects_replaced_root_and_oversized_manifest() {
            {
                let (temp, source, root, manifest, checkpoint, _lock) = checkpoint_fixture();
                let old_root = temp.path().join("old-root");
                fs::rename(&root, &old_root).unwrap();
                fs::create_dir(&root).unwrap();
                fs::set_permissions(&root, fs::Permissions::from_mode(0o700)).unwrap();
                assert!(super::super::verify_checkpoint(
                    &source,
                    &root,
                    &manifest,
                    Some(&checkpoint)
                )
                .is_err());
            }
            {
                let (_temp, source, root, manifest, checkpoint, _lock) = checkpoint_fixture();
                let file = OpenOptions::new().write(true).open(&manifest).unwrap();
                file.set_len(MAX_MANIFEST_BYTES + 1).unwrap();
                assert!(super::super::verify_checkpoint(
                    &source,
                    &root,
                    &manifest,
                    Some(&checkpoint)
                )
                .is_err());
            }
        }

        #[test]
        fn checkpoint_rejects_malformed_schema_even_with_matching_digest() {
            for case in ["version", "media", "blob_path", "sidecar_path", "oversize"] {
                let (_temp, source, root, manifest, mut checkpoint, _lock) = checkpoint_fixture();
                let mut value: serde_json::Value =
                    serde_json::from_slice(&fs::read(&manifest).unwrap()).unwrap();
                match case {
                    "version" => value["version"] = 2.into(),
                    "media" => value["files"][0]["original_path"] = "/other.mkv".into(),
                    "blob_path" => value["files"][0]["backup_file"] = "../escape".into(),
                    "sidecar_path" => value["files"][1]["original_path"] = "/other.srt".into(),
                    "oversize" => value["files"][0]["size"] = (MAX_BACKUP_BYTES + 1).into(),
                    _ => unreachable!(),
                }
                let bytes = serde_json::to_vec(&value).unwrap();
                fs::write(&manifest, &bytes).unwrap();
                checkpoint.manifest_size = bytes.len() as u64;
                checkpoint.manifest_sha256 = format!("{:x}", Sha256::digest(&bytes));
                assert!(
                    super::super::verify_checkpoint(&source, &root, &manifest, Some(&checkpoint))
                        .is_err(),
                    "{case}"
                );
            }
        }

        #[test]
        fn checkpoint_capture_uses_the_published_backup_not_a_later_manifest() {
            let (_temp, source, root) = setup();
            fs::write(source.with_file_name("Show.S01E01.eng.srt"), b"subtitles").unwrap();
            let lock = super::super::acquire_recovery_lock(&source, &root).unwrap();
            assert!(
                super::super::capture_checkpoint(&source, &root, &root.join("missing"), &lock)
                    .is_err()
            );
            let manifest = super::super::backup_current_file(&source, &root, &lock).unwrap();
            let sidecar = manifest.parent().unwrap().join("0001.blob");
            let changed = b"modified!";
            fs::write(&sidecar, changed).unwrap();
            let mut value: serde_json::Value =
                serde_json::from_slice(&fs::read(&manifest).unwrap()).unwrap();
            value["files"][1]["size"] = (changed.len() as u64).into();
            value["files"][1]["sha256"] = format!("{:x}", Sha256::digest(changed)).into();
            fs::write(&manifest, serde_json::to_vec(&value).unwrap()).unwrap();
            assert!(super::super::capture_checkpoint(&source, &root, &manifest, &lock).is_err());
        }

        #[test]
        fn checkpoint_capture_rejects_changed_source_and_wrong_lock() {
            let (_temp, source, root, manifest, _checkpoint, lock) = checkpoint_fixture();
            fs::write(&source, b"changed media!").unwrap();
            assert!(super::super::capture_checkpoint(&source, &root, &manifest, &lock).is_err());
            let other = source.with_file_name("Other.mkv");
            fs::write(&other, b"other").unwrap();
            assert!(super::super::capture_checkpoint(&other, &root, &manifest, &lock).is_err());
        }

        fn backup_directories(root: &Path) -> Result<Vec<PathBuf>> {
            let mut directories = Vec::new();
            for entry in fs::read_dir(root)? {
                let entry = entry?;
                if entry
                    .file_name()
                    .to_string_lossy()
                    .starts_with(".dpn-recovery-")
                {
                    directories.push(entry.path());
                }
            }
            Ok(directories)
        }

        fn single_backup_directory(root: &Path) -> Result<PathBuf> {
            let mut directories = backup_directories(root)?;
            if directories.len() != 1 {
                bail!("expected one backup directory");
            }
            Ok(directories.remove(0))
        }

        #[test]
        fn stat_identity_matches_standard_file_metadata() {
            let (_temp, source, _root) = setup();
            let directory = open_directory(source.parent().unwrap()).unwrap();
            let name = CString::new(source.file_name().unwrap().as_bytes()).unwrap();
            assert_eq!(
                stat_identity(&stat_at(&directory, &name).unwrap()),
                identity(&fs::metadata(&source).unwrap())
            );
        }

        #[test]
        fn copies_media_and_matching_sidecars_with_verified_manifest() {
            let (_temp, source, root) = setup();
            let dir = source.parent().unwrap();
            fs::write(dir.join("Show.S01E01.en.srt"), b"english text").unwrap();
            fs::write(dir.join("Show.S01E01.eng.forced.srt"), b"forced text").unwrap();
            fs::write(dir.join("Show.S01E01.forced.srt"), b"unlabelled forced").unwrap();
            fs::write(dir.join("Show.S01E01.sdh.en.vtt"), b"label order").unwrap();
            fs::write(dir.join("Show.S01E01.en.full.srt"), b"full track").unwrap();
            fs::write(dir.join("Show.S01E01.portuguese.srt"), b"portuguese track").unwrap();
            fs::write(dir.join("Show.S01E02.en.srt"), b"another episode").unwrap();
            fs::set_permissions(&source, fs::Permissions::from_mode(0o640)).unwrap();
            let initial_metadata = fs::metadata(&source).unwrap();

            let lock = super::super::acquire_recovery_lock(&source, &root).unwrap();
            let manifest = super::super::backup_current_file(&source, &root, &lock).unwrap();
            let data: serde_json::Value =
                serde_json::from_slice(&fs::read(&manifest).unwrap()).unwrap();
            let files = data["files"].as_array().unwrap();
            assert_eq!(files.len(), 7);
            for item in files {
                let original = PathBuf::from(item["original_path"].as_str().unwrap());
                let backup = manifest
                    .parent()
                    .unwrap()
                    .join(item["backup_file"].as_str().unwrap());
                let original_data = fs::read(&original).unwrap();
                assert_eq!(fs::read(&backup).unwrap(), original_data);
                assert_ne!(
                    identity(&fs::metadata(&original).unwrap()),
                    identity(&fs::metadata(&backup).unwrap())
                );
                assert_eq!(item["size"].as_u64().unwrap(), original_data.len() as u64);
                assert_eq!(
                    item["sha256"].as_str().unwrap(),
                    format!("{:x}", Sha256::digest(&original_data))
                );
            }
            let source_record = files
                .iter()
                .find(|item| item["original_path"].as_str() == source.to_str())
                .unwrap();
            let metadata = &source_record["original_metadata"];
            assert_eq!(
                metadata["mode"].as_u64(),
                Some(initial_metadata.mode() as u64)
            );
            assert_eq!(
                metadata["uid"].as_u64(),
                Some(initial_metadata.uid() as u64)
            );
            assert_eq!(
                metadata["gid"].as_u64(),
                Some(initial_metadata.gid() as u64)
            );
            assert_eq!(
                metadata["mtime_secs"].as_i64(),
                Some(initial_metadata.mtime())
            );
            assert_eq!(
                metadata["mtime_nsec"].as_i64(),
                Some(initial_metadata.mtime_nsec())
            );
            assert_eq!(
                metadata["atime_secs"].as_i64(),
                Some(initial_metadata.atime())
            );
            assert_eq!(
                metadata["atime_nsec"].as_i64(),
                Some(initial_metadata.atime_nsec())
            );
            assert_eq!(
                metadata["ctime_secs"].as_i64(),
                Some(initial_metadata.ctime())
            );
            assert_eq!(
                metadata["ctime_nsec"].as_i64(),
                Some(initial_metadata.ctime_nsec())
            );
            assert_eq!(
                fs::metadata(manifest.parent().unwrap()).unwrap().mode() & 0o777,
                0o700
            );
        }

        #[test]
        fn matching_target_lock_is_required_and_remains_exclusive_until_dropped() {
            let (_temp, source, root) = setup();
            let lock = super::super::acquire_recovery_lock(&source, &root).unwrap();
            assert!(super::super::acquire_recovery_lock(&source, &root).is_err());
            let dotted_source = source
                .parent()
                .unwrap()
                .join(".")
                .join(source.file_name().unwrap());
            assert!(super::super::acquire_recovery_lock(&dotted_source, &root).is_err());
            let other = source.parent().unwrap().join("another.mkv");
            fs::write(&other, b"other").unwrap();
            assert!(super::super::backup_current_file(&other, &root, &lock).is_err());
            assert!(super::super::backup_current_file(&source, &root, &lock).is_ok());
            assert!(super::super::acquire_recovery_lock(&source, &root).is_err());
            drop(lock);
            assert!(super::super::acquire_recovery_lock(&source, &root).is_ok());
        }

        #[test]
        fn repeated_backups_never_overwrite_existing_files() {
            let (_temp, source, root) = setup();
            fs::write(root.join("existing"), b"keep").unwrap();
            let first = backup(&source, &root).unwrap();
            let second = backup(&source, &root).unwrap();
            assert_ne!(first, second);
            assert!(first.is_file() && second.is_file());
            assert_eq!(fs::read(root.join("existing")).unwrap(), b"keep");
        }

        #[test]
        fn rejects_unsafe_roots_and_ambiguous_sidecars() {
            let (temp, source, root) = setup();
            fs::set_permissions(&root, fs::Permissions::from_mode(0o755)).unwrap();
            assert!(backup(&source, &root).is_err());
            fs::set_permissions(&root, fs::Permissions::from_mode(0o700)).unwrap();
            let unsafe_parent = temp.path().join("unsafe");
            let nested_root = unsafe_parent.join("root");
            fs::create_dir(&unsafe_parent).unwrap();
            fs::create_dir(&nested_root).unwrap();
            fs::set_permissions(&unsafe_parent, fs::Permissions::from_mode(0o777)).unwrap();
            fs::set_permissions(&nested_root, fs::Permissions::from_mode(0o700)).unwrap();
            assert!(backup(&source, &nested_root).is_err());
            let sidecar = source
                .parent()
                .unwrap()
                .join("Show.S01E01.en.commentary.srt");
            fs::write(&sidecar, b"unsupported").unwrap();
            assert!(backup(&source, &root).is_err());
            fs::remove_file(sidecar).unwrap();
            let differently_cased = source.parent().unwrap().join("show.S01E01.en.srt");
            fs::write(&differently_cased, b"ambiguous").unwrap();
            assert!(backup(&source, &root).is_err());
            fs::remove_file(differently_cased).unwrap();
            let unknown_format = source.parent().unwrap().join("Show.S01E01.en.scc");
            fs::write(&unknown_format, b"unsupported format").unwrap();
            assert!(backup(&source, &root).is_err());
            fs::remove_file(unknown_format).unwrap();
            let unknown_extension = source.parent().unwrap().join("Show.S01E01.en.unknownext");
            fs::write(&unknown_extension, b"unsupported format").unwrap();
            assert!(backup(&source, &root).is_err());
            fs::remove_file(unknown_extension).unwrap();
            let ttml_xml = source.parent().unwrap().join("Show.S01E01.en.xml");
            fs::write(&ttml_xml, b"<tt></tt>").unwrap();
            let error = backup(&source, &root).unwrap_err();
            assert!(format!("{error:#}").contains("Show.S01E01.en.xml"));
            fs::remove_file(ttml_xml).unwrap();
            let unlabelled_xml = source.parent().unwrap().join("Show.S01E01.xml");
            fs::write(&unlabelled_xml, b"<tt></tt>").unwrap();
            let error = backup(&source, &root).unwrap_err();
            assert!(format!("{error:#}").contains("Show.S01E01.xml"));
            assert!(backup_directories(&root).unwrap().is_empty());
        }

        #[test]
        fn rejects_unsafe_paths_and_aliases() {
            let (temp, source, root) = setup();
            let inside = source.parent().unwrap().join("recovery");
            fs::create_dir(&inside).unwrap();
            assert!(backup(&source, &inside).is_err());
            let symlinked = temp.path().join("link.mkv");
            symlink(&source, &symlinked).unwrap();
            assert!(backup(&symlinked, &root).is_err());
            let link = temp.path().join("hardlink.mkv");
            fs::hard_link(&source, &link).unwrap();
            assert!(backup(&source, &root).is_err());
            fs::remove_file(link).unwrap();
            symlink(&source, source.parent().unwrap().join("Show.S01E01.en.srt")).unwrap();
            assert!(backup(&source, &root).is_err());
            assert!(backup_directories(&root).unwrap().is_empty());
        }

        #[test]
        fn rejects_changed_sources_and_cleans_only_own_incomplete_backup() {
            let (_temp, source, root) = setup();
            fs::write(root.join("unrelated"), b"keep").unwrap();
            let result = backup_with_hook(&source, &root, || {
                fs::write(&source, b"changed media")?;
                Ok(())
            });
            assert!(result.is_err());
            assert!(backup_directories(&root).unwrap().is_empty());
            assert_eq!(fs::read(root.join("unrelated")).unwrap(), b"keep");
            assert!(backup_with_hook(&source, &root, || bail!("simulated failure")).is_err());
            assert!(backup_directories(&root).unwrap().is_empty());
            assert_eq!(fs::read(root.join("unrelated")).unwrap(), b"keep");
        }

        #[test]
        fn rejects_source_permission_change_during_backup() {
            let (_temp, source, root) = setup();
            fs::set_permissions(&source, fs::Permissions::from_mode(0o644)).unwrap();
            let result = backup_with_hook(&source, &root, || {
                fs::set_permissions(&source, fs::Permissions::from_mode(0o600))?;
                Ok(())
            });
            assert!(result.is_err());
            assert!(backup_directories(&root).unwrap().is_empty());
        }

        #[test]
        fn manifest_conflict_does_not_overwrite_or_remove_unknown_files() {
            let (_temp, source, root) = setup();
            let mut conflict = None;
            let result = backup_with_hook(&source, &root, || {
                let directory = single_backup_directory(&root)?;
                let path = directory.join("manifest.json");
                fs::write(&path, b"do not overwrite")?;
                conflict = Some(path);
                Ok(())
            });
            assert!(result.is_err());
            let path = conflict.unwrap();
            assert_eq!(fs::read(&path).unwrap(), b"do not overwrite");
            assert!(!path.parent().unwrap().join("0000.blob").exists());
            assert_eq!(fs::read(&source).unwrap(), b"original media");
        }

        #[test]
        fn rejects_root_permission_change_and_corrupted_pending_manifest() {
            let (_temp, source, root) = setup();
            let result = backup_with_hook(&source, &root, || {
                fs::set_permissions(&root, fs::Permissions::from_mode(0o777))?;
                Ok(())
            });
            assert!(result.is_err());
            fs::set_permissions(&root, fs::Permissions::from_mode(0o700)).unwrap();
            assert!(backup_directories(&root).unwrap().is_empty());
            let result = backup_with_hook(&source, &root, || {
                let directory = single_backup_directory(&root)?;
                fs::write(directory.join(".manifest.pending"), b"corrupt")?;
                Ok(())
            });
            assert!(result.is_err());
            assert!(backup_directories(&root).unwrap().is_empty());
        }

        #[test]
        fn rejects_corrupted_copy_before_manifest() {
            let (_temp, source, root) = setup();
            let result = backup_with_hook(&source, &root, || {
                let directory = single_backup_directory(&root)?;
                fs::write(directory.join("0000.blob"), b"corrupted")?;
                Ok(())
            });
            assert!(result.is_err());
            assert!(backup_directories(&root).unwrap().is_empty());
        }

        #[test]
        fn copy_is_bounded_and_oversized_source_is_rejected() {
            let (_temp, source, root) = setup();
            let capacity = 100 * 1024 * 1024 * 1024u128;
            assert!(validate_budget(10, 0, MIN_SPACE_RESERVE_BYTES + 9, capacity).is_err());
            assert!(validate_budget(10, 0, MIN_SPACE_RESERVE_BYTES + 10, capacity).is_ok());
            assert!(validate_budget(MAX_BACKUP_BYTES + 1, 0, capacity, capacity).is_err());
            assert!(validate_budget(10, MAX_ROOT_BYTES - 9, capacity, capacity).is_err());
            assert!(validate_budget(10, MAX_ROOT_BYTES - 10, capacity, capacity).is_ok());
            let big_capacity = 500 * 1024 * 1024 * 1024u128;
            let five_percent = big_capacity / 20;
            assert!(validate_budget(10, 0, five_percent + 9, big_capacity).is_err());
            assert!(validate_budget(10, 0, five_percent + 10, big_capacity).is_ok());
            let output_path = root.join("bounded.tmp");
            let mut input = File::open(&source).unwrap();
            let mut output = OpenOptions::new()
                .read(true)
                .write(true)
                .create_new(true)
                .open(&output_path)
                .unwrap();
            let (size, _) = copy_and_hash(&mut input, &mut output, 3).unwrap();
            assert_eq!(size, 3);
            assert_eq!(fs::read(&output_path).unwrap(), b"ori");
            input.seek(SeekFrom::Start(0)).unwrap();
            assert!(hash_reader(&mut input, 100).is_err());
            fs::remove_file(output_path).unwrap();
            OpenOptions::new()
                .write(true)
                .open(&source)
                .unwrap()
                .set_len(MAX_BACKUP_BYTES + 1)
                .unwrap();
            assert!(backup(&source, &root).is_err());
            assert!(backup_directories(&root).unwrap().is_empty());
        }

        #[test]
        fn global_admission_serializes_different_targets_and_preserves_backups() {
            let (_temp, source, root) = setup();
            let other = source.parent().unwrap().join("other.mkv");
            fs::write(&other, b"other media").unwrap();
            let lock_a = super::super::acquire_recovery_lock(&source, &root).unwrap();
            let lock_b = super::super::acquire_recovery_lock(&other, &root).unwrap();
            let first = backup_with_hook(&source, &root, || {
                let error = super::super::backup_current_file(&other, &root, &lock_b).unwrap_err();
                assert!(format!("{error:#}").contains("admission lock"));
                Ok(())
            })
            .unwrap();
            let second = super::super::backup_current_file(&other, &root, &lock_b).unwrap();
            assert!(first.is_file() && second.is_file());
            assert_eq!(backup_directories(&root).unwrap().len(), 2);
            drop(lock_a);
        }

        #[test]
        fn quota_rejects_symlinks_without_deleting_them() {
            let (_temp, source, root) = setup();
            let unsafe_entry = root.join("prior-link");
            symlink(&source, &unsafe_entry).unwrap();
            assert!(backup(&source, &root).is_err());
            assert!(fs::symlink_metadata(&unsafe_entry)
                .unwrap()
                .file_type()
                .is_symlink());
            assert_eq!(fs::read(&source).unwrap(), b"original media");
            assert!(backup_directories(&root).unwrap().is_empty());
        }

        #[test]
        fn quota_walk_handles_wide_roots_under_low_descriptor_limit() {
            const CHILD_ROOT: &str = "DPN_QUOTA_LOW_FD_TEST_ROOT";
            if let Some(root) = std::env::var_os(CHILD_ROOT) {
                let root = PathBuf::from(root);
                let mut limit = std::mem::MaybeUninit::<libc::rlimit>::uninit();
                assert_eq!(
                    unsafe { libc::getrlimit(libc::RLIMIT_NOFILE, limit.as_mut_ptr()) },
                    0
                );
                let mut limit = unsafe { limit.assume_init() };
                limit.rlim_cur = limit.rlim_cur.min(64);
                assert_eq!(unsafe { libc::setrlimit(libc::RLIMIT_NOFILE, &limit) }, 0);
                let handle = open_protected_root(&root).unwrap();
                assert_eq!(recovery_root_usage(&root, &handle).unwrap(), 256);
                return;
            }
            let (_temp, _source, root) = setup();
            for index in 0..256 {
                let folder = root.join(format!("old-{index:03}"));
                fs::create_dir(&folder).unwrap();
                fs::write(folder.join("file"), b"x").unwrap();
            }
            let status = std::process::Command::new(std::env::current_exe().unwrap())
                .args([
                    "quota_walk_handles_wide_roots_under_low_descriptor_limit",
                    "--nocapture",
                ])
                .env(CHILD_ROOT, &root)
                .status()
                .unwrap();
            assert!(status.success());
        }

        #[test]
        fn quota_walk_rejects_excessive_depth() {
            let (_temp, _source, root) = setup();
            let mut directory = root.clone();
            for _ in 0..=MAX_QUOTA_DEPTH {
                directory = directory.join("d");
                fs::create_dir(&directory).unwrap();
            }
            let handle = open_protected_root(&root).unwrap();
            assert!(recovery_root_usage(&root, &handle).is_err());
            assert!(directory.is_dir());
        }

        #[test]
        fn root_quota_counts_prior_and_abandoned_files_and_never_deletes_them() {
            let (_temp, source, root) = setup();
            let existing = root.join("old-backup");
            fs::create_dir(&existing).unwrap();
            fs::set_permissions(&existing, fs::Permissions::from_mode(0o700)).unwrap();
            let prior = existing.join("old.blob");
            let file = File::create(&prior).unwrap();
            file.set_len((MAX_ROOT_BYTES - 1) as u64).unwrap();
            let root_handle = open_protected_root(&root).unwrap();
            assert_eq!(
                recovery_root_usage(&root, &root_handle).unwrap(),
                MAX_ROOT_BYTES - 1
            );
            assert!(backup(&source, &root).is_err());
            assert_eq!(
                fs::metadata(&prior).unwrap().len(),
                (MAX_ROOT_BYTES - 1) as u64
            );
            assert!(backup_directories(&root).unwrap().is_empty());
        }

        #[test]
        fn crash_boundaries_leave_only_a_complete_atomic_marker() {
            const PHASE: &str = "DPN_RECOVERY_CRASH_TEST_PHASE";
            const SOURCE: &str = "DPN_RECOVERY_CRASH_TEST_SOURCE";
            const ROOT: &str = "DPN_RECOVERY_CRASH_TEST_ROOT";
            if let Ok(phase) = std::env::var(PHASE) {
                let source = PathBuf::from(std::env::var_os(SOURCE).unwrap());
                let root = PathBuf::from(std::env::var_os(ROOT).unwrap());
                let before = || -> Result<()> {
                    let directory = single_backup_directory(&root)?;
                    assert!(directory.join(".manifest.pending").is_file());
                    assert!(!directory.join("manifest.json").exists());
                    if phase == "before" {
                        std::process::exit(73);
                    }
                    Ok(())
                };
                let after = || -> Result<()> {
                    if phase == "after" {
                        std::process::exit(74);
                    }
                    Ok(())
                };
                let _ = backup_with_hooks(&source, &root, before, after);
                panic!("crash hook was not reached");
            }
            for phase in ["before", "after"] {
                let (_temp, source, root) = setup();
                let status = std::process::Command::new(std::env::current_exe().unwrap())
                    .args([
                        "crash_boundaries_leave_only_a_complete_atomic_marker",
                        "--nocapture",
                    ])
                    .env(PHASE, phase)
                    .env(SOURCE, &source)
                    .env(ROOT, &root)
                    .status()
                    .unwrap();
                assert_eq!(status.code(), Some(if phase == "before" { 73 } else { 74 }));
                let directory = single_backup_directory(&root).unwrap();
                let marker = directory.join("manifest.json");
                if phase == "before" {
                    assert!(!marker.exists());
                } else {
                    let json: serde_json::Value =
                        serde_json::from_slice(&fs::read(marker).unwrap()).unwrap();
                    let item = &json["files"][0];
                    let data =
                        fs::read(directory.join(item["backup_file"].as_str().unwrap())).unwrap();
                    assert_eq!(data, fs::read(&source).unwrap());
                    assert_eq!(
                        item["sha256"].as_str().unwrap(),
                        format!("{:x}", Sha256::digest(&data))
                    );
                }
            }
        }

        #[test]
        fn rejects_new_sidecars_during_copy() {
            let (_temp, source, root) = setup();
            let result = backup_with_hook(&source, &root, || {
                fs::write(source.parent().unwrap().join("Show.S01E01.en.srt"), b"late")?;
                Ok(())
            });
            assert!(result.is_err());
            assert!(backup_directories(&root).unwrap().is_empty());
        }
    }
}
