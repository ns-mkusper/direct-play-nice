use anyhow::{Context, Result};
use log::{debug, info, warn};
use std::fs::OpenOptions;
use std::io::ErrorKind;
use std::path::PathBuf;
use std::thread;
use std::time::Duration;

pub const MAX_CONCURRENT_CONVERSIONS: usize = 2;
const SLOT_PREFIX: &str = "direct-play-nice-slot";
const RETRY_SLEEP: Duration = Duration::from_millis(750);

pub struct SlotGuard {
    path: PathBuf,
}

impl SlotGuard {
    fn new(path: PathBuf) -> Self {
        Self { path }
    }
}

impl Drop for SlotGuard {
    fn drop(&mut self) {
        if let Err(err) = std::fs::remove_file(&self.path) {
            if err.kind() != ErrorKind::NotFound {
                warn!(
                    "Failed to release conversion slot '{}': {}",
                    self.path.display(),
                    err
                );
            }
        } else {
            debug!("Released conversion slot '{}'.", self.path.display());
        }
    }
}

fn lock_directory() -> PathBuf {
    if let Ok(path) = std::env::var("DIRECT_PLAY_NICE_LOCK_DIR") {
        PathBuf::from(path)
    } else {
        std::env::temp_dir()
    }
}

pub fn acquire_slot() -> Result<SlotGuard> {
    let base_dir = lock_directory();
    if let Err(err) = std::fs::create_dir_all(&base_dir) {
        if err.kind() != ErrorKind::AlreadyExists {
            warn!(
                "Failed to ensure lock directory '{}': {}",
                base_dir.display(),
                err
            );
        }
    }
    let mut logged_wait = false;
    loop {
        for idx in 0..MAX_CONCURRENT_CONVERSIONS {
            let mut path = base_dir.clone();
            path.push(format!("{}-{}.lock", SLOT_PREFIX, idx));
            match OpenOptions::new().write(true).create_new(true).open(&path) {
                Ok(mut file) => {
                    use std::io::Write;
                    let pid = std::process::id();
                    let _ = writeln!(file, "pid={pid}");
                    info!(
                        "Acquired conversion slot {}/{}",
                        idx + 1,
                        MAX_CONCURRENT_CONVERSIONS
                    );
                    return Ok(SlotGuard::new(path));
                }
                Err(err) if err.kind() == ErrorKind::AlreadyExists => continue,
                Err(err) => {
                    return Err(err).with_context(|| {
                        format!("Failed to reserve conversion slot '{}'.", path.display())
                    });
                }
            }
        }

        if !logged_wait {
            info!(
                "All conversion slots in use; waiting for one of {} active jobs to complete...",
                MAX_CONCURRENT_CONVERSIONS
            );
            logged_wait = true;
        }

        thread::sleep(RETRY_SLEEP);
    }
}
