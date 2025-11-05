use anyhow::{Context, Result};
use fs2::FileExt;
use log::{debug, info, warn};
use serde_json::Value;
use std::fs::{self, File, OpenOptions};
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;
use std::thread;
use std::time::Duration;

const SLOT_PREFIX: &str = "direct-play-nice-slot";
const RETRY_SLEEP: Duration = Duration::from_millis(750);
const DEFAULT_JOBS_PER_GPU: usize = 2;

const MAX_JOBS_ENV: &str = "DIRECT_PLAY_NICE_MAX_JOBS";
const JOBS_PER_GPU_ENV: &str = "DIRECT_PLAY_NICE_JOBS_PER_GPU";
const LOCK_DIR_ENV: &str = "DIRECT_PLAY_NICE_LOCK_DIR";

#[derive(Clone)]
struct SlotSpec {
    gpu: Option<GpuKind>,
    path: PathBuf,
}

#[derive(Clone, Debug)]
enum GpuKind {
    Nvidia { index: usize },
    Amd { index: usize, identifier: String },
}

pub struct SlotGuard {
    file: File,
    path: PathBuf,
    gpu: Option<GpuKind>,
    prev_env: Option<PrevGpuEnv>,
}

impl Drop for SlotGuard {
    fn drop(&mut self) {
        if let Some(prev) = self.prev_env.take() {
            prev.restore();
        }

        if let Err(err) = self.file.unlock() {
            warn!(
                "Failed to release conversion slot '{}': {}",
                self.path.display(),
                err
            );
        } else {
            debug!("Released conversion slot '{}'.", self.path.display());
        }
    }
}

impl SlotGuard {
    fn apply_gpu_affinity(&mut self) {
        match &self.gpu {
            Some(GpuKind::Nvidia { index }) => {
                let prev = PrevGpuEnv::capture();
                self.prev_env = Some(prev);

                let idx_str = index.to_string();
                std::env::set_var("CUDA_VISIBLE_DEVICES", &idx_str);
                std::env::set_var("GPU_DEVICE_ORDINAL", &idx_str);
                std::env::set_var("NV_GPU", &idx_str);
                info!("Pinned conversion to NVIDIA GPU {}", index);
            }
            Some(GpuKind::Amd { index, identifier }) => {
                let prev = PrevGpuEnv::capture();
                self.prev_env = Some(prev);

                #[cfg(not(target_os = "windows"))]
                {
                    let idx_str = index.to_string();
                    std::env::set_var("ROCR_VISIBLE_DEVICES", &idx_str);
                    std::env::set_var("HIP_VISIBLE_DEVICES", &idx_str);
                    std::env::set_var("GPU_DEVICE_IDENTIFIER", identifier);
                }
                info!("Pinned conversion to AMD GPU {} ({})", index, identifier);
            }
            None => {}
        }
    }
}

struct PrevGpuEnv {
    cuda_visible: Option<Option<String>>,
    gpu_device_ordinal: Option<Option<String>>,
    nv_gpu: Option<Option<String>>,
    rocr_visible: Option<Option<String>>,
    hip_visible: Option<Option<String>>,
    gpu_identifier: Option<Option<String>>,
}

impl PrevGpuEnv {
    fn capture() -> Self {
        PrevGpuEnv {
            cuda_visible: Some(std::env::var("CUDA_VISIBLE_DEVICES").ok()),
            gpu_device_ordinal: Some(std::env::var("GPU_DEVICE_ORDINAL").ok()),
            nv_gpu: Some(std::env::var("NV_GPU").ok()),
            rocr_visible: Some(std::env::var("ROCR_VISIBLE_DEVICES").ok()),
            hip_visible: Some(std::env::var("HIP_VISIBLE_DEVICES").ok()),
            gpu_identifier: Some(std::env::var("GPU_DEVICE_IDENTIFIER").ok()),
        }
    }

    fn restore(self) {
        restore_env("CUDA_VISIBLE_DEVICES", self.cuda_visible);
        restore_env("GPU_DEVICE_ORDINAL", self.gpu_device_ordinal);
        restore_env("NV_GPU", self.nv_gpu);
        restore_env("ROCR_VISIBLE_DEVICES", self.rocr_visible);
        restore_env("HIP_VISIBLE_DEVICES", self.hip_visible);
        restore_env("GPU_DEVICE_IDENTIFIER", self.gpu_identifier);
    }
}

fn restore_env(key: &str, value: Option<Option<String>>) {
    match value {
        Some(Some(val)) => std::env::set_var(key, val),
        Some(None) => std::env::remove_var(key),
        None => {}
    }
}

pub fn acquire_slot() -> Result<SlotGuard> {
    let specs = slot_specs();
    let mut logged_wait = false;

    loop {
        for spec in specs.iter() {
            let file = OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .open(&spec.path)
                .with_context(|| format!("Failed to open lock file '{}'.", spec.path.display()))?;

            match file.try_lock_exclusive() {
                Ok(_) => {
                    info!("Acquired conversion slot '{}'", spec.path.display());
                    let mut guard = SlotGuard {
                        file,
                        path: spec.path.clone(),
                        gpu: spec.gpu.clone(),
                        prev_env: None,
                    };
                    guard.apply_gpu_affinity();
                    return Ok(guard);
                }
                Err(err)
                    if err.kind() == ErrorKind::WouldBlock
                        || matches!(err.raw_os_error(), Some(32 | 33)) =>
                {
                    continue;
                }
                Err(err) => {
                    return Err(err)
                        .with_context(|| format!("Failed to lock '{}'.", spec.path.display()));
                }
            }
        }

        if !logged_wait {
            info!("All conversion slots in use; waiting for one to free up...");
            logged_wait = true;
        }
        thread::sleep(RETRY_SLEEP);
    }
}

fn slot_specs() -> &'static [SlotSpec] {
    static SPECS: OnceLock<Vec<SlotSpec>> = OnceLock::new();
    SPECS.get_or_init(build_slot_specs)
}

fn build_slot_specs() -> Vec<SlotSpec> {
    let base_dir = lock_directory();
    if let Err(err) = fs::create_dir_all(&base_dir) {
        warn!(
            "Failed to create lock directory '{}': {}",
            base_dir.display(),
            err
        );
    }

    let gpus = detect_gpus();
    if let Some(max_jobs) = max_jobs_override() {
        return build_override_slots(&base_dir, &gpus, max_jobs);
    }

    if !gpus.is_empty() {
        let jobs = jobs_per_gpu();
        return build_gpu_slots(&base_dir, &gpus, jobs);
    }

    vec![SlotSpec {
        gpu: None,
        path: base_dir.join(format!("{}-cpu.lock", SLOT_PREFIX)),
    }]
}

fn build_override_slots(base: &Path, gpus: &[GpuKind], max_jobs: usize) -> Vec<SlotSpec> {
    let mut specs = Vec::new();
    let total = max_jobs.max(1);

    for idx in 0..total {
        let gpu = if !gpus.is_empty() {
            Some(gpus[idx % gpus.len()].clone())
        } else {
            None
        };
        let path = base.join(format!("{}-override-{}.lock", SLOT_PREFIX, idx));
        specs.push(SlotSpec { gpu, path });
    }
    specs
}

fn build_gpu_slots(base: &Path, gpus: &[GpuKind], jobs_per_gpu: usize) -> Vec<SlotSpec> {
    let jobs = jobs_per_gpu.max(1);
    let mut specs = Vec::new();
    for gpu in gpus {
        for slot in 0..jobs {
            let label = match gpu {
                GpuKind::Nvidia { index } => format!("nv{}", index),
                GpuKind::Amd { index, .. } => format!("amd{}", index),
            };
            let path = base.join(format!("{}-{}-slot{}.lock", SLOT_PREFIX, label, slot));
            specs.push(SlotSpec {
                gpu: Some(gpu.clone()),
                path,
            });
        }
    }
    if specs.is_empty() {
        warn!("GPU detection returned no slots; falling back to CPU queue.");
        specs.push(SlotSpec {
            gpu: None,
            path: base.join(format!("{}-cpu.lock", SLOT_PREFIX)),
        });
    }
    specs
}

fn lock_directory() -> PathBuf {
    if let Ok(path) = std::env::var(LOCK_DIR_ENV) {
        PathBuf::from(path)
    } else {
        std::env::temp_dir()
    }
}

fn max_jobs_override() -> Option<usize> {
    if let Ok(val) = std::env::var(MAX_JOBS_ENV) {
        if let Ok(parsed) = val.trim().parse::<usize>() {
            if parsed >= 1 {
                info!("Max conversions overridden via {}={}", MAX_JOBS_ENV, parsed);
                return Some(parsed);
            }
        }
        warn!(
            "Ignoring invalid {} value '{}'; using auto-detected limit.",
            MAX_JOBS_ENV, val
        );
    }
    None
}

fn jobs_per_gpu() -> usize {
    if let Ok(val) = std::env::var(JOBS_PER_GPU_ENV) {
        if let Ok(parsed) = val.trim().parse::<usize>() {
            if parsed >= 1 {
                info!(
                    "Per-GPU conversion slots overridden via {}={}",
                    JOBS_PER_GPU_ENV, parsed
                );
                return parsed;
            }
        }
        warn!(
            "Ignoring invalid {} value '{}'; using default {} per GPU.",
            JOBS_PER_GPU_ENV, val, DEFAULT_JOBS_PER_GPU
        );
    }
    DEFAULT_JOBS_PER_GPU
}

fn detect_gpus() -> Vec<GpuKind> {
    let mut gpus = detect_nvidia_gpus();
    let amd = detect_amd_gpus();
    gpus.extend(amd);
    gpus
}

fn detect_nvidia_gpus() -> Vec<GpuKind> {
    let output = match Command::new("nvidia-smi")
        .arg("--query-gpu=index")
        .arg("--format=csv,noheader")
        .output()
    {
        Ok(out) if out.status.success() => out,
        _ => return Vec::new(),
    };

    let stdout = String::from_utf8_lossy(&output.stdout);
    let mut indexes = Vec::new();
    for line in stdout.lines() {
        if let Ok(idx) = line.trim().parse::<usize>() {
            indexes.push(idx);
        }
    }
    indexes.sort_unstable();
    indexes.dedup();
    if !indexes.is_empty() {
        info!(
            "Detected NVIDIA GPUs with indexes {:?}; defaulting to {} jobs per GPU.",
            indexes, DEFAULT_JOBS_PER_GPU
        );
    }

    indexes
        .into_iter()
        .map(|idx| GpuKind::Nvidia { index: idx })
        .collect()
}

fn detect_amd_gpus() -> Vec<GpuKind> {
    if cfg!(target_os = "windows") {
        detect_amd_gpus_windows()
    } else {
        detect_amd_gpus_unix()
    }
}

fn detect_amd_gpus_unix() -> Vec<GpuKind> {
    let output = match Command::new("rocm-smi")
        .arg("--showid")
        .arg("--json")
        .output()
    {
        Ok(out) if out.status.success() => out,
        _ => return Vec::new(),
    };

    let json = match serde_json::from_slice::<Value>(&output.stdout) {
        Ok(val) => val,
        Err(err) => {
            warn!("Failed to parse rocm-smi output: {}", err);
            return Vec::new();
        }
    };

    let mut gpus = Vec::new();
    if let Some(map) = json.as_object() {
        for (key, value) in map {
            if let Some(obj) = value.as_object() {
                if let Some(id_val) = obj.get("GPU ID").and_then(|v| v.as_str()) {
                    if let Ok(index) = key.trim_start_matches("GPU").trim().parse::<usize>() {
                        info!("Detected AMD GPU {} with ID {}", index, id_val);
                        gpus.push(GpuKind::Amd {
                            index,
                            identifier: id_val.to_string(),
                        });
                    }
                }
            }
        }
    }
    gpus
}

fn detect_amd_gpus_windows() -> Vec<GpuKind> {
    let output = match Command::new("powershell")
        .args([
            "-NoProfile",
            "-Command",
            "Get-CimInstance Win32_VideoController | Where-Object { $_.AdapterCompatibility -like '*AMD*' } | Select-Object Name,PNPDeviceID | ConvertTo-Json",
        ])
        .output()
    {
        Ok(out) if out.status.success() => out,
        _ => return Vec::new(),
    };

    let data = String::from_utf8_lossy(&output.stdout);
    if data.trim().is_empty() {
        return Vec::new();
    }

    let json: Value = match serde_json::from_str(&data) {
        Ok(val) => val,
        Err(err) => {
            warn!("Failed to parse PowerShell GPU JSON: {}", err);
            return Vec::new();
        }
    };

    let mut gpus = Vec::new();
    let entries = match json {
        serde_json::Value::Array(arr) => arr,
        serde_json::Value::Object(_) => vec![json],
        _ => return Vec::new(),
    };

    for (idx, entry) in entries.into_iter().enumerate() {
        if let Some(obj) = entry.as_object() {
            let name = obj
                .get("Name")
                .and_then(|v| v.as_str())
                .unwrap_or("AMD GPU");
            let pnp = obj
                .get("PNPDeviceID")
                .and_then(|v| v.as_str())
                .unwrap_or("unknown");
            info!("Detected AMD GPU {} ({})", idx, name);
            gpus.push(GpuKind::Amd {
                index: idx,
                identifier: format!("{}|{}", name, pnp),
            });
        }
    }
    gpus
}
