//! ONNX Runtime execution-provider configuration.
//!
//! Encapsulates CUDA/DirectML/CoreML/CPU provider selection and legacy GPU
//! compatibility toggles used by PP-OCR session initialization.

use anyhow::{bail, Result};
use log::{debug, info, warn};
#[cfg(any(target_os = "linux", target_os = "windows"))]
use ort::execution_providers::cuda::{CUDAExecutionProvider, CuDNNConvAlgorithmSearch};
#[cfg(any(target_os = "linux", target_os = "windows"))]
use ort::execution_providers::ArenaExtendStrategy;
#[cfg(target_vendor = "apple")]
use ort::execution_providers::CoreMLExecutionProvider;
#[cfg(target_os = "windows")]
use ort::execution_providers::DirectMLExecutionProvider;
use ort::execution_providers::{
    CPUExecutionProvider, ExecutionProvider, ExecutionProviderDispatch,
};
use std::env;
use std::fs;
use std::path::Path;
use std::process::Command;
use std::sync::{
    atomic::{AtomicBool, Ordering},
    OnceLock,
};

use super::{thread_ocr_cuda_device, PpOcrVariant};

pub(in crate::subtitle_ocr) static ORT_ENV_INIT: OnceLock<()> = OnceLock::new();
pub(in crate::subtitle_ocr) static ORT_ENV_GPU_AVAILABLE: OnceLock<bool> = OnceLock::new();
pub(in crate::subtitle_ocr) static FORCE_CPU_EP: AtomicBool = AtomicBool::new(false);
pub(in crate::subtitle_ocr) static LEGACY_NVIDIA_MAXWELL: OnceLock<bool> = OnceLock::new();

/// Executes the prefer ppocr v3 for legacy nvidia routine.
pub(in crate::subtitle_ocr) fn prefer_ppocr_v3_for_legacy_nvidia() -> bool {
    *LEGACY_NVIDIA_MAXWELL.get_or_init(detect_legacy_nvidia_maxwell)
}

/// Executes the detect legacy nvidia maxwell routine.
pub(in crate::subtitle_ocr) fn detect_legacy_nvidia_maxwell() -> bool {
    #[cfg(not(any(target_os = "linux", target_os = "windows")))]
    {
        return false;
    }

    #[cfg(any(target_os = "linux", target_os = "windows"))]
    {
        let output = Command::new("nvidia-smi")
            .arg("--query-gpu=compute_cap,name")
            .arg("--format=csv,noheader,nounits")
            .output();

        let Ok(output) = output else {
            return false;
        };
        if !output.status.success() {
            return false;
        }

        let stdout = String::from_utf8_lossy(&output.stdout);
        for line in stdout
            .lines()
            .map(str::trim)
            .filter(|line| !line.is_empty())
        {
            let mut parts = line.splitn(2, ',').map(str::trim);
            let cap = parts.next().unwrap_or_default();
            let name = parts.next().unwrap_or_default();
            let major = cap
                .split('.')
                .next()
                .and_then(|s| s.parse::<u32>().ok())
                .unwrap_or(999);
            if major <= 5 {
                info!(
                    "Detected legacy NVIDIA GPU '{}'(compute capability {}). Auto-selecting PP-OCRv3.",
                    name, cap
                );
                return true;
            }
        }
        false
    }
}

/// Executes the require gpu routine.
pub(in crate::subtitle_ocr) fn require_gpu() -> bool {
    env::var("DPN_OCR_REQUIRE_GPU").ok().as_deref() == Some("1")
}

/// Executes the skip ppocr cls routine.
pub(in crate::subtitle_ocr) fn skip_ppocr_cls(variant: PpOcrVariant, require_gpu: bool) -> bool {
    let configured = env::var("DPN_OCR_SKIP_CLS").ok().and_then(|v| {
        let v = v.trim();
        if v.eq_ignore_ascii_case("1")
            || v.eq_ignore_ascii_case("true")
            || v.eq_ignore_ascii_case("yes")
            || v.eq_ignore_ascii_case("on")
        {
            Some(true)
        } else if v.eq_ignore_ascii_case("0")
            || v.eq_ignore_ascii_case("false")
            || v.eq_ignore_ascii_case("no")
            || v.eq_ignore_ascii_case("off")
        {
            Some(false)
        } else {
            None
        }
    });
    configured.unwrap_or(matches!(variant, PpOcrVariant::V3) && require_gpu)
}

/// Executes the force cpu execution providers routine.
pub(in crate::subtitle_ocr) fn force_cpu_execution_providers() -> bool {
    if env::var("DPN_OCR_FORCE_CPU").ok().as_deref() == Some("1") {
        return true;
    }
    FORCE_CPU_EP.load(Ordering::Relaxed)
}

/// Executes the format provider kinds routine.
pub(in crate::subtitle_ocr) fn format_provider_kinds(kinds: &[ExecutionProviderKind]) -> String {
    kinds
        .iter()
        .map(|kind| kind.label())
        .collect::<Vec<_>>()
        .join(", ")
}

#[cfg(any(target_os = "linux", target_os = "windows"))]
/// Executes the apply cuda env overrides routine.
pub(in crate::subtitle_ocr) fn apply_cuda_env_overrides(
    mut ep: CUDAExecutionProvider,
) -> CUDAExecutionProvider {
    let flags = match env::var("ORT_CUDA_FLAGS") {
        Ok(val) => val,
        Err(_) => return ep,
    };

    for chunk in flags.split(',') {
        let chunk = chunk.trim();
        if chunk.is_empty() {
            continue;
        }
        let Some((key, value)) = chunk.split_once('=') else {
            continue;
        };
        let key = key.trim();
        let value = value.trim();
        match key {
            "device_id" => {
                if let Ok(parsed) = value.parse::<i32>() {
                    ep = ep.with_device_id(parsed);
                    info!("Applied ORT_CUDA_FLAGS device_id={}", parsed);
                } else {
                    warn!("Invalid ORT_CUDA_FLAGS device_id '{}'; ignoring.", value);
                }
            }
            "gpu_mem_limit" => {
                if let Ok(parsed) = value.parse::<usize>() {
                    ep = ep.with_memory_limit(parsed);
                    info!("Applied ORT_CUDA_FLAGS gpu_mem_limit={}", parsed);
                } else {
                    warn!(
                        "Invalid ORT_CUDA_FLAGS gpu_mem_limit '{}'; ignoring.",
                        value
                    );
                }
            }
            _ => {}
        }
    }

    ep
}

#[cfg(any(target_os = "linux", target_os = "windows"))]
/// Executes the build cuda provider routine.
pub(in crate::subtitle_ocr) fn build_cuda_provider(require_gpu: bool) -> ExecutionProviderDispatch {
    let mut ep = CUDAExecutionProvider::default();
    if let Some(device_id) = thread_ocr_cuda_device() {
        ep = ep.with_device_id(device_id);
        debug!("Assigning OCR worker to CUDA device {}", device_id);
    }
    ep = apply_cuda_env_overrides(ep);
    let perf_profile = env_flag_enabled("DPN_OCR_CUDA_PERF_PROFILE");
    let safety_brakes = !env_flag_enabled("DPN_OCR_DISABLE_CUDA_SAFETY_BRAKES");
    ep = if perf_profile {
        info!("CUDA EP performance profile enabled (DPN_OCR_CUDA_PERF_PROFILE=1).");
        ep.with_conv_algorithm_search(CuDNNConvAlgorithmSearch::Exhaustive)
            .with_cuda_graph(true)
            .with_conv_max_workspace(true)
            .with_arena_extend_strategy(ArenaExtendStrategy::NextPowerOfTwo)
    } else if safety_brakes {
        info!("CUDA EP safety brakes enabled for Maxwell-class GPUs.");
        ep.with_conv_algorithm_search(CuDNNConvAlgorithmSearch::Heuristic)
            .with_cuda_graph(false)
            .with_conv_max_workspace(false)
            .with_arena_extend_strategy(ArenaExtendStrategy::SameAsRequested)
    } else {
        info!(
            "CUDA EP safety brakes disabled (DPN_OCR_DISABLE_CUDA_SAFETY_BRAKES=1); using balanced settings."
        );
        ep.with_conv_algorithm_search(CuDNNConvAlgorithmSearch::Exhaustive)
            .with_cuda_graph(false)
            .with_conv_max_workspace(true)
            .with_arena_extend_strategy(ArenaExtendStrategy::NextPowerOfTwo)
    };
    if let Some(algo) = cuda_conv_algo_override() {
        ep = ep.with_conv_algorithm_search(algo);
    }
    let strict_cuda = require_gpu || !env_flag_enabled("DPN_OCR_CUDA_FAIL_SILENT");
    let ep = ep.build();
    if strict_cuda {
        ep.error_on_failure()
    } else {
        ep
    }
}

/// Executes the env flag enabled routine.
pub(in crate::subtitle_ocr) fn env_flag_enabled(key: &str) -> bool {
    env::var(key)
        .ok()
        .map(|v| {
            let x = v.trim().to_ascii_lowercase();
            matches!(x.as_str(), "1" | "true" | "yes" | "on")
        })
        .unwrap_or(false)
}

#[cfg(any(target_os = "linux", target_os = "windows"))]
/// Executes the cuda conv algo override routine.
pub(in crate::subtitle_ocr) fn cuda_conv_algo_override() -> Option<CuDNNConvAlgorithmSearch> {
    let raw = env::var("DPN_OCR_CUDA_CONV_ALGO").ok()?;
    match raw.trim().to_ascii_lowercase().as_str() {
        "default" => {
            info!("Overriding CUDA conv algorithm via DPN_OCR_CUDA_CONV_ALGO=default.");
            Some(CuDNNConvAlgorithmSearch::Default)
        }
        "heuristic" => {
            info!("Overriding CUDA conv algorithm via DPN_OCR_CUDA_CONV_ALGO=heuristic.");
            Some(CuDNNConvAlgorithmSearch::Heuristic)
        }
        "exhaustive" => {
            info!("Overriding CUDA conv algorithm via DPN_OCR_CUDA_CONV_ALGO=exhaustive.");
            Some(CuDNNConvAlgorithmSearch::Exhaustive)
        }
        other => {
            warn!(
                "Ignoring invalid DPN_OCR_CUDA_CONV_ALGO='{}'; expected one of: default, heuristic, exhaustive.",
                other
            );
            None
        }
    }
}

/// Executes the allow legacy cuda maxwell routine.
pub(in crate::subtitle_ocr) fn allow_legacy_cuda_maxwell() -> bool {
    env_flag_enabled("DPN_OCR_ALLOW_LEGACY_CUDA")
}

#[cfg(not(any(target_os = "linux", target_os = "windows")))]
/// Executes the build cuda provider routine.
pub(in crate::subtitle_ocr) fn build_cuda_provider(
    _require_gpu: bool,
) -> ExecutionProviderDispatch {
    unreachable!("CUDA execution provider is not supported on this platform");
}

#[cfg(not(any(target_os = "linux", target_os = "windows")))]
/// Executes the cuda conv algo override routine.
pub(in crate::subtitle_ocr) fn cuda_conv_algo_override() -> Option<()> {
    None
}

#[cfg(target_os = "windows")]
/// Executes the build directml provider routine.
pub(in crate::subtitle_ocr) fn build_directml_provider(
    require_gpu: bool,
) -> ExecutionProviderDispatch {
    let mut ep = DirectMLExecutionProvider::default().build();
    if require_gpu {
        ep = ep.error_on_failure();
    }
    ep
}

#[cfg(not(target_os = "windows"))]
/// Executes the build directml provider routine.
pub(in crate::subtitle_ocr) fn build_directml_provider(
    _require_gpu: bool,
) -> ExecutionProviderDispatch {
    unreachable!("DirectML execution provider is only supported on Windows");
}

#[cfg(target_vendor = "apple")]
/// Executes the build coreml provider routine.
pub(in crate::subtitle_ocr) fn build_coreml_provider(
    require_gpu: bool,
) -> ExecutionProviderDispatch {
    let mut ep = CoreMLExecutionProvider::default().build();
    if require_gpu {
        ep = ep.error_on_failure();
    }
    ep
}

#[cfg(not(target_vendor = "apple"))]
/// Executes the build coreml provider routine.
pub(in crate::subtitle_ocr) fn build_coreml_provider(
    _require_gpu: bool,
) -> ExecutionProviderDispatch {
    unreachable!("CoreML execution provider is only supported on Apple platforms");
}

#[cfg(target_os = "windows")]
/// Executes the detect directml available routine.
pub(in crate::subtitle_ocr) fn detect_directml_available(force_cpu: bool) -> bool {
    if force_cpu {
        return false;
    }
    let directml = DirectMLExecutionProvider::default();
    match directml.is_available() {
        Ok(true) => {
            info!("ONNX Runtime DirectML execution provider available");
            true
        }
        Ok(false) => {
            warn!("ONNX Runtime DirectML execution provider not available; falling back to CPU.");
            false
        }
        Err(err) => {
            warn!(
                "Failed to query DirectML execution provider availability: {}",
                err
            );
            false
        }
    }
}

#[cfg(not(target_os = "windows"))]
/// Executes the detect directml available routine.
pub(in crate::subtitle_ocr) fn detect_directml_available(_force_cpu: bool) -> bool {
    false
}

#[cfg(target_vendor = "apple")]
/// Executes the detect coreml available routine.
pub(in crate::subtitle_ocr) fn detect_coreml_available(force_cpu: bool) -> bool {
    if force_cpu {
        return false;
    }
    let coreml = CoreMLExecutionProvider::default();
    match coreml.is_available() {
        Ok(true) => {
            info!("ONNX Runtime CoreML execution provider available");
            true
        }
        Ok(false) => {
            warn!("ONNX Runtime CoreML execution provider not available; falling back to CPU.");
            false
        }
        Err(err) => {
            warn!(
                "Failed to query CoreML execution provider availability: {}",
                err
            );
            false
        }
    }
}

#[cfg(not(target_vendor = "apple"))]
/// Executes the detect coreml available routine.
pub(in crate::subtitle_ocr) fn detect_coreml_available(_force_cpu: bool) -> bool {
    false
}

/// Executes the select execution provider plan routine.
pub(in crate::subtitle_ocr) fn select_execution_provider_plan(
    require_gpu: bool,
    cuda_available: bool,
    directml_available: bool,
    coreml_available: bool,
) -> Result<(Vec<ExecutionProviderKind>, bool)> {
    let mut kinds = Vec::new();
    let mut gpu_available = false;

    if cuda_available {
        gpu_available = true;
        kinds.push(ExecutionProviderKind::Cuda);
    }
    if directml_available {
        gpu_available = true;
        kinds.push(ExecutionProviderKind::DirectML);
    }
    if coreml_available {
        gpu_available = true;
        kinds.push(ExecutionProviderKind::CoreML);
    }

    if require_gpu && !gpu_available {
        bail!("No GPU execution providers available. Install the required GPU runtime libraries or unset DPN_OCR_REQUIRE_GPU=1 to allow CPU fallback.");
    }

    kinds.push(ExecutionProviderKind::Cpu);
    Ok((kinds, gpu_available))
}

#[cfg(target_os = "linux")]
/// Executes the library visible on system routine.
pub(in crate::subtitle_ocr) fn library_visible_on_system(lib_prefix: &str) -> bool {
    if let Ok(output) = Command::new("ldconfig").arg("-p").output() {
        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            if stdout.lines().any(|line| line.contains(lib_prefix)) {
                return true;
            }
        }
    }

    if let Ok(ld_library_path) = env::var("LD_LIBRARY_PATH") {
        for dir in ld_library_path.split(':').filter(|s| !s.trim().is_empty()) {
            let path = Path::new(dir);
            if !path.is_dir() {
                continue;
            }
            let entries = match fs::read_dir(path) {
                Ok(entries) => entries,
                Err(_) => continue,
            };
            for entry in entries.flatten() {
                if entry.file_name().to_string_lossy().starts_with(lib_prefix) {
                    return true;
                }
            }
        }
    }

    const COMMON_LIB_DIRS: [&str; 6] = [
        "/usr/lib",
        "/usr/lib64",
        "/usr/local/lib",
        "/usr/local/lib64",
        "/usr/local/cuda/lib64",
        "/opt/cuda/lib64",
    ];
    for dir in COMMON_LIB_DIRS {
        let path = Path::new(dir);
        if !path.is_dir() {
            continue;
        }
        let entries = match fs::read_dir(path) {
            Ok(entries) => entries,
            Err(_) => continue,
        };
        for entry in entries.flatten() {
            if entry.file_name().to_string_lossy().starts_with(lib_prefix) {
                return true;
            }
        }
    }

    false
}

#[cfg(target_os = "linux")]
/// Executes the missing cuda runtime libraries routine.
pub(in crate::subtitle_ocr) fn missing_cuda_runtime_libraries() -> Vec<&'static str> {
    const REQUIRED: [&str; 4] = [
        "libcudart.so",
        "libcublas.so",
        "libcublasLt.so",
        "libcudnn.so",
    ];
    REQUIRED
        .into_iter()
        .filter(|lib| !library_visible_on_system(lib))
        .collect()
}

#[cfg(not(target_os = "linux"))]
/// Executes the missing cuda runtime libraries routine.
pub(in crate::subtitle_ocr) fn missing_cuda_runtime_libraries() -> Vec<&'static str> {
    Vec::new()
}

/// Executes the detect nvidia gpu indexes routine.
pub(in crate::subtitle_ocr) fn detect_nvidia_gpu_indexes() -> Vec<i32> {
    #[cfg(not(any(target_os = "linux", target_os = "windows")))]
    {
        return Vec::new();
    }

    #[cfg(any(target_os = "linux", target_os = "windows"))]
    {
        let output = match Command::new("nvidia-smi")
            .arg("--query-gpu=index")
            .arg("--format=csv,noheader")
            .output()
        {
            Ok(out) if out.status.success() => out,
            _ => return Vec::new(),
        };

        let mut indexes: Vec<i32> = String::from_utf8_lossy(&output.stdout)
            .lines()
            .filter_map(|line| line.trim().parse::<i32>().ok())
            .collect();
        indexes.sort_unstable();
        indexes.dedup();
        indexes
    }
}

/// Executes the build execution providers routine.
pub(in crate::subtitle_ocr) fn build_execution_providers() -> Result<ExecutionProviderSelection> {
    let require_gpu = require_gpu();
    if require_gpu {
        info!("DPN_OCR_REQUIRE_GPU=1; GPU execution provider is required.");
    }
    let mut force_cpu = force_cpu_execution_providers();
    if force_cpu {
        warn!("DPN_OCR_FORCE_CPU=1; disabling GPU execution providers.");
    }
    if require_gpu && force_cpu {
        bail!("DPN_OCR_REQUIRE_GPU=1 cannot be used with DPN_OCR_FORCE_CPU=1.");
    }
    let mut providers = Vec::new();
    let cuda_available = if force_cpu {
        false
    } else {
        #[cfg(any(target_os = "linux", target_os = "windows"))]
        {
            let cuda = CUDAExecutionProvider::default();
            match cuda.is_available() {
                Ok(true) => {
                    info!("ONNX Runtime CUDA execution provider available");
                    true
                }
                Ok(false) => {
                    warn!(
                        "ONNX Runtime CUDA execution provider not available; falling back to CPU."
                    );
                    false
                }
                Err(err) => {
                    warn!(
                        "Failed to query CUDA execution provider availability: {}",
                        err
                    );
                    false
                }
            }
        }
        #[cfg(not(any(target_os = "linux", target_os = "windows")))]
        {
            false
        }
    };
    if cuda_available {
        let missing = missing_cuda_runtime_libraries();
        if !missing.is_empty() {
            let msg = format!(
                "CUDA runtime appears incomplete (missing: {}). \
                 Install CUDA/cuDNN runtime libraries and ensure they are on the linker path.",
                missing.join(", ")
            );
            if require_gpu {
                bail!("{msg}");
            }
            warn!("{msg} Falling back to CPU OCR.");
            FORCE_CPU_EP.store(true, Ordering::Relaxed);
            force_cpu = true;
        }
    }

    let directml_available = detect_directml_available(force_cpu);
    let coreml_available = detect_coreml_available(force_cpu);

    if !force_cpu
        && cuda_available
        && prefer_ppocr_v3_for_legacy_nvidia()
        && !allow_legacy_cuda_maxwell()
    {
        let msg = "Detected legacy NVIDIA Maxwell GPU. Disabling CUDA EP for OCR by default; \
                   set DPN_OCR_ALLOW_LEGACY_CUDA=1 to force an experimental GPU attempt.";
        if require_gpu {
            bail!("{msg}");
        }
        warn!("{msg}");
        FORCE_CPU_EP.store(true, Ordering::Relaxed);
        force_cpu = true;
    }

    let cuda_available = if force_cpu { false } else { cuda_available };
    let directml_available = if force_cpu { false } else { directml_available };
    let coreml_available = if force_cpu { false } else { coreml_available };

    let (kinds, gpu_available) = select_execution_provider_plan(
        require_gpu,
        cuda_available,
        directml_available,
        coreml_available,
    )?;

    if gpu_available {
        info!(
            "OCR execution provider order: {}",
            format_provider_kinds(&kinds)
        );
    } else if force_cpu {
        info!("OCR execution provider order: cpu (GPU providers disabled)");
    } else {
        warn!(
            "No GPU execution providers available; PP-OCR will run on CPU. \
             Install the GPU runtime libraries or set DPN_OCR_REQUIRE_GPU=1 to fail instead."
        );
    }

    for kind in kinds {
        match kind {
            ExecutionProviderKind::Cuda => providers.push(build_cuda_provider(require_gpu)),
            ExecutionProviderKind::DirectML => providers.push(build_directml_provider(require_gpu)),
            ExecutionProviderKind::CoreML => providers.push(build_coreml_provider(require_gpu)),
            ExecutionProviderKind::Cpu => providers.push(CPUExecutionProvider::default().build()),
        }
    }

    Ok(ExecutionProviderSelection {
        providers,
        gpu_available,
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// Enumerates options for ExecutionProviderKind.
pub(in crate::subtitle_ocr) enum ExecutionProviderKind {
    Cuda,
    DirectML,
    CoreML,
    Cpu,
}

/// Implements behavior for `ExecutionProviderKind`.
impl ExecutionProviderKind {
    /// Executes the label routine.
    fn label(self) -> &'static str {
        match self {
            ExecutionProviderKind::Cuda => "cuda",
            ExecutionProviderKind::DirectML => "directml",
            ExecutionProviderKind::CoreML => "coreml",
            ExecutionProviderKind::Cpu => "cpu",
        }
    }
}

/// Stores data for ExecutionProviderSelection.
pub(in crate::subtitle_ocr) struct ExecutionProviderSelection {
    pub(in crate::subtitle_ocr) providers: Vec<ExecutionProviderDispatch>,
    pub(in crate::subtitle_ocr) gpu_available: bool,
}
