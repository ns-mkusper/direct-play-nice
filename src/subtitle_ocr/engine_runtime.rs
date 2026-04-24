fn init_ort_environment() -> Result<bool> {
    if ORT_ENV_INIT.get().is_some() {
        return Ok(*ORT_ENV_GPU_AVAILABLE.get().unwrap_or(&false));
    }
    let selection = build_execution_providers()?;
    match ort::init().commit() {
        Ok(true) => info!("Initialized ONNX Runtime environment for OCR execution providers"),
        Ok(false) => debug!("ONNX Runtime environment already initialized; skipping reconfigure"),
        Err(err) => {
            warn!("Failed to initialize ONNX Runtime environment: {}", err);
            return Err(anyhow!(
                "Failed to initialize ONNX Runtime environment: {err}"
            ));
        }
    }
    let _ = ORT_ENV_INIT.set(());
    let _ = ORT_ENV_GPU_AVAILABLE.set(selection.gpu_available);
    Ok(selection.gpu_available)
}

thread_local! {
    static OCR_CUDA_DEVICE_ID: std::cell::Cell<Option<i32>> = const { std::cell::Cell::new(None) };
}

pub(super) struct OcrCudaDeviceGuard {
    previous: Option<i32>,
}

impl Drop for OcrCudaDeviceGuard {
    fn drop(&mut self) {
        OCR_CUDA_DEVICE_ID.with(|slot| slot.set(self.previous));
    }
}

pub(super) fn set_thread_ocr_cuda_device(device_id: Option<i32>) -> OcrCudaDeviceGuard {
    let previous = OCR_CUDA_DEVICE_ID.with(|slot| {
        let prev = slot.get();
        slot.set(device_id);
        prev
    });
    OcrCudaDeviceGuard { previous }
}

fn thread_ocr_cuda_device() -> Option<i32> {
    OCR_CUDA_DEVICE_ID.with(|slot| slot.get())
}

struct ModelSpec {
    filename: &'static str,
    url: &'static str,
    sha256: &'static str,
}

const PPOCR_V4_DET_MODEL: ModelSpec = ModelSpec {
    filename: "ch_PP-OCRv4_det_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/det/ch_PP-OCRv4_det_infer.onnx",
    sha256: "D2A7720D45A54257208B1E13E36A8479894CB74155A5EFE29462512D42F49DA9",
};
const PPOCR_V4_CLS_MODEL: ModelSpec = ModelSpec {
    filename: "ch_ppocr_mobile_v2.0_cls_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/cls/ch_ppocr_mobile_v2.0_cls_infer.onnx",
    sha256: "E47ACEDF663230F8863FF1AB0E64DD2D82B838FCEB5957146DAB185A89D6215C",
};
const PPOCR_V4_REC_MODEL: ModelSpec = ModelSpec {
    filename: "en_PP-OCRv4_rec_infer.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/v3.7.0/onnx/PP-OCRv4/rec/en_PP-OCRv4_rec_infer.onnx",
    sha256: "E8770C967605983D1570CDF5352041DFB68FA0C21664F49F47B155ABD3E0E318",
};

const PPOCR_V3_DET_MODEL: ModelSpec = ModelSpec {
    filename: "ch_PP-OCRv3_det_infer.onnx",
    url: "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/ch_PP-OCRv3_det_infer.onnx?download=true",
    sha256: "3439588C030FAEA393A54515F51E983D8E155B19A2E8ABA7891934C1CF0DE526",
};
const PPOCR_V3_CLS_MODEL: ModelSpec = ModelSpec {
    filename: "ch_ppocr_mobile_v2.0_cls_train.onnx",
    url: "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/ch_ppocr_mobile_v2.0_cls_train.onnx?download=true",
    sha256: "70581B300B83BABD9E0DD1D7D74C5B006869E8796DA277A70C2E405BF9D77C82",
};
const PPOCR_V3_REC_MODEL: ModelSpec = ModelSpec {
    filename: "en_PP-OCRv3_rec_infer.onnx",
    url: "https://huggingface.co/SWHL/RapidOCR/resolve/main/PP-OCRv3/en_PP-OCRv3_rec_infer.onnx?download=true",
    sha256: "EF7ABD8BD3629AE57EA2C28B425C1BD258A871B93FD2FE7C433946ADE9B5D9EA",
};

struct PpOcrModels {
    det: PathBuf,
    cls: PathBuf,
    rec: PathBuf,
}

impl PpOcrEngine {
    fn new(model_dir: &Path, variant: PpOcrVariant, skip_cls: bool) -> Result<Self> {
        let models = ensure_ppocr_models(model_dir, variant, skip_cls)?;
        let latin_rec = resolve_optional_latin_rec_model(model_dir, variant)?;
        let japanese_rec = resolve_optional_japanese_rec_model(model_dir, variant)?;
        let korean_rec = resolve_optional_korean_rec_model(model_dir, variant)?;
        let cjk_rec = resolve_optional_cjk_rec_model(model_dir, variant)?;
        info!(
            "Initializing {} models (det='{}', cls='{}', rec='{}')",
            variant.label(),
            models.det.display(),
            models.cls.display(),
            models.rec.display()
        );
        let english_ocr = init_ocr_lite(
            variant,
            "english",
            &models.det,
            &models.cls,
            &models.rec,
        )?;

        let latin_ocr = if let Some(latin_rec_path) = latin_rec {
            info!(
                "Initializing {} latin rec model at '{}'",
                variant.label(),
                latin_rec_path.display()
            );
            match init_ocr_lite(variant, "latin", &models.det, &models.cls, &latin_rec_path) {
                Ok(ocr) => Some(ocr),
                Err(err) => {
                    warn!(
                        "{} latin rec model failed to initialize; falling back to english rec model only: {:#} (debug: {:?})",
                        variant.label(),
                        err,
                        err
                    );
                    None
                }
            }
        } else {
            info!(
                "{} latin rec model not configured/found; using english rec model for all languages.",
                variant.label()
            );
            None
        };

        let japanese_ocr =
            init_optional_rec_profile(variant, "japanese", &models.det, &models.cls, japanese_rec);
        let korean_ocr =
            init_optional_rec_profile(variant, "korean", &models.det, &models.cls, korean_rec);
        let cjk_ocr = init_optional_rec_profile(variant, "cjk", &models.det, &models.cls, cjk_rec);

        Ok(Self {
            english_ocr,
            latin_ocr,
            japanese_ocr,
            korean_ocr,
            cjk_ocr,
            variant,
        })
    }
}

fn init_optional_rec_profile(
    variant: PpOcrVariant,
    profile_label: &'static str,
    det: &Path,
    cls: &Path,
    rec_path: Option<PathBuf>,
) -> Option<OcrLite> {
    let Some(rec_path) = rec_path else {
        info!(
            "{} {} rec model not configured/found; fallback routing will use other rec profiles.",
            variant.label(),
            profile_label
        );
        return None;
    };

    info!(
        "Initializing {} {} rec model at '{}'",
        variant.label(),
        profile_label,
        rec_path.display()
    );
    match init_ocr_lite(variant, profile_label, det, cls, &rec_path) {
        Ok(ocr) => Some(ocr),
        Err(err) => {
            warn!(
                "{} {} rec model failed to initialize; routing will fall back to other rec profiles: {:#} (debug: {:?})",
                variant.label(),
                profile_label,
                err,
                err
            );
            None
        }
    }
}

fn init_ocr_lite(
    variant: PpOcrVariant,
    profile_label: &str,
    det: &Path,
    cls: &Path,
    rec: &Path,
) -> Result<OcrLite> {
    let mut ocr = OcrLite::new();
    ocr.init_models_custom(
        det.to_string_lossy().as_ref(),
        cls.to_string_lossy().as_ref(),
        rec.to_string_lossy().as_ref(),
        configure_ort_builder,
    )
    .map_err(|err| {
        anyhow!(
            "failed to initialize {} {} models: {} (debug: {:?})",
            variant.label(),
            profile_label,
            err,
            err
        )
    })?;
    Ok(ocr)
}

fn configure_ort_builder(builder: SessionBuilder) -> Result<SessionBuilder, ort::Error> {
    let selection = build_execution_providers().map_err(|err| ort::Error::new(err.to_string()))?;
    let mut builder = builder.with_execution_providers(selection.providers)?;
    builder = builder.with_intra_threads(
        std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(2),
    )?;
    builder.with_inter_threads(
        std::thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(2),
    )
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExecutionProviderKind {
    Cuda,
    DirectML,
    CoreML,
    Cpu,
}

impl ExecutionProviderKind {
    fn label(self) -> &'static str {
        match self {
            ExecutionProviderKind::Cuda => "cuda",
            ExecutionProviderKind::DirectML => "directml",
            ExecutionProviderKind::CoreML => "coreml",
            ExecutionProviderKind::Cpu => "cpu",
        }
    }
}

struct ExecutionProviderSelection {
    providers: Vec<ExecutionProviderDispatch>,
    gpu_available: bool,
}

fn require_gpu() -> bool {
    env::var("DPN_OCR_REQUIRE_GPU").ok().as_deref() == Some("1")
}

fn skip_ppocr_cls(variant: PpOcrVariant, require_gpu: bool) -> bool {
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

fn force_cpu_execution_providers() -> bool {
    if env::var("DPN_OCR_FORCE_CPU").ok().as_deref() == Some("1") {
        return true;
    }
    FORCE_CPU_EP.load(Ordering::Relaxed)
}

fn format_provider_kinds(kinds: &[ExecutionProviderKind]) -> String {
    kinds
        .iter()
        .map(|kind| kind.label())
        .collect::<Vec<_>>()
        .join(", ")
}

#[cfg(any(target_os = "linux", target_os = "windows"))]
fn apply_cuda_env_overrides(mut ep: CUDAExecutionProvider) -> CUDAExecutionProvider {
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
fn build_cuda_provider(require_gpu: bool) -> ExecutionProviderDispatch {
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

fn env_flag_enabled(key: &str) -> bool {
    env::var(key)
        .ok()
        .map(|v| {
            let x = v.trim().to_ascii_lowercase();
            matches!(x.as_str(), "1" | "true" | "yes" | "on")
        })
        .unwrap_or(false)
}

fn cuda_conv_algo_override() -> Option<CuDNNConvAlgorithmSearch> {
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

fn allow_legacy_cuda_maxwell() -> bool {
    env_flag_enabled("DPN_OCR_ALLOW_LEGACY_CUDA")
}

#[cfg(not(any(target_os = "linux", target_os = "windows")))]
fn build_cuda_provider(_require_gpu: bool) -> ExecutionProviderDispatch {
    unreachable!("CUDA execution provider is not supported on this platform");
}

#[cfg(target_os = "windows")]
fn build_directml_provider(require_gpu: bool) -> ExecutionProviderDispatch {
    let mut ep = DirectMLExecutionProvider::default().build();
    if require_gpu {
        ep = ep.error_on_failure();
    }
    ep
}

#[cfg(not(target_os = "windows"))]
fn build_directml_provider(_require_gpu: bool) -> ExecutionProviderDispatch {
    unreachable!("DirectML execution provider is only supported on Windows");
}

#[cfg(target_vendor = "apple")]
fn build_coreml_provider(require_gpu: bool) -> ExecutionProviderDispatch {
    let mut ep = CoreMLExecutionProvider::default().build();
    if require_gpu {
        ep = ep.error_on_failure();
    }
    ep
}

#[cfg(not(target_vendor = "apple"))]
fn build_coreml_provider(_require_gpu: bool) -> ExecutionProviderDispatch {
    unreachable!("CoreML execution provider is only supported on Apple platforms");
}

#[cfg(target_os = "windows")]
fn detect_directml_available(force_cpu: bool) -> bool {
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
fn detect_directml_available(_force_cpu: bool) -> bool {
    false
}

#[cfg(target_vendor = "apple")]
fn detect_coreml_available(force_cpu: bool) -> bool {
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
fn detect_coreml_available(_force_cpu: bool) -> bool {
    false
}

fn select_execution_provider_plan(
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
fn library_visible_on_system(lib_prefix: &str) -> bool {
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
                if entry
                    .file_name()
                    .to_string_lossy()
                    .starts_with(lib_prefix)
                {
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
            if entry
                .file_name()
                .to_string_lossy()
                .starts_with(lib_prefix)
            {
                return true;
            }
        }
    }

    false
}

#[cfg(target_os = "linux")]
fn missing_cuda_runtime_libraries() -> Vec<&'static str> {
    const REQUIRED: [&str; 4] = ["libcudart.so", "libcublas.so", "libcublasLt.so", "libcudnn.so"];
    REQUIRED
        .into_iter()
        .filter(|lib| !library_visible_on_system(lib))
        .collect()
}

#[cfg(not(target_os = "linux"))]
fn missing_cuda_runtime_libraries() -> Vec<&'static str> {
    Vec::new()
}

pub(super) fn detect_nvidia_gpu_indexes() -> Vec<i32> {
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

fn build_execution_providers() -> Result<ExecutionProviderSelection> {
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

    // Modern ORT/CUDA builds are unreliable on legacy Maxwell cards (e.g., GTX 960).
    // Default to CPU to avoid repeated GPU-init churn unless explicitly overridden.
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
            ExecutionProviderKind::Cuda => {
                providers.push(build_cuda_provider(require_gpu));
            }
            ExecutionProviderKind::DirectML => {
                providers.push(build_directml_provider(require_gpu));
            }
            ExecutionProviderKind::CoreML => {
                providers.push(build_coreml_provider(require_gpu));
            }
            ExecutionProviderKind::Cpu => {
                providers.push(CPUExecutionProvider::default().build());
            }
        }
    }

    Ok(ExecutionProviderSelection {
        providers,
        gpu_available,
    })
}
