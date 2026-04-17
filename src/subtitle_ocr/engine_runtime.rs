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
        info!(
            "Initializing {} models (det='{}', cls='{}', rec='{}')",
            variant.label(),
            models.det.display(),
            models.cls.display(),
            models.rec.display()
        );
        let mut ocr = OcrLite::new();
        ocr.init_models_custom(
            models.det.to_string_lossy().as_ref(),
            models.cls.to_string_lossy().as_ref(),
            models.rec.to_string_lossy().as_ref(),
            configure_ort_builder,
        )
        .map_err(|err| anyhow!("failed to initialize {} models: {}", variant.label(), err))?;
        Ok(Self { ocr, variant })
    }
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
    ep = apply_cuda_env_overrides(ep);
    ep = ep
        .with_conv_algorithm_search(CuDNNConvAlgorithmSearch::Heuristic)
        .with_cuda_graph(false)
        .with_conv_max_workspace(false)
        .with_arena_extend_strategy(ArenaExtendStrategy::SameAsRequested);
    info!("CUDA EP safety brakes enabled for Maxwell-class GPUs.");
    let mut ep = ep.build();
    if require_gpu {
        ep = ep.error_on_failure();
    }
    ep
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

fn build_execution_providers() -> Result<ExecutionProviderSelection> {
    let require_gpu = require_gpu();
    if require_gpu {
        info!("DPN_OCR_REQUIRE_GPU=1; GPU execution provider is required.");
    }
    let force_cpu = force_cpu_execution_providers();
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

    let directml_available = detect_directml_available(force_cpu);
    let coreml_available = detect_coreml_available(force_cpu);

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

