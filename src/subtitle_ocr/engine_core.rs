struct SubtitleCandidate {
    stream_index: i32,
    language_tag: Option<String>,
}

#[derive(Debug)]
struct OcrTask {
    order: usize,
    stream_index: i32,
    language: String,
    subtitle_path: PathBuf,
}

#[derive(Debug)]
struct OcrTaskOutput {
    order: usize,
    stream_index: i32,
    language: String,
    subtitle_path: PathBuf,
    cues: Vec<SubtitleCue>,
}

#[derive(Debug, Clone)]
struct SubtitleCue {
    start_ms: i64,
    end_ms: i64,
    text: String,
}

#[derive(Debug, Clone)]
struct OcrBoundingBox {
    left: i32,
    top: i32,
    right: i32,
    bottom: i32,
}

#[derive(Debug, Clone)]
struct OcrLine {
    text: String,
    bbox: Option<OcrBoundingBox>,
    score: Option<f32>,
    color: Option<(u8, u8, u8)>,
    italic: bool,
}

#[derive(Debug, Default)]
struct OcrOutput {
    lines: Vec<OcrLine>,
}

trait SubtitleConverter {
    fn extract_lines(&mut self, image_path: &Path, language: &str) -> Result<OcrOutput>;
}

struct TesseractEngine;

struct ExternalEngine {
    command: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PpOcrVariant {
    V3,
    V4,
}

impl PpOcrVariant {
    fn label(self) -> &'static str {
        match self {
            PpOcrVariant::V3 => "PP-OCRv3",
            PpOcrVariant::V4 => "PP-OCRv4",
        }
    }

    fn model_specs(self) -> (&'static ModelSpec, &'static ModelSpec, &'static ModelSpec) {
        match self {
            PpOcrVariant::V3 => (
                &PPOCR_V3_DET_MODEL,
                &PPOCR_V3_CLS_MODEL,
                &PPOCR_V3_REC_MODEL,
            ),
            PpOcrVariant::V4 => (
                &PPOCR_V4_DET_MODEL,
                &PPOCR_V4_CLS_MODEL,
                &PPOCR_V4_REC_MODEL,
            ),
        }
    }
}

struct PpOcrEngine {
    english_ocr: OcrLite,
    latin_ocr: Option<OcrLite>,
    japanese_ocr: Option<OcrLite>,
    korean_ocr: Option<OcrLite>,
    cjk_ocr: Option<OcrLite>,
    variant: PpOcrVariant,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OcrRecProfile {
    English,
    Latin,
    Japanese,
    Korean,
    Cjk,
}

impl SubtitleConverter for TesseractEngine {
    fn extract_lines(&mut self, image_path: &Path, language: &str) -> Result<OcrOutput> {
        let text = run_tesseract(image_path, language)?;
        if text.is_empty() {
            return Ok(OcrOutput::default());
        }
        Ok(OcrOutput {
            lines: vec![OcrLine {
                text,
                bbox: None,
                score: None,
                color: None,
                italic: false,
            }],
        })
    }
}

impl SubtitleConverter for ExternalEngine {
    fn extract_lines(&mut self, image_path: &Path, language: &str) -> Result<OcrOutput> {
        let text = run_external_ocr_command(image_path, language, &self.command)?;
        if text.is_empty() {
            return Ok(OcrOutput::default());
        }
        Ok(OcrOutput {
            lines: vec![OcrLine {
                text,
                bbox: None,
                score: None,
                color: None,
                italic: false,
            }],
        })
    }
}

impl SubtitleConverter for PpOcrEngine {
    fn extract_lines(&mut self, image_path: &Path, language: &str) -> Result<OcrOutput> {
        let rec_profile = rec_profile_for_language(language);
        let (ocr, rec_label) = match rec_profile {
            OcrRecProfile::Japanese => {
                if let Some(japanese_ocr) = self.japanese_ocr.as_mut() {
                    (japanese_ocr, "japanese")
                } else if let Some(cjk_ocr) = self.cjk_ocr.as_mut() {
                    (cjk_ocr, "cjk")
                } else {
                    (&mut self.english_ocr, "english")
                }
            }
            OcrRecProfile::Korean => {
                if let Some(korean_ocr) = self.korean_ocr.as_mut() {
                    (korean_ocr, "korean")
                } else if let Some(cjk_ocr) = self.cjk_ocr.as_mut() {
                    (cjk_ocr, "cjk")
                } else {
                    (&mut self.english_ocr, "english")
                }
            }
            OcrRecProfile::Cjk => {
                if let Some(cjk_ocr) = self.cjk_ocr.as_mut() {
                    (cjk_ocr, "cjk")
                } else {
                    (&mut self.english_ocr, "english")
                }
            }
            OcrRecProfile::Latin => {
                if let Some(latin_ocr) = self.latin_ocr.as_mut() {
                    (latin_ocr, "latin")
                } else {
                    (&mut self.english_ocr, "english")
                }
            }
            OcrRecProfile::English => (&mut self.english_ocr, "english"),
        };

        let img = load_image(image_path)?;
        let result = ocr
            .detect(&img, 50, 1024, 0.5, 0.3, 1.6, false, false)
            .map_err(|err| {
                anyhow!(
                    "{} failed (rec_profile={}, language={}): {} (debug: {:?})",
                    self.variant.label(),
                    rec_label,
                    language,
                    err,
                    err
                )
            })?;

        let mut lines = Vec::with_capacity(result.text_blocks.len());
        for block in result.text_blocks {
            let text = normalize_utf8_text(&block.text);
            if text.is_empty() {
                continue;
            }
            let bbox = bounding_box_from_points(&block.box_points);
            lines.push(OcrLine {
                text,
                bbox,
                score: Some(block.text_score),
                color: None,
                italic: false,
            });
        }

        let lines = merge_ocr_lines_with_spacing(lines);

        Ok(OcrOutput { lines })
    }
}

fn rec_profile_for_language(language: &str) -> OcrRecProfile {
    let normalized =
        map_language_tag_to_tesseract(language).unwrap_or_else(|| language.to_ascii_lowercase());
    match normalized.as_str() {
        "eng" => OcrRecProfile::English,
        "jpn" | "ja" => OcrRecProfile::Japanese,
        "kor" | "ko" => OcrRecProfile::Korean,
        "chi_sim" | "chi_tra" | "chi" | "zho" | "zh" => OcrRecProfile::Cjk,
        "fra" | "fre" | "spa" | "deu" | "ger" | "ita" | "por" | "nld" | "swe" | "dan"
        | "nor" | "fin" | "ron" | "pol" | "ces" | "slk" | "hun" | "tur" | "cat" | "glg"
        | "ind" | "vie" => OcrRecProfile::Latin,
        _ => OcrRecProfile::English,
    }
}

static ORT_ENV_INIT: OnceLock<()> = OnceLock::new();
static ORT_ENV_GPU_AVAILABLE: OnceLock<bool> = OnceLock::new();
static FORCE_CPU_EP: AtomicBool = AtomicBool::new(false);
static TESSERACT_LANG_CACHE: OnceLock<Result<HashSet<String>>> = OnceLock::new();
static LEGACY_NVIDIA_MAXWELL: OnceLock<bool> = OnceLock::new();
static DISABLE_TESS_FALLBACK_LOGGED: AtomicBool = AtomicBool::new(false);

pub fn convert_bitmap_subtitles(
    input_file: &CStr,
    work_dir: &Path,
    sub_mode: SubMode,
    default_language: Option<&str>,
    ocr_engine: OcrEngine,
    ocr_format: OcrFormat,
    ocr_external_command: Option<&str>,
) -> Result<Vec<OcrSubtitleTrack>> {
    if matches!(sub_mode, SubMode::Skip) {
        return Ok(Vec::new());
    }

    let candidates = discover_candidates(input_file, sub_mode)?;
    if candidates.is_empty() {
        return Ok(Vec::new());
    }

    if matches!(ocr_engine, OcrEngine::External) && ocr_external_command.is_none() {
        bail!("--ocr-engine=external requires --ocr-external-command");
    }

    let input_path = input_file
        .to_str()
        .map_err(|_| anyhow!("Input path must be valid UTF-8 for OCR side pass"))?
        .to_string();
    apply_ocr_cuda_visible_devices_override();
    let system_language = detect_system_ocr_language();
    let video_dimensions = probe_video_dimensions(input_file);

    let (resolved_engine, mut seed_engine) = build_ocr_engine(ocr_engine, ocr_external_command)?;
    let available_langs = if matches!(resolved_engine, OcrEngine::Tesseract) {
        list_tesseract_languages().context(
            "Failed to query Tesseract language packs. Install `tesseract-ocr` and required traineddata files.",
        )?
    } else {
        HashSet::new()
    };

    let mut tasks = Vec::with_capacity(candidates.len());
    for (order, candidate) in candidates.into_iter().enumerate() {
        let resolved_lang = resolve_ocr_language(
            candidate.language_tag.as_deref(),
            default_language,
            system_language.as_deref(),
            &available_langs,
            resolved_engine,
        );
        let subtitle_path = work_dir.join(format!(
            "stream-{}.{}",
            candidate.stream_index,
            ocr_format.extension()
        ));
        tasks.push(OcrTask {
            order,
            stream_index: candidate.stream_index,
            language: resolved_lang,
            subtitle_path,
        });
    }

    let worker_plan = plan_ocr_workers(resolved_engine, tasks.len());
    let outputs = if worker_plan.worker_count <= 1 {
        let mut outputs = Vec::with_capacity(tasks.len());
        let total_tasks = tasks.len().max(1);
        for (idx, task) in tasks.into_iter().enumerate() {
            let cues = ocr_single_stream(
                &input_path,
                task.stream_index,
                &task.language,
                work_dir,
                ocr_format,
                video_dimensions,
                resolved_engine,
                &mut *seed_engine,
            )?;
            outputs.push(OcrTaskOutput {
                order: task.order,
                stream_index: task.stream_index,
                language: task.language,
                subtitle_path: task.subtitle_path,
                cues,
            });
            log_ocr_stream_progress(idx + 1, total_tasks);
        }
        outputs
    } else {
        let total_tasks = tasks.len();
        drop(seed_engine);
        run_ocr_tasks_parallel(
            tasks,
            worker_plan,
            input_path.clone(),
            work_dir.to_path_buf(),
            ocr_format,
            video_dimensions,
            resolved_engine,
            ocr_external_command.map(str::to_string),
            total_tasks,
        )?
    };

    finalize_ocr_outputs(outputs, ocr_format, video_dimensions)
}

fn apply_ocr_cuda_visible_devices_override() {
    let Ok(raw) = env::var("DPN_OCR_CUDA_DEVICES") else {
        return;
    };
    let parsed = parse_cuda_device_list(&raw);
    if parsed.is_empty() {
        return;
    }
    let normalized = parsed
        .iter()
        .map(i32::to_string)
        .collect::<Vec<_>>()
        .join(",");
    let current = env::var("CUDA_VISIBLE_DEVICES").ok();
    if current.as_deref() == Some(normalized.as_str()) {
        return;
    }
    if let Some(existing) = current {
        warn!(
            "Overriding CUDA_VISIBLE_DEVICES='{}' with DPN_OCR_CUDA_DEVICES='{}' for OCR initialization.",
            existing, normalized
        );
    } else {
        info!(
            "Setting CUDA_VISIBLE_DEVICES='{}' from DPN_OCR_CUDA_DEVICES for OCR initialization.",
            normalized
        );
    }
    env::set_var("CUDA_VISIBLE_DEVICES", normalized);
}

impl OcrFormat {
    fn extension(self) -> &'static str {
        match self {
            OcrFormat::Srt => "srt",
            OcrFormat::Ass => "ass",
        }
    }
}

#[derive(Debug, Clone)]
struct OcrWorkerPlan {
    worker_count: usize,
    device_ids: Vec<i32>,
}

fn plan_ocr_workers(resolved_engine: OcrEngine, task_count: usize) -> OcrWorkerPlan {
    let available_parallelism = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    let max_jobs_override = ocr_max_jobs_env();
    let jobs_per_gpu = ocr_jobs_per_gpu_env().unwrap_or(1);
    let gpu_available = *ORT_ENV_GPU_AVAILABLE.get().unwrap_or(&false)
        && !force_cpu_execution_providers();
    let device_ids = if matches!(resolved_engine, OcrEngine::PpOcrV3 | OcrEngine::PpOcrV4)
        && gpu_available
    {
        detect_ocr_cuda_devices()
    } else {
        Vec::new()
    };

    let plan = plan_ocr_workers_with_inputs(
        resolved_engine,
        task_count,
        available_parallelism,
        max_jobs_override,
        jobs_per_gpu,
        gpu_available,
        device_ids,
    );
    if matches!(resolved_engine, OcrEngine::PpOcrV3 | OcrEngine::PpOcrV4) {
        info!(
            "OCR worker plan: engine={:?}, tasks={}, workers={}, gpu_available={}, devices={:?}, jobs_per_gpu={}, max_jobs={:?}",
            resolved_engine,
            task_count,
            plan.worker_count,
            gpu_available,
            plan.device_ids,
            jobs_per_gpu,
            max_jobs_override
        );
    }
    plan
}

fn plan_ocr_workers_with_inputs(
    resolved_engine: OcrEngine,
    task_count: usize,
    available_parallelism: usize,
    max_jobs_override: Option<usize>,
    jobs_per_gpu: usize,
    gpu_available: bool,
    mut device_ids: Vec<i32>,
) -> OcrWorkerPlan {
    if task_count <= 1 {
        return OcrWorkerPlan {
            worker_count: task_count,
            device_ids: Vec::new(),
        };
    }

    let mut workers = max_jobs_override.unwrap_or(available_parallelism);
    workers = workers.max(1).min(task_count);

    if !matches!(resolved_engine, OcrEngine::PpOcrV3 | OcrEngine::PpOcrV4) {
        return OcrWorkerPlan {
            worker_count: workers,
            device_ids: Vec::new(),
        };
    }

    if !gpu_available {
        return OcrWorkerPlan {
            worker_count: workers,
            device_ids: Vec::new(),
        };
    }

    device_ids.sort_unstable();
    device_ids.dedup();
    if device_ids.is_empty() {
        warn!(
            "GPU OCR is available but no CUDA device indexes were resolved; falling back to a single OCR worker. \
             Set DPN_OCR_CUDA_DEVICES=0,1,... explicitly if needed."
        );
        return OcrWorkerPlan {
            worker_count: 1,
            device_ids,
        };
    }

    let jobs_per_gpu = jobs_per_gpu.max(1);
    let capacity = device_ids.len().saturating_mul(jobs_per_gpu).max(1);
    workers = workers.min(capacity);
    OcrWorkerPlan {
        worker_count: workers.max(1),
        device_ids,
    }
}

fn ocr_jobs_per_gpu_env() -> Option<usize> {
    parse_positive_usize_env("DPN_OCR_JOBS_PER_GPU")
}

fn ocr_max_jobs_env() -> Option<usize> {
    parse_positive_usize_env("DPN_OCR_MAX_JOBS")
}

fn parse_positive_usize_env(key: &str) -> Option<usize> {
    let raw = env::var(key).ok()?;
    match raw.trim().parse::<usize>() {
        Ok(value) if value >= 1 => Some(value),
        _ => {
            warn!("Ignoring invalid {}='{}'; expected integer >= 1.", key, raw);
            None
        }
    }
}

fn parse_cuda_device_list(value: &str) -> Vec<i32> {
    let mut out = Vec::new();
    for token in value.split(',').map(str::trim).filter(|s| !s.is_empty()) {
        if let Ok(id) = token.parse::<i32>() {
            out.push(id);
        }
    }
    out.sort_unstable();
    out.dedup();
    out
}

fn detect_ocr_cuda_devices() -> Vec<i32> {
    if let Ok(raw) = env::var("DPN_OCR_CUDA_DEVICES") {
        let parsed = parse_cuda_device_list(&raw);
        if parsed.is_empty() {
            warn!(
                "Ignoring DPN_OCR_CUDA_DEVICES='{}'; expected comma-separated GPU indexes.",
                raw
            );
        } else {
            info!("OCR CUDA devices set by DPN_OCR_CUDA_DEVICES={:?}", parsed);
            return parsed;
        }
    }

    if let Ok(raw) = env::var("CUDA_VISIBLE_DEVICES") {
        let parsed = parse_cuda_device_list(&raw);
        if !parsed.is_empty() {
            info!("OCR CUDA devices inferred from CUDA_VISIBLE_DEVICES={:?}", parsed);
            return parsed;
        }
    }

    if env::var("DPN_OCR_DISABLE_NVIDIA_SMI").ok().as_deref() == Some("1") {
        info!("DPN_OCR_DISABLE_NVIDIA_SMI=1; skipping nvidia-smi CUDA device probe.");
        return Vec::new();
    }

    // Best-effort fallback for dynamic multi-GPU assignment when no env override is present.
    let detected = detect_nvidia_gpu_indexes();
    if detected.is_empty() {
        warn!("nvidia-smi probe did not return any CUDA device indexes for OCR.");
    } else {
        info!("OCR CUDA devices detected via nvidia-smi: {:?}", detected);
    }
    detected
}

fn run_ocr_tasks_parallel(
    tasks: Vec<OcrTask>,
    worker_plan: OcrWorkerPlan,
    input_path: String,
    work_dir: PathBuf,
    ocr_format: OcrFormat,
    video_dimensions: Option<(u32, u32)>,
    resolved_engine: OcrEngine,
    ocr_external_command: Option<String>,
    total_tasks: usize,
) -> Result<Vec<OcrTaskOutput>> {
    let completed = Arc::new(AtomicUsize::new(0));
    let worker_count = worker_plan.worker_count.max(1);
    align_cuda_visible_devices_with_worker_plan(&worker_plan.device_ids);
    let worker_batches = build_ocr_worker_batches(tasks, worker_count, &worker_plan.device_ids);
    if !worker_plan.device_ids.is_empty() {
        info!(
            "Running OCR with {} workers across CUDA devices {:?}",
            worker_count, worker_plan.device_ids
        );
    } else {
        info!(
            "Running OCR with {} workers (no explicit CUDA device assignment)",
            worker_count
        );
    }

    let mut handles = Vec::with_capacity(worker_count);
    for (worker_idx, batch) in worker_batches.into_iter().enumerate() {
        let input_path = input_path.clone();
        let work_dir = work_dir.clone();
        let command = ocr_external_command.clone();
        let completed = Arc::clone(&completed);
        let assigned_device = batch.assigned_device;
        let worker_tasks = batch.tasks;

        handles.push(thread::spawn(move || -> Result<Vec<OcrTaskOutput>> {
            let _device_guard = set_thread_ocr_cuda_device(assigned_device);
            let mut engine = create_ocr_engine(resolved_engine, command.as_deref()).with_context(
                || {
                    if let Some(device_id) = assigned_device {
                        format!(
                            "failed to initialize OCR worker {} on CUDA device {}",
                            worker_idx, device_id
                        )
                    } else {
                        format!(
                            "failed to initialize OCR worker {} (no explicit CUDA device)",
                            worker_idx
                        )
                    }
                },
            )?;
            let mut local_outputs = Vec::with_capacity(worker_tasks.len());
            if let Some(device_id) = assigned_device {
                info!(
                    "OCR worker {} processing {} subtitle stream task(s) on CUDA device {}",
                    worker_idx,
                    worker_tasks.len(),
                    device_id
                );
            }
            for task in worker_tasks {
                let cues = ocr_single_stream(
                    &input_path,
                    task.stream_index,
                    &task.language,
                    &work_dir,
                    ocr_format,
                    video_dimensions,
                    resolved_engine,
                    &mut *engine,
                )?;

                local_outputs.push(OcrTaskOutput {
                    order: task.order,
                    stream_index: task.stream_index,
                    language: task.language,
                    subtitle_path: task.subtitle_path,
                    cues,
                });
                let done = completed.fetch_add(1, Ordering::Relaxed) + 1;
                log_ocr_stream_progress(done, total_tasks.max(1));
            }
            Ok(local_outputs)
        }));
    }

    let mut outputs = Vec::new();
    for handle in handles {
        let mut worker_outputs = handle
            .join()
            .map_err(|_| anyhow!("OCR worker thread panicked"))??;
        outputs.append(&mut worker_outputs);
    }
    outputs.sort_by_key(|output| output.order);
    Ok(outputs)
}

fn align_cuda_visible_devices_with_worker_plan(device_ids: &[i32]) {
    if device_ids.len() <= 1 {
        return;
    }

    let planned = device_ids
        .iter()
        .map(i32::to_string)
        .collect::<Vec<_>>()
        .join(",");
    let current = env::var("CUDA_VISIBLE_DEVICES").ok();
    let current_parsed = current
        .as_deref()
        .map(parse_cuda_device_list)
        .unwrap_or_default();

    let should_override = current_parsed.len() < device_ids.len()
        || !device_ids.iter().all(|id| current_parsed.contains(id));
    if !should_override {
        return;
    }

    if let Some(raw) = current {
        warn!(
            "CUDA_VISIBLE_DEVICES='{}' does not expose all planned OCR devices {:?}; overriding to '{}'.",
            raw, device_ids, planned
        );
    } else {
        info!(
            "CUDA_VISIBLE_DEVICES was unset; exposing planned OCR devices {:?} via CUDA_VISIBLE_DEVICES='{}'.",
            device_ids, planned
        );
    }
    env::set_var("CUDA_VISIBLE_DEVICES", planned);
}

#[derive(Debug)]
struct OcrWorkerBatch {
    assigned_device: Option<i32>,
    tasks: Vec<OcrTask>,
}

fn build_ocr_worker_batches(
    tasks: Vec<OcrTask>,
    worker_count: usize,
    device_ids: &[i32],
) -> Vec<OcrWorkerBatch> {
    let worker_count = worker_count.max(1);
    let mut batches = (0..worker_count)
        .map(|idx| OcrWorkerBatch {
            assigned_device: if device_ids.is_empty() {
                None
            } else {
                Some(device_ids[idx % device_ids.len()])
            },
            tasks: Vec::new(),
        })
        .collect::<Vec<_>>();

    for (task_idx, task) in tasks.into_iter().enumerate() {
        let worker_idx = task_idx % worker_count;
        batches[worker_idx].tasks.push(task);
    }

    batches
}

fn log_ocr_stream_progress(completed: usize, total: usize) {
    let pct = ((completed as f32 / total as f32) * 100.0).round().clamp(0.0, 100.0) as u32;
    info!(
        "OCR progress: {}/{} subtitle streams complete ({}%)",
        completed, total, pct
    );
}

fn finalize_ocr_outputs(
    outputs: Vec<OcrTaskOutput>,
    ocr_format: OcrFormat,
    video_dimensions: Option<(u32, u32)>,
) -> Result<Vec<OcrSubtitleTrack>> {
    let mut tracks = Vec::with_capacity(outputs.len());
    for output in outputs {
        match ocr_format {
            OcrFormat::Srt => write_srt(&output.subtitle_path, &output.cues)?,
            OcrFormat::Ass => write_ass(&output.subtitle_path, &output.cues, video_dimensions)?,
        }

        info!(
            "OCR subtitle stream {} -> '{}' ({} cues, format={:?})",
            output.stream_index,
            output.subtitle_path.display(),
            output.cues.len(),
            ocr_format
        );

        tracks.push(OcrSubtitleTrack {
            language: output.language,
            subtitle_path: output.subtitle_path,
            format: ocr_format,
        });
    }
    Ok(tracks)
}

fn build_ocr_engine(
    ocr_engine: OcrEngine,
    ocr_external_command: Option<&str>,
) -> Result<(OcrEngine, Box<dyn SubtitleConverter>)> {
    match ocr_engine {
        OcrEngine::Tesseract => Ok((
            OcrEngine::Tesseract,
            create_ocr_engine(OcrEngine::Tesseract, ocr_external_command)?,
        )),
        OcrEngine::External => Ok((
            OcrEngine::External,
            create_ocr_engine(OcrEngine::External, ocr_external_command)?,
        )),
        OcrEngine::PpOcrV3 => {
            let variant = PpOcrVariant::V3;
            let gpu_available = init_ort_environment()?;
            if !gpu_available {
                warn!(
                    "{} running without GPU acceleration. \
                     Install the CUDA/DirectML/CoreML runtime or set DPN_OCR_REQUIRE_GPU=1 to fail if GPU is required.",
                    variant.label(),
                );
            }
            Ok((
                OcrEngine::PpOcrV3,
                create_ocr_engine(OcrEngine::PpOcrV3, ocr_external_command)?,
            ))
        }
        OcrEngine::PpOcrV4 => {
            let variant = PpOcrVariant::V4;
            let gpu_available = init_ort_environment()?;
            if !gpu_available {
                warn!(
                    "{} running without GPU acceleration. \
                     Install the CUDA/DirectML/CoreML runtime or set DPN_OCR_REQUIRE_GPU=1 to fail if GPU is required.",
                    variant.label(),
                );
            }
            Ok((
                OcrEngine::PpOcrV4,
                create_ocr_engine(OcrEngine::PpOcrV4, ocr_external_command)?,
            ))
        }
        OcrEngine::Auto => {
            let require_gpu = require_gpu();
            let gpu_available = match init_ort_environment() {
                Ok(available) => available,
                Err(err) => {
                    if require_gpu {
                        return Err(err);
                    }
                    warn!("PP-OCR unavailable; falling back to Tesseract: {}", err);
                    return Ok((OcrEngine::Tesseract, Box::new(TesseractEngine)));
                }
            };

            let selected_engine = auto_engine_preference(gpu_available);
            if matches!(selected_engine, OcrEngine::Tesseract) {
                info!("Auto-selected Tesseract (no GPU execution provider available).");
                return Ok((
                    OcrEngine::Tesseract,
                    create_ocr_engine(OcrEngine::Tesseract, ocr_external_command)?,
                ));
            }
            let variant = match selected_engine {
                OcrEngine::PpOcrV3 => PpOcrVariant::V3,
                OcrEngine::PpOcrV4 => PpOcrVariant::V4,
                _ => PpOcrVariant::V4,
            };

            match create_ocr_engine(selected_engine, ocr_external_command) {
                Ok(engine) => {
                    info!(
                        "Auto-selected {} engine with GPU acceleration",
                        variant.label()
                    );
                    Ok((selected_engine, engine))
                }
                Err(err) => {
                    if require_gpu {
                        return Err(err);
                    }
                    warn!(
                        "{} unavailable; falling back to Tesseract: {}",
                        variant.label(),
                        err
                    );
                    Ok((
                        OcrEngine::Tesseract,
                        create_ocr_engine(OcrEngine::Tesseract, ocr_external_command)?,
                    ))
                }
            }
        }
    }
}

fn create_ocr_engine(
    resolved_engine: OcrEngine,
    ocr_external_command: Option<&str>,
) -> Result<Box<dyn SubtitleConverter>> {
    match resolved_engine {
        OcrEngine::Tesseract => Ok(Box::new(TesseractEngine)),
        OcrEngine::External => {
            let command = ocr_external_command
                .ok_or_else(|| anyhow!("missing OCR external command"))?
                .to_string();
            Ok(Box::new(ExternalEngine { command }))
        }
        OcrEngine::PpOcrV3 => {
            let model_dir = resolve_model_dir()?;
            let engine = init_ppocr_engine(&model_dir, require_gpu(), PpOcrVariant::V3)?;
            Ok(Box::new(engine))
        }
        OcrEngine::PpOcrV4 => {
            let model_dir = resolve_model_dir()?;
            let engine = init_ppocr_engine(&model_dir, require_gpu(), PpOcrVariant::V4)?;
            Ok(Box::new(engine))
        }
        OcrEngine::Auto => unreachable!("Auto engine must be resolved before creation"),
    }
}

fn auto_engine_preference(gpu_available: bool) -> OcrEngine {
    auto_engine_preference_with_capability(gpu_available, prefer_ppocr_v3_for_legacy_nvidia())
}

fn auto_engine_preference_with_capability(
    gpu_available: bool,
    prefer_v3_on_gpu: bool,
) -> OcrEngine {
    if !gpu_available {
        return OcrEngine::Tesseract;
    }
    if prefer_v3_on_gpu {
        OcrEngine::PpOcrV3
    } else {
        OcrEngine::PpOcrV4
    }
}

fn prefer_ppocr_v3_for_legacy_nvidia() -> bool {
    *LEGACY_NVIDIA_MAXWELL.get_or_init(detect_legacy_nvidia_maxwell)
}

fn detect_legacy_nvidia_maxwell() -> bool {
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
                    name,
                    cap
                );
                return true;
            }
        }
        false
    }
}

fn disable_tesseract_quality_fallback() -> bool {
    let disabled = env::var("DPN_OCR_DISABLE_TESS_FALLBACK")
        .ok()
        .map(|v| {
            let x = v.trim().to_ascii_lowercase();
            matches!(x.as_str(), "1" | "true" | "yes" | "on")
        })
        .unwrap_or(false);
    if disabled && !DISABLE_TESS_FALLBACK_LOGGED.swap(true, Ordering::Relaxed) {
        warn!(
            "DPN_OCR_DISABLE_TESS_FALLBACK=1 set; skipping Tesseract quality fallback and using pure PP-OCR output."
        );
    }
    disabled
}

fn ppocr_require_gpu_error(variant: PpOcrVariant, err: &anyhow::Error) -> anyhow::Error {
    anyhow!(
        "{} failed to initialize with DPN_OCR_REQUIRE_GPU=1. \
         Verify CUDA/ONNX Runtime GPU libraries are installed. Underlying error: {:#} (debug: {:?})",
        variant.label(),
        err,
        err
    )
}

fn init_ppocr_engine(
    model_dir: &Path,
    require_gpu: bool,
    variant: PpOcrVariant,
) -> Result<PpOcrEngine> {
    let skip_cls = skip_ppocr_cls(variant, require_gpu);
    if skip_cls {
        info!(
            "{} classifier model is disabled (DPN_OCR_SKIP_CLS or Maxwell GPU mode).",
            variant.label()
        );
    }
    match PpOcrEngine::new(model_dir, variant, skip_cls) {
        Ok(engine) => Ok(engine),
        Err(err) => {
            if require_gpu {
                return Err(ppocr_require_gpu_error(variant, &err));
            }
            if force_cpu_execution_providers() {
                return Err(err);
            }
            warn!(
                "{} failed to initialize with GPU providers; retrying with CPU-only providers: {:#} (debug: {:?})",
                variant.label(),
                err,
                err
            );
            FORCE_CPU_EP.store(true, Ordering::Relaxed);
            match PpOcrEngine::new(model_dir, variant, skip_cls) {
                Ok(engine) => {
                    info!(
                        "{} initialized with CPU-only execution provider",
                        variant.label()
                    );
                    Ok(engine)
                }
                Err(retry_err) => {
                    warn!(
                        "{} CPU-only initialization failed; falling back: {:#} (debug: {:?})",
                        variant.label(),
                        retry_err,
                        retry_err
                    );
                    Err(err)
                }
            }
        }
    }
}
