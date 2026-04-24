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

    fn default_latin_rec_spec(self) -> &'static ModelSpec {
        match self {
            PpOcrVariant::V3 => &PPOCR_V3_LATIN_REC_MODEL,
            PpOcrVariant::V4 => &PPOCR_V4_LATIN_REC_MODEL,
        }
    }

    fn default_japanese_rec_spec(self) -> &'static ModelSpec {
        match self {
            PpOcrVariant::V3 => &PPOCR_V3_JAPANESE_REC_MODEL,
            PpOcrVariant::V4 => &PPOCR_V4_JAPANESE_REC_MODEL,
        }
    }

    fn default_korean_rec_spec(self) -> &'static ModelSpec {
        match self {
            PpOcrVariant::V3 => &PPOCR_V3_KOREAN_REC_MODEL,
            PpOcrVariant::V4 => &PPOCR_V4_KOREAN_REC_MODEL,
        }
    }

    fn default_cjk_rec_spec(self) -> &'static ModelSpec {
        match self {
            PpOcrVariant::V3 => &PPOCR_V3_CJK_REC_MODEL,
            PpOcrVariant::V4 => &PPOCR_V4_CJK_REC_MODEL,
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
static FORCE_TESS_NON_ENGLISH_LOGGED: AtomicBool = AtomicBool::new(false);

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
            let cues = ocr_single_stream(OcrStreamRequest {
                input_path: &input_path,
                stream_index: task.stream_index,
                language: &task.language,
                work_dir,
                ocr_format,
                video_dimensions,
                ocr_engine: resolved_engine,
                engine: &mut *seed_engine,
            })?;
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
        let params = OcrParallelParams {
            input_path: input_path.clone(),
            work_dir: work_dir.to_path_buf(),
            ocr_format,
            video_dimensions,
            resolved_engine,
            ocr_external_command: ocr_external_command.map(str::to_string),
            total_tasks,
        };
        run_ocr_tasks_parallel(tasks, worker_plan, params)?
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

struct OcrParallelParams {
    input_path: String,
    work_dir: PathBuf,
    ocr_format: OcrFormat,
    video_dimensions: Option<(u32, u32)>,
    resolved_engine: OcrEngine,
    ocr_external_command: Option<String>,
    total_tasks: usize,
}

fn run_ocr_tasks_parallel(
    tasks: Vec<OcrTask>,
    worker_plan: OcrWorkerPlan,
    params: OcrParallelParams,
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
        let input_path = params.input_path.clone();
        let work_dir = params.work_dir.clone();
        let command = params.ocr_external_command.clone();
        let completed = Arc::clone(&completed);
        let assigned_device = batch.assigned_device;
        let worker_tasks = batch.tasks;
        let ocr_format = params.ocr_format;
        let video_dimensions = params.video_dimensions;
        let resolved_engine = params.resolved_engine;
        let total_tasks = params.total_tasks;

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
                let cues = ocr_single_stream(OcrStreamRequest {
                    input_path: &input_path,
                    stream_index: task.stream_index,
                    language: &task.language,
                    work_dir: &work_dir,
                    ocr_format,
                    video_dimensions,
                    ocr_engine: resolved_engine,
                    engine: &mut *engine,
                })?;

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

const PPOCR_V4_LATIN_REC_MODEL: ModelSpec = ModelSpec {
    filename: "latin_PP-OCRv3_rec_mobile.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/latin_PP-OCRv3_rec_mobile.onnx",
    sha256: "E9D7A33667E8AAA702862975186ADF2012E3F390CC0F9422865957125F8071CF",
};
const PPOCR_V4_JAPANESE_REC_MODEL: ModelSpec = ModelSpec {
    filename: "japan_PP-OCRv4_rec_mobile.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/japan_PP-OCRv4_rec_mobile.onnx",
    sha256: "E1075A67DBA758ECFC7EBC78A10AE61C95AC8FB66A9C86FAB5541E33F085CB7A",
};
const PPOCR_V4_KOREAN_REC_MODEL: ModelSpec = ModelSpec {
    filename: "korean_PP-OCRv4_rec_mobile.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/korean_PP-OCRv4_rec_mobile.onnx",
    sha256: "AB151BA9065ECCD98F884CF4D927DB091BE86137276392072EDD4F9D43AD7426",
};
const PPOCR_V4_CJK_REC_MODEL: ModelSpec = ModelSpec {
    filename: "chinese_cht_PP-OCRv3_rec_mobile.onnx",
    url: "https://www.modelscope.cn/models/RapidAI/RapidOCR/resolve/master/onnx/PP-OCRv4/rec/chinese_cht_PP-OCRv3_rec_mobile.onnx",
    sha256: "779656D044CE388045E02EA9244724616194E63928606436CDFC6DC3C9528CC6",
};

const PPOCR_V3_LATIN_REC_MODEL: ModelSpec = PPOCR_V4_LATIN_REC_MODEL;
const PPOCR_V3_JAPANESE_REC_MODEL: ModelSpec = PPOCR_V4_JAPANESE_REC_MODEL;
const PPOCR_V3_KOREAN_REC_MODEL: ModelSpec = PPOCR_V4_KOREAN_REC_MODEL;
const PPOCR_V3_CJK_REC_MODEL: ModelSpec = PPOCR_V4_CJK_REC_MODEL;

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
fn resolve_model_dir() -> Result<PathBuf> {
    if let Some(dir) = env::var_os("DPN_OCR_MODEL_DIR") {
        let path = PathBuf::from(dir);
        fs::create_dir_all(&path)
            .with_context(|| format!("creating OCR model directory '{}'", path.display()))?;
        return Ok(path);
    }

    if let Ok(exe) = env::current_exe() {
        if let Some(parent) = exe.parent() {
            let candidate = parent.join("models");
            if candidate.is_dir() {
                return Ok(candidate);
            }
        }
    }

    let fallback = if let Some(xdg) = env::var_os("XDG_CONFIG_HOME") {
        PathBuf::from(xdg).join("direct-play-nice").join("models")
    } else if let Some(home) = env::var_os("HOME") {
        PathBuf::from(home)
            .join(".config")
            .join("direct-play-nice")
            .join("models")
    } else {
        env::temp_dir().join("direct-play-nice-models")
    };

    fs::create_dir_all(&fallback)
        .with_context(|| format!("creating OCR model directory '{}'", fallback.display()))?;
    Ok(fallback)
}

fn ensure_ppocr_models(
    model_dir: &Path,
    variant: PpOcrVariant,
    skip_cls: bool,
) -> Result<PpOcrModels> {
    let (det_spec, cls_spec, rec_spec) = variant.model_specs();
    if skip_cls {
        debug!(
            "Skipping classifier is requested, but this build uses mandatory classifier initialization; loading cls model."
        );
    }
    let det = ensure_model_file(model_dir, det_spec)?;
    let cls = ensure_model_file(model_dir, cls_spec)?;
    let rec = ensure_model_file(model_dir, rec_spec)?;
    Ok(PpOcrModels { det, cls, rec })
}

fn resolve_optional_latin_rec_model(
    model_dir: &Path,
    variant: PpOcrVariant,
) -> Result<Option<PathBuf>> {
    resolve_optional_rec_model_with_candidates(
        "DPN_OCR_REC_LATIN_MODEL",
        model_dir,
        match variant {
            PpOcrVariant::V3 => &[
                "latin_PP-OCRv3_rec_mobile.onnx",
                "latin_PP-OCRv3_rec_infer.onnx",
                "latin_ppocr_mobile_v2.0_rec_infer.onnx",
                "multilingual_PP-OCRv3_rec_infer.onnx",
            ],
            PpOcrVariant::V4 => &[
                "latin_PP-OCRv3_rec_mobile.onnx",
                "latin_PP-OCRv4_rec_infer.onnx",
                "latin_ppocr_mobile_v2.0_rec_infer.onnx",
                "multilingual_PP-OCRv4_rec_infer.onnx",
            ],
        },
        variant.default_latin_rec_spec(),
        "latin",
        variant,
    )
}

fn resolve_optional_japanese_rec_model(
    model_dir: &Path,
    variant: PpOcrVariant,
) -> Result<Option<PathBuf>> {
    resolve_optional_rec_model_with_candidates(
        "DPN_OCR_REC_JAPANESE_MODEL",
        model_dir,
        match variant {
            PpOcrVariant::V3 => &[
                "japan_PP-OCRv4_rec_mobile.onnx",
                "japan_PP-OCRv3_rec_infer.onnx",
                "japanese_PP-OCRv3_rec_infer.onnx",
                "ja_PP-OCRv3_rec_infer.onnx",
            ],
            PpOcrVariant::V4 => &[
                "japan_PP-OCRv4_rec_mobile.onnx",
                "japan_PP-OCRv4_rec_infer.onnx",
                "japanese_PP-OCRv4_rec_infer.onnx",
                "ja_PP-OCRv4_rec_infer.onnx",
            ],
        },
        variant.default_japanese_rec_spec(),
        "japanese",
        variant,
    )
}

fn resolve_optional_korean_rec_model(
    model_dir: &Path,
    variant: PpOcrVariant,
) -> Result<Option<PathBuf>> {
    resolve_optional_rec_model_with_candidates(
        "DPN_OCR_REC_KOREAN_MODEL",
        model_dir,
        match variant {
            PpOcrVariant::V3 => &[
                "korean_PP-OCRv4_rec_mobile.onnx",
                "korean_PP-OCRv3_rec_infer.onnx",
                "ko_PP-OCRv3_rec_infer.onnx",
            ],
            PpOcrVariant::V4 => &[
                "korean_PP-OCRv4_rec_mobile.onnx",
                "korean_PP-OCRv4_rec_infer.onnx",
                "ko_PP-OCRv4_rec_infer.onnx",
            ],
        },
        variant.default_korean_rec_spec(),
        "korean",
        variant,
    )
}

fn resolve_optional_cjk_rec_model(
    model_dir: &Path,
    variant: PpOcrVariant,
) -> Result<Option<PathBuf>> {
    resolve_optional_rec_model_with_candidates(
        "DPN_OCR_REC_CJK_MODEL",
        model_dir,
        match variant {
            PpOcrVariant::V3 => &[
                "chinese_cht_PP-OCRv3_rec_mobile.onnx",
                "cjk_PP-OCRv3_rec_infer.onnx",
                "chinese_PP-OCRv3_rec_infer.onnx",
                "zh_PP-OCRv3_rec_infer.onnx",
            ],
            PpOcrVariant::V4 => &[
                "chinese_cht_PP-OCRv3_rec_mobile.onnx",
                "cjk_PP-OCRv4_rec_infer.onnx",
                "chinese_PP-OCRv4_rec_infer.onnx",
                "zh_PP-OCRv4_rec_infer.onnx",
            ],
        },
        variant.default_cjk_rec_spec(),
        "cjk",
        variant,
    )
}

fn resolve_optional_rec_model_with_candidates(
    env_key: &str,
    model_dir: &Path,
    candidates: &[&str],
    default_spec: &ModelSpec,
    profile_label: &str,
    variant: PpOcrVariant,
) -> Result<Option<PathBuf>> {
    if let Some(path) = env::var_os(env_key) {
        let rec_path = PathBuf::from(path);
        if !rec_path.is_file() {
            bail!(
                "{} is set but file does not exist: '{}'",
                env_key,
                rec_path.display()
            );
        }
        return Ok(Some(rec_path));
    }

    for candidate in candidates {
        let path = model_dir.join(candidate);
        if path.is_file() {
            return Ok(Some(path));
        }
    }

    match ensure_model_file(model_dir, default_spec) {
        Ok(path) => {
            info!(
                "{} {} rec model auto-provisioned at '{}'",
                variant.label(),
                profile_label,
                path.display()
            );
            Ok(Some(path))
        }
        Err(err) => {
            warn!(
                "{} {} rec model auto-provisioning failed; continuing with fallback routing: {:#}",
                variant.label(),
                profile_label,
                err
            );
            Ok(None)
        }
    }
}

fn ensure_model_file(model_dir: &Path, spec: &ModelSpec) -> Result<PathBuf> {
    let path = model_dir.join(spec.filename);
    if path.exists() {
        if let Ok(hash) = sha256_file(&path) {
            if hash != spec.sha256.to_ascii_lowercase() {
                warn!(
                    "OCR model '{}' hash mismatch (got {}, expected {}). Using existing file anyway.",
                    path.display(),
                    hash,
                    spec.sha256
                );
            }
        }
        return Ok(path);
    }

    info!(
        "Downloading OCR model '{}' from {}",
        spec.filename, spec.url
    );
    download_model_with_values(&path, spec.url, spec.sha256, spec.filename)?;
    Ok(path)
}

#[cfg(test)]
fn ensure_model_file_with_values(
    model_dir: &Path,
    filename: &str,
    url: &str,
    sha256: &str,
) -> Result<PathBuf> {
    let path = model_dir.join(filename);
    if path.exists() {
        return Ok(path);
    }
    info!("Downloading OCR model '{}' from {}", filename, url);
    download_model_with_values(&path, url, sha256, filename)?;
    Ok(path)
}

fn download_model_with_values(path: &Path, url: &str, sha256: &str, filename: &str) -> Result<()> {
    let parent = path
        .parent()
        .ok_or_else(|| anyhow!("invalid OCR model path '{}'", path.display()))?;
    fs::create_dir_all(parent)
        .with_context(|| format!("creating OCR model directory '{}'", parent.display()))?;

    let tmp_path = path.with_extension("download");
    let response = ureq::get(url)
        .call()
        .with_context(|| format!("downloading OCR model from {}", url))?;
    let mut reader = response.into_reader();
    let mut file = fs::File::create(&tmp_path)
        .with_context(|| format!("creating '{}'", tmp_path.display()))?;
    let mut hasher = Sha256::new();
    let mut buf = [0u8; 16 * 1024];
    let mut download_result = Ok(());
    loop {
        let read = match reader.read(&mut buf) {
            Ok(read) => read,
            Err(err) => {
                download_result = Err(anyhow!(err).context("reading OCR model download"));
                break;
            }
        };
        if read == 0 {
            break;
        }
        hasher.update(&buf[..read]);
        if let Err(err) = file.write_all(&buf[..read]) {
            download_result = Err(anyhow!(err).context("writing OCR model download"));
            break;
        }
    }
    let _ = file.flush();

    if let Err(err) = download_result {
        let _ = fs::remove_file(&tmp_path);
        return Err(err);
    }

    let actual_hash = to_hex_lower(&hasher.finalize());
    let expected = sha256.to_ascii_lowercase();
    if actual_hash != expected {
        let _ = fs::remove_file(&tmp_path);
        bail!(
            "downloaded OCR model '{}' hash mismatch (got {}, expected {})",
            filename,
            actual_hash,
            expected
        );
    }

    fs::rename(&tmp_path, path).with_context(|| {
        format!(
            "renaming OCR model download '{}' -> '{}'",
            tmp_path.display(),
            path.display()
        )
    })?;
    Ok(())
}

fn sha256_file(path: &Path) -> Result<String> {
    let mut file = fs::File::open(path)?;
    let mut hasher = Sha256::new();
    let mut buf = [0u8; 16 * 1024];
    loop {
        let read = file.read(&mut buf)?;
        if read == 0 {
            break;
        }
        hasher.update(&buf[..read]);
    }
    Ok(to_hex_lower(&hasher.finalize()))
}

fn to_hex_lower(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len() * 2);
    for byte in bytes {
        use std::fmt::Write as _;
        let _ = write!(&mut out, "{:02x}", byte);
    }
    out
}
