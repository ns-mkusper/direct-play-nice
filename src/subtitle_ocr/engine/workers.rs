use anyhow::{anyhow, Context, Result};
use log::{info, warn};
use std::env;
use std::path::PathBuf;
use std::sync::{
    atomic::{AtomicUsize, Ordering},
    Arc,
};
use std::thread;

use super::detect_nvidia_gpu_indexes;
use super::{
    create_ocr_engine, force_cpu_execution_providers, ocr_single_stream,
    set_thread_ocr_cuda_device, write_ass, write_srt, OcrEngine, OcrFormat, OcrSubtitleTrack,
    OcrTask, OcrTaskOutput, ORT_ENV_GPU_AVAILABLE,
};

/// Applies `DPN_OCR_CUDA_DEVICES` onto `CUDA_VISIBLE_DEVICES` before OCR initialization.
///
/// This ensures provider initialization and worker planning observe the same visible GPU set.
pub(in crate::subtitle_ocr) fn apply_ocr_cuda_visible_devices_override() {
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

#[derive(Debug, Clone)]
pub(in crate::subtitle_ocr) struct OcrWorkerPlan {
    pub(in crate::subtitle_ocr) worker_count: usize,
    pub(in crate::subtitle_ocr) device_ids: Vec<i32>,
}

/// Builds an OCR worker plan from runtime capabilities and optional env overrides.
pub(in crate::subtitle_ocr) fn plan_ocr_workers(
    resolved_engine: OcrEngine,
    task_count: usize,
) -> OcrWorkerPlan {
    let available_parallelism = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1);
    let max_jobs_override = ocr_max_jobs_env();
    let jobs_per_gpu = ocr_jobs_per_gpu_env().unwrap_or(1);
    let gpu_available =
        *ORT_ENV_GPU_AVAILABLE.get().unwrap_or(&false) && !force_cpu_execution_providers();
    let device_ids =
        if matches!(resolved_engine, OcrEngine::PpOcrV3 | OcrEngine::PpOcrV4) && gpu_available {
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

/// Pure planning helper used by tests and production code.
///
/// Applies CPU caps, GPU capacity (`devices * jobs_per_gpu`), and fallback rules.
pub(in crate::subtitle_ocr) fn plan_ocr_workers_with_inputs(
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

/// Reads `DPN_OCR_JOBS_PER_GPU` as a positive integer.
pub(in crate::subtitle_ocr) fn ocr_jobs_per_gpu_env() -> Option<usize> {
    parse_positive_usize_env("DPN_OCR_JOBS_PER_GPU")
}

/// Reads `DPN_OCR_MAX_JOBS` as a positive integer.
pub(in crate::subtitle_ocr) fn ocr_max_jobs_env() -> Option<usize> {
    parse_positive_usize_env("DPN_OCR_MAX_JOBS")
}

/// Parses a required positive integer env value, warning on invalid input.
pub(in crate::subtitle_ocr) fn parse_positive_usize_env(key: &str) -> Option<usize> {
    let raw = env::var(key).ok()?;
    match raw.trim().parse::<usize>() {
        Ok(value) if value >= 1 => Some(value),
        _ => {
            warn!("Ignoring invalid {}='{}'; expected integer >= 1.", key, raw);
            None
        }
    }
}

/// Parses a comma-separated CUDA device list, removing duplicates and invalid entries.
pub(in crate::subtitle_ocr) fn parse_cuda_device_list(value: &str) -> Vec<i32> {
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

/// Resolves CUDA device indexes for OCR from explicit env vars or `nvidia-smi`.
pub(in crate::subtitle_ocr) fn detect_ocr_cuda_devices() -> Vec<i32> {
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
            info!(
                "OCR CUDA devices inferred from CUDA_VISIBLE_DEVICES={:?}",
                parsed
            );
            return parsed;
        }
    }

    if env::var("DPN_OCR_DISABLE_NVIDIA_SMI").ok().as_deref() == Some("1") {
        info!("DPN_OCR_DISABLE_NVIDIA_SMI=1; skipping nvidia-smi CUDA device probe.");
        return Vec::new();
    }

    let detected = detect_nvidia_gpu_indexes();
    if detected.is_empty() {
        warn!("nvidia-smi probe did not return any CUDA device indexes for OCR.");
    } else {
        info!("OCR CUDA devices detected via nvidia-smi: {:?}", detected);
    }
    detected
}

pub(in crate::subtitle_ocr) struct OcrParallelParams {
    pub(in crate::subtitle_ocr) input_path: String,
    pub(in crate::subtitle_ocr) work_dir: PathBuf,
    pub(in crate::subtitle_ocr) ocr_format: OcrFormat,
    pub(in crate::subtitle_ocr) video_dimensions: Option<(u32, u32)>,
    pub(in crate::subtitle_ocr) resolved_engine: OcrEngine,
    pub(in crate::subtitle_ocr) ocr_external_command: Option<String>,
    pub(in crate::subtitle_ocr) total_tasks: usize,
}

/// Executes OCR tasks in parallel worker threads and returns results in original stream order.
pub(in crate::subtitle_ocr) fn run_ocr_tasks_parallel(
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
            let mut engine =
                create_ocr_engine(resolved_engine, command.as_deref()).with_context(|| {
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
                })?;
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

/// Ensures `CUDA_VISIBLE_DEVICES` includes all GPUs required by the worker plan.
pub(in crate::subtitle_ocr) fn align_cuda_visible_devices_with_worker_plan(device_ids: &[i32]) {
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
pub(in crate::subtitle_ocr) struct OcrWorkerBatch {
    pub(in crate::subtitle_ocr) assigned_device: Option<i32>,
    pub(in crate::subtitle_ocr) tasks: Vec<OcrTask>,
}

/// Shards OCR tasks across workers in round-robin order, assigning optional GPU ids per worker.
pub(in crate::subtitle_ocr) fn build_ocr_worker_batches(
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

/// Emits per-stream OCR progress as a coarse percentage.
pub(in crate::subtitle_ocr) fn log_ocr_stream_progress(completed: usize, total: usize) {
    let pct = ((completed as f32 / total as f32) * 100.0)
        .round()
        .clamp(0.0, 100.0) as u32;
    info!(
        "OCR progress: {}/{} subtitle streams complete ({}%)",
        completed, total, pct
    );
}

/// Writes OCR outputs to disk and converts task outputs into mux-ready subtitle tracks.
pub(in crate::subtitle_ocr) fn finalize_ocr_outputs(
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
