use criterion::{black_box, criterion_group, criterion_main, Criterion};
use paddle_ocr_rs::ocr_lite::OcrLite;
use std::path::PathBuf;
use std::time::Duration;

fn build_execution_providers() -> Vec<ort::execution_providers::ExecutionProviderDispatch> {
    let mut providers = Vec::new();
    #[cfg(any(target_os = "linux", target_os = "windows"))]
    {
        providers.push(ort::execution_providers::CUDAExecutionProvider::default().build());
    }
    #[cfg(target_os = "windows")]
    {
        providers.push(ort::execution_providers::DirectMLExecutionProvider::default().build());
    }
    #[cfg(target_vendor = "apple")]
    {
        providers.push(ort::execution_providers::CoreMLExecutionProvider::default().build());
    }
    providers.push(ort::execution_providers::CPUExecutionProvider::default().build());
    providers
}

fn resolve_model_dir() -> Option<PathBuf> {
    if let Ok(dir) = std::env::var("DPN_OCR_MODEL_DIR") {
        return Some(PathBuf::from(dir));
    }
    let exe_dir = std::env::current_exe()
        .ok()
        .and_then(|exe| exe.parent().map(PathBuf::from));
    if let Some(dir) = exe_dir {
        let candidate = dir.join("models");
        if candidate.is_dir() {
            return Some(candidate);
        }
    }
    if let Ok(home) = std::env::var("HOME") {
        let candidate = PathBuf::from(home)
            .join(".config")
            .join("direct-play-nice")
            .join("models");
        if candidate.is_dir() {
            return Some(candidate);
        }
    }
    None
}

fn load_test_image() -> image::RgbImage {
    let candidate = PathBuf::from("tests/fixtures/hard_subtitle.png");
    if candidate.exists() {
        if let Ok(img) = image::open(candidate) {
            return img.to_rgb8();
        }
    }

    // Fallback: synthesize a simple image to keep the benchmark runnable.
    let mut img = image::RgbImage::new(640, 120);
    for pixel in img.pixels_mut() {
        *pixel = image::Rgb([0u8, 0u8, 0u8]);
    }
    img
}

fn bench_ppocr_inference(c: &mut Criterion) {
    let Some(model_dir) = resolve_model_dir() else {
        eprintln!("Skipping OCR benchmark: model directory not found.");
        return;
    };

    let det = model_dir.join("ch_PP-OCRv4_det_infer.onnx");
    let cls = model_dir.join("ch_ppocr_mobile_v2.0_cls_infer.onnx");
    let rec = model_dir.join("en_PP-OCRv4_rec_infer.onnx");
    if !det.exists() || !cls.exists() || !rec.exists() {
        eprintln!(
            "Skipping OCR benchmark: required models missing in {:?}",
            model_dir
        );
        return;
    }

    let _ = ort::init()
        .with_execution_providers(build_execution_providers())
        .commit();

    let mut ocr = OcrLite::new();
    let threads = std::thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(2);
    ocr.init_models(
        det.to_string_lossy().as_ref(),
        cls.to_string_lossy().as_ref(),
        rec.to_string_lossy().as_ref(),
        threads,
    )
    .expect("initialize OCR models");

    let img = load_test_image();

    let mut group = c.benchmark_group("OCR Conversion Speed");
    group.sample_size(100);
    group.measurement_time(Duration::from_secs(10));
    group.bench_function("extract_text", |b| {
        b.iter(|| {
            let result = ocr.detect(black_box(&img), 50, 1024, 0.5, 0.3, 1.6, false, false);
            let _ = black_box(result);
        })
    });
    group.finish();
}

criterion_group!(benches, bench_ppocr_inference);
criterion_main!(benches);
