use std::error::Error;
use std::path::Path;
use std::process::Command;

pub const SCENE_FRAMES: usize = 12;
pub const SCENE_COUNT: usize = 4;
const SAMPLE_WIDTH: usize = 160;
const SAMPLE_HEIGHT: usize = 90;
const SAMPLE_PIXELS: usize = SAMPLE_WIDTH * SAMPLE_HEIGHT;

pub fn create_scene_cut_clip(path: &Path) -> Result<(), Box<dyn Error>> {
    // Six binary blocks distinguish each frame, even within one scene.
    let mut filter = "[0:v][1:v][2:v][3:v]concat=n=4:v=1:a=0".to_string();
    for bit in 0..6 {
        let x = 24 + bit * 80;
        filter.push_str(&format!(
            ",drawbox=x={x}:y=24:w=48:h=48:color=black:t=fill,drawbox=x={x}:y=24:w=48:h=48:color=white:t=fill:enable='bitand(n,{})'",
            1 << bit
        ));
    }
    filter.push_str("[v]");
    let output = Command::new("ffmpeg")
        .args(["-hide_banner", "-loglevel", "error", "-y"])
        .args(["red", "blue", "green", "yellow"].iter().flat_map(|color| {
            [
                "-f".to_string(),
                "lavfi".to_string(),
                "-i".to_string(),
                format!("color={color}:size=960x540:rate=24:duration=0.5"),
            ]
        }))
        .args([
            "-f",
            "lavfi",
            "-i",
            "sine=frequency=1000:sample_rate=48000:duration=2",
            "-filter_complex",
        ])
        .arg(&filter)
        .args([
            "-map",
            "[v]",
            "-map",
            "4:a:0",
            "-c:a",
            "aac",
            "-c:v",
            "libx264",
            "-pix_fmt",
            "yuv420p",
            "-preset",
            "fast",
            "-bf",
            "3",
            "-g",
            "96",
            "-x264-params",
            "b-adapt=0:scenecut=0",
        ])
        .arg(path)
        .output()?;
    if !output.status.success() {
        return Err(format!(
            "ffmpeg scene-cut generation failed: {}",
            String::from_utf8_lossy(&output.stderr)
        )
        .into());
    }
    Ok(())
}

#[derive(Clone, Debug)]
pub struct SceneFrame {
    pub mean: [f64; 3],
    pub marker: usize,
}

// Decode in presentation order. Sampling block interiors survives resizing and
// lossy encoding; uncertain marker values are errors rather than guessed bits.
pub fn decoded_scene_frames(path: &Path) -> Result<Vec<SceneFrame>, Box<dyn Error>> {
    let output = Command::new("ffmpeg")
        .args(["-hide_banner", "-loglevel", "error", "-i"])
        .arg(path)
        .args([
            "-map",
            "0:v:0",
            "-vf",
            "scale=160:90",
            "-fps_mode",
            "passthrough",
            "-pix_fmt",
            "rgb24",
            "-f",
            "rawvideo",
            "-threads",
            "1",
            "pipe:1",
        ])
        .output()?;
    if !output.status.success() {
        return Err(format!(
            "ffmpeg frame decode failed for {}: {}",
            path.display(),
            String::from_utf8_lossy(&output.stderr)
        )
        .into());
    }
    let frame_bytes = SAMPLE_PIXELS * 3;
    if output.stdout.len() % frame_bytes != 0 {
        return Err("ffmpeg returned a partial RGB frame".into());
    }
    output
        .stdout
        .chunks_exact(frame_bytes)
        .enumerate()
        .map(|(index, frame)| {
            let mut mean = [0.0; 3];
            for pixel in frame.as_chunks::<3>().0 {
                for channel in 0..3 {
                    mean[channel] += f64::from(pixel[channel]) / SAMPLE_PIXELS as f64;
                }
            }
            let mut marker = 0;
            for bit in 0..6 {
                let x = (48 + 80 * bit) * SAMPLE_WIDTH / 960;
                let y = 48 * SAMPLE_HEIGHT / 540;
                let mut brightness = 0_u32;
                for row in y - 1..=y + 1 {
                    for col in x - 1..=x + 1 {
                        let offset = (row * SAMPLE_WIDTH + col) * 3;
                        brightness += frame[offset..offset + 3]
                            .iter()
                            .map(|&value| u32::from(value))
                            .sum::<u32>();
                    }
                }
                // Sum of 27 channels. Leave a generous uncertainty band.
                if brightness >= 27 * 160 {
                    marker |= 1 << bit;
                } else if brightness > 27 * 96 {
                    return Err(format!(
                        "{} frame {index} marker bit {bit} is ambiguous (mean brightness {:.1})",
                        path.display(),
                        f64::from(brightness) / 27.0
                    )
                    .into());
                }
            }
            Ok(SceneFrame { mean, marker })
        })
        .collect()
}

fn color_distance(a: [f64; 3], b: [f64; 3]) -> f64 {
    (0..3).map(|channel| (a[channel] - b[channel]).abs()).sum()
}

/// Validate the fixture itself, then compare every frame's identity and scene.
pub fn check_scene_frames(source: &[SceneFrame], output: &[SceneFrame]) -> Result<(), String> {
    let expected_count = SCENE_FRAMES * SCENE_COUNT;
    if source.len() != expected_count {
        return Err(format!(
            "fixture frame count: expected {expected_count}, got {}",
            source.len()
        ));
    }
    if output.len() != source.len() {
        return Err(format!(
            "output frame count: expected {}, got {}",
            source.len(),
            output.len()
        ));
    }
    let scenes: Vec<_> = (0..SCENE_COUNT)
        .map(|scene| source[scene * SCENE_FRAMES + SCENE_FRAMES / 2].mean)
        .collect();
    for (scene, reference) in scenes.iter().enumerate() {
        for (other, other_reference) in scenes.iter().enumerate().skip(scene + 1) {
            if color_distance(*reference, *other_reference) <= 100.0 {
                return Err(format!(
                    "fixture scenes {scene} and {other} are not distinct"
                ));
            }
        }
    }
    let classify = |frame: [f64; 3]| {
        scenes
            .iter()
            .enumerate()
            .map(|(scene, reference)| (scene, color_distance(frame, *reference)))
            .min_by(|a, b| a.1.total_cmp(&b.1))
            .expect("nonempty scene references")
    };
    for (index, frame) in source.iter().enumerate() {
        if frame.marker != index {
            return Err(format!(
                "fixture frame {index}: expected marker {index}, got {}",
                frame.marker
            ));
        }
        let (scene, distance) = classify(frame.mean);
        if scene != index / SCENE_FRAMES || distance >= 75.0 {
            return Err(format!(
                "fixture frame {index}: expected scene {}, got {scene}, RGB distance {distance:.1}",
                index / SCENE_FRAMES
            ));
        }
    }
    for (index, (expected, actual)) in source.iter().zip(output).enumerate() {
        if actual.marker != expected.marker {
            return Err(format!(
                "frame {index}: expected marker {}, got {}",
                expected.marker, actual.marker
            ));
        }
        let expected_scene = index / SCENE_FRAMES;
        let (scene, distance) = classify(actual.mean);
        if scene != expected_scene || distance >= 75.0 {
            return Err(format!("frame {index}: expected scene {expected_scene}, got {scene}, RGB distance {distance:.1}"));
        }
    }
    Ok(())
}
