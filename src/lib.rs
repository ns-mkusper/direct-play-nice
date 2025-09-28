pub mod gpu;

// Simple scoring helper to test primary selection criteria
pub fn score_video(criteria: &str, width: u32, height: u32, bitrate: u64, fps_num: u32, fps_den: u32) -> u128 {
    let area = (width as u128) * (height as u128);
    let br = bitrate as u128;
    let fps_milli: u128 = if fps_den == 0 { 0 } else { ((fps_num as u128) * 1000) / (fps_den as u128) };
    match criteria {
        "resolution" => (area << 40) + br,
        "bitrate" => (br << 40) + area,
        "fps" => (fps_milli << 56) + area,
        _ => (area << 40) + br,
    }
}
