//! Library helpers used by the `direct_play_nice` CLI.
//!
//! This crate primarily ships a CLI binary. The public items here support
//! device-profile resolution and hardware probing used by that CLI.
//! For end-user usage, prefer the project manual in `docs/` (mdBook) and the
//! top-level `README.md`.
#![deny(missing_docs)]

/// Streaming-device definitions and compatibility helpers.
pub mod devices;
/// Hardware acceleration detection and codec probing helpers.
pub mod gpu;

/// Creates a sortable score tuple for primary-video selection.
///
/// The tuple order depends on `criteria`:
/// - `"resolution"` => `(area, bitrate, fps)`
/// - `"bitrate"` => `(bitrate, area, fps)`
/// - `"fps"` => `(fps, area, bitrate)`
/// - any other value defaults to the `"resolution"` order
///
/// # Examples
///
/// ```rust
/// let score = direct_play_nice::score_video("resolution", 1920, 1080, 8_000_000, 30000, 1001);
/// assert!(score.0 > 0);
/// ```
pub fn score_video(
    criteria: &str,
    width: u32,
    height: u32,
    bitrate: u64,
    fps_num: u32,
    fps_den: u32,
) -> (u128, u128, u128) {
    let area = (width as u128) * (height as u128);
    let br = bitrate as u128;
    let fps_milli: u128 = if fps_den == 0 {
        0
    } else {
        ((fps_num as u128) * 1000) / (fps_den as u128)
    };
    match criteria {
        "resolution" => (area, br, fps_milli),
        "bitrate" => (br, area, fps_milli),
        "fps" => (fps_milli, area, br),
        _ => (area, br, fps_milli),
    }
}
