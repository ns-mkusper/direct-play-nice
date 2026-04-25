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

/// Criteria used when scoring candidate video streams.
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum ScoreCriteria {
    /// Prioritize resolution (pixel area), then bitrate, then frame rate.
    Resolution,
    /// Prioritize bitrate, then resolution, then frame rate.
    Bitrate,
    /// Prioritize frame rate, then resolution, then bitrate.
    Fps,
}

/// Creates a sortable score tuple for primary-video selection.
///
/// The tuple order depends on `criteria`.
///
/// # Examples
///
/// ```rust
/// let score = direct_play_nice::score_video(
///     direct_play_nice::ScoreCriteria::Resolution,
///     1920,
///     1080,
///     8_000_000,
///     30000,
///     1001,
/// );
/// assert!(score.0 > 0);
/// ```
pub fn score_video(
    criteria: ScoreCriteria,
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
        ScoreCriteria::Resolution => (area, br, fps_milli),
        ScoreCriteria::Bitrate => (br, area, fps_milli),
        ScoreCriteria::Fps => (fps_milli, area, br),
    }
}

/// Backward-compatible string-based scoring helper.
///
/// Unknown values default to [`ScoreCriteria::Resolution`].
pub fn score_video_legacy(
    criteria: &str,
    width: u32,
    height: u32,
    bitrate: u64,
    fps_num: u32,
    fps_den: u32,
) -> (u128, u128, u128) {
    let criteria = match criteria {
        "bitrate" => ScoreCriteria::Bitrate,
        "fps" => ScoreCriteria::Fps,
        _ => ScoreCriteria::Resolution,
    };
    score_video(criteria, width, height, bitrate, fps_num, fps_den)
}
