//! Module for h264.

use crate::transcoder::prelude::*;

/// Holds state for H264RateLimit.
pub(crate) struct H264RateLimit {
    pub(crate) max_bitrate_bits: i64,
    pub(crate) max_buffer_bits: i64,
}

/// Runs the h264 high profile rate limits operation.
pub(crate) fn h264_high_profile_rate_limits(level: H264Level) -> Option<H264RateLimit> {
    use H264Level::*;
    const K: i64 = 1_000;

    let (rate_kbits, buffer_kbits) = match level {
        Level1 => (80, 175),
        Level1_1 => (240, 500),
        Level1_2 => (480, 1_000),
        Level1_3 => (960, 2_000),
        Level2 => (2_500, 2_000),
        Level2_1 => (4_000, 4_000),
        Level2_2 => (4_000, 4_000),
        Level3 => (12_500, 10_000),
        Level3_1 => (17_500, 14_000),
        Level3_2 => (25_000, 20_000),
        Level4 => (25_000, 25_000),
        Level4_1 => (62_500, 62_500),
        Level4_2 => (62_500, 62_500),
        Level5 => (135_000, 135_000),
        Level5_1 => (240_000, 240_000),
        Level5_2 => return None,
    };

    Some(H264RateLimit {
        max_bitrate_bits: rate_kbits * K,
        max_buffer_bits: buffer_kbits * K,
    })
}

/// Runs the enforce h264 constraints operation.
pub(crate) fn enforce_h264_constraints(
    encode_context: &mut AVCodecContext,
    target_profile: H264Profile,
    target_level: H264Level,
    encoder_name: &str,
) {
    let level_option_value = level_option_value_for_encoder(encoder_name, target_level);
    let ctx_ptr = encode_context.as_mut_ptr();
    if should_apply_profile_option(encoder_name) {
        apply_h264_profile_option(ctx_ptr, encoder_name, target_profile);
    }
    unsafe {
        let mut level_applied = set_codec_option_str(ctx_ptr, "level", &level_option_value);
        if !level_applied && encoder_name.to_ascii_lowercase().contains("nvenc") {
            level_applied = set_codec_option_i64(ctx_ptr, "level", target_level as i32 as i64);
        }
        if !level_applied {
            debug!(
                "Failed to set level option '{}' for encoder {}; relying on direct struct assignment",
                level_option_value, encoder_name
            );
        }
        (*ctx_ptr).profile = target_profile as i32;
        (*ctx_ptr).level = target_level as i32;
    }

    let actual_profile = encode_context.profile;
    let actual_level = encode_context.level;
    let target_profile_desc = describe_h264_profile(target_profile as i32);
    let target_level_desc = describe_h264_level(target_level as i32);
    let reported_profile_desc = describe_h264_profile(actual_profile);
    let reported_level_desc = describe_h264_level(actual_level);

    info!(
        "Video encoder {}: requested profile {} level {}; reported profile {} level {}",
        encoder_name,
        target_profile_desc,
        target_level_desc,
        reported_profile_desc,
        reported_level_desc
    );

    if actual_profile == 0 {
        info!(
            "Video encoder {} reported unknown H.264 profile after init (target {})",
            encoder_name, target_profile_desc
        );
    } else if actual_profile > target_profile as i32 {
        warn!(
            "Video encoder {} elevated profile to {} (target was {})",
            encoder_name,
            describe_h264_profile(actual_profile),
            describe_h264_profile(target_profile as i32)
        );
    }

    if actual_level == 0 {
        info!(
            "Video encoder {} reported unknown H.264 level after init (target {})",
            encoder_name, target_level_desc
        );
    } else if actual_level > target_level as i32 {
        warn!(
            "Video encoder {} elevated level to {} (target was {})",
            encoder_name,
            describe_h264_level(actual_level),
            describe_h264_level(target_level as i32)
        );
    }
}

/// Runs the should apply profile option operation.
pub(crate) fn should_apply_profile_option(encoder_name: &str) -> bool {
    let encoder_name_lower = encoder_name.to_ascii_lowercase();
    encoder_name_lower.contains("x264") || encoder_name_lower.contains("nvenc")
}

/// Runs the level option value for encoder operation.
pub(crate) fn level_option_value_for_encoder(encoder_name: &str, level: H264Level) -> String {
    let lower = encoder_name.to_ascii_lowercase();
    if lower.contains("nvenc") || lower.contains("amf") || lower.contains("qsv") {
        level.ffmpeg_name().to_string()
    } else {
        (level as i32).to_string()
    }
}

/// Runs the apply h264 profile option operation.
pub(crate) fn apply_h264_profile_option(
    ctx_ptr: *mut ffi::AVCodecContext,
    encoder_name: &str,
    profile: H264Profile,
) {
    let lower = encoder_name.to_ascii_lowercase();
    let applied = unsafe { set_codec_option_str(ctx_ptr, "profile", profile.ffmpeg_name()) };
    if !applied && lower.contains("nvenc") {
        if let Some(value) = nvenc_profile_value(profile) {
            unsafe {
                set_codec_option_i64(ctx_ptr, "profile", value);
            }
        }
    }
}

/// Runs the nvenc profile value operation.
fn nvenc_profile_value(profile: H264Profile) -> Option<i64> {
    match profile {
        H264Profile::Baseline => Some(0),
        H264Profile::Main => Some(1),
        H264Profile::High => Some(2),
        H264Profile::High444 => Some(3),
        _ => None,
    }
}

#[derive(Debug)]
/// Holds state for HwProfileLevelMismatch.
pub(crate) struct HwProfileLevelMismatch {
    pub(crate) encoder: String,
    pub(crate) expected_profile: H264Profile,
    pub(crate) expected_level: H264Level,
    pub(crate) actual_profile: Option<H264Profile>,
    pub(crate) actual_level: Option<H264Level>,
    pub(crate) used_hw_encoder: bool,
    pub(crate) output_path: String,
}

/// Provides methods for `HwProfileLevelMismatch`.
impl HwProfileLevelMismatch {
    /// Runs the new operation.
    fn new(
        encoder: String,
        expected_profile: H264Profile,
        expected_level: H264Level,
        actual_profile: Option<H264Profile>,
        actual_level: Option<H264Level>,
        used_hw_encoder: bool,
        output_path: String,
    ) -> Self {
        Self {
            encoder,
            expected_profile,
            expected_level,
            actual_profile,
            actual_level,
            used_hw_encoder,
            output_path,
        }
    }
}

/// Provides methods for `HwProfileLevelMismatch`.
impl std::fmt::Display for HwProfileLevelMismatch {
    /// Runs the fmt operation.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "encoder {} produced H.264 profile {:?} level {:?} but expected profile {:?} level {:?} for {}",
            self.encoder,
            self.actual_profile.unwrap_or(H264Profile::Baseline),
            self.actual_level.unwrap_or(H264Level::Level1),
            self.expected_profile,
            self.expected_level,
            self.output_path
        )
    }
}

/// Provides methods for `HwProfileLevelMismatch {}`.
impl std::error::Error for HwProfileLevelMismatch {}

#[derive(Debug)]
/// Holds state for HwEncoderInitError.
pub(crate) struct HwEncoderInitError {
    pub(crate) encoder: String,
    pub(crate) message: String,
}

/// Provides methods for `HwEncoderInitError`.
impl HwEncoderInitError {
    /// Runs the new operation.
    pub(crate) fn new(encoder: String, message: String) -> Self {
        Self { encoder, message }
    }
}

/// Provides methods for `HwEncoderInitError`.
impl std::fmt::Display for HwEncoderInitError {
    /// Runs the fmt operation.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "hardware encoder {} failed to initialize: {}",
            self.encoder, self.message
        )
    }
}

/// Provides methods for `HwEncoderInitError {}`.
impl std::error::Error for HwEncoderInitError {}

#[derive(Debug)]
/// Holds state for DecoderError.
pub(crate) struct DecoderError {
    codec: String,
    stream_index: i32,
    message: String,
}

/// Provides methods for `DecoderError`.
impl DecoderError {
    /// Runs the new operation.
    pub(crate) fn new(codec: String, stream_index: i32, message: String) -> Self {
        Self {
            codec,
            stream_index,
            message,
        }
    }
}

/// Provides methods for `DecoderError`.
impl std::fmt::Display for DecoderError {
    /// Runs the fmt operation.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "decoder '{}' for stream {} failed: {}",
            self.codec, self.stream_index, self.message
        )
    }
}

/// Provides methods for `DecoderError {}`.
impl std::error::Error for DecoderError {}

#[derive(Debug, Clone)]
/// Holds state for H264Verification.
pub(crate) struct H264Verification {
    pub(crate) expected_profile: H264Profile,
    pub(crate) expected_level: H264Level,
    pub(crate) actual_profile: H264Profile,
    pub(crate) actual_level: H264Level,
}

/// Provides methods for `H264Verification`.
impl H264Verification {
    /// Runs the is valid operation.
    pub(crate) fn is_valid(&self) -> bool {
        self.actual_profile == self.expected_profile && self.actual_level == self.expected_level
    }
}

/// Runs the verify output h264 profile level operation.
pub(crate) fn verify_output_h264_profile_level(
    output_file: &CStr,
    output_path: &Path,
    expected_profile: H264Profile,
    expected_level: H264Level,
    encoder_name: Option<&str>,
    used_hw_encoder: bool,
) -> Result<H264Verification> {
    let display_path = output_path.display().to_string();
    let input_ctx = AVFormatContextInput::open(output_file)
        .with_context(|| format!("Opening '{}' to verify H.264 profile/level", display_path))?;

    let mut actual_profile: Option<H264Profile> = None;
    let mut actual_level: Option<H264Level> = None;

    for stream in input_ctx.streams() {
        if stream.codecpar().codec_type != ffi::AVMEDIA_TYPE_VIDEO {
            continue;
        }
        if stream.codecpar().codec_id != ffi::AV_CODEC_ID_H264 {
            break;
        }
        actual_profile = H264Profile::try_from(stream.codecpar().profile).ok();
        actual_level = H264Level::try_from(stream.codecpar().level).ok();
        break;
    }

    match (actual_profile, actual_level) {
        (Some(profile), Some(level)) if profile == expected_profile && level == expected_level => {
            debug!(
                "Verified H.264 profile {:?} level {:?} for '{}'",
                profile, level, display_path
            );
            Ok(H264Verification {
                expected_profile,
                expected_level,
                actual_profile: profile,
                actual_level: level,
            })
        }
        _ => Err(anyhow!(HwProfileLevelMismatch::new(
            encoder_name.unwrap_or("unknown encoder").to_string(),
            expected_profile,
            expected_level,
            actual_profile,
            actual_level,
            used_hw_encoder,
            display_path,
        ))),
    }
}

/// Runs the check h264 profile level constraints operation.
pub(crate) fn check_h264_profile_level_constraints(
    stream_codec_id: ffi::AVCodecID,
    raw_profile: i32,
    raw_level: i32,
    min_h264_profile: H264Profile,
    min_h264_level: H264Level,
    reasons: &mut Vec<String>,
) {
    if stream_codec_id != ffi::AV_CODEC_ID_H264 {
        return;
    }

    match H264Profile::try_from(raw_profile) {
        Ok(profile) => {
            if profile > min_h264_profile {
                reasons.push(format!(
                    "H.264 profile {:?} exceeds device limit {:?}",
                    profile, min_h264_profile
                ));
            }
        }
        Err(_) => reasons.push("H.264 profile unknown; cannot confirm compatibility".into()),
    }

    match H264Level::try_from(raw_level) {
        Ok(level) => {
            if level > min_h264_level {
                reasons.push(format!(
                    "H.264 level {:?} exceeds device limit {:?}",
                    level, min_h264_level
                ));
            }
        }
        Err(_) => reasons.push("H.264 level unknown; cannot confirm compatibility".into()),
    }
}

/// Runs the set codec option str operation.
pub(crate) unsafe fn set_codec_option_str(
    ctx: *mut ffi::AVCodecContext,
    key: &str,
    value: &str,
) -> bool {
    if ctx.is_null() {
        warn!(
            "Failed to set codec option {}='{}': encoder context is null",
            key, value
        );
        return false;
    }
    match (CString::new(key), CString::new(value)) {
        (Ok(k), Ok(v)) => {
            let ret = ffi::av_opt_set(
                ctx as *mut c_void,
                k.as_ptr(),
                v.as_ptr(),
                ffi::AV_OPT_SEARCH_CHILDREN as i32,
            );
            if ret == 0 {
                trace!("Codec option {}='{}' set", key, value);
                true
            } else if ret != ffi::AVERROR_OPTION_NOT_FOUND {
                warn!(
                    "Failed to set codec option {}='{}': {}",
                    key,
                    value,
                    av_error_to_string(ret)
                );
                false
            } else {
                false
            }
        }
        _ => {
            warn!(
                "Failed to set codec option {}='{}': invalid CString",
                key, value
            );
            false
        }
    }
}

/// Runs the set codec option i64 operation.
pub(crate) unsafe fn set_codec_option_i64(
    ctx: *mut ffi::AVCodecContext,
    key: &str,
    value: i64,
) -> bool {
    if ctx.is_null() {
        warn!(
            "Failed to set codec option {}={} (int): encoder context is null",
            key, value
        );
        return false;
    }
    match CString::new(key) {
        Ok(k) => {
            let ret = ffi::av_opt_set_int(
                ctx as *mut c_void,
                k.as_ptr(),
                value,
                ffi::AV_OPT_SEARCH_CHILDREN as i32,
            );
            if ret == 0 {
                trace!("Codec option {}={} (int) set", key, value);
                true
            } else if ret != ffi::AVERROR_OPTION_NOT_FOUND {
                warn!(
                    "Failed to set codec option {}={} (int): {}",
                    key,
                    value,
                    av_error_to_string(ret)
                );
                false
            } else {
                false
            }
        }
        Err(_) => {
            warn!(
                "Failed to set codec option {}={} (int): invalid CString",
                key, value
            );
            false
        }
    }
}
