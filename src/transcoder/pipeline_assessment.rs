//! Direct-play evaluation helpers that explain compatibility decisions and track post-conversion verification outcomes.

use crate::transcoder::prelude::*;

pub(crate) struct DirectPlayAssessment {
    pub(crate) compatible: bool,
    pub(crate) reasons: Vec<String>,
}

#[derive(Debug, Default)]
/// Post-conversion signals consumed by retry and warning logic.
pub(crate) struct ConversionOutcome {
    pub(crate) h264_verification: Option<H264Verification>,
}

impl ConversionOutcome {
    pub(crate) fn profile_verified(&self) -> bool {
        self.h264_verification
            .as_ref()
            .map(|check| check.is_valid())
            .unwrap_or(true)
    }
}

#[derive(Clone, Copy)]
/// Inputs required to evaluate whether a source can direct-play unchanged.
pub(crate) struct DirectPlayConstraints<'a> {
    pub(crate) target_is_mp4: bool,
    pub(crate) sub_mode: SubMode,
    pub(crate) target_video_codec: ffi::AVCodecID,
    pub(crate) target_audio_codec: ffi::AVCodecID,
    pub(crate) h264_constraints: Option<(H264Profile, H264Level)>,
    pub(crate) max_fps: u32,
    pub(crate) device_cap: (u32, u32),
    pub(crate) supported_containers: &'a [ContainerFormat],
    pub(crate) quality_limits: &'a QualityLimits,
    pub(crate) primary_video_stream_index: Option<usize>,
    pub(crate) primary_criteria: PrimaryVideoCriteria,
}

pub(crate) fn assess_direct_play_compatibility(
    input_file: &CStr,
    constraints: DirectPlayConstraints<'_>,
) -> Result<DirectPlayAssessment> {
    let DirectPlayConstraints {
        target_is_mp4,
        sub_mode,
        target_video_codec,
        target_audio_codec,
        h264_constraints,
        max_fps,
        device_cap,
        supported_containers,
        quality_limits,
        primary_video_stream_index,
        primary_criteria,
    } = constraints;

    let ictx = AVFormatContextInput::open(input_file)?;
    let primary_idx =
        select_primary_video_stream_index(&ictx, primary_video_stream_index, primary_criteria)?;

    let streams: Vec<_> = ictx.streams().iter().collect();
    let video_stream = streams.get(primary_idx).ok_or_else(|| {
        anyhow!(
            "Primary video stream index {} out of range while checking direct-play compatibility",
            primary_idx
        )
    })?;

    // Collect every incompatibility so users can see the full decision context.
    let mut reasons = Vec::new();
    let video_par = video_stream.codecpar();
    let detected_format = input_format_name(&ictx);
    let input_path = PathBuf::from(input_file.to_string_lossy().into_owned());
    let input_ext = input_path
        .extension()
        .and_then(|ext| ext.to_str())
        .map(|ext| ext.to_ascii_lowercase())
        .unwrap_or_default();
    let input_container = container_from_detected_input(&detected_format, &input_ext);
    match input_container {
        Some(container) => {
            if !supported_containers.contains(&container) {
                reasons.push(format!(
                    "input container '{}' (detected as '{}') is not supported by all selected devices",
                    container.as_str(),
                    detected_format
                ));
            }
        }
        None => reasons.push(format!(
            "input container is unknown (detected as '{}', extension '{}'); cannot confirm compatibility",
            detected_format, input_ext
        )),
    }

    for stream in &streams {
        let disposition_flags = stream_disposition(stream);
        if (disposition_flags & ffi::AV_DISPOSITION_ATTACHED_PIC as i32) != 0 {
            reasons.push("input contains an attached picture stream".to_string());
            break;
        }
        if stream.codecpar().codec_type == ffi::AVMEDIA_TYPE_ATTACHMENT {
            reasons.push("input contains an attachment stream".to_string());
            break;
        }
    }

    if video_par.codec_id != target_video_codec {
        reasons.push(format!(
            "video codec {} is not compatible with required {}",
            describe_codec(video_par.codec_id),
            describe_codec(target_video_codec)
        ));
    }

    if video_par.width <= 0 || video_par.height <= 0 {
        reasons.push("video resolution unknown".to_string());
    } else if (video_par.width as u32) > device_cap.0 || (video_par.height as u32) > device_cap.1 {
        reasons.push(format!(
            "video resolution {}x{} exceeds device limit {}x{}",
            video_par.width, video_par.height, device_cap.0, device_cap.1
        ));
    }

    if let Some((quality_w, quality_h)) = quality_limits.max_video_dimensions {
        if video_par.width > 0
            && video_par.height > 0
            && ((video_par.width as u32) > quality_w || (video_par.height as u32) > quality_h)
        {
            reasons.push(format!(
                "video resolution {}x{} exceeds requested quality limit {}x{}",
                video_par.width, video_par.height, quality_w, quality_h
            ));
        }
    }

    if let Some(max_video_bitrate) = quality_limits.max_video_bitrate {
        let mut video_bit_rate = video_par.bit_rate;
        if video_bit_rate <= 0 {
            // Some demuxers expose bitrate only through the raw codecpar pointer.
            video_bit_rate = stream_raw_bit_rate(video_stream);
        }
        if video_bit_rate <= 0 {
            reasons.push(
                "video bitrate unknown; cannot confirm compliance with requested quality limit"
                    .into(),
            );
        } else if video_bit_rate > max_video_bitrate {
            reasons.push(format!(
                "video bitrate {} bps exceeds requested limit {} bps",
                video_bit_rate, max_video_bitrate
            ));
        }
    }

    if max_fps > 0 {
        match estimate_stream_fps(video_stream) {
            Some(fps) => {
                if fps > max_fps as f64 + 0.5 {
                    reasons.push(format!(
                        "video frame rate {:.2} fps exceeds device limit {} fps",
                        fps, max_fps
                    ));
                }
            }
            None => reasons.push("video frame rate unknown; cannot confirm compatibility".into()),
        }
    }

    if target_video_codec == ffi::AV_CODEC_ID_H264 {
        if let Some((min_h264_profile, min_h264_level)) = h264_constraints {
            check_h264_profile_level_constraints(
                video_par.codec_id,
                video_par.profile,
                video_par.level,
                min_h264_profile,
                min_h264_level,
                &mut reasons,
            );
        } else {
            reasons.push(
                "H.264 constraints unavailable; cannot confirm profile/level compatibility".into(),
            );
        }
    }

    let mut audio_ok = false;
    let mut audio_quality_reason: Option<String> = None;
    for stream in &streams {
        let codecpar = stream.codecpar();
        if codecpar.codec_type != ffi::AVMEDIA_TYPE_AUDIO {
            continue;
        }
        if codecpar.codec_id != target_audio_codec {
            continue;
        }

        if let Some(max_audio_bitrate) = quality_limits.max_audio_bitrate {
            let mut audio_bit_rate = codecpar.bit_rate;
            if audio_bit_rate <= 0 {
                // Mirror video-path fallback for containers with sparse stream metadata.
                audio_bit_rate = stream_raw_bit_rate(stream);
            }

            if audio_bit_rate <= 0 {
                if audio_quality_reason.is_none() {
                    audio_quality_reason = Some(
                        "audio bitrate unknown; cannot confirm compliance with requested quality limit"
                            .into(),
                    );
                }
                continue;
            }

            if audio_bit_rate > max_audio_bitrate {
                if audio_quality_reason.is_none() {
                    audio_quality_reason = Some(format!(
                        "audio bitrate {} bps exceeds requested limit {} bps",
                        audio_bit_rate, max_audio_bitrate
                    ));
                }
                continue;
            }
        }

        audio_ok = true;
        break;
    }

    if !audio_ok {
        if let Some(reason) = audio_quality_reason {
            reasons.push(reason);
        } else {
            reasons.push(format!(
                "no audio stream with compatible codec {} found",
                describe_codec(target_audio_codec)
            ));
        }
    }

    if target_is_mp4 && !matches!(sub_mode, SubMode::Skip) {
        for stream in &streams {
            let codecpar = stream.codecpar();
            if codecpar.codec_type == ffi::AVMEDIA_TYPE_SUBTITLE
                && is_image_based_subtitle(codecpar.codec_id)
            {
                reasons.push(format!(
                    "bitmap subtitle stream {} requires OCR conversion for MP4 direct-play",
                    stream.index
                ));
                break;
            }
        }
    }

    Ok(DirectPlayAssessment {
        compatible: reasons.is_empty(),
        reasons,
    })
}

fn estimate_stream_fps(stream: &AVStreamRef) -> Option<f64> {
    if let Some(rational) = stream.guess_framerate() {
        rational_to_f64(rational)
    } else {
        let avg = unsafe { (*stream.as_ptr()).avg_frame_rate };
        rational_to_f64(avg)
    }
}

/// Maps FFmpeg demuxer names plus the path extension to the container model
/// used for device compatibility.
///
/// Demuxer names are more reliable than arbitrary extensions, but FFmpeg's MOV
/// demuxer reports a comma-separated family (`mov,mp4,m4a,...`) for multiple
/// user-visible containers. For that family the extension disambiguates the
/// device-facing container; for unambiguous demuxers like Matroska, the demuxer
/// takes precedence.
fn container_from_detected_input(demuxer_name: &str, extension: &str) -> Option<ContainerFormat> {
    if demuxer_name
        .split(',')
        .any(|part| matches!(part.trim(), "mov" | "mp4" | "m4v"))
    {
        return ContainerFormat::from_extension(extension).or(Some(ContainerFormat::Mp4));
    }

    container_from_demuxer_name(demuxer_name).or_else(|| ContainerFormat::from_extension(extension))
}

/// Maps unambiguous FFmpeg demuxer names to the container model used for device
/// compatibility.
fn container_from_demuxer_name(name: &str) -> Option<ContainerFormat> {
    name.split(',').find_map(|part| match part.trim() {
        "matroska" | "webm" => Some(ContainerFormat::Mkv),
        _ => None,
    })
}

pub(crate) fn rational_to_f64(rational: ffi::AVRational) -> Option<f64> {
    if rational.num <= 0 || rational.den <= 0 {
        None
    } else {
        Some(rational.num as f64 / rational.den as f64)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detected_container_mapping_handles_ffmpeg_alias_lists() {
        assert_eq!(
            container_from_detected_input("mov,mp4,m4a,3gp,3g2,mj2", "mp4"),
            Some(ContainerFormat::Mp4)
        );
        assert_eq!(
            container_from_detected_input("mov,mp4,m4a,3gp,3g2,mj2", "mov"),
            Some(ContainerFormat::Mov)
        );
        assert_eq!(
            container_from_detected_input("matroska,webm", "not-real"),
            Some(ContainerFormat::Mkv)
        );
    }

    #[test]
    fn demuxer_container_mapping_rejects_unknown_formats() {
        assert_eq!(container_from_detected_input("mpegts", ""), None);
        assert_eq!(container_from_detected_input("", ""), None);
    }
}
