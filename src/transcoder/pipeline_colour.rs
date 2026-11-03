//! Colour interpretation and sample-aspect-ratio selection for re-encoded video.
//!
//! The software path converts frames with the legacy `sws_scale` API, which
//! never reads a frame's colour tags: left alone it assumes BT.601 and infers
//! range from the pixel format only. These helpers tell swscale what the source
//! really is, keep the source matrix through the conversion, and tag the
//! encoder with what it actually receives.

use crate::transcoder::prelude::*;

/// Height from which untagged video is assumed to be BT.709, matching what
/// players and FFmpeg's own heuristics do for HD material.
const HD_MIN_HEIGHT: i32 = 720;

/// The Y'CbCr matrix a stream uses: its tag when it has a usable one,
/// otherwise BT.601 for SD and BT.709 for HD.
pub(crate) fn resolved_matrix(colorspace: ffi::AVColorSpace, height: i32) -> ffi::AVColorSpace {
    match colorspace {
        ffi::AVCOL_SPC_BT709
        | ffi::AVCOL_SPC_BT470BG
        | ffi::AVCOL_SPC_SMPTE170M
        | ffi::AVCOL_SPC_SMPTE240M
        | ffi::AVCOL_SPC_FCC
        | ffi::AVCOL_SPC_BT2020_NCL
        | ffi::AVCOL_SPC_BT2020_CL => colorspace,
        _ if height >= HD_MIN_HEIGHT => ffi::AVCOL_SPC_BT709,
        _ => ffi::AVCOL_SPC_SMPTE170M,
    }
}

/// swscale coefficient table id for a matrix.
fn sws_colorspace(matrix: ffi::AVColorSpace) -> i32 {
    (match matrix {
        ffi::AVCOL_SPC_BT709 => ffi::SWS_CS_ITU709,
        ffi::AVCOL_SPC_FCC => ffi::SWS_CS_FCC,
        ffi::AVCOL_SPC_SMPTE240M => ffi::SWS_CS_SMPTE240M,
        ffi::AVCOL_SPC_BT2020_NCL | ffi::AVCOL_SPC_BT2020_CL => ffi::SWS_CS_BT2020,
        _ => ffi::SWS_CS_ITU601,
    }) as i32
}

/// Whether samples use the full 0-255 range: an explicit JPEG range tag, or a
/// legacy `yuvj*` pixel format when the range is untagged.
pub(crate) fn is_full_range(range: ffi::AVColorRange, pix_fmt: ffi::AVPixelFormat) -> bool {
    match range {
        ffi::AVCOL_RANGE_JPEG => true,
        ffi::AVCOL_RANGE_MPEG => false,
        _ => matches!(
            pix_fmt,
            ffi::AV_PIX_FMT_YUVJ420P
                | ffi::AV_PIX_FMT_YUVJ422P
                | ffi::AV_PIX_FMT_YUVJ444P
                | ffi::AV_PIX_FMT_YUVJ440P
                | ffi::AV_PIX_FMT_YUVJ411P
        ),
    }
}

/// Build a swscale context that converts `frame` to `dst_w`x`dst_h` in
/// `dst_format`, reading the frame's own matrix and range and writing
/// `dst_matrix` at limited range, or full range when `dst_full` (RGB).
///
/// Ranges must be known before `sws_init_context`: for same-size, same-format
/// conversions swscale otherwise picks a plain copy and later colour details
/// never take effect, so full-range input would pass through unconverted.
pub(crate) fn colour_aware_sws_context(
    frame: &AVFrame,
    dst_w: i32,
    dst_h: i32,
    dst_format: ffi::AVPixelFormat,
    flags: ffi::SwsFlags,
    dst_matrix: ffi::AVColorSpace,
    dst_full: bool,
) -> Option<SwsContext> {
    let src_full = is_full_range(frame.color_range, frame.format as ffi::AVPixelFormat);
    // Configure through AVOptions rather than struct fields: the public
    // SwsContext layout differs between FFmpeg releases, so field offsets in
    // the generated bindings are not safe across library versions.
    let options: [(&CStr, i64); 9] = [
        (c"srcw", frame.width as i64),
        (c"srch", frame.height as i64),
        (c"src_format", frame.format as i64),
        (c"dstw", dst_w as i64),
        (c"dsth", dst_h as i64),
        (c"dst_format", dst_format as i64),
        (c"sws_flags", flags as i64),
        (c"src_range", src_full as i64),
        (c"dst_range", dst_full as i64),
    ];
    let mut context = unsafe {
        let raw = std::ptr::NonNull::new(ffi::sws_alloc_context())?;
        // Owned from here on, so the context is freed on every early return.
        let context = SwsContext::from_raw(raw);
        for (name, value) in options {
            if ffi::av_opt_set_int(raw.as_ptr().cast(), name.as_ptr(), value, 0) < 0 {
                return None;
            }
        }
        if ffi::sws_init_context(raw.as_ptr(), ptr::null_mut(), ptr::null_mut()) < 0 {
            return None;
        }
        context
    };
    configure_sws_colour(&mut context, frame, src_full, dst_matrix, dst_full);
    Some(context)
}

/// Set the conversion matrices on an initialised context: the frame's own
/// matrix in, `dst_matrix` out, with the ranges the context was built for.
fn configure_sws_colour(
    sws_context: &mut SwsContext,
    frame: &AVFrame,
    src_full: bool,
    dst_matrix: ffi::AVColorSpace,
    dst_full: bool,
) {
    let src_matrix = resolved_matrix(frame.colorspace, frame.height);
    let rc = unsafe {
        ffi::sws_setColorspaceDetails(
            sws_context.as_mut_ptr(),
            ffi::sws_getCoefficients(sws_colorspace(src_matrix)),
            src_full as i32,
            ffi::sws_getCoefficients(sws_colorspace(dst_matrix)),
            dst_full as i32,
            0,
            1 << 16,
            1 << 16,
        )
    };
    if rc < 0 {
        // Not fatal: swscale refuses this for some format pairs and falls
        // back to its defaults, which is what happened before this was set.
        trace!(
            "swscale ignored colour details for pix_fmt {}",
            frame.format
        );
    }
}

/// `primary` unless it is the "unspecified" value, else `fallback`. Generic
/// because bindgen gives FFmpeg's C enums different integer types per
/// platform (unsigned on Linux and macOS, signed with MSVC).
fn first_specified<T: PartialEq>(primary: T, fallback: T, unspecified: T) -> T {
    if primary != unspecified {
        primary
    } else {
        fallback
    }
}

/// Copy the source's colour description onto the encoder. The matrix is the
/// one `configure_sws_colour` keeps, and output is always limited range
/// because the encoder receives `yuv420p`. Primaries, transfer and chroma
/// siting pass through only when the source states them.
pub(crate) fn apply_output_colour(
    decode_context: &AVCodecContext,
    encode_context: &mut AVCodecContext,
    input_stream: &AVStreamRef,
) {
    let par = input_stream.codecpar();
    let colorspace = first_specified(
        decode_context.colorspace,
        par.color_space,
        ffi::AVCOL_SPC_UNSPECIFIED,
    );
    let primaries = first_specified(
        decode_context.color_primaries,
        par.color_primaries,
        ffi::AVCOL_PRI_UNSPECIFIED,
    );
    let trc = first_specified(
        decode_context.color_trc,
        par.color_trc,
        ffi::AVCOL_TRC_UNSPECIFIED,
    );
    let chroma = first_specified(
        decode_context.chroma_sample_location,
        par.chroma_location,
        ffi::AVCHROMA_LOC_UNSPECIFIED,
    );
    // DPN does not tone map, so an HDR source becomes 8-bit SDR-range H.264 or
    // HEVC. Carrying PQ/HLG and BT.2020 tags over would ask displays to switch
    // to HDR for a stream that is not HDR10/HLG, so leave them unset as before.
    let hdr = matches!(trc, ffi::AVCOL_TRC_SMPTE2084 | ffi::AVCOL_TRC_ARIB_STD_B67);
    let (primaries, trc) = if hdr {
        debug!("HDR source re-encoded without tone mapping; not tagging output as HDR");
        (ffi::AVCOL_PRI_UNSPECIFIED, ffi::AVCOL_TRC_UNSPECIFIED)
    } else {
        (primaries, trc)
    };
    let matrix = resolved_matrix(colorspace, decode_context.height.max(par.height));
    unsafe {
        let enc = encode_context.as_mut_ptr();
        (*enc).colorspace = matrix;
        (*enc).color_range = ffi::AVCOL_RANGE_MPEG;
        (*enc).color_primaries = primaries;
        (*enc).color_trc = trc;
        (*enc).chroma_sample_location = chroma;
    }
    debug!(
        "Output colour: matrix {} (source tag {}), primaries {}, trc {}, limited range",
        matrix, colorspace, primaries, trc
    );
}

fn valid_ratio(r: ffi::AVRational) -> bool {
    r.num > 0 && r.den > 0
}

/// Sample aspect ratio with FFmpeg's precedence (`av_guess_sample_aspect_ratio`):
/// the container's stream value first, then codec parameters, then the decoder.
/// Matroska and MP4 often carry SAR only at container level.
pub(crate) fn effective_sample_aspect_ratio(
    input_stream: &AVStreamRef,
    decode_context: &AVCodecContext,
) -> ffi::AVRational {
    [
        input_stream.sample_aspect_ratio,
        input_stream.codecpar().sample_aspect_ratio,
        decode_context.sample_aspect_ratio,
    ]
    .into_iter()
    .find(|r| valid_ratio(*r))
    .unwrap_or(ffi::AVRational { num: 0, den: 1 })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn untagged_matrix_follows_resolution() {
        assert_eq!(
            resolved_matrix(ffi::AVCOL_SPC_UNSPECIFIED, 480),
            ffi::AVCOL_SPC_SMPTE170M
        );
        assert_eq!(
            resolved_matrix(ffi::AVCOL_SPC_UNSPECIFIED, 576),
            ffi::AVCOL_SPC_SMPTE170M
        );
        assert_eq!(
            resolved_matrix(ffi::AVCOL_SPC_UNSPECIFIED, 720),
            ffi::AVCOL_SPC_BT709
        );
        assert_eq!(
            resolved_matrix(ffi::AVCOL_SPC_RESERVED, 1080),
            ffi::AVCOL_SPC_BT709
        );
    }

    #[test]
    fn tagged_matrix_wins_over_resolution() {
        assert_eq!(
            resolved_matrix(ffi::AVCOL_SPC_BT709, 480),
            ffi::AVCOL_SPC_BT709
        );
        assert_eq!(
            resolved_matrix(ffi::AVCOL_SPC_BT470BG, 1080),
            ffi::AVCOL_SPC_BT470BG
        );
        assert_eq!(
            resolved_matrix(ffi::AVCOL_SPC_BT2020_NCL, 2160),
            ffi::AVCOL_SPC_BT2020_NCL
        );
    }

    #[test]
    fn matrices_map_to_matching_swscale_tables() {
        assert_eq!(
            sws_colorspace(ffi::AVCOL_SPC_BT709),
            ffi::SWS_CS_ITU709 as i32
        );
        assert_eq!(
            sws_colorspace(ffi::AVCOL_SPC_SMPTE170M),
            ffi::SWS_CS_ITU601 as i32
        );
        assert_eq!(
            sws_colorspace(ffi::AVCOL_SPC_BT470BG),
            ffi::SWS_CS_ITU601 as i32
        );
        assert_eq!(
            sws_colorspace(ffi::AVCOL_SPC_BT2020_NCL),
            ffi::SWS_CS_BT2020 as i32
        );
    }

    #[test]
    fn range_comes_from_tag_then_legacy_pixel_format() {
        assert!(is_full_range(
            ffi::AVCOL_RANGE_JPEG,
            ffi::AV_PIX_FMT_YUV420P
        ));
        assert!(!is_full_range(
            ffi::AVCOL_RANGE_MPEG,
            ffi::AV_PIX_FMT_YUVJ420P
        ));
        assert!(is_full_range(
            ffi::AVCOL_RANGE_UNSPECIFIED,
            ffi::AV_PIX_FMT_YUVJ420P
        ));
        assert!(!is_full_range(
            ffi::AVCOL_RANGE_UNSPECIFIED,
            ffi::AV_PIX_FMT_YUV420P
        ));
    }
}
