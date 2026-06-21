//! Hardware capability helpers for FFmpeg device probing, encoder discovery, and acceleration checks.

use clap::ValueEnum;
use rsmpeg::avcodec::{AVCodec, AVCodecRef};
use rsmpeg::ffi::{self};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::ffi::{c_void, CStr, CString};
#[cfg(unix)]
use std::thread;
#[cfg(unix)]
use std::time::{Duration, Instant};

#[derive(Copy, Clone, Eq, PartialEq, Debug, ValueEnum, Deserialize)]
#[serde(rename_all = "lowercase")]
/// User-facing hardware-acceleration preference.
pub enum HwAccel {
    /// Auto-select hardware acceleration (prefers CUDA path in current logic).
    Auto,
    /// Disable hardware acceleration.
    None,
    /// NVIDIA NVENC-based acceleration.
    Nvenc,
    /// VAAPI-based acceleration.
    Vaapi,
    /// Intel Quick Sync Video acceleration.
    Qsv,
    /// Apple VideoToolbox acceleration.
    Videotoolbox,
    /// AMD AMF acceleration.
    Amf,
}

/// Reports whether the linked FFmpeg avfilter build exposes the `scale_cuda` filter.
pub fn scale_cuda_filter_available() -> bool {
    #[cfg(target_os = "linux")]
    {
        let Ok(name) = CString::new("scale_cuda") else {
            return false;
        };
        unsafe { !ffi::avfilter_get_by_name(name.as_ptr()).is_null() }
    }

    #[cfg(not(target_os = "linux"))]
    {
        false
    }
}

/// Attempts to create an FFmpeg hardware device by name.
///
/// Returns a retained `AVBufferRef` on success.
pub fn try_create_hw_device(device_name: &str) -> Option<*mut ffi::AVBufferRef> {
    unsafe {
        let c_name = CString::new(device_name).ok()?;
        let dev_type = ffi::av_hwdevice_find_type_by_name(c_name.as_ptr());
        if dev_type == ffi::AV_HWDEVICE_TYPE_NONE {
            return None;
        }
        let mut buf: *mut ffi::AVBufferRef = std::ptr::null_mut();
        let ret = ffi::av_hwdevice_ctx_create(
            &mut buf,
            dev_type,
            std::ptr::null(),
            std::ptr::null_mut(),
            0,
        );
        if ret >= 0 && !buf.is_null() {
            Some(buf)
        } else {
            None
        }
    }
}

/// Acquires a hardware device based on the selected preference.
pub fn acquire_hw_device(pref: HwAccel) -> Option<*mut ffi::AVBufferRef> {
    match pref {
        HwAccel::Auto | HwAccel::Nvenc => try_create_hw_device("cuda"),
        HwAccel::Vaapi => try_create_hw_device("vaapi"),
        HwAccel::Qsv => try_create_hw_device("qsv"),
        HwAccel::Videotoolbox => try_create_hw_device("videotoolbox"),
        HwAccel::Amf => try_create_hw_device("d3d11va").or_else(|| try_create_hw_device("dxva2")),
        HwAccel::None => None,
    }
}

/// Finds a suitable hardware encoder and matching device context.
pub fn find_hw_encoder(
    codec_id: ffi::AVCodecID,
    pref: HwAccel,
    shared_device: Option<*mut ffi::AVBufferRef>,
) -> (Option<AVCodecRef<'static>>, Option<*mut ffi::AVBufferRef>) {
    let encoders = encoder_candidates(codec_id);

    let filter = |name: &str| match pref {
        HwAccel::Auto => true,
        HwAccel::None => false,
        HwAccel::Nvenc => name.contains("nvenc"),
        HwAccel::Vaapi => name.contains("vaapi"),
        HwAccel::Qsv => name.contains("qsv"),
        HwAccel::Videotoolbox => name.contains("videotoolbox"),
        HwAccel::Amf => name.contains("amf"),
    };

    for (encoder_name, dev_types) in encoders.iter().copied() {
        if !filter(encoder_name) {
            continue;
        }
        let Ok(cname) = CString::new(encoder_name) else {
            continue;
        };
        if let Some(encoder) = AVCodec::find_encoder_by_name(cname.as_c_str()) {
            let mut device_ref: Option<*mut ffi::AVBufferRef> = None;
            let mut success = dev_types.is_empty();
            if let Some(shared) = shared_device {
                unsafe {
                    device_ref = Some(ffi::av_buffer_ref(shared));
                    success = true;
                }
            } else {
                for dev in dev_types {
                    if let Some(buf) = try_create_hw_device(dev) {
                        device_ref = Some(buf);
                        success = true;
                        break;
                    }
                }
                if success {
                    // keep the created device as-is
                }
            }
            if !success {
                continue;
            }
            return (Some(encoder), device_ref);
        }
    }
    (None, None)
}

/// Returns candidate hardware encoders for a codec id.
pub fn encoder_candidates(
    codec_id: ffi::AVCodecID,
) -> &'static [(&'static str, &'static [&'static str])] {
    match codec_id {
        ffi::AV_CODEC_ID_H264 => h264_candidates(),
        ffi::AV_CODEC_ID_HEVC => hevc_candidates(),
        _ => &[],
    }
}

#[cfg(target_os = "macos")]
const fn h264_candidates() -> &'static [(&'static str, &'static [&'static str])] {
    &[
        ("h264_videotoolbox", &["videotoolbox"]),
        ("h264_nvenc", &["cuda"]),
        ("h264_qsv", &["qsv"]),
        ("h264_vaapi", &["vaapi"]),
        ("h264_amf", &["d3d11va", "dxva2"]),
    ]
}
#[cfg(target_os = "macos")]
const fn hevc_candidates() -> &'static [(&'static str, &'static [&'static str])] {
    &[
        ("hevc_videotoolbox", &["videotoolbox"]),
        ("hevc_nvenc", &["cuda"]),
        ("hevc_qsv", &["qsv"]),
        ("hevc_vaapi", &["vaapi"]),
        ("hevc_amf", &["d3d11va", "dxva2"]),
    ]
}

#[cfg(all(not(target_os = "macos"), target_os = "windows"))]
const fn h264_candidates() -> &'static [(&'static str, &'static [&'static str])] {
    &[
        ("h264_amf", &["d3d11va", "dxva2"]),
        ("h264_nvenc", &["cuda"]),
        ("h264_qsv", &["qsv"]),
    ]
}
#[cfg(all(not(target_os = "macos"), target_os = "windows"))]
const fn hevc_candidates() -> &'static [(&'static str, &'static [&'static str])] {
    &[
        ("hevc_amf", &["d3d11va", "dxva2"]),
        ("hevc_nvenc", &["cuda"]),
        ("hevc_qsv", &["qsv"]),
    ]
}

#[cfg(all(not(target_os = "macos"), not(target_os = "windows")))]
const fn h264_candidates() -> &'static [(&'static str, &'static [&'static str])] {
    &[
        ("h264_nvenc", &["cuda"]),
        ("h264_vaapi", &["vaapi"]),
        ("h264_qsv", &["qsv"]),
    ]
}
#[cfg(all(not(target_os = "macos"), not(target_os = "windows")))]
const fn hevc_candidates() -> &'static [(&'static str, &'static [&'static str])] {
    &[
        ("hevc_nvenc", &["cuda"]),
        ("hevc_vaapi", &["vaapi"]),
        ("hevc_qsv", &["qsv"]),
    ]
}

#[cfg(unix)]
const HW_DEVICE_PROBE_TIMEOUT: Duration = Duration::from_secs(5);
const HW_DEVICE_PROBE_TYPES: &[&str] = &[
    "cuda",
    "vaapi",
    "qsv",
    "videotoolbox",
    "d3d11va",
    "dxva2",
    "opencl",
    "vulkan",
    "drm",
    "mediacodec",
];

fn linked_ffmpeg_hw_device_type(device_name: &str) -> Option<ffi::AVHWDeviceType> {
    unsafe {
        let c_name = CString::new(device_name).ok()?;
        let dev_type = ffi::av_hwdevice_find_type_by_name(c_name.as_ptr());
        (dev_type != ffi::AV_HWDEVICE_TYPE_NONE).then_some(dev_type)
    }
}

#[cfg(test)]
fn linked_ffmpeg_has_hw_device(device_name: &str) -> bool {
    linked_ffmpeg_hw_device_type(device_name).is_some()
}

#[cfg(unix)]
fn probe_hw_device_in_child_process(device_name: &str) -> bool {
    let Some(dev_type) = linked_ffmpeg_hw_device_type(device_name) else {
        return false;
    };

    unsafe {
        let pid = libc::fork();
        if pid < 0 {
            return false;
        }
        if pid == 0 {
            let mut buf: *mut ffi::AVBufferRef = std::ptr::null_mut();
            let ret = ffi::av_hwdevice_ctx_create(
                &mut buf,
                dev_type,
                std::ptr::null(),
                std::ptr::null_mut(),
                0,
            );
            let ok = ret >= 0 && !buf.is_null();
            if !buf.is_null() {
                ffi::av_buffer_unref(&mut buf);
            }
            libc::_exit(if ok { 0 } else { 2 });
        }

        let deadline = Instant::now() + HW_DEVICE_PROBE_TIMEOUT;
        loop {
            let mut status = 0;
            let wait = libc::waitpid(pid, &mut status, libc::WNOHANG);
            if wait == pid {
                return libc::WIFEXITED(status) && libc::WEXITSTATUS(status) == 0;
            }
            if wait < 0 {
                return false;
            }
            if Instant::now() >= deadline {
                let _ = libc::kill(pid, libc::SIGKILL);
                let _ = libc::waitpid(pid, &mut status, 0);
                return false;
            }
            thread::sleep(Duration::from_millis(25));
        }
    }
}

#[cfg(not(unix))]
fn probe_hw_device_in_child_process(device_name: &str) -> bool {
    if let Some(mut buf) = try_create_hw_device(device_name) {
        unsafe { ffi::av_buffer_unref(&mut buf) };
        true
    } else {
        false
    }
}

/// Probes availability of common FFmpeg hardware-device backends.
///
/// # Examples
///
/// ```rust
/// let devices = direct_play_nice::gpu::probe_hw_devices();
/// assert!(!devices.is_empty());
/// ```
pub fn probe_hw_devices() -> HashMap<&'static str, bool> {
    let mut map = HashMap::new();
    for &t in HW_DEVICE_PROBE_TYPES {
        map.insert(t, probe_hw_device_in_child_process(t));
    }
    map
}

#[derive(Serialize)]
/// JSON-friendly hardware-device probe record.
pub struct HwDeviceEntry {
    /// Device backend name (for example, `"cuda"`).
    pub name: String,
    /// Whether this backend was successfully initialized.
    pub available: bool,
}

#[derive(Serialize)]
/// JSON-friendly hardware-encoder probe record.
pub struct HwEncoderEntry {
    /// FFmpeg encoder name.
    pub name: String,
    /// Whether the encoder exists in the linked FFmpeg build.
    pub present: bool,
    /// Hardware-device backends that can back this encoder.
    pub required_devices: Vec<String>,
    /// Whether the encoder is both present and usable.
    pub available: bool,
}

/// Probes known hardware encoders and marks whether they are usable.
pub fn probe_hw_encoders(device_ok: &HashMap<&'static str, bool>) -> Vec<HwEncoderEntry> {
    let encs: &[(&str, &[&str])] = &[
        ("h264_nvenc", &["cuda"]),
        ("hevc_nvenc", &["cuda"]),
        ("h264_qsv", &["qsv"]),
        ("hevc_qsv", &["qsv"]),
        ("h264_vaapi", &["vaapi"]),
        ("hevc_vaapi", &["vaapi"]),
        ("h264_videotoolbox", &["videotoolbox"]),
        ("hevc_videotoolbox", &["videotoolbox"]),
        ("h264_amf", &["d3d11va", "dxva2"]),
        ("hevc_amf", &["d3d11va", "dxva2"]),
    ];
    let mut out = Vec::new();
    for (name, devs) in encs {
        let present = match CString::new(*name) {
            Ok(cname) => AVCodec::find_encoder_by_name(cname.as_c_str()).is_some(),
            Err(_) => false,
        };
        let usable = present
            && devs
                .iter()
                .any(|d| device_ok.get(d).copied().unwrap_or(false));
        out.push(HwEncoderEntry {
            name: name.to_string(),
            present,
            required_devices: devs.iter().map(|s| s.to_string()).collect(),
            available: usable,
        });
    }
    out
}

/// Prints a concise hardware-device and hardware-encoder probe report.
pub fn print_probe() {
    println!("Hardware Probe:\n");
    let devices = probe_hw_devices();
    println!("Devices (av_hwdevice_ctx_create):");
    for (name, ok) in devices.iter() {
        println!(
            "  - {:<13} {}",
            name,
            if *ok { "available" } else { "unavailable" }
        );
    }
    println!();
    let encs = probe_hw_encoders(&devices);
    println!("Encoders (present + device available):");
    for e in encs {
        let uses = e.required_devices.join(",");
        println!(
            "  - {:<20} {:<11} (devices: {})",
            e.name,
            if e.available {
                "available"
            } else {
                "unavailable"
            },
            uses
        );
    }
}

fn media_type_to_str(t: ffi::AVMediaType) -> &'static str {
    match t {
        ffi::AVMEDIA_TYPE_VIDEO => "video",
        ffi::AVMEDIA_TYPE_AUDIO => "audio",
        ffi::AVMEDIA_TYPE_SUBTITLE => "subtitle",
        ffi::AVMEDIA_TYPE_DATA => "data",
        _ => "other",
    }
}

/// Prints FFmpeg encoder/decoder inventory with optional filtering.
pub fn print_probe_codecs(only_video: bool, only_hw: bool) {
    unsafe {
        let ver_codec = ffi::avcodec_version();
        let ver_fmt = ffi::avformat_version();
        let ver_util = ffi::avutil_version();
        let conf = CStr::from_ptr(ffi::avcodec_configuration());
        println!("FFmpeg Versions:");
        println!("  avcodec: {}", ver_codec);
        println!("  avformat: {}", ver_fmt);
        println!("  avutil: {}", ver_util);
        println!("Configuration:\n  {}\n", conf.to_string_lossy());
        println!(
            "CUDA resize filters:\n  - scale_cuda {:<11} (linked FFmpeg avfilter)\n",
            if scale_cuda_filter_available() {
                "available"
            } else {
                "unavailable"
            }
        );

        let mut opaque: *mut c_void = std::ptr::null_mut();
        let mut enc_count = 0usize;
        let mut dec_count = 0usize;
        println!("Encoders:");
        loop {
            let codec = ffi::av_codec_iterate(&mut opaque as *mut _);
            if codec.is_null() {
                break;
            }
            if ffi::av_codec_is_encoder(codec) != 0 {
                enc_count += 1;
                let name = if !(*codec).name.is_null() {
                    CStr::from_ptr((*codec).name).to_string_lossy().into_owned()
                } else {
                    String::from("<unknown>")
                };
                let long_name = if !(*codec).long_name.is_null() {
                    CStr::from_ptr((*codec).long_name)
                        .to_string_lossy()
                        .into_owned()
                } else {
                    String::new()
                };
                let mt = media_type_to_str((*codec).type_);
                let is_video = mt == "video";
                let is_hw_encoder = name.contains("nvenc")
                    || name.contains("vaapi")
                    || name.contains("qsv")
                    || name.contains("videotoolbox")
                    || name.contains("amf");
                if (only_video && !is_video) || (only_hw && !is_hw_encoder) {
                    continue;
                }
                println!(
                    "  - {:<22} {:<40} [{}{}]",
                    name,
                    long_name,
                    mt,
                    if is_hw_encoder { ", hw" } else { "" }
                );
            }
        }

        opaque = std::ptr::null_mut();
        println!("\nDecoders (with HW configs):");
        let devices_map = probe_hw_devices();
        loop {
            let codec = ffi::av_codec_iterate(&mut opaque as *mut _);
            if codec.is_null() {
                break;
            }
            if ffi::av_codec_is_decoder(codec) != 0 {
                dec_count += 1;
                let name = if !(*codec).name.is_null() {
                    CStr::from_ptr((*codec).name).to_string_lossy().into_owned()
                } else {
                    String::from("<unknown>")
                };
                let long_name = if !(*codec).long_name.is_null() {
                    CStr::from_ptr((*codec).long_name)
                        .to_string_lossy()
                        .into_owned()
                } else {
                    String::new()
                };
                let mt = media_type_to_str((*codec).type_);
                let is_video = mt == "video";
                if !only_video || is_video {
                    print!("  - {:<22} {:<40} [{}]", name, long_name, mt);
                }
                let mut i = 0;
                let mut hw_entries: Vec<String> = Vec::new();
                loop {
                    let cfg = ffi::avcodec_get_hw_config(codec, i);
                    if cfg.is_null() {
                        break;
                    }
                    let dtype = (*cfg).device_type;
                    let dname_ptr = ffi::av_hwdevice_get_type_name(dtype);
                    let dname = if !dname_ptr.is_null() {
                        CStr::from_ptr(dname_ptr).to_string_lossy().into_owned()
                    } else {
                        format!("type={}", dtype)
                    };
                    let available = devices_map.get::<str>(&dname).copied().unwrap_or(false);
                    hw_entries.push(format!("{}{}", dname, if available { "(ok)" } else { "" }));
                    i += 1;
                }
                let has_hw = !hw_entries.is_empty();
                if only_hw && !has_hw {
                    continue;
                }
                if only_video && !is_video {
                    continue;
                }
                if has_hw {
                    println!("  [hw: {}]", hw_entries.join(", "));
                } else {
                    println!();
                }
            }
        }
        println!("\nSummary: encoders={}, decoders={}", enc_count, dec_count);
    }
}

#[derive(Serialize)]
/// JSON-friendly codec inventory entry.
pub struct CodecEntry {
    /// FFmpeg codec short name.
    pub name: String,
    /// Human-readable codec name.
    pub long_name: String,
    /// `"encoder"` or `"decoder"`.
    pub kind: String,
    /// Media type such as `"video"` or `"audio"`.
    pub media_type: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    /// Optional decoder hardware-device list.
    pub hw_devices: Option<Vec<String>>,
}

#[derive(Serialize)]
/// Structured summary of CUDA resize probe information.
pub struct CudaResizeProbe {
    /// Whether the linked FFmpeg avfilter build exposes the `scale_cuda` filter.
    pub scale_cuda_filter_available: bool,
}

#[derive(Serialize)]
/// Structured summary of hardware and codec probe information.
pub struct CodecProbeSummary {
    /// FFmpeg version and configuration metadata.
    pub ffmpeg: serde_json::Value,
    /// CUDA resize-specific FFmpeg filter availability.
    pub cuda_resize: CudaResizeProbe,
    /// Hardware-device probe rows.
    pub devices: Vec<HwDeviceEntry>,
    /// Hardware-encoder probe rows.
    pub hw_encoders: Vec<HwEncoderEntry>,
    /// Encoder inventory rows.
    pub encoders: Vec<CodecEntry>,
    /// Decoder inventory rows.
    pub decoders: Vec<CodecEntry>,
}

/// Builds a JSON-serializable probe summary.
///
/// # Examples
///
/// ```rust
/// let summary = direct_play_nice::gpu::gather_probe_json(true, true, true, false);
/// assert!(!summary.devices.is_empty());
/// ```
pub fn gather_probe_json(
    only_video: bool,
    only_hw: bool,
    include_hw: bool,
    include_codecs: bool,
) -> CodecProbeSummary {
    let devices_map = probe_hw_devices();
    let device_entries: Vec<HwDeviceEntry> = devices_map
        .iter()
        .map(|(k, v)| HwDeviceEntry {
            name: k.to_string(),
            available: *v,
        })
        .collect();
    let hw_encs = if include_hw {
        probe_hw_encoders(&devices_map)
    } else {
        Vec::new()
    };
    unsafe {
        let ver = serde_json::json!({
            "avcodec": ffi::avcodec_version(),
            "avformat": ffi::avformat_version(),
            "avutil": ffi::avutil_version(),
            "configuration": CStr::from_ptr(ffi::avcodec_configuration()).to_string_lossy(),
        });
        let mut encoders: Vec<CodecEntry> = Vec::new();
        let mut decoders: Vec<CodecEntry> = Vec::new();
        if include_codecs {
            let mut opaque: *mut c_void = std::ptr::null_mut();
            loop {
                let codec = ffi::av_codec_iterate(&mut opaque as *mut _);
                if codec.is_null() {
                    break;
                }
                if ffi::av_codec_is_encoder(codec) == 0 {
                    continue;
                }
                let name = if !(*codec).name.is_null() {
                    CStr::from_ptr((*codec).name).to_string_lossy().into_owned()
                } else {
                    String::from("<unknown>")
                };
                let long_name = if !(*codec).long_name.is_null() {
                    CStr::from_ptr((*codec).long_name)
                        .to_string_lossy()
                        .into_owned()
                } else {
                    String::new()
                };
                let mt = media_type_to_str((*codec).type_).to_string();
                let is_video = mt == "video";
                let is_hw = name.contains("nvenc")
                    || name.contains("vaapi")
                    || name.contains("qsv")
                    || name.contains("videotoolbox")
                    || name.contains("amf");
                if (only_video && !is_video) || (only_hw && !is_hw) {
                    continue;
                }
                encoders.push(CodecEntry {
                    name,
                    long_name,
                    kind: "encoder".into(),
                    media_type: mt,
                    hw_devices: None,
                });
            }
            opaque = std::ptr::null_mut();
            loop {
                let codec = ffi::av_codec_iterate(&mut opaque as *mut _);
                if codec.is_null() {
                    break;
                }
                if ffi::av_codec_is_decoder(codec) == 0 {
                    continue;
                }
                let name = if !(*codec).name.is_null() {
                    CStr::from_ptr((*codec).name).to_string_lossy().into_owned()
                } else {
                    String::from("<unknown>")
                };
                let long_name = if !(*codec).long_name.is_null() {
                    CStr::from_ptr((*codec).long_name)
                        .to_string_lossy()
                        .into_owned()
                } else {
                    String::new()
                };
                let mt = media_type_to_str((*codec).type_).to_string();
                let is_video = mt == "video";
                let mut i = 0;
                let mut hw_list: Vec<String> = Vec::new();
                loop {
                    let cfg = ffi::avcodec_get_hw_config(codec, i);
                    if cfg.is_null() {
                        break;
                    }
                    let dtype = (*cfg).device_type;
                    let dname_ptr = ffi::av_hwdevice_get_type_name(dtype);
                    let dname = if !dname_ptr.is_null() {
                        CStr::from_ptr(dname_ptr).to_string_lossy().into_owned()
                    } else {
                        format!("type={}", dtype)
                    };
                    let available = devices_map.get::<str>(&dname).copied().unwrap_or(false);
                    hw_list.push(if available {
                        format!("{}(ok)", dname)
                    } else {
                        dname
                    });
                    i += 1;
                }
                let has_hw = !hw_list.is_empty();
                if (only_video && !is_video) || (only_hw && !has_hw) {
                    continue;
                }
                decoders.push(CodecEntry {
                    name,
                    long_name,
                    kind: "decoder".into(),
                    media_type: mt,
                    hw_devices: if has_hw { Some(hw_list) } else { None },
                });
            }
        }
        CodecProbeSummary {
            ffmpeg: ver,
            cuda_resize: CudaResizeProbe {
                scale_cuda_filter_available: scale_cuda_filter_available(),
            },
            devices: device_entries,
            hw_encoders: hw_encs,
            encoders,
            decoders,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn unknown_hw_device_is_not_reported_available() {
        assert!(!linked_ffmpeg_has_hw_device("not-a-real-ffmpeg-hw-device"));
        assert!(!probe_hw_device_in_child_process(
            "not-a-real-ffmpeg-hw-device"
        ));
    }

    #[test]
    fn hw_probe_returns_all_known_device_keys() {
        let devices = probe_hw_devices();
        for key in HW_DEVICE_PROBE_TYPES {
            assert!(devices.contains_key(key));
        }
    }
}
