//! Built-in streaming-device catalog and lookup helpers.

/// Apple TV model definitions.
pub mod apple_tv;
/// Chromecast and Google TV model definitions.
pub mod chromecast;
/// Shared profile types and profile-planning logic.
pub mod device_profile;
/// Amazon Fire TV model definitions.
pub mod fire_tv;
/// Roku model definitions.
pub mod roku;

#[allow(unused_imports)]
pub use device_profile::{
    plan_output_profile, resolve_target_profile, ContainerFormat, DeviceFamily, H264Level,
    H264Profile, InputMediaProfile, PlannedOutputProfile, Resolution, ResolvedTargetProfile,
    StreamingDevice,
};

/// All built-in streaming devices known by this crate.
pub const STREAMING_DEVICES: &[StreamingDevice] = &[
    chromecast::CHROMECAST_1ST_GEN,
    chromecast::CHROMECAST_2ND_GEN,
    chromecast::CHROMECAST_3RD_GEN,
    chromecast::CHROMECAST_ULTRA,
    chromecast::CHROMECAST_GOOGLE_TV,
    chromecast::GOOGLE_TV_STREAMER,
    chromecast::NEST_HUB,
    chromecast::NEST_HUB_MAX,
    roku::ROKU_EXPRESS,
    roku::ROKU_STREAMING_STICK_4K,
    roku::ROKU_ULTRA,
    apple_tv::APPLE_TV_HD,
    apple_tv::APPLE_TV_4K_1ST_GEN,
    apple_tv::APPLE_TV_4K_2ND_GEN,
    apple_tv::APPLE_TV_4K_3RD_GEN,
    fire_tv::FIRE_TV_STICK_4K,
    fire_tv::FIRE_TV_STICK_4K_MAX,
    fire_tv::FIRE_TV_CUBE_3RD_GEN,
];

/// Finds a device by model id, case-insensitively.
///
/// # Examples
///
/// ```rust
/// let maybe_device = direct_play_nice::devices::find_by_model("roku_ultra");
/// assert!(maybe_device.is_some());
/// ```
pub fn find_by_model(model: &str) -> Option<&'static StreamingDevice> {
    STREAMING_DEVICES
        .iter()
        .find(|device| device.model.eq_ignore_ascii_case(model.trim()))
}

/// Returns all devices that belong to `family`.
///
/// # Examples
///
/// ```rust
/// let rokus =
///     direct_play_nice::devices::devices_for_family(direct_play_nice::devices::DeviceFamily::Roku);
/// assert!(!rokus.is_empty());
/// ```
pub fn devices_for_family(family: DeviceFamily) -> Vec<&'static StreamingDevice> {
    STREAMING_DEVICES
        .iter()
        .filter(|device| device.family == family)
        .collect()
}

/// Returns supported model ids for all built-in devices.
pub fn supported_model_ids() -> Vec<&'static str> {
    STREAMING_DEVICES.iter().map(|d| d.model).collect()
}

#[cfg(test)]
mod tests {
    use crate::devices::*;

    #[test]
    /// Executes the resolve all is compatible with every device routine.
    fn resolve_all_is_compatible_with_every_device() {
        let all: Vec<&StreamingDevice> = STREAMING_DEVICES.iter().collect();
        let profile = resolve_target_profile(&all).expect("all profile should resolve");
        for device in STREAMING_DEVICES {
            assert!(
                profile.is_compatible_with_device(device),
                "Resolved all-device profile is not compatible with {}",
                device.model
            );
        }
    }
}
