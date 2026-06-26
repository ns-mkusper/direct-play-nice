//! Configuration subsystem for resolving config sources and deserializing TOML into runtime settings.

use crate::gpu::HwAccel;
use crate::{
    AudioQuality, OcrEngine, OcrFormat, PrimaryVideoCriteria, ResizeBackend, ResizeQuality,
    ServarrLanguageAuditScope, ServarrLanguageCandidatePolicy, SubMode, SubtitleFailurePolicy,
    UnsupportedVideoPolicy, VideoCodecPreference, VideoQuality,
};
use anyhow::{anyhow, Context, Result};
use serde::Deserialize;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

const DEFAULT_DIR_NAME: &str = "direct-play-nice";
const DEFAULT_FILE_NAME: &str = "config.toml";
pub const CONFIG_ENV_VAR: &str = "DIRECT_PLAY_NICE_CONFIG";

#[derive(Debug, Clone, Deserialize, Default)]
#[serde(default)]
pub struct Config {
    pub streaming_devices: Option<StreamingDevicesSetting>,
    pub video_quality: Option<VideoQuality>,
    pub video_codec: Option<VideoCodecPreference>,
    pub audio_quality: Option<AudioQuality>,
    pub max_video_bitrate: Option<String>,
    pub max_audio_bitrate: Option<String>,
    pub resize_quality: Option<ResizeQuality>,
    pub resize_backend: Option<ResizeBackend>,
    pub hw_accel: Option<HwAccel>,
    pub unsupported_video_policy: Option<UnsupportedVideoPolicy>,
    pub primary_video_stream_index: Option<usize>,
    pub primary_video_criteria: Option<PrimaryVideoCriteria>,
    pub servarr_output_extension: Option<String>,
    pub servarr_output_suffix: Option<String>,
    pub servarr_language_audit: Option<bool>,
    pub servarr_language_audit_scope: Option<ServarrLanguageAuditScope>,
    pub servarr_language_audit_lookback_days: Option<u32>,
    pub servarr_language_audit_max_searches: Option<usize>,
    pub servarr_language_audit_no_candidate_cooldown_days: Option<u32>,
    pub servarr_language_audit_episode_ids: Option<String>,
    pub servarr_language_check: Option<bool>,
    pub required_audio_languages: Option<String>,
    pub required_subtitle_languages: Option<String>,
    pub servarr_api_url: Option<String>,
    pub servarr_api_key: Option<String>,
    pub servarr_language_dry_run: Option<bool>,
    pub servarr_untagged_audio_language: Option<String>,
    pub servarr_untagged_subtitle_language: Option<String>,
    pub servarr_language_candidate_policy: Option<ServarrLanguageCandidatePolicy>,
    pub sub_mode: Option<SubMode>,
    pub subtitle_failure_policy: Option<SubtitleFailurePolicy>,
    pub ocr_default_language: Option<String>,
    pub ocr_engine: Option<OcrEngine>,
    pub ocr_format: Option<OcrFormat>,
    pub ocr_external_command: Option<String>,
    pub ocr_write_srt_sidecar: Option<bool>,
    pub skip_codec_check: Option<bool>,
    pub validate_output: Option<bool>,
    pub visual_validate_output: Option<bool>,
    pub visual_quality_report: Option<bool>,
    pub visual_scan_frames: Option<usize>,
    pub visual_sample_interval: Option<usize>,
    pub visual_failure_ratio: Option<f64>,
    pub delete_source: Option<bool>,
    pub plex: Option<PlexSettings>,
}

#[derive(Debug, Clone, Deserialize, Default)]
#[serde(default)]
pub struct PlexSettings {
    pub refresh: Option<bool>,
    pub url: Option<String>,
    pub token: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub enum StreamingDevicesSetting {
    Single(String),
    List(Vec<String>),
}

pub enum ConfigSource {
    Cli(PathBuf),
    Env(PathBuf),
    Default(PathBuf),
}

pub fn load(cli_path: Option<&Path>) -> Result<Option<(Config, ConfigSource)>> {
    if let Some(path) = cli_path {
        let cfg = read_from_path(path)?;
        return Ok(Some((cfg, ConfigSource::Cli(path.to_path_buf()))));
    }

    if let Some(env_path) = env::var_os(CONFIG_ENV_VAR) {
        let path = PathBuf::from(env_path);
        let cfg = read_from_path(&path)?;
        return Ok(Some((cfg, ConfigSource::Env(path))));
    }

    if let Some(path) = resolve_default_path() {
        if path.exists() {
            let cfg = read_from_path(&path)?;
            return Ok(Some((cfg, ConfigSource::Default(path))));
        }
    }

    Ok(None)
}

fn read_from_path(path: &Path) -> Result<Config> {
    if !path.exists() {
        return Err(anyhow!(
            "Configuration file '{}' does not exist.",
            path.display()
        ));
    }
    let contents =
        fs::read_to_string(path).with_context(|| format!("Reading config '{}'", path.display()))?;
    let cfg: Config = toml::from_str(&contents)
        .with_context(|| format!("Parsing config '{}'", path.display()))?;
    Ok(cfg)
}

fn resolve_default_path() -> Option<PathBuf> {
    if let Some(xdg) = env::var_os("XDG_CONFIG_HOME") {
        let mut path = PathBuf::from(xdg);
        path.push(DEFAULT_DIR_NAME);
        path.push(DEFAULT_FILE_NAME);
        return Some(path);
    }

    if let Some(home) = env::var_os("HOME") {
        let mut path = PathBuf::from(home);
        path.push(".config");
        path.push(DEFAULT_DIR_NAME);
        path.push(DEFAULT_FILE_NAME);
        return Some(path);
    }

    None
}

#[cfg(test)]
mod tests {
    use crate::config::*;
    use std::io::Write;
    use std::sync::{Mutex, MutexGuard, OnceLock};
    use tempfile::NamedTempFile;

    fn env_lock() -> MutexGuard<'static, ()> {
        static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        LOCK.get_or_init(|| Mutex::new(())).lock().unwrap()
    }

    #[test]
    fn load_from_cli_path() {
        let mut tmp = NamedTempFile::new().unwrap();
        write!(
            tmp,
            r#"
            [plex]
            refresh = true
            url = "http://localhost:32400"
            token = "abc123"
            "#
        )
        .unwrap();

        let path = tmp.path().to_path_buf();
        let (cfg, source) = load(Some(&path)).unwrap().unwrap();
        assert!(matches!(source, ConfigSource::Cli(_)));
        let plex = cfg.plex.unwrap();
        assert_eq!(plex.refresh, Some(true));
        assert_eq!(plex.url.as_deref(), Some("http://localhost:32400"));
        assert_eq!(plex.token.as_deref(), Some("abc123"));
    }

    #[test]
    fn load_from_env_path() {
        let _guard = env_lock();
        let mut tmp = NamedTempFile::new().unwrap();
        write!(
            tmp,
            r#"
            [plex]
            refresh = false
            token = "env-token"
            "#
        )
        .unwrap();

        let original_cfg = env::var_os(CONFIG_ENV_VAR);
        env::set_var(CONFIG_ENV_VAR, tmp.path());

        let (cfg, source) = load(None).unwrap().unwrap();
        assert!(matches!(source, ConfigSource::Env(_)));
        assert_eq!(cfg.plex.unwrap().token.as_deref(), Some("env-token"));

        match original_cfg {
            Some(val) => env::set_var(CONFIG_ENV_VAR, val),
            None => env::remove_var(CONFIG_ENV_VAR),
        }
    }

    #[test]
    fn absent_config_returns_none() {
        let _guard = env_lock();
        let original_cfg = env::var_os(CONFIG_ENV_VAR);
        let original_xdg = env::var_os("XDG_CONFIG_HOME");
        let original_home = env::var_os("HOME");

        env::remove_var(CONFIG_ENV_VAR);
        env::remove_var("XDG_CONFIG_HOME");
        env::remove_var("HOME");

        let loaded = load(None).unwrap();
        assert!(loaded.is_none());

        match original_cfg {
            Some(val) => env::set_var(CONFIG_ENV_VAR, val),
            None => env::remove_var(CONFIG_ENV_VAR),
        }
        match original_xdg {
            Some(val) => env::set_var("XDG_CONFIG_HOME", val),
            None => env::remove_var("XDG_CONFIG_HOME"),
        }
        match original_home {
            Some(val) => env::set_var("HOME", val),
            None => env::remove_var("HOME"),
        }
    }

    #[test]
    fn parses_subtitle_ocr_settings() {
        let mut tmp = NamedTempFile::new().unwrap();
        write!(
            tmp,
            r#"
            sub_mode = "force"
            ocr_default_language = "spa"
            ocr_engine = "external"
            ocr_format = "ass"
            ocr_external_command = "python3 /opt/ocr/run.py"
            ocr_write_srt_sidecar = true
            "#
        )
        .unwrap();

        let cfg = read_from_path(tmp.path()).unwrap();
        assert_eq!(cfg.sub_mode, Some(SubMode::Force));
        assert_eq!(cfg.ocr_default_language.as_deref(), Some("spa"));
        assert_eq!(cfg.ocr_engine, Some(OcrEngine::External));
        assert_eq!(cfg.ocr_format, Some(OcrFormat::Ass));
        assert_eq!(
            cfg.ocr_external_command.as_deref(),
            Some("python3 /opt/ocr/run.py")
        );
        assert_eq!(cfg.ocr_write_srt_sidecar, Some(true));
    }

    #[test]
    fn parses_servarr_language_check_settings() {
        let mut tmp = NamedTempFile::new().unwrap();
        write!(
            tmp,
            r#"
            servarr_language_audit = true
            servarr_language_audit_scope = "inventory"
            servarr_language_audit_lookback_days = 30
            servarr_language_audit_max_searches = 20
            servarr_language_audit_no_candidate_cooldown_days = 14
            servarr_language_audit_episode_ids = "1,2,3"
            servarr_language_check = true
            required_audio_languages = "eng,jpn"
            required_subtitle_languages = "eng,spa"
            servarr_api_url = "http://localhost:8989"
            servarr_api_key = "secret"
            servarr_language_dry_run = true
            servarr_untagged_audio_language = "eng"
            servarr_untagged_subtitle_language = "eng"
            servarr_language_candidate_policy = "custom-format-or-title"
            "#
        )
        .unwrap();

        let cfg = read_from_path(tmp.path()).unwrap();
        assert_eq!(cfg.servarr_language_audit, Some(true));
        assert_eq!(
            cfg.servarr_language_audit_scope,
            Some(ServarrLanguageAuditScope::Inventory)
        );
        assert_eq!(cfg.servarr_language_audit_lookback_days, Some(30));
        assert_eq!(cfg.servarr_language_audit_max_searches, Some(20));
        assert_eq!(
            cfg.servarr_language_audit_no_candidate_cooldown_days,
            Some(14)
        );
        assert_eq!(
            cfg.servarr_language_audit_episode_ids.as_deref(),
            Some("1,2,3")
        );
        assert_eq!(cfg.servarr_language_check, Some(true));
        assert_eq!(cfg.required_audio_languages.as_deref(), Some("eng,jpn"));
        assert_eq!(cfg.required_subtitle_languages.as_deref(), Some("eng,spa"));
        assert_eq!(
            cfg.servarr_api_url.as_deref(),
            Some("http://localhost:8989")
        );
        assert_eq!(cfg.servarr_api_key.as_deref(), Some("secret"));
        assert_eq!(cfg.servarr_language_dry_run, Some(true));
        assert_eq!(cfg.servarr_untagged_audio_language.as_deref(), Some("eng"));
        assert_eq!(
            cfg.servarr_untagged_subtitle_language.as_deref(),
            Some("eng")
        );
        assert_eq!(
            cfg.servarr_language_candidate_policy,
            Some(ServarrLanguageCandidatePolicy::CustomFormatOrTitle)
        );
    }

    #[test]
    fn parses_resize_settings() {
        let mut tmp = NamedTempFile::new().unwrap();
        write!(
            tmp,
            r#"
            resize_quality = "lanczos"
            resize_backend = "cuda"
            "#
        )
        .unwrap();

        let cfg = read_from_path(tmp.path()).unwrap();
        assert_eq!(cfg.resize_quality, Some(ResizeQuality::Lanczos));
        assert_eq!(cfg.resize_backend, Some(ResizeBackend::Cuda));
    }
}
