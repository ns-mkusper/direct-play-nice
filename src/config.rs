use anyhow::{anyhow, Context, Result};
use serde::Deserialize;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

const DEFAULT_DIR_NAME: &str = "direct-play-nice";
const DEFAULT_FILE_NAME: &str = "config.toml";
pub const CONFIG_ENV_VAR: &str = "DIRECT_PLAY_NICE_CONFIG";

#[derive(Debug, Clone, Deserialize, Default)]
pub struct Config {
    #[serde(default)]
    pub plex: Option<PlexSettings>,
}

#[derive(Debug, Clone, Deserialize, Default)]
pub struct PlexSettings {
    pub refresh: Option<bool>,
    pub url: Option<String>,
    pub token: Option<String>,
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
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

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
}
