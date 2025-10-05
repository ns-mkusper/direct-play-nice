use anyhow::{anyhow, Context, Result};
use log::{info, warn};
use std::collections::BTreeMap;
use std::collections::BTreeSet;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub struct FileConfig {
    pub path: PathBuf,
    pub values: BTreeMap<String, String>,
}

impl FileConfig {
    pub fn get(&self, key: &str) -> Option<&str> {
        self.values.get(key).map(|s| s.as_str())
    }
}

pub fn load_config(path_override: Option<&Path>) -> Result<Option<FileConfig>> {
    let mut candidates = Vec::new();

    if let Some(path) = path_override {
        candidates.push(path.to_path_buf());
    } else {
        if let Some(env_path) = env::var_os("DIRECT_PLAY_NICE_CONFIG")
            .filter(|value| !value.is_empty())
        {
            candidates.push(PathBuf::from(env_path));
        }
        candidates.extend(default_config_candidates());
    }

    for candidate in candidates {
        if candidate.as_os_str().is_empty() || !candidate.exists() {
            continue;
        }

        let contents = fs::read_to_string(&candidate).with_context(|| {
            format!(
                "Failed to read configuration file at {}",
                candidate.display()
            )
        })?;

        let values = parse_kv_config(&contents, &candidate)?;
        info!("Using configuration from {}", candidate.display());

        return Ok(Some(FileConfig {
            path: candidate,
            values,
        }));
    }

    Ok(None)
}

pub fn default_config_path() -> Option<PathBuf> {
    let candidates = default_config_candidates();
    for path in &candidates {
        if path.exists() {
            return Some(path.clone());
        }
    }
    candidates.into_iter().next()
}

fn default_config_candidates() -> Vec<PathBuf> {
    let mut seen = BTreeSet::new();
    let mut out = Vec::new();

    let mut push_unique = |path: PathBuf, out: &mut Vec<PathBuf>| {
        if !path.as_os_str().is_empty() && seen.insert(path.clone()) {
            out.push(path);
        }
    };

    if let Some(xdg_config) = env::var_os("XDG_CONFIG_HOME").filter(|val| !val.is_empty()) {
        let mut path = PathBuf::from(xdg_config);
        path.push("direct-play-nice");
        path.push("config.toml");
        push_unique(path, &mut out);
    }

    if let Some(home) = detect_home_dir() {
        let mut path = home.join(".config");
        path.push("direct-play-nice");
        path.push("config.toml");
        push_unique(path, &mut out);

        push_unique(home.join("direct-play-nice.toml"), &mut out);

        let mut nested = home.join("direct-play-nice");
        nested.push("config.toml");
        push_unique(nested, &mut out);
    }

    if let Some(home_env) = env::var_os("HOME").filter(|val| !val.is_empty()) {
        let mut path = PathBuf::from(home_env);
        path.push(".config");
        path.push("direct-play-nice");
        path.push("config.toml");
        push_unique(path, &mut out);
    }

    if let Ok(current_dir) = env::current_dir() {
        push_unique(current_dir.join("direct-play-nice.toml"), &mut out);
        push_unique(
            current_dir.join("direct-play-nice").join("config.toml"),
            &mut out,
        );
    }

    if let Ok(exe_path) = env::current_exe() {
        if let Some(parent) = exe_path.parent() {
            push_unique(parent.join("direct-play-nice.toml"), &mut out);
            push_unique(parent.join("direct-play-nice").join("config.toml"), &mut out);
        }
    }

    push_unique(PathBuf::from("/etc/direct-play-nice/config.toml"), &mut out);
    push_unique(PathBuf::from("/usr/local/etc/direct-play-nice/config.toml"), &mut out);

    out
}

fn detect_home_dir() -> Option<PathBuf> {
    if let Some(home) = env::var_os("HOME").filter(|val| !val.is_empty()) {
        return Some(PathBuf::from(home));
    }

    #[cfg(unix)]
    {
        use std::ffi::CStr;

        unsafe {
            let uid = libc::getuid();
            let pwd = libc::getpwuid(uid);
            if pwd.is_null() {
                return None;
            }
            let dir_ptr = (*pwd).pw_dir;
            if dir_ptr.is_null() {
                return None;
            }
            if let Ok(path_str) = CStr::from_ptr(dir_ptr).to_str() {
                if !path_str.is_empty() {
                    return Some(PathBuf::from(path_str));
                }
            }
        }
    }

    None
}

fn parse_kv_config(contents: &str, path: &Path) -> Result<BTreeMap<String, String>> {
    let mut values = BTreeMap::new();

    for (idx, raw_line) in contents.lines().enumerate() {
        let line_no = idx + 1;
        let trimmed = raw_line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') || trimmed.starts_with(";") {
            continue;
        }

        let without_inline_comment = match trimmed.split_once('#') {
            Some((prefix, _)) => prefix.trim_end(),
            None => trimmed,
        };

        let without_comment = match without_inline_comment.split_once(';') {
            Some((prefix, _)) => prefix.trim_end(),
            None => without_inline_comment,
        };

        let (key_part, value_part) = without_comment.split_once('=')
            .ok_or_else(|| {
            anyhow!(
                "Invalid config line {}:{}: expected 'key = value' format",
                path.display(),
                line_no
            )
        })?;

        let key = key_part.trim();
        if key.is_empty() {
            return Err(anyhow!(
                "Invalid config line {}:{}: key cannot be empty",
                path.display(),
                line_no
            ));
        }

        let mut value = value_part.trim();
        if value.is_empty() {
            return Err(anyhow!(
                "Invalid config line {}:{}: value cannot be empty",
                path.display(),
                line_no
            ));
        }

        if value.len() >= 2 {
            let bytes = value.as_bytes();
            let first = bytes[0];
            let last = bytes[bytes.len() - 1];
            if (first == b'"' && last == b'"') || (first == b'\'' && last == b'\'') {
                value = &value[1..value.len() - 1];
            }
        }
        let key_lower = key.to_ascii_lowercase();
        if values.insert(key_lower.clone(), value.to_string()).is_some() {
            warn!(
                "Duplicate key '{}' encountered in config {}; using last value",
                key_lower,
                path.display()
            );
        }
    }

    Ok(values)
}
