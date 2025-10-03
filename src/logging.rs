use log::info;
use std::env;

const SONARR_PREFIX: &str = "sonarr_";
const RADARR_PREFIX: &str = "radarr_";

fn relevant_env(kind: super::servarr::IntegrationKind) -> Vec<(String, String)> {
    let prefix = match kind {
        super::servarr::IntegrationKind::Sonarr => SONARR_PREFIX,
        super::servarr::IntegrationKind::Radarr => RADARR_PREFIX,
    };

    let mut entries: Vec<(String, String)> = env::vars()
        .filter(|(key, _)| key.to_ascii_lowercase().starts_with(prefix))
        .collect();
    entries.sort_by(|a, b| a.0.cmp(&b.0));
    entries
}

pub fn collect_relevant_env(kind: super::servarr::IntegrationKind) -> Vec<(String, String)> {
    relevant_env(kind)
}

pub fn log_relevant_env(kind: super::servarr::IntegrationKind) {
    let entries = relevant_env(kind);
    let header = format!("ServeArr env snapshot ({} entries):", entries.len());
    info!("{}", header);
    for (key, value) in entries {
        let lower = key.to_ascii_lowercase();
        let display_value = if lower.ends_with("_path") || lower.contains("_path_") {
            value
        } else if value.len() > 200 {
            format!("{}…", &value[..200])
        } else {
            value
        };
        let line = format!("  {} = {}", key, display_value);
        info!("{}", line);
    }
}
