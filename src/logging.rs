use log::info;
use std::env;

const SONARR_PREFIX: &str = "sonarr_";
const RADARR_PREFIX: &str = "radarr_";

pub fn log_relevant_env(kind: super::servarr::IntegrationKind) {
    let prefix = match kind {
        super::servarr::IntegrationKind::Sonarr => SONARR_PREFIX,
        super::servarr::IntegrationKind::Radarr => RADARR_PREFIX,
    };

    let mut entries: Vec<(String, String)> = env::vars()
        .filter(|(key, _)| key.to_ascii_lowercase().starts_with(prefix))
        .collect();
    entries.sort_by(|a, b| a.0.cmp(&b.0));

    for (key, value) in entries {
        let display_value = if key.to_ascii_lowercase().ends_with("_path")
            || key.to_ascii_lowercase().contains("_path_")
        {
            value
        } else if value.len() > 200 {
            format!("{}…", &value[..200])
        } else {
            value
        };
        info!("ENV {} = {}", key, display_value);
    }
}
