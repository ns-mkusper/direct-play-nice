use log::info;
use std::env;

const SONARR_PREFIX: &str = "sonarr_";
const RADARR_PREFIX: &str = "radarr_";

/// Runs the relevant env operation.
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

/// Runs the collect relevant env operation.
pub fn collect_relevant_env(kind: super::servarr::IntegrationKind) -> Vec<(String, String)> {
    relevant_env(kind)
}

/// Runs the log relevant env operation.
pub fn log_relevant_env(kind: super::servarr::IntegrationKind) {
    let entries = relevant_env(kind);
    let header = format!("ServeArr env snapshot ({} entries):", entries.len());
    info!("{}", header);
    for (key, value) in entries {
        let lower = key.to_ascii_lowercase();
        let display_value = if should_redact_env_value(&lower) {
            "<redacted>".to_string()
        } else if lower.ends_with("_path") || lower.contains("_path_") {
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

/// Runs the should redact env value operation.
fn should_redact_env_value(key_lower: &str) -> bool {
    key_lower.contains("token")
        || key_lower.contains("password")
        || key_lower.contains("secret")
        || key_lower.contains("api_key")
        || key_lower.contains("apikey")
}

#[cfg(test)]
mod tests {
    use super::should_redact_env_value;

    #[test]
    /// Runs the redacts sensitive keys operation.
    fn redacts_sensitive_keys() {
        assert!(should_redact_env_value("sonarr_apikey"));
        assert!(should_redact_env_value("radarr_api_key"));
        assert!(should_redact_env_value("sonarr_token"));
        assert!(should_redact_env_value("radarr_password"));
        assert!(should_redact_env_value("radarr_secret_value"));
    }

    #[test]
    /// Runs the keeps non sensitive keys operation.
    fn keeps_non_sensitive_keys() {
        assert!(!should_redact_env_value("sonarr_series_path"));
        assert!(!should_redact_env_value("radarr_movie_title"));
    }
}
