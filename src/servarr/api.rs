//! Minimal Sonarr/Radarr API helpers used by optional language mismatch handling.

use super::env_helpers::get_env_ignore_case;
use super::IntegrationKind;
use anyhow::{anyhow, bail, Context, Result};
use serde_json::json;

#[derive(Debug, Clone, Default)]
pub struct ApiSettings {
    pub url: Option<String>,
    pub api_key: Option<String>,
}

pub fn trigger_redownload_search(kind: IntegrationKind, settings: &ApiSettings) -> Result<()> {
    let base_url = settings
        .url
        .clone()
        .or_else(|| env_url(kind))
        .ok_or_else(|| {
            anyhow!(
                "{} API URL is required to trigger a redownload search",
                kind.label()
            )
        })?;
    let api_key = settings
        .api_key
        .clone()
        .or_else(|| env_api_key(kind))
        .ok_or_else(|| {
            anyhow!(
                "{} API key is required to trigger a redownload search",
                kind.label()
            )
        })?;

    let body = match kind {
        IntegrationKind::Sonarr => {
            let episode_ids = env_int_list(&[
                "sonarr_episodefile_episodeids",
                "sonarr_episodefile_episode_ids",
                "sonarr_episode_ids",
                "sonarr_episode_id",
            ]);
            if episode_ids.is_empty() {
                bail!(
                    "Sonarr language mismatch detected, but no episode id env var was available to trigger EpisodeSearch"
                );
            }
            json!({ "name": "EpisodeSearch", "episodeIds": episode_ids })
        }
        IntegrationKind::Radarr => {
            let movie_ids = env_int_list(&["radarr_movie_id", "radarr_moviefile_movie_id"]);
            if movie_ids.is_empty() {
                bail!(
                    "Radarr language mismatch detected, but no movie id env var was available to trigger MoviesSearch"
                );
            }
            json!({ "name": "MoviesSearch", "movieIds": movie_ids })
        }
    };

    let endpoint = format!("{}/api/v3/command", base_url.trim_end_matches('/'));
    let response = ureq::post(&endpoint)
        .set("X-Api-Key", &api_key)
        .set("Content-Type", "application/json")
        .send_string(&body.to_string())
        .with_context(|| format!("requesting {} redownload search", kind.label()))?;

    if !(200..300).contains(&response.status()) {
        bail!(
            "{} redownload search request failed with HTTP {}",
            kind.label(),
            response.status()
        );
    }

    Ok(())
}

fn env_url(kind: IntegrationKind) -> Option<String> {
    match kind {
        IntegrationKind::Sonarr => {
            first_env(&["DIRECT_PLAY_NICE_SONARR_URL", "SONARR_URL", "sonarr_url"])
        }
        IntegrationKind::Radarr => {
            first_env(&["DIRECT_PLAY_NICE_RADARR_URL", "RADARR_URL", "radarr_url"])
        }
    }
}

fn env_api_key(kind: IntegrationKind) -> Option<String> {
    match kind {
        IntegrationKind::Sonarr => first_env(&[
            "DIRECT_PLAY_NICE_SONARR_API_KEY",
            "SONARR_API_KEY",
            "sonarr_api_key",
            "sonarr_apikey",
        ]),
        IntegrationKind::Radarr => first_env(&[
            "DIRECT_PLAY_NICE_RADARR_API_KEY",
            "RADARR_API_KEY",
            "radarr_api_key",
            "radarr_apikey",
        ]),
    }
}

fn first_env(keys: &[&str]) -> Option<String> {
    keys.iter()
        .filter_map(|key| get_env_ignore_case(key))
        .map(|value| value.trim().to_string())
        .find(|value| !value.is_empty())
}

fn env_int_list(keys: &[&str]) -> Vec<i64> {
    for key in keys {
        if let Some(raw) = get_env_ignore_case(key) {
            let ids = raw
                .split(['|', ',', ';', ' '])
                .filter_map(|part| part.trim().parse::<i64>().ok())
                .collect::<Vec<_>>();
            if !ids.is_empty() {
                return ids;
            }
        }
    }
    Vec::new()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::sync::{Mutex, MutexGuard, OnceLock};

    fn env_lock() -> MutexGuard<'static, ()> {
        static LOCK: OnceLock<Mutex<()>> = OnceLock::new();
        LOCK.get_or_init(|| Mutex::new(())).lock().unwrap()
    }

    #[test]
    fn env_int_list_accepts_common_delimiters() {
        let _guard = env_lock();
        env::set_var("sonarr_episodefile_episodeids", "10|11,12;13 14");
        assert_eq!(
            env_int_list(&["sonarr_episodefile_episodeids"]),
            vec![10, 11, 12, 13, 14]
        );
        env::remove_var("sonarr_episodefile_episodeids");
    }
}
