//! Minimal Sonarr/Radarr API helpers used by optional language mismatch handling.

use super::env_helpers::get_env_ignore_case;
use super::language::{normalize_language_tag, LanguageRequirements};
use super::IntegrationKind;
use crate::ServarrLanguageCandidatePolicy;
use anyhow::{anyhow, bail, Context, Result};
use log::{info, warn};
use serde_json::Value;
use std::collections::BTreeSet;
use std::time::Duration;
use ureq::{Agent, AgentBuilder};

#[derive(Debug, Clone, Default)]
pub struct ApiSettings {
    pub url: Option<String>,
    pub api_key: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RedownloadOptions {
    pub dry_run: bool,
    pub candidate_policy: ServarrLanguageCandidatePolicy,
}

impl Default for RedownloadOptions {
    fn default() -> Self {
        Self {
            dry_run: false,
            candidate_policy: ServarrLanguageCandidatePolicy::Strict,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RedownloadOutcome {
    Grabbed { title: String },
    DryRun { summary: String },
    NoVerifiedRelease { reason: String },
}

pub fn trigger_verified_redownload(
    kind: IntegrationKind,
    settings: &ApiSettings,
    requirements: &LanguageRequirements,
    options: RedownloadOptions,
) -> Result<RedownloadOutcome> {
    let client = ApiClient::from_settings(kind, settings)?;
    let target = Target::from_env(kind)?;
    let releases = client.search_releases(&target)?;
    let Some(release) = select_verified_release(&releases, requirements, options.candidate_policy)
    else {
        return Ok(RedownloadOutcome::NoVerifiedRelease {
            reason: format!(
                "{} manual search returned no approved release matching {:?} candidate policy for required audio{} languages",
                kind.label(),
                options.candidate_policy,
                if requirements.subtitles.is_empty() {
                    ""
                } else {
                    "/subtitle"
                }
            ),
        });
    };

    let title = release_title(release).unwrap_or_else(|| "<unknown release>".to_string());
    let evidence = release_match_evidence(release, requirements, options.candidate_policy);
    if options.dry_run {
        return Ok(RedownloadOutcome::DryRun {
            summary: format!(
                "would grab '{}' using {:?} policy ({}) and then blocklist the current history item if identifiable",
                title, options.candidate_policy, evidence
            ),
        });
    }

    let Some(history_id) = client.find_current_history_id(kind)? else {
        bail!(
            "{} found a verified replacement but could not identify the current download history record to blacklist; refusing to grab replacement.",
            kind.label()
        );
    };

    client.grab_release(release)?;
    client.mark_history_failed(history_id)?;
    info!(
        "Marked existing {} download history record {} as failed/blocklisted after grabbing replacement.",
        kind.label(),
        history_id
    );
    Ok(RedownloadOutcome::Grabbed { title })
}

#[derive(Debug, Clone)]
struct ApiClient {
    kind: IntegrationKind,
    base_url: String,
    api_key: String,
    agent: Agent,
}

impl ApiClient {
    fn from_settings(kind: IntegrationKind, settings: &ApiSettings) -> Result<Self> {
        let base_url = settings
            .url
            .clone()
            .or_else(|| env_url(kind))
            .ok_or_else(|| {
                anyhow!(
                    "{} API URL is required to inspect and grab a verified redownload",
                    kind.label()
                )
            })?;
        let api_key = settings
            .api_key
            .clone()
            .or_else(|| env_api_key(kind))
            .ok_or_else(|| {
                anyhow!(
                    "{} API key is required to inspect and grab a verified redownload",
                    kind.label()
                )
            })?;

        Ok(Self {
            kind,
            base_url: base_url.trim_end_matches('/').to_string(),
            api_key,
            agent: AgentBuilder::new()
                .timeout_read(Duration::from_secs(30 * 60))
                .timeout_write(Duration::from_secs(5 * 60))
                .timeout_connect(Duration::from_secs(60))
                .build(),
        })
    }

    fn search_releases(&self, target: &Target) -> Result<Vec<Value>> {
        let endpoint = match target {
            Target::SonarrEpisode { episode_id } => {
                format!("{}/api/v3/release?episodeId={}", self.base_url, episode_id)
            }
            Target::RadarrMovie { movie_id } => {
                format!("{}/api/v3/release?movieId={}", self.base_url, movie_id)
            }
        };
        let response = self
            .get(&endpoint)
            .with_context(|| format!("requesting {} manual release search", self.kind.label()))?;
        response.as_array().cloned().ok_or_else(|| {
            anyhow!(
                "{} release search did not return an array",
                self.kind.label()
            )
        })
    }

    fn grab_release(&self, release: &Value) -> Result<()> {
        let endpoint = format!("{}/api/v3/release", self.base_url);
        self.post_json(&endpoint, release)
            .with_context(|| format!("grabbing selected {} release", self.kind.label()))?;
        Ok(())
    }

    fn mark_history_failed(&self, history_id: i64) -> Result<()> {
        let endpoint = format!("{}/api/v3/history/failed/{}", self.base_url, history_id);
        self.post_json(&endpoint, &Value::Null)
            .with_context(|| format!("marking {} history item failed", self.kind.label()))?;
        Ok(())
    }

    fn find_current_history_id(&self, kind: IntegrationKind) -> Result<Option<i64>> {
        if let Some(id) = explicit_history_id(kind) {
            return Ok(Some(id));
        }
        let Some(download_id) = download_id(kind) else {
            warn!(
                "{} download id env var not available; cannot resolve history record for blocklisting.",
                kind.label()
            );
            return Ok(None);
        };
        let endpoint = format!(
            "{}/api/v3/history?page=1&pageSize=10&sortKey=date&sortDirection=descending&downloadId={}",
            self.base_url,
            url_encode(&download_id)
        );
        let response = self
            .get(&endpoint)
            .with_context(|| format!("looking up {} history by download id", self.kind.label()))?;
        let records = response
            .get("records")
            .and_then(Value::as_array)
            .cloned()
            .unwrap_or_default();
        Ok(records
            .iter()
            .filter_map(|record| record.get("id").and_then(Value::as_i64))
            .next())
    }

    fn get(&self, endpoint: &str) -> Result<Value> {
        let response = self
            .agent
            .get(endpoint)
            .set("X-Api-Key", &self.api_key)
            .call()
            .with_context(|| format!("GET {}", endpoint))?;
        read_json_response(response)
    }

    fn post_json(&self, endpoint: &str, body: &Value) -> Result<Value> {
        let request = self
            .agent
            .post(endpoint)
            .set("X-Api-Key", &self.api_key)
            .set("Content-Type", "application/json");
        let response = if body.is_null() {
            request.call()
        } else {
            request.send_string(&body.to_string())
        }
        .with_context(|| format!("POST {}", endpoint))?;
        read_json_response(response)
    }
}

fn read_json_response(response: ureq::Response) -> Result<Value> {
    if !(200..300).contains(&response.status()) {
        bail!("HTTP request failed with status {}", response.status());
    }
    let text = response.into_string()?;
    if text.trim().is_empty() {
        return Ok(Value::Null);
    }
    serde_json::from_str(&text).context("parsing JSON response")
}

#[derive(Debug, Clone, Copy)]
enum Target {
    SonarrEpisode { episode_id: i64 },
    RadarrMovie { movie_id: i64 },
}

impl Target {
    fn from_env(kind: IntegrationKind) -> Result<Self> {
        match kind {
            IntegrationKind::Sonarr => env_int_list(&[
                "sonarr_episodefile_episodeids",
                "sonarr_episodefile_episode_ids",
                "sonarr_episode_ids",
                "sonarr_episode_id",
            ])
            .into_iter()
            .next()
            .map(|episode_id| Target::SonarrEpisode { episode_id })
            .ok_or_else(|| {
                anyhow!(
                    "Sonarr language mismatch detected, but no episode id env var was available for manual release search"
                )
            }),
            IntegrationKind::Radarr => env_int_list(&["radarr_movie_id", "radarr_moviefile_movie_id"])
                .into_iter()
                .next()
                .map(|movie_id| Target::RadarrMovie { movie_id })
                .ok_or_else(|| {
                    anyhow!(
                        "Radarr language mismatch detected, but no movie id env var was available for manual release search"
                    )
                }),
        }
    }
}

fn select_verified_release<'a>(
    releases: &'a [Value],
    requirements: &LanguageRequirements,
    policy: ServarrLanguageCandidatePolicy,
) -> Option<&'a Value> {
    releases
        .iter()
        .filter(|release| release_is_grabbable(release))
        .filter(|release| release_satisfies_languages(release, requirements, policy))
        .max_by_key(|release| release_score(release))
}

fn release_is_grabbable(release: &Value) -> bool {
    if release.get("rejected").and_then(Value::as_bool) == Some(true) {
        return false;
    }
    if release.get("downloadAllowed").and_then(Value::as_bool) == Some(false) {
        return false;
    }
    true
}

fn release_satisfies_languages(
    release: &Value,
    requirements: &LanguageRequirements,
    policy: ServarrLanguageCandidatePolicy,
) -> bool {
    let audio_ok = release_audio_satisfies(release, requirements, policy);
    let subtitles_ok = release_subtitles_satisfy(release, requirements, policy);
    audio_ok && subtitles_ok
}

fn release_audio_satisfies(
    release: &Value,
    requirements: &LanguageRequirements,
    policy: ServarrLanguageCandidatePolicy,
) -> bool {
    if requirements.audio.is_empty() {
        return true;
    }
    let langs = release_languages(release);
    if requirements.audio.iter().all(|lang| langs.contains(lang)) {
        return true;
    }
    match policy {
        ServarrLanguageCandidatePolicy::Strict => false,
        ServarrLanguageCandidatePolicy::CustomFormat => {
            custom_formats_indicate_audio(release, &requirements.audio)
        }
        ServarrLanguageCandidatePolicy::CustomFormatOrTitle => {
            custom_formats_indicate_audio(release, &requirements.audio)
                || title_indicates_audio(release, &requirements.audio, false)
        }
        ServarrLanguageCandidatePolicy::TitleGuess => {
            custom_formats_indicate_audio(release, &requirements.audio)
                || title_indicates_audio(release, &requirements.audio, true)
        }
    }
}

fn release_subtitles_satisfy(
    release: &Value,
    requirements: &LanguageRequirements,
    policy: ServarrLanguageCandidatePolicy,
) -> bool {
    if requirements.subtitles.is_empty() {
        return true;
    }
    let subtitle_langs = release_subtitle_languages(release);
    if !subtitle_langs.is_empty()
        && requirements
            .subtitles
            .iter()
            .all(|lang| subtitle_langs.contains(lang))
    {
        return true;
    }
    match policy {
        ServarrLanguageCandidatePolicy::Strict => false,
        ServarrLanguageCandidatePolicy::CustomFormat => custom_formats_indicate_subtitles(release),
        ServarrLanguageCandidatePolicy::CustomFormatOrTitle => {
            custom_formats_indicate_subtitles(release) || title_indicates_subtitles(release, false)
        }
        ServarrLanguageCandidatePolicy::TitleGuess => {
            custom_formats_indicate_subtitles(release) || title_indicates_subtitles(release, true)
        }
    }
}

fn release_languages(release: &Value) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    collect_language_values(release.get("languages"), &mut out);
    collect_language_values(release.get("language"), &mut out);
    out
}

fn release_subtitle_languages(release: &Value) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    for key in ["subtitleLanguages", "subtitles", "subtitleLanguage"] {
        collect_language_values(release.get(key), &mut out);
    }
    out
}

fn custom_formats(release: &Value) -> Vec<String> {
    let mut out = Vec::new();
    if let Some(items) = release.get("customFormats").and_then(Value::as_array) {
        for item in items {
            if let Some(name) = item.get("name").and_then(Value::as_str) {
                out.push(name.to_ascii_lowercase());
            } else if let Some(name) = item.as_str() {
                out.push(name.to_ascii_lowercase());
            }
        }
    }
    out
}

fn release_title_lower(release: &Value) -> String {
    release_title(release)
        .unwrap_or_default()
        .to_ascii_lowercase()
        .replace(['.', '_', '-'], " ")
}

fn custom_formats_indicate_audio(release: &Value, required_audio: &[String]) -> bool {
    let formats = custom_formats(release);
    if formats
        .iter()
        .any(|f| f.contains("multi") && (f.contains("audio") || f.contains("dub")))
    {
        return true;
    }
    if required_audio.iter().any(|l| l == "eng")
        && formats
            .iter()
            .any(|f| f.contains("dub") || f.contains("eng"))
    {
        return true;
    }
    false
}

fn custom_formats_indicate_subtitles(release: &Value) -> bool {
    custom_formats(release)
        .iter()
        .any(|f| f.contains("sub") || f.contains("msub"))
}

fn title_indicates_audio(release: &Value, required_audio: &[String], loose: bool) -> bool {
    let title = release_title_lower(release);
    if title.contains("dual audio") || title.contains("multi audio") || title.contains("multi dub")
    {
        return true;
    }
    if required_audio.iter().any(|l| l == "eng")
        && (title.contains("english dub") || title.contains("eng dub"))
    {
        return true;
    }
    if loose {
        return required_audio.iter().any(|lang| match lang.as_str() {
            "eng" => title.contains("eng") || title.contains("dub"),
            "jpn" => title.contains("jpn") || title.contains("jap") || title.contains("japanese"),
            "spa" => title.contains("spa") || title.contains("spanish"),
            "fra" => title.contains("fre") || title.contains("fra") || title.contains("french"),
            _ => title.contains(lang),
        });
    }
    false
}

fn title_indicates_subtitles(release: &Value, loose: bool) -> bool {
    let title = release_title_lower(release);
    title.contains("multi subs")
        || title.contains("multi sub")
        || title.contains("msubs")
        || title.contains("multi subtitles")
        || (loose && (title.contains("sub") || title.contains("subs")))
}

fn release_match_evidence(
    release: &Value,
    requirements: &LanguageRequirements,
    policy: ServarrLanguageCandidatePolicy,
) -> String {
    let mut parts = Vec::new();
    let langs = release_languages(release);
    if !langs.is_empty() {
        parts.push(format!(
            "release languages [{}]",
            langs.into_iter().collect::<Vec<_>>().join(", ")
        ));
    }
    let subtitle_langs = release_subtitle_languages(release);
    if !subtitle_langs.is_empty() {
        parts.push(format!(
            "subtitle languages [{}]",
            subtitle_langs.into_iter().collect::<Vec<_>>().join(", ")
        ));
    }
    let formats = custom_formats(release);
    if !formats.is_empty() {
        parts.push(format!("custom formats [{}]", formats.join(", ")));
    }
    if policy != ServarrLanguageCandidatePolicy::Strict {
        if custom_formats_indicate_audio(release, &requirements.audio) {
            parts.push("custom-format audio hint".to_string());
        }
        if custom_formats_indicate_subtitles(release) {
            parts.push("custom-format subtitle hint".to_string());
        }
        if title_indicates_audio(
            release,
            &requirements.audio,
            policy == ServarrLanguageCandidatePolicy::TitleGuess,
        ) {
            parts.push("title audio hint".to_string());
        }
        if title_indicates_subtitles(
            release,
            policy == ServarrLanguageCandidatePolicy::TitleGuess,
        ) {
            parts.push("title subtitle hint".to_string());
        }
    }
    if parts.is_empty() {
        "no language evidence".to_string()
    } else {
        parts.join("; ")
    }
}

fn collect_language_values(value: Option<&Value>, out: &mut BTreeSet<String>) {
    match value {
        Some(Value::Array(items)) => {
            for item in items {
                collect_language_values(Some(item), out);
            }
        }
        Some(Value::Object(map)) => {
            for key in [
                "name",
                "nameLower",
                "language",
                "isoCode",
                "iso6392",
                "code",
            ] {
                if let Some(lang) = map
                    .get(key)
                    .and_then(Value::as_str)
                    .and_then(normalize_language_tag)
                {
                    out.insert(lang);
                }
            }
        }
        Some(Value::String(raw)) => {
            if let Some(lang) = normalize_language_tag(raw) {
                out.insert(lang);
            }
        }
        _ => {}
    }
}

fn release_score(release: &Value) -> i64 {
    release
        .get("customFormatScore")
        .and_then(Value::as_i64)
        .unwrap_or(0)
        + release
            .get("qualityWeight")
            .and_then(Value::as_i64)
            .unwrap_or(0)
}

fn release_title(release: &Value) -> Option<String> {
    release
        .get("title")
        .or_else(|| release.get("releaseTitle"))
        .and_then(Value::as_str)
        .map(ToOwned::to_owned)
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

fn explicit_history_id(kind: IntegrationKind) -> Option<i64> {
    match kind {
        IntegrationKind::Sonarr => first_env(&[
            "DIRECT_PLAY_NICE_SONARR_HISTORY_ID",
            "sonarr_history_id",
            "sonarr_download_history_id",
        ]),
        IntegrationKind::Radarr => first_env(&[
            "DIRECT_PLAY_NICE_RADARR_HISTORY_ID",
            "radarr_history_id",
            "radarr_download_history_id",
        ]),
    }
    .and_then(|raw| raw.parse().ok())
}

fn download_id(kind: IntegrationKind) -> Option<String> {
    match kind {
        IntegrationKind::Sonarr => first_env(&["sonarr_download_id", "sonarr_downloadid"]),
        IntegrationKind::Radarr => first_env(&["radarr_download_id", "radarr_downloadid"]),
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

fn url_encode(input: &str) -> String {
    input
        .bytes()
        .flat_map(|byte| match byte {
            b'A'..=b'Z' | b'a'..=b'z' | b'0'..=b'9' | b'-' | b'_' | b'.' | b'~' => {
                vec![byte as char]
            }
            _ => format!("%{byte:02X}").chars().collect(),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockito::Matcher;
    use serde_json::json;
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

    #[test]
    fn sonarr_verified_redownload_grabs_specific_release_then_blocklists_old_history() {
        let _guard = env_lock();
        env::set_var("sonarr_episode_id", "77");
        env::set_var("DIRECT_PLAY_NICE_SONARR_HISTORY_ID", "1234");

        let mut server = mockito::Server::new();
        let release = json!({
            "title": "verified replacement",
            "guid": "abc",
            "indexerId": 1,
            "rejected": false,
            "languages": [{"name": "English"}]
        });
        let search = server
            .mock("GET", "/api/v3/release")
            .match_query(Matcher::UrlEncoded("episodeId".into(), "77".into()))
            .with_status(200)
            .with_body(json!([release]).to_string())
            .create();
        let grab = server
            .mock("POST", "/api/v3/release")
            .match_header("x-api-key", "test-key")
            .with_status(200)
            .with_body("{}")
            .create();
        let blocklist = server
            .mock("POST", "/api/v3/history/failed/1234")
            .match_header("x-api-key", "test-key")
            .with_status(200)
            .with_body("{}")
            .create();

        let outcome = trigger_verified_redownload(
            IntegrationKind::Sonarr,
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &LanguageRequirements {
                enabled: true,
                audio: vec!["eng".to_string()],
                subtitles: Vec::new(),
            },
            RedownloadOptions::default(),
        )
        .unwrap();

        assert_eq!(
            outcome,
            RedownloadOutcome::Grabbed {
                title: "verified replacement".to_string()
            }
        );
        search.assert();
        grab.assert();
        blocklist.assert();

        env::remove_var("sonarr_episode_id");
        env::remove_var("DIRECT_PLAY_NICE_SONARR_HISTORY_ID");
    }

    #[test]
    fn does_not_grab_or_blocklist_when_no_verified_release_exists() {
        let _guard = env_lock();
        env::set_var("radarr_movie_id", "88");
        env::set_var("DIRECT_PLAY_NICE_RADARR_HISTORY_ID", "4321");

        let mut server = mockito::Server::new();
        let search = server
            .mock("GET", "/api/v3/release")
            .match_query(Matcher::UrlEncoded("movieId".into(), "88".into()))
            .with_status(200)
            .with_body(
                json!([{
                    "title": "wrong language",
                    "rejected": false,
                    "languages": [{"name": "French"}]
                }])
                .to_string(),
            )
            .create();

        let outcome = trigger_verified_redownload(
            IntegrationKind::Radarr,
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &LanguageRequirements {
                enabled: true,
                audio: vec!["eng".to_string()],
                subtitles: Vec::new(),
            },
            RedownloadOptions::default(),
        )
        .unwrap();

        assert!(matches!(
            outcome,
            RedownloadOutcome::NoVerifiedRelease { .. }
        ));
        search.assert();

        env::remove_var("radarr_movie_id");
        env::remove_var("DIRECT_PLAY_NICE_RADARR_HISTORY_ID");
    }

    #[test]
    fn refuses_to_grab_when_old_history_cannot_be_identified() {
        let _guard = env_lock();
        env::set_var("sonarr_episode_id", "77");
        env::remove_var("DIRECT_PLAY_NICE_SONARR_HISTORY_ID");
        env::remove_var("sonarr_download_id");

        let mut server = mockito::Server::new();
        let search = server
            .mock("GET", "/api/v3/release")
            .match_query(Matcher::UrlEncoded("episodeId".into(), "77".into()))
            .with_status(200)
            .with_body(
                json!([{
                    "title": "verified replacement",
                    "rejected": false,
                    "languages": [{"name": "English"}]
                }])
                .to_string(),
            )
            .create();

        let err = trigger_verified_redownload(
            IntegrationKind::Sonarr,
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &LanguageRequirements {
                enabled: true,
                audio: vec!["eng".to_string()],
                subtitles: Vec::new(),
            },
            RedownloadOptions::default(),
        )
        .unwrap_err();

        assert!(err
            .to_string()
            .contains("could not identify the current download history record"));
        search.assert();

        env::remove_var("sonarr_episode_id");
    }

    #[test]
    fn dry_run_does_not_grab_or_blocklist() {
        let _guard = env_lock();
        env::set_var("sonarr_episode_id", "77");
        env::set_var("DIRECT_PLAY_NICE_SONARR_HISTORY_ID", "1234");

        let mut server = mockito::Server::new();
        let search = server
            .mock("GET", "/api/v3/release")
            .match_query(Matcher::UrlEncoded("episodeId".into(), "77".into()))
            .with_status(200)
            .with_body(
                json!([{
                    "title":"dry run candidate",
                    "rejected": false,
                    "languages":[{"name":"English"}]
                }])
                .to_string(),
            )
            .create();

        let outcome = trigger_verified_redownload(
            IntegrationKind::Sonarr,
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &LanguageRequirements {
                enabled: true,
                audio: vec!["eng".to_string()],
                subtitles: Vec::new(),
            },
            RedownloadOptions {
                dry_run: true,
                candidate_policy: ServarrLanguageCandidatePolicy::Strict,
            },
        )
        .unwrap();

        assert!(matches!(outcome, RedownloadOutcome::DryRun { .. }));
        search.assert();

        env::remove_var("sonarr_episode_id");
        env::remove_var("DIRECT_PLAY_NICE_SONARR_HISTORY_ID");
    }

    #[test]
    fn custom_format_policy_accepts_multi_audio_and_multi_sub_hints() {
        let req = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string(), "jpn".to_string()],
            subtitles: vec!["eng".to_string()],
        };
        let releases = vec![json!({
            "title":"candidate",
            "rejected": false,
            "customFormats":[{"name":"Anime-multi-audio"}, {"name":"anime-multi-sub"}],
            "customFormatScore": 600
        })];

        assert!(select_verified_release(
            &releases,
            &req,
            ServarrLanguageCandidatePolicy::CustomFormat
        )
        .is_some());
    }

    #[test]
    fn title_policy_accepts_dual_audio_multi_sub_tokens() {
        let req = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string(), "jpn".to_string()],
            subtitles: vec!["eng".to_string()],
        };
        let releases = vec![json!({
            "title":"Show S01E01 1080p Dual-Audio Multi-Subs",
            "rejected": false
        })];

        assert!(select_verified_release(
            &releases,
            &req,
            ServarrLanguageCandidatePolicy::CustomFormatOrTitle
        )
        .is_some());
    }

    #[test]
    fn selects_only_grabbable_release_with_required_audio_language() {
        let req = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string()],
            subtitles: Vec::new(),
        };
        let releases = vec![
            json!({"title":"bad", "rejected": true, "languages":[{"name":"English"}], "customFormatScore": 100}),
            json!({"title":"wrong language", "rejected": false, "languages":[{"name":"French"}], "customFormatScore": 200}),
            json!({"title":"good", "rejected": false, "languages":[{"name":"English"}], "customFormatScore": 10}),
        ];

        let selected =
            select_verified_release(&releases, &req, ServarrLanguageCandidatePolicy::Strict)
                .unwrap();
        assert_eq!(release_title(selected).as_deref(), Some("good"));
    }

    #[test]
    fn rejects_subtitle_requirement_without_verified_subtitle_metadata() {
        let req = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string()],
            subtitles: vec!["spa".to_string()],
        };
        let releases = vec![json!({"title":"ambiguous", "languages":[{"name":"English"}]})];
        assert!(
            select_verified_release(&releases, &req, ServarrLanguageCandidatePolicy::Strict)
                .is_none()
        );
    }

    #[test]
    fn accepts_subtitle_requirement_when_explicit_metadata_exists() {
        let req = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string()],
            subtitles: vec!["spa".to_string()],
        };
        let releases = vec![json!({
            "title":"explicit subtitles",
            "languages":[{"name":"English"}],
            "subtitleLanguages":[{"name":"Spanish"}]
        })];
        assert!(
            select_verified_release(&releases, &req, ServarrLanguageCandidatePolicy::Strict)
                .is_some()
        );
    }
}
