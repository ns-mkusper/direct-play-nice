//! Minimal Sonarr/Radarr API helpers used by optional language mismatch handling.

use super::cache;
use super::env_helpers::get_env_ignore_case;
use super::language::{
    normalize_language_tag, parse_language_tokens, report_from_present, LanguageCheckReport,
    LanguageRequirements, UntaggedRetagOptions,
};
use super::IntegrationKind;
use crate::{ServarrLanguageAuditScope, ServarrLanguageCandidatePolicy};
use anyhow::{anyhow, bail, Context, Result};
use log::{info, warn};
use serde_json::Value;
use std::collections::BTreeSet;
use std::path::PathBuf;
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
pub struct AuditOptions {
    pub scope: ServarrLanguageAuditScope,
    pub lookback_days: u32,
    pub max_searches: usize,
    pub episode_ids: Vec<i64>,
    pub untagged_retag: UntaggedRetagOptions,
}

impl Default for AuditOptions {
    fn default() -> Self {
        Self {
            scope: ServarrLanguageAuditScope::History,
            lookback_days: 30,
            max_searches: 20,
            episode_ids: Vec::new(),
            untagged_retag: UntaggedRetagOptions::default(),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct AuditSummary {
    pub checked: usize,
    pub missing: usize,
    pub searched: usize,
    pub grabbed: usize,
    pub dry_run: usize,
    pub no_candidate: usize,
    pub errors: usize,
}

impl AuditSummary {
    fn merge(&mut self, other: Self) {
        self.checked += other.checked;
        self.missing += other.missing;
        self.searched += other.searched;
        self.grabbed += other.grabbed;
        self.dry_run += other.dry_run;
        self.no_candidate += other.no_candidate;
        self.errors += other.errors;
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RedownloadOutcome {
    Grabbed { title: String },
    DryRun { summary: String },
    NoVerifiedRelease { reason: String },
}

pub fn run_language_audit(
    kind: IntegrationKind,
    settings: &ApiSettings,
    requirements: &LanguageRequirements,
    redownload_options: RedownloadOptions,
    audit_options: AuditOptions,
) -> Result<AuditSummary> {
    match kind {
        IntegrationKind::Sonarr => {
            run_sonarr_language_audit(settings, requirements, redownload_options, audit_options)
        }
        IntegrationKind::Radarr => {
            run_radarr_language_audit(settings, requirements, redownload_options, audit_options)
        }
    }
}

pub fn run_sonarr_language_audit(
    settings: &ApiSettings,
    requirements: &LanguageRequirements,
    redownload_options: RedownloadOptions,
    audit_options: AuditOptions,
) -> Result<AuditSummary> {
    let client = ApiClient::from_settings(IntegrationKind::Sonarr, settings)?;
    let mut summary =
        client.sonarr_force_import_pending_language_upgrades(requirements, redownload_options)?;
    let audit_summary = match audit_options.scope {
        ServarrLanguageAuditScope::History => run_sonarr_history_language_audit(
            &client,
            requirements,
            redownload_options,
            audit_options,
        )?,
        ServarrLanguageAuditScope::Inventory => run_sonarr_inventory_language_audit(
            &client,
            requirements,
            redownload_options,
            audit_options,
        )?,
    };
    summary.merge(audit_summary);
    log_audit_summary("Sonarr", &summary);
    Ok(summary)
}

fn run_sonarr_history_language_audit(
    client: &ApiClient,
    requirements: &LanguageRequirements,
    redownload_options: RedownloadOptions,
    audit_options: AuditOptions,
) -> Result<AuditSummary> {
    let records = client.sonarr_recent_import_history(audit_options.lookback_days)?;
    let mut summary = AuditSummary::default();
    let mut searched = 0usize;

    for record in records {
        let Some(episode_id) = history_episode_id(&record) else {
            continue;
        };
        if !audit_options.episode_ids.is_empty() && !audit_options.episode_ids.contains(&episode_id)
        {
            continue;
        }
        let Some(history_id) = record.get("id").and_then(Value::as_i64) else {
            continue;
        };
        let Some(episode_file) = client.sonarr_current_episode_file(episode_id, &mut summary)
        else {
            continue;
        };
        process_sonarr_language_audit_item(
            client,
            episode_id,
            Some(history_id),
            episode_file,
            requirements,
            redownload_options,
            audit_options.max_searches,
            &audit_options.untagged_retag,
            &mut searched,
            &mut summary,
        );
    }

    Ok(summary)
}

fn run_sonarr_inventory_language_audit(
    client: &ApiClient,
    requirements: &LanguageRequirements,
    redownload_options: RedownloadOptions,
    audit_options: AuditOptions,
) -> Result<AuditSummary> {
    let items = if audit_options.episode_ids.is_empty() {
        client.sonarr_inventory_episode_files()?
    } else {
        client.sonarr_inventory_episode_files_for_ids(&audit_options.episode_ids)?
    };
    let mut summary = AuditSummary::default();
    let mut searched = 0usize;

    for (episode_id, episode_file) in items {
        if searched >= audit_options.max_searches {
            break;
        }
        process_sonarr_language_audit_item(
            client,
            episode_id,
            None,
            episode_file,
            requirements,
            redownload_options,
            audit_options.max_searches,
            &audit_options.untagged_retag,
            &mut searched,
            &mut summary,
        );
    }

    Ok(summary)
}

#[allow(clippy::too_many_arguments)]
fn process_sonarr_language_audit_item(
    client: &ApiClient,
    episode_id: i64,
    history_id: Option<i64>,
    episode_file: Value,
    requirements: &LanguageRequirements,
    redownload_options: RedownloadOptions,
    max_searches: usize,
    untagged_retag: &UntaggedRetagOptions,
    searched: &mut usize,
    summary: &mut AuditSummary,
) {
    summary.checked += 1;
    let mut report = language_report_from_episode_file(&episode_file, requirements);
    if !report.satisfied() && client.kind == IntegrationKind::Sonarr && untagged_retag.enabled() {
        report = maybe_retag_audit_file(
            IntegrationKind::Sonarr,
            &episode_file,
            requirements,
            untagged_retag,
            report,
        );
    }
    record_language_audit_assessment(
        IntegrationKind::Sonarr,
        "sonarr:episodefile",
        &episode_file,
        requirements,
        &report,
    );
    if report.satisfied() {
        return;
    }
    summary.missing += 1;
    if *searched >= max_searches {
        return;
    }
    *searched += 1;
    summary.searched += 1;

    let history_id = match history_id {
        Some(id) => Some(id),
        None => match client.sonarr_latest_import_history_id(episode_id) {
            Ok(id) => id,
            Err(err) => {
                summary.errors += 1;
                warn!(
                    "Sonarr inventory language audit could not fetch import history for episode {}: {}",
                    episode_id, err
                );
                return;
            }
        },
    };
    let Some(history_id) = history_id else {
        if redownload_options.dry_run {
            client.apply_audit_redownload_outcome(
                Target::SonarrEpisode { episode_id },
                0,
                requirements,
                redownload_options,
                summary,
            );
        } else {
            summary.errors += 1;
            warn!(
                "Sonarr inventory language audit could not identify import history for episode {}; refusing to grab replacement.",
                episode_id
            );
        }
        return;
    };

    client.apply_audit_redownload_outcome(
        Target::SonarrEpisode { episode_id },
        history_id,
        requirements,
        redownload_options,
        summary,
    );
}

pub fn run_radarr_language_audit(
    settings: &ApiSettings,
    requirements: &LanguageRequirements,
    redownload_options: RedownloadOptions,
    audit_options: AuditOptions,
) -> Result<AuditSummary> {
    let client = ApiClient::from_settings(IntegrationKind::Radarr, settings)?;
    let mut summary =
        client.radarr_force_import_pending_language_upgrades(requirements, redownload_options)?;
    let records = client.radarr_recent_import_history(audit_options.lookback_days)?;
    let mut searched = 0usize;

    for record in records {
        let Some(movie_id) = record.get("movieId").and_then(Value::as_i64) else {
            continue;
        };
        let Some(history_id) = record.get("id").and_then(Value::as_i64) else {
            continue;
        };
        let Some(movie_file) = client.radarr_current_movie_file(movie_id, &mut summary) else {
            continue;
        };
        summary.checked += 1;
        let report = language_report_from_episode_file(&movie_file, requirements);
        record_language_audit_assessment(
            IntegrationKind::Radarr,
            "radarr:moviefile",
            &movie_file,
            requirements,
            &report,
        );
        if report.satisfied() {
            continue;
        }
        summary.missing += 1;
        if searched >= audit_options.max_searches {
            continue;
        }
        searched += 1;
        summary.searched += 1;
        client.apply_audit_redownload_outcome(
            Target::RadarrMovie { movie_id },
            history_id,
            requirements,
            redownload_options,
            &mut summary,
        );
    }

    log_audit_summary("Radarr", &summary);
    Ok(summary)
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
    let Some(release) = select_verified_release_for_target(
        &releases,
        target,
        requirements,
        options.candidate_policy,
    ) else {
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
                "would grab '{}' using {:?} policy ({}) and then blocklist the current history item if identifiable{}",
                title,
                options.candidate_policy,
                evidence,
                if release_rejections_are_only_existing_file_cutoff(release) {
                    "; accepting Arr's existing-file/cutoff rejection as a language-upgrade override"
                } else {
                    ""
                }
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

    fn redownload_for_target(
        &self,
        target: Target,
        history_id: i64,
        requirements: &LanguageRequirements,
        options: RedownloadOptions,
    ) -> Result<RedownloadOutcome> {
        let releases = self.search_releases(&target)?;
        let Some(release) = select_verified_release_for_target(
            &releases,
            target,
            requirements,
            options.candidate_policy,
        ) else {
            return Ok(RedownloadOutcome::NoVerifiedRelease {
                reason: format!(
                    "{} manual search returned no approved release matching {:?} candidate policy for required audio{} languages",
                    self.kind.label(),
                    options.candidate_policy,
                    if requirements.subtitles.is_empty() { "" } else { "/subtitle" }
                ),
            });
        };
        let title = release_title(release).unwrap_or_else(|| "<unknown release>".to_string());
        let evidence = release_match_evidence(release, requirements, options.candidate_policy);
        if options.dry_run {
            return Ok(RedownloadOutcome::DryRun {
                summary: format!(
                    "would grab '{}' using {:?} policy ({}) and then blocklist history item {}{}",
                    title,
                    options.candidate_policy,
                    evidence,
                    history_id,
                    if release_rejections_are_only_existing_file_cutoff(release) {
                        "; accepting Arr's existing-file/cutoff rejection as a language-upgrade override"
                    } else {
                        ""
                    }
                ),
            });
        }
        self.grab_release(release)?;
        self.mark_history_failed(history_id)?;
        Ok(RedownloadOutcome::Grabbed { title })
    }

    fn sonarr_recent_import_history(&self, lookback_days: u32) -> Result<Vec<Value>> {
        let mut out = Vec::new();
        let min_day = current_day_number().saturating_sub(lookback_days as i64);
        for page in 1..=10 {
            let endpoint = format!(
                "{}/api/v3/history?page={page}&pageSize=100&sortKey=date&sortDirection=descending&includeSeries=true&includeEpisode=true",
                self.base_url
            );
            let response = self.get(&endpoint)?;
            let records = response
                .get("records")
                .and_then(Value::as_array)
                .cloned()
                .unwrap_or_default();
            if records.is_empty() {
                break;
            }
            let mut saw_old = false;
            for record in records {
                if let Some(day) = record
                    .get("date")
                    .and_then(Value::as_str)
                    .and_then(iso_day_number)
                {
                    if day < min_day {
                        saw_old = true;
                        continue;
                    }
                }
                if record.get("eventType").and_then(Value::as_str) == Some("downloadFolderImported")
                {
                    out.push(record);
                }
            }
            if saw_old {
                break;
            }
        }
        Ok(out)
    }

    fn sonarr_inventory_episode_files_for_ids(
        &self,
        episode_ids: &[i64],
    ) -> Result<Vec<(i64, Value)>> {
        let mut out = Vec::new();
        for &episode_id in episode_ids {
            let episode = self.sonarr_episode(episode_id)?;
            let Some(file_id) = episode.get("episodeFileId").and_then(Value::as_i64) else {
                continue;
            };
            let file = self.sonarr_episode_file(file_id)?;
            out.push((episode_id, file));
        }
        Ok(out)
    }

    fn sonarr_inventory_episode_files(&self) -> Result<Vec<(i64, Value)>> {
        let series = self
            .get(&format!("{}/api/v3/series", self.base_url))?
            .as_array()
            .cloned()
            .ok_or_else(|| anyhow!("Sonarr series endpoint did not return an array"))?;
        let mut out = Vec::new();
        for item in series {
            let Some(series_id) = item.get("id").and_then(Value::as_i64) else {
                continue;
            };
            let endpoint = format!(
                "{}/api/v3/episode?seriesId={}&includeEpisodeFile=true",
                self.base_url, series_id
            );
            let episodes = self
                .get(&endpoint)?
                .as_array()
                .cloned()
                .ok_or_else(|| anyhow!("Sonarr episode endpoint did not return an array"))?;
            for episode in episodes {
                if episode.get("hasFile").and_then(Value::as_bool) == Some(false) {
                    continue;
                }
                let Some(episode_id) = episode.get("id").and_then(Value::as_i64) else {
                    continue;
                };
                let Some(file_id) = episode.get("episodeFileId").and_then(Value::as_i64) else {
                    continue;
                };
                if let Some(file) = episode.get("episodeFile").filter(|file| file.is_object()) {
                    out.push((episode_id, file.clone()));
                    continue;
                }
                match self.sonarr_episode_file(file_id) {
                    Ok(file) => out.push((episode_id, file)),
                    Err(err) => warn!(
                        "Sonarr inventory language audit could not fetch episode file {} for episode {}: {}",
                        file_id, episode_id, err
                    ),
                }
            }
        }
        Ok(out)
    }

    fn sonarr_latest_import_history_id(&self, episode_id: i64) -> Result<Option<i64>> {
        let endpoint = format!(
            "{}/api/v3/history?page=1&pageSize=20&sortKey=date&sortDirection=descending&episodeId={}",
            self.base_url, episode_id
        );
        let response = self.get(&endpoint)?;
        let records = response
            .get("records")
            .and_then(Value::as_array)
            .cloned()
            .unwrap_or_default();
        Ok(records
            .iter()
            .find(|record| {
                record.get("eventType").and_then(Value::as_str) == Some("downloadFolderImported")
            })
            .and_then(|record| record.get("id").and_then(Value::as_i64)))
    }

    fn sonarr_current_episode_file(
        &self,
        episode_id: i64,
        summary: &mut AuditSummary,
    ) -> Option<Value> {
        let episode = match self.sonarr_episode(episode_id) {
            Ok(episode) => episode,
            Err(err) => {
                summary.errors += 1;
                warn!(
                    "Sonarr language audit could not fetch episode {}: {}",
                    episode_id, err
                );
                return None;
            }
        };
        let file_id = episode.get("episodeFileId").and_then(Value::as_i64)?;
        match self.sonarr_episode_file(file_id) {
            Ok(file) => Some(file),
            Err(err) => {
                summary.errors += 1;
                warn!(
                    "Sonarr language audit could not fetch episode file {}: {}",
                    file_id, err
                );
                None
            }
        }
    }

    fn radarr_current_movie_file(
        &self,
        movie_id: i64,
        summary: &mut AuditSummary,
    ) -> Option<Value> {
        let movie = match self.radarr_movie(movie_id) {
            Ok(movie) => movie,
            Err(err) => {
                summary.errors += 1;
                warn!(
                    "Radarr language audit could not fetch movie {}: {}",
                    movie_id, err
                );
                return None;
            }
        };
        let file_id = movie.get("movieFileId").and_then(Value::as_i64)?;
        match self.radarr_movie_file(file_id) {
            Ok(file) => Some(file),
            Err(err) => {
                summary.errors += 1;
                warn!(
                    "Radarr language audit could not fetch movie file {}: {}",
                    file_id, err
                );
                None
            }
        }
    }

    fn sonarr_episode(&self, episode_id: i64) -> Result<Value> {
        self.get(&format!("{}/api/v3/episode/{}", self.base_url, episode_id))
    }

    fn sonarr_episode_file(&self, file_id: i64) -> Result<Value> {
        self.get(&format!("{}/api/v3/episodefile/{}", self.base_url, file_id))
    }

    fn radarr_recent_import_history(&self, lookback_days: u32) -> Result<Vec<Value>> {
        let mut out = Vec::new();
        let min_day = current_day_number().saturating_sub(lookback_days as i64);
        for page in 1..=10 {
            let endpoint = format!(
                "{}/api/v3/history?page={page}&pageSize=100&sortKey=date&sortDirection=descending&includeMovie=true",
                self.base_url
            );
            let response = self.get(&endpoint)?;
            let records = response
                .get("records")
                .and_then(Value::as_array)
                .cloned()
                .unwrap_or_default();
            if records.is_empty() {
                break;
            }
            let mut saw_old = false;
            for record in records {
                if let Some(day) = record
                    .get("date")
                    .and_then(Value::as_str)
                    .and_then(iso_day_number)
                {
                    if day < min_day {
                        saw_old = true;
                        continue;
                    }
                }
                if record.get("eventType").and_then(Value::as_str) == Some("downloadFolderImported")
                {
                    out.push(record);
                }
            }
            if saw_old {
                break;
            }
        }
        Ok(out)
    }

    fn sonarr_force_import_pending_language_upgrades(
        &self,
        requirements: &LanguageRequirements,
        options: RedownloadOptions,
    ) -> Result<AuditSummary> {
        let mut summary = AuditSummary::default();
        let response = self.get(&format!(
            "{}/api/v3/queue?page=1&pageSize=100&includeSeries=true&includeEpisode=true",
            self.base_url
        ))?;
        let records = response
            .get("records")
            .and_then(Value::as_array)
            .cloned()
            .unwrap_or_default();
        for queue_item in records {
            let state = queue_item
                .get("trackedDownloadState")
                .and_then(Value::as_str)
                .unwrap_or_default();
            let status = queue_item
                .get("status")
                .and_then(Value::as_str)
                .unwrap_or_default();
            if state != "importPending" || status != "completed" {
                continue;
            }
            let Some(episode_id) =
                queue_item
                    .get("episodeId")
                    .and_then(Value::as_i64)
                    .or_else(|| {
                        queue_item
                            .get("episode")
                            .and_then(|episode| episode.get("id"))
                            .and_then(Value::as_i64)
                    })
            else {
                continue;
            };
            let Some(download_id) = queue_item.get("downloadId").and_then(Value::as_str) else {
                continue;
            };
            let current_file_id = match self
                .sonarr_episode_file_id_for_queue_item(&queue_item, episode_id)
            {
                Ok(Some(file_id)) => file_id,
                Ok(None) => continue,
                Err(err) => {
                    summary.errors += 1;
                    warn!(
                        "Sonarr language audit could not resolve current episode file for pending episode {}: {}",
                        episode_id, err
                    );
                    continue;
                }
            };
            let current_file = match self.sonarr_episode_file(current_file_id) {
                Ok(file) => file,
                Err(err) => {
                    summary.errors += 1;
                    warn!(
                        "Sonarr language audit could not fetch current episode file {} for pending episode {}: {}",
                        current_file_id, episode_id, err
                    );
                    continue;
                }
            };
            let current_report = language_report_from_episode_file(&current_file, requirements);
            if current_report.satisfied() {
                continue;
            }
            let manual_items = self.sonarr_manual_import_items(download_id)?;
            let Some(item) = manual_items.into_iter().find(|item| {
                sonarr_manual_import_item_matches_episode(item, episode_id)
                    && manual_import_item_satisfies(
                        item,
                        &queue_item,
                        requirements,
                        options.candidate_policy,
                    )
            }) else {
                continue;
            };
            summary.missing += 1;
            summary.searched += 1;
            let title = queue_item
                .get("title")
                .and_then(Value::as_str)
                .unwrap_or("<pending import>");
            if options.dry_run {
                summary.dry_run += 1;
                info!(
                    "Sonarr language audit dry-run: would force-import pending episode {} from '{}'.",
                    episode_id, title
                );
                continue;
            }
            self.sonarr_delete_episode_file(current_file_id)?;
            self.sonarr_post_manual_import(item, episode_id)?;
            if let Some(queue_id) = queue_item.get("id").and_then(Value::as_i64) {
                if let Err(err) = self.sonarr_delete_queue_item(queue_id) {
                    warn!(
                        "Sonarr language audit force-imported episode {} but could not remove completed queue item {}: {}",
                        episode_id, queue_id, err
                    );
                }
            }
            summary.grabbed += 1;
            info!(
                "Sonarr language audit force-imported pending language upgrade for episode {} from '{}'.",
                episode_id, title
            );
        }
        Ok(summary)
    }

    fn sonarr_episode_file_id_for_queue_item(
        &self,
        queue_item: &Value,
        episode_id: i64,
    ) -> Result<Option<i64>> {
        if let Some(file_id) = queue_item
            .get("episodeFileId")
            .and_then(Value::as_i64)
            .or_else(|| {
                queue_item
                    .get("episode")
                    .and_then(|episode| episode.get("episodeFileId"))
                    .and_then(Value::as_i64)
            })
        {
            return Ok(Some(file_id));
        }
        Ok(self
            .sonarr_episode(episode_id)?
            .get("episodeFileId")
            .and_then(Value::as_i64))
    }

    fn sonarr_manual_import_items(&self, download_id: &str) -> Result<Vec<Value>> {
        let endpoint = format!(
            "{}/api/v3/manualimport?downloadId={}&filterExistingFiles=false",
            self.base_url,
            url_encode(download_id)
        );
        self.get(&endpoint)?
            .as_array()
            .cloned()
            .ok_or_else(|| anyhow!("Sonarr manual import did not return an array"))
    }

    fn sonarr_post_manual_import(&self, mut item: Value, episode_id: i64) -> Result<()> {
        if let Value::Object(ref mut map) = item {
            map.insert(
                "episodeIds".to_string(),
                Value::Array(vec![Value::from(episode_id)]),
            );
            if !map.contains_key("seriesId") {
                if let Some(series_id) = map
                    .get("series")
                    .and_then(|series| series.get("id"))
                    .and_then(Value::as_i64)
                {
                    map.insert("seriesId".to_string(), Value::from(series_id));
                }
            }
        }
        let endpoint = format!("{}/api/v3/manualimport", self.base_url);
        self.post_json(&endpoint, &Value::Array(vec![item]))?;
        Ok(())
    }

    fn sonarr_delete_episode_file(&self, file_id: i64) -> Result<()> {
        let endpoint = format!("{}/api/v3/episodefile/{}", self.base_url, file_id);
        self.delete(&endpoint)?;
        Ok(())
    }

    fn sonarr_delete_queue_item(&self, queue_id: i64) -> Result<()> {
        let endpoint = format!(
            "{}/api/v3/queue/{}?removeFromClient=false&blocklist=false",
            self.base_url, queue_id
        );
        self.delete(&endpoint)?;
        Ok(())
    }

    fn radarr_movie(&self, movie_id: i64) -> Result<Value> {
        self.get(&format!("{}/api/v3/movie/{}", self.base_url, movie_id))
    }

    fn radarr_movie_file(&self, file_id: i64) -> Result<Value> {
        self.get(&format!("{}/api/v3/moviefile/{}", self.base_url, file_id))
    }

    fn radarr_force_import_pending_language_upgrades(
        &self,
        requirements: &LanguageRequirements,
        options: RedownloadOptions,
    ) -> Result<AuditSummary> {
        let mut summary = AuditSummary::default();
        let response = self.get(&format!(
            "{}/api/v3/queue?page=1&pageSize=100&includeMovie=true",
            self.base_url
        ))?;
        let records = response
            .get("records")
            .and_then(Value::as_array)
            .cloned()
            .unwrap_or_default();
        for queue_item in records {
            let state = queue_item
                .get("trackedDownloadState")
                .and_then(Value::as_str)
                .unwrap_or_default();
            let status = queue_item
                .get("status")
                .and_then(Value::as_str)
                .unwrap_or_default();
            if state != "importPending" || status != "completed" {
                continue;
            }
            let Some(movie_id) = queue_item
                .get("movieId")
                .and_then(Value::as_i64)
                .or_else(|| {
                    queue_item
                        .get("movie")
                        .and_then(|m| m.get("id"))
                        .and_then(Value::as_i64)
                })
            else {
                continue;
            };
            let Some(download_id) = queue_item.get("downloadId").and_then(Value::as_str) else {
                continue;
            };
            let Some(current_file_id) = queue_item
                .get("movie")
                .and_then(|m| m.get("movieFileId"))
                .and_then(Value::as_i64)
            else {
                continue;
            };
            let Some(current_file) = queue_item.get("movie").and_then(|m| m.get("movieFile"))
            else {
                continue;
            };
            let current_report = language_report_from_episode_file(current_file, requirements);
            if current_report.satisfied() {
                continue;
            }
            let manual_items = self.radarr_manual_import_items(download_id, movie_id)?;
            let Some(item) = manual_items.into_iter().find(|item| {
                manual_import_item_satisfies(
                    item,
                    &queue_item,
                    requirements,
                    options.candidate_policy,
                )
            }) else {
                continue;
            };
            summary.missing += 1;
            summary.searched += 1;
            let title = queue_item
                .get("title")
                .and_then(Value::as_str)
                .unwrap_or("<pending import>");
            if options.dry_run {
                summary.dry_run += 1;
                info!(
                    "Radarr language audit dry-run: would force-import pending movie {} from '{}'.",
                    movie_id, title
                );
                continue;
            }
            self.radarr_delete_movie_file(current_file_id)?;
            self.radarr_post_manual_import(item, movie_id)?;
            if let Some(queue_id) = queue_item.get("id").and_then(Value::as_i64) {
                if let Err(err) = self.radarr_delete_queue_item(queue_id) {
                    warn!(
                        "Radarr language audit force-imported movie {} but could not remove completed queue item {}: {}",
                        movie_id, queue_id, err
                    );
                }
            }
            summary.grabbed += 1;
            info!(
                "Radarr language audit force-imported pending language upgrade for movie {} from '{}'.",
                movie_id, title
            );
        }
        Ok(summary)
    }

    fn radarr_manual_import_items(&self, download_id: &str, movie_id: i64) -> Result<Vec<Value>> {
        let endpoint = format!(
            "{}/api/v3/manualimport?downloadId={}&movieId={}&filterExistingFiles=false",
            self.base_url,
            url_encode(download_id),
            movie_id
        );
        self.get(&endpoint)?
            .as_array()
            .cloned()
            .ok_or_else(|| anyhow!("Radarr manual import did not return an array"))
    }

    fn radarr_post_manual_import(&self, mut item: Value, movie_id: i64) -> Result<()> {
        if let Value::Object(ref mut map) = item {
            map.insert("movieId".to_string(), Value::from(movie_id));
        }
        let endpoint = format!("{}/api/v3/manualimport", self.base_url);
        self.post_json(&endpoint, &Value::Array(vec![item]))?;
        Ok(())
    }

    fn radarr_delete_movie_file(&self, file_id: i64) -> Result<()> {
        let endpoint = format!("{}/api/v3/moviefile/{}", self.base_url, file_id);
        self.delete(&endpoint)?;
        Ok(())
    }

    fn radarr_delete_queue_item(&self, queue_id: i64) -> Result<()> {
        let endpoint = format!(
            "{}/api/v3/queue/{}?removeFromClient=false&blocklist=false",
            self.base_url, queue_id
        );
        self.delete(&endpoint)?;
        Ok(())
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

    fn apply_audit_redownload_outcome(
        &self,
        target: Target,
        history_id: i64,
        requirements: &LanguageRequirements,
        options: RedownloadOptions,
        summary: &mut AuditSummary,
    ) {
        let label = self.kind.label();
        let target_id = target.id();
        match self.redownload_for_target(target, history_id, requirements, options) {
            Ok(RedownloadOutcome::Grabbed { title }) => {
                summary.grabbed += 1;
                info!(
                    "{} language audit grabbed replacement for {} {}: {}",
                    label,
                    target.noun(),
                    target_id,
                    title
                );
            }
            Ok(RedownloadOutcome::DryRun { summary: details }) => {
                summary.dry_run += 1;
                info!(
                    "{} language audit dry-run for {} {}: {}",
                    label,
                    target.noun(),
                    target_id,
                    details
                );
            }
            Ok(RedownloadOutcome::NoVerifiedRelease { reason }) => {
                summary.no_candidate += 1;
                info!(
                    "{} language audit found no replacement for {} {}: {}",
                    label,
                    target.noun(),
                    target_id,
                    reason
                );
            }
            Err(err) => {
                summary.errors += 1;
                warn!(
                    "{} language audit replacement check failed for {} {}: {}",
                    label,
                    target.noun(),
                    target_id,
                    err
                );
            }
        }
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

    fn delete(&self, endpoint: &str) -> Result<Value> {
        let response = self
            .agent
            .delete(endpoint)
            .set("X-Api-Key", &self.api_key)
            .call()
            .with_context(|| format!("DELETE {}", endpoint))?;
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

fn history_episode_id(record: &Value) -> Option<i64> {
    record
        .get("episode")
        .and_then(|episode| episode.get("id"))
        .and_then(Value::as_i64)
        .or_else(|| record.get("episodeId").and_then(Value::as_i64))
}

fn manual_import_item_satisfies(
    item: &Value,
    queue_item: &Value,
    requirements: &LanguageRequirements,
    policy: ServarrLanguageCandidatePolicy,
) -> bool {
    let title = queue_item
        .get("title")
        .and_then(Value::as_str)
        .unwrap_or_default();
    let mut candidate = item.clone();
    if let Value::Object(ref mut map) = candidate {
        map.insert("title".to_string(), Value::from(title));
    }
    release_satisfies_languages(&candidate, requirements, policy)
}

fn sonarr_manual_import_item_matches_episode(item: &Value, episode_id: i64) -> bool {
    if let Some(episodes) = item.get("episodes").and_then(Value::as_array) {
        return episodes.len() == 1
            && episodes[0].get("id").and_then(Value::as_i64) == Some(episode_id);
    }
    if let Some(episode_ids) = item.get("episodeIds").and_then(Value::as_array) {
        return episode_ids.len() == 1 && episode_ids[0].as_i64() == Some(episode_id);
    }
    item.get("episodeId").and_then(Value::as_i64) == Some(episode_id)
}

fn language_report_from_episode_file(
    episode_file: &Value,
    requirements: &LanguageRequirements,
) -> LanguageCheckReport {
    let media = episode_file.get("mediaInfo").unwrap_or(&Value::Null);
    let audio = media
        .get("audioLanguages")
        .and_then(Value::as_str)
        .map(parse_language_tokens)
        .unwrap_or_else(|| {
            let mut out = BTreeSet::new();
            collect_language_values(episode_file.get("languages"), &mut out);
            out.into_iter().collect()
        });
    let subtitles = media
        .get("subtitles")
        .and_then(Value::as_str)
        .map(parse_language_tokens)
        .unwrap_or_default();
    report_from_present(audio, subtitles, requirements)
}

fn maybe_retag_audit_file(
    kind: IntegrationKind,
    media_file: &Value,
    requirements: &LanguageRequirements,
    untagged_retag: &UntaggedRetagOptions,
    current_report: LanguageCheckReport,
) -> LanguageCheckReport {
    let Some(path) = media_file
        .get("path")
        .and_then(Value::as_str)
        .map(PathBuf::from)
    else {
        return current_report;
    };
    if !path.exists() {
        return current_report;
    }
    match super::language::retag_unknown_streams(&path, requirements, untagged_retag) {
        Ok(retag) if retag.changed() => {
            info!(
                "{} {}retagged unknown stream language metadata for '{}': audio={}, subtitles={}",
                kind.label(),
                if retag.dry_run { "would have " } else { "" },
                path.display(),
                retag.audio_streams,
                retag.subtitle_streams
            );
            if retag.dry_run {
                current_report
            } else {
                super::language::check_file(&path, requirements).unwrap_or(current_report)
            }
        }
        Ok(_) => current_report,
        Err(err) => {
            warn!(
                "{} inventory language audit failed to retag unknown stream metadata for '{}': {}",
                kind.label(),
                path.display(),
                err
            );
            current_report
        }
    }
}

fn record_language_audit_assessment(
    kind: IntegrationKind,
    fallback_prefix: &str,
    media_file: &Value,
    requirements: &LanguageRequirements,
    report: &LanguageCheckReport,
) {
    let path = media_file
        .get("path")
        .and_then(Value::as_str)
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            let id = media_file
                .get("id")
                .and_then(Value::as_i64)
                .map(|id| id.to_string())
                .unwrap_or_else(|| "unknown".to_string());
            PathBuf::from(format!("{fallback_prefix}:{id}"))
        });
    if let Err(err) = cache::record_assessment(kind, &path, requirements, report) {
        warn!(
            "{} language audit failed to update DPN cache for '{}': {}",
            kind.label(),
            path.display(),
            err
        );
    }
}

fn log_audit_summary(label: &str, summary: &AuditSummary) {
    info!(
        "{} language audit complete: checked={}, missing={}, searched={}, dry_run={}, grabbed={}, no_candidate={}, errors={}",
        label,
        summary.checked,
        summary.missing,
        summary.searched,
        summary.dry_run,
        summary.grabbed,
        summary.no_candidate,
        summary.errors
    );
}

fn current_day_number() -> i64 {
    let unix_days = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default()
        .as_secs() as i64
        / 86_400;
    unix_days + days_from_civil(1970, 1, 1)
}

fn iso_day_number(value: &str) -> Option<i64> {
    let date = value.get(0..10)?;
    let mut parts = date.split('-');
    let year = parts.next()?.parse().ok()?;
    let month = parts.next()?.parse().ok()?;
    let day = parts.next()?.parse().ok()?;
    Some(days_from_civil(year, month, day))
}

fn days_from_civil(year: i64, month: i64, day: i64) -> i64 {
    let y = year - if month <= 2 { 1 } else { 0 };
    let era = if y >= 0 { y } else { y - 399 } / 400;
    let yoe = y - era * 400;
    let mp = month + if month > 2 { -3 } else { 9 };
    let doy = (153 * mp + 2) / 5 + day - 1;
    let doe = yoe * 365 + yoe / 4 - yoe / 100 + doy;
    era * 146_097 + doe
}

#[derive(Debug, Clone, Copy)]
enum Target {
    SonarrEpisode { episode_id: i64 },
    RadarrMovie { movie_id: i64 },
}

impl Target {
    fn id(self) -> i64 {
        match self {
            Target::SonarrEpisode { episode_id } => episode_id,
            Target::RadarrMovie { movie_id } => movie_id,
        }
    }

    fn noun(self) -> &'static str {
        match self {
            Target::SonarrEpisode { .. } => "episode",
            Target::RadarrMovie { .. } => "movie",
        }
    }

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

fn select_verified_release_for_target<'a>(
    releases: &'a [Value],
    target: Target,
    requirements: &LanguageRequirements,
    policy: ServarrLanguageCandidatePolicy,
) -> Option<&'a Value> {
    releases
        .iter()
        .filter(|release| release_matches_target(release, target))
        .filter(|release| release_can_be_language_upgrade_candidate(release))
        .filter(|release| release_satisfies_languages(release, requirements, policy))
        .max_by_key(|release| release_score(release))
}

fn release_matches_target(release: &Value, target: Target) -> bool {
    match target {
        Target::RadarrMovie { .. } => true,
        Target::SonarrEpisode { episode_id } => sonarr_release_matches_episode(release, episode_id),
    }
}

fn sonarr_release_matches_episode(release: &Value, episode_id: i64) -> bool {
    if release_type_is_multi_episode_or_pack(release) {
        return false;
    }
    if let Some(mapped) = release.get("mappedEpisodeInfo").and_then(Value::as_array) {
        if !mapped.is_empty() {
            return mapped.len() == 1
                && mapped[0].get("id").and_then(Value::as_i64) == Some(episode_id);
        }
    }
    if let Some(ids) = release.get("episodeIds").and_then(Value::as_array) {
        return ids.len() == 1 && ids[0].as_i64() == Some(episode_id);
    }
    if let Some(id) = release.get("episodeId").and_then(Value::as_i64) {
        return id == episode_id;
    }
    if release
        .get("episodeNumbers")
        .and_then(Value::as_array)
        .is_some_and(|numbers| numbers.len() > 1)
    {
        return false;
    }
    if release
        .get("absoluteEpisodeNumbers")
        .and_then(Value::as_array)
        .is_some_and(|numbers| numbers.len() > 1)
    {
        return false;
    }
    !release_title_looks_like_episode_range(release)
}

fn release_type_is_multi_episode_or_pack(release: &Value) -> bool {
    release
        .get("releaseType")
        .and_then(Value::as_str)
        .map(|value| {
            let value = value.to_ascii_lowercase();
            value.contains("pack") || value.contains("multi") || value.contains("season")
        })
        .unwrap_or(false)
}

fn release_title_looks_like_episode_range(release: &Value) -> bool {
    let Some(title) = release_title(release) else {
        return false;
    };
    title_has_numeric_episode_range(&title)
}

fn title_has_numeric_episode_range(title: &str) -> bool {
    let bytes = title.as_bytes();
    for idx in 0..bytes.len() {
        if bytes[idx] != b'-' {
            continue;
        }
        let left = count_ascii_digits_before(bytes, idx);
        let right = count_ascii_digits_after(bytes, idx + 1);
        if (1..=3).contains(&left) && (1..=3).contains(&right) {
            return true;
        }
    }
    false
}

fn count_ascii_digits_before(bytes: &[u8], mut idx: usize) -> usize {
    let mut count = 0;
    while idx > 0 {
        idx -= 1;
        if !bytes[idx].is_ascii_digit() {
            break;
        }
        count += 1;
    }
    count
}

fn count_ascii_digits_after(bytes: &[u8], mut idx: usize) -> usize {
    let mut count = 0;
    while idx < bytes.len() && bytes[idx].is_ascii_digit() {
        count += 1;
        idx += 1;
    }
    count
}

fn release_can_be_language_upgrade_candidate(release: &Value) -> bool {
    if release.get("downloadAllowed").and_then(Value::as_bool) == Some(false) {
        return false;
    }
    if release.get("rejected").and_then(Value::as_bool) != Some(true) {
        return true;
    }
    release_rejections_are_only_existing_file_cutoff(release)
}

fn release_rejections_are_only_existing_file_cutoff(release: &Value) -> bool {
    let Some(rejections) = release.get("rejections").and_then(Value::as_array) else {
        return false;
    };
    !rejections.is_empty()
        && rejections.iter().all(|reason| {
            let reason = reason.as_str().unwrap_or_default().to_ascii_lowercase();
            reason.contains("existing file")
                || reason.contains("meets cutoff")
                || reason.contains("quality profile does not allow upgrades")
                || reason.contains("equal or higher custom format score")
                || reason.contains("equal or higher preference")
        })
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

    fn mock_empty_queue(server: &mut mockito::Server) -> mockito::Mock {
        server
            .mock("GET", "/api/v3/queue")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [] }).to_string())
            .create()
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
    fn sonarr_inventory_audit_dry_run_searches_current_episode_files() {
        let mut server = mockito::Server::new();
        let queue = mock_empty_queue(&mut server);
        let series = server
            .mock("GET", "/api/v3/series")
            .with_status(200)
            .with_body(json!([{ "id": 1, "title": "Example" }]).to_string())
            .create();
        let episodes = server
            .mock("GET", "/api/v3/episode")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(
                json!([{
                    "id": 77,
                    "seriesId": 1,
                    "hasFile": true,
                    "episodeFileId": 555,
                    "episodeFile": {
                        "id": 555,
                        "path": "/media/example.mkv",
                        "mediaInfo": {
                            "audioLanguages": "jpn",
                            "subtitles": "eng"
                        }
                    }
                }])
                .to_string(),
            )
            .create();
        let history = server
            .mock("GET", "/api/v3/history")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [] }).to_string())
            .create();
        let search = server
            .mock("GET", "/api/v3/release")
            .match_query(Matcher::UrlEncoded("episodeId".into(), "77".into()))
            .with_status(200)
            .with_body(
                json!([{
                    "title":"example S01E01 1080p Dual-Audio Multi-Subs",
                    "rejected": true,
                    "downloadAllowed": true,
                    "rejections":["Existing file meets cutoff: Unknown"],
                    "customFormatScore": 875
                }])
                .to_string(),
            )
            .create();

        let summary = run_sonarr_language_audit(
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &LanguageRequirements {
                enabled: true,
                audio: vec!["eng".to_string(), "jpn".to_string()],
                subtitles: vec!["eng".to_string()],
            },
            RedownloadOptions {
                dry_run: true,
                candidate_policy: ServarrLanguageCandidatePolicy::CustomFormatOrTitle,
            },
            AuditOptions {
                scope: ServarrLanguageAuditScope::Inventory,
                lookback_days: 30,
                max_searches: 10,
                episode_ids: Vec::new(),
                untagged_retag: UntaggedRetagOptions::default(),
            },
        )
        .unwrap();

        assert_eq!(summary.checked, 1);
        assert_eq!(summary.missing, 1);
        assert_eq!(summary.searched, 1);
        assert_eq!(summary.dry_run, 1);
        assert_eq!(summary.errors, 0);
        queue.assert();
        series.assert();
        episodes.assert();
        history.assert();
        search.assert();
    }

    #[test]
    fn sonarr_inventory_audit_grabs_replacement_and_blocklists_latest_history() {
        let mut server = mockito::Server::new();
        let queue = mock_empty_queue(&mut server);
        let series = server
            .mock("GET", "/api/v3/series")
            .with_status(200)
            .with_body(json!([{ "id": 1, "title": "Example" }]).to_string())
            .create();
        let episodes = server
            .mock("GET", "/api/v3/episode")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(
                json!([{
                    "id": 77,
                    "seriesId": 1,
                    "hasFile": true,
                    "episodeFileId": 555,
                    "episodeFile": {
                        "id": 555,
                        "path": "/media/example.mkv",
                        "mediaInfo": { "audioLanguages": "jpn", "subtitles": "eng" }
                    }
                }])
                .to_string(),
            )
            .create();
        let history = server
            .mock("GET", "/api/v3/history")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(
                json!({
                    "records": [{
                        "id": 1234,
                        "eventType": "downloadFolderImported",
                        "episodeId": 77
                    }]
                })
                .to_string(),
            )
            .create();
        let search = server
            .mock("GET", "/api/v3/release")
            .match_query(Matcher::UrlEncoded("episodeId".into(), "77".into()))
            .with_status(200)
            .with_body(
                json!([{
                    "title": "target replacement",
                    "rejected": false,
                    "languages": [{"name":"English"}, {"name":"Japanese"}],
                    "subtitleLanguages": [{"name":"English"}]
                }])
                .to_string(),
            )
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

        let summary = run_sonarr_language_audit(
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &LanguageRequirements {
                enabled: true,
                audio: vec!["eng".to_string(), "jpn".to_string()],
                subtitles: vec!["eng".to_string()],
            },
            RedownloadOptions {
                dry_run: false,
                candidate_policy: ServarrLanguageCandidatePolicy::Strict,
            },
            AuditOptions {
                scope: ServarrLanguageAuditScope::Inventory,
                lookback_days: 30,
                max_searches: 10,
                episode_ids: Vec::new(),
                untagged_retag: UntaggedRetagOptions::default(),
            },
        )
        .unwrap();

        assert_eq!(summary.checked, 1);
        assert_eq!(summary.missing, 1);
        assert_eq!(summary.searched, 1);
        assert_eq!(summary.grabbed, 1);
        assert_eq!(summary.errors, 0);
        queue.assert();
        series.assert();
        episodes.assert();
        history.assert();
        search.assert();
        grab.assert();
        blocklist.assert();
    }

    #[test]
    fn sonarr_inventory_audit_stops_after_max_searches() {
        let mut server = mockito::Server::new();
        let queue = mock_empty_queue(&mut server);
        let series = server
            .mock("GET", "/api/v3/series")
            .with_status(200)
            .with_body(json!([{ "id": 1, "title": "Example" }]).to_string())
            .create();
        let episodes = server
            .mock("GET", "/api/v3/episode")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(
                json!([
                    {
                        "id": 77,
                        "seriesId": 1,
                        "hasFile": true,
                        "episodeFileId": 555,
                        "episodeFile": {
                            "id": 555,
                            "path": "/media/one.mkv",
                            "mediaInfo": { "audioLanguages": "jpn", "subtitles": "eng" }
                        }
                    },
                    {
                        "id": 78,
                        "seriesId": 1,
                        "hasFile": true,
                        "episodeFileId": 556,
                        "episodeFile": {
                            "id": 556,
                            "path": "/media/two.mkv",
                            "mediaInfo": { "audioLanguages": "jpn", "subtitles": "eng" }
                        }
                    }
                ])
                .to_string(),
            )
            .create();
        let history = server
            .mock("GET", "/api/v3/history")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [] }).to_string())
            .create();
        let search = server
            .mock("GET", "/api/v3/release")
            .match_query(Matcher::UrlEncoded("episodeId".into(), "77".into()))
            .with_status(200)
            .with_body(json!([]).to_string())
            .create();

        let summary = run_sonarr_language_audit(
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &LanguageRequirements {
                enabled: true,
                audio: vec!["eng".to_string(), "jpn".to_string()],
                subtitles: vec!["eng".to_string()],
            },
            RedownloadOptions {
                dry_run: true,
                candidate_policy: ServarrLanguageCandidatePolicy::Strict,
            },
            AuditOptions {
                scope: ServarrLanguageAuditScope::Inventory,
                lookback_days: 30,
                max_searches: 1,
                episode_ids: Vec::new(),
                untagged_retag: UntaggedRetagOptions::default(),
            },
        )
        .unwrap();

        assert_eq!(summary.checked, 1);
        assert_eq!(summary.missing, 1);
        assert_eq!(summary.searched, 1);
        assert_eq!(summary.no_candidate, 1);
        queue.assert();
        series.assert();
        episodes.assert();
        history.assert();
        search.assert();
    }

    #[test]
    fn sonarr_inventory_audit_targeted_episode_ids_skip_series_scan() {
        let mut server = mockito::Server::new();
        let queue = mock_empty_queue(&mut server);
        let episode = server
            .mock("GET", "/api/v3/episode/77")
            .with_status(200)
            .with_body(json!({ "id": 77, "episodeFileId": 555 }).to_string())
            .create();
        let episode_file = server
            .mock("GET", "/api/v3/episodefile/555")
            .with_status(200)
            .with_body(
                json!({
                    "id": 555,
                    "path": "/media/example.mkv",
                    "mediaInfo": { "audioLanguages": "jpn", "subtitles": "eng" }
                })
                .to_string(),
            )
            .create();
        let history = server
            .mock("GET", "/api/v3/history")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [] }).to_string())
            .create();
        let search = server
            .mock("GET", "/api/v3/release")
            .match_query(Matcher::UrlEncoded("episodeId".into(), "77".into()))
            .with_status(200)
            .with_body(
                json!([{ "title": "target Dual-Audio Multi-Subs", "rejected": false }]).to_string(),
            )
            .create();

        let summary = run_sonarr_language_audit(
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &LanguageRequirements {
                enabled: true,
                audio: vec!["eng".to_string(), "jpn".to_string()],
                subtitles: vec!["eng".to_string()],
            },
            RedownloadOptions {
                dry_run: true,
                candidate_policy: ServarrLanguageCandidatePolicy::CustomFormatOrTitle,
            },
            AuditOptions {
                scope: ServarrLanguageAuditScope::Inventory,
                lookback_days: 30,
                max_searches: 10,
                episode_ids: vec![77],
                untagged_retag: UntaggedRetagOptions::default(),
            },
        )
        .unwrap();

        assert_eq!(summary.checked, 1);
        assert_eq!(summary.missing, 1);
        assert_eq!(summary.searched, 1);
        assert_eq!(summary.dry_run, 1);
        queue.assert();
        episode.assert();
        episode_file.assert();
        history.assert();
        search.assert();
    }

    #[test]
    fn sonarr_inventory_apply_refuses_without_import_history() {
        let mut server = mockito::Server::new();
        let queue = mock_empty_queue(&mut server);
        let episode = server
            .mock("GET", "/api/v3/episode/77")
            .with_status(200)
            .with_body(json!({ "id": 77, "episodeFileId": 555 }).to_string())
            .create();
        let episode_file = server
            .mock("GET", "/api/v3/episodefile/555")
            .with_status(200)
            .with_body(
                json!({
                    "id": 555,
                    "path": "/media/example.mkv",
                    "mediaInfo": { "audioLanguages": "jpn", "subtitles": "eng" }
                })
                .to_string(),
            )
            .create();
        let history = server
            .mock("GET", "/api/v3/history")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [] }).to_string())
            .create();

        let summary = run_sonarr_language_audit(
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &LanguageRequirements {
                enabled: true,
                audio: vec!["eng".to_string(), "jpn".to_string()],
                subtitles: vec!["eng".to_string()],
            },
            RedownloadOptions {
                dry_run: false,
                candidate_policy: ServarrLanguageCandidatePolicy::CustomFormatOrTitle,
            },
            AuditOptions {
                scope: ServarrLanguageAuditScope::Inventory,
                lookback_days: 30,
                max_searches: 10,
                episode_ids: vec![77],
                untagged_retag: UntaggedRetagOptions::default(),
            },
        )
        .unwrap();

        assert_eq!(summary.checked, 1);
        assert_eq!(summary.missing, 1);
        assert_eq!(summary.searched, 1);
        assert_eq!(summary.errors, 1);
        assert_eq!(summary.grabbed, 0);
        queue.assert();
        episode.assert();
        episode_file.assert();
        history.assert();
    }

    fn sonarr_pending_queue_item() -> Value {
        json!({
            "id": 321,
            "episodeId": 77,
            "downloadId": "download 1",
            "title": "episode dual audio",
            "status": "completed",
            "trackedDownloadState": "importPending",
            "episode": {
                "id": 77,
                "episodeFileId": 555
            }
        })
    }

    fn language_requirements() -> LanguageRequirements {
        LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string(), "jpn".to_string()],
            subtitles: vec!["eng".to_string()],
        }
    }

    #[test]
    fn sonarr_force_import_pending_language_upgrade_dry_run_is_safe() {
        let mut server = mockito::Server::new();
        let queue = server
            .mock("GET", "/api/v3/queue")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [sonarr_pending_queue_item()] }).to_string())
            .create();
        let episode_file = server
            .mock("GET", "/api/v3/episodefile/555")
            .with_status(200)
            .with_body(
                json!({
                    "id": 555,
                    "path": "/shows/current.mkv",
                    "mediaInfo": { "audioLanguages": "jpn", "subtitles": "eng" }
                })
                .to_string(),
            )
            .create();
        let manual = server
            .mock("GET", "/api/v3/manualimport")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(
                json!([{
                    "path": "/downloads/episode.mkv",
                    "episodes": [{"id": 77}],
                    "series": {"id": 1},
                    "languages": [{"name":"English"}, {"name":"Japanese"}],
                    "subtitleLanguages": [{"name":"English"}]
                }])
                .to_string(),
            )
            .create();
        let history = server
            .mock("GET", "/api/v3/history")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [] }).to_string())
            .create();

        let summary = run_sonarr_language_audit(
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &language_requirements(),
            RedownloadOptions {
                dry_run: true,
                candidate_policy: ServarrLanguageCandidatePolicy::Strict,
            },
            AuditOptions {
                scope: ServarrLanguageAuditScope::History,
                lookback_days: 30,
                max_searches: 10,
                episode_ids: Vec::new(),
                untagged_retag: UntaggedRetagOptions::default(),
            },
        )
        .unwrap();

        assert_eq!(summary.missing, 1);
        assert_eq!(summary.searched, 1);
        assert_eq!(summary.dry_run, 1);
        assert_eq!(summary.grabbed, 0);
        queue.assert();
        episode_file.assert();
        manual.assert();
        history.assert();
    }

    #[test]
    fn sonarr_force_import_pending_language_upgrade_applies_manual_import() {
        let mut server = mockito::Server::new();
        let queue = server
            .mock("GET", "/api/v3/queue")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [sonarr_pending_queue_item()] }).to_string())
            .create();
        let episode_file = server
            .mock("GET", "/api/v3/episodefile/555")
            .with_status(200)
            .with_body(
                json!({
                    "id": 555,
                    "path": "/shows/current.mkv",
                    "mediaInfo": { "audioLanguages": "jpn", "subtitles": "eng" }
                })
                .to_string(),
            )
            .create();
        let manual = server
            .mock("GET", "/api/v3/manualimport")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(
                json!([{
                    "path": "/downloads/episode.mkv",
                    "episodes": [{"id": 77}],
                    "series": {"id": 1},
                    "languages": [{"name":"English"}, {"name":"Japanese"}],
                    "subtitleLanguages": [{"name":"English"}]
                }])
                .to_string(),
            )
            .create();
        let delete_file = server
            .mock("DELETE", "/api/v3/episodefile/555")
            .with_status(200)
            .with_body("{}")
            .create();
        let post_import = server
            .mock("POST", "/api/v3/manualimport")
            .match_header("content-type", "application/json")
            .with_status(200)
            .with_body("{}")
            .create();
        let delete_queue = server
            .mock("DELETE", "/api/v3/queue/321")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body("{}")
            .create();
        let history = server
            .mock("GET", "/api/v3/history")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [] }).to_string())
            .create();

        let summary = run_sonarr_language_audit(
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &language_requirements(),
            RedownloadOptions {
                dry_run: false,
                candidate_policy: ServarrLanguageCandidatePolicy::Strict,
            },
            AuditOptions {
                scope: ServarrLanguageAuditScope::History,
                lookback_days: 30,
                max_searches: 10,
                episode_ids: Vec::new(),
                untagged_retag: UntaggedRetagOptions::default(),
            },
        )
        .unwrap();

        assert_eq!(summary.missing, 1);
        assert_eq!(summary.searched, 1);
        assert_eq!(summary.grabbed, 1);
        assert_eq!(summary.errors, 0);
        queue.assert();
        episode_file.assert();
        manual.assert();
        delete_file.assert();
        post_import.assert();
        delete_queue.assert();
        history.assert();
    }

    #[test]
    fn sonarr_force_import_skips_multi_episode_manual_import_item() {
        let mut server = mockito::Server::new();
        let queue = server
            .mock("GET", "/api/v3/queue")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [sonarr_pending_queue_item()] }).to_string())
            .create();
        let episode_file = server
            .mock("GET", "/api/v3/episodefile/555")
            .with_status(200)
            .with_body(
                json!({
                    "id": 555,
                    "path": "/shows/current.mkv",
                    "mediaInfo": { "audioLanguages": "jpn", "subtitles": "eng" }
                })
                .to_string(),
            )
            .create();
        let manual = server
            .mock("GET", "/api/v3/manualimport")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(
                json!([{
                    "path": "/downloads/pack.mkv",
                    "episodes": [{"id": 77}, {"id": 78}],
                    "languages": [{"name":"English"}, {"name":"Japanese"}],
                    "subtitleLanguages": [{"name":"English"}]
                }])
                .to_string(),
            )
            .create();
        let history = server
            .mock("GET", "/api/v3/history")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [] }).to_string())
            .create();

        let summary = run_sonarr_language_audit(
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &language_requirements(),
            RedownloadOptions {
                dry_run: false,
                candidate_policy: ServarrLanguageCandidatePolicy::Strict,
            },
            AuditOptions {
                scope: ServarrLanguageAuditScope::History,
                lookback_days: 30,
                max_searches: 10,
                episode_ids: Vec::new(),
                untagged_retag: UntaggedRetagOptions::default(),
            },
        )
        .unwrap();

        assert_eq!(summary.missing, 0);
        assert_eq!(summary.searched, 0);
        assert_eq!(summary.grabbed, 0);
        assert_eq!(summary.errors, 0);
        queue.assert();
        episode_file.assert();
        manual.assert();
        history.assert();
    }

    #[test]
    fn sonarr_force_import_skips_when_manual_import_item_is_for_different_episode() {
        let mut server = mockito::Server::new();
        let queue = server
            .mock("GET", "/api/v3/queue")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [sonarr_pending_queue_item()] }).to_string())
            .create();
        let episode_file = server
            .mock("GET", "/api/v3/episodefile/555")
            .with_status(200)
            .with_body(
                json!({
                    "id": 555,
                    "path": "/shows/current.mkv",
                    "mediaInfo": { "audioLanguages": "jpn", "subtitles": "eng" }
                })
                .to_string(),
            )
            .create();
        let manual = server
            .mock("GET", "/api/v3/manualimport")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(
                json!([{
                    "path": "/downloads/wrong.mkv",
                    "episodes": [{"id": 78}],
                    "languages": [{"name":"English"}, {"name":"Japanese"}],
                    "subtitleLanguages": [{"name":"English"}]
                }])
                .to_string(),
            )
            .create();
        let history = server
            .mock("GET", "/api/v3/history")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [] }).to_string())
            .create();

        let summary = run_sonarr_language_audit(
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &language_requirements(),
            RedownloadOptions {
                dry_run: false,
                candidate_policy: ServarrLanguageCandidatePolicy::Strict,
            },
            AuditOptions {
                scope: ServarrLanguageAuditScope::History,
                lookback_days: 30,
                max_searches: 10,
                episode_ids: Vec::new(),
                untagged_retag: UntaggedRetagOptions::default(),
            },
        )
        .unwrap();

        assert_eq!(summary.missing, 0);
        assert_eq!(summary.searched, 0);
        assert_eq!(summary.grabbed, 0);
        assert_eq!(summary.errors, 0);
        queue.assert();
        episode_file.assert();
        manual.assert();
        history.assert();
    }

    fn radarr_pending_queue_item() -> Value {
        json!({
            "id": 123,
            "movieId": 88,
            "downloadId": "download 1",
            "title": "movie dual audio",
            "status": "completed",
            "trackedDownloadState": "importPending",
            "movie": {
                "id": 88,
                "movieFileId": 999,
                "movieFile": {
                    "id": 999,
                    "path": "/movies/current.mkv",
                    "mediaInfo": { "audioLanguages": "jpn", "subtitles": "" }
                }
            }
        })
    }

    fn radarr_requirements() -> LanguageRequirements {
        LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string(), "jpn".to_string()],
            subtitles: vec!["eng".to_string()],
        }
    }

    #[test]
    fn radarr_force_import_pending_language_upgrade_dry_run_is_safe() {
        let mut server = mockito::Server::new();
        let queue = server
            .mock("GET", "/api/v3/queue")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [radarr_pending_queue_item()] }).to_string())
            .create();
        let manual = server
            .mock("GET", "/api/v3/manualimport")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(
                json!([{
                    "path": "/downloads/movie.mkv",
                    "languages": [{"name":"English"}, {"name":"Japanese"}],
                    "subtitleLanguages": [{"name":"English"}]
                }])
                .to_string(),
            )
            .create();
        let history = server
            .mock("GET", "/api/v3/history")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [] }).to_string())
            .create();

        let summary = run_radarr_language_audit(
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &radarr_requirements(),
            RedownloadOptions {
                dry_run: true,
                candidate_policy: ServarrLanguageCandidatePolicy::Strict,
            },
            AuditOptions {
                scope: ServarrLanguageAuditScope::History,
                lookback_days: 30,
                max_searches: 10,
                episode_ids: Vec::new(),
                untagged_retag: UntaggedRetagOptions::default(),
            },
        )
        .unwrap();

        assert_eq!(summary.missing, 1);
        assert_eq!(summary.searched, 1);
        assert_eq!(summary.dry_run, 1);
        assert_eq!(summary.grabbed, 0);
        queue.assert();
        manual.assert();
        history.assert();
    }

    #[test]
    fn radarr_force_import_pending_language_upgrade_applies_manual_import() {
        let mut server = mockito::Server::new();
        let queue = server
            .mock("GET", "/api/v3/queue")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [radarr_pending_queue_item()] }).to_string())
            .create();
        let manual = server
            .mock("GET", "/api/v3/manualimport")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(
                json!([{
                    "path": "/downloads/movie.mkv",
                    "languages": [{"name":"English"}, {"name":"Japanese"}],
                    "subtitleLanguages": [{"name":"English"}]
                }])
                .to_string(),
            )
            .create();
        let delete_file = server
            .mock("DELETE", "/api/v3/moviefile/999")
            .with_status(200)
            .with_body("{}")
            .create();
        let post_import = server
            .mock("POST", "/api/v3/manualimport")
            .match_header("content-type", "application/json")
            .with_status(200)
            .with_body("{}")
            .create();
        let delete_queue = server
            .mock("DELETE", "/api/v3/queue/123")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body("{}")
            .create();
        let history = server
            .mock("GET", "/api/v3/history")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [] }).to_string())
            .create();

        let summary = run_radarr_language_audit(
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &radarr_requirements(),
            RedownloadOptions {
                dry_run: false,
                candidate_policy: ServarrLanguageCandidatePolicy::Strict,
            },
            AuditOptions {
                scope: ServarrLanguageAuditScope::History,
                lookback_days: 30,
                max_searches: 10,
                episode_ids: Vec::new(),
                untagged_retag: UntaggedRetagOptions::default(),
            },
        )
        .unwrap();

        assert_eq!(summary.missing, 1);
        assert_eq!(summary.searched, 1);
        assert_eq!(summary.grabbed, 1);
        assert_eq!(summary.errors, 0);
        queue.assert();
        manual.assert();
        delete_file.assert();
        post_import.assert();
        delete_queue.assert();
        history.assert();
    }

    #[test]
    fn radarr_force_import_skips_when_current_movie_file_already_satisfies_requirements() {
        let mut server = mockito::Server::new();
        let mut queue_item = radarr_pending_queue_item();
        queue_item["movie"]["movieFile"]["mediaInfo"] = json!({
            "audioLanguages": "eng/jpn",
            "subtitles": "eng"
        });
        let queue = server
            .mock("GET", "/api/v3/queue")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [queue_item] }).to_string())
            .create();
        let history = server
            .mock("GET", "/api/v3/history")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [] }).to_string())
            .create();

        let summary = run_radarr_language_audit(
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &radarr_requirements(),
            RedownloadOptions {
                dry_run: false,
                candidate_policy: ServarrLanguageCandidatePolicy::Strict,
            },
            AuditOptions {
                scope: ServarrLanguageAuditScope::History,
                lookback_days: 30,
                max_searches: 10,
                episode_ids: Vec::new(),
                untagged_retag: UntaggedRetagOptions::default(),
            },
        )
        .unwrap();

        assert_eq!(summary.missing, 0);
        assert_eq!(summary.searched, 0);
        assert_eq!(summary.grabbed, 0);
        assert_eq!(summary.errors, 0);
        queue.assert();
        history.assert();
    }

    #[test]
    fn radarr_force_import_skips_when_manual_import_item_lacks_languages() {
        let mut server = mockito::Server::new();
        let queue = server
            .mock("GET", "/api/v3/queue")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [radarr_pending_queue_item()] }).to_string())
            .create();
        let manual = server
            .mock("GET", "/api/v3/manualimport")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!([{ "path": "/downloads/movie.mkv" }]).to_string())
            .create();
        let history = server
            .mock("GET", "/api/v3/history")
            .match_query(Matcher::Any)
            .with_status(200)
            .with_body(json!({ "records": [] }).to_string())
            .create();

        let summary = run_radarr_language_audit(
            &ApiSettings {
                url: Some(server.url()),
                api_key: Some("test-key".to_string()),
            },
            &radarr_requirements(),
            RedownloadOptions {
                dry_run: false,
                candidate_policy: ServarrLanguageCandidatePolicy::Strict,
            },
            AuditOptions {
                scope: ServarrLanguageAuditScope::History,
                lookback_days: 30,
                max_searches: 10,
                episode_ids: Vec::new(),
                untagged_retag: UntaggedRetagOptions::default(),
            },
        )
        .unwrap();

        assert_eq!(summary.missing, 0);
        assert_eq!(summary.searched, 0);
        assert_eq!(summary.grabbed, 0);
        assert_eq!(summary.errors, 0);
        queue.assert();
        manual.assert();
        history.assert();
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
    fn sonarr_target_selection_rejects_multi_episode_pack_even_when_language_matches() {
        let req = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string()],
            subtitles: Vec::new(),
        };
        let releases = vec![
            json!({
                "title":"Show - 01-12 1080p Dual Audio",
                "rejected": true,
                "downloadAllowed": true,
                "rejections":["Existing file meets cutoff: Unknown"],
                "mappedEpisodeInfo":[
                    {"id": 77, "seasonNumber": 1, "episodeNumber": 1},
                    {"id": 78, "seasonNumber": 1, "episodeNumber": 2}
                ],
                "languages":[{"name":"English"}],
                "customFormatScore": 900
            }),
            json!({
                "title":"Show S01E01 1080p English Dub",
                "rejected": true,
                "downloadAllowed": true,
                "rejections":["Existing file meets cutoff: Unknown"],
                "mappedEpisodeInfo":[{"id": 77, "seasonNumber": 1, "episodeNumber": 1}],
                "languages":[{"name":"English"}],
                "customFormatScore": 100
            }),
        ];

        let selected = select_verified_release_for_target(
            &releases,
            Target::SonarrEpisode { episode_id: 77 },
            &req,
            ServarrLanguageCandidatePolicy::Strict,
        )
        .unwrap();
        assert_eq!(
            release_title(selected).as_deref(),
            Some("Show S01E01 1080p English Dub")
        );
    }

    #[test]
    fn sonarr_target_selection_rejects_pack_without_mapping_by_title_range() {
        let req = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string()],
            subtitles: Vec::new(),
        };
        let releases = vec![json!({
            "title":"Show - 01-12 1080p Dual Audio",
            "rejected": false,
            "languages":[{"name":"English"}],
            "customFormatScore": 900
        })];

        assert!(select_verified_release_for_target(
            &releases,
            Target::SonarrEpisode { episode_id: 77 },
            &req,
            ServarrLanguageCandidatePolicy::Strict,
        )
        .is_none());
    }

    #[test]
    fn sonarr_target_selection_rejects_episode_requested_rejection() {
        let req = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string()],
            subtitles: Vec::new(),
        };
        let releases = vec![json!({
            "title":"Wrong Show - 12 Dual Audio",
            "rejected": true,
            "downloadAllowed": true,
            "rejections":[
                "Existing file meets cutoff: Unknown",
                "Episode wasn't requested: 1x12"
            ],
            "mappedEpisodeInfo":[{"id": 88, "seasonNumber": 1, "episodeNumber": 12}],
            "languages":[{"name":"English"}],
            "customFormatScore": 900
        })];

        assert!(select_verified_release_for_target(
            &releases,
            Target::SonarrEpisode { episode_id: 77 },
            &req,
            ServarrLanguageCandidatePolicy::Strict,
        )
        .is_none());
    }

    #[test]
    fn accepts_existing_file_cutoff_rejection_for_language_upgrade_candidate() {
        let req = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string(), "jpn".to_string()],
            subtitles: vec!["eng".to_string()],
        };
        let releases = vec![json!({
            "title":"language upgrade candidate 1080p Dual-Audio Multi-Subs",
            "rejected": true,
            "downloadAllowed": true,
            "rejections":[
                "Existing file and the Quality profile does not allow upgrades",
                "Existing file meets cutoff: Unknown",
                "Existing file on disk has a equal or higher Custom Format score: 0",
                "Existing file on disk is of equal or higher preference: Remux-1080p"
            ],
            "customFormatScore": 875
        })];

        assert!(select_verified_release_for_target(
            &releases,
            Target::RadarrMovie { movie_id: 1 },
            &req,
            ServarrLanguageCandidatePolicy::CustomFormatOrTitle
        )
        .is_some());
    }

    #[test]
    fn still_rejects_unrelated_arr_rejections() {
        let req = LanguageRequirements {
            enabled: true,
            audio: vec!["eng".to_string(), "jpn".to_string()],
            subtitles: vec!["eng".to_string()],
        };
        let releases = vec![json!({
            "title":"language upgrade candidate 1080p Dual-Audio Multi-Subs",
            "rejected": true,
            "downloadAllowed": true,
            "rejections":["Not enough seeders: 0. Minimum seeders: 1"],
            "customFormatScore": 875
        })];

        assert!(select_verified_release_for_target(
            &releases,
            Target::RadarrMovie { movie_id: 1 },
            &req,
            ServarrLanguageCandidatePolicy::CustomFormatOrTitle
        )
        .is_none());
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

        assert!(select_verified_release_for_target(
            &releases,
            Target::RadarrMovie { movie_id: 1 },
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

        assert!(select_verified_release_for_target(
            &releases,
            Target::RadarrMovie { movie_id: 1 },
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

        let selected = select_verified_release_for_target(
            &releases,
            Target::RadarrMovie { movie_id: 1 },
            &req,
            ServarrLanguageCandidatePolicy::Strict,
        )
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
        assert!(select_verified_release_for_target(
            &releases,
            Target::RadarrMovie { movie_id: 1 },
            &req,
            ServarrLanguageCandidatePolicy::Strict
        )
        .is_none());
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
        assert!(select_verified_release_for_target(
            &releases,
            Target::RadarrMovie { movie_id: 1 },
            &req,
            ServarrLanguageCandidatePolicy::Strict
        )
        .is_some());
    }
}
