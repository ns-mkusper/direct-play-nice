use crate::config::PlexSettings;
use anyhow::{anyhow, Result};
use log::{debug, info, warn};
use roxmltree::Document;
use std::env;
use std::path::{Path, PathBuf};
use std::time::Duration;
use ureq::{Agent, AgentBuilder, Error as UreqError};

#[derive(Debug, Clone)]
struct PlexSection {
    id: String,
    title: Option<String>,
    locations: Vec<PathBuf>,
}

#[derive(Debug)]
pub struct PlexRefresher {
    base_url: String,
    token: String,
    agent: Agent,
}

impl PlexRefresher {
    pub fn from_sources(
        config: Option<&PlexSettings>,
        cli_flag: bool,
        cli_url: Option<&str>,
        cli_token: Option<&str>,
    ) -> Result<Option<Self>> {
        let config_flag = config.and_then(|cfg| cfg.refresh);
        let env_flag = env::var("DIRECT_PLAY_NICE_PLEX_REFRESH")
            .ok()
            .and_then(parse_boolish);
        let mut refresh_enabled = cli_flag;
        if !refresh_enabled {
            if let Some(flag) = config_flag {
                refresh_enabled = flag;
            }
        }
        if !refresh_enabled {
            if let Some(flag) = env_flag {
                refresh_enabled = flag;
            }
        }

        let base_url = cli_url
            .map(str::to_string)
            .or_else(|| config.and_then(|cfg| cfg.url.clone()))
            .or_else(|| env::var("DIRECT_PLAY_NICE_PLEX_URL").ok())
            .or_else(|| env::var("PLEX_URL").ok());

        let token = cli_token
            .map(str::to_string)
            .or_else(|| config.and_then(|cfg| cfg.token.clone()))
            .or_else(|| env::var("DIRECT_PLAY_NICE_PLEX_TOKEN").ok())
            .or_else(|| env::var("PLEX_TOKEN").ok());

        if !refresh_enabled && base_url.is_some() && token.is_some() {
            refresh_enabled = true;
        }

        if !refresh_enabled {
            return Ok(None);
        }

        let token = token.ok_or_else(|| {
            anyhow!(
                "Plex refresh requested but no token provided. \
                 Supply --plex-token or set DIRECT_PLAY_NICE_PLEX_TOKEN / PLEX_TOKEN."
            )
        })?;

        let base_url = base_url
            .unwrap_or_else(|| String::from("http://127.0.0.1:32400"))
            .trim_end_matches('/')
            .to_string();

        let agent = AgentBuilder::new().timeout(Duration::from_secs(10)).build();

        info!("Plex refresh enabled; targeting server at {}.", base_url);

        Ok(Some(PlexRefresher {
            base_url,
            token,
            agent,
        }))
    }

    pub fn refresh_path(&self, path: &Path) -> Result<()> {
        let canonical = canonicalize_or(path);
        let target_dir = if canonical.is_dir() {
            canonical
        } else {
            canonical.parent().map(Path::to_path_buf).ok_or_else(|| {
                anyhow!(
                    "Unable to determine parent directory for '{}'",
                    path.display()
                )
            })?
        };

        let canonical_dir = canonicalize_or(&target_dir);
        let dir_for_query = normalize_path_for_query(&canonical_dir);

        let sections = self.fetch_sections()?;
        let mut matched_sections = Vec::new();
        for section in &sections {
            if section
                .locations
                .iter()
                .any(|loc| directory_within(&canonical_dir, loc))
            {
                matched_sections.push(section.clone());
            }
        }

        if matched_sections.is_empty() {
            warn!(
                "Plex refresh skipped: no library section matches '{}'.",
                target_dir.display()
            );
            return Ok(());
        }

        for section in matched_sections {
            if let Err(err) = self.refresh_section(&section, &dir_for_query) {
                warn!(
                    "Failed to trigger Plex refresh for '{}' in section '{}': {}",
                    target_dir.display(),
                    section.title.as_deref().unwrap_or(&section.id),
                    err
                );
            } else {
                info!(
                    "Requested Plex refresh for '{}' (section '{}').",
                    target_dir.display(),
                    section.title.as_deref().unwrap_or(&section.id)
                );
            }
        }

        Ok(())
    }

    fn refresh_section(&self, section: &PlexSection, dir_for_query: &str) -> Result<()> {
        let url = format!("{}/library/sections/{}/refresh", self.base_url, section.id);
        info!(
            "Requesting Plex refresh at {} for path '{}'.",
            url, dir_for_query
        );
        let response = self
            .agent
            .get(&url)
            .query("path", dir_for_query)
            .query("X-Plex-Token", &self.token)
            .call()
            .map_err(|err| map_ureq_error(err, "refresh request"))?;

        let status = response.status();
        if status >= 400 {
            return Err(anyhow!("Plex refresh returned HTTP {}", status));
        }

        Ok(())
    }

    fn fetch_sections(&self) -> Result<Vec<PlexSection>> {
        let url = format!("{}/library/sections", self.base_url);
        info!("Querying Plex library sections from {}.", url);
        let response = self
            .agent
            .get(&url)
            .query("X-Plex-Token", &self.token)
            .call()
            .map_err(|err| map_ureq_error(err, "fetch sections"))?;

        let body = response
            .into_string()
            .map_err(|err| anyhow!("Failed to read Plex response: {}", err))?;

        let doc = Document::parse(&body)
            .map_err(|err| anyhow!("Failed to parse Plex response XML: {}", err))?;

        let mut sections = Vec::new();
        for node in doc.descendants().filter(|n| n.has_tag_name("Directory")) {
            let key = match node.attribute("key") {
                Some(k) => k.to_string(),
                None => continue,
            };
            let title = node.attribute("title").map(str::to_string);
            let mut locations = Vec::new();
            for loc in node.children().filter(|n| n.has_tag_name("Location")) {
                if let Some(path_attr) = loc.attribute("path") {
                    locations.push(PathBuf::from(path_attr));
                }
            }
            if locations.is_empty() {
                debug!(
                    "Skipping Plex section '{}' (key {}) without locations.",
                    title.as_deref().unwrap_or(&key),
                    key
                );
                continue;
            }
            sections.push(PlexSection {
                id: key,
                title,
                locations,
            });
        }

        if sections.is_empty() {
            warn!("No Plex library sections discovered at {}.", url);
        }
        info!(
            "Plex library section query returned {} section(s).",
            sections.len()
        );

        Ok(sections)
    }
}

fn parse_boolish(input: String) -> Option<bool> {
    let trimmed = input.trim().to_ascii_lowercase();
    match trimmed.as_str() {
        "1" | "true" | "yes" | "on" | "enable" | "enabled" => Some(true),
        "0" | "false" | "no" | "off" | "disable" | "disabled" => Some(false),
        _ => None,
    }
}

fn canonicalize_or(path: &Path) -> PathBuf {
    match path.canonicalize() {
        Ok(p) => p,
        Err(_) => PathBuf::from(path),
    }
}

fn directory_within(canonical_target: &Path, root: &Path) -> bool {
    let root = canonicalize_or(root);
    canonical_target.starts_with(&root)
}

fn normalize_path_for_query(path: &Path) -> String {
    let mut repr = path.to_string_lossy().to_string();
    if cfg!(windows) {
        if let Some(stripped) = repr.strip_prefix(r"\\?\") {
            repr = stripped.to_string();
        }
    }
    repr
}

fn map_ureq_error(err: UreqError, context: &str) -> anyhow::Error {
    match err {
        UreqError::Status(code, resp) => {
            let body = resp.into_string().unwrap_or_default();
            if body.is_empty() {
                anyhow!("Plex {} failed with HTTP {}", context, code)
            } else {
                anyhow!("Plex {} failed with HTTP {}: {}", context, code, body)
            }
        }
        UreqError::Transport(transport) => {
            anyhow!("Plex {} transport error: {}", context, transport)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_boolish_true_values() {
        for val in ["1", "true", "YES", "Enabled"] {
            assert_eq!(parse_boolish(val.to_string()), Some(true));
        }
    }

    #[test]
    fn parse_boolish_false_values() {
        for val in ["0", "false", "NO", "Disabled"] {
            assert_eq!(parse_boolish(val.to_string()), Some(false));
        }
    }

    #[test]
    fn parse_boolish_unknown() {
        assert_eq!(parse_boolish("maybe".to_string()), None);
    }

    #[test]
    fn refresher_uses_config_defaults() {
        let mut plex_cfg = PlexSettings::default();
        plex_cfg.refresh = Some(true);
        plex_cfg.url = Some("http://localhost:3777".to_string());
        plex_cfg.token = Some("token-from-config".to_string());

        let refresher = PlexRefresher::from_sources(Some(&plex_cfg), false, None, None)
            .unwrap()
            .expect("refresher should be enabled");

        assert_eq!(refresher.base_url, "http://localhost:3777");
        assert_eq!(refresher.token, "token-from-config");
    }

    #[test]
    fn cli_overrides_config() {
        let mut plex_cfg = PlexSettings::default();
        plex_cfg.refresh = Some(true);
        plex_cfg.url = Some("http://localhost:3777".to_string());
        plex_cfg.token = Some("token-from-config".to_string());

        let refresher = PlexRefresher::from_sources(
            Some(&plex_cfg),
            false,
            Some("http://override:32400"),
            Some("override-token"),
        )
        .unwrap()
        .expect("refresher should be enabled");

        assert_eq!(refresher.base_url, "http://override:32400");
        assert_eq!(refresher.token, "override-token");
    }
}
