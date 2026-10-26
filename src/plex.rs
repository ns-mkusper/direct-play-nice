//! Plex integration utilities for triggering post-conversion library refresh requests
//! and removing the dead media versions that a replaced file leaves behind.

use crate::config::PlexSettings;
use anyhow::{anyhow, Result};
use log::{debug, info, warn};
use roxmltree::Document;
use std::collections::BTreeSet;
use std::env;
use std::path::{Path, PathBuf};
use std::time::{Duration, Instant};
use ureq::{Agent, AgentBuilder, Error as UreqError};

#[derive(Debug, Clone)]
struct PlexSection {
    id: String,
    title: Option<String>,
    locations: Vec<PathBuf>,
    refreshing: bool,
}

/// How long to wait for Plex to finish scanning a refreshed folder before
/// giving up on removing the replaced file's media version.
const DEFAULT_SCAN_WAIT: Duration = Duration::from_secs(180);
/// Delay between scan status polls while waiting for a refresh to finish.
const DEFAULT_SCAN_POLL_INTERVAL: Duration = Duration::from_secs(2);

#[derive(Debug)]
pub struct PlexRefresher {
    base_url: String,
    token: String,
    agent: Agent,
    remove_replaced_versions: bool,
    scan_wait: Duration,
    scan_poll_interval: Duration,
}

/// A metadata item (episode or movie) listed in a Plex folder view.
#[derive(Debug, Clone)]
struct PlexItem {
    rating_key: String,
    part_files: Vec<String>,
}

/// One media version of a metadata item.
#[derive(Debug, Clone)]
struct PlexMedia {
    id: String,
    deleted: bool,
    part_files: Vec<String>,
}

impl PlexRefresher {
    pub fn from_sources(
        config: Option<&PlexSettings>,
        cli_flag: bool,
        cli_url: Option<&str>,
        cli_token: Option<&str>,
        cli_remove_replaced_versions: Option<bool>,
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

        let remove_replaced_versions = cli_remove_replaced_versions
            .or_else(|| config.and_then(|cfg| cfg.remove_replaced_versions))
            .or_else(|| {
                env::var("DIRECT_PLAY_NICE_PLEX_REMOVE_REPLACED_VERSIONS")
                    .ok()
                    .and_then(parse_boolish)
            })
            .unwrap_or(true);

        let agent = AgentBuilder::new().timeout(Duration::from_secs(10)).build();

        info!("Plex refresh enabled; targeting server at {}.", base_url);

        Ok(Some(PlexRefresher {
            base_url,
            token,
            agent,
            remove_replaced_versions,
            scan_wait: DEFAULT_SCAN_WAIT,
            scan_poll_interval: DEFAULT_SCAN_POLL_INTERVAL,
        }))
    }

    /// Requests a Plex refresh for `new_path`. When `replaced_path` names a
    /// file that the conversion removed, also waits for the scan and removes
    /// that file's dead media version from the Plex item. Cleanup problems
    /// are logged as warnings and never returned as errors.
    pub fn refresh_replaced(&self, new_path: &Path, replaced_path: Option<&Path>) -> Result<()> {
        let sections = self.refresh_path_in_sections(new_path)?;
        let Some(old_path) = replaced_path else {
            return Ok(());
        };
        if !self.remove_replaced_versions || sections.is_empty() {
            return Ok(());
        }
        if path_variants(old_path) == path_variants(new_path) {
            return Ok(());
        }
        for section in &sections {
            if let Err(err) = self.remove_replaced_version(section, old_path, new_path) {
                warn!(
                    "Could not remove the replaced Plex media version for '{}' in section '{}': {}",
                    old_path.display(),
                    section.title.as_deref().unwrap_or(&section.id),
                    err
                );
            }
        }
        Ok(())
    }

    /// Requests a refresh and returns the sections the request was sent to.
    fn refresh_path_in_sections(&self, path: &Path) -> Result<Vec<PlexSection>> {
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

        let sections = self.fetch_sections(true)?;
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
            return Ok(Vec::new());
        }

        let mut refreshed = Vec::new();
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
                refreshed.push(section);
            }
        }

        Ok(refreshed)
    }

    /// Waits for the section scan to list `new_path`, then deletes the media
    /// versions of the matching item(s) that point at `old_path`.
    ///
    /// Safety rules (all must hold for a media version to be deleted):
    /// - one of its parts is exactly the replaced file,
    /// - none of its part files exist on disk (deleting a media version whose
    ///   file exists makes Plex delete that file),
    /// - the item keeps at least one other version that is not marked deleted
    ///   and whose files exist.
    ///
    /// Only `DELETE /library/metadata/{id}/media/{mediaId}` is used; whole
    /// items are never deleted and section trash is never emptied.
    fn remove_replaced_version(
        &self,
        section: &PlexSection,
        old_path: &Path,
        new_path: &Path,
    ) -> Result<()> {
        let old_variants = path_variants(old_path);
        let new_variants = path_variants(new_path);
        let old_folder = old_path
            .parent()
            .ok_or_else(|| anyhow!("'{}' has no parent directory", old_path.display()))?;
        let new_folder = new_path
            .parent()
            .ok_or_else(|| anyhow!("'{}' has no parent directory", new_path.display()))?;
        let old_components = section_relative_components(section, old_folder);
        let new_components = section_relative_components(section, new_folder).ok_or_else(|| {
            anyhow!(
                "'{}' is not inside any location of the section",
                new_folder.display()
            )
        })?;

        let deadline = Instant::now() + self.scan_wait;
        let mut items = loop {
            let refreshing = self.section_refreshing(&section.id)?;
            let items = self.folder_items(&section.id, &new_components)?;
            let new_listed = items
                .iter()
                .any(|item| item.part_files.iter().any(|f| new_variants.contains(f)));
            if new_listed && !refreshing {
                break items;
            }
            if Instant::now() >= deadline {
                warn!(
                    "Plex did not finish scanning '{}' within {}s; leaving any replaced media version of '{}' in place.",
                    new_folder.display(),
                    self.scan_wait.as_secs(),
                    old_path.display()
                );
                return Ok(());
            }
            std::thread::sleep(self.scan_poll_interval);
        };
        if let Some(components) = old_components.filter(|c| *c != new_components) {
            items.extend(self.folder_items(&section.id, &components)?);
        }

        let mut candidates: Vec<String> = items
            .iter()
            .filter(|item| {
                item.part_files
                    .iter()
                    .any(|f| old_variants.contains(f) || new_variants.contains(f))
            })
            .map(|item| item.rating_key.clone())
            .collect();
        candidates.sort();
        candidates.dedup();

        for rating_key in candidates {
            let media = self.item_media(&rating_key)?;
            let dead: Vec<&PlexMedia> = media
                .iter()
                .filter(|m| m.part_files.iter().any(|f| old_variants.contains(f)))
                .collect();
            if dead.is_empty() {
                debug!(
                    "Plex item {} no longer lists '{}'; nothing to remove.",
                    rating_key,
                    old_path.display()
                );
                continue;
            }
            let dead_ids: BTreeSet<&str> = dead.iter().map(|m| m.id.as_str()).collect();
            let has_live_version = media
                .iter()
                .filter(|m| !dead_ids.contains(m.id.as_str()))
                .any(|m| {
                    !m.deleted
                        && !m.part_files.is_empty()
                        && m.part_files.iter().all(|f| Path::new(f).exists())
                });
            if !has_live_version {
                warn!(
                    "Plex item {} has no other available version; keeping the media version for '{}'.",
                    rating_key,
                    old_path.display()
                );
                continue;
            }
            for media in dead {
                if let Some(existing) = media
                    .part_files
                    .iter()
                    .find(|f| !file_is_gone(Path::new(f)))
                {
                    warn!(
                        "Keeping Plex media version {} of item {} because '{}' still exists on disk.",
                        media.id, rating_key, existing
                    );
                    continue;
                }
                match self.delete_media(&rating_key, &media.id) {
                    Ok(()) => info!(
                        "Removed replaced Plex media version {} ('{}') from item {}.",
                        media.id,
                        old_path.display(),
                        rating_key
                    ),
                    Err(err) => warn!(
                        "Failed to remove Plex media version {} of item {}: {}",
                        media.id, rating_key, err
                    ),
                }
            }
        }
        Ok(())
    }

    fn section_refreshing(&self, section_id: &str) -> Result<bool> {
        Ok(self
            .fetch_sections(false)?
            .iter()
            .any(|s| s.id == section_id && s.refreshing))
    }

    /// Walks the section's folder view down `components` and returns the
    /// metadata items listed in that folder. Returns an empty list when the
    /// folder is not (yet) known to Plex.
    fn folder_items(&self, section_id: &str, components: &[String]) -> Result<Vec<PlexItem>> {
        let mut keys = vec![format!("/library/sections/{}/folder", section_id)];
        for component in components {
            let mut next = Vec::new();
            for key in &keys {
                let body = self.get_xml(key, "folder browse")?;
                let doc = parse_xml(&body)?;
                for node in doc.descendants().filter(|n| n.has_tag_name("Directory")) {
                    if node.attribute("title") == Some(component.as_str()) {
                        if let Some(child) = node.attribute("key") {
                            next.push(child.to_string());
                        }
                    }
                }
            }
            if next.is_empty() {
                return Ok(Vec::new());
            }
            keys = next;
        }

        let mut items = Vec::new();
        for key in &keys {
            let body = self.get_xml(key, "folder browse")?;
            let doc = parse_xml(&body)?;
            for node in doc
                .root_element()
                .children()
                .filter(|n| n.is_element() && n.attribute("ratingKey").is_some())
            {
                if node.has_tag_name("Directory") {
                    continue;
                }
                let rating_key = node.attribute("ratingKey").unwrap_or_default().to_string();
                let part_files = node
                    .descendants()
                    .filter(|n| n.has_tag_name("Part"))
                    .filter_map(|n| n.attribute("file").map(str::to_string))
                    .collect();
                items.push(PlexItem {
                    rating_key,
                    part_files,
                });
            }
        }
        Ok(items)
    }

    fn item_media(&self, rating_key: &str) -> Result<Vec<PlexMedia>> {
        let body = self.get_xml(&format!("/library/metadata/{}", rating_key), "item lookup")?;
        let doc = parse_xml(&body)?;
        let item = doc
            .root_element()
            .children()
            .find(|n| n.is_element() && n.attribute("ratingKey") == Some(rating_key))
            .ok_or_else(|| anyhow!("Plex item {} not found", rating_key))?;
        Ok(item
            .children()
            .filter(|n| n.has_tag_name("Media"))
            .filter_map(|m| {
                let id = m.attribute("id")?.to_string();
                Some(PlexMedia {
                    id,
                    deleted: m.attribute("deletedAt").is_some(),
                    part_files: m
                        .children()
                        .filter(|p| p.has_tag_name("Part"))
                        .filter_map(|p| p.attribute("file").map(str::to_string))
                        .collect(),
                })
            })
            .collect())
    }

    fn delete_media(&self, rating_key: &str, media_id: &str) -> Result<()> {
        let url = format!(
            "{}/library/metadata/{}/media/{}",
            self.base_url, rating_key, media_id
        );
        self.agent
            .delete(&url)
            .query("X-Plex-Token", &self.token)
            .call()
            .map_err(|err| map_ureq_error(err, "media version delete"))?;
        Ok(())
    }

    fn get_xml(&self, key: &str, context: &str) -> Result<String> {
        let url = format!("{}{}", self.base_url, key);
        self.agent
            .get(&url)
            .query("X-Plex-Token", &self.token)
            .call()
            .map_err(|err| map_ureq_error(err, context))?
            .into_string()
            .map_err(|err| anyhow!("Failed to read Plex response: {}", err))
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

    /// Lists library sections. `verbose` logs the query at info level; scan
    /// status polling passes `false` to keep the log readable.
    fn fetch_sections(&self, verbose: bool) -> Result<Vec<PlexSection>> {
        let level = if verbose {
            log::Level::Info
        } else {
            log::Level::Debug
        };
        let url = format!("{}/library/sections", self.base_url);
        log::log!(level, "Querying Plex library sections from {}.", url);
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
                refreshing: node.attribute("refreshing") == Some("1"),
            });
        }

        if sections.is_empty() {
            warn!("No Plex library sections discovered at {}.", url);
        }
        log::log!(
            level,
            "Plex library section query returned {} section(s).",
            sections.len()
        );

        Ok(sections)
    }
}

fn parse_xml(body: &str) -> Result<Document<'_>> {
    Document::parse(body).map_err(|err| anyhow!("Failed to parse Plex response XML: {}", err))
}

/// The spellings of `path` Plex may report: as given, and with its parent
/// directory canonicalized (the file itself may already be gone).
fn path_variants(path: &Path) -> BTreeSet<String> {
    let mut variants = BTreeSet::new();
    variants.insert(normalize_path_for_query(path));
    if let (Some(parent), Some(name)) = (path.parent(), path.file_name()) {
        if let Ok(canonical) = parent.canonicalize() {
            variants.insert(normalize_path_for_query(&canonical.join(name)));
        }
    }
    variants
}

/// True only when the file is definitely absent. Errors other than
/// "not found" (for example permission problems) count as present.
fn file_is_gone(path: &Path) -> bool {
    matches!(
        std::fs::symlink_metadata(path),
        Err(err) if err.kind() == std::io::ErrorKind::NotFound
    )
}

/// Folder names from the matching section location down to `folder`, as the
/// Plex folder view titles them.
fn section_relative_components(section: &PlexSection, folder: &Path) -> Option<Vec<String>> {
    let folder = canonicalize_or(folder);
    section.locations.iter().find_map(|loc| {
        let root = canonicalize_or(loc);
        folder.strip_prefix(&root).ok().map(|rel| {
            rel.components()
                .map(|c| c.as_os_str().to_string_lossy().to_string())
                .collect()
        })
    })
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
    use crate::plex::*;

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
        let plex_cfg = PlexSettings {
            refresh: Some(true),
            url: Some("http://localhost:3777".to_string()),
            token: Some("token-from-config".to_string()),
            remove_replaced_versions: None,
        };

        let refresher = PlexRefresher::from_sources(Some(&plex_cfg), false, None, None, None)
            .unwrap()
            .expect("refresher should be enabled");

        assert_eq!(refresher.base_url, "http://localhost:3777");
        assert_eq!(refresher.token, "token-from-config");
    }

    #[test]
    fn cli_overrides_config() {
        let plex_cfg = PlexSettings {
            refresh: Some(true),
            url: Some("http://localhost:3777".to_string()),
            token: Some("token-from-config".to_string()),
            remove_replaced_versions: None,
        };

        let refresher = PlexRefresher::from_sources(
            Some(&plex_cfg),
            false,
            Some("http://override:32400"),
            Some("override-token"),
            None,
        )
        .unwrap()
        .expect("refresher should be enabled");

        assert_eq!(refresher.base_url, "http://override:32400");
        assert_eq!(refresher.token, "override-token");
    }
    #[test]
    fn remove_replaced_versions_defaults_on_and_respects_overrides() {
        let mut plex_cfg = PlexSettings {
            refresh: Some(true),
            url: Some("http://localhost:3777".to_string()),
            token: Some("t".to_string()),
            remove_replaced_versions: None,
        };
        let refresher = PlexRefresher::from_sources(Some(&plex_cfg), false, None, None, None)
            .unwrap()
            .unwrap();
        assert!(refresher.remove_replaced_versions);

        plex_cfg.remove_replaced_versions = Some(false);
        let refresher = PlexRefresher::from_sources(Some(&plex_cfg), false, None, None, None)
            .unwrap()
            .unwrap();
        assert!(!refresher.remove_replaced_versions);

        let refresher = PlexRefresher::from_sources(Some(&plex_cfg), false, None, None, Some(true))
            .unwrap()
            .unwrap();
        assert!(refresher.remove_replaced_versions);
    }

    mod replaced_versions {
        use crate::plex::*;
        use mockito::{Matcher, Mock, Server};
        use tempfile::TempDir;

        struct Fixture {
            _dir: TempDir,
            root: PathBuf,
            old_path: PathBuf,
            new_path: PathBuf,
        }

        fn fixture() -> Fixture {
            let dir = tempfile::tempdir().unwrap();
            // Plex reports plain paths; drop the Windows `\\?\` prefix that
            // canonicalize adds.
            let root = PathBuf::from(normalize_path_for_query(
                &dir.path().canonicalize().unwrap(),
            ));
            let season = root.join("Show").join("Season 1");
            std::fs::create_dir_all(&season).unwrap();
            let old_path = season.join("Show - S01E01.mkv");
            let new_path = season.join("Show - S01E01.fixed.mp4");
            std::fs::write(&new_path, b"converted").unwrap();
            Fixture {
                _dir: dir,
                root,
                old_path,
                new_path,
            }
        }

        fn refresher(server: &Server, remove: bool) -> PlexRefresher {
            PlexRefresher {
                base_url: server.url(),
                token: "tok".to_string(),
                agent: AgentBuilder::new().timeout(Duration::from_secs(5)).build(),
                remove_replaced_versions: remove,
                scan_wait: Duration::from_millis(300),
                scan_poll_interval: Duration::from_millis(20),
            }
        }

        fn media_xml(id: &str, file: &Path, deleted: bool) -> String {
            let deleted_attr = if deleted {
                " deletedAt=\"1700000000\""
            } else {
                ""
            };
            format!(
                "<Media id=\"{id}\"{deleted_attr}><Part id=\"{id}\" file=\"{}\" /></Media>",
                file.display()
            )
        }

        /// Mocks the section list, refresh and folder walk. Returns the mocks
        /// that must stay alive for the duration of the test.
        fn mock_library(
            server: &mut Server,
            fx: &Fixture,
            refreshing: bool,
            folder_media: &str,
        ) -> Vec<Mock> {
            let sections = format!(
                "<MediaContainer><Directory key=\"1\" title=\"Anime\" refreshing=\"{}\"><Location path=\"{}\" /></Directory></MediaContainer>",
                if refreshing { 1 } else { 0 },
                fx.root.display()
            );
            vec![
                server
                    .mock("GET", "/library/sections")
                    .match_query(Matcher::Any)
                    .with_body(sections)
                    .create(),
                server
                    .mock("GET", "/library/sections/1/refresh")
                    .match_query(Matcher::Any)
                    .with_body("")
                    .create(),
                server
                    .mock("GET", "/library/sections/1/folder")
                    .match_query(Matcher::Regex("^X-Plex-Token=tok$".to_string()))
                    .with_body("<MediaContainer><Directory key=\"/library/sections/1/folder?parent=10\" title=\"Show\" /></MediaContainer>")
                    .create(),
                server
                    .mock("GET", "/library/sections/1/folder")
                    .match_query(Matcher::UrlEncoded("parent".into(), "10".into()))
                    .with_body("<MediaContainer><Directory key=\"/library/sections/1/folder?parent=11\" title=\"Season 1\" /></MediaContainer>")
                    .create(),
                server
                    .mock("GET", "/library/sections/1/folder")
                    .match_query(Matcher::UrlEncoded("parent".into(), "11".into()))
                    .with_body(format!(
                        "<MediaContainer><Video ratingKey=\"100\" type=\"episode\">{folder_media}</Video></MediaContainer>"
                    ))
                    .create(),
            ]
        }

        fn mock_item(server: &mut Server, media: &str) -> Mock {
            server
                .mock("GET", "/library/metadata/100")
                .match_query(Matcher::Any)
                .with_body(format!(
                    "<MediaContainer><Video ratingKey=\"100\" type=\"episode\">{media}</Video></MediaContainer>"
                ))
                .create()
        }

        fn mock_item_delete(server: &mut Server) -> Mock {
            server
                .mock("DELETE", "/library/metadata/100")
                .match_query(Matcher::Any)
                .expect(0)
                .create()
        }

        fn mock_media_delete(server: &mut Server, expected: usize) -> Mock {
            server
                .mock("DELETE", "/library/metadata/100/media/1")
                .match_query(Matcher::UrlEncoded("X-Plex-Token".into(), "tok".into()))
                .with_status(200)
                .expect(expected)
                .create()
        }

        fn dead_and_live_media(fx: &Fixture) -> String {
            format!(
                "{}{}",
                media_xml("1", &fx.old_path, true),
                media_xml("2", &fx.new_path, false)
            )
        }

        #[test]
        fn dead_version_is_removed() {
            let fx = fixture();
            let mut server = Server::new();
            let media = dead_and_live_media(&fx);
            let _library = mock_library(&mut server, &fx, false, &media);
            let _item = mock_item(&mut server, &media);
            let item_delete = mock_item_delete(&mut server);
            let media_delete = mock_media_delete(&mut server, 1);

            refresher(&server, true)
                .refresh_replaced(&fx.new_path, Some(&fx.old_path))
                .unwrap();

            media_delete.assert();
            item_delete.assert();
        }

        #[test]
        fn version_whose_file_exists_is_kept() {
            let fx = fixture();
            std::fs::write(&fx.old_path, b"original").unwrap();
            let mut server = Server::new();
            let media = dead_and_live_media(&fx);
            let _library = mock_library(&mut server, &fx, false, &media);
            let _item = mock_item(&mut server, &media);
            let item_delete = mock_item_delete(&mut server);
            let media_delete = mock_media_delete(&mut server, 0);

            refresher(&server, true)
                .refresh_replaced(&fx.new_path, Some(&fx.old_path))
                .unwrap();

            media_delete.assert();
            item_delete.assert();
            assert!(fx.old_path.exists());
        }

        #[test]
        fn item_without_another_live_version_is_left_alone() {
            let fx = fixture();
            let mut server = Server::new();
            let folder_media = dead_and_live_media(&fx);
            let item_media = format!(
                "{}{}",
                media_xml("1", &fx.old_path, true),
                media_xml("2", &fx.new_path, true)
            );
            let _library = mock_library(&mut server, &fx, false, &folder_media);
            let _item = mock_item(&mut server, &item_media);
            let item_delete = mock_item_delete(&mut server);
            let media_delete = mock_media_delete(&mut server, 0);

            refresher(&server, true)
                .refresh_replaced(&fx.new_path, Some(&fx.old_path))
                .unwrap();

            media_delete.assert();
            item_delete.assert();
        }

        #[test]
        fn option_off_sends_no_deletes() {
            let fx = fixture();
            let mut server = Server::new();
            let media = dead_and_live_media(&fx);
            let _library = mock_library(&mut server, &fx, false, &media);
            let item = server
                .mock("GET", "/library/metadata/100")
                .match_query(Matcher::Any)
                .expect(0)
                .create();
            let item_delete = mock_item_delete(&mut server);
            let media_delete = mock_media_delete(&mut server, 0);

            refresher(&server, false)
                .refresh_replaced(&fx.new_path, Some(&fx.old_path))
                .unwrap();

            item.assert();
            media_delete.assert();
            item_delete.assert();
        }

        #[test]
        fn scan_wait_timeout_is_not_an_error() {
            let fx = fixture();
            let mut server = Server::new();
            let media = dead_and_live_media(&fx);
            let _library = mock_library(&mut server, &fx, true, &media);
            let item = server
                .mock("GET", "/library/metadata/100")
                .match_query(Matcher::Any)
                .expect(0)
                .create();
            let item_delete = mock_item_delete(&mut server);
            let media_delete = mock_media_delete(&mut server, 0);

            let result =
                refresher(&server, true).refresh_replaced(&fx.new_path, Some(&fx.old_path));

            assert!(result.is_ok());
            item.assert();
            media_delete.assert();
            item_delete.assert();
        }
    }
}
