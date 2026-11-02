//! Dry-run reporting: describes what a conversion or replacement would do without touching any file.

use std::path::PathBuf;

use serde::Serialize;

use crate::servarr::{IntegrationKind, ReplacePlan};
use crate::types::OutputFormat;

/// What the run would do to the input once dry-run is removed.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum DryRunAction {
    /// Input already satisfies the device constraints; nothing would be written.
    Skip,
    /// Video and/or audio would be transcoded into the output.
    Transcode,
    /// Streams would be copied and bitmap subtitles OCR'd into the output.
    RemuxSubtitles,
}

impl DryRunAction {
    fn label(self) -> &'static str {
        match self {
            DryRunAction::Skip => "skip (already direct-play compatible)",
            DryRunAction::Transcode => "transcode",
            DryRunAction::RemuxSubtitles => "remux with OCR subtitles",
        }
    }
}

/// How the source file would be treated after a successful run.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
#[serde(rename_all = "kebab-case")]
pub(crate) enum SourceHandling {
    /// The source stays where it is.
    Keep,
    /// `--delete-source` removes the source after a verified conversion.
    DeleteAfterVerify,
    /// Servarr mode moves the source to a backup path, promotes the output, then removes the backup.
    ReplaceViaBackup,
}

impl SourceHandling {
    fn label(self) -> &'static str {
        match self {
            SourceHandling::Keep => "left untouched",
            SourceHandling::DeleteAfterVerify => {
                "deleted after the output passes verification (--delete-source)"
            }
            SourceHandling::ReplaceViaBackup => {
                "moved to the backup path, then removed once the output is promoted"
            }
        }
    }
}

/// Everything the run resolved before it would have started writing.
#[derive(Debug, Clone, Serialize)]
pub(crate) struct DryRunReport {
    pub mode: String,
    pub action: DryRunAction,
    pub input: PathBuf,
    pub output: PathBuf,
    pub temp_outputs: Vec<PathBuf>,
    pub backup: Option<PathBuf>,
    pub source_handling: SourceHandling,
    pub reasons: Vec<String>,
    pub target_devices: Vec<String>,
    pub video_codec: String,
    pub audio_codec: String,
    pub container: String,
    pub subtitle_ocr_pass: bool,
    /// AI upscale model description when the opt-in path is enabled.
    pub ai_upscale: Option<String>,
}

impl DryRunReport {
    pub(crate) fn mode_for(plan: Option<&ReplacePlan>) -> String {
        match plan {
            Some(plan) => format!(
                "{} {} event",
                match plan.kind {
                    IntegrationKind::Sonarr => "Sonarr",
                    IntegrationKind::Radarr => "Radarr",
                },
                plan.event_type
            ),
            None => "direct conversion".to_string(),
        }
    }

    pub(crate) fn render(&self, format: OutputFormat) -> String {
        match format {
            OutputFormat::Json => serde_json::to_string_pretty(self)
                .unwrap_or_else(|err| format!("{{\"error\": \"{err}\"}}")),
            OutputFormat::Text => self.render_text(),
        }
    }

    fn render_text(&self) -> String {
        let mut out = String::new();
        out.push_str("Dry run: no file will be written, renamed, or deleted.\n");
        out.push_str(&format!("  Mode:            {}\n", self.mode));
        out.push_str(&format!("  Input:           {}\n", self.input.display()));
        out.push_str(&format!("  Action:          {}\n", self.action.label()));
        for reason in &self.reasons {
            out.push_str(&format!("                   - {reason}\n"));
        }
        if self.action != DryRunAction::Skip {
            out.push_str(&format!("  Output:          {}\n", self.output.display()));
            for temp in &self.temp_outputs {
                out.push_str(&format!("  Temp output:     {}\n", temp.display()));
            }
            if let Some(backup) = &self.backup {
                out.push_str(&format!("  Backup:          {}\n", backup.display()));
            }
        }
        out.push_str(&format!(
            "  Source:          {}\n",
            self.source_handling.label()
        ));
        out.push_str(&format!(
            "  Target devices:  {}\n",
            self.target_devices.join(", ")
        ));
        out.push_str(&format!(
            "  Codecs:          video {}, audio {}, container {}\n",
            self.video_codec, self.audio_codec, self.container
        ));
        out.push_str(&format!(
            "  Subtitle OCR:    {}\n",
            if self.subtitle_ocr_pass {
                "enabled"
            } else {
                "disabled"
            }
        ));
        if let Some(model) = &self.ai_upscale {
            out.push_str(&format!("  AI upscale:      {model}\n"));
        }
        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample(action: DryRunAction) -> DryRunReport {
        DryRunReport {
            mode: "direct conversion".to_string(),
            action,
            input: PathBuf::from("/media/in.mkv"),
            output: PathBuf::from("/media/out.mp4"),
            temp_outputs: vec![PathBuf::from("/media/out.conv.mp4")],
            backup: None,
            source_handling: SourceHandling::Keep,
            reasons: vec!["video codec hevc is not supported".to_string()],
            target_devices: vec!["Chromecast (1st gen)".to_string()],
            video_codec: "h264".to_string(),
            audio_codec: "aac".to_string(),
            container: "mp4".to_string(),
            subtitle_ocr_pass: true,
            ai_upscale: None,
        }
    }

    #[test]
    fn text_report_lists_paths_and_reasons() {
        let text = sample(DryRunAction::Transcode).render(OutputFormat::Text);
        assert!(text.starts_with("Dry run: no file will be written"));
        assert!(text.contains("Action:          transcode"));
        assert!(text.contains("- video codec hevc is not supported"));
        assert!(text.contains("Output:          /media/out.mp4"));
        assert!(text.contains("Temp output:     /media/out.conv.mp4"));
        assert!(text.contains("Source:          left untouched"));
    }

    #[test]
    fn skip_report_omits_output_paths() {
        let mut report = sample(DryRunAction::Skip);
        report.reasons.clear();
        let text = report.render(OutputFormat::Text);
        assert!(text.contains("already direct-play compatible"));
        assert!(!text.contains("Output:"));
        assert!(!text.contains("Temp output:"));
    }

    #[test]
    fn json_report_round_trips_enums_as_kebab_case() {
        let json = sample(DryRunAction::RemuxSubtitles).render(OutputFormat::Json);
        let value: serde_json::Value = serde_json::from_str(&json).unwrap();
        assert_eq!(value["action"], "remux-subtitles");
        assert_eq!(value["source_handling"], "keep");
        assert_eq!(value["output"], "/media/out.mp4");
        assert_eq!(value["subtitle_ocr_pass"], true);
    }
}
