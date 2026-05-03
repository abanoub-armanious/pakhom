# ==============================================================================
# Package-Level Documentation and Global Imports
# ==============================================================================

#' pakhom: AI-Assisted Reflexive Thematic Analysis
#'
#' Conducts AI-assisted reflexive thematic analysis following Braun & Clarke's
#' approach. Uses progressive sequential coding, iterative bottom-up theme
#' generation, and deterministic code-path cascading. Supports OpenAI and
#' Anthropic providers, codebook-first learning from prior studies,
#' checkpoint/resume, and publication-quality HTML reporting.
#'
#' @section The name:
#' \strong{pakhom} (Coptic \emph{eagle}) is the native Coptic Egyptian form
#' of the name of Saint Pachomius the Great (c. 292-348 CE), the desert
#' abbot whose written Rule established the genre of
#' methodology-as-written-document in Christian tradition. The Pachomian
#' Rule was the first codified framework for organized communal practice;
#' it transformed the unruly anchorite tradition into reproducible,
#' inspectable, transmissible discipline. This package is a digital
#' descendant of that tradition: AI behavior in qualitative analysis is
#' constrained at the architectural level by methodologically-coherent
#' rules, not at the configuration level by user discipline. Pakhom
#' codified the Rule; pakhom codifies the methodology-as-permission-
#' structure.
#'
#' @section Main function:
#' \code{\link{run_analysis}} — orchestrates the full pipeline from a YAML config.
#'
#' @section Key features:
#' \itemize{
#'   \item Progressive sequential coding (entries read one at a time, like NVivo)
#'   \item Thematic saturation detection (triangulated: code creation rate,
#'         reuse stability, AI self-assessment)
#'   \item Iterative bottom-up theme generation (codes merged into clusters
#'         through multiple passes until convergence)
#'   \item Deterministic code-path cascading (entries map to themes through codes)
#'   \item Code-aware sentiment analysis (sentiment scored after coding, using
#'         codes as context)
#'   \item Codebook-first learning from prior studies (QDPX, Excel, CSV codebooks
#'         with full theme/subtheme/code hierarchies)
#'   \item Researcher review points (pause after coding or theme generation)
#'   \item Checkpoint/resume for long-running analyses
#'   \item Correlation analysis of theme-sentiment relationships
#'   \item Anti-fabrication: every AI-attributed verbatim claim verified
#'         against source via a four-step ladder; fabricated quotes dropped
#'         and logged (Sprint-4 T0.1)
#'   \item Publication-quality HTML report with Tier-0 data integrity
#'         dashboard, saturation curves, theme narratives, and interactive
#'         tables
#' }
#'
#' @name pakhom-package
#' @aliases pakhom
"_PACKAGE"

# -- Global imports used across multiple files ---------------------------------

#' @import dplyr
#' @import tibble
#' @importFrom rlang .data %||%
#' @importFrom logger log_info log_warn log_error log_debug
#' @importFrom tictoc tic toc
#' @importFrom jsonlite fromJSON toJSON write_json
#' @importFrom stringdist stringdist
#' @importFrom utils head tail
#' @importFrom stats cor cor.test var median sd p.adjust setNames runif quantile qnorm
#' @importFrom stats complete.cases shapiro.test wilcox.test fisher.test chisq.test
#' @importFrom graphics par hist
#' @importFrom grDevices png colorRampPalette dev.off
#' @importFrom ggplot2 ggplot aes geom_histogram
#' @importFrom scales comma percent
#' @importFrom knitr kable
NULL

# -- Package-level constants (fallback defaults for config-driven values) -------
.SENTIMENT_NEGATIVE_THRESHOLD <- -0.2
.SENTIMENT_POSITIVE_THRESHOLD <- 0.2
.DEFAULT_SIMILARITY_THRESHOLD <- 0.85

# -- Methodology modes (multi-mode architecture, T1.3) -------------------------
# Each mode encodes a methodologically coherent posture for AI agency:
#   * reflexive_scaffold     -- AI as provocateur; researcher does all
#                               meaning-making. Aligned with Braun & Clarke
#                               2022 reflexive TA + Jowsey et al. 2025
#                               critique (AI never produces findings).
#   * codebook_collaborative -- AI proposes codes; researcher gates each.
#                               Codebook ships as researcher's deliverable.
#                               IRR + saturation as quality diagnostics.
#                               For codebook TA / template TA / Big-Q-friendly
#                               framework analysis.
#   * framework_applied      -- Researcher provides theoretical framework;
#                               AI applies it; researcher reviews. Anomaly
#                               tracking critical (codes that resist the
#                               framework get flagged for theoretical
#                               revision). Absorbs content-analytic use case
#                               via positivist framework choice.
# See vignette("methodology-modes") (forthcoming) for the epistemological
# rationale and the mapping to Cognitio Emergens Agency Configurations
# (Lin 2025, arXiv:2505.03105) and Prahl ARC postures (Qual Health Res 2026,
# doi:10.1177/10497323251401503).
.METHODOLOGY_MODE_REFLEXIVE_SCAFFOLD <- "reflexive_scaffold"
.METHODOLOGY_MODE_CODEBOOK_COLLABORATIVE <- "codebook_collaborative"
.METHODOLOGY_MODE_FRAMEWORK_APPLIED <- "framework_applied"
.VALID_METHODOLOGY_MODES <- c(
  .METHODOLOGY_MODE_REFLEXIVE_SCAFFOLD,
  .METHODOLOGY_MODE_CODEBOOK_COLLABORATIVE,
  .METHODOLOGY_MODE_FRAMEWORK_APPLIED
)

# Suppress R CMD check notes for dplyr pronouns
utils::globalVariables(c(
  ".", "n", "theme_name", "emerged_themes", "sentiment_score",
  "all_emotions", "emotion", "emotion_intensity", "confidence", "frequency",
  "code_text", "code_type", "significant", "correlation", "effect_size",
  "p_value", "var1", "var2", "source_table", "std_text",
  "std_id", "original_text",
  "entry", "code_lower", "code_key", "entry_ids", "excerpts",
  "code_stem", "text",
  # Cross-run comparison
  "theme_prev", "theme_curr", "theme_name", "n_entries",
  "pair_key", "n_runs_significant", "mean_correlation",
  "shift", "reclassified", "run_idx", "n_runs_present",
  "sentiment_score_curr", "sentiment_score_prev",
  # OS.2 (correlation reframe): used inside dplyr filter() in
  # interpret_correlations() in R/16_report_helpers.R
  "meaningful_effect"
))
