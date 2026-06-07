# ==============================================================================
# Package-Level Documentation and Global Imports
# ==============================================================================

#' pakhom: AI-Assisted Thematic Analysis with Methodology-as-Architecture
#'
#' Conducts AI-assisted thematic analysis -- reflexive, codebook, and
#' framework modes -- with methodology
#' codified at the architectural level. Three methodologically-distinct
#' operating modes (Reflexive Scaffold, Codebook Collaborative,
#' Framework Applied) shape the AI's role explicitly so the chosen
#' epistemic stance is visible to reviewers, replicable across runs,
#' and stamped onto every output.
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
#' @section Three methodology modes:
#' Each mode encodes a different posture for AI agency. The mode
#' declaration is mandatory in every config (no default); it is locked
#' at run start, stamped on every output, and any change creates a
#' fork run with parent_run_id linkage.
#' \describe{
#'   \item{\code{reflexive_scaffold} (Mode 1)}{AI as Socratic gadfly
#'     (Sarkar 2024). The researcher authors codes and themes (typically
#'     in NVivo / ATLAS.ti); pakhom contributes the provocateur loop
#'     that surfaces counter-narratives, absent voices, alternative
#'     interpretations, disconfirming evidence, and assumption-surfacing
#'     terms. The AI never names themes or codes. Use \code{\link{run_mode1}}.
#'     Per AC6 (symmetric obligations), Mode 1's burden parity with
#'     Modes 2/3 is delivered through \emph{reflexive memos} -- typed
#'     Markdown notes round-tripped via YAML frontmatter
#'     (\code{\link{add_memo}}, \code{\link{persist_memos}}).}
#'   \item{\code{codebook_collaborative} (Mode 2)}{AI proposes codes;
#'     researcher gates each at the codebook + theme review pause-points.
#'     The auto-pipeline of \code{\link{run_analysis}}; this is what users
#'     coming from a codebook TA / template TA tradition will recognize.
#'     IRR + saturation are quality diagnostics; researcher review points
#'     interleave with the AI's progressive coding pass.}
#'   \item{\code{framework_applied} (Mode 3)}{Researcher provides a
#'     theoretical framework (e.g., Theory of Planned Behavior, COM-B,
#'     Theoretical Domains Framework -- pre-built specs ship in
#'     \code{inst/extdata/frameworks/}); AI applies it verbatim and
#'     flags entries that resist the framework as anomalies per the
#'     framework's anomaly_handling policy. Use \code{\link{run_analysis}}
#'     with \code{config$methodology$framework_spec_path} set. The
#'     framework spec is archived byte-equivalently into the run dir
#'     and its sha256 is stamped into \code{run_metadata.json}.}
#' }
#'
#' @section Architectural commitments (AC1-AC10):
#' These commitments are load-bearing and do not weaken across modes.
#' \itemize{
#'   \item \strong{AC1}: AI is scaffold by architecture, not by configuration.
#'   \item \strong{AC2}: Three modes; no fourth.
#'   \item \strong{AC3}: No default mode; explicit declaration mandatory.
#'   \item \strong{AC4}: Methodology stamped on every output (ClinicalTrials.gov
#'     pattern -- run_metadata.json, every CSV/JSON header, HTML stamp).
#'   \item \strong{AC5}: Soft-lock with audit trail; methodology change
#'     creates a new run with parent_run_id linkage (REDCap dev/production
#'     pattern).
#'   \item \strong{AC6}: Symmetric researcher-burden obligations across
#'     modes (anti-gaming).
#'   \item \strong{AC7}: Universal Tier-0 transparency requirements
#'     (T0.1 quote provenance, T0.2 participant spread, T0.3 coverage)
#'     in all modes.
#'   \item \strong{AC8}: Modes are configurations of one architecture,
#'     never separate code paths.
#'   \item \strong{AC9}: Methodology rules generated from config and
#'     injected into the model context every turn.
#'   \item \strong{AC10}: Stage-gating via filesystem state.
#' }
#'
#' @section Tier-0 universal transparency requirements:
#' Three commitments mandatory in every mode, addressing the most-cited
#' empirical critiques of LLM-for-TA tools.
#' \describe{
#'   \item{T0.1 -- Quote provenance + 4-step verification ladder}{Every
#'     AI-attributed verbatim claim runs through strict offline match,
#'     normalized match, substring search, and embedding similarity.
#'     Fabricated quotes are dropped silently and logged to
#'     \code{fabrication_log.csv}. Mode 1 + Anthropic + framework_applied
#'     constraints handled per provider.}
#'   \item{T0.2 -- Participant spread per theme}{Every theme reports
#'     n_distinct_contributors + Gini coefficient + top contributor share,
#'     so themes that look prevalent but rest on one heavy poster get
#'     surfaced (Jowsey et al. 2025 "Frankenstein" finding).}
#'   \item{T0.3 -- Whole-corpus coverage assertion}{Modes 2/3 assert
#'     "no silent truncation in the LLM call path" via \code{\link{compute_corpus_coverage}};
#'     Mode 1 asserts "no silent skip across themes x provocation
#'     categories" via \code{\link{compute_mode1_coverage}}. Both inherit
#'     a virtual \code{Tier0Coverage} parent class so the report
#'     dispatches uniformly via \code{\link{render_tier0_coverage_card}}.}
#' }
#'
#' @section Main entry points:
#' \describe{
#'   \item{\code{\link{run_analysis}}}{Modes 2 + 3 orchestrator (data load
#'     -> coding -> sentiment -> themes -> correlations -> report ->
#'     finalize_run).}
#'   \item{\code{\link{run_mode1}}}{Mode 1 orchestrator (provocateur
#'     loop + memos + Mode 1 report). Mirrors run_analysis's scaffolding
#'     but routes through \code{\link{run_provocateur_questioning}}.}
#'   \item{\code{\link{create_config}} / \code{\link{config_wizard_app}}}{Create
#'     a config programmatically or via a Shiny wizard.}
#'   \item{\code{\link{load_framework_spec}}}{Load a theoretical framework
#'     for Mode 3 (built-in: \code{"tpb"}, \code{"comb"}, \code{"tdf"}).}
#'   \item{\code{\link{add_memo}} / \code{\link{persist_memos}}}{Mode 1
#'     reflexive memo CRUD + Markdown round-trip.}
#'   \item{\code{\link{compare_runs}} / \code{\link{compare_models}}}{Cross-run
#'     and inter-model reliability comparisons.}
#' }
#'
#' @section Provider support:
#' OpenAI (GPT-4o family) and Anthropic (Claude family) with a unified
#' \code{\link{ai_complete}} abstraction. Mode 3 + Anthropic structurally
#' precludes the Citations API (forced \code{tool_use} schema and
#' Citations API output are mutually exclusive on the same response);
#' the Mode 3 report renders an explicit footnote disclosing this rather
#' than letting reviewers infer a bug.
#'
#' @section Further reading:
#' \itemize{
#'   \item \code{vignette("getting-started")} -- step-by-step Mode 2 walkthrough
#'   \item \code{vignette("methodology-modes")} -- choosing between the
#'     three modes; worked examples for each
#'   \item Sarkar 2024 (CACM) "AI Should Challenge, Not Obey" -- Mode 1 motivation
#'   \item Braun and Clarke 2022 -- reflexive TA foundation
#'   \item Jowsey et al. 2025 (PLOS One, doi:10.1371/journal.pone.0330217)
#'     -- the "Frankenstein" finding that motivated Tier-0
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
# Removed .DEFAULT_SIMILARITY_THRESHOLD <- 0.85 --
# was declared but never read. The two distinct similarity thresholds
# in the package (.QUOTE_EMBEDDING_VERIFICATION_THRESHOLD in
# R/quote_provenance.R for T0.1 verification, and the embedding-
# similarity hint in R/13_themes.R::.run_merge_pass) carry their own
# cited values.

# Maximum per-entry text length passed to the LLM during coding. Long
# entries are truncated to this cap before being included in a prompt;
# the T0.1 verification ladder still validates against the FULL
# untruncated text so any quote the model fabricates from beyond-cap
# content gets dropped. The manuscript-learning module no
# longer shares this constant -- it carries its own cap
# (`max_manuscript_chars`, default 12000, configurable via
# config$learning$max_manuscript_chars). This constant is now the
# floor for the context-window-aware cap (ai.max_entry_chars).
.MAX_ENTRY_CHARS <- 8000L

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
  # Used inside dplyr filter() in
  # interpret_correlations() in R/16_report_helpers.R
  "meaningful_effect"
))
