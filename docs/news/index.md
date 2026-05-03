# Changelog

## pakhom 1.0.0

### Pipeline Architecture

pakhom uses a progressive sequential coding pipeline faithful to how
manual reflexive thematic analysis works in NVivo and similar QDA
platforms:

1.  **Codebook-first learning** from prior manual analyses (QDPX, Excel,
    CSV)
2.  **Progressive sequential coding** – entries read one at a time,
    coded inline
3.  **Thematic saturation detection** – triangulated stopping criterion
4.  **Code-aware sentiment analysis** – sentiment scored after coding
    with code context
5.  **Iterative bottom-up theme generation** – sequential merging of
    codes into clusters
6.  **Deterministic code-path cascading** – entries map to themes
    through their codes

### Major Features

- Multi-provider AI support (OpenAI GPT-4o, Anthropic Claude Sonnet 4)
- Progressive sequential coding: entries processed one at a time with a
  growing codebook, just like a human researcher in NVivo. No batch
  coding, no separate deduplication or consolidation steps
- Thematic saturation detection using triangulated signals: code
  creation rate monitoring (Guest et al., 2020), code reuse stability
  (De Paoli & Mathis, 2024 adaptation), and AI self-assessment.
  Saturation curve saved for publication
- Iterative bottom-up theme generation: codes are merged through
  multiple sequential passes until no more productive groupings exist.
  Themes and subthemes emerge organically from the data
- Deterministic code-path cascading: entry-to-theme mapping flows
  through the code hierarchy (no AI re-reading of raw text), faithful to
  the inductive process
- Code-aware sentiment analysis: sentiment scored after initial coding,
  using assigned codes as context for more accurate emotional valence
  detection
- Codebook-first learning from prior studies: full theme/subtheme/code
  hierarchy extraction from QDPX files (NVivo exports), Excel, and CSV
  codebooks. Manuscripts serve as supplementary clarification only when
  codebook descriptions are insufficient
- Cross-study qualitative synthesis: structural patterns identified
  across all available prior analyses collectively (not just in
  sequence)
- Researcher review points: pause the pipeline after coding or theme
  generation to curate the AI’s output before continuing
- Checkpoint/resume system with flat checkpoint architecture for
  reliable resume across retries and interruptions
- Inter-rater reliability: Cohen’s kappa and Krippendorff’s alpha with
  small-sample correction for human verification of AI-generated codes
- Reddit scraper with OAuth authentication
- Rich HTML report with interactive DataTables, saturation curves,
  sentiment-coded quotes, theme detail drill-downs, and cross-run
  comparison dashboard
- Cross-run comparison module with 7 analysis dimensions (sample
  overlap, sentiment drift, code stability, theme evolution, entry
  migration, correlation persistence, run dashboard)
- Dynamic token-aware batch sizing for AI operations
- Interactive Shiny configuration wizard for building config.yaml

### Bug Fixes and Improvements

- Fixed: thematic-saturation detection no longer fires prematurely on
  longer runs. The previous implementation derived each code’s
  birth-time-in-coded-entries from per-checkpoint accumulator lists that
  get reset every checkpoint interval; after the first reset the signal
  collapsed toward zero and saturation could be declared even while
  novel codes were still being created. Now stored directly in a
  parallel `code_n_coded_at_birth` map at the moment of code creation
- Added: `analysis_schema_version` field in `run_metadata.json`. The
  cross-run comparison module (`compare_runs`, `compare_models`,
  `list_available_runs`) is now schema-aware: incompatible snapshots are
  excluded with a clear warning rather than silently NA-padded
- Removed: hardcoded medication-research framing from sentiment and
  cross-study-synthesis prompts. The sentiment system prompt no longer
  asserts “qualitative health research” / “clinical experiences /
  treatment effects” regardless of the user’s actual research domain;
  `.synthesize_cross_study_patterns` no longer matches theme names
  against six medication-specific regex categories or injects an
  unconditional medication-narrative-arc claim into the AI’s learning
  context. Replaced with a domain-neutral, evidence-based listing of the
  actual top-level themes from each prior codebook
- Removed: `confidence` from substantive correlation analyses
  (`prepare_correlation_data`, `compare_theme_groups`). The AI sentiment
  prompt elicits confidence and emotion_intensity in the same single
  call, so they co-vary by design (r \>= 0.83 across all observed runs);
  reporting their correlation as a finding misled readers. Confidence
  remains in the per-entry `sentiment_scores.csv` as a diagnostic
- Fixed: `DT` package (in Suggests) now has graceful fallback to
  [`knitr::kable`](https://rdrr.io/pkg/knitr/man/kable.html) when not
  installed
- Fixed: [`.html_esc()`](../reference/dot-html_esc.md) now escapes
  single quotes to prevent XSS in HTML attribute contexts
- Fixed: Variable shadowing of exported
  [`n_themes()`](../reference/n_themes.md) function in report builder
- Replaced [`library(tidyverse)`](https://tidyverse.tidyverse.org) with
  targeted imports in generated Rmd for faster rendering
- Added `error = TRUE` to generated Rmd chunks so individual chunk
  failures don’t prevent report rendering
- Added warnings when [`load_data()`](../reference/load_data.md)
  auto-excludes database tables
- Added small-sample-size warnings for theme membership correlations
- Added methodological notes on saturation criteria and pipeline-wide
  multiple testing in report appendix
- Standardized error messages with
  [`validate_class()`](../reference/validate_class.md) helper
- Added [`list_available_runs()`](../reference/list_available_runs.md)
  convenience function for cross-run comparison
- Added `getting-started` vignette (now also documents Reddit’s
  post-2025 Responsible Builder Program approval requirement for the
  optional scraper)
- Added [`config_wizard_app()`](../reference/config_wizard_app.md) for
  interactive configuration building
- Removed leftover stale references to the pre-1.0 architecture’s
  removed pipeline steps (relevance filtering, batch coding, code
  consolidation, theme assignment) from the report’s learning-
  transparency section, the methodology appendix, and the distributed
  default config template
- Cleaned all R CMD check warnings: documented previously undocumented
  function arguments, declared previously implicit base-stats imports
  (`complete.cases`, `shapiro.test`, `wilcox.test`, `fisher.test`,
  `chisq.test`), removed non-ASCII characters from R sources
