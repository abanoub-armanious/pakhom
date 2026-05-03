# pakhom 1.0.0

## Sprint-4 phase 31: Mode 1 (Reflexive Scaffold) full orchestrator

Closes the phase 30 audit's CRITICAL findings C1 + C2: Mode 1 was
operational at the provocateur-loop level (`run_provocateur_questioning`)
but lacked the AC4 + AC7 scaffolding that Modes 2/3 have. A Mode 1 run
emitted only a `ResearcherReflectionLog` -- no `run_metadata.json`, no
T0.2 spread, no T0.3 coverage, no rendered report, no `finalize_run()`
call. AC4 ("methodology stamped on every output") and AC7 ("universal
Tier-0 in all modes") were aspirational for Mode 1; phase 31 makes them
operational.

- **`run_mode1()` orchestrator** (new `R/mode1_orchestrator.R`). Top-level
  Mode 1 entry point that mirrors `run_analysis()`'s scaffolding (output
  dir + run metadata + methodology rules + audit log + fabrication log
  + finalize_run + integrity check) but routes through the provocateur
  loop instead of progressive coding. Produces a complete Mode 1 run
  directory under `outputs/<run-id>_M1/` with every Tier-0/Tier-1
  artifact a reviewer would expect, plus Mode 1-specific canonical
  artifacts: `reflection_log.json`, `provocations.csv`,
  `provocation_attempts.csv`, `themes.json`, `coverage_mode1.json`.

- **`compute_mode1_coverage()` + `ProvocationCoverage` S3** (new in
  `R/mode1_orchestrator.R`). Mode 1's analog of T0.3. Where Mode 2/3 assert
  "no silent truncation in the LLM call path" (every preprocessed entry
  reached the LLM), Mode 1 asserts **"no silent skip across themes ×
  provocation categories"** + "the full corpus was provided to per-
  category prompts." Distinguishes legitimate empty results (a category
  that returned zero provocations because no qualifying entries existed)
  from silent skips (a category that was never attempted) -- the central
  semantic that lets the coverage card make a defensible claim.
  `ProvocationCoverage` shares a `Tier0Coverage` virtual parent class
  with `CorpusCoverage` so the report renderer dispatches uniformly via
  the new `render_tier0_coverage_card()` S3 generic.

- **`generate_mode1_report()`** (new `R/mode1_report.R`). Mode 1-specific
  HTML report renderer. The existing `generate_report()` is wired to
  coding_state + sentiment + correlations + AI synthesis -- none of
  which exist in Mode 1, and stubbing them out would risk silent Mode 2/3
  regressions. The Mode 1 report instead reuses atomic helpers
  (`stamp_methodology_html`, `.build_tier0_dashboard`,
  `.build_participant_spread_card`) and adds Mode 1-specific section
  builders for per-theme provocations grouped by category, the Mode 1
  coverage card via `render_tier0_coverage_card()` S3 dispatch, and a
  deterministic executive summary that surfaces top categories, themes
  attracting the most disconfirming evidence, and participant-
  concentration flags.

- **`compute_provocation_provenance_stats()`** (new). Mode 1's analog of
  `compute_quote_provenance_stats()`. Walks
  `reflection_log$provocations`, extracts each provocation's
  `QuoteProvenance` field (built and verified by the per-category
  function via `.citation_to_provocation`), and feeds them through
  `quote_provenance_summary()`. Provocations from observational
  categories (absent_voice, parts of assumption_surfacing) carry NULL
  provenance and are excluded from the verification stats -- the Tier-0
  dashboard's domain is verbatim claims.

- **ResearcherReflectionLog schema 1.1.0**: adds `provocation_attempts`
  and `skipped_themes` data.frames. The attempt tracker records one row
  per (theme × category) attempt regardless of how many provocations the
  AI emitted -- the row's existence proves "not silently skipped" while
  the `n_emitted` column measures emission. The skipped_themes tracker
  records themes the orchestrator bypassed with a stated reason (e.g.,
  zero supporting entries) so the coverage card distinguishes
  *explicit skip with stated reason* from *silent skip*. Backward-
  compatible: 1.0.0 logs loaded as `resume_log` have the new slots
  backfilled empty.

- **`render_tier0_coverage_card()` S3 generic** (new in
  `R/corpus_coverage.R`). Single dispatch entry point for the report's
  Tier-0 coverage card; methods on `CorpusCoverage` (Mode 2/3, in
  `R/17_report.R`) and `ProvocationCoverage` (Mode 1, in
  `R/mode1_orchestrator.R`) keep the call site in `.build_rmd_content`
  branch-free. The legacy `.build_corpus_coverage_card()` is preserved as
  a thin compat wrapper that routes through the generic so existing tests
  in `test-corpus_coverage.R` and `test-tier0-smoke.R` continue to pass.

- **`verify_run_integrity()` mode dispatch**. The integrity-check
  function now dispatches on `config$methodology$mode`. Mode 1 expects a
  different artifact set (no sentiment_scores.csv, no correlations.csv,
  no theme_entries directory; instead reflection_log.json,
  provocations.csv, provocation_attempts.csv, coverage_mode1.json).
  Mode 2/3 expectations unchanged.

- **Bug fix**: `find_latest_run()` regex predated the T1.7 mode-suffixed
  run dirs (phase 25-27) and silently returned NULL for ANY mode-
  suffixed dir, breaking the resume path across all modes. Regex updated
  to allow the optional `_M[123]` tail. Caught while writing the AC5
  resume-finalized refusal test for Mode 1; affects Mode 2 and Mode 3
  resume too.

- **Pipeline friendly-error update**: `R/18_pipeline.R`'s Mode 1 refusal
  message now points users at `run_mode1()` (the scaffolded entry point)
  in addition to `run_provocateur_questioning()` (the bare loop).

- **Audit-driven hardening (3 parallel general-purpose audit subagents)**.
  The audit pattern caught real issues that unit-testing alone missed;
  fixes applied in the same commit:
  * **C1** `attempts_per_category` and `explicit_skip_reasons` now
    serialize as named JSON objects (previously `auto_unbox=TRUE` on
    named integer vectors produced anonymous arrays in
    `coverage_mode1.json`, defeating replay/audit). `ProvocationCoverage`
    schema bumped 1.0.0 -> 2.0.0.
  * **H1** `compute_mode1_coverage` now partitions attempts into
    in-scope vs out-of-scope WRT `requested_categories` (previously
    `factor(..., levels=requested)` silently dropped unexpected-category
    rows from `attempts_per_category` while still counting them via
    `nrow(attempts)`, producing contradictory `recorded > expected`).
    New fields: `n_unexpected_category_attempts`, `unexpected_categories`,
    `no_unexpected_category_attempts`.
  * **H2 / H3** `no_silent_skip` headline now requires
    `n_themes_input > 0L` AND `n_themes_attempted > 0L` so degenerate
    states (zero-themes input, all-themes-explicit-skipped) don't grade
    as verified coverage. Coverage card banner branches accordingly with
    distinct messages for each degenerate case.
  * **M3** Replaced the unconditional `no_silent_corpus_truncation = TRUE`
    boolean (which overclaimed -- the per-category prompts in
    `R/provocateur.R` include only theme-supporting entries, not the
    full corpus text) with two honest fields:
    `corpus_provided_to_per_category_fns` (TRUE -- `data` IS passed) and
    `llm_prompt_includes_full_corpus` (FALSE -- prompts only embed
    supporting-entry text). Coverage card adds a "prompt context" note
    explaining the constraint and flagging corpus-search retrieval as a
    future phase. The verification ladder still catches any hallucinated
    entry_id the LLM might invent.
  * **A.H3** `.read_reflection_log_json` now uses `simplifyVector=FALSE`
    and explicitly re-classes nested `Provocation` + `QuoteProvenance`
    objects after the JSON read (previously the round-trip stripped S3
    classes and downstream resume-time consumers gating on
    `inherits(...)` would silently emit NA-cited rows).
  * **A.L4** `init_run_state` is now called AFTER `create_ai_provider`
    so `model_primary` and `model_fast` get stamped into
    `run_metadata.json` (parity with run_analysis -- previously Mode 1
    metadata was missing those cross-mode-comparison fields).
  * **A.H1 / H2** Wrong-mode + finalized-resume errors use single
    multi-line messages (parity with run_analysis's friendly-error
    style); `find_latest_run` returning NULL on `resume=TRUE` now logs
    "No previous run found" instead of falling through silently.
  * **B (XSS)** Theme names from researcher-supplied input are now
    HTML-escaped in the executive summary's concentration-flags and
    disconfirming-evidence lines (previously a crafted theme name like
    `<script>alert(1)</script>` would interpolate raw into the rendered
    Rmd). The per-theme provocation section already escaped via
    `.html_esc`; this closes the remaining unescaped path.
  * **B (semantic)** Executive summary now distinguishes "no fabrications
    detected" (verbatim claims existed AND none failed verification)
    from "no verbatim claims to verify" (e.g., a Mode 1 run that only
    used absent_voice or assumption_surfacing erased-terms produces
    NULL-provenance provocations -- nothing to fabricate from).

Phase 31 net adds ~225 tests across `test-mode1-coverage.R`,
`test-mode1-orchestrator.R`, `test-mode1-report.R`, plus extensions to
`test-provocateur.R` (attempt-tracking + schema 1.1.0 assertion) and
`test-mode3-framework.R` (run_mode1 reference in the friendly-error).
Test count: 2032 -> 2089 net (the audit-fix tests pin every issue
listed above so the same bugs cannot regress). R CMD check stays at
0 errors / 0 warnings / 2 routine NOTEs (21 imports + future
timestamps -- both environmental and unchanged from prior phases).

## Pre-publication rename: thematicai -> pakhom

Pre-publication rename so the package's name matches its GitHub repo and
methodology-paper identity. The new name **pakhom** (Coptic ⲡⲁϩⲱⲙ,
"eagle") honors Saint Pachomius the Great (c. 292–348 CE), the Coptic
Egyptian abbot whose written **Rule** of communal discipline established
the genre of methodology-as-written-document. The package extends that
lineage to AI-assisted thematic analysis: methodology is codified at
the **architectural** level, not at the configuration level. The Coptic
form *pakhom* (rather than the Hellenized *Pachomius*) is used
deliberately — naming a tradition in its own voice.

The previous name *thematicai* had real conflicts that the multi-round
investigation surfaced: namespace adjacency to the existing CRAN package
`thematic` (Posit, ggplot2 theming), trademark adjacency to Thematic
Analysis Inc. (the YC-backed customer-feedback SaaS company), and an
SEO-impossible name (the descriptive phrase "thematic AI" is used by
every commercial QDA vendor). pakhom solves all three: clean CRAN
namespace, no commercial conflicts, distinctive search profile.

Mechanical change only — all behavior unchanged. S3 class names like
`ThematicConfig` and `ProgressiveCodingState` are preserved because
they describe what the object *is conceptually* (a thematic analysis
config, a progressive coding state), not the package brand.

## Sprint-4 Phase B: Tier-0 Universal Requirements (in progress)

Phase B addresses the most-cited empirical critiques of LLM-for-TA tools
via three "Frankenstein-derived" universal requirements (mandatory in all
modes). The naming references Jowsey, Braun, Clarke, Lupton & Fine 2025
(PLOS One, doi:10.1371/journal.pone.0330217) which characterized
Microsoft Copilot's failures as Frankenstein-like assemblage from
disconnected fragments.

- **T0.1 part 1: Quote provenance + 4-step verification ladder** — new
  `R/quote_provenance.R` module with `make_quote()` constructor (deterministic
  SHA-1 quote_id, source SHA-256 for drift detection), `verify_quote()`
  four-step ladder (strict offline string match → normalized match
  (smart-quote/whitespace/case) → substring search → embedding cosine
  via provider) that downgrades verification_status accordingly,
  `verify_quotes()` batch wrapper, `init_fabrication_log()` /
  `log_fabrication()` for the methodology paper's KPI CSV at
  `outputs/<run>/fabrication_log.csv`, and `quote_provenance_summary()`
  for the upcoming report dashboard. Verification ladder distinguishes
  fabricated (text never in source) from drifted (source edited since
  attribution) via SHA-256 comparison. Render policy: fabricated quotes
  are never rendered; unverified get warning markers; drifted trigger
  corpus-integrity warnings. Module is foundation for the upcoming
  Anthropic Citations API integration (T0.1 part 3).

- **T0.1 part 3a: Tier-0 Data Integrity Dashboard in the report** — every
  generated HTML report now renders a "Data Integrity Dashboard (T0.1)"
  card immediately after the Executive Summary, showing how many
  AI-attributed verbatim claims were checked, how many verified exactly
  vs fuzzy (with method breakdown by ladder step: string_match,
  normalized_match, substring_search, embedding_cosine), how many
  fabrications were dropped, and a relative-path link to
  `outputs/<run>/fabrication_log.csv` when fabrications occurred. The
  dashboard cites Jowsey et al. 2025 doi:10.1371/journal.pone.0330217 in
  its body so reviewers immediately see what the package is doing about
  the field's most-cited critique. New `compute_quote_provenance_stats(coding_state)`
  exported helper aggregates verification stats from any
  ProgressiveCodingState; pre-T0.1 states (no `$provenance` on
  segments) get an empty-summary dashboard explaining why.
  Singular/plural noun-verb agreement is correct for both 0, 1, and N+
  fabrications.

- **T0.1 part 2: Verification ladder wired into per-entry coding** —
  `.code_entry_progressive()` now constructs a `QuoteProvenance` for every
  AI-attributed coded segment and runs the 4-step ladder against the
  source entry text. Fabricated segments are dropped from the codebook
  AND the entry's coded_segments AND written to
  `outputs/<run>/fabrication_log.csv` AND emit a `quote_fabricated`
  audit decision (T1.4 schema slot) for cross-run analysis. Verified
  segments (exact OR fuzzy) keep the QuoteProvenance attached as
  `seg$provenance` so downstream consumers can show verification
  status, attribute back to the AI call (`ai_call_id` joins to audit
  log `request_id`), and re-verify at report-load time. `run_progressive_coding()`
  and the pipeline orchestrator gain a `fabrication_log` parameter
  (NULL keeps verification active but skips the CSV; pipeline always
  passes a real log). The previous primitive substring-match validation
  (which would silently keep AI-fabricated text "as-is") is removed.

## Sprint-4 Phase A: Foundation (complete)

The Sprint-4 architectural rebuild transforms pakhom from "AI-assisted
thematic analysis tool" into a multi-mode AI-qualitative platform with
explicit per-methodology agency configuration, Frankenstein-derived
universal requirements, and methodology-as-permission-structure
architecture. Phase A lays the foundation; subsequent phases ship the
mode clusters, Tier-0 universals, and open-science infrastructure.

Phase A items shipped (all backward-compatible — existing v1.x
configs and runs continue to work):

- **OS.1: Saturation Signal 2 citation precision** — `slope_ratio` is now
  documented as De Paoli & Mathis (2024) Inductive Thematic Saturation
  ratio (doi:10.1007/s11135-024-01950-6). The 0.05 stopping threshold is
  noted as stricter than De Paoli's illustrative 0.28 single-timepoint
  observation because we use the ratio as a stopping criterion.

- **OS.2: Correlations reframed as exploratory associations** — themes are
  inductively derived from the same data the correlations are computed on,
  so framing the results as significance-tested findings was misleading.
  Per Rothman (1990, *Epidemiology* 1(1):43-46), correlations now ship
  with raw + Benjamini-Hochberg + Bonferroni p-values side-by-side and
  meaningful-effect-size flags. Reports use "Exploratory Associations"
  framing with hypothesis-generating language. Legacy column names
  (`p_value`, `p_adjusted`, `significant`) preserved for back-compat.

- **T1.3: Multi-mode methodology declaration** — three modes shipped:
  `reflexive_scaffold` (AI as provocateur, Mode 1), `codebook_collaborative`
  (AI proposes, researcher gates, Mode 2), `framework_applied` (AI applies
  researcher's framework, Mode 3). Mode 4 (AI-heavy content analysis)
  intentionally not shipped per the Cochrane RevMan refusal pattern;
  content-analytic use case absorbed into Mode 3 via positivist framework
  choice. New `methodology_decision_aid()` function provides a 3-question
  wizard or non-interactive recommendation engine. Configs missing the
  methodology block fail validation with a pointer to the decision aid.
  References: Lin (2025) Cognitio Emergens (arXiv:2505.03105), Prahl ARC
  (Qual Health Res 2026, doi:10.1177/10497323251401503), Jowsey, Braun,
  Clarke, Lupton & Fine (2025, doi:10.1177/10778004251401851).

- **default_config() warning on silent methodology default** — closes a
  subtle internal contradiction: phase 11 cited Spool 2011 (>95% of users
  never change defaults) for "no silent default-mode trap" but
  `default_config()` itself was a silent trap. Now warns when called
  without `methodology` and falls back to `codebook_collaborative`;
  explicit-mode `default_config("...")` is silent.

- **T1.1: `ai_complete()` returns structured list with provenance** —
  refactored from returning a bare string to returning
  `list(content, model, usage, finish_reason, raw_response, prompt_hash,
  request_id)`. The structured return unblocks the audit log expansion
  (T1.4), Structured Outputs migration (T1.2), and replay_run (OS.5)
  simultaneously. Token usage is normalized across providers (Anthropic's
  `input_tokens`/`output_tokens` remap to OpenAI-style names); Anthropic's
  `stop_reason` normalizes to canonical `stop`/`length`/`tool_use`.
  `prompt_hash` is a SHA-256 over the JSON serialization of the request
  (stable across R versions and platforms) and is used as the cache key
  for replay. `ai_complete()` is internal so the change is contained to
  in-package callers (7 sites updated); no external callers affected.

- **T1.2: Native Structured Outputs migration (OpenAI strict json_schema +
  Anthropic forced tool-use)** — six task schemas in
  `R/structured_schemas.R` (coding, saturation, sentiment, theming,
  insight, synthesis) replace the prompt-based JSON-mode coercion that
  previously relied on `parse_json_safely()` defensive parsing. With the
  schema enforced server-side (`response_format = list(type =
  "json_schema", strict = TRUE, ...)` for OpenAI; a forced
  `record_analysis` tool-call for Anthropic), JSON parse failures are
  eliminated and schema-drift bugs become impossible: a model update
  can no longer silently change the response shape. Schemas are designed
  to satisfy OpenAI's strict-mode constraints (the stricter of the two
  providers): every object has `additionalProperties = FALSE`, every
  property is in `required` (optional fields use nullable types like
  `list("integer", "null")` for theming's merge_into), `required` and
  `enum` arrays use `list()` to survive jsonlite's auto_unbox. The
  `.validate_schema()` helper catches mistakes at package-load and test
  time so a malformed schema fails fast in R rather than producing an
  opaque OpenAI 400. Reasoning models (o1/o3/o4) silently fall back to
  json_mode because they don't support strict json_schema as of writing.
  `prompt_hash` (the OS.5 replay cache key) now includes
  `response_schema` so requests with different schemas don't collide.
  All six in-package callers migrated (`.code_entry_progressive`,
  `.ai_saturation_check`, `analyze_sentiment`, `.run_merge_pass`,
  `generate_insights`, `generate_ai_synthesis`); `parse_json_safely()`
  remains as a defensive parsing layer for one minor version per the
  Sprint-4 design plan, then will be removed.

- **T1.4: Audit log schema expansion + content-addressable response cache**
  — `init_audit_log()` accepts `config` and auto-stamps
  `methodology_mode` on every JSONL record. New `log_ai_request()` helper
  records each `ai_complete()` call as an `ai_request` audit decision
  with model, usage, finish_reason, prompt_hash, and request_id. Raw API
  responses are stored content-addressably in `api_responses/{prompt_hash}.json`
  via the new `init_response_cache()`/`cache_response()`/`read_cached_response()`
  module — JSONL stays light, identical requests dedupe on disk, and the
  layout is exactly what `replay_run()` (OS.5) needs. `summarize_audit_log()`
  surfaces new stats: `total_ai_requests`, `total_tokens_used`,
  `ai_requests_by_model`, `methodology_modes_observed`. New decision_types
  declared up-front so Phases B/C/D items don't bounce off validation when
  they land: `provocation_emitted`, `memo_added`, `positionality_recorded`,
  `reflexivity_collapse_detected`, `mode_changed`, `quote_verified`,
  `quote_fabricated`. Schema version bumped 1.0 -> 1.1 (minor: additive,
  pre-1.1 runs remain comparable to post-1.1 runs because the structural
  artifacts read by `compare_runs()` are unchanged). Incidental fix:
  `n_written` counter in `AuditLog`/`ResponseCache` now uses
  environment-backed state so increments survive R's pass-by-value
  semantics — `close_audit_log()`'s "N decisions recorded" log message now
  reflects the actual count instead of always reading 0.

## Pipeline Architecture

pakhom uses a progressive sequential coding pipeline faithful to how manual
reflexive thematic analysis works in NVivo and similar QDA platforms:

1. **Codebook-first learning** from prior manual analyses (QDPX, Excel, CSV)
2. **Progressive sequential coding** -- entries read one at a time, coded inline
3. **Thematic saturation detection** -- triangulated stopping criterion
4. **Code-aware sentiment analysis** -- sentiment scored after coding with code context
5. **Iterative bottom-up theme generation** -- sequential merging of codes into clusters
6. **Deterministic code-path cascading** -- entries map to themes through their codes

## Major Features

- Multi-provider AI support (OpenAI GPT-4o, Anthropic Claude Sonnet 4)
- Progressive sequential coding: entries processed one at a time with a growing
  codebook, just like a human researcher in NVivo. No batch coding, no separate
  deduplication or consolidation steps
- Thematic saturation detection using triangulated signals: code creation rate
  monitoring (Guest et al., 2020), Inductive Thematic Saturation ratio
  (De Paoli & Mathis, 2024, doi:10.1007/s11135-024-01950-6), and AI
  self-assessment. Saturation curve saved for publication
- Iterative bottom-up theme generation: codes are merged through multiple
  sequential passes until no more productive groupings exist. Themes and
  subthemes emerge organically from the data
- Deterministic code-path cascading: entry-to-theme mapping flows through the
  code hierarchy (no AI re-reading of raw text), faithful to the inductive process
- Code-aware sentiment analysis: sentiment scored after initial coding, using
  assigned codes as context for more accurate emotional valence detection
- Codebook-first learning from prior studies: full theme/subtheme/code hierarchy
  extraction from QDPX files (NVivo exports), Excel, and CSV codebooks.
  Manuscripts serve as supplementary clarification only when codebook descriptions
  are insufficient
- Cross-study qualitative synthesis: structural patterns identified across all
  available prior analyses collectively (not just in sequence)
- Researcher review points: pause the pipeline after coding or theme generation
  to curate the AI's output before continuing
- Checkpoint/resume system with flat checkpoint architecture for reliable resume
  across retries and interruptions
- Inter-rater reliability: Cohen's kappa and Krippendorff's alpha with
  small-sample correction for human verification of AI-generated codes
- Reddit scraper with OAuth authentication
- Rich HTML report with interactive DataTables, saturation curves, sentiment-coded
  quotes, theme detail drill-downs, and cross-run comparison dashboard
- Cross-run comparison module with 7 analysis dimensions (sample overlap,
  sentiment drift, code stability, theme evolution, entry migration,
  correlation persistence, run dashboard)
- Dynamic token-aware batch sizing for AI operations
- Interactive Shiny configuration wizard for building config.yaml

## Bug Fixes and Improvements

- Fixed: token-limits table in the methodology appendix now filters to
  the v1.0 task whitelist (`coding`, `theming`, `sentiment`, `review`,
  `insight`, `synthesis`). User configs migrated from earlier versions
  may carry stale `consolidation` / `assignment` / `relevance` keys;
  these are no longer surfaced in the report
- Fixed: `close_audit_log()` is now idempotent. The pause-for-review
  pipeline branches both call `close_audit_log` explicitly and rely on
  an `on.exit` safety net; previously the second call logged a spurious
  "invalid connection" warning. Now silently no-ops when the connection
  is already closed
- Removed: optional `tiktoken` integration in `estimate_tokens()`. The
  package is not on CRAN and was triggering an undeclared-namespace
  warning under `R CMD check --as-cran`. The script-aware character
  heuristic is sufficient for batch-size budgeting (the only place
  token estimation is used) and was already the production path on any
  install without `tiktoken`
- Removed: leftover references to the pre-1.0 architecture in the
  Reddit scraper docstring, `06_manuscript_learning.R` placeholder
  comment, the `compute_dynamic_batches` example text, and the
  `globalVariables` registration of `relevance_score`
- Fixed: thematic-saturation detection no longer fires prematurely on
  longer runs. The previous implementation derived each code's
  birth-time-in-coded-entries from per-checkpoint accumulator lists
  that get reset every checkpoint interval; after the first reset the
  signal collapsed toward zero and saturation could be declared even
  while novel codes were still being created. Now stored directly in
  a parallel `code_n_coded_at_birth` map at the moment of code creation
- Added: `analysis_schema_version` field in `run_metadata.json`. The
  cross-run comparison module (`compare_runs`, `compare_models`,
  `list_available_runs`) is now schema-aware: incompatible snapshots
  are excluded with a clear warning rather than silently NA-padded
- Removed: hardcoded medication-research framing from sentiment and
  cross-study-synthesis prompts. The sentiment system prompt no longer
  asserts "qualitative health research" / "clinical experiences /
  treatment effects" regardless of the user's actual research domain;
  `.synthesize_cross_study_patterns` no longer matches theme names
  against six medication-specific regex categories or injects an
  unconditional medication-narrative-arc claim into the AI's learning
  context. Replaced with a domain-neutral, evidence-based listing of
  the actual top-level themes from each prior codebook
- Removed: `confidence` from substantive correlation analyses
  (`prepare_correlation_data`, `compare_theme_groups`). The AI sentiment
  prompt elicits confidence and emotion_intensity in the same single
  call, so they co-vary by design (r >= 0.83 across all observed runs);
  reporting their correlation as a finding misled readers. Confidence
  remains in the per-entry `sentiment_scores.csv` as a diagnostic
- Fixed: `DT` package (in Suggests) now has graceful fallback to `knitr::kable`
  when not installed
- Fixed: `.html_esc()` now escapes single quotes to prevent XSS in HTML
  attribute contexts
- Fixed: Variable shadowing of exported `n_themes()` function in report builder
- Replaced `library(tidyverse)` with targeted imports in generated Rmd for
  faster rendering
- Added `error = TRUE` to generated Rmd chunks so individual chunk failures
  don't prevent report rendering
- Added warnings when `load_data()` auto-excludes database tables
- Added small-sample-size warnings for theme membership correlations
- Added methodological notes on saturation criteria and pipeline-wide
  multiple testing in report appendix
- Standardized error messages with `validate_class()` helper
- Added `list_available_runs()` convenience function for cross-run comparison
- Added `getting-started` vignette (now also documents Reddit's post-2025
  Responsible Builder Program approval requirement for the optional
  scraper)
- Added `config_wizard_app()` for interactive configuration building
- Removed leftover stale references to the pre-1.0 architecture's
  removed pipeline steps (relevance filtering, batch coding, code
  consolidation, theme assignment) from the report's learning-
  transparency section, the methodology appendix, and the distributed
  default config template
- Cleaned all R CMD check warnings: documented previously undocumented
  function arguments, declared previously implicit base-stats imports
  (`complete.cases`, `shapiro.test`, `wilcox.test`, `fisher.test`,
  `chisq.test`), removed non-ASCII characters from R sources
