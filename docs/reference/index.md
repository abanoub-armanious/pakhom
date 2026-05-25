# Package index

## Run an Analysis

Top-level entry points. run_analysis() drives Modes 2 + 3 (data -\>
coding -\> sentiment -\> themes -\> correlations -\> report -\>
finalize_run). run_mode1() drives Mode 1 (Reflexive Scaffold) with the
same Tier-0 / Tier-1 scaffolding but routes through the provocateur
loop. Both produce a finalized run directory with full audit trail and
HTML report.

- [`run_analysis()`](https://abanoub-armanious.github.io/pakhom/reference/run_analysis.md)
  : Run the full thematic analysis pipeline (Mode 2 + Mode 3)
- [`run_mode1()`](https://abanoub-armanious.github.io/pakhom/reference/run_mode1.md)
  : Run a Mode 1 (Reflexive Scaffold) provocateur analysis
- [`load_config()`](https://abanoub-armanious.github.io/pakhom/reference/load_config.md)
  : Load analysis configuration from YAML file
- [`create_config()`](https://abanoub-armanious.github.io/pakhom/reference/create_config.md)
  : Create a minimal configuration file
- [`config_wizard()`](https://abanoub-armanious.github.io/pakhom/reference/config_wizard.md)
  : Interactive configuration wizard
- [`config_wizard_app()`](https://abanoub-armanious.github.io/pakhom/reference/config_wizard_app.md)
  : Launch the interactive configuration wizard

## Methodology Modes (Sprint-4)

The three methodologically-distinct operating modes, the framework spec
module (Mode 3), the provocateur loop (Mode 1), and the reflexive memo
CRUD (Mode 1 burden parity per AC6).

- [`run_provocateur_questioning()`](https://abanoub-armanious.github.io/pakhom/reference/run_provocateur_questioning.md)
  : Run provocateur questioning across themes (Mode 1 entry point)
- [`load_framework_spec()`](https://abanoub-armanious.github.io/pakhom/reference/load_framework_spec.md)
  : Load + validate a theoretical framework specification
- [`list_builtin_frameworks()`](https://abanoub-armanious.github.io/pakhom/reference/list_builtin_frameworks.md)
  : List the built-in frameworks shipped with pakhom
- [`archive_framework_spec()`](https://abanoub-armanious.github.io/pakhom/reference/archive_framework_spec.md)
  : Archive a Mode 3 framework spec into the run output directory
- [`framework_prompt_block()`](https://abanoub-armanious.github.io/pakhom/reference/framework_prompt_block.md)
  : Build the prompt block describing the framework's constructs
- [`apply_framework_themes()`](https://abanoub-armanious.github.io/pakhom/reference/apply_framework_themes.md)
  : Apply framework constructs as themes + handle anomalies (Mode 3)
- [`make_memo()`](https://abanoub-armanious.github.io/pakhom/reference/make_memo.md)
  : Construct a Memo S3 object
- [`add_memo()`](https://abanoub-armanious.github.io/pakhom/reference/add_memo.md)
  : Add a memo to a ResearcherReflectionLog
- [`read_memo()`](https://abanoub-armanious.github.io/pakhom/reference/read_memo.md)
  : Read a memo from a ResearcherReflectionLog by id
- [`list_memos()`](https://abanoub-armanious.github.io/pakhom/reference/list_memos.md)
  : List memos in a ResearcherReflectionLog as a tibble
- [`persist_memos()`](https://abanoub-armanious.github.io/pakhom/reference/persist_memos.md)
  : Persist all memos in a ResearcherReflectionLog to disk
- [`load_memos()`](https://abanoub-armanious.github.io/pakhom/reference/load_memos.md)
  : Load memos from a run output directory back into Memo objects
- [`memo_to_markdown()`](https://abanoub-armanious.github.io/pakhom/reference/memo_to_markdown.md)
  : Serialize a Memo to a Markdown string with YAML frontmatter
- [`markdown_to_memo()`](https://abanoub-armanious.github.io/pakhom/reference/markdown_to_memo.md)
  : Parse a Markdown-with-YAML-frontmatter string back into a Memo
- [`create_reflection_log()`](https://abanoub-armanious.github.io/pakhom/reference/create_reflection_log.md)
  : Initialize a ResearcherReflectionLog
- [`make_provocation()`](https://abanoub-armanious.github.io/pakhom/reference/make_provocation.md)
  : Construct a Provocation object
- [`provoke_counter_narrative()`](https://abanoub-armanious.github.io/pakhom/reference/provoke_counter_narrative.md)
  : Counter-narrative provocation
- [`provoke_disconfirming_evidence()`](https://abanoub-armanious.github.io/pakhom/reference/provoke_disconfirming_evidence.md)
  : Disconfirming-evidence provocation
- [`provoke_alternative_interpretation()`](https://abanoub-armanious.github.io/pakhom/reference/provoke_alternative_interpretation.md)
  : Alternative-interpretation provocation
- [`provoke_absent_voice()`](https://abanoub-armanious.github.io/pakhom/reference/provoke_absent_voice.md)
  : Absent-voice provocation
- [`provoke_assumption_surfacing()`](https://abanoub-armanious.github.io/pakhom/reference/provoke_assumption_surfacing.md)
  : Assumption-surfacing provocation

## Tier-0 Transparency (T0.1 / T0.2 / T0.3)

Quote provenance + verification ladder (T0.1), participant spread per
theme (T0.2 – computed inside aggregate_theme_statistics), and corpus
coverage / no-silent-skip (T0.3). The render_tier0_coverage_card generic
dispatches on a virtual Tier0Coverage parent class shared by
CorpusCoverage (Mode 2/3) and ProvocationCoverage (Mode 1).

- [`make_quote()`](https://abanoub-armanious.github.io/pakhom/reference/make_quote.md)
  : Construct a Quote provenance object
- [`verify_quote()`](https://abanoub-armanious.github.io/pakhom/reference/verify_quote.md)
  : Verify a quote against its source text via the four-step ladder
- [`quote_provenance_summary()`](https://abanoub-armanious.github.io/pakhom/reference/quote_provenance_summary.md)
  : Summarize quote provenance for the report's Tier-0 dashboard
- [`compute_quote_provenance_stats()`](https://abanoub-armanious.github.io/pakhom/reference/compute_quote_provenance_stats.md)
  : Aggregate verification stats across all coded segments in a coding
  state
- [`compute_corpus_coverage()`](https://abanoub-armanious.github.io/pakhom/reference/compute_corpus_coverage.md)
  : Compute corpus coverage from a completed coding run
- [`compute_mode1_coverage()`](https://abanoub-armanious.github.io/pakhom/reference/compute_mode1_coverage.md)
  : Compute Mode 1 (Reflexive Scaffold) coverage from a finished
  provocateur run
- [`compute_mode1_theme_stats()`](https://abanoub-armanious.github.io/pakhom/reference/compute_mode1_theme_stats.md)
  : Compute per-theme statistics for a Mode 1 run
- [`compute_provocation_provenance_stats()`](https://abanoub-armanious.github.io/pakhom/reference/compute_provocation_provenance_stats.md)
  : Aggregate verification stats across all provocations in a reflection
  log
- [`render_tier0_coverage_card()`](https://abanoub-armanious.github.io/pakhom/reference/render_tier0_coverage_card.md)
  : Render the Tier-0 coverage card for a coverage object
- [`init_fabrication_log()`](https://abanoub-armanious.github.io/pakhom/reference/init_fabrication_log.md)
  : Initialize the fabrication log
- [`log_fabrication()`](https://abanoub-armanious.github.io/pakhom/reference/log_fabrication.md)
  : Append a fabricated quote to the fabrication log
- [`make_quote_from_citation()`](https://abanoub-armanious.github.io/pakhom/reference/make_quote_from_citation.md)
  : Construct a QuoteProvenance from a single Anthropic citation
- [`make_quotes_from_citations()`](https://abanoub-armanious.github.io/pakhom/reference/make_quotes_from_citations.md)
  : Construct QuoteProvenance objects from a list of Anthropic citations
- [`verify_quotes()`](https://abanoub-armanious.github.io/pakhom/reference/verify_quotes.md)
  : Verify a batch of quotes against a corpus
- [`write_corpus_coverage()`](https://abanoub-armanious.github.io/pakhom/reference/write_corpus_coverage.md)
  : Persist a CorpusCoverage / ProvocationCoverage object to disk

## Methodological Transparency Report (OS.6)

Phase 58 Stage 1A: self-contained HTML methodological transparency
report bundling Lincoln & Guba (1985) trustworthiness mapping,
reflexivity completeness, T0.1 quote provenance summary, T0.3 corpus
coverage funnel, AC9 audit log summary, and theme set summary. Reads
disk artifacts only; never re-runs the pipeline.

- [`bundle_transparency_report()`](https://abanoub-armanious.github.io/pakhom/reference/bundle_transparency_report.md)
  : Bundle a run's transparency artifacts into a single report

## Methodology Decision Aids

User-facing helpers for choosing a methodology mode and for annotating
plots / reports with the mode and provenance evidence.

- [`methodology_decision_aid()`](https://abanoub-armanious.github.io/pakhom/reference/methodology_decision_aid.md)
  : Methodology decision aid
- [`methodology_plot_caption()`](https://abanoub-armanious.github.io/pakhom/reference/methodology_plot_caption.md)
  : Build a caption string suitable for use as a ggplot watermark

## Run State + Soft-Lock (T1.5)

The REDCap dev/production pattern: every run lives in active or
finalized state; methodology cannot silently change between states. Fork
a run via clone_run_with_new_mode().

- [`init_run_state()`](https://abanoub-armanious.github.io/pakhom/reference/init_run_state.md)
  : Initialize the run-metadata record for a new (or resumed) run
- [`read_run_metadata()`](https://abanoub-armanious.github.io/pakhom/reference/read_run_metadata.md)
  : Read and parse run_metadata.json for a run directory
- [`is_run_finalized()`](https://abanoub-armanious.github.io/pakhom/reference/is_run_finalized.md)
  : Check whether a run directory is finalized
- [`finalize_run()`](https://abanoub-armanious.github.io/pakhom/reference/finalize_run.md)
  : Mark a run as finalized
- [`methodology_mismatch_status()`](https://abanoub-armanious.github.io/pakhom/reference/methodology_mismatch_status.md)
  : Detect a methodology mismatch between a config and an existing run
- [`clone_run_with_new_mode()`](https://abanoub-armanious.github.io/pakhom/reference/clone_run_with_new_mode.md)
  : Clone a run directory with a new methodology mode

## Output Stamping (T1.7 / AC4)

Methodology stamping helpers. Every CSV / JSON / HTML / plot produced by
a run carries the mode + run id stamp so a reviewer reading any artifact
alone sees the methodology declaration.

- [`stamp_methodology_html()`](https://abanoub-armanious.github.io/pakhom/reference/stamp_methodology_html.md)
  : Build an HTML methodology badge for the report header
- [`stamp_methodology_csv()`](https://abanoub-armanious.github.io/pakhom/reference/stamp_methodology_csv.md)
  : Stamp a CSV file with a methodology comment header
- [`stamp_methodology_json()`](https://abanoub-armanious.github.io/pakhom/reference/stamp_methodology_json.md)
  : Stamp a JSON file with a methodology envelope
- [`stamp_methodology_console()`](https://abanoub-armanious.github.io/pakhom/reference/stamp_methodology_console.md)
  : Build a console banner string for the methodology mode
- [`methodology_label()`](https://abanoub-armanious.github.io/pakhom/reference/methodology_label.md)
  : Human-readable label for a methodology mode
- [`methodology_short_code()`](https://abanoub-armanious.github.io/pakhom/reference/methodology_short_code.md)
  : Map a methodology mode to its short-code (M1 / M2 / M3)
- [`methodology_description_short()`](https://abanoub-armanious.github.io/pakhom/reference/methodology_description_short.md)
  : One-line description of what the mode commits the AI to
- [`run_id_with_mode()`](https://abanoub-armanious.github.io/pakhom/reference/run_id_with_mode.md)
  : Build a Mode N run-directory suffix for a fresh run

## Methodology Rules (T1.6 / AC9)

Lin & Corley (2025) pattern: methodology rules generated from config and
injected into the model context every turn.

- [`generate_methodology_rules()`](https://abanoub-armanious.github.io/pakhom/reference/generate_methodology_rules.md)
  : Generate the methodology-rules text for a config

- [`write_methodology_rules()`](https://abanoub-armanious.github.io/pakhom/reference/write_methodology_rules.md)
  :

  Write methodology rules to a markdown file under `run_dir`

## S3 Print Methods

print() dispatchers for the package’s S3 classes. Each is invoked
automatically when you print(x) an object of the corresponding class;
they are documented here for reference.

- [`print(`*`<AIProvider>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.AIProvider.md)
  : Print method for AIProvider
- [`print(`*`<CheckpointManager>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.CheckpointManager.md)
  : Print method for CheckpointManager
- [`print(`*`<Code>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.Code.md)
  : Print method for Code
- [`print(`*`<ComparisonResult>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.ComparisonResult.md)
  : Print method for ComparisonResult
- [`print(`*`<CorpusCoverage>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.CorpusCoverage.md)
  : Print method for CorpusCoverage
- [`print(`*`<FrameworkSpec>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.FrameworkSpec.md)
  : Print method for FrameworkSpec
- [`print(`*`<LiveTracker>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.LiveTracker.md)
  : Print method for LiveTracker
- [`print(`*`<Memo>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.Memo.md)
  : Print method for Memo
- [`print(`*`<Provocation>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.Provocation.md)
  : Print method for Provocation
- [`print(`*`<ProvocationCoverage>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.ProvocationCoverage.md)
  : Print method for ProvocationCoverage
- [`print(`*`<QuoteProvenance>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.QuoteProvenance.md)
  : Print method for QuoteProvenance
- [`print(`*`<ResearcherReflectionLog>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.ResearcherReflectionLog.md)
  : Print method for ResearcherReflectionLog
- [`print(`*`<ResponseCache>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.ResponseCache.md)
  : Print method for ResponseCache
- [`print(`*`<Subtheme>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.Subtheme.md)
  : Print method for Subtheme
- [`print(`*`<ThematicConfig>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.ThematicConfig.md)
  : Print method for ThematicConfig
- [`print(`*`<ThemeSet>`*`)`](https://abanoub-armanious.github.io/pakhom/reference/print.ThemeSet.md)
  : Print method for ThemeSet

## Internal Helpers

Documented internal helpers used by the pipeline. Listed for
completeness; not part of the user-facing API and may change without
notice.

- [`detect_columns()`](https://abanoub-armanious.github.io/pakhom/reference/detect_columns.md)
  : Detect and map columns based on platform type
- [`export_theme_entry_csvs()`](https://abanoub-armanious.github.io/pakhom/reference/export_theme_entry_csvs.md)
  : Export CSV files for each theme's entries
- [`generate_ai_synthesis()`](https://abanoub-armanious.github.io/pakhom/reference/generate_ai_synthesis.md)
  : Generate AI-powered executive summary and conclusion
- [`generate_downloads_section()`](https://abanoub-armanious.github.io/pakhom/reference/generate_downloads_section.md)
  : Generate downloads appendix section
- [`generate_insights()`](https://abanoub-armanious.github.io/pakhom/reference/generate_insights.md)
  : Generate AI insights from correlation findings
- [`generate_run_id()`](https://abanoub-armanious.github.io/pakhom/reference/generate_run_id.md)
  : Generate a unique run ID based on timestamp
- [`generate_theme_detail_section()`](https://abanoub-armanious.github.io/pakhom/reference/generate_theme_detail_section.md)
  : Generate a theme detail section for appendix
- [`get_emotion_interpretation()`](https://abanoub-armanious.github.io/pakhom/reference/get_emotion_interpretation.md)
  : Get interpretation text for an emotion
- [`hash_config()`](https://abanoub-armanious.github.io/pakhom/reference/hash_config.md)
  : Compute a hash of a config file for change detection
- [`load_and_combine_tables()`](https://abanoub-armanious.github.io/pakhom/reference/load_and_combine_tables.md)
  : Load and combine multiple tables from a SQLite database
- [`make_anchor_id()`](https://abanoub-armanious.github.io/pakhom/reference/make_anchor_id.md)
  : Create an HTML anchor ID from a string
- [`make_safe_filename()`](https://abanoub-armanious.github.io/pakhom/reference/make_safe_filename.md)
  : Create a safe filename from a string
- [`parse_json_safely()`](https://abanoub-armanious.github.io/pakhom/reference/parse_json_safely.md)
  : Parse JSON safely with automatic repair for truncated/malformed
  responses
- [`parse_raw_data_files()`](https://abanoub-armanious.github.io/pakhom/reference/parse_raw_data_files.md)
  : Parse raw data DOCX files and extract metadata from filenames
- [`preprocess_text()`](https://abanoub-armanious.github.io/pakhom/reference/preprocess_text.md)
  : Preprocess text data for analysis
- [`standardize_data()`](https://abanoub-armanious.github.io/pakhom/reference/standardize_data.md)
  : Standardize data to common schema
- [`truncate_text()`](https://abanoub-armanious.github.io/pakhom/reference/truncate_text.md)
  : Truncate text to specified length with ellipsis
- [`validate_config()`](https://abanoub-armanious.github.io/pakhom/reference/validate_config.md)
  : Validate configuration completeness and correctness
- [`.LIVE_CODEBOOK_SNAPSHOT_EVERY`](https://abanoub-armanious.github.io/pakhom/reference/dot-LIVE_CODEBOOK_SNAPSHOT_EVERY.md)
  : Default codebook-snapshot rewrite cadence
- [`.THEME_DEFAULTS`](https://abanoub-armanious.github.io/pakhom/reference/dot-THEME_DEFAULTS.md)
  : Default values for optional theme fields
- [`.THEME_REQUIRED_FIELDS`](https://abanoub-armanious.github.io/pakhom/reference/dot-THEME_REQUIRED_FIELDS.md)
  : Required fields for each theme within a ThemeSet

## Configuration

Configuration creation, validation, and defaults

- [`default_config()`](https://abanoub-armanious.github.io/pakhom/reference/default_config.md)
  : Create a default configuration object
- [`validate_methodology_mode()`](https://abanoub-armanious.github.io/pakhom/reference/validate_methodology_mode.md)
  : Validate a methodology mode declaration

## Data Loading & Exploration

Load data from SQLite and explore databases

- [`load_data()`](https://abanoub-armanious.github.io/pakhom/reference/load_data.md)
  : Load data from a SQLite database
- [`explore_database()`](https://abanoub-armanious.github.io/pakhom/reference/explore_database.md)
  : Explore a SQLite database schema
- [`detect_variable_types()`](https://abanoub-armanious.github.io/pakhom/reference/detect_variable_types.md)
  : Detect variable types for dynamic correlation method selection

## AI Provider

AI provider abstraction for OpenAI and Anthropic

- [`create_ai_provider()`](https://abanoub-armanious.github.io/pakhom/reference/create_ai_provider.md)
  : Create an AI provider client
- [`cache_response()`](https://abanoub-armanious.github.io/pakhom/reference/cache_response.md)
  : Write a raw API response to the cache, indexed by prompt_hash
- [`init_response_cache()`](https://abanoub-armanious.github.io/pakhom/reference/init_response_cache.md)
  : Initialize a content-addressable response cache
- [`read_cached_response()`](https://abanoub-armanious.github.io/pakhom/reference/read_cached_response.md)
  : Read a cached raw response by prompt_hash

## Pipeline Steps

Individual analysis steps (called by run_analysis)

- [`run_progressive_coding()`](https://abanoub-armanious.github.io/pakhom/reference/run_progressive_coding.md)
  : Run progressive sequential coding on all entries
- [`create_coding_state()`](https://abanoub-armanious.github.io/pakhom/reference/create_coding_state.md)
  : Create a new progressive coding state
- [`get_analytic_sample()`](https://abanoub-armanious.github.io/pakhom/reference/get_analytic_sample.md)
  : Get the analytic sample (entries that received at least one code)
- [`analyze_sentiment()`](https://abanoub-armanious.github.io/pakhom/reference/analyze_sentiment.md)
  : Run batch sentiment analysis on all entries
- [`generate_themes_iterative()`](https://abanoub-armanious.github.io/pakhom/reference/generate_themes_iterative.md)
  : Generate themes via HAC + AI-judged divisive tree walk
- [`cascade_theme_assignments()`](https://abanoub-armanious.github.io/pakhom/reference/cascade_theme_assignments.md)
  : Cascade theme assignments from codes to entries deterministically
- [`enrich_themes()`](https://abanoub-armanious.github.io/pakhom/reference/enrich_themes.md)
  : Enrich themes with entry counts, sentiment, and quotes
- [`calculate_correlations()`](https://abanoub-armanious.github.io/pakhom/reference/calculate_correlations.md)
  : Calculate correlation matrix with p-values

## Themes & Theme Data

Working with ThemeSet objects and Code / Subtheme accessors

- [`create_theme_set()`](https://abanoub-armanious.github.io/pakhom/reference/create_theme_set.md)
  : Create a ThemeSet object (canonical internal representation)
- [`create_code_object()`](https://abanoub-armanious.github.io/pakhom/reference/create_code_object.md)
  : Create a Code S3 object
- [`create_subtheme()`](https://abanoub-armanious.github.io/pakhom/reference/create_subtheme.md)
  : Create a Subtheme S3 object
- [`theme_names()`](https://abanoub-armanious.github.io/pakhom/reference/theme_names.md)
  : Extract theme names from ThemeSet
- [`n_themes()`](https://abanoub-armanious.github.io/pakhom/reference/n_themes.md)
  : Get the number of themes
- [`theme_set_to_tibble()`](https://abanoub-armanious.github.io/pakhom/reference/theme_set_to_tibble.md)
  : Convert ThemeSet to tibble for export/inspection
- [`normalize_theme_result()`](https://abanoub-armanious.github.io/pakhom/reference/normalize_theme_result.md)
  : Normalize raw AI theme output to canonical ThemeSet
- [`prune_empty_themes()`](https://abanoub-armanious.github.io/pakhom/reference/prune_empty_themes.md)
  : Remove themes with zero assigned entries after enrichment
- [`theme_codes()`](https://abanoub-armanious.github.io/pakhom/reference/theme_codes.md)
  : Flatten code names across all subthemes of a theme (back-compat with
  codes_included)
- [`theme_code_keys()`](https://abanoub-armanious.github.io/pakhom/reference/theme_code_keys.md)
  : Flatten code keys across all subthemes of a theme
- [`theme_code_objects()`](https://abanoub-armanious.github.io/pakhom/reference/theme_code_objects.md)
  : Flatten Code S3 objects across all subthemes (and sub-subthemes) of
  a theme
- [`theme_segments()`](https://abanoub-armanious.github.io/pakhom/reference/theme_segments.md)
  : Flatten coded_segments across all codes of a theme
- [`theme_n_subthemes()`](https://abanoub-armanious.github.io/pakhom/reference/theme_n_subthemes.md)
  : Number of TOP-LEVEL real subthemes in a theme (excludes virtual
  wrappers)
- [`theme_n_subthemes_total()`](https://abanoub-armanious.github.io/pakhom/reference/theme_n_subthemes_total.md)
  : Total real (named) subthemes across every depth of a theme
- [`subtheme_code_names()`](https://abanoub-armanious.github.io/pakhom/reference/subtheme_code_names.md)
  : Code names (display) within a Subtheme
- [`subtheme_code_keys()`](https://abanoub-armanious.github.io/pakhom/reference/subtheme_code_keys.md)
  : Code keys within a Subtheme
- [`subtheme_n_codes()`](https://abanoub-armanious.github.io/pakhom/reference/subtheme_n_codes.md)
  : Number of DIRECT codes in a Subtheme (excludes nested sub-subthemes)
- [`subtheme_n_codes_total()`](https://abanoub-armanious.github.io/pakhom/reference/subtheme_n_codes_total.md)
  : Number of codes in a Subtheme INCLUDING nested sub-subthemes
- [`subtheme_n_subthemes()`](https://abanoub-armanious.github.io/pakhom/reference/subtheme_n_subthemes.md)
  : Number of nested subthemes within a Subtheme

## Checkpoint & Resume

Save and restore pipeline progress

- [`init_checkpoints()`](https://abanoub-armanious.github.io/pakhom/reference/init_checkpoints.md)
  : Initialize checkpoint system for a pipeline run
- [`save_checkpoint()`](https://abanoub-armanious.github.io/pakhom/reference/save_checkpoint.md)
  : Save checkpoint at a given pipeline step
- [`save_partial_checkpoint()`](https://abanoub-armanious.github.io/pakhom/reference/save_partial_checkpoint.md)
  : Save partial checkpoint within a step (for long-running batch
  operations)
- [`load_checkpoint()`](https://abanoub-armanious.github.io/pakhom/reference/load_checkpoint.md)
  : Load checkpoint for a step
- [`list_checkpoints()`](https://abanoub-armanious.github.io/pakhom/reference/list_checkpoints.md)
  : List available checkpoints with metadata
- [`find_latest_run()`](https://abanoub-armanious.github.io/pakhom/reference/find_latest_run.md)
  : Find the most recent run folder in the results directory
- [`find_resume_point()`](https://abanoub-armanious.github.io/pakhom/reference/find_resume_point.md)
  : Determine the last completed step for resume

## Manuscript Learning

Learn from prior manual thematic analyses

- [`load_previous_studies()`](https://abanoub-armanious.github.io/pakhom/reference/load_previous_studies.md)
  : Load all previous studies from a base directory
- [`generate_learning_context()`](https://abanoub-armanious.github.io/pakhom/reference/generate_learning_context.md)
  : Generate task-specific learning context from previous analyses
- [`generate_learning_reflection()`](https://abanoub-armanious.github.io/pakhom/reference/generate_learning_reflection.md)
  : Generate AI reflection on what was learned from previous studies
- [`discover_study_folders()`](https://abanoub-armanious.github.io/pakhom/reference/discover_study_folders.md)
  : Discover study folders matching a pattern
- [`extract_manuscript_sections()`](https://abanoub-armanious.github.io/pakhom/reference/extract_manuscript_sections.md)
  : Extract structured sections from manuscript text
- [`parse_manuscript()`](https://abanoub-armanious.github.io/pakhom/reference/parse_manuscript.md)
  : Parse a finalized themes manuscript (DOCX or PDF)

## Human Verification

Inter-rater reliability and code verification

- [`run_human_verification()`](https://abanoub-armanious.github.io/pakhom/reference/run_human_verification.md)
  : Run human verification / IRR process
- [`verify_excerpts()`](https://abanoub-armanious.github.io/pakhom/reference/verify_excerpts.md)
  : Verify coded excerpts against source text

## Report & Export

HTML report, CSV/JSON exports, and QDA-software interoperability

- [`generate_report()`](https://abanoub-armanious.github.io/pakhom/reference/generate_report.md)
  : Generate the full HTML analysis report
- [`generate_mode1_report()`](https://abanoub-armanious.github.io/pakhom/reference/generate_mode1_report.md)
  : Generate the Mode 1 (Reflexive Scaffold) HTML analysis report
- [`export_results()`](https://abanoub-armanious.github.io/pakhom/reference/export_results.md)
  : Export all analysis results to files
- [`export_qdpx()`](https://abanoub-armanious.github.io/pakhom/reference/export_qdpx.md)
  : Export coding results to QDPX format
- [`export_theme_subtheme_summary_csvs()`](https://abanoub-armanious.github.io/pakhom/reference/export_theme_subtheme_summary_csvs.md)
  : Export per-theme paper-style subtheme-summary CSVs (Phase 55)

## Comparison

Compare results across pipeline runs and across AI models

- [`compare_runs()`](https://abanoub-armanious.github.io/pakhom/reference/compare_runs.md)
  : Compare the current run against all previous runs
- [`compare_models()`](https://abanoub-armanious.github.io/pakhom/reference/compare_models.md)
  : Compare runs that used different AI models for inter-model
  reliability
- [`list_available_runs()`](https://abanoub-armanious.github.io/pakhom/reference/list_available_runs.md)
  : List available analysis runs
- [`compare_theme_groups()`](https://abanoub-armanious.github.io/pakhom/reference/compare_theme_groups.md)
  : Compare continuous variables across theme groups using Mann-Whitney
  U tests

## Temporal Analysis

Within-run longitudinal analysis when entries have timestamps

- [`analyze_temporal_patterns()`](https://abanoub-armanious.github.io/pakhom/reference/analyze_temporal_patterns.md)
  : Analyse temporal patterns in theme prevalence within a single run
- [`generate_temporal_plots()`](https://abanoub-armanious.github.io/pakhom/reference/generate_temporal_plots.md)
  : Generate PNG plots for temporal analysis results

## Audit Log

JSONL trail of every AI decision for post-hoc transparency review

- [`init_audit_log()`](https://abanoub-armanious.github.io/pakhom/reference/init_audit_log.md)
  : Initialize the AI decision audit log

- [`close_audit_log()`](https://abanoub-armanious.github.io/pakhom/reference/close_audit_log.md)
  : Close the audit log file connection

- [`log_ai_decision()`](https://abanoub-armanious.github.io/pakhom/reference/log_ai_decision.md)
  : Record a single AI decision in the audit log

- [`log_ai_request()`](https://abanoub-armanious.github.io/pakhom/reference/log_ai_request.md)
  :

  Record an AI request with the structured response from `ai_complete`

- [`summarize_audit_log()`](https://abanoub-armanious.github.io/pakhom/reference/summarize_audit_log.md)
  : Summarize the AI decision audit log

## Mode 1 Live Tracking

Live in-memory tracking of Mode 1 (reflexive scaffold) provocation
progress so the orchestrator can monitor cluster coverage and code
assignments mid-run.

- [`init_live_tracker()`](https://abanoub-armanious.github.io/pakhom/reference/init_live_tracker.md)
  : Initialize the live tracker for a run

- [`live_record_assignment()`](https://abanoub-armanious.github.io/pakhom/reference/live_record_assignment.md)
  : Record one (entry, code, segment) assignment to the live tracker

- [`live_snapshot_codebook()`](https://abanoub-armanious.github.io/pakhom/reference/live_snapshot_codebook.md)
  :

  Snapshot the current codebook to `codebook_live.json`

- [`live_snapshot_clusters()`](https://abanoub-armanious.github.io/pakhom/reference/live_snapshot_clusters.md)
  :

  Snapshot the current theme/cluster hierarchy to `code_to_cluster.json`

## Statistical Analysis

Correlation and co-occurrence helpers

- [`aggregate_overall_statistics()`](https://abanoub-armanious.github.io/pakhom/reference/aggregate_overall_statistics.md)
  : Aggregate overall analysis statistics for report
- [`aggregate_theme_statistics()`](https://abanoub-armanious.github.io/pakhom/reference/aggregate_theme_statistics.md)
  : Aggregate per-theme statistics for report
- [`test_theme_cooccurrence()`](https://abanoub-armanious.github.io/pakhom/reference/test_theme_cooccurrence.md)
  : Test theme co-occurrence patterns with chi-square tests of
  independence
- [`create_theme_network()`](https://abanoub-armanious.github.io/pakhom/reference/create_theme_network.md)
  : Create theme co-occurrence network visualization
- [`create_correlation_plot()`](https://abanoub-armanious.github.io/pakhom/reference/create_correlation_plot.md)
  : Create correlation plot
- [`interpret_correlations()`](https://abanoub-armanious.github.io/pakhom/reference/interpret_correlations.md)
  : Interpret correlation results for reporting
- [`prepare_correlation_data()`](https://abanoub-armanious.github.io/pakhom/reference/prepare_correlation_data.md)
  : Prepare data for correlation analysis
- [`extract_significant()`](https://abanoub-armanious.github.io/pakhom/reference/extract_significant.md)
  : Extract significant correlations as tidy tibble

## Scraper

Reddit data collection

- [`scrape_reddit()`](https://abanoub-armanious.github.io/pakhom/reference/scrape_reddit.md)
  : Scrape Reddit subreddits into a SQLite database

## Pipeline Integrity

Verify pipeline run integrity

- [`verify_run_integrity()`](https://abanoub-armanious.github.io/pakhom/reference/verify_run_integrity.md)
  : Verify that a run directory contains all expected output files

## Pipeline Step Helpers

Exported functions called from inside the pipeline that researchers may
also call directly for finer control (e.g., feeding external coding
state through enrichment + cascade independently).

- [`as_coding_results()`](https://abanoub-armanious.github.io/pakhom/reference/as_coding_results.md)
  : Convert ProgressiveCodingState to legacy CodingResults format
- [`compute_coding_benchmarks()`](https://abanoub-armanious.github.io/pakhom/reference/compute_coding_benchmarks.md)
  : Compute empirical coding benchmarks from parsed QDA codebooks
- [`enrich_themes()`](https://abanoub-armanious.github.io/pakhom/reference/enrich_themes.md)
  : Enrich themes with entry counts, sentiment, and quotes
- [`parse_codebook()`](https://abanoub-armanious.github.io/pakhom/reference/parse_codebook.md)
  : Parse a QDA software codebook export (NVivo, ATLAS.ti, MAXQDA, or
  generic)

## Package

- [`pakhom-package`](https://abanoub-armanious.github.io/pakhom/reference/pakhom-package.md)
  [`pakhom`](https://abanoub-armanious.github.io/pakhom/reference/pakhom-package.md)
  : pakhom: AI-Assisted Reflexive Thematic Analysis
