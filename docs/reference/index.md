# Package index

## Run an Analysis

Core entry points for running the pipeline

- [`run_analysis()`](run_analysis.md) : Run the full thematic analysis
  pipeline
- [`load_config()`](load_config.md) : Load analysis configuration from
  YAML file
- [`create_config()`](create_config.md) : Create a minimal configuration
  file
- [`config_wizard()`](config_wizard.md) : Interactive configuration
  wizard
- [`config_wizard_app()`](config_wizard_app.md) : Launch the interactive
  configuration wizard

## Configuration

Configuration creation, validation, and defaults

- [`default_config()`](default_config.md) : Create a default
  configuration object

## Data Loading & Exploration

Load data from SQLite and explore databases

- [`load_data()`](load_data.md) : Load data from a SQLite database
- [`explore_database()`](explore_database.md) : Explore a SQLite
  database schema
- [`detect_variable_types()`](detect_variable_types.md) : Detect
  variable types for dynamic correlation method selection

## AI Provider

AI provider abstraction for OpenAI and Anthropic

- [`create_ai_provider()`](create_ai_provider.md) : Create an AI
  provider client

## Pipeline Steps

Individual analysis steps (called by run_analysis)

- [`run_progressive_coding()`](run_progressive_coding.md) : Run
  progressive sequential coding on all entries
- [`create_coding_state()`](create_coding_state.md) : Create a new
  progressive coding state
- [`get_analytic_sample()`](get_analytic_sample.md) : Get the analytic
  sample (entries that received at least one code)
- [`analyze_sentiment()`](analyze_sentiment.md) : Run batch sentiment
  analysis on all entries
- [`generate_themes_iterative()`](generate_themes_iterative.md) :
  Generate themes through iterative bottom-up merging
- [`cascade_theme_assignments()`](cascade_theme_assignments.md) :
  Cascade theme assignments from codes to entries deterministically
- [`enrich_themes()`](enrich_themes.md) : Enrich themes with entry
  counts, sentiment, and quotes
- [`calculate_correlations()`](calculate_correlations.md) : Calculate
  correlation matrix with p-values

## Themes & Theme Data

Working with ThemeSet objects

- [`create_theme_set()`](create_theme_set.md) : Create a ThemeSet object
  (canonical internal representation)
- [`theme_names()`](theme_names.md) : Extract theme names from ThemeSet
- [`n_themes()`](n_themes.md) : Get the number of themes
- [`theme_set_to_tibble()`](theme_set_to_tibble.md) : Convert ThemeSet
  to tibble for export/inspection
- [`normalize_theme_result()`](normalize_theme_result.md) : Normalize
  raw AI theme output to canonical ThemeSet
- [`prune_empty_themes()`](prune_empty_themes.md) : Remove themes with
  zero assigned entries after enrichment

## Checkpoint & Resume

Save and restore pipeline progress

- [`init_checkpoints()`](init_checkpoints.md) : Initialize checkpoint
  system for a pipeline run

## Manuscript Learning

Learn from prior manual thematic analyses

- [`load_previous_studies()`](load_previous_studies.md) : Load all
  previous studies from a base directory
- [`generate_learning_context()`](generate_learning_context.md) :
  Generate task-specific learning context from previous analyses
- [`generate_learning_reflection()`](generate_learning_reflection.md) :
  Generate AI reflection on what was learned from previous studies

## Human Verification

Inter-rater reliability and code verification

- [`run_human_verification()`](run_human_verification.md) : Run human
  verification / IRR process
- [`verify_excerpts()`](verify_excerpts.md) : Verify coded excerpts
  against source text

## Report & Export

HTML report, CSV/JSON exports, and QDA-software interoperability

- [`generate_report()`](generate_report.md) : Generate the full HTML
  analysis report
- [`export_results()`](export_results.md) : Export all analysis results
  to files
- [`export_qdpx()`](export_qdpx.md) : Export coding results to QDPX
  format

## Comparison

Compare results across pipeline runs and across AI models

- [`compare_runs()`](compare_runs.md) : Compare the current run against
  all previous runs
- [`compare_models()`](compare_models.md) : Compare runs that used
  different AI models for inter-model reliability
- [`list_available_runs()`](list_available_runs.md) : List available
  analysis runs
- [`compare_theme_groups()`](compare_theme_groups.md) : Compare
  continuous variables across theme groups using Mann-Whitney U tests

## Temporal Analysis

Within-run longitudinal analysis when entries have timestamps

- [`analyze_temporal_patterns()`](analyze_temporal_patterns.md) :
  Analyse temporal patterns in theme prevalence within a single run
- [`generate_temporal_plots()`](generate_temporal_plots.md) : Generate
  PNG plots for temporal analysis results

## Audit Log

JSONL trail of every AI decision for post-hoc transparency review

- [`init_audit_log()`](init_audit_log.md) : Initialize the AI decision
  audit log
- [`close_audit_log()`](close_audit_log.md) : Close the audit log file
  connection

## Statistical Analysis

Correlation and co-occurrence helpers

- [`aggregate_overall_statistics()`](aggregate_overall_statistics.md) :
  Aggregate overall analysis statistics for report
- [`aggregate_theme_statistics()`](aggregate_theme_statistics.md) :
  Aggregate per-theme statistics for report
- [`test_theme_cooccurrence()`](test_theme_cooccurrence.md) : Test theme
  co-occurrence patterns with chi-square tests of independence
- [`create_theme_network()`](create_theme_network.md) : Create theme
  co-occurrence network visualization

## Scraper

Reddit data collection

- [`scrape_reddit()`](scrape_reddit.md) : Scrape Reddit subreddits into
  a SQLite database

## Pipeline Integrity

Verify pipeline run integrity

- [`verify_run_integrity()`](verify_run_integrity.md) : Verify that a
  run directory contains all expected output files

## Internal Helpers

Internal functions used by the pipeline (not typically called directly)

- [`aggregate_overall_statistics()`](aggregate_overall_statistics.md) :
  Aggregate overall analysis statistics for report
- [`aggregate_theme_statistics()`](aggregate_theme_statistics.md) :
  Aggregate per-theme statistics for report
- [`ai_complete()`](ai_complete.md) : High-level AI completion with
  retry and error handling
- [`ai_complete_fast()`](ai_complete_fast.md) : Send a quick completion
  using the fast/cheap model
- [`analyze_sentiment()`](analyze_sentiment.md) : Run batch sentiment
  analysis on all entries
- [`analyze_temporal_patterns()`](analyze_temporal_patterns.md) :
  Analyse temporal patterns in theme prevalence within a single run
- [`as_coding_results()`](as_coding_results.md) : Convert
  ProgressiveCodingState to legacy CodingResults format
- [`calculate_correlations()`](calculate_correlations.md) : Calculate
  correlation matrix with p-values
- [`cascade_theme_assignments()`](cascade_theme_assignments.md) :
  Cascade theme assignments from codes to entries deterministically
- [`close_audit_log()`](close_audit_log.md) : Close the audit log file
  connection
- [`compare_models()`](compare_models.md) : Compare runs that used
  different AI models for inter-model reliability
- [`compare_runs()`](compare_runs.md) : Compare the current run against
  all previous runs
- [`compare_theme_groups()`](compare_theme_groups.md) : Compare
  continuous variables across theme groups using Mann-Whitney U tests
- [`compute_coding_benchmarks()`](compute_coding_benchmarks.md) :
  Compute empirical coding benchmarks from parsed QDA codebooks
- [`config_wizard()`](config_wizard.md) : Interactive configuration
  wizard
- [`config_wizard_app()`](config_wizard_app.md) : Launch the interactive
  configuration wizard
- [`create_ai_provider()`](create_ai_provider.md) : Create an AI
  provider client
- [`create_coding_state()`](create_coding_state.md) : Create a new
  progressive coding state
- [`create_config()`](create_config.md) : Create a minimal configuration
  file
- [`create_correlation_plot()`](create_correlation_plot.md) : Create
  correlation plot
- [`create_theme_network()`](create_theme_network.md) : Create theme
  co-occurrence network visualization
- [`create_theme_set()`](create_theme_set.md) : Create a ThemeSet object
  (canonical internal representation)
- [`default_config()`](default_config.md) : Create a default
  configuration object
- [`detect_columns()`](detect_columns.md) : Detect and map columns based
  on platform type
- [`detect_variable_types()`](detect_variable_types.md) : Detect
  variable types for dynamic correlation method selection
- [`discover_study_folders()`](discover_study_folders.md) : Discover
  study folders matching a pattern
- [`.THEME_DEFAULTS`](dot-THEME_DEFAULTS.md) : Default values for
  optional theme fields
- [`.THEME_REQUIRED_FIELDS`](dot-THEME_REQUIRED_FIELDS.md) : Required
  fields for each theme within a ThemeSet
- [`enrich_themes()`](enrich_themes.md) : Enrich themes with entry
  counts, sentiment, and quotes
- [`explore_database()`](explore_database.md) : Explore a SQLite
  database schema
- [`export_qdpx()`](export_qdpx.md) : Export coding results to QDPX
  format
- [`export_results()`](export_results.md) : Export all analysis results
  to files
- [`export_theme_entry_csvs()`](export_theme_entry_csvs.md) : Export CSV
  files for each theme's entries
- [`extract_manuscript_sections()`](extract_manuscript_sections.md) :
  Extract structured sections from manuscript text
- [`extract_significant()`](extract_significant.md) : Extract
  significant correlations as tidy tibble
- [`find_latest_run()`](find_latest_run.md) : Find the most recent run
  folder in the results directory
- [`find_resume_point()`](find_resume_point.md) : Determine the last
  completed step for resume
- [`generate_ai_synthesis()`](generate_ai_synthesis.md) : Generate
  AI-powered executive summary and conclusion
- [`generate_downloads_section()`](generate_downloads_section.md) :
  Generate downloads appendix section
- [`generate_insights()`](generate_insights.md) : Generate AI insights
  from correlation findings
- [`generate_learning_context()`](generate_learning_context.md) :
  Generate task-specific learning context from previous analyses
- [`generate_learning_reflection()`](generate_learning_reflection.md) :
  Generate AI reflection on what was learned from previous studies
- [`generate_report()`](generate_report.md) : Generate the full HTML
  analysis report
- [`generate_run_id()`](generate_run_id.md) : Generate a unique run ID
  based on timestamp
- [`generate_temporal_plots()`](generate_temporal_plots.md) : Generate
  PNG plots for temporal analysis results
- [`generate_theme_detail_section()`](generate_theme_detail_section.md)
  : Generate a theme detail section for appendix
- [`generate_themes_iterative()`](generate_themes_iterative.md) :
  Generate themes through iterative bottom-up merging
- [`get_analytic_sample()`](get_analytic_sample.md) : Get the analytic
  sample (entries that received at least one code)
- [`get_emotion_interpretation()`](get_emotion_interpretation.md) : Get
  interpretation text for an emotion
- [`hash_config()`](hash_config.md) : Compute a hash of a config file
  for change detection
- [`init_audit_log()`](init_audit_log.md) : Initialize the AI decision
  audit log
- [`init_checkpoints()`](init_checkpoints.md) : Initialize checkpoint
  system for a pipeline run
- [`interpret_correlations()`](interpret_correlations.md) : Interpret
  correlation results for reporting
- [`list_available_runs()`](list_available_runs.md) : List available
  analysis runs
- [`list_checkpoints()`](list_checkpoints.md) : List available
  checkpoints with metadata
- [`load_and_combine_tables()`](load_and_combine_tables.md) : Load and
  combine multiple tables from a SQLite database
- [`load_checkpoint()`](load_checkpoint.md) : Load checkpoint for a step
- [`load_config()`](load_config.md) : Load analysis configuration from
  YAML file
- [`load_data()`](load_data.md) : Load data from a SQLite database
- [`load_previous_studies()`](load_previous_studies.md) : Load all
  previous studies from a base directory
- [`log_ai_decision()`](log_ai_decision.md) : Record a single AI
  decision in the audit log
- [`make_anchor_id()`](make_anchor_id.md) : Create an HTML anchor ID
  from a string
- [`make_safe_filename()`](make_safe_filename.md) : Create a safe
  filename from a string
- [`n_themes()`](n_themes.md) : Get the number of themes
- [`normalize_theme_result()`](normalize_theme_result.md) : Normalize
  raw AI theme output to canonical ThemeSet
- [`parse_codebook()`](parse_codebook.md) : Parse a QDA software
  codebook export (NVivo, ATLAS.ti, MAXQDA, or generic)
- [`parse_json_safely()`](parse_json_safely.md) : Parse JSON safely with
  automatic repair for truncated/malformed responses
- [`parse_manuscript()`](parse_manuscript.md) : Parse a finalized themes
  manuscript (DOCX or PDF)
- [`parse_raw_data_files()`](parse_raw_data_files.md) : Parse raw data
  DOCX files and extract metadata from filenames
- [`prepare_correlation_data()`](prepare_correlation_data.md) : Prepare
  data for correlation analysis
- [`preprocess_text()`](preprocess_text.md) : Preprocess text data for
  analysis
- [`print(`*`<AIProvider>`*`)`](print.AIProvider.md) : Print method for
  AIProvider
- [`print(`*`<CheckpointManager>`*`)`](print.CheckpointManager.md) :
  Print method for CheckpointManager
- [`print(`*`<ComparisonResult>`*`)`](print.ComparisonResult.md) : Print
  method for ComparisonResult
- [`print(`*`<ThematicConfig>`*`)`](print.ThematicConfig.md) : Print
  method for ThematicConfig
- [`print(`*`<ThemeSet>`*`)`](print.ThemeSet.md) : Print method for
  ThemeSet
- [`prune_empty_themes()`](prune_empty_themes.md) : Remove themes with
  zero assigned entries after enrichment
- [`run_analysis()`](run_analysis.md) : Run the full thematic analysis
  pipeline
- [`run_human_verification()`](run_human_verification.md) : Run human
  verification / IRR process
- [`run_progressive_coding()`](run_progressive_coding.md) : Run
  progressive sequential coding on all entries
- [`save_checkpoint()`](save_checkpoint.md) : Save checkpoint at a given
  pipeline step
- [`save_partial_checkpoint()`](save_partial_checkpoint.md) : Save
  partial checkpoint within a step (for long-running batch operations)
- [`scrape_reddit()`](scrape_reddit.md) : Scrape Reddit subreddits into
  a SQLite database
- [`standardize_data()`](standardize_data.md) : Standardize data to
  common schema
- [`summarize_audit_log()`](summarize_audit_log.md) : Summarize the AI
  decision audit log
- [`test_theme_cooccurrence()`](test_theme_cooccurrence.md) : Test theme
  co-occurrence patterns with chi-square tests of independence
- [`pakhom-package`](pakhom-package.md)
  [`pakhom`](pakhom-package.md) : pakhom: AI-Assisted
  Reflexive Thematic Analysis
- [`theme_names()`](theme_names.md) : Extract theme names from ThemeSet
- [`theme_set_to_tibble()`](theme_set_to_tibble.md) : Convert ThemeSet
  to tibble for export/inspection
- [`truncate_text()`](truncate_text.md) : Truncate text to specified
  length with ellipsis
- [`validate_config()`](validate_config.md) : Validate configuration
  completeness and correctness
- [`verify_excerpts()`](verify_excerpts.md) : Verify coded excerpts
  against source text
- [`verify_run_integrity()`](verify_run_integrity.md) : Verify that a
  run directory contains all expected output files
- [`compute_dynamic_batches()`](compute_dynamic_batches.md) : Compute
  dynamic batch indices based on token budget
- [`compute_embeddings()`](compute_embeddings.md) : Compute text
  embeddings via AI provider
- [`estimate_tokens()`](estimate_tokens.md) : Estimate token count for
  text
- [`generate_saturation_plot()`](generate_saturation_plot.md) : Generate
  saturation curve plot
- [`` `%||%` ``](null-coalesce.md) : Null-coalescing operator (re-export
  from rlang)
- [`review_progressive_codebook()`](review_progressive_codebook.md) :
  Export progressive codebook for researcher review
- [`review_themes()`](review_themes.md) : Export theme review sheet and
  apply modifications on resume
- [`safe_progress_bar()`](safe_progress_bar.md) : Create a progress bar
  that works in non-interactive/background mode
- [`validate_class()`](validate_class.md) : Validate that an object
  inherits from a given class
- [`validate_data_columns()`](validate_data_columns.md) : Validate that
  a data frame has required columns
- [`validate_provider()`](validate_provider.md) : Validate that an AI
  provider is properly constructed
