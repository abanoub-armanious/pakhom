# ==============================================================================
# Report Generation -- Rmd-based HTML Report with Exports
# ==============================================================================
# Replaces ~2000 lines of inline HTML string concatenation from the old script.
# Uses an external Rmd template + CSS file from inst/rmd/.
# ==============================================================================

#' Escape strings for safe HTML embedding
#' @param x Character string to escape
#' @return HTML-safe string
#' @keywords internal
.html_esc <- function(x) {
  if (is.null(x) || is.na(x)) return("")
  x <- as.character(x)
  if (requireNamespace("htmltools", quietly = TRUE)) {
    # htmltools::htmlEscape doesn't escape quotes by default
    out <- as.character(htmltools::htmlEscape(x))
    out <- gsub('"', "&quot;", out, fixed = TRUE)
    gsub("'", "&#39;", out, fixed = TRUE)
  } else {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x <- gsub("'", "&#39;", x, fixed = TRUE)
    x
  }
}

#' Export all analysis results to files
#'
#' @param data tibble with all analysis columns
#' @param theme_set ThemeSet object
#' @param correlations_df Correlations tibble
#' @param insights Insights list
#' @param consolidated ConsolidatedCodes list
#' @param output_dir Output directory path
#' @param methodology_mode Optional methodology mode (T1.7). When
#'   non-NULL, every CSV produced is stamped with a comment header
#'   identifying the mode and run id (per AC4). NULL skips stamping --
#'   used by tests / legacy callers.
#' @return List of export file paths
#' @export
export_results <- function(data, theme_set, correlations_df, insights,
                            consolidated, output_dir,
                            methodology_mode = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  export_files <- list()
  # T1.7 (AC4): methodology stamp on every CSV produced by this run.
  # Helper closes over methodology_mode + output_dir so the call sites
  # below stay one-line.
  .stamp <- function(path) {
    if (is.null(methodology_mode) || !file.exists(path)) return(invisible(NULL))
    tryCatch(stamp_methodology_csv(path, methodology_mode,
                                     run_id = basename(output_dir)),
             error = function(e) log_debug("CSV stamp skipped: {e$message}"))
  }

  # --- Sentiment scores CSV ---
  sentiment_file <- file.path(output_dir, "sentiment_scores.csv")
  sentiment_cols <- intersect(
    c("std_id", "std_text", "sentiment_score", "all_emotions",
      "emotion_intensity", "confidence", "emerged_themes", "n_themes"),
    names(data)
  )
  readr::write_csv(data[, sentiment_cols, drop = FALSE], sentiment_file)
  .stamp(sentiment_file)
  export_files$sentiment_file <- sentiment_file
  log_info("Exported sentiment scores: {sentiment_file}")

  # --- Consolidated codes CSV (Bug #2 fix: actually writes data) ---
  codes_file <- file.path(output_dir, "consolidated_codes.csv")
  if (!is.null(consolidated) && !is.null(consolidated$codes) && nrow(consolidated$codes) > 0) {
    readr::write_csv(consolidated$codes, codes_file)
    log_info("Exported {nrow(consolidated$codes)} consolidated codes: {codes_file}")
  } else {
    readr::write_csv(tibble(code_text = character(), frequency = integer(), code_type = character()),
                      codes_file)
    log_warn("No consolidated codes to export")
  }
  .stamp(codes_file)
  export_files$codes_file <- codes_file

  # --- Correlations CSV ---
  correlations_file <- file.path(output_dir, "correlations.csv")
  if (!is.null(correlations_df) && nrow(correlations_df) > 0) {
    readr::write_csv(correlations_df, correlations_file)
    log_info("Exported {nrow(correlations_df)} correlation pairs: {correlations_file}")
  } else {
    readr::write_csv(tibble(var1 = character(), var2 = character(),
                             correlation = numeric(), p_value = numeric(),
                             significant = logical(), effect_size = character()),
                      correlations_file)
  }
  .stamp(correlations_file)
  export_files$correlations_file <- correlations_file

  # --- Themes JSON ---
  themes_file <- file.path(output_dir, "themes.json")
  themes_json <- theme_set_to_tibble(theme_set)
  jsonlite::write_json(themes_json, themes_file, pretty = TRUE, auto_unbox = TRUE)
  if (!is.null(methodology_mode)) {
    tryCatch(stamp_methodology_json(themes_file, methodology_mode,
                                      run_id = basename(output_dir)),
             error = function(e) log_debug("JSON stamp skipped: {e$message}"))
  }
  export_files$themes_file <- themes_file
  log_info("Exported themes: {themes_file}")

  # --- Per-theme CSV files ---
  theme_csv_files <- export_theme_entry_csvs(data, theme_set, output_dir,
                                              methodology_mode = methodology_mode)
  export_files$theme_csv_files <- theme_csv_files

  # --- Correlation plot ---
  export_files$plot_file <- file.path(output_dir, "correlation_plot.png")

  log_info("All results exported to: {output_dir}")
  export_files
}

#' Verify that a run directory contains all expected output files
#'
#' Checks for the core data files that every completed run should contain,
#' plus conditional files based on config settings.
#'
#' @param run_dir Path to the run directory
#' @param config ThematicConfig (or list) used for the run, to check conditional outputs
#' @return List with `expected` (all expected files), `present` (found files),
#'   `missing` (expected but not found), `complete` (logical)
#' @export
verify_run_integrity <- function(run_dir, config = list()) {
  # Phase 31: dispatch on methodology mode. Mode 1 (Reflexive Scaffold)
  # produces a different artifact set from Modes 2/3 (no sentiment, no
  # correlations, no theme_entries directory) -- a unified expected
  # list would silently mark every Mode 1 run as incomplete.
  meth_mode <- tryCatch(config$methodology$mode, error = function(e) NULL)
  if (identical(meth_mode, "reflexive_scaffold")) {
    return(.verify_run_integrity_mode1(run_dir, config))
  }

  # Core files every completed run must have
  expected <- c(
    "sentiment_scores.csv",
    "consolidated_codes.csv",
    "correlations.csv",
    "themes.json",
    "theme_entries",
    "analysis_report.Rmd",
    # Sprint-4 Tier-0 + Tier-1 outputs that MUST be present in any
    # complete run. Per AC4 (methodology stamped on every output),
    # integrity check must verify these exist -- otherwise a run that
    # silently lost the audit trail would still report complete=TRUE.
    "run_metadata.json",            # T1.5: REDCap-style state record
    "rules/methodology_rules.md",   # T1.6: archived rules text
    "fabrication_log.csv",          # T0.1: anti-fabrication audit trail
    "ai_decisions.jsonl"            # T1.4: AI decision audit log
  )

  # Conditional files based on config
  if (isTRUE(config$output$generate_report)) {
    expected <- c(expected, "analysis_report.html", "styles.css", "theme_details")
  }
  if (isTRUE(config$output$generate_correlation_plot)) {
    expected <- c(expected, "correlation_plot.png")
  }
  # T1.4: when raw-response capture is enabled (default TRUE), the
  # api_responses/ directory MUST exist for replay_run() to work.
  if (isTRUE(config$audit$capture_raw_responses %||% TRUE)) {
    expected <- c(expected, "api_responses")
  }

  present <- expected[file.exists(file.path(run_dir, expected))]
  missing <- setdiff(expected, present)

  list(
    expected = expected,
    present = present,
    missing = missing,
    complete = length(missing) == 0
  )
}

#' Export CSV files for each theme's entries
#'
#' @param data tibble with theme_membership_* or emerged_themes columns
#' @param theme_set ThemeSet object
#' @param output_dir Output directory
#' @param methodology_mode Optional methodology mode (T1.7). When
#'   non-NULL, every CSV produced is stamped with a comment header
#'   identifying the mode and run id (per AC4). NULL skips stamping --
#'   used by tests / legacy callers.
#' @return Named list of file info per theme
export_theme_entry_csvs <- function(data, theme_set, output_dir,
                                      methodology_mode = NULL) {
  theme_dir <- file.path(output_dir, "theme_entries")
  dir.create(theme_dir, recursive = TRUE, showWarnings = FALSE)

  theme_csv_files <- list()
  # T0.2: include std_author so per-theme CSVs preserve the contributor data
  # the participant-spread metrics on the dashboard were computed from. Per
  # AC4 (methodology stamped on every output), Tier-0-relevant columns
  # propagate to all output artifacts -- silent omission would let a
  # downstream consumer recompute participant spread from the wrong shape.
  export_cols <- intersect(
    c("std_id", "std_text", "std_author", "sentiment_score", "all_emotions",
      "emotion_intensity", "emerged_themes", "n_themes", "source_table"),
    names(data)
  )

  for (tn in theme_names(theme_set)) {
    # Use multi-label membership column to find all entries in this theme
    safe_col <- paste0("theme_membership_", make.names(tn))
    if (safe_col %in% names(data)) {
      entries <- data[data[[safe_col]] == 1L, ]
    } else if ("emerged_themes" %in% names(data)) {
      entries <- data[!is.na(data$emerged_themes) &
                       grepl(tn, data$emerged_themes, fixed = TRUE), ]
    } else {
      next
    }
    if (nrow(entries) == 0) next

    safe_name <- make_safe_filename(tn)
    csv_path <- file.path(theme_dir, paste0(safe_name, ".csv"))
    readr::write_csv(entries[, intersect(export_cols, names(entries)), drop = FALSE], csv_path)
    # T1.7 (AC4): stamp the file with the methodology mode so any
    # downstream consumer parsing the CSV sees the declaration up-front.
    # The stamp is a comment-style header line; readr::read_csv with
    # comment = "#" strips it transparently.
    if (!is.null(methodology_mode)) {
      tryCatch(stamp_methodology_csv(csv_path, methodology_mode,
                                       run_id = basename(output_dir)),
               error = function(e) log_debug("CSV stamp skipped: {e$message}"))
    }

    theme_csv_files[[tn]] <- list(
      file_path = csv_path,
      relative_path = file.path("theme_entries", paste0(safe_name, ".csv"))
    )
  }

  # Master CSV with all entries that have any theme assignment
  master_path <- file.path(theme_dir, "all_entries_by_theme.csv")
  master_data <- data |>
    filter(!is.na(emerged_themes)) |>
    arrange(emerged_themes) |>
    select(any_of(export_cols))
  readr::write_csv(master_data, master_path)
  if (!is.null(methodology_mode)) {
    tryCatch(stamp_methodology_csv(master_path, methodology_mode,
                                     run_id = basename(output_dir)),
             error = function(e) log_debug("CSV stamp skipped: {e$message}"))
  }

  log_info("Exported {length(theme_csv_files)} theme CSV files + master CSV")
  theme_csv_files
}

#' Generate the full HTML analysis report
#'
#' Builds an Rmd file from data and renders it to HTML.
#'
#' @param data tibble with all analysis columns
#' @param theme_set ThemeSet object
#' @param correlations_df Correlations tibble
#' @param insights Insights list
#' @param export_files List of export file paths
#' @param consolidated ConsolidatedCodes list (or NULL)
#' @param learning_context LearningContext object (or NULL)
#' @param provider AIProvider object (or NULL)
#' @param config ThematicConfig object (or NULL)
#' @param output_file Path for the HTML report
#' @param irr_result Inter-rater reliability result list (or NULL)
#' @param comparison_result ComparisonResult object from compare_runs() (or NULL)
#' @param self_contained If TRUE (default), produce a self-contained HTML file
#'   with all resources embedded. Set to FALSE for faster rendering and smaller
#'   file size (external CSS/JS will be referenced).
#' @param coding_results Legacy CodingResults list as returned by
#'   \code{as_coding_results}. Used to populate per-theme entry tables with
#'   the codes assigned to each entry. Pass NULL to omit the codes column.
#' @param coding_state ProgressiveCodingState with saturation data (or NULL)
#' @param excerpt_verification Optional list returned by \code{verify_excerpts}
#'   containing substring_stats and (optionally) coherence_stats. When
#'   provided, the report's data-quality appendix shows excerpt validation
#'   results.
#' @param theme_group_tests Optional tibble returned by
#'   \code{compare_theme_groups} (Mann-Whitney U tests). When provided, the
#'   correlation section gains a 'Theme Group Comparisons' subsection.
#' @param cooccurrence_tests Optional tibble returned by
#'   \code{test_theme_cooccurrence} (chi-square / Fisher tests). When
#'   provided, the correlation section gains a 'Theme Co-occurrence'
#'   subsection.
#' @param audit_log Optional \code{AuditLog} object (T1.4) forwarded to
#'   \code{generate_ai_synthesis} so the executive-summary AI call is
#'   recorded as an \code{ai_request} audit decision.
#' @param response_cache Optional \code{ResponseCache} object (T1.4)
#'   forwarded to \code{generate_ai_synthesis} so the raw API response is
#'   written to the cache and referenced from the audit log.
#' @param coverage Optional \code{CorpusCoverage} object (T0.3) from
#'   \code{\link{compute_corpus_coverage}}. When provided, the report
#'   renders a Tier-0 corpus-coverage card asserting that every entry
#'   surviving preprocessing reached the LLM (no silent truncation).
#'   When NULL the card renders an explicit "coverage not computed"
#'   notice rather than silently omitting -- absence is itself a
#'   transparency signal per AC4.
#' @return Path to generated HTML report
#' @export
generate_report <- function(data, theme_set, correlations_df, insights,
                             export_files, consolidated = NULL,
                             learning_context = NULL, provider = NULL,
                             config = NULL, output_file = "analysis_report.html",
                             irr_result = NULL, comparison_result = NULL,
                             self_contained = TRUE, coding_results = NULL,
                             coding_state = NULL,
                             excerpt_verification = NULL,
                             theme_group_tests = NULL,
                             cooccurrence_tests = NULL,
                             audit_log = NULL,
                             response_cache = NULL,
                             coverage = NULL) {
  validate_class(theme_set, "ThemeSet")

  # Validate inputs
  stopifnot(
    is.data.frame(data),
    is.data.frame(correlations_df) || is.null(correlations_df)
  )

  log_info("Generating HTML report...")
  tic("Report generation")

  # Aggregate statistics
  theme_stats <- aggregate_theme_statistics(data, theme_set, consolidated)
  overall_stats <- aggregate_overall_statistics(data, theme_set, consolidated,
                                                 learning_context, config)

  # AI synthesis
  ai_synthesis <- generate_ai_synthesis(overall_stats, theme_stats, correlations_df,
                                         insights, theme_set, provider,
                                         config = config,
                                         audit_log = audit_log,
                                         response_cache = response_cache)

  # Correlation interpretation
  corr_interpretation <- interpret_correlations(correlations_df, theme_stats)

  # Theme ordering by prevalence
  theme_order <- overall_stats$themes |> arrange(desc(n)) |> pull(theme_name)

  # Determine output paths
  output_dir <- dirname(output_file)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  rmd_file <- gsub("\\.html$", ".Rmd", output_file)

  # Build the R Markdown content
  rmd_content <- .build_rmd_content(
    overall_stats = overall_stats,
    theme_stats = theme_stats,
    theme_order = theme_order,
    ai_synthesis = ai_synthesis,
    corr_interpretation = corr_interpretation,
    insights = insights,
    export_files = export_files,
    config = config,
    irr_result = irr_result,
    comparison_result = comparison_result,
    self_contained = self_contained,
    theme_group_tests = theme_group_tests,
    cooccurrence_tests = cooccurrence_tests,
    excerpt_verification = excerpt_verification,
    coding_state = coding_state,
    coverage = coverage,
    run_id = basename(output_dir)
  )

  # Write Rmd (collapse to single string to prevent duplication)
  rmd_content <- paste(rmd_content, collapse = "\n")
  writeLines(rmd_content, rmd_file)
  log_info("R Markdown file written: {rmd_file}")

  # Copy CSS to output directory
  css_src <- system.file("rmd", "styles.css", package = "pakhom")
  if (nchar(css_src) > 0 && file.exists(css_src)) {
    file.copy(css_src, file.path(output_dir, "styles.css"), overwrite = TRUE)
  }

  # Generate separate theme detail HTML files
  theme_detail_files <- .generate_theme_detail_htmls(
    theme_stats, theme_order, export_files, output_dir,
    data = data, coding_results = coding_results
  )

  # Ensure pandoc is available (RStudio bundles it, but CLI runs may not find it)
  if (!rmarkdown::pandoc_available()) {
    rstudio_pandoc <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"
    if (dir.exists(rstudio_pandoc)) {
      Sys.setenv(RSTUDIO_PANDOC = rstudio_pandoc)
      log_info("Using RStudio-bundled pandoc: {rstudio_pandoc}")
    }
  }

  # Render (use normalizePath to ensure absolute paths for CLI contexts)
  abs_output_dir <- normalizePath(output_dir, mustWork = TRUE)
  abs_rmd_file <- normalizePath(rmd_file, mustWork = TRUE)
  tryCatch({
    rmarkdown::render(
      abs_rmd_file,
      output_file = basename(output_file),
      output_dir = abs_output_dir,
      knit_root_dir = abs_output_dir,
      quiet = TRUE
    )
    log_info("HTML report generated: {output_file}")
  }, error = function(e) {
    log_error("Could not render HTML report: {e$message}")
    log_info("R Markdown file saved for manual rendering: {rmd_file}")
  })

  toc()

  # Return NULL if the report file was not actually created
  if (!file.exists(output_file)) {
    log_error("Report file not created: {output_file}")
    return(NULL)
  }

  output_file
}

# ==============================================================================
# Internal: Build R Markdown content
# ==============================================================================

.build_rmd_content <- function(overall_stats, theme_stats, theme_order,
                                ai_synthesis, corr_interpretation, insights,
                                export_files, config, irr_result = NULL,
                                comparison_result = NULL,
                                self_contained = TRUE,
                                theme_group_tests = NULL,
                                cooccurrence_tests = NULL,
                                excerpt_verification = NULL,
                                coding_state = NULL,
                                coverage = NULL,
                                run_id = NULL) {

  theme_count <- length(theme_stats)

  # --- YAML header ---
  safe_focus <- gsub("'", "''", .html_esc(overall_stats$research_focus))
  sc_flag <- if (isTRUE(self_contained)) "true" else "false"
  content <- paste0(
    "---\n",
    "title: 'Thematic Analysis Report'\n",
    "subtitle: '", safe_focus, "'\n",
    "date: '", Sys.Date(), "'\n",
    "output:\n",
    "  html_document:\n",
    "    toc: true\n",
    "    toc_depth: 3\n",
    "    toc_float:\n",
    "      collapsed: true\n",
    "      smooth_scroll: true\n",
    "    theme: flatly\n",
    "    highlight: pygments\n",
    "    self_contained: ", sc_flag, "\n",
    "    css: styles.css\n",
    "---\n\n"
  )

  # --- Setup chunk ---
  content <- paste0(content,
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,\n",
    "                      fig.width = 10, fig.height = 5.5, dpi = 150,\n",
    "                      error = TRUE)\n",
    "library(dplyr)\n",
    "library(ggplot2)\n",
    "library(readr)\n",
    "library(knitr)\n",
    "library(scales)\n",
    "has_dt <- requireNamespace('DT', quietly = TRUE)\n\n",
    .ggplot_theme_code(),
    "\n```\n\n"
  )

  # --- Executive Summary ---
  sentiment_class <- if (is.na(overall_stats$sentiment$mean)) "neutral"
    else if (overall_stats$sentiment$mean < .SENTIMENT_NEGATIVE_THRESHOLD) "negative"
    else if (overall_stats$sentiment$mean > .SENTIMENT_POSITIVE_THRESHOLD) "positive"
    else "neutral"

  # T1.7 (AC4): methodology stamp at the top of the report so a reviewer
  # who picks up the rendered HTML sees the mode declaration before the
  # substantive analysis. Built from config (the canonical source) with
  # a fallback "Unknown methodology" for legacy runs without a config.
  meth_mode <- tryCatch(config$methodology$mode, error = function(e) NULL)
  content <- paste0(content,
    stamp_methodology_html(meth_mode, run_id = run_id), '\n'
  )

  content <- paste0(content,
    '<div class="hero-section">\n',
    '\n# Executive Summary\n\n',
    ai_synthesis$executive_summary, '\n',
    '</div>\n\n',
    .build_metrics_dashboard(overall_stats, theme_count, sentiment_class,
                             comparison_result = comparison_result),
    '\n\n'
  )

  # T0.1 part 3: Tier-0 Data Integrity Dashboard. Renders immediately under
  # the executive summary so reviewers see verification results before
  # reading themes. Reads coding_state's per-segment $provenance fields
  # (populated by phase 18 wiring); falls back gracefully when coding_state
  # is missing / pre-T0.1.
  tier0_stats <- compute_quote_provenance_stats(coding_state)
  content <- paste0(content,
    .build_tier0_dashboard(tier0_stats,
                           fabrication_log_relpath = "fabrication_log.csv")
  )

  # T0.3 corpus coverage assertion. Pairs with T0.1: T0.1 says "no
  # fabrications", T0.3 says "no silent truncation". Both are Tier-0
  # transparency cards rendered before the substantive analysis so
  # reviewers see the integrity claims first. coverage is NULL on
  # legacy/test report calls -- the generic dispatches to the
  # "unavailable" default rather than crashing or omitting silently.
  content <- paste0(content,
    render_tier0_coverage_card(coverage)
  )

  # Inline data overview context into executive summary (Issue 4)
  rc <- overall_stats$research_context
  rc_text <- if (!is.null(rc) && nzchar(trimws(rc))) {
    paste0(" using data from ", .html_esc(rc))
  } else {
    ""
  }
  content <- paste0(content,
    "This analysis examines **", .html_esc(overall_stats$research_focus),
    "**", rc_text,
    ". The analysis was conducted on ", format(overall_stats$analysis_date, "%B %d, %Y"), ".\n\n"
  )
  if (!is.null(overall_stats$source_breakdown)) {
    content <- paste0(content,
      "| Source | Count | Percentage |\n",
      "|--------|------:|----------:|\n"
    )
    for (i in seq_len(nrow(overall_stats$source_breakdown))) {
      row <- overall_stats$source_breakdown[i, ]
      content <- paste0(content,
        "| ", row$source_table, " | ", format(row$n, big.mark = ","),
        " | ", row$pct, "% |\n"
      )
    }
    content <- paste0(content, "\n")
  }
  if (!is.null(overall_stats$learning)) {
    content <- paste0(content,
      "The AI analysis was informed by **", overall_stats$learning$n_studies,
      " previous studies** (",
      format(overall_stats$learning$context_characters, big.mark = ","),
      " characters of learning context).\n\n"
    )
  }
  content <- paste0(content,
    '<div class="download-box">\n',
    '**Quick Download:** <a href="theme_entries/all_entries_by_theme.csv" class="download-link" download>',
    'Download all ', overall_stats$total_entries, ' entries as CSV</a>\n',
    '</div>\n\n'
  )

  # --- Learning Transparency ---
  if (!is.null(overall_stats$learning)) {
    content <- paste0(content, .build_learning_transparency(overall_stats$learning))
  }

  # --- Emotional Landscape ---
  content <- paste0(content, .build_emotional_landscape(overall_stats, export_files))

  # --- Thematic Analysis ---
  content <- paste0(content, .build_thematic_section(
    theme_stats, theme_order, theme_count, export_files
  ))

  # --- Correlation Analysis ---
  content <- paste0(content, .build_correlation_section(corr_interpretation, export_files,
                                                         theme_group_tests = theme_group_tests,
                                                         cooccurrence_tests = cooccurrence_tests))

  # --- Cross-Run Comparison ---
  if (!is.null(comparison_result) && inherits(comparison_result, "ComparisonResult")) {
    content <- paste0(content, .build_comparison_section(comparison_result))
  }

  # --- Synthesis & Conclusion (merged, Issue 12) ---
  content <- paste0(content, .build_synthesis_section(insights, ai_synthesis = ai_synthesis))

  # --- Human Verification / IRR ---
  if (!is.null(irr_result) && !is.null(irr_result$irr_stats)) {
    content <- paste0(content, .build_irr_section(irr_result))
  }

  # --- Thematic Saturation ---
  if (!is.null(coding_state) && !is.null(coding_state$saturation)) {
    content <- paste0(content, .build_saturation_section(coding_state))
  }

  # --- Appendix A: Methodology ---
  content <- paste0(content, .build_methodology_appendix(overall_stats, export_files, config,
                                                          excerpt_verification = excerpt_verification))

  # --- Appendix B: Theme Details ---
  content <- paste0(content,
    "# Appendix B: Theme Details {#theme-details-appendix}\n\n",
    "Each theme has its own interactive detail page with full entry data, searchable tables, ",
    "and downloadable CSVs.\n\n"
  )
  theme_idx <- 0
  for (tn in theme_order) {
    if (!tn %in% names(theme_stats)) next
    theme_idx <- theme_idx + 1
    ts <- theme_stats[[tn]]
    safe_fn <- make_safe_filename(tn)
    sent_mean <- if (!is.null(ts$sentiment$mean) && !is.na(ts$sentiment$mean)) {
      ts$sentiment$mean
    } else {
      0
    }
    sent_label <- if (sent_mean < .SENTIMENT_NEGATIVE_THRESHOLD) "negative"
      else if (sent_mean > .SENTIMENT_POSITIVE_THRESHOLD) "positive"
      else "mixed"
    content <- paste0(content,
      theme_idx, ". **[", .html_esc(tn), "](theme_details/theme_", safe_fn, ".html)** -- ",
      ts$n_entries, " entries, ", sent_label, " sentiment (mean: ", round(sent_mean, 2), ")\n"
    )
  }
  content <- paste0(content, "\n")

  # --- Appendix C: Downloads ---
  content <- paste0(content, generate_downloads_section(export_files, theme_stats))

  # --- Footer ---
  content <- paste0(content,
    "---\n\n",
    "<p style='text-align: center; color: var(--text-muted); font-size: 0.85rem; margin-top: 3rem;'>",
    "Report generated on ", format(Sys.time(), "%Y-%m-%d at %H:%M:%S"),
    " using ", tryCatch(
      paste0("pakhom v", as.character(utils::packageVersion("pakhom"))),
      error = function(e) "pakhom"
    ), " by <a href='https://www.linkedin.com/in/abanoubarmanious/' target='_blank' style='color: inherit;'>Abanoub J. Armanious, MS</a></p>\n"
  )

  content
}

# ==============================================================================
# Internal: Section builders
# ==============================================================================

.build_metrics_dashboard <- function(stats, n_themes, sentiment_class,
                                     comparison_result = NULL) {
  # Minimal inline SVG icons (24x24, currentColor)
  icon_entries <- '<span class="metric-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"/><polyline points="14 2 14 8 20 8"/><line x1="16" y1="13" x2="8" y2="13"/><line x1="16" y1="17" x2="8" y2="17"/><polyline points="10 9 9 9 8 9"/></svg></span>'
  icon_themes <- '<span class="metric-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="3" width="7" height="7"/><rect x="14" y="3" width="7" height="7"/><rect x="14" y="14" width="7" height="7"/><rect x="3" y="14" width="7" height="7"/></svg></span>'
  icon_codes <- '<span class="metric-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M20.59 13.41l-7.17 7.17a2 2 0 0 1-2.83 0L2 12V2h10l8.59 8.59a2 2 0 0 1 0 2.82z"/><line x1="7" y1="7" x2="7.01" y2="7"/></svg></span>'
  icon_sentiment <- '<span class="metric-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="20" x2="12" y2="10"/><line x1="18" y1="20" x2="18" y2="4"/><line x1="6" y1="20" x2="6" y2="16"/></svg></span>'
  icon_negative <- '<span class="metric-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="23 18 13.5 8.5 8.5 13.5 1 6"/><polyline points="17 18 23 18 23 12"/></svg></span>'
  icon_positive <- '<span class="metric-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="23 6 13.5 15.5 8.5 10.5 1 18"/><polyline points="17 6 23 6 23 12"/></svg></span>'

  cards <- paste0(
    '<div class="metrics-grid">\n',
    '<div class="metric-card">\n',
    icon_entries, '\n',
    '<div class="metric-value">', format(stats$total_entries, big.mark = ","), '</div>\n',
    '<div class="metric-label">Entries Analyzed</div>\n',
    '</div>\n',
    '<div class="metric-card">\n',
    icon_themes, '\n',
    '<div class="metric-value">', n_themes, '</div>\n',
    '<div class="metric-label">Themes Identified</div>\n',
    '</div>\n',
    '<div class="metric-card">\n',
    icon_codes, '\n',
    '<div class="metric-value">', stats$coding$total_unique_codes, '</div>\n',
    '<div class="metric-label">Unique Codes</div>\n',
    '</div>\n',
    '<div class="metric-card ', sentiment_class, '">\n',
    icon_sentiment, '\n',
    '<div class="metric-value">', stats$sentiment$mean, '</div>\n',
    '<div class="metric-label">Mean Sentiment</div>\n',
    '</div>\n',
    '<div class="metric-card negative">\n',
    icon_negative, '\n',
    '<div class="metric-value">', stats$sentiment$pct_negative, '%</div>\n',
    '<div class="metric-label">Negative</div>\n',
    '</div>\n',
    '<div class="metric-card positive">\n',
    icon_positive, '\n',
    '<div class="metric-value">', stats$sentiment$pct_positive, '%</div>\n',
    '<div class="metric-label">Positive</div>\n',
    '</div>\n'
  )

  # Issue 13: optional stability metric card from cross-run comparison
  if (!is.null(comparison_result)) {
    stability_info <- tryCatch({
      # Prefer theme stability rate (how many entries kept same theme across runs)
      if (!is.null(comparison_result$entry_migration) &&
          !is.na(comparison_result$entry_migration$stability_rate)) {
        list(
          value = round(comparison_result$entry_migration$stability_rate * 100, 1),
          label = "Theme Stability"
        )
      } else if (!is.null(comparison_result$sample_overlap) &&
                 !is.null(comparison_result$sample_overlap$pairwise) &&
                 !is.na(comparison_result$sample_overlap$pairwise$jaccard_index %||% NA)) {
        list(
          value = round(comparison_result$sample_overlap$pairwise$jaccard_index * 100, 1),
          label = "Sample Overlap (Jaccard)"
        )
      } else {
        NULL
      }
    }, error = function(e) NULL)

    if (!is.null(stability_info)) {
      icon_stability <- '<span class="metric-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z"/></svg></span>'
      cards <- paste0(cards,
        '<div class="metric-card">\n',
        icon_stability, '\n',
        '<div class="metric-value">', stability_info$value, '%</div>\n',
        '<div class="metric-label">', stability_info$label, '</div>\n',
        '</div>\n'
      )
    }
  }

  paste0(cards, '</div>\n')
}


.build_irr_section <- function(irr_result) {
  stats <- irr_result$irr_stats

  # Pull alpha fields with fallback for backward compat
  alpha_val <- stats$krippendorff_alpha %||% NA_real_
  alpha_interp <- stats$alpha_interpretation %||% "N/A"

  content <- paste0(
    "# Inter-Rater Reliability\n\n",
    "A human verification step was performed to assess coding agreement between ",
    "the AI system and a human researcher. Code matching used fuzzy string ",
    "comparison (Jaro-Winkler distance) to account for minor wording differences.\n\n",
    "## Agreement Statistics\n\n",
    "| Metric | Value | Interpretation |\n",
    "|--------|------:|----------------|\n",
    "| Krippendorff's Alpha | ", alpha_val, " | ", alpha_interp, " |\n",
    "| Cohen's Kappa | ", stats$cohens_kappa, " | ", stats$kappa_interpretation, " |\n",
    "| Percent Agreement | ", stats$percent_agreement, "% | |\n",
    "| Mean Jaccard Similarity | ", stats$jaccard_similarity, " | |\n",
    "| Entries Compared | ", stats$n_entries, " | |\n\n"
  )

  # Interpretation -- prioritize Krippendorff's alpha as recommended metric
  if (!is.na(alpha_val)) {
    content <- paste0(content,
      "## Interpretation\n\n",
      "**Krippendorff's alpha** of **", alpha_val, "** indicates **",
      tolower(alpha_interp), "** (Krippendorff, 2011). ",
      "Alpha is the recommended reliability metric for multi-label coding as it ",
      "handles prevalence bias and sparse code matrices better than Cohen's kappa. ",
      "Krippendorff recommends alpha >= 0.667 for tentative conclusions and ",
      ">= 0.800 for reliable conclusions.\n\n",
      "Cohen's kappa of **", stats$cohens_kappa, "** (",
      tolower(stats$kappa_interpretation), " agreement; Landis & Koch, 1977) ",
      "is provided for comparison. ",
      "The mean Jaccard similarity of **", stats$jaccard_similarity,
      "** represents the average overlap between code sets assigned by each rater.\n\n"
    )
  } else if (!is.na(stats$cohens_kappa)) {
    content <- paste0(content,
      "## Interpretation\n\n",
      "Cohen's kappa of **", stats$cohens_kappa, "** indicates **",
      tolower(stats$kappa_interpretation), " agreement** (Landis & Koch, 1977). ",
      "The mean Jaccard similarity of **", stats$jaccard_similarity,
      "** represents the average overlap between code sets assigned by each rater.\n\n"
    )
  }

  content
}

.build_learning_transparency <- function(learning) {
  content <- paste0(
    "# Learning from Previous Studies\n\n",
    "Before analyzing the current dataset, the AI system studied **", learning$n_studies,
    " previous manually-coded analyses**: ", paste(learning$study_names, collapse = ", "),
    ". These prior analyses provided calibration data that shaped every major stage ",
    "of the current pipeline.\n\n"
  )

  # How learning was used across pipeline stages
  content <- paste0(content,
    "## How Prior Analyses Guided Each Pipeline Stage\n\n",
    '<div class="callout callout-neutral">\n',
    "The learning context was injected into three pipeline stages to ensure the AI's ",
    "analytical behavior matches the depth, granularity, and specificity demonstrated ",
    "by the human researchers in the previous studies.\n",
    "</div>\n\n",
    "| Pipeline Stage | How Prior Studies Were Used | Context Size |\n",
    "|----------------|---------------------------|-------------:|\n",
    "| **Initial Coding** | Prior theme examples and raw data excerpts set the expected ",
    "level of code specificity and analytical depth | ", format(learning$coding_chars, big.mark = ","), " chars |\n",
    "| **Theme Generation** | Prior thematic structures guided the AI toward producing ",
    "themes at comparable granularity to human-identified themes | ", format(learning$theming_chars, big.mark = ","), " chars |\n",
    "| **Theme Review** | Human-generated themes served as a specificity benchmark -- ",
    "AI themes too vague compared to the prior studies were flagged for revision | ",
    format(learning$review_chars, big.mark = ","), " chars |\n",
    "| **Total** | | **", format(learning$context_characters, big.mark = ","), " chars** |\n\n"
  )

  # Show AI reflection on what was learned
  if (nchar(learning$reflection) > 0) {
    content <- paste0(content,
      "## What the AI Learned and How It Applied Those Findings\n\n",
      learning$reflection, "\n\n"
    )
  }

  # Show specific learning content excerpts
  has_excerpts <- !is.null(learning$coding_excerpt) && nchar(learning$coding_excerpt) > 0
  if (has_excerpts) {
    content <- paste0(content,
      "## Specific Learning Context Provided to the AI\n\n",
      "The following excerpts show what was actually sent to the AI from the prior ",
      "studies. Each pipeline stage received a tailored slice of the learning context ",
      "optimized for its specific analytical task. This transparency is critical for ",
      "assessing whether the AI had adequate calibration data.\n\n"
    )

    content <- paste0(content,
      "*Calibrates code specificity and analytical depth.*\n\n",
      "<details>\n",
      "<summary><strong>Coding Context Excerpt</strong> (click to expand)</summary>\n\n",
      "```\n", learning$coding_excerpt, "\n```\n\n",
      "</details>\n\n"
    )
  }
  if (!is.null(learning$theming_excerpt) && nchar(learning$theming_excerpt) > 0) {
    content <- paste0(content,
      "*Guides theme granularity comparable to human analyses.*\n\n",
      "<details>\n",
      "<summary><strong>Theming Context Excerpt</strong> (click to expand)</summary>\n\n",
      "```\n", learning$theming_excerpt, "\n```\n\n",
      "</details>\n\n"
    )
  }
  if (!is.null(learning$review_excerpt) && nchar(learning$review_excerpt) > 0) {
    content <- paste0(content,
      "*Specificity benchmark for theme quality validation.*\n\n",
      "<details>\n",
      "<summary><strong>Review Calibration Excerpt</strong> (click to expand)</summary>\n\n",
      "```\n", learning$review_excerpt, "\n```\n\n",
      "</details>\n\n"
    )
  }

  content
}

.build_emotional_landscape <- function(stats, export_files) {
  content <- paste0(
    "# Emotional Landscape\n\n",
    "## Sentiment Distribution\n\n",
    "```{r sentiment-histogram}\n",
    "data <- read_csv('", basename(export_files$sentiment_file), "', show_col_types = FALSE)\n\n",
    "ggplot(data, aes(x = sentiment_score)) +\n",
    "  geom_histogram(bins = 35, fill = '#3498DB', color = 'white', alpha = 0.85) +\n",
    "  geom_vline(xintercept = mean(data$sentiment_score, na.rm = TRUE),\n",
    "             color = '#E74C3C', linetype = 'dashed', linewidth = 1) +\n",
    "  annotate('label', x = mean(data$sentiment_score, na.rm = TRUE), y = Inf,\n",
    "           label = paste0('Mean: ', round(mean(data$sentiment_score, na.rm = TRUE), 2)),\n",
    "           vjust = 1.5, fill = '#E74C3C', color = 'white', fontface = 'bold', size = 3.5) +\n",
    "  labs(title = 'Distribution of Sentiment Scores',\n",
    "       subtitle = 'Vertical line indicates mean sentiment across all entries',\n",
    "       x = 'Sentiment Score (-1 = Very Negative, +1 = Very Positive)',\n",
    "       y = 'Number of Entries') +\n",
    "  theme_report() +\n",
    "  scale_x_continuous(breaks = seq(-1, 1, 0.25))\n",
    "```\n\n"
  )

  # Methodological note on bimodal distributions (Issue 5)
  content <- paste0(content,
    '<div class="callout callout-neutral">\n',
    '<strong>Methodological Note:</strong> Bimodal sentiment distributions are common in ',
    'health and support communities, where entries naturally cluster around distress narratives ',
    'and recovery/positive-experience narratives. Additionally, the coupled emotion-sentiment ',
    'prompt architecture (which elicits both emotion and sentiment simultaneously) may ',
    'amplify polarity. Interpret distribution shape with both factors in mind.\n',
    '</div>\n\n'
  )

  # Emotional tone interpretation
  affect <- if (is.na(stats$sentiment$mean)) "**mixed emotional states**"
    else if (stats$sentiment$mean < .SENTIMENT_NEGATIVE_THRESHOLD) "**negative affect**"
    else if (stats$sentiment$mean > .SENTIMENT_POSITIVE_THRESHOLD) "**positive affect**"
    else "**mixed emotional states**"

  content <- paste0(content,
    "## Interpreting the Emotional Tone\n\n",
    "The sentiment distribution reveals a community predominantly experiencing ",
    affect, " (mean = ", stats$sentiment$mean, ", SD = ", stats$sentiment$sd,
    "). Specifically, **", stats$sentiment$pct_negative,
    "%** of entries showed negative sentiment, while **",
    stats$sentiment$pct_positive, "%** were positive.\n\n"
  )

  # Emotion bar chart
  content <- paste0(content,
    "## Emotion Distribution (Multi-Label)\n\n",
    "Entries may express multiple emotions simultaneously. The chart below counts ",
    "each emotion independently -- an entry expressing both sadness and anger ",
    "contributes to both counts.\n\n",
    "```{r emotion-bar}\n",
    "# Multi-label emotion counting: split all_emotions on semicolons\n",
    "emo_col <- 'all_emotions'\n",
    "raw_emo <- data[[emo_col]][!is.na(data[[emo_col]])]\n",
    "all_labels <- trimws(unlist(strsplit(raw_emo, ';\\\\s*')))\n",
    "all_labels <- all_labels[nchar(all_labels) > 0]\n",
    "emo_tbl <- sort(table(all_labels), decreasing = TRUE)\n",
    "n_entries_with_emo <- length(raw_emo)\n",
    "emotion_data <- tibble::tibble(\n",
    "  emotion = names(emo_tbl),\n",
    "  n = as.integer(emo_tbl),\n",
    "  pct = round(100 * as.integer(emo_tbl) / max(n_entries_with_emo, 1), 1)\n",
    ")\n\n",
    "n_emotions <- nrow(emotion_data)\n",
    "emotion_colors <- colorRampPalette(report_colors)(n_emotions)\n",
    "names(emotion_colors) <- emotion_data$emotion\n\n",
    "ggplot(emotion_data, aes(x = reorder(emotion, n), y = n, fill = emotion)) +\n",
    "  geom_col(alpha = 0.9, width = 0.7) +\n",
    "  geom_text(aes(label = paste0(pct, '%')), hjust = -0.15, size = 3.2, fontface = 'bold') +\n",
    "  coord_flip() +\n",
    "  labs(title = 'Emotion Distribution (Multi-Label)',\n",
    "       subtitle = 'Entries may express multiple emotions; percentages are of entries with any emotion',\n",
    "       x = '', y = 'Number of Occurrences') +\n",
    "  theme_report() +\n",
    "  theme(legend.position = 'none') +\n",
    "  scale_fill_manual(values = emotion_colors) +\n",
    "  expand_limits(y = max(emotion_data$n) * 1.12)\n",
    "```\n\n"
  )

  # Emotion table
  if (nrow(stats$emotions) > 0) {
    content <- paste0(content,
      "### Emotion Breakdown\n\n",
      "| Emotion | Count | Percentage | Interpretation |\n",
      "|---------|------:|----------:|-----------------|\n"
    )
    for (i in seq_len(min(8, nrow(stats$emotions)))) {
      row <- stats$emotions[i, ]
      interp <- get_emotion_interpretation(row$emotion)
      content <- paste0(content,
        "| ", row$emotion, " | ", format(row$n, big.mark = ","),
        " | ", row$pct, "% | ", stringr::str_to_sentence(interp), " |\n"
      )
    }
    content <- paste0(content, "\n")
  }

  content
}

.build_thematic_section <- function(theme_stats, theme_order, n_themes, export_files) {
  content <- paste0(
    "# Thematic Analysis\n\n",
    "The analysis identified **", n_themes, " distinct themes** through an iterative process of ",
    "coding, consolidation, and refinement.\n\n",
    "*Click \"View Full Details\" on any theme to see complete entries and statistics.*\n\n",
    "```{r theme-distribution}\n",
    "# Count entries per theme using multi-label membership columns\n",
    "membership_cols <- grep('^theme_membership_', names(data), value = TRUE)\n",
    "if (length(membership_cols) > 0) {\n",
    "  theme_counts <- vapply(membership_cols, function(col) sum(data[[col]] == 1L, na.rm = TRUE), integer(1))\n",
    "  theme_labels <- sub('^theme_membership_', '', names(theme_counts))\n",
    "  theme_labels <- gsub('\\\\.', ' ', theme_labels)\n",
    "  theme_data <- tibble::tibble(theme_name = theme_labels, n = as.integer(theme_counts))\n",
    "} else {\n",
    "  all_themes <- unlist(strsplit(data$emerged_themes[!is.na(data$emerged_themes)], ';\\\\s*'))\n",
    "  theme_tbl <- sort(table(trimws(all_themes)), decreasing = TRUE)\n",
    "  theme_data <- tibble::tibble(theme_name = names(theme_tbl), n = as.integer(theme_tbl))\n",
    "}\n",
    "theme_data <- theme_data |> dplyr::filter(n > 0) |> dplyr::arrange(dplyr::desc(n))\n",
    "theme_data$pct <- round(100 * theme_data$n / nrow(data), 1)\n\n",
    "theme_colors <- colorRampPalette(report_colors)(nrow(theme_data))\n",
    "names(theme_colors) <- theme_data$theme_name\n\n",
    "ggplot(theme_data, aes(x = reorder(theme_name, n), y = n, fill = theme_name)) +\n",
    "  geom_col(alpha = 0.9, width = 0.75) +\n",
    "  geom_text(aes(label = paste0(n, ' (', pct, '%)')), hjust = -0.05, size = 3.2, fontface = 'bold') +\n",
    "  coord_flip() +\n",
    "  labs(title = 'Theme Distribution (Multi-Label)',\n",
    "       subtitle = 'Entries may appear under multiple themes',\n",
    "       x = '', y = 'Number of Entries') +\n",
    "  theme_report() +\n",
    "  theme(legend.position = 'none') +\n",
    "  scale_fill_manual(values = theme_colors) +\n",
    "  expand_limits(y = max(theme_data$n) * 1.15)\n",
    "```\n\n",
    "## Sentiment Comparison Across Themes\n\n",
    "```{r sentiment-boxplot-thematic}\n",
    "# Build long-format data for multi-label sentiment boxplot\n",
    "membership_cols <- grep('^theme_membership_', names(data), value = TRUE)\n",
    "if (length(membership_cols) > 0) {\n",
    "  long_data <- do.call(rbind, lapply(membership_cols, function(col) {\n",
    "    entries <- data[data[[col]] == 1L & !is.na(data$sentiment_score), ]\n",
    "    if (nrow(entries) == 0) return(NULL)\n",
    "    tn <- gsub('\\\\.', ' ', sub('^theme_membership_', '', col))\n",
    "    tibble::tibble(theme_name = tn, sentiment_score = entries$sentiment_score)\n",
    "  }))\n",
    "} else {\n",
    "  long_data <- tibble::tibble(theme_name = character(), sentiment_score = numeric())\n",
    "}\n",
    "if (!is.null(long_data) && nrow(long_data) > 0) {\n",
    "n_themes_plot <- length(unique(long_data$theme_name))\n",
    "ggplot(long_data,\n",
    "       aes(x = reorder(theme_name, sentiment_score, FUN = median),\n",
    "           y = sentiment_score, fill = theme_name)) +\n",
    "  geom_boxplot(alpha = 0.8, outlier.alpha = 0.4, outlier.size = 1.5) +\n",
    "  geom_hline(yintercept = 0, linetype = 'dashed', color = '#7F8C8D', linewidth = 0.5) +\n",
    "  coord_flip() +\n",
    "  labs(title = 'Sentiment Distribution by Theme',\n",
    "       subtitle = 'Boxplots showing median, IQR, and outliers (multi-label)',\n",
    "       x = '', y = 'Sentiment Score') +\n",
    "  theme_report() +\n",
    "  theme(legend.position = 'none') +\n",
    "  scale_fill_manual(values = colorRampPalette(report_colors)(n_themes_plot))\n",
    "}\n",
    "```\n\n"
  )

  # Theme cards
  theme_index <- 0
  for (tn in theme_order) {
    if (!tn %in% names(theme_stats)) next
    theme_index <- theme_index + 1
    ts <- theme_stats[[tn]]
    csv_info <- export_files$theme_csv_files[[tn]]

    sent_class <- if (is.na(ts$sentiment$mean)) "neutral"
      else if (ts$sentiment$mean < .SENTIMENT_NEGATIVE_THRESHOLD) "negative"
      else if (ts$sentiment$mean > .SENTIMENT_POSITIVE_THRESHOLD) "positive"
      else "neutral"

    content <- paste0(content,
      '<div class="theme-card theme-', theme_index, '" id="theme-summary-', theme_index, '">\n\n',
      '## <span class="theme-badge">', theme_index, '</span> ', .html_esc(tn), '\n\n',
      '<p class="theme-description">', .html_esc(ts$description %||% ""), '</p>\n\n',
      '<div class="theme-meta">\n',
      '<div class="theme-meta-item">\n',
      '<span class="theme-meta-value">', ts$n_entries, ' (', ts$pct_of_total, '%)</span>\n',
      '<span class="theme-meta-label">Entries</span>\n',
      '</div>\n',
      '<div class="theme-meta-item">\n',
      '<span class="theme-meta-value text-', sent_class, '">', ts$sentiment$mean, '</span>\n',
      '<span class="theme-meta-label">Mean Sentiment</span>\n',
      '</div>\n',
      '<div class="theme-meta-item">\n',
      '<span class="theme-meta-value">', ts$sentiment$pct_negative, '% / ', ts$sentiment$pct_positive, '%</span>\n',
      '<span class="theme-meta-label">Neg / Pos</span>\n',
      '</div>\n',
      '<div class="theme-meta-item">\n',
      '<span class="theme-meta-value">', ts$intensity$mean, '</span>\n',
      '<span class="theme-meta-label">Intensity</span>\n',
      '</div>\n',
      '</div>\n\n'
    )

    # Keywords
    if (!is.null(ts$keywords) && length(ts$keywords) > 0 && !all(is.na(ts$keywords))) {
      pills <- vapply(ts$keywords[seq_len(min(5, length(ts$keywords)))], function(k) {
        paste0('<span class="keyword-pill">', .html_esc(k), '</span>')
      }, character(1))
      content <- paste0(content,
        '<div class="keywords-container">\n',
        paste(pills, collapse = "\n"), '\n',
        '</div>\n\n'
      )
    }

    # T0.2 participant distribution: count, Gini, top contributor share, with
    # a concentration warning when one author dominates. Renders an
    # "unavailable" variant when std_author isn't present (preserves the
    # absence-as-signal pattern -- silent omission would itself be a
    # methodology problem per Jowsey 2025).
    content <- paste0(content,
      .build_participant_spread_card(ts$participant_spread))

    # Representative quotes
    content <- paste0(content, "### Representative Voices\n\n")
    if (!is.null(ts$quotes_with_context) && length(ts$quotes_with_context) > 0) {
      for (quote_type in names(ts$quotes_with_context)) {
        q <- ts$quotes_with_context[[quote_type]]
        if (is.null(q$text) || is.na(q$text)) next

        qclass <- if (q$sentiment < .SENTIMENT_NEGATIVE_THRESHOLD) "negative"
          else if (q$sentiment > .SENTIMENT_POSITIVE_THRESHOLD) "positive"
          else "neutral"

        slabel <- if (q$sentiment < -0.3) "High Distress"
          else if (q$sentiment < 0) "Moderate Distress"
          else if (q$sentiment < 0.3) "Neutral/Mixed"
          else "Positive"

        content <- paste0(content,
          '<div class="quote-box ', qclass, '">\n',
          .html_esc(gsub("\n", " ", q$text)), '\n',
          '<div class="quote-meta">\n',
          '<span class="sentiment-pill ', qclass, '">', slabel, '</span>\n',
          'Sentiment: ', round(q$sentiment, 2), ' &bull; ', q$emotion %||% "N/A", '\n',
          '</div>\n',
          '</div>\n\n'
        )
      }
    }

    # Detail link
    safe_fn <- make_safe_filename(tn)
    content <- paste0(content,
      '<a href="theme_details/theme_', safe_fn, '.html" class="drill-down-link" target="_blank">',
      'View Full Details: ', ts$n_entries, ' Entries</a>\n'
    )

    # CSV link
    if (!is.null(csv_info)) {
      content <- paste0(content,
        '<span class="csv-link-small"><a href="', csv_info$relative_path,
        '" download>Download CSV</a></span>\n'
      )
    }

    content <- paste0(content, '\n</div>\n\n')
  }

  content
}

# ==============================================================================
# Participant distribution card (Sprint-4 T0.2)
# ==============================================================================

#' Render the per-theme Participant Distribution card
#'
#' Empirical answer to Jowsey et al. 2025's Frankenstein finding that "none
#' of the Copilot outputs reported the participant spread". Three metrics
#' are surfaced as a meta card:
#' \itemize{
#'   \item \code{n_distinct_contributors} -- count of unique authors
#'   \item \code{contributor_gini} -- Gini coefficient (0 = even, 1 = one
#'     contributor takes everything)
#'   \item \code{top_contributor_share} -- fraction from the most prolific
#'     contributor (the "is this one person's theme?" check)
#' }
#'
#' Concentration warnings:
#' \itemize{
#'   \item When \code{n_distinct_contributors == 1}, renders a "single
#'     contributor" notice -- the theme has zero participant spread.
#'   \item When \code{top_contributor_share > 0.5} (one contributor owns
#'     more than half), renders a caution banner.
#' }
#'
#' Unavailable variant: when \code{participant_spread$available} is FALSE
#' (no \code{std_author} column in the data, or no non-NA author values
#' for this theme), renders a "Participant data not available" notice.
#' Silent omission is rejected because the absence itself carries
#' methodological signal (a Tier-0 universal that explicitly cannot be
#' computed must say so).
#'
#' @param ps participant_spread sub-list from
#'   \code{aggregate_theme_statistics()} (or NULL/missing on legacy stats).
#' @return Character HTML/markdown string for the card.
#' @keywords internal
.build_participant_spread_card <- function(ps) {
  if (is.null(ps)) {
    # Legacy stats objects predate T0.2 -- treat as unavailable rather
    # than crashing.
    return(paste0(
      '<div class="participant-spread-card participant-spread-unavailable">\n',
      '<div class="ps-header">Participant Distribution</div>\n',
      '<p class="ps-unavailable-note">Author data not available for ',
      'this analysis run.</p>\n',
      '</div>\n\n'
    ))
  }

  if (!isTRUE(ps$available)) {
    return(paste0(
      '<div class="participant-spread-card participant-spread-unavailable">\n',
      '<div class="ps-header">Participant Distribution</div>\n',
      '<p class="ps-unavailable-note">Author data not available for ',
      'this dataset; participant-spread metrics cannot be computed. ',
      'Per Tier-0 transparency policy this absence is reported rather ',
      'than silently omitted.</p>\n',
      '</div>\n\n'
    ))
  }

  n_contrib <- ps$n_distinct_contributors %||% 0L
  gini      <- ps$contributor_gini      %||% NA_real_
  top_share <- ps$top_contributor_share %||% NA_real_

  gini_str  <- if (is.na(gini))      "n/a" else sprintf("%.2f", gini)
  share_str <- if (is.na(top_share)) "n/a" else sprintf("%.0f%%",
                                                         100 * top_share)

  # Concentration warning -- threshold tuned to flag themes that look
  # prevalent but actually lean on one heavy poster. The single-contributor
  # case is its own message because n=1 means top_share=1.0 by definition
  # and the count itself is the warning.
  warn_msg <- NULL
  share_warn_class <- ""
  if (n_contrib == 1L) {
    warn_msg <- paste0(
      "Single contributor only. This theme has no participant spread; ",
      "treat findings as a single voice, not a community pattern."
    )
    share_warn_class <- "ps-warn"
  } else if (!is.na(top_share) && top_share > 0.5) {
    warn_msg <- paste0(
      sprintf("%s of this theme's entries come from one contributor", share_str),
      " (top contributor share > 50%). Consider whether the theme reflects ",
      "a community pattern or a single user's framing."
    )
    share_warn_class <- "ps-warn"
  }

  paste0(
    '<div class="participant-spread-card">\n',
    '<div class="ps-header">Participant Distribution</div>\n',
    '<div class="ps-stats">\n',
    '<div class="ps-stat">',
    '<span class="ps-value">', n_contrib, '</span>',
    '<span class="ps-label">Distinct contributors</span>',
    '</div>\n',
    '<div class="ps-stat">',
    '<span class="ps-value">', gini_str, '</span>',
    '<span class="ps-label">Gini coefficient</span>',
    '</div>\n',
    '<div class="ps-stat">',
    '<span class="ps-value ', share_warn_class, '">', share_str, '</span>',
    '<span class="ps-label">Top contributor share</span>',
    '</div>\n',
    '</div>\n',
    if (!is.null(warn_msg)) paste0(
      '<div class="ps-warning">', .html_esc(warn_msg), '</div>\n'
    ) else "",
    '</div>\n\n'
  )
}


# ==============================================================================
# Corpus coverage card (Sprint-4 T0.3)
# ==============================================================================

#' Render the Tier-0 corpus-coverage assertion card
#'
#' Empirical answer to Jowsey et al. 2025's Frankenstein finding that
#' Microsoft Copilot "drew themes from only the first 2-3 pages of data."
#' pakhom processes entries strictly one at a time; this card surfaces the
#' funnel from preprocessed data to LLM-processed entries to coded entries
#' and asserts the headline \code{no_silent_truncation} claim explicitly.
#'
#' Pairs with the T0.1 verification dashboard: T0.1 says "no fabrications",
#' T0.3 says "no silent truncation". Both are Tier-0 transparency cards
#' rendered above the substantive analysis so reviewers see the integrity
#' claims first.
#'
#' Unavailable variant: when \code{coverage} is NULL (legacy report call,
#' or coverage computation failed) the card renders an explicit
#' "coverage data unavailable" notice rather than omitting silently. Per
#' AC4 (methodology stamped on every output), absence of the card is
#' itself a failure signal so we say so.
#'
#' @param coverage A \code{CorpusCoverage} object from
#'   \code{\link{compute_corpus_coverage}}, or NULL.
#' @return Character HTML/markdown string for the card.
#' @keywords internal
.build_corpus_coverage_card <- function(coverage) {
  # Phase 31: this name is preserved as a thin compat wrapper around
  # the new render_tier0_coverage_card generic. Existing tests in
  # test-corpus_coverage.R + test-tier0-smoke.R call this under
  # pakhom:::.build_corpus_coverage_card; routing through the generic
  # keeps their assertions valid while letting Mode 1 dispatch via
  # render_tier0_coverage_card.ProvocationCoverage.
  render_tier0_coverage_card(coverage)
}

#' @rdname render_tier0_coverage_card
#' @export
render_tier0_coverage_card.CorpusCoverage <- function(x, ...) {
  coverage <- x
  ok <- isTRUE(coverage$no_silent_truncation)
  banner_class <- if (ok) "coverage-banner-ok" else "coverage-banner-warn"
  banner_msg <- if (ok) {
    paste0(
      "All ",
      format(coverage$n_input_to_coding, big.mark = ","),
      " entries from the preprocessed dataset were sent to the LLM. ",
      "No silent truncation in the coding-call path."
    )
  } else if (coverage$n_input_to_coding == 0L) {
    "Empty dataset: coding step received zero entries."
  } else {
    paste0(
      format(coverage$n_unprocessed, big.mark = ","),
      " of ",
      format(coverage$n_input_to_coding, big.mark = ","),
      " entries did NOT reach the LLM. Coverage is incomplete; ",
      "investigate before publishing."
    )
  }

  # Funnel rows -- only show pre-coding rows when we have data for them
  funnel_rows <- character(0)
  if (!is.na(coverage$n_raw_loaded)) {
    funnel_rows <- c(funnel_rows, sprintf(
      '<tr><td>Raw rows loaded</td><td>%s</td><td></td></tr>',
      format(coverage$n_raw_loaded, big.mark = ",")
    ))
  }
  if (!is.na(coverage$n_after_preprocessing)) {
    drop_pp <- if (!is.na(coverage$n_raw_loaded))
      format(coverage$n_raw_loaded - coverage$n_after_preprocessing,
             big.mark = ",")
    else ""
    drop_label <- if (nzchar(drop_pp))
      sprintf("%s removed (preprocessing: dedup + length filter)", drop_pp)
    else "Preprocessed entries"
    funnel_rows <- c(funnel_rows, sprintf(
      '<tr><td>After preprocessing</td><td>%s</td><td>%s</td></tr>',
      format(coverage$n_after_preprocessing, big.mark = ","),
      drop_label
    ))
  }
  if (!is.na(coverage$test_mode_sample_size)) {
    funnel_rows <- c(funnel_rows, sprintf(
      '<tr><td>Test-mode sub-sample</td><td>%s</td><td>%s</td></tr>',
      format(coverage$test_mode_sample_size, big.mark = ","),
      "Random sub-sample (test mode enabled)"
    ))
  }
  funnel_rows <- c(funnel_rows, sprintf(
    '<tr class="coverage-row-input"><td>Input to coding step</td><td>%s</td><td>%s</td></tr>',
    format(coverage$n_input_to_coding, big.mark = ","),
    "Entries fed to progressive sequential coding"
  ))
  funnel_rows <- c(funnel_rows, sprintf(
    '<tr class="coverage-row-llm"><td>LLM-processed</td><td>%s</td><td>%s</td></tr>',
    format(coverage$n_processed, big.mark = ","),
    if (ok) "All input entries reached the LLM"
    else sprintf("Gap: %s entries did not reach the LLM",
                 format(coverage$n_unprocessed, big.mark = ","))
  ))
  funnel_rows <- c(funnel_rows, sprintf(
    '<tr><td>&nbsp;&nbsp;-- of those, coded</td><td>%s</td><td>%s</td></tr>',
    format(coverage$n_coded, big.mark = ","),
    "Received at least one code"
  ))
  funnel_rows <- c(funnel_rows, sprintf(
    '<tr><td>&nbsp;&nbsp;-- of those, skipped</td><td>%s</td><td>%s</td></tr>',
    format(coverage$n_skipped, big.mark = ","),
    "AI judged: no applicable content"
  ))

  # Skip-reason breakdown -- only render when we have skips
  skip_block <- ""
  if (length(coverage$skip_reasons) > 0L) {
    rows <- vapply(seq_along(coverage$skip_reasons), function(i) {
      reason <- names(coverage$skip_reasons)[i]
      n      <- coverage$skip_reasons[[i]]
      sprintf("<li><strong>%s</strong>: %s</li>",
              .html_esc(reason), format(n, big.mark = ","))
    }, character(1))
    skip_block <- paste0(
      '<div class="coverage-skip-reasons">\n',
      '<div class="coverage-subheader">Skip reasons</div>\n',
      '<ul>', paste(rows, collapse = ""), '</ul>\n',
      '</div>\n'
    )
  }

  paste0(
    '<div class="coverage-card">\n',
    '<div class="coverage-header">Corpus Coverage (T0.3)</div>\n',
    '<div class="coverage-banner ', banner_class, '">', banner_msg, '</div>\n',
    '<div class="coverage-funnel-wrapper">\n',
    '<table class="coverage-funnel">\n',
    '<thead><tr><th>Stage</th><th>Entries</th><th>Note</th></tr></thead>\n',
    '<tbody>\n', paste(funnel_rows, collapse = "\n"), '\n</tbody>\n',
    '</table>\n',
    '</div>\n',
    sprintf(
      '<div class="coverage-volume">%s words (%s characters / %s bytes) of source text processed by the LLM.</div>\n',
      format(coverage$words_processed, big.mark = ","),
      format(coverage$chars_processed, big.mark = ","),
      format(coverage$bytes_processed, big.mark = ",")
    ),
    skip_block,
    '<p class="coverage-citation">Addresses Jowsey et al. 2025 ',
    '(doi:10.1371/journal.pone.0330217), which found that Microsoft ',
    'Copilot drew themes from only the first 2-3 pages of data. ',
    'pakhom processes entries strictly one at a time; this funnel is ',
    'the empirical proof of full-corpus coverage in the LLM call path.</p>\n',
    '</div>\n\n'
  )
}


.build_correlation_section <- function(corr_interpretation, export_files,
                                       theme_group_tests = NULL,
                                       cooccurrence_tests = NULL) {
  content <- paste0(
    "# Correlation Analysis\n\n",
    "## Overview\n\n"
  )

  if (!is.null(corr_interpretation)) {
    content <- paste0(content, corr_interpretation$summary, "\n\n")
  }

  content <- paste0(content,
    "## Correlation Matrix\n\n",
    "![Correlation Matrix](", basename(export_files$plot_file %||% "correlation_plot.png"), ")\n\n",
    "*Cells display correlation coefficients; intensity reflects |r|. ",
    "Effect sizes and 95% confidence intervals are the primary inferential ",
    "tools (see Overview above for the exploratory-framing rationale).*\n\n",
    "## Exploratory Associations\n\n",
    "_Sorted by absolute effect size (|r|). The table reports correlations ",
    "with their 95% confidence intervals and three p-value adjustments (raw, ",
    "Benjamini-Hochberg FDR, Bonferroni FWER) for transparency. Treat these as ",
    "hypothesis-generating; themes were inductively derived from this corpus._\n\n",
    "```{r correlation-table}\n",
    "correlations <- read_csv('", basename(export_files$correlations_file), "', show_col_types = FALSE)\n\n",
    "# Filter by meaningful effect (|r| >= 0.10) when available, else legacy 'significant'\n",
    "flag_col <- if ('meaningful_effect' %in% names(correlations)) 'meaningful_effect' else 'significant'\n",
    "sig_corrs <- correlations |>\n",
    "  filter(.data[[flag_col]]) |>\n",
    "  arrange(desc(abs(correlation))) |>\n",
    "  mutate(\n",
    "    var1 = gsub('theme_membership_', '', var1),\n",
    "    var2 = gsub('theme_membership_', '', var2),\n",
    "    var1 = gsub('_', ' ', var1),\n",
    "    var2 = gsub('_', ' ', var2),\n",
    "    var1 = gsub('\\\\.', ' ', var1),\n",
    "    var2 = gsub('\\\\.', ' ', var2),\n",
    "    var1 = tools::toTitleCase(var1),\n",
    "    var2 = tools::toTitleCase(var2),\n",
    "    Direction = ifelse(correlation > 0, 'Positive', 'Negative'),\n",
    "    `Effect Size` = effect_size\n",
    "  )\n",
    "select_cols <- c('Variable 1' = 'var1', 'Variable 2' = 'var2',\n",
    "       'Correlation' = 'correlation', 'Direction' = 'Direction',\n",
    "       'Effect Size' = 'Effect Size')\n",
    "if ('ci_lower' %in% names(sig_corrs) && 'ci_upper' %in% names(sig_corrs)) {\n",
    "  sig_corrs$`95% CI` <- sprintf('[%.3f, %.3f]', sig_corrs$ci_lower, sig_corrs$ci_upper)\n",
    "  select_cols <- c(select_cols, '95% CI' = '95% CI')\n",
    "}\n",
    "# Tiered p-values when post-OS.2 result; single p_value for legacy data.\n",
    "if (all(c('p_raw', 'p_bh', 'p_bonferroni') %in% names(sig_corrs))) {\n",
    "  select_cols <- c(select_cols,\n",
    "    'p (raw)' = 'p_raw', 'p (BH FDR)' = 'p_bh', 'p (Bonf)' = 'p_bonferroni')\n",
    "} else {\n",
    "  select_cols <- c(select_cols, 'P-value' = 'p_value')\n",
    "}\n",
    "if ('method' %in% names(sig_corrs)) {\n",
    "  sig_corrs$Method <- tools::toTitleCase(sig_corrs$method)\n",
    "  select_cols <- c(select_cols, 'Method' = 'Method')\n",
    "}\n",
    "sig_corrs <- sig_corrs |> select(!!!select_cols)\n\n",
    "if (has_dt) {\n",
    "  numeric_cols <- intersect(c('Correlation', 'p (raw)', 'p (BH FDR)',\n",
    "                              'p (Bonf)', 'P-value'), names(sig_corrs))\n",
    "  DT::datatable(sig_corrs,\n",
    "                options = list(pageLength = 10, dom = 'ftp', scrollX = TRUE),\n",
    "                rownames = FALSE,\n",
    "                class = 'compact stripe') |>\n",
    "    DT::formatRound(columns = numeric_cols, digits = 3)\n",
    "} else {\n",
    "  knitr::kable(sig_corrs, digits = 3)\n",
    "}\n",
    "```\n\n"
  )

  # --- Theme Group Comparisons (Mann-Whitney U) ---
  if (!is.null(theme_group_tests) && is.data.frame(theme_group_tests) && nrow(theme_group_tests) > 0) {
    has_tiered_p <- all(c("p_raw", "p_bh", "p_bonferroni") %in% names(theme_group_tests))
    content <- paste0(content,
      "## Theme Group Comparisons\n\n",
      "Mann-Whitney U tests comparing continuous variables (sentiment, emotion ",
      "intensity) between entries assigned to each theme versus those not ",
      "assigned. Effect sizes (Cohen's r conventions: 0.10 small, 0.30 medium, ",
      "0.50 large) are the primary inferential signals; p-values under three ",
      "regimes (raw, Benjamini-Hochberg FDR, Bonferroni FWER) are reported for ",
      "transparency. Sorted by effect size.\n\n",
      "```{r theme-group-tests}\n",
      "tgt <- tibble::tibble(\n",
      "  Theme = ", deparse1(theme_group_tests$theme), ",\n",
      "  Variable = ", deparse1(theme_group_tests$variable), ",\n",
      "  `Mean (Members)` = ", deparse1(round(theme_group_tests$mean_members, 3)), ",\n",
      "  `Mean (Non-members)` = ", deparse1(round(theme_group_tests$mean_non_members, 3)), ",\n",
      "  `W Statistic` = ", deparse1(theme_group_tests$w_statistic), ",\n",
      "  `Effect Size (r)` = ", deparse1(round(theme_group_tests$effect_r, 3)),
      if (has_tiered_p) paste0(",\n",
        "  `p (raw)` = ", deparse1(signif(theme_group_tests$p_raw, 4)), ",\n",
        "  `p (BH FDR)` = ", deparse1(signif(theme_group_tests$p_bh, 4)), ",\n",
        "  `p (Bonf)` = ", deparse1(signif(theme_group_tests$p_bonferroni, 4))
      ) else paste0(",\n",
        "  `P-value` = ", deparse1(signif(theme_group_tests$p_adjusted, 4))
      ), "\n",
      ")\n",
      "if (has_dt) {\n",
      "  DT::datatable(tgt,\n",
      "                options = list(pageLength = 10, dom = 'ftp', scrollX = TRUE),\n",
      "                rownames = FALSE,\n",
      "                class = 'compact stripe',\n",
      "                caption = 'Mann-Whitney U: Theme Members vs Non-Members')\n",
      "} else {\n",
      "  knitr::kable(tgt, digits = 3, caption = 'Mann-Whitney U: Theme Members vs Non-Members')\n",
      "}\n",
      "```\n\n"
    )
  }

  # --- Theme Co-occurrence (Chi-square / Fisher's exact) ---
  if (!is.null(cooccurrence_tests) && is.data.frame(cooccurrence_tests) && nrow(cooccurrence_tests) > 0) {
    has_tiered_p <- all(c("p_raw", "p_bh", "p_bonferroni") %in% names(cooccurrence_tests))
    content <- paste0(content,
      "## Theme Co-occurrence\n\n",
      "Chi-square tests of independence (or Fisher's exact test when expected ",
      "frequencies < 5) examining whether theme co-occurrence patterns differ ",
      "from what would be expected by chance. Cramer's V (effect size) is the ",
      "primary inferential signal; p-values under three regimes (raw, ",
      "Benjamini-Hochberg FDR, Bonferroni FWER) are reported for transparency. ",
      "Sorted by |Cramer's V|.\n\n",
      "```{r theme-cooccurrence}\n",
      "cooc <- tibble::tibble(\n",
      "  `Theme 1` = ", deparse1(cooccurrence_tests$theme1), ",\n",
      "  `Theme 2` = ", deparse1(cooccurrence_tests$theme2), ",\n",
      "  `Observed Both` = ", deparse1(cooccurrence_tests$observed_both), ",\n",
      "  `Expected Both` = ", deparse1(round(cooccurrence_tests$expected_both, 1)), ",\n",
      "  Statistic = ", deparse1(round(cooccurrence_tests$statistic, 3)), ",\n",
      "  `Cramer's V` = ", deparse1(round(cooccurrence_tests$cramers_v, 3)),
      if (has_tiered_p) paste0(",\n",
        "  `p (raw)` = ", deparse1(signif(cooccurrence_tests$p_raw, 4)), ",\n",
        "  `p (BH FDR)` = ", deparse1(signif(cooccurrence_tests$p_bh, 4)), ",\n",
        "  `p (Bonf)` = ", deparse1(signif(cooccurrence_tests$p_bonferroni, 4))
      ) else paste0(",\n",
        "  `P-value` = ", deparse1(signif(cooccurrence_tests$p_adjusted, 4))
      ), "\n",
      ")\n",
      "if (has_dt) {\n",
      "  DT::datatable(cooc,\n",
      "                options = list(pageLength = 10, dom = 'ftp', scrollX = TRUE),\n",
      "                rownames = FALSE,\n",
      "                class = 'compact stripe',\n",
      "                caption = 'Theme Co-occurrence: Chi-Square / Fisher Tests')\n",
      "} else {\n",
      "  knitr::kable(cooc, digits = 3, caption = 'Theme Co-occurrence: Chi-Square / Fisher Tests')\n",
      "}\n",
      "```\n\n"
    )
  }

  content
}

.build_synthesis_section <- function(insights, ai_synthesis = NULL) {
  content <- "# Synthesis & Conclusion\n\n"

  # Key findings
  if (!is.null(insights$key_findings) && length(insights$key_findings) > 0) {
    content <- paste0(content, "## Key Findings\n\n")
    findings <- insights$key_findings

    if (is.data.frame(findings)) {
      for (i in seq_len(min(5, nrow(findings)))) {
        content <- paste0(content,
          "### ", i, ". ", findings$insight[i], "\n\n",
          findings$explanation[i], "\n\n"
        )
      }
    } else if (is.list(findings)) {
      for (i in seq_along(findings)) {
        f <- findings[[i]]
        insight_text <- if (is.list(f)) f$insight else as.character(f)
        explanation <- if (is.list(f)) f$explanation %||% "" else ""
        content <- paste0(content, "### ", i, ". ", insight_text, "\n\n")
        if (nchar(explanation) > 0) {
          content <- paste0(content, explanation, "\n\n")
        }
      }
    }
  }

  if (!is.null(insights$theoretical_implications)) {
    content <- paste0(content,
      "## Theoretical Implications\n\n",
      insights$theoretical_implications, "\n\n"
    )
  }

  if (!is.null(insights$practical_implications)) {
    content <- paste0(content,
      "## Practical Implications\n\n",
      insights$practical_implications, "\n\n"
    )
  }

  # Append conclusion into synthesis section (Issue 12)
  if (!is.null(ai_synthesis) && !is.null(ai_synthesis$conclusion)) {
    content <- paste0(content,
      "## Conclusion\n\n",
      ai_synthesis$conclusion, "\n\n"
    )
  }

  content
}

# ==============================================================================
# Saturation section
# ==============================================================================

.build_saturation_section <- function(coding_state) {
  sat <- coding_state$saturation
  curve <- sat$curve

  content <- "# Thematic Saturation Analysis\n\n"

  if (isTRUE(sat$reached)) {
    content <- paste0(content,
      "Thematic saturation was **reached** after coding **",
      sat$reached_at_coded, "** of ", sat$total_entries_at_saturation,
      " total entries. At that point, the codebook contained **",
      length(coding_state$codebook), "** unique codes.\n\n"
    )

    # Describe which signals triggered saturation
    signals <- c()
    if (isTRUE(sat$signals$code_creation_rate)) {
      signals <- c(signals, "new code creation rate dropped below threshold")
    }
    if (isTRUE(sat$signals$slope_ratio)) {
      signals <- c(signals, "Inductive Thematic Saturation ratio reached threshold (De Paoli & Mathis, 2024)")
    }
    if (isTRUE(sat$signals$ai_self_assessment)) {
      signals <- c(signals, "AI self-assessment reported no novel patterns remaining")
    }

    if (length(signals) > 0) {
      content <- paste0(content,
        "Saturation was triggered by the following convergent signals: ",
        paste(signals, collapse = "; "), ".\n\n"
      )
    }

    content <- paste0(content,
      "The saturation ratio (codes / coded entries) was **",
      sat$saturation_ratio, "**, indicating that on average one new code was created ",
      "for every ", round(1 / sat$saturation_ratio), " coded entries.\n\n"
    )
  } else {
    content <- paste0(content,
      "Thematic saturation was **not reached** during this analysis. ",
      "All ", length(coding_state$entries_processed), " entries were processed, ",
      "yielding ", length(coding_state$codebook), " unique codes.\n\n"
    )
  }

  # Saturation curve (generated inline via R code in the Rmd)
  content <- paste0(content, "## Saturation Curve\n\n")
  {
    if (nrow(curve) > 0) {
      content <- paste0(content,
        "```{r saturation-curve, echo=FALSE, fig.width=9, fig.height=5.5}\n",
        "curve_data <- data.frame(\n",
        "  entries_coded = c(", paste(curve$entries_coded, collapse = ", "), "),\n",
        "  n_codes = c(", paste(curve$n_codes, collapse = ", "), "),\n",
        "  new_codes = c(", paste(curve$new_codes_in_window, collapse = ", "), ")\n",
        ")\n",
        "par(mar = c(5, 5, 4, 5))\n",
        "plot(curve_data$entries_coded, curve_data$n_codes,\n",
        "     type = 'l', lwd = 2.5, col = '#2c3e50',\n",
        "     xlab = 'Entries Coded', ylab = 'Cumulative Unique Codes',\n",
        "     main = 'Thematic Saturation Curve', las = 1, bty = 'l')\n",
        "par(new = TRUE)\n",
        "plot(curve_data$entries_coded, curve_data$new_codes,\n",
        "     type = 'l', lwd = 1.5, col = '#e74c3c', lty = 2,\n",
        "     axes = FALSE, xlab = '', ylab = '')\n",
        "axis(side = 4, col = '#e74c3c', col.axis = '#e74c3c', las = 1)\n",
        "mtext('New Codes per Window', side = 4, line = 3, col = '#e74c3c')\n"
      )

      if (isTRUE(sat$reached)) {
        sat_idx <- which.min(abs(curve$entries_coded - sat$reached_at_coded))
        content <- paste0(content,
          "abline(v = ", sat$reached_at_coded, ", col = '#e67e22', lty = 3, lwd = 1.5)\n",
          "points(", sat$reached_at_coded, ", ", curve$n_codes[sat_idx],
          ", pch = 19, col = '#e67e22', cex = 2)\n",
          "text(", sat$reached_at_coded, ", ", curve$n_codes[sat_idx],
          ", labels = paste0('Saturation\\n(', ", sat$reached_at_coded,
          ", ' entries, ', ", curve$n_codes[sat_idx], ", ' codes)'),\n",
          "     pos = 4, col = '#e67e22', cex = 0.8, font = 2)\n"
        )
      }

      content <- paste0(content,
        "legend('right',\n",
        "  legend = c('Cumulative codes', 'New codes/window'",
        if (isTRUE(sat$reached)) ", 'Saturation point'" else "",
        "),\n",
        "  col = c('#2c3e50', '#e74c3c'",
        if (isTRUE(sat$reached)) ", '#e67e22'" else "",
        "),\n",
        "  lty = c(1, 2",
        if (isTRUE(sat$reached)) ", NA" else "",
        "),\n",
        "  pch = c(NA, NA",
        if (isTRUE(sat$reached)) ", 19" else "",
        "),\n",
        "  lwd = c(2.5, 1.5",
        if (isTRUE(sat$reached)) ", NA" else "",
        "),\n",
        "  cex = 0.75, bg = 'white')\n",
        "```\n\n"
      )
    }
  }

  # Methodological note for paper
  content <- paste0(content,
    "## Methodological Note\n\n",
    "> **Suggested text for methods section:** "
  )

  if (isTRUE(sat$reached)) {
    content <- paste0(content,
      "Thematic saturation was assessed using a triangulated approach combining ",
      "code creation rate monitoring (Guest, Namey, & Chen, 2020), Inductive ",
      "Thematic Saturation ratio analysis (De Paoli & Mathis, 2024, ",
      "doi:10.1007/s11135-024-01950-6), and AI self-assessment. The ITS ratio ",
      "(unique codes / total code assignments; threshold < 0.05 in this ",
      "implementation) measures codebook stability through code reuse density. ",
      "Saturation was declared when at least two of three independent signals ",
      "converged, indicating that the codebook was stable and no novel thematic ",
      "patterns were emerging. Saturation was reached after coding ",
      sat$reached_at_coded, " of ", sat$total_entries_at_saturation,
      " entries, at which point ", length(coding_state$codebook),
      " unique codes had been identified.\n\n"
    )
  } else {
    content <- paste0(content,
      "All ", length(coding_state$entries_processed), " entries were coded. ",
      "Thematic saturation was monitored using code creation rate tracking ",
      "(Guest, Namey, & Chen, 2020) and Inductive Thematic Saturation ratio ",
      "analysis (De Paoli & Mathis, 2024, doi:10.1007/s11135-024-01950-6), ",
      "but was not formally declared as new codes continued to emerge ",
      "throughout the analysis.\n\n"
    )
  }

  content
}

.build_methodology_appendix <- function(stats, export_files, config,
                                         excerpt_verification = NULL) {
  content <- paste0(
    "# Appendix A: Methodology\n\n",
    "## Analysis Process\n\n",
    "This analysis employed AI-assisted reflexive thematic analysis following Braun and Clarke's ",
    "approach, using a progressive sequential coding pipeline:\n\n",
    "1. **Learning from prior studies** -- codebook structures and coding conventions from previous manual analyses\n",
    "2. **Progressive sequential coding** -- each entry read individually; applicable text coded with existing or novel codes\n",
    "3. **Thematic saturation detection** -- triangulated monitoring of code creation rate, reuse stability, and AI self-assessment\n",
    "4. **Code-aware sentiment analysis** -- sentiment scored on coded entries using assigned codes as context\n",
    "5. **Iterative bottom-up theme generation** -- sequential merging of codes into clusters across multiple passes\n",
    "6. **Deterministic code-path cascading** -- entries mapped to themes through their codes (no AI re-reading)\n",
    "7. **Correlation analysis** -- statistical associations between themes, sentiment, and metadata\n\n",
    "## Top Codes\n\n",
    "```{r code-table}\n",
    "codes <- read_csv('", basename(export_files$codes_file), "', show_col_types = FALSE)\n\n",
    "codes_display <- codes |>\n",
    "  arrange(desc(frequency)) |>\n",
    "  head(30) |>\n",
    "  select(Code = code_text, Type = code_type, Frequency = frequency)\n\n",
    "if (has_dt) {\n",
    "  DT::datatable(codes_display,\n",
    "                options = list(pageLength = 10, dom = 'ftp'),\n",
    "                rownames = FALSE,\n",
    "                class = 'compact stripe',\n",
    "                caption = 'Top 30 Consolidated Codes by Frequency')\n",
    "} else {\n",
    "  knitr::kable(codes_display, caption = 'Top 30 Consolidated Codes by Frequency')\n",
    "}\n",
    "```\n\n"
  )

  # Config table (guard against NULL config)
  if (is.null(config)) config <- list()
  provider_name <- config$ai$provider %||% "openai"
  model_name <- config$ai[[provider_name]]$models$primary %||% "N/A"
  min_themes <- config$analysis$themes$min_themes
  max_themes <- config$analysis$themes$max_themes
  max_prop <- (config$analysis$themes$max_theme_proportion %||% 0.60) * 100

  theme_range_str <- if (!is.null(min_themes) && !is.null(max_themes)) {
    paste0(min_themes, "-", max_themes, " (soft guidance)")
  } else {
    "Data-driven (no fixed range)"
  }

  # Expanded configuration table
  fast_model <- config$ai[[provider_name]]$models$fast %||% model_name
  reasoning_model <- config$ai[[provider_name]]$models$reasoning %||% "N/A"
  corr_method <- config$analysis$correlations$method %||% "spearman"
  p_adjust <- config$analysis$correlations$adjust_method %||% "bonferroni"
  dynamic_corr <- if (isTRUE(config$analysis$correlations$dynamic_method)) "Yes (per-pair)" else "No"
  multi_label <- if (isTRUE(config$analysis$themes$multi_label_assignment)) "Yes" else "No"

  content <- paste0(content,
    "## AI Models and Configuration\n\n",
    "| Parameter | Value |\n",
    "|-----------|-------|\n",
    "| AI Provider | ", provider_name, " |\n",
    "| Primary Model | ", model_name, " |\n",
    "| Fast Model (sentiment) | ", fast_model, " |\n",
    "| Reasoning Model (themes, review) | ", reasoning_model, " |\n",
    "| Theme Range | ", theme_range_str, " |\n",
    "| Max Theme Proportion | ", max_prop, "% |\n",
    "| Multi-Label Assignment | ", multi_label, " |\n",
    "| Correlation Method | ", corr_method, " |\n",
    "| Dynamic Method Selection | ", dynamic_corr, " |\n",
    "| P-Value Adjustment | ", p_adjust, " |\n\n"
  )

  # Note on dynamic correlation method selection
  if (isTRUE(config$analysis$correlations$dynamic_method)) {
    content <- paste0(content,
      "**Dynamic Correlation Method Selection:** When enabled, the correlation method ",
      "is selected per variable pair based on variable types. Binary-binary pairs use ",
      "Pearson (phi coefficient), binary-continuous pairs use Pearson (point-biserial), ",
      "continuous pairs use Pearson if both pass Shapiro-Wilk normality test (otherwise Spearman), ",
      "and ordinal pairs use Spearman rank correlation.\n\n"
    )
  }

  # Token limits table -- restrict to tasks actually used by the v1.0
  # pipeline so legacy keys (consolidation/assignment/relevance from the
  # pre-1.0 architecture) carried over in user configs are not shown.
  v1_tasks <- c("coding", "theming", "sentiment", "review", "insight", "synthesis")
  max_tokens <- config$ai$max_tokens
  if (!is.null(max_tokens) && length(max_tokens) > 0) {
    active_tasks <- intersect(v1_tasks, names(max_tokens))
    if (length(active_tasks) > 0) {
      content <- paste0(content,
        "## Token Limits per Task\n\n",
        "| Task | Max Tokens | Temperature |\n",
        "|------|----------:|------------:|\n"
      )
      temps <- config$ai$temperature %||% list()
      for (task_name in active_tasks) {
        temp_val <- temps[[task_name]] %||% "default"
        content <- paste0(content,
          "| ", task_name, " | ",
          format(max_tokens[[task_name]], big.mark = ","), " | ",
          temp_val, " |\n"
        )
      }
      content <- paste0(content, "\n")
    }
  }

  # Methodological notes
  content <- paste0(content,
    "## Statistical Notes\n\n",
    "**Correlation method:** The default Spearman rank correlation is used between ",
    "continuous sentiment scores and binary theme membership variables. When correlating ",
    "binary (0/1) membership with continuous variables, researchers may also consider ",
    "Pearson correlation (equivalent to point-biserial correlation) as an alternative. ",
    "The `method` parameter in the configuration allows switching between methods.\n\n",
    "**Multiple testing:** Bonferroni correction is applied within the correlation analysis ",
    "to control family-wise error rate. However, the full analysis pipeline involves ",
    "multiple sequential decision points (saturation detection, theme convergence ",
    "detection, theme cascading, and merge-pass termination). Each decision introduces ",
    "potential for cumulative error. Readers should interpret individual findings ",
    "within this context and prioritize patterns that replicate across runs.\n\n",
    "**Theme group comparisons:** Mann-Whitney U tests (non-parametric) compare continuous ",
    "variables between theme members and non-members. Effect size is computed as ",
    "r = |Z| / sqrt(N). P-values are Bonferroni-adjusted across all tests.\n\n",
    "**Theme co-occurrence:** Chi-square tests of independence assess whether theme pairs ",
    "co-occur more or less often than expected by chance. Fisher's exact test is substituted ",
    "when any expected cell frequency falls below 5. Effect size is reported as Cramer's V.\n\n"
  )

  # Excerpt verification results
  if (!is.null(excerpt_verification)) {
    content <- paste0(content, "## Data Quality: Excerpt Verification\n\n")

    ss <- excerpt_verification$substring_stats
    if (!is.null(ss) && ss$total > 0) {
      content <- paste0(content,
        "**Substring Validation:** ", ss$valid, " of ", ss$total,
        " coded excerpts (", ss$pct_valid, "%) are verbatim substrings of their source text.\n\n"
      )
      if (ss$invalid > 0) {
        content <- paste0(content,
          "<div class='callout callout-neutral'>\n",
          ss$invalid, " excerpt(s) could not be matched as exact substrings. ",
          "These may have been paraphrased by the AI coder or truncated during processing.\n",
          "</div>\n\n"
        )
      }
    }

    cs <- excerpt_verification$coherence_stats
    if (!is.null(cs)) {
      content <- paste0(content,
        "**Theme-Excerpt Coherence:** AI spot-check of ", cs$n_checked,
        " random excerpt-theme pairings yielded a mean coherence score of **",
        cs$mean_score, "/5**"
      )
      if (cs$n_low_coherence > 0) {
        content <- paste0(content,
          " (", cs$n_low_coherence, " pair(s) scored 2 or below).\n\n")
      } else {
        content <- paste0(content, ".\n\n")
      }
    }
  }

  content
}

# ==============================================================================
# Internal: Cross-Run Comparison Section
# ==============================================================================

.build_comparison_section <- function(comparison) {
  content <- paste0(
    "# Cross-Run Comparison {.tabset}\n\n",
    "This section compares the current analysis run against **",
    comparison$n_runs - 1, " previous run(s)**.\n\n"
  )

  # --- 1. Sample Overlap ---
  if (!is.null(comparison$sample_overlap)) {
    so <- comparison$sample_overlap
    pw <- so$pairwise

    content <- paste0(content,
      "## Sample Overlap\n\n",
      "<div class='metrics-grid'>\n",
      "<div class='metric-card'><div class='metric-value'>",
        sprintf("%.1f%%", pw$pct_shared),
        "</div><div class='metric-label'>Entries Shared</div></div>\n",
      "<div class='metric-card'><div class='metric-value'>",
        pw$n_new,
        "</div><div class='metric-label'>New Entries</div></div>\n",
      "<div class='metric-card'><div class='metric-value'>",
        pw$n_dropped,
        "</div><div class='metric-label'>Dropped Entries</div></div>\n",
      "</div>\n\n"
    )

    # Interpretation banner
    interp_class <- switch(so$interpretation,
      "identical sample" = "positive",
      "mostly same sample" = "positive",
      "overlapping samples" = "neutral",
      "largely different samples" = "negative",
      "neutral"
    )
    content <- paste0(content,
      "<div class='callout callout-", interp_class, "'>\n",
      "<strong>Sample Assessment:</strong> ",
      .html_esc(tools::toTitleCase(so$interpretation)),
      " (Jaccard index: ", sprintf("%.3f", pw$jaccard_index), ")",
      if (so$text_changes > 0) paste0(
        ". Note: ", so$text_changes, " shared entries had text changes (re-preprocessing detected)."
      ) else "",
      "\n</div>\n\n"
    )

    # Source composition table if available
    if (nrow(so$per_run) > 0 && !all(is.na(so$per_run$posts_pct))) {
      content <- paste0(content,
        "**Source Composition Across Runs:**\n\n",
        "| Run | Total | Posts | Comments | Posts % |\n",
        "|-----|-------|-------|----------|--------|\n"
      )
      for (i in seq_len(nrow(so$per_run))) {
        r <- so$per_run[i, ]
        content <- paste0(content,
          "| ", .html_esc(r$run_id), " | ", r$total_entries,
          " | ", ifelse(is.na(r$n_from_posts), "&mdash;", r$n_from_posts),
          " | ", ifelse(is.na(r$n_from_comments), "&mdash;", r$n_from_comments),
          " | ", ifelse(is.na(r$posts_pct), "&mdash;", paste0(r$posts_pct, "%")),
          " |\n"
        )
      }
      content <- paste0(content, "\n")
    }
  }

  # --- 2. Sentiment Drift ---
  if (!is.null(comparison$sentiment_drift)) {
    sd <- comparison$sentiment_drift

    content <- paste0(content,
      "## Sentiment Drift\n\n"
    )

    # Summary metrics
    if (!is.null(sd$summary) && !is.na(sd$summary$mean_shift)) {
      shift_dir <- if (sd$summary$mean_shift > 0.05) "more positive" else
        if (sd$summary$mean_shift < -0.05) "more negative" else "stable"

      content <- paste0(content,
        "<div class='metrics-grid'>\n",
        "<div class='metric-card'><div class='metric-value'>",
          sprintf("%+.3f", sd$summary$mean_shift),
          "</div><div class='metric-label'>Mean Shift</div></div>\n",
        "<div class='metric-card'><div class='metric-value'>",
          sprintf("%.1f%%", sd$summary$reclassification_rate %||% 0),
          "</div><div class='metric-label'>Emotion Reclassification</div></div>\n",
        "<div class='metric-card'><div class='metric-value'>",
          sd$summary$n_shared_entries,
          "</div><div class='metric-label'>Entries Compared</div></div>\n",
        "</div>\n\n",
        "Sentiment has been **", shift_dir, "** between runs.\n\n"
      )
    }

    # Per-run sentiment trend table
    if (nrow(sd$per_run) > 0) {
      content <- paste0(content,
        "**Sentiment Summary Per Run:**\n\n",
        "| Run | Mean | Median | SD | Top Emotions | % Negative | % Positive |\n",
        "|-----|------|--------|----|--------------------|------------|------------|\n"
      )
      for (i in seq_len(nrow(sd$per_run))) {
        r <- sd$per_run[i, ]
        content <- paste0(content,
          "| ", .html_esc(r$run_id),
          " | ", sprintf("%.3f", r$mean_sentiment),
          " | ", sprintf("%.3f", r$median_sentiment),
          " | ", sprintf("%.3f", r$sd_sentiment),
          " | ", .html_esc(r$top_emotions %||% "\u2014"),
          " | ", sprintf("%.1f%%", r$pct_negative),
          " | ", sprintf("%.1f%%", r$pct_positive),
          " |\n"
        )
      }
      content <- paste0(content, "\n")
    }

    # Sentiment drift plot (if 2+ runs)
    if (nrow(sd$per_run) >= 2) {
      content <- paste0(content,
        "```{r sentiment-drift-plot, fig.width=8, fig.height=4, echo=FALSE}\n",
        "sentiment_trend <- data.frame(\n",
        "  run = c(", paste0('"', sd$per_run$run_id, '"', collapse = ", "), "),\n",
        "  mean_sentiment = c(", paste(sd$per_run$mean_sentiment, collapse = ", "), "),\n",
        "  stringsAsFactors = FALSE\n",
        ")\n",
        "sentiment_trend$run <- factor(sentiment_trend$run, levels = sentiment_trend$run)\n",
        "ggplot(sentiment_trend, aes(x = run, y = mean_sentiment, group = 1)) +\n",
        "  geom_line(color = '#4477AA', linewidth = 1.2) +\n",
        "  geom_point(color = '#4477AA', size = 3) +\n",
        "  geom_hline(yintercept = 0, linetype = 'dashed', color = '#999') +\n",
        "  labs(title = 'Mean Sentiment Across Runs', x = NULL, y = 'Mean Sentiment') +\n",
        "  theme_report() +\n",
        "  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))\n",
        "```\n\n"
      )
    }
  }

  # --- 3. Code Stability ---
  if (!is.null(comparison$code_stability)) {
    cs <- comparison$code_stability

    content <- paste0(content,
      "## Code Stability\n\n"
    )

    if (!is.null(cs$stability)) {
      content <- paste0(content,
        "<div class='metrics-grid'>\n",
        "<div class='metric-card'><div class='metric-value'>",
          sprintf("%.1f%%", cs$stability$jaccard_overall * 100),
          "</div><div class='metric-label'>Code Set Overlap</div></div>\n",
        "<div class='metric-card'><div class='metric-value'>",
          sprintf("%.1f%%", cs$stability$churn_rate * 100),
          "</div><div class='metric-label'>Churn Rate</div></div>\n",
        "<div class='metric-card'><div class='metric-value'>",
          cs$stability$n_stable %||% 0,
          " / ", (cs$stability$n_renamed %||% 0),
          " / ", cs$stability$n_new %||% 0,
          " / ", cs$stability$n_dropped %||% 0,
          "</div><div class='metric-label'>Stable / Renamed / New / Dropped</div></div>\n",
        "</div>\n\n"
      )
    }

    # Renamed codes table
    if (!is.null(cs$pairwise$renamed) && nrow(cs$pairwise$renamed) > 0) {
      content <- paste0(content,
        "**Renamed Codes (high similarity, different text):**\n\n",
        "| Previous | Current | Similarity |\n",
        "|----------|---------|------------|\n"
      )
      for (i in seq_len(min(10, nrow(cs$pairwise$renamed)))) {
        r <- cs$pairwise$renamed[i, ]
        content <- paste0(content,
          "| ", .html_esc(r$code_prev), " | ", .html_esc(r$code_curr),
          " | ", sprintf("%.1f%%", r$similarity * 100), " |\n"
        )
      }
      content <- paste0(content, "\n")
    }
  }

  # --- 4. Theme Evolution ---
  if (!is.null(comparison$theme_evolution) && !is.null(comparison$theme_evolution$pairwise)) {
    te <- comparison$theme_evolution$pairwise

    content <- paste0(content,
      "## Theme Evolution\n\n"
    )

    # Persisted themes
    if (nrow(te$persisted) > 0) {
      content <- paste0(content,
        "**Persisted Themes** (matched across runs):\n\n",
        "| Previous | Current | Name Sim | Code Overlap | Entries (prev &rarr; curr) | Sentiment |\n",
        "|----------|---------|----------|-------------|----------------------|----------|\n"
      )
      for (i in seq_len(nrow(te$persisted))) {
        r <- te$persisted[i, ]
        ec_change <- if (!is.na(r$entry_count_prev) && !is.na(r$entry_count_curr)) {
          paste0(r$entry_count_prev, " &rarr; ", r$entry_count_curr)
        } else "&mdash;"
        sent_change <- if (!is.na(r$sentiment_prev) && !is.na(r$sentiment_curr)) {
          paste0(r$sentiment_prev, " &rarr; ", r$sentiment_curr)
        } else "&mdash;"

        content <- paste0(content,
          "| ", .html_esc(r$theme_prev), " | ", .html_esc(r$theme_curr),
          " | ", sprintf("%.0f%%", r$name_sim * 100),
          " | ", sprintf("%.0f%%", r$code_jaccard * 100),
          " | ", ec_change,
          " | ", sent_change,
          " |\n"
        )
      }
      content <- paste0(content, "\n")
    }

    # New themes
    if (nrow(te$new) > 0) {
      content <- paste0(content,
        "**New Themes** (not in previous run):\n\n"
      )
      for (i in seq_len(nrow(te$new))) {
        content <- paste0(content,
          "- <span class='comparison-badge comparison-badge-new'>NEW</span> **",
          .html_esc(te$new$theme_name[i]), "**",
          if (!is.na(te$new$entry_count[i])) paste0(" (", te$new$entry_count[i], " entries)") else "",
          "\n"
        )
      }
      content <- paste0(content, "\n")
    }

    # Disappeared themes
    if (nrow(te$disappeared) > 0) {
      content <- paste0(content,
        "**Disappeared Themes** (in previous run, not current):\n\n"
      )
      for (i in seq_len(nrow(te$disappeared))) {
        content <- paste0(content,
          "- <span class='comparison-badge comparison-badge-gone'>GONE</span> **",
          .html_esc(te$disappeared$theme_name[i]), "**",
          if (!is.na(te$disappeared$entry_count[i])) paste0(" (had ", te$disappeared$entry_count[i], " entries)") else "",
          "\n"
        )
      }
      content <- paste0(content, "\n")
    }
  }

  # --- 5. Entry Migration ---
  if (!is.null(comparison$entry_migration) && !is.na(comparison$entry_migration$stability_rate)) {
    em <- comparison$entry_migration

    content <- paste0(content,
      "## Entry Migration\n\n",
      "<div class='metrics-grid'>\n",
      "<div class='metric-card'><div class='metric-value'>",
        sprintf("%.1f%%", em$stability_rate * 100),
        "</div><div class='metric-label'>Theme Stability</div></div>\n",
      "<div class='metric-card'><div class='metric-value'>",
        em$n_stable,
        "</div><div class='metric-label'>Stable Entries</div></div>\n",
      "<div class='metric-card'><div class='metric-value'>",
        em$n_migrated,
        "</div><div class='metric-label'>Migrated Entries</div></div>\n",
      "</div>\n\n"
    )

    # Migration heatmap
    if (nrow(em$matrix) > 0) {
      # Serialize matrix data for the ggplot chunk
      mat_str <- paste0(
        "data.frame(\n",
        "  theme_prev = c(", paste0('"', em$matrix$theme_prev, '"', collapse = ", "), "),\n",
        "  theme_curr = c(", paste0('"', em$matrix$theme_curr, '"', collapse = ", "), "),\n",
        "  n_entries = c(", paste(em$matrix$n_entries, collapse = ", "), "),\n",
        "  stringsAsFactors = FALSE\n",
        ")"
      )

      content <- paste0(content,
        "```{r migration-heatmap, fig.width=10, fig.height=7, echo=FALSE}\n",
        "migration_data <- ", mat_str, "\n",
        "# Truncate long theme names\n",
        "migration_data$theme_prev <- ifelse(nchar(migration_data$theme_prev) > 30,\n",
        "  paste0(substr(migration_data$theme_prev, 1, 27), '...'), migration_data$theme_prev)\n",
        "migration_data$theme_curr <- ifelse(nchar(migration_data$theme_curr) > 30,\n",
        "  paste0(substr(migration_data$theme_curr, 1, 27), '...'), migration_data$theme_curr)\n",
        "ggplot(migration_data, aes(x = theme_curr, y = theme_prev, fill = n_entries)) +\n",
        "  geom_tile(color = 'white', linewidth = 0.5) +\n",
        "  geom_text(aes(label = n_entries), color = 'black', size = 3.5) +\n",
        "  scale_fill_gradient(low = '#f0f4ff', high = '#4477AA', name = 'Entries') +\n",
        "  labs(title = 'Entry Migration Between Runs',\n",
        "       x = 'Current Run Themes', y = 'Previous Run Themes') +\n",
        "  theme_report() +\n",
        "  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),\n",
        "        axis.text.y = element_text(size = 8))\n",
        "```\n\n"
      )
    }
  }

  # --- 6. Correlation Stability ---
  if (!is.null(comparison$correlation_stability)) {
    cors <- comparison$correlation_stability

    content <- paste0(content,
      "## Correlation Stability\n\n"
    )

    if (nrow(cors$persistent) > 0) {
      content <- paste0(content,
        "**Persistent Correlations** (significant in all ", comparison$n_runs, " runs):\n\n",
        "| Variable Pair | Mean r | Runs Significant |\n",
        "|--------------|--------|------------------|\n"
      )
      for (i in seq_len(nrow(cors$persistent))) {
        r <- cors$persistent[i, ]
        content <- paste0(content,
          "| ", .html_esc(r$var1), " &harr; ", .html_esc(r$var2),
          " | ", sprintf("%.3f", r$mean_correlation),
          " | ", r$n_runs_significant, "/", comparison$n_runs,
          " |\n"
        )
      }
      content <- paste0(content, "\n")
    }

    n_inter <- nrow(cors$intermittent)
    n_spec <- nrow(cors$run_specific)
    if (n_inter > 0 || n_spec > 0) {
      content <- paste0(content,
        "Additionally: **", n_inter, "** intermittent correlation(s) ",
        "and **", n_spec, "** run-specific correlation(s) were identified.\n\n"
      )
    }

    if (nrow(cors$persistent) == 0 && n_inter == 0 && n_spec == 0) {
      content <- paste0(content,
        "No significant correlations found across runs for comparison.\n\n"
      )
    }
  }

  # --- 7. Run Dashboard ---
  if (!is.null(comparison$dashboard) && nrow(comparison$dashboard) > 0) {
    db <- comparison$dashboard

    content <- paste0(content,
      "## Run Dashboard\n\n",
      "| Run | Date | Entries | Themes | Mean Sent. | Emotion | Sig. Corr. | Codes |\n",
      "|-----|------|---------|--------|-----------|---------|------------|-------|\n"
    )
    for (i in seq_len(nrow(db))) {
      r <- db[i, ]
      content <- paste0(content,
        "| ", .html_esc(r$run_id),
        " | ", .html_esc(r$date),
        " | ", r$total_entries,
        " | ", r$n_themes,
        " | ", sprintf("%.3f", r$mean_sentiment),
        " | ", .html_esc(r$top_emotions %||% "\u2014"),
        " | ", r$n_significant_correlations,
        " | ", r$n_codes,
        " |\n"
      )
    }
    content <- paste0(content, "\n")
  }

  content
}

# ==============================================================================
# Internal: ggplot2 theme code for Rmd setup chunk
# ==============================================================================

.ggplot_theme_code <- function() {
  '
# Custom ggplot2 theme matching report style
theme_report <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#EAECEE", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "#2C3E50", linewidth = 0.3),
      axis.ticks = element_line(color = "#BDC3C7", linewidth = 0.3),
      axis.text = element_text(color = "#7F8C8D", size = 10),
      axis.title = element_text(color = "#2C3E50", size = 11, face = "bold"),
      plot.title = element_text(color = "#2C3E50", size = 15, face = "bold",
                                margin = margin(b = 8)),
      plot.subtitle = element_text(color = "#7F8C8D", size = 11,
                                   margin = margin(b = 12)),
      legend.position = "bottom",
      legend.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "#2C3E50", face = "bold", size = 9),
      legend.text = element_text(color = "#7F8C8D", size = 9),
      plot.margin = margin(15, 15, 15, 15)
    )
}

report_colors <- c(
  "#3498DB", "#9B59B6", "#E74C3C", "#27AE60",
  "#F39C12", "#1ABC9C", "#E67E22", "#34495E",
  "#16A085", "#8E44AD"
)

# Expand report_colors palette to n colors using interpolation
expand_report_colors <- function(n) {
  if (n <= length(report_colors)) {
    return(report_colors[seq_len(n)])
  }
  grDevices::colorRampPalette(report_colors)(n)
}

sentiment_colors <- c(
  "negative" = "#E74C3C",
  "neutral" = "#F39C12",
  "positive" = "#27AE60"
)
'
}

# ==============================================================================
# Internal: Generate separate theme detail HTML files
# ==============================================================================

.generate_theme_detail_htmls <- function(theme_stats, theme_order, export_files,
                                          output_dir, data = NULL,
                                          coding_results = NULL) {
  detail_dir <- file.path(output_dir, "theme_details")
  dir.create(detail_dir, recursive = TRUE, showWarnings = FALSE)

  generated <- list()

  for (tn in theme_order) {
    if (!tn %in% names(theme_stats)) next
    ts <- theme_stats[[tn]]
    safe_name <- make_safe_filename(tn)
    detail_file <- file.path(detail_dir, paste0("theme_", safe_name, ".html"))

    html <- paste0(
      '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
      '<meta charset="UTF-8">\n',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0">\n',
      '<title>', .html_esc(tn), ' -- Theme Details</title>\n',
      '<link rel="stylesheet" href="../styles.css">\n',
      # DataTables CDN with offline fallback
      '<link rel="stylesheet" href="https://cdn.datatables.net/1.13.8/css/jquery.dataTables.min.css">\n',
      '<script src="https://code.jquery.com/jquery-3.7.1.min.js"></script>\n',
      '<script src="https://cdn.datatables.net/1.13.8/js/jquery.dataTables.min.js"></script>\n',
      '<style>\n',
      '#entries-table { table-layout: fixed; width: 100% !important; }\n',
      '#entries-table th:nth-child(1) { width: 40%; }\n',
      '#entries-table th:nth-child(2) { width: 10%; }\n',
      '#entries-table th:nth-child(3) { width: 12%; }\n',
      '#entries-table th:nth-child(4) { width: 13%; }\n',
      '#entries-table th:nth-child(5) { width: 25%; }\n',
      '#entries-table td:first-child { max-width: 400px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }\n',
      '#entries-table td:first-child:hover { white-space: normal; word-wrap: break-word; }\n',
      '</style>\n',
      '</head>\n<body>\n',
      '<div style="max-width: 900px; margin: 2rem auto; padding: 0 1.5rem;">\n',
      '<a href="../analysis_report.html" class="appendix-back-link">Back to Report</a>\n',
      '<h1>', .html_esc(tn), '</h1>\n',
      '<p class="theme-description">', .html_esc(ts$description %||% ""), '</p>\n',
      '<div class="theme-meta">\n',
      '<div class="theme-meta-item"><span class="theme-meta-value">', ts$n_entries, '</span>',
      '<span class="theme-meta-label">Total Entries</span></div>\n',
      '<div class="theme-meta-item"><span class="theme-meta-value">', ts$sentiment$mean, '</span>',
      '<span class="theme-meta-label">Mean Sentiment</span></div>\n',
      '<div class="theme-meta-item"><span class="theme-meta-value">', ts$intensity$mean, '</span>',
      '<span class="theme-meta-label">Mean Intensity</span></div>\n',
      '</div>\n'
    )

    # Subthemes (structured with descriptions when available)
    if (!is.null(ts$subthemes_structured) && length(ts$subthemes_structured) > 0) {
      html <- paste0(html, '<h2>Subthemes</h2>\n<div class="subthemes-list">\n')
      for (s in ts$subthemes_structured) {
        s_name <- s$name %||% as.character(s)
        s_desc <- s$description %||% ""
        html <- paste0(html,
          '<div class="subtheme-item" style="margin-bottom: 0.5rem;">\n',
          '<strong>', .html_esc(s_name), '</strong>',
          if (nchar(s_desc) > 0) paste0(' &mdash; ', .html_esc(s_desc)) else "",
          '\n</div>\n')
      }
      html <- paste0(html, '</div>\n')
    } else if (length(ts$subthemes) > 0 && !all(is.na(unlist(ts$subthemes)))) {
      subs <- ts$subthemes[!is.na(ts$subthemes) & nchar(as.character(ts$subthemes)) > 0]
      if (length(subs) > 0) {
        html <- paste0(html, '<h2>Subthemes</h2>\n<div>\n')
        for (s in subs) {
          html <- paste0(html, '<span class="keyword-pill">', .html_esc(s), '</span>\n')
        }
        html <- paste0(html, '</div>\n')
      }
    }

    # Keywords
    if (!is.null(ts$keywords) && length(ts$keywords) > 0) {
      html <- paste0(html, '<h2>Keywords</h2>\n<div class="keywords-container">\n')
      for (k in ts$keywords) {
        html <- paste0(html, '<span class="keyword-pill">', .html_esc(k), '</span>\n')
      }
      html <- paste0(html, '</div>\n')
    }

    # Quotes
    html <- paste0(html, '<h2>Representative Quotes</h2>\n')
    if (!is.null(ts$quotes_with_context) && length(ts$quotes_with_context) > 0) {
      for (qt in names(ts$quotes_with_context)) {
        q <- ts$quotes_with_context[[qt]]
        if (is.null(q$text) || is.na(q$text)) next
        q_sent <- q$sentiment %||% 0
        qclass <- if (is.na(q_sent)) "neutral" else if (q_sent < .SENTIMENT_NEGATIVE_THRESHOLD) "negative" else if (q_sent > .SENTIMENT_POSITIVE_THRESHOLD) "positive" else "neutral"
        html <- paste0(html,
          '<div class="quote-box ', qclass, '">\n',
          .html_esc(gsub("\n", " ", q$text)), '\n',
          '<div class="quote-meta">\n',
          '<span class="sentiment-pill ', qclass, '">Sentiment: ', round(q_sent, 2), '</span>\n',
          ' Emotion: ', .html_esc(q$emotion %||% "N/A"), '\n',
          '</div>\n</div>\n'
        )
      }
    }

    # Interactive entry table (Issue 8)
    if (!is.null(data)) {
      safe_col <- paste0("theme_membership_", make.names(tn))
      if (safe_col %in% names(data)) {
        theme_entries <- data[data[[safe_col]] == 1L, ]
      } else if ("emerged_themes" %in% names(data)) {
        theme_entries <- data[!is.na(data$emerged_themes) &
                               grepl(tn, data$emerged_themes, fixed = TRUE), ]
      } else {
        theme_entries <- data[0, ]
      }
      if (nrow(theme_entries) > 0) {
        text_col <- if ("original_text" %in% names(theme_entries)) "original_text" else "std_text"

        html <- paste0(html, '<h2>All Entries</h2>\n',
          '<table id="entries-table" class="display" style="width:100%">\n',
          '<thead><tr><th>Text</th><th>Sentiment</th><th>Emotion</th>',
          '<th>Sent. Confidence</th><th>Codes</th></tr></thead>\n<tbody>\n')

        entry_excerpts <- if (!is.null(coding_results)) coding_results$entry_excerpts else NULL

        for (ri in seq_len(nrow(theme_entries))) {
          row <- theme_entries[ri, ]
          entry_id <- as.character(row$std_id)
          full_text <- as.character(row[[text_col]])
          display_text <- .html_esc(substr(full_text, 1, 200))
          sent_val <- round(row$sentiment_score %||% 0, 2)
          emotion <- .html_esc(row$all_emotions %||% "N/A")
          conf <- round(row$confidence %||% 0, 2)

          # Get codes for this entry
          codes_str <- ""
          if (!is.null(entry_excerpts) && !is.null(entry_excerpts[[entry_id]])) {
            code_names <- vapply(entry_excerpts[[entry_id]], function(x) x$code %||% "", character(1))
            codes_str <- .html_esc(paste(unique(code_names), collapse = "; "))
          }

          html <- paste0(html,
            '<tr><td>', display_text, '</td><td>', sent_val,
            '</td><td>', emotion, '</td><td>', conf,
            '</td><td>', codes_str, '</td></tr>\n')
        }

        html <- paste0(html, '</tbody>\n</table>\n')
      }
    }

    # DataTable init script (graceful offline fallback)
    html <- paste0(html,
      '<script>\n',
      'document.addEventListener("DOMContentLoaded", function() {\n',
      '  if (typeof jQuery !== "undefined" && jQuery.fn.DataTable) {\n',
      '    jQuery("#entries-table").DataTable({pageLength: 25, scrollX: true});\n',
      '  }\n',
      '});\n',
      '</script>\n')

    # Download link
    csv_info <- export_files$theme_csv_files[[tn]]
    if (!is.null(csv_info)) {
      csv_rel <- paste0("../", csv_info$relative_path)
      html <- paste0(html,
        '<div class="download-box">\n',
        '<a href="', csv_rel, '" class="download-link" download>',
        'Download All ', ts$n_entries, ' Entries as CSV</a>\n',
        '</div>\n'
      )
    }

    html <- paste0(html, '</div>\n</body>\n</html>')
    writeLines(html, detail_file)

    generated[[tn]] <- list(
      file_path = detail_file,
      relative_path = paste0("theme_details/theme_", safe_name, ".html")
    )
  }

  log_info("Generated {length(generated)} theme detail HTML files")
  generated
}
