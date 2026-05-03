# ==============================================================================
# Main Pipeline Orchestrator -- run_analysis()
# ==============================================================================
# Inductive-first pipeline: codebook-first learning -> progressive sequential
# coding -> code-aware sentiment -> iterative bottom-up theme merging ->
# deterministic code-path cascading -> correlations -> report.
# ==============================================================================

#' Run the full thematic analysis pipeline
#'
#' Orchestrates all steps from data loading through report generation.
#' Supports checkpoint/resume for expensive API operations.
#'
#' @param config_path Path to YAML config file
#' @param resume Logical; if TRUE, resume from last checkpoint
#' @param config_overrides Named list of dot-path overrides
#' @return List with data, analytic_data, coding_state, theme_set, correlations, etc.
#' @export
run_analysis <- function(config_path, resume = FALSE, config_overrides = list()) {

  # ========================================================================
  # STEP 0: Load config & initialize
  # ========================================================================

  old_locale <- Sys.getlocale("LC_CTYPE")
  if (!grepl("UTF-8|utf8", old_locale, ignore.case = TRUE)) {
    tryCatch({
      Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
      log_info("Set locale to UTF-8 for proper Unicode handling")
    }, warning = function(w) log_warn("Could not set UTF-8 locale: {w$message}"),
    error = function(e) log_warn("Could not set UTF-8 locale: {e$message}"))
    on.exit(tryCatch(Sys.setlocale("LC_CTYPE", old_locale), error = function(e) NULL), add = TRUE)
  }

  config <- load_config(config_path, overrides = config_overrides)

  # Set logger threshold
  log_level_str <- toupper(config$logging$log_level %||% "INFO")
  log_level_map <- list(
    DEBUG = logger::DEBUG, INFO = logger::INFO,
    WARN = logger::WARN, ERROR = logger::ERROR
  )
  if (!is.null(log_level_map[[log_level_str]])) {
    logger::log_threshold(log_level_map[[log_level_str]])
  }

  pkg_version <- tryCatch(
    as.character(utils::packageVersion("pakhom")),
    error = function(e) "dev"
  )
  log_info("========================================")
  log_info("STARTING pakhom pipeline v{pkg_version}")
  log_info("Study: {config$study$name}")
  log_info("========================================")

  total_time <- tic("Total analysis")

  concepts <- config$study$concepts

  # Propagate settings into subsection configs
  dyn_batch <- config$analysis$dynamic_batching %||% TRUE
  config$analysis$sentiment$dynamic_batching <- dyn_batch
  # Propagate reflexivity fields into subsection configs
  reflexivity_block <- .build_reflexivity_block(config$study)
  config$analysis$coding$researcher_positionality <- config$study$researcher_positionality
  config$analysis$coding$reflexivity_block <- reflexivity_block
  config$analysis$themes$researcher_positionality <- config$study$researcher_positionality
  config$analysis$themes$reflexivity_block <- reflexivity_block
  config$analysis$sentiment$reflexivity_block <- reflexivity_block
  config$analysis$correlations$reflexivity_block <- reflexivity_block

  # Initialize AI provider
  provider <- create_ai_provider(config$ai$provider, config)

  # Create output directory
  results_base <- config$output$results_dir
  dir.create(results_base, recursive = TRUE, showWarnings = FALSE)

  # Determine resume point
  resume_step <- NULL
  if (isTRUE(resume)) {
    latest_run <- find_latest_run(results_base)
    if (!is.null(latest_run)) {
      output_dir <- file.path(results_base, latest_run)
      log_info("Resuming run: {latest_run}")
    } else {
      output_dir <- file.path(results_base, generate_run_id())
      log_info("No previous run found -- starting fresh as {basename(output_dir)}")
    }
  } else {
    output_dir <- file.path(results_base, generate_run_id())
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  checkpoint <- init_checkpoints(
    output_dir = output_dir,
    config_hash = hash_config(config_path)
  )

  # Save run metadata (for cross-run and inter-model comparison).
  # `analysis_schema_version` records which output-column schema this run
  # produced; .SCHEMA_VERSION lives in R/15_comparison.R. compare_runs uses
  # this to refuse cross-schema comparisons that would silently NA-pad
  # several panels.
  run_metadata <- list(
    provider = config$ai$provider,
    model_primary = provider$models$primary,
    model_fast = provider$models$fast %||% provider$models$primary,
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    config_hash = hash_config(config_path),
    study_name = config$study$name,
    research_focus = config$study$research_focus,
    package_version = as.character(utils::packageVersion("pakhom")),
    analysis_schema_version = .SCHEMA_VERSION
  )
  tryCatch(
    jsonlite::write_json(run_metadata, file.path(output_dir, "run_metadata.json"),
                          pretty = TRUE, auto_unbox = TRUE),
    error = function(e) log_warn("Could not save run metadata: {e$message}")
  )

  # Initialize AI decision audit log (T1.4: methodology_mode auto-stamped on
  # every record because we pass config; pre-T1.4 callers without config still
  # work, just without methodology stamping).
  audit_log <- tryCatch(
    init_audit_log(output_dir, config = config),
    error = function(e) { log_warn("Audit log init failed: {e$message}"); NULL }
  )
  on.exit(if (!is.null(audit_log)) tryCatch(close_audit_log(audit_log), error = function(e) NULL), add = TRUE)

  # Initialize content-addressable response cache (T1.4 / OS.5 prep). When
  # config$audit$capture_raw_responses is FALSE the cache is created in
  # disabled mode (no directory, no writes) so caller code is unconditional.
  response_cache <- tryCatch(
    init_response_cache(output_dir, config = config),
    error = function(e) { log_warn("Response cache init failed: {e$message}"); NULL }
  )

  # T0.1: initialize the fabrication log (writes outputs/<run>/fabrication_log.csv).
  # The verification ladder in .code_entry_progressive runs unconditionally;
  # the CSV is the audit artifact for the methodology paper's
  # fabrication-rate KPI. NULL on init failure so callers degrade gracefully.
  fabrication_log <- tryCatch(
    init_fabrication_log(output_dir),
    error = function(e) { log_warn("Fabrication log init failed: {e$message}"); NULL }
  )

  if (isTRUE(resume)) {
    resume_step <- find_resume_point(checkpoint)
    if (!is.null(resume_step)) {
      log_info("Resuming from checkpoint: {resume_step}")
    } else {
      log_info("No checkpoint found -- starting fresh")
    }
  }

  completed <- if (!is.null(resume_step)) {
    list_checkpoints(checkpoint)$completed
  } else {
    character(0)
  }

  # ========================================================================
  # STEP 0b: Scrape data (optional)
  # ========================================================================
  if (isTRUE(config$scraping$enabled)) {
    log_info("\n[STEP 0b] Scraping Reddit data...")
    tryCatch({
      scrape_result <- scrape_reddit(config)
      log_info("Scraping added {scrape_result$posts_added} posts, {scrape_result$comments_added} comments")
    }, error = function(e) {
      log_warn("Scraping failed: {e$message}")
      log_warn("Continuing with existing data in database")
    })
  }

  # ========================================================================
  # STEP 1: Load previous analyses (codebook-first learning)
  # ========================================================================
  learning_context <- .empty_learning_context()

  if (isTRUE(config$learning$enabled)) {
    log_info("\n[STEP 1] Loading previous analyses (codebook-first)...")
    tryCatch({
      studies <- load_previous_studies(config$learning$base_dir, config$learning)
      learning_context <- generate_learning_context(studies)
      learning_context <- generate_learning_reflection(learning_context, provider,
                                                         audit_log = audit_log,
                                                         response_cache = response_cache)
    }, error = function(e) {
      log_warn("Codebook learning failed: {e$message}")
      log_warn("Continuing without learning context")
    })
  } else {
    log_info("\n[STEP 1] Learning from previous analyses disabled -- skipping")
  }

  # ========================================================================
  # STEP 2: Load and preprocess data
  # ========================================================================
  if ("data_loaded" %in% completed) {
    log_info("\n[STEP 2] Loading data from checkpoint...")
    data <- load_checkpoint(checkpoint, "data_loaded")
  } else {
    log_info("\n[STEP 2] Loading and preparing data...")

    db_path <- config$data$database
    tables <- config$data$tables

    if (length(tables) > 1) {
      data <- load_and_combine_tables(db_path, tables,
                                       source_type = config$data$source_type,
                                       config = config$data)
    } else {
      raw <- load_data(db_path, tables[1])
      col_map <- detect_columns(raw, config$data$source_type, config$data)
      data <- standardize_data(raw, col_map)
    }

    preprocess_config <- config$data$preprocessing
    preprocess_config$source_type <- config$data$source_type
    data <- preprocess_text(data, preprocess_config)
    log_info("Data loaded: {nrow(data)} entries")

    # Apply test mode sampling if enabled
    if (isTRUE(config$analysis$test_mode$enabled)) {
      test_n <- config$analysis$test_mode$sample_size %||% 100
      test_seed <- config$analysis$test_mode$seed %||% 42
      if (test_n < nrow(data)) {
        set.seed(test_seed)
        data <- data[sample(nrow(data), test_n), ]
        log_info("TEST MODE: sampled {test_n} entries (seed={test_seed})")
      }
    }

    save_checkpoint(checkpoint, "data_loaded", data)
  }

  # ========================================================================
  # STEP 3: Progressive sequential coding
  # ========================================================================
  coding_state <- NULL

  if ("progressive_coding" %in% completed) {
    log_info("\n[STEP 3] Loading coding state from checkpoint...")
    coding_state <- load_checkpoint(checkpoint, "progressive_coding")
  } else {
    log_info("\n[STEP 3] Running progressive sequential coding...")
    log_info("  Processing {nrow(data)} entries one at a time (no batching)...")

    # Check for partial coding state to resume from
    resume_state <- NULL
    partial_path <- file.path(checkpoint$checkpoint_dir, "progressive_coding_partial.rds")
    if (file.exists(partial_path)) {
      partial_checkpoint <- tryCatch(readRDS(partial_path), error = function(e) NULL)
      if (!is.null(partial_checkpoint)) {
        # Partial checkpoints are saved as list(data = CodingState, progress_idx, timestamp)
        resume_state <- if (inherits(partial_checkpoint, "ProgressiveCodingState")) {
          partial_checkpoint
        } else if (is.list(partial_checkpoint) && inherits(partial_checkpoint$data, "ProgressiveCodingState")) {
          partial_checkpoint$data
        } else {
          log_warn("Partial checkpoint exists but format unrecognized -- starting fresh")
          NULL
        }
        if (!is.null(resume_state)) {
          log_info("  Found partial coding state: {length(resume_state$entries_processed)} entries already processed, {length(resume_state$codebook)} codes")
        }
      }
    }

    coding_state <- run_progressive_coding(
      data, provider, config$analysis$coding,
      learning_context = learning_context,
      research_focus = config$study$research_focus,
      checkpoint = checkpoint,
      concepts = concepts,
      resume_state = resume_state,
      audit_log = audit_log,
      response_cache = response_cache,
      fabrication_log = fabrication_log
    )

    save_checkpoint(checkpoint, "progressive_coding", coding_state)
  }

  # Generate saturation curve plot
  saturation_plot_path <- generate_saturation_plot(coding_state, output_dir)

  # Derive analytic sample (entries that received at least one code)
  analytic_data <- get_analytic_sample(coding_state, data)
  log_info("Analytic sample: {nrow(analytic_data)}/{nrow(data)} entries received codes")

  # ========================================================================
  # STEP 3b: Human verification / IRR (optional, before codebook review)
  # ========================================================================
  irr_result <- NULL
  if (isTRUE(config$analysis$human_verification$enabled)) {
    log_info("\n[STEP 3b] Human verification (IRR)...")
    irr_result <- tryCatch({
      run_human_verification(
        data = analytic_data,
        coding_state = coding_state,
        config = config$analysis$human_verification,
        output_dir = output_dir
      )
    }, error = function(e) {
      log_warn("Human verification failed: {e$message}")
      NULL
    })
    if (!is.null(irr_result) && irr_result$status == "exported") {
      log_info("Pipeline paused for human verification.")
      log_info("Complete the coding sheet and re-run with resume = TRUE.")
      if (!is.null(audit_log)) tryCatch(close_audit_log(audit_log), error = function(e) NULL)
      toc()
      return(invisible(list(
        status = "paused_for_human_verification",
        output_dir = output_dir,
        data = data, coding_state = coding_state
      )))
    }
  }

  # ========================================================================
  # RECURSIVE REVIEW LOOP
  # Wraps: codebook review -> sentiment -> theme generation -> theme review
  # The researcher can loop back from theme review to codebook review
  # to iteratively refine the analysis (Braun & Clarke reflexive TA).
  # ========================================================================
  review_iteration <- 0L
  max_review_iterations <- config$analysis$review_points$max_iterations %||% 3L
  theme_set <- NULL

  repeat {
    review_iteration <- review_iteration + 1L
    if (review_iteration > 1L) {
      log_info("\n========== REVIEW ITERATION {review_iteration}/{max_review_iterations} ==========")
    }

    # Re-read completed checkpoints (may have been invalidated by loop-back)
    completed <- list_checkpoints(checkpoint)$completed

    # ------------------------------------------------------------------
    # PAUSE POINT A: Researcher review of codebook
    # ------------------------------------------------------------------
    if (isTRUE(config$analysis$review_points$after_coding)) {
      review_result <- review_progressive_codebook(coding_state, output_dir,
                                                    audit_log = audit_log,
                                                    irr_result = irr_result)
      if (review_result$status == "exported") {
        log_info("Pipeline paused for codebook review (iteration {review_iteration}).")
        log_info("Edit 'codebook_review.csv', save as 'codebook_reviewed.csv',")
        log_info("then re-run with resume = TRUE.")
        if (!is.null(audit_log)) tryCatch(close_audit_log(audit_log), error = function(e) NULL)
        toc()
        return(invisible(list(
          status = "paused_for_codebook_review",
          output_dir = output_dir,
          data = data, coding_state = coding_state
        )))
      }
      coding_state <- review_result$coding_state
      if (review_result$status == "applied") {
        analytic_data <- get_analytic_sample(coding_state, data)
        save_checkpoint(checkpoint, "progressive_coding", coding_state)
      }
    }

    # ------------------------------------------------------------------
    # STEP 4: Sentiment analysis (code-aware)
    # ------------------------------------------------------------------
    if (review_iteration == 1L && "sentiment_done" %in% completed) {
      log_info("\n[STEP 4] Loading sentiment data from checkpoint...")
      analytic_data <- load_checkpoint(checkpoint, "sentiment_done")
    } else {
      log_info("\n[STEP 4] Running code-aware sentiment analysis on {nrow(analytic_data)} entries...")
      analytic_data <- analyze_sentiment(
        analytic_data, provider, config$analysis$sentiment,
        checkpoint = checkpoint,
        research_focus = config$study$research_focus,
        coding_state = coding_state,
        audit_log = audit_log,
        response_cache = response_cache
      )
      save_checkpoint(checkpoint, "sentiment_done", analytic_data)
    }

    # ------------------------------------------------------------------
    # STEP 5: Iterative bottom-up theme generation
    # ------------------------------------------------------------------
    if (review_iteration == 1L && "themes_generated" %in% completed) {
      log_info("\n[STEP 5] Loading themes from checkpoint...")
      theme_set <- load_checkpoint(checkpoint, "themes_generated")
    } else {
      log_info("\n[STEP 5] Generating themes via iterative bottom-up merging...")
      theme_set <- generate_themes_iterative(
        coding_state, provider, config$analysis$themes,
        learning_context = learning_context,
        research_focus = config$study$research_focus,
        concepts = concepts,
        audit_log = audit_log,
        response_cache = response_cache
      )

      if (is.null(theme_set)) {
        log_error("Theme generation failed -- aborting pipeline")
        toc()
        return(NULL)
      }

      save_checkpoint(checkpoint, "themes_generated", theme_set)
    }

    # ------------------------------------------------------------------
    # PAUSE POINT B: Researcher review of themes
    # ------------------------------------------------------------------
    if (isTRUE(config$analysis$review_points$after_themes)) {
      review_result <- review_themes(theme_set, output_dir, audit_log = audit_log)
      if (review_result$status == "exported") {
        log_info("Pipeline paused for theme review (iteration {review_iteration}).")
        log_info("Re-run with resume = TRUE after reviewing.")
        log_info("Set disposition to 'revise_codebook' in review_disposition.csv to loop back.")
        if (!is.null(audit_log)) tryCatch(close_audit_log(audit_log), error = function(e) NULL)
        toc()
        return(invisible(list(
          status = "paused_for_theme_review",
          output_dir = output_dir,
          data = data, coding_state = coding_state, theme_set = theme_set
        )))
      }
      theme_set <- review_result$theme_set
      if (review_result$status == "applied") {
        # Rebuild code-to-theme mapping after researcher restructuring
        theme_set <- rebuild_code_to_theme_map(theme_set, coding_state)
        save_checkpoint(checkpoint, "themes_generated", theme_set)

        # Check disposition: loop back or continue?
        disposition <- read_review_disposition(output_dir)
        if (disposition == "revise_codebook" &&
            review_iteration < max_review_iterations) {
          log_info("Researcher requested codebook revision (iteration {review_iteration}/{max_review_iterations})")

          # Log disposition to audit
          if (!is.null(audit_log)) {
            log_ai_decision(audit_log, "researcher_review", "review_disposition",
                            disposition = "revise_codebook",
                            iteration = review_iteration)
          }

          # Invalidate downstream checkpoints so they re-run
          invalidate_checkpoints_from(checkpoint, "sentiment_done")

          # Clean reviewed files for fresh export in next iteration
          review_dir <- file.path(output_dir, "researcher_review")
          for (f in c("codebook_reviewed.csv", "themes_reviewed.csv",
                      "review_disposition.csv")) {
            fpath <- file.path(review_dir, f)
            if (file.exists(fpath)) file.remove(fpath)
          }

          next  # Loop back to codebook review
        }
      }
    }

    break  # Normal forward flow -- proceed to cascading and correlations
  }

  # ========================================================================
  # STEP 5b: Deterministic theme cascading (pure R, no AI)
  # ========================================================================
  log_info("\n[STEP 5b] Cascading theme assignments (code-path, deterministic)...")
  analytic_data <- cascade_theme_assignments(analytic_data, coding_state, theme_set)

  # Enrich themes with entry counts, sentiment, quotes
  theme_set <- enrich_themes(theme_set, analytic_data, coding_state)

  # Safety net: prune any themes that ended up with zero entries
  # (shouldn't happen with code-path cascading, but guard against edge cases)
  theme_set <- prune_empty_themes(theme_set)
  valid_names <- theme_names(theme_set)

  # ========================================================================
  # STEP 6: Correlation analysis
  # ========================================================================
  correlations_df <- NULL
  corr_results <- NULL
  insights <- list()
  theme_group_tests <- NULL
  cooccurrence_tests <- NULL

  if ("correlations" %in% completed) {
    log_info("\n[STEP 6] Loading correlations from checkpoint...")
    corr_result <- load_checkpoint(checkpoint, "correlations")
    correlations_df <- corr_result$correlations_df
    corr_results <- corr_result$corr_results
    insights <- corr_result$insights
    theme_group_tests <- corr_result$theme_group_tests
    cooccurrence_tests <- corr_result$cooccurrence_tests
  } else {
    log_info("\n[STEP 6] Running correlation analysis...")
    corr_data <- prepare_correlation_data(analytic_data, theme_set,
                                           config$analysis$correlations)

    use_dynamic <- config$analysis$correlations$dynamic_method %||% TRUE
    var_types <- if (isTRUE(use_dynamic)) detect_variable_types(corr_data) else NULL

    corr_results <- calculate_correlations(
      corr_data,
      method = config$analysis$correlations$method %||% "spearman",
      adjust_method = config$analysis$correlations$adjust_method %||% "bonferroni",
      var_types = var_types,
      dynamic_method = use_dynamic
    )
    correlations_df <- extract_significant(corr_results, corr_data = corr_data)

    log_info("Running theme group comparisons...")
    theme_group_tests <- tryCatch(
      compare_theme_groups(analytic_data, theme_set, config$analysis$correlations),
      error = function(e) { log_warn("Theme group comparison failed: {e$message}"); NULL }
    )

    log_info("Testing theme co-occurrence...")
    cooccurrence_tests <- tryCatch(
      test_theme_cooccurrence(analytic_data, theme_set),
      error = function(e) { log_warn("Co-occurrence test failed: {e$message}"); NULL }
    )

    log_info("Generating insights...")
    insights <- generate_insights(correlations_df, theme_set, provider,
                                   research_focus = config$study$research_focus,
                                   config = config$analysis$correlations,
                                   audit_log = audit_log,
                                   response_cache = response_cache)

    save_checkpoint(checkpoint, "correlations",
                     list(correlations_df = correlations_df,
                          corr_results = corr_results,
                          insights = insights,
                          theme_group_tests = theme_group_tests,
                          cooccurrence_tests = cooccurrence_tests))
  }

  # ========================================================================
  # STEP 7: Export results
  # ========================================================================
  log_info("\n[STEP 7] Exporting results...")

  # Convert coding_state to legacy format for export compatibility
  coding_results <- as_coding_results(coding_state)
  consolidated <- .build_pseudo_consolidated(coding_state)

  export_files <- export_results(analytic_data, theme_set, correlations_df, insights,
                                  consolidated, output_dir)

  if (isTRUE(config$output$generate_correlation_plot)) {
    create_correlation_plot(corr_results, export_files$plot_file)
  }

  # Theme network plot
  network_file <- file.path(output_dir, "theme_network.png")
  tryCatch({
    create_theme_network(analytic_data, theme_set, output_path = network_file)
  }, error = function(e) log_warn("Theme network plot failed: {e$message}"))

  # ========================================================================
  # STEP 8: Cross-run comparison (optional)
  # ========================================================================
  comparison_result <- NULL
  if (isTRUE(config$output$comparison_enabled %||% TRUE)) {
    log_info("\n[STEP 8] Comparing with previous runs...")
    comparison_result <- tryCatch({
      cr <- compare_runs(output_dir, results_base, config)
      if (!is.null(cr)) {
        log_info("Comparison complete: {cr$n_runs} runs compared")
        comp_file <- file.path(output_dir, "comparison_data.json")
        tryCatch(
          jsonlite::write_json(cr$dashboard, comp_file, pretty = TRUE),
          error = function(e) log_warn("Could not save comparison data: {e$message}")
        )
      }
      cr
    }, error = function(e) {
      log_warn("Cross-run comparison failed: {e$message}")
      NULL
    })
  }

  # ========================================================================
  # STEP 9: Generate report
  # ========================================================================
  if (isTRUE(config$output$generate_report)) {
    log_info("\n[STEP 9] Generating analysis report...")
    report_file <- file.path(output_dir, "analysis_report.html")
    generate_report(
      data = analytic_data,
      theme_set = theme_set,
      correlations_df = correlations_df,
      insights = insights,
      export_files = export_files,
      consolidated = consolidated,
      learning_context = learning_context,
      provider = provider,
      config = config,
      output_file = report_file,
      comparison_result = comparison_result,
      coding_results = coding_results,
      coding_state = coding_state,
      theme_group_tests = theme_group_tests,
      cooccurrence_tests = cooccurrence_tests,
      audit_log = audit_log,
      response_cache = response_cache
    )
  }

  # ========================================================================
  # STEP 9b: QDPX export (for QDA software interoperability)
  # ========================================================================
  if (isTRUE(config$output$export_qdpx)) {
    log_info("\n[STEP 9b] Exporting QDPX file...")
    tryCatch({
      qdpx_path <- file.path(output_dir, paste0(make_safe_filename(config$study$name), ".qdpx"))
      export_qdpx(coding_state, analytic_data, qdpx_path,
                   theme_set = theme_set,
                   study_name = config$study$name %||% "pakhom export")
    }, error = function(e) log_warn("QDPX export failed: {e$message}"))
  }

  # ========================================================================
  # STEP 9c: Longitudinal / temporal analysis
  # ========================================================================
  temporal_results <- NULL
  if ("std_timestamp" %in% names(analytic_data)) {
    log_info("\n[STEP 9c] Running temporal analysis...")
    temporal_results <- tryCatch({
      tr <- analyze_temporal_patterns(analytic_data, theme_set, coding_state)
      if (isTRUE(tr$has_temporal_data)) {
        generate_temporal_plots(tr, output_dir)
        log_info("Temporal analysis complete: {tr$period_type} periods")
      } else {
        log_info("No parseable timestamps -- temporal analysis skipped")
      }
      tr
    }, error = function(e) {
      log_warn("Temporal analysis failed: {e$message}")
      NULL
    })
  }

  # ========================================================================
  # STEP 10: Post-run integrity check
  # ========================================================================
  integrity <- verify_run_integrity(output_dir, config)
  if (length(integrity$missing) > 0) {
    log_warn("Run integrity: {length(integrity$missing)} file(s) missing:")
    for (f in integrity$missing) log_warn("  - {f}")
  } else {
    log_info("Run integrity: all {length(integrity$expected)} expected files present")
  }

  # ========================================================================
  # SUMMARY
  # ========================================================================
  toc()

  log_info("\n========================================")
  log_info("ANALYSIS COMPLETE")
  log_info("========================================")
  log_info("Total entries:       {nrow(data)}")
  log_info("Analytic sample:     {nrow(analytic_data)}")
  log_info("Themes identified:   {n_themes(theme_set)}")
  log_info("Significant correlations: {sum(correlations_df$significant, na.rm = TRUE)}")
  log_info("Results saved to: {output_dir}")
  log_info("========================================")

  invisible(list(
    data = data,
    analytic_data = analytic_data,
    coding_state = coding_state,
    theme_set = theme_set,
    correlations = correlations_df,
    insights = insights,
    learning_context = learning_context,
    comparison_result = comparison_result,
    export_files = export_files,
    output_dir = output_dir,
    config = config,
    integrity = integrity
  ))
}

# ==============================================================================
# Helper: Build pseudo-consolidated codes for export compatibility
# ==============================================================================

#' @keywords internal
.build_pseudo_consolidated <- function(coding_state) {
  if (!inherits(coding_state, "ProgressiveCodingState")) return(NULL)
  if (length(coding_state$codebook) == 0) return(NULL)

  codes <- tibble::tibble(
    code_key = names(coding_state$codebook),
    code_text = vapply(coding_state$codebook, function(cb) cb$code_name, character(1)),
    code_type = vapply(coding_state$codebook, function(cb) cb$type %||% "descriptive", character(1)),
    frequency = vapply(coding_state$codebook, function(cb) cb$frequency, integer(1)),
    n_entries = vapply(coding_state$codebook, function(cb) length(unique(cb$entry_ids)), integer(1)),
    original_codes = vapply(coding_state$codebook, function(cb) cb$code_name, character(1)),
    entry_ids = vapply(coding_state$codebook, function(cb) {
      paste(unique(cb$entry_ids), collapse = ",")
    }, character(1))
  )

  code_mapping <- setNames(
    vapply(coding_state$codebook, function(cb) cb$code_name, character(1)),
    names(coding_state$codebook)
  )

  list(codes = codes, code_mapping = as.list(code_mapping))
}

# ==============================================================================
# Helper: Build reflexivity block for AI prompt injection
# ==============================================================================

#' Build a standardized reflexivity text block from study config
#'
#' Combines researcher_positionality, research_paradigm, and reflexive_notes
#' into a single text block suitable for injection into AI system prompts.
#' Returns empty string if no reflexivity fields are set.
#'
#' @param study_config The study section of ThematicConfig
#' @return Character string (may be empty)
#' @keywords internal
.build_reflexivity_block <- function(study_config) {
  parts <- character(0)

  pos <- study_config$researcher_positionality
  if (!is.null(pos) && nchar(pos) > 0) {
    parts <- c(parts, paste0("Researcher positionality: ", pos))
  }

  paradigm <- study_config$research_paradigm
  if (!is.null(paradigm) && nchar(paradigm) > 0) {
    parts <- c(parts, paste0("Research paradigm: ", paradigm))
  }

  notes <- study_config$reflexive_notes
  if (!is.null(notes) && nchar(notes) > 0) {
    parts <- c(parts, paste0("Researcher reflections: ", notes))
  }

  if (length(parts) == 0) return("")

  paste0(
    "\n## RESEARCHER POSITIONALITY & REFLEXIVITY\n",
    paste(parts, collapse = "\n"),
    "\nLet this perspective inform your analysis.\n"
  )
}
