# ==============================================================================
# Main Pipeline Orchestrator -- run_analysis()
# ==============================================================================
# Inductive-first pipeline: codebook-first learning -> progressive sequential
# coding -> code-aware sentiment -> HAC + AI-judged divisive tree walk for
# theme generation (Phase 52) -> deterministic code-path cascading ->
# correlations -> report.
# ==============================================================================

#' Run the full thematic analysis pipeline (Mode 2 + Mode 3)
#'
#' Orchestrates all steps from data loading through report generation.
#' Supports checkpoint/resume for expensive API operations. Drives Modes
#' 2 (Codebook Collaborative) and 3 (Framework Applied); Mode 1
#' (Reflexive Scaffold) uses \code{\link{run_mode1}} instead.
#'
#' @param config_path Path to YAML config file. The config must declare
#'   \code{methodology$mode} as one of \code{"codebook_collaborative"}
#'   or \code{"framework_applied"} (Mode 1 has its own entry point).
#'   For Mode 3, the config must also set
#'   \code{methodology$framework_spec_path} to a built-in framework
#'   alias (\code{"tpb"}, \code{"comb"}, \code{"tdf"}) or a path to a
#'   custom framework YAML/JSON.
#' @param resume Logical; if TRUE, resume from last checkpoint. Per AC5
#'   (soft-lock with audit trail), a finalized run cannot be resumed
#'   in place -- doing so would overwrite the canonical outputs without
#'   a fork record. Use \code{\link{clone_run_with_new_mode}} to fork
#'   into a new run dir.
#' @param config_overrides Named list of dot-path overrides applied
#'   after config load. Useful for batch runs.
#' @return Invisible list with \code{data}, \code{analytic_data},
#'   \code{coding_state}, \code{theme_set}, \code{correlations},
#'   \code{insights}, \code{learning_context}, \code{comparison_result},
#'   \code{export_files}, \code{output_dir}, \code{config},
#'   \code{integrity}.
#' @seealso \code{\link{run_mode1}} (Mode 1 entry point);
#'   \code{\link{create_config}} (config builder);
#'   \code{\link{load_framework_spec}} (Mode 3 framework loader);
#'   \code{vignette("methodology-modes")} (per-mode worked examples).
#' @examples
#' \dontrun{
#' # Mode 2 (Codebook Collaborative) -- the auto-pipeline
#' create_config(
#'   methodology = "codebook_collaborative",
#'   study_name = "My Study",
#'   research_focus = "How does X relate to Y?",
#'   database_path = "my_data.db",
#'   output_path = "config.yaml"
#' )
#' result <- run_analysis("config.yaml")
#'
#' # Mode 3 (Framework Applied) -- apply a theoretical framework
#' create_config(
#'   methodology = "framework_applied",
#'   framework_spec_path = "tpb",  # built-in TPB; or path to custom spec
#'   study_name = "TPB analysis",
#'   research_focus = "Behavioral intention -> behavior",
#'   database_path = "my_data.db",
#'   output_path = "config.yaml"
#' )
#' result <- run_analysis("config.yaml")
#' }
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
  # T1.7 (AC4): run-dir name carries the methodology mode short-code so a
  # reviewer scanning the outputs/ directory sees Mode 1 / 2 / 3 without
  # opening run_metadata.json. Spec: SPRINT4_DESIGN.md line 237.
  meth_mode <- .config_methodology_mode(config)

  # AC2 mode dispatch: refuse Mode 1 BEFORE creating output_dir so a
  # mistaken Mode 1 invocation doesn't leave a stranded empty run dir
  # under outputs/. Mode 1 has its own dedicated entry point
  # (run_provocateur_questioning); it does NOT use the auto-pipeline.
  if (identical(meth_mode, "reflexive_scaffold")) {
    stop(
      "Mode 1 (Reflexive Scaffold) does not use the run_analysis() ",
      "auto-pipeline. In Mode 1 the researcher authors codes and themes ",
      "(typically in NVivo / ATLAS.ti); pakhom contributes the ",
      "provocateur questioning loop with full Tier-0/Tier-1 scaffolding ",
      "(run_metadata.json, methodology rules, audit + fabrication logs, ",
      "Mode 1 report, finalize_run).\n\n",
      "Use the dedicated Mode 1 entry point:\n",
      "  result <- run_mode1(\n",
      "    data        = your_corpus,\n",
      "    theme_set   = your_researcher_authored_themes,\n",
      "    config_path = 'config.yaml'\n",
      "  )\n\n",
      "(For the bare provocateur loop without scaffolding -- e.g., for ",
      "test code -- run_provocateur_questioning() is still available.)\n\n",
      "If you intended to use the auto-pipeline, choose Mode 2 ",
      "(codebook_collaborative) or Mode 3 (framework_applied) instead.",
      call. = FALSE
    )
  }

  if (isTRUE(resume)) {
    latest_run <- find_latest_run(results_base)
    if (!is.null(latest_run)) {
      output_dir <- file.path(results_base, latest_run)
      # AC5 enforcement: a finalized run is the FROZEN canonical record.
      # Resuming into it silently overwrites analysis_report.html / CSVs /
      # audit_log.jsonl etc. and the audit trail is lost (replay no longer
      # reproduces the file state of the finalized run). Refuse rather
      # than silently mutate. Spec: SPRINT4_DESIGN.md AC5 ("soft-lock with
      # audit trail; methodology change creates new run").
      if (is_run_finalized(output_dir)) {
        stop(sprintf(
          "Run %s is FINALIZED. Per AC5 (soft-lock with audit trail), a ",
          output_dir
        ),
        "finalized run cannot be resumed in place -- doing so would overwrite ",
        "the canonical outputs (report, CSVs, audit_log.jsonl) without a ",
        "fork record. To re-run with the same methodology, pass resume=FALSE ",
        "to start a fresh run dir. To re-run with a different methodology, ",
        "use clone_run_with_new_mode() to fork into a new dir with ",
        "parent_run_id linkage.",
        call. = FALSE)
      }
      log_info("Resuming run: {latest_run}")
    } else {
      output_dir <- file.path(results_base,
                               run_id_with_mode(generate_run_id(), meth_mode))
      log_info("No previous run found -- starting fresh as {basename(output_dir)}")
    }
  } else {
    output_dir <- file.path(results_base,
                             run_id_with_mode(generate_run_id(), meth_mode))
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # T1.7 (AC4): stamp the methodology + run id in the console banner so a
  # reviewer scrolling logs sees the declaration up-front.
  log_info(stamp_methodology_console(meth_mode, basename(output_dir)))

  # (Mode 1 dispatch was hoisted above output_dir creation so a Mode 1
  # invocation doesn't leave a stranded empty run dir; see audit
  # phase 30.5 finding H3.)

  # Mode 3 (Framework Applied): load the researcher's framework spec.
  # Validation already ensured framework_spec_path is non-empty when
  # mode == "framework_applied" (R/01_config.R). Loading early catches
  # spec errors before expensive coding work.
  framework_spec <- NULL
  framework_archive <- NULL  # populated below for Mode 3
  if (identical(meth_mode, "framework_applied")) {
    framework_spec <- tryCatch(
      load_framework_spec(config$methodology$framework_spec_path),
      error = function(e) {
        stop(
          "Mode 3 (Framework Applied) requires a valid framework spec at ",
          sprintf("'%s', but loading failed: %s",
                   config$methodology$framework_spec_path, e$message),
          call. = FALSE
        )
      }
    )
    log_info("Mode 3 framework loaded: '{framework_spec$name}' ({length(framework_spec$constructs)} constructs)")

    # Phase 32 (audit H1 + H2): archive the framework spec verbatim
    # under outputs/<run>/framework_applied.{yaml|json} and capture the
    # sha256 hash + identity metadata for stamping into
    # run_metadata.json. Per AC4 the archive is mandatory for Mode 3 --
    # without it a reviewer cannot reconstruct WHICH framework was
    # used.
    #
    # Phase 32 audit (M1 + M2): the previous version wrapped this call
    # in a tryCatch that absorbed archive errors into a warning,
    # letting the run finalize with no framework_name / framework_hash
    # in run_metadata.json AND no archived spec on disk. AC4 requires
    # the archive; without it the run's provenance is broken. The
    # auditor's M1+M2 finding is correct: archive failure must be a
    # hard refuse, not a soft warn -- symmetric with the "Mode 3
    # requires a valid framework spec" stop() above. Failure here
    # aborts run_analysis() before any expensive coding work; the
    # output_dir is already created but contains no AI-call artifacts
    # so cleanup is straightforward.
    framework_archive <- archive_framework_spec(framework_spec, output_dir,
                                                   run_id = basename(output_dir))
  }

  checkpoint <- init_checkpoints(
    output_dir = output_dir,
    config_hash = hash_config(config_path)
  )

  # T1.6 methodology rules: generated from config.yaml's methodology block
  # and written to outputs/<run>/rules/methodology_rules.md. The provider
  # also carries the generated rules so ai_complete() injects them as a
  # system-prompt prefix on every call (per AC9 -- rules in the model's
  # context-window every turn). create_ai_provider() handles the prefix
  # injection; here we additionally archive the rules text under the run
  # output directory so the methodology paper can reference exactly which
  # rules were in force during this run.
  tryCatch(write_methodology_rules(config, output_dir),
           error = function(e) log_warn("Could not write methodology rules: {e$message}"))

  # T1.5 soft-lock + parent_run_id check. Before writing run_metadata, see
  # if the run_dir already has a metadata file from a prior run with
  # different methodology -- a finalized mismatch must refuse rather
  # than silently overwriting (AC5: methodology change creates new run);
  # an active mismatch warns but allows continuation (the user might be
  # legitimately fixing a typo'd config mid-run).
  status <- methodology_mismatch_status(output_dir, config)
  if (identical(status, "mismatch_finalized")) {
    prior <- read_run_metadata(output_dir)
    stop(sprintf(
      "run_dir %s is FINALIZED with methodology '%s' but config declares '%s'. ",
      output_dir, prior$methodology_mode, config$methodology$mode
    ),
    "Per AC5 (soft-lock with audit trail), methodology cannot be silently ",
    "re-declared on a finalized run. Use clone_run_with_new_mode() to fork ",
    "into a new run directory with parent_run_id linkage.",
    call. = FALSE)
  } else if (identical(status, "mismatch_active")) {
    prior <- read_run_metadata(output_dir)
    log_warn("Methodology mismatch on active run: stored '{prior$methodology_mode}' vs config '{config$methodology$mode}'. Overwriting metadata; this is not a fork.")
  }

  # Save run metadata (for cross-run and inter-model comparison).
  # `analysis_schema_version` records which output-column schema this run
  # produced; .SCHEMA_VERSION lives in R/15_comparison.R. compare_runs uses
  # this to refuse cross-schema comparisons that would silently NA-pad
  # several panels.
  #
  # T1.5: also stamps methodology_mode + parent_run_id + mode_changed_from
  # + mode_locked_at + is_finalized so the run carries its REDCap-style
  # state record. The init_run_state helper in R/run_state.R is the
  # canonical writer; here we extend the helper's output with the
  # provider/model fields the pipeline carries.
  # Phase 32 (audit H1 + H2): when Mode 3, splat the framework archive
  # metadata into run_metadata.json so cross-run comparisons + replay
  # can route off the framework's identity (name + sha256). Mode 1 and
  # Mode 2 runs leave these fields out (init_run_state writes only the
  # fields supplied via ...). framework_archive may be NULL on Mode 3
  # if the archive call failed; downstream verify_run_integrity
  # surfaces the gap.
  # Audit L2 (phase 32): jsonlite::write_json with auto_unbox=TRUE
  # collapses length-1 character vectors into JSON scalars. For a
  # single-construct framework that would mean
  # framework_construct_ids serializes as a string instead of an
  # array, breaking downstream parsers expecting an array shape.
  # Wrapping construct_ids in as.list() preserves the array shape
  # (jsonlite serializes a length-1 list as a length-1 array).
  framework_extras <- if (!is.null(framework_archive)) list(
    framework_name             = framework_archive$name,
    framework_hash             = framework_archive$hash,
    framework_relative_path    = framework_archive$relative_path,
    framework_epistemic_stance = framework_archive$epistemic_stance,
    framework_anomaly_handling = framework_archive$anomaly_handling,
    framework_n_constructs     = framework_archive$n_constructs,
    framework_construct_ids    = as.list(framework_archive$construct_ids),
    framework_schema_version   = framework_archive$schema_version
  ) else list()

  meta_methodology <- do.call(init_run_state, c(list(
    run_dir          = output_dir,
    run_id           = basename(output_dir),
    methodology_mode = config$methodology$mode,
    parent_run_id    = config$methodology$parent_run_id,
    mode_changed_from = config$methodology$mode_changed_from,
    # Provider/study fields (preserved from earlier schema)
    provider                = config$ai$provider,
    model_primary           = provider$models$primary,
    model_fast              = provider$models$fast %||% provider$models$primary,
    timestamp               = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    config_hash             = hash_config(config_path),
    study_name              = config$study$name,
    research_focus          = config$study$research_focus,
    package_version         = as.character(utils::packageVersion("pakhom")),
    analysis_schema_version = .SCHEMA_VERSION
  ), framework_extras))

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
    init_fabrication_log(output_dir, methodology_mode = config$methodology$mode),
    error = function(e) { log_warn("Fabrication log init failed: {e$message}"); NULL }
  )

  # Phase 53 / C3: initialize the live tracker. Writes three streamed/snapshot
  # artifacts under outputs/<run>/live/ -- code_assignments.jsonl (append-only
  # event log), codebook_live.json (atomic-rewrite snapshot of the current
  # codebook), code_to_cluster.json (atomic-rewrite snapshot of theme/subtheme
  # hierarchy as the HAC tree walk produces themes). Researchers can `tail -F`
  # or `cat` these during a long run to watch the analysis evolve.
  live_tracker <- tryCatch(
    init_live_tracker(output_dir),
    error = function(e) { log_warn("Live tracker init failed: {e$message}"); NULL }
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
      # Phase 50c: thread the user's config-set limits through.
      # Previously this call took function defaults, which stuck
      # max_manuscript_chars at .MAX_ENTRY_CHARS=8000 -- ignoring the
      # user's config$learning$max_manuscript_chars (12000 default).
      learning_context <- generate_learning_context(
        studies,
        max_codebook_chars = config$learning$max_codebook_chars %||% 20000L,
        max_manuscript_chars = config$learning$max_manuscript_chars %||% 12000L,
        max_raw_samples = config$learning$max_raw_samples %||% 5L
      )
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
      # Multi-table loads add source_table via load_and_combine_tables;
      # single-table runs need the same column or downstream consumers
      # (export_theme_entry_csvs, aggregate_overall_statistics) silently
      # render reports without source breakdown.
      data$source_table <- tables[1]
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
      fabrication_log = fabrication_log,
      framework_spec = framework_spec,
      live_tracker = live_tracker
    )

    save_checkpoint(checkpoint, "progressive_coding", coding_state)
  }

  # Generate saturation curve plot
  saturation_plot_path <- generate_saturation_plot(
    coding_state, output_dir,
    methodology_mode = config$methodology$mode,
    run_id = basename(output_dir)
  )

  # T0.3 corpus coverage assertion: compute the funnel from preprocessed
  # data to LLM-processed entries to coded entries, plus the headline
  # "no silent truncation" claim. Computed once after coding completes;
  # the report renders it as a Tier-0 transparency card. We don't have
  # pre-preprocessing counts threaded through the pipeline yet (the
  # data_loaded checkpoint already represents post-preprocess data), so
  # those fields are NA -- the headline claim doesn't depend on them.
  coverage <- tryCatch(
    compute_corpus_coverage(
      coding_state = coding_state,
      data         = data,
      n_after_preprocessing = nrow(data),
      test_mode_sample_size = if (isTRUE(config$analysis$test_mode$enabled))
                                config$analysis$test_mode$sample_size %||% nrow(data)
                              else NA_integer_
    ),
    error = function(e) {
      # Per AC4 ("methodology stamped on every output"), a coverage failure
      # is itself transparency-relevant -- the audit log gets a
      # coverage_failure record so investigators can find why the report's
      # coverage card later renders the unavailable variant. The pipeline
      # continues so the rest of the report still produces.
      log_warn("Failed to compute corpus coverage: {e$message}")
      if (!is.null(audit_log)) {
        tryCatch(
          log_ai_decision(audit_log, "coverage", "coverage_failure",
                          error_message = e$message,
                          n_input_to_coding = nrow(data),
                          n_processed = length(coding_state$entries_processed)),
          error = function(e2) {
            log_warn("Audit-log entry for coverage failure also failed: {e2$message}")
          }
        )
      }
      NULL
    }
  )

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
        output_dir = output_dir,
        methodology_mode = config$methodology$mode
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
                                                    irr_result = irr_result,
                                                    methodology_mode = config$methodology$mode)
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
      # Phase 58 Tier 4 H-12: re-emit the sentiment summary from the
      # LOADED checkpoint data so the on-screen log header matches the
      # actual final data. Pre-fix the resume path skipped
      # analyze_sentiment entirely, which is also where the summary
      # log lines live (R/10_sentiment.R:199-201), so the user only
      # ever saw the PRE-resume summary -- often from a 250-entry
      # sample run, with stale numbers like the Phase 57 audit's
      # -0.139 vs actual -0.0917 mismatch.
      if ("sentiment_score" %in% names(analytic_data)) {
        success_rate <- mean(!is.na(analytic_data$sentiment_score)) * 100
        mean_sent <- mean(analytic_data$sentiment_score, na.rm = TRUE)
        log_info("Sentiment summary (from resumed checkpoint):")
        log_info("  Success rate: {round(success_rate, 1)}%")
        log_info("  Mean sentiment: {round(mean_sent, 3)}")
      }
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
    # STEP 5: Theme generation (HAC + AI-judged divisive tree walk; Phase 52)
    # ------------------------------------------------------------------
    if (review_iteration == 1L && "themes_generated" %in% completed) {
      log_info("\n[STEP 5] Loading themes from checkpoint...")
      theme_set <- load_checkpoint(checkpoint, "themes_generated")
    } else if (!is.null(framework_spec)) {
      # AC2 / AC8 mode dispatch: Mode 3 uses the framework's constructs
      # AS the themes. No iterative merging -- the framework IS the theme
      # structure, fixed at run start. Phase 54: anomaly_handling drives
      # what happens to non-fitting segments (bracket / extend / revise).
      log_info("\n[STEP 5] Mapping framework constructs to themes (Mode 3, anomaly_handling='{framework_spec$anomaly_handling}')...")
      theme_set <- apply_framework_themes(
        coding_state    = coding_state,
        framework_spec  = framework_spec,
        provider        = provider,
        output_dir      = output_dir,
        audit_log       = audit_log,
        response_cache  = response_cache,
        live_tracker    = live_tracker,
        config          = config
      )
      if (is.null(theme_set) || length(theme_set$themes) == 0L) {
        log_warn("Framework theme application produced no themes -- continuing with empty set")
      }
      save_checkpoint(checkpoint, "themes_generated", theme_set)
    } else {
      log_info("\n[STEP 5] Generating themes via HAC + AI-judged divisive tree walk...")
      theme_set <- generate_themes_iterative(
        coding_state, provider, config$analysis$themes,
        learning_context = learning_context,
        research_focus = config$study$research_focus,
        concepts = concepts,
        audit_log = audit_log,
        response_cache = response_cache,
        live_tracker = live_tracker
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
      review_result <- review_themes(theme_set, output_dir, audit_log = audit_log,
                                       methodology_mode = config$methodology$mode)
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
  theme_set <- enrich_themes(theme_set, analytic_data, coding_state,
                               quotes_per_theme = config$analysis$themes$quotes_per_theme %||% 3L)

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

  # Phase 58 Tier 4 C-7 (audit CRITICAL #1 fix): persist EFFECTIVE config
  # values back to the config object UNCONDITIONALLY -- before the
  # checkpoint-resume branch. The pre-followup version mutated config
  # only in the cold-load `else` branch, so a resumed run that hit the
  # `correlations` checkpoint skipped the mutation entirely and the
  # report renderer at R/17_report.R:2551 then read the raw (unresolved)
  # config, reproducing the same lie C-7 was supposed to fix. Phase 57's
  # full-corpus rerun is the canonical resume-from-checkpoint case;
  # placing the mutation here means both cold runs and resumes see the
  # same effective config.
  config$analysis$correlations$dynamic_method <-
    config$analysis$correlations$dynamic_method %||% TRUE
  config$analysis$correlations$method <-
    config$analysis$correlations$method %||% "spearman"
  config$analysis$correlations$adjust_method <-
    config$analysis$correlations$adjust_method %||% "bonferroni"

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

    use_dynamic <- config$analysis$correlations$dynamic_method
    effective_method <- config$analysis$correlations$method
    effective_adjust <- config$analysis$correlations$adjust_method

    var_types <- if (isTRUE(use_dynamic)) detect_variable_types(corr_data) else NULL

    corr_results <- calculate_correlations(
      corr_data,
      method = effective_method,
      adjust_method = effective_adjust,
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
                                  consolidated, output_dir,
                                  methodology_mode = config$methodology$mode)

  if (isTRUE(config$output$generate_correlation_plot)) {
    create_correlation_plot(corr_results, export_files$plot_file,
                             methodology_mode = config$methodology$mode,
                             run_id = basename(output_dir))
  }

  # Theme network plot
  network_file <- file.path(output_dir, "theme_network.png")
  tryCatch({
    create_theme_network(analytic_data, theme_set, output_path = network_file,
                          methodology_mode = config$methodology$mode,
                          run_id = basename(output_dir))
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
        # T1.7 (AC4): stamp the JSON output with methodology mode
        tryCatch(stamp_methodology_json(comp_file, config$methodology$mode,
                                          run_id = basename(output_dir)),
                 error = function(e) log_debug("JSON stamp skipped: {e$message}"))
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
      response_cache = response_cache,
      coverage = coverage,
      # Phase 32: pass framework_spec + archive metadata so the Mode 3
      # report renders the Framework Declaration section + the
      # Citations API silent-bypass footnote in the Tier-0 dashboard.
      framework_spec    = framework_spec,
      framework_archive = framework_archive
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
                   study_name = config$study$name %||% "pakhom export",
                   methodology_mode = config$methodology$mode)
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
        generate_temporal_plots(tr, output_dir,
                                  methodology_mode = config$methodology$mode,
                                  run_id = basename(output_dir))
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

  # T1.5: finalize the run -- methodology declaration is now locked for
  # this canonical output. Any future change to methodology must fork a
  # new run via clone_run_with_new_mode (which writes parent_run_id).
  tryCatch(
    finalize_run(output_dir),
    error = function(e) log_warn("Could not finalize run: {e$message}")
  )

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
