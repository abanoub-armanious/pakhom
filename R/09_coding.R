# ==============================================================================
# Progressive Sequential Coding -- Inductive AI-Assisted Qualitative Coding
# ==============================================================================
# Replaces the old batch coding + dedup + consolidation approach.
# Processes entries strictly one at a time in sequence. The AI maintains
# a growing codebook across entries: for each entry it reads the text
# progressively, codes applicable segments, and either assigns existing
# codes or creates new ones. Entries with no applicable content are
# skipped (excluded from the analytic sample).
#
# This approach is faithful to how manual reflexive thematic analysis
# works in NVivo/ATLAS.ti/MAXQDA.
# ==============================================================================

.PARTIAL_CHECKPOINT_INTERVAL <- 50L

#' Compute the effective per-entry character cap given a provider + config
#'
#' Phase 50f replacement for the hardcoded \code{.MAX_ENTRY_CHARS = 8000L}.
#' Resolution order:
#' \enumerate{
#'   \item If \code{config$ai$max_entry_chars} is a positive integer, use
#'     it verbatim (researcher's explicit override).
#'   \item Otherwise, derive from \code{provider$context_window} (in
#'     tokens) by reserving ~60\% for system prompt + codebook + LLM
#'     completion and assigning the remaining ~40\% to per-entry text.
#'     Convert tokens to chars at ~4 chars/token (English averages
#'     4.0-4.5 chars per BPE token; 4 is a conservative under-estimate
#'     so we don't over-fill context).
#'   \item Floor at 8000L (the legacy default) so behavior is never
#'     worse than the prior hardcode for very-small-context models.
#' }
#'
#' @param provider AIProvider object with \code{$context_window}
#' @param config Pipeline config (reads \code{config$ai$max_entry_chars})
#' @return Integer character cap.
#' @keywords internal
.effective_max_entry_chars <- function(provider, config = list()) {
  override <- tryCatch(config$ai$max_entry_chars, error = function(e) NULL)
  if (!is.null(override) && is.numeric(override) && length(override) == 1L &&
      override > 0L) {
    return(as.integer(override))
  }
  cw <- tryCatch(as.integer(provider$context_window), error = function(e) NULL)
  # MEMORY.md R-quirk: is.na(NULL) returns logical(0); guard with !is.null first
  if (is.null(cw) || length(cw) == 0L || is.na(cw) || cw <= 0L) {
    return(.MAX_ENTRY_CHARS)
  }
  derived <- as.integer(floor(0.40 * cw * 4))   # ~40% of context, 4 chars/token
  max(derived, .MAX_ENTRY_CHARS)                 # never below the legacy floor
}

# ==============================================================================
# S3 class: ProgressiveCodingState
# ==============================================================================

#' Create a new progressive coding state
#'
#' @param learning_context LearningContext object (or NULL)
#' @param config_hash Hash of current config for resume compatibility
#' @return ProgressiveCodingState S3 object
#' @export
create_coding_state <- function(learning_context = NULL, config_hash = NULL) {
  calibration <- list(
    target_granularity = NULL,
    target_codes_per_entry = NULL,
    max_code_coverage_pct = NULL,
    avg_segment_length = NULL,
    example_codes = list(),
    discarded_patterns = character()
  )

  if (!is.null(learning_context) && !is.null(learning_context$benchmarks)) {
    b <- learning_context$benchmarks
    calibration$target_granularity <- b$typical_code_count
    calibration$target_codes_per_entry <- b$codes_per_entry
    calibration$max_code_coverage_pct <- b$max_code_coverage_pct
    calibration$avg_segment_length <- b$avg_segment_length
  }

  state <- list(
    codebook = list(),
    entry_results = list(),
    entries_processed = integer(0),
    entries_skipped = integer(0),
    calibration = calibration,
    config_hash = config_hash,
    created_at = Sys.time(),
    last_updated = Sys.time(),
    # Saturation tracking
    saturation = list(
      curve = data.frame(
        entries_coded = integer(0),
        entries_processed = integer(0),
        n_codes = integer(0),
        new_codes_in_window = integer(0),
        slope_ratio = numeric(0),
        timestamp = as.POSIXct(character(0))
      ),
      # Two parallel maps keyed by code_key:
      #   code_birth_log         -> entry row-index in `data` (used by 21_longitudinal.R
      #                             to look up the timestamp at first code creation)
      #   code_n_coded_at_birth  -> integer count of CODED entries at the moment the
      #                             code was created (used by saturation detection;
      #                             the previous implementation tried to recompute
      #                             this from per-checkpoint accumulator lists,
      #                             which broke after the first checkpoint reset and
      #                             caused premature saturation in long runs)
      code_birth_log = list(),
      code_n_coded_at_birth = list(),
      reached = FALSE,
      reached_at_entry = NA_integer_,
      reached_at_coded = NA_integer_,
      total_entries_at_saturation = NA_integer_,
      signals = list(
        code_creation_rate = FALSE,
        slope_ratio = FALSE,
        ai_self_assessment = FALSE
      ),
      saturation_ratio = NA_real_
    )
  )
  class(state) <- "ProgressiveCodingState"
  state
}

# ==============================================================================
# Main function
# ==============================================================================

#' Run progressive sequential coding on all entries
#'
#' Processes entries strictly one at a time. For each entry, the AI reads
#' the text and codes applicable segments using existing codes or creating
#' new ones. Entries with no applicable content are skipped.
#'
#' @param data Tibble with std_text, std_id columns
#' @param provider AIProvider object
#' @param config Coding config section
#' @param learning_context LearningContext object (or NULL)
#' @param research_focus Research focus string
#' @param checkpoint CheckpointManager (or NULL)
#' @param concepts Character vector of core research concepts (or NULL)
#' @param resume_state ProgressiveCodingState from a previous partial run (or NULL)
#' @param audit_log An AuditLog object (from \code{init_audit_log}) for
#'   recording each coding decision (entry skipped, code assigned, new
#'   code created), or NULL to disable audit logging for this step.
#' @param response_cache An optional ResponseCache object (from
#'   \code{\link{init_response_cache}}). When provided, raw API responses
#'   for each per-entry coding ai_complete() call are written to the cache
#'   and a reference is recorded in the audit log (T1.4). Pass \code{NULL}
#'   to skip raw-response capture.
#' @param fabrication_log An optional FabricationLog object (from
#'   \code{\link{init_fabrication_log}}). T0.1 verification ALWAYS runs --
#'   each per-segment AI-attributed verbatim text is checked against the
#'   entry via the four-step ladder, and fabricated segments are dropped
#'   regardless of whether a log is supplied. When \code{fabrication_log}
#'   is non-NULL, fabrications are also written to
#'   \code{outputs/<run>/fabrication_log.csv} as a CSV audit artifact for
#'   the methodology paper's KPI. Pass \code{NULL} to skip the CSV (the
#'   default for tests + non-pipeline callers).
#' @param framework_spec Optional \code{FrameworkSpec} object (from
#'   \code{\link{load_framework_spec}}). When supplied (Mode 3 / Framework
#'   Applied), the codebook is pre-populated with the framework's
#'   constructs and the AI is constrained to apply them verbatim
#'   (no NEW: prefix path). Anomaly segments go to a dedicated
#'   "anomaly" key. NULL preserves Mode 2 (free-form codebook) behavior.
#' @param live_tracker Optional \code{LiveTracker} (Phase 53; from
#'   \code{\link{init_live_tracker}}). When provided, every coded
#'   segment streams to \code{outputs/<run>/live/code_assignments.jsonl}
#'   and \code{codebook_live.json} is rewritten after every entry so a
#'   researcher can \code{tail -F} or \code{cat} those files mid-run.
#'   Pass \code{NULL} (default) to disable.
#' @return ProgressiveCodingState with all entries processed
#' @export
run_progressive_coding <- function(data, provider, config = list(),
                                    learning_context = NULL,
                                    research_focus = "",
                                    checkpoint = NULL,
                                    concepts = NULL,
                                    resume_state = NULL,
                                    audit_log = NULL,
                                    response_cache = NULL,
                                    fabrication_log = NULL,
                                    framework_spec = NULL,
                                    live_tracker = NULL) {
  config$max_retries_per_entry <- config$max_retries_per_entry %||% 1L
  config$include_in_vivo <- config$include_in_vivo %||% TRUE
  # Phase 50e: removed `config$code_style %||% "descriptive"` --
  # the value was set but never read.
  config$checkpoint_interval <- config$checkpoint_interval %||% .PARTIAL_CHECKPOINT_INTERVAL

  # Saturation detection configuration
  config$saturation_enabled <- config$saturation_enabled %||% TRUE
  config$saturation_window <- config$saturation_window %||% 200L
  config$saturation_threshold <- config$saturation_threshold %||% 2L
  config$saturation_confirmations <- config$saturation_confirmations %||% 3L
  config$min_coded_before_saturation <- config$min_coded_before_saturation %||% 500L
  config$ai_assessment_interval <- config$ai_assessment_interval %||% 200L

  validate_data_columns(data, c("std_text", "std_id"), "run_progressive_coding")
  validate_provider(provider, caller = "run_progressive_coding")

  n <- nrow(data)
  log_info("Starting progressive sequential coding for {n} entries...")
  tic("Progressive coding")

  # Initialize or resume state
  if (!is.null(resume_state) && inherits(resume_state, "ProgressiveCodingState")) {
    state <- resume_state
    log_info("Resuming from previous state: {length(state$entries_processed)} entries already processed, {length(state$codebook)} codes")

    # Check config compatibility
    if (!is.null(state$config_hash) && !is.null(config$config_hash) &&
        state$config_hash != config$config_hash) {
      log_warn("Config has changed since last run. Continuing with existing codes but new config.")
    }

    # AC2/AC8: cross-mode resume guard. If a Mode 2 partial state is
    # resumed under a framework_spec arg, dispatch would Frankenstein
    # the codebook (free-form keys + new construct keys mixed). Refuse
    # rather than silently corrupt the state.
    if (!is.null(framework_spec)) {
      validate_class(framework_spec, "FrameworkSpec")
      missing_constructs <- setdiff(framework_spec$construct_ids,
                                       names(state$codebook))
      if (length(missing_constructs) > 0L) {
        stop(sprintf(
          paste0("Mode 3 resume guard: resumed coding_state's codebook is ",
                 "missing %d framework constructs (%s), suggesting it was ",
                 "produced under a different mode or a different framework. ",
                 "Per AC2 (each mode operating as declared), refusing to ",
                 "silently mix codebooks. Start a fresh Mode 3 run or ",
                 "resume from a state produced under THIS framework."),
          length(missing_constructs),
          paste(head(missing_constructs, 5L), collapse = ", ")
        ), call. = FALSE)
      }
    }

    # Backward-compat: states saved before the saturation tracking fix lack
    # code_n_coded_at_birth. Initialize the map and seed existing codes with 0,
    # which is the conservative choice (treats them as 'born early', so they
    # never count toward 'recent' code creation in any saturation window).
    if (is.null(state$saturation$code_n_coded_at_birth)) {
      state$saturation$code_n_coded_at_birth <- setNames(
        as.list(rep(0L, length(state$saturation$code_birth_log))),
        names(state$saturation$code_birth_log)
      )
      log_info("Resumed state predates saturation fix; seeded {length(state$saturation$code_n_coded_at_birth)} existing codes with conservative birth-count of 0")
    }
  } else {
    config_hash <- if (!is.null(checkpoint)) checkpoint$config_hash else NULL
    state <- create_coding_state(learning_context, config_hash)
    # Mode 3 (Framework Applied): pre-populate the codebook with the
    # framework's constructs so the AI's enum-constrained outputs map
    # directly into existing codebook keys (no NEW: prefix path is
    # exercised). Per AC2, Mode 3's "codebook" IS the framework, fixed
    # at run start.
    if (!is.null(framework_spec)) {
      validate_class(framework_spec, "FrameworkSpec")
      for (c in framework_spec$constructs) {
        state$codebook[[c$id]] <- list(
          code_name      = c$name,
          description    = c$description,
          type           = "framework_construct",
          frequency      = 0L,
          entry_ids      = character(0),
          coded_segments = list()
        )
      }
      # The "anomaly" bucket captures non-fitting segments. Pre-populated
      # so the schema-enum dispatch always lands in an existing codebook
      # key (no novelty creation path).
      state$codebook[["anomaly"]] <- list(
        code_name      = "Anomaly (non-fitting)",
        description    = paste0(
          "Segments that resist the '", framework_spec$name, "' framework"
        ),
        type           = "anomaly",
        frequency      = 0L,
        entry_ids      = character(0),
        coded_segments = list()
      )
      log_info("Mode 3 codebook pre-populated with {length(framework_spec$constructs)} constructs + anomaly bucket")
    }
  }

  # Determine remaining entries
  remaining <- setdiff(seq_len(n), state$entries_processed)
  log_info("{length(remaining)} entries remaining to process")

  if (length(remaining) == 0) {
    log_info("All entries already processed. Nothing to do.")
    toc()
    return(state)
  }

  # Build the system prompt (stable across entries, only codebook summary changes)
  base_system_prompt <- .build_progressive_system_prompt(
    research_focus = research_focus,
    concepts = concepts,
    config = config,
    learning_context = learning_context
  )

  # Mode 3: framework prompt block is loop-invariant (depends only on
  # framework_spec, not on the per-entry state). Compute once before the
  # loop -- otherwise a 150-entry corpus pays 150 redundant validate_class()
  # + paste calls.
  framework_prompt_text <- if (!is.null(framework_spec)) {
    framework_prompt_block(framework_spec)
  } else NULL

  batch_delay <- provider$rate_limits$delay_between_batches %||% 0.5

  # Saturation tracking state
  prev_n_codes <- length(state$codebook)
  saturation_low_windows <- 0L

  # Use lists for O(1) append (avoid O(n^2) vector growth)
  new_processed <- list()
  new_skipped <- list()
  proc_idx <- 0L

  for (idx in remaining) {
    # Check if saturation was already reached (e.g., from a resumed state)
    if (isTRUE(state$saturation$reached) && isTRUE(config$saturation_enabled)) {
      log_info("Saturation already reached -- skipping remaining entries")
      break
    }

    entry_id <- as.character(data$std_id[idx])
    entry_text <- data$std_text[idx]

    if (is.na(entry_text) || nchar(trimws(entry_text)) < 10) {
      proc_idx <- proc_idx + 1L
      new_processed[[proc_idx]] <- idx
      new_skipped[[length(new_skipped) + 1L]] <- idx
      state$entry_results[[entry_id]] <- list(
        codes_assigned = character(0),
        coded_segments = list(),
        skipped = TRUE,
        skip_reason = "Text too short or missing"
      )
      if (!is.null(audit_log)) {
        log_ai_decision(audit_log, "coding", "entry_skipped",
                        entry_id = entry_id, reason = "Text too short or missing")
      }
      next
    }

    # Track codes before this entry (for new code detection)
    codes_before <- length(state$codebook)

    # Code this entry (with retry)
    state <- .code_entry_progressive(
      text = entry_text,
      entry_id = entry_id,
      entry_index = idx,
      state = state,
      provider = provider,
      config = config,
      base_system_prompt = base_system_prompt,
      audit_log = audit_log,
      response_cache = response_cache,
      fabrication_log = fabrication_log,
      framework_spec = framework_spec,
      framework_prompt_text = framework_prompt_text
    )

    # Track this entry in the O(1) lists
    proc_idx <- proc_idx + 1L
    new_processed[[proc_idx]] <- idx

    # Determine if this entry was skipped
    er <- state$entry_results[[entry_id]]
    was_skipped <- is.null(er) || isTRUE(er$skipped)
    if (was_skipped) {
      new_skipped[[length(new_skipped) + 1L]] <- idx
      if (!is.null(audit_log)) {
        log_ai_decision(audit_log, "coding", "entry_skipped",
                        entry_id = entry_id, reason = er$skip_reason %||% "no_relevant_content")
      }
    } else {
      # Phase 53 / C3: stream every (entry, code, segment) assignment to
      # the live tracker so a researcher can `tail -F` the artifact during
      # a long run. Done in the same loop as audit-log emission so the
      # two stay in lockstep. The is_new_code flag is computed against
      # codes_before; codebook_snapshot is rewritten at end-of-entry below.
      if (!is.null(audit_log) || !is.null(live_tracker)) {
        codes_before_set <- if (!is.null(live_tracker)) {
          # Snapshot of codebook keys BEFORE this entry, so we can flag
          # newly-created codes as is_new_code = TRUE in the live event log.
          if (codes_before == 0L) character(0L) else
            head(names(state$codebook), codes_before)
        } else NULL

        for (seg in er$coded_segments) {
          if (!is.null(audit_log)) {
            log_ai_decision(audit_log, "coding", "code_assignment",
                            entry_id = entry_id, code_name = seg$code_name,
                            code_key = seg$code_key)
          }
          if (!is.null(live_tracker)) {
            live_tracker <- live_record_assignment(
              tracker     = live_tracker,
              entry_id    = entry_id,
              code_key    = seg$code_key,
              code_name   = seg$code_name,
              segment     = seg,
              is_new_code = !(seg$code_key %in% codes_before_set),
              entry_index = idx
            )
          }
        }
      }
    }

    # Phase 53 / C3: rewrite codebook_live.json after every entry (cadence
    # configurable via init_live_tracker(codebook_snapshot_every = N); the
    # default 1 = after every entry).
    if (!is.null(live_tracker)) {
      live_tracker <- live_snapshot_codebook(
        tracker     = live_tracker,
        codebook    = state$codebook,
        entry_index = idx
      )
    }

    # Compute n_coded NOW (used by both code-birth tracking and saturation
    # detection below). Counted as: total processed entries (committed +
    # current accumulator) minus total skipped entries.
    # NOTE: re: future cleanup -- the entry-row-index stored in code_birth_log
    # is used by 21_longitudinal.R to look up data$.parsed_ts[earliest_entry_idx],
    # but that function operates on analytic_data (a subset of data), so the
    # index can be off when entries are skipped. That bug is independent of
    # this saturation fix; tracked separately.
    n_done <- length(state$entries_processed) + proc_idx
    n_skipped_total <- length(state$entries_skipped) + length(new_skipped)
    n_coded <- n_done - n_skipped_total

    # Track new codes created by this entry
    codes_after <- length(state$codebook)
    n_new_codes <- codes_after - codes_before
    if (n_new_codes > 0) {
      new_keys <- setdiff(names(state$codebook), names(state$saturation$code_birth_log))
      for (nk in new_keys) {
        state$saturation$code_birth_log[[nk]] <- idx
        state$saturation$code_n_coded_at_birth[[nk]] <- n_coded
      }
      if (!is.null(audit_log)) {
        for (nk in new_keys) {
          log_ai_decision(audit_log, "coding", "new_code_created",
                          entry_id = entry_id,
                          code_name = state$codebook[[nk]]$code_name,
                          code_key = nk)
        }
      }
    }

    # Rate limiting
    if (batch_delay > 0) Sys.sleep(batch_delay)

    if (proc_idx %% 50 == 0 || n_done == n) {
      log_info("  Progress: {n_done}/{n} entries ({round(100 * n_done / n)}%), ",
               "{length(state$codebook)} codes, {n_coded} coded, {n_skipped_total} skipped")
    }

    # Record saturation curve data point every 50 coded entries
    if (n_coded > 0 && n_coded %% 50 == 0) {
      window_size <- min(config$saturation_window, n_coded)

      # New-codes-in-window: count codes whose birth-time (measured as the
      # n_coded value when they were created) falls within the last
      # `window_size` coded entries. Direct lookup of the persistent
      # code_n_coded_at_birth map -- correct across checkpoint resets,
      # correct across resumes, O(|codebook|) per check.
      if (length(state$saturation$code_n_coded_at_birth) > 0) {
        births <- unlist(state$saturation$code_n_coded_at_birth, use.names = FALSE)
        new_in_window <- sum(births > (n_coded - window_size))
      } else {
        new_in_window <- 0L
      }

      # Compute Inductive Thematic Saturation (ITS) ratio per De Paoli & Mathis 2024
      # (Quality & Quantity, doi:10.1007/s11135-024-01950-6): unique codes / total
      # code assignments. Low ratio = high reuse density = stable codebook.
      total_assignments <- sum(vapply(state$codebook, function(cb) cb$frequency, integer(1)))
      slope_ratio <- if (total_assignments > 0) codes_after / total_assignments else 1.0

      state$saturation$curve <- rbind(state$saturation$curve, data.frame(
        entries_coded = n_coded,
        entries_processed = n_done,
        n_codes = codes_after,
        new_codes_in_window = as.integer(new_in_window),
        slope_ratio = round(slope_ratio, 4),
        timestamp = Sys.time()
      ))

      # --- Saturation detection (only after minimum entries) ---
      if (isTRUE(config$saturation_enabled) && n_coded >= config$min_coded_before_saturation) {

        # Signal 1: Code creation rate (Guest-style)
        signal_creation <- new_in_window <= config$saturation_threshold

        # Signal 2: Inductive Thematic Saturation ratio (De Paoli & Mathis 2024).
        # ITS = unique_codes / total_assignments. De Paoli notes the ratio "should
        # not be too close to 1" (every assignment a new code = unsaturated). We
        # use a stopping threshold of 0.05 (1 unique code per 20 assignments),
        # stricter than De Paoli's illustrative observation of 0.28 because we
        # use the ratio as a stopping criterion, not a single-timepoint observation.
        signal_slope <- slope_ratio < 0.05

        # Signal 3: AI self-assessment (every N coded entries)
        signal_ai <- FALSE
        if (n_coded %% config$ai_assessment_interval == 0) {
          signal_ai <- .ai_saturation_check(state, provider, research_focus,
                                              audit_log = audit_log,
                                              response_cache = response_cache)
          if (signal_ai) {
            log_info("  AI self-assessment: no novel patterns detected")
          }
        }

        # Count active signals
        n_signals <- sum(c(signal_creation, signal_slope, signal_ai))

        if (signal_creation) {
          saturation_low_windows <- saturation_low_windows + 1L
        } else {
          saturation_low_windows <- 0L
        }

        # Triangulated stopping: 2+ signals OR sustained low creation rate
        if (n_signals >= 2 || saturation_low_windows >= config$saturation_confirmations) {
          state$saturation$reached <- TRUE
          state$saturation$reached_at_entry <- n_done
          state$saturation$reached_at_coded <- n_coded
          state$saturation$total_entries_at_saturation <- n
          state$saturation$signals$code_creation_rate <- signal_creation
          state$saturation$signals$slope_ratio <- signal_slope
          state$saturation$signals$ai_self_assessment <- signal_ai
          state$saturation$saturation_ratio <- round(codes_after / n_coded, 4)

          triggered_by <- paste(
            c(if (signal_creation) "code_creation_rate" else NULL,
              if (signal_slope) "slope_ratio" else NULL,
              if (signal_ai) "ai_self_assessment" else NULL,
              if (saturation_low_windows >= config$saturation_confirmations) "sustained_low_creation" else NULL),
            collapse = " + "
          )

          log_info("*** THEMATIC SATURATION REACHED ***")
          log_info("  At entry {n_done}/{n} ({n_coded} coded)")
          log_info("  Codebook: {codes_after} codes")
          log_info("  Triggered by: {triggered_by}")
          log_info("  Saturation ratio: {state$saturation$saturation_ratio}")
          log_info("  Remaining {n - n_done} entries will not be processed")
          break
        }
      }
    }

    # Periodic checkpoint -- merge lists into state vectors first
    if (!is.null(checkpoint) && proc_idx %% config$checkpoint_interval == 0) {
      state$entries_processed <- c(state$entries_processed, as.integer(unlist(new_processed)))
      state$entries_skipped <- c(state$entries_skipped, as.integer(unlist(new_skipped)))
      new_processed <- list()
      new_skipped <- list()
      proc_idx <- 0L
      state$last_updated <- Sys.time()
      save_partial_checkpoint(checkpoint, "progressive_coding", state, n_done)
    }
  }

  # Merge remaining accumulated lists into state vectors
  if (proc_idx > 0) {
    state$entries_processed <- c(state$entries_processed, as.integer(unlist(new_processed[seq_len(proc_idx)])))
    state$entries_skipped <- c(state$entries_skipped, as.integer(unlist(new_skipped)))
  }

  state$last_updated <- Sys.time()
  toc()

  n_total_processed <- length(state$entries_processed)
  n_total_skipped <- length(state$entries_skipped)
  n_coded <- n_total_processed - n_total_skipped
  log_info("Progressive coding complete:")
  log_info("  Entries processed: {n_total_processed}")
  log_info("  Entries coded:     {n_coded}")
  log_info("  Entries skipped:   {n_total_skipped}")
  log_info("  Total codes:       {length(state$codebook)}")
  if (isTRUE(state$saturation$reached)) {
    log_info("  Saturation:        reached at entry {state$saturation$reached_at_entry} ({state$saturation$reached_at_coded} coded)")
  } else {
    log_info("  Saturation:        not reached (all entries processed)")
  }

  # Phase 53 / C3: force a final codebook_live.json snapshot regardless of
  # cadence so the on-disk state always reflects post-coding reality.
  if (!is.null(live_tracker)) {
    live_snapshot_codebook(live_tracker, state$codebook, force = TRUE)
  }

  state
}

# ==============================================================================
# Per-entry coding
# ==============================================================================

#' @keywords internal
.code_entry_progressive <- function(text, entry_id, entry_index, state,
                                     provider, config, base_system_prompt,
                                     audit_log = NULL,
                                     response_cache = NULL,
                                     fabrication_log = NULL,
                                     framework_spec = NULL,
                                     framework_prompt_text = NULL) {
  # Three coding paths, dispatched by mode + provider:
  #   1. FRAMEWORK (Mode 3): framework_spec is non-NULL -> AI applies
  #      researcher's a-priori constructs verbatim; "anomaly" code captures
  #      content that doesn't fit. No NEW codes allowed; codebook is
  #      pre-populated with the framework's constructs at pipeline init.
  #   2. CITATIONS (Mode 2 + Anthropic provider): T0.1 PREVENTION layer.
  #      Model returns server-side-guaranteed offsets into source.
  #   3. SCHEMA (Mode 2 + OpenAI, or Mode 1 fallback): existing forced
  #      tool_use with free-form codes.
  # All three paths feed into the same per-segment processor for
  # verification + codebook update -- per AC8 (modes are configurations
  # of one architecture, never separate code paths).
  use_framework <- !is.null(framework_spec)
  use_citations <- (!use_framework) && .use_citations_for_provider(provider, config)

  # System prompt: methodology framing + codebook context. In Mode 3,
  # codebook context is replaced with the framework prompt block (the
  # only allowed code names). In Mode 2, the AI sees the growing
  # codebook so it can reuse codes.
  if (use_framework) {
    # framework_prompt_text is hoisted out of the loop in run_progressive_coding;
    # fall back to recomputing only for direct callers (e.g., tests) that
    # didn't pre-compute.
    fpt <- framework_prompt_text %||% framework_prompt_block(framework_spec)
    system_prompt <- paste0(
      base_system_prompt, "\n\n",
      fpt
    )
  } else {
    codebook_summary <- .build_codebook_summary(state, max_codes = 80,
                                                  recent_window = 20)
    system_prompt <- paste0(
      base_system_prompt,
      "\n## YOUR CURRENT CODEBOOK\n",
      if (nchar(codebook_summary) > 0) {
        paste0("You have created these codes so far. Use them when applicable, ",
               "or create NEW codes when the text discusses something not yet captured:\n\n",
               codebook_summary)
      } else {
        "This is the FIRST entry. Create new codes as needed.\n"
      }
    )
  }

  # Phase 50f: auto-context-aware per-entry truncation. Was previously
  # hardcoded at .MAX_ENTRY_CHARS = 8000L, which used only ~1.5% of
  # gpt-4o's 128K-token context; long-form entries (interviews,
  # essays, multi-paragraph Reddit posts) lost everything past
  # character 8001 -- biasing the codebook toward early-narrative
  # content. Effective cap: scale to ~40% of the model's context
  # window in chars (assuming ~4 chars/token), reserving the rest
  # for system prompt + codebook + completion. Override via
  # config$ai$max_entry_chars if user wants explicit control.
  effective_max_chars <- .effective_max_entry_chars(provider, config)
  truncated_text <- substr(text, 1, effective_max_chars)
  if (nchar(text) > effective_max_chars) {
    log_debug("Entry {entry_id}: text truncated from {nchar(text)} to {effective_max_chars} chars")
  }

  # Path-specific user prompt + ai_complete kwargs.
  if (use_framework) {
    user_prompt <- .build_progressive_framework_user_prompt(truncated_text,
                                                              framework_spec)
    ai_kwargs <- list(
      json_mode       = FALSE,
      response_schema = .coding_schema_framework(framework_spec$construct_ids),
      documents       = NULL
    )
  } else if (use_citations) {
    # Citations path: entry travels as a document with citations enabled;
    # the prompt instructs JSON output without offsets (Anthropic returns
    # them via the citations array). No response_schema (incompatible with
    # citations: forced tool_use produces no text blocks for citations to
    # attach to).
    user_prompt <- .build_progressive_citations_user_prompt()
    ai_kwargs <- list(
      json_mode       = TRUE,
      response_schema = NULL,
      documents       = list(list(id = entry_id,
                                   text = truncated_text,
                                   type = "data_entry"))
    )
  } else {
    # Schema path: existing forced-tool_use flow with strict JSON schema.
    user_prompt <- .build_progressive_schema_user_prompt(truncated_text)
    ai_kwargs <- list(
      json_mode       = FALSE,
      response_schema = .coding_schema(),
      documents       = NULL
    )
  }

  result <- NULL
  # Capture model + request_id + citations at outer scope so the per-segment
  # T0.1 wiring can attribute each QuoteProvenance to the AI call. Env-backed
  # because <<- from inside tryCatch's expr block walks past the function's
  # local frame and writes to globalenv instead (subtle R quirk).
  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model     <- NA_character_
  ai_meta$call_id   <- NA_character_
  ai_meta$citations <- list()
  retries <- config$max_retries_per_entry %||% 1L

  for (attempt in seq_len(retries + 1)) {
    result <- tryCatch({
      ai_result <- ai_complete(provider, user_prompt, system_prompt,
                                task            = "coding",
                                json_mode       = ai_kwargs$json_mode,
                                response_schema = ai_kwargs$response_schema,
                                documents       = ai_kwargs$documents)
      ai_meta$model     <- ai_result$model      %||% NA_character_
      ai_meta$call_id   <- ai_result$request_id %||% NA_character_
      ai_meta$citations <- ai_result$citations  %||% list()
      if (!is.null(audit_log)) {
        log_ai_request(audit_log, "coding", ai_result, response_cache,
                        entry_id = entry_id, attempt = attempt)
      }
      response <- ai_result$content

      parsed <- parse_json_safely(response)

      if (is.null(parsed)) {
        log_warn("Entry {entry_id}: JSON parse failed. Response snippet: {substr(response, 1, 200)}")
      }

      parsed
    }, error = function(e) {
      if (attempt <= retries) {
        log_debug("Entry {entry_id}: attempt {attempt} failed ({e$message}), retrying...")
        Sys.sleep(1)
      } else {
        log_warn("Entry {entry_id}: all attempts failed ({e$message})")
      }
      NULL
    })

    if (!is.null(result)) break
  }

  # Process result
  if (is.null(result) || isTRUE(result$skipped)) {
    skip_reason <- if (!is.null(result) && isTRUE(result$skipped)) {
      result$skip_reason %||% "No applicable content"
    } else {
      "AI response parse failure"
    }
    state$entry_results[[entry_id]] <- list(
      codes_assigned = character(0),
      coded_segments = list(),
      skipped = TRUE,
      skip_reason = skip_reason
    )
    return(state)
  }

  segments_raw <- result$coded_segments
  if (is.null(segments_raw) || length(segments_raw) == 0) {
    state$entry_results[[entry_id]] <- list(
      codes_assigned = character(0),
      coded_segments = list(),
      skipped = TRUE,
      skip_reason = "No coded segments returned"
    )
    return(state)
  }
  segments <- .normalize_segments(segments_raw)

  # Per-iteration accumulators -- mutate via env reference so the per-segment
  # helper can update them without returning multiple values.
  acc <- new.env(parent = emptyenv())
  acc$entry_codes    <- character(0)
  acc$entry_segments <- list()

  for (i in seq_along(segments)) {
    seg <- segments[[i]]
    state <- .handle_one_coded_segment(
      seg              = seg,
      seg_index        = i,
      use_citations    = use_citations,
      use_framework    = use_framework,
      framework_spec   = framework_spec,
      ai_meta          = ai_meta,
      documents        = ai_kwargs$documents,
      text             = text,
      entry_id         = entry_id,
      state            = state,
      acc              = acc,
      audit_log        = audit_log,
      fabrication_log  = fabrication_log
    )
  }

  state$entry_results[[entry_id]] <- list(
    codes_assigned = acc$entry_codes,
    coded_segments = acc$entry_segments,
    skipped        = FALSE,
    skip_reason    = NA_character_
  )

  state
}

# ==============================================================================
# Per-segment processing (shared between schema and citations paths)
# ==============================================================================

#' Decide whether to use the Anthropic Citations API path for this provider
#'
#' Returns TRUE for Anthropic providers; FALSE otherwise. Future Sprint-4
#' phases may add a config opt-out (\code{config$data_integrity$use_citations_api}),
#' but the default-on-for-Anthropic stance is load-bearing -- the Citations
#' API is the package's primary anti-fabrication PREVENTION layer (T0.1
#' part 3b) and disabling it weakens the architectural commitment to AC1
#' (AI is scaffold by architecture, not by configuration).
#' @keywords internal
.use_citations_for_provider <- function(provider, config) {
  isTRUE(provider$provider == "anthropic")
}

#' Normalize the AI's coded_segments payload into a uniform list-of-lists
#'
#' jsonlite may return a data.frame (when all segments share the same fields)
#' or a single named list (when only one segment). Downstream code expects
#' a list of named lists, so this helper coerces both shapes.
#' @keywords internal
.normalize_segments <- function(segments_raw) {
  if (is.data.frame(segments_raw)) {
    return(lapply(seq_len(nrow(segments_raw)),
                  function(i) as.list(segments_raw[i, ])))
  }
  if (is.list(segments_raw) && !is.null(names(segments_raw)) &&
      "text" %in% names(segments_raw)) {
    return(list(segments_raw))
  }
  segments_raw
}

#' Build the schema-path user prompt (existing T1.2 flow)
#' @keywords internal
.build_progressive_schema_user_prompt <- function(truncated_text) {
  safe_text <- if (requireNamespace("jsonlite", quietly = TRUE)) {
    raw_json <- jsonlite::toJSON(truncated_text, auto_unbox = TRUE)
    substr(raw_json, 2, nchar(raw_json) - 1)
  } else {
    gsub('(["\\\\\n\r\t])', '\\\\\\1', truncated_text)
  }
  paste0(
    "As you read through this entry, code any text segments applicable to the research question.\n\n",
    'Entry text: "', safe_text, '"'
  )
}

#' Build the Mode 3 (framework-applied) user prompt
#'
#' The framework's constructs are listed in the system prompt (via
#' framework_prompt_block). The user prompt presents the entry text and
#' instructs the AI to apply the framework constructs verbatim, flagging
#' segments that resist the framework as \code{construct_id = "anomaly"}.
#' @keywords internal
.build_progressive_framework_user_prompt <- function(truncated_text,
                                                       framework_spec) {
  safe_text <- if (requireNamespace("jsonlite", quietly = TRUE)) {
    raw_json <- jsonlite::toJSON(truncated_text, auto_unbox = TRUE)
    substr(raw_json, 2, nchar(raw_json) - 1)
  } else {
    gsub('(["\\\\\n\r\t])', '\\\\\\1', truncated_text)
  }
  paste0(
    "Apply the framework's constructs to any text segments in this entry that fit them. ",
    "For each applicable segment:\n",
    "- Set `construct_id` to one of the framework's construct ids (listed in the system prompt above).\n",
    "- Set `anomaly_reason` to \"\" (the empty string) for normal construct applications.\n",
    "If a segment resists the framework (no construct fits), code it as ",
    "`construct_id: \"anomaly\"` and set `anomaly_reason` to a one-sentence ",
    "explanation of why the framework doesn't capture it. Do NOT force a fit; ",
    "the framework's anomaly_handling policy treats these as first-class output.\n\n",
    'Entry text: "', safe_text, '"'
  )
}

#' Build the citations-path user prompt (T0.1 part 3b)
#'
#' The model receives the entry as a document content block (passed
#' alongside this prompt by .anthropic_completion when documents is
#' set). The prompt instructs JSON-mode output where each segment's
#' \code{text} field is a verbatim quote from the document; the
#' Anthropic API attaches a citation to each verbatim quote, producing
#' server-side-guaranteed character offsets into the source. The model
#' is explicitly instructed NOT to invent quotes -- the QuoteProvenance
#' bridge cross-checks the model's claim against Anthropic's citation
#' span, and the verification ladder runs as defense in depth.
#' @keywords internal
.build_progressive_citations_user_prompt <- function() {
  paste0(
    "Read the document provided as context, then identify code-worthy segments.\n\n",
    "OUTPUT FORMAT (CRITICAL):\n",
    "- Output ONLY a single JSON object. No preamble, no explanation, no ",
    "commentary before or after the JSON.\n",
    "- Do not wrap the JSON in markdown code fences.\n",
    "- Your entire response must be parseable as JSON.\n\n",
    "JSON shape:\n",
    "{\n",
    '  "skipped": false,\n',
    '  "skip_reason": "",\n',
    '  "coded_segments": [\n',
    "    {\n",
    '      "text": "<verbatim quote from the document>",\n',
    '      "code": "<existing code name OR \\"NEW: <new code name>\\">",\n',
    '      "code_description": "<brief description, required for NEW codes>",\n',
    '      "code_type": "descriptive | emotional | process | in_vivo"\n',
    "    }\n",
    "  ]\n",
    "}\n\n",
    "If nothing in the document is applicable, return ",
    '{"skipped": true, "skip_reason": "<reason>", "coded_segments": []}.\n\n',
    "CRITICAL ANTI-FABRICATION RULES:\n",
    "- The `text` field MUST be a verbatim slice of the document, character-for-character.\n",
    "- Do NOT paraphrase. Do NOT invent quotes. Do NOT combine fragments.\n",
    "- If you cannot find a verbatim slice that supports the code, do not include the segment.\n",
    "- Each verbatim quote will be cross-checked against the source document; ",
    "non-verbatim segments are dropped from the analysis."
  )
}

#' Process one coded segment from either the schema or citations path
#'
#' Builds a path-appropriate \code{QuoteProvenance} (free-form via
#' \code{make_quote} or citation-bridged via \code{make_quote_from_citation}),
#' runs the verification ladder, and -- if not fabricated -- updates the
#' codebook and accumulators.
#'
#' Mutates \code{acc$entry_codes} and \code{acc$entry_segments} via
#' environment reference so the caller doesn't have to thread them
#' through return values.
#'
#' @return Updated \code{state} (immutable per-call; the codebook is
#'   updated when a non-fabricated segment is processed).
#' @keywords internal
.handle_one_coded_segment <- function(seg, seg_index, use_citations,
                                       use_framework = FALSE,
                                       framework_spec = NULL,
                                       ai_meta,
                                       documents, text, entry_id, state, acc,
                                       audit_log, fabrication_log) {
  seg_text  <- as.character(seg$text             %||% "")[1]
  # Mode 3 (framework) returns a `construct_id` field; Mode 2 returns
  # `code` (with optional NEW: prefix). Map both to a uniform code_key.
  if (isTRUE(use_framework)) {
    construct_id   <- as.character(seg$construct_id %||% "")[1]
    anomaly_reason <- as.character(seg$anomaly_reason %||% "")[1]
    seg_code <- construct_id
    seg_desc <- if (identical(construct_id, "anomaly")) {
      paste0("Anomaly: ", anomaly_reason)
    } else {
      # Use the framework's construct description for new construct entries
      idx <- match(construct_id, framework_spec$construct_ids)
      if (!is.na(idx)) framework_spec$constructs[[idx]]$description else ""
    }
    seg_type <- "framework_construct"
  } else {
    seg_code <- as.character(seg$code             %||% "")[1]
    seg_desc <- as.character(seg$code_description %||% "")[1]
    seg_type <- as.character(seg$code_type        %||% "descriptive")[1]
  }
  # In the schema path the model returns offsets; in the citations path
  # it doesn't (offsets come from Anthropic's citation array).
  seg_start <- suppressWarnings(as.integer(seg$start_char %||% NA_integer_)[1])
  seg_end   <- suppressWarnings(as.integer(seg$end_char   %||% NA_integer_)[1])

  if (nchar(seg_code) == 0 || nchar(seg_text) < 3) return(state)

  # Determine code_key. Mode 3 uses construct_id verbatim (no NEW: prefix,
  # no novelty creation -- the framework is fixed). Mode 2 parses NEW:.
  if (isTRUE(use_framework)) {
    is_new    <- !(seg_code %in% names(state$codebook))
    code_name <- if (identical(seg_code, "anomaly")) {
      "Anomaly (non-fitting)"
    } else {
      idx <- match(seg_code, framework_spec$construct_ids)
      if (!is.na(idx)) framework_spec$constructs[[idx]]$name else seg_code
    }
    code_key  <- seg_code
  } else {
    is_new <- grepl("^NEW:", seg_code, ignore.case = TRUE)
    if (is_new) {
      code_name <- trimws(sub("^NEW:\\s*", "", seg_code, ignore.case = TRUE))
    } else {
      code_name <- trimws(seg_code)
    }
    code_key <- tolower(code_name)
    # The AI sees the full codebook and decides whether to create a new code or
    # use an existing one. Only EXACT key matches collapse to existing -- no
    # substring/fuzzy matching; the AI's judgment is the authority on novelty.
    if (is_new && code_key %in% names(state$codebook)) {
      is_new <- FALSE
    }
  }

  # Build the (unverified) QuoteProvenance per path:
  # - Citations path: pair this segment with the corresponding citation by
  #   emission order, validated by string match. If pairing succeeds the
  #   quote carries citation_source = "anthropic_citations_api"; otherwise
  #   we fall back to model_freeform (the model returned a `text` claim
  #   without an attached citation, so we treat it like the schema path).
  # - Schema path: classic make_quote with model-supplied offsets.
  quote_partial <- if (isTRUE(use_citations)) {
    .build_quote_from_citations_path(
      seg_text = seg_text, seg_index = seg_index,
      citations = ai_meta$citations, documents = documents,
      text = text, entry_id = entry_id, code_key = code_key,
      ai_meta = ai_meta
    )
  } else {
    .build_quote_from_schema_path(
      seg_text = seg_text, seg_start = seg_start, seg_end = seg_end,
      text = text, entry_id = entry_id, code_key = code_key,
      ai_meta = ai_meta
    )
  }

  quote <- verify_quote(quote_partial, text)

  if (identical(quote$verification_status, "fabricated")) {
    # Anti-fabrication enforcement: drop the segment, log to the fabrication
    # CSV for the methodology paper KPI, and emit a quote_fabricated audit
    # decision so cross-run analysis can attribute fabrications to specific
    # ai_call_ids.
    log_fabrication(fabrication_log, quote)
    if (!is.null(audit_log)) {
      log_ai_decision(audit_log, "quote_verification", "quote_fabricated",
                      entry_id  = entry_id, code_name = code_name,
                      quote_id  = quote$quote_id,
                      ai_call_id = quote$ai_call_id %||% NA_character_,
                      exact_text = substr(seg_text, 1, 200))
    }
    log_warn("Entry {entry_id}: AI returned fabricated quote for code '{code_name}'; segment dropped.")
    return(state)
  }

  if (identical(quote$verification_status, "drifted")) {
    # Source corpus changed between attribution and verification time
    # (source_text_sha256 mismatch AND ladder failed). Per spec the quote
    # is excluded from rendering pending researcher review; we log it to
    # the audit trail so cross-run analysis can attribute drifts.
    if (!is.null(audit_log)) {
      log_ai_decision(audit_log, "quote_verification", "quote_drifted",
                      entry_id  = entry_id, code_name = code_name,
                      quote_id  = quote$quote_id,
                      ai_call_id = quote$ai_call_id %||% NA_character_,
                      exact_text = substr(seg_text, 1, 200))
    }
    log_warn("Entry {entry_id}: quote drifted (source SHA mismatch) for code '{code_name}'; segment dropped pending review.")
    return(state)
  }

  # Verified -- attach provenance to the segment record so downstream
  # rendering can show verification status, and the methodology paper can
  # compute per-run fabrication rates from the codebook.
  seg_record <- list(
    entry_id   = entry_id,
    text       = seg_text,
    start_char = seg_start,
    end_char   = seg_end,
    provenance = quote
  )

  if (is_new || !(code_key %in% names(state$codebook))) {
    state$codebook[[code_key]] <- list(
      code_name      = code_name,
      description    = seg_desc,
      type           = seg_type,
      frequency      = 1L,
      entry_ids      = entry_id,
      coded_segments = list(seg_record)
    )
  } else {
    cb_entry <- state$codebook[[code_key]]
    cb_entry$frequency <- cb_entry$frequency + 1L
    cb_entry$entry_ids <- unique(c(cb_entry$entry_ids, entry_id))
    cb_entry$coded_segments[[length(cb_entry$coded_segments) + 1L]] <- seg_record
    state$codebook[[code_key]] <- cb_entry
  }

  acc$entry_codes <- unique(c(acc$entry_codes, code_key))
  acc$entry_segments[[length(acc$entry_segments) + 1L]] <- list(
    code_key   = code_key,
    code_name  = code_name,
    text       = seg_text,
    start_char = seg_start,
    end_char   = seg_end,
    provenance = quote
  )

  state
}

#' Build a QuoteProvenance for the schema path (offsets from the model)
#' @keywords internal
.build_quote_from_schema_path <- function(seg_text, seg_start, seg_end,
                                           text, entry_id, code_key, ai_meta) {
  # Build offsets defensively -- the AI sometimes returns NA / out-of-range
  # values. The substring search step in verify_quote recovers from
  # imprecise offsets when the text is genuinely in the source.
  sc <- if (is.na(seg_start) || seg_start < 0L) 0L else seg_start
  ec <- if (is.na(seg_end) || seg_end <= sc) sc + nchar(seg_text) else seg_end
  make_quote(
    source_doc_id      = entry_id,
    source_doc_type    = "data_entry",
    source_text        = text,
    start_char         = sc,
    end_char           = ec,
    exact_text         = seg_text,
    attributed_code_id = code_key,
    ai_model           = ai_meta$model,
    ai_call_id         = ai_meta$call_id,
    citation_source    = "model_freeform"
  )
}

#' Build a QuoteProvenance for the citations path (offsets from Anthropic)
#'
#' Pairs the model's segment with the corresponding citation by:
#' \enumerate{
#'   \item Emission-order match (\code{citations[[seg_index]]} -- the
#'         most common success case when the model emits one citation
#'         per segment).
#'   \item Cited-text string match (handles cases where the model emits
#'         citations in a different order than segments, or extra commentary
#'         citations interleave with the JSON).
#'   \item Fallback to the schema path's freeform constructor, leaving the
#'         verification ladder to recover offsets via substring search.
#'         citation_source becomes \code{"model_freeform"} so the dashboard
#'         distinguishes citation-API-grounded quotes from those that fell
#'         back.
#' }
#' @keywords internal
.build_quote_from_citations_path <- function(seg_text, seg_index, citations,
                                              documents, text, entry_id,
                                              code_key, ai_meta) {
  if (length(citations) == 0L) {
    return(.build_quote_from_schema_path(seg_text, NA_integer_, NA_integer_,
                                          text, entry_id, code_key, ai_meta))
  }

  matched <- NULL

  # Emission-order pairing first
  if (seg_index <= length(citations)) {
    cand <- citations[[seg_index]]
    if (.citation_text_matches(cand, seg_text)) {
      matched <- cand
    }
  }

  # String-match fallback: scan all citations for one whose cited_text
  # equals the segment's claimed text
  if (is.null(matched)) {
    for (c in citations) {
      if (.citation_text_matches(c, seg_text)) {
        matched <- c
        break
      }
    }
  }

  if (is.null(matched)) {
    # No citation pairs cleanly with this segment -- model may have emitted
    # the JSON without proper citation interleaving, or the cited_text
    # differs slightly. Fall back to schema-path constructor; the
    # verification ladder will run normally.
    return(.build_quote_from_schema_path(seg_text, NA_integer_, NA_integer_,
                                          text, entry_id, code_key, ai_meta))
  }

  # Successful pairing: build via the citations bridge with the document type
  # carried through (so source_doc_type stays meaningful for QDPX export and
  # report rendering). The bridge stores source_text_sha256 over the source
  # text we pass it; later verify_quote re-hashes the FULL entry text and
  # compares. To keep these in sync (and avoid spurious "drifted" status on
  # long entries where the prompt was truncated to .MAX_ENTRY_CHARS but the
  # verifier sees the full text), we substitute the full text into the
  # documents copy passed to the bridge. Anthropic's citation indices are
  # computed against the truncated prompt text, but since the truncation is
  # a strict prefix of the full text, indices remain valid pointers.
  docs_for_provenance <- documents
  if (length(docs_for_provenance) >= 1L) {
    docs_for_provenance[[1L]]$text <- text
  }
  make_quote_from_citation(
    citation            = matched,
    documents           = docs_for_provenance,
    attributed_code_id  = code_key,
    ai_model            = ai_meta$model,
    ai_call_id          = ai_meta$call_id,
    source_doc_type_default = "data_entry"
  )
}

#' Check whether a citation's cited_text equals a segment's claimed text
#'
#' Uses normalized comparison (whitespace + smart quotes + case) so trivial
#' formatting differences in the model's JSON encoding don't cause spurious
#' fallback to model_freeform. The verification ladder will further verify
#' the byte identity once the QuoteProvenance is built.
#' @keywords internal
.citation_text_matches <- function(citation, seg_text) {
  cited <- citation$cited_text %||% ""
  identical(.normalize_quote_text(cited),
            .normalize_quote_text(seg_text))
}

# ==============================================================================
# Prompt construction
# ==============================================================================

#' @keywords internal
.build_progressive_system_prompt <- function(research_focus, concepts, config,
                                              learning_context) {
  concept_str <- if (!is.null(concepts) && length(concepts) > 0) {
    paste(concepts, collapse = ", ")
  } else {
    research_focus
  }

  prompt <- paste0(
    "You are an expert qualitative researcher performing progressive coding.\n\n",
    "Research focus: ", research_focus, "\n"
  )

  if (!is.null(concepts) && length(concepts) > 0) {
    prompt <- paste0(prompt, "Core concepts: ", concept_str, "\n")
  }

  # Inject reflexivity block (positionality, paradigm, reflexive notes)
  reflexivity <- config$reflexivity_block %||% ""
  if (nchar(reflexivity) > 0) {
    prompt <- paste0(prompt, reflexivity)
  }

  prompt <- paste0(prompt,
    "\n## YOUR TASK\n",
    "As you read through the entry text, code any portions applicable to the ",
    "research question above. For each applicable segment you encounter:\n",
    "1. Extract the EXACT text segment (verbatim substring)\n",
    "2. Either assign an EXISTING code from your codebook, OR create a NEW code\n",
    "If you reach the end of the entry having coded nothing, return skipped=true.\n\n",

    "## CODING GUIDELINES\n",
    "- Code SPECIFIC TEXT SEGMENTS as you encounter them, not the entire entry\n",
    .build_segment_length_guideline(learning_context, config),
    .build_code_length_guideline(learning_context, config),
    "- Capture relationships between concepts when present -- codes should reflect ",
    "HOW concepts interact, not just list individual concepts\n",
    "- The SAME text segment may carry multiple layers of meaning. If a passage is ",
    "relevant to more than one code, produce MULTIPLE coded_segments entries with ",
    "the same (or overlapping) text but different codes. Overlapping codes on the ",
    "same text are expected and methodologically appropriate (Braun & Clarke, 2006)\n",
    "- Use an existing code when the text discusses the same concept; create a new code ",
    "only when the text discusses something genuinely not captured by any existing code\n",
    "- You may also apply an existing code AND create a new code for the same text ",
    "if it captures a distinct dimension not covered by the existing code\n",
    "- It is OK to skip an entry entirely if nothing is applicable\n",
    "- It is OK to code only part of an entry -- skip irrelevant portions\n",
    "- Prefer specificity over generality in code names\n"
  )

  if (isTRUE(config$include_in_vivo)) {
    prompt <- paste0(prompt,
      "- Include 'in vivo' codes using the participant's own words when impactful\n")
  }

  # Learning context: codebook examples, style, discards
  if (!is.null(learning_context)) {
    if (nchar(learning_context$for_coding_style %||% "") > 0) {
      prompt <- paste0(prompt, "\n## CODING STYLE (from previous analyses)\n",
                        learning_context$for_coding_style, "\n")
    }
    if (nchar(learning_context$for_coding_discards %||% "") > 0) {
      prompt <- paste0(prompt, "\n## CODES TO AVOID (from previous analyses)\n",
                        learning_context$for_coding_discards, "\n")
    }
    if (nchar(learning_context$for_coding_examples %||% "") > 0) {
      prompt <- paste0(prompt, "\n", learning_context$for_coding_examples, "\n")
    }
    if (nchar(learning_context$for_coding_calibration %||% "") > 0) {
      prompt <- paste0(prompt, "\n", learning_context$for_coding_calibration, "\n")
    }
  }

  prompt <- paste0(prompt,
    "\n## RESPONSE GUIDANCE\n",
    "If the entry has applicable content: set skipped = false, skip_reason = ",
    "\"\", and provide one entry in coded_segments per coded passage. For each ",
    "segment: text is the exact verbatim substring from the entry (DO NOT ",
    "paraphrase); start_char/end_char are character offsets; code is either an ",
    "existing code name or \"NEW: <name>\" for a novel code; code_description ",
    "is required for NEW codes (use empty string \"\" when reusing an existing ",
    "code); code_type is one of descriptive, emotional, process, or in_vivo.\n\n",
    "If nothing in the entry is applicable: set skipped = true, skip_reason ",
    "to a brief explanation, and coded_segments to an empty array [].\n\n",
    "The response shape is enforced by the structured-output schema; you ",
    "must always provide all three top-level fields (skipped, skip_reason, ",
    "coded_segments) even when one is empty."
  )

  prompt
}

# ==============================================================================
# Dynamic coding guideline helpers
# ==============================================================================

#' Build segment length guideline from benchmarks, config, or defaults
#'
#' Priority: (1) empirical benchmarks from prior analyses (averaged across
#' codebooks), (2) user-configured values, (3) package defaults.
#'
#' @param learning_context LearningContext (or NULL)
#' @param config Coding config section
#' @return Character string for the prompt
#' @keywords internal
.build_segment_length_guideline <- function(learning_context, config) {
  # Priority 1: Empirical benchmarks from prior analyses
  if (!is.null(learning_context) && !is.null(learning_context$benchmarks)) {
    avg_len <- learning_context$benchmarks$avg_segment_length
    if (!is.null(avg_len) && !is.na(avg_len) && avg_len > 0) {
      # Build a range around the empirical average
      low <- max(10L, round(avg_len * 0.5))
      high <- round(avg_len * 1.5)
      return(paste0("- Each coded segment should be a meaningful excerpt (based on ",
                     "prior analyses: typically ~", avg_len, " characters, ",
                     "range ", low, "-", high, ")\n"))
    }
  }

  # Priority 2: User-configured values
  seg_min <- config$segment_length_min
  seg_max <- config$segment_length_max
  if (!is.null(seg_min) && !is.null(seg_max)) {
    return(paste0("- Each coded segment should be a meaningful excerpt (typically ",
                   seg_min, "-", seg_max, " characters)\n"))
  }

  # Priority 3: Package default
  paste0("- Each coded segment should be a meaningful excerpt (typically 30-200 characters)\n")
}

#' Build code label length guideline from benchmarks, config, or defaults
#' @param learning_context LearningContext (or NULL)
#' @param config Coding config section
#' @return Character string for the prompt
#' @keywords internal
.build_code_length_guideline <- function(learning_context, config) {
  # Priority 1: Empirical benchmarks from prior analyses
  if (!is.null(learning_context) && !is.null(learning_context$benchmarks)) {
    avg_words <- learning_context$benchmarks$code_word_count
    if (!is.null(avg_words) && !is.na(avg_words) && avg_words > 0) {
      low <- max(2L, round(avg_words - 1))
      high <- round(avg_words + 2)
      return(paste0("- Codes should be concise but specific (based on prior analyses: ",
                     "typically ~", round(avg_words, 1), " words, range ", low, "-", high, ")\n"))
    }
  }

  # Priority 2: User-configured values
  code_min <- config$code_label_min_words
  code_max <- config$code_label_max_words
  if (!is.null(code_min) && !is.null(code_max)) {
    return(paste0("- Codes should be concise but specific (", code_min, "-", code_max, " words)\n"))
  }

  # Priority 3: Package default
  paste0("- Codes should be concise but specific (3-8 words)\n")
}

# ==============================================================================
# Codebook summary for prompt injection
# ==============================================================================

#' @keywords internal
.build_codebook_summary <- function(state, max_codes = 80, recent_window = 20) {
  cb <- state$codebook
  if (length(cb) == 0) return("")

  # Prioritize high-frequency codes + recently created codes
  code_data <- lapply(names(cb), function(key) {
    list(
      key = key,
      name = cb[[key]]$code_name,
      freq = cb[[key]]$frequency,
      desc = cb[[key]]$description %||% "",
      type = cb[[key]]$type %||% "descriptive"
    )
  })

  # Sort by frequency descending
  freqs <- vapply(code_data, function(x) x$freq, integer(1))
  sorted_idx <- order(-freqs)

  # Take top codes by frequency
  n_top <- max_codes - min(recent_window, length(code_data))
  top_idx <- sorted_idx[seq_len(min(n_top, length(sorted_idx)))]

  # Also include recently created codes (last N codes added)
  all_keys <- names(cb)
  recent_keys <- tail(all_keys, recent_window)
  recent_idx <- which(names(cb) %in% recent_keys)
  selected <- unique(c(top_idx, recent_idx))
  selected <- selected[seq_len(min(max_codes, length(selected)))]

  lines <- vapply(selected, function(i) {
    d <- code_data[[i]]
    desc_str <- if (!is.null(d$desc) && !is.na(d$desc) && nchar(d$desc) > 0) paste0(" -- ", substr(d$desc, 1, 80)) else ""
    sprintf("  %d. \"%s\" (freq=%d, type=%s)%s", i, d$name, d$freq, d$type, desc_str)
  }, character(1))

  paste(lines, collapse = "\n")
}

# ==============================================================================
# Saturation detection helpers
# ==============================================================================

#' AI self-assessment for saturation
#'
#' Asks the AI whether it has encountered novel patterns recently that
#' don't fit existing codes. Returns TRUE if the AI reports no novel patterns.
#'
#' @param state ProgressiveCodingState with current codebook
#' @param provider AIProvider object
#' @param research_focus Research focus string
#' @return Logical: TRUE if AI reports no novel patterns (saturation signal)
#' @keywords internal
.ai_saturation_check <- function(state, provider, research_focus,
                                  audit_log = NULL,
                                  response_cache = NULL) {
  n_codes <- length(state$codebook)
  if (n_codes == 0) return(FALSE)

  # Build a compact codebook summary
  code_names <- vapply(state$codebook, function(cb) cb$code_name, character(1))
  codebook_text <- paste(seq_along(code_names), ". ", code_names, sep = "", collapse = "\n")

  prompt <- paste0(
    "You have been coding entries about: ", research_focus, "\n\n",
    "Your current codebook has ", n_codes, " codes:\n",
    codebook_text, "\n\n",
    "Based on the variety of entries you have been processing, do you believe ",
    "there are significant patterns or topics related to the research focus that ",
    "are NOT yet captured by any existing code?\n\n",
    "Set novel_patterns_remaining = true if you believe more themes remain to ",
    "be discovered; false if the codebook is essentially complete. Provide a ",
    "brief reasoning string explaining your judgment."
  )

  result <- tryCatch({
    ai_result <- ai_complete(provider, prompt,
                              "You are assessing whether thematic saturation has been reached in a qualitative coding process.",
                              task = "saturation_check",
                              response_schema = .saturation_schema())
    if (!is.null(audit_log)) {
      log_ai_request(audit_log, "saturation", ai_result, response_cache,
                      n_codes = n_codes)
    }
    parse_json_safely(ai_result$content)
  }, error = function(e) {
    log_debug("AI saturation check failed: {e$message}")
    NULL
  })

  if (is.null(result)) return(FALSE)

  # Returns TRUE when AI says no novel patterns remain (saturation signal)
  !isTRUE(result$novel_patterns_remaining)
}

#' Generate saturation curve plot
#'
#' Creates a PNG plot showing cumulative codes vs coded entries,
#' with the saturation point marked if reached.
#'
#' @param state ProgressiveCodingState with saturation data
#' @param output_dir Directory to save the plot
#' @param methodology_mode Optional character (T1.7 / AC4): when supplied,
#'   adds a footer caption to the plot identifying the mode + run.
#' @param run_id Optional character: run identifier for the footer.
#' @return Path to the generated PNG, or NULL
#' @keywords internal
generate_saturation_plot <- function(state, output_dir,
                                       methodology_mode = NULL,
                                       run_id = NULL) {
  curve <- state$saturation$curve
  if (is.null(curve) || nrow(curve) == 0) return(NULL)

  plot_path <- file.path(output_dir, "saturation_curve.png")

  tryCatch({
    grDevices::png(plot_path, width = 900, height = 600, res = 120)
    graphics::par(mar = c(5, 5, 4, 5))

    # Main plot: codes vs coded entries
    graphics::plot(
      curve$entries_coded, curve$n_codes,
      type = "l", lwd = 2.5, col = "#2c3e50",
      xlab = "Entries Coded",
      ylab = "Cumulative Unique Codes",
      main = "Thematic Saturation Curve",
      las = 1, bty = "l"
    )

    # Add new-codes-per-window on secondary axis
    graphics::par(new = TRUE)
    graphics::plot(
      curve$entries_coded, curve$new_codes_in_window,
      type = "l", lwd = 1.5, col = "#e74c3c", lty = 2,
      axes = FALSE, xlab = "", ylab = ""
    )
    graphics::axis(side = 4, col = "#e74c3c", col.axis = "#e74c3c", las = 1)
    graphics::mtext("New Codes per Window", side = 4, line = 3, col = "#e74c3c")

    # Mark saturation point if reached
    if (isTRUE(state$saturation$reached)) {
      sat_coded <- state$saturation$reached_at_coded
      sat_codes <- curve$n_codes[which.min(abs(curve$entries_coded - sat_coded))]

      graphics::par(new = FALSE)
      graphics::points(sat_coded, sat_codes, pch = 19, col = "#e67e22", cex = 2)
      graphics::abline(v = sat_coded, col = "#e67e22", lty = 3, lwd = 1.5)
      graphics::text(
        sat_coded, sat_codes,
        labels = sprintf("Saturation\n(%d entries, %d codes)", sat_coded, sat_codes),
        pos = 4, col = "#e67e22", cex = 0.8, font = 2
      )
    }

    # Legend
    graphics::legend("right",
      legend = c("Cumulative codes", "New codes/window",
                  if (isTRUE(state$saturation$reached)) "Saturation point" else NULL),
      col = c("#2c3e50", "#e74c3c",
               if (isTRUE(state$saturation$reached)) "#e67e22" else NULL),
      lty = c(1, 2, NA),
      pch = c(NA, NA, if (isTRUE(state$saturation$reached)) 19 else NULL),
      lwd = c(2.5, 1.5, NA),
      cex = 0.75, bg = "white"
    )

    # T1.7 (AC4): methodology stamp footer. Base R plots use mtext for
    # captions -- ggplot's labs(caption=...) equivalent. Subtle gray and
    # outside the main plot area so the methodology is visible without
    # interfering with data interpretation.
    if (!is.null(methodology_mode)) {
      graphics::mtext(
        methodology_plot_caption(methodology_mode, run_id),
        side = 1, line = 3.5, cex = 0.7, col = "#7F8C8D", adj = 1
      )
    }

    grDevices::dev.off()
    log_info("Saturation curve saved: {plot_path}")
    plot_path
  }, error = function(e) {
    log_warn("Failed to generate saturation plot: {e$message}")
    tryCatch(grDevices::dev.off(), error = function(e2) NULL)
    NULL
  })
}

# ==============================================================================
# Analytic sample extraction
# ==============================================================================

#' Get the analytic sample (entries that received at least one code)
#'
#' @param state ProgressiveCodingState
#' @param data Full data tibble
#' @return Filtered tibble containing only coded entries
#' @export
get_analytic_sample <- function(state, data) {
  if (!inherits(state, "ProgressiveCodingState")) {
    stop("state must be a ProgressiveCodingState object")
  }

  coded_ids <- names(state$entry_results)[
    !vapply(state$entry_results, function(r) isTRUE(r$skipped), logical(1))
  ]

  filtered <- data[data$std_id %in% coded_ids, ]
  log_info("Analytic sample: {nrow(filtered)}/{nrow(data)} entries received codes")
  filtered
}

# ==============================================================================
# Legacy compatibility
# ==============================================================================

#' Convert ProgressiveCodingState to legacy CodingResults format
#'
#' @param state ProgressiveCodingState
#' @return CodingResults-compatible list
#' @export
as_coding_results <- function(state) {
  if (!inherits(state, "ProgressiveCodingState")) {
    stop("state must be a ProgressiveCodingState object")
  }

  # Build all_codes structure
  all_codes <- list()
  for (key in names(state$codebook)) {
    cb <- state$codebook[[key]]
    all_codes[[key]] <- list(
      code = cb$code_name,
      type = cb$type,
      frequency = cb$frequency,
      entry_ids = cb$entry_ids,
      excerpts = lapply(cb$coded_segments, function(seg) {
        list(entry_id = seg$entry_id, excerpt = seg$text, validated = TRUE)
      })
    )
  }

  # Build entry_codes structure
  entry_codes <- list()
  entry_excerpts <- list()
  for (eid in names(state$entry_results)) {
    er <- state$entry_results[[eid]]
    if (isTRUE(er$skipped)) next

    entry_codes[[eid]] <- lapply(er$coded_segments, function(seg) {
      list(code = seg$code_name, type = "descriptive", excerpt = seg$text,
           excerpt_validated = TRUE)
    })

    entry_excerpts[[eid]] <- lapply(er$coded_segments, function(seg) {
      list(code = seg$code_name, excerpt = seg$text, validated = TRUE)
    })
  }

  list(
    all_codes = all_codes,
    entry_codes = entry_codes,
    entry_excerpts = entry_excerpts,
    total_applications = sum(vapply(state$codebook, function(x) x$frequency, integer(1))),
    unique_codes = length(state$codebook),
    entries_coded = length(state$entries_processed) - length(state$entries_skipped)
  )
}

# ==============================================================================
# Excerpt verification (preserved from original)
# ==============================================================================

#' Verify coded excerpts against source text
#'
#' @param data Tibble with std_text, std_id columns
#' @param coding_results CodingResults list (or ProgressiveCodingState)
#' @param provider AIProvider (optional, for coherence check)
#' @param sample_size Number of entries to check coherence for
#' @return List with substring_stats, coherence_stats, issues
#' @export
verify_excerpts <- function(data, coding_results, provider = NULL, sample_size = 20) {
  # Convert ProgressiveCodingState if needed
  if (inherits(coding_results, "ProgressiveCodingState")) {
    coding_results <- as_coding_results(coding_results)
  }

  entry_excerpts <- coding_results$entry_excerpts
  if (is.null(entry_excerpts) || length(entry_excerpts) == 0) {
    log_warn("No entry excerpts to verify")
    return(list(
      substring_stats = list(total = 0, valid = 0, invalid = 0, pct_valid = 100),
      coherence_stats = NULL,
      issues = tibble::tibble(entry_id = character(), code = character(),
                               excerpt = character(), issue_type = character(),
                               details = character())
    ))
  }

  text_lookup <- stats::setNames(as.character(data$std_text), as.character(data$std_id))

  normalize_ws <- function(x) gsub("\\s+", " ", trimws(x))
  total <- 0L
  valid <- 0L
  invalid <- 0L
  issues <- list()

  for (entry_id in names(entry_excerpts)) {
    orig_text <- text_lookup[[entry_id]]
    if (is.null(orig_text) || is.na(orig_text)) next
    norm_text <- normalize_ws(orig_text)

    for (exc in entry_excerpts[[entry_id]]) {
      excerpt <- exc$excerpt
      if (is.null(excerpt) || is.na(excerpt) || nchar(excerpt) < 5) next
      total <- total + 1L

      norm_exc <- normalize_ws(excerpt)
      found <- grepl(norm_exc, norm_text, fixed = TRUE) ||
               grepl(tolower(norm_exc), tolower(norm_text), fixed = TRUE)

      if (found) {
        valid <- valid + 1L
      } else {
        invalid <- invalid + 1L
        issues[[length(issues) + 1]] <- list(
          entry_id = entry_id,
          code = exc$code %||% "",
          excerpt = substr(excerpt, 1, 100),
          issue_type = "substring_mismatch",
          details = "Excerpt not found as substring of source text"
        )
      }
    }
  }

  pct_valid <- if (total > 0) round(valid / total * 100, 1) else 100
  substring_stats <- list(total = total, valid = valid, invalid = invalid, pct_valid = pct_valid)
  log_info("Excerpt substring validation: {valid}/{total} valid ({pct_valid}%)")

  issues_df <- if (length(issues) > 0) {
    tibble::tibble(
      entry_id = vapply(issues, function(x) x$entry_id, character(1)),
      code = vapply(issues, function(x) x$code, character(1)),
      excerpt = vapply(issues, function(x) x$excerpt, character(1)),
      issue_type = vapply(issues, function(x) x$issue_type, character(1)),
      details = vapply(issues, function(x) x$details, character(1))
    )
  } else {
    tibble::tibble(entry_id = character(), code = character(),
                    excerpt = character(), issue_type = character(),
                    details = character())
  }

  list(
    substring_stats = substring_stats,
    coherence_stats = NULL,
    issues = issues_df
  )
}
