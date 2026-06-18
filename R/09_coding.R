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
#' Replacement for the hardcoded \code{.MAX_ENTRY_CHARS = 8000L}.
#' Resolution order:
#' \enumerate{
#'   \item If \code{config$ai$max_entry_chars} is a positive integer, use
#'     it verbatim (researcher's explicit override).
#'   \item Otherwise, derive from \code{provider$context_window} (in
#'     tokens) by reserving ~60\% for system prompt + codebook + LLM
#'     completion and assigning the remaining ~40\% to per-entry text.
#'     Convert tokens to chars at ~4 chars/token (English averages
#'     4.0-4.5 chars per BPE token; 4 is a conservative under-estimate
#'     so context isn't over-filled).
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
  # R quirk: is.na(NULL) returns logical(0); guard with !is.null first
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
      code_birth_entry_id = list(),
      code_n_coded_at_birth = list(),
      reached = FALSE,
      reached_at_entry = NA_integer_,
      reached_at_coded = NA_integer_,
      total_entries_at_saturation = NA_integer_,
      # collapsed the earlier signals$ sub-list (code_creation_rate /
      # slope_ratio / ai_self_assessment) into a single ai_self_assessment field.
      # The two heuristic signals are gone (their thresholds were the hardcoded
      # gates C1 rejects). Kept as a list (not a scalar) so audit_log records
      # using signals$ai_self_assessment continue to parse + replay; back-compat
      # state files with all three sub-fields still load.
      signals = list(
        ai_self_assessment = FALSE
      ),
      # AI saturation arbiter (R/saturation_arbiter.R) records its
      # 2-4 sentence articulation + 1-2 sentence rationale here when it
      # declares saturation. Both are NA_character_ on a non-saturated run
      # (pre-init so any future report-side reader
      # can rely on the field's presence without %||% gymnastics).
      ai_articulation = NA_character_,
      ai_rationale = NA_character_,
      saturation_ratio = NA_real_,
      # pre-init the
      # dedupe field so a freshly created ProgressiveCodingState has
      # the same schema as a post-arbiter state. -1L means "no
      # arbiter call has fired yet" (the modulo gate's `!= -1L`
      # check accepts the first valid n_coded).
      last_arbiter_n_coded = -1L
    ),
    # per-code embedding cache for additive semantic
    # retrieval. code_embeddings is keyed by code_key; each value is a
    # numeric vector matching the embedding model's dimensionality
    # (text-embedding-3-small => 1536 dims). Populated on demand the
    # first time each code is seen by .retrieve_semantic_codes; survives
    # checkpoint save/restore via saveRDS like the rest of state.
    semantic_cache = list(
      code_embeddings = list()
    ),
    # codebook size at the most recent
    # description-refresh attempt. Drives the every-N-new-codes
    # refresh cadence in .maybe_refresh_high_freq_descriptions.
    # 0L initial value means the first refresh check fires once the
    # codebook hits the refresh interval (default 100 codes).
    last_description_refresh_at_size = 0L
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
#'   and a reference is recorded in the audit log. Pass \code{NULL}
#'   to skip raw-response capture.
#' @param fabrication_log An optional FabricationLog object (from
#'   \code{\link{init_fabrication_log}}). T0.1 verification ALWAYS runs --
#'   each per-segment AI-attributed verbatim text is checked against the
#'   entry via the verification ladder, and fabricated segments are dropped
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
#' @param live_tracker Optional \code{LiveTracker} (from
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
  # Aggregate AI-failure breaker knobs (M-34). NOTE: the pipeline passes
  # config$analysis$coding AS this function's `config`, so these are read
  # at the TOP level of the sub-config (like max_retries_per_entry above).
  config$max_consecutive_entry_failures <-
    as.integer(config$max_consecutive_entry_failures %||% 10L)
  config$max_failed_entry_fraction <-
    as.numeric(config$max_failed_entry_fraction %||% 0.25)
  config$include_in_vivo <- config$include_in_vivo %||% TRUE
  # Removed `config$code_style %||% "descriptive"` --
  # the value was set but never read.
  config$checkpoint_interval <- config$checkpoint_interval %||% .PARTIAL_CHECKPOINT_INTERVAL

  # all six earlier hardcoded saturation knobs
  # (saturation_enabled / saturation_window / saturation_threshold /
  # saturation_confirmations / min_coded_before_saturation /
  # ai_assessment_interval) removed per C1 ("AI decides when to stop;
  # no hardcoded saturation thresholds"). The AI arbiter
  # (.ai_judge_saturation in R/saturation_arbiter.R) is now the sole
  # decision; cadence is auto-scaled by .saturation_cadence(nrow(data)).
  # See R/saturation_arbiter.R header for the rewrite rationale.

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

  # Re-queue AI-call failures from a prior (crashed/tripped) run. A
  # periodic checkpoint taken mid-outage can bake failure-marked records
  # (failure = TRUE: NULL result after retries, never an AI-judged skip)
  # into entries_processed; without eviction, resume would carry them
  # forever as "AI response parse failure" skips. Drop the records and
  # their indices so they are genuinely retried.
  failed_ids <- names(Filter(function(er) isTRUE(er$failure),
                             state$entry_results))
  if (length(failed_ids) > 0L) {
    failed_idx <- which(as.character(data$std_id) %in% failed_ids)
    state$entries_processed <- setdiff(state$entries_processed, failed_idx)
    state$entries_skipped   <- setdiff(state$entries_skipped,   failed_idx)
    state$entry_results[failed_ids] <- NULL
    log_info(paste0("Re-queuing {length(failed_ids)} entr",
                    "{if (length(failed_ids) == 1L) 'y' else 'ies'} whose ",
                    "AI calls failed in a previous run (network/parse ",
                    "failures are retried on resume, not carried as skips)"))
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
    learning_context = learning_context,
    framework_spec = framework_spec
  )

  # Mode 3: framework prompt block is loop-invariant (depends only on
  # framework_spec, not on the per-entry state). Compute once before the
  # loop -- otherwise a 150-entry corpus pays 150 redundant validate_class()
  # + paste calls.
  framework_prompt_text <- if (!is.null(framework_spec)) {
    framework_prompt_block(framework_spec)
  } else NULL

  batch_delay <- provider$rate_limits$delay_between_batches %||% 0.5

  # saturation tracking state
  # saturation_cadence: AI arbiter check cadence, auto-scaled by corpus
  # size per .saturation_cadence(). Replaces the earlier
  # ai_assessment_interval / saturation_window / etc. knobs.
  saturation_cadence <- .saturation_cadence(n)
  # Consecutive AI-call failures; resets on any successful call.
  # After 3 in a row, log one warning per failure streak; the arbiter
  # keeps retrying at each cadence checkpoint (never silently saturate;
  # never silently never-saturate).
  saturation_failure_streak <- 0L

  # Aggregate AI-failure breaker (M-34): a provider/network outage
  # mid-run must NOT be silently recorded as entry skips and reported as
  # a substantive near-empty analysis. Counts only failure-marked records
  # (NULL result after retries) -- never legitimate AI-judged skips, so
  # high-skip corpora cannot false-trip.
  n_ai_attempted <- 0L
  n_ai_failed <- 0L
  consecutive_ai_failures <- 0L

  # Use lists for O(1) append (avoid O(n^2) vector growth)
  new_processed <- list()
  new_skipped <- list()
  proc_idx <- 0L

  for (idx in remaining) {
    # Check if saturation was already reached (e.g., from a resumed state)
    if (isTRUE(state$saturation$reached)) {
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
        skip_reason = "Text too short or missing",
        failure = FALSE,
        chars_total = if (is.na(entry_text)) 0L else nchar(entry_text),
        chars_sent = 0L,       # never sent to the LLM
        truncated = FALSE
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

    # Aggregate AI-failure breaker (M-34). Trip BEFORE the periodic
    # checkpoint merge can bake an outage's failure-skips into a saved
    # checkpoint that resume would never retry. No partial checkpoint is
    # written on the trip path for the same reason.
    n_ai_attempted <- n_ai_attempted + 1L
    entry_failed <- isTRUE(state$entry_results[[entry_id]]$failure)
    if (entry_failed) {
      n_ai_failed <- n_ai_failed + 1L
      consecutive_ai_failures <- consecutive_ai_failures + 1L
    } else {
      consecutive_ai_failures <- 0L
    }
    tripped <- consecutive_ai_failures >= config$max_consecutive_entry_failures ||
      (n_ai_attempted >= 20L &&
         n_ai_failed / n_ai_attempted > config$max_failed_entry_fraction)
    if (tripped) {
      msg <- paste0(
        "Aggregate AI-call failure breaker tripped: ", n_ai_failed, " of ",
        n_ai_attempted, " attempted entries failed (",
        consecutive_ai_failures, " consecutive). This is likely a ",
        "provider/network outage or invalid credentials -- NOT a ",
        "substantive 'no applicable content' result. Fix connectivity ",
        "and re-run with resume = TRUE; entries not yet checkpointed ",
        "will be retried. (Thresholds: max_consecutive_entry_failures = ",
        config$max_consecutive_entry_failures,
        ", max_failed_entry_fraction = ",
        config$max_failed_entry_fraction, ".)"
      )
      stop(structure(
        class = c("pakhom_coding_failure_breaker", "error", "condition"),
        list(message = msg, call = sys.call(-1))
      ))
    }

    # every-N-new-codes description refresh pass.
    # Skips silently when conditions aren't met (cheap O(N codes)
    # filter). Mode 3 skips entirely because framework constructs
    # carry researcher-authored descriptions that shouldn't be
    # re-written.
    if (is.null(framework_spec)) {
      state <- .maybe_refresh_high_freq_descriptions(
        state            = state,
        provider         = provider,
        audit_log        = audit_log,
        response_cache   = response_cache,
        # `config` here is already the analysis.coding block (see the call in
        # 18_pipeline.R and the failure-knob reads above), so these knobs live
        # at its top level -- NOT under config$analysis$coding (which would be
        # double-nested and always fall back to the defaults, ignoring the
        # user's config.yaml).
        refresh_interval = as.integer(
          config$description_refresh_interval %||% 100L),
        min_freq         = as.integer(
          config$description_refresh_min_freq %||% 50L),
        sample_segments  = as.integer(
          config$description_refresh_sample_segments %||% 5L)
      )
    }

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
      # C3: stream every (entry, code, segment) assignment to
      # the live tracker so a researcher can `tail -F` the artifact during
      # a long run. Done in the same loop as audit-log emission so the
      # two stay in lockstep. The is_new_code flag is computed against
      # codes_before; codebook_snapshot is rewritten at end-of-entry below.
      if (!is.null(audit_log) || !is.null(live_tracker)) {
        codes_before_set <- if (!is.null(live_tracker)) {
          # Snapshot of codebook keys BEFORE this entry, to flag
          # newly-created codes as is_new_code = TRUE in the live event log.
          if (codes_before == 0L) character(0L) else
            head(names(state$codebook), codes_before)
        } else NULL
        # dedupe is_new_code emission. An earlier version
        # if an entry contributed multiple segments under a single NEW code,
        # every segment received is_new_code=TRUE in the live tracker
        # (audit_log fires new_code_created once correctly; the live event
        # log over-counted by ~1.7%). Track which new codes have already
        # been marked WITHIN this entry; subsequent segments under the
        # same code emit is_new_code=FALSE.
        first_seen_new_keys <- character(0L)

        for (seg in er$coded_segments) {
          if (!is.null(audit_log)) {
            log_ai_decision(audit_log, "coding", "code_assignment",
                            entry_id = entry_id, code_name = seg$code_name,
                            code_key = seg$code_key)
          }
          if (!is.null(live_tracker)) {
            is_new_segment <- !(seg$code_key %in% codes_before_set) &&
                               !(seg$code_key %in% first_seen_new_keys)
            if (is_new_segment) {
              first_seen_new_keys <- c(first_seen_new_keys, seg$code_key)
            }
            live_tracker <- live_record_assignment(
              tracker     = live_tracker,
              entry_id    = entry_id,
              code_key    = seg$code_key,
              code_name   = seg$code_name,
              segment     = seg,
              is_new_code = is_new_segment,
              entry_index = idx
            )
          }
        }
      }
    }

    # C3: rewrite codebook_live.json after every entry (cadence
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
        # Also record the STABLE std_id of the birth entry. 21_longitudinal.R
        # resolves first_code_date via this id (robust to row subsetting /
        # reordering), instead of indexing by the positional `idx`, which is
        # off whenever the analytic frame has dropped skipped entries.
        state$saturation$code_birth_entry_id[[nk]] <- entry_id
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

    # saturation curve + AI arbiter check.
    # Curve cadence: min(50, saturation_cadence) -- on small corpora the
    # arbiter fires more often than 50 ticks so the curve cadence is
    # tightened to match (otherwise the arbiter would see "no curve data
    # yet" on its first check). For large corpora cadence > 50 and the
    # 50-tick cadence is kept so the saturation plot stays smooth.
    # An earlier version had a
    # hardcoded 50 here that drifted out of sync with the arbiter
    # cadence on mid-sized corpora.
    curve_cadence <- min(50L, saturation_cadence)
    if (n_coded > 0 && n_coded %% curve_cadence == 0) {
      # Window for the new-codes-in-window curve point: use the arbiter
      # cadence (one window per arbiter check). Bounded by n_coded so
      # early curve points use a smaller window naturally.
      window_size <- min(saturation_cadence, n_coded)

      # New-codes-in-window: count codes whose birth-time (measured as
      # the n_coded value when they were created) falls within the last
      # `window_size` coded entries. Direct lookup of the persistent
      # code_n_coded_at_birth map -- correct across checkpoint resets,
      # correct across resumes, O(|codebook|) per check.
      if (length(state$saturation$code_n_coded_at_birth) > 0) {
        births <- unlist(state$saturation$code_n_coded_at_birth, use.names = FALSE)
        new_in_window <- sum(births > (n_coded - window_size))
      } else {
        new_in_window <- 0L
      }

      # Inductive Thematic Saturation (ITS) ratio per De Paoli & Mathis
      # 2024: unique_codes / total_assignments. Recorded in the curve
      # for the AI arbiter's prompt evidence + the saturation plot.
      # An earlier version used a hardcoded 0.05 stopping threshold; that's
      # gone -- the AI judges the trajectory directly.
      total_assignments <- sum(vapply(state$codebook,
                                        function(cb) cb$frequency,
                                        integer(1)))
      # NB: despite the name (kept for curve-column schema stability), this is
      # the codes-to-assignments RATIO (distinct codes / total code
      # applications), an inverse code-reuse density -- NOT a regression slope.
      slope_ratio <- if (total_assignments > 0) {
        codes_after / total_assignments
      } else 1.0

      state$saturation$curve <- rbind(state$saturation$curve, data.frame(
        entries_coded = n_coded,
        entries_processed = n_done,
        n_codes = codes_after,
        new_codes_in_window = as.integer(new_in_window),
        slope_ratio = round(slope_ratio, 4),
        timestamp = Sys.time()
      ))
    }

    # AI saturation arbiter (C1: AI decides when to stop).
    # Fires every `saturation_cadence` coded entries -- no min-entries
    # gate, no kill switch. The AI can output "uncertain" when the
    # evidence is too thin to judge, so an early check is harmless.
    # dedupe arbiter calls when an entry is
    # SKIPPED right after a coded entry whose n_coded hit the cadence
    # multiple. An earlier modulo gate would re-fire on every
    # skipped iteration that followed (since n_coded was unchanged),
    # producing 26 duplicate arbiter calls at 9 distinct n_coded
    # values on a large run (~$0.30 wasted). last_arbiter_n_coded
    # is the n_coded value the arbiter last evaluated; only re-fire
    # when it advances. NULL on fresh runs (first arbiter call
    # naturally falls through).
    last_arbiter_n <- state$saturation$last_arbiter_n_coded %||% -1L
    # Gate the saturation arbiter on
    # is.null(framework_spec). In Mode 3 the codebook is pre-populated
    # with framework constructs (R/09_coding.R:285-319 above) and the
    # AI saturation arbiter's prompt (R/saturation_arbiter.R:215) is
    # framed for INDUCTIVE coding -- it asks the AI to judge whether
    # the codebook has stopped growing. In Mode 3 the codebook NEVER
    # grows beyond the pre-populated constructs (assignments only
    # increment existing-code frequencies), so the arbiter
    # mechanically saturates at the first call (entry 23/250 in
    # smoke runs -- 92% of corpus skipped).
    # The right Mode 3 stop criterion is corpus exhaustion, not
    # codebook stability. Skip the arbiter entirely when a framework
    # spec is present.
    if (is.null(framework_spec) && n_coded > 0 &&
        n_coded %% saturation_cadence == 0 &&
        n_coded != last_arbiter_n) {
      state$saturation$last_arbiter_n_coded <- n_coded
      judgment <- .ai_judge_saturation(
        state = state, provider = provider,
        research_focus = research_focus,
        n_coded = n_coded, n_corpus = n, n_done = n_done,
        audit_log = audit_log, response_cache = response_cache
      )

      if (!judgment$success) {
        saturation_failure_streak <- saturation_failure_streak + 1L
        if (saturation_failure_streak == 3L) {
          log_warn(paste0(
            "AI saturation arbiter failed 3 consecutive times; ",
            "coding continues and the arbiter will retry at the next ",
            "cadence checkpoint. The failure streak resets on the next ",
            "successful call."
          ))
        }
      } else {
        saturation_failure_streak <- 0L
        if (identical(judgment$verdict, "reached")) {
          state$saturation$reached <- TRUE
          state$saturation$reached_at_entry <- n_done
          state$saturation$reached_at_coded <- n_coded
          state$saturation$total_entries_at_saturation <- n
          state$saturation$signals$ai_self_assessment <- TRUE
          state$saturation$ai_articulation <- judgment$articulation
          state$saturation$ai_rationale <- judgment$rationale
          state$saturation$saturation_ratio <- round(
            length(state$codebook) / n_coded, 4
          )

          log_info("*** THEMATIC SATURATION REACHED (AI arbiter) ***")
          log_info("  At entry {n_done}/{n} ({n_coded} coded)")
          log_info("  Codebook: {length(state$codebook)} codes")
          log_info("  AI articulation: {substr(judgment$articulation, 1, 200)}")
          log_info("  AI rationale: {judgment$rationale}")
          log_info("  Saturation ratio: {state$saturation$saturation_ratio}")
          log_info("  Remaining {n - n_done} entries will not be processed")
          break
        }
        # judgment$verdict %in% c("not_yet", "uncertain"): continue coding.
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

  # End-of-run failure-fraction check. The in-loop fraction gate has a
  # 20-attempt floor (a fraction over a handful of entries is noise), so
  # a SMALL corpus can finish with most AI calls failed and no trip --
  # warn loudly so an outage run is not read as a substantive result.
  if (n_ai_attempted > 0L && n_ai_attempted < 20L &&
      n_ai_failed / n_ai_attempted > config$max_failed_entry_fraction) {
    log_warn(paste0(
      "AI calls failed for ", n_ai_failed, " of ", n_ai_attempted,
      " attempted entries -- above the max_failed_entry_fraction ",
      "threshold (", config$max_failed_entry_fraction, "), but the ",
      "corpus was too small for the in-run breaker (< 20 attempts). ",
      "Treat this run's near-empty coding as a likely provider/network ",
      "outage, NOT a substantive result; re-run with resume = TRUE."
    ))
  }

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

  # C3: force a final codebook_live.json snapshot regardless of
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
    # additive retrieval. Frequency + recency
    # (legacy behavior) plus per-entry semantic top-K cosine retrieval
    # against cached code embeddings. With 4,059-code codebooks the
    # an earlier top-80 window made every entry past ~entry 1000 see
    # only 2% of the existing codebook, so re-encounters looked new.
    # max_codes raised 80 -> 150; semantic top-K added; embeddings
    # cached per code key in state$semantic_cache$code_embeddings.
    cb_result <- .build_codebook_summary_with_retrieval(
      state,
      max_codes      = 150L,
      recent_window  = 20L,
      entry_text     = text,
      provider       = provider,
      top_k_semantic = 30L
    )
    codebook_summary <- cb_result$summary
    if (length(cb_result$new_embeddings) > 0L) {
      if (is.null(state$semantic_cache)) {
        state$semantic_cache <- list(code_embeddings = list())
      }
      for (key in names(cb_result$new_embeddings)) {
        state$semantic_cache$code_embeddings[[key]] <- cb_result$new_embeddings[[key]]
      }
    }
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

  # Auto-context-aware per-entry truncation. Was previously
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
  # Within-entry truncation accounting (T0.3): recorded on every
  # entry_results write so the coverage card can disclose how many entries
  # were sent truncated and how many characters actually reached the LLM.
  chars_total <- nchar(text)
  chars_sent  <- nchar(truncated_text)
  entry_truncated <- chars_total > effective_max_chars
  if (entry_truncated) {
    log_debug("Entry {entry_id}: text truncated from {chars_total} to {effective_max_chars} chars")
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

  # A coding call uses a non-zero temperature, so a JSON parse failure on one
  # attempt may succeed on the next (a fresh sample) -- retrying it is a
  # legitimate recovery, NOT a deterministic re-charge. This is deliberately
  # distinct from ai_complete()'s permanent-error class (empty choices / forced
  # tool not called / non-retryable 4xx), which IS deterministic and is never
  # retried. max_retries_per_entry (default 1) bounds the extra paid attempts.
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
      skip_reason = skip_reason,
      # failure discriminates a real AI-call breakdown (NULL result after
      # retries: network / timeout / parse) from a legitimate AI-judged
      # skip -- the aggregate failure breaker keys on this, never on the
      # free-text skip_reason.
      failure = is.null(result),
      chars_total = chars_total,
      chars_sent = chars_sent,
      truncated = entry_truncated
    )
    return(state)
  }

  segments_raw <- result$coded_segments
  if (is.null(segments_raw) || length(segments_raw) == 0) {
    state$entry_results[[entry_id]] <- list(
      codes_assigned = character(0),
      coded_segments = list(),
      skipped = TRUE,
      skip_reason = "No coded segments returned",
      failure = FALSE,
      chars_total = chars_total,
      chars_sent = chars_sent,
      truncated = entry_truncated
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
    skip_reason    = NA_character_,
    failure        = FALSE,
    chars_total    = chars_total,
    chars_sent     = chars_sent,
    truncated      = entry_truncated
  )

  state
}

# ==============================================================================
# Per-segment processing (shared between schema and citations paths)
# ==============================================================================

#' Decide whether to use the Anthropic Citations API path for this provider
#'
#' Returns TRUE for Anthropic providers; FALSE otherwise. A future
#' release may add a config opt-out (\code{config$data_integrity$use_citations_api}),
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

#' Build the schema-path user prompt
#'
#' An earlier implementation JSON-escaped
#' the entry text via \code{jsonlite::toJSON(truncated_text, auto_unbox =
#' TRUE)} then stripped the outer quotes and wrapped the result in
#' literal quote marks. This made embedded \code{"} / \code{\\}
#' characters appear as 2-character escape sequences in the prompt --
#' so the AI's emitted \code{start_char} / \code{end_char} offsets
#' referenced the ESCAPED form, but \code{verify_quote} re-fetches the
#' UN-escaped source and tries to match at the same indices. Every
#' entry with a single \code{"} silently produced an off-by-one
#' verification failure that Step 3 substring-search papered over,
#' driving a large run to 99.89% verified_fuzzy / 0.11%
#' verified_exact. Fenced fence the entry text with explicit XML-style
#' delimiters and pass the text verbatim, so the AI sees exactly the
#' same character offsets that the verifier will check.
#' @keywords internal
.build_progressive_schema_user_prompt <- function(truncated_text) {
  # The opening and closing tags are constructed at runtime (not as
  # literal strings in the surrounding instructions) so a regex/parser
  # scanning the prompt for the tags only matches the actual fence,
  # not a description of the tags. Pre-fix the explanation paragraph
  # contained "<entry_text>...</entry_text>" verbatim which caused
  # the test mock's regexpr to match the explanation instead of the
  # entry. (Avoids a class of false-positive tag matches in tools that
  # post-process the prompt without a real XML parser.)
  open_tag  <- paste0("<", "entry_text", ">")
  close_tag <- paste0("</", "entry_text", ">")
  # Defensive escape for the rare case
  # where the entry text literally contains the closing-tag sentinel
  # (e.g. tutorial snippets, HTML fragments). The pre-followup version
  # would render an unbalanced fence; the AI could read the inner
  # </entry_text> as the closing marker and emit offsets relative to
  # a truncated view. Replacement uses an unambiguous sentinel that
  # parsers / readers see as "literal text" -- the AI never re-emits
  # this exact sequence as a tag.
  safe_text <- .escape_entry_text_fence(truncated_text)
  paste0(
    "As you read through this entry, code any text segments applicable to the research question.\n\n",
    "The entry text appears between the opening tag (XML-style 'entry_text') ",
    "and the matching closing tag below. When emitting start_char / end_char ",
    "offsets, count characters starting at 0 from the FIRST character INSIDE ",
    "the opening tag; the closing tag marks the (exclusive) end of the text ",
    "and is NOT part of the entry.\n\n",
    open_tag,
    safe_text,
    close_tag
  )
}

#' Defensive escape for the entry-text fence
#'
#' When the entry text literally
#' contains \code{</entry_text>}, the prompt fence is unbalanced and the
#' AI may compute offsets against a truncated view of the entry. This
#' helper replaces the closing-tag sentinel inside the entry text with
#' an unambiguous escape that parsers see as literal text.
#' The offsets the AI emits will then be against the ESCAPED text, which
#' is what \code{verify_quote} also sees (no un-escaping before
#' verification; the escape is a deterministic 1:1 character mapping
#' that preserves character-position arithmetic for the relevant range).
#' For typical Reddit posts this is a no-op; only adversarial / tutorial
#' inputs trigger the substitution.
#' @keywords internal
.escape_entry_text_fence <- function(text) {
  sentinel <- paste0("</", "entry_text", ">")
  if (!grepl(sentinel, text, fixed = TRUE)) return(text)
  # Replace each occurrence with an inline placeholder that has the
  # same character length so any offsets the AI might emit for text
  # AFTER the substitution still map correctly. 14 characters in,
  # 14 characters out.
  gsub(sentinel, "[end-tag-lit]", text, fixed = TRUE)
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
  # Same offset-correctness fix as
  # .build_progressive_schema_user_prompt. Tags constructed at
  # runtime so a parser scanning the prompt matches only the actual
  # fence, not a description of the tags. A
  # defensive escape for adversarial inputs that contain the
  # closing-tag sentinel verbatim.
  open_tag  <- paste0("<", "entry_text", ">")
  close_tag <- paste0("</", "entry_text", ">")
  safe_text <- .escape_entry_text_fence(truncated_text)
  paste0(
    "Apply the framework's constructs to any text segments in this entry that fit them. ",
    "For each applicable segment:\n",
    "- Set `construct_id` to one of the framework's construct ids (listed in the system prompt above).\n",
    "- Set `anomaly_reason` to \"\" (the empty string) for normal construct applications.\n",
    "If a segment resists the framework (no construct fits), code it as ",
    "`construct_id: \"anomaly\"` and set `anomaly_reason` to a one-sentence ",
    "explanation of why the framework doesn't capture it. Do NOT force a fit; ",
    "the framework's anomaly_handling policy treats these as first-class output.\n\n",
    "The entry text appears between the opening tag (XML-style 'entry_text') ",
    "and the matching closing tag below. When emitting start_char / end_char ",
    "offsets, count characters starting at 0 from the FIRST character INSIDE ",
    "the opening tag; the closing tag marks the (exclusive) end of the text ",
    "and is NOT part of the entry.\n\n",
    open_tag,
    safe_text,
    close_tag
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
    construct_id   <- trimws(as.character(seg$construct_id %||% "")[1])
    anomaly_reason <- as.character(seg$anomaly_reason %||% "")[1]
    valid_ids      <- framework_spec$construct_ids %||% character(0)
    # Mode 3 applies a FIXED framework: the model may only return a real
    # construct_id or the literal "anomaly". A construct_id outside the
    # framework must NOT be silently admitted as a new construct -- that would
    # let the model invent constructs and quietly drop those entries out of
    # every framework theme. Re-route it to the anomaly bucket (the
    # methodologically-required home for framework-resistant content) and
    # record the model's out-of-framework proposal in the reason.
    if (nchar(construct_id) > 0 && !identical(construct_id, "anomaly") &&
        !(construct_id %in% valid_ids)) {
      log_warn(sprintf(
        paste0("Mode 3: model returned construct_id '%s' not in the framework ",
               "(%s); routing the segment to the anomaly bucket."),
        construct_id, paste(valid_ids, collapse = ", ")))
      anomaly_reason <- if (nzchar(anomaly_reason)) {
        paste0(anomaly_reason, " [model proposed out-of-framework construct '",
               construct_id, "']")
      } else {
        paste0("Model proposed out-of-framework construct '", construct_id, "'")
      }
      construct_id <- "anomaly"
    }
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
    # Detect NEW: marker on the original (before strip). `["']*` allowance
    # catches quote-wrapped forms like `"NEW: Foo"` (C-4 audit MEDIUM-4).
    is_new <- grepl("^[\"']*\\s*(\\d+\\.\\s*)?\\s*NEW:", seg_code,
                     ignore.case = TRUE)
    code_name <- .normalize_code_name(seg_code)
    # Post-normalization empty guard (C-4 audit MEDIUM-3): inputs like
    # "321." or bare "NEW:" collapse to "" once prefixes strip. The
    # pre-norm guard at the top of the function only catches the
    # nchar(seg_code) == 0 case. Without this it would write a codebook entry
    # under the empty-string key.
    if (is.na(code_name) || nchar(code_name) == 0L) {
      log_warn(paste0(
        "Entry ", entry_id, ": AI returned a code that normalized to ",
        "empty ('", substr(seg_code, 1, 60), "'); segment dropped."
      ))
      return(state)
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
  #   it falls back to model_freeform (the model returned a `text` claim
  #   without an attached citation, so it is treated like the schema path).
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
      # emit failure_reason so audit
      # log readers can attribute fabrications to specific ladder
      # failure modes without joining against fabrication_log.csv.
      log_ai_decision(audit_log, "quote_verification", "quote_fabricated",
                      entry_id  = entry_id, code_name = code_name,
                      quote_id  = quote$quote_id,
                      ai_call_id = quote$ai_call_id %||% NA_character_,
                      exact_text = substr(seg_text, 1, 200),
                      failure_reason = quote$verification_failure_reason
                                         %||% NA_character_)
    }
    log_warn("Entry {entry_id}: AI returned fabricated quote for code '{code_name}'; segment dropped.")
    return(state)
  }

  if (identical(quote$verification_status, "drifted")) {
    # Source corpus changed between attribution and verification time
    # (source_text_sha256 mismatch AND ladder failed). Per spec the quote
    # is excluded from rendering pending researcher review; it is logged to
    # the audit trail so cross-run analysis can attribute drifts.
    if (!is.null(audit_log)) {
      # Thread failure_reason for
      # drifted quotes too (only fabricated had the kwarg
      # pre-followup, leaving drifted records without attribution
      # for downstream cross-run analysis).
      log_ai_decision(audit_log, "quote_verification", "quote_drifted",
                      entry_id  = entry_id, code_name = code_name,
                      quote_id  = quote$quote_id,
                      ai_call_id = quote$ai_call_id %||% NA_character_,
                      exact_text = substr(seg_text, 1, 200),
                      failure_reason = quote$verification_failure_reason
                                         %||% NA_character_)
    }
    log_warn("Entry {entry_id}: quote drifted (source SHA mismatch) for code '{code_name}'; segment dropped pending review.")
    return(state)
  }

  # Verified -- attach provenance to the segment record so downstream
  # rendering can show verification status, and the methodology paper can
  # compute per-run fabrication rates from the codebook.
  # Emit the quote_verified audit record (the denominator counterpart of
  # quote_fabricated/quote_drifted above) so the transparency report's
  # "Verifications run" counts real verification events rather than a
  # code_assignment proxy. No exact_text -- keep the record small.
  if (!is.null(audit_log)) {
    log_ai_decision(audit_log, "quote_verification", "quote_verified",
                    entry_id  = entry_id, code_name = code_name,
                    quote_id  = quote$quote_id,
                    ai_call_id = quote$ai_call_id %||% NA_character_,
                    verification_status = quote$verification_status,
                    verification_method = quote$verification_method
                                            %||% NA_character_)
  }
  seg_record <- list(
    entry_id   = entry_id,
    text       = seg_text,
    start_char = seg_start,
    end_char   = seg_end,
    provenance = quote
  )

  if (is_new || !(code_key %in% names(state$codebook))) {
    # backfill empty descriptions on new code
    # admission. The AI's schema is told "code_description required for
    # NEW codes" but doesn't always comply -- and when it matches a
    # learning-context name without re-stating the description, the
    # field arrives empty. Without backfill, downstream consumers see
    # codes with NA descriptions. The fallback uses the first segment's
    # text as a "first observed" snippet so reviewers have at least
    # one anchor for what the code captured. A subsequent
    # refresh pass (.maybe_refresh_high_freq_descriptions,
    # selects by frequency >= min_freq, NOT by matching this text)
    # replaces it once the code accumulates enough segments to support
    # a real description.
    # Also guard against NA. jsonlite
    # parses JSON `null` to NA_character_ which would crash the
    # subsequent if-condition ("missing value where TRUE/FALSE needed").
    # dropped the "[D-7 placeholder; awaiting refresh]"
    # engineering marker -- it shipped verbatim into themes.json (an
    # internal artifact masquerading as a code description) AND, because
    # the description feeds the clustering embedding input
    # (paste(name, description) in R/13_themes.R), the marker text biased
    # clustering. The honest human default below keeps the informative
    # first-segment snippet (the real reviewer anchor) without the marker.
    admit_desc <- if (is.null(seg_desc) || is.na(seg_desc) ||
                      !nzchar(trimws(seg_desc %||% ""))) {
      snippet <- substr(as.character(seg_text)[1], 1, 150)
      log_warn(paste0(
        "Entry ", entry_id, ": new code '", code_name,
        "' admitted with empty description; using first-segment ",
        "text as a provisional description (will be refreshed when the ",
        "code reaches the description-refresh frequency)."
      ))
      paste0("First observed in: ", snippet)
    } else {
      seg_desc
    }
    state$codebook[[code_key]] <- list(
      code_name      = code_name,
      description    = admit_desc,
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
  # text passed to it; later verify_quote re-hashes the FULL entry text and
  # compares. To keep these in sync (and avoid spurious "drifted" status on
  # long entries where the prompt was truncated to .MAX_ENTRY_CHARS but the
  # verifier sees the full text), the full text is substituted into the
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
                                              learning_context,
                                              framework_spec = NULL) {
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

  # inject the AI-articulated relevance criterion (Methodology
  # Assistant, Step 2.5). It operationalizes what is on-focus for THIS study,
  # replacing the loose "applicable to the research question" framing with a
  # study-specific inclusion/exclusion criterion -- the upstream fix for focus
  # drift. Empty when no criterion was articulated -> the task framing below
  # falls back to the prior wording (byte-identical to the earlier behavior).
  relevance <- config$relevance_block %||% ""
  if (nchar(relevance) > 0) {
    prompt <- paste0(prompt, "\n", relevance, "\n")
  }

  task_intro <- if (nchar(relevance) > 0) {
    paste0(
      "As you read through the entry text, code the segments that meet the ",
      "RELEVANCE CRITERION above. For each on-focus segment you encounter:\n")
  } else {
    paste0(
      "As you read through the entry text, code any portions applicable to the ",
      "research question above. For each applicable segment you encounter:\n")
  }

  prompt <- paste0(prompt,
    "\n## YOUR TASK\n",
    task_intro,
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
    # The prior-studies codebook hierarchy is the PRIMARY learning source for
    # INDUCTIVE coding (Modes 1/2): send it first so the model reuses established
    # codes rather than re-inventing them. In Mode 3 (framework_spec present) the
    # codebook IS the fixed framework, so injecting a different prior codebook
    # ("reuse these codes") would contradict deductive framework application --
    # skip it there.
    if (is.null(framework_spec) &&
        nchar(learning_context$for_coding_hierarchy %||% "") > 0) {
      prompt <- paste0(prompt,
                       "\n## CODEBOOK FROM PRIOR STUDIES (reuse these codes where the text fits them)\n",
                       learning_context$for_coding_hierarchy, "\n")
    }
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

#' AI-driven refresh of a single high-frequency code's description
#'
#' re-prompts the AI with a sample of the segments
#' the code has accumulated and asks for a description that captures the
#' SHARED conceptual core across them. The earlier codebook anchored
#' each description to the FIRST segment that created the code, so
#' high-frequency codes (e.g. Compulsive Eating Behavior, freq=1127)
#' carried descriptions that described only one of many distinct meanings
#' the code accumulated.
#'
#' Returns NULL on AI failure (caller leaves description unchanged but
#' still bumps last_description_refresh_at to avoid retrying on every
#' cadence).
#'
#' @keywords internal
.refresh_code_description <- function(provider, code_name, current_description,
                                        sample_segments,
                                        audit_log = NULL,
                                        response_cache = NULL,
                                        methodology_override = NULL) {
  if (is.null(provider)) return(NULL)
  if (length(sample_segments) == 0L) return(NULL)

  segment_block <- paste(vapply(seq_along(sample_segments), function(i) {
    txt <- as.character(sample_segments[[i]]$text %||% "")[1]
    sprintf("[%d] %s", i, substr(txt, 1, 300))
  }, character(1)), collapse = "\n\n")

  system_prompt <- paste0(
    "You are reviewing a thematic code that has accumulated multiple ",
    "segments since it was first created. Your task is to refresh the ",
    "code's description so it accurately reflects the CONCEPTUAL CORE ",
    "shared across all sample segments.\n\n",
    "The current description was anchored to the FIRST segment that ",
    "created the code; if the actual scope has drifted, your refresh ",
    "should capture the broader pattern that unifies every segment in ",
    "the sample. Be specific enough to distinguish from sibling codes ",
    "but general enough to cover every segment shown."
  )

  prompt <- paste0(
    "## CODE\n",
    "Name: ", code_name, "\n",
    "Current description: ", current_description %||% "(empty)", "\n\n",
    "## SAMPLE SEGMENTS (n = ", length(sample_segments), ")\n\n",
    segment_block,
    "\n\n## TASK\n",
    "Refresh the description in 1-2 sentences to accurately reflect ",
    "what these segments share. Do not just restate the code name."
  )

  result <- tryCatch({
    ai_result <- ai_complete(
      provider, prompt, system_prompt,
      task                  = "description_refresh",
      temperature           = 0,
      response_schema       = .code_description_refresh_schema(),
      methodology_override  = methodology_override
    )
    if (!is.null(audit_log)) {
      log_ai_request(audit_log, "description_refresh", ai_result, response_cache,
                      code_name = code_name,
                      n_sample_segments = length(sample_segments))
    }
    parse_json_safely(ai_result$content)
  }, error = function(e) {
    log_warn(paste0(
      "Description refresh failed for code '",
      code_name, "': ", conditionMessage(e)
    ))
    NULL
  })

  if (is.null(result) || is.null(result$description)) return(NULL)
  refreshed <- as.character(result$description)[1]
  if (!nzchar(trimws(refreshed))) return(NULL)
  trimws(refreshed)
}

#' Walk the codebook for high-frequency codes due for description refresh
#'
#' every \code{refresh_interval} new codes admitted
#' to the codebook, scan for codes with \code{frequency >= min_freq}
#' that haven't been refreshed in this cycle, sample
#' \code{sample_segments} of their coded_segments, and ask the AI to
#' refresh their description. Updates the codebook in place and stamps
#' \code{last_description_refresh_at} on each refreshed code.
#'
#' @keywords internal
.maybe_refresh_high_freq_descriptions <- function(state, provider,
                                                    audit_log = NULL,
                                                    response_cache = NULL,
                                                    refresh_interval = 100L,
                                                    min_freq = 50L,
                                                    sample_segments = 5L,
                                                    methodology_override = NULL) {
  if (is.null(provider)) return(state)
  cb <- state$codebook
  current_size <- length(cb)
  if (current_size == 0L) return(state)

  # Explicit disable when interval <= 0.
  # Without this guard, interval=0 makes (current_size - last_attempt
  # < 0) always FALSE -> dispatcher fires on every entry instead of
  # never. The commit / config docs promise interval=0 disables; this
  # makes that promise honest.
  if (refresh_interval <= 0L) return(state)

  last_attempt <- state$last_description_refresh_at_size %||% 0L
  if (current_size - last_attempt < refresh_interval) return(state)

  # Identify high-freq codes due for refresh.
  needs_refresh <- character(0)
  for (key in names(cb)) {
    code <- cb[[key]]
    if ((code$frequency %||% 0L) < min_freq) next
    last_refresh <- code$last_description_refresh_at %||% 0L
    if (current_size - last_refresh >= refresh_interval) {
      needs_refresh <- c(needs_refresh, key)
    }
  }

  state$last_description_refresh_at_size <- current_size

  if (length(needs_refresh) == 0L) return(state)

  log_info(paste0(
    "Description refresh pass for ",
    length(needs_refresh), " code(s) (freq >= ", min_freq,
    "; codebook = ", current_size, " codes)"
  ))

  for (key in needs_refresh) {
    code <- state$codebook[[key]]
    segs <- code$coded_segments %||% list()
    n_seg <- length(segs)
    if (n_seg == 0L) {
      state$codebook[[key]]$last_description_refresh_at <- current_size
      next
    }

    # Deterministic sampling for
    # replay-equivalence (R7). Pre-fix `sample()` produced different
    # segment indices on every rerun, persisting different refreshed
    # descriptions in state -- the only stochastic R-side decision in
    # the per-entry loop. Use evenly-spaced indices so the same code +
    # same coded_segments always yield the same sample.
    sample_idx <- if (n_seg <= sample_segments) {
      seq_len(n_seg)
    } else {
      as.integer(round(seq(1, n_seg, length.out = sample_segments)))
    }
    sample_segs <- segs[sample_idx]

    refreshed <- .refresh_code_description(
      provider             = provider,
      code_name            = code$code_name,
      current_description  = code$description,
      sample_segments      = sample_segs,
      audit_log            = audit_log,
      response_cache       = response_cache,
      methodology_override = methodology_override
    )
    if (!is.null(refreshed)) {
      state$codebook[[key]]$description <- refreshed
    }
    state$codebook[[key]]$last_description_refresh_at <- current_size
  }

  state
}

#' Defensive code-name normalization
#'
#' strips numbered-list prefixes (`321. `), `NEW:`
#' markers, and surrounding ASCII or Unicode smart quotes that the AI may
#' echo back from the codebook-summary prompt format. Applied once at code
#' admission so the codebook key is canonical regardless of which prefix
#' the AI emitted. Idempotent: two passes handle ordering variants like
#' `"1. NEW: Burnout"` and `"NEW: 1. Burnout"`.
#'
#' @param name Character scalar; the raw code name returned by the AI.
#' @return Cleaned code name with all known prefix/quote noise removed.
#' @keywords internal
.normalize_code_name <- function(name) {
  if (is.null(name)) return(NA_character_)
  if (length(name) == 1L && is.na(name)) return(NA_character_)
  name <- as.character(name)[1]
  # Collapse Unicode smart quotes to ASCII " before the regex pass.
  # The smart quotes are constructed from raw UTF-8 bytes rather than
  # source-file literals because R's source parser replaces non-ASCII
  # literals with `<U+xxxx>` escape sequences when the running locale is
  # C/POSIX, breaking any direct-match strategy. `useBytes = TRUE` forces
  # byte-level comparison regardless of encoding tags.
  left_dq  <- rawToChar(as.raw(c(0xE2, 0x80, 0x9C)))  # U+201C "
  right_dq <- rawToChar(as.raw(c(0xE2, 0x80, 0x9D)))  # U+201D "
  left_sq  <- rawToChar(as.raw(c(0xE2, 0x80, 0x98)))  # U+2018 '
  right_sq <- rawToChar(as.raw(c(0xE2, 0x80, 0x99)))  # U+2019 '
  name <- gsub(left_dq,  "\"", name, fixed = TRUE, useBytes = TRUE)
  name <- gsub(right_dq, "\"", name, fixed = TRUE, useBytes = TRUE)
  name <- gsub(left_sq,  "'",  name, fixed = TRUE, useBytes = TRUE)
  name <- gsub(right_sq, "'",  name, fixed = TRUE, useBytes = TRUE)
  name <- trimws(name)
  # Two passes so ordering variants like "1. NEW: foo" and "NEW: 1. foo"
  # both collapse to "foo".
  for (i in 1:2) {
    name <- sub("^\\d+\\.\\s*", "", name)
    name <- sub("^NEW:\\s*", "", name, ignore.case = TRUE)
    name <- sub("^[\"']+", "", name)
    name <- sub("[\"']+$", "", name)
    name <- trimws(name)
  }
  name
}

#' per-entry semantic top-K retrieval against codebook
#'
#' Computes cosine similarity between the current entry's embedding and
#' each code's `name: description` embedding, returning the top-K indices.
#' Uses + populates `state$semantic_cache$code_embeddings` so each code is
#' embedded at most once across the full coding run. Returns
#' `integer(0)` when the provider doesn't support embeddings, when no
#' provider/entry_text is supplied, or when the API call fails. Always
#' degrades gracefully -- the caller falls back to frequency-only.
#'
#' @param state ProgressiveCodingState carrying $codebook + $semantic_cache.
#' @param code_data List of per-code records (key/name/desc/freq/type),
#'   matching the same row-order indices used by the caller.
#' @param entry_text Current entry's raw text (single character scalar).
#' @param provider AIProvider used to compute embeddings.
#' @param top_k Maximum number of semantic-retrieval indices to return.
#' @return list(indices = integer, new_embeddings = named list of vectors)
#' @keywords internal
.retrieve_semantic_codes <- function(state, code_data, entry_text, provider,
                                       top_k) {
  empty <- list(indices = integer(0), new_embeddings = list())
  if (is.null(provider) || is.null(entry_text)) return(empty)
  if (is.null(top_k) || top_k <= 0L || length(code_data) == 0L) return(empty)
  if (!nzchar(as.character(entry_text)[1])) return(empty)

  # 1. Compute entry embedding (one API call).
  entry_mat <- tryCatch(
    compute_embeddings(provider, as.character(entry_text)[1]),
    error = function(e) {
      log_debug("entry embedding failed: {e$message}")
      NULL
    }
  )
  if (is.null(entry_mat) || !is.matrix(entry_mat) || nrow(entry_mat) == 0L) {
    return(empty)
  }
  entry_emb  <- entry_mat[1, ]
  entry_norm <- sqrt(sum(entry_emb^2))
  if (!is.finite(entry_norm) || entry_norm == 0) return(empty)

  # 2. Identify codes needing fresh embeddings (cache miss). Batch the
  #    embed call to amortize HTTP overhead and stay within OpenAI's
  #    per-request limits.
  cache <- state$semantic_cache$code_embeddings %||% list()
  needs_idx  <- integer(0)
  needs_text <- character(0)
  for (i in seq_along(code_data)) {
    key <- code_data[[i]]$key
    if (is.null(cache[[key]])) {
      txt <- paste0(code_data[[i]]$name, ": ", code_data[[i]]$desc %||% "")
      needs_idx  <- c(needs_idx, i)
      needs_text <- c(needs_text, txt)
    }
  }

  new_embeddings <- list()
  if (length(needs_idx) > 0L) {
    new_mat <- tryCatch(
      compute_embeddings(provider, needs_text),
      error = function(e) {
        log_debug("code embedding batch failed: {e$message}")
        NULL
      }
    )
    if (!is.null(new_mat) && is.matrix(new_mat) &&
        nrow(new_mat) == length(needs_idx)) {
      for (j in seq_along(needs_idx)) {
        key <- code_data[[needs_idx[j]]]$key
        cache[[key]] <- new_mat[j, ]
        new_embeddings[[key]] <- new_mat[j, ]
      }
    }
  }

  # 3. Score each code by cosine similarity to entry. Codes whose
  #    embedding is still missing (batch failed) or has a mismatching
  #    dimensionality (e.g. a stale cache vector from a prior run that
  #    used a different embedding model) get -Inf and fall out.
  entry_dim <- length(entry_emb)
  sims <- vapply(seq_along(code_data), function(i) {
    emb <- cache[[code_data[[i]]$key]]
    if (is.null(emb)) return(-Inf)
    # C-6 audit LOW-1: guard against silently-recycled cosine on
    # mismatched embedding dimensions (would yield garbage scores or
    # warn). Survives across runs that switch embedding models.
    if (length(emb) != entry_dim) return(-Inf)
    emb_norm <- sqrt(sum(emb^2))
    if (!is.finite(emb_norm) || emb_norm == 0) return(-Inf)
    sum(emb * entry_emb) / (emb_norm * entry_norm)
  }, numeric(1))

  finite_n <- sum(is.finite(sims))
  if (finite_n == 0L) return(list(indices = integer(0), new_embeddings = new_embeddings))

  ordered <- order(-sims)
  ordered <- ordered[seq_len(min(top_k, finite_n))]
  list(indices = ordered, new_embeddings = new_embeddings)
}

#' codebook summary with additive semantic retrieval
#'
#' Variant of \code{.build_codebook_summary} that performs additional
#' top-K semantic retrieval against the current entry's text on top of
#' the frequency + recency selection. Returns a list so the caller can
#' persist any newly-computed code embeddings into the coding state's
#' cache (the function takes \code{state} by value -- mutating
#' \code{state$semantic_cache$code_embeddings} here would be invisible
#' to the caller).
#'
#' @return list(summary = <character>, new_embeddings = <named list of vectors>)
#' @keywords internal
.build_codebook_summary_with_retrieval <- function(state, max_codes = 150L,
                                                     recent_window = 20L,
                                                     entry_text = NULL,
                                                     provider = NULL,
                                                     top_k_semantic = 30L) {
  cb <- state$codebook
  if (length(cb) == 0) {
    return(list(summary = "", new_embeddings = list()))
  }

  code_data <- lapply(names(cb), function(key) {
    list(
      key  = key,
      name = cb[[key]]$code_name,
      freq = cb[[key]]$frequency,
      desc = cb[[key]]$description %||% "",
      type = cb[[key]]$type %||% "descriptive"
    )
  })

  freqs <- vapply(code_data, function(x) x$freq, integer(1))
  sorted_idx <- order(-freqs)

  # budget the max_codes cap across THREE selection cohorts
  # (frequency / recency / semantic) so the semantic slots aren't silently
  # truncated when frequency + recency already fill the cap. desired_top
  # falls back to legacy behavior when top_k_semantic = 0 (the back-compat
  # wrapper path used by older tests).
  recent_budget   <- min(as.integer(recent_window), length(code_data))
  semantic_budget <- max(0L, as.integer(top_k_semantic))
  desired_top     <- max(0L, as.integer(max_codes) - recent_budget - semantic_budget)
  top_idx <- sorted_idx[seq_len(min(desired_top, length(sorted_idx)))]

  all_keys <- names(cb)
  recent_keys <- tail(all_keys, recent_window)
  recent_idx <- which(names(cb) %in% recent_keys)

  # Semantic top-K (additive). With provider+entry_text NULL or top_k=0
  # this returns integer(0) and falls back to freq+recency only.
  semantic_result <- .retrieve_semantic_codes(state, code_data, entry_text,
                                                provider, top_k_semantic)

  # Union the three cohorts (freq -> recent -> semantic); ties broken by
  # first-appearance order so frequency leads. Cap at max_codes as the
  # final budget.
  selected <- unique(c(top_idx, recent_idx, semantic_result$indices))
  selected <- selected[seq_len(min(max_codes, length(selected)))]

  # Bare bullet format: no numeric prefix, no quotes.
  lines <- vapply(selected, function(i) {
    d <- code_data[[i]]
    desc_str <- if (!is.null(d$desc) && !is.na(d$desc) && nchar(d$desc) > 0) {
      paste0(" -- ", substr(d$desc, 1, 80))
    } else ""
    sprintf("  - %s (freq=%d, type=%s)%s", d$name, d$freq, d$type, desc_str)
  }, character(1))

  list(
    summary        = paste(lines, collapse = "\n"),
    new_embeddings = semantic_result$new_embeddings
  )
}

#' Codebook-summary builder (legacy interface, frequency + recency only)
#'
#' Back-compat wrapper around .build_codebook_summary_with_retrieval that
#' returns just the summary string. Used by existing tests and any caller
#' that doesn't have an entry_text / provider to do semantic retrieval.
#' Default \code{max_codes = 80} preserves the earlier behavior for
#' callers that don't override it; the production callsite in
#' \code{.code_entry_progressive} uses the with_retrieval variant directly
#' with \code{max_codes = 150}.
#'
#' @keywords internal
.build_codebook_summary <- function(state, max_codes = 80, recent_window = 20) {
  .build_codebook_summary_with_retrieval(
    state, max_codes = max_codes, recent_window = recent_window,
    entry_text = NULL, provider = NULL, top_k_semantic = 0L
  )$summary
}

# ==============================================================================
# Saturation detection helpers
# ==============================================================================
# the legacy .ai_saturation_check() (binary novel_patterns_remaining
# yes/no) was replaced by .ai_judge_saturation() (3-valued verdict +
# articulation requirement) in R/saturation_arbiter.R. The helper below
# (generate_saturation_plot) is unchanged.

#' Generate saturation curve plot
#'
#' Creates a PNG plot showing cumulative codes vs coded entries,
#' with the saturation point marked if reached.
#'
#' @param state ProgressiveCodingState with saturation data
#' @param output_dir Directory to save the plot
#' @param methodology_mode Optional character. When supplied,
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

  # The analytic sample is the entries that actually received >=1 surviving
  # code -- not merely the entries that were "not skipped". An entry examined
  # but left with no code (all segments dropped by quote verification, too
  # short, or normalized to empty) contributes nothing to the code -> theme
  # cascade or the correlations, and including it would inflate every
  # prevalence denominator in the report.
  coded_ids <- names(state$entry_results)[
    vapply(state$entry_results, function(r) {
      if (isTRUE(r$skipped)) return(FALSE)
      ca <- r$codes_assigned
      any(!is.na(ca) & nzchar(as.character(ca)))
    }, logical(1))
  ]

  filtered <- data[data$std_id %in% coded_ids, ]
  log_info("Analytic sample: {nrow(filtered)}/{nrow(data)} entries received >=1 code")
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
