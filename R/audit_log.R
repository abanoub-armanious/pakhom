# ==============================================================================
# AI Decision Audit Trail — JSONL Logging for Transparency
# ==============================================================================
# Records every AI decision (code assignments, new codes, merges, sentiment
# scores, saturation signals, theme structures, insights) as one JSON line per
# event in {output_dir}/ai_decisions.jsonl.
#
# The audit log enables post-hoc review of how the AI reasoned at each step,
# supporting reflexive practice and reproducibility.
# ==============================================================================

# -- Valid enums ---------------------------------------------------------------

#' Audit log schema version
#'
#' every audit record stamps this version so a
#' downstream replayer / cross-run comparator can detect schema drift.
#' An earlier design had ai_decisions.jsonl as the only first-class artifact
#' lacking a version stamp (live tracker artifacts all carry
#' schema_version="1.0.0"). Bump this constant when the record schema
#' changes incompatibly.
#'
#' \itemize{
#'   \item 1.0.0: initial stamping. Schema includes
#'     timestamp, step, decision_type, methodology_mode (when set),
#'     plus arbitrary user-supplied \code{...} fields.
#' }
#' @keywords internal
.AUDIT_LOG_SCHEMA_VERSION <- "1.0.0"

.valid_audit_steps <- c(
  # Pre-T1.4 pipeline steps
  "coding", "sentiment", "theming", "saturation", "insight", "synthesis",
  "researcher_review",
  # T1.4 additions: declared up-front so the schema is ready when the Phase
  # B/C/D items that emit these step kinds land. The audit-log validator
  # would otherwise reject them as unknown steps.
  "provocateur",          # M1.1+ : reflexive_scaffold provocateur questioning
  "memo",                  # M1.3  : reflexive memos as data
  "positionality",         # M1.4  : repeated/dynamic positionality
  "reflexivity",           # M1.5  : reflexivity-collapse detection
  "mode_change",           # T1.5  : methodology re-declaration with parent_run_id
  "quote_verification",    # T0.1  : Tier-0 quote provenance verification ladder
  "coverage",              # T0.3  : Tier-0 corpus-coverage computation
  "methodology_assistant"  # Step 2.5 relevance + metric articulation
)

.valid_decision_types <- c(
  # Pre-T1.4 decision types. "saturation_signal" is retained in the
  # allowlist for back-compat: audit logs from earlier runs use it,
  # and replay_run() validates each historical line against this
  # allowlist. Newer runs emit "saturation_judgment" (see below)
  # instead; "saturation_signal" should not be written by new code.
  "code_assignment", "new_code_created", "entry_skipped", "merge_decision",
  "sentiment_assignment", "saturation_signal", "theme_structure",
  "insight_generation",
  # Researcher review decision types
  "code_renamed", "code_deleted", "code_merged", "code_split",
  "code_description_updated", "theme_renamed", "theme_deleted",
  "theme_merged", "theme_restructured", "theme_created",
  "review_memo_added", "review_disposition",
  # T1.4 additions: see the matching audit-step comments above for the future
  # items each new decision_type belongs to. Declaring them now means
  # downstream callers in Phases B/C/D don't bounce off validation when they
  # land. ai_request is the most-called new type -- emitted by log_ai_request()
  # for every ai_complete() call.
  "ai_request",                       # T1.4  : every ai_complete() call
  "provocation_emitted",              # M1.1
  "memo_added",                       # M1.3
  "positionality_recorded",           # M1.4
  "reflexivity_collapse_detected",    # M1.5
  "mode_changed",                     # T1.5
  "quote_verified",                   # T0.1
  "quote_fabricated",                 # T0.1
  "quote_drifted",                    # T0.1: source corpus changed since attribution
  "coverage_failure",                 # T0.3: corpus-coverage computation failed
  "cluster_decision",                 # legacy v1 HAC tree-walk per-node verdict
  "framework_revision_suggested",     # revise policy wrote framework_review.csv
  "saturation_judgment",              # AI arbiter verdict (reached/not_yet/uncertain)
  "clustering_proposal",              # v2 per-pass clustering proposal (continue|converged)
  "label_pass",                       # v2 post-convergence labeling pass
  "relevance_criterion",              # methodology-assistant relevance articulation
  "metric_interpretation",            # methodology-assistant per-metric interpretation
  "research_coverage"                 # research-question coverage assessment
)

# -- Constructor ---------------------------------------------------------------

#' Initialize the AI decision audit log
#'
#' Opens (or creates) a JSONL file at \code{{output_dir}/ai_decisions.jsonl} and
#' returns an \code{AuditLog} S3 object that can be passed to
#' \code{\link{log_ai_decision}} throughout the pipeline.
#'
#' If the file already exists it is opened in append mode so that resumed runs
#' continue the same log.
#'
#' T1.4 additions:
#' \itemize{
#'   \item Accepts \code{config} so methodology metadata can flow into every
#'     audit record. When \code{config$methodology$mode} is set,
#'     \code{\link{log_ai_decision}} auto-stamps it on every JSONL record.
#'     This is the load-bearing change for cross-mode comparison: every
#'     decision in the log is unambiguously attributable to the methodology
#'     it was made under.
#'   \item Counter state (\code{n_written}) is held in an internal environment
#'     so increments from \code{\link{log_ai_decision}} mutate correctly
#'     across function calls. (Pre-T1.4, \code{n_written} was a plain list
#'     field that suffered R's pass-by-value semantics and stayed at 0
#'     forever -- a latent bug in \code{close_audit_log}'s "N decisions
#'     recorded" log message. Fixed here.)
#' }
#'
#' @param output_dir Character. Base output directory for the current run.
#' @param config A ThematicConfig (or NULL). When non-NULL,
#'   \code{config$methodology$mode} is captured and auto-stamped on every
#'   subsequent audit record.
#' @return An \code{AuditLog} S3 object (a list with class attribute).
#' @export
init_audit_log <- function(output_dir, config = NULL) {
  stopifnot(is.character(output_dir), length(output_dir) == 1L)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  log_path <- file.path(output_dir, "ai_decisions.jsonl")

  con <- tryCatch(
    file(log_path, open = "a"),
    error = function(e) {
      logger::log_error("Failed to open audit log at {log_path}: {e$message}")
      stop("Cannot open audit log: ", e$message, call. = FALSE)
    }
  )

  methodology_mode <- if (!is.null(config)) config$methodology$mode else NULL

  # Environment-backed counter: survives pass-by-value into log_ai_decision().
  state <- new.env(parent = emptyenv())
  state$n_written <- 0L

  audit <- list(
    path             = log_path,
    con              = con,
    output_dir       = output_dir,
    methodology_mode = methodology_mode,
    state            = state
  )

  class(audit) <- "AuditLog"

  logger::log_info("Audit log initialised: {log_path}")
  if (!is.null(methodology_mode)) {
    logger::log_info("  methodology_mode auto-stamped on every record: {methodology_mode}")
  }
  audit
}

# -- Logging -------------------------------------------------------------------

#' Record a single AI decision in the audit log
#'
#' Appends one JSON line to the JSONL audit file.
#'
#' @param audit  An \code{AuditLog} object returned by
#'   \code{\link{init_audit_log}}.
#' @param step   Character. Pipeline step — one of \code{"coding"},
#'   \code{"sentiment"}, \code{"theming"}, \code{"saturation"},
#'   \code{"insight"}, or \code{"synthesis"}.
#' @param decision_type Character. Type of decision — one of
#'   \code{"code_assignment"}, \code{"new_code_created"},
#'   \code{"entry_skipped"}, \code{"merge_decision"},
#'   \code{"sentiment_assignment"}, \code{"saturation_signal"},
#'   \code{"theme_structure"}, or \code{"insight_generation"}.
#' @param ... Additional named fields to include in the JSON record (e.g.
#'   \code{entry_id}, \code{code_name}, \code{rationale}, \code{model},
#'   \code{tokens_used}).
#'
#' @return Invisibly returns \code{audit} (for pipe-friendly usage).
#' @export
log_ai_decision <- function(audit, step, decision_type, ...) {
  # ---- Input validation ------------------------------------------------------
  stopifnot(inherits(audit, "AuditLog"))

  if (!step %in% .valid_audit_steps) {
    stop(
      "Invalid audit step '", step, "'. Must be one of: ",
      paste(.valid_audit_steps, collapse = ", "),
      call. = FALSE
    )
  }
  if (!decision_type %in% .valid_decision_types) {
    stop(
      "Invalid decision_type '", decision_type, "'. Must be one of: ",
      paste(.valid_decision_types, collapse = ", "),
      call. = FALSE
    )
  }

  # ---- Build the record ------------------------------------------------------
  # emit timestamps in UTC so cross-file
  # ordering (ai_decisions.jsonl + live/*.json + fabrication_log.csv)
  # doesn't require parsing each record's TZ offset. The %z format
  # specifier still carries the offset for back-compat with parsers
  # that expect ISO 8601 with offset. An earlier audit log used
  # the system's local TZ while the live tracker already used UTC
  # -- normalizing both to UTC makes the audit trail mergeable
  # without per-record TZ resolution.
  # schema_version is
  # the FIRST field in every record, matching live tracker
  # convention (see live_record_assignment in R/live_tracking.R).
  # An earlier audit log had it as the second field, which
  # inconsistency surfaced to any downstream consumer that read
  # records positionally.
  base_fields <- list(
    schema_version = .AUDIT_LOG_SCHEMA_VERSION,
    timestamp      = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z", tz = "UTC"),
    step           = step,
    decision_type  = decision_type
  )

  # T1.4: Auto-stamp methodology_mode if it was captured at init_audit_log().
  # Records written before any methodology was declared (e.g., from older
  # callers in pre-T1.4 test fixtures) silently omit this field for
  # back-compat; summarize_audit_log() handles missing methodology_mode by
  # reporting it as <unset>.
  if (!is.null(audit$methodology_mode)) {
    base_fields$methodology_mode <- audit$methodology_mode
  }

  extra  <- list(...)
  record <- c(base_fields, extra)

  # ---- Write one JSON line ---------------------------------------------------
  tryCatch({
    json_line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null")
    writeLines(json_line, con = audit$con)
    flush(audit$con)
    # T1.4: env-backed counter mutates correctly (pre-T1.4 bug fix; see
    # init_audit_log() docs).
    audit$state$n_written <- audit$state$n_written + 1L
    logger::log_debug(
      "Audit [{audit$state$n_written}]: {step}/{decision_type}"
    )
  }, error = function(e) {
    logger::log_error(
      "Failed to write audit record ({step}/{decision_type}): {e$message}"
    )
  })

  invisible(audit)
}

#' Record an AI request with the structured response from \code{ai_complete}
#'
#' Convenience wrapper around \code{\link{log_ai_decision}} that records an
#' \code{"ai_request"} decision and unpacks the structured fields from
#' \code{\link{ai_complete}}'s return value (T1.1) into the audit record:
#' model, finish_reason, prompt_hash, request_id, and per-record token usage.
#'
#' If a \code{ResponseCache} is provided, the
#' \code{raw_response} is also written to the cache (content-addressable by
#' \code{prompt_hash}) and the cache path is recorded in the audit log entry.
#' This separation keeps the JSONL file lightweight while preserving the full
#' API response for replay.
#'
#' Silently no-ops on \code{NULL ai_result} (e.g., when \code{ai_complete}
#' threw and the caller's tryCatch returned NULL) so callers can wrap calls in
#' \code{tryCatch} and still call \code{log_ai_request} unconditionally.
#'
#' @param audit An \code{AuditLog} object.
#' @param step Pipeline step (e.g., \code{"coding"}, \code{"sentiment"}).
#'   Must be one of \code{.valid_audit_steps}.
#' @param ai_result The list returned by \code{\link{ai_complete}}.
#' @param response_cache Optional \code{\link{init_response_cache}} object.
#'   When provided, the raw_response is written to the cache and the path
#'   is recorded in the JSONL record.
#' @param ... Additional caller-specific named fields to include in the
#'   record (e.g., \code{entry_id = "abc"}, \code{batch_idx = 3}).
#' @return Invisibly returns \code{audit}.
#' @export
log_ai_request <- function(audit, step, ai_result, response_cache = NULL, ...) {
  stopifnot(inherits(audit, "AuditLog"))
  if (is.null(ai_result)) return(invisible(audit))

  # Cache raw_response if a cache is provided. Returns NA_character_ when the
  # cache is disabled or the write fails -- either way the request is still recorded
  # rest of the audit fields so the request appears in the log.
  cached_path <- NA_character_
  if (!is.null(response_cache)) {
    cached_path <- cache_response(response_cache, ai_result)
  }

  log_ai_decision(
    audit, step, "ai_request",
    model             = ai_result$model %||% NA_character_,
    finish_reason     = ai_result$finish_reason %||% NA_character_,
    prompt_hash       = ai_result$prompt_hash %||% NA_character_,
    request_id        = ai_result$request_id %||% NA_character_,
    usage_prompt      = ai_result$usage$prompt_tokens %||% NA_integer_,
    usage_completion  = ai_result$usage$completion_tokens %||% NA_integer_,
    usage_total       = ai_result$usage$total_tokens %||% NA_integer_,
    raw_response_path = cached_path,
    ...
  )
}

# -- Teardown ------------------------------------------------------------------

#' Close the audit log file connection
#'
#' Flushes any buffered output and closes the underlying file connection.
#'
#' @param audit An \code{AuditLog} object.
#' @return Invisibly returns \code{NULL}.
#' @export
close_audit_log <- function(audit) {
  stopifnot(inherits(audit, "AuditLog"))

  # Idempotent: callers (explicit close + on.exit safety net) may invoke
  # this twice for the same audit log. Silently no-op if the underlying
  # connection has already been closed/invalidated.
  con_open <- tryCatch(isOpen(audit$con), error = function(e) FALSE)
  if (!isTRUE(con_open)) {
    return(invisible(NULL))
  }

  tryCatch({
    close(audit$con)
    n_written <- if (is.environment(audit$state)) audit$state$n_written else 0L
    logger::log_info(
      "Audit log closed ({n_written} decisions recorded): {audit$path}"
    )
  }, error = function(e) {
    logger::log_warn("Error closing audit log connection: {e$message}")
  })

  invisible(NULL)
}

# -- Summary -------------------------------------------------------------------

#' Summarize the AI decision audit log
#'
#' Reads \code{{output_dir}/ai_decisions.jsonl} and produces a summary of all
#' recorded decisions. Useful for post-analysis review and reporting.
#'
#' T1.4 additions to the returned list: \code{total_ai_requests},
#' \code{total_tokens_used}, \code{ai_requests_by_model}, and
#' \code{methodology_modes_observed}. Older audit logs missing these fields
#' return zero/empty values for the new keys; pre-T1.4 records still surface
#' in \code{decisions_by_type}/\code{decisions_by_step} as before.
#'
#' @param output_dir Character. The same output directory passed to
#'   \code{\link{init_audit_log}}.
#' @return A named list with:
#' \describe{
#'   \item{total_decisions}{Integer — total number of logged decisions.}
#'   \item{decisions_by_type}{Named integer vector — counts per
#'     \code{decision_type}.}
#'   \item{decisions_by_step}{Named integer vector — counts per pipeline
#'     \code{step}.}
#'   \item{new_codes_timeline}{A \code{data.frame} with columns
#'     \code{timestamp} and \code{cumulative_codes}, showing the running total
#'     of new codes over time.}
#'   \item{entries_skipped}{Integer — number of \code{entry_skipped} decisions.}
#'   \item{merge_decisions_accepted}{Integer — merge decisions where
#'     \code{action == "merge"}.}
#'   \item{merge_decisions_standalone}{Integer — merge decisions where
#'     \code{action == "standalone"}.}
#'   \item{total_ai_requests}{Integer (T1.4) — count of \code{ai_request}
#'     records (one per \code{ai_complete} call).}
#'   \item{total_tokens_used}{Integer (T1.4) — sum of \code{usage_total}
#'     across all \code{ai_request} records (NA values dropped from the sum).}
#'   \item{ai_requests_by_model}{Named integer vector (T1.4) — \code{ai_request}
#'     counts per model name.}
#'   \item{methodology_modes_observed}{Character vector (T1.4) — unique
#'     non-NA values of the \code{methodology_mode} field across all records.
#'     Should normally be length-1 or length-0; length >1 indicates a run
#'     where the methodology was changed mid-pipeline (T1.5 mode_change flow).}
#' }
#' @export
summarize_audit_log <- function(output_dir) {
  log_path <- file.path(output_dir, "ai_decisions.jsonl")

  if (!file.exists(log_path)) {
    logger::log_warn("No audit log found at {log_path}")
    return(.empty_audit_summary())
  }

  # ---- Read lines ------------------------------------------------------------
  lines <- tryCatch(
    readLines(log_path, warn = FALSE),
    error = function(e) {
      logger::log_error("Cannot read audit log: {e$message}")
      return(character(0))
    }
  )
  lines <- lines[nzchar(trimws(lines))]

  if (length(lines) == 0L) {
    logger::log_info("Audit log is empty: {log_path}")
    return(.empty_audit_summary())
  }

  # ---- Parse JSON lines ------------------------------------------------------
  records <- lapply(lines, function(ln) {
    tryCatch(
      jsonlite::fromJSON(ln, simplifyVector = TRUE),
      error = function(e) {
        logger::log_debug("Skipping malformed audit line: {e$message}")
        NULL
      }
    )
  })
  records <- Filter(Negate(is.null), records)

  if (length(records) == 0L) {
    logger::log_warn("All audit lines were malformed")
    return(.empty_audit_summary())
  }

  # ---- Aggregate -------------------------------------------------------------
  types <- vapply(records, function(r) r$decision_type %||% NA_character_,
                  character(1))
  steps <- vapply(records, function(r) r$step %||% NA_character_,
                  character(1))

  decisions_by_type <- table(types[!is.na(types)])
  decisions_by_step <- table(steps[!is.na(steps)])

  # New-codes timeline
  new_code_idx <- which(types == "new_code_created")
  if (length(new_code_idx) > 0L) {
    ts <- vapply(records[new_code_idx],
                 function(r) r$timestamp %||% NA_character_,
                 character(1))
    new_codes_timeline <- data.frame(
      timestamp        = ts,
      cumulative_codes = seq_along(ts),
      stringsAsFactors = FALSE
    )
  } else {
    new_codes_timeline <- data.frame(
      timestamp        = character(0),
      cumulative_codes = integer(0),
      stringsAsFactors = FALSE
    )
  }

  # Merge decisions
  merge_idx <- which(types == "merge_decision")
  merge_actions <- vapply(records[merge_idx],
                          function(r) r$action %||% NA_character_,
                          character(1))

  # T1.4: ai_request stats. ai_request records carry usage_total, model, and
  # methodology_mode (auto-stamped). Missing fields tolerated for back-compat
  # with pre-T1.4 logs.
  ai_idx <- which(types == "ai_request")
  ai_models <- vapply(records[ai_idx],
                      function(r) r$model %||% NA_character_,
                      character(1))
  ai_tokens <- vapply(records[ai_idx],
                      function(r) {
                        v <- r$usage_total %||% NA_integer_
                        if (is.null(v)) NA_integer_ else as.integer(v[[1]])
                      },
                      integer(1))
  ai_models_table <- table(ai_models[!is.na(ai_models)])

  # methodology_modes_observed across all records (not just ai_request).
  # length-0 means no record had the field (pre-T1.4 logs); length-1 is the
  # normal post-T1.4 single-mode run; length-2+ flags a mid-pipeline mode
  # change (T1.5 flow).
  methodology_modes <- vapply(records,
                              function(r) r$methodology_mode %||% NA_character_,
                              character(1))
  methodology_modes_observed <- unique(methodology_modes[!is.na(methodology_modes)])

  list(
    total_decisions          = length(records),
    decisions_by_type        = as.integer(decisions_by_type) |>
      stats::setNames(names(decisions_by_type)),
    decisions_by_step        = as.integer(decisions_by_step) |>
      stats::setNames(names(decisions_by_step)),
    new_codes_timeline       = new_codes_timeline,
    entries_skipped          = sum(types == "entry_skipped", na.rm = TRUE),
    merge_decisions_accepted  = sum(merge_actions == "merge", na.rm = TRUE),
    merge_decisions_standalone = sum(merge_actions == "standalone", na.rm = TRUE),
    # T1.4 additions
    total_ai_requests        = length(ai_idx),
    total_tokens_used        = sum(ai_tokens, na.rm = TRUE),
    ai_requests_by_model     = as.integer(ai_models_table) |>
      stats::setNames(names(ai_models_table)),
    methodology_modes_observed = methodology_modes_observed
  )
}

# -- Internal helpers ----------------------------------------------------------

#' Return an empty audit summary structure
#' @keywords internal
.empty_audit_summary <- function() {
  list(
    total_decisions          = 0L,
    decisions_by_type        = stats::setNames(integer(0), character(0)),
    decisions_by_step        = stats::setNames(integer(0), character(0)),
    new_codes_timeline       = data.frame(
      timestamp        = character(0),
      cumulative_codes = integer(0),
      stringsAsFactors = FALSE
    ),
    entries_skipped          = 0L,
    merge_decisions_accepted  = 0L,
    merge_decisions_standalone = 0L,
    # T1.4 additions: zero/empty defaults match the populated-summary shape
    total_ai_requests          = 0L,
    total_tokens_used          = 0L,
    ai_requests_by_model       = stats::setNames(integer(0), character(0)),
    methodology_modes_observed = character(0)
  )
}
