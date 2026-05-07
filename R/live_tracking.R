# ==============================================================================
# Live Tracking Artifacts (Phase 53; C3 commitment)
# ==============================================================================
# Three streamed/snapshot artifacts written DURING analysis so a researcher can
# `tail -F` or `cat` files in the run directory and watch the codebook + theme
# clustering grow in real time. Complementary to the audit log
# (ai_decisions.jsonl): the audit log is a chronological event stream of AI
# DECISIONS; the live tracker is a state-of-the-world view of the CODEBOOK
# and THEME HIERARCHY as they evolve.
#
# Three files:
#   1. code_assignments.jsonl  -- append-only event log; one JSONL line per
#      coded segment recorded in coding_state$codebook (entry_id, code_key,
#      code_name, is_new_code, segment text + offsets, timestamp).
#   2. codebook_live.json      -- atomic-rewrite snapshot of the current
#      codebook (per-code: name, description, type, frequency, entry_ids,
#      n_segments). Refreshed after every entry by default, or every N
#      entries via codebook_snapshot_every. Researchers `cat` the file to
#      see the codebook NOW.
#   3. code_to_cluster.json    -- atomic-rewrite snapshot of the cluster
#      hierarchy as the HAC tree walk produces themes. After each theme
#      decision, the file contains: themes (each with codes), pending
#      walk state (decisions so far). Refreshed at every node in the walk.
#
# The C3 commitment from the rewrite plan: "researcher wants to see, in real
# time, what entries are listed under what codes (with quoted segments
# highlighted) and what codes are listed under what clusters."
# ==============================================================================

#' Default codebook-snapshot rewrite cadence
#'
#' How many entries between codebook_live.json rewrites. Set to 1 for "after
#' every entry"; default to 1 because the snapshot is small enough that
#' atomic rewrite is cheap (10ms even for a 500-code codebook). Phase 56's
#' performance pass may revisit this.
.LIVE_CODEBOOK_SNAPSHOT_EVERY <- 1L

# ==============================================================================
# Constructor
# ==============================================================================

#' Initialize the live tracker for a run
#'
#' Creates (or truncates) the three artifact files in
#' \code{<output_dir>/live/} and returns a \code{LiveTracker} S3 object that
#' can be passed to \code{run_progressive_coding()},
#' \code{generate_themes_iterative()}, and other pipeline functions.
#'
#' Pass \code{NULL} to any of these functions to disable live tracking
#' entirely (for tests, mock pipelines, or runs that don't need it).
#'
#' @param output_dir Run directory (the parent; this function creates a
#'   \code{live/} subdirectory inside).
#' @param codebook_snapshot_every Integer; rewrite codebook_live.json every
#'   N entries (default 1 = after every entry).
#' @return \code{LiveTracker} S3 object.
#' @export
init_live_tracker <- function(output_dir,
                                codebook_snapshot_every = .LIVE_CODEBOOK_SNAPSHOT_EVERY) {
  if (is.null(output_dir) || !nzchar(output_dir)) {
    stop("init_live_tracker: output_dir must be a non-empty path", call. = FALSE)
  }

  live_dir <- file.path(output_dir, "live")
  dir.create(live_dir, recursive = TRUE, showWarnings = FALSE)

  paths <- list(
    assignments       = file.path(live_dir, "code_assignments.jsonl"),
    codebook_snapshot = file.path(live_dir, "codebook_live.json"),
    cluster_snapshot  = file.path(live_dir, "code_to_cluster.json")
  )

  # Truncate / initialize each file. JSONL gets an empty file (lines append);
  # JSON snapshots get an "uninitialized" placeholder so a `cat` early in
  # the run returns valid JSON rather than an empty file (which jq / parsers
  # error on).
  file.create(paths$assignments)
  .live_atomic_write_json(paths$codebook_snapshot, list(
    schema_version = "1.0.0",
    snapshot_time  = .live_now_iso(),
    snapshot_index = 0L,
    n_codes        = 0L,
    codes          = list()
  ))
  .live_atomic_write_json(paths$cluster_snapshot, list(
    schema_version = "1.0.0",
    snapshot_time  = .live_now_iso(),
    snapshot_index = 0L,
    walk_status    = "not_started",
    n_decisions    = 0L,
    n_themes       = 0L,
    themes         = list(),
    decisions      = list()
  ))

  # Counters live in an environment so writers can mutate them in place
  # without requiring the caller to reassign. Pass-by-value on a list
  # would silently lose counter increments (Phase 53 audit CRITICAL-1):
  # the on-disk file would still be rewritten -- atomic writes are
  # side-effects -- but tracker$n_assignments / n_codebook_snapshots /
  # n_cluster_snapshots would stay 0 across the whole run, breaking
  # the print method's diagnostic output and any downstream code that
  # reasons over the counts.
  counters <- new.env(parent = emptyenv())
  counters$n_assignments        <- 0L
  counters$n_codebook_snapshots <- 0L
  counters$n_cluster_snapshots  <- 0L

  obj <- list(
    paths                    = paths,
    codebook_snapshot_every  = as.integer(codebook_snapshot_every),
    counters                 = counters
  )
  class(obj) <- "LiveTracker"
  obj
}

#' Print method for LiveTracker
#' @param x LiveTracker object
#' @param ... ignored
#' @export
print.LiveTracker <- function(x, ...) {
  c <- x$counters
  cat(sprintf("<LiveTracker> %d assignments / %d codebook snapshots / %d cluster snapshots\n",
              c$n_assignments, c$n_codebook_snapshots, c$n_cluster_snapshots))
  cat(sprintf("  assignments: %s\n", x$paths$assignments))
  cat(sprintf("  codebook:    %s\n", x$paths$codebook_snapshot))
  cat(sprintf("  clusters:    %s\n", x$paths$cluster_snapshot))
  invisible(x)
}

# ==============================================================================
# Recording: code assignments (JSONL append)
# ==============================================================================

#' Record one (entry, code, segment) assignment to the live tracker
#'
#' Appends a JSONL line to \code{code_assignments.jsonl}. Safe to call with
#' \code{tracker = NULL} (no-op).
#'
#' @param tracker A \code{LiveTracker} or NULL
#' @param entry_id Character entry id (std_id)
#' @param code_key Codebook key (sanitized id)
#' @param code_name Human-readable code name
#' @param segment List with \code{text}, \code{start_char}, \code{end_char}
#' @param is_new_code Logical; whether this assignment created the code
#' @param entry_index Integer position of the entry in the input (for ordering)
#' @return The (possibly updated) tracker, invisibly.
#' @export
live_record_assignment <- function(tracker, entry_id, code_key, code_name,
                                      segment, is_new_code = FALSE,
                                      entry_index = NA_integer_) {
  if (is.null(tracker)) return(invisible(NULL))
  validate_class(tracker, "LiveTracker")

  # Build the JSONL record. Keep field names stable -- downstream tools
  # may parse this format.
  record <- list(
    schema_version = "1.0.0",
    event_type     = "code_assignment",
    timestamp      = .live_now_iso(),
    entry_id       = entry_id,
    entry_index    = if (is.na(entry_index)) NULL else as.integer(entry_index),
    code_key       = code_key,
    code_name      = code_name,
    is_new_code    = isTRUE(is_new_code),
    segment = list(
      text       = substr(as.character(segment$text %||% ""), 1, 500),
      start_char = as.integer(segment$start_char %||% NA_integer_),
      end_char   = as.integer(segment$end_char %||% NA_integer_),
      verification_status = segment$provenance$verification_status %||% NA_character_
    )
  )

  line <- jsonlite::toJSON(record, auto_unbox = TRUE, null = "null", force = TRUE)
  cat(line, "\n", sep = "", file = tracker$paths$assignments, append = TRUE)
  tracker$counters$n_assignments <- tracker$counters$n_assignments + 1L
  invisible(tracker)
}

# ==============================================================================
# Snapshots: codebook (atomic JSON rewrite)
# ==============================================================================

#' Snapshot the current codebook to \code{codebook_live.json}
#'
#' Atomic rewrite: writes to a temp file in the same directory, then
#' \code{file.rename}s over the live file. A researcher \code{cat}-ing the
#' file always sees a coherent snapshot (no torn read).
#'
#' Honors \code{tracker$codebook_snapshot_every}: writes only when the
#' tracker's \code{n_codebook_snapshots} would increment to a multiple of
#' the cadence (i.e., every-N-entries).
#'
#' Safe to call with \code{tracker = NULL} (no-op).
#'
#' @param tracker A \code{LiveTracker} or NULL
#' @param codebook \code{coding_state$codebook} (named list of code records)
#' @param entry_index Optional integer; used in the snapshot timestamp
#' @param force Logical; bypass the every-N gate (used at end-of-coding)
#' @return The (possibly updated) tracker, invisibly.
#' @export
live_snapshot_codebook <- function(tracker, codebook, entry_index = NA_integer_,
                                      force = FALSE) {
  if (is.null(tracker)) return(invisible(NULL))
  validate_class(tracker, "LiveTracker")

  # Cadence gate. The counter increments on every call (regardless of
  # whether the on-disk file is rewritten); the gate decides if THIS call
  # actually triggers a write. Counters live in tracker$counters (env)
  # so the increment persists across calls without reassignment.
  tracker$counters$n_codebook_snapshots <-
    tracker$counters$n_codebook_snapshots + 1L
  call_idx <- tracker$counters$n_codebook_snapshots

  if (!isTRUE(force)) {
    cadence <- tracker$codebook_snapshot_every %||% 1L
    if (cadence > 1L && call_idx %% cadence != 0L) {
      return(invisible(tracker))
    }
  }

  codes_payload <- lapply(names(codebook %||% list()), function(k) {
    cb <- codebook[[k]]
    list(
      key            = k,
      name           = cb$code_name %||% k,
      description    = cb$description %||% "",
      type           = cb$type %||% "descriptive",
      frequency      = as.integer(cb$frequency %||% 0L),
      entry_ids      = I(as.character(cb$entry_ids %||% character(0))),
      n_segments     = length(cb$coded_segments %||% list())
    )
  })

  payload <- list(
    schema_version = "1.0.0",
    snapshot_time  = .live_now_iso(),
    snapshot_index = call_idx,
    last_entry_index = if (is.na(entry_index)) NULL else as.integer(entry_index),
    n_codes        = length(codes_payload),
    codes          = codes_payload
  )

  .live_atomic_write_json(tracker$paths$codebook_snapshot, payload)
  invisible(tracker)
}

# ==============================================================================
# Snapshots: cluster hierarchy (atomic JSON rewrite)
# ==============================================================================

#' Snapshot the current theme/cluster hierarchy to \code{code_to_cluster.json}
#'
#' Called from \code{.walk_for_themes()} / \code{.walk_for_subthemes()}
#' after each AI decision. Captures the in-progress theme structure so a
#' researcher can watch the HAC tree walk produce themes in real time.
#'
#' Atomic rewrite. Safe to call with \code{tracker = NULL} (no-op).
#'
#' @param tracker A \code{LiveTracker} or NULL
#' @param walk_status One of \code{"in_progress"}, \code{"theme_walk_complete"},
#'   \code{"subtheme_walk_complete"}
#' @param walk_state The walk_state environment (or list) carrying
#'   \code{n_calls}, \code{n_failed_calls}, \code{decisions}.
#' @param themes_so_far List of in-progress theme records (each with
#'   \code{name}, \code{description}, \code{code_indices}, \code{code_keys}).
#'   Optional; the snapshot just records empty themes when the walk is
#'   mid-flight.
#' @return The (possibly updated) tracker, invisibly.
#' @export
live_snapshot_clusters <- function(tracker, walk_status,
                                       walk_state = NULL,
                                       themes_so_far = list()) {
  if (is.null(tracker)) return(invisible(NULL))
  validate_class(tracker, "LiveTracker")

  decisions <- if (is.null(walk_state)) list() else walk_state$decisions %||% list()
  n_calls   <- if (is.null(walk_state)) 0L else (walk_state$n_calls %||% 0L)

  tracker$counters$n_cluster_snapshots <-
    tracker$counters$n_cluster_snapshots + 1L

  payload <- list(
    schema_version = "1.0.0",
    snapshot_time  = .live_now_iso(),
    snapshot_index = tracker$counters$n_cluster_snapshots,
    walk_status    = walk_status,
    n_decisions    = length(decisions),
    n_themes       = length(themes_so_far),
    themes         = lapply(themes_so_far, function(t) {
      list(
        name         = t$name %||% NA_character_,
        description  = t$description %||% "",
        decision_origin = t$decision_origin %||% NA_character_,
        n_codes      = length(t$code_indices %||% integer(0)),
        code_keys    = I(as.character(t$code_keys %||% character(0)))
      )
    }),
    decisions      = lapply(decisions, function(d) {
      list(
        call_idx     = as.integer(d$call_idx %||% NA_integer_),
        level        = d$level %||% NA_character_,
        parent       = d$parent %||% NA_character_,
        n_codes      = as.integer(d$n_codes %||% 0L),
        decision     = d$decision %||% NA_character_,
        proposed_name = d$name %||% NA_character_,
        articulation_excerpt = substr(as.character(d$articulation %||% ""), 1, 300)
      )
    }),
    n_failed_calls = if (is.null(walk_state)) 0L else (walk_state$n_failed_calls %||% 0L),
    n_calls        = n_calls
  )

  .live_atomic_write_json(tracker$paths$cluster_snapshot, payload)
  invisible(tracker)
}

# ==============================================================================
# Helpers
# ==============================================================================

#' Atomic JSON write (write-temp + rename)
#' @keywords internal
.live_atomic_write_json <- function(path, data) {
  tmp <- paste0(path, ".tmp.", Sys.getpid())
  jsonlite::write_json(data, tmp, pretty = TRUE, auto_unbox = TRUE,
                        null = "null", force = TRUE)
  # file.rename is atomic on POSIX when src + dst are on the same filesystem.
  # Both tmp and path are in the same dir so this is safe.
  file.rename(tmp, path)
  invisible(NULL)
}

#' ISO 8601 UTC timestamp
#' @keywords internal
.live_now_iso <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
}
