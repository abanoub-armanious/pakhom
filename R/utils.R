# ==============================================================================
# Utility Functions
# ==============================================================================
#
# ERROR HANDLING STRATEGY
# ==============================================================================
# This package uses context-appropriate error handling, not a uniform mechanism.
# Each module follows the strategy best suited to its role in the pipeline:
#
#   1. FAIL FAST (stop()) -- Configuration & validation
#      validate_config(), validate_data_columns(), validate_class()
#      Rationale: Invalid config or missing columns mean the pipeline cannot
#      run correctly. Accumulate all errors first, then fail with a single
#      multi-line message so the researcher fixes everything in one pass.
#
#   2. RETRY WITH BACKOFF (tryCatch + loop) -- AI API calls
#      ai_complete() retries up to 3 times with exponential backoff (2^n sec,
#      capped at 60s) for rate limits (429), 1s delay for other transient errors.
#      Rationale: API calls are expensive in pipeline time. Transient failures
#      (rate limits, network blips) are common and worth retrying before giving up.
#
#   3. GRACEFUL DEGRADATION (tryCatch -> NULL) -- Batch operations
#      Relevance filtering, sentiment analysis, and individual entry coding
#      catch per-batch or per-item errors, log a warning, and continue.
#      Missing results become NA values in output columns.
#      Rationale: One failed batch out of 20 shouldn't kill a 25-minute pipeline.
#      Downstream code is NA-tolerant.
#
#   4. PROGRESSIVE REPAIR (multi-tier fallback) -- JSON parsing
#      parse_json_safely() attempts: direct parse -> close brackets -> truncate
#      to last complete element -> extract largest valid JSON subset.
#      Returns NULL only when all repair strategies fail.
#      Rationale: AI output is unpredictable. Aggressive repair recovers the
#      majority of malformed responses without re-calling the API.
#
#   5. TIERED FALLBACK (per-entry) -- Progressive coding
#      run_progressive_coding() retries on AI failure and falls back to
#      skipping entries that cannot be coded after max retries.
#      Rationale: A skipped entry is acceptable; a crashed pipeline is not.
#
#   6. CONTINUE ON ERROR (tryCatch -> warn + skip) -- Optional pipeline steps
#      Scraping, manuscript learning, comparison, and network plots catch
#      errors, log a warning, and continue with what's available.
#      Rationale: These are non-essential enrichments. A scraping failure
#      shouldn't prevent analysis of existing data.
#
#   7. CORRUPTION RECOVERY -- Checkpoint system
#      If a manifest file is corrupted, start fresh rather than crashing.
#      Re-read manifest from disk each time (R is pass-by-value).
#      Rationale: Pipeline resilience > checkpoint integrity. A corrupted
#      manifest only costs one re-run of already-completed steps.
#
# All strategies log via the logger package at appropriate levels:
#   DEBUG: Internal details (batch numbers, retry attempts)
#   INFO:  Normal progress (step completion, counts)
#   WARN:  Recoverable issues (skipped items, fallbacks triggered)
#   ERROR: Fatal issues (config invalid, all retries exhausted)
# ==============================================================================

#' Create a safe filename from a string
#' @param name Input string
#' @return Filesystem-safe string
make_safe_filename <- function(name) {
  safe <- gsub("[^A-Za-z0-9 _-]", "", name)
  safe <- gsub("\\s+", "_", trimws(safe))
  safe <- tolower(safe)
  if (nchar(safe) > 80) safe <- substr(safe, 1, 80)
  if (nchar(safe) == 0) safe <- "unnamed"
  safe
}

#' Create an HTML anchor ID from a string
#' @param name Input string
#' @return Anchor-safe string
make_anchor_id <- function(name) {
  id <- gsub("[^A-Za-z0-9 -]", "", name)
  id <- gsub("\\s+", "-", trimws(id))
  tolower(id)
}

#' Truncate text to specified length with ellipsis
#' @param text Input string
#' @param max_length Maximum characters
#' @return Truncated string
truncate_text <- function(text, max_length = 200) {
  if (is.na(text) || nchar(text) <= max_length) return(text)
  paste0(substr(text, 1, max_length - 3), "...")
}

#' Generate a unique run ID based on timestamp (UTC).
#'
#' Always emitted in UTC so two researchers in different timezones running the
#' same analysis at the same wall-clock moment produce comparable run IDs.
#'
#' @return Character string like "run_2026-02-23_143052"
generate_run_id <- function() {
  paste0("run_", format(Sys.time(), "%Y-%m-%d_%H%M%S", tz = "UTC"))
}

#' Run an expression under a fixed RNG seed (withr optional)
#'
#' Mirrors \code{withr::with_seed()} when the suggested \code{withr} package is
#' installed; otherwise saves, sets, and restores the global RNG manually so the
#' result is reproducible without leaving the caller's RNG stream perturbed.
#' This keeps reproducibility working even when \code{withr} (Suggests) is absent,
#' so callers never hard-depend on it.
#'
#' @param seed Integer seed.
#' @param code Expression evaluated under \code{seed} (lazily evaluated).
#' @return The value of \code{code}.
#' @keywords internal
#' @noRd
.with_seed <- function(seed, code) {
  if (requireNamespace("withr", quietly = TRUE)) {
    return(withr::with_seed(seed, code))
  }
  # Fallback: replicate withr::with_seed -- seed the RNG for `code`, then
  # restore the caller's prior RNG state so the global stream is untouched.
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  } else {
    on.exit(
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      },
      add = TRUE
    )
  }
  set.seed(seed)
  code
}

#' Create a progress bar that works in non-interactive/background mode
#'
#' When R runs without a terminal (e.g., background jobs), the progress
#' package can hang. This returns a no-op progress bar in that case.
#'
#' For CRAN, the \code{progress} package is now Suggests
#' rather than Imports. When it isn't installed (or the session is
#' non-interactive), this falls through to a logger-based progress
#' indicator -- no functional regression, just no spinning bar.
#'
#' @param format Progress bar format string
#' @param total Total number of ticks
#' @return A progress_bar object or a no-op list with a $tick() method
#' @keywords internal
safe_progress_bar <- function(format, total) {
  has_progress <- requireNamespace("progress", quietly = TRUE)
  if (has_progress && (interactive() || isatty(stderr()))) {
    progress::progress_bar$new(format = format, total = total,
                                  clear = FALSE)
  } else {
    # No-op progress bar for non-interactive/background mode (or when
    # the progress package isn't installed, since it's now Suggests).
    counter <- 0L
    list(tick = function() {
      counter <<- counter + 1L
      if (counter %% 10 == 0 || counter == total) {
        log_info("  Progress: {counter}/{total} ({round(100 * counter / total)}%)")
      }
    })
  }
}

#' Null-coalescing operator (re-export from rlang)
#' @name null-coalesce
#' @keywords internal
`%||%` <- rlang::`%||%`

# ==============================================================================
# Concept Context Builder — Dynamic multi-concept support
# ==============================================================================

# ==============================================================================
# Token Estimation & Dynamic Batching
# ==============================================================================

#' Estimate token count for text
#'
#' Uses a script-aware character-to-token heuristic (~4 chars/token for
#' Latin/Cyrillic scripts, ~1.5 chars/token for CJK; mixed scripts use a
#' weighted average). Sufficient for batch-size budgeting where a small
#' over- or under-estimate is harmless.
#'
#' @param text Character string(s) to estimate
#' @param model Reserved for future per-model tuning (currently unused).
#' @return Integer vector of estimated token counts
#' @keywords internal
estimate_tokens <- function(text, model = "gpt-4o") {
  # Script-aware heuristic
  # CJK characters are typically 1-2 tokens each (~1.5 chars/token)
  # Latin/Cyrillic scripts average ~4 chars/token
  # Mixed scripts use weighted average
  vapply(text, function(t) {
    if (is.na(t) || nchar(t) == 0) return(0L)
    chars <- nchar(t)

    # Count CJK characters (Unicode ranges for CJK Unified Ideographs + common CJK blocks)
    cjk_count <- nchar(gsub("[^\u4e00-\u9fff\u3040-\u309f\u30a0-\u30ff\uac00-\ud7af]", "", t))

    if (cjk_count == 0) {
      # Pure Latin/Cyrillic: ~4 chars per token
      as.integer(ceiling(chars / 4))
    } else if (cjk_count > chars * 0.5) {
      # Predominantly CJK: ~1.5 chars per token
      as.integer(ceiling(chars / 1.5))
    } else {
      # Mixed: weighted average
      latin_chars <- chars - cjk_count
      as.integer(ceiling(latin_chars / 4) + ceiling(cjk_count / 1.5))
    }
  }, integer(1), USE.NAMES = FALSE)
}

#' Compute dynamic batch indices based on token budget
#'
#' Splits a set of text entries into batches that respect a maximum token
#' budget per batch. Longer entries get fewer per batch; shorter entries
#' get more. The fixed \code{max_batch_size} acts as a ceiling.
#'
#' @param texts Character vector of text entries
#' @param max_batch_tokens Maximum tokens per batch (for the entries portion)
#' @param max_batch_size Hard ceiling on entries per batch (fallback/safety)
#' @param chars_per_entry Max characters that will be used per entry in the prompt
#'   (e.g., 800 for sentiment batching). Entries are virtually truncated to
#'   this length for token estimation.
#' @return List of integer vectors, each containing row indices for one batch
#' @keywords internal
compute_dynamic_batches <- function(texts, max_batch_tokens, max_batch_size = 50,
                                     chars_per_entry = 1500) {
  # Guard against NA texts (replace with empty string for token estimation)
  texts[is.na(texts)] <- ""
  truncated <- substr(texts, 1, chars_per_entry)
  token_est <- estimate_tokens(truncated)
  # Ensure all token estimates are at least 1 to prevent infinite loops
  token_est <- pmax(token_est, 1L)

  batches <- list()
  current_batch <- vector("integer", length(token_est))
  batch_len <- 0L
  current_tokens <- 0L

  for (i in seq_along(token_est)) {
    if (batch_len >= max_batch_size ||
        (current_tokens + token_est[i] > max_batch_tokens && batch_len > 0)) {
      batches[[length(batches) + 1L]] <- current_batch[seq_len(batch_len)]
      current_batch <- vector("integer", length(token_est))
      batch_len <- 0L
      current_tokens <- 0L
    }
    batch_len <- batch_len + 1L
    current_batch[batch_len] <- i
    current_tokens <- current_tokens + token_est[i]
  }

  if (batch_len > 0) {
    batches[[length(batches) + 1L]] <- current_batch[seq_len(batch_len)]
  }

  batches
}
