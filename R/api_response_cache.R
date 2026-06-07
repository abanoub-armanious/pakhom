# ==============================================================================
# Content-Addressable Raw Response Cache
# ==============================================================================
# Stores raw API responses on disk indexed by prompt_hash so replay_run()
# can recover a prior response from the audit log without re-issuing the
# request. Inline storage in the JSONL audit log was rejected because typical
# raw responses are 2-10 KB and a 1000-call run would balloon ai_decisions.jsonl
# to hundreds of MB; content-addressable external storage keeps the JSONL light,
# deduplicates identical requests across the run (the saturation_check prompt
# in particular fires repeatedly with identical inputs), and gives replay_run()
# a clean key-to-file lookup.
#
# Cache layout:
#   {output_dir}/{response_cache_dir}/{prompt_hash}.json
#
# The prompt_hash is a SHA-256 hex digest computed by .compute_prompt_hash() in
# R/02_ai_providers.R; collisions are practically impossible at this hash size.
# Files are pretty-printed JSON for human readability during debugging; the
# size cost is negligible relative to the response payload itself.
#
# State (n_written, n_dedup_skipped) is held in an environment because R lists
# are pass-by-value -- mutating cache$n_written from inside cache_response()
# would only affect the local copy and the caller's counter would never update.
# An environment-backed counter is reference-typed and mutates correctly.
# ==============================================================================

#' Initialize a content-addressable response cache
#'
#' Creates the cache directory under the run output directory and returns a
#' \code{ResponseCache} S3 object that can be passed to
#' \code{\link{cache_response}} and \code{\link{read_cached_response}}.
#'
#' If \code{config$audit$capture_raw_responses} is \code{FALSE} (a power-user
#' opt-out), the cache is created in disabled mode: write/read calls are no-ops
#' and the cache directory is not created. This matches the conservative
#' default in \code{default_config()} (\code{capture_raw_responses = TRUE}).
#'
#' @param output_dir Character. Run output directory (where ai_decisions.jsonl
#'   lives). The cache lives at \code{output_dir/response_cache_dir/}.
#' @param config A ThematicConfig (or NULL). Reads
#'   \code{config$audit$capture_raw_responses} (default TRUE) and
#'   \code{config$audit$response_cache_dir} (default \code{"api_responses"}).
#' @return A ResponseCache S3 object.
#' @export
init_response_cache <- function(output_dir, config = NULL) {
  stopifnot(is.character(output_dir), length(output_dir) == 1L)

  capture_enabled <- TRUE
  cache_subdir    <- "api_responses"
  if (!is.null(config)) {
    capture_enabled <- isTRUE(config$audit$capture_raw_responses %||% TRUE)
    cache_subdir    <- config$audit$response_cache_dir %||% "api_responses"
  }

  cache_dir <- file.path(output_dir, cache_subdir)
  if (capture_enabled) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Environment-backed counters survive pass-by-value into cache_response().
  state <- new.env(parent = emptyenv())
  state$n_written       <- 0L
  state$n_dedup_skipped <- 0L

  cache <- list(
    output_dir   = output_dir,
    cache_dir    = cache_dir,
    cache_subdir = cache_subdir,
    enabled      = capture_enabled,
    state        = state
  )
  class(cache) <- "ResponseCache"

  if (capture_enabled) {
    log_info("Response cache initialised: {cache_dir}")
  } else {
    log_info("Response cache DISABLED (audit.capture_raw_responses = FALSE)")
  }

  cache
}

#' Write a raw API response to the cache, indexed by prompt_hash
#'
#' If the cache is disabled, returns \code{NA_character_} without writing.
#' If a file with the same prompt_hash already exists (i.e., the same request
#' was made earlier in the run), the write is skipped (deduplication) and
#' the existing relative path is returned. Otherwise, writes the
#' \code{raw_response} as pretty-printed JSON.
#'
#' Errors during write are caught and logged; the function returns
#' \code{NA_character_} on failure rather than propagating, because audit-log
#' capture should never break the analysis pipeline.
#'
#' @param cache A ResponseCache object from \code{\link{init_response_cache}}.
#' @param ai_result The structured list returned by \code{\link{ai_complete}}.
#'   Must contain \code{prompt_hash} (used as the key) and \code{raw_response}
#'   (the payload to cache).
#' @return Character. Path to the cached response file, RELATIVE to
#'   \code{output_dir} (so audit log records remain portable when the run
#'   directory is moved). \code{NA_character_} if the cache is disabled,
#'   \code{ai_result} is malformed, or the write fails.
#' @export
cache_response <- function(cache, ai_result) {
  stopifnot(inherits(cache, "ResponseCache"))
  if (!isTRUE(cache$enabled))                 return(NA_character_)
  if (is.null(ai_result))                     return(NA_character_)
  if (is.null(ai_result$prompt_hash))         return(NA_character_)
  if (is.null(ai_result$raw_response))        return(NA_character_)

  filename  <- paste0(ai_result$prompt_hash, ".json")
  full_path <- file.path(cache$cache_dir, filename)
  rel_path  <- file.path(cache$cache_subdir, filename)

  if (file.exists(full_path)) {
    cache$state$n_dedup_skipped <- cache$state$n_dedup_skipped + 1L
    return(rel_path)
  }

  ok <- tryCatch({
    jsonlite::write_json(
      ai_result$raw_response, full_path,
      pretty = TRUE, auto_unbox = TRUE, force = TRUE, null = "null"
    )
    TRUE
  }, error = function(e) {
    log_warn("Failed to cache response {ai_result$prompt_hash}: {e$message}")
    FALSE
  })

  if (!isTRUE(ok)) return(NA_character_)
  cache$state$n_written <- cache$state$n_written + 1L
  rel_path
}

#' Read a cached raw response by prompt_hash
#'
#' Looks up a previously-cached response. Used by \code{replay_run()}
#' (planned) to reproduce a prior run's AI calls from on-disk artifacts.
#'
#' @param cache A ResponseCache object
#' @param prompt_hash Character SHA-256 hex digest (from
#'   \code{ai_result$prompt_hash} or an audit log record).
#' @return The parsed \code{raw_response} list that was cached, or \code{NULL}
#'   if the cache is disabled or no matching file exists.
#' @export
read_cached_response <- function(cache, prompt_hash) {
  stopifnot(inherits(cache, "ResponseCache"))
  if (!isTRUE(cache$enabled)) return(NULL)
  if (is.null(prompt_hash) || !is.character(prompt_hash) ||
      length(prompt_hash) != 1L || !nzchar(prompt_hash)) {
    return(NULL)
  }

  full_path <- file.path(cache$cache_dir, paste0(prompt_hash, ".json"))
  if (!file.exists(full_path)) return(NULL)

  tryCatch(
    jsonlite::fromJSON(full_path, simplifyVector = FALSE),
    error = function(e) {
      log_warn("Failed to read cached response {prompt_hash}: {e$message}")
      NULL
    }
  )
}

#' Print method for ResponseCache
#' @param x ResponseCache object
#' @param ... Ignored
#' @export
print.ResponseCache <- function(x, ...) {
  cat("ResponseCache\n")
  cat(sprintf("  Enabled:     %s\n", x$enabled))
  cat(sprintf("  Directory:   %s\n", x$cache_dir))
  if (isTRUE(x$enabled)) {
    cat(sprintf("  Written:     %d response(s)\n", x$state$n_written))
    cat(sprintf("  Dedup skips: %d\n", x$state$n_dedup_skipped))
  }
  invisible(x)
}
