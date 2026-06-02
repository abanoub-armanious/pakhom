#!/usr/bin/env Rscript
# ==============================================================================
# Phase 63 -- shared theme-structure metrics for the v2 singleton-merge tune.
#
# Sourced by phase63_full_run.R and phase63_ab_validation.R. Pure measurement,
# no API. Every metric is read off a returned ThemeSet (no re-computation).
#
# The two load-bearing metrics for the #3 tune:
#   * single_code_rate -- #(themes with exactly 1 code) / #themes. MUST FALL
#     (or stay 0 where already 0) under the steer = the efficacy signal.
#   * max_share        -- largest theme's code count / total codes. MUST NOT
#     RISE materially = the kitchen-sink/over-merge guardrail (Phase-48
#     catastrophe was 0.82; a healthy Phase-57 run was 0.058).
# ==============================================================================

# `%||%` and theme_code_keys() come from the loaded pakhom namespace.

.p63_theme_sizes <- function(themes) {
  # Codes per theme. v2 partitions codes disjointly across themes, so these
  # sum to the total codebook size. theme_code_keys() is the Phase-51 getter.
  vapply(themes, function(th) {
    k <- tryCatch(length(theme_code_keys(th)), error = function(e) NA_integer_)
    as.integer(k)
  }, integer(1))
}

.p63_theme_metrics <- function(ts, kinds = NULL) {
  empty <- list(n_themes = 0L, total_codes = 0L, single_code_rate = NA_real_,
                n_single = 0L, max_share = NA_real_, max_theme_n = NA_integer_,
                sizes = integer(0), names = character(0),
                converged_at = NA, n_passes = NA)
  if (is.null(ts) || is.null(ts$themes) || length(ts$themes) == 0L) return(empty)

  themes <- ts$themes
  if (!is.null(kinds)) {
    keep <- vapply(themes, function(th) {
      tk <- tryCatch(as.character(th$theme_kind %||% "framework")[1],
                     error = function(e) "framework")
      tk %in% kinds
    }, logical(1))
    themes <- themes[keep]
  }
  if (length(themes) == 0L) return(empty)

  sizes <- .p63_theme_sizes(themes)
  total <- sum(sizes, na.rm = TRUE)
  list(
    n_themes         = length(themes),
    total_codes      = total,
    single_code_rate = if (length(sizes)) mean(sizes == 1L, na.rm = TRUE) else NA_real_,
    n_single         = sum(sizes == 1L, na.rm = TRUE),
    max_share        = if (isTRUE(total > 0)) max(sizes, na.rm = TRUE) / total else NA_real_,
    max_theme_n      = if (length(sizes)) max(sizes, na.rm = TRUE) else NA_integer_,
    sizes            = sizes,
    names            = vapply(themes, function(th) as.character(th$name %||% "")[1], character(1)),
    converged_at     = tryCatch(ts$merge_history$converged_at_pass %||% NA, error = function(e) NA),
    n_passes         = tryCatch(ts$merge_history$n_substantive_passes %||% NA, error = function(e) NA)
  )
}

# Is the #3 steer present in the loaded v2 source? Self-documents which arm a
# results file came from (old = FALSE, new = TRUE). The signature phrase is
# unique to the approved steer wording.
.p63_steer_present <- function(pkg_dir) {
  f <- file.path(pkg_dir, "R", "theme_algorithm_v2.R")
  if (!file.exists(f)) return(NA)
  any(grepl("more SPECIFIC INSTANCE", readLines(f, warn = FALSE), fixed = TRUE))
}

# Sum gpt-4o token cost from an ai_decisions.jsonl (the audit-log token fields
# are usage_prompt / usage_completion -- the 61.5 lesson). gpt-4o: $2.50/1M in,
# $10/1M out.
.p63_cost_from_audit <- function(jsonl_path) {
  if (!file.exists(jsonl_path)) return(list(prompt = 0, completion = 0, usd = 0))
  ls <- readLines(jsonl_path, warn = FALSE)
  pt <- ct <- 0
  for (l in ls) {
    o <- tryCatch(jsonlite::fromJSON(l), error = function(e) NULL); if (is.null(o)) next
    p  <- suppressWarnings(as.numeric(o$usage_prompt))
    cc <- suppressWarnings(as.numeric(o$usage_completion))
    if (length(p)  == 1L && !is.na(p))  pt <- pt + p
    if (length(cc) == 1L && !is.na(cc)) ct <- ct + cc
  }
  list(prompt = pt, completion = ct, usd = pt / 1e6 * 2.5 + ct / 1e6 * 10)
}
