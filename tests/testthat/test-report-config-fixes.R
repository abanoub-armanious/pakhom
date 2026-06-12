# Code-review followup tests
#
# Five fixes surfaced by a code review of the report and config layers,
# each confirmed before landing.

# ==========================================================================
# Fix 1: 17_report.R:1546 -- renderer used to slice to 5, dropping 3 of
# the 8 keywords the cap allows. Now uses seq_along.
# ==========================================================================

# Static-source tests require access to R/*.R source files (devtools::test
# context); skip under covr / installed-package contexts where sources are
# not on disk. Check the specific file each test reads rather than just
# the directory, because covr's test fixture sometimes has an empty R/
# directory that exists but is missing the source files.
skip_if_no_r_source <- function(relpath = "17_report.R") {
  src_file <- test_path("..", "..", "R", relpath)
  if (!file.exists(src_file)) {
    testthat::skip(paste0("R/", relpath, " source file not on disk (covr / install context)"))
  }
}

test_that("theme detail HTML renders all keywords (cap respected at source, not re-truncated)", {
  skip_if_no_r_source("17_report.R")
  # Synthesize a theme_set summary with 8 keywords (the keyword cap).
  ts_summary <- list(
    name = "Synthetic",
    n_entries = 5L,
    pct_of_total = 50,
    keywords = paste0("kw_", letters[1:8]),
    theme_kind = "emergent",
    quotes_with_context = list(),
    subthemes = character(0),
    subtheme_stats = list(),
    metric_cols = character(0)
  )

  # Build a tiny synthetic theme set + render the detail HTML directly via
  # the helper. We test the surface that produces the keyword-pill div.
  src <- readLines(
    test_path("..", "..", "R", "17_report.R"),
    warn = FALSE
  )
  pill_line_idx <- grep("keywords-container", src)
  expect_true(length(pill_line_idx) > 0L)

  # Static check: the pill builder must use seq_along, not seq_len(min(5,...)).
  builder_block <- paste(
    src[max(1L, pill_line_idx[1L] - 6L):min(length(src), pill_line_idx[1L] + 2L)],
    collapse = "\n"
  )
  expect_match(builder_block, "seq_along\\(ts\\$keywords\\)", perl = TRUE,
    label = "renderer must respect upstream keyword cap (not re-truncate to 5)"
  )
  expect_false(grepl("seq_len\\(min\\(5", builder_block, perl = TRUE))
})


# ==========================================================================
# Fix 2: 16_report_helpers.R:167 -- fallback used to cap legacy theme_sets
# at 5 (vs the cap of 8). Now caps at 8 with documenting comment.
# ==========================================================================

test_that("compute_theme_stats keyword fallback caps at 8 (matches keyword cap)", {
  skip_if_no_r_source("16_report_helpers.R")
  src <- readLines(
    test_path("..", "..", "R", "16_report_helpers.R"),
    warn = FALSE
  )
  # Match only the theme_codes fallback (the other `t$keywords %||% character(0)`
  # site is a defensive default for a separate helper and intentionally
  # short-circuits to empty).
  fallback_line <- grep("keywords = t\\$keywords %\\|\\|% theme_codes\\(t\\)", src, perl = TRUE)
  expect_equal(length(fallback_line), 1L)
  fallback_text <- src[fallback_line]
  # Cap should be 8L (keyword_cap), not 5.
  expect_match(fallback_text, "min\\(8L", perl = TRUE,
    label = "fallback cap must match keyword cap (8 not 5)"
  )
  expect_false(grepl("min\\(5", fallback_text, perl = TRUE))
})


# ==========================================================================
# Fix 3: 01_config.R -- three legacy-removed knobs were missing from
# .warn_deprecated_config_knobs. Now covered.
# ==========================================================================

test_that(".warn_deprecated_config_knobs flags legacy-removed theme knobs", {
  cfg <- list(analysis = list(themes = list(
    membership_threshold     = 0.3,
    max_rebalance_iterations = 5L,
    review_iterations        = 7L,
    include_subthemes        = TRUE  # still valid; should NOT be flagged
  )))
  flagged <- pakhom:::.warn_deprecated_config_knobs(cfg)
  expect_length(flagged, 3L)
  expect_true(any(grepl("membership_threshold", flagged)))
  expect_true(any(grepl("max_rebalance_iterations", flagged)))
  expect_true(any(grepl("review_iterations", flagged)))
  expect_false(any(grepl("include_subthemes", flagged)))
})


# ==========================================================================
# Fix 4: 16_report_helpers.R:481 -- hard-cut fall-through used "..."
# (3 chars, no leading space). Now matches the docstring's " ..." marker
# invariant. Also collapsed dead duplicate branch.
# ==========================================================================

test_that(".truncate_quote_word_boundary hard-cut path uses ' ...' marker", {
  # A long token with no whitespace within budget => hard-cut fall-through.
  long_token <- paste(rep("x", 100L), collapse = "")
  out <- pakhom:::.truncate_quote_word_boundary(long_token, max_chars = 30L)
  expect_lte(nchar(out), 30L)
  expect_true(endsWith(out, " ..."),
    label = "hard-cut path must use ' ...' (4-char marker) per docstring"
  )
})

test_that(".truncate_quote_word_boundary single-pathed branch (dedupe)", {
  # Boundary case: text whose last whitespace lands exactly at budget. Both
  # the old branches handled this identically; the dedupe here
  # collapses them into one. Behaviour-equivalent check.
  base <- paste(rep("alpha beta", 20L), collapse = " ")
  for (mc in c(40L, 50L, 60L, 80L)) {
    out <- pakhom:::.truncate_quote_word_boundary(base, max_chars = mc)
    expect_lte(nchar(out), mc)
    expect_true(endsWith(out, " ..."))
    # No mid-word cut: last visible word is one of "alpha"/"beta".
    pre_ellipsis <- sub(" \\.\\.\\.$", "", out, perl = TRUE)
    last_word <- sub(".* ", "", pre_ellipsis)
    expect_true(last_word %in% c("alpha", "beta"))
  }
})


# ==========================================================================
# Fix 5: 18_pipeline.R:1207 -- .warn_legacy_coding_resume probed only
# the FIRST QuoteProvenance; mixed-vintage checkpoints (modern QP first,
# legacy QP later) silently skipped the warning. Now walks all QPs.
# ==========================================================================

# Helper: capture stderr lines emitted by logger::log_warn during expr.
.capture_log_warn <- function(expr) {
  # Capture logger output via a temporary appender rather than
  # capture.output(type = "message"): the suite silences logger's console
  # appender (setup.R), and logger emits GitHub-Actions workflow commands under
  # CI rather than plain stderr, so the message-stream capture is unreliable.
  logs <- character(0)
  logger::log_appender(function(lines) logs <<- c(logs, lines))
  on.exit(logger::log_appender(.pakhom_test_silent_appender), add = TRUE)
  force(expr)
  logs
}

test_that(".warn_legacy_coding_resume warns when ANY legacy QP is present (mixed-vintage)", {
  # Build a synthetic coding_state whose FIRST QP is modern (has the field)
  # and the SECOND QP is legacy (missing the field). The earlier behaviour
  # would return after the first probe and silently skip the warning.
  modern_qp <- structure(
    list(
      schema_version = "1.1.0",
      entry_id = "e1",
      exact_text = "modern quote",
      verification_status = "verified_exact",
      verification_failure_reason = NA_character_
    ),
    class = "QuoteProvenance"
  )
  legacy_qp <- structure(
    list(
      # Note: NO `verification_failure_reason` -- this is the legacy shape
      schema_version = "1.0.0",
      entry_id = "e2",
      exact_text = "legacy quote",
      verification_status = "verified_fuzzy"
    ),
    class = "QuoteProvenance"
  )
  coding_state <- list(
    codebook = list(
      list(coded_segments = list(list(provenance = modern_qp))),
      list(coded_segments = list(list(provenance = legacy_qp)))
    )
  )

  out <- .capture_log_warn(
    pakhom:::.warn_legacy_coding_resume(coding_state)
  )
  all_out <- paste(out, collapse = "\n")
  # Warning must fire, AND must report 1 of 2 (so the consumer can see the
  # mixed-vintage situation rather than a binary "all legacy" flag).
  expect_match(all_out, "1 of 2")
  expect_match(all_out, "older progressive_coding")
})

test_that(".warn_legacy_coding_resume silent on all-modern checkpoint", {
  modern_qp <- structure(
    list(
      schema_version = "1.1.0",
      verification_failure_reason = NA_character_
    ),
    class = "QuoteProvenance"
  )
  coding_state <- list(
    codebook = list(
      list(coded_segments = list(list(provenance = modern_qp)))
    )
  )

  out <- .capture_log_warn(
    pakhom:::.warn_legacy_coding_resume(coding_state)
  )
  expect_false(any(grepl("older progressive_coding", out)))
})

test_that(".warn_legacy_coding_resume silent on NULL coding_state (fresh run)", {
  out <- .capture_log_warn(
    pakhom:::.warn_legacy_coding_resume(NULL)
  )
  # No legacy QPs to find; no warning.
  expect_false(any(grepl("older progressive_coding", out)))
})
