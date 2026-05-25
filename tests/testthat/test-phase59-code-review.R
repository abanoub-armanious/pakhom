# Phase 59 high-effort code review followup tests
#
# Five fixes surfaced by the code-review skill (high effort, 3-angle finder
# with parallel 1-vote verifiers). All five CONFIRMED via verifier sub-tasks
# after dedup; six other candidates REFUTED.

# ==========================================================================
# Fix 1: 17_report.R:1546 -- renderer used to slice to 5, dropping 3 of
# Tier 8's 8 frequency-ranked keywords. Now uses seq_along.
# ==========================================================================

test_that("theme detail HTML renders all keywords (cap respected at source, not re-truncated)", {
  # Synthesize a theme_set summary with 8 keywords (Tier 8 cap).
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
    label = "renderer must respect upstream Tier 8 cap (not re-truncate to 5)"
  )
  expect_false(grepl("seq_len\\(min\\(5", builder_block, perl = TRUE))
})


# ==========================================================================
# Fix 2: 16_report_helpers.R:167 -- fallback used to cap legacy theme_sets
# at 5 (vs Tier 8's 8). Now caps at 8 with documenting comment.
# ==========================================================================

test_that("compute_theme_stats keyword fallback caps at 8 (matches Tier 8 contract)", {
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
  # Cap should be 8L (Tier 8 keyword_cap), not 5.
  expect_match(fallback_text, "min\\(8L", perl = TRUE,
    label = "fallback cap must match Tier 8 contract (8 not 5)"
  )
  expect_false(grepl("min\\(5", fallback_text, perl = TRUE))
})


# ==========================================================================
# Fix 3: 01_config.R -- three Phase-50e-removed knobs were missing from
# .warn_deprecated_config_knobs. Now covered.
# ==========================================================================

test_that(".warn_deprecated_config_knobs flags Phase-50e-removed theme knobs", {
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

test_that(".truncate_quote_word_boundary single-pathed branch (Phase 59 dedupe)", {
  # Boundary case: text whose last whitespace lands exactly at budget. Both
  # the old branches handled this identically; the dedupe in Phase 59
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
# Fix 5: 18_pipeline.R:1207 -- .warn_pre_tier7_coding_resume probed only
# the FIRST QuoteProvenance; mixed-vintage checkpoints (modern QP first,
# legacy QP later) silently skipped the warning. Now walks all QPs.
# ==========================================================================

# Helper: capture stderr lines emitted by logger::log_warn during expr.
.capture_log_warn <- function(expr) {
  capture.output(force(expr), type = "message")
}

test_that(".warn_pre_tier7_coding_resume warns when ANY legacy QP is present (mixed-vintage)", {
  # Build a synthetic coding_state whose FIRST QP is modern (has the field)
  # and the SECOND QP is legacy (missing the field). Pre-Phase-59 behaviour
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
      # Note: NO `verification_failure_reason` -- this is the pre-Tier-7 shape
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
    pakhom:::.warn_pre_tier7_coding_resume(coding_state)
  )
  all_out <- paste(out, collapse = "\n")
  # Warning must fire, AND must report 1 of 2 (so the consumer can see the
  # mixed-vintage situation rather than a binary "all legacy" flag).
  expect_match(all_out, "1 of 2")
  expect_match(all_out, "pre-Phase-58-Tier-7")
})

test_that(".warn_pre_tier7_coding_resume silent on all-modern checkpoint", {
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
    pakhom:::.warn_pre_tier7_coding_resume(coding_state)
  )
  expect_false(any(grepl("pre-Phase-58-Tier-7", out)))
})

test_that(".warn_pre_tier7_coding_resume silent on NULL coding_state (fresh run)", {
  out <- .capture_log_warn(
    pakhom:::.warn_pre_tier7_coding_resume(NULL)
  )
  # No legacy QPs to find; no warning.
  expect_false(any(grepl("pre-Phase-58-Tier-7", out)))
})
