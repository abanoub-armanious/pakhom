# Regression tests for the Batch-1 result-integrity fixes (audit 2026-06-11).
# Each test pins a specific bug that produced wrong published numbers.

# --- coverage / analytic sample: examined-but-uncoded entries ----------------
test_that("an examined-but-uncoded entry is counted separately, not as 'coded'", {
  state <- create_coding_state()
  state$entry_results[["e1"]] <- list(  # genuinely coded
    codes_assigned = "c1", coded_segments = list(), skipped = FALSE,
    skip_reason = NA_character_, failure = FALSE
  )
  state$entry_results[["e2"]] <- list(  # AI skipped
    codes_assigned = character(0), coded_segments = list(), skipped = TRUE,
    skip_reason = "no applicable content", failure = FALSE
  )
  state$entry_results[["e3"]] <- list(  # examined, all segments dropped -> 0 codes
    codes_assigned = character(0), coded_segments = list(), skipped = FALSE,
    skip_reason = NA_character_, failure = FALSE
  )
  data <- tibble::tibble(
    std_id = c("e1", "e2", "e3"),
    std_text = c("alpha text", "beta text", "gamma text")
  )

  cov <- compute_corpus_coverage(state, data)
  expect_equal(cov$n_processed, 3L)
  expect_equal(cov$n_skipped, 1L)
  expect_equal(cov$n_coded, 1L)              # was 2 before the fix (e3 counted)
  expect_equal(cov$n_examined_no_codes, 1L)  # e3 lives here now

  # get_analytic_sample uses the same definition: only e1 survives.
  samp <- get_analytic_sample(state, data)
  expect_equal(nrow(samp), 1L)
  expect_equal(samp$std_id, "e1")
})

# --- create_theme_set: colliding membership keys are disambiguated -----------
test_that("create_theme_set disambiguates themes whose names collapse to the same make.names key", {
  ts <- suppressWarnings(create_theme_set(list(
    list(id = 1, name = "Focus/Mood", codes_included = c("a")),
    list(id = 2, name = "Focus.Mood", codes_included = c("b"))  # same make.names() key
  )))
  keys <- vapply(ts$themes, function(t) make.names(t$name), character(1))
  expect_equal(length(unique(keys)), 2L)               # no shared membership column
  expect_false(identical(ts$themes[[1]]$name, ts$themes[[2]]$name))
})

# --- subtheme membership is exact, not substring -----------------------------
test_that(".compute_subtheme_statistics uses exact subtheme membership (no substring bleed)", {
  theme <- list(
    name = "T",
    subthemes = list(
      create_subtheme(name = "Focus", description = "", codes = character(0)),
      create_subtheme(name = "Deep-work quality", description = "", codes = character(0))
    )
  )
  data <- tibble::tibble(
    std_id = c("e1", "e2", "e3"),
    std_text = c("x", "y", "z"),
    theme_membership_T = c(1L, 1L, 1L),
    subtheme_assignments = c("Focus", "Deep-work quality", "Deep-work quality"),
    score = c(10, 20, 30)
  )
  stats <- pakhom:::.compute_subtheme_statistics(theme, data, metric_cols = "score")
  # "Focus" must match ONLY e1 (n=1). The old substring grepl() matched all
  # three because "Focus" is a substring of "Deep-work quality" -> n=3.
  expect_equal(stats[["Focus"]]$n, 1L)
  expect_equal(stats[["Deep-work quality"]]$n, 2L)
})
