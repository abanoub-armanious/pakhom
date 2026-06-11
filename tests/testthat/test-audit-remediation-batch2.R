# Regression tests for the Batch-2 cross-run-comparison fixes (audit 2026-06-11).

# --- compare_models pairs by MODEL, not by chronology (the critical bug) ------
test_that(".select_cross_model_pair picks the newest run and the newest DIFFERENT-model run", {
  # chronological, oldest-first; basenames are the model_used keys
  dirs   <- c("out/anthropic_run", "out/openai_run1", "out/openai_run2")
  models <- list(anthropic_run = "anthropic/claude",
                 openai_run1   = "openai/gpt",
                 openai_run2   = "openai/gpt")
  pair <- pakhom:::.select_cross_model_pair(dirs, models)
  expect_equal(basename(pair$current), "openai_run2")    # newest
  expect_equal(basename(pair$partner), "anthropic_run")  # newest run of a different model
  expect_equal(pair$current_model, "openai/gpt")
  expect_equal(pair$partner_model, "anthropic/claude")
})

test_that(".select_cross_model_pair returns no partner when every run used the same model", {
  dirs   <- c("out/r1", "out/r2")
  models <- list(r1 = "openai/gpt", r2 = "openai/gpt")
  pair   <- pakhom:::.select_cross_model_pair(dirs, models)
  expect_null(pair$partner)                              # -> fall back to latest-vs-previous
  expect_equal(basename(pair$current), "r2")
})

test_that(".select_cross_model_pair, with 3 models, pairs newest with the most-recent OTHER model", {
  dirs   <- c("out/a1", "out/b1", "out/c1")
  models <- list(a1 = "openai/gpt", b1 = "anthropic/claude", c1 = "openai/gpt")
  pair   <- pakhom:::.select_cross_model_pair(dirs, models)
  expect_equal(basename(pair$current), "c1")             # newest (openai)
  expect_equal(basename(pair$partner), "b1")             # newest non-openai (anthropic)
})

# --- theme match is code-dominant: same codes, different label -> persisted ---
test_that("two themes with identical codes but different model-chosen names are matched, not new+disappeared", {
  themes_a <- tibble::tibble(
    name = "Sleep Disruption",
    codes_included = list(c("waking_at_night", "cant_fall_asleep", "early_waking"))
  )
  themes_b <- tibble::tibble(
    name = "Nighttime Wakefulness",  # very different label, identical codes
    codes_included = list(c("waking_at_night", "cant_fall_asleep", "early_waking"))
  )
  res <- pakhom:::.match_themes_pairwise(themes_a, themes_b, threshold = 0.75)
  expect_equal(nrow(res$persisted), 1L)   # under the old 0.6*name+0.4*code blend this was 0
  expect_equal(nrow(res$new), 0L)
  expect_equal(nrow(res$disappeared), 0L)
})
