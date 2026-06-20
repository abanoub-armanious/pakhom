# Regression tests for the Batch-12 provider/config fixes (audit 2026-06-11).

test_that(".config_defaults gives Anthropic a distinct (cheaper) fast model", {
  d <- pakhom:::.config_defaults()
  expect_false(identical(d$ai$anthropic$models$primary,
                         d$ai$anthropic$models$fast))
  # consistent with .default_models("anthropic")
  expect_identical(d$ai$anthropic$models$fast,
                   pakhom:::.default_models("anthropic")$fast)
})

test_that("the Shiny wizard build refuses to default the methodology (AC3 backstop)", {
  skip_if_not_installed("shiny")
  # No methodology_mode supplied -> must error, not silently produce Mode 2.
  expect_error(
    pakhom:::.build_config_from_inputs(list(study_name = "s", research_focus = "x")),
    "methodology"
  )
})

test_that("the Shiny wizard drops empty tokens from comma-separated concepts", {
  skip_if_not_installed("shiny")
  cfg <- pakhom:::.build_config_from_inputs(list(
    methodology_mode = "codebook_collaborative",
    study_name = "s", research_focus = "x",
    concepts = "a,,b, ,c,"
  ))
  expect_equal(unlist(cfg$study$concepts, use.names = FALSE), c("a", "b", "c"))
})
