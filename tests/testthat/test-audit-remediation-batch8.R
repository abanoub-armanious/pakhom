# Regression tests for the Batch-8 learning-from-prior-studies fixes (audit 2026-06-11).

test_that("the prior-studies codebook hierarchy is injected into the coding prompt", {
  lc <- list(for_coding_hierarchy = "## PRIOR CODEBOOK\n- focus_quality\n- adoption")
  prompt <- pakhom:::.build_progressive_system_prompt(
    research_focus = "x", concepts = NULL, config = list(), learning_context = lc
  )
  # Previously for_coding_hierarchy was built + logged but NEVER sent to the
  # model; it must now appear in the coding prompt.
  expect_true(grepl("CODEBOOK FROM PRIOR STUDIES", prompt, fixed = TRUE))
  expect_true(grepl("focus_quality", prompt, fixed = TRUE))
})

test_that(".empty_learning_context exposes an empty for_coding_hierarchy", {
  ec <- pakhom:::.empty_learning_context()
  expect_true("for_coding_hierarchy" %in% names(ec))
  expect_equal(ec$for_coding_hierarchy, "")
})

test_that(".infer_hierarchy recovers parent + level from path-delimited code names", {
  cb <- tibble::tibble(
    code_name = c("Focus", "Focus\\Distraction", "Adoption::Reminders", "Mood > Anxiety"),
    parent_code = NA_character_,
    hierarchy_level = 0L
  )
  out <- pakhom:::.infer_hierarchy(cb)
  # Leaf becomes the code name; the parent + depth are recovered.
  expect_equal(out$code_name, c("Focus", "Distraction", "Reminders", "Anxiety"))
  expect_equal(out$parent_code[2], "Focus")
  expect_equal(out$parent_code[3], "Adoption")
  expect_equal(out$parent_code[4], "Mood")
  expect_equal(out$hierarchy_level, c(0L, 1L, 1L, 1L))
})

test_that(".infer_hierarchy leaves a plain code name with '>' untouched", {
  cb <- tibble::tibble(code_name = "score>5", parent_code = NA_character_, hierarchy_level = 0L)
  out <- pakhom:::.infer_hierarchy(cb)
  expect_equal(out$code_name, "score>5")
  expect_true(is.na(out$parent_code[1]))
})
