# Regression tests for the Batch-8 learning-from-prior-studies fixes (audit 2026-06-11).

test_that("the prior-studies codebook hierarchy is injected into the coding prompt", {
  lc <- list(for_coding_hierarchy = "## PRIOR CODEBOOK\n- sleep_quality\n- adherence")
  prompt <- pakhom:::.build_progressive_system_prompt(
    research_focus = "x", concepts = NULL, config = list(), learning_context = lc
  )
  # Previously for_coding_hierarchy was built + logged but NEVER sent to the
  # model; it must now appear in the coding prompt.
  expect_true(grepl("CODEBOOK FROM PRIOR STUDIES", prompt, fixed = TRUE))
  expect_true(grepl("sleep_quality", prompt, fixed = TRUE))
})

test_that(".empty_learning_context exposes an empty for_coding_hierarchy", {
  ec <- pakhom:::.empty_learning_context()
  expect_true("for_coding_hierarchy" %in% names(ec))
  expect_equal(ec$for_coding_hierarchy, "")
})

test_that(".infer_hierarchy recovers parent + level from path-delimited code names", {
  cb <- tibble::tibble(
    code_name = c("Sleep", "Sleep\\Insomnia", "Adherence::Reminders", "Mood > Anxiety"),
    parent_code = NA_character_,
    hierarchy_level = 0L
  )
  out <- pakhom:::.infer_hierarchy(cb)
  # Leaf becomes the code name; the parent + depth are recovered.
  expect_equal(out$code_name, c("Sleep", "Insomnia", "Reminders", "Anxiety"))
  expect_equal(out$parent_code[2], "Sleep")
  expect_equal(out$parent_code[3], "Adherence")
  expect_equal(out$parent_code[4], "Mood")
  expect_equal(out$hierarchy_level, c(0L, 1L, 1L, 1L))
})

test_that(".infer_hierarchy leaves a plain code name with '>' untouched", {
  cb <- tibble::tibble(code_name = "score>5", parent_code = NA_character_, hierarchy_level = 0L)
  out <- pakhom:::.infer_hierarchy(cb)
  expect_equal(out$code_name, "score>5")
  expect_true(is.na(out$parent_code[1]))
})
