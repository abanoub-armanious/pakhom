# Phase 61.3 integration: the Methodology Assistant's relevance criterion is
# injected into the coding system prompt (the upstream focus-drift fix), with
# byte-identical fallback to the prior wording when no criterion is present.
# (The full Step-2.5 pipeline path is exercised end-to-end by test-pipeline-e2e.R
# via the "methodology" mock branch.)

test_that(".build_progressive_system_prompt injects the relevance block when present", {
  cfg <- list(relevance_block = paste0(
    "## RELEVANCE CRITERION FOR THIS STUDY\n",
    "A segment is on-focus if it links medication to sleep.\n\n",
    "Code only segments that meet this relevance criterion. Adjacent context ",
    "that does not directly satisfy it should NOT be coded."))
  p <- pakhom:::.build_progressive_system_prompt(
    research_focus = "medication and sleep", concepts = NULL,
    config = cfg, learning_context = NULL)
  expect_match(p, "RELEVANCE CRITERION FOR THIS STUDY", fixed = TRUE)
  # the task framing now defers to the criterion, not the loose "applicable"
  expect_match(p, "code the segments that meet the RELEVANCE CRITERION above", fixed = TRUE)
  expect_false(grepl("code any portions applicable to the", p, fixed = TRUE))
})

test_that(".build_progressive_system_prompt keeps prior wording with NO relevance block (back-compat)", {
  p <- pakhom:::.build_progressive_system_prompt(
    research_focus = "medication and sleep", concepts = NULL,
    config = list(), learning_context = NULL)
  expect_match(p, "code any portions applicable to the", fixed = TRUE)
  expect_false(grepl("RELEVANCE CRITERION FOR THIS STUDY", p, fixed = TRUE))
})

test_that("the relevance-injection seam composes (prompt_block -> config -> coding prompt)", {
  rel <- new_relevance_criterion(
    relevance_criterion = "A segment is on-focus if it discusses medication timing.",
    on_focus_examples  = c("I take my pills at 9pm"),
    off_focus_examples = c("I went for a walk"),
    discrimination_principle = "ties to medication timing vs not",
    source = "ai")
  cfg <- list(relevance_block = relevance_criterion_prompt_block(rel))
  p <- pakhom:::.build_progressive_system_prompt("focus", NULL, cfg, NULL)
  expect_match(p, "medication timing", fixed = TRUE)
  expect_match(p, "RELEVANCE CRITERION above", fixed = TRUE)
  expect_match(p, "ON-FOCUS EXAMPLES", fixed = TRUE)
})

test_that("methodology_setup is in checkpoint step_order, upstream of coding (61.3a audit MEDIUM)", {
  mgr <- init_checkpoints(withr::local_tempdir(), config_hash = "test")
  expect_true("methodology_setup" %in% mgr$step_order)
  # Upstream of coding: invalidating a full re-run (from data_loaded) clears the
  # articulation; invalidating only coding (from progressive_coding) keeps it.
  expect_gt(match("methodology_setup", mgr$step_order),
            match("data_loaded", mgr$step_order))
  expect_lt(match("methodology_setup", mgr$step_order),
            match("progressive_coding", mgr$step_order))

  save_checkpoint(mgr, "data_loaded", list(x = 1))
  save_checkpoint(mgr, "methodology_setup", list(y = 2))
  save_checkpoint(mgr, "progressive_coding", list(z = 3))
  invalidate_checkpoints_from(mgr, "progressive_coding")
  expect_true("methodology_setup" %in% list_checkpoints(mgr)$completed)   # kept (upstream)
  invalidate_checkpoints_from(mgr, "data_loaded")
  expect_false("methodology_setup" %in% list_checkpoints(mgr)$completed)  # now cleared
})
