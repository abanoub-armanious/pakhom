# Integration: the Methodology Assistant's relevance criterion is
# injected into the coding system prompt (the upstream focus-drift fix), with
# byte-identical fallback to the prior wording when no criterion is present.
# (The full Step-2.5 pipeline path is exercised end-to-end by test-pipeline-e2e.R
# via the "methodology" mock branch.)

test_that(".build_progressive_system_prompt injects the relevance block when present", {
  cfg <- list(relevance_block = paste0(
    "## RELEVANCE CRITERION FOR THIS STUDY\n",
    "A segment is on-focus if it links scheduling to focus.\n\n",
    "Code only segments that meet this relevance criterion. Adjacent context ",
    "that does not directly satisfy it should NOT be coded."))
  p <- pakhom:::.build_progressive_system_prompt(
    research_focus = "scheduling and focus", concepts = NULL,
    config = cfg, learning_context = NULL)
  expect_match(p, "RELEVANCE CRITERION FOR THIS STUDY", fixed = TRUE)
  # the task framing now defers to the criterion, not the loose "applicable"
  expect_match(p, "code the segments that meet the RELEVANCE CRITERION above", fixed = TRUE)
  expect_false(grepl("code any portions applicable to the", p, fixed = TRUE))
})

test_that(".build_progressive_system_prompt keeps prior wording with NO relevance block (back-compat)", {
  p <- pakhom:::.build_progressive_system_prompt(
    research_focus = "scheduling and focus", concepts = NULL,
    config = list(), learning_context = NULL)
  expect_match(p, "code any portions applicable to the", fixed = TRUE)
  expect_false(grepl("RELEVANCE CRITERION FOR THIS STUDY", p, fixed = TRUE))
})

test_that("the relevance-injection seam composes (prompt_block -> config -> coding prompt)", {
  rel <- new_relevance_criterion(
    relevance_criterion = "A segment is on-focus if it discusses meeting load.",
    on_focus_examples  = c("I block my calendar at 9pm"),
    off_focus_examples = c("I went for a walk"),
    discrimination_principle = "ties to meeting load vs not",
    source = "ai")
  cfg <- list(relevance_block = relevance_criterion_prompt_block(rel))
  p <- pakhom:::.build_progressive_system_prompt("focus", NULL, cfg, NULL)
  expect_match(p, "meeting load", fixed = TRUE)
  expect_match(p, "RELEVANCE CRITERION above", fixed = TRUE)
  expect_match(p, "ON-FOCUS EXAMPLES", fixed = TRUE)
})

test_that("methodology_setup is in checkpoint step_order, upstream of coding (61.3a MEDIUM)", {
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

# ---- 61.3b: per-subtheme stats reroute through AI-chosen primitives ----------

test_that(".metric_interpretation_record finds a column's record by name (61.3b)", {
  mi <- new_metric_interpretation(metrics = list(
    list(column_name = "score", column_description = "heavy-tailed",
         requested_primitives = list(list(primitive = "prim_median", rationale = "r")),
         interpretation_note = "use median")))
  expect_equal(pakhom:::.metric_interpretation_record(mi, "score")$column_name, "score")
  expect_null(pakhom:::.metric_interpretation_record(mi, "upvote_ratio"))  # no record
  expect_null(pakhom:::.metric_interpretation_record(NULL, "score"))        # no interpretation
})

test_that(".compute_requested_primitives dispatches via compute_metric_stat, fail-honest (61.3b)", {
  rec <- list(column_name = "score", column_description = "heavy-tailed count",
              requested_primitives = list(
                list(primitive = "prim_median",   rationale = "robust"),
                list(primitive = "prim_p90",      rationale = "tail"),
                list(primitive = "prim_unknownx", rationale = "gap")),   # R4 fail-honest
              interpretation_note = "median + p90; the mean is misleading")
  res <- pakhom:::.compute_requested_primitives(rec, c(0, 1, 5, 100, 2))
  expect_equal(res$interpretation_note, "median + p90; the mean is misleading")
  expect_length(res$requested, 3L)
  expect_equal(res$requested[[1]]$value, stats::median(c(0, 1, 5, 100, 2)))
  expect_true(res$requested[[1]]$available)
  expect_false(res$requested[[3]]$available)        # unknown primitive -> available=FALSE
})

test_that(".compute_subtheme_statistics adds AI primitives per interpreted column + keeps legacy (61.3b)", {
  theme <- list(name = "T", description = "d", subthemes = list(
    create_subtheme(name = "S1", description = "s1",
                    codes = list(create_code_object(key = "k", name = "K")))))
  data <- tibble::tibble(
    std_id = paste0("e", 1:5), std_text = paste("quote", 1:5),
    score        = c(0, 1, 5, 100, 2),                 # interpreted (median + p90)
    upvote_ratio = c(0.5, 0.9, 1.0, 0.97, 0.8),        # NOT interpreted -> legacy only
    theme_membership_T = rep(1L, 5L),
    subtheme_assignments = rep("S1", 5L))
  mi <- new_metric_interpretation(metrics = list(
    list(column_name = "score", column_description = "heavy-tailed",
         requested_primitives = list(list(primitive = "prim_median", rationale = "r"),
                                     list(primitive = "prim_p90",    rationale = "r")),
         interpretation_note = "median + p90")))
  out <- pakhom:::.compute_subtheme_statistics(
    theme = theme, data = data, metric_cols = c("score", "upvote_ratio"),
    metric_interpretation = mi)
  rec <- out[["S1"]]
  # legacy battery is ALWAYS present, for BOTH columns (renderer fallback)
  expect_equal(rec$metric_stats$score$median, 2)
  expect_false(is.null(rec$metric_stats$upvote_ratio))
  # AI stats ONLY for the interpreted column (per-column fallback, N1/N2)
  expect_true("score" %in% names(rec$ai_metric_stats))
  expect_false("upvote_ratio" %in% names(rec$ai_metric_stats))
  expect_equal(rec$ai_metric_stats$score$interpretation_note, "median + p90")
  expect_setequal(vapply(rec$ai_metric_stats$score$requested,
                         function(x) x$primitive, character(1)),
                  c("prim_median", "prim_p90"))
})

test_that(".compute_subtheme_statistics is back-compat with NULL interpretation (61.3b)", {
  theme <- list(name = "T", description = "d", subthemes = list(
    create_subtheme(name = "S1", description = "s1",
                    codes = list(create_code_object(key = "k", name = "K")))))
  data <- tibble::tibble(
    std_id = paste0("e", 1:3), std_text = paste("q", 1:3), score = c(1, 2, 3),
    theme_membership_T = rep(1L, 3L), subtheme_assignments = rep("S1", 3L))
  out <- pakhom:::.compute_subtheme_statistics(
    theme = theme, data = data, metric_cols = "score")  # no metric_interpretation
  rec <- out[["S1"]]
  expect_equal(rec$metric_stats$score$median, 2)        # legacy battery intact
  expect_length(rec$ai_metric_stats, 0L)                # empty -> renderer unchanged
})
