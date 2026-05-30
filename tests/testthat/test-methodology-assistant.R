# Tests for R/methodology_assistant.R + the two Phase 61.2 schemas
#
# The Methodology Assistant (Step 2.5): the AI articulates a relevance criterion
# and per-metric interpretations, with FREE-STRING primitive requests (no enum /
# no menu, honoring the package's no-hardcoding principle). Covers the schemas,
# the S3 objects + serialization round-trip, the AI callers (mocked), the
# orchestrator's discovery + replay paths, the archive, and the coding-prompt
# injection block.

# ---- fixtures ----------------------------------------------------------------

.methodology_test_data <- function(n = 30L) {
  base <- as.POSIXct("2024-01-01 18:00:00", tz = "UTC")
  tibble::tibble(
    std_id        = paste0("e", seq_len(n)),
    std_text      = rep(c("medication keeps me up at night",
                          "binge episodes decreased on the new dose",
                          "went for a run this morning"),
                        length.out = n),
    std_timestamp = as.character(base + (seq_len(n) - 1L) * 3600 * 7),
    score         = as.numeric(rep(c(0, 1, 5, 100, 2), length.out = n)),
    upvote_ratio  = round(rep(c(0.5, 0.9, 1.0, 0.97), length.out = n), 2),
    source_table  = rep(c("posts", "comments"), length.out = n)  # internal-excluded
  )
}

.relevance_json <- function() jsonlite::toJSON(list(
  research_focus_paraphrase = "How psychiatric medication affects sleep and binge eating.",
  relevance_criterion = paste0(
    "A segment is on-focus when it links a psychiatric medication to either ",
    "sleep or binge-eating behavior -- timing, side effects, dosage, or efficacy."),
  on_focus_examples = list("The pills keep me up at night",
                           "My cravings dropped after the dose change"),
  off_focus_examples = list("I went for a run today", "The weather has been nice"),
  discrimination_principle = "On-focus segments tie medication to sleep or eating; adjacent ones mention only one in isolation."
), auto_unbox = TRUE)

.metrics_json <- function() jsonlite::toJSON(list(
  metrics = list(
    list(column_name = "score", column_description = "Heavy-tailed upvote count.",
         requested_primitives = list(
           list(primitive = "prim_median", rationale = "robust center for skewed counts"),
           list(primitive = "prim_p90", rationale = "honest high end"),
           list(primitive = "prim_made_up_index", rationale = "wish this existed")),  # unknown -> R4
         interpretation_note = "Report median and 90th percentile; the mean is misleading."),
    list(column_name = "upvote_ratio", column_description = "Bounded proportion in [0,1].",
         requested_primitives = list(
           list(primitive = "prim_median", rationale = "robust center for a bounded ratio")),
         interpretation_note = "Most posts are highly upvoted.")
  ),
  temporal_columns = list(
    list(column_name = "std_timestamp", column_description = "Posting timestamps.",
         requested_primitives = list(
           list(primitive = "prim_hour_of_day_distribution", rationale = "time-of-day rhythm")),
         interpretation_note = "Posting peaks in the evening.")
  )
), auto_unbox = TRUE)

# A single mocked ai_complete that returns the right response per call, keyed off
# the user prompt (relevance prompt contains "CORPUS SAMPLE"; metric prompt does
# not). Lets the orchestrator test exercise both calls in sequence.
.mock_ai <- function(relevance = .relevance_json(), metrics = .metrics_json()) {
  function(...) {
    a <- list(...)
    up <- tryCatch(as.character(a[[2]]), error = function(e) "")
    content <- if (length(up) >= 1L && grepl("CORPUS SAMPLE", up[1], fixed = TRUE)) {
      relevance
    } else {
      metrics
    }
    list(content = content, model = "gpt-mock", request_id = "r1",
         usage = list(prompt_tokens = 100L, completion_tokens = 50L, total_tokens = 150L),
         finish_reason = "stop", raw_response = list(), prompt_hash = "h",
         citations = list())
  }
}

.mk_articulations <- function(source = "ai") {
  rel <- new_relevance_criterion(
    research_focus_paraphrase = "para",
    relevance_criterion = "A segment is on-focus if it links medication to sleep or eating.",
    on_focus_examples = c("pills keep me up", "cravings dropped"),
    off_focus_examples = c("nice weather"),                 # length 1 -> tests array shape
    discrimination_principle = "link present vs absent",
    source = source)
  mi <- new_metric_interpretation(
    metrics = list(
      list(column_name = "score", column_description = "heavy-tailed count",
           requested_primitives = list(
             list(primitive = "prim_median", rationale = "robust"),
             list(primitive = "prim_p90", rationale = "tail")),
           interpretation_note = "median + p90")),
    temporal_columns = list(
      list(column_name = "std_timestamp", column_description = "timestamps",
           requested_primitives = list(
             list(primitive = "prim_hour_of_day_distribution", rationale = "rhythm")),
           interpretation_note = "evening peak")),
    source = source)
  new_methodology_articulations(rel, mi, research_focus = "focus", source = source)
}

.skip_if_no_mock <- function() {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
}

# ---- schemas -----------------------------------------------------------------

test_that("both methodology schemas pass strict-mode validation", {
  expect_silent(pakhom:::.validate_schema(.relevance_criterion_schema()))
  expect_silent(pakhom:::.validate_schema(.metric_intelligence_schema()))
})

test_that("the metric schema's primitive field is a free string, NOT an enum", {
  sch <- .metric_intelligence_schema()
  prim <- sch$properties$metrics$items$properties$requested_primitives$items$properties$primitive
  expect_equal(prim$type, "string")
  expect_null(prim$enum)                       # the no-menu guarantee, asserted
})

test_that("Phase 61.2 decision types are in the audit-log allowlist", {
  # The prior-phase landmine: a decision_type missing here crashes every
  # production run with an audit log on the first call.
  expect_true("relevance_criterion" %in% pakhom:::.valid_decision_types)
  expect_true("metric_interpretation" %in% pakhom:::.valid_decision_types)
})

# ---- S3 objects + serialization ----------------------------------------------

test_that("S3 constructors validate types and normalize fields", {
  rel <- new_relevance_criterion(relevance_criterion = "x", on_focus_examples = c("a", "b"))
  expect_s3_class(rel, "RelevanceCriterion")
  expect_equal(rel$source, "ai")
  expect_equal(rel$on_focus_examples, c("a", "b"))
  # NULL fields normalize to "" (not NA) via .scalar_chr
  expect_identical(new_relevance_criterion()$research_focus_paraphrase, "")
  expect_error(new_methodology_articulations(list(), list()), "RelevanceCriterion")
})

test_that("articulations round-trip through to_list -> JSON -> from_list", {
  art <- .mk_articulations()
  lst <- methodology_articulations_to_list(art)
  json <- jsonlite::toJSON(lst, auto_unbox = TRUE, null = "null")
  back <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  art2 <- methodology_articulations_from_list(back)

  expect_equal(art2$relevance$relevance_criterion, art$relevance$relevance_criterion)
  expect_equal(art2$relevance$on_focus_examples, art$relevance$on_focus_examples)
  # the length-1 off_focus array survived auto_unbox (as.list wrapping)
  expect_equal(art2$relevance$off_focus_examples, "nice weather")
  expect_length(art2$relevance$off_focus_examples, 1L)
  expect_equal(length(art2$metric_interpretation$metrics), 1L)
  expect_equal(art2$metric_interpretation$metrics[[1]]$requested_primitives[[2]]$primitive,
               "prim_p90")
  expect_equal(art2$metric_interpretation$temporal_columns[[1]]$column_name, "std_timestamp")
})

# ---- prompt builders + column detection --------------------------------------

test_that("temporal column detection finds std_timestamp only when parseable", {
  d <- .methodology_test_data()
  expect_equal(.detect_temporal_columns(d), "std_timestamp")
  expect_equal(.detect_temporal_columns(tibble::tibble(x = 1)), character(0))
  expect_equal(.detect_temporal_columns(tibble::tibble(std_timestamp = c(NA, NA))),
               character(0))
})

test_that("metric-columns block shows raw sample values, not pre-computed stats", {
  d <- .methodology_test_data()
  blk <- pakhom:::.build_metric_columns_block(d, c("score", "upvote_ratio"), "std_timestamp")
  expect_match(blk, "score", fixed = TRUE)
  expect_match(blk, "upvote_ratio", fixed = TRUE)
  expect_match(blk, "TIMESTAMP COLUMNS", fixed = TRUE)
  expect_match(blk, "non-missing=", fixed = TRUE)
  # no mean/median pre-computed for the AI
  expect_false(grepl("mean=|median=", blk))
})

test_that("corpus-sample block is deterministic and spread across the corpus", {
  d <- .methodology_test_data()
  expect_identical(pakhom:::.build_corpus_sample_block(d),
                   pakhom:::.build_corpus_sample_block(d))   # no RNG
  expect_match(pakhom:::.build_corpus_sample_block(d), "1\\. ")
})

# ---- AI callers (mocked) -----------------------------------------------------

test_that("articulate_relevance_criterion builds a RelevanceCriterion (mocked)", {
  .skip_if_no_mock()
  local_mocked_bindings(ai_complete = .mock_ai(), .package = "pakhom")
  rel <- articulate_relevance_criterion("focus", "CORPUS SAMPLE\n1. x", mock_provider())
  expect_s3_class(rel, "RelevanceCriterion")
  expect_match(rel$relevance_criterion, "on-focus")
  expect_length(rel$on_focus_examples, 2L)
  expect_equal(rel$source, "ai")
})

test_that("articulate_relevance_criterion fails LOUDLY on an empty response", {
  .skip_if_no_mock()
  local_mocked_bindings(
    ai_complete = function(...) list(content = "{}", model = "m", request_id = "r",
                                     usage = list(), finish_reason = "stop"),
    .package = "pakhom")
  expect_error(articulate_relevance_criterion("focus", "CORPUS SAMPLE", mock_provider()),
               "empty or unparseable")
})

test_that("interpret_metrics preserves AI-requested primitives, incl. catalog gaps (R4)", {
  .skip_if_no_mock()
  d <- .methodology_test_data()
  local_mocked_bindings(ai_complete = .mock_ai(), .package = "pakhom")
  mi <- interpret_metrics(d, "focus", c("score", "upvote_ratio"), "std_timestamp",
                          mock_provider())
  expect_s3_class(mi, "MetricInterpretation")
  expect_equal(length(mi$metrics), 2L)
  prims <- vapply(mi$metrics[[1]]$requested_primitives, function(p) p$primitive, character(1))
  # the unknown primitive is PRESERVED, not silently dropped (fail-honest happens
  # later at compute time, transparently)
  expect_true("prim_made_up_index" %in% prims)
  expect_true("prim_median" %in% prims)
})

test_that("interpret_metrics short-circuits (no AI call) when there are no columns", {
  .skip_if_no_mock()
  local_mocked_bindings(
    ai_complete = function(...) stop("ai_complete must NOT be called when there are no columns"),
    .package = "pakhom")
  d <- tibble::tibble(std_id = "e1", std_text = "x")   # no metric/temporal columns
  mi <- interpret_metrics(d, "focus", character(0), character(0), mock_provider())
  expect_s3_class(mi, "MetricInterpretation")
  expect_length(mi$metrics, 0L)
  expect_length(mi$temporal_columns, 0L)
})

test_that(".warn_unknown_primitives reports catalog gaps without dropping them", {
  mi <- new_metric_interpretation(metrics = list(
    list(column_name = "score", column_description = "d",
         requested_primitives = list(list(primitive = "prim_median", rationale = "r"),
                                     list(primitive = "prim_nope", rationale = "r")),
         interpretation_note = "n")))
  unknown <- pakhom:::.warn_unknown_primitives(mi)
  expect_equal(unknown, "prim_nope")
})

# ---- orchestrator: discovery + replay ----------------------------------------

test_that("run_methodology_assistant discovery path makes both AI calls", {
  .skip_if_no_mock()
  d <- .methodology_test_data()
  cfg <- list(study = list(research_focus = "medication, sleep, binge eating",
                           inferred_methodology = NULL))
  local_mocked_bindings(ai_complete = .mock_ai(), .package = "pakhom")
  art <- run_methodology_assistant(d, cfg, mock_provider())
  expect_s3_class(art, "MethodologyArticulations")
  expect_equal(art$source, "ai")
  expect_match(art$relevance$relevance_criterion, "on-focus")
  expect_equal(length(art$metric_interpretation$metrics), 2L)
  expect_equal(length(art$metric_interpretation$temporal_columns), 1L)
})

test_that("run_methodology_assistant replay path uses pinned block, NO AI call", {
  .skip_if_no_mock()
  pinned <- methodology_articulations_to_list(.mk_articulations())
  cfg <- list(study = list(research_focus = "focus", inferred_methodology = pinned))
  local_mocked_bindings(
    ai_complete = function(...) stop("ai_complete must NOT be called in replay mode"),
    .package = "pakhom")
  art <- run_methodology_assistant(.methodology_test_data(), cfg, mock_provider())
  expect_s3_class(art, "MethodologyArticulations")
  expect_equal(art$source, "pinned")
  expect_equal(art$relevance$relevance_criterion,
               "A segment is on-focus if it links medication to sleep or eating.")
})

test_that("run_methodology_assistant errors when research_focus is missing", {
  expect_error(
    run_methodology_assistant(.methodology_test_data(),
                              list(study = list(research_focus = "")),
                              mock_provider()),
    "research_focus is required")
})

# ---- replay loader -----------------------------------------------------------

test_that("load_pinned_methodology errors on a block missing the relevance criterion", {
  expect_error(load_pinned_methodology(list(metrics = list())),
               "missing relevance_criterion")
  expect_error(load_pinned_methodology("not a list"), "must be a list")
})

# ---- archive -----------------------------------------------------------------

test_that("archive writes methodology_articulations.{md,json} and they round-trip", {
  art <- .mk_articulations()
  run_dir <- withr::local_tempdir()
  paths <- archive_methodology_articulations(art, run_dir)
  expect_true(file.exists(paths$md))
  expect_true(file.exists(paths$json))

  md <- paste(readLines(paths$md), collapse = "\n")
  expect_match(md, "Methodology Articulations", fixed = TRUE)
  expect_match(md, "prim_median", fixed = TRUE)

  back <- methodology_articulations_from_list(
    jsonlite::fromJSON(paths$json, simplifyVector = FALSE))
  expect_equal(back$relevance$relevance_criterion, art$relevance$relevance_criterion)
  expect_equal(length(back$metric_interpretation$metrics), 1L)
})

# ---- coding-prompt injection block (for 61.3) --------------------------------

test_that("relevance_criterion_prompt_block emits an injectable block", {
  rel <- .mk_articulations()$relevance
  blk <- relevance_criterion_prompt_block(rel)
  expect_match(blk, "RELEVANCE CRITERION FOR THIS STUDY", fixed = TRUE)
  expect_match(blk, "ON-FOCUS EXAMPLES", fixed = TRUE)
  expect_match(blk, "OFF-FOCUS EXAMPLES", fixed = TRUE)
  expect_match(blk, "should NOT be coded", fixed = TRUE)
  # empty when there is no usable criterion (caller keeps prior wording)
  expect_equal(relevance_criterion_prompt_block(NULL), "")
  expect_equal(relevance_criterion_prompt_block(new_relevance_criterion()), "")
})
