# Tests for R/methodology_assistant.R + its two schemas
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
    std_text      = rep(c("scheduling keeps me up at night",
                          "overwork episodes decreased on the new shift",
                          "went for a run this morning"),
                        length.out = n),
    std_timestamp = as.character(base + (seq_len(n) - 1L) * 3600 * 7),
    score         = as.numeric(rep(c(0, 1, 5, 100, 2), length.out = n)),
    upvote_ratio  = round(rep(c(0.5, 0.9, 1.0, 0.97), length.out = n), 2),
    source_table  = rep(c("posts", "comments"), length.out = n)  # internal-excluded
  )
}

.relevance_json <- function() jsonlite::toJSON(list(
  research_focus_paraphrase = "How flexible scheduling affects focus and overwork.",
  relevance_criterion = paste0(
    "A segment is on-focus when it links a flexible scheduling to either ",
    "focus or overwork behavior: timing, schedule, or effectiveness."),
  on_focus_examples = list("The late meetings keep me up at night",
                           "My overtime urges dropped after the shift change"),
  off_focus_examples = list("I went for a run today", "The weather has been nice"),
  discrimination_principle = "On-focus segments tie scheduling to focus or overworking; adjacent ones mention only one in isolation."
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
    relevance_criterion = "A segment is on-focus if it links scheduling to focus or overworking.",
    on_focus_examples = c("late meetings keep me up", "overtime urges dropped"),
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

test_that("methodology decision types are in the -log allowlist", {
  # The prior-phase issue: a decision_type missing here crashes every
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
  cfg <- list(study = list(research_focus = "scheduling, focus, overwork",
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
               "A segment is on-focus if it links scheduling to focus or overworking.")
})

test_that("run_methodology_assistant errors when research_focus is missing", {
  expect_error(
    run_methodology_assistant(.methodology_test_data(),
                              list(study = list(research_focus = "")),
                              mock_provider()),
    "research_focus is required")
})

# ---- 62.5: research_context grounds the provenance judgment -------------------
# Surfaced by the 62.5 smoke: without the dataset's source/provenance, the AI
# read a column literally named "score" (Reddit upvotes) as a substantive rating
# scale -- the exact platform-vs-phenomenon conflation 62.1 exists to prevent.
# The fix threads config$study$research_context into the metric prompt + adds the
# design's fail-honest "lean to the cautious reading when unclear" instruction.

.capturing_ai <- function(cap) function(provider, user_prompt, system_prompt, ...) {
  resp <- function(content) list(
    content = content, model = "m", request_id = "r",
    usage = list(prompt_tokens = 1L, completion_tokens = 1L, total_tokens = 2L),
    finish_reason = "stop", raw_response = list(), prompt_hash = "h", citations = list())
  if (grepl("CORPUS SAMPLE", user_prompt, fixed = TRUE)) return(resp(.relevance_json()))
  cap$metric_user   <- user_prompt
  cap$metric_system <- system_prompt
  resp(.metrics_json())
}

test_that("research_context is threaded into the metric-intelligence prompt (62.5)", {
  .skip_if_no_mock()
  cap <- new.env()
  local_mocked_bindings(ai_complete = .capturing_ai(cap), .package = "pakhom")
  run_methodology_assistant(.methodology_test_data(), list(study = list(
    research_focus = "scheduling & focus",
    research_context = "Reddit posts and comments from overwork subreddits",
    inferred_methodology = NULL)), mock_provider())
  expect_match(cap$metric_user, "RESEARCH CONTEXT", fixed = TRUE)
  expect_match(cap$metric_user, "Reddit posts and comments from overwork subreddits", fixed = TRUE)
  # the design's fail-honest lean-to-metadata instruction lives in the system prompt
  expect_match(cap$metric_system, "lean toward the cautious reading", fixed = TRUE)
  # 62.5b: the small-n spread reliability caution is elicited for interpretation_note
  expect_match(cap$metric_system, "indicative, not precise, at small n", fixed = TRUE)
})

test_that("metric prompt omits the RESEARCH CONTEXT block when none configured (back-compat, 62.5)", {
  .skip_if_no_mock()
  cap <- new.env()
  local_mocked_bindings(ai_complete = .capturing_ai(cap), .package = "pakhom")
  run_methodology_assistant(.methodology_test_data(), list(study = list(
    research_focus = "scheduling & focus", inferred_methodology = NULL)), mock_provider())
  expect_false(grepl("RESEARCH CONTEXT", cap$metric_user, fixed = TRUE))
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

# ----  regressions (C1 / H1 / M1) -------------------------------

test_that("the methodology_assistant STEP is in the allowlist (C1)", {
  # log_ai_decision validates BOTH step and decision_type; the prior phase's
  # issue recurred on the step axis. Without this the path crashes.
  expect_true("methodology_assistant" %in% pakhom:::.valid_audit_steps)
})

test_that("run_methodology_assistant does not crash WITH a real log (C1)", {
  .skip_if_no_mock()
  d <- .methodology_test_data()
  cfg <- list(study = list(research_focus = "scheduling and focus", inferred_methodology = NULL))
  audit_dir <- withr::local_tempdir()
  audit <- init_audit_log(audit_dir)
  local_mocked_bindings(ai_complete = .mock_ai(), .package = "pakhom")
  expect_no_error(art <- run_methodology_assistant(d, cfg, mock_provider(), audit_log = audit))
  expect_s3_class(art, "MethodologyArticulations")
  jsonl <- file.path(audit_dir, "ai_decisions.jsonl")
  expect_true(file.exists(jsonl))
  expect_gt(length(readLines(jsonl)), 0L)         # records written, no crash
})

test_that(".detect_temporal_columns is non-throwing on unparseable cells (H1)", {
  # one garbage cell in an otherwise-valid column used to THROW (crashing Step 2.5)
  d <- tibble::tibble(std_timestamp = c("2024-01-01 18:00:00", "garbage", "2024-01-02 09:00:00"))
  expect_no_error(res <- .detect_temporal_columns(d))
  expect_equal(res, "std_timestamp")              # majority parses -> still temporal
  expect_equal(.detect_temporal_columns(tibble::tibble(std_timestamp = c("x", "y"))),
               character(0))                       # all-unparseable -> not temporal, no throw
})

test_that("interpret_metrics recovers a single-object metrics response (M1)", {
  .skip_if_no_mock()
  single_obj <- jsonlite::toJSON(list(
    metrics = list(column_name = "score", column_description = "d",
                   requested_primitives = list(list(primitive = "prim_median", rationale = "r")),
                   interpretation_note = "n"),     # a JSON OBJECT, not a 1-element array
    temporal_columns = list()
  ), auto_unbox = TRUE)
  local_mocked_bindings(
    ai_complete = function(...) list(content = single_obj, model = "m", request_id = "r",
                                     usage = list(), finish_reason = "stop"),
    .package = "pakhom")
  mi <- interpret_metrics(.methodology_test_data(), "focus", c("score"), character(0),
                          mock_provider())
  expect_equal(length(mi$metrics), 1L)            # wrapped, not mis-iterated into an opaque error
  expect_equal(mi$metrics[[1]]$column_name, "score")
})
