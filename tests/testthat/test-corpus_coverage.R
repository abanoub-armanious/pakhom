# Tests for the corpus coverage assertion (T0.3, R/corpus_coverage.R)
#
# T0.3 is the third Tier-0 universal: pakhom asserts in the report that the
# LLM saw every entry that survived preprocessing. This is the empirical
# answer to Jowsey 2025's finding that Microsoft Copilot "drew themes from
# only the first 2-3 pages of data."

# Helper: build a finalized ProgressiveCodingState with the requested
# entry_results layout (some coded, some skipped). The codebook is left
# empty since coverage doesn't depend on it.
.make_state <- function(coded_ids = character(),
                         skipped_specs = list()) {
  state <- create_coding_state()
  for (id in coded_ids) {
    state$entry_results[[id]] <- list(
      codes_assigned = "code1",
      coded_segments = list(list(text = "x", start_char = 0L,
                                  end_char = 1L)),
      skipped        = FALSE,
      skip_reason    = NA_character_
    )
  }
  for (spec in skipped_specs) {
    state$entry_results[[spec$id]] <- list(
      codes_assigned = character(0),
      coded_segments = list(),
      skipped        = TRUE,
      skip_reason    = spec$reason %||% "unspecified"
    )
  }
  state
}

# Helper: build a tibble with std_id and std_text for a vector of ids
.make_data <- function(ids, texts = NULL) {
  if (is.null(texts)) {
    texts <- vapply(ids, function(id) {
      paste0("This is the entry text for ", id,
              ", containing some content to count.")
    }, character(1))
  }
  tibble::tibble(std_id = ids, std_text = texts)
}

# ==============================================================================
# compute_corpus_coverage -- happy path
# ==============================================================================

test_that("compute_corpus_coverage returns a CorpusCoverage S3 object with all schema fields", {
  state <- .make_state(coded_ids = c("e1", "e2", "e3"))
  data  <- .make_data(c("e1", "e2", "e3"))
  cov <- compute_corpus_coverage(state, data)

  expect_s3_class(cov, "CorpusCoverage")
  expected_fields <- c(
    "n_raw_loaded", "n_after_preprocessing", "test_mode_sample_size",
    "n_input_to_coding", "n_processed", "n_unprocessed", "unprocessed_ids",
    "n_skipped", "skip_reasons", "n_coded",
    "bytes_processed", "chars_processed", "words_processed",
    "n_entries_truncated", "chars_sent_to_llm", "truncation_tracked",
    "coverage_rate", "no_silent_truncation",
    # stop_reason / saturation_reached / reached_at_entry
    # distinguish intentional saturation-arbiter early-stop from
    # silent truncation so the T0.3 banner can render the right
    # language (audit CRITICAL-1).
    "stop_reason", "saturation_reached", "reached_at_entry",
    "computed_at", "schema_version"
  )
  expect_setequal(names(cov), expected_fields)
})

test_that("compute_corpus_coverage: complete run yields no_silent_truncation=TRUE and rate=1.0", {
  state <- .make_state(coded_ids = c("e1", "e2", "e3"))
  data  <- .make_data(c("e1", "e2", "e3"))
  cov <- compute_corpus_coverage(state, data)

  expect_true(cov$no_silent_truncation)
  expect_equal(cov$coverage_rate, 1.0)
  expect_equal(cov$n_input_to_coding, 3L)
  expect_equal(cov$n_processed,       3L)
  expect_equal(cov$n_unprocessed,     0L)
  expect_length(cov$unprocessed_ids, 0L)
  expect_equal(cov$n_coded,   3L)
  expect_equal(cov$n_skipped, 0L)
})

test_that("compute_corpus_coverage: mixed coded + skipped totals correctly", {
  state <- .make_state(
    coded_ids = c("e1", "e2"),
    skipped_specs = list(
      list(id = "e3", reason = "No applicable content"),
      list(id = "e4", reason = "No applicable content"),
      list(id = "e5", reason = "AI response parse failure")
    )
  )
  data <- .make_data(c("e1", "e2", "e3", "e4", "e5"))
  cov <- compute_corpus_coverage(state, data)

  expect_equal(cov$n_processed, 5L)
  expect_equal(cov$n_coded,     2L)
  expect_equal(cov$n_skipped,   3L)
  expect_true(cov$no_silent_truncation)

  # Skip reason tabulation
  expect_equal(cov$skip_reasons[["No applicable content"]], 2L)
  expect_equal(cov$skip_reasons[["AI response parse failure"]], 1L)
})

# ==============================================================================
# compute_corpus_coverage -- detection of silent truncation
# ==============================================================================

test_that("compute_corpus_coverage flags silent truncation when input > processed", {
  # data has 5 entries but only 3 entry_results -- the LLM didn't see
  # entries 4 and 5 (e.g., crash mid-run, partial state)
  state <- .make_state(coded_ids = c("e1", "e2", "e3"))
  data  <- .make_data(c("e1", "e2", "e3", "e4", "e5"))
  cov <- compute_corpus_coverage(state, data)

  expect_false(cov$no_silent_truncation)
  expect_equal(cov$n_input_to_coding, 5L)
  expect_equal(cov$n_processed,       3L)
  expect_equal(cov$n_unprocessed,     2L)
  expect_setequal(cov$unprocessed_ids, c("e4", "e5"))
  expect_equal(round(cov$coverage_rate, 1), 0.6)
})

test_that("compute_corpus_coverage on empty input dataset does NOT claim coverage", {
  # Edge case: zero input entries -> coverage_rate is NA, the
  # no_silent_truncation flag is FALSE because there's nothing to
  # actually verify (vacuous-truth answer would be misleading).
  state <- .make_state()
  data  <- tibble::tibble(std_id = character(0), std_text = character(0))
  cov <- compute_corpus_coverage(state, data)

  expect_equal(cov$n_input_to_coding, 0L)
  expect_equal(cov$n_processed,       0L)
  expect_identical(cov$coverage_rate, NA_real_)
  expect_false(cov$no_silent_truncation)
})

test_that("compute_corpus_coverage: all-skipped dataset still asserts coverage", {
  # When every entry is skipped (e.g., all off-topic for the research
  # focus), coverage is still TRUE -- the LLM saw every entry, it just
  # judged none applicable. This distinguishes "we didn't process the
  # data" from "we processed the data but it was all off-topic".
  state <- .make_state(skipped_specs = list(
    list(id = "e1", reason = "No applicable content"),
    list(id = "e2", reason = "No applicable content")
  ))
  data <- .make_data(c("e1", "e2"))
  cov <- compute_corpus_coverage(state, data)

  expect_true(cov$no_silent_truncation)
  expect_equal(cov$n_processed, 2L)
  expect_equal(cov$n_skipped,   2L)
  expect_equal(cov$n_coded,     0L)
  expect_equal(cov$coverage_rate, 1.0)
})

# ==============================================================================
# compute_corpus_coverage -- byte/word counts + optional fields
# ==============================================================================

test_that("compute_corpus_coverage computes byte / character / word counts on processed text", {
  state <- .make_state(coded_ids = c("e1", "e2"))
  data  <- tibble::tibble(
    std_id   = c("e1", "e2"),
    std_text = c("Hello world", "Two three four")
  )
  cov <- compute_corpus_coverage(state, data)

  # 11 + 14 = 25 chars; same for bytes (ASCII)
  expect_equal(cov$chars_processed, 25L)
  expect_equal(cov$bytes_processed, 25L)
  # Word counts: 2 + 3 = 5
  expect_equal(cov$words_processed, 5L)
})

test_that("compute_corpus_coverage stores optional pre-coding counts when supplied", {
  state <- .make_state(coded_ids = c("e1"))
  data  <- .make_data("e1")
  cov <- compute_corpus_coverage(
    state, data,
    n_raw_loaded          = 100L,
    n_after_preprocessing = 50L,
    test_mode_sample_size = 20L
  )
  expect_equal(cov$n_raw_loaded,          100L)
  expect_equal(cov$n_after_preprocessing,  50L)
  expect_equal(cov$test_mode_sample_size,  20L)
})

test_that("compute_corpus_coverage defaults optional pre-coding counts to NA_integer_", {
  state <- .make_state(coded_ids = c("e1"))
  data  <- .make_data("e1")
  cov <- compute_corpus_coverage(state, data)
  expect_identical(cov$n_raw_loaded,          NA_integer_)
  expect_identical(cov$n_after_preprocessing, NA_integer_)
  expect_identical(cov$test_mode_sample_size, NA_integer_)
})

# ==============================================================================
# compute_corpus_coverage -- input validation
# ==============================================================================

test_that("compute_corpus_coverage rejects non-ProgressiveCodingState input", {
  data <- .make_data("e1")
  expect_error(
    compute_corpus_coverage(list(), data),
    "ProgressiveCodingState"
  )
})

test_that("compute_corpus_coverage rejects non-data.frame data", {
  state <- .make_state()
  expect_error(
    compute_corpus_coverage(state, "not a frame"),
    "data must be a data.frame"
  )
})

test_that("compute_corpus_coverage rejects data missing std_id column", {
  state <- .make_state(coded_ids = "e1")
  data  <- tibble::tibble(some_col = "x")
  expect_error(
    compute_corpus_coverage(state, data),
    "std_id"
  )
})

# ==============================================================================
# .count_words_safe internal helper
# ==============================================================================

test_that(".count_words_safe handles common cases", {
  expect_equal(pakhom:::.count_words_safe("hello world"), 2L)
  expect_equal(pakhom:::.count_words_safe(""), 0L)
  expect_equal(pakhom:::.count_words_safe(NA_character_), 0L)
  # Multiple whitespace types collapse to one delimiter
  expect_equal(pakhom:::.count_words_safe("a\tb  c\n d"), 4L)
  # Vectorized
  expect_equal(pakhom:::.count_words_safe(c("one", "two three")), c(1L, 2L))
})

# ==============================================================================
# print method
# ==============================================================================

test_that("print.CorpusCoverage shows headline assertion", {
  state <- .make_state(coded_ids = c("e1", "e2"))
  data  <- .make_data(c("e1", "e2"))
  cov   <- compute_corpus_coverage(state, data)
  out   <- capture.output(print(cov))
  expect_true(any(grepl("CorpusCoverage", out)))
  expect_true(any(grepl("LLM-processed entries", out)))
  expect_true(any(grepl("No silent truncation", out)))
  expect_true(any(grepl("TRUE", out)))
})

test_that("print.CorpusCoverage on truncated state surfaces the gap", {
  state <- .make_state(coded_ids = "e1")  # only 1 processed, but 3 input
  data  <- .make_data(c("e1", "e2", "e3"))
  cov   <- compute_corpus_coverage(state, data)
  out   <- capture.output(print(cov))
  expect_true(any(grepl("Unprocessed", out)))
  expect_true(any(grepl("INVESTIGATE", out)))
})

# ==============================================================================
# .build_corpus_coverage_card -- report rendering
# ==============================================================================

test_that(".build_corpus_coverage_card renders the OK banner for complete coverage", {
  state <- .make_state(coded_ids = c("e1", "e2", "e3"))
  data  <- .make_data(c("e1", "e2", "e3"))
  cov   <- compute_corpus_coverage(state, data)
  html  <- pakhom:::.build_corpus_coverage_card(cov)

  expect_match(html, "Corpus Coverage \\(T0\\.3\\)")
  expect_match(html, "coverage-banner-ok")
  expect_match(html, "All 3 entries from the preprocessed dataset")
  expect_match(html, "entry-level coverage")
})

test_that(".build_corpus_coverage_card renders the WARN banner when truncation detected", {
  state <- .make_state(coded_ids = c("e1"))
  data  <- .make_data(c("e1", "e2", "e3"))
  cov   <- compute_corpus_coverage(state, data)
  html  <- pakhom:::.build_corpus_coverage_card(cov)

  expect_match(html, "coverage-banner-warn")
  expect_match(html, "2 of 3 entries did NOT reach the LLM")
  expect_match(html, "investigate")
})

test_that(".build_corpus_coverage_card includes funnel stages and counts", {
  state <- .make_state(
    coded_ids = c("e1", "e2"),
    skipped_specs = list(list(id = "e3", reason = "No applicable content"))
  )
  data <- .make_data(c("e1", "e2", "e3"))
  cov  <- compute_corpus_coverage(state, data)
  html <- pakhom:::.build_corpus_coverage_card(cov)

  expect_match(html, "Input to coding step")
  expect_match(html, "LLM-processed")
  expect_match(html, "of those, coded")
  expect_match(html, "of those, skipped")
  expect_match(html, "Skip reasons")
  expect_match(html, "No applicable content")
})

test_that(".build_corpus_coverage_card omits skip-reasons block when there are no skips", {
  state <- .make_state(coded_ids = c("e1", "e2"))
  data  <- .make_data(c("e1", "e2"))
  cov   <- compute_corpus_coverage(state, data)
  html  <- pakhom:::.build_corpus_coverage_card(cov)
  expect_false(grepl("coverage-skip-reasons", html))
})

test_that(".build_corpus_coverage_card shows pre-coding rows when supplied", {
  state <- .make_state(coded_ids = c("e1"))
  data  <- .make_data("e1")
  cov   <- compute_corpus_coverage(
    state, data,
    n_raw_loaded = 100L,
    n_after_preprocessing = 50L
  )
  html <- pakhom:::.build_corpus_coverage_card(cov)
  expect_match(html, "Raw rows loaded")
  expect_match(html, "After preprocessing")
  expect_match(html, "50 removed")  # 100 - 50 = 50 removed by preprocessing
})

test_that(".build_corpus_coverage_card shows test-mode row when sampling was used", {
  state <- .make_state(coded_ids = c("e1"))
  data  <- .make_data("e1")
  cov   <- compute_corpus_coverage(state, data, test_mode_sample_size = 100L)
  html  <- pakhom:::.build_corpus_coverage_card(cov)
  expect_match(html, "Test-mode sub-sample")
})

test_that(".build_corpus_coverage_card includes the volume + Jowsey citation", {
  state <- .make_state(coded_ids = c("e1"))
  data  <- tibble::tibble(std_id = "e1",
                          std_text = "Hello world from the corpus")
  cov   <- compute_corpus_coverage(state, data)
  html  <- pakhom:::.build_corpus_coverage_card(cov)
  expect_match(html, "5 words")
  expect_match(html, "Jowsey")
  expect_match(html, "first 2-3 pages")
})

test_that(".build_corpus_coverage_card on NULL renders unavailable variant (legacy)", {
  html <- pakhom:::.build_corpus_coverage_card(NULL)
  expect_match(html, "coverage-unavailable")
  expect_match(html, "Coverage data not computed")
  expect_match(html, "Tier-0 transparency")
})

test_that(".build_corpus_coverage_card on non-CorpusCoverage object renders unavailable", {
  # Defensive check: callers that pass the wrong type get the unavailable
  # variant rather than a crash.
  html <- pakhom:::.build_corpus_coverage_card(list(some = "field"))
  expect_match(html, "coverage-unavailable")
})

# ==============================================================================
# Saturation-aware coverage (audit CRITICAL-1)
# ==============================================================================
# Earlier, the headline no_silent_truncation flag was simply
# (n_unprocessed == 0L), so any saturation-triggered early stop made it
# render FALSE -- which is wrong: the arbiter is the methodologically
# intentional stop. The current code distinguishes the two by reading
# coding_state$saturation$reached + $reached_at_entry.

test_that("coverage with NO saturation reports stop_reason='all_entries_processed'", {
  state <- .make_state(coded_ids = c("e1", "e2", "e3"))
  data  <- .make_data(c("e1", "e2", "e3"))
  cov <- compute_corpus_coverage(state, data)
  expect_equal(cov$stop_reason, "all_entries_processed")
  expect_false(isTRUE(cov$saturation_reached))
  expect_true(is.na(cov$reached_at_entry))
  expect_true(cov$no_silent_truncation)
})

test_that("coverage with saturation_reached + intact tail is no_silent_truncation=TRUE", {
  # Simulate: AI arbiter declared saturation at entry 60 of 100. Entries
  # 1..60 are in entry_results; entries 61..100 were intentionally NOT
  # processed. n_unprocessed = 40, which exactly equals the post-saturation
  # tail (100 - 60). Coverage should be TRUE with stop_reason flagging
  # the intentional case.
  state <- .make_state(coded_ids = paste0("e", 1:60))
  state$saturation$reached <- TRUE
  state$saturation$reached_at_entry <- 60L
  state$saturation$reached_at_coded <- 60L
  state$saturation$total_entries_at_saturation <- 100L
  state$saturation$ai_articulation <- paste0(
    "Codebook flat for 6 windows; reuse density 0.94; new_in_window=0."
  )
  data <- .make_data(paste0("e", 1:100))
  cov <- compute_corpus_coverage(state, data)
  expect_true(cov$no_silent_truncation)
  expect_equal(cov$stop_reason, "saturation_arbiter_reached")
  expect_true(cov$saturation_reached)
  expect_equal(cov$reached_at_entry, 60L)
  expect_equal(cov$n_unprocessed, 40L)
})

test_that("coverage with saturation_reached BUT missing tail flags silent truncation", {
  # Defensive: even with saturation_reached=TRUE, if n_unprocessed
  # exceeds the expected post-saturation tail (e.g., due to a pre-
  # saturation processing gap), no_silent_truncation must still be FALSE
  # so the T0.3 banner doesn't lie.
  state <- .make_state(coded_ids = c("e1", "e2", "e5"))  # missing e3, e4
  state$saturation$reached <- TRUE
  state$saturation$reached_at_entry <- 5L  # arbiter ran AT entry 5 of 10
  state$saturation$reached_at_coded <- 3L
  state$saturation$total_entries_at_saturation <- 10L
  data <- .make_data(paste0("e", 1:10))
  cov <- compute_corpus_coverage(state, data)
  # Expected tail = 10 - 5 = 5 (entries 6..10). But n_unprocessed
  # includes e3 + e4 + e6..e10 = 7 entries, which is MORE than 5.
  # T0.3 must catch the genuine gap.
  expect_equal(cov$n_unprocessed, 7L)
  expect_false(cov$no_silent_truncation)
  expect_equal(cov$stop_reason, "saturation_arbiter_reached")  # the kind of stop
})

test_that("render_tier0_coverage_card emits saturation banner on intentional stop", {
  state <- .make_state(coded_ids = paste0("e", 1:60))
  state$saturation$reached <- TRUE
  state$saturation$reached_at_entry <- 60L
  state$saturation$reached_at_coded <- 60L
  state$saturation$total_entries_at_saturation <- 100L
  data <- .make_data(paste0("e", 1:100))
  cov <- compute_corpus_coverage(state, data)
  html <- render_tier0_coverage_card(cov)
  # Saturation banner language
  expect_match(html, "saturation arbiter judged")
  expect_match(html, "intentionally")
  # Banner class is the saturation variant, not the warning variant
  expect_match(html, "coverage-banner-saturated")
  expect_false(grepl("Coverage is incomplete; investigate", html))
})

# ==============================================================================
# Within-entry truncation accounting (schema 1.1.0)
# ==============================================================================

test_that("compute_corpus_coverage aggregates truncation fields when all records carry them", {
  state <- create_coding_state()
  state$entry_results[["e1"]] <- list(
    codes_assigned = "code1", coded_segments = list(),
    skipped = FALSE, skip_reason = NA_character_,
    failure = FALSE, chars_total = 100L, chars_sent = 100L, truncated = FALSE
  )
  state$entry_results[["e2"]] <- list(
    codes_assigned = "code1", coded_segments = list(),
    skipped = FALSE, skip_reason = NA_character_,
    failure = FALSE, chars_total = 9000L, chars_sent = 8000L, truncated = TRUE
  )
  data <- .make_data(c("e1", "e2"))
  cov <- compute_corpus_coverage(state, data)

  expect_true(cov$truncation_tracked)
  expect_equal(cov$n_entries_truncated, 1L)
  expect_equal(cov$chars_sent_to_llm, 8100L)
  expect_equal(cov$schema_version, "1.1.0")
})

test_that("compute_corpus_coverage reports NA + untracked for legacy records (never fabricates zero)", {
  # Hand-built legacy state: records lack chars_sent/truncated entirely.
  state <- .make_state(coded_ids = c("e1", "e2"))
  data  <- .make_data(c("e1", "e2"))
  cov <- compute_corpus_coverage(state, data)

  expect_false(cov$truncation_tracked)
  expect_true(is.na(cov$n_entries_truncated))
  expect_true(is.na(cov$chars_sent_to_llm))
  # The card and print method must not error on the NA/untracked path
  html <- pakhom:::.build_corpus_coverage_card(cov)
  expect_match(html, "not tracked", fixed = TRUE)
  expect_no_error(capture.output(print(cov)))
})

test_that("coverage card discloses truncation in the ok-banner when entries were truncated", {
  state <- create_coding_state()
  state$entry_results[["e1"]] <- list(
    codes_assigned = "code1", coded_segments = list(),
    skipped = FALSE, skip_reason = NA_character_,
    failure = FALSE, chars_total = 9000L, chars_sent = 8000L, truncated = TRUE
  )
  data <- .make_data("e1")
  cov <- compute_corpus_coverage(state, data)
  html <- pakhom:::.build_corpus_coverage_card(cov)

  expect_match(html, "1 entries exceeded the per-entry character cap", fixed = TRUE)
  expect_match(html, "characters of source text sent to the LLM", fixed = TRUE)
})

# ==============================================================================
# Aggregate AI-failure breaker (M-34)
# ==============================================================================

test_that(".cluster_skip_reasons routes the AI failure string to its own category", {
  reasons <- stats::setNames(
    c(3L, 2L, 1L),
    c("AI response parse failure",
      "Discusses a social network feature, unrelated to focus",
      "Mentions a timeout in a basketball game, off-topic")
  )
  clusters <- pakhom:::.cluster_skip_reasons(reasons)
  labels <- vapply(clusters, function(cl) cl$label, character(1))
  failure_idx <- grep("AI call failure", labels)
  expect_length(failure_idx, 1L)
  expect_equal(clusters[[failure_idx]]$count, 3L)
  # The free-text reasons mentioning "network"/"timeout" must NOT be
  # bucketed as failures (they are legitimate AI-judged skips).
  other_counts <- sum(vapply(clusters[-failure_idx], function(cl) cl$count,
                             integer(1)))
  expect_equal(other_counts, 3L)
})

test_that("run_progressive_coding trips the breaker on consecutive AI failures", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")
  local_mocked_bindings(
    ai_complete = function(...) stop("simulated network outage"),
    .package = "pakhom"
  )
  data <- tibble::tibble(
    std_id = paste0("e", 1:30),
    std_text = paste("This entry has plenty of text to be coded, number", 1:30)
  )
  err <- tryCatch(
    suppressWarnings(run_progressive_coding(
      data, provider = mock_provider(),
      config = list(max_consecutive_entry_failures = 5L),
      research_focus = "test"
    )),
    error = function(e) e
  )
  expect_s3_class(err, "pakhom_coding_failure_breaker")
  expect_match(conditionMessage(err), "resume = TRUE", fixed = TRUE)
  expect_match(conditionMessage(err), "5 consecutive", fixed = TRUE)
})

test_that("legitimate AI-judged skips never trip the breaker (high-skip corpus)", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")
  skip_response <- jsonlite::toJSON(list(
    skipped = TRUE, skip_reason = "No applicable content",
    coded_segments = list()
  ), auto_unbox = TRUE)
  local_mocked_bindings(
    ai_complete = function(...) list(
      content = skip_response, model = "mock", request_id = "req",
      usage = list(prompt_tokens = 1L, completion_tokens = 1L, total_tokens = 2L),
      finish_reason = "stop", raw_response = list(), prompt_hash = "h"
    ),
    .package = "pakhom"
  )
  data <- tibble::tibble(
    std_id = paste0("e", 1:25),
    std_text = paste("This entry has plenty of text to be skipped, number", 1:25)
  )
  expect_no_error(suppressWarnings(
    state <- run_progressive_coding(
      data, provider = mock_provider(),
      config = list(max_consecutive_entry_failures = 5L,
                    max_failed_entry_fraction = 0.1),
      research_focus = "test"
    )
  ))
  expect_equal(sum(vapply(state$entry_results,
                          function(er) isTRUE(er$skipped), logical(1))), 25L)
})
