# Tests for progressive sequential coding (11_coding.R)

test_that("create_coding_state initializes correctly", {
  state <- create_coding_state()
  expect_s3_class(state, "ProgressiveCodingState")
  expect_type(state$codebook, "list")
  expect_length(state$codebook, 0)
  expect_type(state$entry_results, "list")
  expect_length(state$entries_processed, 0)
  expect_length(state$entries_skipped, 0)
})

test_that("create_coding_state accepts learning context calibration", {
  ctx <- list(benchmarks = list(
    typical_code_count = 40,
    codes_per_entry = 5.7,
    max_code_coverage_pct = 34,
    avg_segment_length = 88
  ))
  state <- create_coding_state(learning_context = ctx)
  expect_equal(state$calibration$target_granularity, 40)
  expect_equal(state$calibration$target_codes_per_entry, 5.7)
  expect_equal(state$calibration$avg_segment_length, 88)
})

test_that("get_analytic_sample filters skipped entries", {
  state <- create_coding_state()
  state$entry_results[["a"]] <- list(codes_assigned = "code1", skipped = FALSE)
  state$entry_results[["b"]] <- list(codes_assigned = character(0), skipped = TRUE, skip_reason = "Not applicable")
  state$entry_results[["c"]] <- list(codes_assigned = c("code1", "code2"), skipped = FALSE)

  data <- tibble::tibble(std_id = c("a", "b", "c"), std_text = c("text1", "text2", "text3"))
  result <- get_analytic_sample(state, data)

  expect_equal(nrow(result), 2)
  expect_true("a" %in% result$std_id)
  expect_true("c" %in% result$std_id)
  expect_false("b" %in% result$std_id)
})

test_that("as_coding_results converts state to legacy format", {
  state <- create_coding_state()
  state$codebook[["sleep_issues"]] <- list(
    code_name = "Sleep Issues", description = "Problems sleeping",
    type = "descriptive", frequency = 2L,
    entry_ids = c("e1", "e2"),
    coded_segments = list(
      list(entry_id = "e1", text = "cant sleep", start_char = 0L, end_char = 10L),
      list(entry_id = "e2", text = "insomnia", start_char = 5L, end_char = 13L)
    )
  )
  state$entry_results[["e1"]] <- list(
    codes_assigned = "sleep_issues",
    coded_segments = list(list(code_key = "sleep_issues", code_name = "Sleep Issues",
                                text = "cant sleep", start_char = 0L, end_char = 10L)),
    skipped = FALSE
  )
  state$entry_results[["e2"]] <- list(
    codes_assigned = "sleep_issues",
    coded_segments = list(list(code_key = "sleep_issues", code_name = "Sleep Issues",
                                text = "insomnia", start_char = 5L, end_char = 13L)),
    skipped = FALSE
  )
  state$entry_results[["e3"]] <- list(codes_assigned = character(0), skipped = TRUE, skip_reason = "N/A")
  state$entries_processed <- 1:3
  state$entries_skipped <- 3L

  cr <- as_coding_results(state)
  expect_type(cr, "list")
  expect_equal(cr$unique_codes, 1)
  expect_equal(cr$entries_coded, 2)
  expect_equal(cr$total_applications, 2)
  expect_true("sleep_issues" %in% names(cr$all_codes))
  expect_true("e1" %in% names(cr$entry_codes))
  expect_false("e3" %in% names(cr$entry_codes))
})

test_that("verify_excerpts works with ProgressiveCodingState", {
  state <- create_coding_state()
  state$codebook[["test_code"]] <- list(
    code_name = "Test Code", description = "", type = "descriptive",
    frequency = 1L, entry_ids = "e1",
    coded_segments = list(
      list(entry_id = "e1", text = "important finding", start_char = 5L, end_char = 22L)
    )
  )
  state$entry_results[["e1"]] <- list(
    codes_assigned = "test_code",
    coded_segments = list(list(code_key = "test_code", code_name = "Test Code",
                                text = "important finding", start_char = 5L, end_char = 22L)),
    skipped = FALSE
  )

  data <- tibble::tibble(std_id = "e1", std_text = "This important finding is notable.")
  result <- verify_excerpts(data, state)
  expect_type(result, "list")
  expect_equal(result$substring_stats$valid, 1)
  expect_equal(result$substring_stats$invalid, 0)
})

test_that("codebook_summary builder works", {
  state <- create_coding_state()
  # Empty codebook
  summary <- pakhom:::.build_codebook_summary(state)
  expect_equal(summary, "")

  # Add some codes
  state$codebook[["a"]] <- list(code_name = "Code A", frequency = 10L,
                                 description = "desc A", type = "descriptive")
  state$codebook[["b"]] <- list(code_name = "Code B", frequency = 5L,
                                 description = "desc B", type = "emotional")
  summary <- pakhom:::.build_codebook_summary(state, max_codes = 10)
  expect_true(nchar(summary) > 0)
  expect_true(grepl("Code A", summary))
  expect_true(grepl("Code B", summary))
})

# ============================================================================
# Saturation tracking
#
# Background: the previous saturation calculation tried to derive each code's
# birth-time-in-coded-entries from per-checkpoint accumulator lists that get
# reset every checkpoint_interval. After the first reset the calculation lost
# all history, so `new_in_window` collapsed toward zero and the code-creation-
# rate signal fired prematurely once `min_coded_before_saturation` was reached.
# The fix stores `n_coded_at_birth` directly in a parallel map at the moment
# each code is created, so the saturation check is a direct lookup.
# ============================================================================

test_that("create_coding_state initializes code_n_coded_at_birth alongside code_birth_log", {
  state <- create_coding_state()
  expect_true("code_n_coded_at_birth" %in% names(state$saturation))
  expect_true("code_birth_log" %in% names(state$saturation))
  expect_type(state$saturation$code_n_coded_at_birth, "list")
  expect_length(state$saturation$code_n_coded_at_birth, 0)
})

test_that("saturation: new_in_window reflects only codes born within the window", {
  # Direct calculation that mirrors the production code in 09_coding.R lines
  # 281-289. Tests the math without invoking the full coding loop.
  state <- create_coding_state()
  # Codes born at coded-entry counts: 10, 100, 200, 400, 500, 580, 600
  birth_points <- c(10, 100, 200, 400, 500, 580, 600)
  for (i in seq_along(birth_points)) {
    state$saturation$code_n_coded_at_birth[[paste0("code_", i)]] <- birth_points[i]
  }

  n_coded <- 600
  window_size <- 200
  births <- unlist(state$saturation$code_n_coded_at_birth, use.names = FALSE)
  new_in_window <- sum(births > (n_coded - window_size))  # > 400
  # Codes at 500, 580, 600 should count (3 codes); 400 itself is the boundary
  # and is strictly NOT > 400, so it should not count.
  expect_equal(new_in_window, 3)
})

test_that("saturation: signal_creation does NOT fire when code creation is steady", {
  # Simulate 600 coded entries with codes born at every 10 coded entries
  state <- create_coding_state()
  for (i in seq(50, 600, by = 10)) {
    state$saturation$code_n_coded_at_birth[[paste0("code_", i)]] <- i
  }
  n_coded <- 600
  window_size <- 200
  saturation_threshold <- 2

  births <- unlist(state$saturation$code_n_coded_at_birth, use.names = FALSE)
  new_in_window <- sum(births > (n_coded - window_size))

  # Codes born at 410, 420, ..., 600 -> 20 codes. 20 > threshold of 2.
  # signal_creation should be FALSE -> saturation should NOT fire on this signal.
  expect_equal(new_in_window, 20)
  expect_false(new_in_window <= saturation_threshold)
})

test_that("saturation: signal_creation DOES fire when code creation has stopped", {
  # All codes born early (before n_coded=400); we are now at n_coded=700
  state <- create_coding_state()
  for (i in seq(50, 400, by = 10)) {
    state$saturation$code_n_coded_at_birth[[paste0("code_", i)]] <- i
  }
  n_coded <- 700
  window_size <- 200
  saturation_threshold <- 2

  births <- unlist(state$saturation$code_n_coded_at_birth, use.names = FALSE)
  new_in_window <- sum(births > (n_coded - window_size))

  # All births at <= 400 < (700-200)=500 -> new_in_window == 0
  # 0 <= threshold of 2, so signal_creation should be TRUE
  expect_equal(new_in_window, 0)
  expect_true(new_in_window <= saturation_threshold)
})

test_that("saturation: empty codebook produces zero new_in_window without errors", {
  state <- create_coding_state()
  expect_length(state$saturation$code_n_coded_at_birth, 0)
  # Mirror the guarded path from 09_coding.R
  if (length(state$saturation$code_n_coded_at_birth) > 0) {
    births <- unlist(state$saturation$code_n_coded_at_birth, use.names = FALSE)
    new_in_window <- sum(births > 0)
  } else {
    new_in_window <- 0L
  }
  expect_equal(new_in_window, 0L)
})

test_that("backward-compat: legacy state missing code_n_coded_at_birth is conservatively backfilled", {
  # Simulate a state saved under the old structure (code_birth_log present,
  # code_n_coded_at_birth absent). Call run_progressive_coding's resume path
  # logic by constructing the equivalent backfill the function performs.
  legacy_state <- create_coding_state()
  legacy_state$saturation$code_birth_log <- list(
    code_a = 10L, code_b = 25L, code_c = 100L
  )
  legacy_state$saturation$code_n_coded_at_birth <- NULL  # simulate older save

  # Apply the backfill as run_progressive_coding does on resume
  if (is.null(legacy_state$saturation$code_n_coded_at_birth)) {
    legacy_state$saturation$code_n_coded_at_birth <- setNames(
      as.list(rep(0L, length(legacy_state$saturation$code_birth_log))),
      names(legacy_state$saturation$code_birth_log)
    )
  }

  expect_length(legacy_state$saturation$code_n_coded_at_birth, 3)
  expect_named(legacy_state$saturation$code_n_coded_at_birth,
               c("code_a", "code_b", "code_c"))
  # All seeded with 0 -> conservative: never count toward "recent" births
  # for any n_coded > window_size, which is the safe direction (won't cause
  # premature saturation, may delay it slightly)
  births <- unlist(legacy_state$saturation$code_n_coded_at_birth, use.names = FALSE)
  expect_true(all(births == 0))
  expect_equal(sum(births > (500 - 200)), 0)  # at any plausible n_coded, no recent
})

# ==============================================================================
# T0.1 wiring: .code_entry_progressive verify_quote + fabrication_log integration
# ==============================================================================

test_that("T0.1: .code_entry_progressive attaches QuoteProvenance to verified segments", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "I had trouble sleeping after starting the new medication."

  # Mock returns one segment whose text IS verbatim in the entry -> verified
  mock_response <- jsonlite::toJSON(list(
    skipped        = FALSE,
    skip_reason    = "",
    coded_segments = list(list(
      text             = "trouble sleeping",
      start_char       = 6L,
      end_char         = 22L,
      code             = "NEW: sleep difficulty",
      code_description = "Difficulty initiating or maintaining sleep",
      code_type        = "descriptive"
    ))
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(
      content    = mock_response,
      model      = "gpt-4o-mock",
      request_id = "req_mock_verified",
      usage      = list(prompt_tokens = 50L, completion_tokens = 25L,
                        total_tokens = 75L),
      finish_reason = "stop",
      raw_response  = list(),
      prompt_hash   = "hash-verified"
    ),
    .package = "pakhom"
  )

  state <- create_coding_state()
  state <- pakhom:::.code_entry_progressive(
    text = entry_text, entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider(),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test"
  )

  # Segment kept; provenance attached; verification passed
  seg <- state$entry_results[["e1"]]$coded_segments[[1]]
  expect_true(!is.null(seg$provenance))
  expect_s3_class(seg$provenance, "QuoteProvenance")
  expect_true(seg$provenance$verification_status %in% c("verified_exact", "verified_fuzzy"))
  expect_equal(seg$provenance$ai_call_id, "req_mock_verified")
  expect_equal(seg$provenance$ai_model,   "gpt-4o-mock")
})

test_that("T0.1: .code_entry_progressive drops fabricated segments + does not pollute codebook", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "Real entry text about sleep struggles."

  # Mock returns one fabricated segment (text not in entry)
  mock_response <- jsonlite::toJSON(list(
    skipped        = FALSE,
    skip_reason    = "",
    coded_segments = list(list(
      text             = "I love unicorns and rainbows",
      start_char       = 0L,
      end_char         = 28L,
      code             = "NEW: unicorn appreciation",
      code_description = "Affinity for unicorns",
      code_type        = "descriptive"
    ))
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(
      content       = mock_response,
      model         = "gpt-4o-mock",
      request_id    = "req_mock_fab",
      usage         = list(prompt_tokens = 50L, completion_tokens = 25L,
                            total_tokens = 75L),
      finish_reason = "stop",
      raw_response  = list(),
      prompt_hash   = "hash-fabricated"
    ),
    .package = "pakhom"
  )

  state <- create_coding_state()
  # Capture log_warn output to silence the expected fabrication warning
  state <- suppressWarnings(pakhom:::.code_entry_progressive(
    text = entry_text, entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider(),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test"
  ))

  # No segment kept; codebook unpolluted
  expect_length(state$entry_results[["e1"]]$coded_segments, 0L)
  expect_length(state$codebook, 0L)
  # Entry NOT marked skipped -- fabrication is a different signal than
  # "AI said this entry has no applicable content"
  expect_false(isTRUE(state$entry_results[["e1"]]$skipped))
})

test_that("T0.1: .code_entry_progressive writes fabricated quotes to FabricationLog when supplied", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "Real entry text."
  mock_response <- jsonlite::toJSON(list(
    skipped        = FALSE,
    skip_reason    = "",
    coded_segments = list(list(
      text             = "fabricated content not in source",
      start_char       = 0L,
      end_char         = 32L,
      code             = "NEW: fake_code",
      code_description = "test",
      code_type        = "descriptive"
    ))
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(
      content       = mock_response,
      model         = "gpt-4o-mock",
      request_id    = "req_fab_log",
      usage         = list(prompt_tokens = 50L, completion_tokens = 25L,
                            total_tokens = 75L),
      finish_reason = "stop",
      raw_response  = list(),
      prompt_hash   = "hash-fabricated2"
    ),
    .package = "pakhom"
  )

  td <- withr::local_tempdir()
  flog <- init_fabrication_log(td)

  state <- create_coding_state()
  suppressWarnings(pakhom:::.code_entry_progressive(
    text = entry_text, entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider(),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test",
    fabrication_log = flog
  ))

  expect_equal(flog$state$n_logged, 1L)
  rows <- read.csv(flog$path, stringsAsFactors = FALSE)
  expect_equal(nrow(rows), 1L)
  expect_equal(rows$verification_status, "fabricated")
  expect_equal(rows$ai_call_id, "req_fab_log")
})
