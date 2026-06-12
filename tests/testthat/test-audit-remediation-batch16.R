# Regression tests for the verification follow-up fixes (audit 2026-06-11, batch 16).

test_that("Mode 3 (framework) does NOT inject the prior-studies codebook into the coding prompt", {
  spec <- load_framework_spec("tpb")
  lc <- list(for_coding_hierarchy = "## PRIOR CODEBOOK\n- some_prior_code")
  p_m3 <- pakhom:::.build_progressive_system_prompt("x", NULL, list(), lc, framework_spec = spec)
  p_m2 <- pakhom:::.build_progressive_system_prompt("x", NULL, list(), lc, framework_spec = NULL)
  # Mode 3 applies a fixed framework -> a "reuse these prior codes" block would
  # contradict it; Mode 2 (inductive) still gets it.
  expect_false(grepl("CODEBOOK FROM PRIOR STUDIES", p_m3, fixed = TRUE))
  expect_true(grepl("CODEBOOK FROM PRIOR STUDIES", p_m2, fixed = TRUE))
})

test_that(".select_cross_model_pair returns no partner when the newest run's model is NA", {
  dirs <- c("out/r1", "out/r2")          # r2 newest, but no metadata -> model NA
  models <- list(r1 = "openai/gpt")
  pair <- pakhom:::.select_cross_model_pair(dirs, models)
  expect_null(pair$partner)              # cannot form a verified cross-model pair
})

test_that("ai_complete with max_retries < 1 still makes one attempt and errors cleanly", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")
  calls <- new.env(parent = emptyenv()); calls$n <- 0L
  local_mocked_bindings(
    .anthropic_completion = function(...) {
      calls$n <- calls$n + 1L
      stop("Anthropic API error (HTTP 503): transient")
    },
    .package = "pakhom"
  )
  # max_retries = 0 must not crash with "object 'attempt' not found"; it runs
  # one attempt (max(1L, 0)) and fails with the normal message.
  expect_error(ai_complete(mock_provider("anthropic"), "p", task = "coding",
                           max_retries = 0),
               "AI request failed after")
  expect_equal(calls$n, 1L)
})

test_that("Mode 3 construct_id matching is whitespace-tolerant", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")
  spec <- load_framework_spec("tpb")
  data <- tibble::tibble(std_id = "e1", std_text = "I intend to comply.")
  mock_response <- jsonlite::toJSON(list(
    skipped = FALSE, skip_reason = "",
    coded_segments = list(list(text = "I intend", start_char = 0L, end_char = 8L,
                               construct_id = "  intention  ", anomaly_reason = ""))
  ), auto_unbox = TRUE)
  local_mocked_bindings(
    ai_complete = function(...) list(
      content = mock_response, model = "m", request_id = "r",
      usage = list(prompt_tokens=1L, completion_tokens=1L, total_tokens=2L),
      finish_reason = "stop", raw_response = list(), prompt_hash = "h",
      citations = list()
    ), .package = "pakhom"
  )
  state <- suppressWarnings(run_progressive_coding(
    data = data, provider = mock_provider("anthropic"),
    config = list(max_retries_per_entry = 1L), research_focus = "x",
    framework_spec = spec))
  # "  intention  " must map to the real construct, NOT route to anomaly.
  expect_equal(state$codebook[["intention"]]$frequency, 1L)
  expect_equal(state$codebook[["anomaly"]]$frequency, 0L)
})
