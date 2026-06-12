# Regression tests for the Batch-7 paid-call retry classification (audit 2026-06-11).

test_that("ai_complete does NOT retry a permanent post-200 error (no re-charge)", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")
  calls <- new.env(parent = emptyenv()); calls$n <- 0L
  local_mocked_bindings(
    .anthropic_completion = function(...) {
      calls$n <- calls$n + 1L
      pakhom:::.stop_permanent("Anthropic API returned empty content array")
    },
    .package = "pakhom"
  )
  prov <- mock_provider("anthropic")
  expect_error(ai_complete(prov, "p", task = "coding", max_retries = 3),
               "empty content array")
  expect_equal(calls$n, 1L)   # failed fast -- one billed attempt, not three
})

test_that("ai_complete DOES retry a transient (5xx) error up to max_retries", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")
  calls <- new.env(parent = emptyenv()); calls$n <- 0L
  local_mocked_bindings(
    .anthropic_completion = function(...) {
      calls$n <- calls$n + 1L
      stop("Anthropic API error (HTTP 503): server overloaded")
    },
    .package = "pakhom"
  )
  prov <- mock_provider("anthropic")
  expect_error(ai_complete(prov, "p", task = "coding", max_retries = 2))
  expect_equal(calls$n, 2L)   # transient -> retried every attempt
})
