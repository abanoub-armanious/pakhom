# Tests for content-addressable response cache (T1.4)
# api_response_cache.R provides on-disk storage of raw_response keyed by
# prompt_hash; the planned replay_run() consumes it.

# ---- Helpers ----------------------------------------------------------------

mk_ai_result <- function(prompt_hash = "abc123",
                         content = "test content",
                         raw_response = list(id = "test-id", choices = list())) {
  list(
    content       = content,
    model         = "gpt-4o-test",
    usage         = list(prompt_tokens = 10L, completion_tokens = 5L,
                         total_tokens = 15L),
    finish_reason = "stop",
    raw_response  = raw_response,
    prompt_hash   = prompt_hash,
    request_id    = "req_test"
  )
}

# ---- init_response_cache ----------------------------------------------------

test_that("init_response_cache returns ResponseCache with default settings", {
  td <- withr::local_tempdir()
  cache <- init_response_cache(td)
  expect_s3_class(cache, "ResponseCache")
  expect_true(cache$enabled)
  expect_equal(cache$cache_subdir, "api_responses")
  expect_true(dir.exists(cache$cache_dir))
})

test_that("init_response_cache respects config$audit$capture_raw_responses = FALSE", {
  td <- withr::local_tempdir()
  config <- list(audit = list(capture_raw_responses = FALSE))
  cache <- init_response_cache(td, config = config)
  expect_false(cache$enabled)
  # Disabled cache should not create the directory (no point littering disk)
  expect_false(dir.exists(cache$cache_dir))
})

test_that("init_response_cache uses config$audit$response_cache_dir override", {
  td <- withr::local_tempdir()
  config <- list(audit = list(capture_raw_responses = TRUE,
                              response_cache_dir = "raw_responses"))
  cache <- init_response_cache(td, config = config)
  expect_equal(cache$cache_subdir, "raw_responses")
  expect_true(grepl("raw_responses$", cache$cache_dir))
  expect_true(dir.exists(cache$cache_dir))
})

# ---- cache_response ---------------------------------------------------------

test_that("cache_response writes raw_response keyed by prompt_hash", {
  td <- withr::local_tempdir()
  cache <- init_response_cache(td)
  ai_result <- mk_ai_result(prompt_hash = "deadbeef")

  rel_path <- cache_response(cache, ai_result)
  expect_equal(rel_path, file.path("api_responses", "deadbeef.json"))
  expect_true(file.exists(file.path(td, "api_responses", "deadbeef.json")))
  expect_equal(cache$state$n_written, 1L)
})

test_that("cache_response stores raw_response payload as round-trippable JSON", {
  td <- withr::local_tempdir()
  cache <- init_response_cache(td)
  payload <- list(id = "msg_xyz", model = "gpt-4o",
                  choices = list(list(text = "answer", finish_reason = "stop")))
  ai_result <- mk_ai_result(prompt_hash = "rt-test", raw_response = payload)

  cache_response(cache, ai_result)
  read_back <- jsonlite::fromJSON(
    file.path(td, "api_responses", "rt-test.json"),
    simplifyVector = FALSE
  )
  expect_equal(read_back$id, "msg_xyz")
  expect_equal(read_back$model, "gpt-4o")
  expect_equal(read_back$choices[[1]]$text, "answer")
})

test_that("cache_response deduplicates identical requests (same prompt_hash)", {
  td <- withr::local_tempdir()
  cache <- init_response_cache(td)
  ai_result <- mk_ai_result(prompt_hash = "dedup-key")

  # First call writes the file.
  rel1 <- cache_response(cache, ai_result)
  expect_equal(cache$state$n_written, 1L)
  expect_equal(cache$state$n_dedup_skipped, 0L)

  # Second call with same prompt_hash should return the existing path and
  # increment the dedup-skip counter, NOT rewrite the file.
  mtime_before <- file.info(file.path(td, "api_responses", "dedup-key.json"))$mtime
  Sys.sleep(0.01)
  rel2 <- cache_response(cache, ai_result)
  mtime_after  <- file.info(file.path(td, "api_responses", "dedup-key.json"))$mtime

  expect_identical(rel1, rel2)
  expect_equal(cache$state$n_written, 1L)         # no second write
  expect_equal(cache$state$n_dedup_skipped, 1L)
  expect_identical(mtime_before, mtime_after)     # file untouched
})

test_that("cache_response is no-op when cache is disabled", {
  td <- withr::local_tempdir()
  cache <- init_response_cache(td,
                                config = list(audit = list(capture_raw_responses = FALSE)))
  ai_result <- mk_ai_result(prompt_hash = "noop")

  rel <- cache_response(cache, ai_result)
  expect_identical(rel, NA_character_)
  expect_false(dir.exists(cache$cache_dir))
})

test_that("cache_response returns NA_character_ on malformed ai_result", {
  td <- withr::local_tempdir()
  cache <- init_response_cache(td)

  # NULL ai_result
  expect_identical(cache_response(cache, NULL), NA_character_)
  # missing prompt_hash
  expect_identical(
    cache_response(cache, list(raw_response = list(x = 1))),
    NA_character_
  )
  # missing raw_response
  expect_identical(
    cache_response(cache, list(prompt_hash = "h")),
    NA_character_
  )
})

test_that("cache_response counter survives pass-by-value (env-backed state)", {
  # Regression test: pre-T1.4, AuditLog used a plain list field for n_written
  # which never updated due to R's pass-by-value semantics. ResponseCache uses
  # an environment-backed state to avoid that bug. Verify by calling
  # cache_response 5 times from a separate function and reading the counter
  # back from the original cache object.
  td <- withr::local_tempdir()
  cache <- init_response_cache(td)

  write_n <- function(cache, n) {
    for (i in seq_len(n)) {
      cache_response(cache,
                     mk_ai_result(prompt_hash = sprintf("hash-%d", i)))
    }
  }
  write_n(cache, 5)
  expect_equal(cache$state$n_written, 5L)
})

# ---- read_cached_response ---------------------------------------------------

test_that("read_cached_response retrieves a previously-cached payload", {
  td <- withr::local_tempdir()
  cache <- init_response_cache(td)
  payload <- list(model = "test-model", text = "hello world")
  cache_response(cache, mk_ai_result(prompt_hash = "read-test",
                                      raw_response = payload))

  recovered <- read_cached_response(cache, "read-test")
  expect_equal(recovered$model, "test-model")
  expect_equal(recovered$text, "hello world")
})

test_that("read_cached_response returns NULL on missing key", {
  td <- withr::local_tempdir()
  cache <- init_response_cache(td)
  expect_null(read_cached_response(cache, "nonexistent-hash"))
})

test_that("read_cached_response returns NULL when cache is disabled", {
  td <- withr::local_tempdir()
  cache <- init_response_cache(td,
                                config = list(audit = list(capture_raw_responses = FALSE)))
  expect_null(read_cached_response(cache, "anything"))
})

test_that("read_cached_response rejects malformed prompt_hash inputs", {
  td <- withr::local_tempdir()
  cache <- init_response_cache(td)
  expect_null(read_cached_response(cache, NULL))
  expect_null(read_cached_response(cache, ""))
  expect_null(read_cached_response(cache, c("a", "b")))
  expect_null(read_cached_response(cache, 42))
})

# ---- print method -----------------------------------------------------------

test_that("print.ResponseCache reports state without error", {
  td <- withr::local_tempdir()
  cache <- init_response_cache(td)
  cache_response(cache, mk_ai_result(prompt_hash = "print-test"))

  expect_output(print(cache), "ResponseCache")
  expect_output(print(cache), "Enabled.*TRUE")
  expect_output(print(cache), "Written.*1")
})
