# Regression tests for the Batch-5 crash/robustness fixes (audit 2026-06-11).

test_that("parse_json_safely returns NULL (does not crash) on atomic JSON with expected_key", {
  # Pre-fix: parsed[["results"]] on an atomic value threw "subscript out of
  # bounds" and crashed the caller (e.g. the Mode 1 provocateur path).
  expect_null(parse_json_safely("42", expected_key = "results"))
  expect_null(parse_json_safely('"hi"', expected_key = "results"))
  expect_null(parse_json_safely("[1,2,3]", expected_key = "provocations"))
  expect_null(parse_json_safely("true", expected_key = "results"))
  # A valid object with the key still works.
  ok <- parse_json_safely('{"results":[1,2]}', expected_key = "results")
  expect_false(is.null(ok))
  expect_equal(ok$results, c(1L, 2L))
})

test_that(".build_metric_columns_block does not crash on an Inf metric value", {
  df <- data.frame(rate = c(1, 2, Inf, 4, 5))
  out <- pakhom:::.build_metric_columns_block(df, metric_cols = "rate",
                                              temporal_cols = character(0))
  expect_type(out, "character")
  expect_true(grepl("rate", out))   # Inf is dropped, finite values summarized
})

test_that(".decode_unicode_escapes keeps an invalid codepoint instead of NA-wiping the entry", {
  # intToUtf8 returns NA (without erroring) for a lone surrogate; the old code
  # then gsub'd NA into the whole string, dropping the entry.
  inp <- "before <U+D800> after"
  out <- pakhom:::.decode_unicode_escapes(inp)
  expect_false(is.na(out))
  expect_true(grepl("before", out) && grepl("after", out))
})

test_that("read_review_disposition defaults a present-but-NA disposition cell to 'continue'", {
  dir <- withr::local_tempdir()
  rr <- file.path(dir, "researcher_review")
  dir.create(rr, recursive = TRUE)
  readr::write_csv(data.frame(disposition = NA_character_),
                   file.path(rr, "review_disposition.csv"))
  expect_equal(pakhom:::read_review_disposition(dir), "continue")
})
