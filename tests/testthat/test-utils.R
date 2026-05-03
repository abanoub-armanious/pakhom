# Tests for utility functions (utils.R and utils_validation.R)

test_that("make_safe_filename handles special characters", {
  expect_equal(make_safe_filename("Hello World!"), "hello_world")
  expect_equal(make_safe_filename("file/with\\slashes"), "filewithslashes")
  expect_equal(make_safe_filename("dots.and.periods"), "dotsandperiods")
})

test_that("make_safe_filename handles empty input", {
  result <- make_safe_filename("!!!@@@")
  expect_equal(result, "unnamed")
})

test_that("make_safe_filename truncates long names", {
  long_name <- paste(rep("a", 100), collapse = "")
  result <- make_safe_filename(long_name)
  expect_true(nchar(result) <= 80)
})

test_that("truncate_text works correctly", {
  expect_equal(truncate_text("short", 200), "short")
  long_text <- paste(rep("a", 300), collapse = "")
  truncated <- truncate_text(long_text, 200)
  expect_equal(nchar(truncated), 200)
  expect_true(grepl("\\.\\.\\.$", truncated))
})

test_that("truncate_text handles NA", {
  expect_true(is.na(truncate_text(NA, 200)))
})

test_that("truncate_text returns text unchanged when under limit", {
  expect_equal(truncate_text("hello world", 200), "hello world")
})

test_that("estimate_tokens returns integer vector", {
  tokens <- estimate_tokens(c("hello world", "another text"))
  expect_type(tokens, "integer")
  expect_length(tokens, 2)
  expect_true(all(tokens > 0))
})

test_that("estimate_tokens handles NA and empty strings", {
  tokens <- estimate_tokens(c("hello", NA, ""))
  expect_type(tokens, "integer")
  expect_length(tokens, 3)
  expect_true(tokens[1] > 0)
  # NA or 0 for NA/empty input depending on tiktoken availability
  expect_true(is.na(tokens[2]) || tokens[2] == 0L)
})

test_that("null coalescing operator works", {
  expect_equal(NULL %||% 42, 42)
  expect_equal(99 %||% 42, 99)
  expect_equal("text" %||% "default", "text")
})

test_that("validate_data_columns catches missing columns", {
  data <- tibble::tibble(a = 1, b = 2)
  expect_error(validate_data_columns(data, c("a", "c"), "test"), "c")
  expect_true(validate_data_columns(data, c("a", "b"), "test"))
})

test_that("validate_data_columns rejects non-data-frame input", {
  expect_error(validate_data_columns("not a df", "col", "test"), "data frame")
})

test_that("make_anchor_id creates valid IDs", {
  expect_equal(make_anchor_id("Hello World"), "hello-world")
  expect_equal(make_anchor_id("Theme #1: Test!"), "theme-1-test")
})

test_that("compute_dynamic_batches splits entries correctly", {
  texts <- c("short", paste(rep("word", 500), collapse = " "), "another short")
  batches <- compute_dynamic_batches(texts, max_batch_tokens = 50,
                                      max_batch_size = 10,
                                      chars_per_entry = 1500)
  expect_true(length(batches) >= 1)
  # All indices should be covered
  all_indices <- sort(unlist(batches))
  expect_equal(all_indices, 1:3)
})

test_that("compute_dynamic_batches respects max_batch_size", {
  texts <- rep("hello world", 20)
  batches <- compute_dynamic_batches(texts, max_batch_tokens = 100000,
                                      max_batch_size = 5,
                                      chars_per_entry = 100)
  # Each batch should have at most 5 entries
  for (b in batches) {
    expect_true(length(b) <= 5)
  }
  expect_equal(sort(unlist(batches)), 1:20)
})
