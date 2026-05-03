# Tests for dynamic batching and token estimation utilities (utils.R)
# (Relevance filtering was removed from the pipeline; these test the
#  still-live utility functions that originated in that module)

test_that("compute_dynamic_batches covers all indices exactly once", {
  texts <- paste(sample(letters, 500, replace = TRUE), collapse = "")
  texts <- rep(texts, 100)

  batches <- compute_dynamic_batches(texts, max_batch_tokens = 500, max_batch_size = 30)

  all_indices <- unlist(batches)
  expect_equal(sort(all_indices), seq_along(texts))
  # No duplicates

  expect_equal(length(all_indices), length(unique(all_indices)))
})

test_that("compute_dynamic_batches respects max_batch_size", {
  texts <- rep("short text", 100)
  batches <- compute_dynamic_batches(texts, max_batch_tokens = 999999, max_batch_size = 10)

  batch_sizes <- vapply(batches, length, integer(1))
  expect_true(all(batch_sizes <= 10))
})

test_that("compute_dynamic_batches handles NA text", {
  texts <- c("hello world", NA, "some more text", NA, "final")
  batches <- compute_dynamic_batches(texts, max_batch_tokens = 1000, max_batch_size = 50)

  all_indices <- unlist(batches)
  expect_equal(sort(all_indices), 1:5)
})

test_that("compute_dynamic_batches handles single entry", {
  batches <- compute_dynamic_batches("just one entry", max_batch_tokens = 1000)
  expect_length(batches, 1)
  expect_equal(batches[[1]], 1L)
})

test_that("compute_dynamic_batches handles empty input", {
  batches <- compute_dynamic_batches(character(0), max_batch_tokens = 1000)
  expect_length(batches, 0)
})

test_that("estimate_tokens returns integer vector", {
  tokens <- estimate_tokens(c("hello world", "a longer piece of text here", ""))
  expect_type(tokens, "integer")
  expect_length(tokens, 3)
  expect_true(tokens[1] > 0)
  expect_true(tokens[2] > tokens[1])
  expect_equal(tokens[3], 0L)
})
