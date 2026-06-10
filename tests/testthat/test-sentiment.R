# Tests for sentiment analysis (10_sentiment.R)

test_that("analyze_sentiment validates required columns", {
  data <- tibble::tibble(wrong_col = "text")
  mock_provider <- structure(list(), class = "AIProvider")
  expect_error(analyze_sentiment(data, provider = mock_provider), "std_text")
})

test_that("analyze_sentiment validates provider is not NULL", {
  data <- tibble::tibble(std_text = "hello", std_id = "e1")
  expect_error(analyze_sentiment(data, provider = NULL), "provider|NULL")
})

test_that("analyze_sentiment validates provider type", {
  data <- tibble::tibble(std_text = "hello", std_id = "e1")
  expect_error(analyze_sentiment(data, provider = "not_a_provider"), "AIProvider")
})

# --- Result assignment tests ---

test_that(".assign_sentiment_results handles data.frame results (pass-by-value)", {
  data <- tibble::tibble(
    std_id = paste0("e", 1:5),
    std_text = paste("Entry", 1:5),
    sentiment_score = rep(NA_real_, 5),
    confidence = rep(NA_real_, 5),
    all_emotions = rep(NA_character_, 5),
    emotion_intensity = rep(NA_real_, 5)
  )

  results <- data.frame(
    id = c(1, 2, 3),
    sentiment_score = c(-0.5, 0.3, 0.0),
    confidence = c(0.9, 0.8, 0.7),
    all_emotions = c("sadness; anger", "hope", "neutral"),
    emotion_intensity = c(0.8, 0.6, 0.3),
    stringsAsFactors = FALSE
  )

  # Critical: must capture return value (pass-by-value)
  updated <- pakhom:::.assign_sentiment_results(data, results, 1:3)
  expect_equal(updated$sentiment_score[1], -0.5)
  expect_equal(updated$sentiment_score[2], 0.3)
  expect_equal(updated$all_emotions[1], "sadness; anger")
  expect_equal(updated$all_emotions[2], "hope")
  # Unassigned entries stay NA
  expect_true(is.na(updated$sentiment_score[4]))
})

test_that(".assign_sentiment_results handles list-of-lists results", {
  data <- tibble::tibble(
    std_id = paste0("e", 1:3),
    std_text = paste("Entry", 1:3),
    sentiment_score = rep(NA_real_, 3),
    confidence = rep(NA_real_, 3),
    all_emotions = rep(NA_character_, 3),
    emotion_intensity = rep(NA_real_, 3)
  )

  results <- list(
    list(id = 1, sentiment_score = -0.8, confidence = 0.95,
         all_emotions = "anger", emotion_intensity = 0.9),
    list(id = 2, sentiment_score = 0.6, confidence = 0.85,
         all_emotions = "joy; surprise", emotion_intensity = 0.7)
  )

  updated <- pakhom:::.assign_sentiment_results(data, results, 1:3)
  expect_equal(updated$sentiment_score[1], -0.8)
  expect_equal(updated$all_emotions[1], "anger")
  expect_equal(updated$all_emotions[2], "joy; surprise")
  expect_true(is.na(updated$sentiment_score[3]))
})

test_that(".assign_sentiment_results clamps scores to [-1, 1]", {
  data <- tibble::tibble(
    std_id = "e1", std_text = "test",
    sentiment_score = NA_real_, confidence = NA_real_,
    all_emotions = NA_character_,
    emotion_intensity = NA_real_
  )

  results <- list(
    list(id = 1, sentiment_score = 1.5, confidence = 0.9,
         all_emotions = "joy", emotion_intensity = 0.8)
  )

  updated <- pakhom:::.assign_sentiment_results(data, results, 1L)
  expect_equal(updated$sentiment_score[1], 1.0)
})

test_that(".assign_sentiment_results ignores out-of-range IDs", {
  data <- tibble::tibble(
    std_id = paste0("e", 1:3),
    std_text = paste("Entry", 1:3),
    sentiment_score = rep(NA_real_, 3),
    confidence = rep(NA_real_, 3),
    all_emotions = rep(NA_character_, 3),
    emotion_intensity = rep(NA_real_, 3)
  )

  results <- list(
    list(id = 99, sentiment_score = 0.5, confidence = 0.9,
         all_emotions = "joy", emotion_intensity = 0.7)
  )

  updated <- pakhom:::.assign_sentiment_results(data, results, 1:3)
  expect_true(all(is.na(updated$sentiment_score)))
})

# --- AI integration path test ---

test_that("analyze_sentiment populates all columns with mocked AI", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  data <- tibble::tibble(
    std_id = paste0("e", 1:3),
    std_text = c(
      "I feel terrible after binge eating last night",
      "The medication really helped me sleep better",
      "I am worried about the side effects of my treatment"
    )
  )

  mock_response <- '{"results": [
    {"id": 1, "sentiment_score": -0.7, "confidence": 0.9, "emotions": ["sadness", "anger"], "emotion_intensity": 0.8},
    {"id": 2, "sentiment_score": 0.6, "confidence": 0.85, "emotions": ["joy"], "emotion_intensity": 0.6},
    {"id": 3, "sentiment_score": -0.3, "confidence": 0.8, "emotions": ["fear"], "emotion_intensity": 0.5}
  ]}'

  # Mock returns the structured-list shape of ai_complete() (T1.1 refactor).
  # Caller in 10_sentiment.R extracts $content; mock supplies that field.
  local_mocked_bindings(
    ai_complete_fast = function(...) list(content = mock_response),
    .package = "pakhom"
  )

  config <- list(
    batch_size = 10,
    dynamic_batching = FALSE,
    include_confidence = TRUE,
    include_emotions = TRUE,
    emotion_categories = c("joy", "sadness", "anger", "fear")
  )

  result <- analyze_sentiment(data, provider = mock_provider(),
                               config = config, research_focus = "test")

  expect_equal(nrow(result), 3)
  expect_true(all(!is.na(result$sentiment_score)))
  expect_true(all(!is.na(result$confidence)))
  expect_true("all_emotions" %in% names(result))
  expect_true(all(!is.na(result$all_emotions)))
  expect_true(all(!is.na(result$emotion_intensity)))

  # Check specific values
  expect_equal(result$sentiment_score[1], -0.7)
  expect_equal(result$all_emotions[1], "sadness; anger")
  expect_equal(result$all_emotions[2], "joy")
  expect_equal(result$sentiment_score[3], -0.3)
})


# --- Partial-checkpoint resume (crash-safety consumption) --------------------
# A mock provider that answers whatever entry ids the prompt asks for, and
# records every id it was asked to score.

.sentiment_resume_mock <- function(counter_env) {
  function(provider, prompt, system_prompt, ...) {
    counter_env$calls <- counter_env$calls + 1L
    ids <- as.integer(gsub("\\[|\\]", "",
      regmatches(prompt, gregexpr("\\[[0-9]+\\]", prompt))[[1]]))
    counter_env$ids <- c(counter_env$ids, ids)
    rows <- vapply(ids, function(i) sprintf(
      '{"id": %d, "sentiment_score": 0.5, "confidence": 0.9, "emotions": ["joy"], "emotion_intensity": 0.7}',
      i), character(1))
    list(content = paste0('{"results": [', paste(rows, collapse = ","), "]}"))
  }
}

.sentiment_resume_fixture <- function(n = 5L) {
  tibble::tibble(
    std_id = paste0("e", seq_len(n)),
    std_text = paste("Entry text number", seq_len(n)),
    sentiment_score = NA_real_, confidence = NA_real_,
    all_emotions = NA_character_, emotion_intensity = NA_real_
  )
}

test_that("analyze_sentiment adopts scored rows from a partial checkpoint and skips them", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mgr <- init_checkpoints(tmp, config_hash = "x")

  partial_data <- .sentiment_resume_fixture()
  partial_data$sentiment_score[1:3] <- c(0.11, 0.12, 0.13)
  partial_data$confidence[1:3] <- 0.8
  partial_data$all_emotions[1:3] <- "trust"
  partial_data$emotion_intensity[1:3] <- 0.4
  save_partial_checkpoint(mgr, "sentiment_done", partial_data, 3L)

  counter <- new.env(); counter$calls <- 0L; counter$ids <- integer(0)
  local_mocked_bindings(ai_complete_fast = .sentiment_resume_mock(counter),
                        .package = "pakhom")

  result <- analyze_sentiment(
    .sentiment_resume_fixture()[, c("std_id", "std_text")],
    provider = mock_provider(),
    config = list(batch_size = 10, dynamic_batching = FALSE),
    checkpoint = mgr
  )

  # Only the two unscored rows were sent to the provider
  expect_equal(counter$calls, 1L)
  expect_setequal(counter$ids, 4:5)
  # Adopted rows keep the partial's exact values; fresh rows get the mock's
  expect_equal(result$sentiment_score[1:3], c(0.11, 0.12, 0.13))
  expect_equal(result$all_emotions[1], "trust")
  expect_equal(result$sentiment_score[4:5], c(0.5, 0.5))
})

test_that("analyze_sentiment merges the partial by std_id, not row position", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mgr <- init_checkpoints(tmp, config_hash = "x")

  # The partial's rows are in reversed order relative to the resume input
  partial_data <- .sentiment_resume_fixture()[5:1, ]
  partial_data$sentiment_score[partial_data$std_id == "e4"] <- -0.44
  partial_data$sentiment_score[partial_data$std_id == "e5"] <- -0.55
  partial_data$confidence[partial_data$std_id %in% c("e4", "e5")] <- 0.9
  partial_data$all_emotions[partial_data$std_id %in% c("e4", "e5")] <- "fear"
  partial_data$emotion_intensity[partial_data$std_id %in% c("e4", "e5")] <- 0.6
  save_partial_checkpoint(mgr, "sentiment_done", partial_data, 2L)

  counter <- new.env(); counter$calls <- 0L; counter$ids <- integer(0)
  local_mocked_bindings(ai_complete_fast = .sentiment_resume_mock(counter),
                        .package = "pakhom")

  result <- analyze_sentiment(
    .sentiment_resume_fixture()[, c("std_id", "std_text")],
    provider = mock_provider(),
    config = list(batch_size = 10, dynamic_batching = FALSE),
    checkpoint = mgr
  )

  # e4/e5 land on the rows with those ids despite the order mismatch
  expect_equal(result$sentiment_score[result$std_id == "e4"], -0.44)
  expect_equal(result$sentiment_score[result$std_id == "e5"], -0.55)
  expect_setequal(counter$ids, 1:3)
})

test_that("analyze_sentiment runs fresh on a corrupt partial checkpoint", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mgr <- init_checkpoints(tmp, config_hash = "x")
  writeLines("not a valid rds payload",
             file.path(mgr$checkpoint_dir, "sentiment_done_partial.rds"))

  counter <- new.env(); counter$calls <- 0L; counter$ids <- integer(0)
  local_mocked_bindings(ai_complete_fast = .sentiment_resume_mock(counter),
                        .package = "pakhom")

  expect_no_error(result <- suppressWarnings(analyze_sentiment(
    .sentiment_resume_fixture()[, c("std_id", "std_text")],
    provider = mock_provider(),
    config = list(batch_size = 10, dynamic_batching = FALSE),
    checkpoint = mgr
  )))
  expect_setequal(counter$ids, 1:5)
  expect_true(all(!is.na(result$sentiment_score)))
})

test_that("analyze_sentiment makes zero provider calls when the partial covers everything", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mgr <- init_checkpoints(tmp, config_hash = "x")

  partial_data <- .sentiment_resume_fixture()
  partial_data$sentiment_score <- seq(0.1, 0.5, by = 0.1)
  partial_data$confidence <- 0.9
  partial_data$all_emotions <- "joy"
  partial_data$emotion_intensity <- 0.5
  save_partial_checkpoint(mgr, "sentiment_done", partial_data, 5L)

  counter <- new.env(); counter$calls <- 0L; counter$ids <- integer(0)
  local_mocked_bindings(ai_complete_fast = .sentiment_resume_mock(counter),
                        .package = "pakhom")

  result <- analyze_sentiment(
    .sentiment_resume_fixture()[, c("std_id", "std_text")],
    provider = mock_provider(),
    config = list(batch_size = 10, dynamic_batching = FALSE),
    checkpoint = mgr
  )

  expect_equal(counter$calls, 0L)
  expect_equal(result$sentiment_score, seq(0.1, 0.5, by = 0.1))
})

test_that("analyze_sentiment ignores partial rows whose std_id left the sample", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mgr <- init_checkpoints(tmp, config_hash = "x")

  partial_data <- .sentiment_resume_fixture()
  partial_data$std_id[2] <- "ghost_entry"   # no longer in the sample
  partial_data$sentiment_score[1:2] <- c(0.11, 0.99)
  partial_data$confidence[1:2] <- 0.8
  partial_data$all_emotions[1:2] <- "trust"
  partial_data$emotion_intensity[1:2] <- 0.4
  save_partial_checkpoint(mgr, "sentiment_done", partial_data, 2L)

  counter <- new.env(); counter$calls <- 0L; counter$ids <- integer(0)
  local_mocked_bindings(ai_complete_fast = .sentiment_resume_mock(counter),
                        .package = "pakhom")

  expect_no_error(result <- analyze_sentiment(
    .sentiment_resume_fixture()[, c("std_id", "std_text")],
    provider = mock_provider(),
    config = list(batch_size = 10, dynamic_batching = FALSE),
    checkpoint = mgr
  ))
  expect_equal(result$sentiment_score[1], 0.11)   # adopted
  expect_false(any(result$sentiment_score == 0.99))  # ghost ignored
  expect_setequal(counter$ids, 2:5)               # e2..e5 re-scored
})

test_that("save_checkpoint deletes the step's superseded partial", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  mgr <- init_checkpoints(tmp, config_hash = "x")

  save_partial_checkpoint(mgr, "sentiment_done", list(x = 1), 50L)
  partial_path <- file.path(mgr$checkpoint_dir, "sentiment_done_partial.rds")
  expect_true(file.exists(partial_path))

  save_checkpoint(mgr, "sentiment_done", list(x = 2))
  expect_false(file.exists(partial_path))
})
