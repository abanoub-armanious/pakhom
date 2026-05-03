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

# --- Backward compatibility with legacy primary_emotion format ---

test_that("analyze_sentiment handles legacy primary_emotion JSON format", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  data <- tibble::tibble(
    std_id = paste0("e", 1:2),
    std_text = c(
      "I feel terrible after binge eating last night",
      "The medication really helped me sleep better"
    )
  )

  # Legacy format: primary_emotion as a string (no emotions array)
  mock_response_legacy <- '{"results": [
    {"id": 1, "sentiment_score": -0.7, "confidence": 0.9, "primary_emotion": "sadness", "emotion_intensity": 0.8},
    {"id": 2, "sentiment_score": 0.6, "confidence": 0.85, "primary_emotion": "joy", "emotion_intensity": 0.6}
  ]}'

  local_mocked_bindings(
    ai_complete_fast = function(...) list(content = mock_response_legacy),
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

  expect_equal(nrow(result), 2)
  # Legacy primary_emotion should be converted to all_emotions
  expect_equal(result$all_emotions[1], "sadness")
  expect_equal(result$all_emotions[2], "joy")
  expect_false("primary_emotion" %in% names(result))
})
