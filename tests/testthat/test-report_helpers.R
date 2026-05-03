# Tests for report helper functions (20_report_helpers.R)

test_that("get_emotion_interpretation returns known interpretation", {
  result <- get_emotion_interpretation("sadness")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  expect_true(grepl("grief|loss|pain", result))
})

test_that("get_emotion_interpretation handles uppercase input", {
  result <- get_emotion_interpretation("ANGER")
  expect_type(result, "character")
  expect_true(grepl("frustration|resentment|injustice", result))
})

test_that("get_emotion_interpretation handles unknown emotions", {
  result <- get_emotion_interpretation("bewilderment")
  expect_type(result, "character")
  expect_true(grepl("bewilderment", result))
})

test_that("get_emotion_interpretation covers all built-in emotions", {
  known_emotions <- c("sadness", "anger", "fear", "disgust", "joy",
                       "surprise", "trust", "anticipation", "frustration",
                       "anxiety", "hope", "shame", "guilt", "confusion",
                       "resignation", "relief", "gratitude", "empathy")
  for (em in known_emotions) {
    result <- get_emotion_interpretation(em)
    expect_true(nchar(result) > 0, info = paste("Empty result for", em))
    # Known emotions should NOT have the fallback pattern
    expect_false(grepl(paste0("reflects ", em, "-related"), result),
                 info = paste("Fallback used for known emotion:", em))
  }
})

test_that("aggregate_overall_statistics returns required fields", {
  data <- tibble::tibble(
    std_id = paste0("e", 1:5),
    std_text = paste("Text", 1:5),
    emerged_themes = c("A", "A", "B", "B", "A"),
    theme_membership_A = c(1L, 1L, 0L, 0L, 1L),
    theme_membership_B = c(0L, 0L, 1L, 1L, 0L),
    sentiment_score = c(0.5, -0.3, 0.1, -0.8, 0.2),
    all_emotions = c("joy", "sadness", "neutral", "anger", "hope")
  )
  theme_set <- create_theme_set(list(
    list(name = "A", description = "Theme A", codes_included = "c1"),
    list(name = "B", description = "Theme B", codes_included = "c2")
  ))
  result <- aggregate_overall_statistics(data, theme_set)
  expect_type(result, "list")
  expect_equal(result$total_entries, 5)
  expect_equal(result$n_themes, 2)
  expect_true(!is.null(result$sentiment))
  expect_true(!is.null(result$sentiment$mean))
  expect_true(!is.null(result$sentiment$sd))
  expect_true(!is.null(result$emotions))
  expect_true(!is.null(result$themes))
})

test_that("aggregate_overall_statistics handles missing optional columns", {
  data <- tibble::tibble(
    std_id = paste0("e", 1:3),
    std_text = paste("Text", 1:3),
    sentiment_score = c(0.1, -0.2, 0.3)
  )
  theme_set <- create_theme_set(list(
    list(name = "Only", description = "Only theme", codes_included = "c1")
  ))
  # No emerged_themes or all_emotions columns
  result <- aggregate_overall_statistics(data, theme_set)
  expect_equal(result$total_entries, 3)
  expect_equal(result$n_themes, 1)
  # themes df should be empty since no emerged_themes column
  expect_equal(nrow(result$themes), 0)
})
