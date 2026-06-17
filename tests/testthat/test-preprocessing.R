# Tests for text preprocessing (08_preprocessing.R)

test_that("preprocess_text removes empty and NA entries", {
  data <- tibble::tibble(
    std_text = c("Valid text here for testing", NA, "", "Another valid entry here"),
    std_id = paste0("e", 1:4)
  )
  config <- list(min_text_length = 5, max_text_length = 10000,
                 remove_urls = TRUE, remove_mentions = TRUE,
                 remove_hashtags = FALSE, lowercase = FALSE,
                 custom_cleaning_rules = list(),
                 source_type = "generic")
  result <- preprocess_text(data, config)
  expect_equal(nrow(result), 2)
  expect_true(all(!is.na(result$std_text)))
})

test_that("preprocess_text filters by min_text_length", {
  data <- tibble::tibble(
    std_text = c("Short", "This is a longer text that should pass the filter easily"),
    std_id = c("e1", "e2")
  )
  config <- list(min_text_length = 20, max_text_length = 10000,
                 remove_urls = TRUE, remove_mentions = TRUE,
                 remove_hashtags = FALSE, lowercase = FALSE,
                 custom_cleaning_rules = list(),
                 source_type = "generic")
  result <- preprocess_text(data, config)
  expect_equal(nrow(result), 1)
})

test_that("preprocess_text deduplicates exact matches", {
  data <- tibble::tibble(
    std_text = c("Same text here for dedup testing", "Same text here for dedup testing", "Different text entry here"),
    std_id = c("e1", "e2", "e3")
  )
  config <- list(min_text_length = 5, max_text_length = 10000,
                 remove_urls = TRUE, remove_mentions = TRUE,
                 remove_hashtags = FALSE, lowercase = FALSE,
                 custom_cleaning_rules = list(),
                 source_type = "generic")
  result <- preprocess_text(data, config)
  expect_equal(nrow(result), 2)
})

test_that("preprocess_text validates required columns", {
  data <- tibble::tibble(wrong_col = "text")
  config <- list(min_text_length = 5, max_text_length = 10000,
                 source_type = "generic")
  expect_error(preprocess_text(data, config), "std_text")
})

# --- Transformation tests ---

test_that("URL removal works correctly", {
  texts <- c(
    "Check https://example.com/page for info",
    "Visit www.example.org for details",
    "No URLs in this text at all"
  )
  config <- list(remove_urls = TRUE, remove_mentions = FALSE,
                 remove_hashtags = FALSE, lowercase = FALSE)
  result <- pakhom:::.clean_text(texts, config, "generic")
  expect_false(grepl("https://", result[1]))
  expect_false(grepl("www\\.", result[2]))
  expect_equal(trimws(result[3]), "No URLs in this text at all")
})

test_that("mention removal works for Reddit", {
  texts <- c("Thanks u/someuser for the tip", "Check r/focus for more")
  config <- list(remove_urls = FALSE, remove_mentions = TRUE,
                 remove_hashtags = FALSE, lowercase = FALSE)
  result <- pakhom:::.clean_reddit(texts, config)
  expect_false(grepl("u/someuser", result[1]))
  expect_true(grepl("\\[subreddit\\]", result[2]))
})

test_that("hashtag removal works", {
  texts <- c("Feeling #tired and #anxious today")
  config <- list(remove_urls = FALSE, remove_mentions = FALSE,
                 remove_hashtags = TRUE, lowercase = FALSE)
  result <- pakhom:::.clean_reddit(texts, config)
  expect_false(grepl("#tired", result[1]))
  expect_false(grepl("#anxious", result[1]))
})

test_that("HTML entity decoding works", {
  texts <- c("Tom &amp; Jerry &lt;3", "She said &quot;hello&quot; &#39;again&#39;")
  result <- pakhom:::.decode_html_entities(texts)
  expect_equal(result[1], "Tom & Jerry <3")
  expect_equal(result[2], "She said \"hello\" 'again'")
})

test_that("Unicode escape decoding works", {
  texts <- c("It<U+2019>s a test", "No escapes here")
  result <- pakhom:::.decode_unicode_escapes(texts)
  expect_true(grepl("\u2019", result[1]))
  expect_equal(result[2], "No escapes here")
})

test_that("lowercase conversion works when enabled", {
  data <- tibble::tibble(
    std_text = c("THIS IS UPPERCASE TEXT for testing purposes here"),
    std_id = "e1"
  )
  config <- list(min_text_length = 5, max_text_length = 10000,
                 remove_urls = FALSE, remove_mentions = FALSE,
                 remove_hashtags = FALSE, lowercase = TRUE,
                 custom_cleaning_rules = list(),
                 source_type = "generic")
  result <- preprocess_text(data, config)
  expect_equal(result$std_text[1], "this is uppercase text for testing purposes here")
})

test_that("custom cleaning rules are applied", {
  texts <- c("PII: SSN 123-45-6789 should be removed")
  config <- list(
    remove_urls = FALSE, remove_mentions = FALSE,
    remove_hashtags = FALSE, lowercase = FALSE,
    custom_cleaning_rules = list(
      list(pattern = "\\d{3}-\\d{2}-\\d{4}", replacement = "[REDACTED]")
    )
  )
  result <- pakhom:::.clean_text(texts, config, "generic")
  expect_true(grepl("\\[REDACTED\\]", result[1]))
  expect_false(grepl("123-45-6789", result[1]))
})

test_that("Reddit markdown artifacts are stripped", {
  texts <- c("This is **bold** and *italic* text ~~strikethrough~~")
  config <- list(remove_urls = FALSE, remove_mentions = FALSE,
                 remove_hashtags = FALSE, lowercase = FALSE)
  result <- pakhom:::.clean_reddit(texts, config)
  expect_false(grepl("\\*\\*", result[1]))
  expect_false(grepl("~~", result[1]))
  expect_true(grepl("bold", result[1]))
  expect_true(grepl("italic", result[1]))
})
