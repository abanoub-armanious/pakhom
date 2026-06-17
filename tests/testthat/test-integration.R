# Integration tests — end-to-end without AI calls

test_that("export_results and report helpers work end-to-end", {
  skip_on_cran()

  data <- sample_data(10)
  data$emerged_themes <- c(rep("Focus Fragmentation", 6), rep("Policy Effectiveness", 4))
  data$theme_membership_Focus.Disruption <- c(rep(1L, 6), rep(0L, 4))
  data$theme_membership_Treatment.Efficacy <- c(rep(0L, 6), rep(1L, 4))
  data$theme_confidence <- runif(10, 0.5, 1.0)
  data$secondary_themes <- ""
  data$theme_rationale <- "code overlap"

  ts <- mock_theme_set()
  enriched_ts <- enrich_themes(ts, data)

  corr_df <- tibble::tibble(
    var1 = "sentiment_score",
    var2 = "theme_membership_Focus.Disruption",
    correlation = -0.42, p_value = 0.003,
    significant = TRUE, effect_size = "moderate"
  )

  consolidated <- list(codes = tibble::tibble(
    code_text = c("distraction", "appetite change", "meeting load"),
    frequency = c(8L, 5L, 4L),
    code_type = c("ai", "ai", "ai")
  ))

  insights <- list(
    key_findings = list(
      list(insight = "Focus disruption is linked to meeting load",
           explanation = "6 of 10 entries mention focus issues.")
    ),
    theoretical_implications = "Supports the circadian disruption hypothesis.",
    practical_implications = "Consider meeting load interventions."
  )

  tmp_dir <- withr::local_tempdir()

  # Export results
  files <- export_results(data, enriched_ts, corr_df, insights, consolidated, tmp_dir)
  expect_true(file.exists(files$sentiment_file))
  expect_true(file.exists(files$codes_file))
  expect_true(file.exists(files$themes_file))

  # Verify CSV content
  sentiment_csv <- readr::read_csv(files$sentiment_file, show_col_types = FALSE)
  expect_equal(nrow(sentiment_csv), 10)
  expect_true("sentiment_score" %in% names(sentiment_csv))

  codes_csv <- readr::read_csv(files$codes_file, show_col_types = FALSE)
  expect_equal(nrow(codes_csv), 3)

  # Verify theme JSON
  themes_json <- jsonlite::fromJSON(files$themes_file)
  expect_equal(nrow(themes_json), 2)
})

test_that("aggregate statistics produce consistent results", {
  data <- sample_data(10)
  data$emerged_themes <- c(rep("Focus Fragmentation", 6), rep("Policy Effectiveness", 4))
  data$theme_membership_Focus.Disruption <- c(rep(1L, 6), rep(0L, 4))
  data$theme_membership_Treatment.Efficacy <- c(rep(0L, 6), rep(1L, 4))
  ts <- mock_theme_set()

  theme_stats <- aggregate_theme_statistics(data, ts)
  overall_stats <- aggregate_overall_statistics(data, ts, config = mock_config())

  # Theme stats consistency
  total_from_themes <- sum(vapply(theme_stats, function(s) s$n_entries, integer(1)))
  expect_equal(total_from_themes, 10)

  # Overall stats
  expect_equal(overall_stats$total_entries, 10)
  expect_true(overall_stats$sentiment$mean >= -1 && overall_stats$sentiment$mean <= 1)

  # Percentages should sum to 100
  pcts <- vapply(theme_stats, function(s) as.numeric(s$pct_of_total), numeric(1))
  expect_equal(sum(pcts), 100, tolerance = 1)
})
