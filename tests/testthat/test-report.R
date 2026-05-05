# Tests for report generation (21_report.R)

test_that(".html_esc escapes all dangerous characters", {
  expect_equal(pakhom:::.html_esc("a & b"), "a &amp; b")
  expect_equal(pakhom:::.html_esc("<script>"), "&lt;script&gt;")
  expect_equal(pakhom:::.html_esc('She said "hi"'), "She said &quot;hi&quot;")
  expect_equal(pakhom:::.html_esc("it's"), "it&#39;s")
})

test_that(".html_esc handles NULL and NA", {
  expect_equal(pakhom:::.html_esc(NULL), "")
  expect_equal(pakhom:::.html_esc(NA), "")
})

test_that(".html_esc handles numeric input", {
  expect_equal(pakhom:::.html_esc(42), "42")
})

test_that("aggregate_overall_statistics returns expected structure", {
  data <- sample_data(10)
  data$emerged_themes <- c(rep("Sleep Disruption", 6), rep("Treatment Efficacy", 4))
  data$theme_membership_Sleep.Disruption <- c(rep(1L, 6), rep(0L, 4))
  data$theme_membership_Treatment.Efficacy <- c(rep(0L, 6), rep(1L, 4))
  ts <- mock_theme_set()

  stats <- aggregate_overall_statistics(data, ts, consolidated = NULL,
                                         learning_context = NULL, config = mock_config())
  expect_true(is.list(stats))
  expect_equal(stats$total_entries, 10)
  expect_true(!is.null(stats$sentiment))
  expect_true(!is.null(stats$sentiment$mean))
  expect_true(!is.null(stats$sentiment$pct_negative))
  expect_true(!is.null(stats$sentiment$pct_positive))
})

test_that("aggregate_theme_statistics returns one entry per theme", {
  data <- sample_data(10)
  data$emerged_themes <- c(rep("Sleep Disruption", 6), rep("Treatment Efficacy", 4))
  data$theme_membership_Sleep.Disruption <- c(rep(1L, 6), rep(0L, 4))
  data$theme_membership_Treatment.Efficacy <- c(rep(0L, 6), rep(1L, 4))
  ts <- mock_theme_set()

  theme_stats <- aggregate_theme_statistics(data, ts)
  expect_equal(length(theme_stats), 2)
  expect_true("Sleep Disruption" %in% names(theme_stats))
  expect_true("Treatment Efficacy" %in% names(theme_stats))

  # Check per-theme stats structure
  sd_stats <- theme_stats[["Sleep Disruption"]]
  expect_equal(sd_stats$n_entries, 6)
  expect_true(!is.null(sd_stats$sentiment))
})

test_that("export_results creates expected files", {
  skip_on_cran()

  data <- sample_data(10)
  data$emerged_themes <- c(rep("Sleep Disruption", 6), rep("Treatment Efficacy", 4))
  data$theme_membership_Sleep.Disruption <- c(rep(1L, 6), rep(0L, 4))
  data$theme_membership_Treatment.Efficacy <- c(rep(0L, 6), rep(1L, 4))
  ts <- mock_theme_set()

  tmp_dir <- withr::local_tempdir()

  corr_df <- tibble::tibble(
    var1 = "sentiment_score", var2 = "theme_membership_Sleep.Disruption",
    correlation = 0.35, p_value = 0.01, significant = TRUE,
    effect_size = "moderate"
  )

  files <- export_results(
    data = data,
    theme_set = ts,
    correlations_df = corr_df,
    insights = list(key_findings = list()),
    consolidated = list(codes = tibble::tibble(
      code_text = c("insomnia", "appetite"), frequency = c(5L, 3L),
      code_type = c("ai", "ai")
    )),
    output_dir = tmp_dir
  )

  expect_true(file.exists(files$sentiment_file))
  expect_true(file.exists(files$codes_file))
  expect_true(file.exists(files$correlations_file))
  expect_true(file.exists(files$themes_file))
})

# --- verify_run_integrity tests ---

test_that("verify_run_integrity detects complete run", {
  tmp_dir <- withr::local_tempdir()
  # Sprint-4 update: integrity check now also requires the Tier-0 + Tier-1
  # outputs (run_metadata.json, rules/methodology_rules.md, fabrication
  # log, audit log, api_responses dir per AC4). Test fixture creates them.
  core_files <- c("sentiment_scores.csv", "consolidated_codes.csv",
                   "correlations.csv", "themes.json", "analysis_report.Rmd",
                   "run_metadata.json", "fabrication_log.csv",
                   "ai_decisions.jsonl")
  for (f in core_files) file.create(file.path(tmp_dir, f))
  dir.create(file.path(tmp_dir, "theme_entries"))
  dir.create(file.path(tmp_dir, "rules"))
  file.create(file.path(tmp_dir, "rules", "methodology_rules.md"))
  dir.create(file.path(tmp_dir, "api_responses"))

  result <- verify_run_integrity(tmp_dir, config = list())
  expect_true(result$complete)
  expect_length(result$missing, 0)
})

test_that("verify_run_integrity detects missing files", {
  tmp_dir <- withr::local_tempdir()
  # Only create themes.json — everything else missing
  file.create(file.path(tmp_dir, "themes.json"))

  result <- verify_run_integrity(tmp_dir, config = list())
  expect_false(result$complete)
  expect_true("sentiment_scores.csv" %in% result$missing)
  expect_true("consolidated_codes.csv" %in% result$missing)
  expect_false("themes.json" %in% result$missing)
})

test_that("verify_run_integrity checks conditional files from config", {
  tmp_dir <- withr::local_tempdir()
  core_files <- c("sentiment_scores.csv", "consolidated_codes.csv",
                   "themes.json", "analysis_report.Rmd")
  for (f in core_files) file.create(file.path(tmp_dir, f))
  dir.create(file.path(tmp_dir, "theme_entries"))
  # Phase 39: correlation_plot.png is now expected only when
  # correlations.csv has at least one data row (small samples
  # legitimately produce a 0-row correlations.csv and skip the plot).
  # Drop a one-row correlations.csv so the integrity check expects the
  # plot to exist -- the test's intent.
  writeLines(c("var1,var2,correlation,p_value,significant,effect_size",
               "a,b,0.5,0.01,TRUE,medium"),
             file.path(tmp_dir, "correlations.csv"))

  # With report enabled, should expect HTML + styles + theme_details
  config <- list(output = list(generate_report = TRUE, generate_correlation_plot = TRUE))
  result <- verify_run_integrity(tmp_dir, config)
  expect_false(result$complete)
  expect_true("analysis_report.html" %in% result$missing)
  expect_true("correlation_plot.png" %in% result$missing)
})
