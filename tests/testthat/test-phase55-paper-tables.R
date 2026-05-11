# Tests for Phase 55: paper-style per-theme + per-subtheme tables
#
# The deliverables under test are:
#   * .detect_metric_columns -- dataset-agnostic metric detection
#   * .compute_subtheme_statistics -- per-subtheme stats with metric-tagged
#     example quotes
#   * .format_metric_summary / .format_metric_value / .format_metric_tag --
#     pretty-printing helpers
#   * .build_subtheme_summary_table -- HTML/Markdown table render
#   * export_theme_subtheme_summary_csvs -- per-theme paper-style CSV artifact

# ---- .detect_metric_columns -----------------------------------------------

test_that(".detect_metric_columns auto-detects numeric, non-internal columns", {
  df <- tibble::tibble(
    std_id = c("e1", "e2"),
    std_text = c("a", "b"),
    sentiment_score = c(0.1, -0.2),
    emotion_intensity = c(0.5, 0.7),
    theme_membership_Foo = c(1L, 0L),
    Drug_Rating = c(8L, 6L),
    Like_Count = c(12, 5)
  )
  out <- pakhom:::.detect_metric_columns(df)
  expect_setequal(out, c("Drug_Rating", "Like_Count"))
})

test_that(".detect_metric_columns honors explicit override via direct arg", {
  df <- tibble::tibble(
    std_id = c("e1"),
    Drug_Rating = c(8L),
    Like_Count = c(12L),
    Score = c(3L)
  )
  out <- pakhom:::.detect_metric_columns(df, explicit = c("Drug_Rating", "Score"))
  expect_setequal(out, c("Drug_Rating", "Score"))
})

test_that(".detect_metric_columns honors explicit override via config dig", {
  df <- tibble::tibble(
    std_id = c("e1"),
    Drug_Rating = c(8L),
    Like_Count = c(12L)
  )
  cfg <- list(data = list(column_mappings = list(metric_columns = "Like_Count")))
  out <- pakhom:::.detect_metric_columns(df, config = cfg)
  expect_equal(out, "Like_Count")
})

test_that(".detect_metric_columns drops non-existent override columns", {
  df <- tibble::tibble(std_id = "e1", Drug_Rating = 8L)
  out <- pakhom:::.detect_metric_columns(df, explicit = c("Drug_Rating", "Missing"))
  expect_equal(out, "Drug_Rating")
})

test_that(".detect_metric_columns returns character(0) when no metrics", {
  df <- tibble::tibble(std_id = "e1", std_text = "x", sentiment_score = 0)
  out <- pakhom:::.detect_metric_columns(df)
  expect_equal(out, character(0))
})

# ---- .format_metric_summary / .format_metric_value / .format_metric_tag ---

test_that(".format_metric_summary handles center/spread + NA + rounding", {
  expect_equal(pakhom:::.format_metric_summary(8.0, 1.5),  "8.00 (1.50)")
  expect_equal(pakhom:::.format_metric_summary(8, 1, digits = 1L), "8.0 (1.0)")
  expect_equal(pakhom:::.format_metric_summary(NA_real_, 1), "n/a")
  expect_equal(pakhom:::.format_metric_summary(1, NA_real_), "n/a")
})

test_that(".format_metric_value preserves integer-looking numerics", {
  expect_equal(pakhom:::.format_metric_value(8L), "8")
  expect_equal(pakhom:::.format_metric_value(8.0), "8")
  expect_equal(pakhom:::.format_metric_value(7.5), "7.5")
  expect_equal(pakhom:::.format_metric_value("text"), "text")
})

test_that(".format_metric_tag emits bracketed tag with present metrics only", {
  row <- data.frame(Drug_Rating = 8, Like_Count = NA_real_,
                     Score = 3.5, stringsAsFactors = FALSE)
  out <- pakhom:::.format_metric_tag(row,
                                       c("Drug_Rating", "Like_Count", "Score"))
  expect_equal(out, "[Drug_Rating: 8; Score: 3.5]")
})

test_that(".format_metric_tag returns empty string when all metrics NA", {
  row <- data.frame(A = NA_real_, B = NA_real_, stringsAsFactors = FALSE)
  expect_equal(pakhom:::.format_metric_tag(row, c("A", "B")), "")
})

# ---- .compute_subtheme_statistics -----------------------------------------

.make_theme_for_subtheme_test <- function() {
  # Phase 51 hierarchy: theme has 2 real subthemes + 0 virtual.
  list(
    name = "Daily routines",
    description = "How participants weave the medication into daily life.",
    subthemes = list(
      create_subtheme(
        name = "Morning routine",
        description = "Taking the medication first thing.",
        codes = list(create_code_object(key = "morning", name = "Morning"))
      ),
      create_subtheme(
        name = "Evening routine",
        description = "Taking the medication at night.",
        codes = list(create_code_object(key = "evening", name = "Evening"))
      )
    )
  )
}

test_that(".compute_subtheme_statistics returns one record per real subtheme", {
  theme <- .make_theme_for_subtheme_test()
  data <- tibble::tibble(
    std_id = paste0("e", 1:6),
    std_text = c("morning quote 1", "morning quote 2", "morning quote 3",
                   "evening quote 1", "evening quote 2", "evening quote 3"),
    sentiment_score = c(0.1, 0.2, 0.3, -0.1, -0.2, -0.3),
    Drug_Rating = c(8L, 7L, 9L, 6L, 5L, 4L),
    Like_Count = c(10, 12, 8, 5, 7, 3),
    theme_membership_Daily.routines = rep(1L, 6L),
    subtheme_assignments = c(rep("Morning routine", 3L),
                              rep("Evening routine", 3L))
  )
  out <- pakhom:::.compute_subtheme_statistics(
    theme = theme, data = data,
    metric_cols = c("Drug_Rating", "Like_Count"),
    quotes_per_subtheme = 3L
  )
  expect_named(out, c("Morning routine", "Evening routine"))

  morning <- out[["Morning routine"]]
  expect_equal(morning$n, 3L)
  expect_equal(morning$metric_stats$Drug_Rating$median, 8)
  expect_equal(morning$metric_stats$Drug_Rating$mean,   8)

  evening <- out[["Evening routine"]]
  expect_equal(evening$n, 3L)
  expect_equal(evening$metric_stats$Drug_Rating$median, 5)
})

test_that(".compute_subtheme_statistics skips virtual NA-named subthemes", {
  theme <- list(
    name = "Theme X",
    description = "",
    subthemes = list(
      create_subtheme(name = NA_character_, description = "",
                       codes = list(create_code_object(key = "k", name = "K"))),
      create_subtheme(name = "Real subtheme",
                       description = "Has a name.",
                       codes = list(create_code_object(key = "k2", name = "K2")))
    )
  )
  data <- tibble::tibble(
    std_id = "e1", std_text = "x",
    theme_membership_Theme.X = 1L,
    subtheme_assignments = "Real subtheme"
  )
  out <- pakhom:::.compute_subtheme_statistics(theme, data, character(0))
  expect_named(out, "Real subtheme")  # virtual is dropped
})

test_that(".compute_subtheme_statistics handles missing subtheme_assignments", {
  # Fallback path: when the data lacks a subtheme_assignments column,
  # every theme entry is treated as belonging to every subtheme.
  theme <- .make_theme_for_subtheme_test()
  data <- tibble::tibble(
    std_id = paste0("e", 1:3),
    std_text = "x",
    theme_membership_Daily.routines = rep(1L, 3L),
    Drug_Rating = c(8L, 6L, 7L)
  )
  out <- pakhom:::.compute_subtheme_statistics(
    theme, data, metric_cols = "Drug_Rating", quotes_per_subtheme = 1L
  )
  expect_equal(out$`Morning routine`$n, 3L)
  expect_equal(out$`Evening routine`$n, 3L)
})

test_that(".compute_subtheme_statistics example quotes carry metric tags", {
  theme <- .make_theme_for_subtheme_test()
  # NB: the sentiment-positioned selector filters out entries with text
  # shorter than 50 chars; pad the fixture texts so the selector keeps
  # them (and exercises the metric-tag path).
  data <- tibble::tibble(
    std_id = c("e1", "e2"),
    std_text = c(
      "I take it every morning right after I wake up because that is when I find I can stick to a routine.",
      "I take it every night just before bed because that's when I have time to focus on self-care."
    ),
    sentiment_score = c(0.1, -0.1),
    Drug_Rating = c(9L, 5L),
    theme_membership_Daily.routines = c(1L, 1L),
    subtheme_assignments = c("Morning routine", "Evening routine")
  )
  out <- pakhom:::.compute_subtheme_statistics(
    theme, data, metric_cols = "Drug_Rating", quotes_per_subtheme = 1L
  )
  expect_match(out$`Morning routine`$example_quotes[1], "Drug_Rating: 9")
  expect_match(out$`Evening routine`$example_quotes[1], "Drug_Rating: 5")
})

# ---- .build_subtheme_summary_table ----------------------------------------

test_that(".build_subtheme_summary_table returns '' when subtheme_stats empty", {
  ts <- list(subtheme_stats = list(), metric_cols = character(0))
  expect_equal(pakhom:::.build_subtheme_summary_table(ts), "")
})

test_that(".build_subtheme_summary_table emits a row per subtheme + metric cols", {
  ts <- list(
    metric_cols = c("Drug_Rating", "Like_Count"),
    subtheme_stats = list(
      `Morning` = list(name = "Morning", description = "AM dose", n = 3L,
                        metric_stats = list(
                          Drug_Rating = list(median = 8, mad = 1,
                                              mean = 7.8, sd = 0.9),
                          Like_Count  = list(median = 10, mad = 2,
                                              mean = 11, sd = 1.5)
                        ),
                        example_quotes = c("first morning quote [Drug_Rating: 8]"))
    )
  )
  html <- pakhom:::.build_subtheme_summary_table(ts)
  expect_match(html, "<table")
  # Phase 55 audit MEDIUM-2: metric column names are pretty-printed in
  # the header (underscores -> spaces). The underlying column name
  # stays canonical in the CSV + stats list.
  expect_match(html, "Median\\(MAD\\) Drug Rating")
  expect_match(html, "Mean\\(SD\\) Like Count")
  expect_match(html, "Morning")
  expect_match(html, "8.00 \\(1.00\\)")  # Median(MAD) for Drug Rating
  expect_match(html, "first morning quote")
  # Phase 55 audit MEDIUM-15: caption clarifies bracketed metrics are
  # source entry values, not subtheme aggregates.
  expect_match(html, "bracketed values after each example")
})

# ---- export_theme_subtheme_summary_csvs -----------------------------------

test_that("export_theme_subtheme_summary_csvs writes per-theme + master CSVs", {
  tmp_dir <- withr::local_tempdir()
  theme_stats <- list(
    `Theme A` = list(
      metric_cols = c("Drug_Rating"),
      subtheme_stats = list(
        `Sub 1` = list(
          name = "Sub 1", description = "First sub", n = 3L,
          metric_stats = list(
            Drug_Rating = list(median = 8, mad = 1, mean = 7.5, sd = 0.7,
                                 n_observed = 3L)
          ),
          example_quotes = c("Quote one [Drug_Rating: 8]")
        ),
        `Sub 2` = list(
          name = "Sub 2", description = "Second sub", n = 2L,
          metric_stats = list(
            Drug_Rating = list(median = 5, mad = 1, mean = 5, sd = 1,
                                 n_observed = 2L)
          ),
          example_quotes = c("Quote two [Drug_Rating: 5]")
        )
      )
    ),
    `Theme B` = list(metric_cols = character(0), subtheme_stats = list())
  )

  files <- export_theme_subtheme_summary_csvs(theme_stats, tmp_dir)
  expect_true("Theme A" %in% names(files))
  expect_false("Theme B" %in% names(files))  # empty subtheme_stats -> skipped

  per_theme <- readr::read_csv(files[["Theme A"]]$file_path,
                                  show_col_types = FALSE)
  expect_setequal(per_theme$subtheme, c("Sub 1", "Sub 2"))
  expect_true("Drug_Rating_median" %in% names(per_theme))
  expect_true("Drug_Rating_mad"    %in% names(per_theme))
  expect_true("Drug_Rating_mean"   %in% names(per_theme))
  expect_true("Drug_Rating_sd"     %in% names(per_theme))
  expect_true("examples_of_comments" %in% names(per_theme))

  master_path <- file.path(tmp_dir, "theme_summaries", "all_subthemes.csv")
  expect_true(file.exists(master_path))
  master <- readr::read_csv(master_path, show_col_types = FALSE)
  expect_equal(nrow(master), 2L)
})

test_that("export_theme_subtheme_summary_csvs returns list() when no themes have subthemes", {
  tmp_dir <- withr::local_tempdir()
  theme_stats <- list(
    `Theme A` = list(metric_cols = character(0), subtheme_stats = list())
  )
  files <- export_theme_subtheme_summary_csvs(theme_stats, tmp_dir)
  expect_length(files, 0L)
  # The output directory may or may not exist; the master CSV definitely
  # shouldn't (no rows to write).
  master_path <- file.path(tmp_dir, "theme_summaries", "all_subthemes.csv")
  expect_false(file.exists(master_path))
})

# ---- end-to-end: aggregate_theme_statistics carries subtheme_stats --------

test_that("aggregate_theme_statistics attaches subtheme_stats + metric_cols", {
  ts_obj <- create_theme_set(list(
    list(
      name = "Theme T",
      description = "",
      subthemes = list(create_subtheme(
        name = "Sub S", description = "",
        codes = list(create_code_object(key = "k", name = "K"))
      ))
    )
  ))
  # Include emotion_intensity so the parent aggregate's intensity stats
  # don't warn on a missing column (unrelated to Phase 55 but exercised).
  data <- tibble::tibble(
    std_id = c("e1", "e2"),
    std_text = c("a", "b"),
    sentiment_score = c(0.1, -0.1),
    emotion_intensity = c(0.5, 0.6),
    all_emotions = c("neutral", "neutral"),
    Drug_Rating = c(8L, 6L),
    theme_membership_Theme.T = c(1L, 1L),
    subtheme_assignments = c("Sub S", "Sub S"),
    emerged_themes = c("Theme T", "Theme T")
  )
  out <- aggregate_theme_statistics(data, ts_obj)
  expect_true("subtheme_stats" %in% names(out[["Theme T"]]))
  expect_named(out[["Theme T"]]$subtheme_stats, "Sub S")
  expect_true("Drug_Rating" %in% out[["Theme T"]]$metric_cols)
  expect_equal(out[["Theme T"]]$subtheme_stats$`Sub S`$metric_stats$Drug_Rating$median, 7)
})
