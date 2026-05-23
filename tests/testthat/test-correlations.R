# Tests for correlation analysis (18_correlations.R)

test_that("prepare_correlation_data creates columns from theme membership", {
  data <- tibble::tibble(
    std_id = paste0("e", 1:6),
    emerged_themes = c("Theme A", "Theme B", "Theme A", "Theme B", "Theme A; Theme B", "Theme B"),
    sentiment_score = runif(6, -1, 1),
    theme_membership_Theme.A = c(1L, 0L, 1L, 0L, 1L, 0L),
    theme_membership_Theme.B = c(0L, 1L, 0L, 1L, 1L, 1L)
  )
  theme_set <- create_theme_set(list(
    list(name = "Theme A", description = "desc A", codes_included = "code1"),
    list(name = "Theme B", description = "desc B", codes_included = "code2")
  ))

  config <- list(use_multi_label = TRUE, min_theme_entries = 1,
                 min_observations = 3, numeric_columns = NULL)
  result <- prepare_correlation_data(data, theme_set, config)
  expect_s3_class(result, "tbl_df")
  expect_true(ncol(result) >= 1)
  # Multi-label columns should be present
  expect_true(any(grepl("theme_membership_", names(result))))
})

test_that("prepare_correlation_data validates theme columns exist", {
  data <- tibble::tibble(std_id = "e1", std_text = "text")
  theme_set <- create_theme_set(list(
    list(name = "A", description = "d", codes_included = "c")
  ))
  expect_error(prepare_correlation_data(data, theme_set), "theme columns")
})

test_that("calculate_correlations returns matrix and p-values", {
  set.seed(123)
  corr_data <- tibble::tibble(
    var1 = rnorm(50),
    var2 = rnorm(50),
    var3 = rnorm(50)
  )
  result <- calculate_correlations(corr_data, method = "spearman")
  expect_type(result, "list")
  expect_true(!is.null(result$correlation_matrix))
  expect_true(!is.null(result$p_values))
  expect_true(!is.null(result$p_adjusted))
  expect_equal(nrow(result$correlation_matrix), 3)
  expect_equal(ncol(result$correlation_matrix), 3)
  expect_equal(result$method, "spearman")
})

test_that("extract_significant applies Bonferroni correction", {
  set.seed(42)
  n <- 100
  corr_data <- tibble::tibble(
    a = rnorm(n),
    b = rnorm(n),
    c = rnorm(n)
  )
  corr_data$b <- corr_data$a + rnorm(n, 0, 0.3)  # strongly correlated

  corr_result <- calculate_correlations(corr_data, method = "spearman")
  sig <- extract_significant(corr_result, p_threshold = 0.05)
  expect_s3_class(sig, "tbl_df")
  expect_true("var1" %in% names(sig))
  expect_true("var2" %in% names(sig))
  expect_true("correlation" %in% names(sig))
  expect_true("significant" %in% names(sig))
  # The a-b pair should be significant given the strong correlation
  expect_true(any(sig$significant))
})

test_that("extract_significant returns all pairs", {
  set.seed(99)
  corr_data <- tibble::tibble(
    x = rnorm(50),
    y = rnorm(50),
    z = rnorm(50)
  )
  corr_result <- calculate_correlations(corr_data, method = "spearman")
  sig <- extract_significant(corr_result, p_threshold = 0.05)
  # 3 variables => 3 unique pairs

expect_equal(nrow(sig), 3)
})

test_that("extract_significant computes CIs when corr_data provided", {
  set.seed(42)
  cd <- tibble::tibble(a = rnorm(50), b = rnorm(50, sd = 0.3))
  cd$b <- cd$a + cd$b  # correlated
  # Use pearson method since cor.test only returns CIs for pearson
  results <- calculate_correlations(cd, method = "pearson")
  df <- extract_significant(results, corr_data = cd)
  expect_true("ci_lower" %in% names(df))
  expect_true(any(!is.na(df$ci_lower)))
})

# --- Dynamic method selection tests ---

test_that("detect_variable_types identifies binary/ordinal/continuous", {
  # Phase 58 Tier 6 H-13: ordinal threshold raised from 7 to 21 so
  # VADER-shaped sentiment (21 levels) classifies as ordinal. Need
  # > 21 distinct values for the "continuous" path.
  set.seed(1L)
  n <- 40L
  cd <- tibble::tibble(
    binary_col = rep(c(0, 1), n / 2L),
    ordinal_col = rep(1:5, n / 5L),
    sentiment_like_col = round(runif(n, -1, 1), 1),  # 21-level grid
    continuous_col = rnorm(n)                          # 40 distinct
  )
  types <- detect_variable_types(cd)
  expect_equal(types[["binary_col"]], "binary")
  expect_equal(types[["ordinal_col"]], "ordinal")
  expect_equal(types[["sentiment_like_col"]], "ordinal")  # H-13 path
  expect_equal(types[["continuous_col"]], "continuous")
})

test_that("detect_variable_types honors override ordinal_max", {
  cd <- tibble::tibble(
    five_levels = c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1)
  )
  # default (21L): 5 distinct -> ordinal
  expect_equal(detect_variable_types(cd)[["five_levels"]], "ordinal")
  # tighter cap (3L): 5 distinct -> continuous
  expect_equal(detect_variable_types(cd, ordinal_max = 3L)[["five_levels"]],
                "continuous")
})

test_that("calculate_correlations with dynamic_method selects per-pair", {
  set.seed(42)
  n <- 50
  cd <- tibble::tibble(
    binary_var = sample(c(0, 1), n, replace = TRUE),
    continuous_var = rnorm(n)
  )
  cd$continuous_var <- cd$continuous_var + cd$binary_var * 0.5

  var_types <- detect_variable_types(cd)
  results <- calculate_correlations(cd, dynamic_method = TRUE, var_types = var_types)

  expect_equal(results$method, "dynamic")
  expect_true(!is.null(results$methods_used))
  # Binary-continuous pair should use Pearson (point-biserial)
  expect_equal(results$methods_used[1, 2], "pearson")
})

test_that("calculate_correlations backward compat with dynamic_method=FALSE", {
  set.seed(42)
  cd <- tibble::tibble(a = rnorm(30), b = rnorm(30))
  results <- calculate_correlations(cd, method = "spearman", dynamic_method = FALSE)
  expect_equal(results$method, "spearman")
  expect_null(results$methods_used)
})

test_that("extract_significant includes method column with dynamic results", {
  set.seed(42)
  n <- 50
  cd <- tibble::tibble(
    binary_var = sample(c(0, 1), n, replace = TRUE),
    continuous_var = rnorm(n)
  )
  cd$continuous_var <- cd$continuous_var + cd$binary_var * 2

  var_types <- detect_variable_types(cd)
  results <- calculate_correlations(cd, dynamic_method = TRUE, var_types = var_types)
  df <- extract_significant(results, corr_data = cd)
  expect_true("method" %in% names(df))
})

# --- compare_theme_groups tests ---

test_that("compare_theme_groups returns tibble with expected columns", {
  set.seed(42)
  n <- 60
  data <- tibble::tibble(
    std_id = paste0("e", 1:n),
    sentiment_score = rnorm(n),
    theme_membership_Theme.A = as.integer(sample(c(0, 1), n, replace = TRUE)),
    theme_membership_Theme.B = as.integer(sample(c(0, 1), n, replace = TRUE))
  )
  # Build emerged_themes from membership columns
  data$emerged_themes <- ifelse(
    data$theme_membership_Theme.A == 1L & data$theme_membership_Theme.B == 1L,
    "Theme A; Theme B",
    ifelse(data$theme_membership_Theme.A == 1L, "Theme A",
           ifelse(data$theme_membership_Theme.B == 1L, "Theme B", NA_character_))
  )
  theme_set <- create_theme_set(list(
    list(name = "Theme A", description = "desc A", codes_included = "c1"),
    list(name = "Theme B", description = "desc B", codes_included = "c2")
  ))
  result <- compare_theme_groups(data, theme_set, config = list(min_theme_entries = 5))
  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > 0)
  expected_cols <- c("theme", "variable", "mean_members", "mean_non_members",
                     "w_statistic", "p_value", "effect_r", "direction", "p_adjusted", "significant")
  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("Missing column:", col))
  }
})

test_that("compare_theme_groups handles theme with too few members", {
  set.seed(42)
  n <- 30
  data <- tibble::tibble(
    std_id = paste0("e", 1:n),
    emerged_themes = c(rep("Theme A", 28), rep(NA_character_, 2)),
    sentiment_score = rnorm(n),
    theme_membership_Theme.A = c(rep(1L, 28), rep(0L, 2))  # only 2 non-members
  )
  theme_set <- create_theme_set(list(
    list(name = "Theme A", description = "desc A", codes_included = "c1")
  ))
  result <- compare_theme_groups(data, theme_set, config = list(min_theme_entries = 5))
  # Should return empty tibble because non-member group < 5
  expect_equal(nrow(result), 0)
})

# --- test_theme_cooccurrence tests ---

test_that("test_theme_cooccurrence returns tibble with expected columns", {
  set.seed(42)
  n <- 80
  data <- tibble::tibble(
    std_id = paste0("e", 1:n),
    theme_membership_Theme.A = as.integer(sample(c(0, 1), n, replace = TRUE)),
    theme_membership_Theme.B = as.integer(sample(c(0, 1), n, replace = TRUE)),
    theme_membership_Theme.C = as.integer(sample(c(0, 1), n, replace = TRUE))
  )
  theme_set <- create_theme_set(list(
    list(name = "Theme A", description = "d", codes_included = "c"),
    list(name = "Theme B", description = "d", codes_included = "c"),
    list(name = "Theme C", description = "d", codes_included = "c")
  ))
  result <- test_theme_cooccurrence(data, theme_set)
  expect_s3_class(result, "tbl_df")
  # 3 themes => 3 pairs
  expect_equal(nrow(result), 3)
  expected_cols <- c("theme1", "theme2", "observed_both", "expected_both",
                     "statistic", "p_value", "cramers_v")
  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("Missing column:", col))
  }
})

test_that("test_theme_cooccurrence uses Fisher when expected < 5", {
  # Create data where one cell will have very low expected count
  n <- 20
  data <- tibble::tibble(
    std_id = paste0("e", 1:n),
    theme_membership_Rare.A = c(rep(1L, 2), rep(0L, n - 2)),
    theme_membership_Rare.B = c(rep(1L, 3), rep(0L, n - 3))
  )
  theme_set <- create_theme_set(list(
    list(name = "Rare A", description = "d", codes_included = "c"),
    list(name = "Rare B", description = "d", codes_included = "c")
  ))
  result <- test_theme_cooccurrence(data, theme_set, min_expected = 5)
  expect_s3_class(result, "tbl_df")
  if (nrow(result) > 0) {
    # Fisher's test doesn't produce a chi-square statistic
    expect_true("method" %in% names(result))
    expect_true(any(result$method == "Fisher"))
  }
})
