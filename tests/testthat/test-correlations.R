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

test_that("extract_significant flags within-sentiment-instrument pairs excluded_from_findings (kept, not dropped)", {
  set.seed(7)
  n <- 80
  cd <- tibble::tibble(
    sentiment_score   = rnorm(n),
    emotion_intensity = rnorm(n),
    score             = rnorm(n)            # an EXTERNAL metadata variable
  )
  cd$emotion_intensity <- -cd$sentiment_score + rnorm(n, 0, 0.3)  # the artifact pair (strongest)
  res <- calculate_correlations(cd, method = "spearman")
  sig <- extract_significant(res, p_threshold = 0.05)
  get_pair <- function(df, a, b) df[(df$var1 == a & df$var2 == b) |
                                    (df$var1 == b & df$var2 == a), , drop = FALSE]
  # The intra-instrument pair is KEPT in the matrix (auditable) but flagged and
  # zeroed out of the findings flags -- even though it is the strongest correlation.
  wf <- get_pair(sig, "sentiment_score", "emotion_intensity")
  expect_equal(nrow(wf), 1L)
  expect_true(wf$excluded_from_findings)
  expect_false(wf$significant)
  expect_false(wf$meaningful_effect)
  expect_identical(wf$exclusion_reason, "within_affect_instrument")
  # pairs with an EXTERNAL variable are retained AND not excluded (real-findings path)
  ext <- get_pair(sig, "sentiment_score", "score")
  if (nrow(ext) == 0L) ext <- get_pair(sig, "emotion_intensity", "score")
  expect_equal(nrow(ext), 1L)
  expect_false(ext$excluded_from_findings)
})

test_that("extract_significant flags affect x theme-membership pairs excluded_from_findings (kept, not dropped)", {
  # Both sentiment_score and theme_membership_* are the AI analyst's OWN codings of
  # the same text; their correlation is internal coding consistency, fully circular
  # when the theme is affect-defined. It must be KEPT in the matrix for audit but
  # flagged + removed from findings, while a SUBSTANTIVE engagement-metadata x theme
  # pair (independent measures) stays a real finding.
  set.seed(11)
  n <- 90
  cd <- tibble::tibble(sentiment_score = rnorm(n))
  cd$theme_membership_Emotional_Consequences <- as.integer(cd$sentiment_score < 0)
  cd$score <- cd$theme_membership_Emotional_Consequences * 3 + rnorm(n, 0, 0.6)
  # binary membership -> Spearman ties -> expected cor.test ties warning (fixture artifact)
  res <- suppressWarnings(calculate_correlations(cd, method = "spearman"))
  sig <- extract_significant(res, p_threshold = 0.05)
  get_pair <- function(df, a, b) df[(df$var1 == a & df$var2 == b) |
                                    (df$var1 == b & df$var2 == a), , drop = FALSE]
  circ <- get_pair(sig, "sentiment_score", "theme_membership_Emotional_Consequences")
  expect_equal(nrow(circ), 1L)
  expect_true(circ$excluded_from_findings)
  expect_false(circ$significant)
  expect_identical(circ$exclusion_reason, "affect_x_theme_membership")
  subst <- get_pair(sig, "score", "theme_membership_Emotional_Consequences")
  expect_equal(nrow(subst), 1L)
  expect_false(subst$excluded_from_findings)
})

test_that(".fisher_z_ci uses a wider, method-appropriate SE for Spearman (M1)", {
  cip <- .fisher_z_ci(0.8, 30, method = "pearson")
  cis <- .fisher_z_ci(0.8, 30, method = "spearman")
  expect_gt(diff(cis), diff(cip))  # Spearman CI strictly wider (not anti-conservative)
  # exact Bonett-Wright value: se = sqrt((1 + r^2/2)/(n-3))
  r <- 0.8; n <- 30; z <- atanh(r); se <- sqrt((1 + r^2 / 2) / (n - 3))
  expect_equal(cis, c(round(tanh(z - qnorm(0.975) * se), 3),
                      round(tanh(z + qnorm(0.975) * se), 3)))
  expect_true(all(is.na(.fisher_z_ci(0.8, 3, method = "spearman"))))  # n < 4 guard
  expect_equal(.fisher_z_ci(0.5, 50), .fisher_z_ci(0.5, 50, method = "pearson"))  # default = pearson
})

test_that("extract_significant re-scopes the BH/Bonferroni family to non-excluded pairs (M2)", {
  # Circular / within-instrument artifact pairs carry tiny p-values by
  # construction; they must be EXCLUDED from the multiple-comparison family so a
  # genuine pair's adjusted p reflects only the substantive tests.
  set.seed(23)
  n <- 120
  cd <- tibble::tibble(sentiment_score = rnorm(n), emotion_intensity = rnorm(n))
  cd$emotion_intensity <- -cd$sentiment_score + rnorm(n, 0, 0.2)      # within-instrument artifact
  cd$theme_membership_A <- as.integer(cd$sentiment_score < 0)          # affect-defined theme
  cd$score <- rnorm(n)                                                 # external metadata
  cd$num_comments <- cd$score * 0.25 + rnorm(n, 0, 1)                  # a real, modest pair
  res <- suppressWarnings(calculate_correlations(cd, method = "spearman"))
  sig <- extract_significant(res, p_threshold = 0.05)
  excl <- sig[sig$excluded_from_findings, , drop = FALSE]
  kept <- sig[!sig$excluded_from_findings & !is.na(sig$p_raw), , drop = FALSE]
  expect_gt(nrow(excl), 0L)
  expect_gt(nrow(kept), 1L)
  # excluded pairs are out of the family: NA adjusted p + never significant
  expect_true(all(is.na(excl$p_bh)))
  expect_true(all(is.na(excl$p_bonferroni)))
  expect_true(all(!excl$significant))
  # kept pairs' adjusted p == p.adjust over ONLY the kept (substantive) raw p's
  expect_equal(kept$p_bh, p.adjust(kept$p_raw, method = "BH"))
  expect_equal(kept$p_bonferroni, p.adjust(kept$p_raw, method = "bonferroni"))
})

test_that(".rescope_plot_pvalues overlays the re-scoped adjusted p so the plot matches the table (M2 figure consistency)", {
  vars <- c("a", "b", "c")
  pa <- matrix(0.001, 3, 3, dimnames = list(vars, vars)); diag(pa) <- 1  # full-family: all tiny
  df <- data.frame(var1 = c("a", "a", "b"), var2 = c("b", "c", "c"),
                   p_value = c(0.20, NA_real_, 0.04),  # a~b re-scoped n.s.; a~c excluded (NA); b~c sig
                   stringsAsFactors = FALSE)
  out <- .rescope_plot_pvalues(pa, vars, df)
  expect_equal(out["a", "b"], 0.20); expect_equal(out["b", "a"], 0.20)  # kept pair -> re-scoped p overlaid
  expect_equal(out["b", "c"], 0.04)
  expect_equal(out["a", "c"], 0.001)  # NA p_value (excluded) -> untouched here (blanked separately)
  # no-op on NULL / schema-incomplete df (byte-identical, e.g. nothing excluded)
  expect_identical(.rescope_plot_pvalues(pa, vars, NULL), pa)
  expect_identical(.rescope_plot_pvalues(pa, vars, data.frame(x = 1)), pa)
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

# ---------------------------------------------------------------------------
# Phase 63 follow-up: correlations robustness + #2b plot consistency
# (surfaced by an independent adversarial audit of the four shipped fixes)
# ---------------------------------------------------------------------------

test_that("extract_significant returns a full-schema empty result on an all-NA matrix (no crash)", {
  # Constant-variance columns -> every pairwise correlation is NA -> `pairs`
  # is empty. Pre-fix this crashed at arrange(correlation) (the column was
  # never created). It must now return a 0-row tibble with the full schema.
  cd <- tibble::tibble(a = rep(1, 40), b = rep(2, 40), c = rep(3, 40))
  res <- suppressWarnings(calculate_correlations(cd, method = "spearman"))
  expect_true(all(is.na(res$correlation_matrix[upper.tri(res$correlation_matrix)])))
  sig <- NULL
  expect_no_error(sig <- extract_significant(res, p_threshold = 0.05))
  expect_s3_class(sig, "tbl_df")
  expect_equal(nrow(sig), 0L)
  for (col in c("var1", "var2", "correlation", "significant", "meaningful_effect",
                "effect_size", "exclusion_reason", "excluded_from_findings")) {
    expect_true(col %in% names(sig), info = col)
  }
})

test_that("extract_significant empty (<2 var) and all-NA paths share one schema", {
  one <- extract_significant(list(correlation_matrix = matrix(1, 1, 1)), p_threshold = 0.05)
  cd  <- tibble::tibble(a = rep(1, 30), b = rep(2, 30))
  na2 <- suppressWarnings(extract_significant(calculate_correlations(cd, method = "spearman")))
  expect_setequal(names(one), names(na2))
  expect_equal(nrow(one), 0L)
  expect_equal(nrow(na2), 0L)
})

test_that(".build_excluded_pair_matrix marks both triangles; no-ops on NULL/empty/malformed", {
  vn <- c("sentiment_score", "num_comments", "theme_membership_recovery")
  ep <- data.frame(var1 = "sentiment_score", var2 = "theme_membership_recovery",
                   excluded_from_findings = TRUE, stringsAsFactors = FALSE)
  m <- .build_excluded_pair_matrix(vn, ep)
  expect_true(m[1, 3] && m[3, 1])
  expect_equal(sum(m), 2L)                              # one unordered pair, both directions
  expect_false(any(.build_excluded_pair_matrix(vn, NULL)))
  expect_false(any(.build_excluded_pair_matrix(vn, ep[0, ])))
  expect_false(any(.build_excluded_pair_matrix(vn, data.frame(x = 1))))   # missing columns
  ep2 <- data.frame(var1 = "ghost", var2 = "num_comments",
                    excluded_from_findings = TRUE, stringsAsFactors = FALSE)
  expect_false(any(.build_excluded_pair_matrix(vn, ep2)))                  # absent var ignored
})

test_that(".correlation_lollipop_data marks excluded pairs distinctly and never as a finding", {
  vn <- c("sentiment_score", "num_comments", "theme_membership_recovery")
  cm <- matrix(c(1, 0.80, 0.62, 0.80, 1, 0.02, 0.62, 0.02, 1), 3, dimnames = list(vn, vn))
  # the circular pair (1,3) is STRONGLY significant; without the flag it would
  # be coloured "p < 0.05"
  pa <- matrix(c(0, 0.001, 0.001, 0.001, 0, 0.95, 0.001, 0.95, 0), 3, dimnames = list(vn, vn))
  m  <- .build_excluded_pair_matrix(vn, data.frame(
          var1 = "sentiment_score", var2 = "theme_membership_recovery",
          excluded_from_findings = TRUE, stringsAsFactors = FALSE))
  d <- .correlation_lollipop_data(cm, pa, top_n = 10, excluded_mat = m)
  expect_equal(nrow(d), 3L)                             # excluded pair KEPT, not dropped
  is_excl <- grepl("sentiment_score", d$label) & grepl("theme_membership_recovery", d$label)
  expect_identical(as.character(d$significant[is_excl]), "excluded (circular)")
  expect_false(as.character(d$significant[is_excl]) == "p < 0.05")   # absence of the bad pattern
  is_real <- grepl("sentiment_score", d$label) & grepl("num_comments", d$label)
  expect_identical(as.character(d$significant[is_real]), "p < 0.05") # real finding preserved
  expect_equal(d$r, d$r[order(-abs(d$r))])              # ranked by |r| descending
  d0 <- .correlation_lollipop_data(cm, pa, top_n = 10, excluded_mat = NULL)
  expect_false(any(as.character(d0$significant) == "excluded (circular)"))  # NULL -> back-compat
})

test_that(".mask_excluded_pvalues blanks excluded cells and is NULL-safe", {
  vn <- c("a", "b", "c")
  pa <- matrix(c(0, 0.001, 0.2, 0.001, 0, 0.3, 0.2, 0.3, 0), 3, dimnames = list(vn, vn))
  m  <- matrix(FALSE, 3, 3, dimnames = list(vn, vn)); m[1, 3] <- m[3, 1] <- TRUE
  pm <- .mask_excluded_pvalues(pa, m)
  expect_equal(pm[1, 3], 1)                             # excluded -> non-significant (blanked)
  expect_equal(pm[1, 2], 0.001)                         # untouched
  expect_null(.mask_excluded_pvalues(NULL, m))
  expect_identical(.mask_excluded_pvalues(pa, NULL), pa)
})
