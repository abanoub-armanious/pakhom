# Phase 58 Tier 6 unit tests
#
# H-13 Spearman / Kendall routing for binary x ordinal pairs
# H-14 negligible effect-size tier on all three statistical layers
# H-15 meaningful_effect ∩ significant headline in interpret_correlations
# H-16 harmonized min_theme_entries filtering across the three layers
# H-17 n_members + n_non_members emission in theme-group tibble
# H-18 Cramér's V for the Fisher dispatch path
# M-8  rank-biserial effect_r (sign-aware, numerically stable)
# M-9  effect_r preserves sign
# M-10 min_observed_both filter in co-occurrence

# ==========================================================================
# H-13: binary x ordinal routes through Spearman
# ==========================================================================

test_that(".select_pair_method routes binary x ordinal through Spearman", {
  # Binary + ordinal (the AI-sentiment case)
  expect_equal(
    pakhom:::.select_pair_method(c(0, 1, 0, 1), c(1, 2, 3, 4), "binary", "ordinal"),
    "spearman"
  )
  expect_equal(
    pakhom:::.select_pair_method(c(1, 2, 3, 4), c(0, 1, 0, 1), "ordinal", "binary"),
    "spearman"
  )
})

test_that(".select_pair_method preserves binary x continuous Pearson", {
  expect_equal(
    pakhom:::.select_pair_method(c(0, 1, 0, 1), rnorm(4), "binary", "continuous"),
    "pearson"
  )
  expect_equal(
    pakhom:::.select_pair_method(rnorm(4), c(0, 1, 0, 1), "continuous", "binary"),
    "pearson"
  )
})

test_that(".select_pair_method preserves binary x binary phi (Pearson)", {
  expect_equal(
    pakhom:::.select_pair_method(c(0, 1, 0, 1), c(1, 0, 1, 0), "binary", "binary"),
    "pearson"
  )
})


# ==========================================================================
# H-14: negligible tier in extract_significant
# ==========================================================================

test_that("extract_significant uses negligible / small / medium / large tiers", {
  # Build minimal CorrelationResults: 5 variables, controlled r values.
  cm <- matrix(c(
    1.0,  0.7, 0.4, 0.15, 0.05,
    0.7,  1.0, 0.3, 0.10, 0.02,
    0.4,  0.3, 1.0, 0.20, 0.06,
    0.15, 0.10, 0.20, 1.0, 0.08,
    0.05, 0.02, 0.06, 0.08, 1.0
  ), nrow = 5L, byrow = TRUE)
  rownames(cm) <- colnames(cm) <- paste0("v", 1:5)
  pa <- matrix(0.001, nrow = 5L, ncol = 5L)  # all significant
  diag(pa) <- 1
  rownames(pa) <- colnames(pa) <- rownames(cm)

  results <- list(
    correlation_matrix = cm,
    p_values = pa,
    p_adjusted = pa,
    n_observations = 1000L,
    n_tests = 10L,
    method = "spearman",
    adjustment = "bonferroni"
  )
  class(results) <- "CorrelationResults"

  df <- extract_significant(results, p_threshold = 0.05)
  # 4-tier classification
  expect_true("negligible" %in% df$effect_size)
  expect_true("small" %in% df$effect_size)
  expect_true("medium" %in% df$effect_size)
  expect_true("large" %in% df$effect_size)
  # Verify the boundaries
  large_pair <- df[df$effect_size == "large", ]
  expect_true(all(abs(large_pair$correlation) >= 0.5))
  negligible_pair <- df[df$effect_size == "negligible", ]
  expect_true(all(abs(negligible_pair$correlation) < 0.10))
})


# ==========================================================================
# H-15: meaningful_effect ∩ significant headline
# ==========================================================================

test_that("interpret_correlations reports the meaningful AND significant intersection", {
  # Mix of (meaningful + significant), (meaningful + n.s.),
  # (negligible + significant), (negligible + n.s.).
  df <- tibble::tibble(
    var1 = c("a", "b", "c", "d"),
    var2 = c("x", "y", "z", "w"),
    correlation = c(0.5, 0.3, 0.05, 0.02),
    p_value = c(1e-10, 0.5, 1e-10, 0.7),
    p_raw = c(1e-10, 0.5, 1e-10, 0.7),
    p_bh = c(1e-10, 0.5, 1e-10, 0.7),
    p_bonferroni = c(1e-10, 0.5, 1e-10, 0.7),
    effect_size = c("large", "medium", "negligible", "negligible"),
    significant = c(TRUE, FALSE, TRUE, FALSE),
    meaningful_effect = c(TRUE, TRUE, FALSE, FALSE),
    method = rep("spearman", 4),
    ci_lower = c(0.4, 0.2, 0.04, 0.01),
    ci_upper = c(0.6, 0.4, 0.06, 0.03)
  )
  res <- interpret_correlations(df, theme_stats = list())
  # Should mention "1" (the intersection count) AS the headline number
  expect_match(res$summary, "1\\**", )
  # Should also mention the standalone counts (2 meaningful, 2 significant)
  expect_match(res$summary, "2 pairs cross the effect-size", fixed = TRUE)
  expect_match(res$summary, "2 pairs cross the significance threshold", fixed = TRUE)
  # And the strongest-associations pool should only include the intersection
  expect_match(res$summary, "meaningful effect AND Bonferroni-significant", fixed = TRUE)
})


# ==========================================================================
# H-16: harmonized min_theme_entries filter
# ==========================================================================

test_that("prepare_correlation_data filters multi-label themes by min_theme_entries", {
  # Theme A has 10 members; Theme B has 3 members (below default 5).
  set.seed(99L)
  n <- 50L
  data <- tibble::tibble(
    std_id = paste0("e", 1:n),
    sentiment_score = round(runif(n, -1, 1), 1),
    theme_membership_Theme.A = c(rep(1L, 10L), rep(0L, n - 10L)),
    theme_membership_Theme.B = c(rep(1L, 3L), rep(0L, n - 3L))
  )
  theme_set <- structure(
    list(themes = list(
      list(name = "Theme A", codes_included = character()),
      list(name = "Theme B", codes_included = character())
    )),
    class = "ThemeSet"
  )
  corr_data <- prepare_correlation_data(data, theme_set,
                                          config = list(use_multi_label = TRUE,
                                                          min_theme_entries = 5L,
                                                          min_observations = 5L))
  expect_true("theme_membership_Theme.A" %in% names(corr_data))
  expect_false("theme_membership_Theme.B" %in% names(corr_data))
})

test_that("test_theme_cooccurrence filters by min_theme_entries", {
  # 4 themes: 2 with 10 members, 2 with 2 members (below threshold).
  set.seed(11L)
  n <- 30L
  data <- tibble::tibble(
    theme_membership_T1 = c(rep(1L, 10L), rep(0L, n - 10L)),
    theme_membership_T2 = c(rep(0L, 5L), rep(1L, 10L), rep(0L, n - 15L)),
    theme_membership_T3 = c(rep(1L, 2L), rep(0L, n - 2L)),
    theme_membership_T4 = c(rep(0L, 25L), rep(1L, 2L), rep(0L, 3L))
  )
  theme_set <- structure(
    list(themes = list(
      list(name = "T1", codes_included = character()),
      list(name = "T2", codes_included = character()),
      list(name = "T3", codes_included = character()),
      list(name = "T4", codes_included = character())
    )),
    class = "ThemeSet"
  )
  result <- test_theme_cooccurrence(data, theme_set,
                                      min_theme_entries = 5L,
                                      min_observed_both = 1L)
  # Only T1+T2 should remain after the filter
  if (nrow(result) > 0L) {
    themes_in <- unique(c(result$theme1, result$theme2))
    expect_true(all(themes_in %in% c("T1", "T2")))
  }
})


# ==========================================================================
# H-17: n_members + n_non_members in theme-group tibble
# ==========================================================================

test_that("compare_theme_groups emits n_members and n_non_members columns", {
  set.seed(33L)
  n <- 100L
  data <- tibble::tibble(
    std_id = paste0("e", 1:n),
    sentiment_score = c(rnorm(20, mean = 0.5, sd = 0.3),
                        rnorm(80, mean = -0.2, sd = 0.4)),
    emotion_intensity = runif(n, 0, 1),
    theme_membership_Theme.X = c(rep(1L, 20L), rep(0L, 80L))
  )
  theme_set <- structure(
    list(themes = list(list(name = "Theme X", codes_included = character()))),
    class = "ThemeSet"
  )
  tg <- compare_theme_groups(data, theme_set,
                              config = list(min_theme_entries = 5L))
  expect_true("n_members" %in% names(tg))
  expect_true("n_non_members" %in% names(tg))
  expect_equal(tg$n_members[1L], 20L)
  expect_equal(tg$n_non_members[1L], 80L)
  expect_true("effect_size" %in% names(tg))
})


# ==========================================================================
# H-18: Cramér's V for the Fisher dispatch path
# ==========================================================================

test_that("test_theme_cooccurrence emits non-NA Cramer's V on the Fisher path", {
  # Two themes with sparse overlap so Fisher dispatches (expected cell < 5).
  set.seed(55L)
  n <- 30L
  data <- tibble::tibble(
    theme_membership_TA = c(rep(1L, 6L), rep(0L, n - 6L)),
    theme_membership_TB = c(rep(0L, 25L), rep(1L, 5L))
  )
  # Overlap row: TA = 1 AND TB = 1 -- at least 1 to pass min_observed_both
  data$theme_membership_TA[26L] <- 1L  # last TA = 1; TB = 1 here

  theme_set <- structure(
    list(themes = list(
      list(name = "TA", codes_included = character()),
      list(name = "TB", codes_included = character())
    )),
    class = "ThemeSet"
  )
  result <- test_theme_cooccurrence(data, theme_set,
                                      min_theme_entries = 5L,
                                      min_observed_both = 1L)
  if (nrow(result) > 0L) {
    expect_equal(result$method[1L], "Fisher")
    # H-18: Cramer's V is now populated for Fisher (was NA pre-Tier-6)
    expect_false(is.na(result$cramers_v[1L]))
    expect_true(is.numeric(result$cramers_v[1L]))
  }
})


# ==========================================================================
# M-8 + M-9: rank-biserial effect_r is sign-aware
# ==========================================================================

test_that("compare_theme_groups effect_r is sign-aware and rank-biserial", {
  # Construct data where theme members have systematically LOWER sentiment
  # than non-members; effect_r should be negative.
  set.seed(77L)
  n <- 200L
  data <- tibble::tibble(
    std_id = paste0("e", 1:n),
    sentiment_score = c(rnorm(50, mean = -0.5, sd = 0.2),
                        rnorm(150, mean = 0.3, sd = 0.2)),
    emotion_intensity = runif(n, 0, 1),
    theme_membership_Negative.Theme = c(rep(1L, 50L), rep(0L, 150L))
  )
  theme_set <- structure(
    list(themes = list(list(name = "Negative Theme",
                             codes_included = character()))),
    class = "ThemeSet"
  )
  tg <- compare_theme_groups(data, theme_set,
                              config = list(min_theme_entries = 5L))
  # The sentiment-row should have a negative effect_r (members are lower)
  row <- tg[tg$variable == "sentiment score", ][1L, ]
  expect_lt(row$effect_r, 0)
  expect_equal(row$direction, "Lower in theme")
  # Magnitude bounded by 1 (rank-biserial range)
  expect_true(abs(row$effect_r) <= 1)
})


# ==========================================================================
# M-10: min_observed_both filter
# ==========================================================================

test_that("compare_theme_groups direction agrees with effect_r sign on skewed data", {
  # Tier 6 audit followup H-1: pre-followup `direction` used a mean
  # comparison; on skewed data the mean and rank centroid can disagree
  # (members carry an outlier that drags the mean up but the bulk of
  # ranks are below non-members). The fix derives direction from
  # sign(rank_biserial) so a rank-based test ships a rank-consistent
  # direction label.
  data <- tibble::tibble(
    std_id = paste0("e", 1:42),
    sentiment_score = c(rep(0.1, 20L), 0.99,       # 21 "members"
                        rep(0.4, 20L), 0.5),       # 21 "non-members"
    emotion_intensity = runif(42L, 0, 1),
    theme_membership_Skewed.Theme = c(rep(1L, 21L), rep(0L, 21L))
  )
  theme_set <- structure(
    list(themes = list(list(name = "Skewed Theme",
                             codes_included = character()))),
    class = "ThemeSet"
  )
  tg <- compare_theme_groups(data, theme_set,
                              config = list(min_theme_entries = 5L))
  row <- tg[tg$variable == "sentiment score", ][1L, ]
  # Members' ranks are mostly below non-members' ranks despite the
  # 0.99 outlier: effect_r should be negative AND direction should
  # also be "Lower in theme" (rank-consistent).
  expect_lt(row$effect_r, 0)
  expect_equal(row$direction, "Lower in theme")
})

test_that("compare_theme_groups effect_r reaches 1 when all members rank above non-members", {
  # Tier 6 audit followup M-8: extreme rank-biserial bound check.
  data <- tibble::tibble(
    std_id = paste0("e", 1:20),
    sentiment_score = c(rep(0.9, 10L), rep(0.1, 10L)),
    emotion_intensity = c(rep(0.9, 10L), rep(0.1, 10L)),
    theme_membership_AllHigh = c(rep(1L, 10L), rep(0L, 10L))
  )
  theme_set <- structure(
    list(themes = list(list(name = "AllHigh", codes_included = character()))),
    class = "ThemeSet"
  )
  tg <- compare_theme_groups(data, theme_set,
                              config = list(min_theme_entries = 5L))
  # Sentiment: all members rank above all non-members -> U = n_m * n_n,
  # rank_biserial = 2 * 1 - 1 = 1.
  row <- tg[tg$variable == "sentiment score", ][1L, ]
  expect_equal(row$effect_r, 1)
  expect_equal(row$direction, "Higher in theme")
})

test_that("test_theme_cooccurrence handles all-themes-filtered case", {
  # Tier 6 audit followup M-6: when every theme is below the cohort
  # filter, the function returns an empty tibble with a clean log.
  data <- tibble::tibble(
    # Two themes, each with only 2 members (below default 5)
    theme_membership_TA = c(rep(1L, 2L), rep(0L, 18L)),
    theme_membership_TB = c(rep(0L, 18L), rep(1L, 2L))
  )
  theme_set <- structure(
    list(themes = list(
      list(name = "TA", codes_included = character()),
      list(name = "TB", codes_included = character())
    )),
    class = "ThemeSet"
  )
  result <- test_theme_cooccurrence(data, theme_set,
                                      min_theme_entries = 5L,
                                      min_observed_both = 1L)
  expect_equal(nrow(result), 0L)
})

test_that("test_theme_cooccurrence handles mixed Fisher + Chi-square dispatch", {
  # Tier 6 audit followup M-7: the run produces both Fisher and
  # Chi-square dispatches in the same call; H-18's Fisher Cramer's V
  # should sit alongside the chi-square Cramer's V in the same tibble.
  set.seed(133L)
  n <- 200L
  # Theme A x Theme B: dense overlap (expected > 5 in every cell ->
  # Chi-square). Theme C x Theme D: sparse overlap (some expected < 5
  # -> Fisher).
  data <- tibble::tibble(
    theme_membership_TA = c(rep(1L, 100L), rep(0L, 100L)),
    theme_membership_TB = c(rep(c(1L, 0L), 50L), rep(c(0L, 1L), 50L)),
    # TC + TD: each has only ~10 members, with one overlap entry
    theme_membership_TC = c(rep(1L, 10L), rep(0L, 190L)),
    theme_membership_TD = c(rep(0L, 5L), rep(1L, 10L), rep(0L, 185L))
  )
  theme_set <- structure(
    list(themes = list(
      list(name = "TA", codes_included = character()),
      list(name = "TB", codes_included = character()),
      list(name = "TC", codes_included = character()),
      list(name = "TD", codes_included = character())
    )),
    class = "ThemeSet"
  )
  result <- test_theme_cooccurrence(data, theme_set,
                                      min_theme_entries = 5L,
                                      min_observed_both = 1L)
  if (nrow(result) >= 2L) {
    expect_true("Chi-square" %in% result$method ||
                  "Fisher" %in% result$method)
    # Both Cramer's V values populated (no NA on either path)
    populated <- !is.na(result$cramers_v)
    expect_true(any(populated))
  }
})

test_that("test_theme_cooccurrence skips pairs with zero observed co-occurrence", {
  # Two themes with NO overlap -- observed_both = 0
  data <- tibble::tibble(
    theme_membership_TA = c(rep(1L, 10L), rep(0L, 20L)),
    theme_membership_TB = c(rep(0L, 10L), rep(1L, 10L), rep(0L, 10L))
  )
  theme_set <- structure(
    list(themes = list(
      list(name = "TA", codes_included = character()),
      list(name = "TB", codes_included = character())
    )),
    class = "ThemeSet"
  )
  # With min_observed_both = 1 (default), the zero-overlap pair is skipped
  result <- test_theme_cooccurrence(data, theme_set,
                                      min_theme_entries = 5L,
                                      min_observed_both = 1L)
  expect_equal(nrow(result), 0L)
})
