# Tests for R/metric_primitives.R
#
# The backend metric-primitive catalog: ~45 deterministic statistics the
# Methodology Assistant can request by name, plus the registry-backed
# catalog accessors and the allowlist dispatcher. Every primitive is checked
# against a hand-computed expected value; the cross-cutting contracts
# (NA-safety, registry<->catalog consistency, fail-honest dispatch, security
# allowlist) are checked over the whole catalog.

# ---- location ----------------------------------------------------------------

test_that("location primitives compute correctly", {
  x <- c(1, 2, 3, 4, 100)
  expect_equal(prim_mean(x), mean(x))
  expect_equal(prim_median(x), 3)
  expect_equal(prim_mode_value(c(2, 2, 3, 3, 3, 4)), 3)
  expect_equal(prim_mode_value(c(5, 5, 1, 1)), 1)   # tie resolves to smallest
})

# ---- spread ------------------------------------------------------------------

test_that("spread primitives compute correctly", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(prim_sd(x), stats::sd(x))
  expect_equal(prim_mad(x), stats::mad(x))          # normal-consistent (1.4826)
  expect_equal(prim_iqr(x), stats::IQR(x, type = 7))
  expect_equal(prim_range_width(x), 4)
  y <- c(2, 4, 6, 8)
  expect_equal(prim_cv(y), stats::sd(y) / mean(y))
  expect_true(is.na(prim_cv(c(-1, 1))))             # mean ~0 -> undefined
})

# ---- position / quantiles ----------------------------------------------------

test_that("position and quantile primitives compute correctly", {
  x <- 0:100
  expect_equal(prim_min(x), 0)
  expect_equal(prim_max(x), 100)
  expect_equal(prim_p10(x), stats::quantile(x, 0.10, type = 7, names = FALSE))
  expect_equal(prim_p25(x), stats::quantile(x, 0.25, type = 7, names = FALSE))
  expect_equal(prim_p75(x), stats::quantile(x, 0.75, type = 7, names = FALSE))
  expect_equal(prim_p90(x), stats::quantile(x, 0.90, type = 7, names = FALSE))
  expect_equal(prim_p95(x), stats::quantile(x, 0.95, type = 7, names = FALSE))
  expect_equal(prim_p99(x), stats::quantile(x, 0.99, type = 7, names = FALSE))
  expect_equal(prim_quantile(x, 0.5), stats::median(x))
  expect_true(is.na(prim_quantile(x, NA)))
  expect_true(is.na(prim_quantile(x, 1.5)))         # out of [0,1]
})

# ---- distribution shape ------------------------------------------------------

test_that("shape primitives match hand-computed values", {
  # skewness g1: x=c(0,0,0,0,10) -> m2=16, m3=96 -> 96/16^1.5 = 1.5
  expect_equal(prim_skewness(c(0, 0, 0, 0, 10)), 1.5)
  expect_equal(prim_skewness(c(1, 2, 3, 4, 5)), 0)            # symmetric
  # excess kurtosis g2: symmetric {-1,-1,1,1} -> m2=1, m4=1 -> 1 - 3 = -2
  # (n>=4; kurtosis is degenerate below that, which the primitive guards.)
  expect_equal(prim_kurtosis_excess(c(-1, -1, 1, 1)), -2)
  expect_equal(prim_n(c(1, 2, NA, 4)), 3)
  expect_equal(prim_n_unique(c(1, 1, 2, 3, 3)), 3)
})

test_that("insufficient-n primitives return NA at their thresholds", {
  expect_true(is.na(prim_sd(5)))                              # n < 2
  expect_true(is.na(prim_skewness(c(1, 2))))                  # n < 3
  expect_true(is.na(prim_kurtosis_excess(c(1, 2, 3))))        # n < 4
  expect_equal(prim_median(5), 5)                             # n = 1 fine
})

# ---- heavy-tail --------------------------------------------------------------

test_that("heavy-tail primitives compute correctly", {
  # log1p(0)=0, log1p(e-1)=1 -> mean 0.5
  expect_equal(prim_log_mean(c(0, exp(1) - 1)), 0.5)
  expect_true(is.na(prim_log_mean(c(-1, 2))))                 # negative -> NA
  expect_true(is.na(prim_log_sd(c(-1, 2))))
  expect_equal(prim_outlier_count_iqr(c(1, 2, 3, 4, 5, 100)), 1)
  expect_equal(prim_max_to_median_ratio(c(1, 2, 3, 4, 10)), 10 / 3)
})

# ---- proportional ------------------------------------------------------------

test_that("proportional primitives compute correctly", {
  x <- c(0, 0, 5, 10, 15)
  expect_equal(prim_proportion_zero(x), 0.4)
  expect_equal(prim_proportion_nonzero(x), 0.6)
  expect_equal(prim_proportion_above(x, 5), 0.4)             # 10, 15
  expect_equal(prim_proportion_below(x, 5), 0.4)             # 0, 0
  expect_equal(prim_proportion_in_range(x, 0, 10), 0.8)      # 0,0,5,10
  expect_true(is.na(prim_proportion_above(x, NA)))
  expect_true(is.na(prim_proportion_in_range(x, NA, 10)))
})

# ---- categorical -------------------------------------------------------------

test_that("categorical primitives compute correctly", {
  expect_equal(prim_entropy(c(1, 2, 3, 4)), 2)               # uniform 4-way bits
  expect_equal(prim_entropy(rep(7, 5)), 0)                   # degenerate
  fd <- prim_frequency_distribution(c(1, 1, 2, 3, 3, 3))
  expect_equal(unname(fd["3"]), 3)
  expect_equal(sum(fd), 6)
  tk <- prim_top_k_values(c(1, 1, 1, 2, 2, 3), 2)
  expect_equal(names(tk)[1], "1")
  expect_equal(unname(tk[1]), 3)
  expect_length(tk, 2)
  expect_length(prim_top3_values(c(1, 1, 2, 2, 3, 3, 4)), 3)
})

# ---- temporal ----------------------------------------------------------------

test_that("temporal primitives accept POSIXct and epoch seconds, UTC", {
  base <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC")       # a Monday
  ts <- base + c(0, 3600, 7200)                               # 00:00, 01:00, 02:00

  hd <- prim_hour_of_day_distribution(ts)
  expect_length(hd, 24)
  expect_equal(unname(hd["00"]), 1)
  expect_equal(unname(hd["01"]), 1)
  expect_equal(unname(hd["02"]), 1)
  # epoch-seconds input yields the identical answer
  expect_equal(prim_hour_of_day_distribution(as.numeric(ts)), hd)

  dw <- prim_day_of_week_distribution(ts)
  expect_length(dw, 7)
  expect_equal(unname(dw["Mon"]), 3)

  mo <- prim_month_of_year_distribution(ts)
  expect_length(mo, 12)
  expect_equal(unname(mo["Jan"]), 3)

  expect_equal(prim_time_span_days(c(base, base + 86400 * 5)), 5)
  expect_equal(prim_median_seconds_between(ts), 3600)
  expect_true(is.na(prim_time_span_days(base)))               # n < 2
})

test_that("entries_by_month and entries_over_time bin correctly", {
  ts <- as.POSIXct(c("2024-01-05", "2024-01-20", "2024-02-10"), tz = "UTC")
  em <- prim_entries_by_month(ts)
  expect_equal(unname(em["2024-01"]), 2)
  expect_equal(unname(em["2024-02"]), 1)

  ot <- prim_entries_over_time(ts, bin_width_days = 30)
  expect_equal(sum(ot), 3)
  expect_length(prim_entries_over_time(ts, NA), 0)            # missing arg -> empty
})

# ---- circular ----------------------------------------------------------------

test_that("circular primitives average angles and clock-times correctly", {
  expect_equal(prim_circular_mean(c(0, pi / 2)), pi / 4)
  # 0.1 and (2pi - 0.1) should average to ~0, not ~pi
  cm <- prim_circular_mean(c(0.1, 2 * pi - 0.1))
  expect_true(cm < 0.2 || cm > 2 * pi - 0.2)
  expect_equal(prim_circular_variance(c(0, 0, 0)), 0)
  expect_equal(prim_circular_variance(c(0, pi)), 1)
  # peak clock hour of 23:00 and 01:00 is ~midnight, NOT noon (the linear-mean trap)
  ts <- as.POSIXct(c("2024-01-01 23:00:00", "2024-01-02 01:00:00"), tz = "UTC")
  ph <- prim_peak_hour_circular(ts)
  expect_true(ph < 0.1 || ph > 23.9)
})

# ---- distribution-shape test -------------------------------------------------

test_that("shapiro_p separates normal from non-normal (deterministic samples)", {
  normalish <- stats::qnorm(stats::ppoints(50))               # maximally normal
  expect_true(prim_shapiro_p(normalish) > 0.05)
  skewed <- c(rep(0, 40), 1:10)
  expect_true(prim_shapiro_p(skewed) < 0.05)
  expect_true(is.na(prim_shapiro_p(c(1, 2))))                 # n < 3
  expect_true(is.na(prim_shapiro_p(rep(5, 50))))             # < 3 distinct values
})

# ---- registry / catalog integrity --------------------------------------------

test_that("catalog and registry are consistent and well-formed", {
  cat <- metric_catalog()
  expect_s3_class(cat, "tbl_df")
  expect_setequal(names(cat),
                  c("primitive", "family", "input_kind", "shape",
                    "needs_args", "description"))
  expect_equal(nrow(cat), length(metric_catalog_names()))
  expect_setequal(cat$primitive, metric_catalog_names())
  expect_false(any(duplicated(cat$primitive)))
  expect_true(all(nzchar(cat$description)))
  expect_true(all(cat$shape %in% c("scalar", "distribution")))
  expect_true(all(cat$input_kind %in% c("numeric", "temporal", "circular")))
  expect_gte(nrow(cat), 40L)                                  # ~45 primitives
})

test_that("format_metric_catalog renders every primitive, grouped by family", {
  txt <- format_metric_catalog()
  expect_type(txt, "character")
  expect_length(txt, 1L)
  for (p in metric_catalog_names()) {
    expect_match(txt, p, fixed = TRUE)
  }
  expect_match(txt, "== temporal ==", fixed = TRUE)
  expect_match(txt, "[requires args]", fixed = TRUE)
  expect_match(txt, "(distribution)", fixed = TRUE)
})

# ---- dispatcher: happy path ---------------------------------------------------

test_that("every catalog primitive dispatches without error and is available", {
  full_args <- list(q = 0.5, threshold = 2, lo = 1, hi = 4, k = 2,
                    bin_width_days = 30)
  for (p in metric_catalog_names()) {
    r <- compute_metric_stat(p, c(1, 2, 3, 4, 5), args = full_args)
    expect_true(r$available, info = p)
    expect_false(is.null(r$value), info = p)
    expect_equal(r$n_observed, 5L, info = p)
  }
})

test_that("dispatcher reports family, shape, value and n_observed", {
  r <- compute_metric_stat("prim_median", c(1, 2, NA, 4, 5))
  expect_true(r$available)
  expect_equal(r$primitive, "prim_median")
  expect_equal(r$family, "location")
  expect_equal(r$shape, "scalar")
  expect_equal(r$n_observed, 4L)
  expect_equal(r$value, stats::median(c(1, 2, 4, 5)))

  d <- compute_metric_stat("prim_frequency_distribution", c(1, 1, 2))
  expect_equal(d$shape, "distribution")
  expect_equal(unname(d$value["1"]), 2)
})

test_that("parameterized primitives read their args through the dispatcher", {
  expect_equal(
    compute_metric_stat("prim_quantile", 0:100, args = list(q = 0.9))$value,
    stats::quantile(0:100, 0.9, type = 7, names = FALSE)
  )
  expect_equal(
    compute_metric_stat("prim_proportion_above", c(0, 0, 5, 10),
                        args = list(threshold = 0))$value, 0.5
  )
  expect_equal(
    compute_metric_stat("prim_proportion_in_range", 1:10,
                        args = list(lo = 3, hi = 7))$value, 0.5
  )
  tk <- compute_metric_stat("prim_top_k_values", c(1, 1, 1, 2, 2, 3),
                           args = list(k = 2))
  expect_length(tk$value, 2L)
  # missing required arg -> NA / empty, still available, no error
  expect_true(is.na(compute_metric_stat("prim_quantile", 0:10,
                                        args = list())$value))
})

# ---- dispatcher: NA-safety over the whole catalog ----------------------------

# Count primitives honestly return 0 (not NA) when there are no observations:
# "how many non-missing values are there?" over no data is genuinely zero, not
# undefined. Every other scalar primitive returns NA on empty/all-NA input.
.count_prims <- c("prim_n", "prim_n_unique")

test_that("every primitive handles empty input gracefully (NA, 0, or empty)", {
  full_args <- list(q = 0.5, threshold = 2, lo = 1, hi = 4, k = 2,
                    bin_width_days = 30)
  for (p in metric_catalog_names()) {
    r <- compute_metric_stat(p, numeric(0), args = full_args)
    expect_true(r$available, info = p)
    expect_equal(r$n_observed, 0L, info = p)
    if (identical(r$shape, "distribution")) {
      expect_length(r$value, 0L)
    } else if (p %in% .count_prims) {
      expect_equal(r$value, 0, info = p)   # zero observations -> count is 0, not NA
    } else {
      expect_true(is.na(r$value), info = p)
    }
  }
})

test_that("scalar numeric primitives return NA (counts return 0) on all-NA input", {
  cat <- metric_catalog()
  scal <- cat$primitive[cat$shape == "scalar" &
                          cat$input_kind == "numeric" &
                          !cat$needs_args]
  for (p in scal) {
    r <- compute_metric_stat(p, c(NA_real_, NA_real_, NA_real_))
    if (p %in% .count_prims) {
      expect_equal(r$value, 0, info = p)
    } else {
      expect_true(is.na(r$value), info = p)
    }
    expect_equal(r$n_observed, 0L, info = p)
  }
})

# ---- non-finite handling + audit regressions (H1/H2/M1/M3/M4/T2) -------------

test_that("non-finite values (Inf/-Inf/NaN) are dropped and never error (H1/M3)", {
  # H1: shape primitives used to ERROR on Inf (Inf - Inf -> NaN hit the m2 guard).
  expect_equal(prim_mean(c(1, 2, 3, Inf)), 2)
  expect_equal(prim_skewness(c(1, 2, 3, Inf)), 0)             # Inf dropped -> symmetric
  expect_equal(prim_kurtosis_excess(c(-1, -1, 1, 1, Inf)), -2)
  expect_equal(prim_max(c(1, 2, NaN, Inf, -Inf)), 2)          # only finite survive
  expect_equal(prim_sd(c(1, 2, 3, Inf)), stats::sd(c(1, 2, 3)))
  # M3: heavy-tail family is now consistent (no Inf leak).
  expect_equal(prim_log_mean(c(1, 2, Inf)), mean(log1p(c(1, 2))))
  expect_equal(prim_cv(c(2, 4, 6, 8, Inf)), prim_cv(c(2, 4, 6, 8)))
  # all-non-finite collapses to "no observations".
  expect_true(is.na(prim_mean(c(Inf, -Inf, NaN))))
})

test_that("n_observed counts only the values the primitive used, not raw length (H2)", {
  expect_equal(compute_metric_stat("prim_mean", c(1, 2, Inf))$n_observed, 2L)
  expect_equal(compute_metric_stat("prim_mean", c("1", "2", "oops"))$n_observed, 2L)
  expect_equal(compute_metric_stat("prim_mean", c(1, 2, NA, 4))$n_observed, 3L)
  # temporal n_observed routes through the time cleaner (POSIXct carrying an NA)
  ts <- as.POSIXct(c("2024-01-01", NA, "2024-01-03"), tz = "UTC")
  expect_equal(compute_metric_stat("prim_time_span_days", ts)$n_observed, 2L)
})

test_that("peak hour stays in [0, 24) at the midnight wrap point (M1)", {
  ts <- as.POSIXct(c("2024-01-01 23:30:00", "2024-01-02 00:30:00"), tz = "UTC")
  ph <- prim_peak_hour_circular(ts)
  expect_gte(ph, 0)
  expect_lt(ph, 24)
})

test_that("entries_over_time guards non-finite / non-positive bin width (M4)", {
  ts <- as.POSIXct(c("2024-01-01", "2024-02-01"), tz = "UTC")
  expect_length(prim_entries_over_time(ts, Inf), 0L)
  expect_length(prim_entries_over_time(ts, NaN), 0L)
  expect_length(prim_entries_over_time(ts, 0), 0L)
  expect_length(prim_entries_over_time(ts, -5), 0L)
})

test_that("dispatcher record shape is uniform across both branches (T2)", {
  ok  <- compute_metric_stat("prim_median", c(1, 2, 3))
  bad <- compute_metric_stat("prim_nope", c(1, 2, 3))
  expect_setequal(names(ok), names(bad))      # same fields whether available or not
  expect_true(is.na(ok$reason))
  expect_false(is.na(bad$reason))
})

# ---- dispatcher: fail-honest + security allowlist ----------------------------

test_that("dispatcher fails honestly on an unknown primitive (R4)", {
  r <- compute_metric_stat("prim_does_not_exist", c(1, 2, 3))
  expect_false(r$available)
  expect_true(is.na(r$value))
  expect_equal(r$n_observed, 0L)
  expect_match(r$reason, "not in the metric catalog")
  expect_match(r$reason, "fail-honest")
  expect_match(r$reason, "prim_does_not_exist", fixed = TRUE)
})

test_that("dispatcher will NOT resolve arbitrary R functions (security boundary)", {
  # Real base-R functions that are NOT catalog primitives must not dispatch.
  for (nm in c("mean", "median", "quantile", "system", "Sys.getenv",
               "eval", "source", "file.remove")) {
    r <- compute_metric_stat(nm, c(1, 2, 3))
    expect_false(r$available, info = nm)
    expect_true(is.na(r$value), info = nm)
  }
})

test_that("dispatcher tolerates a non-scalar / non-character / NA name", {
  expect_false(compute_metric_stat(NA, c(1, 2, 3))$available)
  expect_false(compute_metric_stat(c("a", "b"), c(1, 2, 3))$available)
  expect_false(compute_metric_stat(42, c(1, 2, 3))$available)
  expect_false(compute_metric_stat(NULL, c(1, 2, 3))$available)
})

# ---- temporal cleaner accepts datetime STRINGS (H2 audit regression) ---------

test_that("temporal primitives accept datetime-STRING columns, not only POSIXct/epoch (H2)", {
  # The package's std_timestamp is stored as a formatted datetime STRING; the
  # cleaner previously did as.numeric() -> NA -> a silently-empty distribution.
  # Literal UTC strings (exactly the loader's format) -- not as.character() of a
  # POSIXct, which loses the tzone attribute under arithmetic and renders local.
  ts_str <- c("2024-01-01 00:00:00", "2024-01-01 01:00:00", "2024-01-01 02:00:00")
  r <- compute_metric_stat("prim_hour_of_day_distribution", ts_str)
  expect_true(r$available)
  expect_equal(r$n_observed, 3L)
  expect_equal(sum(r$value), 3)
  expect_equal(unname(r$value["00"]), 1)
  expect_equal(unname(r$value["01"]), 1)
  # scalar temporal primitive on a datetime-string column
  span_str <- c("2024-01-01 00:00:00", "2024-01-06 00:00:00")   # 5 days apart
  expect_equal(prim_time_span_days(span_str), 5)
  # epoch-seconds-as-strings still routes through the epoch path
  expect_equal(prim_time_span_days(as.character(c(0, 86400 * 3))), 3)
})
