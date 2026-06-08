# AI-numeric per-column small-n reliability floor + per-cell flag.
#
# The AI returns min_reliable_n (ITS judgement per column, never a package
# hardcode); the report MARKS -- never hides -- spread/shape cells computed on
# fewer entries than that floor. Explain-don't-gate: the value and its n are
# always shown, and the threshold is the analyst's number. Robust centers and
# plain counts are never marked.

test_that("62.5d: .metric_intelligence_schema carries min_reliable_n (integer, required, strict)", {
  sch <- .metric_intelligence_schema()
  expect_silent(.validate_schema(sch))                     # OpenAI strict-mode well-formed
  cr <- sch$properties$metrics$items
  expect_true("min_reliable_n" %in% names(cr$properties))
  expect_identical(cr$properties$min_reliable_n$type, "integer")
  expect_true("min_reliable_n" %in% unlist(cr$required))   # strict: every property required
  # shared column_record => the temporal records carry it too (harmless, unused)
  expect_true("min_reliable_n" %in%
                names(sch$properties$temporal_columns$items$properties))
})

test_that("62.5d: small-n-sensitive predicate flags dispersion/shape estimators only", {
  expect_true(.metric_primitive_small_n_sensitive("prim_iqr"))
  expect_true(.metric_primitive_small_n_sensitive("prim_mad"))
  expect_true(.metric_primitive_small_n_sensitive("prim_skewness"))
  expect_true(.metric_primitive_small_n_sensitive("prim_kurtosis_excess"))
  expect_true(.metric_primitive_small_n_sensitive("prim_shapiro_p"))
  expect_false(.metric_primitive_small_n_sensitive("prim_median"))   # robust center
  expect_false(.metric_primitive_small_n_sensitive("prim_mean"))
  expect_false(.metric_primitive_small_n_sensitive("prim_n"))        # a count, not an estimate
  expect_false(.metric_primitive_small_n_sensitive("prim_n_unique"))
  expect_false(.metric_primitive_small_n_sensitive("prim_hour_of_day_distribution"))
  expect_false(.metric_primitive_small_n_sensitive("prim_unknown_xyz"))
  expect_false(.metric_primitive_small_n_sensitive(NA_character_))
  # No orphans: every backend-flagged name is a real registry primitive...
  expect_true(all(.SMALL_N_SENSITIVE_PRIMITIVES %in% metric_catalog_names()))
  # ...and each belongs to a genuine spread/shape/heavy-tail/shape-test family.
  cat_tbl <- metric_catalog()
  fams <- cat_tbl$family[match(.SMALL_N_SENSITIVE_PRIMITIVES, cat_tbl$primitive)]
  expect_true(all(fams %in% c("spread", "shape", "heavy_tail", "shape_test")))
})

test_that("62.5d: coerce + serialize round-trip the floor; absent -> NA -> not serialized", {
  rec <- .coerce_column_record(list(column_name = "score", column_description = "d",
           requested_primitives = list(list(primitive = "prim_iqr", rationale = "r")),
           interpretation_note = "n", metric_provenance = "p", min_reliable_n = 12))
  expect_identical(rec$min_reliable_n, 12L)
  expect_identical(.column_record_to_list(rec)$min_reliable_n, 12L)
  # absent floor (a pre-62.5d archive) -> NA -> NOT serialized => byte-identical back-compat
  rec0 <- .coerce_column_record(list(column_name = "x", column_description = "",
            requested_primitives = list(), interpretation_note = "", metric_provenance = ""))
  expect_true(is.na(rec0$min_reliable_n))
  expect_false("min_reliable_n" %in% names(.column_record_to_list(rec0)))
  # invalid (negative / non-numeric) coerces to NA; a valid integer is preserved
  expect_true(is.na(.coerce_reliable_floor(-3)))
  expect_true(is.na(.coerce_reliable_floor("foo")))
  expect_true(is.na(.coerce_reliable_floor(NULL)))
  expect_identical(.coerce_reliable_floor(8), 8L)
})

test_that("62.5d: renderer MARKS small-n spread cells (not robust/above-floor), with a footnote", {
  s_med <- compute_metric_stat("prim_median", c(1, 2, 3, 8))   # n_observed = 4
  s_iqr <- compute_metric_stat("prim_iqr",    c(1, 2, 3, 8))   # n_observed = 4
  b_med <- compute_metric_stat("prim_median", 1:20)            # n_observed = 20
  b_iqr <- compute_metric_stat("prim_iqr",    1:20)            # n_observed = 20
  mk <- function(med, iqr, floor) list(column_description = "upvote score",
          interpretation_note = "note", min_reliable_n = floor, requested = list(med, iqr))
  mkrow <- function(nm, n, med, iqr, floor) list(name = nm, description = "d", n = n,
    metric_stats = list(score = list(median = NA, mad = NA, mean = NA, sd = NA, n_observed = n)),
    ai_metric_stats = list(score = mk(med, iqr, floor)), example_quotes = character(0))
  render <- function(floor) .build_subtheme_summary_table(list(metric_cols = "score",
    subtheme_stats = list(Small = mkrow("Small", 4, s_med, s_iqr, floor),
                          Big   = mkrow("Big",  20, b_med, b_iqr, floor))))
  n_flags <- function(h) lengths(regmatches(h, gregexpr('class="smalln-flag"', h, fixed = TRUE)))

  h10 <- render(10L)
  # Exactly ONE flag: the small subtheme's IQR (spread) cell. The small subtheme's
  # MEDIAN cell (also n=4 < 10) is a robust center => NOT flagged. The big subtheme
  # (n=20 >= 10) => NOT flagged. count == 1 proves both negatives at once.
  expect_equal(n_flags(h10), 1L)
  expect_true(grepl("smalln-footnote", h10, fixed = TRUE))
  expect_true(grepl("&dagger;", h10, fixed = TRUE))
  # the value itself is still shown (explain-don't-gate, never suppressed)
  expect_true(grepl(">3<", h10) || grepl("3.5", h10, fixed = TRUE) || grepl("3", h10))

  # NA floor (a pre-62.5d run, or the AI gave no number) => nothing marked, no footnote.
  hNA <- render(NA_integer_)
  expect_equal(n_flags(hNA), 0L)
  expect_false(grepl("smalln-footnote", hNA, fixed = TRUE))

  # The AI's threshold governs: n=4 is not below a floor of 3 => not marked.
  expect_equal(n_flags(render(3L)), 0L)
})
