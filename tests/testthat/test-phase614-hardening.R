# Audit-followup hardening. Three independent audits (correctness,
# principle+back-compat, cross-tier invariants) all returned SHIP and all
# flagged the SAME single [LOW]: the per-subtheme AI table matched body cells to
# headers by POSITION, correct today only because .compute_subtheme_statistics
# gives every subtheme of a theme the same per-column interpretation record (so
# the requested-primitive lists are already identical in order + length). These
# tests pin the two hardenings that close the latent risk:
#   (a) AI cells are matched to headers by primitive NAME, so a value can never
#       land under the wrong header even if a future caller's subthemes diverge
#       in primitive order;
#   (b) .densify_temporal_distribution never DROPS an observed bin (returns the
#       primitive's value verbatim if the reconstructed grid can't contain it).

# ---- (a) name-based header/value matching ------------------------------------

test_that("subtheme AI table matches cells to headers by NAME, not position", {
  # Two subthemes whose requested-primitive lists are in DIFFERENT order for the
  # same column. The header plan is derived from S1 (median, p90); S2's values
  # must still land under the correct headers (no transposition).
  ai_s1 <- pakhom:::.compute_requested_primitives(
    list(column_name = "score",
         requested_primitives = list(list(primitive = "prim_median", rationale = "r"),
                                     list(primitive = "prim_p90", rationale = "r")),
         interpretation_note = "n"),
    c(0, 0, 0, 0, 80))                                   # S1: median 0, p90 ~64
  ai_s2 <- pakhom:::.compute_requested_primitives(
    list(column_name = "score",
         requested_primitives = list(list(primitive = "prim_p90", rationale = "r"),
                                     list(primitive = "prim_median", rationale = "r")),  # REVERSED
         interpretation_note = "n"),
    c(50, 50, 50, 50, 50))                               # S2: median 50, p90 50
  ts <- list(metric_cols = "score", subtheme_stats = list(
    S1 = list(name = "S1", description = "", n = 5L, metric_stats = list(),
              ai_metric_stats = list(score = ai_s1), example_quotes = character(0)),
    S2 = list(name = "S2", description = "", n = 5L, metric_stats = list(),
              ai_metric_stats = list(score = ai_s2), example_quotes = character(0))))
  out <- pakhom:::.build_subtheme_summary_table(ts)
  rows <- regmatches(out, gregexpr("<tr>.*?</tr>", out))[[1]]
  cell_txt <- function(r) gsub("<[^>]+>", "",
    regmatches(r, gregexpr("<td[^>]*>(.*?)</td>", r))[[1]])
  s1 <- cell_txt(rows[grepl(">S1<", rows)])
  s2 <- cell_txt(rows[grepl(">S2<", rows)])
  # both rows header-aligned: [name, n, median score, p90 score, examples]
  expect_length(s1, 5L)
  expect_length(s2, 5L)
  # S2's median (50) under the "median score" column (index 3), NOT its p90 --
  # the key anti-transposition assertion. Pre-hardening, index 3 of S2's
  # positionally-pulled list was p90, so this would have been wrong.
  expect_equal(s2[3], "50")
  expect_equal(s1[3], "0")        # S1 median
})

test_that("subtheme AI table consumes duplicate primitive names left-to-right", {
  # A pinned record can request the same primitive twice with different args.
  rec <- list(column_name = "score", column_description = "d",
              requested_primitives = list(
                list(primitive = "prim_quantile", rationale = "q25", args = list(q = 0.25)),
                list(primitive = "prim_quantile", rationale = "q90", args = list(q = 0.90))),
              interpretation_note = "n")
  air <- pakhom:::.compute_requested_primitives(rec, c(0, 10, 20, 30, 100))
  ts <- list(metric_cols = "score", subtheme_stats = list(
    S = list(name = "S", description = "", n = 5L, metric_stats = list(),
             ai_metric_stats = list(score = air), example_quotes = character(0))))
  out <- pakhom:::.build_subtheme_summary_table(ts)
  # two "quantile score" headers
  expect_equal(length(regmatches(out, gregexpr("quantile score", out))[[1]]), 2L)
  row <- regmatches(out, gregexpr("<tr>.*?</tr>", out))[[1]]
  row <- row[grepl(">S<", row)]
  cc <- gsub("<[^>]+>", "", regmatches(row, gregexpr("<td[^>]*>(.*?)</td>", row))[[1]])
  # cc = [S, n=5, q25, q90, examples]; consumed left-to-right in plan order
  expect_equal(as.numeric(cc[3]),
               stats::quantile(c(0,10,20,30,100), 0.25, type = 7, names = FALSE))
  expect_equal(as.numeric(cc[4]),
               stats::quantile(c(0,10,20,30,100), 0.90, type = 7, names = FALSE))
})

# ---- (b) densify never drops an observed bin ---------------------------------

test_that(".densify_temporal_distribution never drops an observed bin", {
  # entries_by_month: zero-fills the gap, keeps every observed bin + total
  v <- stats::setNames(c(3, 3), c("2024-01", "2024-03"))
  d <- pakhom:::.densify_temporal_distribution("prim_entries_by_month", v)
  expect_equal(unname(d[["2024-02"]]), 0)
  expect_equal(sum(d), 6)
  expect_true(all(names(v) %in% names(d)))

  # entries_over_time on a REAL primitive grid: densifies, drops nothing
  ts <- as.POSIXct("2024-01-01", tz = "UTC") + c(0, 5, 65) * 86400
  raw <- prim_entries_over_time(ts, 30)
  dd <- pakhom:::.densify_temporal_distribution("prim_entries_over_time", raw,
                                                list(bin_width_days = 30))
  expect_true(all(names(raw) %in% names(dd)))
  expect_equal(sum(dd), sum(raw))

  # Pathological labels not on the reconstructed grid -> return verbatim
  # (the safety invariant: never drop a real bin to force continuity).
  bad <- stats::setNames(c(2, 1), c("2024-01-01", "2024-02-17"))
  out <- pakhom:::.densify_temporal_distribution("prim_entries_over_time", bad,
                                                 list(bin_width_days = 30))
  expect_true(all(names(bad) %in% names(out)))   # 2024-02-17 NOT dropped
  expect_equal(sum(out), sum(bad))
})
