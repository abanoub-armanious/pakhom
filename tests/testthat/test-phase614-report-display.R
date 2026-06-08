# Report DISPLAY of the Methodology Assistant's analytical
# decisions. Three surfaces:
#   (1) per-subtheme summary table prefers the AI's chosen primitives
#       (one column per primitive + interpretation notes), falling back to the
#       legacy Median(MAD)+Mean(SD) battery per column when a column was not
#       interpreted -- BYTE-IDENTICAL legacy output when no column was.
#   (2) the "Methodology Setup" section (relevance criterion + per-metric
#       interpretations) -- see the Step 2 block below.
#   (3) the per-theme temporal panel -- see the Step 3 block below.
#
# The load-bearing principle under test: every surface renders the AI's OWN
# recorded decision (the primitive it named + its free-form note), never a
# fixed taxonomy imposed on the researcher. The backend catalog is invisible.

# ===========================================================================
# Step 1 -- display helpers
# ===========================================================================

test_that(".pretty_primitive_name strips prim_ and spaces underscores", {
  expect_equal(pakhom:::.pretty_primitive_name("prim_p90"), "p90")
  expect_equal(pakhom:::.pretty_primitive_name("prim_hour_of_day_distribution"),
               "hour of day distribution")
  # AI-named primitive the catalog lacks still prints its requested name
  expect_equal(pakhom:::.pretty_primitive_name("circular_mean_of_scores"),
               "circular mean of scores")
})

test_that(".format_primitive_result renders an available scalar", {
  prec <- list(primitive = "prim_median", available = TRUE, shape = "scalar",
               value = 8, n_observed = 5L, reason = NA_character_)
  expect_equal(pakhom:::.format_primitive_result(prec), "8")
})

test_that(".format_primitive_result fails honest on an unavailable primitive (R4)", {
  prec <- list(primitive = "prim_nope", available = FALSE, shape = NA_character_,
               value = NA_real_, n_observed = 0L,
               reason = "Primitive 'prim_nope' is not in the metric catalog.")
  out <- pakhom:::.format_primitive_result(prec)
  expect_match(out, "&mdash;", fixed = TRUE)               # em dash, NOT a number
  expect_match(out, "prim-unavailable", fixed = TRUE)
  expect_match(out, "not in the metric catalog", fixed = TRUE)  # reason in title
  # crucially: no substituted statistic in the VISIBLE cell. The title
  # attribute legitimately contains HTML-escaped apostrophes (&#39;) which
  # carry digits, so strip the title before asserting no number leaked.
  visible <- gsub('title="[^"]*"', "", out)
  expect_false(grepl("[0-9]", visible))
})

test_that(".format_primitive_result renders a distribution compactly, descending", {
  prec <- list(primitive = "prim_frequency_distribution", available = TRUE,
               shape = "distribution",
               value = stats::setNames(c(1, 1, 3, 5), c("1", "3", "4", "5")),
               n_observed = 10L, reason = NA_character_)
  out <- pakhom:::.format_primitive_result(prec)
  expect_match(out, "5: 5", fixed = TRUE)   # most frequent first
  expect_match(out, "4: 3", fixed = TRUE)
})

test_that(".format_primitive_result truncates long distributions honestly", {
  v <- stats::setNames(as.numeric(20:1), sprintf("k%02d", 20:1))
  prec <- list(primitive = "prim_frequency_distribution", available = TRUE,
               shape = "distribution", value = v, n_observed = sum(v),
               reason = NA_character_)
  out <- pakhom:::.format_primitive_result(prec, max_items = 6L)
  expect_match(out, "\\(\\+14 more\\)")   # 20 - 6 shown
})

test_that(".format_primitive_result preserves natural order when asked (temporal)", {
  # hour-of-day style: names already in clock order; do NOT re-sort by count
  v <- stats::setNames(c(2, 9, 1), c("08", "09", "10"))
  out <- pakhom:::.format_primitive_result(
    list(primitive = "prim_hour_of_day_distribution", available = TRUE,
         shape = "distribution", value = v, n_observed = 12L,
         reason = NA_character_),
    natural_order = TRUE)
  # 08 appears before 09 even though 09 has the higher count
  expect_lt(regexpr("08:", out, fixed = TRUE),
            regexpr("09:", out, fixed = TRUE))
})

test_that(".format_primitive_result is NA-safe", {
  expect_equal(pakhom:::.format_primitive_result(NULL), "n/a")
  expect_equal(pakhom:::.format_primitive_result(list(
    primitive = "prim_median", available = TRUE, shape = "scalar",
    value = NA_real_, n_observed = 0L, reason = NA_character_)), "n/a")
  expect_equal(pakhom:::.format_primitive_result(list(
    primitive = "prim_frequency_distribution", available = TRUE,
    shape = "distribution", value = stats::setNames(numeric(0), character(0)),
    n_observed = 0L, reason = NA_character_)), "n/a")
})

# ===========================================================================
# Step 1 -- .build_subtheme_summary_table back-compat + AI path
# ===========================================================================

test_that("subtheme table is BYTE-IDENTICAL to legacy when no column interpreted", {
  # No ai_metric_stats field at all (pre-61.3b stats object). The legacy caption
  # now carries the small-n caveat (audit followup -- the legacy spread
  # battery must disclose small-n fragility too, since the per-cell dagger only fires
  # on the AI path); the table is otherwise byte-identical to the legacy render.
  ts <- list(metric_cols = c("score"),
    subtheme_stats = list(
      Morning = list(name = "Morning", description = "AM", n = 3L,
        metric_stats = list(score = list(median = 8, mad = 1, mean = 7.8, sd = 0.9)),
        example_quotes = c("q1 [score: 8]"))))
  got <- pakhom:::.build_subtheme_summary_table(ts)
  expected <- paste0(
    "<h3>Subthemes (per-subtheme summary)</h3>\n\n",
    "<p class=\"subtheme-table-caption\"><em>",
    "Median(MAD) and Mean(SD) columns are subtheme aggregates -- read spread ",
    "(MAD, SD) as indicative, not precise, when n is small (the n is shown ",
    "beside each); the bracketed values after each example comment are that ",
    "source entry's metric values.</em></p>\n\n",
    "<div class=\"subtheme-table-wrapper\">\n",
    "<table class=\"subtheme-summary-table\">\n",
    "<thead><tr><th>Subtheme</th><th>n</th><th>Median(MAD) score</th>",
    "<th>Mean(SD) score</th><th>Examples of comments</th></tr></thead>\n",
    "<tbody><tr><td><div class=\"st-name\"><strong>Morning</strong></div>",
    "<div class=\"st-desc\"><em>AM</em></div></td><td>3</td>",
    "<td>8.00 (1.00)</td><td>7.80 (0.90)</td>",
    "<td><div class=\"st-quote\">q1 [score: 8]</div></td></tr></tbody>\n",
    "</table>\n</div>\n\n")
  expect_identical(got, expected)
})

test_that("subtheme table is byte-identical legacy when ai_metric_stats present-but-empty", {
  # 61.3b NULL-interpretation path: the field exists but is an empty list.
  ts <- list(metric_cols = c("score"),
    subtheme_stats = list(
      Morning = list(name = "Morning", description = "AM", n = 3L,
        metric_stats = list(score = list(median = 8, mad = 1, mean = 7.8, sd = 0.9)),
        ai_metric_stats = list(),
        example_quotes = c("q1 [score: 8]"))))
  got <- pakhom:::.build_subtheme_summary_table(ts)
  expect_match(got, "Median(MAD) score", fixed = TRUE)
  expect_false(grepl("metric-interpretation-notes", got, fixed = TRUE))
})

test_that("subtheme table renders one column per AI primitive + interpretation note", {
  ai_rec <- pakhom:::.compute_requested_primitives(
    list(column_name = "score", column_description = "Reddit upvotes; heavy-tailed.",
         requested_primitives = list(
           list(primitive = "prim_median", rationale = "robust"),
           list(primitive = "prim_p90",    rationale = "tail")),
         interpretation_note = "Cite median + p90; the mean is misleading."),
    c(0, 1, 5, 100, 2))
  ts <- list(metric_cols = c("score"),
    subtheme_stats = list(
      Morning = list(name = "Morning", description = "AM", n = 5L,
        metric_stats = list(score = list(median = 2, mad = 1.48, mean = 21.6, sd = 43.4)),
        ai_metric_stats = list(score = ai_rec),
        example_quotes = character(0))))
  out <- pakhom:::.build_subtheme_summary_table(ts)
  expect_match(out, "<th>median score</th>", fixed = TRUE)
  expect_match(out, "<th>p90 score</th>", fixed = TRUE)
  expect_false(grepl("Median(MAD) score", out, fixed = TRUE))   # legacy header replaced
  expect_match(out, "<td>2</td>", fixed = TRUE)                  # median value
  expect_match(out, "<td>62</td>", fixed = TRUE)                 # type-7 p90 of the vector
  expect_match(out, "How to read:", fixed = TRUE)
  expect_match(out, "the mean is misleading", fixed = TRUE)
})

test_that("subtheme table mixes AI columns and legacy columns side by side", {
  ai_rec <- pakhom:::.compute_requested_primitives(
    list(column_name = "score", column_description = "heavy-tailed",
         requested_primitives = list(list(primitive = "prim_median", rationale = "r")),
         interpretation_note = "median only"),
    c(1, 2, 3, 4, 5))
  ts <- list(metric_cols = c("score", "upvote_ratio"),  # upvote_ratio NOT interpreted
    subtheme_stats = list(
      S = list(name = "S", description = "", n = 5L,
        metric_stats = list(
          score        = list(median = 3, mad = 1.48, mean = 3, sd = 1.58),
          upvote_ratio = list(median = 0.9, mad = 0.07, mean = 0.83, sd = 0.2)),
        ai_metric_stats = list(score = ai_rec),
        example_quotes = character(0))))
  out <- pakhom:::.build_subtheme_summary_table(ts)
  expect_match(out, "<th>median score</th>", fixed = TRUE)            # AI col
  expect_match(out, "Median(MAD) upvote ratio", fixed = TRUE)          # legacy col kept
  expect_match(out, "0.90 (0.07)", fixed = TRUE)                       # legacy aggregate cell
})

test_that("subtheme table surfaces a fail-honest gap by name (R4) and computes nothing for it", {
  ai_rec <- pakhom:::.compute_requested_primitives(
    list(column_name = "score", column_description = "x",
         requested_primitives = list(
           list(primitive = "prim_median", rationale = "ok"),
           list(primitive = "prim_circular_mean_of_scores", rationale = "gap")),
         interpretation_note = "note"),
    c(1, 2, 3))
  ts <- list(metric_cols = "score",
    subtheme_stats = list(
      S = list(name = "S", description = "", n = 3L,
        metric_stats = list(score = list(median = 2, mad = 1, mean = 2, sd = 1)),
        ai_metric_stats = list(score = ai_rec),
        example_quotes = character(0))))
  out <- pakhom:::.build_subtheme_summary_table(ts)
  expect_match(out, "prim-unavailable", fixed = TRUE)                 # em-dash cell
  expect_match(out, "Requested unavailable primitive", fixed = TRUE)  # named in notes
  expect_match(out, "prim_circular_mean_of_scores", fixed = TRUE)
})

# ===========================================================================
# Step 2 -- Methodology Setup section + archive fallback
# ===========================================================================

.make_methodology_bundle <- function(source = "ai") {
  rel <- new_relevance_criterion(
    research_focus_paraphrase = "How medication timing relates to sleep quality.",
    relevance_criterion = "On-focus if it connects medication (dose/timing) to a sleep outcome.",
    on_focus_examples  = c("I take my pill at 9pm and sleep well."),
    off_focus_examples = c("I love this medication."),
    discrimination_principle = "Must tie medication to a SLEEP outcome.",
    source = source)
  mi <- new_metric_interpretation(
    metrics = list(list(column_name = "score",
      column_description = "Reddit upvotes; heavy-tailed.",
      requested_primitives = list(list(primitive = "prim_median", rationale = "robust"),
                                  list(primitive = "prim_p90", rationale = "tail")),
      interpretation_note = "Cite median + p90; mean is misleading.")),
    temporal_columns = list(list(column_name = "std_timestamp",
      column_description = "Post datetime (UTC).",
      requested_primitives = list(list(primitive = "prim_hour_of_day_distribution",
                                       rationale = "rhythm")),
      interpretation_note = "Evening clustering expected.")),
    source = source)
  new_methodology_articulations(rel, mi, research_focus = "med x sleep", source = source)
}

test_that(".build_methodology_setup_section renders criterion, examples, and tables", {
  out <- pakhom:::.build_methodology_setup_section(.make_methodology_bundle("ai"))
  expect_match(out, "Methodology Setup", fixed = TRUE)
  expect_match(out, "AI-articulated", fixed = TRUE)
  expect_match(out, "connects medication", fixed = TRUE)
  expect_match(out, "I take my pill at 9pm", fixed = TRUE)        # on-focus
  expect_match(out, "I love this medication", fixed = TRUE)        # off-focus
  expect_match(out, "Must tie medication to a SLEEP", fixed = TRUE)
  expect_match(out, "<code>prim_median</code>", fixed = TRUE)      # auditable primitive id
  expect_match(out, "mean is misleading", fixed = TRUE)
  expect_match(out, "std_timestamp", fixed = TRUE)                 # temporal table
  expect_match(out, "Evening clustering expected", fixed = TRUE)
})

test_that(".build_methodology_setup_section marks pinned-replay provenance", {
  out <- pakhom:::.build_methodology_setup_section(.make_methodology_bundle("pinned"))
  expect_match(out, "pinned replay", fixed = TRUE)
})

test_that(".build_methodology_setup_section returns '' for an empty bundle", {
  empty <- new_methodology_articulations(new_relevance_criterion(),
                                         new_metric_interpretation())
  expect_identical(pakhom:::.build_methodology_setup_section(empty), "")
  expect_identical(pakhom:::.build_methodology_setup_section(NULL), "")
})

test_that(".build_methodology_setup_section HTML-escapes (XSS safety)", {
  art <- new_methodology_articulations(
    new_relevance_criterion(relevance_criterion = "x <script>alert(1)</script> y"),
    new_metric_interpretation())
  out <- pakhom:::.build_methodology_setup_section(art)
  expect_match(out, "&lt;script&gt;", fixed = TRUE)
  expect_false(grepl("<script>", out, fixed = TRUE))
})

test_that(".load_methodology_articulations_from_run_dir round-trips the archive", {
  run_dir <- withr::local_tempdir()
  art <- .make_methodology_bundle("ai")
  archive_methodology_articulations(art, run_dir)
  loaded <- pakhom:::.load_methodology_articulations_from_run_dir(run_dir)
  expect_s3_class(loaded, "MethodologyArticulations")
  expect_match(loaded$relevance$relevance_criterion, "connects medication")
  expect_identical(loaded$metric_interpretation$metrics[[1]]$column_name, "score")
  expect_identical(
    loaded$metric_interpretation$metrics[[1]]$requested_primitives[[1]]$primitive,
    "prim_median")
})

test_that(".load_methodology_articulations_from_run_dir returns NULL when absent", {
  expect_null(pakhom:::.load_methodology_articulations_from_run_dir(withr::local_tempdir()))
  expect_null(pakhom:::.load_methodology_articulations_from_run_dir(NULL))
})

test_that("generate_report derives metric_interpretation from the bundle when not passed", {
  # White-box: the derivation guard lives at the top of generate_report. Here we
  # assert the bundle's metric_interpretation is structurally what 61.3b expects.
  art <- .make_methodology_bundle("ai")
  expect_s3_class(art$metric_interpretation, "MetricInterpretation")
  rec <- pakhom:::.metric_interpretation_record(art$metric_interpretation, "score")
  expect_false(is.null(rec))
  expect_identical(rec$requested_primitives[[2]]$primitive, "prim_p90")
})

# ===========================================================================
# Step 3.5 -- primitive args survive coercion + serialization + compute
# ===========================================================================

test_that("pinned-replay primitive args round-trip and drive compute (Step 3.5)", {
  inferred <- list(
    relevance_criterion = "x",
    metrics = list(list(column_name = "score",
      requested_primitives = list(
        list(primitive = "prim_median", rationale = "robust"),
        list(primitive = "prim_quantile", args = list(q = 0.9))),
      interpretation_note = "median + p90")),
    temporal_columns = list(list(column_name = "std_timestamp",
      requested_primitives = list(
        list(primitive = "prim_entries_over_time", args = list(bin_width_days = 30))),
      interpretation_note = "monthly")))
  art <- load_pinned_methodology(inferred)
  rec <- art$metric_interpretation$metrics[[1]]
  expect_identical(rec$requested_primitives[[2]]$args$q, 0.9)         # coercion kept args
  # serialize: args present for parameterized, ABSENT for zero-arg (byte-compat)
  lst <- methodology_articulations_to_list(art)
  expect_identical(lst$metrics[[1]]$requested_primitives[[2]]$args$q, 0.9)
  expect_null(lst$metrics[[1]]$requested_primitives[[1]]$args)
  # reload keeps args
  art2 <- methodology_articulations_from_list(lst)
  expect_identical(
    art2$metric_interpretation$metrics[[1]]$requested_primitives[[2]]$args$q, 0.9)
  # compute USES the args: prim_quantile q=0.9 over c(0,1,5,100,2) is ~62
  res <- pakhom:::.compute_requested_primitives(rec, c(0, 1, 5, 100, 2))
  expect_true(res$requested[[2]]$available)
  expect_equal(res$requested[[2]]$value, 62)                          # tolerance, not identical
})

test_that("discovery records (no args) are byte-identical through serialization", {
  art <- .make_methodology_bundle("ai")   # discovery-shaped: no args anywhere
  lst <- methodology_articulations_to_list(art)
  # no args key anywhere in the serialized metric primitives
  for (m in lst$metrics) for (p in m$requested_primitives) expect_null(p$args)
})

# ===========================================================================
# Step 3 -- per-theme temporal panel (compute + render)
# ===========================================================================

.temporal_test_setup <- function() {
  ts_strings <- c(
    "2024-01-05 21:00:00", "2024-01-12 22:30:00", "2024-01-20 20:15:00",
    "2024-03-03 23:00:00", "2024-03-09 21:45:00", "2024-03-15 19:30:00")  # Feb empty
  entries <- tibble::tibble(
    std_id = paste0("e", 1:6), std_text = paste("entry", 1:6),
    std_timestamp = ts_strings, theme_membership_T = rep(1L, 6L))
  mi <- new_metric_interpretation(
    temporal_columns = list(list(column_name = "std_timestamp",
      column_description = "Post datetime (UTC).",
      requested_primitives = list(
        list(primitive = "prim_hour_of_day_distribution", rationale = "rhythm"),
        list(primitive = "prim_entries_by_month", rationale = "volume"),
        list(primitive = "prim_time_span_days", rationale = "span"),
        list(primitive = "prim_made_up_temporal", rationale = "gap")),
      interpretation_note = "Evening clustering; volume rose in March.")),
    source = "ai")
  list(entries = entries, mi = mi)
}

test_that(".compute_theme_temporal_panel applies the AI's temporal primitives", {
  s <- .temporal_test_setup()
  panel <- pakhom:::.compute_theme_temporal_panel(s$entries, s$mi)
  expect_false(is.null(panel))
  reqs <- panel[[1]]$requested
  prims <- vapply(reqs, function(r) r$primitive, character(1))
  expect_length(reqs, 4L)
  hod <- reqs[[which(prims == "prim_hour_of_day_distribution")]]
  expect_true(hod$available)
  expect_length(hod$value, 24L)                                   # all clock bins
  expect_equal(sum(hod$value[c("19","20","21","22","23")]), 6)    # all evening
})

test_that(".compute_theme_temporal_panel zero-fills the volume timeline (L2)", {
  s <- .temporal_test_setup()
  panel <- pakhom:::.compute_theme_temporal_panel(s$entries, s$mi)
  reqs <- panel[[1]]$requested
  ebm <- reqs[[which(vapply(reqs, function(r) r$primitive, character(1)) ==
                       "prim_entries_by_month")]]
  expect_length(ebm$value, 3L)                                    # Jan, Feb, Mar contiguous
  expect_true("2024-02" %in% names(ebm$value))
  expect_equal(unname(ebm$value[["2024-02"]]), 0)                 # empty bin made visible
  expect_equal(unname(ebm$value[["2024-01"]]), 3)
  expect_equal(unname(ebm$value[["2024-03"]]), 3)
})

test_that(".compute_theme_temporal_panel marks a fail-honest temporal gap (R4)", {
  s <- .temporal_test_setup()
  panel <- pakhom:::.compute_theme_temporal_panel(s$entries, s$mi)
  reqs <- panel[[1]]$requested
  gap <- reqs[[which(vapply(reqs, function(r) r$primitive, character(1)) ==
                       "prim_made_up_temporal")]]
  expect_false(gap$available)
})

test_that(".compute_theme_temporal_panel is NULL when inapplicable", {
  s <- .temporal_test_setup()
  expect_null(pakhom:::.compute_theme_temporal_panel(s$entries, NULL))
  expect_null(pakhom:::.compute_theme_temporal_panel(s$entries, new_metric_interpretation()))
  expect_null(pakhom:::.compute_theme_temporal_panel(s$entries[0, ], s$mi))
})

test_that(".build_temporal_panel renders chronological order + note + gap", {
  s <- .temporal_test_setup()
  panel <- pakhom:::.compute_theme_temporal_panel(s$entries, s$mi)
  html <- pakhom:::.build_temporal_panel(list(temporal_panel = panel))
  expect_match(html, "Posting-time patterns", fixed = TRUE)
  expect_match(html, "hour of day distribution", fixed = TRUE)   # prettied primitive
  # chronological (natural) order, NOT sorted by count
  expect_lt(regexpr("2024-01", html), regexpr("2024-02", html))
  expect_lt(regexpr("2024-02", html), regexpr("2024-03", html))
  expect_match(html, "prim-unavailable", fixed = TRUE)            # fail-honest gap
  expect_match(html, "Evening clustering", fixed = TRUE)          # interpretation note
})

test_that(".build_temporal_panel returns '' when no panel", {
  expect_identical(pakhom:::.build_temporal_panel(list()), "")
  expect_identical(pakhom:::.build_temporal_panel(list(temporal_panel = NULL)), "")
})

test_that("aggregate_theme_statistics attaches a temporal_panel end-to-end", {
  s <- .temporal_test_setup()
  tsobj <- create_theme_set(list(list(name = "T", description = "d",
    subthemes = list(create_subtheme(name = "S", description = "s",
      codes = list(create_code_object(key = "k", name = "K")))))))
  data <- tibble::tibble(
    std_id = paste0("e", 1:6), std_text = paste("q", 1:6),
    std_timestamp = s$entries$std_timestamp, sentiment_score = rep(0, 6),
    emotion_intensity = rep(0.5, 6), all_emotions = rep("neutral", 6),
    theme_membership_T = rep(1L, 6L), subtheme_assignments = rep("S", 6L),
    emerged_themes = rep("T", 6L))
  agg <- aggregate_theme_statistics(data, tsobj, metric_interpretation = s$mi)
  expect_false(is.null(agg[["T"]]$temporal_panel))
  expect_true(nzchar(pakhom:::.build_temporal_panel(agg[["T"]])))
})

test_that("aggregate_theme_statistics: temporal_panel is NULL without temporal interpretation", {
  tsobj <- create_theme_set(list(list(name = "T", description = "d",
    subthemes = list(create_subtheme(name = "S", description = "s",
      codes = list(create_code_object(key = "k", name = "K")))))))
  data <- tibble::tibble(
    std_id = c("e1","e2"), std_text = c("a","b"),
    std_timestamp = c("2024-01-01 10:00:00","2024-01-02 11:00:00"),
    sentiment_score = c(0, 0), emotion_intensity = c(0.5, 0.5),
    all_emotions = c("neutral","neutral"),
    theme_membership_T = c(1L, 1L), subtheme_assignments = c("S","S"),
    emerged_themes = c("T","T"))
  agg <- aggregate_theme_statistics(data, tsobj)   # no metric_interpretation
  expect_null(agg[["T"]]$temporal_panel)
  expect_identical(pakhom:::.build_temporal_panel(agg[["T"]]), "")
})

test_that(".enumerate_year_months spans inclusive months (incl. year boundary)", {
  expect_equal(pakhom:::.enumerate_year_months("2024-11", "2025-02"),
               c("2024-11", "2024-12", "2025-01", "2025-02"))
  expect_equal(pakhom:::.enumerate_year_months("2024-03", "2024-03"), "2024-03")
  expect_equal(pakhom:::.enumerate_year_months("2024-05", "2024-01"), character(0))  # reversed
})
