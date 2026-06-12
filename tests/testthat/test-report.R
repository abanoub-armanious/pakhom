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

test_that(".html_esc is vectorized (longitudinal emergence-table regression)", {
  # The longitudinal emergence table renders a whole column at once via
  # paste0(.md_cell(col), collapse = ""); a length > 1 vector must escape
  # element-wise, not error in is.na() coercion to logical(1).
  expect_equal(pakhom:::.html_esc(c("a & b", "<x>", "c")),
               c("a &amp; b", "&lt;x&gt;", "c"))
  expect_equal(pakhom:::.html_esc(c("ok", NA, "x")), c("ok", "", "x"))
  expect_equal(pakhom:::.html_esc(character(0)), "")
  # .md_cell over a vector likewise does not error
  expect_equal(pakhom:::.md_cell(c("a|b", "c")), c("a\\|b", "c"))
})

# Regression guard: the report's knit-time chunks read the AC4-stamped output
# CSVs (sentiment/correlations/codes). If a generated read_csv omits comment='#',
# the stamp's "# methodology:" line is parsed as the header, every real column
# vanishes, and the rendered report fills 5 sections with R error tracebacks
# (23 were shipping in every run before this fix). The direct reads already pass
# comment='#'; the generated chunks must too. (End-to-end guard: the preflight
# smoke now also asserts no "## Error" survives in the rendered HTML.)
test_that("generated report chunks read stamped CSVs with comment='#' (no error cascade)", {
  data <- sample_data(10)
  data$emerged_themes <- c(rep("Sleep Disruption", 6), rep("Treatment Efficacy", 4))
  data$theme_membership_Sleep.Disruption <- c(rep(1L, 6), rep(0L, 4))
  data$theme_membership_Treatment.Efficacy <- c(rep(0L, 6), rep(1L, 4))
  stats <- aggregate_overall_statistics(data, mock_theme_set(), consolidated = NULL,
                                        learning_context = NULL, config = mock_config())
  ef <- list(sentiment_file = "sentiment_scores.csv",
             correlations_file = "correlations.csv", codes_file = "codes.csv")
  el <- pakhom:::.build_emotional_landscape(stats, ef)
  expect_match(el, "read_csv('sentiment_scores.csv', show_col_types = FALSE, comment = '#')",
               fixed = TRUE)
  # every generated read_csv in this chunk must skip the methodology stamp
  reads <- regmatches(el, gregexpr("read_csv\\([^)]*\\)", el))[[1]]
  expect_true(length(reads) >= 1L)
  expect_true(all(grepl("comment = '#'", reads, fixed = TRUE)))
})

test_that("correlation section guards the no-correlations case (no error cascade)", {
  # A focused corpus can yield ZERO correlation pairs -> header-only correlations.csv
  # -> readr types every column character -> filter/arrange/mutate cascade into 6 R
  # error tracebacks. The chunk must short-circuit to a graceful note.
  cs <- pakhom:::.build_correlation_section(
    corr_interpretation = NULL,
    export_files = list(correlations_file = "correlations.csv",
                        plot_file = "correlation_plot.png"))
  expect_match(cs, "if (nrow(correlations) == 0", fixed = TRUE)
  expect_match(cs, "No exploratory associations met the reporting threshold", fixed = TRUE)
})

test_that("methodology appendix describes the ACTUAL mode, not hardcoded reflexive (#6)", {
  data <- sample_data(10)
  data$emerged_themes <- c(rep("Sleep Disruption", 6), rep("Treatment Efficacy", 4))
  data$theme_membership_Sleep.Disruption <- c(rep(1L, 6), rep(0L, 4))
  data$theme_membership_Treatment.Efficacy <- c(rep(0L, 6), rep(1L, 4))
  stats <- aggregate_overall_statistics(data, mock_theme_set(), consolidated = NULL,
                                        learning_context = NULL, config = mock_config())
  ef <- list(codes_file = "codes.csv", sentiment_file = "sentiment_scores.csv",
             correlations_file = "correlations.csv")
  cfg <- mock_config(); cfg$methodology$mode <- "codebook_collaborative"
  ap <- pakhom:::.build_methodology_appendix(stats, ef, cfg)
  expect_match(ap, "codebook", fixed = TRUE)               # M2 = codebook TA
  expect_false(grepl("reflexive thematic analysis", ap, fixed = TRUE))
  cfg$methodology$mode <- "reflexive_scaffold"
  expect_match(pakhom:::.build_methodology_appendix(stats, ef, cfg), "reflexive", fixed = TRUE)
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

# Regression test — themes.json preserves codes_included as
# a JSON array, not as a ";"-collapsed string. An earlier output-quality
# audit incorrectly flagged "codes_included length 1 across all themes"
# as a merge-tree corruption; the actual cause was the tibble-then-write
# path collapsing character vectors to scalar strings. This test pins
# the array-shape behavior so it can't regress.
test_that("themes.json serializes codes_included / subthemes / keywords as JSON arrays (not ;-collapsed strings)", {
  tmp_dir <- withr::local_tempdir()
  data <- tibble::tibble(
    std_id = paste0("e", 1:6),
    std_text = paste("text", 1:6),
    sentiment_score = c(-0.5, 0, 0.5, -0.3, 0.1, 0.4),
    all_emotions = "joy",
    emotion_intensity = 0.5,
    emerged_themes = c(rep("Sleep Disruption", 3), rep("Treatment Efficacy", 3)),
    n_themes = 1L,
    source_table = "posts",
    theme_membership_Sleep.Disruption     = c(1L, 1L, 1L, 0L, 0L, 0L),
    theme_membership_Treatment.Efficacy   = c(0L, 0L, 0L, 1L, 1L, 1L)
  )
  # themes.json now serializes the canonical Theme -> Subtheme ->
  # Code hierarchy. Build the test fixture with first-class Subtheme S3
  # objects mirroring what generate_themes_iterative() produces in
  # production.
  ts <- create_theme_set(list(
    list(id = 1, name = "Sleep Disruption",
         description = "Themes about sleep disturbances",
         subthemes = list(
           create_subtheme(name = "difficulty falling asleep",
                           description = "trouble onset",
                           codes = c("insomnia", "early waking")),
           create_subtheme(name = "night waking",
                           description = "fragmented sleep cycle",
                           codes = c("fragmented sleep"))
         ),
         keywords = c("sleep", "insomnia"),
         supporting_quotes = c("Quote A", "Quote B"),
         entry_count = 3L,
         prevalence = "high",
         sentiment_tendency = "negative"),
    # Single-element vector — must STILL serialize as array, not unboxed scalar
    list(id = 2, name = "Single Code Theme",
         description = "Theme with one code",
         codes_included = c("solo_code"),
         subthemes = character(0),
         keywords = character(0),
         supporting_quotes = character(0),
         entry_count = 1L,
         prevalence = "low",
         sentiment_tendency = "neutral")
  ))

  files <- export_results(
    data = data, theme_set = ts,
    correlations_df = tibble::tibble(),
    insights = list(key_findings = list()),
    consolidated = list(codes = tibble::tibble(
      code_text = "x", frequency = 1L, code_type = "ai")),
    output_dir = tmp_dir
  )

  # Read back the themes.json with array-preserving parser
  themes_json <- jsonlite::fromJSON(files$themes_file, simplifyVector = FALSE)
  payload <- themes_json$`_payload` %||% themes_json   # in case AC4 stamp wraps it

  # Theme 1 has 3 codes -- must serialize as 3-element array
  t1 <- payload[[1]]
  expect_type(t1$codes_included, "list")           # JSON array -> R list
  expect_equal(length(t1$codes_included), 3L)
  expect_setequal(unlist(t1$codes_included),
                  c("insomnia", "fragmented sleep", "early waking"))

  # Theme 2 has 1 code -- the bug was that auto_unbox collapsed it to
  # a scalar string. Must STILL serialize as a 1-element array.
  t2 <- payload[[2]]
  expect_type(t2$codes_included, "list")
  expect_equal(length(t2$codes_included), 1L)
  expect_equal(t2$codes_included[[1]], "solo_code")

  # Same shape for subthemes + keywords + supporting_quotes
  expect_type(t1$subthemes, "list")
  expect_equal(length(t1$subthemes), 2L)
  expect_type(t1$keywords, "list")
  expect_equal(length(t1$keywords), 2L)
  expect_type(t1$supporting_quotes, "list")
  expect_equal(length(t1$supporting_quotes), 2L)

  # Empty-vector theme: empty array, not null
  expect_type(t2$subthemes, "list")
  expect_equal(length(t2$subthemes), 0L)
})

# --- verify_run_integrity tests ---

test_that("verify_run_integrity detects complete run", {
  tmp_dir <- withr::local_tempdir()
  # The integrity check now also requires the Tier-0 + Tier-1
  # outputs (run_metadata.json, rules/methodology_rules.md, fabrication
  # log, audit log, api_responses dir per AC4). Test fixture creates them.
  core_files <- c("sentiment_scores.csv", "codes.csv",
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
  expect_true("codes.csv" %in% result$missing)
  expect_false("themes.json" %in% result$missing)
})

test_that("verify_run_integrity checks conditional files from config", {
  tmp_dir <- withr::local_tempdir()
  core_files <- c("sentiment_scores.csv", "codes.csv",
                   "themes.json", "analysis_report.Rmd")
  for (f in core_files) file.create(file.path(tmp_dir, f))
  dir.create(file.path(tmp_dir, "theme_entries"))
  # correlation_plot.png is now expected only when
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

test_that(".build_saturation_section labels coded / examined / sampled distinctly (#7a)", {
  # The coverage banner reports the EXAMINED position ("examined 126 of 450
  # sampled") while this section reports the CODED count -- pre-#7a the latter
  # said "coding 40 of 450 total entries", which read as a contradiction with
  # the banner's 126. Now each of the three distinct counts is labeled.
  cs <- list(
    saturation = list(
      reached = TRUE, reached_at_coded = 40L, reached_at_entry = 126L,
      total_entries_at_saturation = 450L,
      curve = data.frame(entries_coded = c(20L, 40L), n_codes = c(31L, 33L),
                         new_codes_in_window = c(31L, 2L), reuse_density = c(0.49, 0.73)),
      ai_articulation = "Sharp decline in new codes per window.",
      ai_rationale = "31 to 2 with reuse density 0.73.", saturation_ratio = 0.825
    ),
    codebook = stats::setNames(replicate(33, list(name = "x"), simplify = FALSE),
                               paste0("c", 1:33))
  )
  sec <- pakhom:::.build_saturation_section(cs)
  expect_match(sec, "coding **40** of the **126** entries examined (450 sampled)", fixed = TRUE)
  # the suggested methods-section paragraph must ALSO be reconciled: NOWHERE in the
  # section should the bare coded-vs-sampled conflation "40 of 450 entries" appear
  # (it survived in the methods text in the first #7a pass -- this pins both sites).
  expect_no_match(sec, "40 of 450 entries", fixed = TRUE)
  expect_match(sec, "coding 40 of the 126 entries examined (450 sampled), at which point",
               fixed = TRUE)  # the methods-section paragraph specifically
  # back-compat: a pre-#7a state file without reached_at_entry -> graceful fallback
  cs2 <- cs; cs2$saturation$reached_at_entry <- NA_integer_
  expect_match(pakhom:::.build_saturation_section(cs2), "of the 450 entries sampled", fixed = TRUE)
})
