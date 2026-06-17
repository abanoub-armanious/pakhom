# Tests for the Mode 1 (Reflexive Scaffold) report renderer
# (R/mode1_report.R). Exercises:
#   - compute_provocation_provenance_stats: T0.1 stats over reflection log
#   - .build_mode1_executive_summary: deterministic exec summary content
#   - .build_mode1_provocation_section: per-theme provocations Rmd
#   - generate_mode1_report: end-to-end Rmd assembly + render
# Mode 1 has no sentiment / correlations / AI synthesis -- the report is
# a Sarkar-pattern artifact (provocations + Tier-0 cards + integrity)
# rather than the Mode 2/3 thematic+correlation report.

# ---- compute_provocation_provenance_stats ---------------------------------

test_that("compute_provocation_provenance_stats returns empty shape for NULL", {
  s <- compute_provocation_provenance_stats(NULL)
  expect_equal(s$total, 0L)
  expect_true(is.na(s$verification_rate))
})

test_that("compute_provocation_provenance_stats returns empty shape for empty log", {
  log <- create_reflection_log()
  s <- compute_provocation_provenance_stats(log)
  expect_equal(s$total, 0L)
})

test_that("compute_provocation_provenance_stats walks provocations + extracts QuoteProvenance", {
  log <- create_reflection_log()
  src <- "I always forget my pills."
  q <- make_quote("e1", "data_entry", src, 0L, 8L, "I always",
                    citation_source = "model_freeform")
  q <- verify_quote(q, src)
  log$provocations[[1]] <- make_provocation(
    category = "counter_narrative", theme_name = "Adherence",
    reason = "denies adherence", provenance = q
  )
  log$provocations[[2]] <- make_provocation(
    category = "absent_voice", theme_name = "Adherence",
    reason = "no adolescent voice",
    provenance = NULL  # observational; should NOT count
  )

  s <- compute_provocation_provenance_stats(log)
  expect_equal(s$total, 1L)  # only the verbatim-cited one
  expect_true(s$verification_rate >= 0 && s$verification_rate <= 1)
})

# ---- generate_mode1_report input validation -------------------------------

test_that("generate_mode1_report rejects non-ThemeSet theme_set", {
  log <- create_reflection_log()
  data <- tibble::tibble(std_id = "e1", std_text = "x")
  expect_error(
    generate_mode1_report(data = data, theme_set = list(),
                            reflection_log = log),
    "ThemeSet"
  )
})

test_that("generate_mode1_report rejects non-ResearcherReflectionLog log", {
  ts <- create_theme_set(list(list(id=1L, name="A", description="",
                                      codes_included="x")))
  data <- tibble::tibble(std_id = "e1", std_text = "x")
  expect_error(
    generate_mode1_report(data = data, theme_set = ts,
                            reflection_log = list()),
    "ResearcherReflectionLog"
  )
})

test_that("generate_mode1_report rejects non-data.frame data", {
  ts <- create_theme_set(list(list(id=1L, name="A", description="",
                                      codes_included="x")))
  log <- create_reflection_log()
  expect_error(
    generate_mode1_report(data = list("not a df"), theme_set = ts,
                            reflection_log = log),
    "data\\.frame"
  )
})

# ---- Rmd content shape ----------------------------------------------------

.build_minimal_mode1_inputs <- function() {
  data <- tibble::tibble(
    std_id = paste0("e", 1:4),
    std_text = c("a", "b", "c", "d"),
    std_author = c("alice", "bob", "alice", "carol"),
    theme_membership_Adherence = c(1L, 1L, 0L, 0L),
    theme_membership_Resistance = c(0L, 0L, 1L, 1L)
  )
  ts <- create_theme_set(list(
    list(id = 1L, name = "Adherence", description = "Adherence theme",
         codes_included = "x"),
    list(id = 2L, name = "Resistance", description = "Resistance theme",
         codes_included = "y")
  ))
  log <- create_reflection_log()
  src <- "I plan to take my scheduling every day."
  q <- make_quote("e1", "data_entry", src, 0L, 6L, "I plan",
                    citation_source = "model_freeform")
  q <- verify_quote(q, src)
  log$provocations[[1]] <- make_provocation(
    category = "counter_narrative", theme_name = "Adherence",
    reason = "test reason", provenance = q
  )
  log$provocation_attempts <- data.frame(
    theme_name   = c("Adherence", "Resistance"),
    category     = c("counter_narrative", "counter_narrative"),
    n_emitted    = c(1L, 0L),
    attempted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  cov <- compute_mode1_coverage(log, ts, data,
                                  requested_categories = "counter_narrative")
  stats <- compute_mode1_theme_stats(data, ts, log)
  list(data = data, theme_set = ts, reflection_log = log,
       coverage = cov, theme_stats = stats)
}

test_that(".build_mode1_rmd_content includes the methodology stamp + Tier-0 dashboards", {
  inp <- .build_minimal_mode1_inputs()
  prov_stats <- compute_provocation_provenance_stats(inp$reflection_log)
  rmd <- pakhom:::.build_mode1_rmd_content(
    data = inp$data, theme_set = inp$theme_set,
    reflection_log = inp$reflection_log,
    coverage = inp$coverage, theme_stats = inp$theme_stats,
    config = list(methodology = list(mode = "reflexive_scaffold")),
    run_id = "test-run", prov_stats = prov_stats,
    integrity = list(expected = c("a", "b"), missing = character(0),
                       complete = TRUE)
  )
  rmd_str <- paste(rmd, collapse = "\n")
  # Methodology stamp at top
  expect_match(rmd_str, "M1 - Reflexive Scaffold")
  # Tier-0 verification dashboard (T0.1)
  expect_match(rmd_str, "Data Integrity Dashboard")
  # Mode 1 coverage card (T0.3)
  expect_match(rmd_str, "Provocation Coverage")
  # Per-theme section
  expect_match(rmd_str, "Provocations by Theme")
  # Both researcher-authored themes appear
  expect_match(rmd_str, "Adherence")
  expect_match(rmd_str, "Resistance")
})

test_that(".build_mode1_rmd_content surfaces participant-spread cards per theme", {
  inp <- .build_minimal_mode1_inputs()
  prov_stats <- compute_provocation_provenance_stats(inp$reflection_log)
  rmd <- pakhom:::.build_mode1_rmd_content(
    data = inp$data, theme_set = inp$theme_set,
    reflection_log = inp$reflection_log,
    coverage = inp$coverage, theme_stats = inp$theme_stats,
    config = list(methodology = list(mode = "reflexive_scaffold")),
    run_id = "test-run", prov_stats = prov_stats
  )
  rmd_str <- paste(rmd, collapse = "\n")
  expect_match(rmd_str, "Participant Distribution")
})

test_that(".build_mode1_rmd_content renders the cited verbatim quote in a provocation block", {
  inp <- .build_minimal_mode1_inputs()
  prov_stats <- compute_provocation_provenance_stats(inp$reflection_log)
  rmd <- pakhom:::.build_mode1_rmd_content(
    data = inp$data, theme_set = inp$theme_set,
    reflection_log = inp$reflection_log,
    coverage = inp$coverage, theme_stats = inp$theme_stats,
    config = list(methodology = list(mode = "reflexive_scaffold")),
    run_id = "test-run", prov_stats = prov_stats
  )
  rmd_str <- paste(rmd, collapse = "\n")
  # The verbatim cited text "I plan" should appear in a provocation block
  expect_match(rmd_str, "I plan")
  # Verification status badge (scoped display label: the badge certifies
  # the QUOTE, not the challenge argument)
  expect_match(rmd_str, "quote verified \\((exact|fuzzy)\\)")
})

test_that(".build_mode1_rmd_content includes the Skipped Themes section when skips exist", {
  inp <- .build_minimal_mode1_inputs()
  inp$reflection_log$skipped_themes <- data.frame(
    theme_name = "Empty",
    reason     = "no_supporting_entries",
    skipped_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  prov_stats <- compute_provocation_provenance_stats(inp$reflection_log)
  rmd <- pakhom:::.build_mode1_rmd_content(
    data = inp$data, theme_set = inp$theme_set,
    reflection_log = inp$reflection_log,
    coverage = inp$coverage, theme_stats = inp$theme_stats,
    config = list(methodology = list(mode = "reflexive_scaffold")),
    run_id = "test-run", prov_stats = prov_stats
  )
  rmd_str <- paste(rmd, collapse = "\n")
  expect_match(rmd_str, "Themes Explicitly Skipped|Explicitly Skipped")
  expect_match(rmd_str, "Empty")
})

# ---- Deterministic executive summary --------------------------------------

test_that(".build_mode1_executive_summary tallies provocations and surfaces top categories", {
  inp <- .build_minimal_mode1_inputs()
  prov_stats <- compute_provocation_provenance_stats(inp$reflection_log)
  s <- pakhom:::.build_mode1_executive_summary(
    inp$reflection_log, inp$theme_set, inp$theme_stats,
    inp$coverage, prov_stats
  )
  expect_match(s, "1 AI-extracted ")
  expect_match(s, "counter_narrative")
})

test_that(".build_mode1_executive_summary flags participant concentration when present", {
  data <- tibble::tibble(
    std_id = paste0("e", 1:5),
    std_text = letters[1:5],
    # All 5 entries authored by one user -> top_share = 1.0
    std_author = rep("monopolist", 5L),
    theme_membership_Concentrated = rep(1L, 5L)
  )
  ts <- create_theme_set(list(list(id = 1L, name = "Concentrated",
                                      description = "", codes_included = "x")))
  log <- create_reflection_log()
  log$provocation_attempts <- data.frame(
    theme_name = "Concentrated", category = "counter_narrative",
    n_emitted = 0L,
    attempted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  cov <- compute_mode1_coverage(log, ts, data,
                                  requested_categories = "counter_narrative")
  stats <- compute_mode1_theme_stats(data, ts, log)
  prov_stats <- compute_provocation_provenance_stats(log)

  s <- pakhom:::.build_mode1_executive_summary(log, ts, stats, cov, prov_stats)
  expect_match(s, "Participant-concentration flags")
  expect_match(s, "Concentrated")
})

# ---- End-to-end: render HTML ----------------------------------------------

test_that("Mode 1 with zero provocations renders without crashing", {
  # Audit B test gap: empty provocations list exercises the
  # `top_cats <- 'none'` and `top_disconfirmed <- 'none'` branches in
  # the deterministic exec summary, plus the empty prov_stats path
  # through .build_tier0_dashboard.
  data <- tibble::tibble(
    std_id = c("e1", "e2"), std_text = c("a", "b"),
    std_author = c("alice", "bob"),
    theme_membership_T = c(1L, 1L)
  )
  ts <- create_theme_set(list(list(id = 1L, name = "T", description = "",
                                      codes_included = "x")))
  log <- create_reflection_log()
  log$provocation_attempts <- data.frame(
    theme_name = "T", category = "counter_narrative",
    n_emitted = 0L,
    attempted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  cov <- compute_mode1_coverage(log, ts, data,
                                  requested_categories = "counter_narrative")
  stats <- compute_mode1_theme_stats(data, ts, log)
  prov_stats <- compute_provocation_provenance_stats(log)

  rmd <- pakhom:::.build_mode1_rmd_content(
    data = data, theme_set = ts, reflection_log = log,
    coverage = cov, theme_stats = stats,
    config = list(methodology = list(mode = "reflexive_scaffold")),
    run_id = "test-zero-provs", prov_stats = prov_stats
  )
  rmd_str <- paste(rmd, collapse = "\n")
  expect_match(rmd_str, "Reflexive Scaffold")
  # Empty case must say "No verbatim claims to verify" (audit B fix),
  # not the misleading "No fabrications detected"
  expect_match(rmd_str, "No verbatim claims to verify")
  expect_no_match(rmd_str, "No fabrications detected")
})

test_that("Mode 1 with all-observational provocations does not falsely claim 'no fabrications'", {
  # Audit B test gap: a Mode 1 run that only used absent_voice or the
  # erased-terms branch of assumption_surfacing produces NULL-provenance
  # provocations only -- there are no verbatim claims to verify.
  log <- create_reflection_log()
  log$provocations[[1]] <- make_provocation(
    category = "absent_voice", theme_name = "T",
    reason = "no adolescent voices",
    provenance = NULL,
    extra = list(dimension = "demographic", description = "adolescents")
  )
  log$provocation_attempts <- data.frame(
    theme_name = "T", category = "absent_voice",
    n_emitted = 1L,
    attempted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  ts <- create_theme_set(list(list(id = 1L, name = "T", description = "",
                                      codes_included = "x")))
  data <- tibble::tibble(std_id = "e1", std_text = "a", std_author = "alice",
                           theme_membership_T = 1L)
  cov <- compute_mode1_coverage(log, ts, data,
                                  requested_categories = "absent_voice")
  stats <- compute_mode1_theme_stats(data, ts, log)
  prov_stats <- compute_provocation_provenance_stats(log)

  expect_equal(prov_stats$total, 0L)  # no verbatim claims
  s <- pakhom:::.build_mode1_executive_summary(log, ts, stats, cov, prov_stats)
  # The exec summary must say "No verbatim claims to verify"
  # (the truthful claim), not "No fabrications detected"
  # (which would be misleading -- there's nothing to fabricate from)
  expect_match(s, "No verbatim claims to verify")
  expect_no_match(s, "No fabrications detected")
})

test_that("special characters in theme name are HTML-escaped, not interpreted", {
  # Audit B test gap: XSS-like input in theme name. .html_esc should
  # neutralize <script> tags, &, quotes. Verifies all interpolation
  # paths in .build_mode1_provocation_section + .render_provocation_block.
  data <- tibble::tibble(
    std_id = "e1", std_text = "x", std_author = "alice",
    `theme_membership_X.script.alert.1...script.X` = 1L  # must match make.names
  )
  # Use a theme name that, when un-escaped, would be HTML markup
  evil_name <- "<script>alert(1)</script>"
  # make.names() will sanitize the column name; we use the make.names form
  safe_col <- paste0("theme_membership_", make.names(evil_name))
  data <- tibble::tibble(
    std_id = "e1", std_text = "x", std_author = "alice"
  )
  data[[safe_col]] <- 1L
  ts <- create_theme_set(list(list(id = 1L, name = evil_name,
                                      description = "& \"dangerous\"",
                                      codes_included = "x")))
  log <- create_reflection_log()
  log$provocation_attempts <- data.frame(
    theme_name = evil_name, category = "counter_narrative",
    n_emitted = 0L,
    attempted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  cov <- compute_mode1_coverage(log, ts, data,
                                  requested_categories = "counter_narrative")
  stats <- compute_mode1_theme_stats(data, ts, log)
  prov_stats <- compute_provocation_provenance_stats(log)

  rmd <- pakhom:::.build_mode1_rmd_content(
    data = data, theme_set = ts, reflection_log = log,
    coverage = cov, theme_stats = stats,
    config = list(methodology = list(mode = "reflexive_scaffold")),
    run_id = "test-xss", prov_stats = prov_stats
  )
  rmd_str <- paste(rmd, collapse = "\n")
  # Raw <script> tag should NOT appear in output (must be escaped)
  expect_no_match(rmd_str, "<script>alert\\(1\\)</script>")
  # Escaped form should be present (the escaping converts < to &lt; etc.)
  expect_match(rmd_str, "&lt;script&gt;")
})

test_that("schema-1.0.0 reflection_log (no skipped_themes slot) renders without crashing", {
  # Audit B test gap: a legacy 1.0.0 log loaded as a resume_log will
  # have skipped_themes / provocation_attempts as NULL. The Rmd builder
  # must short-circuit on the nrow check, not crash on nrow(NULL).
  log <- create_reflection_log()
  log$skipped_themes <- NULL  # simulate 1.0.0 shape
  log$provocation_attempts <- NULL
  ts <- create_theme_set(list(list(id = 1L, name = "T", description = "",
                                      codes_included = "x")))
  data <- tibble::tibble(std_id = "e1", std_text = "x", std_author = "alice",
                           theme_membership_T = 1L)
  cov <- compute_mode1_coverage(log, ts, data,
                                  requested_categories = "counter_narrative")
  stats <- compute_mode1_theme_stats(data, ts, log)
  prov_stats <- compute_provocation_provenance_stats(log)

  expect_no_error(
    rmd <- pakhom:::.build_mode1_rmd_content(
      data = data, theme_set = ts, reflection_log = log,
      coverage = cov, theme_stats = stats,
      config = list(methodology = list(mode = "reflexive_scaffold")),
      run_id = "test-schema-1-0-0", prov_stats = prov_stats
    )
  )
})

test_that("generate_mode1_report renders HTML to disk + writes Rmd", {
  skip_if_not(rmarkdown::pandoc_available() ||
                dir.exists("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"),
              "Requires pandoc for Rmd render")
  inp <- .build_minimal_mode1_inputs()
  out_dir <- withr::local_tempdir()
  out_file <- file.path(out_dir, "analysis_report.html")

  result <- generate_mode1_report(
    data = inp$data, theme_set = inp$theme_set,
    reflection_log = inp$reflection_log,
    coverage = inp$coverage, theme_stats = inp$theme_stats,
    config = list(methodology = list(mode = "reflexive_scaffold"),
                    output = list(generate_report = TRUE),
                    audit = list(capture_raw_responses = FALSE)),
    output_file = out_file
  )

  expect_equal(result, out_file)
  expect_true(file.exists(out_file))
  expect_true(file.exists(file.path(out_dir, "analysis_report.Rmd")))

  # CSS copied over
  expect_true(file.exists(file.path(out_dir, "styles.css")))

  # Sanity-check the rendered HTML contains the methodology declaration
  html <- paste(readLines(out_file, warn = FALSE), collapse = "\n")
  expect_match(html, "Reflexive Scaffold|reflexive_scaffold")
  expect_match(html, "Provocation Coverage|coverage-card")
})
