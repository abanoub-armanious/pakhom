# Tests for the OS.6 transparency report bundler (Stage 1A / Phase 58 followup)

# --------------------------------------------------------------------------
# Helpers: build a synthetic run directory with the minimal artifacts the
# bundler reads. We don't run the full pipeline -- the bundler reads from
# disk only, so static fixtures suffice.
# --------------------------------------------------------------------------

.make_synthetic_run <- function(run_dir,
                                  with_coverage = TRUE,
                                  with_audit = TRUE,
                                  with_fab = TRUE,
                                  with_themes = TRUE) {
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  init_run_state(
    run_dir = run_dir,
    run_id = basename(run_dir),
    methodology_mode = "codebook_collaborative",
    study = list(
      researcher_positionality = "Test researcher",
      research_paradigm = "critical realist",
      reflexive_notes = "I have prior experience with binge eating data."
    )
  )

  if (with_coverage) {
    cov <- structure(list(
      n_input_to_coding = 250L, n_processed = 250L,
      n_unprocessed = 0L, n_skipped = 20L, n_coded = 230L,
      skip_reasons = stats::setNames(c(10L, 10L),
                                       c("off-topic", "too short")),
      words_processed = 12000L, coverage_rate = 1.0,
      no_silent_truncation = TRUE,
      stop_reason = "all_entries_processed",
      saturation_reached = FALSE, reached_at_entry = NA_integer_,
      schema_version = "1.0.0"
    ), class = c("CorpusCoverage", "Tier0Coverage"))
    write_corpus_coverage(cov, run_dir, methodology_mode = NULL)
  }

  if (with_audit) {
    audit <- init_audit_log(run_dir, config = list(
      methodology = list(mode = "codebook_collaborative")
    ))
    for (i in 1:5) {
      log_ai_decision(audit, "coding", "code_assignment",
                       entry_id = paste0("e", i),
                       code_name = "test_code")
    }
    log_ai_decision(audit, "coding", "new_code_created",
                     code_name = "test_code")
    close_audit_log(audit)
  }

  if (with_fab) {
    flog <- init_fabrication_log(run_dir)
    fab_q <- make_quote("d1", "test", "Source text here.",
                         0L, 5L, "missing")
    fab_q <- verify_quote(fab_q, "Source text here.", provider = NULL)
    log_fabrication(flog, fab_q)
  }

  if (with_themes) {
    themes_json <- list(
      list(id = 1L, name = "Theme A", description = "A demo theme",
            theme_kind = "framework", entry_count = 100L),
      list(id = 2L, name = "Theme B", description = "Another",
            theme_kind = "framework", entry_count = 50L)
    )
    jsonlite::write_json(themes_json, file.path(run_dir, "themes.json"),
                          pretty = TRUE, auto_unbox = TRUE,
                          null = "null", force = TRUE)
  }
  invisible(run_dir)
}

# --------------------------------------------------------------------------
# Core tests
# --------------------------------------------------------------------------

test_that("bundle_transparency_report produces HTML + JSON output", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  .make_synthetic_run(run_dir)

  result <- bundle_transparency_report(run_dir)
  expect_true(file.exists(result$html_path))
  expect_true(file.exists(result$json_path))
  expect_match(result$html_path, "transparency_report\\.html$")
  expect_match(result$json_path, "transparency_report\\.json$")
})

test_that("bundle_transparency_report report_data has required sections", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  .make_synthetic_run(run_dir)

  result <- bundle_transparency_report(run_dir)
  rd <- result$report_data
  expect_true(all(c("schema_version", "run_metadata", "reflexivity",
                     "quote_provenance_summary", "corpus_coverage",
                     "audit_log_summary", "theme_set_summary",
                     "lincoln_guba_mapping") %in% names(rd)))
  expect_equal(rd$schema_version, "1.0.0")
})

test_that("Lincoln & Guba mapping has all four trustworthiness criteria", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  .make_synthetic_run(run_dir)

  result <- bundle_transparency_report(run_dir)
  lg <- result$report_data$lincoln_guba_mapping
  expect_true(all(c("credibility", "dependability", "confirmability",
                     "transferability") %in% names(lg)))
  # Each carries a non-empty mechanisms list + run_evidence
  for (key in names(lg)) {
    expect_true(length(lg[[key]]$pakhom_mechanisms) > 0L)
    expect_true(!is.null(lg[[key]]$run_evidence))
  }
})

test_that("reflexivity completeness score reflects filled fields", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  .make_synthetic_run(run_dir)

  result <- bundle_transparency_report(run_dir)
  refl <- result$report_data$reflexivity
  # The synthetic fixture filled all 3 reflexivity fields
  expect_equal(refl$completeness$score, 3L)
  expect_equal(refl$completeness$max_score, 3L)
  expect_equal(length(refl$completeness$missing_fields), 0L)
})

test_that("quote_provenance_summary picks up fabrication log counts", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  .make_synthetic_run(run_dir)

  result <- bundle_transparency_report(run_dir)
  qp <- result$report_data$quote_provenance_summary
  expect_true(qp$available)
  # The synthetic fixture logged 1 fabrication
  expect_equal(qp$n_fabrications_caught, 1L)
})

test_that("corpus_coverage section reads coverage_card.json", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  .make_synthetic_run(run_dir)

  result <- bundle_transparency_report(run_dir)
  cov <- result$report_data$corpus_coverage
  expect_true(cov$available)
  expect_equal(cov$n_coded, 230L)
  expect_equal(cov$stop_reason, "all_entries_processed")
})

test_that("audit log summary reports total + decision-type breakdown", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  .make_synthetic_run(run_dir)

  result <- bundle_transparency_report(run_dir)
  audit <- result$report_data$audit_log_summary
  expect_true(audit$available)
  expect_equal(audit$total, 6L)
  expect_true("code_assignment" %in% names(audit$by_decision_type))
})

test_that("theme set summary reports n_themes + top themes", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  .make_synthetic_run(run_dir)

  result <- bundle_transparency_report(run_dir)
  themes <- result$report_data$theme_set_summary
  expect_true(themes$available)
  expect_equal(themes$n_themes, 2L)
  expect_equal(themes$total_entries, 150L)
})

# --------------------------------------------------------------------------
# Graceful degradation tests
# --------------------------------------------------------------------------

test_that("bundle_transparency_report degrades gracefully when artifacts missing", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  .make_synthetic_run(run_dir,
                       with_coverage = FALSE,
                       with_audit = FALSE,
                       with_fab = FALSE,
                       with_themes = FALSE)

  result <- bundle_transparency_report(run_dir)
  expect_true(file.exists(result$html_path))
  rd <- result$report_data
  expect_false(rd$corpus_coverage$available)
  expect_false(rd$audit_log_summary$available)
  expect_false(rd$theme_set_summary$available)
})

test_that("bundle_transparency_report stops on nonexistent run_dir", {
  expect_error(
    bundle_transparency_report("/nonexistent/path/that/should/not/exist"),
    "does not exist"
  )
})

test_that("HTML output contains expected section headers", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  .make_synthetic_run(run_dir)

  result <- bundle_transparency_report(run_dir)
  html <- paste(readLines(result$html_path), collapse = "\n")
  expect_match(html, "Methodological Transparency Report", fixed = TRUE)
  expect_match(html, "Reflexivity scaffold", fixed = TRUE)
  expect_match(html, "Lincoln &amp; Guba", fixed = TRUE)
  expect_match(html, "T0.1: Quote provenance", fixed = TRUE)
  expect_match(html, "T0.3: Corpus coverage funnel", fixed = TRUE)
  expect_match(html, "AC9: Audit log summary", fixed = TRUE)
  expect_match(html, "Theme set summary", fixed = TRUE)
})

test_that("output_path override produces report at custom location", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  .make_synthetic_run(run_dir)
  custom_path <- file.path(td, "custom_transparency.html")

  result <- bundle_transparency_report(run_dir, output_path = custom_path)
  expect_equal(normalizePath(result$html_path),
                normalizePath(custom_path))
  expect_true(file.exists(custom_path))
})

test_that(".TRANSPARENCY_REPORT_SCHEMA_VERSION constant exists", {
  expect_equal(pakhom:::.TRANSPARENCY_REPORT_SCHEMA_VERSION, "1.0.0")
})


# --------------------------------------------------------------------------
# Audit followup C-1: stamped envelope unwrap
# --------------------------------------------------------------------------

test_that("audit followup C-1: bundler unwraps stamped coverage_card.json", {
  # The pre-followup bundler read coverage_card.json directly and
  # missed that write_corpus_coverage wraps the payload in
  # {_methodology_stamp, _payload} when methodology_mode is non-null.
  # Result: cov$n_processed / cov$stop_reason / etc. were all NULL
  # on real runs.
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  dir.create(run_dir, recursive = TRUE)

  init_run_state(run_dir, basename(run_dir),
                  methodology_mode = "codebook_collaborative")

  # Write STAMPED coverage_card.json (real-world path)
  cov <- structure(list(
    n_input_to_coding = 100L, n_processed = 100L,
    n_unprocessed = 0L, n_skipped = 10L, n_coded = 90L,
    skip_reasons = stats::setNames(integer(0), character(0)),
    words_processed = 5000L, coverage_rate = 1.0,
    no_silent_truncation = TRUE,
    stop_reason = "all_entries_processed",
    saturation_reached = FALSE, reached_at_entry = NA_integer_,
    schema_version = "1.0.0"
  ), class = c("CorpusCoverage", "Tier0Coverage"))
  # Pass methodology_mode so the file gets stamped
  write_corpus_coverage(cov, run_dir,
                          methodology_mode = "codebook_collaborative")

  result <- bundle_transparency_report(run_dir)
  cov_summary <- result$report_data$corpus_coverage
  expect_true(cov_summary$available)
  # Pre-followup these would be NULL because they lived under _payload
  expect_equal(cov_summary$n_coded, 90L)
  expect_equal(cov_summary$stop_reason, "all_entries_processed")
})

test_that("audit followup C-1: bundler unwraps stamped themes.json", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  dir.create(run_dir, recursive = TRUE)

  init_run_state(run_dir, basename(run_dir),
                  methodology_mode = "codebook_collaborative")

  # Write themes.json then stamp it (mimics R/17_report.R real path)
  themes <- list(
    list(id = 1L, name = "Stamped Theme", description = "",
          theme_kind = "framework", entry_count = 42L)
  )
  themes_path <- file.path(run_dir, "themes.json")
  jsonlite::write_json(themes, themes_path, pretty = TRUE,
                        auto_unbox = TRUE, null = "null", force = TRUE)
  stamp_methodology_json(themes_path, "codebook_collaborative",
                          run_id = basename(run_dir))

  result <- bundle_transparency_report(run_dir)
  th <- result$report_data$theme_set_summary
  expect_true(th$available)
  expect_equal(th$n_themes, 1L)
  expect_equal(th$top_themes[[1L]]$name, "Stamped Theme")
})


# --------------------------------------------------------------------------
# Audit followup M-6: malformed artifacts + XSS + parent_run_id
# --------------------------------------------------------------------------

test_that("malformed coverage_card.json triggers graceful degradation", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  .make_synthetic_run(run_dir,
                       with_coverage = FALSE,
                       with_audit = TRUE,
                       with_fab = TRUE,
                       with_themes = TRUE)
  # Write a garbage coverage_card.json
  writeLines("{ not valid json: ", file.path(run_dir, "coverage_card.json"))

  result <- bundle_transparency_report(run_dir)
  cov_summary <- result$report_data$corpus_coverage
  # The malformed file is tryCatch'd; available falls to FALSE
  expect_false(cov_summary$available)
})

test_that("XSS attempt in reflexive_notes is HTML-escaped", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  dir.create(run_dir, recursive = TRUE)
  init_run_state(run_dir, basename(run_dir),
                  methodology_mode = "codebook_collaborative",
                  study = list(
                    researcher_positionality = "<script>alert(1)</script>",
                    research_paradigm = "critical realist",
                    reflexive_notes = "Researcher \"loves\" emoji & code"
                  ))
  result <- bundle_transparency_report(run_dir)
  html <- paste(readLines(result$html_path), collapse = "\n")
  # The <script> is escaped to &lt;script&gt; so no injection
  expect_false(grepl("<script>alert", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;alert", fixed = TRUE)
  # Quotes + ampersand also escaped
  expect_false(grepl("loves\" emoji", html, fixed = TRUE))
  expect_match(html, "&quot;", fixed = TRUE)
})

test_that("parent_run_id mode-fork case is rendered", {
  td <- withr::local_tempdir()
  run_dir <- file.path(td, "run_test")
  dir.create(run_dir, recursive = TRUE)
  init_run_state(run_dir, basename(run_dir),
                  methodology_mode = "codebook_collaborative",
                  parent_run_id = "run_2026-05-01_120000",
                  mode_changed_from = "reflexive_scaffold")
  result <- bundle_transparency_report(run_dir)
  html <- paste(readLines(result$html_path), collapse = "\n")
  expect_match(html, "Parent run", fixed = TRUE)
  expect_match(html, "run_2026-05-01_120000", fixed = TRUE)
  expect_match(html, "reflexive_scaffold", fixed = TRUE)
})

test_that(".tr_unwrap_payload handles both wrapped and unwrapped inputs", {
  wrapped <- list(`_methodology_stamp` = list(mode = "x"),
                   `_payload` = list(actual_field = "value"))
  unwrapped <- list(actual_field = "value")
  expect_equal(pakhom:::.tr_unwrap_payload(wrapped),
                list(actual_field = "value"))
  expect_equal(pakhom:::.tr_unwrap_payload(unwrapped), unwrapped)
  # NULL pass-through
  expect_null(pakhom:::.tr_unwrap_payload(NULL))
})
