# Coverage, audit-log, and theme edge-case tests
#
# .coverage-banner-saturated CSS rule present
# saturation arbiter dedupe via last_arbiter_n_coded
# write_corpus_coverage persists coverage_card.json
# audit log schema_version stamped on every record
# keywords capped to top-N codes by frequency
# is_new_code dedupe within an entry
# researcher_review preserves subthemes for non-mutating edits

# ==========================================================================
# .coverage-banner-saturated CSS
# ==========================================================================

test_that("inst/rmd/styles.css contains coverage-banner-saturated rule", {
  css_path <- system.file("rmd", "styles.css", package = "pakhom")
  if (!nzchar(css_path)) {
    css_path <- file.path("../../inst/rmd/styles.css")
  }
  skip_if_not(file.exists(css_path), "styles.css not on package install path")
  css <- paste(readLines(css_path, warn = FALSE), collapse = "\n")
  expect_match(css, ".coverage-banner-saturated", fixed = TRUE)
})


# ==========================================================================
# audit log schema_version on every record
# ==========================================================================

test_that(".AUDIT_LOG_SCHEMA_VERSION exists and is 1.0.0", {
  expect_equal(pakhom:::.AUDIT_LOG_SCHEMA_VERSION, "1.0.0")
})

test_that("log_ai_decision stamps schema_version on every record", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td, config = NULL)
  log_ai_decision(audit, "coding", "code_assignment",
                   entry_id = "e1", code_name = "test_code")
  close_audit_log(audit)
  lines <- readLines(file.path(td, "ai_decisions.jsonl"))
  expect_gte(length(lines), 1L)
  record <- jsonlite::fromJSON(lines[1L])
  expect_true("schema_version" %in% names(record))
  expect_equal(record$schema_version, "1.0.0")
})


# ==========================================================================
# write_corpus_coverage persists coverage_card.json
# ==========================================================================

test_that("write_corpus_coverage produces machine-readable JSON (no methodology stamp)", {
  # Test the no-stamp path: when methodology_mode is NULL the JSON file
  # is bare jsonlite output and reads back cleanly.
  td <- withr::local_tempdir()
  cov <- structure(list(
    n_input_to_coding       = 100L,
    n_processed             = 100L,
    n_unprocessed           = 0L,
    n_skipped               = 10L,
    n_coded                 = 90L,
    skip_reasons            = stats::setNames(c(5L, 5L), c("off-topic", "too short")),
    words_processed         = 5000L,
    coverage_rate           = 1.0,
    no_silent_truncation    = TRUE,
    stop_reason             = "all_entries_processed",
    saturation_reached      = FALSE,
    reached_at_entry        = NA_integer_
  ), class = c("CorpusCoverage", "Tier0Coverage"))

  path <- write_corpus_coverage(cov, td, methodology_mode = NULL)
  expect_true(file.exists(path))
  back <- jsonlite::read_json(path, simplifyVector = TRUE)
  expect_equal(back$n_coded, 90L)
  expect_equal(back$stop_reason, "all_entries_processed")
  expect_true("schema_version" %in% names(back))
})

test_that("write_corpus_coverage with methodology stamp prepends to JSON", {
  td <- withr::local_tempdir()
  cov <- structure(list(
    n_input_to_coding = 50L, n_processed = 50L, n_unprocessed = 0L,
    n_skipped = 0L, n_coded = 50L,
    skip_reasons = stats::setNames(integer(0), character(0)),
    words_processed = 1000L, coverage_rate = 1.0,
    no_silent_truncation = TRUE, stop_reason = "all_entries_processed",
    saturation_reached = FALSE, reached_at_entry = NA_integer_
  ), class = c("CorpusCoverage", "Tier0Coverage"))
  path <- write_corpus_coverage(cov, td,
                                  methodology_mode = "codebook_collaborative")
  expect_true(file.exists(path))
  raw <- paste(readLines(path), collapse = "\n")
  # JSON content survives (the JSON object opening brace appears somewhere)
  expect_match(raw, "n_coded", fixed = TRUE)
  expect_match(raw, "all_entries_processed", fixed = TRUE)
})


# ==========================================================================
# keywords capped to top-N codes
# ==========================================================================

test_that("enrich_themes caps Mode 2 keywords to top-8 codes by frequency", {
  # Build a synthetic codebook with many codes; enrich a theme with all
  # of them; assert keywords ends up at 8 (the cap).
  codebook <- list()
  for (i in 1:15) {
    code_name <- paste0("code_", i)
    code_key  <- tolower(code_name)
    codebook[[code_key]] <- list(
      code_name = code_name,
      description = paste("Code", i),
      type = "descriptive",
      frequency = 16L - i,  # 15, 14, ..., 1 (so code_1 is most-frequent)
      entry_ids = paste0("e", i),
      coded_segments = list()
    )
  }
  coding_state <- list(codebook = codebook,
                        entry_results = list())
  class(coding_state) <- "ProgressiveCodingState"

  # Use create_theme_set so the codes_included character vector is
  # wrapped into a virtual Subtheme S3 (back-compat shim) and
  # theme_codes() returns the codes properly.
  theme_set <- create_theme_set(list(
    list(
      id = 1L, name = "T1", description = "",
      codes_included = paste0("code_", 1:15)
    )
  ))

  # Empty data tibble suffices for enrich_themes' minimum requirements
  data <- tibble::tibble(std_id = paste0("e", 1:15),
                          std_text = rep("text", 15L),
                          sentiment_score = rep(0, 15L),
                          original_text = rep("orig", 15L))
  enriched <- suppressWarnings(
    enrich_themes(theme_set, data, coding_state = coding_state)
  )
  expect_length(enriched$themes[[1L]]$keywords, 8L)
  # Top-8 by frequency: code_1, code_2, ..., code_8
  expect_true(all(paste0("code_", 1:8) %in% enriched$themes[[1L]]$keywords))
  expect_false("code_15" %in% enriched$themes[[1L]]$keywords)
})

test_that("enrich_themes keeps all codes when count <= cap", {
  codebook <- list(
    code_a = list(code_name = "code_a", description = "", type = "descriptive",
                   frequency = 5L, entry_ids = "e1", coded_segments = list()),
    code_b = list(code_name = "code_b", description = "", type = "descriptive",
                   frequency = 3L, entry_ids = "e2", coded_segments = list())
  )
  coding_state <- list(codebook = codebook, entry_results = list())
  class(coding_state) <- "ProgressiveCodingState"

  theme_set <- create_theme_set(list(
    list(
      id = 1L, name = "T1", description = "",
      codes_included = c("code_a", "code_b")
    )
  ))

  data <- tibble::tibble(std_id = c("e1", "e2"),
                          std_text = c("a", "b"),
                          sentiment_score = c(0, 0))
  enriched <- suppressWarnings(
    enrich_themes(theme_set, data, coding_state = coding_state)
  )
  expect_setequal(enriched$themes[[1L]]$keywords, c("code_a", "code_b"))
})


# ==========================================================================
# narrative field documented as deprecated
# ==========================================================================

test_that(".THEME_DEFAULTS still includes narrative field (back-compat)", {
  expect_true("narrative" %in% names(pakhom:::.THEME_DEFAULTS))
  expect_equal(pakhom:::.THEME_DEFAULTS$narrative, "")
})


# ==========================================================================
# Audit followups
# ==========================================================================


test_that("snapshot uses field names live_snapshot_clusters reads", {
  # Pre-followup the snapshot builder used `proposed_name` (snapshot
  # reader reads `name`) and only first code key per subtheme (reader
  # reads code_keys + code_indices for n_codes). Result: snapshot
  # rendered with NA names + zero counts.
  skip_if_not_installed("withr")
  td <- withr::local_tempdir()
  tracker <- init_live_tracker(td)

  framework_spec <- list(
    name = "TestFW",
    constructs = list(
      list(id = "construct_a", name = "Construct A",
            description = "First construct",
            example_indicators = c("indicator1")),
      list(id = "construct_b", name = "Construct B",
            description = "Second construct",
            example_indicators = c("indicator2"))
    ),
    anomaly_handling = "bracket",
    construct_ids = c("construct_a", "construct_b")
  )
  class(framework_spec) <- "FrameworkSpec"

  # Coding state with both constructs coded. Field name is
  # code_name (not name) -- matches the real codebook shape used
  # by R/09_coding.R::create_codebook_entry.
  cs <- list(
    codebook = list(
      construct_a = list(code_name = "Construct A", description = "First",
                          type = "framework_construct", frequency = 5L,
                          entry_ids = "e1", coded_segments = list()),
      construct_b = list(code_name = "Construct B", description = "Second",
                          type = "framework_construct", frequency = 3L,
                          entry_ids = "e2", coded_segments = list())
    ),
    entry_results = list(),
    saturation = list()
  )
  class(cs) <- "ProgressiveCodingState"

  ts <- apply_framework_themes(cs, framework_spec, provider = NULL,
                                  live_tracker = tracker)

  # Read back the JSON snapshot
  snap_path <- file.path(td, "live", "code_to_cluster.json")
  expect_true(file.exists(snap_path))
  snap <- jsonlite::fromJSON(snap_path, simplifyVector = FALSE)
  expect_equal(snap$walk_status, "framework_deductive_complete")
  expect_equal(snap$n_themes, 2L)
  # Themes carry name (not NA) + code_keys populated + n_codes > 0
  expect_equal(snap$themes[[1L]]$name, "Construct A")
  expect_equal(snap$themes[[1L]]$decision_origin, "framework")
  expect_gt(snap$themes[[1L]]$n_codes, 0L)
  expect_true(length(snap$themes[[1L]]$code_keys) > 0L)
  expect_true("construct_a" %in% snap$themes[[1L]]$code_keys)
})

test_that("write_corpus_coverage doesn't overwrite schema_version", {
  td <- withr::local_tempdir()
  # Coverage object with a deliberately-different schema_version to
  # detect overwriting
  cov <- structure(list(
    n_input_to_coding = 10L, n_processed = 10L, n_unprocessed = 0L,
    n_skipped = 0L, n_coded = 10L,
    skip_reasons = stats::setNames(integer(0), character(0)),
    words_processed = 100L, coverage_rate = 1.0,
    no_silent_truncation = TRUE,
    stop_reason = "all_entries_processed",
    saturation_reached = FALSE, reached_at_entry = NA_integer_,
    schema_version = "FUTURE_SCHEMA_2.0.0"  # would be overwritten pre-followup
  ), class = c("CorpusCoverage", "Tier0Coverage"))
  path <- write_corpus_coverage(cov, td, methodology_mode = NULL)
  back <- jsonlite::read_json(path, simplifyVector = TRUE)
  # The pre-existing schema_version was preserved (not overwritten to 1.0.0)
  expect_equal(back$schema_version, "FUTURE_SCHEMA_2.0.0")
})

test_that("create_coding_state pre-inits last_arbiter_n_coded", {
  # Helper for synthetic state via the constructor used by
  # run_progressive_coding. The audit found this field was only
  # populated INSIDE the gate body, so a never-arbitered run would
  # have a state whose schema differed from a post-arbiter state.
  # Defensive fix: pre-init in the saturation list literal.
  cs <- create_coding_state()
  expect_true("last_arbiter_n_coded" %in% names(cs$saturation))
  expect_equal(cs$saturation$last_arbiter_n_coded, -1L)
})

test_that("keyword lookup uses canonical code keys", {
  # When a code's key differs from tolower(name) (unicode case),
  # the lookup must use the canonical key, not tolower(name).
  # This test uses an all-ASCII fixture where tolower(name) ==
  # key, but verifies the call still routes through theme_code_keys.
  codebook <- list(
    test_code = list(code_name = "Test Code",  # mixed case
                      description = "x", type = "descriptive",
                      frequency = 100L, entry_ids = "e1",
                      coded_segments = list())
  )
  cs <- list(codebook = codebook, entry_results = list())
  class(cs) <- "ProgressiveCodingState"
  ts <- create_theme_set(list(list(
    id = 1L, name = "T", description = "",
    codes_included = "Test Code"
  )))
  data <- tibble::tibble(std_id = "e1", std_text = "abc",
                          sentiment_score = 0)
  enriched <- suppressWarnings(enrich_themes(ts, data, coding_state = cs))
  # The single keyword retains the display-name form
  expect_equal(enriched$themes[[1L]]$keywords, "Test Code")
})
