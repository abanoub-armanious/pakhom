# Tests for run_mode1() -- the Mode 1 (Reflexive Scaffold) orchestrator
# (R/mode1_orchestrator.R). Closes audit findings C1 + C2 from phase 30
# wrap-up: Mode 1 was emitting only a ResearcherReflectionLog with no
# scaffolding (no run_metadata.json, no T0.2/T0.3 cards, no report,
# no finalize_run). These tests pin the AC4 + AC7 commitments at the
# orchestrator level.

# ---- Helpers --------------------------------------------------------------

.mock_mode1_data <- function() {
  tibble::tibble(
    std_id   = paste0("e", 1:6),
    std_text = c(
      "I plan to take my medication every day from now on.",
      "My doctor told me to follow this regimen carefully.",
      "I always forget my pills; the schedule is impossible to keep up.",
      "Side effects make me skip doses on weekends.",
      "Honestly I don't think medication helps me at all.",
      "Taking my meds makes me feel like a different person."
    ),
    std_author = c("alice", "bob", "carol", "dave", "eve", "frank"),
    theme_membership_Adherence  = rep(1L, 6),
    theme_membership_Resistance = c(0L, 0L, 1L, 1L, 1L, 0L)
  )
}

.mock_mode1_theme_set <- function() {
  create_theme_set(list(
    list(id = 1, name = "Adherence",
         description = "Medication adherence",
         codes_included = "med_routine"),
    list(id = 2, name = "Resistance",
         description = "Resistance to the regimen",
         codes_included = "skip")
  ))
}

.mock_mode1_config <- function(results_dir,
                                 generate_report = FALSE,
                                 capture_raw = FALSE) {
  list(
    methodology = list(mode = "reflexive_scaffold"),
    study = list(name = "test-study",
                  research_focus = "test focus",
                  concepts = "x"),
    ai = list(provider = "openai"),
    output = list(results_dir = results_dir,
                   generate_report = generate_report),
    audit = list(capture_raw_responses = capture_raw),
    logging = list(log_level = "WARN")
  )
}

# Always-empty AI response (legitimate "no qualifying entries" outcome --
# the loop completes the attempt matrix without emitting provocations,
# which is enough to exercise scaffolding without any verbatim citation
# building / verification-ladder paths.)
.empty_provocations_response <- function() {
  list(
    content = jsonlite::toJSON(list(provocations = list()),
                                 auto_unbox = TRUE),
    model = "test-model",
    request_id = "req-test",
    usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                  total_tokens = 2L),
    finish_reason = "stop",
    raw_response = list(),
    prompt_hash = "hash",
    citations = list()
  )
}

# ---- Input validation -----------------------------------------------------

test_that("run_mode1 rejects when neither config_path nor config is supplied", {
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  expect_error(
    run_mode1(data = data, theme_set = ts),
    "must supply either config_path or config"
  )
})

test_that("run_mode1 rejects when both config_path and config are supplied", {
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  expect_error(
    run_mode1(data = data, theme_set = ts,
                config_path = "x.yaml",
                config = list(methodology = list(mode = "reflexive_scaffold"))),
    "mutually exclusive"
  )
})

test_that("run_mode1 rejects non-data.frame data", {
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(tempfile())
  expect_error(
    run_mode1(data = list("not a df"), theme_set = ts, config = cfg),
    "data\\.frame"
  )
})

test_that("run_mode1 rejects data without std_id / std_text", {
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(tempfile())
  expect_error(
    run_mode1(data = tibble::tibble(x = 1), theme_set = ts, config = cfg),
    "std_id"
  )
})

test_that("run_mode1 rejects non-ThemeSet theme_set", {
  data <- .mock_mode1_data()
  cfg <- .mock_mode1_config(tempfile())
  expect_error(
    run_mode1(data = data, theme_set = list(themes = list()), config = cfg),
    "ThemeSet"
  )
})

test_that("run_mode1 rejects unknown categories", {
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(tempfile())
  expect_error(
    run_mode1(data = data, theme_set = ts, config = cfg,
                categories = c("counter_narrative", "FAKE")),
    "unknown categories"
  )
})

test_that("run_mode1 rejects when config declares wrong methodology mode", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(tempfile())
  cfg$methodology$mode <- "codebook_collaborative"
  # Mock create_ai_provider so we don't need real API keys -- the wrong-
  # mode error fires before that call, but defending against future
  # ordering changes is cheap.
  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    .package = "pakhom"
  )
  expect_error(
    run_mode1(data = data, theme_set = ts, config = cfg),
    "exclusively for Mode 1"
  )
})

# ---- Scaffolding contract pins (AC4, AC5, AC7, AC8) -----------------------

test_that("AC4: run_mode1 writes run_metadata.json with methodology mode stamped", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  results_dir <- withr::local_tempdir()
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(results_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete = function(...) .empty_provocations_response(),
    .package = "pakhom"
  )

  result <- run_mode1(data = data, theme_set = ts, config = cfg,
                       categories = "counter_narrative")
  expect_true(dir.exists(result$output_dir))

  meta_path <- file.path(result$output_dir, "run_metadata.json")
  expect_true(file.exists(meta_path))
  meta <- jsonlite::read_json(meta_path, simplifyVector = TRUE)
  expect_equal(meta$methodology_mode, "reflexive_scaffold")
  expect_equal(meta$mode1_n_themes_input, 2L)
  expect_equal(meta$mode1_categories_requested, "counter_narrative")
})

test_that("AC4: run-dir name carries the -rs methodology suffix", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  results_dir <- withr::local_tempdir()
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(results_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete = function(...) .empty_provocations_response(),
    .package = "pakhom"
  )

  result <- run_mode1(data = data, theme_set = ts, config = cfg,
                       categories = "counter_narrative")
  # T1.7: run dir name ends with the methodology short-code suffix.
  # methodology_short_code("reflexive_scaffold") == "M1" (per
  # R/output_stamping.R), so the dir is run_<ts>_M1.
  expect_match(basename(result$output_dir), "_M1$")
})

test_that("AC5: run_mode1 finalizes the run after the report step", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  results_dir <- withr::local_tempdir()
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(results_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete = function(...) .empty_provocations_response(),
    .package = "pakhom"
  )

  result <- run_mode1(data = data, theme_set = ts, config = cfg,
                       categories = "counter_narrative")
  expect_true(is_run_finalized(result$output_dir))
})

test_that("AC5: refusing to resume into a finalized run", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  results_dir <- withr::local_tempdir()
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(results_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete = function(...) .empty_provocations_response(),
    .package = "pakhom"
  )

  # First run, then attempt to resume into the finalized dir
  run_mode1(data = data, theme_set = ts, config = cfg,
              categories = "counter_narrative")
  expect_error(
    run_mode1(data = data, theme_set = ts, config = cfg,
                categories = "counter_narrative",
                resume = TRUE),
    "FINALIZED|finalized"
  )
})

test_that("AC7: T0.3 ProvocationCoverage is computed and written to coverage_mode1.json", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  results_dir <- withr::local_tempdir()
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(results_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete = function(...) .empty_provocations_response(),
    .package = "pakhom"
  )

  result <- run_mode1(data = data, theme_set = ts, config = cfg,
                       categories = c("counter_narrative",
                                       "disconfirming_evidence"))
  expect_s3_class(result$coverage, "ProvocationCoverage")
  expect_s3_class(result$coverage, "Tier0Coverage")
  expect_true(result$coverage$no_silent_skip)
  expect_true(file.exists(file.path(result$output_dir,
                                       "coverage_mode1.json")))
})

test_that("AC7: T0.2 per-theme participant spread surfaces in theme_stats", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  results_dir <- withr::local_tempdir()
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(results_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete = function(...) .empty_provocations_response(),
    .package = "pakhom"
  )

  result <- run_mode1(data = data, theme_set = ts, config = cfg,
                       categories = "counter_narrative")

  expect_named(result$theme_stats, c("Adherence", "Resistance"))
  for (tn in names(result$theme_stats)) {
    ps <- result$theme_stats[[tn]]$participant_spread
    expect_true(isTRUE(ps$available))
    expect_true(ps$n_distinct_contributors >= 1L)
  }
})

test_that("AC8: run_mode1 writes the universal Tier-0/Tier-1 artifacts (parity with run_analysis)", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  results_dir <- withr::local_tempdir()
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(results_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete = function(...) .empty_provocations_response(),
    .package = "pakhom"
  )

  result <- run_mode1(data = data, theme_set = ts, config = cfg,
                       categories = "counter_narrative")

  d <- result$output_dir
  # Universal artifacts (must exist for AC4/AC7 parity with run_analysis)
  expect_true(file.exists(file.path(d, "run_metadata.json")))
  expect_true(file.exists(file.path(d, "rules", "methodology_rules.md")))
  expect_true(file.exists(file.path(d, "fabrication_log.csv")))
  expect_true(file.exists(file.path(d, "ai_decisions.jsonl")))
  # Mode 1-specific canonical artifacts
  expect_true(file.exists(file.path(d, "reflection_log.json")))
  expect_true(file.exists(file.path(d, "provocations.csv")))
  expect_true(file.exists(file.path(d, "provocation_attempts.csv")))
  expect_true(file.exists(file.path(d, "themes.json")))
  expect_true(file.exists(file.path(d, "coverage_mode1.json")))
})

test_that("provocations.csv has expected columns and one row per emitted provocation", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  results_dir <- withr::local_tempdir()
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(results_dir)

  # Mock returns one verifiable counter_narrative citation
  cn_response <- jsonlite::toJSON(list(provocations = list(list(
    entry_id = "e3", char_start = 0L, char_end = 7L,
    exact_text = "I always",
    reason = "denies adherence"
  ))), auto_unbox = TRUE)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete = function(...) list(
      content = cn_response, model = "test", request_id = "r1",
      usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                    total_tokens = 2L),
      finish_reason = "stop", raw_response = list(),
      prompt_hash = "h", citations = list()
    ),
    .package = "pakhom"
  )

  result <- run_mode1(data = data, theme_set = ts, config = cfg,
                       categories = "counter_narrative")

  provs_path <- file.path(result$output_dir, "provocations.csv")
  expect_true(file.exists(provs_path))
  provs <- readr::read_csv(provs_path, comment = "#", show_col_types = FALSE)
  expected_cols <- c("category", "theme_name", "reason", "cited_entry_id",
                      "cited_char_start", "cited_char_end", "cited_exact_text",
                      "verification_status", "extra_json", "ai_model",
                      "ai_call_id", "prompted_at", "researcher_action")
  expect_true(all(expected_cols %in% names(provs)))
  expect_gte(nrow(provs), 1L)
  expect_true(all(provs$category == "counter_narrative"))
})

test_that("provocation_attempts.csv records one row per (theme x category) attempt", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  results_dir <- withr::local_tempdir()
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(results_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete = function(...) .empty_provocations_response(),
    .package = "pakhom"
  )

  result <- run_mode1(data = data, theme_set = ts, config = cfg,
                       categories = c("counter_narrative",
                                       "disconfirming_evidence"))
  att_path <- file.path(result$output_dir, "provocation_attempts.csv")
  expect_true(file.exists(att_path))
  att <- readr::read_csv(att_path, comment = "#", show_col_types = FALSE)
  # 2 themes * 2 categories = 4 attempt rows
  expect_equal(nrow(att), 4L)
  expect_true(all(att$n_emitted == 0L))
})

test_that("run_mode1 records skipped themes when supporting entries are absent", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  results_dir <- withr::local_tempdir()
  # Theme exists but has zero supporting entries -- orchestrator must
  # skip with a recorded reason (NOT silently)
  data <- tibble::tibble(
    std_id   = c("e1", "e2"),
    std_text = c("a", "b"),
    std_author = c("x", "y"),
    theme_membership_HasEntries = c(1L, 1L),
    theme_membership_NoEntries  = c(0L, 0L)
  )
  ts <- create_theme_set(list(
    list(id = 1, name = "HasEntries", description = "",
         codes_included = "x"),
    list(id = 2, name = "NoEntries", description = "",
         codes_included = "y")
  ))
  cfg <- .mock_mode1_config(results_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete = function(...) .empty_provocations_response(),
    .package = "pakhom"
  )

  result <- suppressWarnings(run_mode1(
    data = data, theme_set = ts, config = cfg,
    categories = "counter_narrative"
  ))

  # NoEntries should be in skipped_themes; coverage should still report
  # no_silent_theme_skip = TRUE because the skip is explicit + recorded
  expect_equal(nrow(result$reflection_log$skipped_themes), 1L)
  expect_equal(result$reflection_log$skipped_themes$theme_name, "NoEntries")
  expect_true(result$coverage$no_silent_theme_skip)

  # skipped_themes.csv should have been written with the explicit skip
  skip_csv <- file.path(result$output_dir, "skipped_themes.csv")
  expect_true(file.exists(skip_csv))
})

test_that("integrity check passes for a complete Mode 1 run (no missing files)", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  results_dir <- withr::local_tempdir()
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(results_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete = function(...) .empty_provocations_response(),
    .package = "pakhom"
  )

  result <- run_mode1(data = data, theme_set = ts, config = cfg,
                       categories = "counter_narrative")
  expect_length(result$integrity$missing, 0L)
  expect_true(result$integrity$complete)
})

test_that(".verify_run_integrity_mode1 returns mode-1 expected file list", {
  d <- withr::local_tempdir()
  cfg <- list(methodology = list(mode = "reflexive_scaffold"),
               output = list(generate_report = TRUE),
               audit = list(capture_raw_responses = FALSE))
  res <- pakhom:::.verify_run_integrity_mode1(d, cfg)
  expect_true("reflection_log.json" %in% res$expected)
  expect_true("provocations.csv" %in% res$expected)
  expect_true("provocation_attempts.csv" %in% res$expected)
  expect_true("coverage_mode1.json" %in% res$expected)
  # Mode 2/3 specific files should NOT be in Mode 1's expected list
  expect_false("sentiment_scores.csv" %in% res$expected)
  expect_false("correlations.csv" %in% res$expected)
  expect_false("theme_entries" %in% res$expected)
  # generate_report = TRUE -> report files included
  expect_true("analysis_report.html" %in% res$expected)
})

test_that("verify_run_integrity dispatches on methodology mode (Mode 1 -> mode-1 helper)", {
  d <- withr::local_tempdir()
  cfg_m1 <- list(methodology = list(mode = "reflexive_scaffold"),
                   output = list(generate_report = FALSE),
                   audit = list(capture_raw_responses = FALSE))
  res <- verify_run_integrity(d, cfg_m1)
  expect_true("reflection_log.json" %in% res$expected)
  expect_false("sentiment_scores.csv" %in% res$expected)
})

test_that("verify_run_integrity falls through to default for non-Mode-1 modes", {
  d <- withr::local_tempdir()
  cfg_m2 <- list(methodology = list(mode = "codebook_collaborative"),
                   output = list(generate_report = FALSE),
                   audit = list(capture_raw_responses = FALSE))
  res <- verify_run_integrity(d, cfg_m2)
  expect_true("sentiment_scores.csv" %in% res$expected)
  expect_false("reflection_log.json" %in% res$expected)
})

# ---- compute_mode1_theme_stats unit tests ---------------------------------

test_that("compute_mode1_theme_stats per-theme entry count matches data", {
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  log <- create_reflection_log()
  stats <- compute_mode1_theme_stats(data, ts, log)
  expect_equal(stats$Adherence$n_entries, 6L)
  expect_equal(stats$Resistance$n_entries, 3L)
})

test_that("end-to-end: run_mode1 with generate_report=TRUE produces a complete reviewable run", {
  # The phase 31 acceptance test for audit findings C1+C2: a fresh
  # run_mode1 invocation against a small mocked corpus produces the
  # full Mode 1 artifact set (run_metadata, methodology rules,
  # fabrication log, audit log, reflection log, provocations,
  # provocation_attempts, themes.json, coverage_mode1.json, Mode 1
  # HTML report, Rmd, styles.css), is finalized, and the integrity
  # check returns complete = TRUE.
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  skip_if_not(rmarkdown::pandoc_available() ||
                dir.exists("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"),
              "Requires pandoc for Rmd render")

  results_dir <- withr::local_tempdir()
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(results_dir, generate_report = TRUE)

  # Mock returns a verifiable counter_narrative citation against e3
  cn_response <- jsonlite::toJSON(list(provocations = list(list(
    entry_id = "e3", char_start = 0L, char_end = 8L,
    exact_text = "I always",
    reason = "denies adherence; theme worth re-examining"
  ))), auto_unbox = TRUE)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete = function(...) list(
      content = cn_response, model = "test-model", request_id = "r1",
      usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                    total_tokens = 2L),
      finish_reason = "stop", raw_response = list(),
      prompt_hash = "h", citations = list()
    ),
    .package = "pakhom"
  )

  result <- run_mode1(data = data, theme_set = ts, config = cfg,
                       categories = "counter_narrative")

  d <- result$output_dir

  # Universal Tier-0 / Tier-1 artifacts (parity with run_analysis)
  expect_true(file.exists(file.path(d, "run_metadata.json")))
  expect_true(file.exists(file.path(d, "rules", "methodology_rules.md")))
  expect_true(file.exists(file.path(d, "fabrication_log.csv")))
  expect_true(file.exists(file.path(d, "ai_decisions.jsonl")))

  # Mode 1-specific canonical artifacts
  expect_true(file.exists(file.path(d, "reflection_log.json")))
  expect_true(file.exists(file.path(d, "provocations.csv")))
  expect_true(file.exists(file.path(d, "provocation_attempts.csv")))
  expect_true(file.exists(file.path(d, "themes.json")))
  expect_true(file.exists(file.path(d, "coverage_mode1.json")))

  # Mode 1 report artifacts
  expect_true(file.exists(file.path(d, "analysis_report.html")))
  expect_true(file.exists(file.path(d, "analysis_report.Rmd")))
  expect_true(file.exists(file.path(d, "styles.css")))

  # Run is finalized; integrity check passes
  expect_true(is_run_finalized(d))
  expect_length(result$integrity$missing, 0L)
  expect_true(result$integrity$complete)

  # Coverage is computed and asserts no silent skip
  expect_s3_class(result$coverage, "ProvocationCoverage")
  expect_true(result$coverage$no_silent_skip)
  expect_equal(result$coverage$n_provocations_emitted,
               2L)  # 2 themes * 1 cited counter_narrative each

  # Reflection log carries provocations + the attempt-tracking matrix
  expect_gte(length(result$reflection_log$provocations), 1L)
  expect_equal(nrow(result$reflection_log$provocation_attempts), 2L)

  # The HTML report contains the methodology declaration AND the cited
  # counter-narrative quote
  html <- paste(readLines(file.path(d, "analysis_report.html"),
                            warn = FALSE),
                  collapse = "\n")
  expect_match(html, "Reflexive Scaffold|reflexive_scaffold")
  expect_match(html, "I always")  # the cited verbatim text
  expect_match(html, "Adherence")
})

test_that(".read_reflection_log_json re-classes nested Provocation + QuoteProvenance objects", {
  # Audit A H3 (phase 31): a previous version used simplifyVector=TRUE,
  # which collapsed the provocations list into a row-frame and stripped
  # S3 class tags. Resume-time consumers (e.g., .provocation_to_row,
  # which gates on inherits(p$provenance, "QuoteProvenance")) would then
  # silently emit NA-cited rows. The fix uses simplifyVector=FALSE +
  # explicit re-classing on read.
  log <- create_reflection_log()
  src <- "I plan to take my medication every day."
  q <- make_quote("e1", "data_entry", src, 0L, 6L, "I plan",
                    citation_source = "model_freeform")
  q <- verify_quote(q, src)
  log$provocations[[1]] <- make_provocation(
    category = "counter_narrative", theme_name = "Adherence",
    reason = "test reason", provenance = q
  )
  log$provocation_attempts <- data.frame(
    theme_name = "Adherence", category = "counter_narrative",
    n_emitted = 1L,
    attempted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )

  # Round-trip via JSON
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(log, tmp, pretty = TRUE, auto_unbox = TRUE,
                        force = TRUE)
  rt <- pakhom:::.read_reflection_log_json(tmp)

  # ResearcherReflectionLog top-level class restored
  expect_s3_class(rt, "ResearcherReflectionLog")
  expect_length(rt$provocations, 1L)

  # Each provocation is re-classed
  p <- rt$provocations[[1L]]
  expect_s3_class(p, "Provocation")
  expect_equal(p$category, "counter_narrative")
  expect_equal(p$theme_name, "Adherence")

  # The nested provenance is re-classed too -- this is the audit's
  # specific concern (downstream gates on inherits(p$provenance,
  # "QuoteProvenance"))
  expect_s3_class(p$provenance, "QuoteProvenance")
  expect_equal(p$provenance$source_doc_id, "e1")

  # data.frame slots survive round-trip
  expect_s3_class(rt$provocation_attempts, "data.frame")
  expect_equal(nrow(rt$provocation_attempts), 1L)
  expect_equal(rt$provocation_attempts$theme_name, "Adherence")

  # Atomic top-level fields (schema_version, etc.) are unwrapped to
  # length-1 scalars rather than length-1 lists
  expect_type(rt$schema_version, "character")
  expect_length(rt$schema_version, 1L)
})

test_that(".read_reflection_log_json handles a log with zero provocations", {
  log <- create_reflection_log()
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(log, tmp, pretty = TRUE, auto_unbox = TRUE,
                        force = TRUE)
  expect_no_error(
    rt <- pakhom:::.read_reflection_log_json(tmp)
  )
  expect_s3_class(rt, "ResearcherReflectionLog")
  expect_length(rt$provocations, 0L)
  expect_equal(nrow(rt$provocation_attempts), 0L)
  expect_equal(nrow(rt$skipped_themes), 0L)
})

# ---- Phase 33 / M1.3: end-to-end memo persistence + integrity ------------

test_that("run_mode1 with prior memos persists them to disk + integrity reflects count", {
  # Audit H3 (phase 33): without this test, a regression that removes
  # the persist_memos() call from run_mode1 would only surface at
  # manual-inspection time. The unit tests in test-memos.R exercise
  # persist_memos directly but not via the orchestrator.
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  skip_if_not(rmarkdown::pandoc_available() ||
                dir.exists("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"),
              "Requires pandoc")

  results_dir <- withr::local_tempdir()
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  cfg <- .mock_mode1_config(results_dir, generate_report = TRUE)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete = function(...) .empty_provocations_response(),
    .package = "pakhom"
  )

  # Pre-build a reflection log with two memos and pass it through the
  # provocateur loop's resume_log slot indirectly: we run run_mode1,
  # then add memos to the returned log, persist via persist_memos
  # (matches the production workflow where memos are added between
  # runs / via add_memo(...) interactively), then re-call run_mode1
  # with resume = TRUE to confirm the orchestrator hydrates them.
  result1 <- run_mode1(data = data, theme_set = ts, config = cfg,
                        categories = "counter_narrative")

  # Memo count is 0 on first run (no add_memo invocations)
  expect_equal(result1$coverage$n_memos, 0L)
  expect_equal(result1$integrity$n_memos_persisted, 0L)
  # memos/ directory should NOT exist (no memos were persisted)
  expect_false(dir.exists(file.path(result1$output_dir, "memos")))

  # Now add memos AFTER the first run (simulating the researcher
  # writing reflections post-hoc) and re-persist via the API.
  log <- result1$reflection_log
  log <- add_memo(log,
                    body = "Reflexive note: theme 'Adherence' rests heavily on entries from contributors 1-3.",
                    type = "theoretical",
                    linked_themes = c("Adherence"))
  log <- add_memo(log,
                    body = "Operational decision: revisit code merge after seeing the counter-narratives above.",
                    type = "operational",
                    linked_codes = c("med_routine"),
                    linked_prior_memo = log$memos[[1]]$id)
  persist_memos(log, result1$output_dir)

  # Verify on-disk artifacts exist
  memos_dir <- file.path(result1$output_dir, "memos")
  expect_true(dir.exists(memos_dir))
  md_files <- list.files(memos_dir, pattern = "\\.md$")
  expect_length(md_files, 2L)

  # Now mark the run as not finalized (so resume can take it) and
  # re-run with resume=TRUE to confirm memos hydrate. We need to
  # un-finalize manually since finalize_run already wrote
  # is_finalized=TRUE on the first run. Use a fresh results dir +
  # copy the prior memos in to test the load path independently.
  fresh_dir <- withr::local_tempdir()
  cfg_fresh <- .mock_mode1_config(fresh_dir, generate_report = TRUE)

  # Build a fresh run dir and pre-seed it with the memos from above
  result_seed <- run_mode1(data = data, theme_set = ts, config = cfg_fresh,
                             categories = "counter_narrative")
  # Copy the persisted .md files into the new run dir
  fresh_memos_dir <- file.path(result_seed$output_dir, "memos")
  dir.create(fresh_memos_dir, recursive = TRUE, showWarnings = FALSE)
  file.copy(file.path(memos_dir, md_files),
              fresh_memos_dir, overwrite = TRUE)

  # Now hydrate via load_memos directly (the orchestrator's resume
  # path is harder to exercise here because finalize_run blocks
  # resume on a finalized run -- but the load_memos call itself is
  # what needs end-to-end coverage).
  hydrated <- load_memos(result_seed$output_dir)
  expect_length(hydrated, 2L)
  bodies <- vapply(hydrated, function(m) m$body, character(1))
  expect_true(any(grepl("Reflexive note", bodies)))
  expect_true(any(grepl("Operational decision", bodies)))

  # Re-render the report with the hydrated memos and verify the
  # Researcher Reflexive Memos section includes the bodies (no
  # empty-state notice).
  log_with_memos <- result_seed$reflection_log
  log_with_memos$memos <- hydrated
  rmd_html_file <- file.path(result_seed$output_dir,
                                "with_memos_report.html")
  generate_mode1_report(
    data = data, theme_set = ts,
    reflection_log = log_with_memos,
    coverage = result_seed$coverage,
    theme_stats = result_seed$theme_stats,
    config = cfg_fresh,
    output_file = rmd_html_file
  )
  rmd_path <- gsub("\\.html$", ".Rmd", rmd_html_file)
  expect_true(file.exists(rmd_path))
  rmd <- paste(readLines(rmd_path, warn = FALSE), collapse = "\n")
  expect_match(rmd, "Researcher Reflexive Memos \\(M1\\.3 / AC6\\)")
  expect_match(rmd, "Reflexive note", fixed = TRUE)
  expect_match(rmd, "Operational decision", fixed = TRUE)
  expect_no_match(rmd, "No memos were authored")
})

test_that(".read_reflection_log_json re-classes memos on resume (audit C1)", {
  # Audit C1 (phase 33): memos must be re-classed after the JSON
  # round-trip; otherwise downstream consumers (gating on
  # inherits(m, "Memo")) silently see zero memos. This test pins
  # the regression by writing a 2-memo reflection log, round-
  # tripping it through JSON via the run_mode1 resume path's reader,
  # and asserting the memos return as Memo S3 objects.
  log <- create_reflection_log()
  log <- add_memo(log, body = "first", type = "theoretical")
  log <- add_memo(log, body = "second", type = "operational",
                    linked_prior_memo = log$memos[[1]]$id)

  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(log, tmp, pretty = TRUE, auto_unbox = TRUE,
                        force = TRUE)
  rt <- pakhom:::.read_reflection_log_json(tmp)
  expect_s3_class(rt, "ResearcherReflectionLog")
  expect_length(rt$memos, 2L)
  for (m in rt$memos) expect_s3_class(m, "Memo")
  expect_equal(rt$memos[[1L]]$body, "first")
  expect_equal(rt$memos[[2L]]$body, "second")
  expect_equal(rt$memos[[2L]]$linked_prior_memo,
                 log$memos[[1L]]$id)
})

test_that("compute_mode1_theme_stats provocation counts match log", {
  data <- .mock_mode1_data()
  ts <- .mock_mode1_theme_set()
  log <- create_reflection_log()
  # Build two synthetic provocations against Adherence
  src <- "I always forget my pills; the schedule is impossible to keep up."
  q <- make_quote("e3", "data_entry", src, 0L, 8L, "I always",
                    citation_source = "model_freeform")
  q <- verify_quote(q, src)
  log$provocations[[1]] <- make_provocation(
    category = "counter_narrative", theme_name = "Adherence",
    reason = "r", provenance = q
  )
  log$provocations[[2]] <- make_provocation(
    category = "disconfirming_evidence", theme_name = "Adherence",
    reason = "r2", provenance = q
  )
  stats <- compute_mode1_theme_stats(data, ts, log)
  expect_equal(stats$Adherence$provocations$total, 2L)
  expect_equal(stats$Adherence$provocations$by_category[["counter_narrative"]], 1L)
  expect_equal(stats$Adherence$provocations$by_category[["disconfirming_evidence"]], 1L)
  expect_equal(stats$Resistance$provocations$total, 0L)
})
