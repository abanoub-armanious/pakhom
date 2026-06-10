# Tests for the Mode 1 (Reflexive Scaffold) T0.3 coverage computation
# (R/mode1_orchestrator.R::compute_mode1_coverage). The Mode 1 analog of
# compute_corpus_coverage asserts "no silent skip across themes x
# provocation categories" rather than "no silent truncation in the LLM
# call path." Distinguishing legitimate empty results (a category that
# returned zero provocations because no qualifying entries exist) from
# silent skips (a category that was never attempted) is the central
# semantic; these tests pin that.

# ---- Helpers --------------------------------------------------------------

.mock_theme_set <- function(names = c("Adherence", "Resistance")) {
  themes <- lapply(seq_along(names), function(i) {
    list(id = i, name = names[i], description = "", codes_included = "x")
  })
  create_theme_set(themes)
}

.mock_corpus <- function(n = 5L) {
  tibble::tibble(
    std_id   = paste0("e", seq_len(n)),
    std_text = paste("entry", seq_len(n))
  )
}

.populate_attempts <- function(log, theme_names,
                                  categories = .VALID_PROVOCATION_CATEGORIES,
                                  n_emitted_by_pos = NULL) {
  rows_per_theme <- length(categories)
  total_rows <- length(theme_names) * rows_per_theme
  if (is.null(n_emitted_by_pos)) {
    n_emitted_by_pos <- rep(1L, total_rows)
  }
  log$provocation_attempts <- data.frame(
    theme_name   = rep(theme_names, each = rows_per_theme),
    category     = rep(categories, length(theme_names)),
    n_emitted    = as.integer(n_emitted_by_pos),
    attempted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  log$provocations <- replicate(sum(n_emitted_by_pos),
                                  list(category = "x", theme_name = "y"),
                                  simplify = FALSE)
  log
}

# ---- Input validation ------------------------------------------------------

test_that("compute_mode1_coverage rejects non-ResearcherReflectionLog input", {
  ts <- .mock_theme_set()
  data <- .mock_corpus()
  expect_error(
    compute_mode1_coverage(list(some = "thing"), ts, data),
    "ResearcherReflectionLog"
  )
})

test_that("compute_mode1_coverage rejects non-ThemeSet input", {
  log <- create_reflection_log()
  data <- .mock_corpus()
  expect_error(
    compute_mode1_coverage(log, list(themes = list()), data),
    "ThemeSet"
  )
})

test_that("compute_mode1_coverage rejects non-data.frame data", {
  log <- create_reflection_log()
  ts <- .mock_theme_set()
  expect_error(
    compute_mode1_coverage(log, ts, list("not a df")),
    "data\\.frame"
  )
})

test_that("compute_mode1_coverage rejects unknown requested_categories", {
  log <- create_reflection_log()
  ts <- .mock_theme_set()
  data <- .mock_corpus()
  expect_error(
    compute_mode1_coverage(log, ts, data,
                            requested_categories = c("counter_narrative",
                                                      "FAKE")),
    "Unknown requested_categories"
  )
})

# ---- Output shape ----------------------------------------------------------

test_that("compute_mode1_coverage returns ProvocationCoverage + Tier0Coverage", {
  log <- create_reflection_log()
  ts <- .mock_theme_set()
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  expect_s3_class(cov, "ProvocationCoverage")
  expect_s3_class(cov, "Tier0Coverage")
  expect_equal(cov$mode, "reflexive_scaffold")
  # Schema 2.2.0: counter-evidence candidate sampling (bounded,
  # deterministic non-theme sample in the prompts) + the
  # n_candidate_entries_prompt_cap informational field. 2.1.0 added
  # n_memos + memos_by_type; 2.0.0: named-list serialization, partition
  # in/out-of-scope attempts, degenerate-state gating, downgraded
  # corpus-truncation claim.
  expect_equal(cov$schema_version, "2.2.0")
})

test_that("compute_mode1_coverage records every documented field", {
  log <- create_reflection_log()
  ts <- .mock_theme_set()
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  expected <- c(
    "mode", "n_themes_input", "n_themes_attempted",
    "n_themes_explicit_skip", "explicit_skip_reasons",
    "n_themes_silently_skipped", "silently_skipped_theme_names",
    "categories_requested", "n_categories_requested",
    "n_attempts_expected", "n_attempts_recorded",
    # Audit C H1: new fields surfacing unexpected-category drift
    "n_unexpected_category_attempts", "unexpected_categories",
    "attempts_complete", "attempts_per_category",
    "n_provocations_emitted", "n_attempts_with_zero_emit",
    "n_attempts_with_emit", "n_corpus_entries_searchable",
    # Audit C M3: replaced no_silent_corpus_truncation with two
    # honest fields + retained the headline boolean
    "corpus_provided_to_per_category_fns",
    "llm_prompt_includes_full_corpus",
    # 2.2.0: candidate-sample cap for the counter-evidence prompts
    "n_candidate_entries_prompt_cap",
    # M1.3: informational researcher-memo counts
    "n_memos", "memos_by_type",
    "no_silent_theme_skip", "no_unexpected_category_attempts",
    "no_silent_skip",
    "computed_at", "schema_version"
  )
  expect_setequal(names(cov), expected)
})

# ---- Headline assertions ---------------------------------------------------

test_that("happy path: all themes attempted across all categories -> no_silent_skip = TRUE", {
  log <- create_reflection_log()
  log <- .populate_attempts(log, c("Adherence", "Resistance"))
  ts <- .mock_theme_set(c("Adherence", "Resistance"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  expect_true(cov$no_silent_theme_skip)
  expect_true(cov$attempts_complete)
  expect_true(cov$no_silent_skip)
  expect_equal(cov$n_themes_attempted, 2L)
  expect_equal(cov$n_themes_silently_skipped, 0L)
})

test_that("legitimate empty: attempt with n_emitted=0 is NOT a silent skip", {
  log <- create_reflection_log()
  # All attempts present, all returned zero provocations (e.g.,
  # counter_narrative finds no qualifying entries) -- valid analytic
  # outcome, NOT a coverage failure.
  log <- .populate_attempts(log, c("Adherence", "Resistance"),
                              n_emitted_by_pos = rep(0L, 10L))
  ts <- .mock_theme_set(c("Adherence", "Resistance"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  expect_true(cov$no_silent_skip)  # headline still TRUE
  expect_equal(cov$n_attempts_with_emit, 0L)
  expect_equal(cov$n_attempts_with_zero_emit, 10L)
  expect_equal(cov$n_provocations_emitted, 0L)
})

test_that("silent theme skip: theme exists in theme_set but no attempts recorded", {
  log <- create_reflection_log()
  # Only Adherence got attempts; Resistance was silently skipped
  log <- .populate_attempts(log, c("Adherence"))
  ts <- .mock_theme_set(c("Adherence", "Resistance"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  expect_false(cov$no_silent_theme_skip)
  expect_false(cov$no_silent_skip)
  expect_equal(cov$n_themes_silently_skipped, 1L)
  expect_equal(cov$silently_skipped_theme_names, "Resistance")
})

test_that("explicit skip with reason is NOT a silent skip", {
  log <- create_reflection_log()
  log <- .populate_attempts(log, c("Adherence"))
  # Resistance was explicitly skipped (e.g., zero supporting entries)
  log$skipped_themes <- data.frame(
    theme_name = "Resistance",
    reason     = "no_supporting_entries",
    skipped_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  ts <- .mock_theme_set(c("Adherence", "Resistance"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  expect_true(cov$no_silent_theme_skip)
  expect_equal(cov$n_themes_silently_skipped, 0L)
  expect_equal(cov$n_themes_explicit_skip, 1L)
  expect_equal(cov$explicit_skip_reasons[["no_supporting_entries"]], 1L)
})

test_that("attempts_complete = FALSE when attempt matrix is partial", {
  log <- create_reflection_log()
  # Adherence got only 3 of 5 categories (mid-loop crash scenario)
  log$provocation_attempts <- data.frame(
    theme_name = "Adherence",
    category   = c("counter_narrative", "absent_voice",
                   "alternative_interpretation"),
    n_emitted  = c(1L, 0L, 1L),
    attempted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  ts <- .mock_theme_set(c("Adherence"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  expect_equal(cov$n_attempts_expected, 5L)
  expect_equal(cov$n_attempts_recorded, 3L)
  expect_false(cov$attempts_complete)
  expect_false(cov$no_silent_skip)
})

test_that("requested_categories subset reduces expected attempts", {
  log <- create_reflection_log()
  # Only counter_narrative + disconfirming_evidence requested; both
  # attempted on each theme
  log$provocation_attempts <- data.frame(
    theme_name = rep(c("Adherence", "Resistance"), each = 2L),
    category   = rep(c("counter_narrative", "disconfirming_evidence"), 2L),
    n_emitted  = c(1L, 0L, 2L, 1L),
    attempted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  log$provocations <- replicate(4, list(category = "x", theme_name = "y"),
                                 simplify = FALSE)
  ts <- .mock_theme_set(c("Adherence", "Resistance"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(
    log, ts, data,
    requested_categories = c("counter_narrative", "disconfirming_evidence")
  )
  expect_equal(cov$n_categories_requested, 2L)
  expect_equal(cov$n_attempts_expected, 4L)
  expect_equal(cov$n_attempts_recorded, 4L)
  expect_true(cov$attempts_complete)
  expect_true(cov$no_silent_skip)
})

test_that("attempts_per_category surfaces category-level counts", {
  log <- create_reflection_log()
  log <- .populate_attempts(log, c("Adherence", "Resistance"))
  ts <- .mock_theme_set(c("Adherence", "Resistance"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  expect_equal(length(cov$attempts_per_category),
               length(.VALID_PROVOCATION_CATEGORIES))
  # 2 themes * 1 attempt per category = 2 each
  expect_true(all(cov$attempts_per_category == 2L))
})

# ---- Backward compatibility with schema 1.0.0 logs -------------------------

test_that("compute_mode1_coverage handles a schema 1.0.0 log (no tracking slots)", {
  # Build a 1.0.0-shape log by removing the new slots; computation
  # should not crash and should report coverage as if no attempts were
  # made (which is the truthful claim for a 1.0.0 log).
  log <- create_reflection_log()
  log$provocation_attempts <- NULL
  log$skipped_themes <- NULL
  ts <- .mock_theme_set(c("Adherence"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  expect_equal(cov$n_attempts_recorded, 0L)
  expect_equal(cov$n_themes_silently_skipped, 1L)
  expect_false(cov$no_silent_skip)
})

# ---- AC4 + AC7 contract pins ----------------------------------------------

test_that("AC4: ProvocationCoverage carries methodology mode in `mode` field", {
  log <- create_reflection_log()
  ts <- .mock_theme_set()
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  # AC4: "methodology stamped on every output." The coverage object
  # carries its mode so cross-run comparisons / replays can route
  # downstream rendering correctly.
  expect_equal(cov$mode, "reflexive_scaffold")
})

test_that("AC7: ProvocationCoverage shares Tier0Coverage parent with CorpusCoverage", {
  # AC7: "universal Tier-0 in all modes." The shared virtual parent is
  # how the report renderer dispatches uniformly across modes.
  log <- create_reflection_log()
  ts <- .mock_theme_set()
  data <- .mock_corpus()
  prov_cov <- compute_mode1_coverage(log, ts, data)
  expect_s3_class(prov_cov, "Tier0Coverage")

  # Build a minimal CorpusCoverage to assert the same parent
  # (compute_corpus_coverage requires a full ProgressiveCodingState; we
  # confirm the class chain on the existing constructor's output by
  # calling it via skip_if it can't be constructed cheaply).
  cs <- create_coding_state()
  cs$entries_processed <- "e1"
  cs$entry_results[["e1"]] <- list(skipped = FALSE)
  d <- tibble::tibble(std_id = "e1", std_text = "x")
  cc <- compute_corpus_coverage(cs, d)
  expect_s3_class(cc, "Tier0Coverage")
})

# ---- render_tier0_coverage_card dispatch ----------------------------------

test_that("render_tier0_coverage_card dispatches on ProvocationCoverage", {
  log <- create_reflection_log()
  log <- .populate_attempts(log, c("Adherence"))
  ts <- .mock_theme_set(c("Adherence"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  html <- render_tier0_coverage_card(cov)
  expect_type(html, "character")
  expect_true(grepl("Provocation Coverage", html))
  expect_true(grepl("Mode 1", html))
})

test_that("render_tier0_coverage_card falls through to default for NULL", {
  html <- render_tier0_coverage_card(NULL)
  expect_type(html, "character")
  expect_true(grepl("coverage-unavailable", html))
})

test_that("render_tier0_coverage_card.ProvocationCoverage flags silent skip in banner", {
  log <- create_reflection_log()
  log <- .populate_attempts(log, c("Adherence"))  # only 1 of 2 themes
  ts <- .mock_theme_set(c("Adherence", "Resistance"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  html <- render_tier0_coverage_card(cov)
  expect_true(grepl("coverage-banner-warn", html))
  expect_true(grepl("silently skipped|silent skip", html, ignore.case = TRUE))
  expect_true(grepl("Resistance", html))  # the silently-skipped theme is named
})

test_that("render_tier0_coverage_card.ProvocationCoverage shows OK banner on full coverage", {
  log <- create_reflection_log()
  log <- .populate_attempts(log, c("Adherence", "Resistance"))
  ts <- .mock_theme_set(c("Adherence", "Resistance"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  html <- render_tier0_coverage_card(cov)
  expect_true(grepl("coverage-banner-ok", html))
  expect_true(grepl("No silent skip", html, ignore.case = TRUE))
})

test_that("print.ProvocationCoverage produces a structured summary", {
  log <- create_reflection_log()
  log <- .populate_attempts(log, c("Adherence"))
  ts <- .mock_theme_set(c("Adherence"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  out <- capture.output(print(cov))
  expect_true(any(grepl("ProvocationCoverage", out)))
  expect_true(any(grepl("Themes input", out)))
  expect_true(any(grepl("No silent skip", out)))
})

# ---- Audit C C1: named-list serialization round-trips through JSON --------

test_that("attempts_per_category and explicit_skip_reasons serialize as named JSON objects", {
  # Audit C C1: jsonlite::write_json with auto_unbox=TRUE drops names
  # from named integer vectors, producing anonymous arrays. The fix
  # was to store these as named lists. Verify the fix sticks: round-
  # trip through JSON and confirm names survive.
  log <- create_reflection_log()
  log <- .populate_attempts(log, c("Adherence"))
  log$skipped_themes <- data.frame(
    theme_name = "OtherTheme",
    reason     = "no_supporting_entries",
    skipped_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  ts <- .mock_theme_set(c("Adherence", "OtherTheme"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)

  expect_type(cov$attempts_per_category, "list")
  expect_named(cov$attempts_per_category)
  expect_setequal(names(cov$attempts_per_category),
                    .VALID_PROVOCATION_CATEGORIES)

  expect_type(cov$explicit_skip_reasons, "list")
  expect_equal(cov$explicit_skip_reasons[["no_supporting_entries"]], 1L)

  # Round-trip via tempfile
  tmp <- tempfile(fileext = ".json")
  on.exit(unlink(tmp), add = TRUE)
  jsonlite::write_json(cov, tmp, pretty = TRUE, auto_unbox = TRUE,
                        force = TRUE)
  rt <- jsonlite::read_json(tmp, simplifyVector = FALSE)
  # Names preserved on round-trip
  expect_setequal(names(rt$attempts_per_category),
                    .VALID_PROVOCATION_CATEGORIES)
  expect_equal(rt$explicit_skip_reasons$no_supporting_entries, 1L)
})

# ---- Audit C H1: unexpected-category attempts surfaced as anomaly ---------

test_that("attempts against categories outside requested_categories are surfaced separately", {
  # Audit C H1: factor() with levels= filtered out unexpected-category
  # rows but they still counted toward nrow(attempts), producing the
  # contradictory recorded > expected. Now we partition explicitly.
  log <- create_reflection_log()
  # Mix: counter_narrative (in scope) + assumption_surfacing (out of
  # scope when requested = counter_narrative only)
  log$provocation_attempts <- data.frame(
    theme_name = c("Adherence", "Adherence"),
    category   = c("counter_narrative", "assumption_surfacing"),
    n_emitted  = c(1L, 1L),
    attempted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  log$provocations <- replicate(2, list(category="x", theme_name="y"),
                                  simplify = FALSE)
  ts <- .mock_theme_set(c("Adherence"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data,
                                  requested_categories = "counter_narrative")
  expect_equal(cov$n_attempts_recorded, 1L)  # only counter_narrative
  expect_equal(cov$n_attempts_expected, 1L)
  expect_true(cov$attempts_complete)
  expect_equal(cov$n_unexpected_category_attempts, 1L)
  expect_equal(cov$unexpected_categories, "assumption_surfacing")
  expect_false(cov$no_unexpected_category_attempts)
  expect_false(cov$no_silent_skip)  # headline reflects the anomaly
})

# ---- Audit C H2: zero-themes input is NOT a verified-coverage state -------

test_that("empty theme_set is graded as NOT no_silent_skip", {
  # Audit C H2: empty theme_set was previously graded "OK" with banner
  # "All 0 themes were challenged across all 5 categories." That's
  # a degenerate state, not verified coverage. Headline must be FALSE.
  log <- create_reflection_log()
  empty_ts <- create_theme_set(list())
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, empty_ts, data)
  expect_equal(cov$n_themes_input, 0L)
  expect_false(cov$no_silent_skip)
})

test_that("render_tier0_coverage_card.ProvocationCoverage flags zero-themes input in the banner", {
  log <- create_reflection_log()
  empty_ts <- create_theme_set(list())
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, empty_ts, data)
  html <- render_tier0_coverage_card(cov)
  expect_true(grepl("coverage-banner-warn", html))
  expect_true(grepl("No themes were provided", html, ignore.case = TRUE))
})

# ---- Audit C H3: all-themes-explicitly-skipped is NOT verified coverage ---

test_that("all-themes-explicitly-skipped is graded as NOT no_silent_skip", {
  # Audit C H3: every theme being explicit-skipped (e.g., zero supporting
  # entries everywhere) was previously graded "OK" with banner saying
  # "All 2 themes were challenged" -- contradictory with the explicit-
  # skip rows below. Now headline FALSE because n_themes_attempted == 0.
  log <- create_reflection_log()
  log$skipped_themes <- data.frame(
    theme_name = c("Adherence", "Resistance"),
    reason     = c("no_supporting_entries", "no_supporting_entries"),
    skipped_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  ts <- .mock_theme_set(c("Adherence", "Resistance"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  expect_equal(cov$n_themes_attempted, 0L)
  expect_equal(cov$n_themes_explicit_skip, 2L)
  expect_equal(cov$n_themes_silently_skipped, 0L)
  expect_false(cov$no_silent_skip)
})

test_that("render_tier0_coverage_card.ProvocationCoverage flags all-explicit-skip in the banner", {
  log <- create_reflection_log()
  log$skipped_themes <- data.frame(
    theme_name = c("Adherence", "Resistance"),
    reason     = c("no_supporting_entries", "no_supporting_entries"),
    skipped_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  ts <- .mock_theme_set(c("Adherence", "Resistance"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  html <- render_tier0_coverage_card(cov)
  expect_true(grepl("coverage-banner-warn", html))
  expect_true(grepl("skipped before any provocation category was attempted",
                      html))
})

# ---- Audit C M3: corpus-truncation claim downgraded to honest fields ------

test_that("ProvocationCoverage carries the prompt-context fields (corpus passed vs prompt-includes-full)", {
  # Audit C M3: previous no_silent_corpus_truncation = TRUE overclaimed
  # because the per-category prompts include only theme-supporting
  # entries, not the full corpus text. Replaced with two honest fields.
  log <- create_reflection_log()
  log <- .populate_attempts(log, c("Adherence"))
  ts <- .mock_theme_set(c("Adherence"))
  data <- .mock_corpus()
  cov <- compute_mode1_coverage(log, ts, data)
  expect_true(cov$corpus_provided_to_per_category_fns)  # data IS passed
  expect_false(cov$llm_prompt_includes_full_corpus)     # prompt is not
  # The honest fields appear in both the print and the rendered card
  out <- capture.output(print(cov))
  expect_true(any(grepl("Corpus passed to per-category fns", out)))
  expect_true(any(grepl("LLM prompts include full corpus", out)))
  html <- render_tier0_coverage_card(cov)
  expect_true(grepl("supporting-entry", html))
  # The candidate-sampling shape is disclosed
  expect_true(grepl("bounded, deterministic sample", html))
  expect_true(grepl("never contains the whole corpus", html))
  # Old overclaims should NOT appear anymore
  expect_false(grepl("no silent corpus truncation", html, ignore.case=TRUE))
  expect_false(grepl("search the FULL corpus", html, fixed=TRUE))
})

# ---- L2 (audit C): expected_attempts is clamped at 0 ----------------------

test_that("expected_attempts is clamped at 0 against degenerate counts", {
  # Defensive: if a future bug ever made
  # n_themes_explicit_skip + n_themes_silently_skipped > n_themes_input,
  # the multiplication could produce a negative expected_attempts. The
  # pmax(0L, ...) clamp ensures attempts_complete remains a sensible
  # boolean.
  log <- create_reflection_log()
  empty_ts <- create_theme_set(list())
  cov <- compute_mode1_coverage(log, empty_ts, .mock_corpus())
  expect_equal(cov$n_attempts_expected, 0L)
})
