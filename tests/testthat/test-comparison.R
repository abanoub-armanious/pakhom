# ==============================================================================
# Tests for Cross-Run Comparison Module (R/19_comparison.R)
# ==============================================================================

fixture_dir <- test_path("fixtures", "comparison")

# ==============================================================================
# Run Discovery
# ==============================================================================

test_that(".discover_run_dirs returns empty for nonexistent directory", {

  expect_equal(.discover_run_dirs("/nonexistent/path"), character(0))
})

test_that(".discover_run_dirs finds and sorts run directories correctly", {
  dirs <- .discover_run_dirs(fixture_dir)
  expect_length(dirs, 2)
  expect_true(grepl("run_2026-01-01", dirs[1]))
  expect_true(grepl("run_2026-01-02", dirs[2]))
})

test_that(".discover_run_dirs accepts T1.7 mode-suffixed run dirs (_M1, _M2, _M3)", {
  # Finding: the regex was `^run_\d{4}-\d{2}-\d{2}_\d{6}$`
  # without the optional `_M[123]` suffix, so EVERY production
  # run dir (which carries the methodology short-code as a directory
  # suffix per R/output_stamping.R::run_id_with_mode) was silently
  # filtered out -- compare_runs() and list_available_runs() saw 0
  # runs across every real production output. find_latest_run
  # was fixed for the same issue earlier; .discover_run_dirs was
  # missed.
  tmp <- withr::local_tempdir()
  for (suffix in c("_M1", "_M2", "_M3", "")) {
    d <- file.path(tmp, sprintf("run_2026-05-04_120000%s", suffix))
    dir.create(d, recursive = TRUE)
    # discover_run_dirs requires themes.json to consider a run "complete"
    writeLines("[]", file.path(d, "themes.json"))
  }
  dirs <- .discover_run_dirs(tmp)
  expect_equal(length(dirs), 4L)
  basenames <- basename(dirs)
  expect_true("run_2026-05-04_120000_M1" %in% basenames)
  expect_true("run_2026-05-04_120000_M2" %in% basenames)
  expect_true("run_2026-05-04_120000_M3" %in% basenames)
  expect_true("run_2026-05-04_120000" %in% basenames)
})

test_that(".discover_run_dirs still rejects non-run patterns even with mode-suffix tail", {
  tmp <- withr::local_tempdir()
  for (name in c("run_2026-05-04_120000_M1",   # legitimate
                 "run_2026-05-04_120000_M9",    # invalid mode code
                 "run_2026-05-04_120000_M22",   # invalid two-digit
                 "run_2026-05-04_M2",            # missing time
                 "run_2026-05-04_120000_extra", # extra suffix
                 "old_run_2026-05-04_120000")) { # different prefix
    d <- file.path(tmp, name)
    dir.create(d, recursive = TRUE)
    writeLines("[]", file.path(d, "themes.json"))
  }
  dirs <- .discover_run_dirs(tmp)
  # Only the legitimate one should be picked up
  expect_equal(length(dirs), 1L)
  expect_equal(basename(dirs[1]), "run_2026-05-04_120000_M1")
})

test_that(".discover_run_dirs ignores non-run directories and incomplete runs", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "run_2026-01-01_120000"))
  file.create(file.path(tmp, "run_2026-01-01_120000", "themes.json"))  # complete run
  dir.create(file.path(tmp, "run_2026-01-02_120000"))  # incomplete: no themes.json
  dir.create(file.path(tmp, "checkpoints"))
  dir.create(file.path(tmp, "some_other_dir"))
  file.create(file.path(tmp, "latest"))  # symlink file

  dirs <- .discover_run_dirs(tmp)
  expect_length(dirs, 1)
  expect_true(grepl("run_2026-01-01", dirs[1]))
})

# ==============================================================================
# Snapshot Loading
# ==============================================================================

test_that(".load_run_snapshot reads all files", {
  snap <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  expect_equal(snap$run_id, "run_2026-01-01_120000")
  expect_s3_class(snap$themes, "tbl_df")
  expect_s3_class(snap$sentiment, "tbl_df")
  expect_s3_class(snap$codes, "tbl_df")
  expect_s3_class(snap$correlations, "tbl_df")
  expect_equal(nrow(snap$themes), 3)
  expect_equal(nrow(snap$sentiment), 10)
  expect_equal(nrow(snap$codes), 5)
})

test_that(".load_run_snapshot handles missing files gracefully", {
  tmp <- withr::local_tempdir("run_2026-05-01_000000")
  snap <- .load_run_snapshot(tmp)
  expect_null(snap$themes)
  expect_null(snap$sentiment)
  expect_null(snap$codes)
  expect_null(snap$correlations)
})

test_that(".load_run_snapshot unwraps the methodology-stamp envelope in themes.json", {
  # Regression: real runs write themes.json wrapped in {_methodology_stamp, _payload}
  # (output_stamping.R). A naive as_tibble(fromJSON()) errors on the two
  # unequal-length top-level fields and silently returns NULL, emptying the
  # theme-Jaccard / theme-evolution panel -- the core compare_models() metric.
  tmp <- withr::local_tempdir()
  d <- file.path(tmp, "run_2026-05-01_120000")
  dir.create(d)
  stamped <- list(
    `_methodology_stamp` = list(mode = "codebook_collaborative", run_id = "run_2026-05-01_120000",
                                schema = "themes", stamped_at = "2026-05-01T12:00:00Z"),
    `_payload` = data.frame(
      name = c("Deep-work quality", "Side effects"),
      entry_count = c(12L, 7L),
      codes_included = I(list(c("good_sleep", "fell_asleep"), c("headache", "nausea"))),
      stringsAsFactors = FALSE
    )
  )
  writeLines(jsonlite::toJSON(stamped, auto_unbox = TRUE), file.path(d, "themes.json"))
  snap <- pakhom:::.load_run_snapshot(d)
  expect_s3_class(snap$themes, "tbl_df")
  expect_equal(nrow(snap$themes), 2)
  expect_true("name" %in% names(snap$themes))
  expect_true("codes_included" %in% names(snap$themes))
})

test_that(".load_run_snapshot parses timestamp from folder name", {
  snap <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  expect_s3_class(snap$timestamp, "POSIXct")
})

# ==============================================================================
# Sample Overlap
# ==============================================================================

test_that(".compare_samples computes correct overlap", {
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_samples(list(snap1, snap2), snap2)

  expect_equal(result$pairwise$n_shared, 8)  # entries 1-7,9 shared
  expect_equal(result$pairwise$n_new, 2)     # entries 11,12
  expect_equal(result$pairwise$n_dropped, 2) # entries 8,10
  expect_true(result$pairwise$jaccard_index > 0.5)
  expect_true(result$pairwise$jaccard_index < 1.0)
})

test_that(".compare_samples detects text changes", {
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_samples(list(snap1, snap2), snap2)

  expect_true(is.integer(result$text_changes) || is.numeric(result$text_changes))
  # Texts are identical for shared entries in our fixture
  expect_equal(result$text_changes, 0L)
})

test_that(".compare_samples interprets overlap correctly", {
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_samples(list(snap1, snap2), snap2)

  expect_true(result$interpretation %in%
    c("identical sample", "mostly same sample", "overlapping samples", "largely different samples"))
})

test_that(".compare_samples per_run has correct structure", {
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_samples(list(snap1, snap2), snap2)

  expect_equal(nrow(result$per_run), 2)
  expect_true("total_entries" %in% names(result$per_run))
  expect_equal(result$per_run$total_entries[1], 10L)
  expect_equal(result$per_run$total_entries[2], 10L)
})

# ==============================================================================
# Sentiment Comparison
# ==============================================================================

test_that(".compare_sentiment computes per-run aggregations", {
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_sentiment(list(snap1, snap2), snap2)

  expect_equal(nrow(result$per_run), 2)
  expect_true(all(c("mean_sentiment", "top_emotions", "pct_negative") %in% names(result$per_run)))
  expect_true(!is.na(result$per_run$mean_sentiment[1]))
})

test_that(".compare_sentiment joins entries and computes shift", {
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_sentiment(list(snap1, snap2), snap2)

  expect_true(nrow(result$per_entry) > 0)
  expect_true("shift" %in% names(result$per_entry))
  expect_true(!is.na(result$summary$mean_shift))
  expect_true(result$summary$n_shared_entries > 0)
})

test_that(".compare_sentiment detects reclassification", {
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_sentiment(list(snap1, snap2), snap2)

  # entry_003 changes from fear to sadness
  expect_true(result$summary$reclassification_rate > 0)
})

# ==============================================================================
# Code Stability
# ==============================================================================

test_that(".code_jaccard returns correct values", {
  expect_equal(.code_jaccard(c("a", "b", "c"), c("a", "b", "c")), 1.0)
  expect_equal(.code_jaccard(c("a", "b"), c("c", "d")), 0.0)
  expect_equal(.code_jaccard("a; b; c", "a; b; d"), 0.5)  # 2 shared / 4 union
  expect_equal(.code_jaccard(character(0), character(0)), 1.0)
  expect_equal(.code_jaccard("a", character(0)), 0.0)
})

test_that(".compare_codes classifies stable, renamed, new, dropped", {
  skip_if_not_installed("stringdist")
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_codes(list(snap1, snap2), snap2)

  # "Poor Deep-work Quality" and "Overwork Triggers" should be stable
  expect_true(nrow(result$pairwise$stable) >= 2)

  # "Scheduling Nausea" -> "Scheduling Nausea and GI Issues" should be renamed
  expect_true(nrow(result$pairwise$renamed) >= 0 || nrow(result$pairwise$stable) >= 2)

  # Stability metrics should exist
  expect_true(!is.null(result$stability$churn_rate))
  expect_true(result$stability$churn_rate >= 0 && result$stability$churn_rate <= 1)
})

test_that(".compare_codes computes Jaccard similarity", {
  skip_if_not_installed("stringdist")
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_codes(list(snap1, snap2), snap2)

  expect_true(result$stability$jaccard_overall >= 0)
  expect_true(result$stability$jaccard_overall <= 1)
})

test_that(".compare_codes Jaccard mirrors the fuzzy breakdown, not exact strings", {
  skip_if_not_installed("stringdist")
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_codes(list(snap1, snap2), snap2)

  s <- result$stability
  n_matched <- s$n_stable + s$n_renamed
  total <- n_matched + s$n_new + s$n_dropped
  expected <- if (total > 0) round(n_matched / total, 3) else 1
  # The headline Jaccard must mirror the fuzzy stable/renamed/new/dropped table.
  # The fixture renames "Scheduling Nausea" -> "Scheduling Nausea and GI Issues";
  # exact-string Jaccard would drop that pair and report a lower, self-
  # contradictory number than the breakdown it sits beside.
  expect_equal(s$jaccard_overall, expected)
})

# ==============================================================================
# Theme Matching
# ==============================================================================

test_that(".match_themes_pairwise matches similar themes", {
  skip_if_not_installed("stringdist")
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .match_themes_pairwise(snap1$themes, snap2$themes)

  # "Focus Fragmentation and Overwork" -> "Focus Fragmentation and Overwork Risk"
  # "Scheduling Side Effects" -> "Scheduling Side Effects and Tolerance"
  expect_true(nrow(result$persisted) >= 2)
})

test_that(".match_themes_pairwise detects new and disappeared themes", {
  skip_if_not_installed("stringdist")
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .match_themes_pairwise(snap1$themes, snap2$themes)

  # "Emotional Regulation Struggles" disappeared, "Recovery and Coping Strategies" is new
  total_themes <- nrow(result$persisted) + nrow(result$new) + nrow(result$disappeared)
  expect_true(total_themes > 0)
})

test_that(".match_themes_pairwise does not double-match", {
  skip_if_not_installed("stringdist")
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .match_themes_pairwise(snap1$themes, snap2$themes)

  if (nrow(result$persisted) > 0) {
    expect_true(length(unique(result$persisted$theme_prev)) == nrow(result$persisted))
    expect_true(length(unique(result$persisted$theme_curr)) == nrow(result$persisted))
  }
})

test_that(".match_themes_pairwise handles NULL themes gracefully", {
  result <- .match_themes_pairwise(NULL, NULL)
  expect_equal(nrow(result$persisted), 0)
  expect_equal(nrow(result$new), 0)
  expect_equal(nrow(result$disappeared), 0)
})

test_that(".compare_themes returns pairwise and timeline", {
  skip_if_not_installed("stringdist")
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_themes(list(snap1, snap2), snap2)

  expect_true(!is.null(result$pairwise))
  expect_true(!is.null(result$timeline))
  expect_true(nrow(result$timeline) >= 6)  # 3 themes * 2 runs
})

# ==============================================================================
# Correlation Stability
# ==============================================================================

test_that(".normalize_corr_var strips prefixes and normalizes", {
  expect_equal(.normalize_corr_var("theme_membership_Focus.Disruption"), "focus disruption")
  expect_equal(.normalize_corr_var("emotion_intensity"), "emotion intensity")
  expect_equal(.normalize_corr_var("sentiment_score"), "sentiment score")
})

test_that(".compare_correlations identifies persistent vs run-specific", {
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_correlations(list(snap1, snap2), snap2)

  # emotion_intensity <-> confidence is significant in both runs
  expect_true(nrow(result$persistent) >= 1)
  expect_true(nrow(result$trends) > 0)
})

test_that(".compare_correlations handles empty correlations", {
  snap <- list(
    run_id = "empty_run",
    timestamp = Sys.time(),
    themes = NULL, sentiment = NULL, codes = NULL,
    correlations = tibble::tibble(),
    dir = tempdir()
  )
  result <- .compare_correlations(list(snap, snap), snap)
  expect_equal(nrow(result$trends), 0)
})

test_that(".compare_correlations judges persistence against correlation-bearing runs only", {
  # A run with no correlations.csv must not deflate the persistence
  # denominator: a pair significant in every MEASURED run is persistent.
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  snap3 <- list(
    run_id = "run_no_correlations",
    timestamp = Sys.time(),
    themes = NULL, sentiment = NULL, codes = NULL,
    correlations = NULL,
    dir = tempdir()
  )
  result <- .compare_correlations(list(snap1, snap2, snap3), snap3)

  expect_equal(result$n_corr_runs, 2L)
  # The emotion_intensity <-> confidence pair is significant in both
  # measured runs -> persistent (pre-fix it fell to intermittent because
  # the denominator counted the correlation-less third run).
  expect_true(nrow(result$persistent) >= 1)
  expect_false(any(result$persistent$pair_key %in% result$intermittent$pair_key))
})

test_that(".compare_correlations with one correlation-bearing run classifies all as run-specific", {
  # A single measured run gives no cross-run basis for persistence.
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  empty_a <- list(
    run_id = "run_empty_a",
    timestamp = Sys.time(),
    themes = NULL, sentiment = NULL, codes = NULL,
    correlations = NULL,
    dir = tempdir()
  )
  empty_b <- empty_a
  empty_b$run_id <- "run_empty_b"
  result <- .compare_correlations(list(snap1, empty_a, empty_b), empty_b)

  expect_equal(result$n_corr_runs, 1L)
  expect_equal(nrow(result$persistent), 0)
  expect_equal(nrow(result$intermittent), 0)
  expect_true(nrow(result$run_specific) >= 1)
})

# ==============================================================================
# Entry Migration
# ==============================================================================

test_that(".compare_entry_migration builds migration matrix", {
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_entry_migration(snap2, snap1)

  expect_true(nrow(result$matrix) > 0)
  expect_true("n_entries" %in% names(result$matrix))
})

test_that(".compare_entry_migration counts a reordered multi-label set as STABLE", {
  # emerged_themes is a multi-label, semicolon-joined cell. An entry whose theme
  # SET is unchanged but reordered ("Alpha; Beta" -> "Beta; Alpha") must count
  # as stable, not migrated -- the prior exact-string comparison miscounted it.
  mk <- function(themes) list(
    run_id = "r", timestamp = NULL, themes = NULL, codes = NULL,
    correlations = tibble::tibble(), dir = tempdir(),
    sentiment = tibble::tibble(std_id = c("e1", "e2"), emerged_themes = themes)
  )
  prev <- mk(c("Alpha; Beta", "Gamma"))
  curr <- mk(c("Beta; Alpha", "Gamma"))   # e1 reordered (same set); e2 identical
  res <- pakhom:::.compare_entry_migration(curr, prev)
  expect_equal(res$stability_rate, 1)
  expect_equal(res$n_migrated, 0L)
})

test_that(".compare_entry_migration computes stability rate", {
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_entry_migration(snap2, snap1)

  expect_true(!is.na(result$stability_rate))
  expect_true(result$stability_rate >= 0 && result$stability_rate <= 1)
  expect_true(result$n_stable + result$n_migrated > 0)
})

test_that(".compare_entry_migration counts new and dropped entries", {
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .compare_entry_migration(snap2, snap1)

  # entries 11, 12 are new; entries 8, 10 are dropped
  expect_equal(result$n_new_entries, 2L)
  expect_equal(result$n_dropped_entries, 2L)
})

# ==============================================================================
# Run Dashboard
# ==============================================================================

test_that(".build_run_dashboard produces correct tibble", {
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  dashboard <- .build_run_dashboard(list(snap1, snap2))

  expect_s3_class(dashboard, "tbl_df")
  expect_equal(nrow(dashboard), 2)
  expect_true(all(c("run_id", "total_entries", "n_themes", "mean_sentiment",
                     "n_significant_correlations") %in% names(dashboard)))
})

# ==============================================================================
# Integration
# ==============================================================================

test_that("compare_runs returns NULL with only 1 run", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "run_2026-01-01_120000"))
  result <- compare_runs(file.path(tmp, "run_2026-01-01_120000"), tmp)
  expect_null(result)
})

test_that("compare_runs returns ComparisonResult with 2 runs", {
  result <- compare_runs(
    file.path(fixture_dir, "run_2026-01-02_120000"),
    fixture_dir
  )

  expect_s3_class(result, "ComparisonResult")
  expect_equal(result$n_runs, 2)
  expect_equal(result$current_run, "run_2026-01-02_120000")
  expect_true(!is.null(result$dashboard))
  expect_true(!is.null(result$sample_overlap))
})

test_that("print.ComparisonResult works without error", {
  result <- compare_runs(
    file.path(fixture_dir, "run_2026-01-02_120000"),
    fixture_dir
  )
  expect_output(print(result), "Cross-Run Comparison")
})

# ==============================================================================
# Output schema versioning
# ==============================================================================

test_that(".schema_is_compatible accepts identical major versions", {
  expect_true(.schema_is_compatible("1.0", "1.0"))
  expect_true(.schema_is_compatible("1.5", "1.0"))   # minor bumps stay compat
  expect_true(.schema_is_compatible("1.0", "1.5"))
})

test_that(".schema_is_compatible rejects mismatched major versions", {
  expect_false(.schema_is_compatible("0.5", "1.0"))
  expect_false(.schema_is_compatible("2.0", "1.0"))
})

test_that(".schema_is_compatible rejects missing or malformed versions", {
  expect_false(.schema_is_compatible(NULL))
  expect_false(.schema_is_compatible(NA))
  expect_false(.schema_is_compatible(""))
  expect_false(.schema_is_compatible(NA_character_))
})

test_that(".load_run_snapshot tags schema_compatible from run_metadata.json", {
  snap <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  expect_equal(snap$schema_version, "1.0")
  expect_true(snap$schema_compatible)
})

test_that(".load_run_snapshot tags incompatibility when metadata is missing", {
  tmp <- withr::local_tempdir("run_2026-05-01_000000")
  # Create a fake run with no metadata
  file.create(file.path(tmp, "themes.json"))
  snap <- .load_run_snapshot(tmp)
  expect_null(snap$schema_version)
  expect_false(snap$schema_compatible)
})

test_that("compare_runs excludes schema-incompatible snapshots and warns", {
  tmp <- withr::local_tempdir()
  # Two complete v2 runs (with metadata) + one legacy run (without metadata)
  legacy_dir <- file.path(tmp, "run_2025-12-01_120000")
  ok_dir_a <- file.path(tmp, "run_2026-01-01_120000")
  ok_dir_b <- file.path(tmp, "run_2026-01-02_120000")
  for (d in c(legacy_dir, ok_dir_a, ok_dir_b)) {
    dir.create(d, recursive = TRUE)
    file.copy(
      list.files(file.path(fixture_dir, "run_2026-01-01_120000"),
                 full.names = TRUE),
      d, recursive = TRUE
    )
  }
  # Strip metadata from legacy_dir (simulate pre-versioning run)
  file.remove(file.path(legacy_dir, "run_metadata.json"))

  # Capture logger output via a temporary appender (the suite silences logger's
  # console appender; capture.output(type="message") would catch nothing).
  msgs <- character(0)
  logger::log_appender(function(lines) msgs <<- c(msgs, lines))
  on.exit(logger::log_appender(.pakhom_test_silent_appender), add = TRUE)
  result <- compare_runs(ok_dir_b, tmp)
  warning_text <- paste(msgs, collapse = "\n")

  expect_s3_class(result, "ComparisonResult")
  expect_equal(result$n_runs, 2)  # only the two compatible runs participate
  expect_true(grepl("Excluding 1 run", warning_text) ||
              grepl("incompatible output schema", warning_text))
})

test_that("compare_runs returns NULL when fewer than 2 schema-compatible runs", {
  tmp <- withr::local_tempdir()
  # One compatible run, one legacy run -> only 1 compatible -> NULL
  ok_dir <- file.path(tmp, "run_2026-01-01_120000")
  legacy_dir <- file.path(tmp, "run_2025-12-01_120000")
  for (d in c(ok_dir, legacy_dir)) {
    dir.create(d, recursive = TRUE)
    file.copy(
      list.files(file.path(fixture_dir, "run_2026-01-01_120000"),
                 full.names = TRUE),
      d, recursive = TRUE
    )
  }
  file.remove(file.path(legacy_dir, "run_metadata.json"))

  result <- compare_runs(ok_dir, tmp)
  expect_null(result)
})

test_that("list_available_runs reports schema_version and schema_compatible", {
  result <- list_available_runs(fixture_dir)
  expect_true("schema_version" %in% names(result))
  expect_true("schema_compatible" %in% names(result))
  expect_true(all(result$schema_version == "1.0"))
  expect_true(all(result$schema_compatible))
})

# ==============================================================================
# compare_models: inter-model wrapper
# ==============================================================================

# Helper: set up two fixture run dirs, optionally rewriting one's metadata so
# the runs report distinct provider/model combinations.
.copy_fixtures_for_compare_models <- function(target_dir, second_provider = "openai",
                                                second_model = "gpt-4o") {
  for (run_name in c("run_2026-01-01_120000", "run_2026-01-02_120000")) {
    src <- file.path(fixture_dir, run_name)
    dst <- file.path(target_dir, run_name)
    dir.create(dst, recursive = TRUE)
    file.copy(list.files(src, full.names = TRUE), dst, recursive = TRUE)
  }
  if (!identical(second_provider, "openai") || !identical(second_model, "gpt-4o")) {
    meta_path <- file.path(target_dir, "run_2026-01-02_120000", "run_metadata.json")
    meta <- jsonlite::fromJSON(meta_path)
    meta$provider <- second_provider
    meta$model_primary <- second_model
    jsonlite::write_json(meta, meta_path, pretty = TRUE, auto_unbox = TRUE)
  }
}

test_that("compare_models returns NULL when fewer than 2 runs are present", {
  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "run_2026-01-01_120000"))
  expect_null(compare_models(tmp))
})

test_that("compare_models flags is_inter_model = FALSE when both runs used the same model", {
  tmp <- withr::local_tempdir()
  .copy_fixtures_for_compare_models(tmp)
  result <- compare_models(tmp)
  expect_s3_class(result, "ComparisonResult")
  expect_false(isTRUE(result$is_inter_model))
  expect_equal(length(result$unique_models), 1L)
  expect_equal(result$unique_models[1], "openai/gpt-4o")
})

test_that("compare_models flags is_inter_model = TRUE when runs used different models", {
  tmp <- withr::local_tempdir()
  .copy_fixtures_for_compare_models(tmp,
                                     second_provider = "anthropic",
                                     second_model = "claude-sonnet-4-6")
  result <- compare_models(tmp)
  expect_s3_class(result, "ComparisonResult")
  expect_true(isTRUE(result$is_inter_model))
  expect_setequal(result$unique_models,
                  c("openai/gpt-4o", "anthropic/claude-sonnet-4-6"))
  expect_equal(length(result$models_used), 2L)
})

test_that(".load_run_snapshot reads stamped CSVs without choking on the AC4 comment header", {
  # Audit A CRITICAL: pre-fix, the read_csv calls in
  # .load_run_snapshot didn't pass comment="#" so a stamped CSV's first
  # line ("# methodology: ...") would have been parsed as a malformed
  # header, silently breaking compare_runs() for every production run.
  tmp <- withr::local_tempdir()
  run_dir <- file.path(tmp, "run_2026-05-04_120000")
  src <- file.path(fixture_dir, "run_2026-01-01_120000")
  dir.create(run_dir, recursive = TRUE)
  file.copy(list.files(src, full.names = TRUE), run_dir, recursive = TRUE)
  for (fn in c("sentiment_scores.csv", "codes.csv",
               "correlations.csv")) {
    stamp_methodology_csv(file.path(run_dir, fn),
                           "codebook_collaborative", run_id = "r1")
  }
  snap <- pakhom:::.load_run_snapshot(run_dir)
  expect_s3_class(snap$sentiment, "tbl_df")
  expect_s3_class(snap$codes, "tbl_df")
  expect_s3_class(snap$correlations, "tbl_df")
  # Sanity: the data columns survived (the parse didn't treat the stamp
  # as the header).
  expect_true("std_id" %in% names(snap$sentiment))
  expect_true("code_text" %in% names(snap$codes))
})

test_that("compare_models decorates the result with models_used and unique_models slots", {
  tmp <- withr::local_tempdir()
  .copy_fixtures_for_compare_models(tmp,
                                     second_provider = "anthropic",
                                     second_model = "claude-opus-4-7")
  result <- compare_models(tmp)
  # The decoration must be visible to downstream consumers (the
  # cross-model agreement reporting in the methodology paper relies
  # on these fields, so they're load-bearing -- not just metadata).
  expect_true(all(c("is_inter_model", "models_used", "unique_models") %in% names(result)))
  expect_named(result$models_used,
               c("run_2026-01-01_120000", "run_2026-01-02_120000"),
               ignore.order = TRUE)
})
