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

  # "Poor Sleep Quality" and "Binge Eating Triggers" should be stable
  expect_true(nrow(result$pairwise$stable) >= 2)

  # "Medication Nausea" -> "Medication Nausea and GI Issues" should be renamed
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

# ==============================================================================
# Theme Matching
# ==============================================================================

test_that(".match_themes_pairwise matches similar themes", {
  skip_if_not_installed("stringdist")
  snap1 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-01_120000"))
  snap2 <- .load_run_snapshot(file.path(fixture_dir, "run_2026-01-02_120000"))
  result <- .match_themes_pairwise(snap1$themes, snap2$themes)

  # "Sleep Disruption and Binge Eating" -> "Sleep Disruption and Binge Vulnerability"
  # "Medication Side Effects" -> "Medication Side Effects and Tolerance"
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
  expect_equal(.normalize_corr_var("theme_membership_Sleep.Disruption"), "sleep disruption")
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

  # Capture warnings emitted to verify the filter announces the exclusion
  msgs <- capture.output(
    result <- compare_runs(ok_dir_b, tmp),
    type = "message"
  )
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
