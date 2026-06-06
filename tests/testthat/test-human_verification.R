# Tests for human verification / IRR module (14_human_verification.R)
# Tests use synthetic coding data -- no AI or file I/O beyond temp CSVs.

# ==============================================================================
# .compute_cohens_kappa: Known-value tests
# ==============================================================================
test_that("Cohen's kappa = 1.0 for perfect agreement", {
  rater1 <- c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0)
  rater2 <- c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0)

  kappa <- pakhom:::.compute_cohens_kappa(rater1, rater2)
  expect_equal(kappa, 1.0)
})

test_that("Cohen's kappa ~ 0 for chance-level agreement", {
  # Two raters with uncorrelated judgments (large sample)
  set.seed(99)
  n <- 1000
  rater1 <- sample(0:1, n, replace = TRUE)
  rater2 <- sample(0:1, n, replace = TRUE)

  kappa <- pakhom:::.compute_cohens_kappa(rater1, rater2)
  # Should be near 0 (within ~0.1 for random data)
  expect_true(abs(kappa) < 0.15)
})

test_that("Cohen's kappa handles empty vectors", {
  expect_true(is.na(pakhom:::.compute_cohens_kappa(integer(0), integer(0))))
})

test_that("Cohen's kappa handles length mismatch", {
  expect_true(is.na(pakhom:::.compute_cohens_kappa(c(1, 0), c(1, 0, 1))))
})

test_that("Cohen's kappa is negative for worse-than-chance agreement", {
  # Systematically opposite ratings
  rater1 <- c(1, 0, 1, 0, 1, 0, 1, 0)
  rater2 <- c(0, 1, 0, 1, 0, 1, 0, 1)

  kappa <- pakhom:::.compute_cohens_kappa(rater1, rater2)
  expect_true(kappa < 0)
})

# ==============================================================================
# .compute_krippendorff_alpha: Known-value tests
# ==============================================================================
test_that("Krippendorff's alpha = 1.0 for perfect agreement", {
  rater1 <- c(1, 0, 1, 0, 1, 0)
  rater2 <- c(1, 0, 1, 0, 1, 0)

  alpha <- pakhom:::.compute_krippendorff_alpha(rater1, rater2,
                                                      n_codes = 3, n_entries = 2)
  expect_equal(alpha, 1.0)
})

test_that("Krippendorff's alpha < kappa for systematically opposed raters", {
  rater1 <- c(1, 0, 1, 0, 1, 0, 1, 0, 1, 0)
  rater2 <- c(0, 1, 0, 1, 0, 1, 0, 1, 0, 1)

  kappa <- pakhom:::.compute_cohens_kappa(rater1, rater2)
  alpha <- pakhom:::.compute_krippendorff_alpha(rater1, rater2,
                                                      n_codes = 5, n_entries = 2)

  # Both should be negative for systematic disagreement
  expect_true(alpha < 0)
  expect_true(kappa < 0)
})

test_that("Krippendorff's alpha handles empty input", {
  expect_true(is.na(pakhom:::.compute_krippendorff_alpha(
    integer(0), integer(0), n_codes = 0, n_entries = 0
  )))
})

# ==============================================================================
# .interpret_kappa: Landis & Koch scale
# ==============================================================================
test_that("interpret_kappa returns correct labels", {
  expect_equal(pakhom:::.interpret_kappa(NA), "N/A")
  expect_equal(pakhom:::.interpret_kappa(-0.1), "Poor")
  expect_equal(pakhom:::.interpret_kappa(0.1), "Slight")
  expect_equal(pakhom:::.interpret_kappa(0.3), "Fair")
  expect_equal(pakhom:::.interpret_kappa(0.5), "Moderate")
  expect_equal(pakhom:::.interpret_kappa(0.7), "Substantial")
  expect_equal(pakhom:::.interpret_kappa(0.9), "Almost Perfect")
})

# ==============================================================================
# .interpret_alpha: Krippendorff scale
# ==============================================================================
test_that("interpret_alpha returns correct labels", {
  expect_equal(pakhom:::.interpret_alpha(NA), "N/A")
  expect_equal(pakhom:::.interpret_alpha(-0.1), "Unreliable (below chance)")
  expect_equal(pakhom:::.interpret_alpha(0.5), "Unreliable (discard or re-examine)")
  expect_equal(pakhom:::.interpret_alpha(0.7), "Acceptable (tentative conclusions)")
  expect_equal(pakhom:::.interpret_alpha(0.85), "Reliable")
})

# ==============================================================================
# .fuzzy_match_codes: String distance matching
# ==============================================================================
test_that("fuzzy_match_codes matches near-identical strings", {
  result <- pakhom:::.fuzzy_match_codes(
    source = c("sleep disruption", "medication effects"),
    target = c("sleep disruptions", "medication effect", "unrelated code"),
    threshold = 0.35
  )
  expect_true(length(result$matched) >= 2)
  expect_equal(length(result$unmatched), 0)
})

test_that("fuzzy_match_codes rejects dissimilar strings", {
  result <- pakhom:::.fuzzy_match_codes(
    source = c("sleep disruption"),
    target = c("appetite changes", "mood regulation"),
    threshold = 0.35
  )
  expect_equal(length(result$matched), 0)
  expect_equal(length(result$unmatched), 1)
})

test_that("fuzzy_match_codes handles empty inputs", {
  result <- pakhom:::.fuzzy_match_codes(character(0), c("a", "b"))
  expect_equal(length(result$matched), 0)
  expect_equal(length(result$unmatched), 0)

  result2 <- pakhom:::.fuzzy_match_codes(c("a"), character(0))
  expect_equal(length(result2$matched), 0)
  expect_equal(length(result2$unmatched), 1)
})

# ==============================================================================
# .fuzzy_deduplicate_codes: Near-duplicate merging
# ==============================================================================
test_that("fuzzy_deduplicate_codes merges near-duplicates", {
  codes <- c("sleep problems", "sleep problem", "appetite changes", "appetite change")
  result <- pakhom:::.fuzzy_deduplicate_codes(codes, threshold = 0.35)

  # Should reduce to ~2 canonical codes
  expect_true(length(result) <= 3)
  expect_true(length(result) >= 2)
})

test_that("fuzzy_deduplicate_codes preserves distinct codes", {
  codes <- c("insomnia", "binge eating", "depression", "medication")
  result <- pakhom:::.fuzzy_deduplicate_codes(codes, threshold = 0.35)
  expect_equal(length(result), 4)
})

test_that("fuzzy_deduplicate_codes handles single code", {
  expect_equal(pakhom:::.fuzzy_deduplicate_codes("single"), "single")
})

# ==============================================================================
# .map_to_canonical: Code mapping
# ==============================================================================
test_that("map_to_canonical maps codes to nearest canonical", {
  canonical <- c("sleep disruption", "medication effects", "mood changes")

  mapped <- pakhom:::.map_to_canonical(
    codes = c("sleep disruptions", "mood change"),
    canonical = canonical,
    threshold = 0.35
  )
  expect_true("sleep disruption" %in% mapped)
  expect_true("mood changes" %in% mapped)
})

test_that("map_to_canonical returns empty for no matches", {
  mapped <- pakhom:::.map_to_canonical(
    codes = c("completely unrelated xyz"),
    canonical = c("sleep", "eating"),
    threshold = 0.1  # Very strict threshold
  )
  expect_equal(length(mapped), 0)
})

test_that("map_to_canonical handles empty input", {
  result <- pakhom:::.map_to_canonical(character(0), c("a", "b"))
  expect_equal(length(result), 0)
})

# ==============================================================================
# run_human_verification: Export flow
# ==============================================================================
test_that("run_human_verification exports blank sheet and codebook", {
  tmp_dir <- tempfile("irr_test_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  data <- tibble::tibble(
    std_id = paste0("e_", 1:10),
    std_text = paste0("Sample text for entry ", 1:10)
  )

  # Build a ProgressiveCodingState with some codes and entry results
  coding_state <- create_coding_state()
  coding_state$codebook[["sleep_issues"]] <- list(
    code_name = "Sleep Issues", description = "Problems with sleep",
    type = "descriptive", frequency = 5L,
    entry_ids = paste0("e_", 1:5),
    coded_segments = lapply(1:5, function(i) list(
      entry_id = paste0("e_", i), text = "sleep problem", start_char = 0L, end_char = 13L
    ))
  )
  coding_state$codebook[["medication_side_effects"]] <- list(
    code_name = "Medication Side Effects", description = "Side effects of meds",
    type = "descriptive", frequency = 3L,
    entry_ids = paste0("e_", 6:8),
    coded_segments = lapply(6:8, function(i) list(
      entry_id = paste0("e_", i), text = "side effect", start_char = 0L, end_char = 11L
    ))
  )
  for (i in 1:10) {
    eid <- paste0("e_", i)
    if (i <= 5) {
      coding_state$entry_results[[eid]] <- list(
        codes_assigned = "sleep_issues", skipped = FALSE,
        coded_segments = list(list(code_key = "sleep_issues", code_name = "Sleep Issues",
                                    text = "sleep problem", start_char = 0L, end_char = 13L))
      )
    } else if (i <= 8) {
      coding_state$entry_results[[eid]] <- list(
        codes_assigned = "medication_side_effects", skipped = FALSE,
        coded_segments = list(list(code_key = "medication_side_effects",
                                    code_name = "Medication Side Effects",
                                    text = "side effect", start_char = 0L, end_char = 11L))
      )
    } else {
      coding_state$entry_results[[eid]] <- list(
        codes_assigned = character(0), skipped = TRUE, skip_reason = "N/A"
      )
    }
  }

  result <- run_human_verification(
    data, coding_state,
    config = list(sample_size = 5, seed = 42, max_codes_per_entry = 4),
    output_dir = tmp_dir
  )

  expect_equal(result$status, "exported")
  expect_null(result$irr_stats)
  expect_equal(result$n_sample, 5L)
  expect_true(length(result$sample_ids) == 5)

  # Check files were created
  irr_dir <- file.path(tmp_dir, "irr")
  expect_true(file.exists(file.path(irr_dir, "human_coding_sheet.csv")))
  # Codebook is exported only when coding_state$codebook is non-empty
  expect_true(file.exists(file.path(irr_dir, "codebook.csv")) ||
              length(coding_state$codebook) == 0)
  expect_true(file.exists(file.path(irr_dir, "ai_codes_reference.csv")))

  # Check blank sheet structure
  blank <- readr::read_csv(file.path(irr_dir, "human_coding_sheet.csv"),
                            show_col_types = FALSE)
  expect_true("entry_id" %in% names(blank))
  expect_true("text" %in% names(blank))
  expect_true("code_1" %in% names(blank))
  expect_equal(nrow(blank), 5)
})

# ==============================================================================
# .compute_irr_agreement: Full IRR computation
# ==============================================================================
test_that("compute_irr_agreement produces valid stats for matching coders", {
  tmp_dir <- tempfile("irr_compute_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create human and AI coding sheets with high agreement
  human_df <- tibble::tibble(
    entry_id = paste0("e_", 1:5),
    code_1 = c("sleep issues", "medication effects", "sleep issues", "mood changes", "sleep issues"),
    code_2 = c("", "sleep issues", "", "", "medication effects"),
    code_3 = rep("", 5)
  )

  ai_df <- tibble::tibble(
    entry_id = paste0("e_", 1:5),
    code_1 = c("sleep issues", "medication effects", "sleep issues", "mood changes", "sleep issues"),
    code_2 = c("", "sleep issues", "", "", "medication effects"),
    code_3 = rep("", 5)
  )

  codebook_df <- tibble::tibble(
    code_text = c("sleep issues", "medication effects", "mood changes"),
    frequency = c(5L, 3L, 2L)
  )

  human_path <- file.path(tmp_dir, "human.csv")
  ai_path <- file.path(tmp_dir, "ai.csv")
  codebook_path <- file.path(tmp_dir, "codebook.csv")

  readr::write_csv(human_df, human_path)
  readr::write_csv(ai_df, ai_path)
  readr::write_csv(codebook_df, codebook_path)

  result <- pakhom:::.compute_irr_agreement(
    human_path, ai_path, codebook_path,
    sample_ids = paste0("e_", 1:5), max_codes = 3
  )

  expect_type(result, "list")
  expect_true(!is.null(result$cohens_kappa))
  expect_true(!is.null(result$krippendorff_alpha))
  expect_true(!is.null(result$percent_agreement))
  expect_true(!is.null(result$jaccard_similarity))

  # Perfect agreement should yield high scores
  expect_equal(result$percent_agreement, 100.0)
  expect_equal(result$jaccard_similarity, 1.0)
  expect_equal(result$cohens_kappa, 1.0)
  expect_null(result$error)
})

test_that("compute_irr_agreement handles partial disagreement", {
  tmp_dir <- tempfile("irr_partial_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  human_df <- tibble::tibble(
    entry_id = paste0("e_", 1:4),
    code_1 = c("sleep issues", "medication effects", "mood changes", "appetite"),
    code_2 = rep("", 4)
  )

  ai_df <- tibble::tibble(
    entry_id = paste0("e_", 1:4),
    code_1 = c("sleep issues", "dosage problems", "mood changes", "weight gain"),
    code_2 = rep("", 4)
  )

  codebook_df <- tibble::tibble(
    code_text = c("sleep issues", "medication effects", "mood changes", "appetite", "dosage problems", "weight gain"),
    frequency = c(3L, 2L, 2L, 1L, 1L, 1L)
  )

  readr::write_csv(human_df, file.path(tmp_dir, "human.csv"))
  readr::write_csv(ai_df, file.path(tmp_dir, "ai.csv"))
  readr::write_csv(codebook_df, file.path(tmp_dir, "codebook.csv"))

  result <- pakhom:::.compute_irr_agreement(
    file.path(tmp_dir, "human.csv"),
    file.path(tmp_dir, "ai.csv"),
    file.path(tmp_dir, "codebook.csv"),
    sample_ids = paste0("e_", 1:4), max_codes = 2
  )

  # Should show partial agreement (2/4 exact matches)
  expect_true(result$percent_agreement < 100)
  expect_true(result$percent_agreement > 0)
  expect_true(result$cohens_kappa < 1.0)
  expect_null(result$error)
})

test_that("compute_irr_agreement returns error for missing files", {
  result <- pakhom:::.compute_irr_agreement(
    "/nonexistent/human.csv",
    "/nonexistent/ai.csv",
    "/nonexistent/codebook.csv",
    sample_ids = "e_1", max_codes = 3
  )

  expect_true(!is.null(result$error))
  expect_equal(result$n_entries, 0L)
  expect_true(is.na(result$cohens_kappa))
})

# ==============================================================================
# AC4: methodology stamping on IRR CSV exports (T1.7)
# ==============================================================================

test_that("run_human_verification stamps all 3 exported IRR CSVs", {
  tmp_dir <- tempfile("irr_stamp_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  data <- tibble::tibble(
    std_id = paste0("e_", 1:6),
    std_text = paste0("Sample ", 1:6)
  )
  cs <- create_coding_state()
  cs$codebook[["c1"]] <- list(
    code_name = "C1", description = "d", type = "descriptive",
    frequency = 3L, entry_ids = paste0("e_", 1:3),
    coded_segments = list()
  )
  for (i in 1:6) {
    cs$entry_results[[paste0("e_", i)]] <- list(
      codes_assigned = "c1", skipped = FALSE, coded_segments = list()
    )
  }

  result <- run_human_verification(
    data, cs,
    config = list(sample_size = 4, seed = 7, max_codes_per_entry = 4),
    output_dir = tmp_dir,
    methodology_mode = "codebook_collaborative"
  )
  expect_equal(result$status, "exported")

  irr_dir <- file.path(tmp_dir, "irr")
  paths <- c(
    file.path(irr_dir, "human_coding_sheet.csv"),
    file.path(irr_dir, "codebook.csv"),
    file.path(irr_dir, "ai_codes_reference.csv")
  )
  for (p in paths) {
    expect_true(file.exists(p))
    expect_match(readLines(p, n = 1L),
                 "^# methodology: M2 - Codebook Collaborative")
  }
})

test_that("run_human_verification does NOT stamp when methodology_mode is NULL", {
  tmp_dir <- tempfile("irr_nostamp_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  data <- tibble::tibble(
    std_id = paste0("e_", 1:4),
    std_text = paste0("Sample ", 1:4)
  )
  cs <- create_coding_state()
  cs$codebook[["c1"]] <- list(
    code_name = "C1", description = "d", type = "descriptive",
    frequency = 2L, entry_ids = paste0("e_", 1:2),
    coded_segments = list()
  )
  for (i in 1:4) {
    cs$entry_results[[paste0("e_", i)]] <- list(
      codes_assigned = "c1", skipped = FALSE, coded_segments = list()
    )
  }

  run_human_verification(
    data, cs,
    config = list(sample_size = 3, seed = 1, max_codes_per_entry = 3),
    output_dir = tmp_dir
  )
  blank <- file.path(tmp_dir, "irr", "human_coding_sheet.csv")
  expect_false(grepl("^# methodology:", readLines(blank, n = 1L)))
})

test_that(".compute_irr_agreement reads through methodology stamps in completed sheet", {
  tmp_dir <- tempfile("irr_roundtrip_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  irr_dir <- file.path(tmp_dir, "irr")
  dir.create(irr_dir, recursive = TRUE)

  human <- tibble::tibble(
    entry_id = paste0("e_", 1:3),
    code_1 = c("sleep issues", "anxiety", "exhaustion"),
    code_2 = rep("", 3)
  )
  ai <- tibble::tibble(
    entry_id = paste0("e_", 1:3),
    code_1 = c("sleep issues", "anxiety", "fatigue"),
    code_2 = rep("", 3)
  )
  codebook <- tibble::tibble(
    code_text = c("sleep issues", "anxiety", "exhaustion", "fatigue"),
    description = "d", code_type = "descriptive", frequency = 1L
  )

  human_path <- file.path(irr_dir, "human_coding_sheet_completed.csv")
  ai_path <- file.path(irr_dir, "ai_codes_reference.csv")
  cb_path <- file.path(irr_dir, "codebook.csv")
  readr::write_csv(human, human_path)
  readr::write_csv(ai, ai_path)
  readr::write_csv(codebook, cb_path)

  # Stamp all three: simulates a real run where the user kept the stamps
  # when they edited the human coding sheet.
  stamp_methodology_csv(human_path, "codebook_collaborative", run_id = "r1")
  stamp_methodology_csv(ai_path, "codebook_collaborative", run_id = "r1")
  stamp_methodology_csv(cb_path, "codebook_collaborative", run_id = "r1")

  result <- pakhom:::.compute_irr_agreement(
    human_path, ai_path, cb_path,
    sample_ids = paste0("e_", 1:3),
    max_codes = 3
  )

  # Stamp must not poison the comparison: 2 of 3 entries match exactly,
  # the third differs (exhaustion vs fatigue).
  expect_null(result$error)
  expect_equal(result$n_entries, 3L)
  expect_true(result$percent_agreement >= 50)
})

# ==============================================================================
# BUG 2 regression: rater alignment is by entry_id, NOT row position
# ==============================================================================
test_that("compute_irr_agreement aligns by entry_id even when rows are shuffled", {
  tmp_dir <- tempfile("irr_join_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  human_df <- tibble::tibble(
    entry_id = paste0("e_", 1:5),
    code_1 = c("sleep issues", "medication effects", "mood changes", "appetite", "anxiety"),
    code_2 = rep("", 5)
  )
  # SAME codings keyed to the SAME entry_ids, but the AI sheet rows are in a
  # different order -- exactly what happens when a user sorts the spreadsheet.
  ai_df <- human_df[c(4, 2, 5, 1, 3), ]

  hp <- file.path(tmp_dir, "h.csv"); ap <- file.path(tmp_dir, "a.csv")
  cp <- file.path(tmp_dir, "c.csv")
  readr::write_csv(human_df, hp); readr::write_csv(ai_df, ap)
  readr::write_csv(tibble::tibble(code_text = "x", frequency = 1L), cp)

  result <- pakhom:::.compute_irr_agreement(hp, ap, cp,
    sample_ids = paste0("e_", 1:5), max_codes = 2)

  # Joined by entry_id, every entry matches itself -> perfect agreement.
  # Under the OLD positional alignment this would have been badly mis-paired.
  expect_equal(result$n_entries, 5L)
  expect_equal(result$percent_agreement, 100.0)
  expect_equal(result$jaccard_similarity, 1.0)
  expect_equal(result$cohens_kappa, 1.0)
})

test_that("compute_irr_agreement drops entries present in only one sheet", {
  tmp_dir <- tempfile("irr_partialjoin_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  human_df <- tibble::tibble(
    entry_id = paste0("e_", 1:4),
    code_1 = c("sleep issues", "anxiety", "mood changes", "appetite"),
    code_2 = rep("", 4)
  )
  ai_df <- tibble::tibble(  # only e_1..e_3 in common; e_9 is AI-only
    entry_id = c("e_1", "e_2", "e_3", "e_9"),
    code_1 = c("sleep issues", "anxiety", "mood changes", "weight gain"),
    code_2 = rep("", 4)
  )
  hp <- file.path(tmp_dir, "h.csv"); ap <- file.path(tmp_dir, "a.csv")
  cp <- file.path(tmp_dir, "c.csv")
  readr::write_csv(human_df, hp); readr::write_csv(ai_df, ap)
  readr::write_csv(tibble::tibble(code_text = "x", frequency = 1L), cp)

  result <- pakhom:::.compute_irr_agreement(hp, ap, cp,
    sample_ids = paste0("e_", 1:4), max_codes = 2)

  # Only the 3 shared entries are compared (e_4 human-only, e_9 AI-only dropped).
  expect_equal(result$n_entries, 3L)
  expect_equal(result$percent_agreement, 100.0)
})

# ==============================================================================
# BUG 3 regression: conservative threshold keeps semantic variants distinct
# ==============================================================================
test_that("semantic variants are NOT merged (counted as disagreement)", {
  tmp_dir <- tempfile("irr_threshold_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  human_df <- tibble::tibble(entry_id = c("e_1", "e_2"),
    code_1 = c("sleep issues", "mood changes"), code_2 = c("", ""))
  ai_df <- tibble::tibble(entry_id = c("e_1", "e_2"),
    code_1 = c("sleep problems", "mood swings"), code_2 = c("", ""))  # variants
  hp <- file.path(tmp_dir, "h.csv"); ap <- file.path(tmp_dir, "a.csv")
  cp <- file.path(tmp_dir, "c.csv")
  readr::write_csv(human_df, hp); readr::write_csv(ai_df, ap)
  readr::write_csv(tibble::tibble(code_text = "x", frequency = 1L), cp)

  result <- pakhom:::.compute_irr_agreement(hp, ap, cp,
    sample_ids = c("e_1", "e_2"), max_codes = 2)

  # "sleep issues"/"sleep problems" (jw~0.30) and "mood changes"/"mood swings"
  # (jw~0.20) are genuine labelling differences at the 0.15 threshold, so NONE
  # of the entries should be scored as agreement (the old 0.35 wrongly merged
  # them -> falsely perfect agreement).
  expect_equal(result$percent_agreement, 0.0)
  expect_equal(result$jaccard_similarity, 0.0)
})

# ==============================================================================
# BUG 1 regression: set-based Krippendorff alpha + per-code kappa primitives
# ==============================================================================
test_that(".jaccard_set_distance computes set distances", {
  expect_equal(pakhom:::.jaccard_set_distance(c("a", "b"), c("a", "b")), 0)
  expect_equal(pakhom:::.jaccard_set_distance("a", "b"), 1)
  expect_equal(pakhom:::.jaccard_set_distance(c("a", "b"), "a"), 0.5)
  expect_equal(pakhom:::.jaccard_set_distance(character(0), character(0)), 0)
  expect_equal(pakhom:::.jaccard_set_distance("a", character(0)), 1)
})

test_that(".set_krippendorff_alpha matches hand-computed values", {
  # Perfect agreement -> alpha = 1.
  expect_equal(pakhom:::.set_krippendorff_alpha(list("a", "b"), list("a", "b")), 1.0)
  # H1={a},A1={a}; H2={b},A2={c}: D_o=0.5, D_e=5/6 -> alpha = 1 - 0.6 = 0.4.
  expect_equal(
    pakhom:::.set_krippendorff_alpha(list("a", "b"), list("a", "c")),
    0.4, tolerance = 1e-9
  )
  # Systematic disagreement on shared codes -> negative alpha.
  a <- pakhom:::.set_krippendorff_alpha(list("a", "b"), list("b", "a"))
  expect_true(a < 0)
})

test_that(".mean_per_code_kappa averages per-code agreement", {
  # H={a},{b}; A={a},{c}: code a perfect (k=1), codes b,c one-sided (k=0).
  k <- pakhom:::.mean_per_code_kappa(list("a", "b"), list("a", "c"),
                                     c("a", "b", "c"))
  expect_equal(k, 1 / 3, tolerance = 1e-9)
  # Codes present for neither rater are skipped (no spurious 1.0).
  k2 <- pakhom:::.mean_per_code_kappa(list("a"), list("a"), c("a", "zzz"))
  expect_equal(k2, 1.0)
})

test_that("set-based alpha is not inflated by sparse, distinct codes", {
  # 50% of entries share their (single) code, the rest are all-distinct.
  # A flattened binary alpha would be pulled toward 1 by the many 0/0 cells;
  # the set-based alpha must stay moderate.
  tmp_dir <- tempfile("irr_noinflate_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  ids <- paste0("e_", 1:8)
  # e1-e4: identical code in both raters (agreement). e5-e8: genuinely distinct
  # words in each rater (disagreement). All 16 strings are chosen with no shared
  # 4-char prefixes so the conservative canonicalizer does not merge any of them.
  human_df <- tibble::tibble(entry_id = ids,
    code_1 = c("insomnia", "nausea", "cravings", "bloating",
               "dizziness", "fatigue", "anxiety", "weight gain"),
    code_2 = rep("", 8))
  ai_df <- tibble::tibble(entry_id = ids,
    code_1 = c("insomnia", "nausea", "cravings", "bloating",
               "headaches", "restlessness", "depression", "appetite loss"),
    code_2 = rep("", 8))
  hp <- file.path(tmp_dir, "h.csv"); ap <- file.path(tmp_dir, "a.csv")
  cp <- file.path(tmp_dir, "c.csv")
  readr::write_csv(human_df, hp); readr::write_csv(ai_df, ap)
  readr::write_csv(tibble::tibble(code_text = "x", frequency = 1L), cp)

  result <- pakhom:::.compute_irr_agreement(hp, ap, cp,
    sample_ids = ids, max_codes = 2)

  expect_equal(result$percent_agreement, 50.0)         # 4 of 8 entries match
  expect_true(result$krippendorff_alpha < 0.8)          # NOT inflated to ~1
  expect_true(result$krippendorff_alpha > 0)            # but still positive
  # n >= 8 -> a bootstrap CI is produced and well-ordered.
  expect_false(is.na(result$alpha_ci_low))
  expect_false(is.na(result$alpha_ci_high))
  expect_true(result$alpha_ci_low <= result$alpha_ci_high)
})

test_that("bootstrap alpha CI is NA for small samples", {
  ci <- pakhom:::.bootstrap_alpha_ci(list("a", "b", "c"), list("a", "b", "c"),
                                     n_boot = 100L)
  expect_true(all(is.na(ci)))
})

test_that("bootstrap alpha CI preserves the caller's RNG stream", {
  set.seed(123)
  before <- .Random.seed
  invisible(pakhom:::.bootstrap_alpha_ci(
    as.list(letters[1:10]), as.list(letters[1:10]), n_boot = 50L, seed = 7L))
  expect_identical(.Random.seed, before)
})

# ==============================================================================
# The computed IRR must actually reach the report: .build_rmd_content threads
# irr_result into .build_irr_section, and the pipeline passes it to
# generate_report(). (Regression: the pipeline used to compute IRR but never
# pass it, so the entire IRR section was silently absent from every report.)
# ==============================================================================
test_that("the IRR section is rendered into the report when stats are present", {
  data  <- sample_data(8)
  stats <- aggregate_overall_statistics(data, mock_theme_set(), consolidated = NULL,
                                        learning_context = NULL, config = mock_config())
  ef <- list(sentiment_file = "s.csv", correlations_file = "c.csv", codes_file = "codes.csv")
  base_args <- list(
    overall_stats = stats, theme_stats = list(), theme_order = character(0),
    ai_synthesis = list(executive_summary = "ok", conclusion = "done"),
    corr_interpretation = NULL, insights = list(), export_files = ef,
    config = mock_config()
  )
  # Shape mirrors run_human_verification()'s "completed" return.
  irr <- list(status = "completed", irr_stats = list(
    cohens_kappa = 0.71, kappa_interpretation = "Substantial",
    krippendorff_alpha = 0.78, alpha_interpretation = "Acceptable (tentative conclusions)",
    alpha_ci_low = 0.61, alpha_ci_high = 0.90, percent_agreement = 80.0,
    jaccard_similarity = 0.85, n_entries = 20L, n_codes = 12L, error = NULL))

  with_irr <- do.call(pakhom:::.build_rmd_content, c(base_args, list(irr_result = irr)))
  expect_true(grepl("# Inter-Rater Reliability", with_irr, fixed = TRUE))
  expect_true(grepl("Krippendorff's alpha (set-based, Jaccard)", with_irr, fixed = TRUE))
  expect_true(grepl("95% CI [0.61, 0.9", with_irr, fixed = TRUE))
  expect_true(grepl("Mean per-code Cohen's kappa (12 codes)", with_irr, fixed = TRUE))

  # Omitted when no IRR was run (irr_result = NULL).
  no_irr <- do.call(pakhom:::.build_rmd_content, c(base_args, list(irr_result = NULL)))
  expect_false(grepl("# Inter-Rater Reliability", no_irr, fixed = TRUE))
})
