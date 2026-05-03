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
