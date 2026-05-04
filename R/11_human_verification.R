# ==============================================================================
# Human Verification / Inter-Rater Reliability (IRR) Step
# ==============================================================================
# Optional pipeline step where the AI presents a sample of entries WITHOUT
# its codes, along with the full codebook. The human assigns codes, then
# the system computes agreement statistics (Cohen's kappa, percent agreement,
# Jaccard similarity).
# ==============================================================================

#' Run human verification / IRR process
#'
#' Samples entries, exports blank coding sheets and codebook, then checks
#' for a completed human coding sheet. If found, computes agreement stats.
#'
#' @param data tibble with std_text, std_id columns
#' @param coding_state ProgressiveCodingState (provides codebook and AI codes)
#' @param config Human verification config section
#' @param output_dir Output directory path
#' @param checkpoint CheckpointManager (or NULL)
#' @param methodology_mode Optional methodology mode (T1.7 / AC4). When
#'   non-NULL, every IRR CSV produced is stamped with a comment header
#'   identifying the mode and run id. NULL skips stamping (legacy /
#'   test callers).
#' @return List with status ("exported", "completed"), irr_stats, sample_ids
#' @export
run_human_verification <- function(data, coding_state,
                                    config = list(), output_dir = ".",
                                    checkpoint = NULL,
                                    methodology_mode = NULL) {
  config$sample_size <- config$sample_size %||% 20L
  config$seed <- config$seed %||% 42L
  config$max_codes_per_entry <- config$max_codes_per_entry %||% 8L

  irr_dir <- file.path(output_dir, "irr")
  dir.create(irr_dir, recursive = TRUE, showWarnings = FALSE)

  completed_path <- file.path(irr_dir, "human_coding_sheet_completed.csv")
  blank_path <- file.path(irr_dir, "human_coding_sheet.csv")
  codebook_path <- file.path(irr_dir, "codebook.csv")
  ai_ref_path <- file.path(irr_dir, "ai_codes_reference.csv")

  # Sample entries
  set.seed(config$seed)
  n_sample <- min(config$sample_size, nrow(data))
  sample_idx <- sample(nrow(data), n_sample)
  sample_data <- data[sample_idx, ]

  sample_ids <- as.character(sample_data$std_id)

  # T1.7 / AC4: helper closes over methodology_mode + output_dir so the
  # call sites below stay one-line. Mirrors the export_results pattern.
  .stamp <- function(path) {
    if (is.null(methodology_mode) || !file.exists(path)) return(invisible(NULL))
    tryCatch(stamp_methodology_csv(path, methodology_mode,
                                     run_id = basename(output_dir)),
             error = function(e) log_debug("CSV stamp skipped: {e$message}"))
  }

  # Check if completed human sheet already exists
  if (file.exists(completed_path)) {
    log_info("Found completed human coding sheet -- computing IRR...")
    irr_stats <- .compute_irr_agreement(
      completed_path, ai_ref_path, codebook_path,
      sample_ids, config$max_codes_per_entry
    )

    result <- list(
      status = "completed",
      irr_stats = irr_stats,
      sample_ids = sample_ids,
      n_sample = n_sample
    )

    if (!is.null(checkpoint)) {
      save_checkpoint(checkpoint, "irr_completed", result)
    }

    return(result)
  }

  # Export blank coding sheet
  log_info("Exporting human coding sheet ({n_sample} entries)...")
  code_cols <- paste0("code_", seq_len(config$max_codes_per_entry))
  blank_sheet <- tibble(
    entry_id = sample_data$std_id,
    text = sample_data$std_text
  )
  for (cc in code_cols) {
    blank_sheet[[cc]] <- ""
  }
  readr::write_csv(blank_sheet, blank_path)
  .stamp(blank_path)
  log_info("  Exported: {blank_path}")

  # Export codebook from ProgressiveCodingState
  if (!is.null(coding_state$codebook) && length(coding_state$codebook) > 0) {
    codebook_df <- tibble(
      code_name = vapply(coding_state$codebook, function(cb) cb$code_name, character(1)),
      description = vapply(coding_state$codebook, function(cb) cb$description %||% "", character(1)),
      code_type = vapply(coding_state$codebook, function(cb) cb$type %||% "descriptive", character(1)),
      frequency = vapply(coding_state$codebook, function(cb) cb$frequency, integer(1))
    )
    codebook_df <- codebook_df[order(-codebook_df$frequency), ]
    readr::write_csv(codebook_df, codebook_path)
    .stamp(codebook_path)
    log_info("  Exported codebook: {codebook_path} ({nrow(codebook_df)} codes)")
  }

  # Export AI's codes for the same entries (hidden from user initially)
  ai_sheet <- tibble(entry_id = sample_data$std_id)
  for (cc in code_cols) {
    ai_sheet[[cc]] <- ""
  }

  for (i in seq_len(nrow(ai_sheet))) {
    eid <- as.character(ai_sheet$entry_id[i])
    er <- coding_state$entry_results[[eid]]
    if (!is.null(er) && !isTRUE(er$skipped)) {
      code_names <- er$codes_assigned
      code_names <- code_names[!is.na(code_names) & nchar(code_names) > 0]
      # Resolve code keys to human-readable code names
      code_labels <- vapply(code_names, function(k) {
        coding_state$codebook[[k]]$code_name %||% k
      }, character(1))
      for (j in seq_len(min(length(code_labels), config$max_codes_per_entry))) {
        ai_sheet[[code_cols[j]]][i] <- code_labels[j]
      }
    }
  }
  readr::write_csv(ai_sheet, ai_ref_path)
  .stamp(ai_ref_path)
  log_info("  Exported AI reference: {ai_ref_path}")

  log_info("Human verification files exported to: {irr_dir}")
  log_info("Please complete {basename(blank_path)} and save as {basename(completed_path)}")
  log_info("Then re-run the pipeline with resume = TRUE to compute IRR statistics.")

  list(
    status = "exported",
    irr_stats = NULL,
    sample_ids = sample_ids,
    n_sample = n_sample,
    irr_dir = irr_dir
  )
}

# ==============================================================================
# Internal: Compute IRR agreement statistics
# ==============================================================================
# Uses fuzzy string matching (stringdist) to avoid penalizing minor wording
# differences between human and AI codes (e.g., "sleep issues" vs "sleep problems").
# Reports both Cohen's kappa and Krippendorff's alpha -- the latter is preferred
# for multi-label coding with potential missing data (Krippendorff, 2011).
# ==============================================================================

.compute_irr_agreement <- function(human_path, ai_path, codebook_path,
                                    sample_ids, max_codes) {
  # comment="#" so a methodology stamp at the file head survives the
  # round-trip through the user's spreadsheet edit.
  human_df <- tryCatch(
    readr::read_csv(human_path, show_col_types = FALSE, comment = "#"),
    error = function(e) {
      log_error("Could not read human coding sheet: {e$message}")
      return(NULL)
    }
  )

  ai_df <- tryCatch(
    readr::read_csv(ai_path, show_col_types = FALSE, comment = "#"),
    error = function(e) {
      log_error("Could not read AI reference sheet: {e$message}")
      return(NULL)
    }
  )

  if (is.null(human_df) || is.null(ai_df)) {
    return(list(
      cohens_kappa = NA_real_, kappa_interpretation = "Error",
      krippendorff_alpha = NA_real_, alpha_interpretation = "Error",
      percent_agreement = NA_real_, jaccard_similarity = NA_real_,
      n_entries = 0L, error = "Could not read coding sheets"
    ))
  }

  # Read codebook to get all possible codes
  all_codes <- character(0)
  if (file.exists(codebook_path)) {
    codebook <- tryCatch(
      readr::read_csv(codebook_path, show_col_types = FALSE, comment = "#"),
      error = function(e) NULL
    )
    if (!is.null(codebook) && "code_text" %in% names(codebook)) {
      all_codes <- tolower(trimws(codebook$code_text))
    }
  }

  code_cols <- paste0("code_", seq_len(max_codes))
  code_cols <- intersect(code_cols, names(human_df))
  code_cols <- intersect(code_cols, names(ai_df))

  if (length(code_cols) == 0) {
    return(list(
      cohens_kappa = NA_real_, kappa_interpretation = "Error",
      krippendorff_alpha = NA_real_, alpha_interpretation = "Error",
      percent_agreement = NA_real_, jaccard_similarity = NA_real_,
      n_entries = 0L, error = "No code columns found"
    ))
  }

  # Extract code sets per entry for each rater
  n_entries <- min(nrow(human_df), nrow(ai_df))
  agreements <- numeric(0)
  jaccards <- numeric(0)

  for (i in seq_len(n_entries)) {
    human_codes <- tolower(trimws(unlist(human_df[i, code_cols])))
    human_codes <- human_codes[!is.na(human_codes) & nchar(human_codes) > 0]

    ai_codes <- tolower(trimws(unlist(ai_df[i, code_cols])))
    ai_codes <- ai_codes[!is.na(ai_codes) & nchar(ai_codes) > 0]

    # Fuzzy-match human codes to AI codes (and vice versa)
    matched_human <- .fuzzy_match_codes(human_codes, ai_codes)
    matched_ai <- .fuzzy_match_codes(ai_codes, human_codes)

    # n_matched: count of successful matches from human->AI direction
    n_matched <- length(matched_human$matched)
    total_unique <- length(union(human_codes, ai_codes))

    # Jaccard similarity: matched pairs / union of codes
    if (total_unique > 0) {
      jaccards <- c(jaccards, n_matched / total_unique)
    }

    # Percent agreement: full bidirectional match for this entry
    # 1 if all human codes matched AI codes AND all AI codes matched human codes, 0 otherwise
    all_human_matched <- length(matched_human$unmatched) == 0
    all_ai_matched <- length(matched_ai$unmatched) == 0
    entry_agree <- if (length(human_codes) == 0 && length(ai_codes) == 0) 1.0
                   else if (all_human_matched && all_ai_matched) 1.0
                   else 0.0
    agreements <- c(agreements, entry_agree)
  }

  # Build binary agreement matrix using fuzzy matching for kappa and alpha
  # First, build a unified code list by fuzzy-deduplicating all observed codes
  all_observed_codes <- unique(c(
    tolower(trimws(unlist(human_df[, code_cols]))),
    tolower(trimws(unlist(ai_df[, code_cols])))
  ))
  all_observed_codes <- all_observed_codes[!is.na(all_observed_codes) & nchar(all_observed_codes) > 0]

  kappa <- NA_real_
  alpha <- NA_real_

  if (length(all_observed_codes) > 0) {
    # Deduplicate codes list using fuzzy matching (merge near-duplicates)
    canonical_codes <- .fuzzy_deduplicate_codes(all_observed_codes)

    human_binary <- integer(0)
    ai_binary <- integer(0)

    for (i in seq_len(n_entries)) {
      human_codes <- tolower(trimws(unlist(human_df[i, code_cols])))
      human_codes <- human_codes[!is.na(human_codes) & nchar(human_codes) > 0]

      ai_codes <- tolower(trimws(unlist(ai_df[i, code_cols])))
      ai_codes <- ai_codes[!is.na(ai_codes) & nchar(ai_codes) > 0]

      # Map each rater's codes to canonical codes via fuzzy matching
      human_canonical <- .map_to_canonical(human_codes, canonical_codes)
      ai_canonical <- .map_to_canonical(ai_codes, canonical_codes)

      human_binary <- c(human_binary, as.integer(canonical_codes %in% human_canonical))
      ai_binary <- c(ai_binary, as.integer(canonical_codes %in% ai_canonical))
    }

    # NOTE: Kappa/alpha computed on flattened entry-code pairs, which inflates
    # effective sample size for multi-label coding. Interpret with caution.
    # A per-entry set-level agreement metric is also reported (percent_agreement).
    log_info("Note: Kappa/alpha computed on flattened entry-code pairs (see documentation for caveats)")

    kappa <- .compute_cohens_kappa(human_binary, ai_binary)

    # Compute Krippendorff's alpha (nominal, binary data)
    alpha <- .compute_krippendorff_alpha(human_binary, ai_binary,
                                          n_codes = length(canonical_codes),
                                          n_entries = n_entries)
  }

  list(
    cohens_kappa = round(kappa, 3),
    kappa_interpretation = .interpret_kappa(kappa),
    krippendorff_alpha = round(alpha, 3),
    alpha_interpretation = .interpret_alpha(alpha),
    percent_agreement = round(mean(agreements, na.rm = TRUE) * 100, 1),
    jaccard_similarity = round(mean(jaccards, na.rm = TRUE), 3),
    n_entries = n_entries,
    per_entry_jaccard = jaccards,
    per_entry_agreement = agreements,
    error = NULL
  )
}

# ==============================================================================
# Internal: Fuzzy string matching helpers
# ==============================================================================

#' Match codes from source to target using normalized string distance
#' @param source Character vector of codes to match
#' @param target Character vector of codes to match against
#' @param threshold Normalized distance threshold (0 = exact, 1 = anything matches)
#' @return List with `matched` (target codes that were matched) and `unmatched`
#' @keywords internal
.fuzzy_match_codes <- function(source, target, threshold = 0.35) {
  if (length(source) == 0 || length(target) == 0) {
    return(list(matched = character(0), unmatched = source))
  }

  matched <- character(0)
  unmatched <- character(0)

  for (s in source) {
    # Compute normalized string distance to each target code
    dists <- stringdist::stringdist(s, target, method = "jw")  # Jaro-Winkler
    best_idx <- which.min(dists)

    if (length(best_idx) > 0 && dists[best_idx] <= threshold) {
      matched <- c(matched, target[best_idx])
    } else {
      unmatched <- c(unmatched, s)
    }
  }

  list(matched = unique(matched), unmatched = unmatched)
}

#' Deduplicate a list of codes by merging fuzzy near-duplicates
#' @keywords internal
.fuzzy_deduplicate_codes <- function(codes, threshold = 0.35) {
  if (length(codes) <= 1) return(codes)

  canonical <- character(0)
  for (code in codes) {
    if (length(canonical) == 0) {
      canonical <- c(canonical, code)
      next
    }
    dists <- stringdist::stringdist(code, canonical, method = "jw")
    if (min(dists) > threshold) {
      canonical <- c(canonical, code)
    }
    # Otherwise, it's a near-duplicate of an existing canonical code -- skip
  }
  canonical
}

#' Map a set of codes to canonical codes via fuzzy matching
#' @keywords internal
.map_to_canonical <- function(codes, canonical, threshold = 0.35) {
  if (length(codes) == 0) return(character(0))

  mapped <- character(0)
  for (code in codes) {
    dists <- stringdist::stringdist(code, canonical, method = "jw")
    best_idx <- which.min(dists)
    if (length(best_idx) > 0 && dists[best_idx] <= threshold) {
      mapped <- c(mapped, canonical[best_idx])
    }
    # If no match within threshold, the code doesn't map to any canonical code
  }
  unique(mapped)
}

# ==============================================================================
# Internal: Agreement statistics
# ==============================================================================

#' Compute Cohen's kappa for two binary rating vectors
#' @keywords internal
.compute_cohens_kappa <- function(rater1, rater2) {
  if (length(rater1) != length(rater2) || length(rater1) == 0) return(NA_real_)

  # Observed agreement
  p_o <- mean(rater1 == rater2)

  # Expected agreement by chance
  p1_yes <- mean(rater1)
  p2_yes <- mean(rater2)
  p_e <- p1_yes * p2_yes + (1 - p1_yes) * (1 - p2_yes)

  if (p_e == 1) return(1.0)  # Perfect agreement by chance

  (p_o - p_e) / (1 - p_e)
}

#' Compute Krippendorff's alpha for binary nominal data (2 raters)
#'
#' Implements Krippendorff's alpha for nominal data with 2 coders.
#' More robust than Cohen's kappa for sparse binary matrices and handles
#' the prevalence/bias problem better (Krippendorff, 2011).
#'
#' @param rater1 Binary integer vector (flattened: n_entries * n_codes)
#' @param rater2 Binary integer vector (same length as rater1)
#' @param n_codes Number of unique codes
#' @param n_entries Number of entries coded
#' @return Krippendorff's alpha (numeric)
#' @keywords internal
.compute_krippendorff_alpha <- function(rater1, rater2, n_codes, n_entries) {
  if (length(rater1) != length(rater2) || length(rater1) == 0) return(NA_real_)

  # Number of items (each entry-code pair is an item)
  n <- length(rater1)
  # Number of raters per item
  m <- 2L

  # Total number of pairable values
  total_pairs <- n * m * (m - 1)  # = n * 2 since m = 2
  if (total_pairs == 0) return(NA_real_)

  # Observed disagreement (D_o): fraction of within-unit rater pairs that disagree
  # For nominal data, disagreement = 1 if values differ, 0 if same
  n_disagree <- sum(rater1 != rater2)
  D_o <- n_disagree / n

  # Expected disagreement (D_e): based on marginal frequencies
  # For binary nominal: P(disagree by chance) = 2 * p * (1 - p)
  # where p is the overall proportion of 1s
  all_values <- c(rater1, rater2)
  n_total <- length(all_values)
  p_1 <- sum(all_values) / n_total
  p_0 <- 1 - p_1

  # For nominal data with 2 categories:
  # D_e = (n_total / (n_total - 1)) * 2 * p_0 * p_1
  # The (n_total / (n_total - 1)) correction is Krippendorff's small-sample adjustment
  if (n_total <= 1) return(NA_real_)
  D_e <- (n_total / (n_total - 1)) * 2 * p_0 * p_1

  if (D_e == 0) return(1.0)  # Perfect expected agreement

  1 - (D_o / D_e)
}

#' Interpret kappa value using Landis & Koch (1977) scale
#' @keywords internal
.interpret_kappa <- function(kappa) {
  if (is.na(kappa)) return("N/A")
  if (kappa < 0) "Poor"
  else if (kappa < 0.21) "Slight"
  else if (kappa < 0.41) "Fair"
  else if (kappa < 0.61) "Moderate"
  else if (kappa < 0.81) "Substantial"
  else "Almost Perfect"
}

#' Interpret Krippendorff's alpha
#' Krippendorff recommends alpha >= 0.667 as lowest acceptable for tentative
#' conclusions, and alpha >= 0.800 for reliable conclusions.
#' @keywords internal
.interpret_alpha <- function(alpha) {
  if (is.na(alpha)) return("N/A")
  if (alpha < 0) "Unreliable (below chance)"
  else if (alpha < 0.667) "Unreliable (discard or re-examine)"
  else if (alpha < 0.800) "Acceptable (tentative conclusions)"
  else "Reliable"
}
