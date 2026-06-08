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

  # Sample entries. Seed the sampling WITHOUT clobbering the caller's global
  # RNG stream -- run_human_verification is exported, so a bare set.seed() here
  # would permanently reset the user's random state. .with_seed restores it.
  n_sample <- min(config$sample_size, nrow(data))
  sample_idx <- if (is.null(config$seed)) {
    sample(nrow(data), n_sample)
  } else {
    .with_seed(config$seed, sample(nrow(data), n_sample))
  }
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
      sample_ids, config$max_codes_per_entry,
      match_threshold = config$match_threshold %||% 0.15,
      seed = config$seed
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
# Aligns the two raters by entry_id, then canonicalizes codes with conservative
# fuzzy matching (Jaro-Winkler <= 0.15) that bridges spelling / inflection
# differences ("sleep issue" vs "sleep issues") but NOT semantic variants
# ("sleep issues" vs "sleep problems"), which are real disagreements.
# Headline coefficient: Krippendorff's alpha with a Jaccard set-distance -- the
# field-standard agreement metric for set-valued (multi-label) coding (Artstein
# & Poesio, 2008) -- reported with a bootstrap 95% CI. Also reports per-entry
# Jaccard similarity, exact-set percent agreement, and mean per-code Cohen's
# kappa. All are computed per-entry / per-code, so none inflate with codebook
# size (the defect of a flattened binary kappa/alpha).
# ==============================================================================

.compute_irr_agreement <- function(human_path, ai_path, codebook_path,
                                    sample_ids, max_codes,
                                    match_threshold = 0.15,
                                    n_boot = 2000L, seed = 42L) {
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
    return(.empty_irr_result("Could not read coding sheets"))
  }

  # NB: codebook_path is accepted for signature stability but is NOT needed for
  # agreement -- the codes actually assigned by each rater come from the sheets
  # themselves, and the previous codebook read populated a variable that was
  # never used.

  code_cols <- paste0("code_", seq_len(max_codes))
  code_cols <- intersect(code_cols, names(human_df))
  code_cols <- intersect(code_cols, names(ai_df))

  if (length(code_cols) == 0) {
    return(.empty_irr_result("No code columns found"))
  }

  # Collect the non-empty, normalized code strings in one sheet row.
  .row_codes <- function(df, idx) {
    v <- tolower(trimws(as.character(unlist(df[idx, code_cols]))))
    v[!is.na(v) & nchar(v) > 0]
  }

  # --- Align the two raters by entry_id, NOT by row position. -----------------
  # Both sheets carry an entry_id column. A researcher routinely sorts or
  # filters the coding sheet in a spreadsheet before saving it; under the old
  # positional alignment that silently mis-paired rows and corrupted every
  # statistic. Joining on entry_id guarantees each compared pair is the SAME
  # entry. (Legacy sheets that lack entry_id fall back to positional order.)
  if ("entry_id" %in% names(human_df) && "entry_id" %in% names(ai_df)) {
    h_eid <- as.character(human_df$entry_id)
    a_eid <- as.character(ai_df$entry_id)
    if (anyDuplicated(h_eid) || anyDuplicated(a_eid)) {
      log_warn("IRR: duplicate entry_id rows found; using the first occurrence of each.")
    }
    h_first <- !duplicated(h_eid)
    a_first <- !duplicated(a_eid)
    h_map <- stats::setNames(which(h_first), h_eid[h_first])
    a_map <- stats::setNames(which(a_first), a_eid[a_first])
    common <- intersect(names(h_map), names(a_map))
    # Order by the original sample order when available, else by id.
    if (!is.null(sample_ids)) {
      sid <- as.character(sample_ids)
      common <- c(sid[sid %in% common], setdiff(common, sid))
    }
    dropped <- length(union(names(h_map), names(a_map))) - length(common)
    if (dropped > 0) {
      log_warn("IRR: {dropped} entry(ies) present in only one sheet were excluded from agreement.")
    }
    if (length(common) == 0) {
      return(.empty_irr_result("No entry_id values shared between the two coding sheets"))
    }
    human_raw <- lapply(common, function(id) .row_codes(human_df, h_map[[id]]))
    ai_raw    <- lapply(common, function(id) .row_codes(ai_df,    a_map[[id]]))
  } else {
    log_warn("IRR: a coding sheet lacks an entry_id column; falling back to positional row alignment.")
    n_pos <- min(nrow(human_df), nrow(ai_df))
    human_raw <- lapply(seq_len(n_pos), function(i) .row_codes(human_df, i))
    ai_raw    <- lapply(seq_len(n_pos), function(i) .row_codes(ai_df, i))
  }

  n_entries <- length(human_raw)

  # --- Canonicalize codes so set operations are exact. ------------------------
  # The threshold is deliberately conservative (0.15 Jaro-Winkler): it bridges
  # spelling / inflection differences ("sleep issue" vs "sleep issues",
  # jw<=0.04) but NOT semantic variants ("sleep issues" vs "sleep problems",
  # jw~=0.30), which are genuine labelling disagreements and must not be hidden.
  all_observed <- unique(unlist(c(human_raw, ai_raw)))
  all_observed <- all_observed[!is.na(all_observed) & nchar(all_observed) > 0]

  if (length(all_observed) == 0) {
    # Both raters left every compared entry blank -> vacuously perfect agreement.
    return(list(
      cohens_kappa = NA_real_, kappa_interpretation = "N/A",
      krippendorff_alpha = NA_real_, alpha_interpretation = "N/A",
      alpha_ci_low = NA_real_, alpha_ci_high = NA_real_,
      percent_agreement = 100.0, jaccard_similarity = 1.0,
      n_entries = n_entries, n_codes = 0L,
      per_entry_jaccard = rep(1.0, n_entries),
      per_entry_agreement = rep(1.0, n_entries),
      error = NULL
    ))
  }

  canonical_codes <- .fuzzy_deduplicate_codes(all_observed, threshold = match_threshold)
  human_sets <- lapply(human_raw, .map_to_canonical,
                       canonical = canonical_codes, threshold = match_threshold)
  ai_sets    <- lapply(ai_raw, .map_to_canonical,
                       canonical = canonical_codes, threshold = match_threshold)

  # --- Per-entry set metrics (the honest, multi-label-valid agreement). -------
  per_entry_jaccard <- vapply(seq_len(n_entries), function(i)
    1 - .jaccard_set_distance(human_sets[[i]], ai_sets[[i]]), numeric(1))
  per_entry_agreement <- vapply(seq_len(n_entries), function(i) {
    h <- human_sets[[i]]; a <- ai_sets[[i]]
    if (length(h) == 0 && length(a) == 0) 1.0
    else if (setequal(h, a)) 1.0 else 0.0
  }, numeric(1))

  # --- Headline chance-corrected coefficients. --------------------------------
  # Krippendorff's alpha with a Jaccard set-distance is the field-standard
  # agreement coefficient for set-valued (multi-label) coding (Artstein &
  # Poesio, 2008). It operates on per-unit set distances and so -- unlike a
  # flattened binary presence/absence matrix, whose agreement inflates as the
  # number of distinct codes grows -- is NOT distorted by codebook size.
  alpha    <- .set_krippendorff_alpha(human_sets, ai_sets)
  alpha_ci <- .bootstrap_alpha_ci(human_sets, ai_sets, n_boot = n_boot, seed = seed)
  # Cohen's kappa is a single-label coefficient; for multi-label data the code reports
  # the MEAN per-code kappa (each canonical code scored as its own binary
  # present/absent problem across entries). Computed per code, it is likewise
  # immune to the flattening inflation. Supplementary to the set-based alpha.
  kappa <- .mean_per_code_kappa(human_sets, ai_sets, canonical_codes)

  list(
    cohens_kappa = round(kappa, 3),
    kappa_interpretation = .interpret_kappa(kappa),
    krippendorff_alpha = round(alpha, 3),
    alpha_interpretation = .interpret_alpha(alpha),
    alpha_ci_low = round(alpha_ci[1], 3),
    alpha_ci_high = round(alpha_ci[2], 3),
    percent_agreement = round(mean(per_entry_agreement, na.rm = TRUE) * 100, 1),
    jaccard_similarity = round(mean(per_entry_jaccard, na.rm = TRUE), 3),
    n_entries = n_entries,
    n_codes = length(canonical_codes),
    per_entry_jaccard = per_entry_jaccard,
    per_entry_agreement = per_entry_agreement,
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
.fuzzy_match_codes <- function(source, target, threshold = 0.15) {
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
.fuzzy_deduplicate_codes <- function(codes, threshold = 0.15) {
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
.map_to_canonical <- function(codes, canonical, threshold = 0.15) {
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
#' Binary-nominal primitive, retained for the single-label / binary case and
#' its unit tests. NOTE: the multi-label IRR path does NOT apply this to a
#' flattened presence/absence matrix -- that inflates agreement as the codebook
#' grows. Multi-label agreement uses .set_krippendorff_alpha (Jaccard distance).
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

#' Empty/error IRR result with the full result schema
#' @keywords internal
.empty_irr_result <- function(error_msg) {
  list(
    cohens_kappa = NA_real_, kappa_interpretation = "Error",
    krippendorff_alpha = NA_real_, alpha_interpretation = "Error",
    alpha_ci_low = NA_real_, alpha_ci_high = NA_real_,
    percent_agreement = NA_real_, jaccard_similarity = NA_real_,
    n_entries = 0L, n_codes = 0L,
    per_entry_jaccard = numeric(0), per_entry_agreement = numeric(0),
    error = error_msg
  )
}

#' Jaccard distance between two code sets (0 = identical, 1 = disjoint)
#' Two empty sets are treated as identical (distance 0).
#' @keywords internal
.jaccard_set_distance <- function(s, t) {
  if (length(s) == 0 && length(t) == 0) return(0)
  u <- length(union(s, t))
  if (u == 0) return(0)
  1 - length(intersect(s, t)) / u
}

#' Krippendorff's alpha for set-valued (multi-label) coding by 2 raters
#'
#' Uses a Jaccard set-distance: alpha = 1 - D_o / D_e, where D_o is the mean
#' per-entry distance between the two raters' code sets, and D_e is the mean
#' distance over all pairs of the 2n observed code sets (the chance baseline).
#' Because it works on per-entry set distances, it does not inflate with the
#' number of distinct codes -- the defect of a flattened binary kappa/alpha.
#' Returns NA when alpha is undefined (fewer than 2 codings, or D_e == 0 with
#' non-zero observed disagreement).
#' @keywords internal
.set_krippendorff_alpha <- function(human_sets, ai_sets) {
  n <- length(human_sets)
  if (n < 1L) return(NA_real_)

  d_o <- mean(vapply(seq_len(n), function(i)
    .jaccard_set_distance(human_sets[[i]], ai_sets[[i]]), numeric(1)))

  pool <- c(human_sets, ai_sets)
  m <- length(pool)
  if (m < 2L) return(NA_real_)

  total <- 0
  cnt <- 0L
  for (i in seq_len(m - 1L)) {
    for (j in (i + 1L):m) {
      total <- total + .jaccard_set_distance(pool[[i]], pool[[j]])
      cnt <- cnt + 1L
    }
  }
  d_e <- total / cnt
  if (d_e == 0) return(if (d_o == 0) 1.0 else NA_real_)
  1 - d_o / d_e
}

#' Mean per-code Cohen's kappa across entries (multi-label supplementary metric)
#'
#' For each canonical code, builds the binary present/absent vector across
#' entries for each rater and computes Cohen's kappa, then averages over codes.
#' Codes used by neither rater (which would spuriously score 1.0) are skipped.
#' @keywords internal
.mean_per_code_kappa <- function(human_sets, ai_sets, canonical_codes) {
  if (length(canonical_codes) == 0 || length(human_sets) == 0) return(NA_real_)
  n <- length(human_sets)
  kappas <- numeric(0)
  for (code in canonical_codes) {
    h <- vapply(human_sets, function(s) as.integer(code %in% s), integer(1))
    a <- vapply(ai_sets,    function(s) as.integer(code %in% s), integer(1))
    if (sum(h) == 0L && sum(a) == 0L) next  # code present for neither rater
    k <- .compute_cohens_kappa(h, a)
    if (!is.na(k)) kappas <- c(kappas, k)
  }
  if (length(kappas) == 0) return(NA_real_)
  mean(kappas)
}

#' Nonparametric bootstrap 95% CI for the set-based Krippendorff alpha
#'
#' Resamples entries with replacement and recomputes alpha. Returns c(lo, hi),
#' or c(NA, NA) when the sample is too small (< 8 entries) for a meaningful
#' interval. The caller's RNG state is preserved.
#' @keywords internal
.bootstrap_alpha_ci <- function(human_sets, ai_sets,
                                n_boot = 2000L, conf = 0.95, seed = 42L) {
  n <- length(human_sets)
  if (n < 8L || n_boot < 1L) return(c(NA_real_, NA_real_))

  # Seed the bootstrap resampling without leaking RNG state into the caller's
  # session. The prior manual save/restore was conditional on .Random.seed
  # already existing, so in a fresh session (no prior RNG use) it left a fixed
  # seed behind. .with_seed restores -- or removes -- the global state correctly.
  alphas <- .with_seed(seed, vapply(seq_len(n_boot), function(b) {
    idx <- sample.int(n, n, replace = TRUE)
    .set_krippendorff_alpha(human_sets[idx], ai_sets[idx])
  }, numeric(1)))
  alphas <- alphas[is.finite(alphas)]
  if (length(alphas) < 2L) return(c(NA_real_, NA_real_))

  a <- (1 - conf) / 2
  unname(stats::quantile(alphas, c(a, 1 - a), names = FALSE, na.rm = TRUE))
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
