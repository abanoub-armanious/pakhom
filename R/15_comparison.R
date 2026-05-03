# ==============================================================================
# Cross-Run Comparison Module
# ==============================================================================
# Compares the current analysis run against all previous runs to track
# theme evolution, sentiment drift, code stability, correlation persistence,
# entry migration, and sample overlap across analytical iterations.
#
# Output schema versioning
# ------------------------
# Every run writes its schema version to run_metadata.json under the key
# `analysis_schema_version`. .load_run_snapshot reads this and tags the
# snapshot with `schema_compatible = TRUE` only if the major version matches
# .SCHEMA_VERSION (the version this package writes). compare_runs filters
# out incompatible snapshots before any comparison function runs and warns
# the user about which runs were dropped.
#
# Version semantics:
#   "<major>.<minor>"
#     major bump = breaking change (column removed, semantics changed)
#     minor bump = backward-compatible addition (new column added)
#
# Schema 1.0 columns expected in sentiment_scores.csv:
#   std_id, std_text, sentiment_score, all_emotions, emotion_intensity,
#   confidence, emerged_themes, n_themes, theme_membership_<ThemeName> (one per theme)
#
# Snapshots without an analysis_schema_version field (i.e. produced before
# this versioning was introduced) are treated as incompatible -- they're
# from an older output schema and silent NA-padded comparisons against them
# would mislead users. Such old runs should be archived or migrated.
#
# Schema 1.1 (Sprint-4 T1.4) -- backward-compatible additions:
#   * Audit log records now carry a methodology_mode field (auto-stamped from
#     config$methodology$mode at init_audit_log time). Pre-1.1 logs have no
#     such field and summarize_audit_log() reports methodology_modes_observed
#     as character(0) for them.
#   * New ai_request audit decision_type captures every ai_complete() call
#     with model + usage + finish_reason + prompt_hash + request_id +
#     raw_response_path. Pre-1.1 logs have no ai_request records and the
#     ai_requests_by_model / total_tokens_used fields surface as zero.
#   * Per-run api_responses/{prompt_hash}.json directory holds the raw API
#     responses (gated by config$audit$capture_raw_responses). This is the
#     load-bearing input for the upcoming OS.5 replay_run() feature.
#   * sentiment_scores.csv columns (the structural artifact comparison reads)
#     are unchanged from 1.0; runs labelled 1.0 and 1.1 remain comparable
#     because .schema_is_compatible() matches major-version only and these
#     new fields live in audit artifacts, not in the compared snapshots.
# ==============================================================================

#' Current output schema version this package writes
#' @keywords internal
.SCHEMA_VERSION <- "1.1"

#' Check whether a snapshot's schema version is compatible with the current package
#'
#' Compatibility is defined as identical major-version. Minor-version
#' increments are backward-compatible additions and remain comparable.
#'
#' @param snapshot_version Character string like "1.0" or NULL
#' @param current_version  Character string (defaults to .SCHEMA_VERSION)
#' @return Logical TRUE if compatible, FALSE otherwise
#' @keywords internal
.schema_is_compatible <- function(snapshot_version, current_version = .SCHEMA_VERSION) {
  if (is.null(snapshot_version) || is.na(snapshot_version) ||
      !is.character(snapshot_version) || nchar(snapshot_version) == 0) {
    return(FALSE)
  }
  snap_major <- strsplit(snapshot_version, "\\.")[[1]][1]
  curr_major <- strsplit(current_version, "\\.")[[1]][1]
  identical(snap_major, curr_major)
}

#' Compare the current run against all previous runs
#'
#' Discovers all timestamped run directories, loads their exported artifacts,
#' and performs seven comparison analyses covering sample overlap, sentiment
#' drift, code stability, theme evolution, entry migration, correlation
#' stability, and a run summary dashboard.
#'
#' @param current_dir Path to the current run's output directory
#' @param results_base Path to the parent directory containing all run folders
#' @param config ThematicConfig object (or NULL)
#' @return A ComparisonResult S3 object, or NULL if fewer than 2 runs exist
#' @export
compare_runs <- function(current_dir, results_base, config = NULL) {
  threshold <- config$output$comparison_similarity_threshold %||% 0.75

  # Discover all run directories

  all_dirs <- .discover_run_dirs(results_base)

  if (length(all_dirs) < 2) {
    log_info("Only {length(all_dirs)} run(s) found -- skipping comparison")
    return(NULL)
  }

  log_info("Found {length(all_dirs)} runs for comparison")

  # Load snapshots
  snapshots <- list()
  for (d in all_dirs) {
    snap <- tryCatch(.load_run_snapshot(d), error = function(e) {
      log_warn("Could not load run '{basename(d)}': {e$message}")
      NULL
    })
    if (!is.null(snap)) snapshots[[length(snapshots) + 1]] <- snap
  }

  if (length(snapshots) < 2) {
    log_warn("Fewer than 2 runs loaded successfully -- skipping comparison")
    return(NULL)
  }

  # Schema compatibility filter: drop snapshots whose output schema doesn't
  # match the current package's. Comparing across schemas would silently NA-pad
  # several panels (since pre-1.0 runs lack all_emotions / emerged_themes /
  # theme_membership_* columns) and quietly mislead the user.
  excluded <- vapply(snapshots, function(s) !isTRUE(s$schema_compatible), logical(1))
  if (any(excluded)) {
    excluded_runs <- vapply(snapshots[excluded], function(s) {
      ver <- if (is.null(s$schema_version)) "<missing>" else s$schema_version
      sprintf("%s (schema=%s)", s$run_id, ver)
    }, character(1))
    log_warn("Excluding {sum(excluded)} run(s) from comparison: incompatible output schema (current={.SCHEMA_VERSION})")
    for (er in excluded_runs) log_warn("  - {er}")
    log_warn("Migrate or archive older runs to remove this warning. See ?compare_runs.")
    snapshots <- snapshots[!excluded]
    if (length(snapshots) < 2) {
      log_warn("Fewer than 2 schema-compatible runs remain -- skipping comparison")
      return(NULL)
    }
  }

  current <- snapshots[[length(snapshots)]]
  previous <- snapshots[[length(snapshots) - 1]]

  # Run the seven comparison functions in order
  log_info("Comparing samples...")
  sample_overlap <- tryCatch(
    .compare_samples(snapshots, current),
    error = function(e) { log_warn("Sample comparison failed: {e$message}"); NULL }
  )

  log_info("Comparing sentiment...")
  sentiment_drift <- tryCatch(
    .compare_sentiment(snapshots, current),
    error = function(e) { log_warn("Sentiment comparison failed: {e$message}"); NULL }
  )

  log_info("Comparing codes...")
  code_stability <- tryCatch(
    .compare_codes(snapshots, current, threshold = threshold),
    error = function(e) { log_warn("Code comparison failed: {e$message}"); NULL }
  )

  log_info("Comparing themes...")
  theme_evolution <- tryCatch(
    .compare_themes(snapshots, current, threshold = threshold),
    error = function(e) { log_warn("Theme comparison failed: {e$message}"); NULL }
  )

  # Entry migration needs theme matches from the pairwise comparison
  log_info("Analyzing entry migration...")
  theme_matches <- if (!is.null(theme_evolution)) theme_evolution$pairwise else NULL
  entry_migration <- tryCatch(
    .compare_entry_migration(current, previous, theme_matches),
    error = function(e) { log_warn("Entry migration failed: {e$message}"); NULL }
  )

  log_info("Comparing correlations...")
  correlation_stability <- tryCatch(
    .compare_correlations(snapshots, current),
    error = function(e) { log_warn("Correlation comparison failed: {e$message}"); NULL }
  )

  log_info("Building run dashboard...")
  dashboard <- tryCatch(
    .build_run_dashboard(snapshots),
    error = function(e) { log_warn("Dashboard failed: {e$message}"); NULL }
  )

  # Detect inter-model runs (different providers/models across snapshots)
  models_used <- list()
  for (snap in snapshots) {
    if (!is.null(snap$metadata)) {
      model_key <- paste(snap$metadata$provider %||% "unknown",
                          snap$metadata$model_primary %||% "unknown", sep = "/")
      models_used[[snap$run_id]] <- model_key
    }
  }
  unique_models <- unique(unlist(models_used))
  is_inter_model <- length(unique_models) >= 2

  if (is_inter_model) {
    log_info("Inter-model comparison detected: {paste(unique_models, collapse = ', ')}")
  }

  result <- .create_comparison_result(
    dashboard = dashboard,
    sample_overlap = sample_overlap,
    theme_evolution = theme_evolution,
    sentiment_drift = sentiment_drift,
    code_stability = code_stability,
    correlation_stability = correlation_stability,
    entry_migration = entry_migration,
    n_runs = length(snapshots),
    current_run = current$run_id
  )
  result$is_inter_model <- is_inter_model
  result$models_used <- models_used
  result$unique_models <- unique_models
  result
}

# ==============================================================================
# Run Discovery & Loading
# ==============================================================================

#' List available analysis runs
#'
#' Scans a results directory for timestamped run folders and returns a summary
#' tibble with run IDs, dates, paths, and output-schema versions. The
#' `schema_compatible` column flags whether each run can participate in
#' \code{\link{compare_runs}} given the current package's schema version.
#'
#' @param results_base Path to the parent directory containing all run folders
#' @return A tibble with columns: run_id, date, path, schema_version,
#'   schema_compatible
#' @export
list_available_runs <- function(results_base) {
  dirs <- .discover_run_dirs(results_base)
  if (length(dirs) == 0) {
    log_info("No runs found in '{results_base}'")
    return(tibble(
      run_id = character(),
      date = as.Date(character()),
      path = character(),
      schema_version = character(),
      schema_compatible = logical()
    ))
  }

  run_ids <- basename(dirs)
  dates <- as.Date(sub("^run_(\\d{4}-\\d{2}-\\d{2})_\\d{6}$", "\\1", run_ids))

  # Cheaply read just run_metadata.json to get schema info, without loading
  # full snapshots
  schema_versions <- vapply(dirs, function(d) {
    f <- file.path(d, "run_metadata.json")
    if (!file.exists(f)) return(NA_character_)
    md <- tryCatch(jsonlite::fromJSON(f), error = function(e) NULL)
    v <- if (is.list(md)) md$analysis_schema_version else NULL
    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)
  }, character(1))

  schema_compatible <- vapply(schema_versions, .schema_is_compatible, logical(1))

  result <- tibble(
    run_id = run_ids,
    date = dates,
    path = dirs,
    schema_version = schema_versions,
    schema_compatible = schema_compatible
  )

  n_compat <- sum(schema_compatible)
  log_info("Found {nrow(result)} run(s) in '{results_base}' ({n_compat} schema-compatible with current package)")
  result
}

#' Find all timestamped run directories in chronological order
#' @param results_base Base results directory
#' @return Character vector of full paths, oldest first
#' @keywords internal
.discover_run_dirs <- function(results_base) {
  if (!dir.exists(results_base)) return(character(0))

  all_dirs <- list.dirs(results_base, full.names = FALSE, recursive = FALSE)
  run_dirs <- grep("^run_\\d{4}-\\d{2}-\\d{2}_\\d{6}$", all_dirs, value = TRUE)

  if (length(run_dirs) == 0) return(character(0))

  # Sort chronologically (oldest first)
  run_dirs <- sort(run_dirs)

  # Filter out incomplete runs (must have themes.json to be considered complete)
  full_paths <- file.path(results_base, run_dirs)
  has_themes <- vapply(full_paths, function(d) {
    file.exists(file.path(d, "themes.json"))
  }, logical(1))
  run_dirs <- run_dirs[has_themes]

  if (length(run_dirs) == 0) return(character(0))
  file.path(results_base, run_dirs)
}

#' Load a single run's artifacts from disk
#' @param run_dir Path to one run folder
#' @return A RunSnapshot list
#' @keywords internal
.load_run_snapshot <- function(run_dir) {
  run_id <- basename(run_dir)

  # Parse timestamp from folder name
  ts_str <- sub("^run_", "", run_id)
  timestamp <- tryCatch(
    as.POSIXct(ts_str, format = "%Y-%m-%d_%H%M%S"),
    error = function(e) Sys.time()
  )

  # Load each file, NULL if missing
  themes <- tryCatch({
    f <- file.path(run_dir, "themes.json")
    if (file.exists(f)) tibble::as_tibble(jsonlite::fromJSON(f)) else NULL
  }, error = function(e) NULL)

  sentiment <- tryCatch({
    f <- file.path(run_dir, "sentiment_scores.csv")
    if (file.exists(f)) readr::read_csv(f, show_col_types = FALSE) else NULL
  }, error = function(e) NULL)

  codes <- tryCatch({
    f <- file.path(run_dir, "consolidated_codes.csv")
    if (file.exists(f)) readr::read_csv(f, show_col_types = FALSE) else NULL
  }, error = function(e) NULL)

  correlations <- tryCatch({
    f <- file.path(run_dir, "correlations.csv")
    if (file.exists(f)) readr::read_csv(f, show_col_types = FALSE) else NULL
  }, error = function(e) NULL)

  # Load run metadata (provider/model info for inter-model comparison)
  metadata <- tryCatch({
    f <- file.path(run_dir, "run_metadata.json")
    if (file.exists(f)) jsonlite::fromJSON(f) else NULL
  }, error = function(e) NULL)

  # Determine schema compatibility (see header comment for semantics)
  snapshot_version <- if (is.list(metadata)) metadata$analysis_schema_version else NULL
  schema_compatible <- .schema_is_compatible(snapshot_version)

  list(
    run_id = run_id,
    timestamp = timestamp,
    themes = themes,
    sentiment = sentiment,
    codes = codes,
    correlations = correlations,
    metadata = metadata,
    schema_version = snapshot_version,
    schema_compatible = schema_compatible,
    dir = run_dir
  )
}

# ==============================================================================
# 1. Sample Overlap
# ==============================================================================

#' Compare analytical samples across runs
#' @param snapshots List of RunSnapshot objects (chronological)
#' @param current RunSnapshot for the current run
#' @return List with per_run, pairwise, text_changes, interpretation
#' @keywords internal
.compare_samples <- function(snapshots, current) {
  previous <- snapshots[[length(snapshots) - 1]]

  # Per-run summary
  per_run <- tibble::tibble(
    run_id = vapply(snapshots, function(s) s$run_id, character(1)),
    total_entries = vapply(snapshots, function(s) {
      if (!is.null(s$sentiment)) nrow(s$sentiment) else 0L
    }, integer(1))
  )

  # Add source breakdown if source_table column exists
  per_run$n_from_posts <- vapply(snapshots, function(s) {
    if (!is.null(s$sentiment) && "source_table" %in% names(s$sentiment)) {
      sum(s$sentiment$source_table == "posts", na.rm = TRUE)
    } else NA_integer_
  }, integer(1))

  per_run$n_from_comments <- vapply(snapshots, function(s) {
    if (!is.null(s$sentiment) && "source_table" %in% names(s$sentiment)) {
      sum(s$sentiment$source_table == "comments", na.rm = TRUE)
    } else NA_integer_
  }, integer(1))

  per_run$posts_pct <- ifelse(
    per_run$total_entries > 0 & !is.na(per_run$n_from_posts),
    round(per_run$n_from_posts / per_run$total_entries * 100, 1),
    NA_real_
  )

  # Pairwise comparison (current vs previous)
  pairwise <- list(
    n_shared = 0L, n_new = 0L, n_dropped = 0L,
    jaccard_index = 0, pct_shared = 0
  )
  text_changes <- 0L

  if (!is.null(current$sentiment) && !is.null(previous$sentiment) &&
      "std_id" %in% names(current$sentiment) && "std_id" %in% names(previous$sentiment)) {

    curr_ids <- current$sentiment$std_id
    prev_ids <- previous$sentiment$std_id

    shared <- intersect(curr_ids, prev_ids)
    union_ids <- union(curr_ids, prev_ids)

    pairwise <- list(
      n_shared = length(shared),
      n_new = length(setdiff(curr_ids, prev_ids)),
      n_dropped = length(setdiff(prev_ids, curr_ids)),
      jaccard_index = if (length(union_ids) > 0) round(length(shared) / length(union_ids), 3) else 0,
      pct_shared = if (length(curr_ids) > 0) round(length(shared) / length(curr_ids) * 100, 1) else 0
    )

    # Check for text changes in shared entries
    if (length(shared) > 0 && "std_text" %in% names(current$sentiment) &&
        "std_text" %in% names(previous$sentiment)) {
      curr_texts <- current$sentiment |>
        dplyr::filter(.data$std_id %in% shared) |>
        dplyr::arrange(.data$std_id) |>
        dplyr::pull(.data$std_text)
      prev_texts <- previous$sentiment |>
        dplyr::filter(.data$std_id %in% shared) |>
        dplyr::arrange(.data$std_id) |>
        dplyr::pull(.data$std_text)
      text_changes <- sum(curr_texts != prev_texts, na.rm = TRUE)
    }
  }

  # Interpretation
  ji <- pairwise$jaccard_index
  interpretation <- if (ji >= 1.0) {
    "identical sample"
  } else if (ji >= 0.9) {
    "mostly same sample"
  } else if (ji >= 0.5) {
    "overlapping samples"
  } else {
    "largely different samples"
  }

  list(
    per_run = per_run,
    pairwise = pairwise,
    text_changes = text_changes,
    interpretation = interpretation
  )
}

# ==============================================================================
# 2. Sentiment Drift
# ==============================================================================

#' Track sentiment drift across runs
#' @param snapshots List of RunSnapshot objects
#' @param current RunSnapshot for the current run
#' @return List with per_run, per_entry, summary
#' @keywords internal
.compare_sentiment <- function(snapshots, current) {
  # Per-run aggregation
  per_run_list <- lapply(snapshots, function(s) {
    if (is.null(s$sentiment) || !"sentiment_score" %in% names(s$sentiment)) {
      return(tibble::tibble(
        run_id = s$run_id, mean_sentiment = NA_real_, median_sentiment = NA_real_,
        sd_sentiment = NA_real_, top_emotions = NA_character_,
        pct_negative = NA_real_, pct_positive = NA_real_
      ))
    }
    scores <- s$sentiment$sentiment_score

    # Multi-label emotion distribution: split all_emotions and count each
    emo_col <- if ("all_emotions" %in% names(s$sentiment)) "all_emotions"
               else NULL
    emotion_dist <- if (!is.null(emo_col)) {
      raw <- s$sentiment[[emo_col]][!is.na(s$sentiment[[emo_col]])]
      all_labels <- trimws(unlist(strsplit(raw, ";\\s*")))
      all_labels <- all_labels[nchar(all_labels) > 0]
      if (length(all_labels) > 0) {
        tbl <- sort(table(all_labels), decreasing = TRUE)
        paste0(names(tbl)[seq_len(min(3, length(tbl)))], collapse = "; ")
      } else NA_character_
    } else NA_character_

    tibble::tibble(
      run_id = s$run_id,
      mean_sentiment = round(mean(scores, na.rm = TRUE), 3),
      median_sentiment = round(stats::median(scores, na.rm = TRUE), 3),
      sd_sentiment = round(stats::sd(scores, na.rm = TRUE), 3),
      top_emotions = emotion_dist,
      pct_negative = round(sum(scores < 0, na.rm = TRUE) / sum(!is.na(scores)) * 100, 1),
      pct_positive = round(sum(scores > 0, na.rm = TRUE) / sum(!is.na(scores)) * 100, 1)
    )
  })
  per_run <- dplyr::bind_rows(per_run_list)

  # Per-entry comparison (current vs previous)
  previous <- snapshots[[length(snapshots) - 1]]
  per_entry <- tibble::tibble()
  summary_stats <- list(mean_shift = NA_real_, reclassification_rate = NA_real_, n_shared_entries = 0L)

  if (!is.null(current$sentiment) && !is.null(previous$sentiment) &&
      "std_id" %in% names(current$sentiment) && "std_id" %in% names(previous$sentiment) &&
      "sentiment_score" %in% names(current$sentiment) && "sentiment_score" %in% names(previous$sentiment)) {

    # Use all_emotions (multi-label)
    emo_col <- if ("all_emotions" %in% names(current$sentiment)) "all_emotions"
               else NULL

    select_cols <- c("std_id", "sentiment_score")
    if (!is.null(emo_col)) select_cols <- c(select_cols, emo_col)

    curr <- current$sentiment |> dplyr::select(dplyr::any_of(select_cols))
    prev <- previous$sentiment |> dplyr::select(dplyr::any_of(select_cols))

    joined <- dplyr::inner_join(
      curr, prev, by = "std_id", suffix = c("_curr", "_prev")
    )

    if (nrow(joined) > 0) {
      emo_curr_col <- paste0(emo_col, "_curr")
      emo_prev_col <- paste0(emo_col, "_prev")

      per_entry <- joined |>
        dplyr::mutate(
          shift = .data$sentiment_score_curr - .data$sentiment_score_prev,
          # Multi-label emotion comparison: compute Jaccard distance
          reclassified = if (!is.null(emo_col) &&
                             emo_curr_col %in% names(joined) &&
                             emo_prev_col %in% names(joined)) {
            mapply(function(c, p) {
              if (is.na(c) || is.na(p)) return(NA)
              c_set <- trimws(unlist(strsplit(c, ";\\s*")))
              p_set <- trimws(unlist(strsplit(p, ";\\s*")))
              # Jaccard: if sets are identical, not reclassified
              union_size <- length(union(c_set, p_set))
              if (union_size == 0) return(FALSE)
              intersect_size <- length(intersect(c_set, p_set))
              # Reclassified if less than 50% overlap (Jaccard < 0.5)
              (intersect_size / union_size) < 0.5
            }, joined[[emo_curr_col]], joined[[emo_prev_col]])
          } else {
            NA
          }
        )

      summary_stats <- list(
        mean_shift = round(mean(per_entry$shift, na.rm = TRUE), 3),
        reclassification_rate = if (any(!is.na(per_entry$reclassified))) {
          round(sum(per_entry$reclassified, na.rm = TRUE) / sum(!is.na(per_entry$reclassified)) * 100, 1)
        } else NA_real_,
        n_shared_entries = nrow(joined)
      )
    }
  }

  list(per_run = per_run, per_entry = per_entry, summary = summary_stats)
}

# ==============================================================================
# 3. Code Stability
# ==============================================================================

#' Compare consolidated codes across runs
#' @param snapshots List of RunSnapshot objects
#' @param current RunSnapshot for the current run
#' @param threshold Jaro-Winkler threshold
#' @return List with pairwise, stability, all_runs
#' @keywords internal
.compare_codes <- function(snapshots, current, threshold = 0.75) {
  previous <- snapshots[[length(snapshots) - 1]]

  # Pairwise comparison
  pairwise <- list(stable = tibble::tibble(), renamed = tibble::tibble(),
                   new = tibble::tibble(), dropped = tibble::tibble())
  stability <- list(jaccard_overall = 0, n_stable = 0L, n_new = 0L, n_dropped = 0L, churn_rate = 1.0)

  if (!is.null(current$codes) && !is.null(previous$codes) &&
      "code_text" %in% names(current$codes) && "code_text" %in% names(previous$codes)) {

    curr_texts <- tolower(trimws(current$codes$code_text))
    prev_texts <- tolower(trimws(previous$codes$code_text))

    # Fuzzy match codes
    matched_curr <- rep(FALSE, length(curr_texts))
    matched_prev <- rep(FALSE, length(prev_texts))
    stable_pairs <- list()
    renamed_pairs <- list()

    if (requireNamespace("stringdist", quietly = TRUE) &&
        length(curr_texts) > 0 && length(prev_texts) > 0) {

      # Build similarity matrix
      sim_matrix <- 1 - stringdist::stringdistmatrix(
        curr_texts, prev_texts, method = "jw"
      )

      # Greedy best-match
      while (TRUE) {
        max_sim <- max(sim_matrix, na.rm = TRUE)
        if (max_sim < threshold * 0.8) break

        idx <- which(sim_matrix == max_sim, arr.ind = TRUE)[1, , drop = FALSE]
        ci <- idx[1, 1]
        pi <- idx[1, 2]

        matched_curr[ci] <- TRUE
        matched_prev[pi] <- TRUE

        pair <- list(
          code_prev = previous$codes$code_text[pi],
          code_curr = current$codes$code_text[ci],
          similarity = round(max_sim, 3),
          freq_prev = if ("frequency" %in% names(previous$codes)) previous$codes$frequency[pi] else NA_integer_,
          freq_curr = if ("frequency" %in% names(current$codes)) current$codes$frequency[ci] else NA_integer_
        )

        if (max_sim >= 0.95) {
          stable_pairs[[length(stable_pairs) + 1]] <- pair
        } else {
          renamed_pairs[[length(renamed_pairs) + 1]] <- pair
        }

        sim_matrix[ci, ] <- -1
        sim_matrix[, pi] <- -1
      }
    }

    pairwise$stable <- if (length(stable_pairs) > 0) dplyr::bind_rows(stable_pairs) else tibble::tibble()
    pairwise$renamed <- if (length(renamed_pairs) > 0) dplyr::bind_rows(renamed_pairs) else tibble::tibble()

    # New and dropped codes
    new_idx <- which(!matched_curr)
    dropped_idx <- which(!matched_prev)

    if (length(new_idx) > 0) {
      pairwise$new <- tibble::tibble(
        code_text = current$codes$code_text[new_idx],
        frequency = if ("frequency" %in% names(current$codes)) current$codes$frequency[new_idx] else NA_integer_
      )
    }
    if (length(dropped_idx) > 0) {
      pairwise$dropped <- tibble::tibble(
        code_text = previous$codes$code_text[dropped_idx],
        frequency = if ("frequency" %in% names(previous$codes)) previous$codes$frequency[dropped_idx] else NA_integer_
      )
    }

    # Stability metrics
    n_stable <- nrow(pairwise$stable) + nrow(pairwise$renamed)
    n_new <- nrow(pairwise$new)
    n_dropped <- nrow(pairwise$dropped)
    total <- n_stable + n_new + n_dropped

    # Jaccard on exact code text
    jaccard <- .code_jaccard(curr_texts, prev_texts)

    stability <- list(
      jaccard_overall = jaccard,
      n_stable = as.integer(nrow(pairwise$stable)),
      n_renamed = as.integer(nrow(pairwise$renamed)),
      n_new = as.integer(n_new),
      n_dropped = as.integer(n_dropped),
      churn_rate = if (total > 0) round((n_new + n_dropped) / total, 3) else 0
    )
  }

  # Track codes across all runs
  all_runs <- .track_codes_across_runs(snapshots, threshold)

  list(pairwise = pairwise, stability = stability, all_runs = all_runs)
}

#' Track which code clusters appear across multiple runs
#' @keywords internal
.track_codes_across_runs <- function(snapshots, threshold = 0.75) {
  if (length(snapshots) < 2) return(tibble::tibble())

  # Collect all unique code texts across runs
  all_codes <- list()
  for (i in seq_along(snapshots)) {
    s <- snapshots[[i]]
    if (!is.null(s$codes) && "code_text" %in% names(s$codes)) {
      for (ct in s$codes$code_text) {
        all_codes[[length(all_codes) + 1]] <- list(
          code_text = ct,
          run_id = s$run_id,
          run_idx = i
        )
      }
    }
  }

  if (length(all_codes) == 0) return(tibble::tibble())

  dplyr::bind_rows(all_codes) |>
    dplyr::group_by(.data$code_text) |>
    dplyr::summarise(
      n_runs_present = dplyr::n_distinct(.data$run_id),
      first_seen = min(.data$run_idx),
      last_seen = max(.data$run_idx),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$n_runs_present))
}

#' Compute Jaccard similarity between two sets of strings
#' @param codes_a Character vector or semicolon-separated string
#' @param codes_b Character vector or semicolon-separated string
#' @return Numeric between 0 and 1
#' @keywords internal
.code_jaccard <- function(codes_a, codes_b) {
  # Handle semicolon-separated inputs
  if (length(codes_a) == 1 && grepl(";", codes_a)) {
    codes_a <- trimws(strsplit(codes_a, ";")[[1]])
  }
  if (length(codes_b) == 1 && grepl(";", codes_b)) {
    codes_b <- trimws(strsplit(codes_b, ";")[[1]])
  }

  codes_a <- tolower(trimws(codes_a))
  codes_b <- tolower(trimws(codes_b))
  codes_a <- codes_a[nchar(codes_a) > 0]
  codes_b <- codes_b[nchar(codes_b) > 0]

  if (length(codes_a) == 0 && length(codes_b) == 0) return(1)
  if (length(codes_a) == 0 || length(codes_b) == 0) return(0)

  n_intersect <- length(intersect(codes_a, codes_b))
  n_union <- length(union(codes_a, codes_b))

  if (n_union == 0) return(0)
  round(n_intersect / n_union, 3)
}

# ==============================================================================
# 4. Theme Evolution
# ==============================================================================

#' Compare themes across runs using fuzzy matching
#' @param snapshots List of RunSnapshot objects
#' @param current RunSnapshot for the current run
#' @param threshold Jaro-Winkler similarity threshold
#' @return List with pairwise and timeline
#' @keywords internal
.compare_themes <- function(snapshots, current, threshold = 0.75) {
  previous <- snapshots[[length(snapshots) - 1]]

  # Pairwise comparison (current vs previous)
  pairwise <- .match_themes_pairwise(
    previous$themes, current$themes, threshold
  )

  # Timeline: track themes across all runs
  timeline <- .build_theme_timeline(snapshots, threshold)

  list(pairwise = pairwise, timeline = timeline)
}

#' Fuzzy-match themes between two runs
#' @param themes_a tibble of themes from previous run
#' @param themes_b tibble of themes from current run
#' @param threshold Combined similarity threshold
#' @return List with persisted, new, disappeared
#' @keywords internal
.match_themes_pairwise <- function(themes_a, themes_b, threshold = 0.75) {
  result <- list(
    persisted = tibble::tibble(),
    new = tibble::tibble(),
    disappeared = tibble::tibble()
  )

  if (is.null(themes_a) || is.null(themes_b) ||
      !"name" %in% names(themes_a) || !"name" %in% names(themes_b) ||
      nrow(themes_a) == 0 || nrow(themes_b) == 0) {
    return(result)
  }

  names_a <- themes_a$name
  names_b <- themes_b$name

  if (!requireNamespace("stringdist", quietly = TRUE)) {
    log_warn("stringdist not available -- using exact name matching")
    shared <- intersect(names_a, names_b)
    result$persisted <- tibble::tibble(
      theme_prev = shared, theme_curr = shared,
      name_sim = 1.0, code_jaccard = NA_real_,
      entry_count_prev = NA_integer_, entry_count_curr = NA_integer_,
      sentiment_prev = NA_character_, sentiment_curr = NA_character_
    )
    result$new <- tibble::tibble(theme_name = setdiff(names_b, names_a))
    result$disappeared <- tibble::tibble(theme_name = setdiff(names_a, names_b))
    return(result)
  }

  # Compute name similarity matrix
  name_sim <- 1 - stringdist::stringdistmatrix(
    tolower(names_a), tolower(names_b), method = "jw"
  )

  # Compute code Jaccard for each pair
  codes_a <- if ("codes_included" %in% names(themes_a)) themes_a$codes_included else rep("", length(names_a))
  codes_b <- if ("codes_included" %in% names(themes_b)) themes_b$codes_included else rep("", length(names_b))

  code_sim <- matrix(0, nrow = length(names_a), ncol = length(names_b))
  for (i in seq_along(names_a)) {
    for (j in seq_along(names_b)) {
      code_sim[i, j] <- .code_jaccard(
        codes_a[i] %||% "", codes_b[j] %||% ""
      )
    }
  }

  # Combined score
  combined <- 0.6 * name_sim + 0.4 * code_sim

  # Greedy best-match
  matched_a <- rep(FALSE, length(names_a))
  matched_b <- rep(FALSE, length(names_b))
  persisted_list <- list()

  while (TRUE) {
    max_val <- max(combined, na.rm = TRUE)
    if (max_val < threshold) break

    idx <- which(combined == max_val, arr.ind = TRUE)[1, , drop = FALSE]
    ai <- idx[1, 1]
    bi <- idx[1, 2]

    matched_a[ai] <- TRUE
    matched_b[bi] <- TRUE

    ec_prev <- if ("entry_count" %in% names(themes_a)) themes_a$entry_count[ai] else NA_integer_
    ec_curr <- if ("entry_count" %in% names(themes_b)) themes_b$entry_count[bi] else NA_integer_
    st_prev <- if ("sentiment_tendency" %in% names(themes_a)) themes_a$sentiment_tendency[ai] else NA_character_
    st_curr <- if ("sentiment_tendency" %in% names(themes_b)) themes_b$sentiment_tendency[bi] else NA_character_

    persisted_list[[length(persisted_list) + 1]] <- list(
      theme_prev = names_a[ai], theme_curr = names_b[bi],
      name_sim = round(name_sim[ai, bi], 3),
      code_jaccard = round(code_sim[ai, bi], 3),
      entry_count_prev = as.integer(ec_prev),
      entry_count_curr = as.integer(ec_curr),
      sentiment_prev = as.character(st_prev),
      sentiment_curr = as.character(st_curr)
    )

    combined[ai, ] <- -1
    combined[, bi] <- -1
  }

  result$persisted <- if (length(persisted_list) > 0) dplyr::bind_rows(persisted_list) else tibble::tibble()

  # New themes (unmatched in current)
  new_idx <- which(!matched_b)
  if (length(new_idx) > 0) {
    result$new <- tibble::tibble(
      theme_name = names_b[new_idx],
      entry_count = if ("entry_count" %in% names(themes_b)) as.integer(themes_b$entry_count[new_idx]) else NA_integer_,
      sentiment_tendency = if ("sentiment_tendency" %in% names(themes_b)) themes_b$sentiment_tendency[new_idx] else NA_character_
    )
  }

  # Disappeared themes (unmatched in previous)
  disappeared_idx <- which(!matched_a)
  if (length(disappeared_idx) > 0) {
    result$disappeared <- tibble::tibble(
      theme_name = names_a[disappeared_idx],
      entry_count = if ("entry_count" %in% names(themes_a)) as.integer(themes_a$entry_count[disappeared_idx]) else NA_integer_,
      sentiment_tendency = if ("sentiment_tendency" %in% names(themes_a)) themes_a$sentiment_tendency[disappeared_idx] else NA_character_
    )
  }

  result
}

#' Build a timeline of theme clusters across all runs
#' @keywords internal
.build_theme_timeline <- function(snapshots, threshold = 0.75) {
  if (length(snapshots) < 2) return(tibble::tibble())

  rows <- list()
  for (i in seq_along(snapshots)) {
    s <- snapshots[[i]]
    if (!is.null(s$themes) && "name" %in% names(s$themes)) {
      for (j in seq_len(nrow(s$themes))) {
        rows[[length(rows) + 1]] <- tibble::tibble(
          run_id = s$run_id,
          run_idx = i,
          theme_name = s$themes$name[j],
          entry_count = if ("entry_count" %in% names(s$themes)) as.integer(s$themes$entry_count[j]) else NA_integer_,
          prevalence = if ("prevalence" %in% names(s$themes)) s$themes$prevalence[j] else NA_character_,
          sentiment_tendency = if ("sentiment_tendency" %in% names(s$themes)) s$themes$sentiment_tendency[j] else NA_character_
        )
      }
    }
  }

  if (length(rows) == 0) return(tibble::tibble())
  dplyr::bind_rows(rows)
}

# ==============================================================================
# 5. Entry Migration
# ==============================================================================

#' Build entry migration matrix between current and previous run
#' @param current RunSnapshot
#' @param previous RunSnapshot
#' @param theme_matches Pairwise theme match result (or NULL)
#' @return List with matrix, stability_rate, counts
#' @keywords internal
.compare_entry_migration <- function(current, previous, theme_matches = NULL) {
  result <- list(
    matrix = tibble::tibble(),
    stability_rate = NA_real_,
    n_migrated = 0L, n_stable = 0L,
    n_new_entries = 0L, n_dropped_entries = 0L
  )

  # Use emerged_themes (multi-label) for migration tracking
  if (is.null(current$sentiment) || is.null(previous$sentiment) ||
      !"std_id" %in% names(current$sentiment) || !"std_id" %in% names(previous$sentiment) ||
      !"emerged_themes" %in% names(current$sentiment) || !"emerged_themes" %in% names(previous$sentiment)) {
    return(result)
  }

  curr <- current$sentiment |> dplyr::select("std_id", theme_curr = "emerged_themes")
  prev <- previous$sentiment |> dplyr::select("std_id", theme_prev = "emerged_themes")

  joined <- dplyr::inner_join(curr, prev, by = "std_id")

  if (nrow(joined) == 0) return(result)

  # Build migration matrix
  migration <- joined |>
    dplyr::count(.data$theme_prev, .data$theme_curr, name = "n_entries")

  # Determine stable entries (same theme or matched theme)
  if (!is.null(theme_matches) && nrow(theme_matches$persisted) > 0) {
    # Use theme matching to account for renamed themes
    theme_map <- stats::setNames(
      theme_matches$persisted$theme_prev,
      theme_matches$persisted$theme_curr
    )
    joined$theme_prev_mapped <- ifelse(
      joined$theme_curr %in% names(theme_map),
      theme_map[joined$theme_curr],
      joined$theme_curr
    )
    n_stable <- sum(joined$theme_prev == joined$theme_prev_mapped, na.rm = TRUE)
  } else {
    n_stable <- sum(joined$theme_prev == joined$theme_curr, na.rm = TRUE)
  }

  n_total <- nrow(joined)
  n_migrated <- n_total - n_stable

  result$matrix <- migration
  result$stability_rate <- round(n_stable / n_total, 3)
  result$n_migrated <- as.integer(n_migrated)
  result$n_stable <- as.integer(n_stable)
  result$n_new_entries <- as.integer(length(setdiff(curr$std_id, prev$std_id)))
  result$n_dropped_entries <- as.integer(length(setdiff(prev$std_id, curr$std_id)))

  result
}

# ==============================================================================
# 6. Correlation Stability
# ==============================================================================

#' Compare significant correlations across runs
#' @param snapshots List of RunSnapshot objects
#' @param current RunSnapshot for the current run
#' @return List with persistent, intermittent, run_specific, trends
#' @keywords internal
.compare_correlations <- function(snapshots, current) {
  result <- list(
    persistent = tibble::tibble(),
    intermittent = tibble::tibble(),
    run_specific = tibble::tibble(),
    trends = tibble::tibble()
  )

  # Collect all correlations with normalized variable names
  all_corr <- list()
  for (s in snapshots) {
    if (is.null(s$correlations) || !"var1" %in% names(s$correlations)) next

    for (i in seq_len(nrow(s$correlations))) {
      v1 <- .normalize_corr_var(s$correlations$var1[i])
      v2 <- .normalize_corr_var(s$correlations$var2[i])
      # Sort pair alphabetically for consistent keys
      pair <- sort(c(v1, v2))

      all_corr[[length(all_corr) + 1]] <- tibble::tibble(
        var1 = pair[1], var2 = pair[2],
        pair_key = paste(pair, collapse = " <-> "),
        run_id = s$run_id,
        correlation = if ("correlation" %in% names(s$correlations)) s$correlations$correlation[i] else NA_real_,
        p_value = if ("p_value" %in% names(s$correlations)) s$correlations$p_value[i] else NA_real_,
        significant = if ("significant" %in% names(s$correlations)) s$correlations$significant[i] else NA
      )
    }
  }

  if (length(all_corr) == 0) return(result)

  trends <- dplyr::bind_rows(all_corr)
  result$trends <- trends

  n_runs <- length(snapshots)

  # Classify by persistence
  pair_summary <- trends |>
    dplyr::filter(.data$significant == TRUE) |>
    dplyr::group_by(.data$pair_key, .data$var1, .data$var2) |>
    dplyr::summarise(
      n_runs_significant = dplyr::n(),
      mean_correlation = round(mean(.data$correlation, na.rm = TRUE), 3),
      .groups = "drop"
    )

  if (nrow(pair_summary) > 0) {
    result$persistent <- pair_summary |>
      dplyr::filter(.data$n_runs_significant == n_runs)
    result$intermittent <- pair_summary |>
      dplyr::filter(.data$n_runs_significant > 1, .data$n_runs_significant < n_runs)
    result$run_specific <- pair_summary |>
      dplyr::filter(.data$n_runs_significant == 1)
  }

  result
}

#' Normalize correlation variable names for matching across runs
#' @keywords internal
.normalize_corr_var <- function(x) {
  x <- gsub("theme_membership_", "", x)
  x <- gsub("[_.]", " ", x)
  tolower(trimws(x))
}

# ==============================================================================
# 7. Run Summary Dashboard
# ==============================================================================

#' Build the run summary dashboard table
#' @param snapshots List of RunSnapshot objects
#' @return tibble with one row per run
#' @keywords internal
.build_run_dashboard <- function(snapshots) {
  rows <- lapply(snapshots, function(s) {
    n_themes <- if (!is.null(s$themes)) nrow(s$themes) else 0L
    n_entries <- if (!is.null(s$sentiment)) nrow(s$sentiment) else 0L
    mean_sent <- if (!is.null(s$sentiment) && "sentiment_score" %in% names(s$sentiment)) {
      round(mean(s$sentiment$sentiment_score, na.rm = TRUE), 3)
    } else NA_real_

    # Multi-label emotion distribution: show top 3 emotions
    dominant_emo <- if (!is.null(s$sentiment)) {
      emo_col <- if ("all_emotions" %in% names(s$sentiment)) "all_emotions"
                 else NULL
      if (!is.null(emo_col)) {
        raw <- s$sentiment[[emo_col]][!is.na(s$sentiment[[emo_col]])]
        all_labels <- trimws(unlist(strsplit(raw, ";\\s*")))
        all_labels <- all_labels[nchar(all_labels) > 0]
        if (length(all_labels) > 0) {
          tbl <- sort(table(all_labels), decreasing = TRUE)
          paste0(names(tbl)[seq_len(min(3, length(tbl)))], collapse = "; ")
        } else NA_character_
      } else NA_character_
    } else NA_character_

    n_sig_corr <- if (!is.null(s$correlations) && "significant" %in% names(s$correlations)) {
      sum(s$correlations$significant == TRUE, na.rm = TRUE)
    } else 0L

    n_codes <- if (!is.null(s$codes)) nrow(s$codes) else 0L

    tibble::tibble(
      run_id = s$run_id,
      date = format(s$timestamp, "%Y-%m-%d %H:%M"),
      total_entries = as.integer(n_entries),
      n_themes = as.integer(n_themes),
      mean_sentiment = mean_sent,
      top_emotions = dominant_emo,
      n_significant_correlations = as.integer(n_sig_corr),
      n_codes = as.integer(n_codes)
    )
  })

  dplyr::bind_rows(rows)
}

# ==============================================================================
# ComparisonResult S3 Class
# ==============================================================================

#' Create a ComparisonResult S3 object
#' @keywords internal
.create_comparison_result <- function(dashboard, sample_overlap, theme_evolution,
                                       sentiment_drift, code_stability,
                                       correlation_stability, entry_migration,
                                       n_runs, current_run) {
  obj <- list(
    n_runs = n_runs,
    current_run = current_run,
    sample_overlap = sample_overlap,
    dashboard = dashboard,
    theme_evolution = theme_evolution,
    sentiment_drift = sentiment_drift,
    code_stability = code_stability,
    correlation_stability = correlation_stability,
    entry_migration = entry_migration
  )
  class(obj) <- "ComparisonResult"
  obj
}

#' Print method for ComparisonResult
#' @param x ComparisonResult object
#' @param ... Additional arguments (ignored)
#' @export
print.ComparisonResult <- function(x, ...) {
  cat("=== Cross-Run Comparison ===\n")
  cat(sprintf("Runs compared: %d (current: %s)\n", x$n_runs, x$current_run))

  if (!is.null(x$sample_overlap)) {
    cat(sprintf("\nSample overlap: %s (Jaccard: %.3f)\n",
                x$sample_overlap$interpretation,
                x$sample_overlap$pairwise$jaccard_index))
  }

  if (!is.null(x$dashboard) && nrow(x$dashboard) > 0) {
    cat("\nRun Dashboard:\n")
    print(x$dashboard, n = x$n_runs)
  }

  if (!is.null(x$theme_evolution) && !is.null(x$theme_evolution$pairwise)) {
    p <- x$theme_evolution$pairwise
    cat(sprintf("\nTheme Evolution (vs previous): %d persisted, %d new, %d disappeared\n",
                nrow(p$persisted), nrow(p$new), nrow(p$disappeared)))
  }

  if (!is.null(x$sentiment_drift) && !is.null(x$sentiment_drift$summary)) {
    s <- x$sentiment_drift$summary
    cat(sprintf("Sentiment drift: mean shift = %.3f, reclassification rate = %.1f%%\n",
                s$mean_shift %||% 0, s$reclassification_rate %||% 0))
  }

  if (!is.null(x$code_stability) && !is.null(x$code_stability$stability)) {
    cs <- x$code_stability$stability
    cat(sprintf("Code stability: Jaccard = %.3f, churn rate = %.3f\n",
                cs$jaccard_overall, cs$churn_rate))
  }

  if (!is.null(x$entry_migration)) {
    em <- x$entry_migration
    cat(sprintf("Entry migration: stability rate = %.1f%%, %d migrated, %d stable\n",
                (em$stability_rate %||% 0) * 100, em$n_migrated, em$n_stable))
  }

  if (!is.null(x$correlation_stability)) {
    cs <- x$correlation_stability
    cat(sprintf("Correlation stability: %d persistent, %d intermittent, %d run-specific\n",
                nrow(cs$persistent), nrow(cs$intermittent), nrow(cs$run_specific)))
  }

  invisible(x)
}

# ==============================================================================
# Inter-Model Reliability (via Cross-Run Comparison)
# ==============================================================================

#' Compare runs that used different AI models for inter-model reliability
#'
#' A convenience wrapper around \code{\link{compare_runs}} that validates
#' runs used different models and focuses output on agreement metrics
#' suitable for reporting inter-model reliability in publications.
#'
#' @param results_dir Path to the parent directory containing run folders
#' @param config ThematicConfig object (or NULL)
#' @return A ComparisonResult with inter-model agreement metrics, or NULL
#' @export
compare_models <- function(results_dir, config = NULL) {
  all_dirs <- .discover_run_dirs(results_dir)

  if (length(all_dirs) < 2) {
    log_warn("Need at least 2 runs for inter-model comparison")
    return(NULL)
  }

  # Load metadata to check model diversity
  models_used <- list()
  for (d in all_dirs) {
    meta_file <- file.path(d, "run_metadata.json")
    if (file.exists(meta_file)) {
      meta <- tryCatch(jsonlite::fromJSON(meta_file), error = function(e) NULL)
      if (!is.null(meta)) {
        model_key <- paste(meta$provider %||% "unknown", meta$model_primary %||% "unknown", sep = "/")
        models_used[[basename(d)]] <- model_key
      }
    }
  }

  unique_models <- unique(unlist(models_used))
  if (length(unique_models) < 2) {
    log_warn("All runs used the same model ({unique_models[1]}). For inter-model ",
             "reliability, run the pipeline with different AI providers/models.")
    log_info("Proceeding with standard cross-run comparison instead.")
  } else {
    log_info("Inter-model comparison: {length(unique_models)} distinct models detected:")
    for (m in unique_models) {
      runs_with_m <- names(models_used)[models_used == m]
      log_info("  {m}: {length(runs_with_m)} run(s)")
    }
  }

  # Run standard comparison (already includes code/theme/sentiment comparison)
  current_dir <- all_dirs[length(all_dirs)]
  result <- compare_runs(current_dir, results_dir, config)

  if (!is.null(result)) {
    result$is_inter_model <- length(unique_models) >= 2
    result$models_used <- models_used
    result$unique_models <- unique_models
  }

  result
}
