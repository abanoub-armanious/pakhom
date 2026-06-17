# ==============================================================================
# Mid-Pipeline Researcher Review / Curation Points
# ==============================================================================
# Allows researchers to intervene at critical decision points in the pipeline
# by exporting editable CSVs. On resume, the pipeline reads the researcher's
# modifications back in and applies them before proceeding.
#
# Review points:
#   A. After progressive coding -- review/curate the codebook
#   B. After final themes -- review the thematic structure
# ==============================================================================

# Helper: safely extract a string from a CSV cell (handles NA)
.csv_str <- function(x, default = "") {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(default)
  trimws(as.character(x))
}

# ==============================================================================
# Review Point A: Codebook review (after progressive coding)
# ==============================================================================

#' Export progressive codebook for researcher review
#'
#' Exports a CSV where the researcher can review codes created during
#' progressive coding: keep, delete, merge, split, or rename codes.
#' On resume (when the reviewed file exists), applies modifications
#' to the ProgressiveCodingState.
#'
#' @param coding_state ProgressiveCodingState
#' @param output_dir Pipeline output directory
#' @param audit_log Optional AuditLog
#' @param methodology_mode Optional methodology mode. When
#'   non-NULL, the exported review CSV is stamped with a comment header
#'   identifying the mode and run id. NULL skips stamping (legacy /
#'   test callers).
#' @return List with status ("exported" or "applied") and updated coding_state
#' @keywords internal
review_progressive_codebook <- function(coding_state, output_dir,
                                         audit_log = NULL,
                                         methodology_mode = NULL) {
  review_dir <- file.path(output_dir, "researcher_review")
  dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)

  reviewed_path <- file.path(review_dir, "codebook_reviewed.csv")
  export_path <- file.path(review_dir, "codebook_review.csv")

  if (file.exists(reviewed_path)) {
    log_info("Found researcher-reviewed codebook -- applying modifications...")
    # comment="#" so a methodology stamp at the file head survives
    # the round-trip through the user's spreadsheet edit.
    reviewed <- tryCatch(
      readr::read_csv(reviewed_path, show_col_types = FALSE, comment = "#"),
      error = function(e) {
        log_error("Could not read reviewed codebook: {e$message}")
        return(NULL)
      }
    )
    if (is.null(reviewed)) {
      return(list(status = "error", coding_state = coding_state))
    }

    if ("action" %in% names(reviewed)) {
      reviewed$action <- tolower(trimws(reviewed$action))

      # Process deletions
      delete_keys <- reviewed$code_key[reviewed$action == "delete" & !is.na(reviewed$action)]
      for (key in delete_keys) {
        coding_state$codebook[[key]] <- NULL
        # Remove from entry_results
        for (eid in names(coding_state$entry_results)) {
          er <- coding_state$entry_results[[eid]]
          if (!isTRUE(er$skipped)) {
            er$codes_assigned <- setdiff(er$codes_assigned, key)
            er$coded_segments <- er$coded_segments[
              vapply(er$coded_segments, function(s) s$code_key != key, logical(1))
            ]
            coding_state$entry_results[[eid]] <- er
          }
        }
      }
      if (!is.null(audit_log)) {
        for (key in delete_keys) {
          log_ai_decision(audit_log, "researcher_review", "code_deleted", code_key = key)
        }
      }

      # Process renames
      rename_rows <- reviewed[reviewed$action == "rename" & !is.na(reviewed$action) &
                                !is.na(reviewed$new_name), ]
      for (i in seq_len(nrow(rename_rows))) {
        key <- rename_rows$code_key[i]
        new_name <- rename_rows$new_name[i]
        if (key %in% names(coding_state$codebook)) {
          old_name <- coding_state$codebook[[key]]$code_name
          coding_state$codebook[[key]]$code_name <- new_name
          if (!is.null(audit_log)) {
            log_ai_decision(audit_log, "researcher_review", "code_renamed",
                            code_key = key, before = old_name, after = new_name)
          }
        }
      }

      # Process merges
      merge_rows <- reviewed[reviewed$action == "merge" & !is.na(reviewed$action) &
                               !is.na(reviewed$merge_into), ]
      n_merged <- 0L
      for (i in seq_len(nrow(merge_rows))) {
        source_key <- merge_rows$code_key[i]
        target_key <- tolower(trimws(merge_rows$merge_into[i]))

        if (source_key %in% names(coding_state$codebook) &&
            target_key %in% names(coding_state$codebook)) {
          # Merge frequencies and entry_ids
          target <- coding_state$codebook[[target_key]]
          source_cb <- coding_state$codebook[[source_key]]
          target$frequency <- target$frequency + source_cb$frequency
          target$entry_ids <- unique(c(target$entry_ids, source_cb$entry_ids))
          target$coded_segments <- c(target$coded_segments, source_cb$coded_segments)
          coding_state$codebook[[target_key]] <- target

          # Update entry_results to point to target
          for (eid in names(coding_state$entry_results)) {
            er <- coding_state$entry_results[[eid]]
            if (!isTRUE(er$skipped)) {
              # Exact-key replacement: codes_assigned holds whole code KEYS, so
              # substring gsub would corrupt any key that merely contains
              # source_key (e.g. source "sleep" mangling "sleep_problems").
              er$codes_assigned[er$codes_assigned == source_key] <- target_key
              er$codes_assigned <- unique(er$codes_assigned)
              for (j in seq_along(er$coded_segments)) {
                if (er$coded_segments[[j]]$code_key == source_key) {
                  er$coded_segments[[j]]$code_key <- target_key
                  er$coded_segments[[j]]$code_name <- target$code_name
                }
              }
              coding_state$entry_results[[eid]] <- er
            }
          }

          # Remove source code
          coding_state$codebook[[source_key]] <- NULL

          if (!is.null(audit_log)) {
            log_ai_decision(audit_log, "researcher_review", "code_merged",
                            source_key = source_key, target_key = target_key)
          }
          n_merged <- n_merged + 1L
        } else {
          # Don't silently no-op: a typo'd merge_into (target not in the
          # codebook) or an already-removed source must be surfaced, not lost.
          log_warn(paste0("Codebook review: merge skipped -- source '", source_key,
                          "' or target '", target_key, "' not found in the codebook."))
        }
      }

      # Process description updates
      n_desc_updated <- 0L
      if ("new_description" %in% names(reviewed)) {
        for (i in seq_len(nrow(reviewed))) {
          if (!is.na(reviewed$new_description[i]) && nchar(trimws(reviewed$new_description[i])) > 0) {
            key <- reviewed$code_key[i]
            if (key %in% names(coding_state$codebook)) {
              old_desc <- coding_state$codebook[[key]]$description %||% ""
              coding_state$codebook[[key]]$description <- trimws(reviewed$new_description[i])
              n_desc_updated <- n_desc_updated + 1L
              if (!is.null(audit_log)) {
                log_ai_decision(audit_log, "researcher_review", "code_description_updated",
                                code_key = key, before = old_desc,
                                after = coding_state$codebook[[key]]$description)
              }
            }
          }
        }
      }

      # Process splits
      split_rows <- reviewed[!is.na(reviewed$action) & reviewed$action == "split", ]
      n_split <- 0L
      for (i in seq_len(nrow(split_rows))) {
        key <- split_rows$code_key[i]
        if (!key %in% names(coding_state$codebook)) next

        new_name_1 <- .csv_str(split_rows$new_name[i])
        split_name <- .csv_str(split_rows$split_name[i])
        if (nchar(new_name_1) == 0) new_name_1 <- paste0(coding_state$codebook[[key]]$code_name, " (A)")
        if (nchar(split_name) == 0) split_name <- paste0(coding_state$codebook[[key]]$code_name, " (B)")

        original <- coding_state$codebook[[key]]

        # Generate unique key for split code
        split_key <- gsub("[^a-z0-9]", " ", tolower(split_name))
        split_key <- gsub("\\s+", " ", trimws(split_key))
        split_key <- gsub(" ", "_", split_key)
        # Ensure uniqueness
        base_key <- split_key
        counter <- 1L
        while (split_key %in% names(coding_state$codebook)) {
          split_key <- paste0(base_key, "_", counter)
          counter <- counter + 1L
        }

        # Rename original
        old_name <- coding_state$codebook[[key]]$code_name
        coding_state$codebook[[key]]$code_name <- new_name_1

        # Create the second code with FRESH counters. Blind-copying the
        # original's aggregates double-counted frequency/entry_ids across both
        # halves and left the copied codebook-level segments pointing at the OLD
        # key; instead rebuild frequency / entry_ids / coded_segments from the
        # entries actually assigned the new code in the loop below.
        #
        # NB (split semantics): this is a SCAFFOLD split -- the new code is
        # applied to ALL of the original's entries (the system cannot know which
        # entries belong to which half without re-coding), and the renamed
        # original retains them too. The researcher is expected to curate the
        # two halves afterward; until they do, both codes cover the same entries.
        new_code <- original
        new_code$code_name <- split_name
        new_code$entry_ids <- character(0)
        new_code$coded_segments <- list()
        new_code$frequency <- 0L

        split_entry_ids <- character(0)
        split_segments <- list()
        for (eid in names(coding_state$entry_results)) {
          er <- coding_state$entry_results[[eid]]
          if (!isTRUE(er$skipped) && key %in% er$codes_assigned) {
            er$codes_assigned <- unique(c(er$codes_assigned, split_key))
            new_segs <- lapply(er$coded_segments, function(s) {
              if (s$code_key == key) {
                s2 <- s
                s2$code_key <- split_key
                s2$code_name <- split_name
                return(s2)
              }
              NULL
            })
            new_segs <- Filter(Negate(is.null), new_segs)
            er$coded_segments <- c(er$coded_segments, new_segs)
            coding_state$entry_results[[eid]] <- er
            split_entry_ids <- c(split_entry_ids, eid)
            split_segments <- c(split_segments, new_segs)
          }
        }
        new_code$entry_ids <- unique(split_entry_ids)
        new_code$coded_segments <- split_segments
        new_code$frequency <- length(new_code$entry_ids)
        coding_state$codebook[[split_key]] <- new_code

        n_split <- n_split + 1L
        if (!is.null(audit_log)) {
          log_ai_decision(audit_log, "researcher_review", "code_split",
                          original_key = key, original_name = old_name,
                          new_name_1 = new_name_1, split_key = split_key, split_name = split_name)
        }
      }

      # Process researcher memos
      n_memos <- 0L
      if ("researcher_memo" %in% names(reviewed)) {
        for (i in seq_len(nrow(reviewed))) {
          raw_memo <- reviewed$researcher_memo[i]
          memo <- if (is.na(raw_memo)) "" else trimws(raw_memo)
          if (nchar(memo) > 0) {
            key <- reviewed$code_key[i]
            if (key %in% names(coding_state$codebook)) {
              coding_state$codebook[[key]]$researcher_memo <- memo
              n_memos <- n_memos + 1L
              if (!is.null(audit_log)) {
                log_ai_decision(audit_log, "researcher_review", "review_memo_added",
                                code_key = key, memo = memo)
              }
            }
          }
        }
      }

      n_deleted <- length(delete_keys)
      n_renamed <- nrow(rename_rows)
      # n_merged is the count of merges actually applied (set in the loop above),
      # not nrow(merge_rows) -- skipped merges must not be counted as applied.
      log_info("Codebook review applied: {n_deleted} deleted, {n_renamed} renamed, {n_merged} merged, {n_split} split, {n_desc_updated} descriptions updated, {n_memos} memos added")
      log_info("Codebook now has {length(coding_state$codebook)} codes")
    }

    return(list(status = "applied", coding_state = coding_state))
  }

  # Export review sheet
  if (length(coding_state$codebook) == 0) {
    log_warn("No codes to review")
    return(list(status = "skipped", coding_state = coding_state))
  }

  review_df <- tibble::tibble(
    code_key = names(coding_state$codebook),
    code_name = vapply(coding_state$codebook, function(cb) cb$code_name, character(1)),
    description = vapply(coding_state$codebook, function(cb) cb$description %||% "", character(1)),
    frequency = vapply(coding_state$codebook, function(cb) cb$frequency, integer(1)),
    n_entries = vapply(coding_state$codebook, function(cb) length(unique(cb$entry_ids)), integer(1)),
    example_segment = vapply(coding_state$codebook, function(cb) {
      if (length(cb$coded_segments) > 0) {
        substr(cb$coded_segments[[1]]$text %||% "", 1, 200)
      } else ""
    }, character(1)),
    action = "",        # keep, delete, merge, rename, split
    new_name = "",      # for rename action
    merge_into = "",    # for merge action (target code_key)
    new_description = "",   # for editing code descriptions
    split_name = "",        # for split action (name of second code)
    researcher_memo = ""    # for interpretive notes/rationale
  )

  review_df <- review_df[order(-review_df$frequency), ]
  readr::write_csv(review_df, export_path)
  # T1.7 / AC4: stamp the export so the reviewer sees the methodology
  # before they touch the file. Idempotent + readr-comment-safe.
  if (!is.null(methodology_mode)) {
    tryCatch(stamp_methodology_csv(export_path, methodology_mode,
                                     run_id = basename(output_dir)),
             error = function(e) log_debug("CSV stamp skipped: {e$message}"))
  }

  log_info("Exported codebook review sheet: {export_path}")
  log_info("  {nrow(review_df)} codes for review")
  log_info("  Edit the 'action' column (keep/delete/merge/rename/split)")
  log_info("  Save as 'codebook_reviewed.csv' and re-run with resume = TRUE")

  list(status = "exported", coding_state = coding_state)
}

# ==============================================================================
# Review Point B: Theme review (after iterative merge passes)
# ==============================================================================

#' Export theme review sheet and apply modifications on resume
#'
#' Exports a CSV where the researcher can rename, merge, split, or delete
#' generated themes before proceeding to correlations and report generation.
#'
#' @param theme_set ThemeSet S3 object
#' @param output_dir Pipeline output directory
#' @param audit_log Optional AuditLog
#' @param methodology_mode Optional methodology mode. When
#'   non-NULL, the exported review and disposition CSVs are stamped with
#'   a comment header. NULL skips stamping (legacy / test callers).
#' @return List with status and updated theme_set
#' @keywords internal
review_themes <- function(theme_set, output_dir, audit_log = NULL,
                          methodology_mode = NULL) {
  review_dir <- file.path(output_dir, "researcher_review")
  dir.create(review_dir, recursive = TRUE, showWarnings = FALSE)

  reviewed_path <- file.path(review_dir, "themes_reviewed.csv")
  export_path <- file.path(review_dir, "themes_review.csv")

  # If reviewed file exists, apply modifications
  if (file.exists(reviewed_path)) {
    log_info("Found researcher-reviewed themes file -- applying modifications...")
    # comment="#" so a methodology stamp at the file head survives
    # the round-trip through the user's spreadsheet edit.
    reviewed <- tryCatch(
      readr::read_csv(reviewed_path, show_col_types = FALSE, comment = "#"),
      error = function(e) {
        log_error("Could not read reviewed themes file: {e$message}")
        return(NULL)
      }
    )
    if (is.null(reviewed)) {
      return(list(status = "error", theme_set = theme_set))
    }

    if (!"action" %in% names(reviewed)) {
      log_warn("No 'action' column found -- using themes as-is")
      return(list(status = "applied", theme_set = theme_set))
    }

    reviewed$action <- tolower(trimws(reviewed$action))
    themes <- theme_set$themes

    # researcher CRUD operates on the flat codes_included field
    # for ergonomics (CSV is a flat list of codes, not a hierarchy). Drop
    # the canonical Subtheme S3 list before mutation so create_theme_set()
    # at the end rebuilds subthemes from the post-edit codes_included via
    # the back-compat path.
    # Only flatten when the reviewed CSV
    # has actions that genuinely mutate code membership (codes_to_add /
    # codes_to_remove / merge / split). Rename + description-only edits
    # preserve subtheme structure exactly (no code reshuffling), so
    # there's no need to discard the Subtheme S3 list for
    # those. This loss was found to be unnecessary for ~70% of
    # researcher edits on the reviews.
    code_mutating_actions <- c("merge", "split")
    code_mutating_cols    <- intersect(c("codes_to_add", "codes_to_remove"),
                                         names(reviewed))
    has_code_action <- any(reviewed$action %in% code_mutating_actions)
    has_code_edit   <- length(code_mutating_cols) > 0L && any(
      vapply(code_mutating_cols, function(col) {
        any(!is.na(reviewed[[col]]) & nzchar(trimws(reviewed[[col]])))
      }, logical(1))
    )
    needs_flatten <- has_code_action || has_code_edit

    if (needs_flatten) {
      themes <- lapply(themes, function(t) {
        t$codes_included       <- theme_codes(t)
        t$subthemes            <- NULL
        t$subthemes_structured <- NULL
        t
      })
    } else {
      # Just ensure codes_included is fresh for the rename / description
      # path. The clustering re-derives on demand otherwise.
      themes <- lapply(themes, function(t) {
        t$codes_included <- theme_codes(t)
        t
      })
    }

    # Build list of themes to keep, with modifications
    new_themes <- list()
    merge_targets <- list()  # source_name -> target_name

    for (i in seq_len(nrow(reviewed))) {
      action <- reviewed$action[i]
      original_name <- reviewed$theme_name[i]

      # Find matching theme
      theme_idx <- which(vapply(themes, function(t) t$name, character(1)) == original_name)
      if (length(theme_idx) == 0) next
      theme <- themes[[theme_idx]]

      if (is.na(action) || action == "" || action == "keep") {
        # Apply rename if specified
        if ("new_name" %in% names(reviewed) &&
            !is.na(reviewed$new_name[i]) &&
            nchar(trimws(reviewed$new_name[i])) > 0) {
          theme$name <- trimws(reviewed$new_name[i])
          log_info("  Renamed theme: '{original_name}' -> '{theme$name}'")
          if (!is.null(audit_log)) {
            log_ai_decision(audit_log, "researcher_review", "theme_renamed",
                            before = original_name, after = theme$name)
          }
        }
        # Apply description update if specified
        if ("new_description" %in% names(reviewed) &&
            !is.na(reviewed$new_description[i]) &&
            nchar(trimws(reviewed$new_description[i])) > 0) {
          theme$description <- trimws(reviewed$new_description[i])
        }
        # Code reassignment
        if ("codes_to_add" %in% names(reviewed) &&
            !is.na(reviewed$codes_to_add[i]) &&
            nchar(trimws(reviewed$codes_to_add[i])) > 0) {
          add_codes <- trimws(strsplit(reviewed$codes_to_add[i], ";")[[1]])
          add_codes <- add_codes[nchar(add_codes) > 0]
          theme$codes_included <- unique(c(theme$codes_included, add_codes))
          if (!is.null(audit_log) && length(add_codes) > 0) {
            log_ai_decision(audit_log, "researcher_review", "theme_restructured",
                            theme_name = theme$name, action = "codes_added",
                            codes = paste(add_codes, collapse = "; "))
          }
        }
        if ("codes_to_remove" %in% names(reviewed) &&
            !is.na(reviewed$codes_to_remove[i]) &&
            nchar(trimws(reviewed$codes_to_remove[i])) > 0) {
          rm_codes <- trimws(strsplit(reviewed$codes_to_remove[i], ";")[[1]])
          rm_codes <- rm_codes[nchar(rm_codes) > 0]
          theme$codes_included <- setdiff(theme$codes_included, rm_codes)
          if (!is.null(audit_log) && length(rm_codes) > 0) {
            log_ai_decision(audit_log, "researcher_review", "theme_restructured",
                            theme_name = theme$name, action = "codes_removed",
                            codes = paste(rm_codes, collapse = "; "))
          }
        }
        new_themes <- c(new_themes, list(theme))

      } else if (action == "delete") {
        log_info("  Deleted theme: '{original_name}'")
        if (!is.null(audit_log)) {
          log_ai_decision(audit_log, "researcher_review", "theme_deleted",
                          theme_name = original_name)
        }

      } else if (action == "merge") {
        if ("merge_into" %in% names(reviewed) &&
            !is.na(reviewed$merge_into[i]) &&
            nchar(trimws(reviewed$merge_into[i])) > 0) {
          target <- trimws(reviewed$merge_into[i])
          merge_targets[[original_name]] <- target
          log_info("  Will merge theme '{original_name}' into '{target}'")
          if (!is.null(audit_log)) {
            log_ai_decision(audit_log, "researcher_review", "theme_merged",
                            source = original_name, target = target)
          }
        } else {
          log_warn("  Theme '{original_name}' marked 'merge' but no merge_into -- keeping")
          new_themes <- c(new_themes, list(theme))
        }

      } else if (action == "split") {
        # Handled in second pass below
        log_info("  Will split theme: '{original_name}'")
      }
    }

    # Apply merges: combine codes_included from source into target
    for (source_name in names(merge_targets)) {
      target_name <- merge_targets[[source_name]]
      source_idx <- which(vapply(themes, function(t) t$name, character(1)) == source_name)
      target_idx <- which(vapply(new_themes, function(t) t$name, character(1)) == target_name)

      if (length(source_idx) > 0 && length(target_idx) > 0) {
        source_codes <- themes[[source_idx]]$codes_included
        new_themes[[target_idx]]$codes_included <- unique(c(
          new_themes[[target_idx]]$codes_included,
          source_codes
        ))
      }
    }

    # Apply splits
    split_rows <- reviewed[!is.na(reviewed$action) & tolower(trimws(reviewed$action)) == "split", ]
    for (i in seq_len(nrow(split_rows))) {
      original_name <- split_rows$theme_name[i]
      theme_idx <- which(vapply(themes, function(t) t$name, character(1)) == original_name)
      if (length(theme_idx) == 0) next
      theme <- themes[[theme_idx]]

      new_name_1 <- .csv_str(split_rows$new_name[i])
      split_into_name <- .csv_str(split_rows$split_into[i])
      if (nchar(new_name_1) == 0) new_name_1 <- paste0(original_name, " (A)")
      if (nchar(split_into_name) == 0) split_into_name <- paste0(original_name, " (B)")

      # Codes for second theme come from codes_to_remove
      codes_for_second <- character(0)
      if ("codes_to_remove" %in% names(split_rows) &&
          !is.na(split_rows$codes_to_remove[i]) &&
          nchar(trimws(split_rows$codes_to_remove[i])) > 0) {
        codes_for_second <- trimws(strsplit(split_rows$codes_to_remove[i], ";")[[1]])
        codes_for_second <- codes_for_second[nchar(codes_for_second) > 0]
      }

      theme_1 <- theme
      theme_1$name <- new_name_1
      theme_1$codes_included <- setdiff(theme$codes_included, codes_for_second)
      if ("new_description" %in% names(split_rows) &&
          !is.na(split_rows$new_description[i]) &&
          nchar(trimws(split_rows$new_description[i])) > 0) {
        theme_1$description <- trimws(split_rows$new_description[i])
      }

      theme_2 <- theme
      theme_2$name <- split_into_name
      theme_2$codes_included <- codes_for_second
      # theme_1 keeps the original theme's id, so theme_2 needs a fresh,
      # collision-free id (the old `2 * length + 1` could both collide and skip).
      theme_2$id <- max(c(
        vapply(new_themes, function(t) as.integer(t$id %||% 0L), integer(1)),
        as.integer(theme$id %||% 0L), 0L
      )) + 1L

      new_themes <- c(new_themes, list(theme_1), list(theme_2))

      if (!is.null(audit_log)) {
        log_ai_decision(audit_log, "researcher_review", "theme_restructured",
                        action = "split", original = original_name,
                        theme_1 = new_name_1, theme_2 = split_into_name,
                        codes_moved = paste(codes_for_second, collapse = "; "))
      }
      log_info("  Split theme '{original_name}' into '{new_name_1}' and '{split_into_name}'")
    }

    # Create new themes
    create_rows <- reviewed[!is.na(reviewed$action) & tolower(trimws(reviewed$action)) == "create", ]
    for (i in seq_len(nrow(create_rows))) {
      tn <- .csv_str(create_rows$theme_name[i])
      if (nchar(tn) == 0) tn <- .csv_str(create_rows$new_name[i])
      if (nchar(tn) == 0) next
      desc <- .csv_str(create_rows$new_description[i])
      if (nchar(desc) == 0) desc <- .csv_str(create_rows$description[i])
      codes_str <- .csv_str(create_rows$codes_included[i])
      codes_list <- if (nchar(codes_str) > 0) {
        trimws(strsplit(codes_str, ";")[[1]])
      } else character(0)
      codes_list <- codes_list[nchar(codes_list) > 0]

      new_theme <- list(
        id = length(new_themes) + 1L,
        name = tn,
        description = desc,
        codes_included = codes_list,
        prevalence = "medium",
        sentiment_tendency = "neutral",
        subthemes = character(0),
        subthemes_structured = list(),
        keywords = character(0),
        narrative = "",
        supporting_quotes = character(0),
        entry_count = 0L
      )
      new_themes <- c(new_themes, list(new_theme))

      if (!is.null(audit_log)) {
        log_ai_decision(audit_log, "researcher_review", "theme_created",
                        theme_name = tn, codes = paste(codes_list, collapse = "; "))
      }
      log_info("  Created new theme: '{tn}' with {length(codes_list)} codes")
    }

    # Process researcher memos
    if ("researcher_memo" %in% names(reviewed)) {
      for (i in seq_len(nrow(reviewed))) {
        raw_memo <- reviewed$researcher_memo[i]
        memo <- if (is.na(raw_memo)) "" else trimws(raw_memo)
        if (nchar(memo) > 0) {
          tn <- reviewed$theme_name[i]
          new_nm <- .csv_str(reviewed$new_name[i])
          # Find in new_themes and attach memo
          for (j in seq_along(new_themes)) {
            if ((!is.na(tn) && new_themes[[j]]$name == tn) ||
                (nchar(new_nm) > 0 && new_themes[[j]]$name == new_nm)) {
              new_themes[[j]]$researcher_memo <- memo
              break
            }
          }
          if (!is.null(audit_log)) {
            log_ai_decision(audit_log, "researcher_review", "review_memo_added",
                            theme_name = tn, memo = memo)
          }
        }
      }
    }

    # Rebuild theme set
    theme_set <- create_theme_set(new_themes)
    log_info("Theme review applied: {n_themes(theme_set)} themes remain")

    return(list(status = "applied", theme_set = theme_set))
  }

  # Export review sheet
  theme_names_vec <- theme_names(theme_set)
  themes <- theme_set$themes

  review_sheet <- tibble::tibble(
    theme_name = theme_names_vec,
    description = vapply(themes, function(t) t$description %||% "", character(1)),
    codes_included = vapply(themes, function(t) {
      paste(t$codes_included, collapse = "; ")
    }, character(1)),
    action = "",
    new_name = "",
    new_description = "",
    merge_into = "",
    codes_to_add = "",
    codes_to_remove = "",
    split_into = "",
    researcher_memo = ""
  )

  readr::write_csv(review_sheet, export_path)
  # T1.7 / AC4: stamp the export so the reviewer sees the methodology
  # before they touch the file.
  if (!is.null(methodology_mode)) {
    tryCatch(stamp_methodology_csv(export_path, methodology_mode,
                                     run_id = basename(output_dir)),
             error = function(e) log_debug("CSV stamp skipped: {e$message}"))
  }

  disp_df <- tibble::tibble(disposition = "continue")
  disp_path <- file.path(review_dir, "review_disposition.csv")
  readr::write_csv(disp_df, disp_path)
  if (!is.null(methodology_mode)) {
    tryCatch(stamp_methodology_csv(disp_path, methodology_mode,
                                     run_id = basename(output_dir)),
             error = function(e) log_debug("CSV stamp skipped: {e$message}"))
  }
  log_info("Theme review sheet exported: {export_path}")
  log_info("  Review the {nrow(review_sheet)} themes. For each theme, set 'action' to:")
  log_info("    'keep'   - Keep as-is (or set 'new_name'/'new_description' to modify)")
  log_info("    'delete' - Remove this theme entirely")
  log_info("    'merge'  - Merge into another theme (set 'merge_into' to target theme name)")
  log_info("    'create' - Create a new theme (fill in theme_name, description, codes_included)")
  log_info("    'split'  - Split into two themes (set 'split_into' for second theme name)")
  log_info("  Leave 'action' empty to keep unchanged.")
  log_info("  Save as: {basename(reviewed_path)}")
  log_info("  Then re-run with resume = TRUE to continue.")

  list(status = "exported", theme_set = theme_set)
}

#' Read review disposition from theme review directory
#'
#' After theme review, the researcher can set disposition to "revise_codebook"
#' to loop back and revise the codebook before re-running theme generation.
#'
#' @param output_dir Pipeline output directory
#' @return Character: "continue" (default) or "revise_codebook"
#' @keywords internal
read_review_disposition <- function(output_dir) {
  disp_path <- file.path(output_dir, "researcher_review", "review_disposition.csv")
  if (!file.exists(disp_path)) return("continue")
  # comment="#" so a methodology stamp at the file head doesn't poison
  # the parse and silently downgrade the user's "revise_codebook"
  # intent to the default "continue".
  disp <- tryCatch(
    readr::read_csv(disp_path, show_col_types = FALSE, comment = "#"),
    error = function(e) return(data.frame(disposition = "continue"))
  )
  if (is.null(disp) || !"disposition" %in% names(disp) || nrow(disp) == 0) {
    return("continue")
  }
  # A present-but-NA disposition cell (e.g. the researcher cleared the value but
  # left the row) would return NA, and the pipeline's `if (disposition == ...)`
  # branch would then error on a missing value. Default a blank/NA to "continue".
  d <- tolower(trimws(disp$disposition[1]))
  if (is.na(d) || !nzchar(d)) return("continue")
  d
}
