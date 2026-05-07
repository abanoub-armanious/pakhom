# ==============================================================================
# Iterative Bottom-Up Theme Generation + Deterministic Cascading
# ==============================================================================
# Replaces the old one-shot AI theme generation + AI assignment approach.
# Codes are iteratively merged bottom-up into clusters through multiple
# passes until no more productive merges exist. The final passes determine
# themes and subthemes. Entry-to-theme assignment is deterministic through
# the code hierarchy (no AI re-reading of raw text).
# ==============================================================================

.SENTIMENT_TENDENCY_THRESHOLD <- 0.2

# ==============================================================================
# Main function: iterative theme generation
# ==============================================================================

#' Generate themes through iterative bottom-up merging
#'
#' Starting from individual codes, the AI groups codes with similar narratives
#' into clusters through multiple passes. Each pass merges clusters that share
#' higher-level patterns. Stops when no more productive merges exist.
#'
#' @param coding_state ProgressiveCodingState
#' @param provider AIProvider object
#' @param config Theme config section
#' @param learning_context LearningContext (or NULL)
#' @param research_focus Research focus string
#' @param concepts Character vector of core research concepts (or NULL)
#' @param audit_log An AuditLog object (from \code{init_audit_log}) for
#'   recording each merge decision (merge or standalone) and the final
#'   theme structure, or NULL to disable audit logging for this step.
#' @param response_cache An optional ResponseCache object (from
#'   \code{\link{init_response_cache}}). When provided, raw API responses
#'   for each per-item theming ai_complete() call are written to the cache
#'   and referenced from the audit log (T1.4). Pass \code{NULL} to skip
#'   raw-response capture.
#' @return ThemeSet S3 object with merge_history attached
#' @export
generate_themes_iterative <- function(coding_state, provider, config = list(),
                                       learning_context = NULL,
                                       research_focus = "",
                                       concepts = NULL,
                                       audit_log = NULL,
                                       response_cache = NULL) {
  if (!inherits(coding_state, "ProgressiveCodingState")) {
    stop("coding_state must be a ProgressiveCodingState object")
  }

  validate_provider(provider, caller = "generate_themes_iterative")

  config$max_merge_passes <- config$max_merge_passes %||% 10L
  config$stopping_criterion <- config$stopping_criterion %||% "convergence"
  config$min_merges_to_continue <- config$min_merges_to_continue %||% 2L

  # Extract codebook as a simple list for the merge process
  codes <- lapply(names(coding_state$codebook), function(key) {
    cb <- coding_state$codebook[[key]]
    list(
      key = key,
      name = cb$code_name,
      description = cb$description %||% "",
      type = cb$type %||% "descriptive",
      frequency = cb$frequency,
      n_entries = length(unique(cb$entry_ids))
    )
  })

  if (length(codes) == 0) {
    log_warn("No codes in coding state -- cannot generate themes")
    return(create_theme_set(list()))
  }

  log_info("Starting iterative theme generation with {length(codes)} codes")
  tic("Theme generation")

  concept_str <- if (!is.null(concepts) && length(concepts) > 0) {
    paste(concepts, collapse = ", ")
  } else {
    research_focus
  }

  # Calibration from previous studies
  calibration_text <- ""
  if (!is.null(learning_context) && nchar(learning_context$for_theming %||% "") > 0) {
    calibration_text <- paste0(
      "\n## REFERENCE: How previous human researchers organized codes\n",
      learning_context$for_theming, "\n"
    )
  }

  # Initialize merge history
  merge_history <- list(
    passes = list(),
    final_themes = list(),
    final_subthemes = list(),
    code_to_theme_map = list(),
    code_to_subtheme_map = list()
  )

  # Current items to merge (start with individual codes)
  current_items <- lapply(codes, function(c) {
    list(
      label = c$name,
      description = c$description,
      codes = c$key,
      frequency = c$frequency,
      is_singleton = TRUE
    )
  })

  # Compute code co-occurrence matrix (how often codes appear on the same entries)
  co_occurrence <- .compute_code_cooccurrence(coding_state)

  # Compute code embeddings for similarity context (OpenAI only, graceful fallback)
  code_embeddings <- NULL
  code_similarity <- NULL
  if (!is.null(provider$models$embedding)) {
    code_descs <- vapply(codes, function(c) {
      paste(c$name, c$description, sep = ": ")
    }, character(1))
    code_embeddings <- tryCatch(
      compute_embeddings(provider, code_descs),
      error = function(e) { log_debug("Embedding computation skipped: {e$message}"); NULL }
    )
    if (!is.null(code_embeddings)) {
      code_similarity <- .cosine_similarity_matrix(code_embeddings)
      rownames(code_similarity) <- vapply(codes, function(c) c$key, character(1))
      colnames(code_similarity) <- rownames(code_similarity)
      log_info("Code embeddings computed: {nrow(code_similarity)} codes, {ncol(code_embeddings)} dimensions")
    }
  }

  # Iterative merge passes
  pass_num <- 0L
  consecutive_low_merges <- 0L

  repeat {
    pass_num <- pass_num + 1L
    if (pass_num > config$max_merge_passes) {
      log_info("Reached maximum merge passes ({config$max_merge_passes})")
      break
    }

    log_info("=== Merge pass {pass_num}: {length(current_items)} items ===")

    merge_result <- .run_merge_pass(
      items = current_items,
      pass_number = pass_num,
      provider = provider,
      research_focus = research_focus,
      concept_str = concept_str,
      calibration_text = calibration_text,
      reflexivity_block = config$reflexivity_block %||% "",
      co_occurrence = co_occurrence,
      code_similarity = code_similarity,
      audit_log = audit_log,
      response_cache = response_cache
    )

    n_merges <- merge_result$n_merges
    log_info("  Pass {pass_num}: {n_merges} merges, {length(merge_result$items)} items remaining")

    merge_history$passes[[pass_num]] <- list(
      pass_number = pass_num,
      n_input_items = length(current_items),
      n_output_items = length(merge_result$items),
      n_merges = n_merges,
      timestamp = Sys.time()
    )

    current_items <- merge_result$items

    # Check stopping criteria
    if (n_merges == 0) {
      log_info("No merges in pass {pass_num} -- stopping")
      break
    }

    if (n_merges < config$min_merges_to_continue) {
      consecutive_low_merges <- consecutive_low_merges + 1L
      if (consecutive_low_merges >= 2) {
        log_info("Two consecutive low-merge passes -- stopping")
        break
      }
    } else {
      consecutive_low_merges <- 0L
    }

    if (merge_result$ai_says_stop) {
      log_info("AI reports no more productive merges -- stopping")
      break
    }
  }

  # Determine themes and subthemes
  theme_structure <- .determine_theme_subtheme_structure(
    items = current_items,
    merge_history = merge_history,
    coding_state = coding_state
  )

  # Build code-to-theme mapping
  for (theme in theme_structure$themes) {
    for (code_key in theme$all_code_keys) {
      merge_history$code_to_theme_map[[code_key]] <- theme$name
    }
    if (!is.null(theme$subthemes)) {
      for (sub in theme$subthemes) {
        for (code_key in sub$code_keys) {
          merge_history$code_to_subtheme_map[[code_key]] <- sub$name
        }
      }
    }
  }

  merge_history$final_themes <- theme_structure$themes

  # Build ThemeSet
  theme_list <- lapply(seq_along(theme_structure$themes), function(i) {
    t <- theme_structure$themes[[i]]

    subthemes_structured <- if (!is.null(t$subthemes) && length(t$subthemes) > 0) {
      lapply(t$subthemes, function(s) {
        list(name = s$name, description = s$description %||% "")
      })
    } else NULL

    list(
      id = i,
      name = t$name,
      description = t$description %||% "",
      codes_included = vapply(t$all_code_keys, function(k) {
        coding_state$codebook[[k]]$code_name %||% k
      }, character(1)),
      subthemes = if (!is.null(subthemes_structured)) {
        vapply(subthemes_structured, function(s) s$name, character(1))
      } else character(0),
      subthemes_structured = subthemes_structured,
      prevalence = "medium",
      sentiment_tendency = "neutral"
    )
  })

  theme_set <- create_theme_set(
    themes = theme_list,
    thematic_map = paste0("Generated via ", pass_num, " iterative merge passes"),
    analysis_notes = paste0("Bottom-up inductive theme generation with ",
                             length(codes), " initial codes")
  )
  theme_set$merge_history <- merge_history

  # Audit log: final theme structure
  if (!is.null(audit_log)) {
    final_theme_names <- vapply(theme_list, function(t) t$name, character(1))
    log_ai_decision(audit_log, "theming", "theme_structure",
                    n_themes = length(theme_list),
                    theme_names = paste(final_theme_names, collapse = "; "))
  }

  toc()
  log_info("Generated {length(theme_list)} themes via {pass_num} merge passes")

  theme_set
}

# ==============================================================================
# Sequential merge pass -- processes one item at a time
# ==============================================================================
# Mirrors the progressive coding approach: each code/cluster is compared
# against all existing clusters. The AI decides whether to merge it into
# an existing cluster or let it stand alone. This avoids the problem of
# sending hundreds of items in one prompt.
# ==============================================================================

#' Run a single sequential merge pass through all items
#'
#' For each item (starting from the second), present the AI with the current
#' clusters and ask: "Should this item merge into an existing cluster, or
#' stand alone?" After processing all items, return the resulting clusters
#' and the number of merges that occurred.
#'
#' @param items List of items (codes or previously-merged clusters)
#' @param pass_number Integer pass number (for logging)
#' @param provider AIProvider object
#' @param research_focus Research focus string
#' @param concept_str Concept string for the research focus
#' @param calibration_text Calibration text from previous studies
#' @return List with items, n_merges, ai_says_stop
#' @keywords internal
.run_merge_pass <- function(items, pass_number, provider, research_focus,
                             concept_str, calibration_text,
                             reflexivity_block = "",
                             co_occurrence = NULL,
                             code_similarity = NULL,
                             audit_log = NULL,
                             response_cache = NULL) {
  if (length(items) < 2) {
    return(list(items = items, n_merges = 0L, ai_says_stop = TRUE))
  }

  # Build stable system prompt for this pass
  system_prompt <- paste0(
    "You are an expert qualitative researcher organizing codes into thematic groups.\n\n",
    "Research focus: ", research_focus, "\n",
    "Core concepts: ", concept_str, "\n",
    reflexivity_block,
    "\n## YOUR TASK\n",
    "You will be shown existing clusters/codes and a NEW item. Decide whether the ",
    "new item should MERGE into one of the existing clusters, or STAND ALONE.\n\n",
    "## RULES\n",
    "- Only merge if the new item genuinely shares a COMMON NARRATIVE with an ",
    "existing cluster -- not just topical overlap\n",
    "- If merging, provide an updated label that captures what the combined cluster represents\n",
    "- Cluster labels should be concise (5-12 words) and sound like research findings\n",
    "- Each cluster must relate to the research focus\n",
    "- It is perfectly fine for the item to stand alone -- do NOT force merges\n",
    "- If two items share only a vague connection, keep them separate\n\n",
    calibration_text,
    "\n## RESPONSE GUIDANCE\n",
    "Set action to either \"merge\" or \"standalone\". When merging, set ",
    "merge_into to the 1-based index of the target cluster, updated_label to ",
    "a concise (5-12 word) revised label that captures the combined cluster, ",
    "and updated_description to what unifies it. When standalone, set ",
    "merge_into, updated_label, and updated_description all to null. Always ",
    "provide a rationale explaining the decision. The response shape is ",
    "enforced by the structured-output schema."
  )

  # Initialize clusters with the first item
  clusters <- list(items[[1]])
  n_merges <- 0L
  ai_says_stop <- FALSE

  # Process each remaining item sequentially
  for (i in 2:length(items)) {
    new_item <- items[[i]]

    # Build description of current clusters for the prompt
    clusters_text <- .format_clusters_for_prompt(clusters)

    # Build description of the new item
    new_item_text <- .format_single_item(new_item)

    # Build co-occurrence and similarity context for the new item
    context_block <- .build_merge_context(
      new_item = new_item, clusters = clusters,
      co_occurrence = co_occurrence, code_similarity = code_similarity
    )

    prompt <- paste0(
      "## CURRENT CLUSTERS (", length(clusters), " total):\n\n",
      clusters_text, "\n\n",
      "## NEW ITEM TO PLACE:\n\n",
      new_item_text, "\n\n",
      context_block,
      "Should this item merge into an existing cluster, or stand alone?"
    )

    result <- tryCatch({
      ai_result <- ai_complete(provider, prompt, system_prompt,
                                task = "theming",
                                response_schema = .theming_schema())
      if (!is.null(audit_log)) {
        log_ai_request(audit_log, "theming", ai_result, response_cache,
                        pass_number = pass_number, item_index = i)
      }
      parse_json_safely(ai_result$content)
    }, error = function(e) {
      log_debug("Merge decision failed for item {i}: {e$message}")
      NULL
    })

    if (is.null(result) || is.null(result$action)) {
      # On failure, keep item as standalone
      clusters[[length(clusters) + 1L]] <- new_item
      next
    }

    action <- tolower(result$action %||% "standalone")

    if (action == "merge") {
      merge_idx <- as.integer(result$merge_into %||% 0)
      if (merge_idx >= 1 && merge_idx <= length(clusters)) {
        # Merge: combine codes and update label
        target <- clusters[[merge_idx]]
        merged_codes <- unique(c(target$codes, new_item$codes))
        merged_freq <- (target$frequency %||% 0L) + (new_item$frequency %||% 0L)

        updated_label <- result$updated_label %||% result$label %||% target$label
        updated_desc <- result$updated_description %||% result$description %||%
          target$description %||% ""
        if (is.na(updated_desc)) updated_desc <- ""

        # Preserve full merge tree: if target already has children, keep them;
        # if it's a leaf/singleton, wrap it as a child. Same for new_item.
        target_children <- if (!is.null(target$children)) target$children else list(target)
        new_children <- if (!is.null(new_item$children)) list(new_item) else list(new_item)

        clusters[[merge_idx]] <- list(
          label = updated_label,
          description = updated_desc,
          codes = merged_codes,
          frequency = merged_freq,
          is_singleton = FALSE,
          # Full merge tree: each child retains its own children for multi-level subthemes
          children = c(target_children, new_children)
        )

        n_merges <- n_merges + 1L

        if (!is.null(audit_log)) {
          log_ai_decision(audit_log, "theming", "merge_decision",
                          action = "merge",
                          items_merged = paste(new_item$label, "+", target$label),
                          resulting_cluster = updated_label,
                          pass = pass_number)
        }
      } else {
        # Invalid merge target, keep as standalone
        clusters[[length(clusters) + 1L]] <- new_item
      }
    } else {
      # Standalone
      clusters[[length(clusters) + 1L]] <- new_item
      if (!is.null(audit_log)) {
        log_ai_decision(audit_log, "theming", "merge_decision",
                        action = "standalone",
                        item = new_item$label,
                        pass = pass_number)
      }
    }

    # Progress logging every 50 items
    if (i %% 50 == 0) {
      log_info("  Pass {pass_number} progress: {i}/{length(items)} items, {length(clusters)} clusters, {n_merges} merges")
    }
  }

  log_info("  Pass {pass_number} complete: {n_merges} merges, {length(clusters)} clusters from {length(items)} items")

  list(items = clusters, n_merges = n_merges, ai_says_stop = (n_merges == 0))
}

#' Format clusters for the merge prompt (compact summary)
#' @keywords internal
.format_clusters_for_prompt <- function(clusters) {
  lines <- vapply(seq_along(clusters), function(i) {
    cl <- clusters[[i]]
    n_codes <- length(cl$codes)
    codes_preview <- if (n_codes <= 3) {
      paste(cl$codes, collapse = ", ")
    } else {
      paste0(paste(cl$codes[1:3], collapse = ", "), " + ", n_codes - 3, " more")
    }
    desc <- cl$description %||% ""
    if (is.na(desc)) desc <- ""
    desc_str <- if (nchar(desc) > 0) paste0("\n   Desc: ", substr(desc, 1, 150)) else ""

    sprintf('%d. "%s" (%d codes, freq=%d)%s\n   Codes: %s',
            i, cl$label, n_codes, cl$frequency %||% 0, desc_str, codes_preview)
  }, character(1))

  paste(lines, collapse = "\n\n")
}

#' Format a single item for the merge prompt
#' @keywords internal
.format_single_item <- function(item) {
  n_codes <- length(item$codes)
  codes_preview <- if (n_codes <= 5) {
    paste(item$codes, collapse = ", ")
  } else {
    paste0(paste(item$codes[1:5], collapse = ", "), " + ", n_codes - 5, " more")
  }
  desc <- item$description %||% ""
  if (is.na(desc)) desc <- ""
  desc_str <- if (nchar(desc) > 0) paste0("\nDescription: ", substr(desc, 1, 200)) else ""

  sprintf('"%s" (%d codes, freq=%d)%s\nCodes: %s',
          item$label, n_codes, item$frequency %||% 0, desc_str, codes_preview)
}

# ==============================================================================
# Theme/subtheme structure determination from merge history
# ==============================================================================

#' Determine theme and subtheme structure from merge passes
#'
#' After iterative merging, the merge history determines the hierarchy:
#' - Items merged in the LAST productive pass become themes
#' - If a theme was formed by merging clusters from a PREVIOUS pass,
#'   those previous-pass clusters become subthemes
#' - Codes that were never merged become standalone themes (single-code themes)
#'
#' @param items Final list of items after all merge passes
#' @param merge_history List tracking merge passes
#' @param coding_state ProgressiveCodingState
#' @return List with themes (each having optional subthemes)
#' @keywords internal
.determine_theme_subtheme_structure <- function(items, merge_history, coding_state) {
  themes <- list()

  for (item in items) {
    if (length(item$codes) == 0) next

    # If this item has children from merging, they become subthemes
    subthemes <- NULL
    if (!is.null(item$children) && length(item$children) > 1) {
      # Each child that itself contains multiple codes becomes a subtheme
      subthemes <- list()
      for (child in item$children) {
        if (length(child$codes) > 0) {
          subthemes[[length(subthemes) + 1L]] <- list(
            name = child$label %||% paste("Subtheme", length(subthemes) + 1),
            description = child$description %||% "",
            code_keys = child$codes
          )
        }
      }
      # Only keep subthemes if there are at least 2
      # (a single subtheme is just the theme itself)
      if (length(subthemes) < 2) subthemes <- NULL
    }

    themes[[length(themes) + 1L]] <- list(
      name = item$label,
      description = item$description %||% "",
      all_code_keys = item$codes,
      subthemes = subthemes
    )
  }

  list(themes = themes)
}

# ==============================================================================
# Deterministic code-path cascading
# ==============================================================================

#' Cascade theme assignments from codes to entries deterministically
#'
#' For each entry, looks up its assigned codes, maps each code to a theme
#' (and optionally a subtheme) via the merge history, and marks the entry's
#' theme memberships. An entry belongs to EVERY theme that contains any of
#' its codes -- there is no primary/secondary distinction.
#'
#' @param data Tibble with std_id column
#' @param coding_state ProgressiveCodingState
#' @param theme_set ThemeSet with merge_history attached
#' @return Tibble with emerged_themes, theme_membership_* columns,
#'   and subtheme_assignments added
#' @export
cascade_theme_assignments <- function(data, coding_state, theme_set) {
  if (!inherits(coding_state, "ProgressiveCodingState")) {
    stop("coding_state must be a ProgressiveCodingState object")
  }
  validate_class(theme_set, "ThemeSet")

  merge_history <- theme_set$merge_history
  if (is.null(merge_history) || length(merge_history$code_to_theme_map) == 0) {
    log_warn("No code-to-theme mapping -- cannot cascade")
    data$emerged_themes <- NA_character_
    data$n_themes <- 0L
    return(data)
  }

  code_to_theme <- merge_history$code_to_theme_map
  code_to_subtheme <- merge_history$code_to_subtheme_map %||% list()
  valid_themes <- theme_names(theme_set)

  log_info("Cascading theme assignments for {nrow(data)} entries...")
  tic("Theme cascading")

  # Initialize columns
  data$emerged_themes <- NA_character_
  data$n_themes <- 0L
  data$subtheme_assignments <- NA_character_

  # Binary membership columns for correlations
  for (tn in valid_themes) {
    safe_col <- paste0("theme_membership_", make.names(tn))
    data[[safe_col]] <- 0L
  }

  n_assigned <- 0L
  n_unassigned <- 0L

  for (i in seq_len(nrow(data))) {
    entry_id <- as.character(data$std_id[i])
    er <- coding_state$entry_results[[entry_id]]

    if (is.null(er) || isTRUE(er$skipped) || length(er$codes_assigned) == 0) {
      n_unassigned <- n_unassigned + 1L
      next
    }

    # Map each code to its theme(s) and subtheme(s)
    entry_themes <- character(0)
    entry_subthemes <- character(0)

    for (code_key in er$codes_assigned) {
      theme_name <- code_to_theme[[code_key]]
      if (!is.null(theme_name) && theme_name %in% valid_themes) {
        entry_themes <- c(entry_themes, theme_name)
      }

      subtheme_name <- code_to_subtheme[[code_key]]
      if (!is.null(subtheme_name)) {
        entry_subthemes <- c(entry_subthemes, subtheme_name)
      }
    }

    # Order themes by code overlap count (most codes first)
    theme_counts <- table(entry_themes)
    entry_themes <- names(sort(theme_counts, decreasing = TRUE))
    entry_subthemes <- unique(entry_subthemes)

    if (length(entry_themes) == 0) {
      n_unassigned <- n_unassigned + 1L
      next
    }

    data$emerged_themes[i] <- paste(entry_themes, collapse = "; ")
    data$n_themes[i] <- length(entry_themes)
    if (length(entry_subthemes) > 0) {
      data$subtheme_assignments[i] <- paste(entry_subthemes, collapse = "; ")
    }

    # Set binary membership columns
    for (tn in entry_themes) {
      safe_col <- paste0("theme_membership_", make.names(tn))
      if (safe_col %in% names(data)) data[[safe_col]][i] <- 1L
    }

    n_assigned <- n_assigned + 1L
  }

  toc()
  log_info("Theme cascading: {n_assigned} assigned, {n_unassigned} unassigned")

  if (n_assigned > 0) {
    # Report theme distribution
    all_themes <- unlist(strsplit(data$emerged_themes[!is.na(data$emerged_themes)], "; "))
    theme_dist <- sort(table(all_themes), decreasing = TRUE)
    for (tn in names(theme_dist)) {
      log_info("  {tn}: {theme_dist[tn]} entries")
    }
  }

  data
}

# ==============================================================================
# Theme enrichment
# ==============================================================================

#' Apply framework constructs as themes (Mode 3 dispatch for theming)
#'
#' Replaces the inductive bottom-up theme generation with a deterministic
#' mapping from framework constructs to themes. Each framework construct
#' becomes a theme; codes (which in Mode 3 are construct_ids) are
#' included verbatim under their parent construct. The "anomaly" code,
#' if non-empty, becomes its own theme so anomalies are visible in the
#' report rather than buried in the codebook.
#'
#' Per AC8 (modes are configurations of one architecture, never separate
#' code paths): the returned ThemeSet has the same shape as one produced
#' by \code{generate_themes_iterative()}, so all downstream consumers
#' (cascade_theme_assignments, aggregate_theme_statistics, report
#' rendering) work without modification.
#'
#' @param coding_state A \code{ProgressiveCodingState} from a Mode 3 run.
#'   The codebook keys are construct_ids (plus "anomaly").
#' @param framework_spec A loaded \code{FrameworkSpec}.
#' @return A \code{ThemeSet} S3 object: one theme per construct that has
#'   at least one coded entry (constructs with zero entries are dropped),
#'   plus an "Anomaly" theme when anomalies are present.
#' @export
apply_framework_themes <- function(coding_state, framework_spec) {
  validate_class(coding_state, "ProgressiveCodingState")
  validate_class(framework_spec, "FrameworkSpec")

  themes <- list()
  next_id <- 1L

  # Each construct -> one theme (when at least one entry was coded with it)
  for (c in framework_spec$constructs) {
    cb_entry <- coding_state$codebook[[c$id]]
    if (is.null(cb_entry) || (cb_entry$frequency %||% 0L) == 0L) next
    themes[[length(themes) + 1L]] <- list(
      id              = next_id,
      name            = c$name,
      description     = c$description,
      codes_included  = c$id,
      keywords        = c$example_indicators %||% character(0),
      subthemes       = character(0),
      framework_construct_id = c$id  # mode-specific marker
    )
    next_id <- next_id + 1L
  }

  # Anomaly theme: surfaces non-fitting segments per the framework's
  # anomaly_handling policy (Vila-Henninger 2024 abductive coding).
  anomaly_entry <- coding_state$codebook[["anomaly"]]
  if (!is.null(anomaly_entry) && (anomaly_entry$frequency %||% 0L) > 0L) {
    themes[[length(themes) + 1L]] <- list(
      id              = next_id,
      name            = "Anomaly (non-fitting)",
      description     = paste0(
        "Segments that resist the '", framework_spec$name, "' framework. ",
        "Per the framework's anomaly_handling policy ('",
        framework_spec$anomaly_handling, "'), these are surfaced as a ",
        "first-class output rather than forced into a construct that ",
        "doesn't fit."
      ),
      codes_included  = "anomaly",
      keywords        = character(0),
      subthemes       = character(0),
      framework_construct_id = "anomaly"
    )
  }

  if (length(themes) == 0L) {
    log_warn(paste0("apply_framework_themes: no constructs received any ",
                     "coded entries -- generating empty theme set"))
  }

  ts <- create_theme_set(themes)

  # AC8: ThemeSet must have the SAME shape that generate_themes_iterative
  # produces, so cascade_theme_assignments + downstream consumers
  # (aggregate_theme_statistics, enrich_themes, report rendering) work
  # without modification. Populate merge_history$code_to_theme_map keyed
  # by code_key (= construct id in Mode 3) -> theme$name. Without this,
  # cascade_theme_assignments bails out with "No code-to-theme mapping",
  # no theme_membership_* columns get set, and all themes render with
  # n_entries = 0 -- exactly the silent end-to-end Mode 3 failure that
  # phase 29's tests didn't catch.
  ts <- rebuild_code_to_theme_map(ts, coding_state)
  ts
}

#' Enrich themes with entry counts, sentiment, and quotes
#'
#' @param theme_set ThemeSet object
#' @param data Tibble with theme_membership_* and sentiment columns
#' @param coding_state ProgressiveCodingState (optional)
#' @param quotes_per_theme Integer; number of representative quotes to
#'   select per theme. Wired through from
#'   \code{config$analysis$themes$quotes_per_theme}; defaults to 3.
#' @return Enriched ThemeSet
#' @export
enrich_themes <- function(theme_set, data, coding_state = NULL,
                            quotes_per_theme = 3L) {
  validate_class(theme_set, "ThemeSet")

  # Use theme_membership_* columns for entry counting (deterministic)
  for (i in seq_along(theme_set$themes)) {
    tn <- theme_set$themes[[i]]$name
    safe_col <- paste0("theme_membership_", make.names(tn))

    # Find entries belonging to this theme via membership column or emerged_themes
    if (safe_col %in% names(data)) {
      theme_entries <- data[data[[safe_col]] == 1L, ]
    } else if ("emerged_themes" %in% names(data)) {
      theme_entries <- data[!is.na(data$emerged_themes) &
                             grepl(tn, data$emerged_themes, fixed = TRUE), ]
    } else {
      theme_entries <- data[0, ]  # empty
    }

    theme_set$themes[[i]]$entry_count <- nrow(theme_entries)

    total <- nrow(data)
    if (total > 0) {
      pct <- nrow(theme_entries) / total
      theme_set$themes[[i]]$prevalence <- if (pct >= 0.30) "high"
        else if (pct >= 0.10) "medium" else "low"
    }

    if ("sentiment_score" %in% names(theme_entries) && nrow(theme_entries) > 0) {
      mean_sent <- mean(theme_entries$sentiment_score, na.rm = TRUE)
      theme_set$themes[[i]]$sentiment_tendency <- if (is.na(mean_sent)) "neutral"
        else if (mean_sent > .SENTIMENT_TENDENCY_THRESHOLD) "positive"
        else if (mean_sent < -.SENTIMENT_TENDENCY_THRESHOLD) "negative"
        else "mixed"
    }

    if (nrow(theme_entries) > 0) {
      # T0.2 spread-aware sentiment-positioned quote selection. The previous
      # approach was random sampling -- which (a) didn't match the
      # most-negative / median / most-positive labels the report renders, and
      # (b) would happily return three quotes from one heavy poster. Now we
      # call .select_representative_quotes (which is spread-aware and
      # respects sentiment positions), then preserve the ordered
      # character-vector shape that aggregate_theme_statistics expects.
      selected <- .select_representative_quotes(theme_entries,
                                                   n_quotes = quotes_per_theme)
      ordered_labels <- intersect(
        c("most_negative", "median", "most_positive"),
        names(selected)
      )
      qtexts <- vapply(
        ordered_labels,
        function(lbl) substr(as.character(selected[[lbl]]$text %||% ""), 1, 200),
        character(1)
      )
      qtexts <- qtexts[nchar(qtexts) > 0]
      if (length(qtexts) > 0L) {
        theme_set$themes[[i]]$supporting_quotes <- unname(qtexts)
      }
    }

    # In Mode 3, apply_framework_themes already set keywords =
    # framework$example_indicators (the participant phrases the model
    # was told to look for). Overwriting with codes_included would erase
    # exactly the framework signal Mode 3 is supposed to surface, so
    # detect Mode 3 themes via the framework_construct_id marker and
    # preserve their keywords. Mode 2 keeps the existing behavior.
    if (is.null(theme_set$themes[[i]]$framework_construct_id)) {
      theme_set$themes[[i]]$keywords <- theme_set$themes[[i]]$codes_included
    }
  }

  theme_set
}

# ==============================================================================
# Co-occurrence and embedding helpers for merge context
# ==============================================================================

#' Compute code co-occurrence matrix from coding state
#'
#' Counts how many entries contain each pair of codes simultaneously.
#'
#' @param coding_state ProgressiveCodingState
#' @return Named list keyed by sorted "code_a|code_b" pair-key, each value
#'   the integer count of entries in which the two codes co-occurred.
#' @keywords internal
.compute_code_cooccurrence <- function(coding_state) {
  co_occ <- list()

  for (er in coding_state$entry_results) {
    if (isTRUE(er$skipped)) next
    codes <- er$codes_assigned
    if (length(codes) < 2) next

    for (i in seq_len(length(codes) - 1)) {
      for (j in (i + 1):length(codes)) {
        a <- codes[i]
        b <- codes[j]
        # Use sorted key to avoid double-counting
        key <- paste(sort(c(a, b)), collapse = "|")
        co_occ[[key]] <- (co_occ[[key]] %||% 0L) + 1L
      }
    }
  }

  co_occ
}

#' Build merge context string with co-occurrence and embedding similarity
#'
#' Provides the AI with additional quantitative evidence to inform merge decisions.
#'
#' @param new_item The item being placed
#' @param clusters Current clusters
#' @param co_occurrence Co-occurrence list from .compute_code_cooccurrence
#' @param code_similarity Cosine similarity matrix from embeddings (or NULL)
#' @return Character string to insert into the merge prompt (may be empty)
#' @keywords internal
.build_merge_context <- function(new_item, clusters, co_occurrence, code_similarity) {
  if (is.null(co_occurrence) && is.null(code_similarity)) return("")

  new_codes <- new_item$codes
  if (length(new_codes) == 0) return("")

  context_lines <- character(0)

  for (ci in seq_along(clusters)) {
    cl <- clusters[[ci]]
    cl_codes <- cl$codes

    # Co-occurrence: count shared entries between new item's codes and cluster's codes
    if (!is.null(co_occurrence) && length(cl_codes) > 0) {
      shared_entries <- 0L
      for (nc in new_codes) {
        for (cc in cl_codes) {
          key <- paste(sort(c(nc, cc)), collapse = "|")
          shared_entries <- shared_entries + (co_occurrence[[key]] %||% 0L)
        }
      }
      if (shared_entries > 0) {
        context_lines <- c(context_lines,
          sprintf("  - Cluster %d: %d shared entry co-occurrences", ci, shared_entries))
      }
    }

    # Embedding similarity: average cosine similarity between code sets
    if (!is.null(code_similarity) && length(cl_codes) > 0) {
      new_in_mat <- new_codes[new_codes %in% rownames(code_similarity)]
      cl_in_mat <- cl_codes[cl_codes %in% rownames(code_similarity)]
      if (length(new_in_mat) > 0 && length(cl_in_mat) > 0) {
        sims <- code_similarity[new_in_mat, cl_in_mat, drop = FALSE]
        avg_sim <- round(mean(sims), 2)
        if (avg_sim > 0.3) {  # Only show if meaningfully similar
          context_lines <- c(context_lines,
            sprintf("  - Cluster %d: embedding similarity = %.2f", ci, avg_sim))
        }
      }
    }
  }

  if (length(context_lines) == 0) return("")

  paste0(
    "## QUANTITATIVE CONTEXT (for reference, not binding):\n",
    paste(context_lines, collapse = "\n"),
    "\n\n"
  )
}

# ==============================================================================
# Deprecated
# ==============================================================================
