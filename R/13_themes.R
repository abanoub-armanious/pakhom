# ==============================================================================
# Theme Generation: dispatch, deterministic cascade, and shared helpers
# ==============================================================================

# Per-session warning state (so the algorithm='v1' deprecation note is emitted
# once per session, not on every theme-generation call).
.themes_warn_state <- new.env(parent = emptyenv())
# Theme generation runs the multi-pass, embedding-free clustering engine in
# R/theme_algorithm_v2.R: the AI proposes a partition of the whole codebook,
# regroups those clusters over further passes until it declares the partition
# converged, and assigns theme / subtheme names in a dedicated labelling pass
# only after the structure is fixed. This file holds the parts of theme
# generation that sit AROUND that engine:
#   - generate_themes_iterative(): the public entry point. It validates its
#     inputs and dispatches to the engine. (An earlier hierarchical
#     agglomerative clustering + AI tree-walk algorithm was removed; a config
#     pinning algorithm = "v1" is honoured as the current engine with a
#     one-time deprecation notice.)
#   - cascade_theme_assignments(): the deterministic entry-to-theme cascade --
#     each entry is mapped to themes / subthemes through its assigned codes with
#     no AI re-reading of raw text, so given a fixed coding_state it reproduces
#     exactly (pure R; the upstream coding that produced the codes is not).
#   - shared helpers (.extract_codes_from_state) plus the
#     deterministic theme post-processing (keyword + sentiment-tendency
#     enrichment) layered on the engine's output for the report.
#
# Design commitments honoured here and by the engine:
#   - C1 (the AI decides when to stop): no hardcoded n_themes, max_themes,
#     min_codes_per_theme, or similarity gates; the AI's per-pass convergence
#     call makes every structural decision.
#   - C2 (codes preserved through clustering): the Code S3 (R/12_theme_data.R)
#     is the atomic leaf; themes / subthemes carry the original codebook keys
#     and indices, never mutated names / descriptions / assignments.
#   - C5 (no catch-all buckets): clustering can only PARTITION the codes it is
#     given -- there is no "Other" verdict and no schema field for inventing a
#     code -- and names are assigned only after the structure is set, so label
#     pressure cannot shape it.
#   - C7 (mode-aware): the Mode 3 framework path pre-populates the codebook with
#     constructs (R/09_coding.R) so clustering operates on a deductive codebook;
#     Mode 1 does not use this file at all (run_mode1 invokes the provocateur
#     loop).
# ==============================================================================

.SENTIMENT_TENDENCY_THRESHOLD <- 0.2

# ==============================================================================
# Main entry point
# ==============================================================================

#' Generate themes by grouping codes into AI-judged clusters
#'
#' Dispatches to the configured theme algorithm. The default
#' (\code{algorithm = "v2"}) is an embedding-free, multi-pass AI
#' clustering: the model sees all codes at once, proposes a partition into
#' clusters, and on each further pass either groups clusters again or
#' declares the partition converged. There are no hardcoded pass counts or
#' size thresholds, and clustering depth is the AI's dynamic call (C1).
#' Codes are grouped, never combined into new codes (C2); theme and subtheme
#' names are assigned in a dedicated labeling pass after convergence. The
#' earlier \code{algorithm = "v1"} -- code-name embeddings,
#' hierarchical agglomerative clustering (ward.D2), and an AI-judged
#' dendrogram walk -- has been removed; pinning \code{algorithm = "v1"} (or
#' any value other than \code{"v2"}) is honored as v2 with a one-time
#' deprecation notice.
#'
#' The function name retains its earlier form for back-compat with
#' the single production caller (R/18_pipeline.R) and existing test
#' fixtures. A future release may rename it to \code{generate_themes()}.
#'
#' @param coding_state \code{ProgressiveCodingState}
#' @param provider \code{AIProvider} object
#' @param config Theme config section (most legacy knobs are now ignored;
#'   the algorithm has no merge-pass parameters)
#' @param learning_context Optional \code{LearningContext}
#' @param research_focus Research focus string
#' @param concepts Optional character vector of core research concepts
#' @param audit_log Optional \code{AuditLog} for recording each AI decision
#' @param response_cache Optional \code{ResponseCache} for raw response capture
#' @param live_tracker Optional \code{LiveTracker}. When provided,
#'   the cluster snapshot is rewritten after every AI decision so a
#'   researcher can `cat outputs/<run>/live/code_to_cluster.json` mid-run.
#' @param methodology_override Optional character. When non-NULL,
#'   replaces the provider's default methodology rules in every internal
#'   \code{ai_complete} call for this walk. Used by the
#'   emergent-themes pass to inject the Mode 3 inductive variant; NULL
#'   for normal Mode 2 + Mode 3 deductive callers.
#' @return \code{ThemeSet} S3 object. Its \code{merge_history} records the
#'   multi-pass partition history (per-pass clusters + the AI's decisions) for
#'   replay / audit.
#' @export
generate_themes_iterative <- function(coding_state, provider, config = list(),
                                       learning_context = NULL,
                                       research_focus = "",
                                       concepts = NULL,
                                       audit_log = NULL,
                                       response_cache = NULL,
                                       live_tracker = NULL,
                                       methodology_override = NULL) {
  if (!inherits(coding_state, "ProgressiveCodingState")) {
    stop("coding_state must be a ProgressiveCodingState object")
  }
  validate_provider(provider, caller = "generate_themes_iterative")

  # Theme generation runs the v2 multi-pass clustering engine (label-after-
  # clustering) in R/theme_algorithm_v2.R. The legacy "v1" HAC + AI tree-walk
  # algorithm was removed; a config that still pins algorithm = "v1" (or any
  # value other than "v2") is honored as v2 with a one-time deprecation note.
  algorithm <- as.character(config$algorithm %||% "v2")
  if (!identical(algorithm, "v2") &&
      !isTRUE(.themes_warn_state$v1_deprecation_warned)) {
    log_warn("themes algorithm '{algorithm}' is no longer available; using v2 (the only supported theme-generation engine).")
    .themes_warn_state$v1_deprecation_warned <- TRUE
  }
  return(generate_themes_multipass(
    coding_state          = coding_state,
    provider              = provider,
    config                = config,
    learning_context      = learning_context,
    research_focus        = research_focus,
    concepts              = concepts,
    audit_log             = audit_log,
    response_cache        = response_cache,
    live_tracker          = live_tracker,
    methodology_override  = methodology_override
  ))

}

# ==============================================================================
# Code extraction
# ==============================================================================

#' Extract codes from coding state into a uniform record list
#'
#' Each record carries: key, name, description, frequency, entry_ids
#' (character vector). This is the canonical input shape for the clustering
#' engine.
#'
#' @keywords internal
.extract_codes_from_state <- function(coding_state) {
  keys <- names(coding_state$codebook)
  if (is.null(keys) || length(keys) == 0L) return(list())

  out <- lapply(keys, function(k) {
    cb <- coding_state$codebook[[k]]
    list(
      key         = k,
      name        = cb$code_name %||% k,
      description = cb$description %||% "",
      frequency   = as.integer(cb$frequency %||% 0L),
      entry_ids   = unique(as.character(cb$entry_ids %||% character(0)))
    )
  })

  # Drop codes with zero frequency (no entries assigned). These usually
  # come from Mode 3 framework constructs where the construct exists in
  # the spec but no entry was coded with it -- already filtered upstream
  # by apply_framework_themes(); for Mode 2 they don't normally occur but
  # filter defensively.
  keep <- vapply(out, function(c) c$frequency > 0L, logical(1))
  out[keep]
}



# ==============================================================================
# Deterministic code-path cascading (kept from the earlier algorithm)
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

  # Mode 3 emergent themes need per-segment routing. The
  # standard code_to_theme_map can map at most ONE theme to the
  # "anomaly" code key, but under anomaly_handling=extend|revise each
  # anomaly segment may be in a different emergent theme. The
  # apply_framework_themes stashes a (entry_id|start|end) -> theme_name
  # map on theme_set; consulted below for entries that have
  # "anomaly" in their codes_assigned.
  anomaly_seg_to_theme <- theme_set$mode3_anomaly_segment_to_theme %||% list()
  has_emergent_routing <- length(anomaly_seg_to_theme) > 0L
  anomaly_segments_by_entry <- if (has_emergent_routing) {
    .group_anomaly_segments_by_entry(coding_state)
  } else list()

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

    # Mode 3 emergent fan-out: if this
    # entry has any "anomaly" segments AND the ThemeSet carries an
    # emergent routing map, look up each segment's emergent theme.
    # Without this branch, every anomaly-bearing entry would either be
    # tagged with NO theme (under extend/revise, since "anomaly" isn't
    # in code_to_theme_map) or with just the single bracket theme.
    if (has_emergent_routing && "anomaly" %in% er$codes_assigned) {
      entry_segs <- anomaly_segments_by_entry[[entry_id]] %||% list()
      for (seg in entry_segs) {
        seg_id <- .segment_identity_key(seg)
        em_theme <- anomaly_seg_to_theme[[seg_id]]
        if (!is.null(em_theme) && em_theme %in% valid_themes) {
          entry_themes <- c(entry_themes, em_theme)
        }
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
# Emergent themes from anomaly segments (Mode 3 extend/revise)
# ==============================================================================
# When a Mode 3 run produces anomaly segments (text the AI couldn't fit any
# framework construct during deductive coding), the anomaly_handling policy
# decides what happens next:
#   "bracket"     -> single Anomaly catch-all theme (earlier behavior)
#   "extend"      -> abductive inductive pass over the anomaly segments,
#                     producing emergent themes parallel to framework themes
#   "revise"      -> same as extend + framework_review.csv artifact for the
#                     researcher to act on for a future run
#
# The inductive pass has three steps:
#   (a) Batch the anomaly segments and ask the AI to generate an inductive
#       code (3-8 word name + 1-sentence description) per segment via
#       .emergent_coding_schema(). The AI is prompted to REUSE code names
#       across segments expressing the same concept -- consolidation here
#       is welcome because it tightens the downstream clustering.
#   (b) Build a synthetic ProgressiveCodingState whose codebook contains
#       these inductive codes (each carrying the segments it labels) and
#       whose entry_results map original entry_ids to these codes.
#   (c) Run generate_themes_iterative() on the synthetic state. The
#       default algorithm is v2 (multi-pass clustering +
#       label-after-clustering); the dispatch in generate_themes_iterative
#       routes here automatically. The resulting themes are "emergent" --
#       patterns the framework didn't anticipate, surfaced inductively
#       from its residuals.
#
# Per AC2/AC8, the framework spec is NOT mutated. The emergent themes
# extend the analysis OUTPUT, not the framework definition. If a
# researcher wants to update the framework based on what the emergent
# themes reveal, they edit the YAML manually and re-run (the "revise"
# policy writes framework_review.csv to make that editing concrete).
# ==============================================================================

#' Generate emergent themes from a Mode 3 run's anomaly segments
#'
#' Entry point for the "extend" / "revise" anomaly-handling policies.
#' Returns a list of theme records (with subthemes + descriptions) that
#' \code{apply_framework_themes} merges into the final ThemeSet, tagged
#' with \code{theme_kind = "emergent"}. The framework themes proper are
#' unaffected.
#'
#' Edge cases:
#' \itemize{
#'   \item 0 anomaly segments: returns empty list (caller should not
#'     normally reach this; \code{apply_framework_themes} short-circuits).
#'   \item 1 anomaly segment: returns one single-code emergent theme
#'     directly (no AI call -- single-code degenerate case under both v1
#'     and v2).
#'   \item N=2 anomaly segments: minimum viable clustering input.
#' }
#'
#' @keywords internal
.generate_emergent_themes_from_anomalies <- function(coding_state,
                                                       framework_spec,
                                                       provider,
                                                       audit_log = NULL,
                                                       response_cache = NULL,
                                                       live_tracker = NULL,
                                                       methodology_override = NULL) {
  anomaly_entry <- coding_state$codebook[["anomaly"]]
  if (is.null(anomaly_entry)) return(list())
  segs <- anomaly_entry$coded_segments %||% list()
  if (length(segs) == 0L) return(list())

  log_info("anomaly_handling=extend|revise: generating emergent themes from {length(segs)} anomaly segment(s)")
  tic("Emergent theme generation")

  # Edge case: 1 segment -> single-code emergent theme, no AI call needed.
  if (length(segs) == 1L) {
    seg <- segs[[1L]]
    code <- create_code_object(
      key = "emergent_001", name = "Single anomaly outlier",
      description = paste0(
        "A single segment that resisted the '", framework_spec$name,
        "' framework. The corpus did not contain enough similar segments ",
        "to cluster into a substantive emergent theme."
      ),
      type = "emergent_inductive", frequency = 1L,
      entry_ids = seg$entry_id %||% character(0),
      coded_segments = list(seg)
    )
    toc()
    return(list(list(
      name        = "Single anomaly outlier",
      description = code$description,
      subthemes   = list(create_subtheme(
        name = NA_character_, description = "", codes = list(code)
      )),
      keywords        = character(0),
      decision_origin = "single_emergent_segment"
    )))
  }

  # Step (a): batch inductive coding of the anomaly segments
  segment_codes <- .inductive_code_anomaly_segments(
    segments             = segs,
    framework_spec       = framework_spec,
    provider             = provider,
    audit_log            = audit_log,
    response_cache       = response_cache,
    methodology_override = methodology_override
  )
  if (length(segment_codes) == 0L) {
    log_warn("Inductive emergent-coding produced no usable codes; falling back to no emergent themes")
    toc()
    return(list())
  }

  # Step (b): build a synthetic ProgressiveCodingState scoped to anomalies
  synth_state <- .build_synthetic_state_from_emergent_codes(
    anomaly_segments = segs,
    segment_codes    = segment_codes
  )

  # Step (c): run the standard theme-generation pipeline on the synthetic
  # state. The default algorithm is v2 (multi-pass
  # clustering + label-after-clustering); the dispatch lives in
  # generate_themes_iterative() so anomaly emergent themes automatically
  # benefit from C-tenets 3+5 the same way Mode 2 themes do. The
  # resulting ThemeSet wrapper isn't needed -- only the inner theme records, so
  # apply_framework_themes can merge them with theme_kind = "emergent".
  emergent_ts <- generate_themes_iterative(
    coding_state         = synth_state,
    provider             = provider,
    config               = list(),
    research_focus       = paste0(
      "Patterns in segments that resisted the '", framework_spec$name,
      "' framework -- abductive emergent themes (Vila-Henninger 2024)."
    ),
    concepts             = NULL,
    audit_log            = audit_log,
    response_cache       = response_cache,
    live_tracker         = live_tracker,
    methodology_override = methodology_override
  )

  toc()
  log_info("Emergent theme generation produced {n_themes(emergent_ts)} theme(s)")

  # Return the inner theme records (sans the ThemeSet wrapper). The caller
  # will assign new IDs + theme_kind = "emergent" + merge with framework
  # themes.
  emergent_ts$themes
}

#' Batch-generate inductive codes for anomaly segments via the AI
#'
#' One AI call per chunk of up to \code{.EMERGENT_BATCH_SIZE} segments.
#' The prompt anchors the AI to the framework name + constructs so its
#' inductive codes are scoped to "what the framework didn't capture."
#' temperature=0 for replay-equivalence.
#'
#' Returns a list parallel to \code{segments}: for each segment, a
#' \code{(code_name, code_description)} pair. NULL entries indicate the
#' AI returned no code for that segment (rare but possible).
#'
#' @keywords internal
.EMERGENT_BATCH_SIZE <- 50L

.inductive_code_anomaly_segments <- function(segments, framework_spec,
                                                provider,
                                                audit_log = NULL,
                                                response_cache = NULL,
                                                methodology_override = NULL) {
  n <- length(segments)
  if (n == 0L) return(list())

  # Pre-allocate the per-segment result vector with NULL entries
  out <- vector("list", n)

  framework_summary <- paste0(
    "## FRAMEWORK CONTEXT\n",
    "These segments were extracted from a corpus that was deductively coded ",
    "against the '", framework_spec$name, "' framework. The framework's ",
    "constructs are:\n",
    paste(vapply(framework_spec$constructs, function(c) {
      paste0("  - ", c$name, ": ", substr(c$description %||% "", 1, 100))
    }, character(1)), collapse = "\n"),
    "\n\nDuring deductive coding the AI judged that each segment below ",
    "did NOT fit any of the constructs above. Your task is abductive: ",
    "generate an inductive code for each segment that captures the ",
    "conceptual content the framework didn't anticipate.\n\n",
    "Reuse code names across segments that express the same concept -- ",
    "consolidation here is welcome (downstream clustering uses these codes).\n"
  )

  system_prompt <- paste0(
    "You are an expert qualitative researcher doing abductive coding ",
    "(Vila-Henninger 2024). You see segments that resisted a theoretical ",
    "framework and your job is to articulate inductive codes that capture ",
    "what the framework missed.\n\n",
    "Conventions:\n",
    "- Code names should be 3-8 words, descriptive of the CONCEPT not the ",
    "  verbatim words used (e.g., 'Coping rituals during deadlines', not ",
    "  'mentioned prayer').\n",
    "- Code descriptions should be 1-2 sentences explaining what the ",
    "  concept IS, in researcher voice.\n",
    "- Reuse the same code name for segments expressing the same concept. ",
    "  Several segments mapping to one code is GOOD -- it tightens the ",
    "  downstream theme clustering.\n",
    "- The response shape is enforced by the structured-output schema."
  )

  # Chunk the segments so the prompt stays bounded
  chunk_starts <- seq(1L, n, by = .EMERGENT_BATCH_SIZE)
  total_chunks <- length(chunk_starts)

  for (chunk_idx in seq_along(chunk_starts)) {
    start_i <- chunk_starts[chunk_idx]
    end_i   <- min(start_i + .EMERGENT_BATCH_SIZE - 1L, n)
    chunk_segments <- segments[start_i:end_i]

    # Build the segments block. Each segment is shown with its 1-based
    # index within the CHUNK (the schema's segment_index is chunk-local).
    segment_lines <- vapply(seq_along(chunk_segments), function(i) {
      seg <- chunk_segments[[i]]
      sprintf("[%d] %s", i, substr(as.character(seg$text %||% ""), 1, 500))
    }, character(1))

    prompt <- paste0(
      framework_summary, "\n",
      sprintf("## ANOMALY SEGMENTS (chunk %d/%d, %d segments)\n\n",
              chunk_idx, total_chunks, length(chunk_segments)),
      paste(segment_lines, collapse = "\n\n"),
      "\n\n## TASK\n",
      "Generate an inductive code (code_name + code_description) for each ",
      "segment above. Return one entry in `coded_segments` per segment, ",
      "with `segment_index` matching the [N] label shown above (1-based ",
      "within this chunk). Reuse code_name + code_description verbatim ",
      "across segments expressing the same concept."
    )

    result <- tryCatch({
      ai_result <- ai_complete(provider, prompt, system_prompt,
                                task = "theming",
                                temperature = 0,
                                response_schema = .emergent_coding_schema(),
                                methodology_override = methodology_override)
      if (!is.null(audit_log)) {
        log_ai_request(audit_log, "theming", ai_result, response_cache,
                        task_variant = "emergent_inductive_coding",
                        chunk_idx = chunk_idx, n_segments = length(chunk_segments))
      }
      parse_json_safely(ai_result$content)
    }, error = function(e) {
      log_warn(paste0(
        "Emergent inductive coding chunk ", chunk_idx, "/", total_chunks,
        " failed: ", conditionMessage(e),
        "; segments in this chunk will have no inductive codes."
      ))
      NULL
    })

    if (is.null(result) || is.null(result$coded_segments)) next

    # parse_json_safely uses simplifyDataFrame=TRUE, so an array of objects
    # comes back as a data.frame rather than a list-of-lists. Normalize to
    # a row-wise list iterator so the placement loop works in all shapes:
    # data.frame (most common; multi-element arrays), list-of-lists (when
    # simplifier punts), and a NAMED scalar list (a known edge case:
    # single-element arrays collapse to a scalar named list under jsonlite
    # auto-simplification; without this branch the placement loop iterates
    # over the values 1, "Code name", "Description" -- each missing the
    # expected fields, and the segment is silently dropped).
    coded <- result$coded_segments
    rows <- if (is.data.frame(coded)) {
      lapply(seq_len(nrow(coded)), function(r) as.list(coded[r, , drop = FALSE]))
    } else if (is.list(coded) && !is.null(names(coded)) &&
                "segment_index" %in% names(coded)) {
      # Single-element collapse: wrap as a one-row list-of-lists
      list(coded)
    } else if (is.list(coded)) {
      coded
    } else {
      next
    }

    # Place each coded segment back into the global `out` slot via the
    # chunk-local segment_index + chunk start offset.
    # Normalize AI-returned code names
    # here too. This admission path doesn't inject a numbered codebook
    # menu so the specific trigger is absent, but the normalizer is
    # the package's general contract for AI-name hygiene -- apply it on
    # every admission site, not just the Mode 2 codebook one.
    for (cs in rows) {
      idx <- as.integer(cs$segment_index %||% NA_integer_)
      if (is.na(idx) || idx < 1L || idx > length(chunk_segments)) next
      global_idx <- (start_i - 1L) + idx
      raw_name <- as.character(cs$code_name %||% "")
      out[[global_idx]] <- list(
        code_name        = .normalize_code_name(raw_name),
        code_description = as.character(cs$code_description %||% "")
      )
    }
  }

  out
}

#' Build a synthetic ProgressiveCodingState scoped to anomaly segments
#'
#' Consolidates per-segment inductive codes (from
#' \code{.inductive_code_anomaly_segments}) by code_name, packing each
#' unique inductive code as a codebook entry whose \code{coded_segments}
#' list contains the anomaly segments labeled with that code. The
#' resulting state can be passed to \code{generate_themes_iterative} as if it
#' were a normal Mode 2 run scoped to just the anomaly residuals.
#'
#' @keywords internal
.build_synthetic_state_from_emergent_codes <- function(anomaly_segments,
                                                          segment_codes) {
  state <- create_coding_state()

  # Group segments by (consolidated) code_name. The AI was prompted to
  # reuse names; here that is honored by treating duplicates as one code.
  # Slug each code_name to a safe codebook key.
  by_name <- list()
  for (i in seq_along(anomaly_segments)) {
    sc <- segment_codes[[i]]
    if (is.null(sc)) next
    name <- sc$code_name
    if (is.null(name) || !nzchar(name)) next
    key <- .slug_emergent_code_name(name)
    if (is.null(by_name[[key]])) {
      by_name[[key]] <- list(
        code_name      = name,
        description    = sc$code_description %||% "",
        seg_indices    = integer(0)
      )
    }
    by_name[[key]]$seg_indices <- c(by_name[[key]]$seg_indices, i)
  }

  # Build the codebook
  for (key in names(by_name)) {
    grp <- by_name[[key]]
    grp_segs <- anomaly_segments[grp$seg_indices]
    entry_ids <- unique(vapply(grp_segs,
      function(s) as.character(s$entry_id %||% NA_character_), character(1)))
    entry_ids <- entry_ids[!is.na(entry_ids)]
    state$codebook[[key]] <- list(
      code_name      = grp$code_name,
      description    = grp$description,
      type           = "emergent_inductive",
      frequency      = length(grp_segs),
      entry_ids      = entry_ids,
      coded_segments = grp_segs
    )
  }

  # Build entry_results so generate_themes_iterative's downstream
  # consumers (cascade, enrich) see the synthetic codes attached to the
  # original entries. Each entry's codes_assigned is the union of the
  # emergent codes that label its anomaly segments.
  for (i in seq_along(anomaly_segments)) {
    sc <- segment_codes[[i]]
    if (is.null(sc) || !nzchar(sc$code_name %||% "")) next
    key <- .slug_emergent_code_name(sc$code_name)
    eid <- as.character(anomaly_segments[[i]]$entry_id %||% NA_character_)
    if (is.na(eid)) next
    if (is.null(state$entry_results[[eid]])) {
      state$entry_results[[eid]] <- list(
        codes_assigned = character(0),
        coded_segments = list(),
        skipped        = FALSE
      )
    }
    state$entry_results[[eid]]$codes_assigned <-
      unique(c(state$entry_results[[eid]]$codes_assigned, key))
  }

  state
}

#' Slug an emergent code name into a safe codebook key
#'
#' Lowercase + underscore-separated + alphanumeric-only. Truncated to 40
#' chars to keep the key readable in audit logs. Duplicate names produce
#' duplicate keys (intentional -- the caller groups by key).
#'
#' @keywords internal
.slug_emergent_code_name <- function(name) {
  s <- tolower(as.character(name %||% ""))
  s <- gsub("[^a-z0-9]+", "_", s)
  s <- gsub("^_+|_+$", "", s)
  s <- substr(s, 1, 40)
  if (!nzchar(s)) s <- "emergent_unnamed"
  paste0("em_", s)
}

#' Write framework_review.csv for the "revise" anomaly_handling policy
#'
#' One row per anomaly segment. Columns:
#' \itemize{
#'   \item \code{entry_id}: original corpus entry the segment came from
#'   \item \code{segment_text}: the segment that didn't fit the framework
#'   \item \code{emergent_theme}: the emergent theme name the inductive
#'     pass assigned this segment to (or NA when the inductive pass
#'     produced no theme)
#'   \item \code{emergent_code}: the per-segment inductive code name
#'   \item \code{suggested_construct_edit} \emph{(blank)}: column for the
#'     researcher to fill -- what change to the framework spec would let
#'     this segment fit a (new or revised) construct?
#'   \item \code{accepted} \emph{(blank)}: column for the researcher to
#'     mark TRUE/FALSE after deciding whether to act on the suggestion
#' }
#'
#' @keywords internal
.write_framework_review_csv <- function(output_dir, coding_state,
                                          framework_spec, emergent_themes,
                                          audit_log = NULL) {
  if (is.null(output_dir) || !nzchar(output_dir)) {
    log_warn("revise policy: no output_dir supplied; skipping framework_review.csv")
    return(invisible(NULL))
  }
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  anomaly_entry <- coding_state$codebook[["anomaly"]]
  segs <- anomaly_entry$coded_segments %||% list()
  if (length(segs) == 0L) {
    return(invisible(NULL))
  }

  # Build a segment -> emergent-theme map by walking the emergent theme
  # records' subthemes -> Code$coded_segments. Each segment carries its
  # entry_id + offsets which uniquely identify it in the anomaly bucket.
  seg_to_theme <- list()
  seg_to_code  <- list()
  for (et in emergent_themes) {
    for (st in et$subthemes %||% list()) {
      if (!inherits(st, "Subtheme")) next
      for (code in st$codes %||% list()) {
        if (!inherits(code, "Code")) next
        for (cseg in code$coded_segments %||% list()) {
          seg_id <- .segment_identity_key(cseg)
          seg_to_theme[[seg_id]] <- et$name %||% NA_character_
          seg_to_code[[seg_id]]  <- code$name %||% NA_character_
        }
      }
    }
  }

  rows <- lapply(segs, function(seg) {
    seg_id <- .segment_identity_key(seg)
    list(
      entry_id                = as.character(seg$entry_id %||% NA_character_),
      segment_text            = as.character(seg$text %||% ""),
      start_char              = as.integer(seg$start_char %||% NA_integer_),
      end_char                = as.integer(seg$end_char %||% NA_integer_),
      emergent_theme          = seg_to_theme[[seg_id]] %||% NA_character_,
      emergent_code           = seg_to_code[[seg_id]] %||% NA_character_,
      suggested_construct_edit = "",
      accepted                = ""
    )
  })

  df <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  out_path <- file.path(output_dir, "framework_review.csv")
  readr::write_csv(df, out_path)
  # AC4: stamp the methodology mode onto this CSV like every other artifact.
  # This path is Mode 3 only (it requires a framework_spec), so the mode is
  # framework_applied.
  tryCatch(stamp_methodology_csv(out_path, "framework_applied",
                                  run_id = basename(output_dir)),
           error = function(e) log_debug("CSV stamp skipped: {e$message}"))
  log_info("revise policy: framework_review.csv written to {out_path} ({nrow(df)} rows)")

  if (!is.null(audit_log)) {
    log_ai_decision(audit_log, "theming", "framework_revision_suggested",
                    n_segments = nrow(df),
                    policy = "revise",
                    framework_name = framework_spec$name,
                    csv_path = out_path)
  }

  invisible(out_path)
}

#' Stable identity key for a coded segment (used to dedupe across views)
#' @keywords internal
.segment_identity_key <- function(seg) {
  paste0(
    as.character(seg$entry_id %||% "NA"), "|",
    as.integer(seg$start_char %||% -1L), "|",
    as.integer(seg$end_char %||% -1L)
  )
}

#' Group anomaly segments by entry_id for cascade lookup
#'
#' Used by cascade_theme_assignments under Mode 3 extend/revise. Walks
#' \code{coding_state$codebook[["anomaly"]]$coded_segments} once and indexes
#' them by entry_id so per-entry segment routing is O(1) per entry
#' rather than O(N) where N is the total anomaly segment count.
#'
#' @keywords internal
.group_anomaly_segments_by_entry <- function(coding_state) {
  anom <- coding_state$codebook[["anomaly"]]
  segs <- anom$coded_segments %||% list()
  if (length(segs) == 0L) return(list())
  out <- list()
  for (seg in segs) {
    eid <- as.character(seg$entry_id %||% NA_character_)
    if (is.na(eid)) next
    if (is.null(out[[eid]])) out[[eid]] <- list()
    out[[eid]][[length(out[[eid]]) + 1L]] <- seg
  }
  out
}

#' Build a segment-identity -> emergent-theme-name map for cascade fan-out
#'
#' Walks the emergent theme records (each carrying first-class Subtheme
#' S3 -> Code S3 -> coded_segments) and produces a flat list keyed by
#' \code{.segment_identity_key(segment)} mapping to the emergent theme
#' the segment ended up in. Used by \code{cascade_theme_assignments} to
#' route the entries whose anomaly segments landed in each emergent
#' theme into the correct \code{theme_membership_*} columns.
#'
#' Without this map the cascade can only
#' route "anomaly" once (to a single theme), which under extend/revise
#' policies means emergent themes render with entry_count = 0 -- a
#' silent data-loss bug that defeats the whole purpose of the emergent-themes pass.
#'
#' @keywords internal
.build_anomaly_segment_to_theme_map <- function(emergent_themes_raw) {
  if (length(emergent_themes_raw) == 0L) return(list())
  out <- list()
  for (et in emergent_themes_raw) {
    tn <- et$name %||% NA_character_
    if (is.na(tn) || !nzchar(tn)) next
    subs <- et$subthemes %||% list()
    for (st in subs) {
      if (!inherits(st, "Subtheme")) next
      for (code in st$codes %||% list()) {
        if (!inherits(code, "Code")) next
        for (cseg in code$coded_segments %||% list()) {
          seg_id <- .segment_identity_key(cseg)
          out[[seg_id]] <- tn
        }
      }
    }
  }
  out
}

# ==============================================================================
# Mode 3 framework dispatch (dispatches on anomaly_handling)
# ==============================================================================

#' Apply framework constructs as themes + handle anomalies (Mode 3)
#'
#' Generates the framework themes from the framework spec, then dispatches
#' on \code{framework_spec$anomaly_handling} to decide what happens to
#' segments that didn't fit any framework construct (the "anomaly" code's
#' segments).
#'
#' Per AC8 (modes are configurations of one architecture, never separate
#' code paths): the returned ThemeSet has the same shape as one produced
#' by \code{generate_themes_iterative()}, so all downstream consumers
#' (cascade_theme_assignments, aggregate_theme_statistics, report
#' rendering) work without modification.
#'
#' \strong{Anomaly policy dispatch}:
#' \itemize{
#'   \item \code{"bracket"}: legacy behavior. Appends a single
#'     "Anomaly (non-fitting)" theme containing all non-fitting segments.
#'   \item \code{"extend"} (default): runs an abductive inductive pass on the
#'     anomaly segments, producing a section of \strong{emergent themes}
#'     parallel to the framework themes. Each emergent theme is tagged
#'     \code{theme_kind = "emergent"} so the report renderer surfaces it
#'     separately. The framework is NOT mutated -- AC2's "framework fixed
#'     at run start" invariant is intact; the analysis output gains a new
#'     section, that's all.
#'   \item \code{"revise"}: same as \code{"extend"} plus writes
#'     \code{framework_review.csv} to \code{output_dir} (one row per anomaly
#'     segment + suggested-edit columns for the researcher). The
#'     existing \code{after_themes} review pause point (configured via
#'     \code{config$analysis$review_points$after_themes}) is the
#'     integration point where the researcher inspects the CSV alongside
#'     framework + emergent themes and decides whether to edit the
#'     framework spec for a future run. A dedicated
#'     \code{after_framework_coding} pause is deferred (requires
#'     resumable runs with in-flight spec edits, beyond current
#'     checkpoint scope).
#' }
#'
#' The framework themes themselves are unchanged -- AI is still constrained
#' to apply constructs verbatim during the main coding pass (R/09_coding.R);
#' the emergent-themes path operates only on segments that the AI ALREADY
#' classified as "anomaly" during deductive coding. The deductive integrity
#' of the framework analysis is preserved; the inductive pass operates on
#' deductive coding's residuals.
#'
#' @param coding_state A \code{ProgressiveCodingState} from a Mode 3 run.
#'   The codebook keys are construct_ids (plus "anomaly").
#' @param framework_spec A loaded \code{FrameworkSpec}.
#' @param provider Optional \code{AIProvider}. Required when
#'   \code{anomaly_handling} is \code{"extend"} or \code{"revise"} AND
#'   anomalies are present (the inductive pass calls the AI). When NULL
#'   under those policies, falls back to bracket behavior with a warning.
#' @param output_dir Optional run directory path. Required for
#'   \code{"revise"} policy to write \code{framework_review.csv}.
#' @param audit_log Optional \code{AuditLog} threaded into the inductive
#'   pass's AI calls.
#' @param response_cache Optional \code{ResponseCache} threaded through.
#' @param live_tracker Optional \code{LiveTracker} threaded through.
#' @param config Optional \code{ThematicConfig}. When supplied
#'   and the framework spec's anomaly_handling is \code{"extend"} or
#'   \code{"revise"}, the inductive emergent-themes pass receives a
#'   methodology rules override (the inductive-pass variant of the Mode 3
#'   rule, computed via \code{generate_methodology_rules(config,
#'   inductive_pass = TRUE)}) so the AI doesn't see the contradictory
#'   "Do NOT generate new framework constructs" rule from the deductive
#'   default. NULL (the default) falls through to the provider's default
#'   rules -- safe for legacy/test callers; the inductive pass will see
#'   the deductive rule alongside its inductive prompt (the
#'   contradiction the override resolves).
#' @return A \code{ThemeSet} S3 object with framework themes and (under
#'   "extend"/"revise") emergent themes. Themes carry a \code{theme_kind}
#'   field of \code{"framework"} | \code{"emergent"} | \code{"anomaly_bracket"}
#'   so the report renderer can section them.
#' @export
apply_framework_themes <- function(coding_state, framework_spec,
                                    provider = NULL,
                                    output_dir = NULL,
                                    audit_log = NULL,
                                    response_cache = NULL,
                                    live_tracker = NULL,
                                    config = NULL) {
  validate_class(coding_state, "ProgressiveCodingState")
  validate_class(framework_spec, "FrameworkSpec")

  # Under anomaly_handling = extend/revise
  # the inductive emergent-themes pass needs to see the inductive variant of
  # the Mode 3 methodology rule (which permits new-code generation on the
  # anomaly residuals). The default deductive Mode 3 rule says "do NOT
  # generate new framework constructs during coding" -- a direct
  # contradiction with the inductive prompt. Pre-compute the override once
  # here so .generate_emergent_themes_from_anomalies can thread it into
  # both the segment-coding call AND the downstream theme generation for
  # emergent themes. NULL when no config provided (legacy/test callers);
  # the inductive path falls through to the provider default in that case.
  inductive_override <- if (!is.null(config)) {
    tryCatch(generate_methodology_rules(config, inductive_pass = TRUE),
             error = function(e) {
               log_warn("apply_framework_themes: could not compute inductive ",
                        "methodology rules: {e$message}; ",
                        "emergent pass will see default Mode 3 rules.")
               NULL
             })
  } else NULL

  themes <- list()
  next_id <- 1L

  # Each construct -> one framework theme (when at least one entry was
  # coded with it). Build first-class Subtheme containing the construct's
  # hydrated Code. Tag theme_kind = "framework" so the report can section
  # framework themes vs emergent themes.
  for (c in framework_spec$constructs) {
    cb_entry <- coding_state$codebook[[c$id]]
    if (is.null(cb_entry) || (cb_entry$frequency %||% 0L) == 0L) next
    themes[[length(themes) + 1L]] <- list(
      id                      = next_id,
      name                    = c$name,
      description             = c$description,
      subthemes               = list(create_subtheme(
        name = NA_character_, description = "",
        codes = list(.code_from_codebook(c$id, coding_state))
      )),
      keywords                = c$example_indicators %||% character(0),
      framework_construct_id  = c$id,
      theme_kind              = "framework"
    )
    next_id <- next_id + 1L
  }

  # Anomaly policy dispatch. The original kitchen-sink
  # "Anomaly (non-fitting)" catch-all theme is the BRACKET path; EXTEND
  # and REVISE both produce emergent themes via inductive clustering of
  # the anomaly segments.
  anomaly_entry <- coding_state$codebook[["anomaly"]]
  has_anomalies <- !is.null(anomaly_entry) && (anomaly_entry$frequency %||% 0L) > 0L
  policy <- framework_spec$anomaly_handling %||% "extend"

  if (has_anomalies) {
    if (policy == "bracket") {
      themes[[length(themes) + 1L]] <- list(
        id              = next_id,
        name            = "Anomaly (non-fitting)",
        description     = paste0(
          "Segments that resist the '", framework_spec$name, "' framework. ",
          "Per the framework's anomaly_handling policy ('bracket'), these ",
          "are surfaced as a single first-class output rather than ",
          "analyzed inductively. Switch the framework's anomaly_handling ",
          "to 'extend' or 'revise' to cluster these segments into ",
          "emergent themes instead."
        ),
        subthemes       = list(create_subtheme(
          name = NA_character_, description = "",
          codes = list(.code_from_codebook("anomaly", coding_state))
        )),
        keywords                = character(0),
        framework_construct_id  = "anomaly",
        theme_kind              = "anomaly_bracket"
      )
      next_id <- next_id + 1L

    } else if (policy %in% c("extend", "revise")) {
      # Inductive pass requires an AIProvider. If absent, fall back to
      # bracket behavior with a clear warning. Production callers
      # (R/18_pipeline.R) always supply provider; tests sometimes omit it.
      if (is.null(provider)) {
        log_warn(paste0(
          "apply_framework_themes: anomaly_handling='", policy, "' requires an ",
          "AIProvider for inductive emergent-theme generation but none was ",
          "supplied; falling back to bracket behavior (single Anomaly theme)."
        ))
        themes[[length(themes) + 1L]] <- list(
          id              = next_id,
          name            = "Anomaly (non-fitting)",
          description     = paste0(
            "Anomaly segments (provider unavailable for inductive ",
            "clustering; falling back to single bracket theme)."
          ),
          subthemes       = list(create_subtheme(
            name = NA_character_, description = "",
            codes = list(.code_from_codebook("anomaly", coding_state))
          )),
          keywords                = character(0),
          framework_construct_id  = "anomaly",
          theme_kind              = "anomaly_bracket"
        )
        next_id <- next_id + 1L

      } else {
        # Generate emergent themes from the anomaly segments via batch
        # inductive coding + AI-judged clustering.
        emergent_themes_raw <- .generate_emergent_themes_from_anomalies(
          coding_state         = coding_state,
          framework_spec       = framework_spec,
          provider             = provider,
          audit_log            = audit_log,
          response_cache       = response_cache,
          live_tracker         = live_tracker,
          methodology_override = inductive_override
        )

        for (et in emergent_themes_raw) {
          et$id         <- next_id
          et$theme_kind <- "emergent"
          themes[[length(themes) + 1L]] <- et
          next_id <- next_id + 1L
        }

        # Build segment-identity -> emergent-theme-name map.
        # Entry results in Mode 3 record only the
        # "anomaly" code key, not the per-segment inductive codes the
        # emergent pass produced, so the standard code_to_theme_map
        # cascade can't reach emergent themes. Stash this map on the
        # ThemeSet for cascade_theme_assignments to consult.
        emergent_seg_map <- .build_anomaly_segment_to_theme_map(emergent_themes_raw)

        # Revise policy writes the framework_review.csv artifact so the
        # researcher can see anomaly segments side-by-side with the
        # emergent-theme clustering and decide whether to update the
        # framework spec for a future run.
        if (policy == "revise" && !is.null(output_dir)) {
          .write_framework_review_csv(
            output_dir      = output_dir,
            coding_state    = coding_state,
            framework_spec  = framework_spec,
            emergent_themes = emergent_themes_raw,
            audit_log       = audit_log
          )
        }
      }
    } else {
      stop(sprintf(
        "apply_framework_themes: unknown anomaly_handling policy '%s'",
        policy
      ), call. = FALSE)
    }
  }

  # Emit a live cluster snapshot after
  # framework themes are assembled so a researcher cat'ing the
  # code_to_cluster.json mid-run sees the deductive Mode 3 theme
  # construction. Earlier only Mode 2 + Mode 3 emergent walks
  # snapshotted; the deductive framework pass was invisible to live
  # tracking. The snapshot fires once at the end of the deductive
  # pass (multiple emergent walks already snapshot inside the
  # walk_for_themes machinery; this is the orthogonal deductive
  # surface). NULL tracker is a no-op (matches the rest of the file).
  # The snapshot reader at
  # R/live_tracking.R:291-298 expects field names `name`,
  # `description`, `decision_origin`, `n_codes` (from code_indices),
  # `code_keys` (unlist of all subtheme keys). Pre-followup this
  # builder used `proposed_name` (-> NA in the snapshot) and only
  # the first key per subtheme (-> n_codes=0, truncated key list).
  if (!is.null(live_tracker) && length(themes) > 0L) {
    framework_snapshot_themes <- lapply(themes, function(t) {
      # Aggregate every key under every subtheme (not just the first)
      all_keys <- unlist(lapply(t$subthemes %||% list(), function(s) {
        if (inherits(s, "Subtheme")) subtheme_code_keys(s) else character(0)
      }), use.names = FALSE)
      list(
        name            = t$name %||% NA_character_,
        description     = t$description %||% "",
        decision_origin = t$theme_kind %||% "framework",
        code_indices    = seq_along(all_keys),  # surfaces n_codes correctly
        code_keys       = as.character(all_keys)
      )
    })
    tryCatch(
      live_snapshot_clusters(live_tracker,
                              walk_status   = "framework_deductive_complete",
                              themes_so_far = framework_snapshot_themes),
      error = function(e) log_debug(
        "Mode 3 deductive live snapshot skipped: {e$message}"
      )
    )
  }

  if (length(themes) == 0L) {
    log_warn(paste0("apply_framework_themes: no constructs received any ",
                     "coded entries -- generating empty theme set"))
  }

  ts <- create_theme_set(themes)
  ts <- rebuild_code_to_theme_map(ts, coding_state)

  # Stamp the policy + kind summary on the ThemeSet for downstream
  # rendering + the audit trail.
  ts$mode3_anomaly_handling <- policy
  ts$mode3_n_framework_themes <- sum(vapply(themes,
    function(t) identical(t$theme_kind, "framework"), logical(1)))
  ts$mode3_n_emergent_themes <- sum(vapply(themes,
    function(t) identical(t$theme_kind, "emergent"), logical(1)))

  # cascade_theme_assignments routes entries
  # via code_to_theme_map keyed by code_key. In Mode 3 the only key
  # written into entry_results$codes_assigned for anomaly segments is
  # the literal "anomaly" -- the per-segment inductive codes are not
  # reflected in coding_state, so the standard cascade reaches at most
  # ONE theme for ALL anomaly entries. Under extend/revise there is a
  # per-segment mapping (via .build_anomaly_segment_to_theme_map); stash
  # it on the ThemeSet so cascade can fan entries out into the correct
  # emergent themes (one entry can contribute to multiple emergent
  # themes when it has multiple anomaly segments in different clusters).
  if (exists("emergent_seg_map", inherits = FALSE) &&
      length(emergent_seg_map) > 0L) {
    ts$mode3_anomaly_segment_to_theme <- emergent_seg_map
  }

  ts
}

# ==============================================================================
# Theme enrichment (kept from the earlier algorithm)
# ==============================================================================

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
      theme_entries <- data[.entry_in_theme(data$emerged_themes, tn), ]
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
      # (b) would happily return three quotes from one heavy poster. It now
      # calls .select_representative_quotes (which is spread-aware and
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
        # word-boundary truncation (reuses the shared
        # helper) so display quotes don't sever mid-word; visible " ..." marker.
        # Keep the `%||% ""` guard -- the helper's is.na() check errors on NULL.
        function(lbl) .truncate_quote_word_boundary(selected[[lbl]]$text %||% "",
                                                    max_chars = 200L),
        character(1)
      )
      qtexts <- qtexts[nchar(qtexts) > 0]
      if (length(qtexts) > 0L) {
        theme_set$themes[[i]]$supporting_quotes <- unname(qtexts)
        # parallel structured records so a
        # downstream consumer can trace each quote text back to its
        # source entry. The bare-string supporting_quotes field is
        # preserved verbatim for back-compat with any consumer that
        # reads the legacy shape; new consumers should prefer
        # supporting_quote_records.
        records <- lapply(ordered_labels, function(lbl) {
          s <- selected[[lbl]]
          if (is.null(s) || is.null(s$text) || !nzchar(s$text)) return(NULL)
          list(
            # word-boundary truncation (same call as the bare-string
            # supporting_quotes above, so the two fields stay consistent). These
            # are DISPLAY strings only -- no offsets, never re-verified against
            # source (T0.1 runs on the separate coded_segments path), so changing
            # the truncated length cannot touch the provenance contract.
            text         = .truncate_quote_word_boundary(as.character(s$text),
                                                         max_chars = 200L),
            sentiment_score = s$sentiment %||% NA_real_,
            entry_id     = s$entry_id %||% NA_character_,
            source_table = s$source_table %||% NA_character_,
            std_author   = s$author %||% NA_character_,
            position     = lbl  # one of "most_negative" / "median" / "most_positive"
          )
        })
        records <- Filter(Negate(is.null), records)
        if (length(records) > 0L) {
          theme_set$themes[[i]]$supporting_quote_records <- records
        }
      }
    }

    # In Mode 3, apply_framework_themes already set keywords =
    # framework$example_indicators (the participant phrases the model
    # was told to look for). Overwriting with codes_included would erase
    # exactly the framework signal Mode 3 is supposed to surface, so
    # detect Mode 3 themes via the framework_construct_id marker and
    # preserve their keywords. Mode 2 keeps a SUBSET of the codes.
    if (is.null(theme_set$themes[[i]]$framework_construct_id)) {
      # an earlier version of this assignment copied
      # ALL codes (a verbatim duplicate of codes_included) into the
      # keywords field. An audit measured every theme's keywords
      # = codes_included verbatim, including the 237-code mega-theme
      # carrying 237 keywords. Payload bloat + misleading field name
      # (users reasonably expect 5-15 representative terms, not the
      # full code inventory).
      #
      # Fix: keep the top-N codes by frequency in the theme's
      # codebook. Falls back to all-codes when length < N. This makes
      # keywords a useful highlight subset without losing the data --
      # the full code inventory remains in codes_included +
      # subthemes_structured.
      keyword_cap <- 8L
      theme_codes_full <- theme_codes(theme_set$themes[[i]])
      if (length(theme_codes_full) > keyword_cap) {
        # Rank by frequency: pull from coding_state$codebook when
        # available; otherwise fall back to identity order.
        if (!is.null(coding_state$codebook)) {
          # Use the canonical
          # theme_code_keys() helper rather than tolower(name)
          # round-trip. The code key IS lowercase(name) for
          # ASCII names but may diverge for unicode / normalized
          # names; the canonical accessor returns the keys directly
          # without re-deriving.
          theme_code_key_vec <- theme_code_keys(theme_set$themes[[i]])
          freqs <- vapply(theme_code_key_vec, function(k) {
            cb <- coding_state$codebook[[k]]
            if (is.null(cb)) 0L else as.integer(cb$frequency %||% 0L)
          }, integer(1))
          ord <- order(freqs, decreasing = TRUE)
          theme_set$themes[[i]]$keywords <- theme_codes_full[
            utils::head(ord, keyword_cap)
          ]
        } else {
          theme_set$themes[[i]]$keywords <- utils::head(
            theme_codes_full, keyword_cap
          )
        }
      } else {
        theme_set$themes[[i]]$keywords <- theme_codes_full
      }
    }
  }

  theme_set
}
