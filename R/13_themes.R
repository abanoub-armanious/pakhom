# ==============================================================================
# Theme Generation: HAC + AI-judged Divisive Tree-Walk
# ==============================================================================
# Phase 52 rewrite. Replaces the pre-Phase-52 sequential pairwise insertion
# (which produced the kitchen-sink theme bug -- one theme absorbing 593/727
# entries on the saturation run) with deterministic hierarchical agglomerative
# clustering (HAC, ward.D2 linkage, cosine distance from code-name embeddings
# with Jaccard fallback for non-OpenAI providers) followed by an AI-judged
# top-down divisive tree walk. The AI sits at every internal node of the
# HAC tree and decides: coherent_theme | split_required | atomic_outlier.
#
# Bias mitigations enforced by .theme_decision_schema():
#   (a) Articulation requirement -- the AI must write the central organizing
#       concept BEFORE the decision. If forcing one feels artificial it must
#       say so explicitly there.
#   (b) Closed three-valued enum -- no hedging.
#   (c) Most-distant code pair shown unconditionally in every prompt;
#       rationale field requires addressing it specifically.
#
# The C1 commitment (AI decides when to stop) is honored: NO hardcoded
# n_themes, max_themes, max_merge_passes, similarity gates, or stopping
# heuristics. The HAC tree gives a deterministic skeleton; the AI cuts it.
#
# Entry-to-theme cascade is deterministic (cascade_theme_assignments below):
# each entry is mapped to themes/subthemes via its assigned codes, with no
# AI re-reading of raw text. Replay-equivalent given (provider, seed,
# audit_log).
# ==============================================================================

.SENTIMENT_TENDENCY_THRESHOLD <- 0.2

# Maximum codes to enumerate verbatim in a cluster summary prompt. For
# clusters above this size we show the top-N by frequency plus the most
# extreme pairs by cosine distance. Not a methodological knob -- a context-
# window economy bound. See .summarize_cluster_for_prompt() below.
.MAX_PROMPT_CODES <- 50L

# ==============================================================================
# Main entry point
# ==============================================================================

#' Generate themes via HAC + AI-judged divisive tree walk
#'
#' Phase 52 algorithm. Computes pairwise distance between codes (cosine on
#' code-name embeddings; Jaccard fallback on entry-id sets when embeddings
#' are unavailable), runs hierarchical agglomerative clustering (HAC,
#' ward.D2 linkage), then walks the resulting dendrogram top-down with an
#' AI judge at every internal node deciding coherent_theme /
#' split_required / atomic_outlier. For each identified theme, walks one
#' level deeper for subthemes.
#'
#' The function name retains its pre-Phase-52 form for back-compat with
#' the single production caller (R/18_pipeline.R) and existing test
#' fixtures. A future cleanup phase may rename to \code{generate_themes()}.
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
#' @param live_tracker Optional \code{LiveTracker} (Phase 53). When provided,
#'   the cluster snapshot is rewritten after every AI decision so a
#'   researcher can `cat outputs/<run>/live/code_to_cluster.json` mid-run.
#' @return \code{ThemeSet} S3 object with merge_history attached. The
#'   merge_history$tree_walk field carries the HAC tree + per-node
#'   decisions for replay (Phase 52 audit trail).
#' @export
generate_themes_iterative <- function(coding_state, provider, config = list(),
                                       learning_context = NULL,
                                       research_focus = "",
                                       concepts = NULL,
                                       audit_log = NULL,
                                       response_cache = NULL,
                                       live_tracker = NULL) {
  if (!inherits(coding_state, "ProgressiveCodingState")) {
    stop("coding_state must be a ProgressiveCodingState object")
  }
  validate_provider(provider, caller = "generate_themes_iterative")

  # Extract codes from the codebook into a uniform record list
  codes <- .extract_codes_from_state(coding_state)

  # Edge cases: 0 or 1 code
  if (length(codes) == 0L) {
    log_warn("No codes in coding state -- cannot generate themes")
    return(create_theme_set(list()))
  }
  if (length(codes) == 1L) {
    log_info("Only 1 code present -- producing single-theme ThemeSet")
    only <- codes[[1]]
    theme_list <- list(list(
      id = 1L, name = only$name,
      description = only$description %||% "",
      subthemes = list(create_subtheme(
        name = NA_character_, description = "",
        codes = list(.code_from_codebook(only$key, coding_state))
      )),
      prevalence = "medium", sentiment_tendency = "neutral"
    ))
    ts <- create_theme_set(
      themes = theme_list,
      thematic_map = "Single-code corpus; one theme by construction.",
      analysis_notes = "Only 1 code in coding state -- HAC degenerate case."
    )
    ts <- rebuild_code_to_theme_map(ts, coding_state)
    return(ts)
  }

  log_info("Generating themes via HAC + AI tree walk: {length(codes)} codes")
  tic("Theme generation")

  # 1. Distance matrix (cosine on embeddings, or Jaccard fallback)
  dist_obj <- .compute_code_distance_matrix(codes, coding_state, provider)
  dist_matrix <- as.matrix(dist_obj)

  # 2. HAC tree
  hac <- stats::hclust(dist_obj, method = "ward.D2")

  # 3. Co-occurrence (used for prompt context; computed once, reused)
  co_occurrence <- .compute_code_cooccurrence(coding_state)

  # 4. Build the walk context (Phase 53 cleanup of Phase 52 audit
  # MEDIUM-8: the prior implementation passed 14 positional/named args
  # to .evaluate_cluster, .walk_for_themes, and .walk_for_subthemes;
  # the consolidation into walk_ctx + walk_state environment makes the
  # call sites readable and lets us add live_tracker without growing
  # the signature again).
  research_focus_str <- as.character(research_focus %||% "")
  concept_str <- if (!is.null(concepts) && length(concepts) > 0) {
    paste(concepts, collapse = ", ")
  } else {
    research_focus_str
  }

  calibration_text <- ""
  if (!is.null(learning_context) && nchar(learning_context$for_theming %||% "") > 0) {
    calibration_text <- paste0(
      "\n## REFERENCE: How previous human researchers organized codes\n",
      learning_context$for_theming, "\n"
    )
  }

  # walk_state lives in an environment so child closures can mutate it
  # in place without R's `<<-` super-assignment. Matches the pattern in
  # R/audit_log.R and is idiomatic R.
  walk_state <- new.env(parent = emptyenv())
  walk_state$decisions      <- list()
  walk_state$n_calls        <- 0L
  walk_state$n_failed_calls <- 0L
  walk_state$themes_so_far  <- list()  # for live snapshots; populated as walks complete

  walk_ctx <- list(
    provider          = provider,
    research_focus    = research_focus_str,
    concept_str       = concept_str,
    calibration_text  = calibration_text,
    reflexivity_block = config$reflexivity_block %||% "",
    audit_log         = audit_log,
    response_cache    = response_cache,
    live_tracker      = live_tracker,
    walk_state        = walk_state
  )

  # 5. AI tree walk for THEMES (top-down divisive)
  themes_raw <- .walk_for_themes(
    hac_node_idx    = nrow(hac$merge),
    hac             = hac,
    codes           = codes,
    distance_matrix = dist_matrix,
    co_occurrence   = co_occurrence,
    walk_ctx        = walk_ctx
  )

  log_info("Theme walk produced {length(themes_raw)} theme(s) from {walk_state$n_calls} AI decision(s)")
  walk_state$themes_so_far <- .with_code_keys(themes_raw, codes)
  live_snapshot_clusters(live_tracker, walk_status = "theme_walk_complete",
                          walk_state = walk_state,
                          themes_so_far = walk_state$themes_so_far)

  # 6. AI tree walk for SUBTHEMES within each theme (one level deeper)
  themes_raw <- lapply(themes_raw, function(t) {
    if (length(t$code_indices) <= 1L) {
      # Single-code theme -- no subtheme structure
      t$subtheme_groups <- list(list(
        name = NA_character_, description = "",
        code_indices = t$code_indices
      ))
      return(t)
    }

    t$subtheme_groups <- .walk_for_subthemes(
      theme_name      = t$name,
      theme_node_idx  = t$node_idx,
      hac             = hac,
      codes           = codes,
      distance_matrix = dist_matrix,
      co_occurrence   = co_occurrence,
      walk_ctx        = walk_ctx
    )
    t
  })

  log_info("Total AI decisions across theme + subtheme walks: {walk_state$n_calls}")
  walk_state$themes_so_far <- .with_code_keys(themes_raw, codes)
  live_snapshot_clusters(live_tracker, walk_status = "subtheme_walk_complete",
                          walk_state = walk_state,
                          themes_so_far = walk_state$themes_so_far)

  # 6. Build ThemeSet (Phase 51 hierarchy)
  theme_list <- lapply(seq_along(themes_raw), function(i) {
    t <- themes_raw[[i]]

    subthemes <- lapply(t$subtheme_groups, function(g) {
      create_subtheme(
        name        = g$name %||% NA_character_,
        description = g$description %||% "",
        codes       = lapply(g$code_indices, function(ci) {
          .code_from_codebook(codes[[ci]]$key, coding_state)
        })
      )
    })

    list(
      id                 = i,
      name               = t$name,
      description        = t$description %||% "",
      subthemes          = subthemes,
      prevalence         = "medium",
      sentiment_tendency = "neutral"
    )
  })

  ts <- create_theme_set(
    themes       = theme_list,
    thematic_map = sprintf(
      "Generated via HAC (ward.D2, %s distance) + AI tree walk over %d codes; %d AI decisions.",
      attr(dist_obj, "metric") %||% "cosine",
      length(codes), walk_state$n_calls
    ),
    analysis_notes = paste0(
      "Top-down divisive cluster evaluation. Each theme corresponds to a ",
      "cut in the HAC dendrogram where the AI articulated a coherent ",
      "central organizing principle. No hardcoded n_themes/max_themes; ",
      "the AI decides where to cut."
    )
  )

  # Build merge_history-shaped lookup tables. We keep the field name
  # "merge_history" for back-compat with cascade_theme_assignments + audit-
  # log readers, but the new contents reflect the tree-walk algorithm.
  ts$merge_history <- list(
    algorithm           = "hac_divisive_tree_walk",
    distance_metric     = attr(dist_obj, "metric") %||% "cosine",
    linkage             = "ward.D2",
    n_codes             = length(codes),
    n_ai_decisions      = walk_state$n_calls,
    decisions           = walk_state$decisions,
    code_to_theme_map   = list(),
    code_to_subtheme_map = list()
  )
  ts <- rebuild_code_to_theme_map(ts, coding_state)

  if (!is.null(audit_log)) {
    final_theme_names <- vapply(theme_list, function(t) t$name, character(1))
    log_ai_decision(audit_log, "theming", "theme_structure",
                    n_themes = length(theme_list),
                    theme_names = paste(final_theme_names, collapse = "; "),
                    algorithm = "hac_divisive_tree_walk",
                    n_ai_decisions = walk_state$n_calls)
  }

  toc()
  log_info("Generated {length(theme_list)} theme(s) via HAC + AI tree walk ({walk_state$n_calls} AI decisions)")

  ts
}

# ==============================================================================
# Code extraction
# ==============================================================================

#' Extract codes from coding state into a uniform record list
#'
#' Each record carries: key, name, description, frequency, entry_ids
#' (character vector). This is the canonical input shape for the HAC
#' algorithm.
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
  # we filter defensively.
  keep <- vapply(out, function(c) c$frequency > 0L, logical(1))
  out[keep]
}

# ==============================================================================
# Distance matrix: cosine via embeddings, Jaccard fallback
# ==============================================================================

#' Compute pairwise code distance matrix
#'
#' Cosine distance via OpenAI embeddings on "name: description" strings is
#' the preferred metric. When embeddings are unavailable (non-OpenAI
#' provider, network failure, embedding API error) we fall back to
#' Jaccard distance on the entry-id sets -- codes that frequently co-occur
#' on the same entries are treated as similar.
#'
#' Both metrics produce a symmetric \code{dist} object compatible with
#' \code{stats::hclust}. The chosen metric is recorded as an attribute
#' for downstream audit-log stamping.
#'
#' @keywords internal
.compute_code_distance_matrix <- function(codes, coding_state, provider) {
  # Try embeddings first
  if (!is.null(provider$models$embedding)) {
    descs <- vapply(codes, function(c) {
      paste(c$name, c$description, sep = ": ")
    }, character(1))
    embs <- tryCatch(
      compute_embeddings(provider, descs),
      error = function(e) {
        log_warn("Embedding computation failed: {e$message}; falling back to Jaccard")
        NULL
      }
    )
    if (!is.null(embs)) {
      sim <- .cosine_similarity_matrix(embs)
      # Cosine distance = 1 - cosine similarity. Clamp to [0, 2] for
      # numerical safety (cosine similarity may slightly exceed [-1, 1]
      # due to float precision).
      d <- pmax(0, 1 - sim)
      # Symmetrize defensively + zero diagonal
      d <- (d + t(d)) / 2
      diag(d) <- 0
      rownames(d) <- vapply(codes, function(c) c$key, character(1))
      colnames(d) <- rownames(d)
      log_info("Code distance: cosine on {length(codes)}-code embedding matrix")
      out <- stats::as.dist(d)
      attr(out, "metric") <- "cosine_embedding"
      return(out)
    }
  }

  # Jaccard fallback
  log_info("Code distance: Jaccard on entry-id sets (embeddings unavailable)")
  n <- length(codes)
  d <- matrix(0, nrow = n, ncol = n)
  for (i in seq_len(n - 1L)) {
    ei <- codes[[i]]$entry_ids
    for (j in (i + 1L):n) {
      ej <- codes[[j]]$entry_ids
      if (length(ei) == 0L && length(ej) == 0L) {
        # Both empty -- treat as maximally distant (cannot be measured)
        d[i, j] <- 1
      } else {
        inter <- length(intersect(ei, ej))
        uni   <- length(union(ei, ej))
        d[i, j] <- if (uni == 0L) 1 else 1 - (inter / uni)
      }
      d[j, i] <- d[i, j]
    }
  }
  rownames(d) <- vapply(codes, function(c) c$key, character(1))
  colnames(d) <- rownames(d)
  out <- stats::as.dist(d)
  attr(out, "metric") <- "jaccard_entry_ids"
  out
}

# ==============================================================================
# AI tree walk: top-down divisive
# ==============================================================================

#' HAC node helper: map a node index to the leaf code indices under it
#'
#' \code{stats::hclust} encodes the dendrogram as a (n-1) x 2 merge matrix.
#' Negative entries are leaf indices (1..n); positive entries are internal
#' node indices referring to earlier rows of \code{hac$merge}. This helper
#' resolves an "internal node index" (1..n-1) to the set of leaf indices
#' under it. We pass internal-node indices throughout the tree walk
#' because they uniquely identify subtrees.
#'
#' @keywords internal
.leaves_under_node <- function(hac, node_idx) {
  if (node_idx < 0L) return(-node_idx)  # leaf passthrough convention
  out <- integer(0)
  stack <- node_idx
  while (length(stack) > 0L) {
    cur <- stack[length(stack)]
    stack <- stack[-length(stack)]
    if (cur < 0L) {
      out <- c(out, -cur)
    } else {
      pair <- hac$merge[cur, ]
      stack <- c(stack, pair[1], pair[2])
    }
  }
  sort(unique(out))
}

#' Walk the HAC tree top-down to identify themes
#'
#' Recursive divisive walk. At each internal node, the AI sees a summary
#' of the codes under it (with the most-distant pair highlighted) and
#' decides:
#'   coherent_theme  -- this subtree's codes are one theme; stop recursing.
#'   split_required  -- recurse into both children, building separate
#'                       themes from them.
#'   atomic_outlier  -- this subtree's codes are essentially one concept
#'                       (often a leaf or near-leaf); make a theme of it
#'                       and stop recursing.
#'
#' @keywords internal
.walk_for_themes <- function(hac_node_idx, hac, codes, distance_matrix,
                              co_occurrence, walk_ctx) {
  leaves <- .leaves_under_node(hac, hac_node_idx)

  # Single-leaf node -- always an atomic theme
  if (length(leaves) == 1L) {
    return(list(.make_theme_record(leaves, hac_node_idx, codes,
                                    name = codes[[leaves]]$name,
                                    description = codes[[leaves]]$description,
                                    decision_origin = "single_leaf")))
  }

  # Ask the AI to evaluate this cluster
  decision <- .evaluate_cluster(
    cluster_leaves  = leaves,
    node_idx        = hac_node_idx,
    level_label     = "THEME",
    parent_label    = NULL,
    codes           = codes,
    distance_matrix = distance_matrix,
    co_occurrence   = co_occurrence,
    walk_ctx        = walk_ctx
  )

  if (decision$decision %in% c("coherent_theme", "atomic_outlier")) {
    return(list(.make_theme_record(
      leaves, hac_node_idx, codes,
      name        = decision$proposed_name %||% .fallback_theme_name(leaves, codes),
      description = decision$proposed_description %||% "",
      decision_origin = decision$decision
    )))
  }

  # split_required -- recurse into both children of this internal node
  pair <- hac$merge[hac_node_idx, ]
  left_themes  <- .walk_for_themes(.child_node_arg(pair[1]), hac, codes,
                                    distance_matrix, co_occurrence, walk_ctx)
  right_themes <- .walk_for_themes(.child_node_arg(pair[2]), hac, codes,
                                    distance_matrix, co_occurrence, walk_ctx)
  c(left_themes, right_themes)
}

#' Walk a theme's subtree to identify subthemes (one level deeper)
#'
#' For each immediate child of the theme node, the AI judges whether it
#' constitutes a coherent subtheme of the theme. If yes, the child's
#' codes form a Subtheme. If no, the child's codes are flattened directly
#' into the theme (no Subtheme; will be wrapped in a virtual NA-named
#' Subtheme by create_subtheme()/create_theme_set()).
#'
#' Subthemes are at most 1 level deep in Phase 52. Deeper hierarchy
#' (sub-subthemes) is out of scope; if the data calls for it, the
#' researcher can re-run with a tighter research_focus.
#'
#' @keywords internal
.walk_for_subthemes <- function(theme_name, theme_node_idx, hac, codes,
                                  distance_matrix, co_occurrence, walk_ctx) {
  # Single-leaf theme -- no subtheme structure
  if (theme_node_idx < 0L) {
    return(list(list(
      name = NA_character_, description = "",
      code_indices = -theme_node_idx
    )))
  }

  pair <- hac$merge[theme_node_idx, ]
  child_groups <- list()

  for (child in pair) {
    child_leaves <- .leaves_under_node(hac, child)

    # Single-leaf child -- never make it a named subtheme on its own
    if (length(child_leaves) == 1L) {
      child_groups[[length(child_groups) + 1L]] <- list(
        name = NA_character_, description = "",
        code_indices = child_leaves
      )
      next
    }

    decision <- .evaluate_cluster(
      cluster_leaves  = child_leaves,
      node_idx        = if (child < 0L) NA_integer_ else as.integer(child),
      level_label     = "SUBTHEME",
      parent_label    = theme_name,
      codes           = codes,
      distance_matrix = distance_matrix,
      co_occurrence   = co_occurrence,
      walk_ctx        = walk_ctx
    )

    if (decision$decision == "coherent_theme") {
      child_groups[[length(child_groups) + 1L]] <- list(
        name        = decision$proposed_name %||% paste0(theme_name, " (subgroup)"),
        description = decision$proposed_description %||% "",
        code_indices = child_leaves
      )
    } else {
      # split_required or atomic_outlier at the subtheme level: codes flow
      # directly into the parent theme without subtheme grouping. They
      # are accumulated under a virtual NA-named subtheme.
      child_groups[[length(child_groups) + 1L]] <- list(
        name = NA_character_, description = "",
        code_indices = child_leaves
      )
    }
  }

  # Coalesce adjacent virtual (NA-named) groups so codes that flow
  # directly into the theme aren't split across multiple virtual subthemes.
  .coalesce_virtual_subtheme_groups(child_groups)
}

#' Combine adjacent NA-named subtheme groups into one virtual group
#' @keywords internal
.coalesce_virtual_subtheme_groups <- function(groups) {
  if (length(groups) <= 1L) return(groups)
  out <- list()
  cur_virtual <- NULL
  for (g in groups) {
    is_virtual <- is.null(g$name) || is.na(g$name) || nchar(g$name %||% "") == 0L
    if (is_virtual) {
      if (is.null(cur_virtual)) {
        cur_virtual <- g
      } else {
        cur_virtual$code_indices <- c(cur_virtual$code_indices, g$code_indices)
      }
    } else {
      if (!is.null(cur_virtual)) {
        out[[length(out) + 1L]] <- cur_virtual
        cur_virtual <- NULL
      }
      out[[length(out) + 1L]] <- g
    }
  }
  if (!is.null(cur_virtual)) out[[length(out) + 1L]] <- cur_virtual
  out
}

#' Build a theme record from a subtree
#' @keywords internal
.make_theme_record <- function(leaf_indices, node_idx, codes,
                                 name, description, decision_origin) {
  list(
    name             = name,
    description      = description,
    code_indices     = leaf_indices,
    node_idx         = if (length(leaf_indices) == 1L) -leaf_indices[1] else node_idx,
    decision_origin  = decision_origin
  )
}

#' Map an hclust merge-matrix child to a node-index argument
#'
#' Negative -> leaf index; positive -> internal node index.
#' @keywords internal
.child_node_arg <- function(child_val) {
  if (child_val < 0L) child_val else as.integer(child_val)
}

#' Fallback theme name when AI returns null name
#' @keywords internal
.fallback_theme_name <- function(leaf_indices, codes) {
  if (length(leaf_indices) == 1L) return(codes[[leaf_indices]]$name)
  # Pick the highest-frequency code in the cluster as a stand-in
  freqs <- vapply(leaf_indices, function(i) codes[[i]]$frequency, integer(1))
  top <- leaf_indices[which.max(freqs)]
  paste(codes[[top]]$name, "(and related)")
}

#' Add code_keys to theme records for the live cluster snapshot
#'
#' Theme records produced by the walks carry \code{code_indices} (positions
#' in the codes list); the live snapshot writer wants the actual codebook
#' keys for human-readable output. This helper resolves the mapping.
#' @keywords internal
.with_code_keys <- function(themes_raw, codes) {
  lapply(themes_raw, function(t) {
    t$code_keys <- vapply(t$code_indices, function(i) codes[[i]]$key, character(1))
    t
  })
}

# ==============================================================================
# Single AI decision call
# ==============================================================================

#' Ask the AI to evaluate a cluster
#'
#' Builds the cluster summary prompt (with bias-mitigation context: most-
#' distant pair, full per-code list when small, top-N + extremes when
#' large) and calls \code{ai_complete()} with the
#' \code{.theme_decision_schema()}.
#'
#' Returns a structured decision record (decision, name, description,
#' rationale, articulation). Records the decision in walk_state and the
#' audit_log.
#'
#' @keywords internal
.evaluate_cluster <- function(cluster_leaves, node_idx, level_label,
                                parent_label = NULL,
                                codes, distance_matrix, co_occurrence,
                                walk_ctx) {
  # Phase 53 cleanup of Phase 52 audit MEDIUM-8 + LOW-10:
  # walk_state is now an environment, mutated in place without `<<-`.
  # walk_ctx packs the long parameter list from the pre-cleanup version.
  walk_state        <- walk_ctx$walk_state
  provider          <- walk_ctx$provider
  research_focus    <- walk_ctx$research_focus
  concept_str       <- walk_ctx$concept_str
  calibration_text  <- walk_ctx$calibration_text
  reflexivity_block <- walk_ctx$reflexivity_block
  audit_log         <- walk_ctx$audit_log
  response_cache    <- walk_ctx$response_cache
  live_tracker      <- walk_ctx$live_tracker

  walk_state$n_calls <- walk_state$n_calls + 1L
  call_idx <- walk_state$n_calls

  cluster_summary <- .summarize_cluster_for_prompt(
    cluster_leaves = cluster_leaves, codes = codes,
    distance_matrix = distance_matrix, co_occurrence = co_occurrence
  )

  parent_block <- if (!is.null(parent_label)) {
    paste0(
      "## PARENT THEME\n",
      "You are evaluating whether this cluster forms a coherent SUBTHEME ",
      "of: \"", parent_label, "\".\n\n"
    )
  } else ""

  system_prompt <- paste0(
    "You are an expert qualitative researcher evaluating whether a cluster ",
    "of inductively-generated codes shares a single, articulable conceptual ",
    "organizing principle. The clustering algorithm (hierarchical agglomerative ",
    "with cosine distance on code embeddings) has produced a candidate group; ",
    "your job is to decide whether the AI's articulation of that group's ",
    "central concept is honest or whether the cluster has internal fault ",
    "lines that warrant splitting it.\n\n",
    "Research focus: ", research_focus, "\n",
    "Core concepts: ", concept_str, "\n",
    reflexivity_block,
    "\n## YOUR TASK\n",
    "FIRST: articulate, in your own words, the central organizing concept ",
    "that you believe unifies ALL the codes in the cluster. If forcing one ",
    "feels artificial -- if the most-distant code pair stretches the ",
    "principle -- say so explicitly. The articulation must be honest.\n\n",
    "THEN: decide.\n",
    "  coherent_theme:  A clear, non-stretched principle unifies ALL codes.\n",
    "                   Propose a 5-12 word ",
    if (level_label == "SUBTHEME") "subtheme" else "theme",
    " name + 1-2 sentence description.\n",
    "  split_required:  No single principle works; the cluster has at least\n",
    "                   one conceptual fault line. Set proposed_name and\n",
    "                   proposed_description to null.\n",
    "  atomic_outlier:  This is essentially one code (or set so tightly bound\n",
    "                   it acts as one concept) -- the bottom of the recursion.\n",
    "                   Treat as a degenerate ", tolower(level_label), ".\n\n",
    "Your rationale MUST address the most-distant code pair shown in the ",
    "context: does the principle you articulated cover BOTH endpoints? ",
    "If not, that is the conceptual fault line and the answer is split_required.\n\n",
    calibration_text,
    "\n## ANTI-BIAS GUIDANCE\n",
    "- Topical overlap is not a unifying principle. Codes about \"sleep\" and ",
    "codes about \"binge eating\" can co-occur without sharing a concept.\n",
    "- It is fine -- expected, even -- for ",
    if (level_label == "SUBTHEME") "many" else "most",
    " clusters at this depth to ",
    "split. Coherent themes are precious; do not force them.\n",
    "- Single-code or near-single-code outliers are acceptable. A 1-code ",
    "theme reflects a unique participant voice, not a failure.\n",
    "- Embedding distance is a hint, not a verdict. The articulation test ",
    "is the binding criterion."
  )

  prompt <- paste0(
    parent_block,
    "## CLUSTER (", length(cluster_leaves), " codes)\n\n",
    cluster_summary,
    "\n## QUESTION\n",
    "Can ALL ", length(cluster_leaves), " codes above share a single ",
    "articulable conceptual organizing principle? ",
    "Articulate first, then decide."
  )

  result <- tryCatch({
    # temperature = 0: replay-equivalence (AC4 / AC10) requires deterministic
    # AI calls. The provider-level theming temperature default is 0.4
    # (default_config.yaml:143), which would make Phase 52 non-deterministic
    # across reruns of the same corpus. Pass 0 explicitly so themes.json is
    # bit-identical given a fixed (provider, response_cache, distance matrix).
    ai_result <- ai_complete(provider, prompt, system_prompt,
                              task = "theming",
                              temperature = 0,
                              response_schema = .theme_decision_schema())
    if (!is.null(audit_log)) {
      log_ai_request(audit_log, "theming", ai_result, response_cache,
                      level = level_label, node_idx = node_idx,
                      n_codes = length(cluster_leaves), call_idx = call_idx)
    }
    parse_json_safely(ai_result$content)
  }, error = function(e) {
    log_warn("Theme decision call {call_idx} failed: {e$message}; defaulting to split_required")
    walk_state$n_failed_calls <- walk_state$n_failed_calls + 1L
    NULL
  })

  # Circuit breaker (Phase 52 audit MEDIUM-9): repeated AI failures cascade
  # into a pathologically over-split tree (each internal node defaults to
  # split_required -> recursion to leaves -> N atomic-outlier themes for N
  # codes). The result LOOKS like a legitimate analysis but is actually
  # produced by network/API failures. Abort hard once we cross 25% failure
  # rate (and at least 4 failed calls so a tiny cluster doesn't trip it).
  if (walk_state$n_failed_calls >= 4L &&
      walk_state$n_failed_calls > floor(walk_state$n_calls * 0.25)) {
    stop(sprintf(
      "Theme generation aborted: %d of %d AI calls failed (>25%% failure rate). ",
      walk_state$n_failed_calls, walk_state$n_calls
    ),
    "Repeated failures would silently produce a degenerate tree where every code ",
    "becomes its own theme. Investigate the provider connectivity / API errors ",
    "and resume from checkpoint after the underlying issue is fixed.",
    call. = FALSE)
  }

  decision_record <- if (is.null(result) || is.null(result$decision)) {
    list(
      decision = "split_required",
      central_organizing_concept = "",
      proposed_name = NULL, proposed_description = NULL,
      rationale = "AI call failed; defaulting to split for safety."
    )
  } else {
    # Articulation enforcement (Phase 52 audit CRITICAL-1): the schema
    # requires a non-null central_organizing_concept string but cannot
    # enforce a minimum length under OpenAI strict mode. A vacuous
    # articulation ("strategies", "experiences", "") would technically
    # satisfy the schema and let the model push toward coherent_theme
    # without doing the conceptual work. We post-validate length here
    # and force a split when the articulation is too short to be a real
    # central organizing principle. 30 characters is the minimum for a
    # meaningful "principle that unifies ALL these codes" sentence -- a
    # bare noun phrase is not enough.
    .ARTICULATION_MIN_CHARS <- 30L
    raw_articulation <- trimws(as.character(result$central_organizing_concept %||% ""))
    if (nchar(raw_articulation) < .ARTICULATION_MIN_CHARS &&
        result$decision == "coherent_theme") {
      log_warn(paste0(
        "Theme decision call ", call_idx, ": articulation too short ",
        "(", nchar(raw_articulation), " chars; min ", .ARTICULATION_MIN_CHARS,
        ") to support coherent_theme verdict; forcing split_required. ",
        "Articulation was: '", substr(raw_articulation, 1, 80), "'"
      ))
      list(
        decision                   = "split_required",
        central_organizing_concept = raw_articulation,
        proposed_name              = NULL, proposed_description = NULL,
        rationale                  = paste0(
          "Phase 52 articulation enforcement: model emitted ",
          "central_organizing_concept of length ", nchar(raw_articulation),
          " (< ", .ARTICULATION_MIN_CHARS, " chars), which cannot meaningfully ",
          "unify the cluster. Forced split. Original rationale: ",
          result$rationale %||% ""
        )
      )
    } else {
      list(
        decision                   = result$decision,
        central_organizing_concept = raw_articulation,
        proposed_name              = if (isTRUE(nchar(result$proposed_name %||% "") > 0)) result$proposed_name else NULL,
        proposed_description       = if (isTRUE(nchar(result$proposed_description %||% "") > 0)) result$proposed_description else NULL,
        rationale                  = result$rationale %||% ""
      )
    }
  }

  # Record into walk_state for replay + audit
  walk_state$decisions[[length(walk_state$decisions) + 1L]] <- list(
    call_idx     = call_idx,
    level        = level_label,
    parent       = parent_label %||% NA_character_,
    node_idx     = node_idx,
    n_codes      = length(cluster_leaves),
    code_keys    = vapply(cluster_leaves, function(i) codes[[i]]$key, character(1)),
    decision     = decision_record$decision,
    articulation = decision_record$central_organizing_concept,
    name         = decision_record$proposed_name %||% NA_character_,
    rationale    = decision_record$rationale
  )

  # Phase 53 / C3: rewrite code_to_cluster.json after every AI decision so a
  # researcher can `cat outputs/<run>/live/code_to_cluster.json` and see the
  # in-progress decision trace. Themes-so-far is captured during recursion;
  # the final theme list is snapshotted again after the walks complete.
  live_snapshot_clusters(live_tracker, walk_status = "in_progress",
                          walk_state = walk_state,
                          themes_so_far = walk_state$themes_so_far %||% list())

  if (!is.null(audit_log)) {
    # NB: pass the verdict as `verdict = ...` (not `decision = ...`) so R's
    # partial-arg matching doesn't bind it to `decision_type` (the 3rd
    # positional parameter of log_ai_decision). R CMD check --as-cran flags
    # partial matches as portability NOTEs.
    log_ai_decision(audit_log, "theming", "cluster_decision",
                    level = level_label, parent = parent_label %||% "",
                    n_codes = length(cluster_leaves),
                    verdict = decision_record$decision,
                    articulation = substr(decision_record$central_organizing_concept, 1, 300),
                    proposed_name = decision_record$proposed_name %||% "",
                    call_idx = call_idx)
  }

  decision_record
}

# ==============================================================================
# Cluster summarization for AI prompts
# ==============================================================================

#' Summarize a cluster for the AI prompt
#'
#' Layout:
#'   - Codes list: full per-code (name, freq, description) when N <= 50;
#'     top-N by frequency + count of remaining + the 3 most-distant pairs
#'     when N > 50.
#'   - Quantitative context: mean intra-cluster distance, max-distant pair
#'     (always shown -- bias mitigation), top co-occurring pairs.
#'
#' @keywords internal
.summarize_cluster_for_prompt <- function(cluster_leaves, codes,
                                            distance_matrix, co_occurrence) {
  n <- length(cluster_leaves)

  # Code listing
  if (n <= .MAX_PROMPT_CODES) {
    code_lines <- vapply(cluster_leaves, function(i) {
      cd <- codes[[i]]
      desc <- cd$description %||% ""
      desc_str <- if (nchar(desc) > 0) {
        paste0("\n     ", substr(desc, 1, 200))
      } else ""
      sprintf('  - "%s" (key=%s, freq=%d)%s',
              cd$name, cd$key, cd$frequency, desc_str)
    }, character(1))
    code_block <- paste(code_lines, collapse = "\n")
  } else {
    freqs <- vapply(cluster_leaves, function(i) codes[[i]]$frequency, integer(1))
    ord   <- order(freqs, decreasing = TRUE)
    top   <- cluster_leaves[ord[seq_len(.MAX_PROMPT_CODES)]]
    code_lines <- vapply(top, function(i) {
      cd <- codes[[i]]
      sprintf('  - "%s" (freq=%d)', cd$name, cd$frequency)
    }, character(1))
    code_block <- paste(code_lines, collapse = "\n")
    code_block <- paste0(
      code_block, "\n  ... and ", n - .MAX_PROMPT_CODES,
      " other codes (omitted for context-window economy; the most-distant pair below is always shown)."
    )
  }

  # Quantitative context: distances + co-occurrence
  dist_block <- .cluster_distance_summary(cluster_leaves, codes, distance_matrix)
  co_block   <- .cluster_cooccurrence_summary(cluster_leaves, codes, co_occurrence)

  paste0(
    "### Codes\n", code_block, "\n\n",
    "### Quantitative context (computed from embeddings + entry-level co-occurrence)\n",
    dist_block,
    if (nchar(co_block) > 0) paste0("\n", co_block) else ""
  )
}

#' Distance summary for a cluster (always shows the most-distant pair)
#' @keywords internal
.cluster_distance_summary <- function(cluster_leaves, codes, distance_matrix) {
  n <- length(cluster_leaves)
  if (n < 2L) return("- (single-code cluster: no within-cluster distance)\n")

  # Submatrix of pairwise distances within the cluster
  sub <- distance_matrix[cluster_leaves, cluster_leaves]
  off_diag <- sub[upper.tri(sub)]
  mean_d <- if (length(off_diag) > 0L) mean(off_diag) else NA_real_
  max_d  <- if (length(off_diag) > 0L) max(off_diag) else NA_real_

  # Find the actual code pair at the max distance. Use which.max() rather
  # than `sub == max_d` (Phase 52 audit MEDIUM-1) to avoid floating-point
  # equality fragility -- pairs at numerically-identical-but-not-bitwise-
  # equal max distances would otherwise miss. Map the linear index back
  # to (row, col) coordinates within the strict upper triangle.
  most_distant_block <- ""
  if (length(off_diag) > 0L) {
    flat_idx  <- which.max(off_diag)
    upper_pos <- which(upper.tri(sub), arr.ind = TRUE)
    if (nrow(upper_pos) >= flat_idx) {
      i <- cluster_leaves[upper_pos[flat_idx, 1]]
      j <- cluster_leaves[upper_pos[flat_idx, 2]]
      ci <- codes[[i]]; cj <- codes[[j]]
      most_distant_block <- paste0(
        "- Most distant code pair (cosine distance ", round(max_d, 3), "):\n",
        "    A: \"", ci$name, "\"",
        if (nchar(ci$description %||% "") > 0)
          paste0("\n         (", substr(ci$description, 1, 150), ")")
        else "",
        "\n",
        "    B: \"", cj$name, "\"",
        if (nchar(cj$description %||% "") > 0)
          paste0("\n         (", substr(cj$description, 1, 150), ")")
        else "",
        "\n  YOUR ARTICULATION + RATIONALE MUST ADDRESS THIS PAIR EXPLICITLY.\n"
      )
    }
  }

  paste0(
    "- Mean intra-cluster distance: ", round(mean_d, 3), "\n",
    "- Range: [", round(min(off_diag), 3), ", ", round(max_d, 3), "]\n",
    most_distant_block
  )
}

#' Co-occurrence summary for a cluster
#' @keywords internal
.cluster_cooccurrence_summary <- function(cluster_leaves, codes, co_occurrence) {
  if (is.null(co_occurrence) || length(co_occurrence) == 0L) return("")

  cluster_keys <- vapply(cluster_leaves, function(i) codes[[i]]$key, character(1))
  pairs <- list()
  for (i in seq_along(cluster_keys)) {
    if (i == length(cluster_keys)) break
    for (j in (i + 1L):length(cluster_keys)) {
      key <- paste(sort(c(cluster_keys[i], cluster_keys[j])), collapse = "|")
      cnt <- co_occurrence[[key]] %||% 0L
      if (cnt > 0L) {
        pairs[[length(pairs) + 1L]] <- list(
          a = codes[[cluster_leaves[i]]]$name,
          b = codes[[cluster_leaves[j]]]$name,
          n = cnt
        )
      }
    }
  }
  if (length(pairs) == 0L) return("- No within-cluster code co-occurrences recorded\n")

  # Sort by count, take top 5
  pair_counts <- vapply(pairs, function(p) p$n, integer(1))
  ord <- order(pair_counts, decreasing = TRUE)
  top <- pairs[ord[seq_len(min(5L, length(pairs)))]]
  lines <- vapply(top, function(p) {
    sprintf('  - "%s" + "%s": %d shared entries', p$a, p$b, p$n)
  }, character(1))
  paste0("- Top within-cluster co-occurring code pairs:\n",
         paste(lines, collapse = "\n"), "\n")
}

# ==============================================================================
# Co-occurrence matrix (kept from pre-Phase-52; reused by HAC summary)
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

# ==============================================================================
# Deterministic code-path cascading (kept from pre-Phase-52)
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
# Mode 3 framework dispatch (kept from pre-Phase-52)
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

  # Each construct -> one theme (when at least one entry was coded with it).
  # Build first-class Subtheme containing the construct's hydrated Code.
  for (c in framework_spec$constructs) {
    cb_entry <- coding_state$codebook[[c$id]]
    if (is.null(cb_entry) || (cb_entry$frequency %||% 0L) == 0L) next
    themes[[length(themes) + 1L]] <- list(
      id              = next_id,
      name            = c$name,
      description     = c$description,
      subthemes       = list(create_subtheme(
        name = NA_character_, description = "",
        codes = list(.code_from_codebook(c$id, coding_state))
      )),
      keywords        = c$example_indicators %||% character(0),
      framework_construct_id = c$id
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
      subthemes       = list(create_subtheme(
        name = NA_character_, description = "",
        codes = list(.code_from_codebook("anomaly", coding_state))
      )),
      keywords        = character(0),
      framework_construct_id = "anomaly"
    )
  }

  if (length(themes) == 0L) {
    log_warn(paste0("apply_framework_themes: no constructs received any ",
                     "coded entries -- generating empty theme set"))
  }

  ts <- create_theme_set(themes)
  ts <- rebuild_code_to_theme_map(ts, coding_state)
  ts
}

# ==============================================================================
# Theme enrichment (kept from pre-Phase-52)
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
      # Phase 51: read from the canonical hierarchy via theme_codes() rather
      # than the denormalised codes_included field. Avoids any staleness
      # risk if a future caller mutates subthemes between create_theme_set()
      # and enrich_themes() without recomputing the denorm.
      theme_set$themes[[i]]$keywords <- theme_codes(theme_set$themes[[i]])
    }
  }

  theme_set
}
