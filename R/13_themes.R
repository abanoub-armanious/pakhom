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
# AI re-reading of raw text -- given a fixed coding_state it reproduces
# exactly (pure R; the upstream coding that produced the codes is not).
#
# REWRITE-DIRECTION COMMITMENTS HONORED IN THIS FILE:
#   - C1 (AI decides when to stop): no hardcoded n_themes, max_themes,
#     min_codes_per_theme, similarity gates. Saturation arbiter (Phase 56)
#     and the AI tree walk (Phase 52) make all structural decisions.
#   - C2 (codes preserved through clustering): the Code S3 (see
#     R/12_theme_data.R) is the atomic leaf; themes/subthemes carry
#     code_keys + code_indices (the original codebook keys), never
#     mutated names/descriptions/assignments.
#   - C5 (no catch-all buckets): the AI is never offered an "Other"
#     verdict; the closed three-valued enum
#     (coherent_theme | split_required | atomic_outlier) is the only
#     option set.
#   - C7 (mode-aware): Mode 3 framework-applied path pre-populates the
#     codebook with constructs (see R/09_coding.R) so this file's HAC
#     walk operates on a deductive codebook; Mode 1 doesn't use this
#     file at all (run_mode1 invokes the provocateur loop).
#
# PHASE 60 STATUS (2026-05-25): The new v2 algorithm in
# R/theme_algorithm_v2.R is now the production default. It implements:
#   - Multi-pass clustering with AI-declared convergence (C-tenet 3).
#   - Label-after-clustering with the whole tree visible (C-tenet 5).
#   - No articulation gate; no single-leaf auto-theme shortcut.
# The code in THIS file remains as the v1 algorithm for back-compat with
# calibrated test fixtures (test-themes.R Phase 52 + C-1 test groups).
# Dispatch lives in generate_themes_iterative() below: callers passing
# config$algorithm = "v2" (the default) get the new algorithm;
# config$algorithm = "v1" gets the legacy code path in this file.
# The v1 path is scheduled for deletion after Phase 60.8 empirical
# re-validation confirms v2 is stable on real corpora.
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

#' Generate themes by grouping codes into AI-judged clusters
#'
#' Dispatches to the configured theme algorithm. The default
#' (\code{algorithm = "v2"}, Phase 60) is an embedding-free, multi-pass AI
#' clustering: the model sees all codes at once, proposes a partition into
#' clusters, and on each further pass either groups clusters again or
#' declares the partition converged. There are no hardcoded pass counts or
#' size thresholds, and clustering depth is the AI's dynamic call (C1).
#' Codes are grouped, never combined into new codes (C2); theme and subtheme
#' names are assigned in a dedicated labeling pass after convergence. The
#' legacy \code{algorithm = "v1"} (Phase 52) computes code-name embeddings,
#' runs hierarchical agglomerative clustering (ward.D2), and walks the
#' resulting dendrogram with an AI judge at each node; it is retained for
#' back-compatible test fixtures and used only when explicitly pinned.
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
#' @param methodology_override Optional character (Phase 56). When non-NULL,
#'   replaces the provider's default methodology rules in every internal
#'   \code{ai_complete} call for this walk. Used by the Phase 54
#'   emergent-themes pass to inject the Mode 3 inductive variant; NULL
#'   for normal Mode 2 + Mode 3 deductive callers.
#' @return \code{ThemeSet} S3 object. Under v1 a \code{merge_history$tree_walk}
#'   field carries the HAC tree + per-node decisions; under v2 the multi-pass
#'   partition history is recorded for replay/audit instead.
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

  # ------------------------------------------------------------------
  # Phase 60 dispatch: algorithm = "v2" (default) routes to the multi-
  # pass clustering + label-after-clustering implementation in
  # R/theme_algorithm_v2.R. algorithm = "v1" preserves the legacy
  # Phase 52 HAC + tree-walk for back-compat with calibrated test
  # fixtures. The legacy code is targeted for deletion once Phase 60.8
  # empirical re-validation confirms v2 is stable.
  # ------------------------------------------------------------------
  algorithm <- as.character(config$algorithm %||% "v2")
  if (identical(algorithm, "v2")) {
    return(generate_themes_phase60(
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
  if (!identical(algorithm, "v1")) {
    log_warn("Unknown themes algorithm '{algorithm}'; falling back to v1 (legacy)")
  }

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
    provider             = provider,
    research_focus       = research_focus_str,
    concept_str          = concept_str,
    calibration_text     = calibration_text,
    reflexivity_block    = config$reflexivity_block %||% "",
    audit_log            = audit_log,
    response_cache       = response_cache,
    live_tracker         = live_tracker,
    walk_state           = walk_state,
    # Phase 56: per-call methodology rules override (Phase 54 deferral
    # iii). NULL = use provider default; non-NULL string = swap in
    # this text instead. The Phase 54 emergent-themes pass passes the
    # inductive variant of the Mode 3 rule so .evaluate_cluster's
    # AI calls don't see the contradictory "do not generate new
    # constructs" rule from the deductive default.
    methodology_override = methodology_override
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

  # 6. AI tree walk for SUBTHEMES within each theme (Phase 58 Tier 1
  # C-12: recursive depth-N decomposition triggered when a subtheme
  # exceeds the size cap). Configurable via analysis.themes.* knobs;
  # defaults: max_subtheme_depth = 3L, max_codes_per_subtheme = 25L.
  max_subtheme_depth <- as.integer(
    config$analysis$themes$max_subtheme_depth %||% 3L
  )
  max_codes_per_subtheme <- as.integer(
    config$analysis$themes$max_codes_per_subtheme %||% 25L
  )
  themes_raw <- lapply(themes_raw, function(t) {
    if (length(t$code_indices) <= 1L) {
      # Single-code theme -- no subtheme structure
      t$subtheme_groups <- list(list(
        name = NA_character_, description = "",
        code_indices = t$code_indices,
        children = list()
      ))
      return(t)
    }

    t$subtheme_groups <- .walk_for_subthemes(
      theme_name             = t$name,
      theme_node_idx         = t$node_idx,
      hac                    = hac,
      codes                  = codes,
      distance_matrix        = dist_matrix,
      co_occurrence          = co_occurrence,
      walk_ctx               = walk_ctx,
      current_depth          = 1L,
      max_subtheme_depth     = max_subtheme_depth,
      max_codes_per_subtheme = max_codes_per_subtheme
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

    # Phase 58 Tier 1 C-12: nested sub-subthemes from the recursive
    # walker arrive in g$children. Recursively build the Subtheme tree
    # via a helper so each depth level resolves its own Code objects
    # from the codebook.
    build_subtheme <- function(g) {
      create_subtheme(
        name        = g$name %||% NA_character_,
        description = g$description %||% "",
        codes       = lapply(g$code_indices, function(ci) {
          .code_from_codebook(codes[[ci]]$key, coding_state)
        }),
        subthemes   = lapply(g$children %||% list(), build_subtheme)
      )
    }
    subthemes <- lapply(t$subtheme_groups, build_subtheme)

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
      #
      # IMPORTANT: pmax's first argument's attributes (including dim) are
      # transferred to the result; second argument's attributes are not.
      # Calling pmax(0, 1 - sim) silently STRIPS sim's matrix dim and
      # returns a numeric VECTOR (because 0 has length 1 and the result
      # has length n*n). Phase 57 smoke caught this on a 283-code corpus
      # where the downstream rownames(d) <- ... call then errored with
      # "length of 'dimnames' [1] not equal to array extent". Phase 52
      # tests didn't hit it because the synthetic codebooks are <= 6 codes
      # and Jaccard fallback (which doesn't call pmax) was exercised.
      # Argument order corrected below: pmax(1 - sim, 0) preserves dim.
      d <- pmax(1 - sim, 0)
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
    # Phase 58 Tier 8 M-21/AF-29: fall back to a derived description
    # when the AI omits one. Pre-Tier-8 the description silently stayed
    # empty -- Phase 57 had 5 themes with empty descriptions in the
    # final report. The fallback PREFERS the AI's articulation (the
    # Tier-0-C1-gated central_organizing_concept field, already quality-
    # checked for non-vacuous output), then falls back to a top-3-code
    # summary if both proposed_description AND articulation are empty.
    # Tier 8 audit followup CRITICAL-1: pre-followup this read
    # `decision$articulation` -- there is no such field on the decision
    # record. The Tier 0 C-1 work was silently bypassed in 100% of
    # cases. Correct field name is `central_organizing_concept`
    # (see R/13_themes.R:1124/1136 where the record is assembled).
    derived_description <- if (nzchar(decision$proposed_description %||% "")) {
      decision$proposed_description
    } else {
      .derive_theme_description(leaves, codes,
                                  decision$central_organizing_concept)
    }
    return(list(.make_theme_record(
      leaves, hac_node_idx, codes,
      name        = decision$proposed_name %||% .fallback_theme_name(leaves, codes),
      description = derived_description,
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

#' Walk a theme's subtree to identify subthemes (recursive, depth-N)
#'
#' For each immediate child of the theme node, the AI judges whether it
#' constitutes a coherent subtheme of the theme. If yes, the child's
#' codes form a Subtheme. If no, the child's codes are flattened directly
#' into the theme (no Subtheme; will be wrapped in a virtual NA-named
#' Subtheme by create_subtheme()/create_theme_set()).
#'
#' Phase 58 Tier 1 C-12 + AF-8: when a coherent subtheme has more than
#' \code{max_codes_per_subtheme} codes AND we have depth budget left
#' (\code{current_depth < max_subtheme_depth}), the function recurses
#' into that subtheme's subtree to identify sub-subthemes. This is the
#' multi-level decomposition the Phase 57 audit found missing -- the
#' 237-code mega-theme split as just 2 sub-buckets (32 + 205) is the
#' canonical failure mode.
#'
#' Phase 58 Tier 1 AF-4: when the HAC binary cut at this internal node
#' is imbalanced (one branch has &le;1 code) AND the cluster has &gt;3
#' codes total, the function refuses to introduce subtheme structure
#' at all -- the codes flow back into the parent under a single virtual
#' subtheme. 1-code subthemes paired with many-code siblings were the
#' "55 imbalanced binary-split themes" the audit flagged.
#'
#' Returns a list of subtheme-group records. Each record has fields:
#'   name         : character or NA
#'   description  : character
#'   code_indices : integer vector
#'   children     : list of (recursive) subtheme-group records, or list()
#'
#' @param theme_name Parent theme name (passed to .evaluate_cluster as
#'   parent_label so the AI prompt knows the enclosing context).
#' @param theme_node_idx HAC merge-matrix row index for the theme.
#' @param hac Hierarchical clustering object (stats::hclust output).
#' @param codes List of Code records keyed by leaf index.
#' @param distance_matrix Pairwise distance matrix between codes.
#' @param co_occurrence Optional co-occurrence matrix.
#' @param walk_ctx List bundling walk_state + provider + prompts.
#' @param current_depth Depth of this recursive call (1 = direct
#'   subthemes of the theme; 2 = sub-subthemes; ...).
#' @param max_subtheme_depth Maximum recursion depth. Once
#'   \code{current_depth} reaches this value, large subthemes stop
#'   being re-walked. Default 3.
#' @param max_codes_per_subtheme Size threshold that triggers recursion.
#'   A coherent subtheme with more leaves than this gets re-walked one
#'   level deeper. Default 25.
#'
#' @keywords internal
.walk_for_subthemes <- function(theme_name, theme_node_idx, hac, codes,
                                  distance_matrix, co_occurrence, walk_ctx,
                                  current_depth = 1L,
                                  max_subtheme_depth = 3L,
                                  max_codes_per_subtheme = 25L) {
  # Single-leaf -- no decomposition possible
  if (theme_node_idx < 0L) {
    return(list(list(
      name = NA_character_, description = "",
      code_indices = -theme_node_idx,
      children = list()
    )))
  }

  pair <- hac$merge[theme_node_idx, ]
  pair_leaves <- lapply(pair, function(ch) .leaves_under_node(hac, ch))
  pair_sizes  <- vapply(pair_leaves, length, integer(1))
  total_leaves <- sum(pair_sizes)

  # Phase 58 Tier 1 AF-4: refuse imbalanced HAC binary splits at the
  # theme / subtheme level. When one branch is a HAC singleton (artifact
  # of ward.D2 cutting off an outlier code) AND the parent has more than
  # 3 codes total, collapsing the whole cluster into one virtual
  # subtheme produces a more honest representation than "1-code subtheme
  # + many-code subtheme".
  if (total_leaves > 3L && any(pair_sizes <= 1L)) {
    all_leaves <- unlist(pair_leaves, use.names = FALSE)
    return(list(list(
      name = NA_character_, description = "",
      code_indices = all_leaves,
      children = list()
    )))
  }

  child_groups <- list()
  for (k in seq_along(pair)) {
    child <- pair[k]
    child_leaves <- pair_leaves[[k]]

    # Single-leaf child of a small (&le;3 codes) cluster: keep virtual
    # (legitimate edge case where the HAC singleton IS meaningful).
    if (length(child_leaves) == 1L) {
      child_groups[[length(child_groups) + 1L]] <- list(
        name = NA_character_, description = "",
        code_indices = child_leaves,
        children = list()
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
      child_name <- decision$proposed_name %||% paste0(theme_name, " (subgroup)")

      # Phase 58 Tier 1 C-12 + AF-8: recurse into a large coherent
      # subtheme to identify sub-subthemes. Stops when (a) depth budget
      # is exhausted, (b) subtheme is at-or-under the size cap, or (c)
      # the child is itself a HAC leaf (no further structure to walk).
      nested_children <- list()
      if (length(child_leaves) > max_codes_per_subtheme &&
          current_depth < max_subtheme_depth &&
          child > 0L) {
        nested_children <- .walk_for_subthemes(
          theme_name             = child_name,
          theme_node_idx         = as.integer(child),
          hac                    = hac,
          codes                  = codes,
          distance_matrix        = distance_matrix,
          co_occurrence          = co_occurrence,
          walk_ctx               = walk_ctx,
          current_depth          = current_depth + 1L,
          max_subtheme_depth     = max_subtheme_depth,
          max_codes_per_subtheme = max_codes_per_subtheme
        )
      }

      child_groups[[length(child_groups) + 1L]] <- list(
        name         = child_name,
        description  = decision$proposed_description %||% "",
        code_indices = child_leaves,
        children     = nested_children
      )
    } else {
      # split_required or atomic_outlier at this depth: codes flow
      # directly into the parent under a virtual NA-named subtheme.
      child_groups[[length(child_groups) + 1L]] <- list(
        name = NA_character_, description = "",
        code_indices = child_leaves,
        children = list()
      )
    }
  }

  # Coalesce adjacent virtual (NA-named) groups so codes that flow
  # directly into the theme aren't split across multiple virtual subthemes.
  .coalesce_virtual_subtheme_groups(child_groups)
}

#' Combine adjacent NA-named subtheme groups into one virtual group
#'
#' Virtual subthemes carry no children (they are flat by definition), so
#' the merge concatenates code_indices and preserves the empty children
#' list. Named groups pass through unchanged including their nested
#' children produced by the recursive walker.
#'
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
        # Defensive: virtual groups must carry an empty children list
        # (any nested decomposition lives under NAMED parent subthemes).
        if (is.null(cur_virtual$children)) cur_virtual$children <- list()
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

#' Derive a fallback theme description when the AI omits one
#'
#' Phase 58 Tier 8 M-21/AF-29: pre-Tier-8 a coherent_theme verdict
#' with \code{proposed_description = ""} silently produced a theme
#' with an empty description (5 themes on the Phase 57 run). Downstream
#' renderers then displayed blank theme cards. The fallback derives
#' a short summary from (a) the AI's articulation (when non-empty)
#' and (b) the theme's top-3 codes by frequency. Worst-case output
#' is "Theme grouping: <code1>, <code2>, <code3>" -- not poetry, but
#' provably non-empty.
#' @keywords internal
.derive_theme_description <- function(leaf_indices, codes,
                                        articulation = NULL) {
  # Prefer the AI's articulation when available (it was just gated by
  # the Tier 0 C-1 articulation-quality check, so non-empty = non-
  # vacuous by construction).
  if (!is.null(articulation) && nzchar(articulation %||% "")) {
    return(as.character(articulation))
  }
  # Fall back to top-3 codes by frequency
  freqs <- vapply(leaf_indices, function(i) codes[[i]]$frequency, integer(1))
  ord <- order(freqs, decreasing = TRUE)
  top_names <- vapply(
    utils::head(leaf_indices[ord], 3L),
    function(i) codes[[i]]$name,
    character(1)
  )
  paste0("Theme grouping: ", paste(top_names, collapse = "; "), ".")
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
# ============================================================================
# Phase 58 Tier 0 C-1: Articulation gate quality checks
#
# The Phase 52 articulation gate enforced a flat 30-character minimum on the
# AI's central_organizing_concept field. Phase 57 found that this floor was
# permissive enough to pass tautological 85-char articulations on
# 237-code mega-themes (the "Emotional and Physical Impact of Binge Eating"
# kitchen-sink that absorbed 46% of the corpus). Three additional gates fix
# this without changing the upstream HAC tree structure:
#
#   1. Length floor scales by log10(n_codes): n=1 -> 30; n=10 -> 60;
#      n=100 -> 90; n=237 -> 101; n=1000 -> 120. The bigger the cluster,
#      the more the AI must actually say to claim coherence.
#
#   2. Bucket-label openers ("comprehensive", "diverse", "the various",
#      "a range of", "this theme captures the various") signal a
#      list-of-things rather than a unifying principle. Reject.
#
#   3. Tautology check: if the articulation reuses >70% of the proposed
#      theme name's content words (post-stop-word + theme-boilerplate
#      removal), it's a restatement of the name, not an organizing
#      principle. Reject.
#
# All three checks are applied ONLY when result$decision == "coherent_theme".
# A failure forces decision -> split_required (same fail-safe behavior as
# the Phase 52 length floor it replaces).
# ============================================================================

#' Phase 58 C-1: log-scaled minimum articulation length
#'
#' @param n_codes Number of leaf codes in the cluster being evaluated.
#' @return Integer minimum-character count required of a coherent_theme
#'   articulation for a cluster of this size.
#' @keywords internal
.articulation_min_chars <- function(n_codes) {
  # C-1 audit LOW-5: defend against NA / negative / non-integer input.
  # length(cluster_leaves) is always a positive integer in practice but
  # the helper is called by downstream code that could pass anything.
  n_int <- suppressWarnings(as.integer(n_codes)[1])
  if (length(n_int) == 0L || is.na(n_int)) n_int <- 1L
  n <- max(1L, n_int)
  max(30L, as.integer(30 + 30 * log10(n)))
}

#' Phase 58 C-1: bucket-label opener detection
#'
#' Returns TRUE when the articulation opens with a phrase that signals a
#' list-of-things rather than a unifying principle. Used as one of the
#' articulation-quality gates in .evaluate_cluster.
#'
#' @param articulation Character scalar; the raw articulation string.
#' @return Logical TRUE if articulation should be rejected as bucket-y.
#' @keywords internal
.is_bucket_label_opener <- function(articulation) {
  if (is.null(articulation) || length(articulation) == 0L) return(FALSE)
  s <- as.character(articulation)[1]
  if (is.na(s)) return(FALSE)
  s <- trimws(tolower(s))
  if (nchar(s) == 0L) return(FALSE)
  # C-1 audit MEDIUM-2: removed the "^(general|overarching) ... (of|in|for)
  # \\w" pattern. The greedy `.+ ` match was tripping substantive openings
  # like "Overarching pattern of self-medication in participants ..." that
  # are NOT bucket-y. The remaining four patterns target unambiguously
  # list-of-things openers.
  patterns <- c(
    "^this theme (captures|explores|covers|describes|examines) (the )?(various|diverse|many|wide range of|broad range of)\\b",
    "^(comprehensive|multifaceted|mixed|combined|combination of)\\b",
    "^(a variety of|a range of|a wide range of|a broad range of|the various|the diverse|the many)\\b",
    "^(strategies and outcomes|exploration and understanding)\\b"
  )
  any(vapply(patterns, function(p) grepl(p, s, perl = TRUE), logical(1)))
}

#' Phase 58 C-1: tautological-articulation detection
#'
#' Returns TRUE when the articulation reuses more than 70% of the content
#' words from the proposed theme name (after removing English stop words
#' and theme-rendering boilerplate like "theme", "captures", "various").
#' A tautological articulation restates the name without adding a unifying
#' principle.
#'
#' @param articulation Character scalar; the raw articulation string.
#' @param proposed_name Character scalar; the AI's proposed theme name.
#' @return Logical TRUE if articulation should be rejected as tautological.
#' @keywords internal
.is_tautological_articulation <- function(articulation, proposed_name) {
  if (is.null(proposed_name) || length(proposed_name) == 0L) return(FALSE)
  if (!nzchar(as.character(proposed_name)[1])) return(FALSE)
  if (is.null(articulation) || length(articulation) == 0L) return(FALSE)
  if (!nzchar(as.character(articulation)[1])) return(FALSE)

  stop_words <- c(
    "a", "an", "the", "of", "on", "in", "to", "for", "with",
    "and", "or", "but", "by", "from", "into", "as", "at",
    "is", "are", "was", "were", "be", "been", "being",
    "this", "that", "these", "those", "their", "its", "it",
    "theme", "themes", "captures", "captured", "explores", "explored",
    "discusses", "discussed", "describes", "described", "addresses",
    "addressed", "examines", "examined", "represents", "represented",
    "covers", "covered", "various", "diverse", "different", "multiple",
    "many", "range", "ranges"
  )
  tokenize <- function(s) {
    words <- strsplit(tolower(as.character(s)[1]), "[^a-z0-9]+", perl = TRUE)[[1]]
    words <- words[nchar(words) > 0L]
    setdiff(unique(words), stop_words)
  }
  name_tokens <- tokenize(proposed_name)
  art_tokens  <- tokenize(articulation)
  # C-1 audit MEDIUM-1: skip the tautology check when proposed_name has
  # fewer than 2 content words. With a single content word any
  # articulation that mentions it would yield 100% overlap and force a
  # split, even if the articulation is substantive. A theme named
  # "Routines" + articulation "Daily routines around medication and
  # sleep timing" should NOT be rejected as tautological.
  if (length(name_tokens) < 2L) return(FALSE)
  overlap <- length(intersect(name_tokens, art_tokens))
  (overlap / length(name_tokens)) > 0.7
}

#' Ask the AI to evaluate a cluster as a candidate theme or subtheme
#'
#' Builds the cluster-summary prompt (with bias-mitigation context: most-
#' distant pair, full per-code list when small, top-N + extremes when
#' large) and calls \code{ai_complete()} with the
#' \code{.theme_decision_schema()}. Post-validates the articulation via
#' Phase 58 Tier 0 C-1's quality gates (length / bucket-label opener /
#' tautology). Returns a structured decision record (decision, name,
#' description, rationale, articulation). Records the decision in
#' \code{walk_state} and the \code{audit_log}.
#'
#' @keywords internal
.evaluate_cluster <- function(cluster_leaves, node_idx, level_label,
                                parent_label = NULL,
                                codes, distance_matrix, co_occurrence,
                                walk_ctx) {
  # Phase 53 cleanup of Phase 52 audit MEDIUM-8 + LOW-10:
  # walk_state is now an environment, mutated in place without `<<-`.
  # walk_ctx packs the long parameter list from the pre-cleanup version.
  walk_state           <- walk_ctx$walk_state
  provider             <- walk_ctx$provider
  research_focus       <- walk_ctx$research_focus
  concept_str          <- walk_ctx$concept_str
  calibration_text     <- walk_ctx$calibration_text
  reflexivity_block    <- walk_ctx$reflexivity_block
  audit_log            <- walk_ctx$audit_log
  response_cache       <- walk_ctx$response_cache
  live_tracker         <- walk_ctx$live_tracker
  methodology_override <- walk_ctx$methodology_override

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
    # temperature = 0: pin the theming calls to temperature 0 to MINIMIZE
    # run-to-run variance (the provider-level theming default is 0.4, which
    # would add avoidable non-determinism). Temperature 0 is best-effort, not a
    # guarantee -- LLM inference can still vary across runs (especially on
    # providers without a seed parameter); true bit-identical replay is reached
    # only by replaying cached responses by prompt_hash (the planned
    # replay_run(), OS.5), for which temp-0 is the right prerequisite.
    ai_result <- ai_complete(provider, prompt, system_prompt,
                              task = "theming",
                              temperature = 0,
                              response_schema = .theme_decision_schema(),
                              methodology_override = methodology_override)
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
    # Articulation quality gates (Phase 52 length floor + Phase 58 C-1
    # additions). The schema cannot enforce these so we post-validate
    # before letting coherent_theme through:
    #   * Length floor scales by log10(n_codes) -- bigger clusters need
    #     more substance than a bare noun phrase.
    #   * Bucket-label openers ("comprehensive", "the various",
    #     "this theme captures the various ...") signal list-of-things,
    #     not a unifying principle.
    #   * Tautology check: >70% word overlap with proposed_name means
    #     the articulation restates the name without adding a principle.
    # Any failure forces decision -> split_required.
    raw_articulation <- trimws(as.character(result$central_organizing_concept %||% ""))
    proposed_name <- as.character(result$proposed_name %||% "")
    n_codes <- length(cluster_leaves)
    min_chars <- .articulation_min_chars(n_codes)

    failures <- character(0)
    if (identical(result$decision, "coherent_theme")) {
      if (nchar(raw_articulation) < min_chars) {
        failures <- c(failures, sprintf(
          "too short (%d < %d chars for %d-code cluster)",
          nchar(raw_articulation), min_chars, n_codes
        ))
      }
      if (.is_bucket_label_opener(raw_articulation)) {
        failures <- c(failures, "bucket-label opener (list-of-things, not unifying principle)")
      }
      if (.is_tautological_articulation(raw_articulation, proposed_name)) {
        failures <- c(failures, sprintf(
          "tautological (restates proposed_name '%s')",
          substr(proposed_name, 1, 60)
        ))
      }
    }

    if (length(failures) > 0L) {
      log_warn(paste0(
        "Theme decision call ", call_idx, ": articulation failed quality ",
        "checks [", paste(failures, collapse = "; "), "]; forcing ",
        "split_required. Articulation was: '",
        substr(raw_articulation, 1, 100), "'"
      ))
      list(
        decision                   = "split_required",
        central_organizing_concept = raw_articulation,
        proposed_name              = NULL,
        proposed_description       = NULL,
        rationale                  = paste0(
          "Phase 58 articulation enforcement: ",
          paste(failures, collapse = "; "),
          ". Original rationale: ", result$rationale %||% ""
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

  # Phase 54: Mode 3 emergent themes need per-segment routing. The
  # standard code_to_theme_map can map at most ONE theme to the
  # "anomaly" code key, but under anomaly_handling=extend|revise each
  # anomaly segment may be in a different emergent theme. The
  # apply_framework_themes stashes a (entry_id|start|end) -> theme_name
  # map on theme_set; we consult it below for entries that have
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

    # Phase 54 / Mode 3 emergent fan-out (CRITICAL-8 fix): if this
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
# Phase 54: Emergent themes from anomaly segments (Mode 3 extend/revise)
# ==============================================================================
# When a Mode 3 run produces anomaly segments (text the AI couldn't fit any
# framework construct during deductive coding), the anomaly_handling policy
# decides what happens next:
#   "bracket"     -> single Anomaly catch-all theme (pre-Phase-54 behavior)
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
#   (c) Run generate_themes_iterative() on the synthetic state. As of
#       Phase 60 the default algorithm is v2 (multi-pass clustering +
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

  log_info("Phase 54 / anomaly_handling=extend|revise: generating emergent themes from {length(segs)} anomaly segment(s)")
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
  # state. As of Phase 60 the default algorithm is v2 (multi-pass
  # clustering + label-after-clustering); the dispatch lives in
  # generate_themes_iterative() so anomaly emergent themes automatically
  # benefit from C-tenets 3+5 the same way Mode 2 themes do. We don't need
  # the resulting ThemeSet wrapper -- we need the inner theme records so
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
    "  verbatim words used (e.g., 'Spiritual coping during binges', not ",
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
    # simplifier punts), and a NAMED scalar list (Phase 54 audit HIGH-14:
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
    # Phase 58 Tier 0 C-4 audit HIGH-1: normalize AI-returned code names
    # here too. This admission path doesn't inject a numbered codebook
    # menu so the specific C-4 trigger is absent, but the normalizer is
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
#' resulting state can be passed to \code{generate_themes_iterative}
#' (Phase 52 HAC + AI-judged tree walk) as if it were a normal Mode 2
#' run scoped to just the anomaly residuals.
#'
#' @keywords internal
.build_synthetic_state_from_emergent_codes <- function(anomaly_segments,
                                                          segment_codes) {
  state <- create_coding_state()

  # Group segments by (consolidated) code_name. The AI was prompted to
  # reuse names; here we honor that by treating duplicates as one code.
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
#' Phase 54 audit CRITICAL-8: without this map the cascade can only
#' route "anomaly" once (to a single theme), which under extend/revise
#' policies means emergent themes render with entry_count = 0 -- a
#' silent data-loss bug that defeats the whole purpose of Phase 54.
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
# Mode 3 framework dispatch (Phase 54: dispatches on anomaly_handling)
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
#' \strong{Anomaly policy dispatch} (Phase 54):
#' \itemize{
#'   \item \code{"bracket"}: legacy pre-Phase-54 behavior. Appends a single
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
#' @param config Optional \code{ThematicConfig} (Phase 56). When supplied
#'   and the framework spec's anomaly_handling is \code{"extend"} or
#'   \code{"revise"}, the inductive emergent-themes pass receives a
#'   methodology rules override (the inductive-pass variant of the Mode 3
#'   rule, computed via \code{generate_methodology_rules(config,
#'   inductive_pass = TRUE)}) so the AI doesn't see the contradictory
#'   "Do NOT generate new framework constructs" rule from the deductive
#'   default. NULL (the default) falls through to the provider's default
#'   rules -- safe for legacy/test callers; the inductive pass will see
#'   the deductive rule alongside its inductive prompt (the Phase 54
#'   deferral iii contradiction the override resolves).
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

  # Phase 56 (Phase 54 deferral iii): under anomaly_handling = extend/revise
  # the inductive emergent-themes pass needs to see the inductive variant of
  # the Mode 3 methodology rule (which permits new-code generation on the
  # anomaly residuals). The default deductive Mode 3 rule says "do NOT
  # generate new framework constructs during coding" -- a direct
  # contradiction with the inductive prompt. Pre-compute the override once
  # here so .generate_emergent_themes_from_anomalies can thread it into
  # both the segment-coding call AND the downstream HAC tree-walk for
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

  # Anomaly policy dispatch (Phase 54). The original kitchen-sink
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
        # inductive coding + Phase 52 HAC + AI-judged tree walk.
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

        # Build segment-identity -> emergent-theme-name map (Phase 54
        # audit CRITICAL-8). Entry results in Mode 3 record only the
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

  # Phase 58 Tier 8 M-27/P54-(iv): emit a live cluster snapshot after
  # framework themes are assembled so a researcher cat'ing the
  # code_to_cluster.json mid-run sees the deductive Mode 3 theme
  # construction. Pre-Tier-8 only Mode 2 + Mode 3 emergent HAC walks
  # snapshotted; the deductive framework pass was invisible to live
  # tracking. The snapshot fires once at the end of the deductive
  # pass (multiple emergent walks already snapshot inside the
  # walk_for_themes machinery; this is the orthogonal deductive
  # surface). NULL tracker is a no-op (matches the rest of the file).
  # Tier 8 audit followup HIGH-2: the snapshot reader at
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

  # Phase 54 audit CRITICAL-8: cascade_theme_assignments routes entries
  # via code_to_theme_map keyed by code_key. In Mode 3 the only key
  # written into entry_results$codes_assigned for anomaly segments is
  # the literal "anomaly" -- the per-segment inductive codes are not
  # reflected in coding_state, so the standard cascade reaches at most
  # ONE theme for ALL anomaly entries. Under extend/revise we have a
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
        # Phase 62.3: word-boundary truncation (reuses the Phase 58 Tier 9 V-8
        # helper) so display quotes don't sever mid-word; visible " ..." marker.
        # Keep the `%||% ""` guard -- the helper's is.na() check errors on NULL.
        function(lbl) .truncate_quote_word_boundary(selected[[lbl]]$text %||% "",
                                                    max_chars = 200L),
        character(1)
      )
      qtexts <- qtexts[nchar(qtexts) > 0]
      if (length(qtexts) > 0L) {
        theme_set$themes[[i]]$supporting_quotes <- unname(qtexts)
        # Phase 58 Tier 7 M-25/AF-34: parallel structured records so a
        # downstream consumer can trace each quote text back to its
        # source entry. The bare-string supporting_quotes field is
        # preserved verbatim for back-compat with any consumer that
        # reads the legacy shape; new consumers should prefer
        # supporting_quote_records.
        records <- lapply(ordered_labels, function(lbl) {
          s <- selected[[lbl]]
          if (is.null(s) || is.null(s$text) || !nzchar(s$text)) return(NULL)
          list(
            # Phase 62.3: word-boundary truncation (same call as the bare-string
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
      # Phase 58 Tier 8 H-26/AF-31: pre-Tier-8 this assignment copied
      # ALL codes (a verbatim duplicate of codes_included) into the
      # keywords field. Phase 57 audit measured every theme's keywords
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
          # Tier 8 audit followup MEDIUM-1: use the canonical
          # theme_code_keys() helper rather than tolower(name)
          # round-trip. Phase 51's code key IS lowercase(name) for
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
