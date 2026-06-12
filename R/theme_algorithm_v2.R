# ==============================================================================
# Theme Algorithm v2: Multi-Pass Clustering + Label-After-Clustering
# ==============================================================================
# This file implements the v2 theme algorithm that replaced the earlier
# HAC + AI tree-walk. Activated via
# config$analysis$themes$algorithm == "v2" (the default).
#
# REWRITE-DIRECTION COMMITMENTS (C1-C8) ENCODED IN THIS FILE:
#
#   C1 -- AI decides when to stop. The clustering loop terminates ONLY when
#         the AI returns verdict='converged'. No hardcoded max-pass count
#         beyond a 20-pass runaway safety net (which is NOT a methodological
#         cap -- it stops infinite loops from broken provider responses).
#         No hardcoded min-cluster size, no hardcoded n_themes, no hardcoded
#         saturation threshold, no articulation-quality gates.
#
#   C2 -- Codes preserved through clustering. The Code S3 (R/12_theme_data.R)
#         is the atomic leaf. Clustering NEVER mutates code names,
#         descriptions, or assignments. A leaf in pass N is a SET of code
#         KEYS plus lineage metadata, nothing more.
#
#   C3 -- Live tracking artifacts during processing. Each clustering pass
#         writes a snapshot to outputs/<run>/live/clustering_pass_<N>.json
#         via live_record_clustering_pass(). After convergence, the labeled
#         theme set is snapshotted via live_snapshot_clusters().
#
#   C5 -- No catch-all / "Other" buckets. Singleton clusters (length-1
#         leaves that the AI judges don't belong with any others) are first-
#         class results, not bucket-shunting. The AI is never offered an
#         "Other" verdict.
#
#   C7 -- Mode-aware. Mode 1 doesn't reach this file (the provocateur in
#         R/mode1_orchestrator.R authors themes directly). Mode 2 is the
#         canonical target -- multi-pass clustering of the inductive
#         codebook. Mode 3 deductive uses apply_framework_themes() which
#         pre-populates themes from framework constructs and does NOT call
#         this file. Mode 3 inductive (anomaly_handling = "extend" |
#         "revise") calls generate_themes_multipass() on the synthetic
#         anomaly-codebook in .generate_emergent_themes_from_anomalies()
#         via R/13_themes.R.
#
# ALGORITHM IN ONE PARAGRAPH:
#   Pass 1 input is the raw codebook -- each code is a leaf. The AI sees
#   ALL leaves and proposes a partition that groups them into top-level
#   clusters. Pass 2 input is pass-1 clusters as new leaves; the AI may
#   merge them further or declare convergence. Pass N continues until the
#   AI declares convergence. The penultimate stable pass becomes the
#   subtheme layer; the final stable pass becomes the theme layer. Earlier
#   passes are collapsed into subthemes (with full lineage preserved in
#   Subtheme records). After convergence, a SINGLE labeling pass sees the
#   whole tree and names every theme + subtheme, with explicit cross-theme
#   distinctness enforcement.
#
# WHY THIS REPLACES THE EARLIER ALGORITHM:
#   The earlier single-pass HAC + tree-walk + articulation gate produced
#   87-92% single-code themes at scale
#   (Mode 2 Run 1: 60/69 themes had a single code; Run 6: 141/154). The
#   articulation gate flipped 79-87% of the AI's
#   coherent_theme verdicts to split_required, one-way -- the cascade
#   produced singleton leaves. Three independent audits confirmed the
#   algorithm diverged from C-tenets 3 (multi-pass) and 5 (label after).
# ==============================================================================


# ==============================================================================
# Main entry point
# ==============================================================================

#' Generate themes via multi-pass clustering + label-after-clustering
#'
#' The v2 theme algorithm. Multi-pass partitioning: at each pass the AI sees
#' the current leaves (codes initially, then prior-pass clusters) and either
#' proposes a partition into clusters OR declares convergence. The
#' penultimate stable structure becomes subthemes; the final stable
#' structure becomes themes. A dedicated post-convergence labeling pass
#' assigns researcher-facing names + descriptions to every theme and
#' subtheme with the full tree visible.
#'
#' Called by \code{generate_themes_iterative()} when
#' \code{config$analysis$themes$algorithm == "v2"} (the default).
#'
#' @param coding_state \code{ProgressiveCodingState}
#' @param provider \code{AIProvider}
#' @param config Theme config section (only \code{algorithm} +
#'   \code{quotes_per_theme} are consulted; the v2 path has NO threshold
#'   knobs per C1).
#' @param learning_context Optional \code{LearningContext}; if present, its
#'   \code{for_theming} text is added to the clustering prompts as
#'   reference context. Used by the manuscript-learning path.
#' @param research_focus Character; the study's research focus statement.
#' @param concepts Optional character vector of core research concepts.
#' @param audit_log Optional \code{AuditLog}.
#' @param response_cache Optional \code{ResponseCache}.
#' @param live_tracker Optional \code{LiveTracker} (per C3).
#' @param methodology_override Optional character; per-call methodology
#'   rules override. Used by the Mode 3 inductive emergent-themes pass.
#' @return \code{ThemeSet} S3 with merge_history attached.
#' @export
#' @keywords internal
generate_themes_multipass <- function(coding_state, provider, config = list(),
                                       learning_context = NULL,
                                       research_focus = "",
                                       concepts = NULL,
                                       audit_log = NULL,
                                       response_cache = NULL,
                                       live_tracker = NULL,
                                       methodology_override = NULL) {
  if (!inherits(coding_state, "ProgressiveCodingState")) {
    stop("coding_state must be a ProgressiveCodingState object", call. = FALSE)
  }
  validate_provider(provider, caller = "generate_themes_multipass")

  codes <- .extract_codes_from_state(coding_state)

  # Edge cases: 0 or 1 code
  if (length(codes) == 0L) {
    log_warn("No codes in coding state -- cannot generate themes (v2)")
    return(create_theme_set(list()))
  }
  if (length(codes) == 1L) {
    log_info("Only 1 code present -- producing single-theme ThemeSet (v2)")
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
      analysis_notes = "v2: only 1 code in coding state -- multi-pass degenerate case."
    )
    ts$merge_history <- list(
      algorithm           = "multi_pass_v2",
      n_codes             = 1L,
      n_substantive_passes = 0L,
      converged_at_pass   = 1L,
      pass_history        = list(),
      decisions           = list(),
      code_to_theme_map   = list(),
      code_to_subtheme_map = list()
    )
    ts <- rebuild_code_to_theme_map(ts, coding_state)
    return(ts)
  }

  log_info("Generating themes via multi-pass clustering: {length(codes)} codes")
  tic("Theme generation (v2)")

  # ----------------------------------------------------------------------------
  # Build pass-0 leaves: each code is its own leaf
  # ----------------------------------------------------------------------------
  current_leaves <- lapply(seq_along(codes), function(i) {
    cd <- codes[[i]]
    list(
      leaf_id            = sprintf("leaf_p0_%d", i),
      leaf_type          = "code",
      member_code_keys   = cd$key,
      n_codes            = 1L,
      pass_created       = 0L,
      cluster_rationale  = "",   # codes aren't a clustering result
      lineage            = list()
    )
  })

  pass_history <- list()
  walk_state <- new.env(parent = emptyenv())
  walk_state$decisions      <- list()
  walk_state$n_calls        <- 0L
  walk_state$n_failed_calls <- 0L

  research_focus_str <- as.character(research_focus %||% "")
  concept_str <- if (!is.null(concepts) && length(concepts) > 0L) {
    paste(concepts, collapse = ", ")
  } else {
    research_focus_str
  }

  # Reference text from the manuscript-learning loop, if available
  reference_text <- if (!is.null(learning_context) &&
                         nchar(learning_context$for_theming %||% "") > 0L) {
    paste0(
      "\n## REFERENCE: How previous human researchers organized codes\n",
      learning_context$for_theming, "\n"
    )
  } else ""

  # Reflexivity block from the methodology-rules layer
  reflexivity_block <- config$reflexivity_block %||% ""

  # Runaway safety net (NOT a methodological cap). If the AI somehow never
  # converges (broken provider responses, oscillating partitions, ...) the
  # loop stops at MAX_RUNAWAY_PASSES to avoid an infinite loop. 20 is far beyond
  # any methodologically plausible pass count for a corpus that wasn't
  # already saturated; if a real corpus needs >20 passes the design needs
  # revisiting, not the breaker.
  MAX_RUNAWAY_PASSES <- 20L

  # ----------------------------------------------------------------------------
  # The multi-pass clustering loop
  # ----------------------------------------------------------------------------
  pass_n <- 1L
  converged_at <- NA_integer_
  convergence_rationale <- ""

  repeat {
    log_info("[v2] Clustering pass {pass_n} on {length(current_leaves)} leaves")

    proposal <- ai_propose_clustering(
      leaves              = current_leaves,
      pass_index          = pass_n,
      prior_history       = pass_history,
      codes               = codes,
      provider            = provider,
      research_focus      = research_focus_str,
      concept_str         = concept_str,
      reference_text      = reference_text,
      reflexivity_block   = reflexivity_block,
      audit_log           = audit_log,
      response_cache      = response_cache,
      methodology_override = methodology_override,
      walk_state          = walk_state
    )

    # Per-pass live-tracking snapshot (C3)
    live_record_clustering_pass(live_tracker, pass_n, current_leaves, proposal,
                                 codes = codes)

    if (identical(proposal$verdict, "converged")) {
      # CRITICAL FAILURE GUARD: if the AI call
      # at pass 1 failed and was coerced to convergence by the normalizer,
      # the result would silently be one theme per code -- the v1 pathology
      # the entire v2 rewrite was meant to fix. Abort loudly instead.
      # Pass N>1 failure is recoverable (prior-pass clusters are
      # the natural fallback) so this guard fires only at the very first
      # call's failure mode.
      # Distinguish a normalizer-COERCED "convergence" (the AI call failed, or
      # the response was malformed) from a GENUINE pass-1 convergence. The
      # normalizer sets proposal$coerced = TRUE only on its failure path; the
      # AI's own rationale is never pattern-matched, so qualitative wording like
      # "absent support" or "coerced consent" cannot false-trigger an abort.
      ai_failure_coerced <- isTRUE(walk_state$n_failed_calls > 0L) ||
                              isTRUE(proposal$coerced)
      if (pass_n == 1L && ai_failure_coerced && length(current_leaves) > 1L) {
        log_error(paste0(
          "[v2] CRITICAL: pass 1 AI call failed and would coerce to ",
          length(current_leaves), " single-code themes. Aborting rather ",
          "than producing degenerate output. Check provider quota / ",
          "network / response_schema strict-mode compliance. ",
          "Rationale recorded: ", substr(proposal$overall_rationale %||% "", 1, 200)
        ))
        live_record_clustering_pass(live_tracker, pass_n, current_leaves, proposal,
                                     codes = codes)
        stop(sprintf(
          "v2 theme generation aborted at pass 1: AI call failed and coerced fallback would emit %d single-code themes. See log for details.",
          length(current_leaves)
        ), call. = FALSE)
      }

      log_info("[v2] AI declared convergence at pass {pass_n} ({length(current_leaves)} final leaves)")
      converged_at <- pass_n
      convergence_rationale <- proposal$overall_rationale %||% ""
      break
    }

    # Apply the proposed partition: produce a new set of cluster-leaves.
    new_leaves <- apply_partition(current_leaves, proposal$cluster_assignments,
                                    pass_n = pass_n)

    # Idempotence checks: detect "no useful further grouping" even when
    # the AI didn't articulate verdict='converged'.
    #
    # Check A (identity): every new cluster contains exactly one source
    # leaf -- the partition didn't merge anything.
    #
    # Check B (structural equivalence): the new buckets carry the same
    # code-key sets as the previous-pass buckets. The AI repeated its
    # prior partition.
    structurally_repeated_prior <- FALSE
    if (length(pass_history) > 0L) {
      prior_post <- pass_history[[length(pass_history)]]$post_leaves
      if (.partition_is_structurally_equivalent(prior_post, new_leaves)) {
        structurally_repeated_prior <- TRUE
      }
    }
    if (.partition_is_identity(current_leaves, new_leaves) ||
        structurally_repeated_prior) {
      reason_label <- if (structurally_repeated_prior) "structural-repeat" else "identity"
      log_info("[v2] Pass {pass_n}: proposed partition is {reason_label} (no new grouping). Forcing convergence.")
      converged_at <- pass_n
      convergence_rationale <- paste0(
        "Partition coercion at pass ", pass_n, " (", reason_label,
        "): AI proposed a partition that didn't introduce new groupings ",
        "vs. the prior state; treated as convergence. ",
        proposal$overall_rationale %||% ""
      )
      break
    }

    # Record this pass and advance
    pass_history[[pass_n]] <- list(
      pass_n             = pass_n,
      pre_leaves         = current_leaves,
      partition          = proposal$cluster_assignments,
      post_leaves        = new_leaves,
      overall_rationale  = proposal$overall_rationale %||% ""
    )
    current_leaves <- new_leaves
    pass_n <- pass_n + 1L

    if (pass_n > MAX_RUNAWAY_PASSES) {
      log_warn("[v2] Runaway safety net: exceeded {MAX_RUNAWAY_PASSES} passes; aborting clustering loop")
      converged_at <- pass_n - 1L
      convergence_rationale <- paste0(
        "Runaway safety net at pass ", pass_n - 1L,
        ": exceeded the 20-pass safety bound. The AI did not converge ",
        "naturally; downstream output should be inspected manually."
      )
      break
    }
  }

  # ----------------------------------------------------------------------------
  # Derive the theme + subtheme structural skeleton from pass_history
  # ----------------------------------------------------------------------------
  themes_skeleton <- derive_theme_subtheme_structure(
    pass_history = pass_history,
    final_leaves = current_leaves,
    codes        = codes
  )

  log_info("[v2] Derived skeleton: {length(themes_skeleton)} theme(s) from {length(pass_history)} substantive pass(es)")

  # ----------------------------------------------------------------------------
  # Dedicated labeling pass: AI sees the FULL tree and names every node
  # ----------------------------------------------------------------------------
  labeled_skeleton <- ai_label_theme_set(
    skeleton            = themes_skeleton,
    codes               = codes,
    provider            = provider,
    research_focus      = research_focus_str,
    concept_str         = concept_str,
    reflexivity_block   = reflexivity_block,
    audit_log           = audit_log,
    response_cache      = response_cache,
    methodology_override = methodology_override,
    walk_state          = walk_state
  )

  # ----------------------------------------------------------------------------
  # Build the ThemeSet S3 from the labeled skeleton
  # ----------------------------------------------------------------------------
  theme_list <- lapply(seq_along(labeled_skeleton), function(i) {
    th <- labeled_skeleton[[i]]
    subthemes <- if (length(th$subthemes) == 0L) {
      # Theme has no subtheme structure: wrap codes in a single virtual
      # subtheme (NA-named) so the rendering layer stays uniform.
      list(create_subtheme(
        name        = NA_character_,
        description = "",
        codes       = lapply(th$member_code_keys, function(k) {
          .code_from_codebook(k, coding_state)
        })
      ))
    } else {
      lapply(th$subthemes, function(s) {
        sub <- create_subtheme(
          name        = s$name %||% NA_character_,
          description = s$description %||% "",
          codes       = lapply(s$member_code_keys, function(k) {
            .code_from_codebook(k, coding_state)
          })
        )
        # Preserve lineage on the Subtheme S3 for transparency reports.
        # Non-canonical field; downstream consumers may safely ignore it.
        sub$lineage <- s$lineage %||% list()
        sub$decision_origin <- s$decision_origin %||% "multi_pass_subtheme"
        sub
      })
    }

    list(
      id                 = i,
      name               = th$name,
      description        = th$description %||% "",
      subthemes          = subthemes,
      prevalence         = "medium",
      sentiment_tendency = "neutral",
      decision_origin    = th$decision_origin %||% "multi_pass_converged"
    )
  })

  ts <- create_theme_set(
    themes       = theme_list,
    thematic_map = sprintf(
      paste0(
        "v2 multi-pass clustering: %d substantive pass(es), converged at ",
        "pass %d; %d themes from %d codes; %d AI calls."
      ),
      length(pass_history),
      converged_at %||% (length(pass_history) + 1L),
      length(theme_list),
      length(codes),
      walk_state$n_calls
    ),
    analysis_notes = paste0(
      "Multi-pass AI-judged clustering. Each pass partitions ",
      "the current leaves; convergence is the AI's call. No hardcoded ",
      "thresholds (per C1). Codes preserved as atomic leaves (per C2). ",
      "Labeling happened in a dedicated post-convergence pass (per C5)."
    )
  )

  # ----------------------------------------------------------------------------
  # Attach merge_history (keeps the field name for cascade_theme_assignments
  # back-compat; content reflects the v2 algorithm)
  # ----------------------------------------------------------------------------
  ts$merge_history <- list(
    algorithm             = "multi_pass_v2",
    n_codes               = length(codes),
    n_substantive_passes  = length(pass_history),
    converged_at_pass     = converged_at,
    convergence_rationale = convergence_rationale,
    n_ai_decisions        = walk_state$n_calls,
    n_failed_calls        = walk_state$n_failed_calls,
    decisions             = walk_state$decisions,
    pass_history          = .summarize_pass_history_for_record(pass_history),
    code_to_theme_map     = list(),
    code_to_subtheme_map  = list()
  )
  ts <- rebuild_code_to_theme_map(ts, coding_state)

  if (!is.null(audit_log)) {
    final_theme_names <- vapply(theme_list, function(t) t$name, character(1))
    log_ai_decision(audit_log, "theming", "theme_structure",
                    algorithm = "multi_pass_v2",
                    n_themes = length(theme_list),
                    theme_names = paste(final_theme_names, collapse = "; "),
                    n_substantive_passes = length(pass_history),
                    n_ai_decisions = walk_state$n_calls)
  }

  # Final cluster-snapshot for live tracking (post-labeling state)
  live_snapshot_clusters(
    live_tracker,
    walk_status = "v2_complete",
    walk_state  = walk_state,
    themes_so_far = lapply(seq_along(theme_list), function(i) {
      t <- theme_list[[i]]
      code_keys_t <- unlist(lapply(t$subthemes, function(s) {
        vapply(s$codes %||% list(), function(c) c$key %||% "", character(1))
      }))
      list(
        name             = t$name,
        description      = t$description,
        decision_origin  = t$decision_origin,
        code_indices     = seq_along(code_keys_t),
        code_keys        = unname(code_keys_t)
      )
    })
  )

  toc()
  log_info("[v2] Generated {length(theme_list)} theme(s) via multi-pass clustering ({walk_state$n_calls} AI decisions)")

  ts
}


# ==============================================================================
# AI propose clustering: one call per pass
# ==============================================================================

#' Ask the AI to propose a partition of the current leaves, or to converge
#'
#' One AI call per pass. The AI sees ALL current leaves at once (full
#' picture, no chunking). For pass 1, leaves are codes. For pass 2+, leaves
#' are clusters from the prior pass, each rendered with its member codes
#' visible so the AI can reason about content, not just cluster IDs.
#'
#' The AI returns either a partition of the leaves into new clusters or
#' verdict='converged'. The schema (\code{.clustering_schema()}) forbids
#' name/description fields during clustering -- labeling is a separate pass
#' (per C-tenet 5).
#'
#' Returns the parsed proposal record:
#' \itemize{
#'   \item \code{verdict}: "continue" or "converged"
#'   \item \code{cluster_assignments}: list of clusters (NULL if converged)
#'   \item \code{overall_rationale}: AI's top-level explanation
#' }
#'
#' Records the call in \code{walk_state} and the audit log.
#'
#' @keywords internal
ai_propose_clustering <- function(leaves, pass_index, prior_history,
                                    codes,
                                    provider,
                                    research_focus = "",
                                    concept_str = "",
                                    reference_text = "",
                                    reflexivity_block = "",
                                    audit_log = NULL,
                                    response_cache = NULL,
                                    methodology_override = NULL,
                                    walk_state = NULL) {
  if (is.null(walk_state)) {
    walk_state <- new.env(parent = emptyenv())
    walk_state$decisions      <- list()
    walk_state$n_calls        <- 0L
    walk_state$n_failed_calls <- 0L
  }
  walk_state$n_calls <- walk_state$n_calls + 1L
  call_idx <- walk_state$n_calls

  n_leaves <- length(leaves)

  # ------------------------------------------------------------------
  # System prompt
  # ------------------------------------------------------------------
  pass_context <- if (pass_index == 1L) {
    paste0(
      "This is PASS 1 of the multi-pass clustering. Each leaf is an ",
      "inductively-generated CODE from the coded corpus. Your task is to ",
      "group these codes into top-level conceptual clusters."
    )
  } else {
    paste0(
      "This is PASS ", pass_index, " of multi-pass clustering. Each leaf ",
      "below is a CLUSTER produced by pass ", pass_index - 1L, ", ",
      "containing one or more codes. Your task is to decide whether any ",
      "of these clusters should be grouped into a larger cluster, OR ",
      "whether the current partition is already as grouped as it ",
      "usefully can be (in which case: declare convergence)."
    )
  }

  prior_block <- .format_prior_history_for_prompt(prior_history)

  system_prompt <- paste0(
    "You are an expert qualitative researcher performing thematic ",
    "clustering of codes from a qualitative dataset. You judge clustering ",
    "structure ONLY -- naming and description happen in a separate later ",
    "step. You see ALL leaves at once and propose a single partition that ",
    "groups them, OR you declare that the current structure is final.\n\n",
    pass_context, "\n\n",
    "Research focus: ", research_focus, "\n",
    "Core concepts: ", concept_str, "\n",
    reflexivity_block,
    reference_text,
    prior_block,
    "\n## YOUR TASK\n",
    "Examine every leaf below. Decide:\n\n",
    "  verdict = 'continue':  At least one useful grouping is possible. ",
    "Propose a PARTITION of the leaves into clusters (each leaf in EXACTLY ",
    "one cluster). For each cluster, explain in plain language what ",
    "principle unifies the leaves you are putting together, naming them ",
    "by their code names (pass 1) or by their member-code names (pass 2+). ",
    "Singleton clusters (a leaf alone) are valid when a leaf does not ",
    "belong with any others.\n\n",
    "  verdict = 'converged':  No further useful grouping is possible. ",
    "The current ", n_leaves, " leaves are the final structure. Set ",
    "cluster_assignments to null. Explain WHY no further grouping would yield ",
    "useful conceptual structure.\n\n",
    "## ANTI-BIAS GUIDANCE\n",
    "- It is NORMAL for clustering to converge after 1-3 passes. Do not ",
    "force additional passes for their own sake.\n",
    "- It is NORMAL for some clusters to be singletons -- a leaf that ",
    "doesn't belong with others is a real finding, not a clustering ",
    "failure.\n",
    "- BUT distinguish a genuinely distinct CONCEPT (keep as a ",
    "singleton) from a narrower, more SPECIFIC INSTANCE of another ",
    "cluster's organizing principle (GROUP it into that cluster). A lone code that is one ",
    "example of a broader theme already in the set -- e.g. a single ",
    "'trigger food' code when a 'triggers' cluster exists -- belongs in ",
    "that theme, not a standalone single-code theme. Reserve singletons ",
    "for concepts with no conceptual home among the other clusters.\n",
    "- Topical overlap is NOT a unifying principle. Codes about 'cost' ",
    "and codes about 'convenience' can co-occur in the same entries ",
    "without sharing a CONCEPT. Group by conceptual organizing principle, ",
    "not by shared keywords.\n",
    "- Do NOT collapse everything into one mega-cluster unless the corpus ",
    "is genuinely about ONE thing. A 'kitchen-sink' theme that absorbs ",
    "most codes is a clustering pathology.\n",
    "- Do NOT propose a name or description for any cluster. Naming is a ",
    "separate later pass. Use the cluster_rationale field to JUSTIFY each ",
    "grouping decision in your own words."
  )

  # ------------------------------------------------------------------
  # User prompt: the leaves
  # ------------------------------------------------------------------
  leaves_block <- .format_leaves_for_prompt(leaves, codes)

  user_prompt <- paste0(
    "## LEAVES (", n_leaves, " total) -- index from 1 to ", n_leaves, "\n\n",
    leaves_block,
    "\n## QUESTION\n",
    "Propose a partition of the ", n_leaves, " leaves above into clusters, ",
    "OR declare convergence. If converging, set cluster_assignments=null. ",
    "If proposing a partition, every leaf must appear in exactly one ",
    "cluster, and each cluster must have at least one cluster_rationale."
  )

  result <- tryCatch({
    ai_result <- ai_complete(
      provider, user_prompt, system_prompt,
      task            = "theming",
      temperature     = 0,
      response_schema = .clustering_schema(),
      methodology_override = methodology_override
    )
    if (!is.null(audit_log)) {
      log_ai_request(audit_log, "theming", ai_result, response_cache,
                      level = sprintf("CLUSTERING_PASS_%d", pass_index),
                      node_idx = pass_index,
                      n_codes = n_leaves,
                      call_idx = call_idx)
    }
    parse_json_safely(ai_result$content)
  }, error = function(e) {
    log_warn("v2 clustering pass {pass_index} call failed: {e$message}; treating as convergence")
    walk_state$n_failed_calls <- walk_state$n_failed_calls + 1L
    NULL
  })

  parsed <- .normalize_clustering_proposal(result, n_leaves, pass_index)

  # Record decision in walk_state
  walk_state$decisions[[length(walk_state$decisions) + 1L]] <- list(
    call_idx          = call_idx,
    level             = sprintf("CLUSTERING_PASS_%d", pass_index),
    pass_index        = pass_index,
    n_leaves          = n_leaves,
    verdict           = parsed$verdict,
    n_clusters        = if (is.null(parsed$cluster_assignments)) 0L
                          else length(parsed$cluster_assignments),
    overall_rationale = parsed$overall_rationale
  )

  if (!is.null(audit_log)) {
    log_ai_decision(audit_log, "theming", "clustering_proposal",
                    algorithm = "multi_pass_v2",
                    pass_index = pass_index,
                    verdict = parsed$verdict,
                    n_leaves = n_leaves,
                    n_clusters = if (is.null(parsed$cluster_assignments)) 0L
                                  else length(parsed$cluster_assignments),
                    rationale = substr(parsed$overall_rationale, 1, 400),
                    call_idx = call_idx)
  }

  parsed
}


#' Coerce an "array-of-objects" JSON field back to a list-of-named-lists
#'
#' \code{parse_json_safely()} uses \code{simplifyDataFrame = TRUE}, so an
#' AI response like \code{[{a:1}, {a:2}]} comes back as a 2-row data frame
#' rather than a list of two lists. This helper turns it back into the
#' list-of-lists shape the v2 algorithm expects, while passing through
#' actual lists unchanged and handling the single-element collapse case.
#'
#' Mirrors the pattern in \code{R/13_themes.R:1818-1838} (the Mode 3
#' emergent-coding admission path) and \code{R/12_theme_data.R:488-491}.
#'
#' @keywords internal
.v2_rows_from_jsonlite <- function(x) {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(r) {
      row <- as.list(x[r, , drop = FALSE])
      # Unwrap list-columns: as.list on a 1-row data frame keeps list-columns
      # as length-1 lists -- unwrap so accessors hit the inner value.
      lapply(row, function(v) if (is.list(v) && length(v) == 1L) v[[1]] else v)
    }))
  }
  if (is.list(x)) {
    # Single-element collapse: a named list with field-name keys is a
    # singleton row, not a list of rows.
    nm <- names(x)
    if (!is.null(nm) && length(nm) > 0L && any(nzchar(nm))) {
      return(list(x))
    }
    return(x)
  }
  list()
}


#' Normalize and validate the AI's clustering proposal
#'
#' Coerces inconsistent verdict/assignment combinations into a usable shape,
#' enforces the partition property (each leaf in exactly one cluster), and
#' falls back to convergence on any unrecoverable malformation.
#'
#' @keywords internal
.normalize_clustering_proposal <- function(raw, n_leaves, pass_index) {
  if (is.null(raw) || is.null(raw$verdict)) {
    log_warn(sprintf(
      "v2 pass %d: AI response missing/unparseable; coercing to convergence",
      pass_index
    ))
    return(list(
      verdict             = "converged",
      cluster_assignments = NULL,
      overall_rationale   = "AI response malformed or absent; coerced to convergence.",
      coerced             = TRUE
    ))
  }

  verdict <- as.character(raw$verdict)
  overall_rationale <- as.character(raw$overall_rationale %||% "")

  if (identical(verdict, "converged")) {
    return(list(
      verdict             = "converged",
      cluster_assignments = NULL,
      overall_rationale   = overall_rationale
    ))
  }

  if (!identical(verdict, "continue")) {
    log_warn(sprintf(
      "v2 pass %d: unknown verdict '%s'; coercing to convergence",
      pass_index, verdict
    ))
    return(list(
      verdict             = "converged",
      cluster_assignments = NULL,
      overall_rationale   = paste0("Unknown verdict '", verdict,
                                    "'; coerced to convergence. ",
                                    overall_rationale),
      coerced             = TRUE
    ))
  }

  # verdict == "continue": validate cluster_assignments
  ca <- .v2_rows_from_jsonlite(raw$cluster_assignments)
  if (length(ca) == 0L) {
    log_warn(sprintf(
      "v2 pass %d: verdict='continue' but cluster_assignments is empty; coercing to convergence",
      pass_index
    ))
    return(list(
      verdict             = "converged",
      cluster_assignments = NULL,
      overall_rationale   = paste0("Empty cluster_assignments with continue verdict; ",
                                    "coerced to convergence. ", overall_rationale),
      coerced             = TRUE
    ))
  }

  # Sanitize each cluster: extract leaf_indices + rationale, validate
  clusters <- list()
  seen_indices <- integer(0)
  for (i in seq_along(ca)) {
    cl <- ca[[i]]
    li <- as.integer(unlist(cl$leaf_indices %||% integer(0)))
    li <- li[!is.na(li) & li >= 1L & li <= n_leaves]
    li <- unique(li)
    if (length(li) == 0L) {
      log_warn(sprintf(
        "v2 pass %d cluster %d: no valid leaf_indices after sanitation; dropping cluster",
        pass_index, i
      ))
      next
    }
    rationale <- as.character(cl$cluster_rationale %||% "")
    clusters[[length(clusters) + 1L]] <- list(
      leaf_indices       = li,
      cluster_rationale  = rationale
    )
    seen_indices <- c(seen_indices, li)
  }

  # Enforce partition property: every leaf must appear in exactly one cluster.
  # Add missing leaves as singletons; drop duplicates from later clusters.
  missing <- setdiff(seq_len(n_leaves), seen_indices)
  if (length(missing) > 0L) {
    log_warn(sprintf(
      "v2 pass %d: %d leaf(s) missing from partition (indices %s); adding as singletons",
      pass_index, length(missing), paste(missing, collapse = ",")
    ))
    for (m in missing) {
      clusters[[length(clusters) + 1L]] <- list(
        leaf_indices       = m,
        cluster_rationale  = sprintf("Auto-singleton (orphaned by partition; pass %d)", pass_index)
      )
    }
  }

  # Drop duplicates: if a leaf appears in multiple clusters, keep the first
  # occurrence. Done after the missing-leaves pass to avoid double-adding.
  already <- integer(0)
  cleaned <- list()
  for (cl in clusters) {
    keep <- setdiff(cl$leaf_indices, already)
    if (length(keep) == 0L) next
    cl$leaf_indices <- keep
    cleaned[[length(cleaned) + 1L]] <- cl
    already <- c(already, keep)
  }

  if (length(cleaned) == 0L) {
    log_warn(sprintf(
      "v2 pass %d: no valid clusters after sanitation; coercing to convergence",
      pass_index
    ))
    return(list(
      verdict             = "converged",
      cluster_assignments = NULL,
      overall_rationale   = paste0("No valid clusters after sanitation; ",
                                    "coerced to convergence. ", overall_rationale),
      coerced             = TRUE
    ))
  }

  list(
    verdict             = "continue",
    cluster_assignments = cleaned,
    overall_rationale   = overall_rationale
  )
}


# ==============================================================================
# Apply partition: pure function
# ==============================================================================

#' Apply a proposed partition to the current leaves; produce new cluster-leaves
#'
#' Pure function (no AI call). Given the current leaves and the AI's
#' proposed cluster_assignments, produces a new list of leaves where each
#' new leaf is a cluster containing the original leaves grouped by the
#' partition.
#'
#' Honors C2: code keys flow through unchanged. The new cluster-leaf carries
#' the union of its member leaves' member_code_keys, plus lineage metadata
#' for transparency.
#'
#' @param leaves Current list of leaf records.
#' @param cluster_assignments List of cluster records, each with
#'   \code{leaf_indices} (1-based into \code{leaves}) and
#'   \code{cluster_rationale}.
#' @param pass_n Integer; the pass number that produced this partition
#'   (used in the new leaves' leaf_id and pass_created fields).
#' @return List of new cluster-leaf records.
#' @keywords internal
apply_partition <- function(leaves, cluster_assignments, pass_n) {
  if (is.null(cluster_assignments) || length(cluster_assignments) == 0L) {
    return(leaves)
  }

  lapply(seq_along(cluster_assignments), function(c_idx) {
    cl <- cluster_assignments[[c_idx]]
    indices <- as.integer(cl$leaf_indices)
    member_leaves <- leaves[indices]

    # Flatten member_code_keys from all member leaves
    member_code_keys <- unique(unlist(lapply(member_leaves, function(l) {
      as.character(l$member_code_keys %||% character(0))
    })))

    # Build lineage: append this cluster-creation step on top of each
    # member's prior lineage. Records the pre-merge identity of each
    # member so a transparency report can render the full ancestry.
    new_lineage <- list(
      pass            = pass_n,
      cluster_index   = c_idx,
      source_leaf_ids = vapply(member_leaves, function(l) l$leaf_id, character(1)),
      n_source_leaves = length(member_leaves),
      cluster_rationale = cl$cluster_rationale %||% ""
    )
    inherited_lineage <- lapply(member_leaves, function(l) l$lineage %||% list())

    list(
      leaf_id            = sprintf("leaf_p%d_%d", pass_n, c_idx),
      leaf_type          = "cluster",
      member_code_keys   = member_code_keys,
      n_codes            = length(member_code_keys),
      pass_created       = pass_n,
      cluster_rationale  = cl$cluster_rationale %||% "",
      source_leaf_ids    = vapply(member_leaves, function(l) l$leaf_id, character(1)),
      source_leaves      = member_leaves,
      lineage            = c(list(new_lineage), inherited_lineage)
    )
  })
}


#' Check whether a proposed partition is the identity (no merges happened)
#'
#' An identity partition is one where every cluster contains exactly one
#' source leaf, so the post-partition leaves are 1:1 with the pre-partition
#' leaves (just renamed). This signals "no useful further grouping" even if
#' the AI didn't explicitly say verdict='converged', and convergence is
#' coerced to avoid wasted iteration.
#'
#' @keywords internal
.partition_is_identity <- function(pre_leaves, post_leaves) {
  if (length(pre_leaves) != length(post_leaves)) return(FALSE)
  # Every post-leaf should have exactly one source_leaf_id
  for (l in post_leaves) {
    if (length(l$source_leaf_ids %||% character(0)) != 1L) return(FALSE)
  }
  pre_ids <- vapply(pre_leaves, function(l) l$leaf_id, character(1))
  post_sources <- vapply(post_leaves, function(l) l$source_leaf_ids[[1]], character(1))
  setequal(pre_ids, post_sources)
}

#' Check whether two partitions are structurally equivalent (oscillation)
#'
#' \code{.partition_is_identity} only catches
#' literal-identity partitions (each cluster has one source leaf). An
#' AI that proposes the SAME multi-leaf partition on consecutive passes
#' (e.g. \code{\{A,B\}+\{C,D\}} then \code{\{A,B\}+\{C,D\}} again) is
#' also signalling that no further useful grouping is possible -- but
#' the second pass's partition isn't an identity over its input (which
#' is already \code{\{A,B\}, \{C,D\}}), so the identity check misses it.
#'
#' This helper compares two sets of leaves by the SETS of their
#' \code{member_code_keys}. If the two leaf-lists carry the same
#' code-key buckets (treating cluster order as irrelevant), they are
#' structurally equivalent, so convergence can be coerced.
#'
#' @keywords internal
.partition_is_structurally_equivalent <- function(pre_leaves, post_leaves) {
  if (length(pre_leaves) != length(post_leaves)) return(FALSE)
  pre_buckets <- lapply(pre_leaves, function(l) {
    sort(as.character(l$member_code_keys %||% character(0)))
  })
  post_buckets <- lapply(post_leaves, function(l) {
    sort(as.character(l$member_code_keys %||% character(0)))
  })
  # Canonical sort: convert each bucket to a single string for set comparison
  pre_keys  <- vapply(pre_buckets,  function(b) paste(b, collapse = "|"), character(1))
  post_keys <- vapply(post_buckets, function(b) paste(b, collapse = "|"), character(1))
  setequal(pre_keys, post_keys) && length(pre_keys) == length(post_keys)
}


# ==============================================================================
# Derive theme + subtheme structure: pure function
# ==============================================================================

#' Derive theme + subtheme structural skeleton from pass history
#'
#' Pure function (no AI call). Given the recorded pass_history and the
#' final converged leaves, produces a two-level structural skeleton:
#' themes (final-pass clusters) containing subthemes (penultimate-pass
#' clusters) containing codes (atomic leaves).
#'
#' Behavior by pass count k = length(pass_history):
#' \itemize{
#'   \item k == 0: AI converged immediately at pass 1. Each code is its
#'     own theme; no subthemes (single virtual subtheme wraps the code).
#'   \item k == 1: AI converged at pass 2 (one substantive pass applied).
#'     Themes = pass-1 clusters; no subthemes (single virtual subtheme
#'     wraps each theme's codes).
#'   \item k >= 2: AI converged at pass k+1. Themes = final-pass clusters
#'     (= pass-k clusters). Subthemes = penultimate-pass clusters (=
#'     pass-(k-1) clusters). Earlier passes (1..k-2) are collapsed into
#'     the subthemes, with their full lineage preserved in the Subtheme's
#'     lineage field.
#' }
#'
#' Returns a list of theme records. Each theme record has:
#' \itemize{
#'   \item \code{theme_index}: 1-based position
#'   \item \code{member_code_keys}: character vector (theme's codes, flat)
#'   \item \code{cluster_rationale}: the AI's rationale for this cluster
#'   \item \code{lineage}: the leaf's lineage chain
#'   \item \code{subthemes}: list of subtheme records (possibly empty)
#'   \item \code{decision_origin}: provenance string
#' }
#'
#' Each subtheme record has the same shape minus \code{subthemes}.
#'
#' @keywords internal
derive_theme_subtheme_structure <- function(pass_history, final_leaves, codes) {
  k <- length(pass_history)

  # ----------------------------------------------------------------
  # k == 0: immediate convergence -- each code is its own theme
  # ----------------------------------------------------------------
  if (k == 0L) {
    return(lapply(seq_along(final_leaves), function(i) {
      l <- final_leaves[[i]]
      list(
        theme_index       = i,
        member_code_keys  = l$member_code_keys,
        cluster_rationale = "Each leaf was a single code; AI converged at pass 1 with no grouping.",
        lineage           = l$lineage %||% list(),
        subthemes         = list(),
        decision_origin   = "single_code_no_merge"
      )
    }))
  }

  # ----------------------------------------------------------------
  # k == 1: one substantive pass applied -- themes = pass-1 clusters
  # ----------------------------------------------------------------
  if (k == 1L) {
    return(lapply(seq_along(final_leaves), function(i) {
      l <- final_leaves[[i]]
      list(
        theme_index       = i,
        member_code_keys  = l$member_code_keys,
        cluster_rationale = l$cluster_rationale %||% "",
        lineage           = l$lineage %||% list(),
        subthemes         = list(),
        decision_origin   = "multi_pass_converged"
      )
    }))
  }

  # ----------------------------------------------------------------
  # k >= 2: themes = final_leaves, subthemes = penultimate clusters
  # ----------------------------------------------------------------
  # The penultimate pass output is the post_leaves of pass_history[[k-1]]
  # AND ALSO the pre_leaves of pass_history[[k]]. They are the same list
  # by construction (current_leaves carries over between iterations).
  penultimate_clusters <- pass_history[[k]]$pre_leaves
  partition_last       <- pass_history[[k]]$partition

  # For each final-pass cluster, look up which penultimate clusters were
  # merged into it via partition_last. Each cluster in partition_last has
  # leaf_indices that point into penultimate_clusters.
  lapply(seq_along(final_leaves), function(theme_idx) {
    theme_leaf <- final_leaves[[theme_idx]]
    cluster_record <- partition_last[[theme_idx]]
    sub_indices <- as.integer(cluster_record$leaf_indices %||% integer(0))

    subthemes <- lapply(seq_along(sub_indices), function(sub_pos) {
      sub_idx  <- sub_indices[[sub_pos]]
      sub_leaf <- penultimate_clusters[[sub_idx]]
      list(
        subtheme_index    = sub_pos,
        member_code_keys  = sub_leaf$member_code_keys,
        cluster_rationale = sub_leaf$cluster_rationale %||% "",
        lineage           = sub_leaf$lineage %||% list(),
        decision_origin   = "multi_pass_subtheme"
      )
    })

    list(
      theme_index       = theme_idx,
      member_code_keys  = theme_leaf$member_code_keys,
      cluster_rationale = theme_leaf$cluster_rationale %||% "",
      lineage           = theme_leaf$lineage %||% list(),
      subthemes         = subthemes,
      decision_origin   = "multi_pass_converged"
    )
  })
}


# ==============================================================================
# AI label theme set: single post-convergence call
# ==============================================================================

#' Assign names + descriptions to every theme and subtheme in one AI call
#'
#' Single AI call after convergence. The AI sees the FULL tree (themes ->
#' subthemes -> codes) with every member code visible, and assigns
#' researcher-facing names + descriptions to every node. Cross-theme name
#' distinctness is explicitly enforced via the prompt; the AI is told that
#' if two themes both deserve the same name, the underlying structure is
#' wrong.
#'
#' The schema (\code{.theme_labeling_schema()}) requires the AI to return
#' the same number of themes as the skeleton has, and the same number of
#' subthemes per theme. The orchestrator binds names positionally via
#' \code{theme_index} / \code{subtheme_index}.
#'
#' On AI failure or malformed response, falls back to derived names (top
#' member code per theme) so the run still produces an output -- but logs a
#' warning so the operator knows labeling didn't succeed.
#'
#' @keywords internal
ai_label_theme_set <- function(skeleton, codes, provider,
                                 research_focus = "",
                                 concept_str = "",
                                 reflexivity_block = "",
                                 audit_log = NULL,
                                 response_cache = NULL,
                                 methodology_override = NULL,
                                 walk_state = NULL) {
  if (length(skeleton) == 0L) return(skeleton)

  if (is.null(walk_state)) {
    walk_state <- new.env(parent = emptyenv())
    walk_state$decisions <- list()
    walk_state$n_calls   <- 0L
    walk_state$n_failed_calls <- 0L
  }
  walk_state$n_calls <- walk_state$n_calls + 1L
  call_idx <- walk_state$n_calls

  # Build a lookup: code_key -> code record (so the prompt can show member codes)
  code_by_key <- stats::setNames(codes, vapply(codes, function(c) c$key, character(1)))

  # ------------------------------------------------------------------
  # Build the prompt
  # ------------------------------------------------------------------
  tree_block <- .format_skeleton_for_labeling_prompt(skeleton, code_by_key)

  system_prompt <- paste0(
    "You are an expert qualitative researcher writing the researcher-facing ",
    "names and descriptions for a finalized theme + subtheme structure. ",
    "The structure is FIXED -- you cannot move codes between themes or ",
    "subthemes. Your only job is to NAME each theme and each subtheme ",
    "with a substantive noun phrase that reads as a research finding, and ",
    "to write a 1-2 sentence description in researcher voice.\n\n",
    "Research focus: ", research_focus, "\n",
    "Core concepts: ", concept_str, "\n",
    reflexivity_block,
    "\n## NAMING REQUIREMENTS\n",
    "- Theme names: 3-12 word substantive noun phrases that name a research ",
    "finding (e.g. 'Identity reconstruction after medication onset'). NOT ",
    "list-of-things openers like 'Various aspects of X', 'Mixed experiences ",
    "with Y', 'Comprehensive view of Z'.\n",
    "- Theme names must be DISTINCT. If two themes both deserve the same ",
    "name, the underlying structure is wrong -- but the structure is fixed ",
    "at this stage, so distinguish them by their unique organizing ",
    "principle. Make each name reflect what makes THAT theme different ",
    "from its siblings.\n",
    "- Subtheme names: 3-10 word phrases that name what their codes share ",
    "and that distinguish from sibling subthemes AND from the parent theme.\n",
    "- Descriptions: state what the central organizing concept IS and how ",
    "it manifests across the member codes. Specific enough to distinguish ",
    "from sibling themes."
  )

  user_prompt <- paste0(
    "## STRUCTURE TO LABEL (", length(skeleton), " themes)\n\n",
    tree_block,
    "\n## YOUR TASK\n",
    "Return the same structure with researcher-facing name + description ",
    "for every theme and every subtheme. Preserve theme_index and ",
    "subtheme_index positions exactly -- do not reorder or add or remove. ",
    "If a theme has no subthemes, return an empty subthemes array for it."
  )

  raw <- tryCatch({
    ai_result <- ai_complete(
      provider, user_prompt, system_prompt,
      task            = "theming",
      temperature     = 0,
      response_schema = .theme_labeling_schema(),
      methodology_override = methodology_override
    )
    if (!is.null(audit_log)) {
      log_ai_request(audit_log, "theming", ai_result, response_cache,
                      level = "LABELING_PASS",
                      node_idx = 0L,
                      n_codes = length(codes),
                      call_idx = call_idx)
    }
    parse_json_safely(ai_result$content)
  }, error = function(e) {
    log_warn("v2 labeling pass failed: {e$message}; falling back to derived names")
    walk_state$n_failed_calls <- walk_state$n_failed_calls + 1L
    NULL
  })

  labeled <- .merge_labels_into_skeleton(skeleton, raw, code_by_key)

  if (!is.null(audit_log)) {
    final_names <- vapply(labeled, function(t) t$name %||% "", character(1))
    log_ai_decision(audit_log, "theming", "label_pass",
                    algorithm = "multi_pass_v2",
                    n_themes = length(labeled),
                    theme_names = paste(final_names, collapse = "; "),
                    call_idx = call_idx,
                    used_fallback = is.null(raw))
  }

  labeled
}


#' Merge AI-returned labels into the structural skeleton
#'
#' Positional binding via theme_index / subtheme_index. Falls back to
#' derived names (top member code) when the AI's response is missing a
#' position or malformed.
#'
#' @keywords internal
.merge_labels_into_skeleton <- function(skeleton, raw, code_by_key) {
  ai_themes <- if (is.null(raw)) list() else .v2_rows_from_jsonlite(raw$themes)

  # Index AI themes by theme_index
  ai_by_idx <- list()
  for (t in ai_themes) {
    ti <- suppressWarnings(as.integer(t$theme_index))
    if (length(ti) == 1L && !is.na(ti)) {
      ai_by_idx[[as.character(ti)]] <- t
    }
  }

  lapply(seq_along(skeleton), function(i) {
    th <- skeleton[[i]]
    ai_t <- ai_by_idx[[as.character(i)]]

    fallback_name <- .derive_fallback_theme_name(th, code_by_key)
    fallback_desc <- .derive_fallback_description(th, code_by_key)

    th$name <- if (!is.null(ai_t) && nzchar(ai_t$name %||% "")) {
      as.character(ai_t$name)
    } else {
      fallback_name
    }
    th$description <- if (!is.null(ai_t) && nzchar(ai_t$description %||% "")) {
      as.character(ai_t$description)
    } else {
      fallback_desc
    }

    # Subthemes
    ai_subs <- if (!is.null(ai_t)) .v2_rows_from_jsonlite(ai_t$subthemes) else list()
    ai_subs_by_idx <- list()
    for (s in ai_subs) {
      si <- suppressWarnings(as.integer(s$subtheme_index))
      if (length(si) == 1L && !is.na(si)) {
        ai_subs_by_idx[[as.character(si)]] <- s
      }
    }

    th$subthemes <- lapply(seq_along(th$subthemes), function(j) {
      sub <- th$subthemes[[j]]
      ai_sub <- ai_subs_by_idx[[as.character(j)]]
      fb_name <- .derive_fallback_subtheme_name(sub, code_by_key)
      fb_desc <- .derive_fallback_description(sub, code_by_key)
      sub$name <- if (!is.null(ai_sub) && nzchar(ai_sub$name %||% "")) {
        as.character(ai_sub$name)
      } else {
        fb_name
      }
      sub$description <- if (!is.null(ai_sub) && nzchar(ai_sub$description %||% "")) {
        as.character(ai_sub$description)
      } else {
        fb_desc
      }
      sub
    })

    th
  })
}


#' Derive a fallback theme name from the top-frequency member code
#' @keywords internal
.derive_fallback_theme_name <- function(node, code_by_key) {
  keys <- as.character(node$member_code_keys %||% character(0))
  if (length(keys) == 0L) return("Unnamed theme")
  if (length(keys) == 1L) {
    cd <- code_by_key[[keys[[1]]]]
    return(as.character(cd$name %||% keys[[1]]))
  }
  # Multi-code: build a "X, Y, and Z" composite from top 3 by frequency.
  # Filter NULL lookups defensively: the lookup
  # would normally hit, since codes is the same list that produced the
  # keys, but if upstream tampering ever creates a mismatch this avoids
  # empty-string artifacts in the fallback name.
  members <- Filter(Negate(is.null), lapply(keys, function(k) code_by_key[[k]]))
  if (length(members) == 0L) return(paste(keys, collapse = " / "))
  freqs <- vapply(members, function(c) as.integer(c$frequency %||% 0L), integer(1))
  ord <- order(freqs, decreasing = TRUE)
  top_names <- vapply(members[ord[seq_len(min(3L, length(members)))]],
                       function(c) as.character(c$name %||% c$key %||% ""),
                       character(1))
  top_names <- top_names[nzchar(top_names)]
  if (length(top_names) == 0L) return(paste(keys[seq_len(min(3L, length(keys)))], collapse = " / "))
  paste(top_names, collapse = " / ")
}

#' Derive a fallback subtheme name (same logic, different default phrasing)
#' @keywords internal
.derive_fallback_subtheme_name <- function(sub, code_by_key) {
  keys <- as.character(sub$member_code_keys %||% character(0))
  if (length(keys) == 0L) return(NA_character_)
  .derive_fallback_theme_name(sub, code_by_key)
}

#' Derive a fallback description by joining top-3 code names
#' @keywords internal
.derive_fallback_description <- function(node, code_by_key) {
  keys <- as.character(node$member_code_keys %||% character(0))
  if (length(keys) == 0L) return("")
  if (length(keys) == 1L) {
    cd <- code_by_key[[keys[[1]]]]
    return(as.character(cd$description %||% ""))
  }
  # Filter NULL lookups defensively
  members <- Filter(Negate(is.null), lapply(keys, function(k) code_by_key[[k]]))
  if (length(members) == 0L) return("")
  freqs <- vapply(members, function(c) as.integer(c$frequency %||% 0L), integer(1))
  ord <- order(freqs, decreasing = TRUE)
  top_names <- vapply(members[ord[seq_len(min(3L, length(members)))]],
                       function(c) as.character(c$name %||% c$key %||% ""),
                       character(1))
  top_names <- top_names[nzchar(top_names)]
  if (length(top_names) == 0L) return("")
  paste0(
    "Codes share a conceptual organizing principle expressed across: ",
    paste(top_names, collapse = ", "), if (length(members) > 3L) ", and others." else "."
  )
}


# ==============================================================================
# Prompt-formatting helpers
# ==============================================================================

# Maximum codes to enumerate verbatim within a single cluster-leaf summary.
# Not a methodological knob -- a context-window economy bound.
.V2_MAX_CLUSTER_MEMBER_CODES <- 50L

#' Render the current leaves as a numbered list for the AI prompt
#'
#' Pass 1 leaves are codes (key + name + description). Pass 2+ leaves are
#' clusters; each is rendered with its member codes flattened and a brief
#' cluster_rationale from the prior pass. Truncated to top-N by frequency
#' for very large clusters per \code{.V2_MAX_CLUSTER_MEMBER_CODES}.
#'
#' @keywords internal
.format_leaves_for_prompt <- function(leaves, codes) {
  code_by_key <- stats::setNames(codes, vapply(codes, function(c) c$key, character(1)))

  lines <- vapply(seq_along(leaves), function(i) {
    l <- leaves[[i]]
    if (identical(l$leaf_type, "code")) {
      ck <- l$member_code_keys[[1]]
      cd <- code_by_key[[ck]]
      desc_str <- if (nzchar(cd$description %||% "")) {
        paste0("\n      Description: ", substr(cd$description, 1, 250))
      } else ""
      sprintf("  [%d] CODE: \"%s\" (freq=%d)%s",
              i, cd$name %||% ck, as.integer(cd$frequency %||% 0L), desc_str)
    } else {
      # Cluster leaf: render member codes
      keys <- as.character(l$member_code_keys %||% character(0))
      n_total <- length(keys)
      members <- lapply(keys, function(k) code_by_key[[k]])
      freqs   <- vapply(members, function(c) as.integer(c$frequency %||% 0L), integer(1))
      ord     <- order(freqs, decreasing = TRUE)
      show_k  <- min(n_total, .V2_MAX_CLUSTER_MEMBER_CODES)
      shown_codes <- members[ord[seq_len(show_k)]]
      member_lines <- vapply(shown_codes, function(c) {
        sprintf("       - \"%s\" (freq=%d)", c$name %||% c$key, c$frequency %||% 0L)
      }, character(1))
      member_block <- paste(member_lines, collapse = "\n")
      omitted <- n_total - show_k
      omitted_note <- if (omitted > 0L) {
        sprintf("\n       ... and %d other codes (top %d by frequency shown).", omitted, show_k)
      } else ""
      prior_rationale <- if (nzchar(l$cluster_rationale %||% "")) {
        paste0("\n      Prior-pass rationale: ", substr(l$cluster_rationale, 1, 400))
      } else ""
      sprintf("  [%d] CLUSTER (%d member codes):%s\n%s%s",
              i, n_total, prior_rationale, member_block, omitted_note)
    }
  }, character(1))

  paste(lines, collapse = "\n\n")
}


#' Format prior-pass history as a context block for the AI prompt
#'
#' Shows the AI its own prior partition decisions so it can reason about
#' oscillation (e.g., "I just merged A+B in pass 1; should I split them
#' now?") and so a labeling pass can refer back to structural rationale.
#' Truncated for context-window economy at deep histories.
#'
#' @keywords internal
.format_prior_history_for_prompt <- function(prior_history) {
  if (length(prior_history) == 0L) return("")
  lines <- character(0)
  lines <- c(lines, "\n## PRIOR PASSES (for reference; the AI's own past decisions)\n")
  for (h in prior_history) {
    n_in  <- length(h$pre_leaves %||% list())
    n_out <- length(h$post_leaves %||% list())
    rat   <- substr(as.character(h$overall_rationale %||% ""), 1, 300)
    lines <- c(lines,
               sprintf("- Pass %d: %d leaves -> %d clusters. Rationale: %s",
                       h$pass_n, n_in, n_out, rat))
  }
  paste0(paste(lines, collapse = "\n"), "\n")
}


#' Render the full theme+subtheme skeleton with member codes for the labeling prompt
#'
#' Shows the AI every theme, every subtheme within it, and every code
#' within those (truncated by .V2_MAX_CLUSTER_MEMBER_CODES per node).
#' Member codes show name + brief description so the AI can name
#' coherently. Cluster rationales from prior passes are included so the AI
#' has the structural context behind each grouping.
#'
#' @keywords internal
.format_skeleton_for_labeling_prompt <- function(skeleton, code_by_key) {
  render_codes <- function(keys) {
    if (length(keys) == 0L) return("        (no codes)")
    members <- lapply(keys, function(k) code_by_key[[k]])
    freqs   <- vapply(members, function(c) as.integer(c$frequency %||% 0L), integer(1))
    ord     <- order(freqs, decreasing = TRUE)
    show_k  <- min(length(members), .V2_MAX_CLUSTER_MEMBER_CODES)
    shown   <- members[ord[seq_len(show_k)]]
    lines <- vapply(shown, function(c) {
      desc <- substr(as.character(c$description %||% ""), 1, 150)
      desc_str <- if (nzchar(desc)) paste0(" -- ", desc) else ""
      sprintf("        - \"%s\" (freq=%d)%s",
              c$name %||% c$key, c$frequency %||% 0L, desc_str)
    }, character(1))
    out <- paste(lines, collapse = "\n")
    if (show_k < length(members)) {
      out <- paste0(out, sprintf("\n        ... and %d other codes (top %d shown by frequency).",
                                  length(members) - show_k, show_k))
    }
    out
  }

  blocks <- vapply(seq_along(skeleton), function(i) {
    th <- skeleton[[i]]
    n_subs <- length(th$subthemes %||% list())
    rat <- substr(as.character(th$cluster_rationale %||% ""), 1, 300)
    rat_block <- if (nzchar(rat)) paste0("\n    Structural rationale: ", rat) else ""

    if (n_subs == 0L) {
      paste0(
        sprintf("### Theme %d (%d codes; no subtheme structure)%s\n",
                i, length(th$member_code_keys %||% character(0)), rat_block),
        "    Codes:\n", render_codes(th$member_code_keys), "\n"
      )
    } else {
      sub_blocks <- vapply(seq_along(th$subthemes), function(j) {
        sub <- th$subthemes[[j]]
        sr  <- substr(as.character(sub$cluster_rationale %||% ""), 1, 300)
        sr_block <- if (nzchar(sr)) paste0("\n      Structural rationale: ", sr) else ""
        paste0(
          sprintf("    Subtheme %d (%d codes):%s\n",
                  j, length(sub$member_code_keys %||% character(0)), sr_block),
          render_codes(sub$member_code_keys)
        )
      }, character(1))
      paste0(
        sprintf("### Theme %d (%d codes, %d subthemes)%s\n",
                i, length(th$member_code_keys %||% character(0)), n_subs, rat_block),
        paste(sub_blocks, collapse = "\n\n")
      )
    }
  }, character(1))

  paste(blocks, collapse = "\n\n")
}


#' Compress the pass_history into a summary form for merge_history
#'
#' The full pass_history carries large objects (the leaves at each pass).
#' The merge_history record needs a compact summary that fits in
#' themes.json without bloating it. Keeps the partition + rationale +
#' counts; drops the leaf payloads.
#'
#' @keywords internal
.summarize_pass_history_for_record <- function(pass_history) {
  lapply(pass_history, function(h) {
    list(
      pass_n            = h$pass_n,
      n_leaves_in       = length(h$pre_leaves %||% list()),
      n_clusters_out    = length(h$post_leaves %||% list()),
      overall_rationale = h$overall_rationale %||% "",
      partition = lapply(h$partition %||% list(), function(p) {
        list(
          leaf_indices      = as.integer(p$leaf_indices %||% integer(0)),
          cluster_rationale = as.character(p$cluster_rationale %||% "")
        )
      })
    )
  })
}
