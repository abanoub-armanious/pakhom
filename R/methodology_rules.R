# ==============================================================================
# Methodology Rules Generation (Sprint-4 T1.6)
# ==============================================================================
# Lin & Corley Interpretive Orchestration "methodological-rules" pattern
# (Stream 1 of the strategic audit). Reads the methodology block of
# config.yaml and generates per-mode rule text that ai_complete() injects
# as a system-prompt prefix on every call.
#
# Why config-driven rules instead of hard-coded prompt strings:
# - The model's behavior in each mode is governed by a SET of rules that
#   must apply on EVERY call -- not just the call that "creates a theme"
#   or "codes an entry". Lin & Corley demonstrated that putting the
#   rules in the model's context-window-every-turn is significantly
#   stronger than a one-shot reminder.
# - Rules are mode-specific. Mode 1 (reflexive_scaffold) tells the model
#   to NEVER name themes; Mode 2 (codebook_collaborative) tells it to
#   PROPOSE codes but not name themes; Mode 3 (framework_applied) tells
#   it to apply a researcher-supplied framework verbatim.
# - The universal rules (apply in all modes) cover the Tier-0
#   commitments: quote provenance, participant spread, full-corpus
#   coverage. These rules say "the model's job is bounded; refuse to
#   exceed bounds; emit refusal as a first-class output rather than
#   inventing".
#
# AC9 ("mode rules generated from config and injected to model context
# every turn") is the load-bearing architectural commitment this module
# implements.
# ==============================================================================

#' Generate the methodology-rules text for a config
#'
#' Returns a single character string suitable for prepending to a system
#' prompt. The string includes mode-specific rules + universal Tier-0
#' rules. When \code{config$methodology$mode} is NULL or invalid, returns
#' the universal-rules-only string with a warning -- this is a soft
#' fallback rather than a hard error because legacy/test contexts may
#' instantiate ai_complete without a full config and we don't want to
#' break those paths.
#'
#' @param config A ThematicConfig (or list with the same shape).
#' @return Character: the rules block, prefixed with a header. Empty
#'   string when nothing meaningful can be generated.
#' @export
generate_methodology_rules <- function(config) {
  if (is.null(config)) return("")
  mode <- .config_methodology_mode(config)
  rules <- character(0)

  # Mode-specific rules
  mode_block <- .mode_rules_for(mode)
  if (nzchar(mode_block)) {
    rules <- c(rules, paste0("## Mode rules (", mode, ")\n", mode_block))
  }

  # Universal Tier-0 rules apply in ALL modes
  rules <- c(rules, paste0("## Universal Tier-0 rules\n", .universal_rules()))

  # Memos rules (M1.3) -- emitted only when memos are enabled (NULL means
  # "derive from mode" which we resolve with mandatory_for_modes)
  memos_block <- .memos_rules_for(mode, config$memos)
  if (nzchar(memos_block)) {
    rules <- c(rules, paste0("## Reflexive memos\n", memos_block))
  }

  # Researcher reflexivity context (positionality / paradigm / notes from
  # the study block). Per Olmos-Vega AMEE Guide 149, surfacing positionality
  # in the model's context window every turn is a discipline check on
  # against drift back to a "neutral observer" framing.
  reflexivity_block <- .reflexivity_block_for(config$study)
  if (nzchar(reflexivity_block)) {
    rules <- c(rules, paste0("## Researcher reflexivity\n", reflexivity_block))
  }

  if (length(rules) == 0L) return("")
  paste0(
    "# pakhom methodology rules\n",
    "# (config-driven; injected on every AI call per AC9)\n\n",
    paste(rules, collapse = "\n\n"),
    "\n"
  )
}

# ==============================================================================
# Per-mode rule blocks
# ==============================================================================

#' @keywords internal
.mode_rules_for <- function(mode) {
  if (is.null(mode) || !nzchar(mode)) {
    return("")
  }
  switch(mode,
    "reflexive_scaffold"      = .rules_reflexive_scaffold(),
    "codebook_collaborative"  = .rules_codebook_collaborative(),
    "framework_applied"       = .rules_framework_applied(),
    {
      log_warn("generate_methodology_rules: unknown mode '{mode}'; emitting universal rules only.")
      ""
    }
  )
}

#' @keywords internal
.rules_reflexive_scaffold <- function() {
  paste0(
    "Mode 1 (Reflexive Scaffold) restricts the model to extractive ",
    "operations only:\n",
    "- NEVER propose theme names. NEVER propose code names.\n",
    "- NEVER interpret meaning. NEVER synthesize across entries.\n",
    "- NEVER write a theme description, summary, or executive insight.\n",
    "- ONLY surface questions, counter-evidence, and bounded retrieval ",
    "(provocations) the researcher might not have considered.\n",
    "- If the researcher asks the model to propose a theme, REFUSE and ",
    "explain that theme proposal is the researcher's role in this mode.\n",
    "- Refusal is a first-class output. When in doubt, return an empty ",
    "result with a one-sentence reason rather than inventing content."
  )
}

#' @keywords internal
.rules_codebook_collaborative <- function() {
  paste0(
    "Mode 2 (Codebook Collaborative) allows the model to propose codes ",
    "but bounds higher-order interpretation:\n",
    "- The model MAY propose codes; the researcher accepts, edits, or ",
    "rejects each. The codebook is the researcher's deliverable.\n",
    "- The model does NOT name themes. Theme naming is the researcher's role.\n",
    "- The model does NOT generate theme-level interpretation, ",
    "summary, or synthesis without explicit researcher prompting.\n",
    "- When asked for theme-level work, redirect to codebook-level ",
    "operations or refuse with an explanation."
  )
}

#' @keywords internal
.rules_framework_applied <- function() {
  paste0(
    "Mode 3 (Framework Applied) constrains the model to the researcher's ",
    "framework:\n",
    "- Apply the supplied framework verbatim during the main coding pass. ",
    "The framework's constructs are the only permitted code names.\n",
    "- Flag entries that resist the framework as anomalies (separate ",
    "field). Do NOT generate new framework constructs during coding.\n",
    "- Theme-level decisions are the researcher's. The model's job during ",
    "deductive coding is rigorous application of the framework, not ",
    "extension of it.\n",
    "- A SEPARATE abductive pass (Phase 54; only invoked when the ",
    "framework spec's anomaly_handling is 'extend' or 'revise') may ask ",
    "you to inductively code the ANOMALY segments AFTER framework coding ",
    "completes. That pass operates only on residuals; it does NOT mutate ",
    "the framework spec. Per Vila-Henninger 2024 abductive coding, the ",
    "emergent themes from that pass complement (but never replace) the ",
    "framework themes."
  )
}

# ==============================================================================
# Universal Tier-0 rules (apply in ALL modes)
# ==============================================================================

#' @keywords internal
.universal_rules <- function() {
  paste0(
    "These rules apply in EVERY mode. They are non-negotiable:\n\n",
    "1. **Quote provenance is mandatory** (T0.1). Return character ",
    "offsets that resolve to EXACT source text. If you cannot produce ",
    "an offset that resolves to the verbatim quote, return null for ",
    "that quote rather than inventing content. Paraphrasing is allowed ",
    "ONLY when explicitly marked with an `ai_paraphrase` field; the ",
    "verbatim quote MUST still be the cited source.\n\n",
    "2. **Participant spread matters** (T0.2). When you are asked to ",
    "select representative quotes, do not over-select from a single ",
    "contributor. Prefer breadth over depth-of-one-poster.\n\n",
    "3. **Full-corpus coverage** (T0.3). When asked to summarize a ",
    "set of entries, address the full set; do NOT silently sample or ",
    "truncate to the first few. If the set is too large to address in ",
    "full, say so explicitly rather than producing a partial summary.\n\n",
    "4. **Refusal is a first-class output**. When you cannot perform ",
    "what was asked under these rules, return an empty/refusal ",
    "response with a one-sentence reason. Do NOT silently produce a ",
    "weakened or reduced answer."
  )
}

# ==============================================================================
# Memos block
# ==============================================================================

#' @keywords internal
.memos_rules_for <- function(mode, memos_cfg) {
  if (is.null(memos_cfg)) return("")
  enabled <- if (is.null(memos_cfg$enabled)) {
    # NULL means "derive from mode": enabled iff mode is in mandatory_for_modes
    isTRUE(mode %in% (memos_cfg$mandatory_for_modes %||% character(0)))
  } else {
    isTRUE(memos_cfg$enabled)
  }
  if (!enabled) return("")
  paste0(
    "Reflexive memos are enabled for this mode. The researcher will be ",
    "prompted at: ",
    paste(memos_cfg$prompt_at %||% c("after_coding", "after_themes"),
          collapse = ", "),
    ". Treat the most-recent memo as additional context for any subsequent ",
    "interpretation; do not contradict the memo without explicit ",
    "researcher direction."
  )
}

# ==============================================================================
# Reflexivity block (positionality, paradigm, free-text notes)
# ==============================================================================

#' @keywords internal
.reflexivity_block_for <- function(study_cfg) {
  if (is.null(study_cfg)) return("")
  fields <- character(0)
  positionality <- study_cfg$researcher_positionality %||% ""
  paradigm      <- study_cfg$research_paradigm        %||% ""
  notes         <- study_cfg$reflexive_notes          %||% ""
  if (nzchar(positionality)) {
    fields <- c(fields, sprintf("Researcher positionality: %s", positionality))
  }
  if (nzchar(paradigm)) {
    fields <- c(fields, sprintf("Research paradigm: %s", paradigm))
  }
  if (nzchar(notes)) {
    fields <- c(fields, sprintf("Reflexive notes: %s", notes))
  }
  if (length(fields) == 0L) return("")
  paste(fields, collapse = "\n")
}

# ==============================================================================
# Convenience: generate + write to outputs/<run>/rules/methodology_rules.md
# ==============================================================================

#' Write methodology rules to a markdown file under \code{run_dir}
#'
#' Creates \code{run_dir/rules/methodology_rules.md} with the generated
#' rules. The file is human-readable and serves as the canonical record
#' of the rules text the model was sent on every call during this run.
#' Per AC9 + AC4, rules are stamped on every call AND archived alongside
#' the run output so the methodology paper can attribute observed AI
#' behavior to the specific rules in force.
#'
#' @param config A ThematicConfig (or list).
#' @param run_dir Path to the run output directory.
#' @return Path to the written file (invisibly), or NULL on failure.
#' @export
write_methodology_rules <- function(config, run_dir) {
  rules <- generate_methodology_rules(config)
  if (!nzchar(rules)) {
    log_debug("write_methodology_rules: no rules to write")
    return(NULL)
  }
  rules_dir <- file.path(run_dir, "rules")
  if (!dir.exists(rules_dir)) {
    dir.create(rules_dir, recursive = TRUE, showWarnings = FALSE)
  }
  path <- file.path(rules_dir, "methodology_rules.md")
  tryCatch({
    writeLines(rules, path)
    log_info("Methodology rules written: {path}")
    invisible(path)
  }, error = function(e) {
    log_warn("Could not write methodology rules: {e$message}")
    NULL
  })
}
