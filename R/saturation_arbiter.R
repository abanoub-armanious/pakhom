# ==============================================================================
# AI saturation arbiter
# ==============================================================================
# Replaces the earlier multi-signal saturation triangulation
# (formerly in R/09_coding.R). The earlier design fired
# three signals -- two heuristic (code creation rate threshold, ITS slope
# ratio < 0.05) and one AI self-assessment -- and stopped when 2+ fired or
# after a hardcoded number of consecutive low-creation windows. Six knobs
# (saturation_enabled / saturation_window / saturation_threshold /
# saturation_confirmations / min_coded_before_saturation /
# ai_assessment_interval) plus the hardcoded 0.05 slope threshold gated
# the decision.
#
# Per the C1 commitment ("AI decides when to stop. No hardcoded
# n_themes/n_subthemes/saturation thresholds; AI judges saturation rather
# than threshold-based heuristics"), all six knobs + the 0.05 slope
# threshold are removed. The heuristic numbers are still computed and
# passed to the AI as EVIDENCE in the prompt, but they no longer GATE the
# decision -- the AI's single 3-valued verdict
# (reached | not_yet | uncertain) is the sole decision.
#
# Cadence: max(20L, ceiling(n_corpus / 50)) -- auto-scaled per corpus;
# no knob. A 9,178-entry corpus checks every ~184 entries (~50 checks
# total). A 100-entry corpus checks every 20 entries (~4 checks).
#
# Articulation requirement: the schema mandates a 2-4 sentence
# articulation BEFORE the verdict (the anti-vacuous pattern).
# Articulations under 30 chars downgrade "reached" -> "not_yet" so the
# AI can't declare saturation without substantive reasoning.
#
# Failure handling: 3 consecutive AI call failures -> one warning is
# logged per failure streak and coding continues; the arbiter retries at
# each subsequent cadence checkpoint (never silently saturate; never
# silently never-saturate). The failure streak resets on any successful
# call.
#
# Audit trail: decision_type = "saturation_judgment" recorded in
# ai_decisions.jsonl with verdict, n_coded, articulation excerpt, and
# rationale. cf. saturation_signal (the earlier design) which carried the
# raw signal booleans.
# ==============================================================================

#' Auto-scaled cadence for the AI saturation arbiter
#'
#' Returns the number of coded entries between successive AI saturation
#' checks. The formula \code{max(20L, ceiling(n_corpus / 50))} produces
#' ~50 checks regardless of corpus size, scaled so small corpora aren't
#' over-polled and large corpora aren't under-polled.
#'
#' Examples:
#' \itemize{
#'   \item n_corpus = 100  -> cadence = 20  (~5 checks)
#'   \item n_corpus = 1000 -> cadence = 20  (~50 checks; cadence floor)
#'   \item n_corpus = 9178 -> cadence = 184 (~50 checks)
#'   \item n_corpus = 50000-> cadence = 1000 (~50 checks)
#' }
#'
#' Floor of 20 prevents over-polling tiny corpora (where the floor
#' produces 5 checks rather than 50).
#'
#' @param n_corpus Integer; total entries in the corpus (after row
#'   filtering, before any are skipped).
#' @return Integer cadence (>= 20L).
#' @keywords internal
.saturation_cadence <- function(n_corpus) {
  max(20L, as.integer(ceiling(n_corpus / 50)))
}

#' Format the recent saturation-curve trajectory as compact prompt text
#'
#' Picks the last \code{n_recent} rows of the saturation curve (if
#' available) and renders each as one line:
#' \code{"entries_coded=300, n_codes=42, new_in_window=3, reuse_density=0.140"}.
#' The reuse_density is 1 - slope_ratio (i.e., fraction of assignments
#' going to EXISTING codes); it's the inverse of the ITS ratio so the AI
#' sees high reuse_density -> high saturation candidate.
#'
#' @keywords internal
.format_saturation_curve_for_prompt <- function(curve, n_recent = 6L) {
  if (is.null(curve) || nrow(curve) == 0L) {
    return("(no curve data yet -- this is the first check)")
  }
  k <- min(n_recent, nrow(curve))
  recent <- curve[(nrow(curve) - k + 1L):nrow(curve), , drop = FALSE]
  lines <- vapply(seq_len(k), function(i) {
    r <- recent[i, ]
    reuse_density <- 1 - (r$slope_ratio %||% 1)
    sprintf("  - entries_coded=%d, n_codes=%d, new_in_window=%d, reuse_density=%.3f",
            as.integer(r$entries_coded %||% 0L),
            as.integer(r$n_codes %||% 0L),
            as.integer(r$new_codes_in_window %||% 0L),
            reuse_density)
  }, character(1))
  paste(lines, collapse = "\n")
}

#' Sample N codes from a codebook, weighted toward the most-frequent
#'
#' Returns up to \code{n} code records (name + frequency) from
#' \code{state$codebook}, sorted by frequency descending. Used to give
#' the AI a sense of what the codebook contains without dumping the
#' whole thing into the prompt for large codebooks.
#'
#' @keywords internal
.sample_codebook_for_prompt <- function(codebook, n = 30L) {
  if (length(codebook) == 0L) return("(empty codebook)")
  freqs <- vapply(codebook, function(cb) cb$frequency %||% 0L, integer(1))
  names_vec <- vapply(codebook, function(cb) cb$code_name %||% "", character(1))
  ord <- order(freqs, decreasing = TRUE)
  k <- min(n, length(ord))
  sel <- ord[seq_len(k)]
  lines <- sprintf("  %2d. %s (n=%d)", seq_len(k),
                   names_vec[sel], freqs[sel])
  paste(lines, collapse = "\n")
}

#' Build the prompt for the AI saturation arbiter
#'
#' Self-contained string. Includes:
#' \itemize{
#'   \item the research focus (so the AI judges saturation in context)
#'   \item the recent saturation-curve trajectory (new-codes-per-window
#'     + reuse density) -- this is the EVIDENCE the earlier
#'     heuristic signals computed; now passed to the AI as data
#'   \item codebook composition summary (top-N codes by frequency)
#'   \item n_coded / n_corpus progress
#' }
#'
#' @keywords internal
.build_saturation_prompt <- function(state, research_focus,
                                       n_coded, n_corpus,
                                       n_done) {
  curve_block <- .format_saturation_curve_for_prompt(state$saturation$curve)
  codebook_block <- .sample_codebook_for_prompt(state$codebook, n = 30L)
  n_codes <- length(state$codebook)

  paste0(
    "## RESEARCH FOCUS\n", research_focus, "\n\n",
    "## SATURATION CHECK\n",
    "You have been coding entries one at a time. Your task is to judge ",
    "whether thematic saturation has been reached: have new entries ",
    "stopped surfacing genuinely novel codes, or is the codebook still ",
    "meaningfully growing?\n\n",
    "## CORPUS PROGRESS\n",
    sprintf("  - entries_coded: %d\n", n_coded),
    sprintf("  - entries_processed (incl. skipped): %d\n", n_done),
    sprintf("  - total corpus size: %d (%d%% processed)\n",
             n_corpus, as.integer(round(100 * n_done / max(1L, n_corpus)))),
    sprintf("  - current codebook size: %d codes\n", n_codes),
    "\n## RECENT GROWTH TRAJECTORY (last several windows)\n",
    curve_block, "\n\n",
    "## CODEBOOK COMPOSITION (top-30 by frequency)\n",
    codebook_block, "\n\n",
    "## TASK\n",
    "Articulate first, then decide.\n\n",
    "ARTICULATE (2-4 sentences): What do you observe in the trajectory ",
    "and codebook? Is the new-codes-per-window count trending toward ",
    "zero or oscillating? Is the reuse_density high (codes being reused) ",
    "or low (every entry generating new codes)? Are the recent additions ",
    "filling conceptual gaps the early codebook missed, or are they ",
    "splits/synonyms of existing codes?\n\n",
    "DECIDE:\n",
    "  reached:    The codebook is stable; new entries are predominantly ",
    "reusing existing codes; the recent additions are not surfacing new ",
    "concepts. STOP coding.\n",
    "  not_yet:    The codebook is still growing meaningfully; recent ",
    "additions are filling genuine conceptual gaps. CONTINUE coding.\n",
    "  uncertain:  The evidence is insufficient to judge (e.g., too ",
    "little coded yet for the trajectory to be informative, or the ",
    "trajectory is highly noisy). CONTINUE coding -- the next check will ",
    "see more evidence.\n\n",
    "## ANTI-BIAS GUIDANCE\n",
    "- A long flat new_in_window=0 stretch is the strongest 'reached' ",
    "signal; one or two zero-windows is not.\n",
    "- High reuse_density alone is not saturation if new-code growth is ",
    "still happening at a substantial rate (the corpus may be ",
    "internally varied).\n",
    "- Early in coding (e.g., n_coded < cadence * 3), 'uncertain' is ",
    "preferred over a confident 'reached' -- the trajectory needs ",
    "several windows of data to be informative.\n",
    "- Saturation is not 'enough has been done'; it is 'the codebook has ",
    "stopped growing'. These are different. Be honest."
  )
}

#' AI saturation arbiter
#'
#' Single 3-valued judgment call to the AI. Returns a list with verdict
#' (one of \code{"reached"}, \code{"not_yet"}, \code{"uncertain"}) plus
#' articulation, rationale, and a success flag (FALSE on parse/API failure).
#'
#' Articulations under 30 chars downgrade "reached" to "not_yet" --
#' The anti-vacuous pattern. Failures are NOT counted against the
#' verdict (the caller's failure-streak counter tracks failures separately).
#'
#' @param state ProgressiveCodingState with codebook + curve
#' @param provider AIProvider
#' @param research_focus Research focus string (for in-context judgment)
#' @param n_coded Number of entries CODED so far (skipped excluded)
#' @param n_corpus Total entries in the corpus
#' @param n_done Total entries PROCESSED so far (coded + skipped)
#' @param audit_log Optional AuditLog
#' @param response_cache Optional ResponseCache
#' @return list(verdict, articulation, rationale, success)
#' @keywords internal
.ai_judge_saturation <- function(state, provider, research_focus,
                                   n_coded, n_corpus, n_done,
                                   audit_log = NULL,
                                   response_cache = NULL) {
  prompt <- .build_saturation_prompt(state, research_focus,
                                       n_coded = n_coded,
                                       n_corpus = n_corpus,
                                       n_done = n_done)

  system_prompt <- paste0(
    "You are an expert qualitative researcher judging whether thematic ",
    "saturation has been reached in an inductive coding process. Your ",
    "verdict is the sole decision for whether coding stops; there is no ",
    "downstream threshold to backstop you. Be honest -- 'uncertain' is a ",
    "valid first-class output when the evidence is insufficient."
  )

  # Every ai_complete attempt records an
  # ai_request audit row regardless of parse outcome -- the failure
  # tail must be reconstructible from the audit log (T1.4 transparency).
  # The tryCatch is split into two parts: the AI call (audit-logged on
  # success even if subsequent JSON parsing fails) and the parse step
  # (separate failure mode).
  ai_result <- tryCatch(
    ai_complete(provider, prompt, system_prompt,
                 task = "saturation_check",
                 # Explicit temperature=0 for replay-equivalence; the
                 # default saturation_check temperature is non-zero, so pass 0.
                 temperature = 0,
                 response_schema = .saturation_decision_schema()),
    error = function(e) {
      log_warn("Saturation judgment AI call failed: {e$message}")
      if (!is.null(audit_log)) {
        # Record the failure as a saturation_judgment with verdict
        # = "ai_failure" so the audit log shows the streak even when
        # ai_complete itself never returned.
        log_ai_decision(audit_log, "saturation", "saturation_judgment",
                        verdict = "ai_failure",
                        n_coded = n_coded,
                        n_corpus = n_corpus,
                        n_codes = length(state$codebook),
                        error_message = substr(conditionMessage(e), 1L, 300L))
      }
      NULL
    }
  )

  if (is.null(ai_result)) {
    return(list(verdict = "uncertain",
                articulation = "",
                rationale = "AI call failure",
                success = FALSE))
  }

  # AI call succeeded -- record the ai_request before attempting parse,
  # so even a malformed/unparseable response is still traceable.
  if (!is.null(audit_log)) {
    log_ai_request(audit_log, "saturation", ai_result, response_cache,
                    n_codes = length(state$codebook),
                    n_coded = n_coded, n_corpus = n_corpus)
  }
  result <- parse_json_safely(ai_result$content)

  if (is.null(result) || is.null(result$verdict)) {
    if (!is.null(audit_log)) {
      log_ai_decision(audit_log, "saturation", "saturation_judgment",
                      verdict = "parse_failure",
                      n_coded = n_coded,
                      n_corpus = n_corpus,
                      n_codes = length(state$codebook),
                      raw_excerpt = substr(as.character(ai_result$content %||% ""), 1L, 300L))
    }
    return(list(verdict = "uncertain",
                articulation = "",
                rationale = "Response parse failure",
                success = FALSE))
  }

  verdict <- as.character(result$verdict)
  articulation <- as.character(result$articulation %||% "")
  rationale <- as.character(result$rationale %||% "")

  # Anti-vacuous pattern: articulations under 30 chars
  # downgrade "reached" to "not_yet" so the AI can't declare
  # saturation without substantive reasoning.
  if (identical(verdict, "reached") && nchar(articulation) < 30L) {
    log_warn(paste0(
      "Saturation judgment: articulation too short (",
      nchar(articulation), " chars; min 30) to support 'reached' verdict; ",
      "forcing 'not_yet'. Articulation was: '",
      substr(articulation, 1L, 200L), "'"
    ))
    verdict <- "not_yet"
  }

  # Verdict must be one of the three valid values. Anything else is
  # treated as a parse failure (verdict = uncertain, success = FALSE)
  # so the caller's failure streak counts it.
  if (!verdict %in% c("reached", "not_yet", "uncertain")) {
    log_warn(paste0(
      "Saturation judgment: unknown verdict '", verdict, "'; ",
      "treating as parse failure."
    ))
    return(list(verdict = "uncertain",
                articulation = articulation,
                rationale = rationale,
                success = FALSE))
  }

  if (!is.null(audit_log)) {
    log_ai_decision(audit_log, "saturation", "saturation_judgment",
                    verdict = verdict,
                    n_coded = n_coded,
                    n_corpus = n_corpus,
                    n_codes = length(state$codebook),
                    articulation_excerpt = substr(articulation, 1L, 300L),
                    rationale = rationale)
  }

  list(verdict = verdict,
       articulation = articulation,
       rationale = rationale,
       success = TRUE)
}
