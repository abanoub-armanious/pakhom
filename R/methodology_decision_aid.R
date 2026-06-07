# ==============================================================================
# Methodology Decision Aid -- T1.3
# ==============================================================================
# Helps researchers choose a methodology mode appropriate for their study
# design. Per the multi-mode investigation (Stream 6 finding), mode choice
# under uncertainty is the single largest predicted failure path; this
# function is the primary mitigation -- "scaffold the choice itself" per
# Carroll & Rosson Training Wheels research.
#
# This is intentionally minimal in v1; the full interactive 5-question
# wizard expands in Phase C when Mode 1 ships. For now it provides:
#   - A printed comparison of the three modes
#   - A non-interactive recommendation function based on simple heuristics
#   - An interactive prompt-driven path when run from a console
# ==============================================================================

#' Methodology decision aid
#'
#' Helps researchers choose between the three methodology modes
#' (\code{reflexive_scaffold}, \code{codebook_collaborative},
#' \code{framework_applied}) by surfacing the decision-relevant differences.
#' Operates in three modes:
#' \itemize{
#'   \item \code{interactive = TRUE} (default in interactive R sessions):
#'     prompts the researcher with a short series of questions and returns
#'     a recommended mode plus reasoning.
#'   \item \code{interactive = FALSE} with criteria supplied: returns a
#'     recommendation deterministically based on the supplied criteria.
#'   \item Neither: prints a comparison of the three modes for the
#'     researcher to read; returns NULL invisibly.
#' }
#'
#' Per AC3, there is no default methodology mode in
#' \code{validate_config()}; this function exists so users can make an
#' informed choice rather than picking arbitrarily.
#'
#' @param interactive Logical; whether to prompt for input. Defaults to
#'   \code{interactive()} -- TRUE in console sessions, FALSE in scripts.
#' @param ta_family Optional character: one of "reflexive", "codebook",
#'   "template", "framework", "content". Used in non-interactive mode.
#' @param has_apriori_framework Optional logical: whether the researcher
#'   has a pre-existing theoretical framework to apply.
#' @param wants_irr Optional logical: whether the researcher wants
#'   inter-rater reliability statistics in the output.
#' @return A list with elements \code{recommended_mode} (character),
#'   \code{reasoning} (character), \code{alternative} (character or NA).
#'   Invisibly NULL when called in print-only mode.
#' @export
#' @examples
#' \dontrun{
#' # Interactive (when running in a console):
#' result <- methodology_decision_aid()
#'
#' # Non-interactive (deterministic):
#' result <- methodology_decision_aid(
#'   interactive = FALSE,
#'   ta_family = "reflexive",
#'   has_apriori_framework = FALSE
#' )
#'
#' # Print-only comparison:
#' methodology_decision_aid(interactive = FALSE)
#' }
methodology_decision_aid <- function(interactive = base::interactive(),
                                     ta_family = NULL,
                                     has_apriori_framework = NULL,
                                     wants_irr = NULL) {

  # Print-only path: show the comparison table and exit.
  if (!isTRUE(interactive) && is.null(ta_family) &&
      is.null(has_apriori_framework) && is.null(wants_irr)) {
    .print_methodology_comparison()
    return(invisible(NULL))
  }

  # Deterministic path: criteria supplied, no prompting.
  if (!isTRUE(interactive)) {
    return(.recommend_methodology_mode(ta_family, has_apriori_framework, wants_irr))
  }

  # Interactive path: short prompt sequence.
  cat("\n=== pakhom methodology decision aid ===\n\n")
  cat("Five short questions to help you pick a methodology mode.\n")
  cat("Run methodology_decision_aid(interactive = FALSE) for a comparison\n")
  cat("of the three modes without prompts.\n\n")

  q1 <- .ask_choice(
    "Q1. Which family of thematic analysis are you doing?",
    c("reflexive (Braun & Clarke 2022 reflexive TA / Big-Q)",
      "codebook (codebook TA / template TA)",
      "framework (theoretical-framework analysis / abductive)",
      "content (content analysis with a-priori categories)",
      "not sure")
  )

  if (q1 == 5L) {
    cat("\nIf you're not sure, that's normal. The shortest path:\n")
    cat("  - If you have a pre-existing theory or framework you want to test:\n")
    cat("    -> framework_applied\n")
    cat("  - Otherwise, if you want to ship a codebook as your deliverable:\n")
    cat("    -> codebook_collaborative\n")
    cat("  - Otherwise, if you treat themes as your interpretive construction:\n")
    cat("    -> reflexive_scaffold\n\n")
    .print_methodology_comparison()
    return(invisible(NULL))
  }

  ta_family_str <- c("reflexive", "codebook", "template",
                     "framework", "content")[q1]

  q2 <- if (q1 %in% c(3L, 4L)) {
    TRUE  # framework / content always have an a-priori framework
  } else {
    .ask_yes_no("Q2. Do you have a pre-existing theoretical framework to apply?")
  }

  q3 <- .ask_yes_no("Q3. Do you want inter-rater reliability (IRR) statistics in your report?")

  rec <- .recommend_methodology_mode(ta_family_str, q2, q3)

  cat("\n=== Recommendation ===\n")
  cat(sprintf("  Mode: %s\n", rec$recommended_mode))
  cat(sprintf("  Reasoning: %s\n", rec$reasoning))
  if (!is.na(rec$alternative)) {
    cat(sprintf("  Alternative to consider: %s\n", rec$alternative))
  }
  cat("\nTo apply: set methodology.mode in your config.yaml to the value above.\n")
  cat("Documentation: see vignette('methodology-modes') (forthcoming).\n\n")

  invisible(rec)
}

# ------------------------------------------------------------------------------
# Internal helpers
# ------------------------------------------------------------------------------

.print_methodology_comparison <- function() {
  cat("\n=== pakhom methodology modes (multi-mode architecture) ===\n\n")
  cat("Three modes; pick one per run. No default. Methodology is stamped on\n")
  cat("every output and cannot be silently changed mid-run.\n\n")

  cat("--- reflexive_scaffold ---\n")
  cat("  Best for: Big-Q reflexive TA per Braun & Clarke 2022\n")
  cat("  AI role:  Provocateur (questions, counter-narratives, absent voices)\n")
  cat("            AI never outputs codes/themes as findings.\n")
  cat("  Required: Reflexive memos at every pause point; positionality at\n")
  cat("            multiple timepoints; quote provenance verification.\n")
  cat("  IRR:      Disabled (incoherent for reflexive TA per RTARG 2024).\n\n")

  cat("--- codebook_collaborative ---\n")
  cat("  Best for: Codebook TA, template TA, applied health-services research\n")
  cat("  AI role:  Collaborator. AI proposes codes; researcher gates each.\n")
  cat("            Codebook ships as deliverable.\n")
  cat("  Required: Researcher-articulated codebook (in researcher's words).\n")
  cat("  IRR:      Available as quality diagnostic.\n\n")

  cat("--- framework_applied ---\n")
  cat("  Best for: Abductive coding, theoretical-framework analysis,\n")
  cat("            framework analysis (Ritchie & Spencer), content analysis\n")
  cat("            with a-priori categories.\n")
  cat("  AI role:  Framework-applier with anomaly flagger. AI applies\n")
  cat("            researcher-supplied framework; flags entries that resist.\n")
  cat("  Required: theoretical_framework.yaml with constructs and citations;\n")
  cat("            anomaly resolution log.\n")
  cat("  IRR:      Available when framework is positivist.\n\n")

  cat("Run methodology_decision_aid() (no args) in an interactive session\n")
  cat("for a guided 3-question recommendation.\n\n")
  invisible(NULL)
}

.recommend_methodology_mode <- function(ta_family, has_apriori_framework, wants_irr) {
  if (is.null(ta_family)) {
    stop("Non-interactive mode requires ta_family. ",
         "Valid: reflexive, codebook, template, framework, content.")
  }
  ta_family <- match.arg(ta_family,
    c("reflexive", "codebook", "template", "framework", "content"))

  if (ta_family == "reflexive") {
    if (isTRUE(wants_irr)) {
      return(list(
        recommended_mode = .METHODOLOGY_MODE_CODEBOOK_COLLABORATIVE,
        reasoning = paste(
          "You declared reflexive TA but want IRR. Per Braun & Clarke 2022 +",
          "RTARG 2024, IRR is methodologically incongruent with reflexive TA",
          "(it presupposes a 'correct' coding which reflexive TA rejects).",
          "If IRR is genuinely required for your audience, you are doing",
          "codebook TA -- pick codebook_collaborative and report your work as",
          "such. If you want to honor reflexive TA, drop the IRR requirement",
          "and pick reflexive_scaffold."),
        alternative = .METHODOLOGY_MODE_REFLEXIVE_SCAFFOLD
      ))
    }
    return(list(
      recommended_mode = .METHODOLOGY_MODE_REFLEXIVE_SCAFFOLD,
      reasoning = paste(
        "Reflexive TA per Braun & Clarke 2022 treats themes as the analyst's",
        "interpretive construction. Mode 1 (reflexive_scaffold) demotes AI to",
        "provocateur (questions, counter-narratives) so the meaning-making",
        "remains the researcher's. Reflexive memos and multi-timepoint",
        "positionality are mandatory."),
      alternative = NA_character_
    ))
  }

  if (ta_family %in% c("codebook", "template")) {
    return(list(
      recommended_mode = .METHODOLOGY_MODE_CODEBOOK_COLLABORATIVE,
      reasoning = paste(
        "Codebook TA / template TA build a structured codebook applied across",
        "the corpus. Mode 2 (codebook_collaborative) has AI propose codes;",
        "the researcher accepts/edits/rejects each. The codebook ships as",
        "the deliverable. IRR available as quality diagnostic."),
      alternative = NA_character_
    ))
  }

  if (ta_family == "framework") {
    return(list(
      recommended_mode = .METHODOLOGY_MODE_FRAMEWORK_APPLIED,
      reasoning = paste(
        "Framework / theoretical-framework analysis applies an a-priori",
        "framework to the data. Mode 3 (framework_applied) has the researcher",
        "supply the framework (theoretical_framework.yaml); AI applies it and",
        "flags entries that resist (the abductive anomaly loop per",
        "Vila-Henninger et al. 2024). Set framework_spec_path in your",
        "config.yaml."),
      alternative = NA_character_
    ))
  }

  if (ta_family == "content") {
    return(list(
      recommended_mode = .METHODOLOGY_MODE_FRAMEWORK_APPLIED,
      reasoning = paste(
        "Content analysis with a-priori categories is supported via Mode 3",
        "(framework_applied) by treating the categorical scheme as a",
        "framework with a positivist epistemic stance. IRR is available.",
        "If your work is genuinely qualitative content analysis (Mayring",
        "tradition) and you need a constructionist framing, codebook_",
        "collaborative may be more appropriate."),
      alternative = .METHODOLOGY_MODE_CODEBOOK_COLLABORATIVE
    ))
  }

  stop("Unhandled ta_family: ", ta_family)
}

# ==============================================================================
# configuration_selection_aid
# ==============================================================================
# Companion to methodology_decision_aid(). Once a researcher has picked a
# methodology mode (Mode 1/2/3), this aid helps them set the right
# configuration knobs based on their corpus size, codebook size, and
# whether the research focus is narrow or broad.
#
# The recommendations encode empirical evidence from the re-validation
# runs. Three Mode 2 runs across two foci and two scales validated the v2
# algorithm's behavior; the numerical bracket below ("expect 6-10 themes
# from 40-150 codes") is derived from those three runs.
# ==============================================================================

#' Configuration-selection aid
#'
#' Once you've picked a methodology mode via
#' \code{methodology_decision_aid()}, this function helps you set
#' \code{config.yaml} knobs based on your corpus shape. It encodes the
#' empirical evidence from re-validation: three Mode 2 runs at
#' different scales (40, 47, 157 codes) and different research-focus
#' breadths produced 6, 10, and 7 themes respectively -- all in the
#' publication-quality 4-10 range with 0% single-code themes.
#'
#' The function returns a list with: \code{expected_themes} (range),
#' \code{expected_passes} (clustering passes; v2 only), and
#' \code{recommended_review_points} (whether the post-coding or
#' post-themes researcher review pause should be enabled).
#'
#' @param mode One of \code{"reflexive_scaffold"} (M1),
#'   \code{"codebook_collaborative"} (M2), \code{"framework_applied"}
#'   (M3). Get this from \code{methodology_decision_aid()} first.
#' @param corpus_size Integer; approximate number of entries in the
#'   corpus. Used to estimate codebook scale.
#' @param estimated_codebook_size Integer; approximate number of codes
#'   you expect after saturation. If NULL, the function estimates from
#'   \code{corpus_size} (typical ratio: 1 code per 1.5-4 entries
#'   depending on focus breadth, e.g. 40 codes from 60 coded entries
#'   up to 157 codes from 140 coded entries).
#' @param focus_shape One of \code{"narrow_intersection"} (e.g.,
#'   "medication x sleep x binge"), \code{"single_focal"} (e.g.,
#'   "medication adherence"), or \code{"broad"} (e.g., "emotional
#'   experiences"). Narrow foci produce fewer codes per coded entry.
#' @return A list with elements \code{expected_themes},
#'   \code{expected_passes}, \code{recommended_review_points},
#'   \code{expected_wall_time_min}, \code{expected_api_spend_usd}, and
#'   \code{notes}.
#' @export
#' @examples
#' \dontrun{
#' # A 250-entry corpus on a narrow medication x sleep question:
#' configuration_selection_aid(
#'   mode = "codebook_collaborative",
#'   corpus_size = 250,
#'   focus_shape = "narrow_intersection"
#' )
#'
#' # A larger 1000-entry corpus with a broad focus:
#' configuration_selection_aid(
#'   mode = "codebook_collaborative",
#'   corpus_size = 1000,
#'   focus_shape = "broad"
#' )
#' }
configuration_selection_aid <- function(mode,
                                          corpus_size,
                                          estimated_codebook_size = NULL,
                                          focus_shape = c("narrow_intersection",
                                                           "single_focal",
                                                           "broad")) {
  mode <- match.arg(mode,
    c("reflexive_scaffold", "codebook_collaborative", "framework_applied"))
  focus_shape <- match.arg(focus_shape)
  if (!is.numeric(corpus_size) || corpus_size < 1L) {
    stop("corpus_size must be a positive integer (the number of entries in your corpus).",
         call. = FALSE)
  }
  corpus_size <- as.integer(corpus_size[1])

  # Mode 1 has no v2 clustering and no expected_themes prediction;
  # researcher authors themes. Return mode-specific guidance.
  if (mode == "reflexive_scaffold") {
    return(list(
      mode = mode,
      expected_themes = NA_integer_,
      expected_passes = NA_integer_,
      recommended_review_points = list(
        after_coding = FALSE,  # Mode 1 doesn't code in the usual sense
        after_themes = FALSE   # Mode 1 doesn't auto-generate themes
      ),
      expected_wall_time_min = .config_wall_time_estimate(corpus_size, mode),
      expected_api_spend_usd = .config_api_spend_estimate(corpus_size, mode),
      notes = paste0(
        "Mode 1 (reflexive_scaffold): the researcher authors themes ",
        "(typically in NVivo / ATLAS.ti); pakhom contributes the ",
        "provocateur questioning loop. Use run_mode1() rather than ",
        "run_analysis(). No theme-count prediction applies."
      )
    ))
  }

  # Mode 3 deductive: themes = framework constructs + (optional) anomaly
  # bucket. No clustering. Theme count = #constructs + (0 or 1).
  if (mode == "framework_applied") {
    return(list(
      mode = mode,
      expected_themes = NA_integer_,
      expected_passes = NA_integer_,
      recommended_review_points = list(
        after_coding  = corpus_size >= 100L,
        after_themes  = FALSE  # framework constructs are predetermined
      ),
      expected_wall_time_min = .config_wall_time_estimate(corpus_size, mode),
      expected_api_spend_usd = .config_api_spend_estimate(corpus_size, mode),
      notes = paste0(
        "Mode 3 (framework_applied): theme count equals your framework's ",
        "construct count (+ 1 if anomaly_handling is 'bracket' / 'extend' / ",
        "'revise'). If anomaly_handling = 'extend' or 'revise', the ",
        "anomaly-emergent themes use the v2 clustering algorithm (Phase ",
        "60.2 wiring); their count follows the Mode 2 bracket below ",
        "applied to the anomaly subset."
      )
    ))
  }

  # Mode 2: estimate codebook size, then theme range from v2 empirical evidence.
  # Observed ratios:
  #   - narrow_intersection: ~40-47 codes from 60-67 coded entries (~0.7-0.8 codes/coded)
  #   - broad (emotional triggers): ~157 codes from 140 coded entries (~1.1 codes/coded)
  # Coded entries are typically 25-60% of corpus size (saturation kicks in around the 25% mark
  # for narrow foci, later for broad foci).
  if (is.null(estimated_codebook_size)) {
    coded_ratio <- switch(focus_shape,
      narrow_intersection = 0.25,
      single_focal        = 0.40,
      broad               = 0.60
    )
    codes_per_coded <- switch(focus_shape,
      narrow_intersection = 0.75,
      single_focal        = 0.90,
      broad               = 1.10
    )
    coded_entries <- corpus_size * coded_ratio
    estimated_codebook_size <- as.integer(coded_entries * codes_per_coded)
  } else {
    estimated_codebook_size <- as.integer(estimated_codebook_size[1])
  }

  # Empirical bracket:
  # Themes: low end mean(6,10,7)-1sd, high end mean+1sd; rounded to [4, 12].
  # Passes scale with log2(codebook size): 40 codes -> 1 pass; 157 codes -> 3 passes.
  expected_passes <- if (estimated_codebook_size <= 60L) 1L
                      else if (estimated_codebook_size <= 100L) 2L
                      else 3L

  # Theme range: tighten for narrow foci (more cohesive), widen for broad foci.
  theme_range <- switch(focus_shape,
    narrow_intersection = c(5L, 8L),
    single_focal        = c(6L, 10L),
    broad               = c(6L, 10L)  # the Round 6 result (7 from 157 codes) supports this
  )

  # Researcher review recommendations:
  # - after_coding: enable when codebook will be reviewed before theming
  #   (good for substantive studies; skip for smokes)
  # - after_themes: enable when expected theme count is at upper edge
  #   (more chance of mild overlap pairs that could merge)
  recommend_after_themes <- estimated_codebook_size < 80L &&
                              theme_range[2] >= 9L

  notes_str <- if (estimated_codebook_size < 40L) {
    paste0(
      "Estimated codebook (", estimated_codebook_size, " codes) is below ",
      "the empirically-validated bracket (40-157 codes). ",
      "v2 should still work but the theme range is extrapolated; consider ",
      "running a smoke first."
    )
  } else if (estimated_codebook_size > 200L) {
    paste0(
      "Estimated codebook (", estimated_codebook_size, " codes) is above ",
      "the empirically-validated bracket (tested up to 157 codes). ",
      "v2's single-call-per-pass should scale to ~500 codes within ",
      "OpenAI gpt-4o's context window, but quality at that scale is ",
      "unknown; smoke first."
    )
  } else {
    paste0(
      "Estimated codebook (", estimated_codebook_size, " codes) is within ",
      "the empirically-validated bracket (40-157 codes). ",
      "Expect ", expected_passes, " substantive clustering pass(es) before ",
      "AI-declared convergence."
    )
  }

  list(
    mode = mode,
    expected_themes = theme_range,
    expected_passes = expected_passes,
    recommended_review_points = list(
      after_coding = corpus_size >= 100L,
      after_themes = recommend_after_themes
    ),
    expected_wall_time_min = .config_wall_time_estimate(corpus_size, mode),
    expected_api_spend_usd = .config_api_spend_estimate(corpus_size, mode),
    notes = notes_str
  )
}

#' Estimate wall-time for a run based on corpus size + mode
#' @keywords internal
.config_wall_time_estimate <- function(corpus_size, mode) {
  # Observed timings: 250-entry sample -> 6-12 min for Mode 2.
  # Scaling: dominated by progressive coding (one AI call per entry).
  # Approx: corpus_size * 0.04 min per entry + 1 min fixed overhead.
  if (mode == "reflexive_scaffold") {
    # Mode 1 is provocateur-driven; fewer AI calls than Mode 2
    return(as.integer(max(2L, ceiling(corpus_size * 0.02 + 1))))
  }
  as.integer(max(3L, ceiling(corpus_size * 0.04 + 1)))
}

#' Estimate OpenAI gpt-4o API spend for a run
#' @keywords internal
.config_api_spend_estimate <- function(corpus_size, mode) {
  # Observed costs on 250-entry runs: ~$1-3 per Mode 2 run.
  # Scaling: corpus_size * $0.01 per entry (one coding call + overhead).
  # This is a rough order-of-magnitude; OpenAI billing dashboard is
  # authoritative.
  est <- if (mode == "reflexive_scaffold") {
    corpus_size * 0.005 + 0.5
  } else {
    corpus_size * 0.01 + 0.5
  }
  round(est, 2)
}


.ask_choice <- function(prompt, options) {
  cat(prompt, "\n")
  for (i in seq_along(options)) cat(sprintf("  %d. %s\n", i, options[i]))
  repeat {
    response <- readline(paste0("  Choice [1-", length(options), "]: "))
    n <- suppressWarnings(as.integer(response))
    if (!is.na(n) && n >= 1L && n <= length(options)) return(n)
    cat("  Please enter a number between 1 and ", length(options), ".\n", sep = "")
  }
}

.ask_yes_no <- function(prompt) {
  repeat {
    response <- tolower(trimws(readline(paste0(prompt, " [y/n]: "))))
    if (response %in% c("y", "yes", "true", "t", "1")) return(TRUE)
    if (response %in% c("n", "no", "false", "f", "0")) return(FALSE)
    cat("  Please answer y or n.\n")
  }
}
