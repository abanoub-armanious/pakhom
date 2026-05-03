# ==============================================================================
# Methodology Decision Aid -- T1.3 (Sprint-4 multi-mode architecture)
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
#' Per Sprint-4 design (AC3), there is no default methodology mode in
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
