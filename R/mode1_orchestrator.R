# ==============================================================================
# Mode 1 (Reflexive Scaffold) Orchestrator -- Sprint-4 phase 31
# ==============================================================================
# Closes audit findings C1 + C2 from phase 30 wrap-up: Mode 1's
# run_provocateur_questioning is operational but lacks the AC4 + AC7
# scaffolding Modes 2 + 3 have. This file builds:
#
#   compute_mode1_coverage()         T0.3 for Mode 1 (theme x category
#                                    attempt matrix + skip detection);
#                                    returns ProvocationCoverage S3 with
#                                    Tier0Coverage virtual parent so the
#                                    report renderer dispatches uniformly.
#   ProvocationCoverage S3           Counterpart to CorpusCoverage. Mode 1
#                                    coverage asserts "no silent skip"
#                                    rather than "no silent truncation"
#                                    -- a category attempt that returns
#                                    zero provocations is a valid outcome,
#                                    a category that was never attempted
#                                    is a coverage failure.
#   render_tier0_coverage_card.ProvocationCoverage
#                                    Mode 1 coverage card (HTML/markdown).
#   run_mode1()                      Top-level orchestrator for Mode 1.
#                                    Mirrors run_analysis()'s scaffolding
#                                    (output_dir + run_metadata.json +
#                                    methodology_rules.md + audit_log +
#                                    fabrication_log + finalize_run) but
#                                    routes through the provocateur loop
#                                    instead of progressive coding ->
#                                    sentiment -> themes.
#
# Architectural commitments addressed:
#   AC4 (methodology stamped on every output) -- Mode 1 was previously
#     emitting only a ResearcherReflectionLog with no run_metadata.json,
#     no rendered report, no finalize_run() call. run_mode1() now writes
#     all three so the canonical Mode 1 output is reviewable on the same
#     terms as Modes 2/3.
#   AC7 (universal Tier-0 in all modes) -- Mode 1 was previously not
#     computing T0.2 spread or T0.3 coverage. run_mode1() now computes
#     both: T0.2 over researcher-supplied themes/data via
#     aggregate_theme_statistics(); T0.3 via compute_mode1_coverage().
#   AC8 (modes are configurations of one architecture) -- run_mode1
#     uses the same audit_log, fabrication_log, response_cache primitives
#     as run_analysis. Mode 1's outputs sit in the same outputs/ tree
#     with the same run_metadata.json schema; only the methodology mode
#     and the coverage class differ.
# ==============================================================================

#' ProvocationCoverage schema version
#'
#' 1.0.0 -- initial phase 31 release.
#' 2.0.0 -- phase 31 audit fixes (audit C: C1 + H1 + H2 + H3 + M3):
#'   * \code{explicit_skip_reasons} and \code{attempts_per_category}
#'     stored as named lists (not named integer vectors) so they
#'     serialize faithfully via \code{jsonlite::write_json} (the
#'     coverage_mode1.json artifact is the canonical reviewable
#'     record per AC4).
#'   * Added \code{n_unexpected_category_attempts} and
#'     \code{unexpected_categories} fields to surface attempts whose
#'     category is outside \code{requested_categories} as a
#'     distinct anomaly (previously silently miscounted).
#'   * \code{no_silent_skip} headline now requires
#'     \code{n_themes_input > 0} AND \code{n_themes_attempted > 0}
#'     so degenerate states (zero-themes input, all-themes-explicit-
#'     skipped) don't grade as verified coverage.
#'   * Replaced the unconditional \code{no_silent_corpus_truncation}
#'     boolean (which overclaimed -- the per-category prompts include
#'     only theme-supporting entries, not the full corpus text) with
#'     two honest fields: \code{corpus_provided_to_per_category_fns}
#'     (TRUE -- the data tibble IS passed) and
#'     \code{llm_prompt_includes_full_corpus} (FALSE -- current
#'     prompts only embed supporting-entry text).
#' 2.1.0 -- phase 33 (M1.3 reflexive memos): added \code{n_memos}
#'   and \code{memos_by_type} informational fields. Memo writing is
#'   a researcher activity, not a pipeline gate -- the headline
#'   \code{no_silent_skip} boolean is unchanged. The fields exist so
#'   the methodology paper can report researcher-side burden as a
#'   KPI alongside AI-side coverage.
#' @keywords internal
.PROVOCATION_COVERAGE_SCHEMA_VERSION <- "2.1.0"

# ==============================================================================
# compute_mode1_coverage -- T0.3 for Mode 1
# ==============================================================================

#' Compute Mode 1 (Reflexive Scaffold) coverage from a finished provocateur run
#'
#' Mode 1's analog of \code{\link{compute_corpus_coverage}}. Where Mode 2/3
#' assert "the LLM saw every entry that survived preprocessing" (no silent
#' truncation), Mode 1 asserts:
#'
#' \itemize{
#'   \item every researcher-authored theme was challenged across every
#'     requested provocation category (no silent theme/category skip);
#'   \item the AI was given the FULL corpus when searching for counter-
#'     evidence (no silent corpus truncation -- by construction in
#'     pakhom's prompt builders, which pass the entire corpus tibble to
#'     each per-category provocation function).
#' }
#'
#' Distinguishing legitimate empty results from silent skips matters:
#' counter_narrative or disconfirming_evidence may legitimately return
#' zero provocations when no qualifying entries exist, and that is a
#' valid analytic outcome -- not a coverage failure. The provocation
#' loop's per-attempt tracking (in \code{ResearcherReflectionLog$provocation_attempts},
#' schema 1.1.0+) records one row per (theme, category) attempt
#' regardless of how many provocations the AI emitted, so this function
#' can answer "was the attempt made?" independently from "did the
#' attempt produce output?".
#'
#' @param reflection_log A \code{ResearcherReflectionLog} returned by
#'   \code{\link{run_provocateur_questioning}} (must be schema 1.1.0+ to
#'   carry the \code{provocation_attempts} + \code{skipped_themes}
#'   slots).
#' @param theme_set The \code{ThemeSet} the provocateur loop ran over.
#' @param data The corpus tibble passed to the loop (used for total
#'   entry count -- \code{nrow(data)} -- which the card surfaces as
#'   "corpus searchable for counter-evidence").
#' @param requested_categories Character vector of provocation categories
#'   the orchestrator requested (defaults to the full set of five). Used
#'   to compute the expected attempt-matrix size; supplying a subset
#'   here means the coverage card grades against that subset rather than
#'   all five.
#' @return A \code{ProvocationCoverage} S3 object (also inherits
#'   \code{Tier0Coverage}).
#' @seealso \code{\link{compute_corpus_coverage}} for the Mode 2/3
#'   counterpart; \code{\link{render_tier0_coverage_card}} (the S3
#'   generic that dispatches on the shared \code{Tier0Coverage}
#'   parent class).
#' @examples
#' \dontrun{
#' # After a run_mode1 invocation:
#' result <- run_mode1(data = my_corpus, theme_set = my_themes,
#'                       config_path = "config.yaml")
#'
#' # The coverage object is already on result$coverage; or recompute:
#' cov <- compute_mode1_coverage(
#'   reflection_log      = result$reflection_log,
#'   theme_set           = result$theme_set,
#'   data                = my_corpus,
#'   requested_categories = c("counter_narrative", "disconfirming_evidence")
#' )
#' cov$no_silent_skip       # headline boolean
#' cov$attempts_per_category # named list keyed by category
#' print(cov)
#' }
#' @export
compute_mode1_coverage <- function(reflection_log, theme_set, data,
                                    requested_categories =
                                      .VALID_PROVOCATION_CATEGORIES) {
  if (!inherits(reflection_log, "ResearcherReflectionLog")) {
    stop("reflection_log must be a ResearcherReflectionLog object",
         call. = FALSE)
  }
  if (!inherits(theme_set, "ThemeSet")) {
    stop("theme_set must be a ThemeSet object", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("data must be a data.frame / tibble", call. = FALSE)
  }
  bad <- setdiff(requested_categories, .VALID_PROVOCATION_CATEGORIES)
  if (length(bad) > 0L) {
    stop(sprintf(
      "Unknown requested_categories: %s. Valid: %s",
      paste(bad, collapse = ", "),
      paste(.VALID_PROVOCATION_CATEGORIES, collapse = ", ")
    ), call. = FALSE)
  }

  # Schema-1.0.0 ResearcherReflectionLog won't carry the new tracking
  # slots; treat them as empty so a coverage compute against a legacy
  # log doesn't crash. The card downstream will reflect "0 attempts
  # recorded" which is the truthful claim for a 1.0.0 log.
  attempts <- reflection_log$provocation_attempts
  if (is.null(attempts) || !is.data.frame(attempts)) {
    attempts <- data.frame(
      theme_name   = character(0),
      category     = character(0),
      n_emitted    = integer(0),
      attempted_at = character(0),
      stringsAsFactors = FALSE
    )
  }
  skipped <- reflection_log$skipped_themes
  if (is.null(skipped) || !is.data.frame(skipped)) {
    skipped <- data.frame(
      theme_name = character(0),
      reason     = character(0),
      skipped_at = character(0),
      stringsAsFactors = FALSE
    )
  }

  theme_names_all <- vapply(theme_set$themes, function(t)
                              as.character(t$name)[1L], character(1))
  n_themes_input <- length(theme_names_all)
  n_categories_requested <- length(requested_categories)

  # H1 (audit C, phase 31): partition attempts into in-scope vs
  # out-of-scope WRT requested_categories. An attempt against a category
  # outside the requested subset is itself a coverage anomaly worth
  # surfacing (it could only happen if the orchestrator and the coverage
  # caller disagree about which categories were requested -- which is
  # exactly the kind of silent drift the Tier-0 commitment is meant to
  # surface). Counting them under n_attempts_recorded while filtering
  # them OUT of attempts_per_category produced contradictory recorded >
  # expected values; the audit caught this. Now we partition explicitly
  # and surface the unexpected count as its own field.
  in_scope_mask <- attempts$category %in% requested_categories
  attempts_in_scope <- attempts[in_scope_mask, , drop = FALSE]
  unexpected_attempts <- attempts[!in_scope_mask, , drop = FALSE]

  # Themes that received >= 1 IN-SCOPE attempt
  themes_with_any_attempt <- unique(as.character(
    attempts_in_scope$theme_name
  ))
  n_themes_attempted <- length(intersect(theme_names_all,
                                          themes_with_any_attempt))

  # Themes that were explicitly skipped with a reason (e.g., zero
  # supporting entries). Distinct from "silently skipped" because the
  # reason is recorded.
  themes_explicit_skip <- unique(as.character(skipped$theme_name))
  n_themes_explicit_skip <- length(intersect(theme_names_all,
                                              themes_explicit_skip))

  # Silent-skip detection: themes that exist in theme_set but appear
  # neither in attempts nor in explicit-skip records. If the orchestrator
  # is correct this is always 0; the field exists so the card can
  # surface a deviation immediately rather than have it disappear.
  themes_silent_skip <- setdiff(
    theme_names_all,
    union(themes_with_any_attempt, themes_explicit_skip)
  )
  n_themes_silently_skipped <- length(themes_silent_skip)

  # Attempt-matrix completeness over IN-SCOPE attempts only.
  # Silently-skipped themes are NOT counted as expected attempts because
  # their absence is the silent-skip signal -- counting them here would
  # let attempts_complete pretend they were processed.
  # L2 (audit C, phase 31): clamp at zero. If a future orchestrator bug
  # caused a theme to appear in BOTH attempts and skipped_themes,
  # the subtraction below could go negative; the existing run loop's
  # `next` after the explicit-skip rbind prevents this on the live
  # path, but we defend the invariant explicitly.
  expected_attempts <- max(0L, as.integer(
    (n_themes_input - n_themes_explicit_skip - n_themes_silently_skipped) *
      n_categories_requested
  ))
  n_attempts_recorded <- nrow(attempts_in_scope)
  attempts_complete <- n_attempts_recorded == expected_attempts

  # Per-category attempt counts. Already partitioned to in-scope above,
  # so factor() levels here are exhaustive (no NA bucket).
  if (n_attempts_recorded > 0L) {
    cat_counts <- as.integer(table(factor(
      attempts_in_scope$category,
      levels = requested_categories
    )))
    names(cat_counts) <- requested_categories
  } else {
    cat_counts <- stats::setNames(
      rep(0L, n_categories_requested), requested_categories
    )
  }

  # Emission stats. n_emitted from each in-scope attempt row sums to
  # total provocations the loop emitted in the requested categories.
  # Provocations against unexpected categories (if any) are NOT in this
  # tally -- they're surfaced via n_unexpected_category_attempts.
  n_provocations_emitted <- length(reflection_log$provocations)
  n_attempts_with_zero_emit <- if (n_attempts_recorded > 0L)
    sum(attempts_in_scope$n_emitted == 0L) else 0L
  n_attempts_with_emit <- n_attempts_recorded - n_attempts_with_zero_emit

  # M3 (audit C, phase 31): the per-category prompts (R/provocateur.R
  # provoke_*) instruct the LLM to "search the FULL corpus" but the
  # prompt builders include only the theme's supporting entries (via
  # .build_theme_supporting_entries) -- the rest of the corpus is NOT
  # in the prompt. Asserting "no silent corpus truncation = TRUE"
  # would overclaim. We instead surface the prompt-context shape: the
  # corpus IS available to the per-category functions (passed as the
  # `data` argument), but the LLM only sees a subset. Future phases
  # may add corpus-search via embeddings + k-nearest neighbors; until
  # then, the honest claim is that the LLM cannot search what it
  # cannot see, so any counter-evidence it returns is drawn from
  # training data -- which the verification ladder catches as
  # fabrication unless the model happens to know the entry_id space.
  # Phase 31 keeps the field for future use but explicitly downgrades
  # its semantics in the rendered card; see render method.
  corpus_provided_to_per_category_fns <- TRUE
  llm_prompt_includes_full_corpus     <- FALSE  # current architecture

  # Phase 33 (M1.3): count typed memos. Informational only -- memo
  # writing is a researcher activity, not an AI-pipeline gate, so
  # n_memos does NOT enter the no_silent_skip headline. But the
  # coverage card surfaces the count so the methodology paper KPI
  # ("did this Mode 1 run carry researcher-side reflexive burden?")
  # is computable from the persisted coverage object alone.
  typed_memos <- Filter(function(m) inherits(m, "Memo"),
                          reflection_log$memos %||% list())
  n_memos <- length(typed_memos)
  memos_by_type <- if (n_memos > 0L) {
    mt <- vapply(typed_memos, function(m) m$type, character(1))
    tbl <- table(mt)
    as.list(stats::setNames(as.integer(tbl), names(tbl)))
  } else list()

  # Headline assertions. AC4: a Mode 1 run with zero themes input is
  # a degenerate / mis-configured state, not a verified-coverage state
  # -- gate no_silent_skip on n_themes_input > 0 (mirrors the n_input>0
  # guard in compute_corpus_coverage's no_silent_truncation). H2 + H3
  # (audit C): when n_themes_attempted == 0 the headline is FALSE even
  # though attempts_complete may be vacuously TRUE (zero expected, zero
  # recorded), because nothing was actually challenged.
  no_silent_theme_skip <- n_themes_silently_skipped == 0L
  no_unexpected_category_attempts <- nrow(unexpected_attempts) == 0L
  no_silent_skip <- no_silent_theme_skip &&
                     attempts_complete &&
                     no_unexpected_category_attempts &&
                     n_themes_input > 0L &&
                     n_themes_attempted > 0L

  obj <- list(
    mode                          = "reflexive_scaffold",
    n_themes_input                = as.integer(n_themes_input),
    n_themes_attempted            = as.integer(n_themes_attempted),
    n_themes_explicit_skip        = as.integer(n_themes_explicit_skip),
    # C1 (audit C, phase 31): named integer vectors lose their names
    # when serialized via jsonlite::write_json with auto_unbox=TRUE
    # (the audit caught coverage_mode1.json having anonymous arrays).
    # Store explicit_skip_reasons + attempts_per_category as named
    # lists -- jsonlite serializes named lists as JSON objects with
    # the keys preserved, so coverage_mode1.json round-trips faithfully
    # for replay / methodology-paper KPI tables.
    explicit_skip_reasons         = if (nrow(skipped) > 0L) {
      tbl <- table(skipped$reason)
      as.list(stats::setNames(as.integer(tbl), names(tbl)))
    } else {
      list()
    },
    n_themes_silently_skipped     = as.integer(n_themes_silently_skipped),
    silently_skipped_theme_names  = themes_silent_skip,
    categories_requested          = requested_categories,
    n_categories_requested        = as.integer(n_categories_requested),
    n_attempts_expected           = as.integer(expected_attempts),
    n_attempts_recorded           = as.integer(n_attempts_recorded),
    n_unexpected_category_attempts = as.integer(nrow(unexpected_attempts)),
    unexpected_categories         = unique(as.character(
                                       unexpected_attempts$category)),
    attempts_complete             = attempts_complete,
    attempts_per_category         = as.list(cat_counts),
    n_provocations_emitted        = as.integer(n_provocations_emitted),
    n_attempts_with_zero_emit     = as.integer(n_attempts_with_zero_emit),
    n_attempts_with_emit          = as.integer(n_attempts_with_emit),
    n_corpus_entries_searchable   = as.integer(nrow(data)),
    corpus_provided_to_per_category_fns = corpus_provided_to_per_category_fns,
    llm_prompt_includes_full_corpus = llm_prompt_includes_full_corpus,
    n_memos                       = as.integer(n_memos),
    memos_by_type                 = memos_by_type,
    no_silent_theme_skip          = no_silent_theme_skip,
    no_unexpected_category_attempts = no_unexpected_category_attempts,
    no_silent_skip                = no_silent_skip,
    computed_at                   = format(Sys.time(),
                                            "%Y-%m-%dT%H:%M:%S%z"),
    schema_version                = .PROVOCATION_COVERAGE_SCHEMA_VERSION
  )
  class(obj) <- c("ProvocationCoverage", "Tier0Coverage")
  obj
}

#' Print method for ProvocationCoverage
#' @param x A ProvocationCoverage object
#' @param ... Ignored
#' @export
print.ProvocationCoverage <- function(x, ...) {
  cat("ProvocationCoverage (Mode 1 / T0.3)\n")
  cat(sprintf("  Themes input:                    %d\n",
              x$n_themes_input %||% 0L))
  cat(sprintf("  Themes attempted:                %d\n",
              x$n_themes_attempted %||% 0L))
  if ((x$n_themes_explicit_skip %||% 0L) > 0L) {
    cat(sprintf("  Themes explicitly skipped:       %d\n",
                x$n_themes_explicit_skip))
    if (length(x$explicit_skip_reasons) > 0L) {
      for (r in names(x$explicit_skip_reasons)) {
        cat(sprintf("    %s: %d\n", r, x$explicit_skip_reasons[[r]]))
      }
    }
  }
  if ((x$n_themes_silently_skipped %||% 0L) > 0L) {
    cat(sprintf("  Themes SILENTLY skipped:         %d  <-- INVESTIGATE\n",
                x$n_themes_silently_skipped))
  }
  if ((x$n_unexpected_category_attempts %||% 0L) > 0L) {
    cat(sprintf(
      "  Unexpected-category attempts:    %d  <-- INVESTIGATE (categories: %s)\n",
      x$n_unexpected_category_attempts,
      paste(x$unexpected_categories %||% character(0), collapse = ", ")
    ))
  }
  cat(sprintf("  Categories requested:            %s\n",
              paste(x$categories_requested %||% character(0),
                    collapse = ", ")))
  cat(sprintf("  Attempts expected:               %d\n",
              x$n_attempts_expected %||% 0L))
  cat(sprintf("  Attempts recorded (in-scope):    %d\n",
              x$n_attempts_recorded %||% 0L))
  cat(sprintf("  Attempts complete:               %s\n",
              if (isTRUE(x$attempts_complete)) "TRUE" else "FALSE"))
  cat(sprintf("  Provocations emitted:            %d\n",
              x$n_provocations_emitted %||% 0L))
  cat(sprintf("  Attempts with >=1 emit:          %d\n",
              x$n_attempts_with_emit %||% 0L))
  cat(sprintf("  Attempts with zero emit:         %d  (legitimate outcome)\n",
              x$n_attempts_with_zero_emit %||% 0L))
  cat(sprintf("  Corpus entries searchable:       %s\n",
              format(x$n_corpus_entries_searchable %||% 0L,
                     big.mark = ",")))
  cat(sprintf("  Corpus passed to per-category fns: %s\n",
              if (isTRUE(x$corpus_provided_to_per_category_fns))
                "TRUE" else "FALSE"))
  cat(sprintf("  LLM prompts include full corpus: %s\n",
              if (isTRUE(x$llm_prompt_includes_full_corpus)) "TRUE"
              else "FALSE (only supporting-entry context per theme)"))
  cat(sprintf("  Researcher memos (M1.3):         %d\n",
              x$n_memos %||% 0L))
  if (length(x$memos_by_type %||% list()) > 0L) {
    for (tn in names(x$memos_by_type)) {
      cat(sprintf("    %s: %d\n", tn, x$memos_by_type[[tn]]))
    }
  }
  cat(sprintf("  No silent theme skip:            %s\n",
              if (isTRUE(x$no_silent_theme_skip)) "TRUE (verified)" else "FALSE"))
  cat(sprintf("  No unexpected-category attempts: %s\n",
              if (isTRUE(x$no_unexpected_category_attempts)) "TRUE (verified)"
              else "FALSE"))
  cat(sprintf("  No silent skip (headline):       %s\n",
              if (isTRUE(x$no_silent_skip)) "TRUE (verified)" else "FALSE"))
  invisible(x)
}

# ==============================================================================
# Tier-0 coverage card -- Mode 1 variant
# ==============================================================================

#' @rdname render_tier0_coverage_card
#' @export
render_tier0_coverage_card.ProvocationCoverage <- function(x, ...) {
  coverage <- x
  # NULL-safety + handle the audit-found degenerate states explicitly:
  #   - n_themes_input == 0L (degenerate / mis-configured run)
  #   - n_themes_attempted == 0L AND no silent skip (everything was
  #     explicit-skipped: technically "no silent skip" but the run
  #     produced no challenge -- not a verified-coverage state)
  #   - unexpected-category attempts (orchestrator drift)
  ok_skip <- isTRUE(coverage$no_silent_skip)
  n_in    <- coverage$n_themes_input    %||% 0L
  n_att   <- coverage$n_themes_attempted %||% 0L
  n_unexp <- coverage$n_unexpected_category_attempts %||% 0L
  n_silent <- coverage$n_themes_silently_skipped %||% 0L

  themes_word <- if (n_in == 1L) "theme" else "themes"
  cats_word <- if ((coverage$n_categories_requested %||% 0L) == 1L)
                  "category" else "categories"

  banner_class <- if (ok_skip) "coverage-banner-ok"
                  else "coverage-banner-warn"
  banner_msg <- if (ok_skip) {
    paste0(
      "All ", n_in, " researcher-authored ", themes_word,
      " were challenged across all ",
      coverage$n_categories_requested, " requested provocation ",
      cats_word, ". No silent skip detected. ",
      "(Per-category prompts received the supporting-entry text for ",
      "each theme; the full ", format(coverage$n_corpus_entries_searchable
                                        %||% 0L, big.mark = ","),
      "-entry corpus was available to the per-category functions but ",
      "was not embedded in the LLM prompts -- see the ",
      "prompt-context note below.)"
    )
  } else if (n_in == 0L) {
    paste0(
      "No themes were provided to run_mode1(). Mode 1 requires a ",
      "researcher-authored ThemeSet to challenge -- a zero-themes ",
      "input is a configuration error, not a verified-coverage state."
    )
  } else if (n_att == 0L) {
    paste0(
      "All ", n_in, " ", themes_word, " were skipped before any ",
      "provocation category was attempted (e.g., zero supporting ",
      "entries for every theme). The run produced no challenge of ",
      "the analytic frame; treat it as a configuration / data-",
      "alignment problem, not a coverage-verified result."
    )
  } else if (n_silent > 0L) {
    paste0(
      n_silent, " ", themes_word, " were SILENTLY skipped (no ",
      "attempt rows AND no explicit-skip records). This indicates ",
      "an orchestrator failure; do not treat the provocations as a ",
      "complete challenge of the analytic frame."
    )
  } else if (n_unexp > 0L) {
    paste0(
      n_unexp, " attempt(s) were recorded against categor",
      if (n_unexp == 1L) "y" else "ies",
      " outside the requested set (",
      paste(coverage$unexpected_categories %||% character(0),
            collapse = ", "),
      "). Orchestrator/coverage caller disagreement; investigate."
    )
  } else {
    paste0(
      "Attempt matrix is incomplete (recorded ",
      coverage$n_attempts_recorded %||% 0L,
      " of expected ", coverage$n_attempts_expected %||% 0L,
      "). Coverage is not assertable; investigate before treating ",
      "provocations as a complete challenge of the analytic frame."
    )
  }

  # Theme breakdown rows: input, attempted, explicit-skip (with reason
  # rollup), silent-skip
  theme_rows <- character(0)
  theme_rows <- c(theme_rows, sprintf(
    '<tr><td>Researcher-authored themes (input)</td><td>%d</td><td>%s</td></tr>',
    coverage$n_themes_input,
    "Themes the orchestrator received"
  ))
  theme_rows <- c(theme_rows, sprintf(
    '<tr class="coverage-row-attempted"><td>Themes attempted</td><td>%d</td><td>%s</td></tr>',
    coverage$n_themes_attempted,
    "Received at least one provocation-category attempt"
  ))
  if (coverage$n_themes_explicit_skip > 0L) {
    reasons_str <- if (length(coverage$explicit_skip_reasons) > 0L) {
      parts <- vapply(seq_along(coverage$explicit_skip_reasons), function(i) {
        sprintf("%s (%d)",
                names(coverage$explicit_skip_reasons)[i],
                coverage$explicit_skip_reasons[[i]])
      }, character(1))
      paste(parts, collapse = "; ")
    } else "reason recorded"
    theme_rows <- c(theme_rows, sprintf(
      '<tr><td>Themes explicitly skipped</td><td>%d</td><td>%s</td></tr>',
      coverage$n_themes_explicit_skip,
      .html_esc(reasons_str)
    ))
  }
  if (n_silent > 0L) {
    silent_names <- coverage$silently_skipped_theme_names %||% character(0)
    names_note <- if (length(silent_names) > 0L)
      sprintf("Names: %s",
              .html_esc(paste(silent_names, collapse = ", ")))
    else "(no names recorded -- malformed coverage object)"
    theme_rows <- c(theme_rows, sprintf(
      '<tr class="coverage-row-warn"><td>Themes SILENTLY skipped</td><td>%d</td><td>%s</td></tr>',
      n_silent, names_note
    ))
  }
  if (n_unexp > 0L) {
    unexp_names <- coverage$unexpected_categories %||% character(0)
    theme_rows <- c(theme_rows, sprintf(
      '<tr class="coverage-row-warn"><td>Unexpected-category attempts</td><td>%d</td><td>%s</td></tr>',
      n_unexp,
      .html_esc(sprintf("Outside requested set: %s",
                        paste(unexp_names, collapse = ", ")))
    ))
  }

  # Per-category attempt counts -- shows whether each requested category
  # was actually attempted across themes. attempts_per_category is now
  # a named list (audit C C1 fix) for JSON-faithful serialization;
  # iteration via seq_along + names() is unchanged.
  apc <- coverage$attempts_per_category %||% list()
  cat_rows <- if (length(apc) > 0L) {
    vapply(seq_along(apc), function(i) {
      cn <- names(apc)[i]
      n  <- as.integer(apc[[i]] %||% 0L)
      sprintf(
        '<tr><td>%s</td><td>%d</td><td>%s</td></tr>',
        .html_esc(cn), n,
        if (n > 0L) "Category attempted on at least one theme"
        else "Category was never attempted (silent category skip)"
      )
    }, character(1))
  } else {
    sprintf('<tr><td colspan="3">No requested categories.</td></tr>')
  }

  # Emission stats footer
  emit_summary <- sprintf(
    paste0(
      "Across %d in-scope attempt(s), %d emitted >=1 provocation and ",
      "%d emitted zero (a legitimate outcome -- e.g., counter_narrative ",
      "finding no qualifying entries -- distinct from a silent skip). ",
      "Total verified provocations issued: %d."
    ),
    coverage$n_attempts_recorded %||% 0L,
    coverage$n_attempts_with_emit %||% 0L,
    coverage$n_attempts_with_zero_emit %||% 0L,
    coverage$n_provocations_emitted %||% 0L
  )

  # Prompt-context note: addresses M3 from the audit. Replaces the
  # previous overclaim ("no silent corpus truncation = TRUE") with an
  # honest description of what the LLM actually sees in current
  # architecture.
  prompt_context_note <- sprintf(
    paste0(
      'Prompt context: per-category prompts include the supporting-',
      'entry text for each theme (data argument: %s entries available; ',
      'prompt embeds only the per-theme subset). The LLM is instructed ',
      'to "search the FULL corpus" but only sees the supporting-entry ',
      'context; counter-evidence the model returns is verified against ',
      'the corpus by the four-step verification ladder (T0.1) -- ',
      'fabricated entry_ids fail .citation_to_provocation lookup and ',
      'are dropped. A future phase will add corpus-search retrieval ',
      'so the LLM can reason against the whole corpus directly.'
    ),
    format(coverage$n_corpus_entries_searchable %||% 0L, big.mark = ",")
  )

  paste0(
    '<div class="coverage-card">\n',
    '<div class="coverage-header">Provocation Coverage (Mode 1 / T0.3)</div>\n',
    '<div class="coverage-banner ', banner_class, '">', banner_msg, '</div>\n',
    '<div class="coverage-funnel-wrapper">\n',
    '<table class="coverage-funnel">\n',
    '<thead><tr><th>Theme stage</th><th>Count</th><th>Note</th></tr></thead>\n',
    '<tbody>\n', paste(theme_rows, collapse = "\n"), '\n</tbody>\n',
    '</table>\n',
    '</div>\n',
    '<div class="coverage-funnel-wrapper">\n',
    '<table class="coverage-funnel">\n',
    '<thead><tr><th>Category</th><th>Themes attempted</th><th>Note</th></tr></thead>\n',
    '<tbody>\n', paste(cat_rows, collapse = "\n"), '\n</tbody>\n',
    '</table>\n',
    '</div>\n',
    '<div class="coverage-volume">', emit_summary, '</div>\n',
    '<div class="coverage-prompt-context"><em>', prompt_context_note,
    '</em></div>\n',
    '<p class="coverage-citation">Mode 1 (Reflexive Scaffold) ',
    'coverage analog of Jowsey et al. 2025 ',
    '(doi:10.1371/journal.pone.0330217). Where the Mode 2/3 coverage ',
    'card asserts "no silent truncation in the LLM call path", Mode 1 ',
    'asserts "no silent skip across themes x provocation categories" -- ',
    'the corresponding transparency claim for Sarkar 2024\'s "AI as ',
    'Socratic gadfly" pattern, in which the AI\'s contribution is ',
    'extractive provocations rather than coding decisions.</p>\n',
    '</div>\n\n'
  )
}

# ==============================================================================
# Per-theme stats for Mode 1 (T0.2 spread + provocation rollups)
# ==============================================================================

#' Compute per-theme statistics for a Mode 1 run
#'
#' Mode 1's analog of \code{aggregate_theme_statistics} for Mode 2/3.
#' Mode 1 has no sentiment / emotions / intensity (the AI didn't run
#' coding or sentiment over the corpus -- the researcher did), so this
#' helper returns only what is meaningful in Mode 1: per-theme entry
#' count, T0.2 participant spread, and provocation rollups (count by
#' category + total + drop count).
#'
#' @param data Tibble with std_id, std_text, plus theme_membership_*
#'   columns or an emerged_themes column. Must carry std_author when
#'   T0.2 spread is desired.
#' @param theme_set Researcher-authored ThemeSet.
#' @param reflection_log Populated ResearcherReflectionLog (post
#'   provocateur loop).
#' @param quotes_per_theme Integer; number of representative quotes to
#'   select per theme. Wired through from
#'   \code{config$analysis$themes$quotes_per_theme}; defaults to 3.
#' @param config Optional ThematicConfig (or config list). When supplied
#'   (Phase 55), \code{config$data$column_mappings$metric_columns} is
#'   used to detect dataset-specific metric columns for the per-theme
#'   Median(MAD) + Mean(SD) summary line in the Mode 1 report. When NULL
#'   or empty, metrics auto-detect from the data via
#'   \code{\link{.detect_metric_columns}}.
#' @return Named list keyed by theme name. Each value carries
#'   \code{n_entries}, \code{participant_spread}, \code{provocations}
#'   (count by category + total), \code{quotes} (raw representative
#'   quotes -- NOT sentiment-sorted because Mode 1 has no sentiment),
#'   plus Phase 55 fields: \code{metric_cols} (character vector) and
#'   \code{metric_stats} (named list of per-metric Median/MAD/Mean/SD/
#'   n_observed records).
#' @export
compute_mode1_theme_stats <- function(data, theme_set, reflection_log,
                                        quotes_per_theme = 3L,
                                        config = NULL) {
  if (!inherits(theme_set, "ThemeSet")) {
    stop("theme_set must be a ThemeSet object", call. = FALSE)
  }
  if (!inherits(reflection_log, "ResearcherReflectionLog")) {
    stop("reflection_log must be a ResearcherReflectionLog object",
         call. = FALSE)
  }

  # Phase 55: dataset-agnostic metric column detection (Mode 1 light
  # touch). Mode 1 themes are researcher-supplied + flat (no AI sub-
  # themes), so we compute the same per-metric Median(MAD) + Mean(SD)
  # at the THEME level (not per-subtheme).
  metric_cols <- .detect_metric_columns(data, config)

  # Build a fast lookup: provocations by theme name x category
  provs_by_theme <- list()
  for (p in reflection_log$provocations) {
    tn <- as.character(p$theme_name)[1L]
    if (is.null(provs_by_theme[[tn]])) {
      provs_by_theme[[tn]] <- list(
        by_category = stats::setNames(
          rep(0L, length(.VALID_PROVOCATION_CATEGORIES)),
          .VALID_PROVOCATION_CATEGORIES
        ),
        total = 0L,
        items = list()
      )
    }
    cat_v <- as.character(p$category)[1L]
    if (cat_v %in% names(provs_by_theme[[tn]]$by_category)) {
      provs_by_theme[[tn]]$by_category[[cat_v]] <-
        provs_by_theme[[tn]]$by_category[[cat_v]] + 1L
    }
    provs_by_theme[[tn]]$total <- provs_by_theme[[tn]]$total + 1L
    provs_by_theme[[tn]]$items[[length(provs_by_theme[[tn]]$items) + 1L]] <- p
  }

  out <- list()
  for (t in theme_set$themes) {
    tn <- as.character(t$name)[1L]
    safe_col <- paste0("theme_membership_", make.names(tn))
    if (safe_col %in% names(data)) {
      entries <- data[data[[safe_col]] == 1L, , drop = FALSE]
    } else if ("emerged_themes" %in% names(data)) {
      entries <- data[!is.na(data$emerged_themes) &
                       grepl(tn, data$emerged_themes, fixed = TRUE), ,
                       drop = FALSE]
    } else {
      entries <- data[0, , drop = FALSE]
    }

    spread <- .compute_participant_spread(entries)

    # Raw representative quotes -- first-N excerpts from the supporting
    # entries. Mode 1 has no sentiment to sort by, so we use entry order.
    n_quotes_to_show <- min(quotes_per_theme, nrow(entries))
    text_col <- if ("std_text" %in% names(entries)) "std_text"
                else if ("original_text" %in% names(entries)) "original_text"
                else NA_character_
    quote_items <- if (n_quotes_to_show > 0L && !is.na(text_col)) {
      lapply(seq_len(n_quotes_to_show), function(i) list(
        std_id = as.character(entries$std_id[i]),
        text   = substr(as.character(entries[[text_col]][i]), 1L, 400L)
      ))
    } else list()

    provs_meta <- provs_by_theme[[tn]] %||% list(
      by_category = stats::setNames(
        rep(0L, length(.VALID_PROVOCATION_CATEGORIES)),
        .VALID_PROVOCATION_CATEGORIES
      ),
      total = 0L,
      items = list()
    )

    # Phase 55: per-metric Median(MAD) + Mean(SD) for each auto-detected
    # metric column. Mode 1 has no subthemes (researcher-supplied flat
    # themes), so these stats live at the THEME level.
    metric_stats <- list()
    for (mc in metric_cols) {
      if (!mc %in% names(entries)) next
      vals <- suppressWarnings(as.numeric(entries[[mc]]))
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0L) {
        metric_stats[[mc]] <- list(median = NA_real_, mad = NA_real_,
                                     mean = NA_real_, sd = NA_real_,
                                     n_observed = 0L)
        next
      }
      metric_stats[[mc]] <- list(
        median     = round(stats::median(vals), 2),
        mad        = round(stats::mad(vals), 2),
        mean       = round(mean(vals), 2),
        sd         = round(stats::sd(vals), 2),
        n_observed = length(vals)
      )
    }

    out[[tn]] <- list(
      name              = tn,
      description       = t$description %||% "",
      n_entries         = nrow(entries),
      participant_spread = spread,
      provocations      = list(
        by_category = provs_meta$by_category,
        total       = provs_meta$total,
        items       = provs_meta$items
      ),
      quotes            = quote_items,
      keywords          = t$keywords %||% character(0),
      metric_cols       = metric_cols,
      metric_stats      = metric_stats
    )
  }
  out
}

# ==============================================================================
# Mode-1 specific verify_run_integrity helper (called from R/17_report.R)
# ==============================================================================

#' Mode 1 expected-files helper for verify_run_integrity
#'
#' Mode 1 produces a different artifact set from Modes 2/3 (no sentiment,
#' no correlations, no theme_entries CSVs). Universal Tier-0 + Tier-1
#' artifacts are still required (run_metadata, methodology rules,
#' fabrication log, audit log). Mode 1-specific outputs: reflection_log
#' JSON + provocations CSV + provocation_attempts CSV + coverage JSON.
#' @keywords internal
.verify_run_integrity_mode1 <- function(run_dir, config = list()) {
  expected <- c(
    # Universal Tier-0 + Tier-1 -- mandatory in all modes per AC4 + AC7
    "run_metadata.json",
    "rules/methodology_rules.md",
    "fabrication_log.csv",
    "ai_decisions.jsonl",
    # Mode 1 canonical artifacts
    "reflection_log.json",
    "provocations.csv",
    "provocation_attempts.csv",
    "themes.json",
    "coverage_mode1.json"
  )
  if (isTRUE(config$output$generate_report)) {
    expected <- c(expected,
                   "analysis_report.html",
                   "analysis_report.Rmd",
                   "styles.css")
  }
  if (isTRUE(config$audit$capture_raw_responses %||% TRUE)) {
    expected <- c(expected, config$audit$response_cache_dir %||% "api_responses")
  }

  present <- expected[file.exists(file.path(run_dir, expected))]
  missing <- setdiff(expected, present)

  # Phase 33 (M1.3): the memos/ directory is conditionally expected --
  # when at least one .md file lives under memos/, the directory MUST
  # be present (it's the canonical persistence layer for researcher
  # memos per AC4). Empty memo state is a valid Mode 1 outcome (a
  # researcher might have used the run only for AI provocations
  # without authoring memos), so memos/ is NOT expected unconditionally.
  # We surface the count separately so the report's integrity card
  # shows it.
  memos_dir <- file.path(run_dir, "memos")
  memo_files <- if (dir.exists(memos_dir))
                  list.files(memos_dir, pattern = "\\.md$",
                              full.names = FALSE)
                else character(0)
  n_memos_persisted <- length(memo_files)

  list(
    expected = expected,
    present  = present,
    missing  = missing,
    complete = length(missing) == 0L,
    n_memos_persisted = n_memos_persisted
  )
}

# ==============================================================================
# Mode 1 export helpers
# ==============================================================================

#' Convert a Provocation S3 object to a flat row for CSV export
#' @keywords internal
.provocation_to_row <- function(p) {
  prov_id <- if (!is.null(p$provenance) &&
                  inherits(p$provenance, "QuoteProvenance")) p$provenance
              else NULL
  data.frame(
    category               = as.character(p$category)[1L],
    theme_name             = as.character(p$theme_name)[1L],
    reason                 = as.character(p$reason)[1L],
    cited_entry_id         = if (!is.null(prov_id)) prov_id$source_doc_id
                              else NA_character_,
    cited_char_start       = if (!is.null(prov_id)) as.integer(prov_id$start_char)
                              else NA_integer_,
    cited_char_end         = if (!is.null(prov_id)) as.integer(prov_id$end_char)
                              else NA_integer_,
    cited_exact_text       = if (!is.null(prov_id)) prov_id$exact_text
                              else NA_character_,
    verification_status    = if (!is.null(prov_id)) prov_id$verification_status
                              else NA_character_,
    extra_json             = tryCatch(
      jsonlite::toJSON(p$extra %||% list(), auto_unbox = TRUE),
      error = function(e) NA_character_
    ),
    ai_model               = as.character(p$ai_model %||% NA_character_),
    ai_call_id             = as.character(p$ai_call_id %||% NA_character_),
    prompted_at            = as.character(p$prompted_at %||% NA_character_),
    researcher_action      = as.character(p$researcher_action %||% NA_character_),
    stringsAsFactors       = FALSE
  )
}

#' Write Mode 1 artifacts: reflection_log.json + provocations.csv + attempts.csv + skipped.csv
#' @keywords internal
.write_mode1_artifacts <- function(reflection_log, output_dir,
                                     methodology_mode = "reflexive_scaffold") {
  # reflection_log.json -- the canonical Mode 1 record. Stamped per AC4.
  rl_path <- file.path(output_dir, "reflection_log.json")
  tryCatch({
    # Provocations carry QuoteProvenance objects which jsonlite handles
    # via their list shape; we write the whole reflection_log directly.
    jsonlite::write_json(reflection_log, rl_path,
                          pretty = TRUE, auto_unbox = TRUE,
                          force = TRUE)
    stamp_methodology_json(rl_path, methodology_mode,
                            run_id = basename(output_dir))
  }, error = function(e) log_warn("Could not write reflection_log.json: {e$message}"))

  # provocations.csv -- flat exportable provocation list
  provs_path <- file.path(output_dir, "provocations.csv")
  rows <- if (length(reflection_log$provocations) > 0L) {
    do.call(rbind, lapply(reflection_log$provocations, .provocation_to_row))
  } else {
    data.frame(
      category = character(0), theme_name = character(0),
      reason = character(0), cited_entry_id = character(0),
      cited_char_start = integer(0), cited_char_end = integer(0),
      cited_exact_text = character(0), verification_status = character(0),
      extra_json = character(0), ai_model = character(0),
      ai_call_id = character(0), prompted_at = character(0),
      researcher_action = character(0),
      stringsAsFactors = FALSE
    )
  }
  tryCatch({
    readr::write_csv(rows, provs_path)
    stamp_methodology_csv(provs_path, methodology_mode,
                            run_id = basename(output_dir))
  }, error = function(e) log_warn("Could not write provocations.csv: {e$message}"))

  # provocation_attempts.csv -- the attempt-tracking matrix that drives
  # T0.3 in Mode 1
  att_path <- file.path(output_dir, "provocation_attempts.csv")
  tryCatch({
    readr::write_csv(reflection_log$provocation_attempts %||%
                       data.frame(theme_name = character(0),
                                   category = character(0),
                                   n_emitted = integer(0),
                                   attempted_at = character(0)),
                      att_path)
    stamp_methodology_csv(att_path, methodology_mode,
                            run_id = basename(output_dir))
  }, error = function(e) log_warn("Could not write provocation_attempts.csv: {e$message}"))

  # skipped_themes.csv -- only when there are skips; the file's absence
  # in that case reflects "no themes were skipped" rather than "the run
  # forgot to record skip data."
  if (!is.null(reflection_log$skipped_themes) &&
      nrow(reflection_log$skipped_themes) > 0L) {
    skip_path <- file.path(output_dir, "skipped_themes.csv")
    tryCatch({
      readr::write_csv(reflection_log$skipped_themes, skip_path)
      stamp_methodology_csv(skip_path, methodology_mode,
                              run_id = basename(output_dir))
    }, error = function(e) log_warn("Could not write skipped_themes.csv: {e$message}"))
  }

  invisible(list(
    reflection_log    = rl_path,
    provocations      = provs_path,
    attempts          = att_path
  ))
}

#' Write themes.json for the researcher-authored frame
#' @keywords internal
.write_mode1_themes_json <- function(theme_set, output_dir,
                                       methodology_mode = "reflexive_scaffold") {
  themes_file <- file.path(output_dir, "themes.json")
  tryCatch({
    themes_json <- theme_set_to_tibble(theme_set)
    jsonlite::write_json(themes_json, themes_file,
                          pretty = TRUE, auto_unbox = TRUE)
    stamp_methodology_json(themes_file, methodology_mode,
                            run_id = basename(output_dir))
  }, error = function(e) log_warn("Could not write themes.json: {e$message}"))
  themes_file
}

#' Write coverage_mode1.json (the ProvocationCoverage object)
#' @keywords internal
.write_mode1_coverage_json <- function(coverage, output_dir,
                                         methodology_mode = "reflexive_scaffold") {
  cov_file <- file.path(output_dir, "coverage_mode1.json")
  tryCatch({
    jsonlite::write_json(coverage, cov_file, pretty = TRUE,
                          auto_unbox = TRUE, force = TRUE)
    stamp_methodology_json(cov_file, methodology_mode,
                            run_id = basename(output_dir))
  }, error = function(e) log_warn("Could not write coverage_mode1.json: {e$message}"))
  cov_file
}

# ==============================================================================
# run_mode1() -- top-level Mode 1 orchestrator
# ==============================================================================

#' Run a Mode 1 (Reflexive Scaffold) provocateur analysis
#'
#' Top-level Mode 1 entry point. Where \code{\link{run_analysis}} runs the
#' Mode 2/3 inductive-/framework-coding pipeline, \code{run_mode1}
#' orchestrates the provocateur loop with the same scaffolding (output
#' directory + run_metadata.json + methodology rules + audit log +
#' fabrication log + finalize_run + report) so a Mode 1 run produces a
#' canonical reviewable artifact set under \code{outputs/<run-id>-rs/}.
#'
#' Mode 1's architectural commitment (Sarkar 2024 / patterns doc):
#' the AI does NOT author themes or codes -- the researcher does, in
#' their own external workflow (NVivo, ATLAS.ti, MAXQDA, etc.). pakhom's
#' contribution is the extractive provocateur loop: counter-narrative,
#' absent voice, alternative interpretation, disconfirming evidence, and
#' assumption surfacing. This function takes the researcher's finished
#' theme set as input and surfaces the AI's challenges to it as
#' verifiable, citation-anchored provocations.
#'
#' @param data Tibble: standardized + preprocessed corpus. Must carry
#'   \code{std_id} + \code{std_text}; should also carry \code{std_author}
#'   for T0.2 participant spread, and either \code{theme_membership_*}
#'   columns or an \code{emerged_themes} column to indicate which entries
#'   support each theme.
#' @param theme_set ThemeSet with researcher-authored themes. The
#'   provocateur loop runs once per theme.
#' @param config_path Path to a YAML config file that declares
#'   \code{methodology.mode = "reflexive_scaffold"}. Mutually exclusive
#'   with \code{config}.
#' @param config A pre-loaded \code{ThematicConfig}. Mutually exclusive
#'   with \code{config_path}.
#' @param categories Character vector of provocation categories to run
#'   (defaults to all five). Restricting this here also restricts the
#'   T0.3 coverage assertion to the supplied subset.
#' @param resume Logical; if TRUE, look for a prior Mode 1 run dir and
#'   resume the provocateur loop from its reflection_log.json. Memos
#'   are also rehydrated from \code{outputs/<run>/memos/<id>.md} (the
#'   canonical persistence layer per AC4).
#' @param config_overrides Named list of dot-path config overrides.
#' @return Invisibly: a list with \code{output_dir}, \code{reflection_log},
#'   \code{theme_set}, \code{coverage}, \code{theme_stats}, \code{config},
#'   \code{integrity}, \code{artifact_paths}.
#' @seealso \code{\link{run_analysis}} (Mode 2/3 entry point);
#'   \code{\link{run_provocateur_questioning}} (the bare provocateur
#'   loop without scaffolding); \code{\link{add_memo}} (Mode 1
#'   reflexive memo CRUD); \code{\link{compute_mode1_coverage}}
#'   (T0.3 coverage compute);
#'   \code{vignette("methodology-modes")} (per-mode worked examples).
#' @examples
#' \dontrun{
#' # 1. Author your themes elsewhere (e.g., NVivo) and load them.
#' #    pakhom never writes themes in Mode 1.
#' my_themes <- create_theme_set(list(
#'   list(id = 1, name = "Adherence",
#'        description = "Researcher-authored: medication adherence",
#'        codes_included = c("med_routine", "daily_pills"))
#' ))
#'
#' # 2. Run the provocateur loop with full Tier-0/Tier-1 scaffolding
#' result <- run_mode1(
#'   data        = my_corpus,        # tibble with std_id + std_text
#'   theme_set   = my_themes,
#'   config_path = "config.yaml"     # methodology.mode = "reflexive_scaffold"
#' )
#'
#' # 3. Add reflexive memos (Mode 1's AC6 burden parity vs Modes 2/3)
#' result$reflection_log <- add_memo(
#'   result$reflection_log,
#'   body = "The 'Adherence' theme rests heavily on contributors 1-3.",
#'   type = "theoretical",
#'   linked_themes = "Adherence"
#' )
#' persist_memos(result$reflection_log, result$output_dir)
#' }
#' @export
run_mode1 <- function(data, theme_set,
                       config_path = NULL, config = NULL,
                       categories = .VALID_PROVOCATION_CATEGORIES,
                       resume = FALSE,
                       config_overrides = list()) {

  # ---- Input validation ----------------------------------------------------
  if (is.null(config_path) && is.null(config)) {
    stop("run_mode1: must supply either config_path or config", call. = FALSE)
  }
  if (!is.null(config_path) && !is.null(config)) {
    stop("run_mode1: config_path and config are mutually exclusive",
         call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("run_mode1: data must be a data.frame / tibble", call. = FALSE)
  }
  if (!"std_id" %in% names(data) || !"std_text" %in% names(data)) {
    stop("run_mode1: data must have std_id and std_text columns",
         call. = FALSE)
  }
  if (!inherits(theme_set, "ThemeSet")) {
    stop("run_mode1: theme_set must be a ThemeSet object (use ",
         "create_theme_set() to build one from researcher-authored themes)",
         call. = FALSE)
  }
  bad <- setdiff(categories, .VALID_PROVOCATION_CATEGORIES)
  if (length(bad) > 0L) {
    stop(sprintf(
      "run_mode1: unknown categories: %s. Valid: %s",
      paste(bad, collapse = ", "),
      paste(.VALID_PROVOCATION_CATEGORIES, collapse = ", ")
    ), call. = FALSE)
  }

  # ---- Locale + logging setup ---------------------------------------------
  old_locale <- Sys.getlocale("LC_CTYPE")
  if (!grepl("UTF-8|utf8", old_locale, ignore.case = TRUE)) {
    tryCatch({
      Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
      log_info("run_mode1: set locale to UTF-8 for proper Unicode handling")
    }, warning = function(w) log_warn("Could not set UTF-8 locale: {w$message}"),
    error = function(e) log_warn("Could not set UTF-8 locale: {e$message}"))
    on.exit(tryCatch(Sys.setlocale("LC_CTYPE", old_locale),
                       error = function(e) NULL), add = TRUE)
  }

  # ---- Load + validate config ---------------------------------------------
  if (is.null(config)) {
    config <- load_config(config_path, overrides = config_overrides)
  }
  meth_mode <- .config_methodology_mode(config)
  if (!identical(meth_mode, "reflexive_scaffold")) {
    # Audit A H1 (phase 31): use a single multi-line message for parity
    # with run_analysis()'s Mode 1 refusal -- a reviewer hitting either
    # error should get the same shape of guidance.
    stop(paste0(
      "run_mode1: config declares methodology.mode = '",
      meth_mode %||% "<unset>",
      "', but run_mode1() is exclusively for Mode 1 (reflexive_scaffold).\n\n",
      "If you intended a different mode:\n",
      "  Mode 2 (codebook_collaborative): use run_analysis(config_path)\n",
      "  Mode 3 (framework_applied):       use run_analysis(config_path) ",
      "with framework_spec_path set\n\n",
      "If you intended Mode 1, set methodology.mode = ",
      "'reflexive_scaffold' in your config.yaml."
    ), call. = FALSE)
  }

  log_level_str <- toupper(config$logging$log_level %||% "INFO")
  log_level_map <- list(DEBUG = logger::DEBUG, INFO = logger::INFO,
                          WARN = logger::WARN, ERROR = logger::ERROR)
  if (!is.null(log_level_map[[log_level_str]])) {
    logger::log_threshold(log_level_map[[log_level_str]])
  }

  pkg_version <- tryCatch(as.character(utils::packageVersion("pakhom")),
                            error = function(e) "dev")
  log_info("========================================")
  log_info("STARTING pakhom Mode 1 (Reflexive Scaffold) v{pkg_version}")
  log_info("Study: {config$study$name}")
  log_info("Themes (researcher-authored): {n_themes(theme_set)}")
  log_info("Provocation categories: {paste(categories, collapse=', ')}")
  log_info("========================================")
  total_time <- tic("Total Mode 1 analysis")

  # ---- Output directory + run state ---------------------------------------
  results_base <- config$output$results_dir
  dir.create(results_base, recursive = TRUE, showWarnings = FALSE)

  # Mode 1 resume looks for the most recent run dir; only resumes if it
  # was a Mode 1 run.
  output_dir <- NULL
  if (isTRUE(resume)) {
    latest <- find_latest_run(results_base)
    if (!is.null(latest)) {
      candidate <- file.path(results_base, latest)
      cand_meta <- read_run_metadata(candidate)
      if (!is.null(cand_meta) &&
          identical(cand_meta$methodology_mode, "reflexive_scaffold")) {
        if (is_run_finalized(candidate)) {
          # Audit A H1 (sprintf parity): single multi-line message
          stop(paste0(
            "run_mode1 resume: latest run ", candidate, " is FINALIZED. ",
            "Per AC5 (soft-lock with audit trail), a finalized run cannot ",
            "be resumed in place -- doing so would overwrite the canonical ",
            "reflection_log.json without a fork record. Pass resume=FALSE ",
            "for a fresh run, or use clone_run_with_new_mode() to fork."
          ), call. = FALSE)
        }
        output_dir <- candidate
        log_info("Resuming Mode 1 run: {basename(output_dir)}")
      } else {
        log_info("Latest run is not Mode 1 ('{cand_meta$methodology_mode %||% \"<no metadata>\"}'); starting fresh.")
      }
    } else {
      # Audit A H2 (phase 31): parity with run_analysis()'s "no previous
      # run found" log so a resume=TRUE invocation against an empty
      # results_dir doesn't fall through silently.
      log_info("No previous run found in {results_base}; starting fresh.")
    }
  }
  if (is.null(output_dir)) {
    output_dir <- file.path(results_base,
                              run_id_with_mode(generate_run_id(), meth_mode))
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # T1.7 (AC4): console banner with methodology stamp
  log_info(stamp_methodology_console(meth_mode, basename(output_dir)))

  # T1.6: methodology rules generation + injection. The provider also
  # needs the rules for ai_complete() to inject them per-call (per AC9
  # -- rules in the model's context every turn).
  tryCatch(write_methodology_rules(config, output_dir),
           error = function(e) log_warn("Could not write methodology rules: {e$message}"))

  # T1.5 mismatch check (relevant on resume path)
  status <- methodology_mismatch_status(output_dir, config)
  if (identical(status, "mismatch_finalized")) {
    prior <- read_run_metadata(output_dir)
    # Audit A H1 (sprintf parity): single multi-line message
    stop(paste0(
      "run_dir ", output_dir, " is FINALIZED with methodology '",
      prior$methodology_mode, "' but config declares '",
      config$methodology$mode, "'. Per AC5 (soft-lock with audit trail), ",
      "methodology cannot be silently re-declared on a finalized run. ",
      "Use clone_run_with_new_mode() to fork into a new run dir."
    ), call. = FALSE)
  } else if (identical(status, "mismatch_active")) {
    prior <- read_run_metadata(output_dir)
    log_warn("Methodology mismatch on active run: stored '{prior$methodology_mode}' vs config '{config$methodology$mode}'. Overwriting metadata.")
  }

  # ---- AI provider (hoisted above init_run_state for L4 parity) -----------
  # Audit A L4 (phase 31): create_ai_provider was previously called AFTER
  # init_run_state, which left the run_metadata.json without
  # model_primary / model_fast fields that Mode 2/3 carry. Hoisting the
  # call up here lets us pass the model fields into init_run_state for
  # cross-mode comparability of run_metadata.json.
  provider <- create_ai_provider(config$ai$provider, config)

  # T1.5: init run state -- now with model_primary / model_fast for
  # cross-mode parity per audit A L4.
  meta <- init_run_state(
    run_dir          = output_dir,
    run_id           = basename(output_dir),
    methodology_mode = config$methodology$mode,
    parent_run_id    = config$methodology$parent_run_id,
    mode_changed_from = config$methodology$mode_changed_from,
    provider                = config$ai$provider,
    model_primary           = provider$models$primary,
    model_fast              = provider$models$fast %||% provider$models$primary,
    timestamp               = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    config_hash             = if (!is.null(config_path)) hash_config(config_path)
                                else NA_character_,
    study_name              = config$study$name,
    research_focus          = config$study$research_focus,
    package_version         = as.character(utils::packageVersion("pakhom")),
    analysis_schema_version = .SCHEMA_VERSION,
    # Mode 1-specific fields so a reviewer scanning run_metadata.json
    # immediately sees what categories were attempted + how many themes
    # the researcher submitted (the analytic frame the AI was challenging).
    mode1_categories_requested = categories,
    mode1_n_themes_input       = as.integer(n_themes(theme_set))
  )

  audit_log <- tryCatch(
    init_audit_log(output_dir, config = config),
    error = function(e) { log_warn("Audit log init failed: {e$message}"); NULL }
  )
  on.exit(if (!is.null(audit_log))
            tryCatch(close_audit_log(audit_log), error = function(e) NULL),
          add = TRUE)

  response_cache <- tryCatch(
    init_response_cache(output_dir, config = config),
    error = function(e) { log_warn("Response cache init failed: {e$message}"); NULL }
  )

  fabrication_log <- tryCatch(
    init_fabrication_log(output_dir, methodology_mode = config$methodology$mode),
    error = function(e) { log_warn("Fabrication log init failed: {e$message}"); NULL }
  )

  # ---- Resume reflection log if available ---------------------------------
  resume_log <- NULL
  if (isTRUE(resume)) {
    rl_path <- file.path(output_dir, "reflection_log.json")
    if (file.exists(rl_path)) {
      resume_log <- tryCatch(.read_reflection_log_json(rl_path),
                               error = function(e) {
                                 log_warn("Could not parse reflection_log.json for resume: {e$message}")
                                 NULL
                               })
      if (!is.null(resume_log)) {
        log_info("Resuming with {length(resume_log$provocations)} prior provocation(s)")
      }
    }
    # Phase 33 (M1.3): hydrate memos from on-disk Markdown files even
    # if the reflection_log.json is unavailable (memos have their own
    # canonical persistence layer per AC4 -- the Markdown files are
    # the source of truth, not the JSON copy). This keeps
    # researcher-authored memos durable across crashes / format
    # changes that would otherwise lose them.
    on_disk_memos <- tryCatch(load_memos(output_dir),
                                error = function(e) {
                                  log_warn("Could not load persisted memos: {e$message}")
                                  list()
                                })
    if (length(on_disk_memos) > 0L) {
      # Phase 33 audit (resume disagreement): if the JSON had memos
      # AND the on-disk count differs, log a warning so the divergence
      # is visible in the run log rather than silent. The on-disk
      # version always wins (per AC4 the .md files are canonical), but
      # a count mismatch usually signals manual intervention worth
      # flagging.
      json_memo_count <- length(resume_log$memos %||% list())
      if (json_memo_count > 0L && json_memo_count != length(on_disk_memos)) {
        log_warn("Memo count mismatch on resume: reflection_log.json has {json_memo_count}, memos/ has {length(on_disk_memos)}; on-disk wins (AC4)")
      }
      if (is.null(resume_log)) {
        resume_log <- create_reflection_log()
      }
      resume_log$memos <- on_disk_memos
      log_info("Resumed with {length(on_disk_memos)} prior memo(s) from {file.path(output_dir, 'memos')}")
    }
  }

  # ---- Provocateur loop ---------------------------------------------------
  log_info("\n[STEP 1] Running provocateur questioning across themes...")
  reflection_log <- run_provocateur_questioning(
    data = data,
    theme_set = theme_set,
    provider = provider,
    config = config,
    categories = categories,
    audit_log = audit_log,
    response_cache = response_cache,
    fabrication_log = fabrication_log,
    resume_log = resume_log
  )

  # ---- T0.3 coverage + T0.2 spread (per-theme stats) ----------------------
  log_info("\n[STEP 2] Computing T0.3 (coverage) + T0.2 (per-theme spread)...")
  coverage <- tryCatch(
    compute_mode1_coverage(reflection_log, theme_set, data,
                             requested_categories = categories),
    error = function(e) {
      log_warn("Mode 1 coverage compute failed: {e$message}")
      NULL
    }
  )
  theme_stats <- tryCatch(
    compute_mode1_theme_stats(data, theme_set, reflection_log,
                                quotes_per_theme = config$analysis$themes$quotes_per_theme %||% 3L,
                                config = config),
    error = function(e) {
      log_warn("Mode 1 theme stats compute failed: {e$message}")
      list()
    }
  )

  # ---- Write Mode 1 canonical artifacts -----------------------------------
  log_info("\n[STEP 3] Writing Mode 1 artifacts...")
  artifact_paths <- .write_mode1_artifacts(reflection_log, output_dir,
                                              methodology_mode = meth_mode)
  themes_path <- .write_mode1_themes_json(theme_set, output_dir,
                                            methodology_mode = meth_mode)
  cov_path <- if (!is.null(coverage))
                .write_mode1_coverage_json(coverage, output_dir,
                                             methodology_mode = meth_mode)
              else NA_character_

  # Phase 33 (M1.3): persist any memos in the reflection log to
  # outputs/<run>/memos/<id>.md. Idempotent + safe-on-empty (returns
  # immediately when no memos). The memos directory is the canonical
  # source of truth for memo content; the reflection_log.json carries
  # an in-memory copy for replay convenience but the Markdown files
  # are what survive a JSON-format change. tryCatch keeps a memo-
  # write failure (e.g., disk full) from killing the rest of the
  # pipeline -- but unlike the framework_archive case (where AC4
  # mandates the archive), memos are researcher-authored and an empty
  # memo set is itself a valid state.
  memo_paths <- tryCatch(persist_memos(reflection_log, output_dir),
                            error = function(e) {
                              log_warn("Could not persist memos: {e$message}")
                              character(0)
                            })

  # ---- Generate the Mode 1 report -----------------------------------------
  if (isTRUE(config$output$generate_report)) {
    log_info("\n[STEP 4] Generating Mode 1 analysis report...")
    report_file <- file.path(output_dir, "analysis_report.html")
    tryCatch(
      generate_mode1_report(
        data            = data,
        theme_set       = theme_set,
        reflection_log  = reflection_log,
        coverage        = coverage,
        theme_stats     = theme_stats,
        config          = config,
        provider        = provider,
        audit_log       = audit_log,
        response_cache  = response_cache,
        fabrication_log = fabrication_log,
        output_file     = report_file
      ),
      error = function(e) log_warn("Mode 1 report generation failed: {e$message}")
    )
  }

  # ---- Integrity check + finalize -----------------------------------------
  log_info("\n[STEP 5] Integrity check + finalize...")
  integrity <- verify_run_integrity(output_dir, config)
  if (length(integrity$missing) > 0L) {
    log_warn("Mode 1 run integrity: {length(integrity$missing)} expected file(s) missing:")
    for (f in integrity$missing) log_warn("  - {f}")
  } else {
    log_info("Mode 1 run integrity: all {length(integrity$expected)} expected files present")
  }

  tryCatch(finalize_run(output_dir),
           error = function(e) log_warn("Could not finalize run: {e$message}"))

  toc()

  log_info("\n========================================")
  log_info("MODE 1 ANALYSIS COMPLETE")
  log_info("========================================")
  log_info("Themes challenged:        {length(theme_stats)}")
  log_info("Provocations emitted:     {length(reflection_log$provocations)}")
  if (!is.null(coverage)) {
    log_info("No silent skip:           {coverage$no_silent_skip}")
  }
  log_info("Results saved to:         {output_dir}")
  log_info("========================================")

  invisible(list(
    output_dir     = output_dir,
    reflection_log = reflection_log,
    theme_set      = theme_set,
    coverage       = coverage,
    theme_stats    = theme_stats,
    config         = config,
    integrity      = integrity,
    artifact_paths = artifact_paths
  ))
}

#' Read a reflection_log.json back into a ResearcherReflectionLog
#'
#' Audit A H3 (phase 31): a previous version used
#' \code{simplifyVector = TRUE}, which collapsed
#' \code{provocations} (a list of uniform-shape objects) into a row-frame
#' AND stripped the \code{Provocation} / \code{QuoteProvenance} S3 class
#' tags from the nested elements. On resume, downstream code that gates
#' on \code{inherits(p, "Provocation")} or
#' \code{inherits(p$provenance, "QuoteProvenance")} (notably
#' \code{.provocation_to_row} and the per-category provocation
#' functions) would silently emit NA-cited rows. The fix here is
#' two-step: (1) read with simplifyVector=FALSE so the provocations
#' list keeps its list-of-lists shape; (2) explicitly re-class each
#' provocation + its provenance after the read. The data.frame slots
#' (provocation_attempts / skipped_themes / positionality_history) are
#' then re-coerced back to data.frames since simplifyVector=FALSE
#' leaves them as lists.
#' @keywords internal
.read_reflection_log_json <- function(path) {
  raw <- jsonlite::read_json(path, simplifyVector = FALSE)

  # Re-class each Provocation + its embedded QuoteProvenance. JSON has
  # no notion of S3 class tags so the round-trip strips them; resume
  # downstream relies on inherits() checks.
  if (!is.null(raw$provocations) && length(raw$provocations) > 0L) {
    raw$provocations <- lapply(raw$provocations, function(p) {
      if (!is.list(p)) return(p)
      if (!is.null(p$provenance) && is.list(p$provenance)) {
        class(p$provenance) <- "QuoteProvenance"
      }
      class(p) <- "Provocation"
      p
    })
  } else {
    raw$provocations <- list()
  }

  # Phase 33 audit C1: re-class each Memo on read. Without this,
  # downstream consumers gating on inherits(m, "Memo")
  # (.build_mode1_memo_section, persist_memos, the n_memos counter on
  # ProvocationCoverage, etc.) silently treat the resumed run as
  # having zero memos. The Markdown-on-disk path (load_memos) is the
  # canonical source of truth and overrides this when memos/ has
  # files, but on a JSON-only resume the re-class is the only thing
  # that keeps memos visible. Schema version is preserved -- a 1.0.0
  # Memo from a future cross-version round-trip stays at 1.0.0.
  if (!is.null(raw$memos) && length(raw$memos) > 0L) {
    raw$memos <- lapply(raw$memos, function(m) {
      if (!is.list(m)) return(m)
      class(m) <- "Memo"
      m
    })
  } else {
    raw$memos <- list()
  }

  # Coerce the data.frame slots back. simplifyVector=FALSE leaves them
  # as a list of row-named-lists; rbind via do.call gives a faithful
  # data.frame round-trip.
  .coerce_rowlist_to_df <- function(rl, expected_cols) {
    if (is.null(rl) || length(rl) == 0L) {
      return(stats::setNames(
        as.data.frame(matrix("", nrow = 0L, ncol = length(expected_cols)),
                       stringsAsFactors = FALSE),
        expected_cols
      ))
    }
    # Each element is a named list; bind them
    df <- do.call(rbind, lapply(rl, function(row) {
      as.data.frame(row, stringsAsFactors = FALSE)
    }))
    df
  }

  raw$provocation_attempts <- .coerce_rowlist_to_df(
    raw$provocation_attempts,
    c("theme_name", "category", "n_emitted", "attempted_at")
  )
  raw$skipped_themes <- .coerce_rowlist_to_df(
    raw$skipped_themes,
    c("theme_name", "reason", "skipped_at")
  )
  raw$positionality_history <- .coerce_rowlist_to_df(
    raw$positionality_history,
    c("timestamp", "statement", "prompt_id")
  )

  # Some atomic fields will have come back as length-1 lists due to
  # simplifyVector=FALSE; unwrap them.
  for (nm in c("config_hash", "created_at", "last_updated",
                "schema_version")) {
    if (!is.null(raw[[nm]]) && is.list(raw[[nm]]) &&
        length(raw[[nm]]) == 1L) {
      raw[[nm]] <- raw[[nm]][[1L]]
    }
  }

  class(raw) <- "ResearcherReflectionLog"
  raw
}
