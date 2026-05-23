# ==============================================================================
# Corpus Coverage Assertion (Sprint-4 T0.3)
# ==============================================================================
# Third Tier-0 universal requirement (after T0.1 quote provenance and T0.2
# participant spread). Empirical answer to Jowsey et al. 2025's
# "Frankenstein" finding that Microsoft Copilot "drew themes from only the
# first 2-3 pages of data" -- silent truncation of the corpus.
#
# pakhom's coding pipeline processes entries strictly one at a time (no
# batching, no truncation in the LLM call path). The CorpusCoverage object
# captures the proof: every entry that survived preprocessing was sent to
# the LLM, and the report renders the funnel explicitly so reviewers don't
# have to take that on faith.
#
# Per AC1 (AI is scaffold by architecture, not by configuration), coverage
# is computed AFTER the coding step regardless of mode. Per AC4
# (methodology stamped on every output), the coverage card is rendered
# unconditionally; absence of the card is itself a failure signal.
#
# Mode-shape: Mode 2/3 use CorpusCoverage; Mode 1 uses ProvocationCoverage
# (in R/mode1_orchestrator.R). Both inherit a virtual Tier0Coverage parent
# class so the report renderer dispatches via render_tier0_coverage_card()
# without conflating the mode-specific semantics. The headline assertion
# differs by mode: M2/M3 says "no silent truncation" (LLM saw every
# entry); M1 says "no silent skip" (every theme was challenged across
# every requested category). Both are coverage claims; the underlying
# evidence is mode-specific.
# ==============================================================================

#' Current schema version for the CorpusCoverage object
#' @keywords internal
.CORPUS_COVERAGE_SCHEMA_VERSION <- "1.0.0"

#' Compute corpus coverage from a completed coding run
#'
#' Asserts the LLM saw every entry that survived preprocessing. Returns a
#' \code{CorpusCoverage} S3 object summarising the funnel from preprocessed
#' data to LLM-processed entries to coded entries, plus the
#' \code{no_silent_truncation} flag that pakhom uses as the headline
#' Tier-0 assertion.
#'
#' Pre-preprocessing counts (e.g., raw rows from the database before
#' deduplication and length filtering) can be supplied via
#' \code{n_raw_loaded} and \code{n_after_preprocessing}; when omitted, the
#' coverage object reports them as \code{NA_integer_} and the card
#' degrades gracefully. The headline assertion (no silent truncation in
#' the LLM call path) does not depend on pre-preprocessing counts.
#'
#' @param coding_state A finalized \code{ProgressiveCodingState} (the
#'   one returned by \code{\link{run_progressive_coding}}).
#' @param data The standardized + preprocessed tibble that was fed to
#'   the coding step (must have \code{std_id} and \code{std_text}).
#'   Used to compute byte counts and to verify every entry has a
#'   matching \code{entry_results} record.
#' @param n_raw_loaded Optional integer: rows loaded from the database
#'   before preprocessing. \code{NA_integer_} when unknown (e.g.,
#'   resumed run where the raw count wasn't preserved across the
#'   checkpoint).
#' @param n_after_preprocessing Optional integer: rows after
#'   preprocessing but before any test-mode sampling. Defaults to
#'   \code{NA_integer_}.
#' @param test_mode_sample_size Optional integer: when test mode was on,
#'   the sub-sample size used. \code{NA_integer_} when test mode was off.
#' @return A \code{CorpusCoverage} S3 object (a list with class).
#' @export
compute_corpus_coverage <- function(coding_state, data,
                                     n_raw_loaded = NA_integer_,
                                     n_after_preprocessing = NA_integer_,
                                     test_mode_sample_size = NA_integer_) {
  if (!inherits(coding_state, "ProgressiveCodingState")) {
    stop("coding_state must be a ProgressiveCodingState object", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("data must be a data.frame / tibble", call. = FALSE)
  }
  if (!"std_id" %in% names(data)) {
    stop("data must have a `std_id` column", call. = FALSE)
  }

  # Duplicate std_ids would silently corrupt the headline assertion: an
  # intersect-based match would dedupe duplicates, making n_processed
  # appear to equal n_unique_ids when actually some duplicate-id entries
  # were never seen. Refuse rather than affirm coverage we can't verify.
  n_dup <- sum(duplicated(data$std_id))
  if (n_dup > 0L) {
    stop(sprintf(
      "data has %d duplicate std_id values; coverage cannot be verified ",
      n_dup
    ),
    "(every entry must have a unique std_id for the LLM-call-path coverage ",
    "claim to be meaningful). Deduplicate the input or fix the upstream join.",
    call. = FALSE)
  }

  n_input <- nrow(data)
  entry_results <- coding_state$entry_results

  # Match data std_ids against entry_results -- every input entry should
  # have a corresponding entry_result if coding ran to completion.
  matched_ids   <- intersect(data$std_id, names(entry_results))
  unmatched_ids <- setdiff(data$std_id, names(entry_results))
  n_processed   <- length(matched_ids)
  n_unprocessed <- length(unmatched_ids)

  # Skip / coded breakdown over MATCHED entry_results
  skipped_flags <- vapply(
    matched_ids,
    function(id) isTRUE(entry_results[[id]]$skipped),
    logical(1)
  )
  n_skipped <- sum(skipped_flags)
  n_coded   <- n_processed - n_skipped

  skip_reasons <- vapply(
    matched_ids[skipped_flags],
    function(id) {
      r <- entry_results[[id]]$skip_reason %||% "unspecified"
      as.character(r)[1]
    },
    character(1)
  )
  skip_reason_table <- if (length(skip_reasons) == 0L) {
    stats::setNames(integer(0), character(0))
  } else {
    tbl <- table(skip_reasons, useNA = "no")
    stats::setNames(as.integer(tbl), names(tbl))
  }

  # Byte and word counts over the entries actually processed (whether
  # coded or skipped -- they were ALL sent to the LLM, which is the
  # coverage claim). Computed from std_text in the input data so
  # truncation in the LLM prompt (.MAX_ENTRY_CHARS cap in
  # .build_progressive_schema_user_prompt) doesn't deflate the figure.
  text_processed <- data$std_text[data$std_id %in% matched_ids]
  bytes_processed <- sum(nchar(text_processed, type = "bytes"),
                          na.rm = TRUE)
  chars_processed <- sum(nchar(text_processed, type = "chars"),
                          na.rm = TRUE)
  words_processed <- sum(.count_words_safe(text_processed), na.rm = TRUE)

  # Phase 56 awareness of saturation-triggered early stop. The pre-Phase-56
  # headline assertion (every input entry has a matching entry_result)
  # would render as FALSE whenever the AI saturation arbiter stops coding
  # early -- exactly the methodologically intentional case T0.3 is supposed
  # to celebrate, not flag as silent truncation. Phase 56 audit CRITICAL-1
  # disentangles the two: saturation-triggered tail is INTENTIONAL coverage
  # (n_unprocessed should equal n - reached_at_entry); any OTHER gap is
  # still silent truncation.
  saturation_reached <- isTRUE(coding_state$saturation$reached)
  if (saturation_reached) {
    reached_at_entry <- coding_state$saturation$reached_at_entry %||% NA_integer_
    expected_unprocessed_tail <- if (!is.na(reached_at_entry)) {
      max(0L, n_input - as.integer(reached_at_entry))
    } else 0L
    # T0.3 satisfied when the unprocessed count exactly matches the
    # tail the arbiter intentionally skipped. A mismatch (more or fewer
    # unprocessed than expected) is still a transparency failure.
    no_silent_truncation <- (n_unprocessed == expected_unprocessed_tail) &&
                              (n_input > 0L)
    stop_reason <- "saturation_arbiter_reached"
  } else {
    no_silent_truncation <- (n_unprocessed == 0L) && (n_input > 0L)
    stop_reason <- "all_entries_processed"
  }

  # Coverage rate as a fraction; useful for the methodology paper KPI
  # over multiple runs.
  coverage_rate <- if (n_input == 0L) NA_real_ else n_processed / n_input

  # Validate optional counts -- coerce to integer so the schema field
  # type is stable
  coerce_int <- function(x) {
    if (is.null(x) || is.na(x)) return(NA_integer_)
    as.integer(x)
  }

  obj <- list(
    n_raw_loaded             = coerce_int(n_raw_loaded),
    n_after_preprocessing    = coerce_int(n_after_preprocessing),
    test_mode_sample_size    = coerce_int(test_mode_sample_size),
    n_input_to_coding        = as.integer(n_input),
    n_processed              = as.integer(n_processed),
    n_unprocessed            = as.integer(n_unprocessed),
    unprocessed_ids          = as.character(unmatched_ids),
    n_skipped                = as.integer(n_skipped),
    skip_reasons             = skip_reason_table,
    n_coded                  = as.integer(n_coded),
    bytes_processed          = as.integer(bytes_processed),
    chars_processed          = as.integer(chars_processed),
    words_processed          = as.integer(words_processed),
    coverage_rate            = coverage_rate,
    no_silent_truncation     = no_silent_truncation,
    # Phase 56: distinguishes saturation-triggered intentional early-stop
    # from genuine silent truncation. Consumers (report renderer + audit
    # log) gate language on this. "all_entries_processed" = entire corpus
    # reached the LLM; "saturation_arbiter_reached" = AI arbiter judged
    # the codebook saturated and stopped intentionally (with the
    # arbiter's articulation + rationale stamped in
    # coding_state$saturation).
    stop_reason              = stop_reason,
    saturation_reached       = saturation_reached,
    reached_at_entry         = coerce_int(coding_state$saturation$reached_at_entry %||% NA_integer_),
    computed_at              = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    schema_version           = .CORPUS_COVERAGE_SCHEMA_VERSION
  )
  # Multi-class: Tier0Coverage is the virtual parent that the report
  # renderer dispatches on (via render_tier0_coverage_card). Mode 1's
  # ProvocationCoverage shares the same parent so a single call site in
  # the report builder handles both. CorpusCoverage methods stay
  # specific to this class; AC7 ("universal Tier-0") is satisfied at the
  # COMMITMENT level (both modes render a coverage card unconditionally)
  # rather than the data-shape level (the underlying claims differ).
  class(obj) <- c("CorpusCoverage", "Tier0Coverage")
  obj
}

#' Print method for CorpusCoverage
#' @param x A CorpusCoverage object
#' @param ... Ignored
#' @export
print.CorpusCoverage <- function(x, ...) {
  cat("CorpusCoverage (T0.3)\n")
  if (!is.na(x$n_raw_loaded)) {
    cat(sprintf("  Raw rows loaded:           %s\n",
                format(x$n_raw_loaded, big.mark = ",")))
  }
  if (!is.na(x$n_after_preprocessing)) {
    cat(sprintf("  After preprocessing:       %s\n",
                format(x$n_after_preprocessing, big.mark = ",")))
  }
  if (!is.na(x$test_mode_sample_size)) {
    cat(sprintf("  Test-mode sample:          %s\n",
                format(x$test_mode_sample_size, big.mark = ",")))
  }
  cat(sprintf("  Input to coding step:      %s\n",
              format(x$n_input_to_coding, big.mark = ",")))
  cat(sprintf("  LLM-processed entries:     %s\n",
              format(x$n_processed, big.mark = ",")))
  if (x$n_unprocessed > 0L) {
    if (isTRUE(x$saturation_reached) &&
        identical(x$stop_reason, "saturation_arbiter_reached")) {
      cat(sprintf("  Unprocessed (intentional): %s  <-- saturation arbiter judged reached at entry %s\n",
                  format(x$n_unprocessed, big.mark = ","),
                  format(x$reached_at_entry %||% NA, big.mark = ",")))
    } else {
      cat(sprintf("  Unprocessed (gap):         %s  <-- INVESTIGATE\n",
                  format(x$n_unprocessed, big.mark = ",")))
    }
  }
  cat(sprintf("    -- of those: coded:      %s\n",
              format(x$n_coded, big.mark = ",")))
  cat(sprintf("    -- of those: skipped:    %s\n",
              format(x$n_skipped, big.mark = ",")))
  cat(sprintf("  Bytes processed:           %s\n",
              format(x$bytes_processed, big.mark = ",")))
  cat(sprintf("  Words processed (approx):  %s\n",
              format(x$words_processed, big.mark = ",")))
  cat(sprintf("  Coverage rate:             %s\n",
              if (is.na(x$coverage_rate)) "n/a"
              else sprintf("%.1f%%", 100 * x$coverage_rate)))
  cat(sprintf("  No silent truncation:      %s\n",
              if (isTRUE(x$no_silent_truncation)) "TRUE (verified)" else "FALSE"))
  invisible(x)
}

#' Approximate word count of a character vector
#'
#' Splits on runs of whitespace and counts the resulting tokens. Used by
#' \code{\link{compute_corpus_coverage}} for the "words processed"
#' figure on the coverage card. Approximate (handles English-like text;
#' degrades gracefully on punctuation-heavy text).
#' @keywords internal
.count_words_safe <- function(text) {
  if (length(text) == 0L) return(integer(0))
  out <- vapply(as.character(text), function(s) {
    if (is.na(s) || !nzchar(s)) return(0L)
    length(strsplit(trimws(s), "\\s+", perl = TRUE)[[1L]])
  }, integer(1), USE.NAMES = FALSE)
  out
}

# ==============================================================================
# Tier-0 coverage card renderer (S3 dispatch on Tier0Coverage subclasses)
# ==============================================================================
# The report builder calls render_tier0_coverage_card(coverage) without
# branching on mode -- the dispatch happens at the S3 method level.
# Methods live with the relevant class:
#   render_tier0_coverage_card.CorpusCoverage     -- in R/17_report.R
#                                                    (renders Mode 2/3 funnel)
#   render_tier0_coverage_card.ProvocationCoverage -- in R/mode1_orchestrator.R
#                                                    (renders Mode 1 attempt
#                                                     matrix + skipped themes)
#   render_tier0_coverage_card.default            -- below; renders an
#                                                    "unavailable" notice when
#                                                    coverage is NULL or an
#                                                    unrecognized type slips in
#
# AC4 ("methodology stamped on every output") forbids silent omission --
# unavailable variant must say so explicitly so a reviewer scanning the
# report knows the absence is reported, not hidden.

#' Render the Tier-0 coverage card for a coverage object
#'
#' S3 generic; the renderer the HTML report calls. Method dispatched on the
#' object's class. NULL is bypassed and routed to a fixed "unavailable"
#' card so the call site in \code{generate_report} / \code{generate_mode1_report}
#' does not need to branch.
#'
#' @param x A coverage object (CorpusCoverage, ProvocationCoverage), NULL,
#'   or any other object (returns the unavailable variant).
#' @param ... Method-specific arguments.
#' @return Character HTML/markdown string for the card.
#' @export
render_tier0_coverage_card <- function(x, ...) {
  if (is.null(x)) return(.tier0_coverage_card_unavailable())
  UseMethod("render_tier0_coverage_card")
}

#' @rdname render_tier0_coverage_card
#' @export
render_tier0_coverage_card.default <- function(x, ...) {
  .tier0_coverage_card_unavailable()
}

#' Unavailable-coverage card (NULL / unrecognized inputs)
#'
#' Per AC4, absence of a Tier-0 card is itself a transparency signal --
#' rather than hide it, the report renders an explicit "not computed"
#' notice. Invoked from \code{render_tier0_coverage_card} when no method
#' matches.
#' @keywords internal
.tier0_coverage_card_unavailable <- function() {
  paste0(
    '<div class="coverage-card coverage-unavailable">\n',
    '<div class="coverage-header">Tier-0 Coverage</div>\n',
    '<p class="coverage-unavailable-note">Coverage data not computed ',
    'for this report. Per Tier-0 transparency policy this absence is ',
    'reported rather than silently omitted -- a complete pakhom run ',
    'computes coverage as part of its finalization step.</p>\n',
    '</div>\n\n'
  )
}

#' Persist a CorpusCoverage / ProvocationCoverage object to disk
#'
#' Phase 58 Tier 8 H-10: pre-Tier-8 the CorpusCoverage S3 was computed
#' in memory and rendered as HTML but never written to disk as
#' machine-readable data. A reproducibility audit couldn't read
#' coverage state without re-running the pipeline. This writer
#' serializes the full coverage object as \code{coverage_card.json}
#' alongside the report HTML, methodology-stamped per AC4.
#'
#' The JSON shape preserves every field on the S3 (\code{n_input_to_-
#' coding}, \code{n_processed}, \code{n_unprocessed}, \code{n_skipped},
#' \code{n_coded}, \code{skip_reasons}, \code{words_processed},
#' \code{coverage_rate}, \code{no_silent_truncation}, \code{stop_-
#' reason}, \code{saturation_reached}, \code{reached_at_entry}, etc.)
#' so a downstream consumer can reconstruct the funnel + saturation
#' state without the original coding_state.
#'
#' @param coverage CorpusCoverage / ProvocationCoverage / Tier0Coverage
#'   object from \code{\link{compute_corpus_coverage}}.
#' @param output_dir Run output directory.
#' @param methodology_mode Optional methodology mode for AC4 stamping.
#' @return Invisible path to the written JSON.
#' @export
write_corpus_coverage <- function(coverage, output_dir,
                                    methodology_mode = NULL) {
  if (is.null(coverage)) {
    return(invisible(NULL))
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(output_dir, "coverage_card.json")

  # Unclass to a plain list so jsonlite renders all fields uniformly
  # (the S3 class would otherwise dispatch any method that the auto-
  # serializer happens to match). skip_reasons is a named integer
  # vector; wrap in as.list so the JSON object retains names.
  cov_list <- unclass(coverage)
  if (!is.null(cov_list$skip_reasons)) {
    cov_list$skip_reasons <- as.list(cov_list$skip_reasons)
  }
  # Phase 58 Tier 8 H-10: schema_version is populated by
  # compute_corpus_coverage() at the source (R/corpus_coverage.R:204
  # via .CORPUS_COVERAGE_SCHEMA_VERSION) and rides on the coverage
  # object. Defensive fill-in only when the source object predates
  # that field (e.g. a pre-Phase-58 cached coverage object loaded
  # from an older run). Tier 8 audit followup HIGH-1: pre-followup
  # this line unconditionally overwrote with "1.0.0", which would
  # silently lie if .CORPUS_COVERAGE_SCHEMA_VERSION ever bumps.
  if (is.null(cov_list$schema_version)) {
    cov_list$schema_version <- .CORPUS_COVERAGE_SCHEMA_VERSION
  }

  jsonlite::write_json(cov_list, path, pretty = TRUE,
                        auto_unbox = TRUE, null = "null",
                        force = TRUE)

  if (!is.null(methodology_mode)) {
    tryCatch(
      stamp_methodology_json(path, methodology_mode,
                              run_id = basename(output_dir)),
      error = function(e) log_debug("coverage_card.json stamp skipped: {e$message}")
    )
  }

  log_info("Coverage card written: {path}")
  invisible(path)
}
