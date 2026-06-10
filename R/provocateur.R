# ==============================================================================
# Provocateur Architecture
# ==============================================================================
# Mode 1 (Reflexive Scaffold) implements Sarkar's "AI as Socratic gadfly"
# (CACM Oct 2024, arXiv 2411.02263) as a shipping qualitative-research
# tool. The AI's job is to surface QUESTIONS the researcher might not
# have considered -- counter-narratives, absent voices, alternative
# interpretations, disconfirming evidence, assumption surfacing -- not
# to propose themes or codes. Theme/code AUTHORSHIP belongs to the
# researcher; the AI is an extractive interlocutor.
#
# Empirical motivation:
# - Sarkar 2024: AI Should Challenge, Not Obey. Three failure modes
#   when AI obeys: dilution (homogenization), distortion (subtle bias
#   absorption), deskilling (loss of critical capacity).
# - Drosos, Sarkar, Toronto 2025 (CHI): provocations measurably induce
#   critical/metacognitive thinking on important non-urgent tasks where
#   the user has expertise + accountability -- exactly the qualitative
#   research profile.
# - Vikan et al. 2026 (Sage): under prolonged AI use, researcher
#   engagement collapses to verification mode; the implicit threshold
#   is real. Provocations counteract this by FORCING the researcher
#   back into the data.
#
# Architecture:
#   ResearcherReflectionLog (S3): the canonical record of the researcher's
#     side of the analysis -- what provocations were fired, what memos
#     were written, what positionality statements were made, what codes
#     and themes the researcher wrote (the AI does NOT write them).
#   Provocation (S3): a single extractive provocation -- category,
#     theme it challenges, entry citation with QuoteProvenance, reason,
#     researcher action.
#   provoke_*(): five category functions, each producing a list of
#     Provocation objects.
#   run_provocateur_questioning(): orchestrator that fires all (or
#     selected) categories per theme, persists to ReflectionLog.
#
# Architectural commitments:
#   AC1 (architecture not config): provocations are bounded by
#     low-degrees-of-freedom prompt templates -- the model has nowhere
#     to drift.
#   AC9 (rules every turn): methodology rules from R/methodology_rules.R
#     are injected automatically via ai_complete (no special handling
#     needed here -- the rules tell the model "Mode 1: return only
#     extractive provocations").
#   AC7 (universal Tier-0): every provocation's cited span is a
#     QuoteProvenance object run through verify_quote -- fabricated
#     provocations are detected and dropped just like fabricated codes.
# ==============================================================================

#' Current ResearcherReflectionLog schema version
#'
#' 1.0.0 -- initial schema: provocations + memos +
#'   positionality_history + reflexivity_collapse_flags +
#'   researcher_authored_codes + researcher_authored_themes.
#' 1.1.0 -- add provocation_attempts
#'   and skipped_themes data.frames so Mode 1 can honestly assert T0.3
#'   coverage. provocation_attempts records one row per (theme, category)
#'   attempt regardless of how many provocations the AI emitted; the
#'   distinction matters because a category that legitimately returns
#'   zero provocations (e.g., counter_narrative finds no qualifying
#'   entries) is NOT a coverage failure -- whereas a category that was
#'   never attempted IS. skipped_themes records themes the orchestrator
#'   bypassed (e.g., zero supporting entries) with an explicit reason,
#'   so the coverage card distinguishes "silent skip" from "explicit
#'   skip with stated reason."
#' 1.2.0 -- M1.3 reflexive memos: the memos slot now holds
#'   a list of typed \code{Memo} S3 objects rather than an unstructured
#'   list. The Memo schema (id, timestamp, author, type, linked_codes,
#'   linked_themes, linked_entries, linked_prior_memo, body) supports
#'   Markdown round-trip with YAML frontmatter and is the AC6
#'   burden-parity counterpart
#'   to Modes 2/3's codebook + theme review pause-points. CRUD via
#'   \code{add_memo}, \code{read_memo}, \code{list_memos}; persistence
#'   via \code{persist_memos} / \code{load_memos}. Backward-compatible:
#'   1.1.0 logs with a list of pre-Memo entries are kept in place
#'   (the new code paths gate on \code{inherits(m, "Memo")}).
#' @keywords internal
.RESEARCHER_REFLECTION_LOG_SCHEMA_VERSION <- "1.2.0"

#' Schema version for individual Provocation S3 objects.
#'
#' Tracks the structure of a single provocation record (separate from
#' the parent ResearcherReflectionLog version). Bumped when the
#' Provocation schema changes incompatibly.
#' @keywords internal
.PROVOCATION_SCHEMA_VERSION <- "1.0.0"

#' Valid provocation category names
#' @keywords internal
.VALID_PROVOCATION_CATEGORIES <- c(
  "counter_narrative",
  "absent_voice",
  "alternative_interpretation",
  "disconfirming_evidence",
  "assumption_surfacing"
)

# ==============================================================================
# ResearcherReflectionLog S3 class
# ==============================================================================

#' Initialize a ResearcherReflectionLog
#'
#' Parallel to \code{ProgressiveCodingState} but for Mode 1: the
#' researcher's side of the analysis. The AI never writes to this object
#' (other than appending Provocations); the researcher's codes, themes,
#' memos, and positionality statements are all human-authored.
#'
#' @param config_hash Optional character: hash of the current config for
#'   resume compatibility (matches the ProgressiveCodingState pattern).
#' @return A \code{ResearcherReflectionLog} S3 object.
#' @export
create_reflection_log <- function(config_hash = NULL) {
  now_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  log <- list(
    provocations               = list(),
    memos                      = list(),
    positionality_history      = data.frame(
      timestamp = character(0),
      statement = character(0),
      prompt_id = character(0),
      stringsAsFactors = FALSE
    ),
    reflexivity_collapse_flags = list(),
    researcher_authored_codes  = list(),
    researcher_authored_themes = list(),
    # T0.3 attempt tracking. One row per (theme x category)
    # attempt the orchestrator made, regardless of whether the AI emitted
    # provocations. Lets compute_mode1_coverage assert "every theme was
    # challenged across every requested category" without conflating
    # "AI returned zero provocations" with "category was never attempted."
    provocation_attempts       = data.frame(
      theme_name   = character(0),
      category     = character(0),
      n_emitted    = integer(0),
      attempted_at = character(0),
      stringsAsFactors = FALSE
    ),
    # T0.3 explicit-skip tracking. Themes the orchestrator
    # bypassed with a stated reason (e.g., zero supporting entries). A
    # silent skip -- a theme that should have been processed but wasn't
    # -- is a coverage failure; an explicit skip with a stated reason is
    # transparency. The coverage card surfaces both.
    skipped_themes             = data.frame(
      theme_name = character(0),
      reason     = character(0),
      skipped_at = character(0),
      stringsAsFactors = FALSE
    ),
    config_hash                = config_hash,
    created_at                 = now_iso,
    last_updated               = now_iso,
    schema_version             = .RESEARCHER_REFLECTION_LOG_SCHEMA_VERSION
  )
  class(log) <- "ResearcherReflectionLog"
  log
}

#' Print method for ResearcherReflectionLog
#' @param x A ResearcherReflectionLog
#' @param ... Ignored
#' @export
print.ResearcherReflectionLog <- function(x, ...) {
  cat("ResearcherReflectionLog\n")
  cat(sprintf("  Provocations:                 %d\n", length(x$provocations)))
  cat(sprintf("  Memos:                        %d\n", length(x$memos)))
  # Schema 1.2.0+: when memos are typed Memo S3 objects, surface the
  # by-type breakdown so the print summary distinguishes operational
  # / coding / theoretical / positionality memos at a glance.
  if (length(x$memos) > 0L) {
    typed_memos <- Filter(function(m) inherits(m, "Memo"), x$memos)
    if (length(typed_memos) > 0L) {
      tmtypes <- vapply(typed_memos, function(m) m$type, character(1))
      ttbl <- table(tmtypes)
      for (tn in names(ttbl)) {
        cat(sprintf("    %s: %d\n", tn, ttbl[[tn]]))
      }
    }
  }
  cat(sprintf("  Positionality entries:        %d\n",
              nrow(x$positionality_history)))
  cat(sprintf("  Reflexivity collapse flags:   %d\n",
              length(x$reflexivity_collapse_flags)))
  cat(sprintf("  Researcher-authored codes:    %d\n",
              length(x$researcher_authored_codes)))
  cat(sprintf("  Researcher-authored themes:   %d\n",
              length(x$researcher_authored_themes)))
  # T0.3 attempt + skip tallies (schema 1.1.0+). NULL-safe so a 1.0.0
  # log loaded by a 1.1.0 reader doesn't blow up the print method.
  attempts <- x$provocation_attempts
  skipped  <- x$skipped_themes
  if (!is.null(attempts)) {
    cat(sprintf("  Provocation attempts:         %d\n", nrow(attempts)))
  }
  if (!is.null(skipped)) {
    cat(sprintf("  Themes skipped (with reason): %d\n", nrow(skipped)))
  }
  if (length(x$provocations) > 0L) {
    cats <- vapply(x$provocations, function(p) p$category, character(1))
    tbl <- table(cats)
    cat("  Provocations by category:\n")
    for (n in names(tbl)) cat(sprintf("    %s: %d\n", n, tbl[[n]]))
  }
  cat(sprintf("  Last updated:                 %s\n", x$last_updated))
  invisible(x)
}

# ==============================================================================
# Provocation S3 class
# ==============================================================================

#' Construct a Provocation object
#'
#' A Provocation is an extractive AI-generated question/observation that
#' challenges the researcher's framing of a theme. Each provocation
#' carries a \code{QuoteProvenance} object so the cited evidence is
#' verifiable -- per AC7, no provocation may cite a fabricated quote.
#'
#' @param category One of \code{.VALID_PROVOCATION_CATEGORIES}.
#' @param theme_name Character: the theme this provocation challenges.
#' @param reason Character: one-line explanation of the provocation.
#' @param provenance A \code{QuoteProvenance} object: the cited evidence
#'   (entry_id, char range, exact_text, verification_status).
#' @param extra Optional named list of category-specific fields.
#' @param ai_model Optional character: model that produced the
#'   provocation.
#' @param ai_call_id Optional character: request_id of the AI call.
#' @return A \code{Provocation} S3 object.
#' @export
make_provocation <- function(category, theme_name, reason,
                               provenance,
                               extra = list(),
                               ai_model = NA_character_,
                               ai_call_id = NA_character_) {
  if (!category %in% .VALID_PROVOCATION_CATEGORIES) {
    stop(sprintf(
      "Invalid provocation category '%s'; expected one of: %s",
      category, paste(.VALID_PROVOCATION_CATEGORIES, collapse = ", ")
    ), call. = FALSE)
  }
  if (!is.null(provenance) && !inherits(provenance, "QuoteProvenance")) {
    stop("provenance must be a QuoteProvenance object (or NULL for ",
         "non-extractive provocation categories like absent_voice)",
         call. = FALSE)
  }
  obj <- list(
    category          = category,
    theme_name        = as.character(theme_name)[1L],
    reason            = as.character(reason)[1L],
    provenance        = provenance,
    extra             = extra,
    ai_model          = ai_model,
    ai_call_id        = ai_call_id,
    prompted_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    researcher_action = NA_character_,  # set later by review UI: opened|memo_added|theme_revised|dismissed
    schema_version    = .PROVOCATION_SCHEMA_VERSION
  )
  class(obj) <- "Provocation"
  obj
}

#' Print method for Provocation
#' @param x A Provocation
#' @param ... Ignored
#' @export
print.Provocation <- function(x, ...) {
  cat(sprintf("Provocation [%s] -> theme: %s\n", x$category, x$theme_name))
  if (!is.null(x$provenance) && inherits(x$provenance, "QuoteProvenance")) {
    cat(sprintf("  Source: %s [%d-%d) (%s)\n",
                x$provenance$source_doc_id,
                x$provenance$start_char,
                x$provenance$end_char,
                x$provenance$verification_status))
    txt <- x$provenance$exact_text
    if (nchar(txt) > 100) txt <- paste0(substr(txt, 1, 97), "...")
    cat(sprintf("  Cited: %s\n", txt))
  }
  cat(sprintf("  Reason: %s\n", x$reason))
  if (!is.na(x$researcher_action)) {
    cat(sprintf("  Action: %s\n", x$researcher_action))
  }
  invisible(x)
}

# ==============================================================================
# JSON schemas for the five provocation categories
# ==============================================================================
# Each schema is bounded + extractive: the model returns entry_ids and
# char-range citations, never free-form theme proposals or interpretations.
# Per Sarkar 2024 / patterns doc: the model's job is selection, not
# generation.

#' Schema for an array of extractive citations (entry_id + span + reason)
#'
#' Used by counter_narrative, disconfirming_evidence, alternative_interpretation
#' (each citation in the supporting evidence).
#' @keywords internal
.provocation_citation_schema <- function() {
  list(
    type = "object",
    additionalProperties = FALSE,
    required = list("entry_id", "char_start", "char_end",
                    "exact_text", "reason"),
    properties = list(
      entry_id    = list(type = "string"),
      char_start  = list(type = "integer"),
      char_end    = list(type = "integer"),
      exact_text  = list(type = "string"),
      reason      = list(type = "string")
    )
  )
}

#' Schema for counter_narrative provocations
#'
#' Returns up to N entries that frame the construct as not-Y.
#' @keywords internal
.coding_schema_counter_narrative <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("provocations"),
    properties = list(
      provocations = list(
        type  = "array",
        items = .provocation_citation_schema()
      )
    )
  )
}

#' Schema for disconfirming_evidence provocations
#'
#' Same shape as counter_narrative (extractive citations).
#' @keywords internal
.coding_schema_disconfirming_evidence <- function() {
  .coding_schema_counter_narrative()
}

#' Schema for alternative_interpretation provocations
#'
#' Returns alternative theme names + the same supporting quotes; no
#' "which is better" judgment.
#' @keywords internal
.coding_schema_alternative_interpretation <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("alternative_names", "shared_quotes"),
    properties = list(
      alternative_names = list(
        type  = "array",
        items = list(type = "string")
      ),
      # The supporting quotes (same set the researcher provided) re-cited
      # so the model's claim is anchored. Each is a QuoteProvenance-style
      # citation.
      shared_quotes = list(
        type  = "array",
        items = .provocation_citation_schema()
      )
    )
  )
}

#' Schema for absent_voice provocations
#'
#' Returns a list of underrepresented segments (demographic / temporal /
#' linguistic). NOT extractive citations -- this category is observational
#' rather than evidence-based.
#' @keywords internal
.coding_schema_absent_voice <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("absent_segments"),
    properties = list(
      absent_segments = list(
        type  = "array",
        items = list(
          type = "object",
          additionalProperties = FALSE,
          required = list("dimension", "description", "reason"),
          properties = list(
            dimension   = list(type = "string",
                               enum = list("demographic", "temporal",
                                           "linguistic", "topical")),
            description = list(type = "string"),
            reason      = list(type = "string")
          )
        )
      )
    )
  )
}

#' Schema for assumption_surfacing provocations
#'
#' Returns alternative terms participants use + a term the researcher's
#' framing erases.
#' @keywords internal
.coding_schema_assumption_surfacing <- function() {
  list(
    type                  = "object",
    additionalProperties  = FALSE,
    required              = list("alternative_terms", "erased_terms"),
    properties = list(
      alternative_terms = list(
        type  = "array",
        items = list(
          type = "object",
          additionalProperties = FALSE,
          required = list("term", "example_entry_id", "exact_text"),
          properties = list(
            term             = list(type = "string"),
            example_entry_id = list(type = "string"),
            exact_text       = list(type = "string")
          )
        )
      ),
      erased_terms = list(
        type  = "array",
        items = list(
          type = "object",
          additionalProperties = FALSE,
          required = list("term", "implication"),
          properties = list(
            term        = list(type = "string"),
            implication = list(type = "string")
          )
        )
      )
    )
  )
}

# ==============================================================================
# Helpers shared by category functions
# ==============================================================================

#' Coerce parse_json_safely output into a list-of-lists
#'
#' jsonlite simplifies a JSON array of uniform-shape objects into a
#' data.frame; iterating with \code{for (cit in df)} then walks COLUMNS
#' (atomic vectors), which breaks the per-citation \code{cit$entry_id}
#' access. This helper normalizes both the data.frame and named-list
#' shapes back to a list-of-lists where each element is one citation.
#' @keywords internal
.normalize_provocations_payload <- function(payload) {
  if (is.null(payload) || length(payload) == 0L) return(list())
  if (is.data.frame(payload)) {
    return(lapply(seq_len(nrow(payload)),
                  function(i) as.list(payload[i, , drop = FALSE])))
  }
  if (is.list(payload) && !is.null(names(payload)) &&
      "entry_id" %in% names(payload)) {
    # jsonlite returned a single named list (one citation collapsed)
    return(list(payload))
  }
  payload
}

#' Build a list of supporting-entry summaries to pass to the AI
#'
#' Used by category functions that need to show the model "here are
#' the entries the researcher believes support theme X". Returns a
#' compact one-line-per-entry string suitable for prompt injection.
#' @keywords internal
.build_theme_supporting_entries <- function(theme_entries, max_chars = 400) {
  if (nrow(theme_entries) == 0L) return("(no supporting entries)")
  text_col <- if ("original_text" %in% names(theme_entries))
                "original_text" else "std_text"
  lines <- vapply(seq_len(nrow(theme_entries)), function(i) {
    txt <- theme_entries[[text_col]][i]
    txt <- substr(txt, 1, max_chars)
    sprintf("- entry_id: %s\n  text: \"%s\"",
            theme_entries$std_id[i], txt)
  }, character(1))
  paste(lines, collapse = "\n")
}

#' Build a bounded candidate pool of NON-theme corpus entries for the
#' evidence-based challenge categories
#'
#' counter_narrative and disconfirming_evidence ask the model to find
#' entries that complicate or contradict a theme. Showing it only the
#' theme's OWN supporting entries makes that structurally impossible --
#' the model cannot cite corpus evidence it never sees. This helper
#' injects a bounded sample of entries OUTSIDE the theme's supporting set
#' so real counter-evidence is reachable; the existing verification flow
#' needs no changes because .citation_to_provocation resolves any cited
#' entry_id against the FULL corpus.
#'
#' The sample is DETERMINISTIC (fixed seed via .with_seed, which restores
#' the caller's RNG state): identical inputs produce identical prompts,
#' keeping the response cache effective and runs reproducible. When the
#' pool exceeds \code{max_entries}, the cap is disclosed to the coverage
#' card via the \code{n_candidate_entries_prompt_cap} field.
#'
#' @param data Tibble: the full corpus (std_id + std_text).
#' @param theme_entries Tibble: the theme's supporting entries.
#' @param max_entries Integer cap on candidates injected per prompt.
#' @param max_chars Per-entry text truncation for the prompt block.
#' @return Compact one-line-per-entry string for prompt injection;
#'   \code{"(no candidate entries -- every corpus entry supports this
#'   theme)"} when the pool is empty.
#' @keywords internal
.build_candidate_counter_entries <- function(data, theme_entries,
                                              max_entries = 25L,
                                              max_chars = 400L) {
  pool <- data[!data$std_id %in% theme_entries$std_id, , drop = FALSE]
  if (nrow(pool) == 0L) {
    return("(no candidate entries -- every corpus entry supports this theme)")
  }
  if (nrow(pool) > max_entries) {
    keep <- .with_seed(20260609L, sample.int(nrow(pool), max_entries))
    pool <- pool[sort(keep), , drop = FALSE]
  }
  text_col <- if ("original_text" %in% names(pool)) "original_text" else "std_text"
  lines <- vapply(seq_len(nrow(pool)), function(i) {
    txt <- substr(pool[[text_col]][i], 1, max_chars)
    sprintf("- entry_id: %s\n  text: \"%s\"", pool$std_id[i], txt)
  }, character(1))
  paste(lines, collapse = "\n")
}

#' Convert one parsed citation row to a Provocation
#'
#' Takes the AI's `{entry_id, char_start, char_end, exact_text, reason}`
#' citation, looks up the source text from \code{data}, builds a
#' QuoteProvenance, runs verify_quote (T0.1 universal), and assembles
#' a Provocation. Returns NULL when the citation is fabricated -- per
#' AC7, fabricated provocations are dropped silently from the
#' provocation list (and logged to the audit log if supplied).
#' @keywords internal
.citation_to_provocation <- function(cit, theme_name, category, data,
                                       ai_meta, audit_log = NULL,
                                       fabrication_log = NULL) {
  entry_id <- as.character(cit$entry_id %||% "")[1L]
  if (!nzchar(entry_id)) return(NULL)

  src_row <- data[data$std_id == entry_id, ]
  if (nrow(src_row) == 0L) {
    log_warn("Provocation cites unknown entry_id '{entry_id}'; dropping.")
    return(NULL)
  }
  src_text <- src_row$std_text[1L]

  # Defensively coerce offsets and bound them
  char_start <- suppressWarnings(as.integer(cit$char_start %||% NA_integer_)[1L])
  char_end   <- suppressWarnings(as.integer(cit$char_end   %||% NA_integer_)[1L])
  exact_text <- as.character(cit$exact_text %||% "")[1L]
  reason     <- as.character(cit$reason     %||% "")[1L]

  if (is.na(char_start) || char_start < 0L) char_start <- 0L
  if (is.na(char_end) || char_end <= char_start) {
    char_end <- char_start + max(1L, nchar(exact_text))
  }

  q <- make_quote(
    source_doc_id      = entry_id,
    source_doc_type    = "data_entry",
    source_text        = src_text,
    start_char         = char_start,
    end_char           = char_end,
    exact_text         = exact_text,
    attributed_theme_id = theme_name,
    ai_model           = ai_meta$model %||% NA_character_,
    ai_call_id         = ai_meta$call_id %||% NA_character_,
    citation_source    = "model_freeform"
  )
  q <- verify_quote(q, src_text)

  if (identical(q$verification_status, "fabricated")) {
    log_fabrication(fabrication_log, q)
    if (!is.null(audit_log)) {
      # Mode 1 provocateur fabrications now thread failure_reason
      # through the audit decision, matching the Mode 2 + Mode 3
      # coding fabrication-attribution behavior. Earlier, provocateur
      # fabrications were indistinguishable from other records.
      log_ai_decision(audit_log, "quote_verification", "quote_fabricated",
                      entry_id   = entry_id,
                      theme_name = theme_name,
                      quote_id   = q$quote_id,
                      ai_call_id = q$ai_call_id %||% NA_character_,
                      provocation_category = category,
                      exact_text = substr(exact_text, 1, 200),
                      failure_reason = q$verification_failure_reason
                                         %||% NA_character_)
    }
    log_warn("Provocation [{category}] for theme '{theme_name}' cited a fabricated quote; dropped.")
    return(NULL)
  }

  # Verified -- emit the quote_verified counterpart of the fabricated
  # record above, so Mode 1 run dirs get matching denominator events for
  # the transparency report's fabrication rate. Guarded: audit_log
  # defaults to NULL here and log_ai_decision rejects NULL. (Drifted is
  # unreachable in this path -- make_quote hashes the same src_text that
  # verify_quote re-hashes -- but gate on the verified statuses anyway.)
  if (!is.null(audit_log) &&
      q$verification_status %in% c("verified_exact", "verified_fuzzy")) {
    log_ai_decision(audit_log, "quote_verification", "quote_verified",
                    entry_id   = entry_id,
                    theme_name = theme_name,
                    quote_id   = q$quote_id,
                    ai_call_id = q$ai_call_id %||% NA_character_,
                    provocation_category = category,
                    verification_status = q$verification_status,
                    verification_method = q$verification_method
                                            %||% NA_character_)
  }

  make_provocation(
    category    = category,
    theme_name  = theme_name,
    reason      = reason,
    provenance  = q,
    ai_model    = ai_meta$model %||% NA_character_,
    ai_call_id  = ai_meta$call_id %||% NA_character_
  )
}

# ==============================================================================
# Five provocation category functions (M1.2)
# ==============================================================================

#' Counter-narrative provocation
#'
#' Given researcher-supplied theme name + supporting entries, AI returns
#' up to \code{n} entries that frame the same construct as not-Y, drawn
#' from the supporting entries plus a bounded, deterministic sample of
#' non-theme corpus entries injected into the prompt (the model sees only
#' what the prompt contains -- it has no retrieval access to the rest of
#' the corpus). Per Sarkar 2024 / patterns doc: extractive only -- the
#' model returns entries (not arguments), and a one-sentence reason per
#' entry.
#'
#' @param theme_name Character: the theme to challenge.
#' @param theme_entries Tibble: entries the researcher believes support
#'   the theme (must have std_id and std_text).
#' @param data Tibble: the full corpus. Used to resolve and verify cited
#'   entry_ids (T0.1) and to draw the bounded candidate sample of
#'   non-theme entries for the counter-evidence categories; the model is
#'   NOT given the full corpus to search.
#' @param provider AIProvider object.
#' @param n Integer: maximum provocations to return (default 5).
#' @param audit_log Optional AuditLog.
#' @param response_cache Optional ResponseCache.
#' @param fabrication_log Optional FabricationLog.
#' @return List of \code{Provocation} objects (verified, non-fabricated).
#' @export
provoke_counter_narrative <- function(theme_name, theme_entries, data, provider,
                                        n = 5L,
                                        audit_log = NULL,
                                        response_cache = NULL,
                                        fabrication_log = NULL) {
  validate_class(provider, "AIProvider")

  prompt <- paste0(
    "You are a research provocateur. The researcher believes theme \"",
    theme_name, "\" is supported by the entries below. ",
    "Search the candidate corpus entries below for up to ", n,
    " entries that, in the ",
    "researcher's own framing, would be the strongest COUNTER-NARRATIVE ",
    "-- entries that, if read first, would lead a researcher to a ",
    "different theme name.\n\n",
    "Researcher's supporting entries:\n",
    .build_theme_supporting_entries(theme_entries),
    "\n\n",
    "Candidate corpus entries (cite ONLY from the entries shown in this ",
    "prompt):\n",
    .build_candidate_counter_entries(data, theme_entries),
    "\n\n",
    "Return JSON: {provocations: [{entry_id, char_start, char_end, ",
    "exact_text, reason}]}. exact_text must be a verbatim slice of the ",
    "entry's text; char_start/char_end must resolve to that exact slice. ",
    "If no qualifying counter-entries exist, return {\"provocations\": []}. ",
    "Do not interpret. Do not synthesize. Do not propose a new theme name."
  )

  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model   <- NA_character_
  ai_meta$call_id <- NA_character_

  ai_result <- tryCatch({
    r <- ai_complete(provider, prompt,
                      task = "review",
                      response_schema = .coding_schema_counter_narrative())
    ai_meta$model   <- r$model      %||% NA_character_
    ai_meta$call_id <- r$request_id %||% NA_character_
    if (!is.null(audit_log)) {
      log_ai_request(audit_log, "provocateur", r, response_cache,
                      provocation_category = "counter_narrative",
                      theme_name = theme_name)
    }
    r
  }, error = function(e) {
    log_warn("provoke_counter_narrative for '{theme_name}' failed: {e$message}")
    NULL
  })

  if (is.null(ai_result)) return(list())
  parsed <- parse_json_safely(ai_result$content, expected_key = "provocations")
  if (is.null(parsed) || is.null(parsed$provocations)) return(list())

  provs <- list()
  for (cit in .normalize_provocations_payload(parsed$provocations)) {
    p <- .citation_to_provocation(cit, theme_name, "counter_narrative",
                                    data, ai_meta,
                                    audit_log = audit_log,
                                    fabrication_log = fabrication_log)
    if (!is.null(p)) provs[[length(provs) + 1L]] <- p
  }
  if (length(provs) > n) provs <- provs[seq_len(n)]
  provs
}

#' Disconfirming-evidence provocation
#'
#' Find up to n entries that most strongly contradict theme X, drawn from
#' the entries shown in the prompt (supporting entries plus the bounded
#' candidate sample of non-theme corpus entries). Same extractive shape
#' as counter_narrative.
#' @inheritParams provoke_counter_narrative
#' @export
provoke_disconfirming_evidence <- function(theme_name, theme_entries, data,
                                             provider, n = 5L,
                                             audit_log = NULL,
                                             response_cache = NULL,
                                             fabrication_log = NULL) {
  validate_class(provider, "AIProvider")

  prompt <- paste0(
    "You are a research provocateur. From the candidate corpus entries ",
    "below, find the ", n, " entries ",
    "that most strongly CONTRADICT theme \"", theme_name, "\". Return ",
    "verbatim citations with one-sentence reasons. Do not interpret or argue.\n\n",
    "Researcher's supporting entries (for theme context):\n",
    .build_theme_supporting_entries(theme_entries),
    "\n\n",
    "Candidate corpus entries (cite ONLY from the entries shown in this ",
    "prompt):\n",
    .build_candidate_counter_entries(data, theme_entries),
    "\n\n",
    "Return JSON: {provocations: [{entry_id, char_start, char_end, ",
    "exact_text, reason}]}. exact_text must be a verbatim slice of the ",
    "entry's text. If no qualifying entries exist, return {\"provocations\": []}."
  )

  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model   <- NA_character_
  ai_meta$call_id <- NA_character_

  ai_result <- tryCatch({
    r <- ai_complete(provider, prompt,
                      task = "review",
                      response_schema = .coding_schema_disconfirming_evidence())
    ai_meta$model   <- r$model      %||% NA_character_
    ai_meta$call_id <- r$request_id %||% NA_character_
    if (!is.null(audit_log)) {
      log_ai_request(audit_log, "provocateur", r, response_cache,
                      provocation_category = "disconfirming_evidence",
                      theme_name = theme_name)
    }
    r
  }, error = function(e) {
    log_warn("provoke_disconfirming_evidence for '{theme_name}' failed: {e$message}")
    NULL
  })

  if (is.null(ai_result)) return(list())
  parsed <- parse_json_safely(ai_result$content, expected_key = "provocations")
  if (is.null(parsed) || is.null(parsed$provocations)) return(list())

  provs <- list()
  for (cit in .normalize_provocations_payload(parsed$provocations)) {
    p <- .citation_to_provocation(cit, theme_name, "disconfirming_evidence",
                                    data, ai_meta,
                                    audit_log = audit_log,
                                    fabrication_log = fabrication_log)
    if (!is.null(p)) provs[[length(provs) + 1L]] <- p
  }
  if (length(provs) > n) provs <- provs[seq_len(n)]
  provs
}

#' Alternative-interpretation provocation
#'
#' Given theme name + 3 supporting quotes, generate methodologically
#' defensible alternative theme names that the same quotes could
#' support. The model does NOT say which is better.
#' @inheritParams provoke_counter_narrative
#' @param n_alternatives Integer: how many alternative names to return
#'   (default 2 per Drosos/Sarkar 2025).
#' @export
provoke_alternative_interpretation <- function(theme_name, theme_entries, data,
                                                 provider, n_alternatives = 2L,
                                                 audit_log = NULL,
                                                 response_cache = NULL,
                                                 fabrication_log = NULL) {
  validate_class(provider, "AIProvider")

  prompt <- paste0(
    "You are a research provocateur. The researcher named theme \"",
    theme_name, "\" based on the entries below. Generate ",
    n_alternatives, " methodologically-defensible ALTERNATIVE THEME NAMES ",
    "that the same entries could support. ",
    "Do NOT say which is better. Do NOT interpret. Just propose alternatives.\n\n",
    "Researcher's supporting entries:\n",
    .build_theme_supporting_entries(theme_entries),
    "\n\n",
    "Return JSON: {alternative_names: [\"...\", \"...\"], ",
    "shared_quotes: [{entry_id, char_start, char_end, exact_text, reason}]}. ",
    "shared_quotes is the same supporting set re-cited as verbatim slices ",
    "(this anchors the alternatives in the same evidence). ",
    "Do NOT add interpretation; the `reason` field on each shared_quote ",
    "should be a brief note on which alternative each quote supports."
  )

  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model   <- NA_character_
  ai_meta$call_id <- NA_character_

  ai_result <- tryCatch({
    r <- ai_complete(provider, prompt,
                      task = "review",
                      response_schema = .coding_schema_alternative_interpretation())
    ai_meta$model   <- r$model      %||% NA_character_
    ai_meta$call_id <- r$request_id %||% NA_character_
    if (!is.null(audit_log)) {
      log_ai_request(audit_log, "provocateur", r, response_cache,
                      provocation_category = "alternative_interpretation",
                      theme_name = theme_name)
    }
    r
  }, error = function(e) {
    log_warn("provoke_alternative_interpretation for '{theme_name}' failed: {e$message}")
    NULL
  })

  if (is.null(ai_result)) return(list())
  parsed <- parse_json_safely(ai_result$content)
  if (is.null(parsed)) return(list())

  alternatives <- as.character(unlist(parsed$alternative_names %||% list()))
  shared_cits  <- .normalize_provocations_payload(
    parsed$shared_quotes %||% list()
  )

  # Each alternative becomes a Provocation; the shared quotes anchor all
  # of them. Attach the first verifiable shared_quote as the
  # provenance for each alternative (so each Provocation has at least
  # one cited piece of evidence).
  anchor <- NULL
  for (cit in shared_cits) {
    a <- .citation_to_provocation(cit, theme_name, "alternative_interpretation",
                                    data, ai_meta,
                                    audit_log = audit_log,
                                    fabrication_log = fabrication_log)
    if (!is.null(a)) { anchor <- a; break }
  }
  # Alternative names are conceptually independent of any single re-cited
  # quote: when no anchor verifies, the substantive challenge (the rival
  # NAMES) must not be silently erased over a citation technicality. Emit
  # them with NULL provenance (the absent_voice precedent) and an explicit
  # flag so the report can show them as unanchored.
  if (is.null(anchor) && length(alternatives) > 0L) {
    log_warn(paste0("alternative_interpretation for '{theme_name}': no shared ",
                    "quote re-citation verified; emitting alternative names ",
                    "without an anchor quote."))
  }

  provs <- list()
  for (alt in alternatives) {
    if (!nzchar(alt)) next
    p <- make_provocation(
      category   = "alternative_interpretation",
      theme_name = theme_name,
      reason     = sprintf("Alternative name: '%s'", alt),
      provenance = if (!is.null(anchor)) anchor$provenance else NULL,
      extra      = list(alternative_name = alt,
                        anchor_quote_verified = !is.null(anchor)),
      ai_model   = ai_meta$model %||% NA_character_,
      ai_call_id = ai_meta$call_id %||% NA_character_
    )
    provs[[length(provs) + 1L]] <- p
  }
  provs
}

#' Absent-voice provocation
#'
#' List demographic / temporal / linguistic / topical segments of the
#' corpus that are underrepresented in the entries supporting theme X.
#' Observational rather than evidence-based; no exact_text citations
#' (the model is reasoning ABOUT absences, not quoting present voices).
#' Returns Provocation objects with NULL provenance and the dimension
#' info in the \code{extra} field.
#' @inheritParams provoke_counter_narrative
#' @export
provoke_absent_voice <- function(theme_name, theme_entries, data, provider,
                                   n = 5L,
                                   audit_log = NULL,
                                   response_cache = NULL,
                                   fabrication_log = NULL) {
  validate_class(provider, "AIProvider")

  has_authors <- "std_author" %in% names(data)
  has_timestamps <- "std_timestamp" %in% names(data)
  has_source <- "source_table" %in% names(data)

  context_lines <- c(
    sprintf("Total entries in corpus: %d", nrow(data)),
    sprintf("Entries the researcher used to support this theme: %d",
            nrow(theme_entries)),
    if (has_authors) sprintf("Distinct contributors in corpus: %d",
                              length(unique(stats::na.omit(data$std_author))))
    else "(no author data)",
    if (has_authors) sprintf("Distinct contributors in theme support: %d",
                              length(unique(stats::na.omit(theme_entries$std_author))))
    else "",
    if (has_timestamps) sprintf("Time span in corpus: %s..%s",
                                  min(data$std_timestamp, na.rm = TRUE),
                                  max(data$std_timestamp, na.rm = TRUE))
    else "",
    if (has_source) sprintf("Sources in corpus: %s",
                              paste(sort(unique(data$source_table)),
                                    collapse = ", "))
    else ""
  )
  context_lines <- context_lines[nzchar(context_lines)]

  prompt <- paste0(
    "You are a research provocateur. The researcher's theme \"",
    theme_name, "\" is built from a subset of the corpus. List up to ",
    n, " segments of the corpus that are UNDERREPRESENTED in the ",
    "entries supporting this theme. Categories: demographic ",
    "(contributor profile), temporal (time period), linguistic ",
    "(phrasing patterns), topical (related but excluded sub-topics). ",
    "Don't speculate about WHY -- just identify the gaps.\n\n",
    "Corpus context:\n",
    paste(paste0("- ", context_lines), collapse = "\n"),
    "\n\n",
    "Researcher's supporting entries (for theme context):\n",
    .build_theme_supporting_entries(theme_entries),
    "\n\n",
    "Return JSON: {absent_segments: [{dimension, description, reason}]}. ",
    "If no clear absences, return {\"absent_segments\": []}."
  )

  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model   <- NA_character_
  ai_meta$call_id <- NA_character_

  ai_result <- tryCatch({
    r <- ai_complete(provider, prompt,
                      task = "review",
                      response_schema = .coding_schema_absent_voice())
    ai_meta$model   <- r$model      %||% NA_character_
    ai_meta$call_id <- r$request_id %||% NA_character_
    if (!is.null(audit_log)) {
      log_ai_request(audit_log, "provocateur", r, response_cache,
                      provocation_category = "absent_voice",
                      theme_name = theme_name)
    }
    r
  }, error = function(e) {
    log_warn("provoke_absent_voice for '{theme_name}' failed: {e$message}")
    NULL
  })

  if (is.null(ai_result)) return(list())
  parsed <- parse_json_safely(ai_result$content)
  if (is.null(parsed)) return(list())

  segments <- .normalize_provocations_payload(parsed$absent_segments %||% list())
  provs <- list()
  for (s in segments) {
    if (!is.list(s)) next
    dim_v  <- as.character(s$dimension %||% "")[1L]
    desc_v <- as.character(s$description %||% "")[1L]
    reason_v <- as.character(s$reason %||% "")[1L]
    if (!nzchar(desc_v)) next
    p <- make_provocation(
      category   = "absent_voice",
      theme_name = theme_name,
      reason     = sprintf("[%s] %s -- %s", dim_v, desc_v, reason_v),
      provenance = NULL,  # absent_voice is observational, not extractive
      extra      = list(dimension = dim_v, description = desc_v),
      ai_model   = ai_meta$model %||% NA_character_,
      ai_call_id = ai_meta$call_id %||% NA_character_
    )
    provs[[length(provs) + 1L]] <- p
  }
  if (length(provs) > n) provs <- provs[seq_len(n)]
  provs
}

#' Assumption-surfacing provocation
#'
#' Given the researcher's theme name + a key term they use, list other
#' terms participants in the corpus use for the same construct (with
#' citations) and identify a term participants use that the researcher's
#' framing erases.
#' @inheritParams provoke_counter_narrative
#' @param key_term Character: the researcher's term to challenge (e.g.,
#'   "binge-eating"). Defaults to the theme name itself.
#' @param n_alternatives Integer: how many alternative-term provocations
#'   to request from the model (default 3).
#' @export
provoke_assumption_surfacing <- function(theme_name, theme_entries, data,
                                            provider,
                                            key_term = NULL,
                                            n_alternatives = 3L,
                                            audit_log = NULL,
                                            response_cache = NULL,
                                            fabrication_log = NULL) {
  validate_class(provider, "AIProvider")
  if (is.null(key_term) || !nzchar(key_term)) key_term <- theme_name

  prompt <- paste0(
    "You are a research provocateur. The researcher's framing of theme ",
    "\"", theme_name, "\" centers on the term \"", key_term, "\". ",
    "List up to ", n_alternatives, " OTHER terms used by participants ",
    "in the corpus to refer to the same construct. Each must include ",
    "a verbatim citation. Then list ONE term that participants in the ",
    "corpus use that the researcher's framing of \"", key_term, "\" erases ",
    "(don't quote -- just identify the term and the implication).\n\n",
    "Researcher's supporting entries:\n",
    .build_theme_supporting_entries(theme_entries),
    "\n\n",
    "Return JSON: {",
    "alternative_terms: [{term, example_entry_id, exact_text}], ",
    "erased_terms: [{term, implication}]}. exact_text on alternative_terms ",
    "must be a verbatim slice of the cited entry."
  )

  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model   <- NA_character_
  ai_meta$call_id <- NA_character_

  ai_result <- tryCatch({
    r <- ai_complete(provider, prompt,
                      task = "review",
                      response_schema = .coding_schema_assumption_surfacing())
    ai_meta$model   <- r$model      %||% NA_character_
    ai_meta$call_id <- r$request_id %||% NA_character_
    if (!is.null(audit_log)) {
      log_ai_request(audit_log, "provocateur", r, response_cache,
                      provocation_category = "assumption_surfacing",
                      theme_name = theme_name)
    }
    r
  }, error = function(e) {
    log_warn("provoke_assumption_surfacing for '{theme_name}' failed: {e$message}")
    NULL
  })

  if (is.null(ai_result)) return(list())
  parsed <- parse_json_safely(ai_result$content)
  if (is.null(parsed)) return(list())

  provs <- list()

  # Alternative terms: each becomes a Provocation with a citation
  for (alt in .normalize_provocations_payload(parsed$alternative_terms %||% list())) {
    if (!is.list(alt)) next
    term_v <- as.character(alt$term %||% "")[1L]
    if (!nzchar(term_v)) next
    cit <- list(
      entry_id   = alt$example_entry_id,
      char_start = 0L,  # offsets unknown -- ladder substring search corrects
      char_end   = max(1L, nchar(alt$exact_text %||% "")),
      exact_text = alt$exact_text,
      reason     = sprintf("Alternative term: '%s'", term_v)
    )
    p <- .citation_to_provocation(cit, theme_name, "assumption_surfacing",
                                    data, ai_meta,
                                    audit_log = audit_log,
                                    fabrication_log = fabrication_log)
    if (!is.null(p)) {
      p$extra$alternative_term <- term_v
      provs[[length(provs) + 1L]] <- p
    }
  }

  # Erased terms: observational; no citation. Each becomes a NULL-provenance
  # Provocation with the term + implication in extra.
  for (er in .normalize_provocations_payload(parsed$erased_terms %||% list())) {
    if (!is.list(er)) next
    term_v <- as.character(er$term %||% "")[1L]
    impl_v <- as.character(er$implication %||% "")[1L]
    if (!nzchar(term_v)) next
    p <- make_provocation(
      category   = "assumption_surfacing",
      theme_name = theme_name,
      reason     = sprintf("Erased term: '%s' -- %s", term_v, impl_v),
      provenance = NULL,
      extra      = list(erased_term = term_v, implication = impl_v),
      ai_model   = ai_meta$model %||% NA_character_,
      ai_call_id = ai_meta$call_id %||% NA_character_
    )
    provs[[length(provs) + 1L]] <- p
  }
  provs
}

# ==============================================================================
# Orchestrator: run_provocateur_questioning (M1.1)
# ==============================================================================

#' Run provocateur questioning across themes (Mode 1 entry point)
#'
#' Orchestrates the five (or selected) provocation categories per theme,
#' assembling a \code{ResearcherReflectionLog} that captures every
#' provocation issued. The log is the AI's contribution to a Mode 1
#' analysis -- the THEMES + CODES are the researcher's authorship, kept
#' on the same log object as \code{researcher_authored_codes /
#' researcher_authored_themes}.
#'
#' Per AC7 (universal Tier-0): every provocation that cites verbatim
#' evidence runs through \code{verify_quote}; fabricated provocations
#' are dropped silently and recorded to the audit + fabrication logs.
#'
#' @param data Tibble with std_id + std_text (standardized corpus).
#' @param theme_set ThemeSet object. Each theme drives one round of
#'   provocations. The researcher must have authored these themes; the
#'   provocateur does NOT name themes.
#' @param provider AIProvider object.
#' @param config Optional config list (used for logging/context).
#' @param categories Character vector of category names to run (default:
#'   all five).
#' @param audit_log Optional AuditLog from \code{init_audit_log}.
#' @param response_cache Optional ResponseCache.
#' @param fabrication_log Optional FabricationLog.
#' @param resume_log Optional \code{ResearcherReflectionLog} to append
#'   to (resume semantics).
#' @return A \code{ResearcherReflectionLog} with provocations populated.
#' @export
run_provocateur_questioning <- function(data, theme_set, provider,
                                          config = list(),
                                          categories = .VALID_PROVOCATION_CATEGORIES,
                                          audit_log = NULL,
                                          response_cache = NULL,
                                          fabrication_log = NULL,
                                          resume_log = NULL) {
  validate_class(theme_set, "ThemeSet")
  validate_class(provider, "AIProvider")
  bad <- setdiff(categories, .VALID_PROVOCATION_CATEGORIES)
  if (length(bad) > 0L) {
    stop(sprintf(
      "Unknown provocation categories: %s. Valid: %s",
      paste(bad, collapse = ", "),
      paste(.VALID_PROVOCATION_CATEGORIES, collapse = ", ")
    ), call. = FALSE)
  }

  log <- if (!is.null(resume_log) && inherits(resume_log,
                                                "ResearcherReflectionLog")) {
    resume_log
  } else {
    create_reflection_log(config_hash = config$config_hash %||% NULL)
  }

  # T0.3 (Mode 1): a 1.0.0 reflection log loaded as a resume_log will
  # not have the attempt / skip slots; backfill them so coverage compute
  # against the resumed log doesn't crash. Empty data frames here mean
  # "no recorded attempts yet" -- the upcoming loop will append.
  if (is.null(log$provocation_attempts)) {
    log$provocation_attempts <- data.frame(
      theme_name   = character(0),
      category     = character(0),
      n_emitted    = integer(0),
      attempted_at = character(0),
      stringsAsFactors = FALSE
    )
  }
  if (is.null(log$skipped_themes)) {
    log$skipped_themes <- data.frame(
      theme_name = character(0),
      reason     = character(0),
      skipped_at = character(0),
      stringsAsFactors = FALSE
    )
  }

  log_info("Provocateur: running {length(categories)} categor(ies) across {length(theme_set$themes)} theme(s)")

  # Mode 1's contract is that the supplied data
  # carries entries-to-themes mapping in EITHER (a) one or more
  # theme_membership_<safe_name> indicator columns or (b) an
  # emerged_themes character column with semicolon-separated theme
  # names. If neither shape is present, every theme will look "empty"
  # and the user gets a per-theme "no supporting entries" warning that
  # masks the real problem (missing input shape). Detect that up front
  # so the user gets one actionable message, not N silent skips.
  any_membership_col <- any(grepl("^theme_membership_", names(data)))
  has_emerged_themes <- "emerged_themes" %in% names(data)
  if (!any_membership_col && !has_emerged_themes) {
    log_warn(paste0(
      "Provocateur: input data has no `theme_membership_<safe_name>` columns ",
      "AND no `emerged_themes` column. Mode 1 maps entries to themes via one ",
      "of these shapes -- without them, every theme will appear to have no ",
      "supporting entries. Consumers typically pass `all_entries_by_theme.csv` ",
      "from a prior Mode 2 run, or build emerged_themes from their own coding ",
      "tool's exports."
    ))
  }

  for (t in theme_set$themes) {
    tn <- t$name
    # Resume idempotency (T0.3): a theme already recorded as skipped in a prior
    # run must not be re-evaluated -- re-running would duplicate its
    # skipped_themes row and corrupt the coverage accounting.
    if (nrow(log$skipped_themes) > 0L && tn %in% log$skipped_themes$theme_name) next
    safe_col <- paste0("theme_membership_", make.names(tn))
    if (safe_col %in% names(data)) {
      theme_entries <- data[data[[safe_col]] == 1L, ]
    } else if ("emerged_themes" %in% names(data)) {
      theme_entries <- data[.entry_in_theme(data$emerged_themes, tn), ]
    } else {
      theme_entries <- data[0, ]
    }

    if (nrow(theme_entries) == 0L) {
      # Distinguish the two possible reasons: input shape missing entirely
      # vs theme genuinely empty (this theme's name doesn't appear anywhere
      # in the input mapping).
      reason <- if (!any_membership_col && !has_emerged_themes) {
        "missing_membership_input"
      } else {
        "no_supporting_entries"
      }
      log_warn("Provocateur: theme '{tn}' has no supporting entries; skipping (reason: {reason}).")
      log$skipped_themes <- rbind(
        log$skipped_themes,
        data.frame(
          theme_name = tn,
          reason     = reason,
          skipped_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
          stringsAsFactors = FALSE
        )
      )
      next
    }

    for (cat in categories) {
      # Resume idempotency (T0.3): skip a (theme, category) pair already
      # attempted in a prior run. Re-running would append a duplicate attempt
      # row -- inflating n_attempts_recorded past n_attempts_expected and
      # flipping attempts_complete / no_silent_skip to FALSE -- and waste AI
      # spend. Resume is therefore incremental: only not-yet-attempted pairs
      # run, and the seeded provocations/attempts are preserved as-is.
      if (nrow(log$provocation_attempts) > 0L &&
          any(log$provocation_attempts$theme_name == tn &
              log$provocation_attempts$category == cat)) {
        next
      }
      provs <- switch(cat,
        "counter_narrative" = provoke_counter_narrative(
          tn, theme_entries, data, provider,
          audit_log = audit_log, response_cache = response_cache,
          fabrication_log = fabrication_log),
        "absent_voice" = provoke_absent_voice(
          tn, theme_entries, data, provider,
          audit_log = audit_log, response_cache = response_cache,
          fabrication_log = fabrication_log),
        "alternative_interpretation" = provoke_alternative_interpretation(
          tn, theme_entries, data, provider,
          audit_log = audit_log, response_cache = response_cache,
          fabrication_log = fabrication_log),
        "disconfirming_evidence" = provoke_disconfirming_evidence(
          tn, theme_entries, data, provider,
          audit_log = audit_log, response_cache = response_cache,
          fabrication_log = fabrication_log),
        "assumption_surfacing" = provoke_assumption_surfacing(
          tn, theme_entries, data, provider,
          audit_log = audit_log, response_cache = response_cache,
          fabrication_log = fabrication_log),
        list()
      )
      # T0.3 (Mode 1): record the attempt regardless of outcome. n_emitted
      # may legitimately be 0 (e.g., counter_narrative finds no qualifying
      # entries); the row's existence proves "not silently skipped." The
      # downstream coverage check distinguishes "attempts made for every
      # theme x category" from "every attempt produced provocations."
      log$provocation_attempts <- rbind(
        log$provocation_attempts,
        data.frame(
          theme_name   = tn,
          category     = cat,
          n_emitted    = as.integer(length(provs)),
          attempted_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
          stringsAsFactors = FALSE
        )
      )
      if (length(provs) > 0L) {
        log$provocations <- c(log$provocations, provs)
        if (!is.null(audit_log)) {
          for (p in provs) {
            tryCatch(
              log_ai_decision(audit_log, "provocateur", "provocation_emitted",
                              category   = p$category,
                              theme_name = p$theme_name,
                              entry_id   = if (!is.null(p$provenance))
                                              p$provenance$source_doc_id
                                            else NA_character_,
                              ai_call_id = p$ai_call_id %||% NA_character_),
              error = function(e) NULL
            )
          }
        }
        log_info("Provocateur [{cat}] for '{tn}': {length(provs)} provocation(s)")
      }
    }
  }

  log$last_updated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  log
}
