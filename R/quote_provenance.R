# ==============================================================================
# Quote Provenance + Verification Ladder
# ==============================================================================
# Every AI-attributed quote in pakhom must carry full provenance metadata
# AND be verified offline against the source corpus (in the pipeline path,
# the cleaned analytic std_text the model coded -- see preprocess_text's
# @details; the raw platform text is preserved in original_text). This is
# the package's
# direct response to the most-cited empirical critique of LLM-for-TA tools:
# Jowsey et al. 2025 (PLOS One, doi:10.1371/journal.pone.0330217) found that
# Microsoft Copilot fabricated quotes that appeared verbatim in the data but
# were not actually in any source -- the "Frankenstein quote" pattern.
#
# Anti-fabrication strategy:
# 1. Every quote object carries source_doc_id + char range + sha256 of the
#    source text used at attribution time. If the source corpus changes
#    between runs, drift is detectable (verification_status = "drifted").
# 2. A verification ladder runs when a quote is created (the coding path
#    verifies every coded-segment quote before it can enter the codebook).
#    The same ladder can be re-applied later via verify_quote()/verify_quotes()
#    -- e.g. for a corpus-drift re-check -- but the default pipeline verifies
#    once, at attribution time:
#      (a) strict offline string match
#      (b) normalized match (whitespace + smart-quote + NFC + case)
#      (c) substring search fallback (corrects offsets)
#      (d) embedding cosine similarity (paraphrase tolerance, threshold 0.85)
#          -- OPTIONAL: runs only when an embedding provider is passed to
#          verify_quote()/verify_quotes(); the default coding path uses (a)-(c)
#          (and is therefore stricter -- no paraphrase tolerance).
#    Each step downgrades verification_status if the previous fails. A quote
#    that fails every applicable step is "fabricated" and never rendered.
# 3. Fabricated quotes are written to outputs/<run>/fabrication_log.csv
#    for the methodology paper's KPI ("fabrication rate per N AI calls").
# 4. Future T0.1 phases add Anthropic Citations API custom content blocks
#    (model returns {quote_id, span_start, span_end} from pre-extracted
#    candidate spans, never asked to write quotes). This module is the
#    foundation those extensions consume.
#
# Schema versioning: .QUOTE_PROVENANCE_SCHEMA_VERSION lets downstream
# consumers (planned replay tooling, methodology paper analyses) detect when the quote
# object shape has evolved. Treat all bumps as additive unless explicitly
# noted; the verification ladder may add new methods without bumping.
# ==============================================================================

#' Current schema version for the quote provenance object
#'
#' \itemize{
#'   \item 1.0.0: base schema -- quote_id,
#'     source_doc_id, source_doc_type, source_text_sha256,
#'     start_char, end_char, exact_text, ai_paraphrase,
#'     attributed_theme_id, attributed_code_id, ai_model,
#'     ai_call_id, citation_source, verification_status,
#'     verification_method, verification_score, verified_at,
#'     schema_version.
#'   \item 1.1.0: adds
#'     \code{verification_failure_reason} -- structured attribution
#'     for fabricated / drifted quotes naming the deepest failed
#'     ladder step (step1_offset_mismatch, step2_normalized_mismatch,
#'     step3_substring_not_found, step4_embedding_below_threshold,
#'     source_text_sha256_mismatch, etc.). NA on verified or
#'     unverified quotes.
#' }
#' Bumping the version lets downstream tools (planned replay tooling, cross-run
#' compare_runs) detect that a loaded QuoteProvenance was produced
#' under a different schema generation.
#' @keywords internal
.QUOTE_PROVENANCE_SCHEMA_VERSION <- "1.1.0"

#' Embedding cosine similarity threshold for the verification ladder's step 4
#' Justified at 0.85 because thematic-analysis paraphrases that retain meaning
#' (as opposed to fabrications) cluster above this threshold in pilot tests.
#' Lower thresholds admit too many fabrications (false positives); higher
#' thresholds reject legitimate paraphrases (false negatives).
#' @keywords internal
.QUOTE_EMBEDDING_VERIFICATION_THRESHOLD <- 0.85

#' Valid verification statuses, ordered from most to least confident.
#' Render policy:
#'   verified_exact / verified_fuzzy -> render normally
#'   unverified                       -> render with warning marker
#'   drifted                          -> corpus-integrity warning at load time
#'   fabricated                       -> never rendered; logged
#' @keywords internal
.VALID_QUOTE_VERIFICATION_STATUSES <- c(
  "verified_exact", "verified_fuzzy", "unverified", "drifted", "fabricated"
)

#' Valid verification methods (which ladder step matched)
#' @keywords internal
.VALID_QUOTE_VERIFICATION_METHODS <- c(
  "string_match", "normalized_match", "substring_search",
  "embedding_cosine", "human_review",
  NA_character_  # method = NA when status = unverified or fabricated
)

#' Valid citation sources (where the quote came from)
#' @keywords internal
.VALID_QUOTE_CITATION_SOURCES <- c(
  "model_freeform",          # AI emitted free-form text claiming to be a quote
  "anthropic_citations_api", # AI used Anthropic's citations mechanism (T0.1 part 3)
  "human_supplied",          # researcher manually entered the quote
  "pipeline_derived"         # not AI-attributed; pipeline sliced from std_text
)

# ==============================================================================
# Constructor
# ==============================================================================

#' Construct a Quote provenance object
#'
#' Builds a structured quote with all provenance metadata fields populated
#' (or set to sensible NA defaults). The quote is created in the
#' \code{"unverified"} state; pass it through \code{\link{verify_quote}} to
#' run the verification ladder.
#'
#' \code{quote_id} is computed deterministically as
#' \code{paste0("qte_", sha1(source_doc_id + start_char + end_char + exact_text))}
#' so the same quote always has the same ID across runs -- critical for
#' replay and cross-run comparison.
#'
#' \code{source_text_sha256} is a hash of the FULL source document text at
#' attribution time. If the source corpus changes between runs (researcher
#' edits a post, re-scrapes data, etc.), \code{verify_quote} can detect
#' the drift by re-hashing.
#'
#' @param source_doc_id Character. Identifier of the source document
#'   (e.g., \code{"post_abc123"}, \code{"comment_def456"}). Pulled from
#'   \code{data$std_id} in the standard pipeline.
#' @param source_doc_type Character. Document type
#'   (e.g., \code{"reddit_post"}, \code{"reddit_comment"},
#'   \code{"interview_segment"}).
#' @param source_text Character. The FULL source document text. Used to
#'   compute \code{source_text_sha256}; not stored on the quote. In the
#'   pipeline path this is the cleaned analytic \code{std_text} the model
#'   coded (see \code{preprocess_text}), not the raw platform text.
#' @param start_char Integer. 0-indexed inclusive start offset (matches
#'   Anthropic Citations API conventions).
#' @param end_char Integer. 0-indexed exclusive end offset.
#' @param exact_text Character. The verbatim slice from the source
#'   (\code{substr(source_text, start_char + 1, end_char)}). Stored
#'   directly to avoid recomputing during rendering.
#' @param ai_paraphrase Optional character. AI's paraphrase of the quote,
#'   if any. \code{NA_character_} when no paraphrase exists.
#' @param attributed_theme_id Optional character. Theme ID this quote
#'   supports.
#' @param attributed_code_id Optional character. Code ID this quote
#'   supports.
#' @param ai_model Optional character. Model that produced the
#'   attribution (e.g., \code{"gpt-4o-2024-08-06"}).
#' @param ai_call_id Optional character. \code{request_id} of the AI call
#'   that produced the quote. Joins to the audit log's \code{request_id}.
#' @param citation_source One of \code{.VALID_QUOTE_CITATION_SOURCES}.
#'   Default \code{"pipeline_derived"} (the safest assumption when unknown).
#' @return A \code{QuoteProvenance} S3 object (a list with class).
#' @export
make_quote <- function(source_doc_id, source_doc_type, source_text,
                       start_char, end_char, exact_text,
                       ai_paraphrase = NA_character_,
                       attributed_theme_id = NA_character_,
                       attributed_code_id = NA_character_,
                       ai_model = NA_character_,
                       ai_call_id = NA_character_,
                       citation_source = "pipeline_derived") {
  stopifnot(
    is.character(source_doc_id), length(source_doc_id) == 1L,
    is.character(source_doc_type), length(source_doc_type) == 1L,
    is.character(source_text), length(source_text) == 1L,
    is.integer(start_char) || (is.numeric(start_char) && start_char == as.integer(start_char)),
    is.integer(end_char)   || (is.numeric(end_char)   && end_char   == as.integer(end_char)),
    is.character(exact_text), length(exact_text) == 1L,
    citation_source %in% .VALID_QUOTE_CITATION_SOURCES
  )
  start_char <- as.integer(start_char)
  end_char   <- as.integer(end_char)
  if (start_char < 0L) stop("start_char must be >= 0", call. = FALSE)
  if (end_char <= start_char) stop("end_char must be > start_char", call. = FALSE)

  source_text_sha256 <- digest::digest(source_text, algo = "sha256",
                                        serialize = FALSE)
  # quote_id is sha1 of the canonical positional fingerprint -- same source +
  # range + text always produces the same ID (replay relies on this).
  quote_id <- paste0("qte_", digest::digest(
    paste(source_doc_id, start_char, end_char, exact_text, sep = "|"),
    algo = "sha1", serialize = FALSE
  ))

  quote <- list(
    quote_id            = quote_id,
    source_doc_id       = source_doc_id,
    source_doc_type     = source_doc_type,
    source_text_sha256  = source_text_sha256,
    start_char          = start_char,
    end_char            = end_char,
    exact_text          = exact_text,
    ai_paraphrase       = ai_paraphrase,
    attributed_theme_id = attributed_theme_id,
    attributed_code_id  = attributed_code_id,
    ai_model            = ai_model,
    ai_call_id          = ai_call_id,
    citation_source     = citation_source,
    verification_status = "unverified",
    verification_method = NA_character_,
    verification_score  = NA_real_,
    # structured failure reason populated
    # when verification_status ends as "fabricated" or "drifted". Lets
    # the methodology paper attribute fabrications to specific ladder
    # failure modes (offset_mismatch / normalized_mismatch /
    # substring_not_found / embedding_below_threshold / source_drift_
    # all_ladder_failed). NA when status is verified_* or unverified.
    verification_failure_reason = NA_character_,
    verified_at         = NA_character_,
    schema_version      = .QUOTE_PROVENANCE_SCHEMA_VERSION
  )
  class(quote) <- "QuoteProvenance"
  quote
}

# ==============================================================================
# Anthropic Citations API bridge
# ==============================================================================
# Converts Anthropic Citations API output (extracted by
# R/02_ai_providers.R::.anthropic_extract_citations) into pakhom's
# QuoteProvenance schema. The model returned spans into provided source
# documents; this bridge looks up the actual source text via document_index
# and constructs a QuoteProvenance with citation_source =
# "anthropic_citations_api". The verification ladder still runs (defense in
# depth -- Anthropic guarantees the indices are valid pointers, but the
# ladder catches corpus drift, encoding issues, and the rare API edge case).
#
# Why this is the prevention layer (vs the detection layer):
# When the model uses Citations API, it cannot fabricate quote text -- it
# returns char_location indices into the source document, and Anthropic
# guarantees those indices are valid. Compare to free-form quote
# generation where the model can produce text that looks plausible but
# isn't in any source (the Frankenstein/Jowsey 2025 failure mode: 44.5% of
# Microsoft Copilot's supporting quotes were fabricated).
# ==============================================================================

#' Construct a QuoteProvenance from a single Anthropic citation
#'
#' Bridges one Anthropic citation object (the shape produced by
#' \code{R/02_ai_providers.R::.anthropic_extract_citations}) to a
#' \code{QuoteProvenance} object with \code{citation_source =
#' "anthropic_citations_api"}. The constructed quote is in the
#' \code{"unverified"} state; chain through \code{\link{verify_quote}}
#' to run the verification ladder.
#'
#' Supported citation types:
#' \itemize{
#'   \item \code{char_location} (plain text source) -- maps directly to
#'     \code{start_char}/\code{end_char}. The \code{cited_text} is stored
#'     as \code{exact_text}; the verification ladder confirms it matches
#'     \code{source[start_char:end_char]} byte-for-byte.
#'   \item \code{page_location} (PDF source) -- not yet supported. PDF
#'     inputs aren't part of pakhom's current data model. Errors with a
#'     clear message rather than silently producing a malformed quote.
#'   \item \code{content_block_location} (custom_content source) -- not
#'     yet supported for the same reason. The Anthropic path uses plain_text source
#'     exclusively; if a future caller switches to custom_content, the
#'     bridge needs a block-index-to-char-offset mapping (caller would
#'     supply per-document block boundaries).
#' }
#'
#' Document lookup: \code{citation$document_index} is 0-indexed into the
#' \code{documents} list passed to \code{ai_complete()}. The bridge
#' converts to 1-indexed for R.
#'
#' @param citation A single citation object from
#'   \code{ai_complete()$citations}. Must have \code{type},
#'   \code{document_index}, and the type-specific span fields populated.
#' @param documents The same documents list passed to
#'   \code{ai_complete()}. Each element must have \code{$id} and
#'   \code{$text}; an optional \code{$type} field overrides the default
#'   \code{source_doc_type}.
#' @param attributed_theme_id,attributed_code_id Optional attribution
#'   metadata. The bridge cannot infer these from the citation alone --
#'   the caller pairs each citation with the code/theme it supports.
#' @param ai_model,ai_call_id The model and request_id from the
#'   \code{ai_complete()} response that produced this citation. Stored
#'   on the QuoteProvenance for audit log linkage.
#' @param ai_paraphrase Optional paraphrase, if the AI rephrased rather
#'   than directly quoted. Defaults to \code{NA_character_} since
#'   Citations API returns verbatim slices.
#' @param source_doc_type_default Default \code{source_doc_type} when the
#'   document doesn't specify \code{$type}. Defaults to
#'   \code{"data_entry"} matching pakhom's coding pipeline convention.
#' @return A \code{QuoteProvenance} object (unverified state) with
#'   \code{citation_source = "anthropic_citations_api"}.
#' @export
make_quote_from_citation <- function(citation, documents,
                                      attributed_theme_id = NA_character_,
                                      attributed_code_id = NA_character_,
                                      ai_model = NA_character_,
                                      ai_call_id = NA_character_,
                                      ai_paraphrase = NA_character_,
                                      source_doc_type_default = "data_entry") {
  if (!is.list(citation) || is.null(citation$type)) {
    stop("`citation` must be a citation object with at least a `type` field",
         call. = FALSE)
  }
  if (!is.list(documents) || length(documents) == 0L) {
    stop("`documents` must be a non-empty list of source documents",
         call. = FALSE)
  }

  # Document lookup: 0-indexed -> 1-indexed
  doc_idx <- citation$document_index
  if (is.null(doc_idx) || is.na(doc_idx) ||
      doc_idx < 0L || doc_idx >= length(documents)) {
    stop(sprintf(
      "citation$document_index (%s) out of range for documents (length %d)",
      format(doc_idx), length(documents)
    ), call. = FALSE)
  }
  doc <- documents[[doc_idx + 1L]]
  if (is.null(doc$id) || is.null(doc$text)) {
    stop(sprintf(
      "documents[[%d]] missing $id or $text required for QuoteProvenance",
      doc_idx + 1L
    ), call. = FALSE)
  }
  source_doc_type <- doc$type %||% source_doc_type_default

  switch(citation$type,
    "char_location" = {
      # exact_text uses Anthropic's cited_text. Verification ladder still
      # runs at the caller's chaining step to confirm
      # source_text[start_char_index:end_char_index] == cited_text. Defense
      # in depth: Anthropic's server-side guarantee covers index validity;
      # the offline verification covers byte-identity (catches corpus
      # drift, encoding mismatch, and any API edge case).
      make_quote(
        source_doc_id      = doc$id,
        source_doc_type    = source_doc_type,
        source_text        = doc$text,
        start_char         = citation$start_char_index,
        end_char           = citation$end_char_index,
        exact_text         = citation$cited_text %||% NA_character_,
        ai_paraphrase      = ai_paraphrase,
        attributed_theme_id = attributed_theme_id,
        attributed_code_id  = attributed_code_id,
        ai_model           = ai_model,
        ai_call_id         = ai_call_id,
        citation_source    = "anthropic_citations_api"
      )
    },
    "page_location" = stop(
      "page_location citations (PDF inputs) are not yet supported by the ",
      "Anthropic citations bridge. pakhom uses plain_text source documents ",
      "exclusively; PDF citation handling requires page-to-char offset ",
      "mapping that hasn't been implemented.",
      call. = FALSE
    ),
    "content_block_location" = stop(
      "content_block_location citations (custom_content sources) are not yet ",
      "supported by the bridge. To use custom_content sources, supply a ",
      "block-index-to-char-offset mapping per document (not yet implemented).",
      call. = FALSE
    ),
    stop(sprintf(
      paste("Unknown Anthropic citation type: %s. Expected one of:",
            "char_location, page_location, content_block_location."),
      citation$type
    ), call. = FALSE)
  )
}

#' Construct QuoteProvenance objects from a list of Anthropic citations
#'
#' Batch convenience over \code{\link{make_quote_from_citation}} when all
#' citations share the same attribution metadata (typical for a single AI
#' call's output where one entry was passed and one code/theme is being
#' attributed). For per-citation distinct attribution, callers should use
#' \code{Map()} or iterate manually.
#'
#' Returns a list in the same order as \code{citations} so callers can
#' zip the result with parallel structures (e.g., a per-segment code list).
#' Empty input returns \code{list()} (not an error).
#'
#' @param citations List of citation objects (\code{ai_complete()$citations}).
#' @param documents Same documents list passed to \code{ai_complete()}.
#' @inheritParams make_quote_from_citation
#' @return List of \code{QuoteProvenance} objects (unverified). Empty list
#'   when \code{citations} is empty.
#' @export
make_quotes_from_citations <- function(citations, documents,
                                        attributed_theme_id = NA_character_,
                                        attributed_code_id = NA_character_,
                                        ai_model = NA_character_,
                                        ai_call_id = NA_character_,
                                        ai_paraphrase = NA_character_,
                                        source_doc_type_default = "data_entry") {
  if (length(citations) == 0L) return(list())
  lapply(citations, function(c) {
    make_quote_from_citation(
      c, documents,
      attributed_theme_id = attributed_theme_id,
      attributed_code_id  = attributed_code_id,
      ai_model            = ai_model,
      ai_call_id          = ai_call_id,
      ai_paraphrase       = ai_paraphrase,
      source_doc_type_default = source_doc_type_default
    )
  })
}

# ==============================================================================
# Verification ladder
# ==============================================================================

#' Verify a quote against its source text via the verification ladder
#'
#' Runs the verification ladder in order; the first match wins and sets
#' \code{verification_status} accordingly. Steps:
#' \enumerate{
#'   \item Strict offline string match (status \code{"verified_exact"},
#'         method \code{"string_match"}, score 1.0)
#'   \item Normalized match: whitespace collapsed, smart quotes ASCII'd,
#'         NFC normalization, case-folded (status \code{"verified_fuzzy"},
#'         method \code{"normalized_match"}, score 0.95)
#'   \item Substring search fallback: looks for normalized exact_text
#'         anywhere in the source; if found, the quote is accepted (status
#'         \code{"verified_fuzzy"}, method \code{"substring_search"}, score
#'         0.85). The recorded start_char/end_char are left as-is and flagged
#'         imprecise by the lower score -- the normalized-to-original offset
#'         mapping is lossy, so they are not recomputed.
#'   \item Embedding cosine similarity: requires \code{provider}; computes
#'         cosine between quote and source-text embeddings; matches if
#'         >= \code{.QUOTE_EMBEDDING_VERIFICATION_THRESHOLD} (status
#'         \code{"verified_fuzzy"}, method \code{"embedding_cosine"}, score
#'         = cosine value). Skipped silently if \code{provider} is NULL or
#'         doesn't support embeddings (the previous status sticks).
#' }
#'
#' Drift detection: before running the ladder, the source text is re-hashed
#' and compared to the quote's \code{source_text_sha256}. If the hashes
#' differ AND none of the ladder steps match, status becomes
#' \code{"drifted"} (the corpus changed since attribution). This is
#' distinguished from \code{"fabricated"} because it suggests the quote
#' was real once but the source has been edited.
#'
#' Failure mode: if all ladder steps fail and there is no drift, status
#' becomes \code{"fabricated"}. Caller should write the quote to the
#' fabrication_log via \code{\link{log_fabrication}}.
#'
#' @param quote A \code{QuoteProvenance} object from \code{\link{make_quote}}.
#' @param source_text Character. Current source document text (re-fetched
#'   at verification time; may differ from the text used at attribution
#'   time -- that's how drift is detected).
#' @param provider Optional AIProvider. When supplied, enables the
#'   embedding-similarity step (4). When NULL, the ladder stops at step 3.
#' @return The \code{QuoteProvenance} object with verification fields
#' updated. \code{verified_at} is set to \code{Sys.time} I.
#' @export
verify_quote <- function(quote, source_text, provider = NULL) {
  stopifnot(inherits(quote, "QuoteProvenance"),
            is.character(source_text), length(source_text) == 1L)

  current_hash <- digest::digest(source_text, algo = "sha256",
                                  serialize = FALSE)
  source_drifted <- !identical(current_hash, quote$source_text_sha256)

  now_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")

  # track the latest-attempted-and-failed
  # ladder step so a downstream fabricated/drifted quote carries an
  # attributable reason. Updated at each step's failure; when all
  # applicable steps fail, the latest value names the deepest step the
  # ladder tried.
  last_failure_reason <- NA_character_

  # ---- Step 1: strict string match at recorded offsets ---------------------
  # OFFSETS ARE 0-INDEXED EXCLUSIVE-END (Anthropic Citations API convention).
  # R's substr is 1-indexed inclusive-end -- convert: +1 for start, end as-is
  # because end is exclusive (so substr's inclusive end = end - 1, but
  # since the target is characters [start, end), substr(..., start+1, end) gets
  # exactly those characters).
  if (quote$end_char <= nchar(source_text)) {
    candidate <- substr(source_text, quote$start_char + 1L, quote$end_char)
    if (identical(candidate, quote$exact_text)) {
      return(.set_verification(quote, "verified_exact", "string_match",
                                1.0, now_iso))
    }
    last_failure_reason <- "step1_offset_mismatch"
  } else {
    last_failure_reason <- "step1_offset_out_of_bounds"
  }

  # ---- Step 2: normalized match at recorded offsets ------------------------
  if (quote$end_char <= nchar(source_text)) {
    candidate <- substr(source_text, quote$start_char + 1L, quote$end_char)
    if (identical(.normalize_quote_text(candidate),
                  .normalize_quote_text(quote$exact_text))) {
      return(.set_verification(quote, "verified_fuzzy", "normalized_match",
                                0.95, now_iso))
    }
    last_failure_reason <- "step2_normalized_mismatch"
  }

  # ---- Step 3: substring search fallback (corrects drift in offsets) ------
  norm_source <- .normalize_quote_text(source_text)
  norm_target <- .normalize_quote_text(quote$exact_text)
  if (nzchar(norm_target)) {
    pos <- regexpr(norm_target, norm_source, fixed = TRUE)
    if (pos > 0) {
      # Found via substring search; offsets in the normalized source map back
      # to the original. No attempt is made to recover exact original offsets (the
      # mapping is lossy after normalization); leave the original offsets and
      # mark the score lower to flag the imprecision.
      return(.set_verification(quote, "verified_fuzzy", "substring_search",
                                0.85, now_iso))
    }
    last_failure_reason <- "step3_substring_not_found"
  } else {
    last_failure_reason <- "step3_target_empty_after_normalization"
  }

  # ---- Step 4: embedding cosine similarity (paraphrase tolerance) ---------
  if (!is.null(provider)) {
    embedding_score <- tryCatch(
      .quote_embedding_similarity(quote$exact_text, source_text, provider),
      error = function(e) {
        log_debug("Quote embedding verification failed: {e$message}")
        NA_real_
      }
    )
    if (!is.na(embedding_score) &&
        embedding_score >= .QUOTE_EMBEDDING_VERIFICATION_THRESHOLD) {
      return(.set_verification(quote, "verified_fuzzy", "embedding_cosine",
                                embedding_score, now_iso))
    }
    last_failure_reason <- if (is.na(embedding_score)) {
      "step4_embedding_unavailable"
    } else {
      "step4_embedding_below_threshold"
    }
  } else if (is.na(last_failure_reason)) {
    # No embedding provider (the default coding path): do NOT overwrite the
    # real reason the quote failed (set by steps 1-3, e.g. substring_not_found).
    # "step4_skipped" only applies if no earlier step had already failed.
    last_failure_reason <- "step4_skipped_no_provider"
  }

  # ---- All ladder steps failed --------------------------------------------
  if (source_drifted) {
    .set_verification(quote, "drifted", NA_character_, NA_real_, now_iso,
                       failure_reason = "source_text_sha256_mismatch")
  } else {
    .set_verification(quote, "fabricated", NA_character_, NA_real_, now_iso,
                       failure_reason = last_failure_reason %||% "all_steps_failed")
  }
}

#' Verify a batch of quotes against a corpus
#'
#' Convenience wrapper that looks up each quote's source text in
#' \code{corpus_lookup} (a named list keyed by source_doc_id) and runs
#' \code{\link{verify_quote}} on each. Quotes whose source is missing
#' from the corpus are marked \code{"drifted"}.
#'
#' @param quotes List of QuoteProvenance objects.
#' @param corpus_lookup Named list: source_doc_id -> source_text.
#' @param provider Optional AIProvider for the embedding ladder step.
#' @return List of QuoteProvenance objects, each with verification fields
#'   populated.
#' @export
verify_quotes <- function(quotes, corpus_lookup, provider = NULL) {
  stopifnot(is.list(quotes), is.list(corpus_lookup))
  lapply(quotes, function(q) {
    if (!inherits(q, "QuoteProvenance")) return(q)
    src <- corpus_lookup[[q$source_doc_id]]
    if (is.null(src)) {
      now_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
      return(.set_verification(q, "drifted", NA_character_, NA_real_, now_iso,
                                failure_reason = "source_missing_from_corpus"))
    }
    verify_quote(q, src, provider = provider)
  })
}

# ==============================================================================
# Fabrication log
# ==============================================================================

#' Initialize the fabrication log
#'
#' Creates (or truncates to header) \code{outputs/<run>/fabrication_log.csv}
#' and returns a \code{FabricationLog} S3 object that can be passed to
#' \code{\link{log_fabrication}}.
#'
#' Each fabricated quote becomes one CSV row with columns: timestamp,
#' quote_id, source_doc_id, attributed_theme_id, attributed_code_id,
#' ai_model, ai_call_id, exact_text, verification_status. The CSV format
#' (rather than JSONL) is deliberate: methodology-paper analyses run R-side
#' aggregations that are easier on a wide CSV than nested JSONL.
#'
#' @param output_dir Run output directory (where the CSV is written).
#' @param methodology_mode Optional methodology mode. When
#'   non-NULL, the CSV header is preceded by a comment-style methodology
#'   stamp identifying the mode and run id, so a reviewer picking up the
#'   bare CSV sees the methodology declaration. NULL skips stamping
#'   (legacy / test callers).
#' @return A FabricationLog S3 object.
#' @export
init_fabrication_log <- function(output_dir, methodology_mode = NULL) {
  stopifnot(is.character(output_dir), length(output_dir) == 1L)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  log_path <- file.path(output_dir, "fabrication_log.csv")

  # Always write a fresh header. If a fabricated quote was logged in a prior
  # session the user is starting over; rotating logs would be over-engineering.
  # append failure_reason column so a methodology
  # paper can attribute fabrications to specific ladder failure modes
  # (step1_offset_mismatch, step2_normalized_mismatch, step3_substring_
  # not_found, step4_embedding_below_threshold, source_text_sha256_
  # mismatch, etc.).
  header <- c(
    "timestamp", "quote_id", "source_doc_id", "attributed_theme_id",
    "attributed_code_id", "ai_model", "ai_call_id", "exact_text",
    "verification_status", "failure_reason"
  )
  con <- file(log_path, open = "w")
  writeLines(paste(header, collapse = ","), con = con)
  close(con)

  # T1.7 / AC4: stamp the file with the methodology mode so any
  # downstream consumer sees the declaration up-front. log_fabrication's
  # append path uses raw cat() to add rows but does not touch the
  # header lines, so the stamp survives subsequent appends.
  if (!is.null(methodology_mode)) {
    tryCatch(stamp_methodology_csv(log_path, methodology_mode,
                                     run_id = basename(output_dir)),
             error = function(e) log_debug("CSV stamp skipped: {e$message}"))
  }

  # Env-backed counter so log_fabrication's increments survive pass-by-value.
  state <- new.env(parent = emptyenv())
  state$n_logged <- 0L

  flog <- list(
    path       = log_path,
    output_dir = output_dir,
    state      = state
  )
  class(flog) <- "FabricationLog"

  log_info("Fabrication log initialised: {log_path}")
  flog
}

#' Append a fabricated quote to the fabrication log
#'
#' Silently no-ops on a non-FabricationLog \code{flog}, on a NULL quote, or
#' on a quote whose verification_status is not \code{"fabricated"} (the
#' single-purpose CSV is for fabrications only; \code{"drifted"} and
#' \code{"unverified"} have other render-time treatments).
#'
#' @param flog A FabricationLog from \code{\link{init_fabrication_log}}.
#' @param quote A QuoteProvenance with verification_status = "fabricated".
#' @return Invisibly returns \code{flog}.
#' @export
log_fabrication <- function(flog, quote) {
  if (is.null(flog) || !inherits(flog, "FabricationLog")) return(invisible(flog))
  if (is.null(quote) || !inherits(quote, "QuoteProvenance")) return(invisible(flog))
  if (!identical(quote$verification_status, "fabricated")) return(invisible(flog))

  row <- list(
    timestamp           = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    quote_id            = quote$quote_id,
    source_doc_id       = quote$source_doc_id,
    attributed_theme_id = quote$attributed_theme_id %||% NA_character_,
    attributed_code_id  = quote$attributed_code_id %||% NA_character_,
    ai_model            = quote$ai_model %||% NA_character_,
    ai_call_id          = quote$ai_call_id %||% NA_character_,
    # Truncate exact_text to keep the CSV scannable; full text is in the
    # raw_response cache and the original audit log.
    exact_text          = substr(quote$exact_text, 1, 500),
    verification_status = quote$verification_status,
    # structured failure reason populated by
    # verify_quote. NA on legacy QuoteProvenance objects without the
    # field (back-compat for runs replayed from earlier cache).
    failure_reason      = quote$verification_failure_reason %||%
                            NA_character_
  )

  tryCatch({
    con <- file(flog$path, open = "a")
    on.exit(close(con))
    writeLines(.csv_row(row), con = con)
    flog$state$n_logged <- flog$state$n_logged + 1L
  }, error = function(e) {
    log_warn("Failed to append fabrication log row: {e$message}")
  })

  invisible(flog)
}

#' Summarize quote provenance for the report's Tier-0 dashboard
#'
#' Computes counts + rates from a list of verified quotes. Used by the
#' Tier-0 dashboard (\code{\link{.build_tier0_dashboard}}) and usable
#' programmatically for cross-run analyses (the methodology paper's KPIs
#' draw from the same shape).
#'
#' T0.1 part 3b adds the \code{by_citation_source} breakdown
#' and \code{verification_rate_by_source} so the dashboard can distinguish
#' Anthropic Citations API quotes (server-side-grounded by Anthropic plus
#' client-verified by the ladder) from model-freeform quotes (client-
#' verified only). Both are valid; the citations source is strictly
#' stronger.
#'
#' @param quotes List of QuoteProvenance objects (after \code{verify_quote}).
#' @return Named list:
#'   \itemize{
#'     \item \code{total}: total quote count
#'     \item \code{by_status}: integer vector keyed by verification_status
#'     \item \code{by_method}: integer vector keyed by verification_method
#'     \item \code{by_citation_source}: integer vector keyed by
#'       \code{citation_source} (\code{"anthropic_citations_api"},
#'       \code{"model_freeform"}, etc.)
#'     \item \code{verification_rate}: proportion in either verified state
#'     \item \code{fabrication_rate}: proportion fabricated
#'     \item \code{drift_rate}: proportion drifted
#'     \item \code{verification_rate_by_source}: named numeric vector --
#'       per-citation_source verification rate (verified / total quotes
#'       with that source). Lets the dashboard expose the differential
#'       reliability of the prevention layer (citations API) vs the
#'       detection-only layer (model_freeform + ladder).
#'     \item \code{n_citations_api}: integer count of quotes with
#'       \code{citation_source == "anthropic_citations_api"}. Convenience
#'       accessor for the dashboard's headline KPI.
#'     \item \code{citations_api_rate}: proportion of quotes that came
#'       through the citations API path (vs. fell back to model_freeform).
#'       This is the package's empirical answer to "did the prevention
#'       layer actually engage on this run?".
#'   }
#' @export
quote_provenance_summary <- function(quotes) {
  if (length(quotes) == 0L) {
    return(list(
      total                       = 0L,
      by_status                   = stats::setNames(integer(0), character(0)),
      by_method                   = stats::setNames(integer(0), character(0)),
      by_citation_source          = stats::setNames(integer(0), character(0)),
      verification_rate           = NA_real_,
      fabrication_rate            = NA_real_,
      drift_rate                  = NA_real_,
      verification_rate_by_source = stats::setNames(numeric(0), character(0)),
      n_citations_api             = 0L,
      citations_api_rate          = NA_real_
    ))
  }

  statuses <- vapply(quotes, function(q) q$verification_status %||% NA_character_,
                     character(1))
  methods  <- vapply(quotes, function(q) q$verification_method %||% NA_character_,
                     character(1))
  sources  <- vapply(quotes, function(q) q$citation_source %||% NA_character_,
                     character(1))

  status_table <- table(statuses, useNA = "no")
  method_table <- table(methods,  useNA = "no")
  source_table <- table(sources,  useNA = "no")

  n <- length(quotes)
  is_verified <- statuses %in% c("verified_exact", "verified_fuzzy")

  # Per-source verification rate. Avoids divide-by-zero when a source
  # has zero entries (the table doesn't include it).
  rate_by_source <- vapply(names(source_table), function(s) {
    src_total <- sum(sources == s, na.rm = TRUE)
    if (src_total == 0L) return(NA_real_)
    sum(sources == s & is_verified, na.rm = TRUE) / src_total
  }, numeric(1))
  names(rate_by_source) <- names(source_table)

  n_citations_api <- sum(sources == "anthropic_citations_api", na.rm = TRUE)

  list(
    total                       = n,
    by_status                   = as.integer(status_table) |>
                                    stats::setNames(names(status_table)),
    by_method                   = as.integer(method_table) |>
                                    stats::setNames(names(method_table)),
    by_citation_source          = as.integer(source_table) |>
                                    stats::setNames(names(source_table)),
    verification_rate           = sum(is_verified, na.rm = TRUE) / n,
    fabrication_rate            = sum(statuses == "fabricated", na.rm = TRUE) / n,
    drift_rate                  = sum(statuses == "drifted",   na.rm = TRUE) / n,
    verification_rate_by_source = rate_by_source,
    n_citations_api             = as.integer(n_citations_api),
    citations_api_rate          = n_citations_api / n
  )
}

#' Aggregate verification stats across all coded segments in a coding state
#'
#' Walks \code{coding_state$codebook}, extracts the \code{$provenance} field
#' attached to each coded segment by \code{.code_entry_progressive} (T0.1
#' part 2 wiring), and feeds them through \code{quote_provenance_summary}.
#'
#' Returns the empty-summary shape (zero counts, NA rates) when:
#' \itemize{
#'   \item \code{coding_state} is NULL (e.g., the run skipped coding)
#'   \item \code{coding_state} predates the T0.1 wiring and has no
#'     \code{$provenance} on its segments (legacy runs)
#'   \item the codebook is empty
#' }
#'
#' This helper is what the report's Tier-0 dashboard reads -- so the
#' empty-summary fallback is load-bearing for back-compat: pre-T0.1 runs
#' still render a dashboard, just one that says "verification not run".
#'
#' @param coding_state ProgressiveCodingState (or NULL).
#' @return The list returned by \code{\link{quote_provenance_summary}}.
#' @export
compute_quote_provenance_stats <- function(coding_state) {
  if (is.null(coding_state) || is.null(coding_state$codebook) ||
      length(coding_state$codebook) == 0L) {
    return(quote_provenance_summary(list()))
  }

  # Walk every codebook entry's coded_segments; collect $provenance fields.
  quotes <- list()
  for (cb_key in names(coding_state$codebook)) {
    segs <- coding_state$codebook[[cb_key]]$coded_segments
    if (is.null(segs)) next
    for (seg in segs) {
      if (!is.null(seg$provenance) &&
          inherits(seg$provenance, "QuoteProvenance")) {
        quotes[[length(quotes) + 1L]] <- seg$provenance
      }
    }
  }

  quote_provenance_summary(quotes)
}

# ==============================================================================
# Print method
# ==============================================================================

#' Print method for QuoteProvenance
#' @param x A QuoteProvenance object
#' @param ... Ignored
#' @export
print.QuoteProvenance <- function(x, ...) {
  cat("QuoteProvenance\n")
  cat(sprintf("  ID:               %s\n", x$quote_id))
  cat(sprintf("  Source:           %s [%d-%d)\n",
              x$source_doc_id, x$start_char, x$end_char))
  cat(sprintf("  Type:             %s\n", x$source_doc_type))
  cat(sprintf("  Citation source:  %s\n", x$citation_source))
  cat(sprintf("  Verification:     %s", x$verification_status))
  if (!is.na(x$verification_method)) {
    cat(sprintf(" via %s (score=%.2f)",
                x$verification_method, x$verification_score))
  }
  cat("\n")
  cat(sprintf("  Text:             %s\n",
              if (nchar(x$exact_text) > 100) {
                paste0(substr(x$exact_text, 1, 97), "...")
              } else x$exact_text))
  invisible(x)
}

# ==============================================================================
# Internal helpers
# ==============================================================================

#' Set verification fields on a quote
#'
#' optional \code{failure_reason} populates
#' \code{verification_failure_reason} for fabricated / drifted statuses.
#' NA when the status is verified_* (the field carries meaning only
#' when verification failed).
#' @keywords internal
.set_verification <- function(quote, status, method, score, verified_at,
                                failure_reason = NA_character_) {
  quote$verification_status         <- status
  quote$verification_method         <- method
  quote$verification_score          <- score
  quote$verified_at                 <- verified_at
  quote$verification_failure_reason <- failure_reason
  quote
}

#' Normalize text for the verification ladder's fuzzy steps
#'
#' Applies (in order): NFC unicode normalization (where stringi is
#' available), smart quote -> ASCII quote conversion, unicode-aware
#' whitespace collapse, case-folding. This catches the most common
#' attribution drift patterns: model returns typographic quotes where
#' source has straight ASCII, model collapses or inserts whitespace
#' (including unicode NBSP / em-space / etc. that the default \code{\\s}
#' regex misses), model lowercases.
#'
#' Earlier this helper only did smart-
#' quote ASCII-fication + standard \code{\\s} whitespace collapse. The
#' An audit found 8 of 50 sampled verbatim spot-checks failed
#' (16% miss rate) -- mostly because (a) source had typographic
#' apostrophes that weren't NFC-normalized to combine with the AI's
#' ASCII rendering, and (b) source had U+00A0 NBSP / U+2009 thin-space
#' that R's PCRE \code{\\s} doesn't match by default. NFC normalization
#' + unicode-aware whitespace class \code{[\\p{Z}\\s]} together resolve
#' both classes of false-positive in the fabrication log.
#' @keywords internal
.normalize_quote_text <- function(x) {
  if (is.na(x) || !nzchar(x)) return("")
  # NFC normalization. NFC composes precomposed
  # unicode chars back to their canonical form -- so an a + combining-acute
  # matches an a-acute precomposed. stringi is in Imports
  # (load-bearing for T0.1 verification fidelity).
  x <- stringi::stri_trans_nfc(x)
  # Convert smart quotes to ASCII straights. Using \u escapes (rather
  # than literal multi-byte U chars in the source) so chartr's
  # length-equality check works regardless of source-file encoding -- some
  # platforms read this file with Encoding(x) = "unknown" which makes
  # nchar count bytes instead of characters and breaks chartr.
  # Codepoints handled (smart single/double quotes + low quotes + primes):
  #   U+2018 LEFT SINGLE QUOTATION MARK    -> '
  #   U+2019 RIGHT SINGLE QUOTATION MARK   -> '
  #   U+201A SINGLE LOW-9 QUOTATION MARK   -> '
  #   U+201C LEFT DOUBLE QUOTATION MARK    -> "
  #   U+201D RIGHT DOUBLE QUOTATION MARK   -> "
  #   U+201E DOUBLE LOW-9 QUOTATION MARK   -> "
  #   U+2032 PRIME                         -> '
  #   U+2033 DOUBLE PRIME                  -> "
  # \u escapes are parse-time and encoding-independent, so this works
  # regardless of the file's Encoding() attribute.
  smart_quotes <- "\u2018\u2019\u201A\u201C\u201D\u201E\u2032\u2033"
  ascii_quotes <- "'''\"\"\"'\""
  x <- chartr(smart_quotes, ascii_quotes, x)
  # unicode-aware whitespace collapse. The default
  # PCRE \s matches [ \t\n\r\f\v] in C locale -- it does NOT match
  # U+00A0 NBSP, U+2009 thin space, U+2003 em space, etc. Sources
  # scraped from web content frequently contain these characters where
  # the AI emits ordinary spaces, producing a substring-search false-
  # positive in the verification ladder. \p{Z} covers all unicode
  # Separator categories (Zs space, Zl line, Zp paragraph).
  x <- gsub("[\\s\\p{Z}]+", " ", x, perl = TRUE)
  # Trim
  x <- trimws(x)
  # Case fold
  tolower(x)
}

#' Compute embedding cosine similarity between a quote and a source text
#'
#' Used by the verification ladder's step 4. Embeds both texts via the
#' provider's embedding endpoint, then takes the cosine of the resulting
#' vectors. Returns NA_real_ if embeddings are unavailable for the
#' provider (Anthropic doesn't currently expose embeddings).
#' @keywords internal
.quote_embedding_similarity <- function(quote_text, source_text, provider) {
  emb <- compute_embeddings(provider, c(quote_text, source_text))
  if (is.null(emb) || nrow(emb) != 2) return(NA_real_)
  sim_matrix <- .cosine_similarity_matrix(emb)
  sim_matrix[1, 2]
}

#' Quote a single value for safe CSV output
#' @keywords internal
.csv_quote <- function(x) {
  if (is.na(x)) return("")
  s <- as.character(x)
  # If the value contains a comma, quote, or newline, RFC 4180 requires
  # quoting and doubling embedded quotes
  if (grepl('[,"\n]', s, perl = TRUE)) {
    s <- gsub('"', '""', s, fixed = TRUE)
    s <- paste0('"', s, '"')
  }
  s
}

#' Build one CSV row from a named list, values in declaration order
#' @keywords internal
.csv_row <- function(row_list) {
  paste(vapply(row_list, .csv_quote, character(1)), collapse = ",")
}
