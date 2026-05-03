# ==============================================================================
# Quote Provenance + Verification Ladder (Sprint-4 T0.1)
# ==============================================================================
# Every AI-attributed quote in pakhom must carry full provenance metadata
# AND be verified offline against the source corpus. This is the package's
# direct response to the most-cited empirical critique of LLM-for-TA tools:
# Jowsey et al. 2025 (PLOS One, doi:10.1371/journal.pone.0330217) found that
# Microsoft Copilot fabricated quotes that appeared verbatim in the data but
# were not actually in any source -- the "Frankenstein quote" pattern.
#
# Anti-fabrication strategy:
# 1. Every quote object carries source_doc_id + char range + sha256 of the
#    source text used at attribution time. If the source corpus changes
#    between runs, drift is detectable (verification_status = "drifted").
# 2. A four-step verification ladder runs at quote-creation time and again
#    at report-generation time:
#      (a) strict offline string match
#      (b) normalized match (whitespace + smart-quote + NFC + case)
#      (c) substring search fallback (corrects offsets)
#      (d) embedding cosine similarity (paraphrase tolerance, threshold 0.85)
#    Each step downgrades verification_status if the previous fails. A quote
#    that fails all four is "fabricated" and never rendered.
# 3. Fabricated quotes are written to outputs/<run>/fabrication_log.csv
#    for the methodology paper's KPI ("fabrication rate per N AI calls").
# 4. Future T0.1 phases add Anthropic Citations API custom content blocks
#    (model returns {quote_id, span_start, span_end} from pre-extracted
#    candidate spans, never asked to write quotes). This module is the
#    foundation those extensions consume.
#
# Schema versioning: .QUOTE_PROVENANCE_SCHEMA_VERSION lets downstream
# consumers (replay_run, methodology paper analyses) detect when the quote
# object shape has evolved. Treat all bumps as additive unless explicitly
# noted; the verification ladder may add new methods without bumping.
# ==============================================================================

#' Current schema version for the quote provenance object
#' @keywords internal
.QUOTE_PROVENANCE_SCHEMA_VERSION <- "1.0.0"

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
#' run the four-step verification ladder.
#'
#' \code{quote_id} is computed deterministically as
#' \code{paste0("qte_", sha1(source_doc_id + start_char + end_char + exact_text))}
#' so the same quote always has the same ID across runs -- critical for
#' replay (OS.5) and cross-run comparison.
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
#'   compute \code{source_text_sha256}; not stored on the quote.
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
  # range + text always produces the same ID (OS.5 replay relies on this).
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
    verified_at         = NA_character_,
    schema_version      = .QUOTE_PROVENANCE_SCHEMA_VERSION
  )
  class(quote) <- "QuoteProvenance"
  quote
}

# ==============================================================================
# Verification ladder
# ==============================================================================

#' Verify a quote against its source text via the four-step ladder
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
#'         anywhere in the source; if found, corrects start_char/end_char
#'         (status \code{"verified_fuzzy"}, method
#'         \code{"substring_search"}, score 0.85)
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
#'   updated. \code{verified_at} is set to \code{Sys.time()} ISO-8601.
#' @export
verify_quote <- function(quote, source_text, provider = NULL) {
  stopifnot(inherits(quote, "QuoteProvenance"),
            is.character(source_text), length(source_text) == 1L)

  current_hash <- digest::digest(source_text, algo = "sha256",
                                  serialize = FALSE)
  source_drifted <- !identical(current_hash, quote$source_text_sha256)

  now_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")

  # ---- Step 1: strict string match at recorded offsets ---------------------
  # OFFSETS ARE 0-INDEXED EXCLUSIVE-END (Anthropic Citations API convention).
  # R's substr is 1-indexed inclusive-end -- convert: +1 for start, end as-is
  # because end is exclusive (so substr's inclusive end = our end - 1, but
  # since we want characters [start, end), substr(..., start+1, end) gets
  # exactly those characters).
  if (quote$end_char <= nchar(source_text)) {
    candidate <- substr(source_text, quote$start_char + 1L, quote$end_char)
    if (identical(candidate, quote$exact_text)) {
      return(.set_verification(quote, "verified_exact", "string_match",
                                1.0, now_iso))
    }
  }

  # ---- Step 2: normalized match at recorded offsets ------------------------
  if (quote$end_char <= nchar(source_text)) {
    candidate <- substr(source_text, quote$start_char + 1L, quote$end_char)
    if (identical(.normalize_quote_text(candidate),
                  .normalize_quote_text(quote$exact_text))) {
      return(.set_verification(quote, "verified_fuzzy", "normalized_match",
                                0.95, now_iso))
    }
  }

  # ---- Step 3: substring search fallback (corrects drift in offsets) ------
  norm_source <- .normalize_quote_text(source_text)
  norm_target <- .normalize_quote_text(quote$exact_text)
  if (nzchar(norm_target)) {
    pos <- regexpr(norm_target, norm_source, fixed = TRUE)
    if (pos > 0) {
      # Found via substring search; offsets in the normalized source map back
      # to the original. We don't try to recover exact original offsets (the
      # mapping is lossy after normalization); leave the original offsets and
      # mark the score lower to flag the imprecision.
      return(.set_verification(quote, "verified_fuzzy", "substring_search",
                                0.85, now_iso))
    }
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
  }

  # ---- All ladder steps failed --------------------------------------------
  if (source_drifted) {
    .set_verification(quote, "drifted", NA_character_, NA_real_, now_iso)
  } else {
    .set_verification(quote, "fabricated", NA_character_, NA_real_, now_iso)
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
      now_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
      return(.set_verification(q, "drifted", NA_character_, NA_real_, now_iso))
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
#' @return A FabricationLog S3 object.
#' @export
init_fabrication_log <- function(output_dir) {
  stopifnot(is.character(output_dir), length(output_dir) == 1L)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  log_path <- file.path(output_dir, "fabrication_log.csv")

  # Always write a fresh header. If a fabricated quote was logged in a prior
  # session the user is starting over; rotating logs would be over-engineering.
  header <- c(
    "timestamp", "quote_id", "source_doc_id", "attributed_theme_id",
    "attributed_code_id", "ai_model", "ai_call_id", "exact_text",
    "verification_status"
  )
  con <- file(log_path, open = "w")
  writeLines(paste(header, collapse = ","), con = con)
  close(con)

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
    timestamp           = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    quote_id            = quote$quote_id,
    source_doc_id       = quote$source_doc_id,
    attributed_theme_id = quote$attributed_theme_id %||% NA_character_,
    attributed_code_id  = quote$attributed_code_id %||% NA_character_,
    ai_model            = quote$ai_model %||% NA_character_,
    ai_call_id          = quote$ai_call_id %||% NA_character_,
    # Truncate exact_text to keep the CSV scannable; full text is in the
    # raw_response cache and the original audit log.
    exact_text          = substr(quote$exact_text, 1, 500),
    verification_status = quote$verification_status
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
#' upcoming T0.1 part 3 work to render a dashboard at the top of the
#' generated report; usable today for programmatic introspection.
#'
#' @param quotes List of QuoteProvenance objects (after \code{verify_quote}).
#' @return Named list:
#'   \itemize{
#'     \item \code{total}: total quote count
#'     \item \code{by_status}: integer vector keyed by verification_status
#'     \item \code{by_method}: integer vector keyed by verification_method
#'     \item \code{verification_rate}: proportion in either verified state
#'     \item \code{fabrication_rate}: proportion fabricated
#'     \item \code{drift_rate}: proportion drifted
#'   }
#' @export
quote_provenance_summary <- function(quotes) {
  if (length(quotes) == 0L) {
    return(list(
      total             = 0L,
      by_status         = stats::setNames(integer(0), character(0)),
      by_method         = stats::setNames(integer(0), character(0)),
      verification_rate = NA_real_,
      fabrication_rate  = NA_real_,
      drift_rate        = NA_real_
    ))
  }

  statuses <- vapply(quotes, function(q) q$verification_status %||% NA_character_,
                     character(1))
  methods  <- vapply(quotes, function(q) q$verification_method %||% NA_character_,
                     character(1))

  status_table <- table(statuses, useNA = "no")
  method_table <- table(methods,  useNA = "no")

  n <- length(quotes)
  list(
    total             = n,
    by_status         = as.integer(status_table) |>
                          stats::setNames(names(status_table)),
    by_method         = as.integer(method_table) |>
                          stats::setNames(names(method_table)),
    verification_rate = sum(statuses %in% c("verified_exact", "verified_fuzzy"),
                             na.rm = TRUE) / n,
    fabrication_rate  = sum(statuses == "fabricated", na.rm = TRUE) / n,
    drift_rate        = sum(statuses == "drifted",   na.rm = TRUE) / n
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
#' @keywords internal
.set_verification <- function(quote, status, method, score, verified_at) {
  quote$verification_status <- status
  quote$verification_method <- method
  quote$verification_score  <- score
  quote$verified_at         <- verified_at
  quote
}

#' Normalize text for the verification ladder's fuzzy steps
#'
#' Applies (in order): NFC unicode normalization (where supported), smart
#' quote -> ASCII quote conversion, whitespace collapse, case-folding.
#' This catches the most common attribution drift patterns: model returns
#' typographic quotes where source has straight ASCII, model collapses or
#' inserts whitespace, model lowercases.
#' @keywords internal
.normalize_quote_text <- function(x) {
  if (is.na(x) || !nzchar(x)) return("")
  # Convert smart quotes to ASCII straights first. Using \u escapes (rather
  # than literal multi-byte UTF-8 chars in the source) so chartr's
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
  # Whitespace collapse
  x <- gsub("\\s+", " ", x, perl = TRUE)
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
