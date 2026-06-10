# ==============================================================================
# Text Preprocessing and Cleaning
# ==============================================================================

#' Preprocess text data for analysis
#'
#' Cleans text, removes artifacts, filters by length, and removes duplicates.
#' Supports platform-specific cleaning rules and custom regex patterns.
#'
#' @details
#' Default transforms (with \code{source_type = "reddit"}, the default when
#' unset): URLs are removed; \code{u/} and at-mentions are deleted;
#' \code{r/<name>} references are replaced by the literal redaction token
#' \code{[subreddit]} (a privacy marker; bracketed editorial insertions are
#' standard qualitative-research convention); markdown bold / italic /
#' strikethrough markup is stripped and quoted (\code{>}) lines are dropped;
#' \code{[removed]} / \code{[deleted]} placeholders are removed; HTML
#' entities and unicode escapes are decoded; text is NFC-normalized and
#' whitespace is collapsed.
#'
#' All downstream coding, T0.1 quote verification, report excerpts, and
#' QDPX export operate on this cleaned \code{std_text} -- so "verbatim" in
#' the T0.1 guarantee means verbatim with respect to the cleaned analytic
#' text. The raw platform text is preserved unchanged in
#' \code{original_text}.
#'
#' @param data Standardized tibble (must have std_text column)
#' @param config Preprocessing config section from YAML
#' @return Filtered tibble with cleaned text
preprocess_text <- function(data, config = list()) {
  # Apply defaults
  config$remove_urls <- config$remove_urls %||% TRUE
  config$remove_mentions <- config$remove_mentions %||% TRUE
  config$remove_hashtags <- config$remove_hashtags %||% FALSE
  config$lowercase <- config$lowercase %||% FALSE
  config$min_text_length <- config$min_text_length %||% 10
  config$max_text_length <- config$max_text_length %||% 10000

  # Validate min/max text length consistency
  min_len <- config$min_text_length
  max_len <- config$max_text_length
  if (!is.null(min_len) && !is.null(max_len) && min_len > max_len) {
    log_warn("min_text_length ({min_len}) > max_text_length ({max_len}); swapping values")
    config$min_text_length <- max_len
    config$max_text_length <- min_len
  }

  source_type <- config$source_type %||% "reddit"

  validate_data_columns(data, "std_text", "preprocess_text")

  original_count <- nrow(data)
  log_info("Preprocessing {original_count} entries (source_type: {source_type})...")

  # Remove entries with missing or empty text
  data <- data |> filter(!is.na(std_text), nchar(trimws(std_text)) > 0)

  # Clean text
  data <- data |>
    mutate(std_text = .clean_text(std_text, config, source_type))

  # Filter by length
  data <- data |>
    filter(
      nchar(std_text) >= config$min_text_length,
      nchar(std_text) <= config$max_text_length
    )

  # Remove exact duplicates
  data <- data |> distinct(std_text, .keep_all = TRUE)

  removed <- original_count - nrow(data)
  log_info("Preprocessing complete: {original_count} -> {nrow(data)} entries ({removed} removed)")

  if (nrow(data) == 0) {
    log_warn("Preprocessing removed all entries. Check min_text_length and data quality.")
  }

  data
}

#' Decode common HTML entities
#' @keywords internal
.decode_html_entities <- function(text) {
  text <- gsub("&amp;", "&", text, fixed = TRUE)
  text <- gsub("&lt;", "<", text, fixed = TRUE)
  text <- gsub("&gt;", ">", text, fixed = TRUE)
  text <- gsub("&quot;", '"', text, fixed = TRUE)
  text <- gsub("&#39;", "'", text, fixed = TRUE)
  text
}

#' Decode R-style Unicode escape sequences like <U+2019>
#'
#' When text is read from databases or files with encoding issues,
#' Unicode characters may appear as literal \code{<U+XXXX>} strings
#' instead of the actual characters.
#' @param text Character vector
#' @return Character vector with Unicode escapes decoded
#' @keywords internal
.decode_unicode_escapes <- function(text) {
  # Find all unique <U+XXXX> patterns across the entire vector
  escapes <- unique(unlist(regmatches(text, gregexpr("<U\\+[0-9A-Fa-f]{4,5}>", text))))
  if (length(escapes) == 0) return(text)

  result <- text
  for (esc in escapes) {
    hex <- sub("^<U\\+([0-9A-Fa-f]+)>$", "\\1", esc)
    char <- tryCatch(
      intToUtf8(strtoi(hex, base = 16L)),
      error = function(e) esc  # keep original if conversion fails
    )
    result <- gsub(esc, char, result, fixed = TRUE)
  }
  result
}

#' Clean text content with platform-aware rules
#' @keywords internal
.clean_text <- function(texts, config, source_type = "reddit") {
  cleaned <- texts

  # --- Platform-agnostic: URL removal ---
  if (isTRUE(config$remove_urls)) {
    cleaned <- gsub("https?://[^\\s)>]+", "", cleaned, perl = TRUE)
    cleaned <- gsub("www\\.\\S+", "", cleaned)
  }

  # --- Platform-specific cleaning ---
  if (source_type == "reddit") {
    cleaned <- .clean_reddit(cleaned, config)
  } else if (source_type == "twitter") {
    cleaned <- .clean_twitter(cleaned, config)
  } else if (source_type == "clinical") {
    cleaned <- .clean_clinical(cleaned, config)
  } else {
    # Generic / unknown platform
    cleaned <- .clean_generic(cleaned, config)
  }

  # --- Custom cleaning rules from config ---
  custom_rules <- config$custom_cleaning_rules
  if (!is.null(custom_rules) && length(custom_rules) > 0) {
    for (rule in custom_rules) {
      if (!is.null(rule$pattern)) {
        cleaned <- gsub(rule$pattern, rule$replacement %||% "", cleaned)
      }
    }
  }

  # --- Always: decode R-style Unicode escapes (e.g., <U+2019> -> ') ---
  cleaned <- .decode_unicode_escapes(cleaned)

  # --- Always: Unicode NFC normalization (ensures consistent representation) ---
  # stringi is in Imports (load-bearing for T0.1).
  cleaned <- stringi::stri_trans_nfc(cleaned)

  # --- Always: normalize whitespace ---
  cleaned <- gsub("\\s+", " ", cleaned)
  cleaned <- trimws(cleaned)

  # Optional lowercase
  if (isTRUE(config$lowercase)) {
    cleaned <- tolower(cleaned)
  }

  cleaned
}

#' Reddit-specific text cleaning
#' @keywords internal
.clean_reddit <- function(texts, config) {
  cleaned <- texts

  # Reddit mentions and subreddit references
  if (isTRUE(config$remove_mentions)) {
    cleaned <- gsub("u/\\w+", "", cleaned)
    cleaned <- gsub("r/\\w+", "[subreddit]", cleaned)
    cleaned <- gsub("@\\w+", "", cleaned)
  }

  # Reddit markdown artifacts
  cleaned <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", cleaned)  # bold
  cleaned <- gsub("\\*([^*]+)\\*", "\\1", cleaned)         # italic
  cleaned <- gsub("~~([^~]+)~~", "\\1", cleaned)           # strikethrough
  cleaned <- gsub("(?m)^&gt;.*$", "", cleaned, perl = TRUE)  # block quotes
  cleaned <- .decode_html_entities(cleaned)

  # Bot/auto-mod text
  cleaned <- gsub("\\[removed\\]", "", cleaned)
  cleaned <- gsub("\\[deleted\\]", "", cleaned)

  # Hashtags (optional)
  if (isTRUE(config$remove_hashtags)) {
    cleaned <- gsub("#\\w+", "", cleaned)
  }

  cleaned
}

#' Twitter/X-specific text cleaning
#' @keywords internal
.clean_twitter <- function(texts, config) {
  cleaned <- texts

  # @mentions
  if (isTRUE(config$remove_mentions)) {
    cleaned <- gsub("@\\w+", "", cleaned)
  }

  # Retweet markers
  cleaned <- gsub("^RT\\s+", "", cleaned)

  # Hashtags: keep the word, remove the #
  if (isTRUE(config$remove_hashtags)) {
    cleaned <- gsub("#(\\w+)", "\\1", cleaned)
  }

  # HTML entities common in tweets
  cleaned <- .decode_html_entities(cleaned)

  cleaned
}

#' Clinical text cleaning (minimal, preserves structure)
#' @keywords internal
.clean_clinical <- function(texts, config) {
  # NOTE: This is illustrative PII redaction only. Not a substitute for proper
  # de-identification tools (e.g., scrubadub, Presidio). Do NOT rely on this
  # for HIPAA/PHI compliance.
  cleaned <- texts

  # Redact common PII patterns (SSN-specific pattern first, then generic labels)
  cleaned <- gsub("\\b\\d{3}-\\d{2}-\\d{4}\\b", "[SSN]", cleaned)
  cleaned <- gsub("\\b(MRN|DOB|SSN)\\s*:\\s*\\S+", "[REDACTED]", cleaned)

  # HTML entities
  cleaned <- .decode_html_entities(cleaned)

  cleaned
}

#' Generic platform text cleaning
#' @keywords internal
.clean_generic <- function(texts, config) {
  cleaned <- texts

  # Basic mention removal
  if (isTRUE(config$remove_mentions)) {
    cleaned <- gsub("@\\w+", "", cleaned)
  }

  # Hashtag removal
  if (isTRUE(config$remove_hashtags)) {
    cleaned <- gsub("#\\w+", "", cleaned)
  }

  # Common HTML entities
  cleaned <- .decode_html_entities(cleaned)

  cleaned
}
