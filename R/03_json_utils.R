# ==============================================================================
# JSON Parsing Utilities — Safe Parsing with Repair Strategies
# ==============================================================================

#' Parse JSON safely with automatic repair for truncated/malformed responses
#'
#' Tries standard parsing first, then progressively more aggressive repair
#' strategies. Returns NULL (not an error) when all strategies fail.
#'
#' @param response Raw JSON string from AI API
#' @param expected_key If provided, validates this key exists in parsed result
#' @param max_repair_attempts Number of repair strategies to try (1-3)
#' @return Parsed R object (list/data.frame) or NULL on failure
parse_json_safely <- function(response, expected_key = NULL,
                              max_repair_attempts = 3) {
  # Handle vector inputs FIRST (the length-1
  # collapse used to happen AFTER `is.na(response)`, which errored on
  # length > 1 with "'length = 2' in coercion to 'logical(1)'" because
  # `||` requires scalar operands. Caught by the new vector-input branch
  # test; the function's docstring claims vector handling so the bug was
  # purely on the early-return guard.)
  if (is.null(response)) {
    log_warn("Empty JSON response received")
    return(NULL)
  }
  if (length(response) != 1L) {
    log_warn("parse_json_safely received vector of length {length(response)}, using first element")
    response <- response[1L]
  }
  if (is.na(response) || nchar(trimws(response)) == 0) {
    log_warn("Empty JSON response received")
    return(NULL)
  }

  # Clean common artifacts
  cleaned <- trimws(response)
  cleaned <- gsub("^```json\\s*", "", cleaned)
  cleaned <- gsub("^```\\s*", "", cleaned)
  cleaned <- gsub("\\s*```$", "", cleaned)

  # Strategy 0: Direct parse
  result <- .try_parse(cleaned, expected_key)
  if (!is.null(result)) return(result)

  if (max_repair_attempts < 1) return(NULL)

  # Strategy 1: Close unclosed brackets
  log_debug("JSON parse failed, trying bracket repair...")
  repaired <- .repair_close_brackets(cleaned)
  result <- .try_parse(repaired, expected_key)
  if (!is.null(result)) return(result)

  if (max_repair_attempts < 2) return(NULL)

  # Strategy 2: Truncate to last complete element
  log_debug("Trying truncation repair...")
  repaired <- .repair_truncated_element(cleaned)
  result <- .try_parse(repaired, expected_key)
  if (!is.null(result)) return(result)

  if (max_repair_attempts < 3) return(NULL)

  # Strategy 3: Find largest valid JSON subset
  log_debug("Trying JSON subset extraction...")
  repaired <- .repair_find_valid_subset(cleaned)
  result <- .try_parse(repaired, expected_key)
  if (!is.null(result)) return(result)

  log_warn("All JSON repair strategies failed")
  NULL
}

#' Try to parse JSON and optionally validate expected key
#' @keywords internal
.try_parse <- function(json_str, expected_key = NULL) {
  parsed <- tryCatch(
    fromJSON(json_str, simplifyVector = TRUE, simplifyDataFrame = TRUE),
    error = function(e) NULL
  )

  if (is.null(parsed)) return(NULL)

  if (!is.null(expected_key) && is.null(parsed[[expected_key]])) {
    return(NULL)
  }

  parsed
}

#' Close unclosed brackets and braces
#' @keywords internal
.repair_close_brackets <- function(json) {
  chars <- strsplit(json, "")[[1]]
  n <- length(chars)
  in_string <- FALSE
  escape_next <- FALSE
  open_braces <- 0L
  open_brackets <- 0L

  for (i in seq_len(n)) {
    ch <- chars[i]
    if (escape_next) {
      escape_next <- FALSE
      next
    }
    if (ch == "\\") {
      escape_next <- TRUE
      next
    }
    if (ch == '"') {
      in_string <- !in_string
      next
    }
    if (!in_string) {
      if (ch == "{") open_braces <- open_braces + 1L
      else if (ch == "}") open_braces <- open_braces - 1L
      else if (ch == "[") open_brackets <- open_brackets + 1L
      else if (ch == "]") open_brackets <- open_brackets - 1L
    }
  }

  repair <- json
  if (open_brackets > 0) repair <- paste0(repair, strrep("]", open_brackets))
  if (open_braces > 0) repair <- paste0(repair, strrep("}", open_braces))

  repair
}

#' Truncate to last complete array element (string-context-aware)
#' @keywords internal
.repair_truncated_element <- function(json) {
  chars <- strsplit(json, "")[[1]]
  n <- length(chars)
  in_string <- FALSE
  escape_next <- FALSE
  last_safe_pos <- 0L

  for (i in seq_len(max(n - 1, 1))) {
    ch <- chars[i]
    if (escape_next) { escape_next <- FALSE; next }
    if (ch == "\\") { escape_next <- TRUE; next }
    if (ch == '"') { in_string <- !in_string; next }
    if (!in_string) {
      # Track }, or ], as safe truncation points (outside strings)
      if (ch %in% c("}", "]") && i < n && chars[i + 1] == ",") {
        last_safe_pos <- i
      }
    }
  }

  if (last_safe_pos > 10) {
    .repair_close_brackets(substr(json, 1, last_safe_pos))
  } else {
    json
  }
}

#' Extract the largest valid JSON object from a string
#' @keywords internal
.repair_find_valid_subset <- function(json) {
  # Try to find a complete top-level object
  first_brace <- regexpr("\\{", json)
  if (first_brace < 1) return(json)

  # Walk through finding matching braces
  chars <- strsplit(json, "")[[1]]
  depth <- 0
  in_string <- FALSE
  escape_next <- FALSE


  for (i in first_brace:length(chars)) {
    if (escape_next) {
      escape_next <- FALSE
      next
    }
    if (chars[i] == "\\") {
      escape_next <- TRUE
      next
    }
    if (chars[i] == '"') {
      in_string <- !in_string
      next
    }
    if (!in_string) {
      if (chars[i] == "{") depth <- depth + 1
      if (chars[i] == "}") {
        depth <- depth - 1
        if (depth == 0) {
          return(substr(json, first_brace, i))
        }
      }
    }
  }

  # If no complete object was found, try bracket repair on original
  json
}
