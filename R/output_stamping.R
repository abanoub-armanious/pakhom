# ==============================================================================
# Output Stamping
# ==============================================================================
# Methodology mode appears in EVERY output artifact. Per AC4 (methodology
# stamped on every output), absence of the stamp is itself a transparency
# failure -- a reviewer who picks up a CSV / HTML / plot / console
# transcript should see at a glance which methodology mode produced it.
#
# This module centralizes the stamping API across output formats:
#   - HTML reports (methodology badge in the header)
#   - CSV exports (comment-style header line: "# methodology: ...")
#   - Console banners (sprintf-printable line)
#   - ggplot watermarks (subtle caption layer)
#   - Run-directory naming (mode short-code suffix: _M1 / _M2 / _M3)
#
# AC4 ("methodology stamped on every output") is the load-bearing
# architectural commitment this module implements. It is the
# ClinicalTrials.gov pattern: methodology declarations travel with the
# data, not in a separate document the reader is trusted to consult.
# ==============================================================================

#' Map a methodology mode to its short-code (M1 / M2 / M3)
#'
#' Used in run-directory names and filenames where the full mode string
#' would be visually noisy. The mapping is fixed: reflexive_scaffold = M1,
#' codebook_collaborative = M2, framework_applied = M3. Returns
#' \code{"M?"} for unknown / NULL modes (visible-failure rather than
#' silent-empty-suffix).
#'
#' @param mode Character, one of the methodology modes.
#' @return Character short-code.
#' @export
methodology_short_code <- function(mode) {
  if (is.null(mode) || is.na(mode) || !nzchar(mode)) return("M?")
  switch(mode,
    "reflexive_scaffold"     = "M1",
    "codebook_collaborative" = "M2",
    "framework_applied"      = "M3",
    "M?"
  )
}

#' Human-readable label for a methodology mode
#'
#' Used in display contexts (HTML badges, console banners) where the
#' raw mode string would be terse. Returns the mode with a Mode-N prefix:
#' \code{"M1 - Reflexive Scaffold"}, etc. Unknown modes render as
#' "Unknown methodology" so the absence is visible.
#'
#' @param mode Character.
#' @return Character label.
#' @export
methodology_label <- function(mode) {
  if (is.null(mode) || is.na(mode) || !nzchar(mode)) {
    return("Unknown methodology")
  }
  switch(mode,
    "reflexive_scaffold"     = "M1 - Reflexive Scaffold",
    "codebook_collaborative" = "M2 - Codebook Collaborative",
    "framework_applied"      = "M3 - Framework Applied",
    paste0("Unknown methodology (", mode, ")")
  )
}

#' One-line description of what the mode commits the AI to
#'
#' Used in HTML stamps as a tooltip / caption. Mirrors the per-mode
#' rule blocks in \code{R/methodology_rules.R} but in a one-liner form.
#'
#' @param mode Character.
#' @return Character description.
#' @export
methodology_description_short <- function(mode) {
  switch(mode %||% "",
    "reflexive_scaffold"     = "AI extractive only; no theme/code naming, no synthesis.",
    "codebook_collaborative" = "AI proposes codes; researcher names themes and synthesizes.",
    "framework_applied"      = "AI applies researcher-supplied framework verbatim; flags anomalies.",
    "Methodology not declared."
  )
}

#' Build a Mode N run-directory suffix for a fresh run
#'
#' Standard pakhom run dirs are timestamped (\code{run_2026-05-03_103415}).
#' T1.7 appends the mode short-code so the directory name itself carries
#' the methodology stamp:
#' \code{run_2026-05-03_103415_M1}.
#'
#' @param base_run_id Character, e.g. \code{"run_2026-05-03_103415"}.
#' @param mode Character methodology mode.
#' @return Character run_id with mode suffix.
#' @export
run_id_with_mode <- function(base_run_id, mode) {
  sc <- methodology_short_code(mode)
  paste0(base_run_id, "_", sc)
}

# ==============================================================================
# HTML stamping
# ==============================================================================

#' Build an HTML methodology badge for the report header
#'
#' Renders a small div with the mode label and short description. Designed
#' to live near the top of the generated report, just below the title.
#'
#' @param mode Character methodology mode.
#' @param run_id Optional character run identifier; rendered alongside
#'   the mode if supplied.
#' @return Character HTML.
#' @export
stamp_methodology_html <- function(mode, run_id = NULL) {
  label <- methodology_label(mode)
  desc  <- methodology_description_short(mode)
  run_html <- if (!is.null(run_id) && nzchar(run_id)) {
    sprintf('<span class="methodology-run-id"> &middot; run %s</span>',
            .html_esc_safe(run_id))
  } else ""
  paste0(
    '<div class="methodology-stamp">\n',
    '  <span class="methodology-stamp-label">Methodology:</span>\n',
    '  <span class="methodology-stamp-mode">', .html_esc_safe(label), '</span>\n',
    '  <span class="methodology-stamp-desc">', .html_esc_safe(desc), '</span>\n',
    '  ', run_html, '\n',
    '</div>\n'
  )
}

#' Tiny HTML escaper used by the stamping API
#'
#' A bigger \code{.html_esc} exists elsewhere but this module is
#' self-contained -- defining a safe local escaper avoids cross-file
#' load-order coupling.
#' @keywords internal
.html_esc_safe <- function(x) {
  if (is.null(x) || is.na(x)) return("")
  s <- as.character(x)
  s <- gsub("&", "&amp;",  s, fixed = TRUE)
  s <- gsub("<", "&lt;",   s, fixed = TRUE)
  s <- gsub(">", "&gt;",   s, fixed = TRUE)
  s <- gsub('"', "&quot;", s, fixed = TRUE)
  s <- gsub("'", "&#39;",  s, fixed = TRUE)
  s
}

# ==============================================================================
# CSV stamping
# ==============================================================================

#' Stamp a CSV file with a methodology comment header
#'
#' Writes a comment-style header line and an empty separator before the
#' CSV body. R's \code{readr::read_csv} and \code{utils::read.csv} both
#' accept a \code{comment} arg that skips lines starting with \code{#};
#' downstream consumers using those parsers transparently strip the stamp.
#' Consumers using a stricter parser see the comment and can re-export
#' without it.
#'
#' Two-line stamp (kept short for tabular tools that DON'T strip
#' comments -- they'll still parse the data starting line 3):
#'
#' \preformatted{
#' # methodology: M1 - Reflexive Scaffold | run: run_2026-...
#' #
#' col1,col2,col3
#' ...
#' }
#'
#' @param csv_path Path to a CSV file (will be re-written with the stamp).
#' @param mode Character methodology mode.
#' @param run_id Optional run identifier.
#' @return Invisibly returns \code{csv_path}.
#' @export
stamp_methodology_csv <- function(csv_path, mode, run_id = NULL) {
  if (!file.exists(csv_path)) {
    log_warn("stamp_methodology_csv: file not found: {csv_path}")
    return(invisible(csv_path))
  }
  body <- readLines(csv_path, warn = FALSE)
  # Idempotent: if the file already has a methodology stamp on line 1, no-op.
  if (length(body) > 0L && grepl("^# methodology:", body[1])) {
    log_debug("CSV already stamped, skipping: {csv_path}")
    return(invisible(csv_path))
  }
  header <- methodology_csv_header_lines(mode, run_id)
  writeLines(c(header, body), csv_path)
  invisible(csv_path)
}

#' Generate the comment-header lines (without writing) for a CSV stamp
#'
#' Useful for callers that build a CSV in-memory and want to prepend the
#' stamp before writing.
#' @keywords internal
methodology_csv_header_lines <- function(mode, run_id = NULL) {
  label <- methodology_label(mode)
  base <- sprintf("# methodology: %s", label)
  if (!is.null(run_id) && nzchar(run_id)) {
    base <- paste0(base, sprintf(" | run: %s", run_id))
  }
  c(base, "#")
}

# ==============================================================================
# JSON stamping
# ==============================================================================

#' Stamp a JSON file with a methodology envelope
#'
#' JSON files cannot accept comment-style headers (the format has no
#' comment syntax), so the stamp is added as a top-level
#' \code{_methodology_stamp} key on an envelope object that wraps the
#' original payload as \code{_payload}. Idempotent: re-stamping a file
#' that already has a \code{_methodology_stamp} envelope no-ops.
#'
#' Output shape:
#' \preformatted{
#' {
#'   "_methodology_stamp": {
#'     "mode": "reflexive_scaffold",
#'     "label": "M1 - Reflexive Scaffold",
#'     "run_id": "run_2026-...",
#'     "stamped_at": "2026-..."
#'   },
#'   "_payload": <original JSON object>
#' }
#' }
#'
#' Consumers reading the original payload should look at
#' \code{json[["_payload"]]} when the envelope is present, falling back
#' to the document root otherwise. Per AC4 every output gets a stamp;
#' per AC1 the consumer's parser is the one place the envelope is
#' acknowledged.
#'
#' @param json_path Path to a JSON file (will be re-written with the stamp).
#' @param mode Character methodology mode.
#' @param run_id Optional character run identifier.
#' @return Invisibly returns \code{json_path}.
#' @export
stamp_methodology_json <- function(json_path, mode, run_id = NULL) {
  if (!file.exists(json_path)) {
    log_warn("stamp_methodology_json: file not found: {json_path}")
    return(invisible(json_path))
  }
  payload <- tryCatch(
    jsonlite::read_json(json_path, simplifyVector = FALSE),
    error = function(e) {
      log_warn("Could not parse JSON for stamping: {e$message}")
      NULL
    }
  )
  if (is.null(payload)) return(invisible(json_path))

  # Idempotent: detect the envelope shape and skip
  if (is.list(payload) && !is.null(payload[["_methodology_stamp"]])) {
    log_debug("JSON already stamped, skipping: {json_path}")
    return(invisible(json_path))
  }

  envelope <- list(
    `_methodology_stamp` = list(
      mode       = mode,
      label      = methodology_label(mode),
      run_id     = run_id,
      stamped_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
    ),
    `_payload` = payload
  )
  tryCatch(
    jsonlite::write_json(envelope, json_path,
                          pretty = TRUE, auto_unbox = TRUE,
                          null = "null"),
    error = function(e) log_warn("Could not write stamped JSON: {e$message}")
  )
  invisible(json_path)
}

# ==============================================================================
# Console stamping
# ==============================================================================

#' Build a console banner string for the methodology mode
#'
#' Used at run start / end to print a one-line banner identifying the
#' mode in force. Not directly printed -- callers \code{cat()} or
#' \code{log_info()} the result so output formatting (prefixes, colors
#' from the logger) is consistent.
#'
#' @param mode Character methodology mode.
#' @param run_id Optional run identifier.
#' @return Character (single line).
#' @export
stamp_methodology_console <- function(mode, run_id = NULL) {
  label <- methodology_label(mode)
  base <- sprintf("[methodology: %s]", label)
  if (!is.null(run_id) && nzchar(run_id)) {
    base <- paste0(base, sprintf(" [run: %s]", run_id))
  }
  base
}

# ==============================================================================
# ggplot watermark caption
# ==============================================================================

#' Build a caption string suitable for use as a ggplot watermark
#'
#' Designed to be passed to \code{ggplot2::labs(caption = ...)} so every
#' plot the report generates carries the methodology stamp. Caption is
#' small, gray, italic by ggplot's default theme -- visible but
#' unobtrusive.
#'
#' @param mode Character methodology mode.
#' @param run_id Optional run identifier.
#' @return Character (single line).
#' @export
methodology_plot_caption <- function(mode, run_id = NULL) {
  label <- methodology_label(mode)
  base <- sprintf("pakhom %s", label)
  if (!is.null(run_id) && nzchar(run_id)) {
    base <- paste0(base, sprintf(" - run %s", run_id))
  }
  base
}
