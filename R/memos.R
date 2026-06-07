# ==============================================================================
# Reflexive Memos as Data
# ==============================================================================
# Per AC6 (symmetric obligations across modes): Mode 2/3 carry researcher
# burden via the codebook-review and theme-review pause-points (CSV
# export-edit-reimport cycles); Mode 1 carries equivalent burden via
# reflexive memos at pause points + dynamic positionality. This module
# implements the memo layer: Memo S3 + add/read/list + Markdown round-trip
# with YAML frontmatter + persistence + load.
#
# Memo schema:
#   id                : memo_<ISO-timestamp-with-dashes>_<3-char-suffix>
#   timestamp         : ISO-8601 (with Z / +HHMM)
#   author            : free-text (default "researcher")
#   type              : operational | coding | theoretical | positionality
#   linked_codes      : character vector (may be empty)
#   linked_themes     : character vector (may be empty)
#   linked_entries    : character vector (may be empty -- std_id values)
#   linked_prior_memo : character or NULL (memo_id of antecedent memo)
#   body              : free-text Markdown
#
# Storage layout:
#   outputs/<run>/memos/<memo_id>.md   -- one file per memo
# ==============================================================================

#' Current schema version for the Memo S3 class
#' @keywords internal
.MEMO_SCHEMA_VERSION <- "1.0.0"

#' Valid memo type enum values
#'
#' The four memo types:
#'   operational  -- procedural notes (e.g., "decided to merge codes X+Y")
#'   coding       -- reflexive notes during the coding pass (Mode 2/3 mostly)
#'   theoretical  -- conceptual / interpretive memos (the classic Charmaz form)
#'   positionality -- positionality statements (input to M1.4 dynamic
#'                    positionality drift analysis); typed memos rather than
#'                    a separate slot so the timeline view can interleave
#'
#' @keywords internal
.VALID_MEMO_TYPES <- c("operational", "coding", "theoretical", "positionality")

# ==============================================================================
# Memo S3 constructor + ID generation
# ==============================================================================

#' Generate a deterministic-ish memo id
#'
#' Format: \code{memo_<ISO8601-with-dashes>_<3-char-random-suffix>}.
#' The timestamp is the canonical ordering key (chronological). The 3-char
#' suffix avoids collisions when two memos are added in the same second --
#' e.g., during a fast batch of provocation responses. The random
#' component is drawn from \code{[a-z0-9]} so the id is filesystem-safe
#' and shell-safe without quoting.
#'
#' @param timestamp ISO-8601 timestamp (defaults to now). Colons in the
#'   time portion are converted to dashes so the id is path-safe on
#'   Windows.
#' @return Character: a memo id.
#' @keywords internal
.generate_memo_id <- function(timestamp = NULL) {
  if (is.null(timestamp) || is.na(timestamp) || !nzchar(timestamp)) {
    timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  }
  # Convert "2026-05-03T10:33:00-0400" -> "2026-05-03T10-33-00-0400"
  ts_safe <- gsub(":", "-", timestamp, fixed = TRUE)
  # 3-char random suffix
  suffix <- paste0(sample(c(letters, 0:9), 3L, replace = TRUE), collapse = "")
  paste0("memo_", ts_safe, "_", suffix)
}

#' Construct a Memo S3 object
#'
#' The body is the researcher's
#' free-text Markdown; the header fields capture the memo's
#' position in the analytic timeline and its links to other artifacts.
#'
#' @param body Character: free-text Markdown content (the memo's body).
#' @param type Memo type; one of \code{"operational"}, \code{"coding"},
#'   \code{"theoretical"}, \code{"positionality"} (default
#'   \code{"theoretical"} per Charmaz convention -- the most common form
#'   when no other type is specified).
#' @param author Character: memo author. Defaults to \code{"researcher"}
#'   so single-researcher analyses don't have to set it; multi-researcher
#'   teams should record explicitly.
#' @param linked_codes Optional character vector of code ids the memo
#'   references.
#' @param linked_themes Optional character vector of theme names the
#'   memo references.
#' @param linked_entries Optional character vector of entry std_ids the
#'   memo cites.
#' @param linked_prior_memo Optional character: memo_id of an
#'   antecedent memo this one extends or revises (forms a chain for
#'   the timeline view).
#' @param timestamp Optional ISO-8601 timestamp (defaults to now).
#' @param id Optional explicit id (defaults to a generated one).
#' @return A \code{Memo} S3 object.
#' @export
make_memo <- function(body,
                       type = "theoretical",
                       author = "researcher",
                       linked_codes = character(0),
                       linked_themes = character(0),
                       linked_entries = character(0),
                       linked_prior_memo = NULL,
                       timestamp = NULL,
                       id = NULL) {
  if (!is.character(body) || length(body) != 1L) {
    stop("make_memo: body must be a single character string", call. = FALSE)
  }
  # Canonicalize body by stripping trailing whitespace
  # at construction time. The serializer (memo_to_markdown) emits the
  # body verbatim plus a single trailing newline; the parser
  # (markdown_to_memo) returns the body without any post-processing.
  # Canonicalizing here makes the round-trip trivially byte-equivalent
  # without losing meaningful content -- trailing whitespace in
  # Markdown bodies is rarely intentional, and a researcher who needs
  # trailing whitespace can use a non-whitespace marker like '.\n' in
  # their final line.
  body <- sub("\\s+$", "", body)
  if (!type %in% .VALID_MEMO_TYPES) {
    stop(sprintf(
      "make_memo: invalid type '%s'; expected one of: %s",
      type, paste(.VALID_MEMO_TYPES, collapse = ", ")
    ), call. = FALSE)
  }
  if (!is.character(author) || length(author) != 1L || !nzchar(author)) {
    stop("make_memo: author must be a single non-empty string",
         call. = FALSE)
  }
  if (is.null(timestamp)) {
    timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  }
  if (is.null(id)) {
    id <- .generate_memo_id(timestamp)
  }
  obj <- list(
    id                = as.character(id)[1L],
    timestamp         = as.character(timestamp)[1L],
    author            = as.character(author)[1L],
    type              = as.character(type)[1L],
    linked_codes      = as.character(linked_codes %||% character(0)),
    linked_themes     = as.character(linked_themes %||% character(0)),
    linked_entries    = as.character(linked_entries %||% character(0)),
    linked_prior_memo = if (!is.null(linked_prior_memo) &&
                              nzchar(linked_prior_memo))
                          as.character(linked_prior_memo)[1L]
                        else NA_character_,
    body              = as.character(body)[1L],
    schema_version    = .MEMO_SCHEMA_VERSION
  )
  class(obj) <- "Memo"
  obj
}

#' Print method for Memo
#' @param x A Memo object
#' @param ... Ignored
#' @export
print.Memo <- function(x, ...) {
  cat(sprintf("Memo [%s]\n", x$type))
  cat(sprintf("  ID:               %s\n", x$id))
  cat(sprintf("  Timestamp:        %s\n", x$timestamp))
  cat(sprintf("  Author:           %s\n", x$author))
  if (length(x$linked_codes) > 0L) {
    cat(sprintf("  Linked codes:     %s\n",
                paste(x$linked_codes, collapse = ", ")))
  }
  if (length(x$linked_themes) > 0L) {
    cat(sprintf("  Linked themes:    %s\n",
                paste(x$linked_themes, collapse = ", ")))
  }
  if (length(x$linked_entries) > 0L) {
    cat(sprintf("  Linked entries:   %d (%s%s)\n",
                length(x$linked_entries),
                paste(head(x$linked_entries, 3L), collapse = ", "),
                if (length(x$linked_entries) > 3L) ", ..." else ""))
  }
  # NULL-safe check. is.na(NULL) returns
  # logical(0) which makes `if` error with "argument is of length
  # zero". A Memo coming back from .read_reflection_log_json could
  # have NULL here when the YAML/JSON serialized "null" without
  # round-tripping to NA_character_. The constructor sets it to
  # NA_character_ so freshly-built memos are fine, but defending
  # against the resume case is cheap.
  if (!is.null(x$linked_prior_memo) &&
      length(x$linked_prior_memo) > 0L &&
      !is.na(x$linked_prior_memo)) {
    cat(sprintf("  Linked prior:     %s\n", x$linked_prior_memo))
  }
  body_preview <- if (nchar(x$body) > 200L)
                    paste0(substr(x$body, 1L, 197L), "...")
                  else x$body
  cat(sprintf("  Body (%d chars):\n    %s\n", nchar(x$body),
              gsub("\n", "\n    ", body_preview, fixed = TRUE)))
  invisible(x)
}

# ==============================================================================
# CRUD on a ResearcherReflectionLog
# ==============================================================================

#' Add a memo to a ResearcherReflectionLog
#'
#' R is pass-by-value: callers must capture the return:
#' \code{log <- add_memo(log, "...")}. The function appends the memo to
#' \code{log$memos} (preserving order) and updates \code{log$last_updated}.
#' When an \code{audit_log} is supplied, a \code{memo_added} decision is
#' recorded so the methodology paper has a timestamp trail of when each
#' memo was authored relative to AI calls.
#'
#' Memos are immutable once added: there is no \code{update_memo} or
#' \code{delete_memo} -- if a researcher needs to revise a thought, they
#' add a NEW memo with \code{linked_prior_memo} pointing at the old one.
#' The chain is the audit trail. This is intentional per the Birks/
#' Chapman/Francis 2025 "Memoing in qualitative research: two decades on"
#' guidance that memo evolution itself is data.
#'
#' @param log A \code{ResearcherReflectionLog}.
#' @param body Character: the memo's Markdown body (or pass a pre-built
#'   \code{Memo} object via \code{memo} instead).
#' @param ... Forwarded to \code{\link{make_memo}} (type, author,
#'   linked_codes, linked_themes, linked_entries, linked_prior_memo,
#'   timestamp, id). Ignored when \code{memo} is supplied.
#' @param memo Optional pre-built \code{Memo} object; supplying this
#'   bypasses \code{make_memo} construction. Mutually exclusive with
#'   \code{body}.
#' @param audit_log Optional \code{AuditLog}; when supplied, a
#'   \code{memo_added} decision is recorded.
#' @return The updated \code{ResearcherReflectionLog}.
#' @seealso \code{\link{make_memo}} (constructor);
#'   \code{\link{persist_memos}} (write all memos to disk as Markdown
#'   with YAML frontmatter); \code{\link{load_memos}} (read them back).
#' @examples
#' log <- create_reflection_log()
#'
#' # Add a theoretical memo linked to a theme
#' log <- add_memo(
#'   log,
#'   body = paste0(
#'     "Adherence themes are over-weighted by contributor X's posts.\n\n",
#'     "Need to interrogate this concentration before publishing."
#'   ),
#'   type = "theoretical",
#'   linked_themes = "Adherence"
#' )
#'
#' # Add an operational memo as a revision of the prior
#' log <- add_memo(
#'   log,
#'   body = "Merged codes med_routine + daily_pills into med_adherence.",
#'   type = "operational",
#'   linked_codes = c("med_routine", "daily_pills"),
#'   linked_prior_memo = log$memos[[1]]$id
#' )
#'
#' list_memos(log)
#' @export
add_memo <- function(log, body = NULL, ..., memo = NULL,
                       audit_log = NULL) {
  validate_class(log, "ResearcherReflectionLog")
  if (is.null(memo) && is.null(body)) {
    stop("add_memo: must supply either body (string) or memo (Memo object)",
         call. = FALSE)
  }
  if (!is.null(memo) && !is.null(body)) {
    stop("add_memo: body and memo are mutually exclusive", call. = FALSE)
  }
  if (is.null(memo)) {
    memo <- make_memo(body, ...)
  } else {
    validate_class(memo, "Memo")
  }

  log$memos[[length(log$memos) + 1L]] <- memo
  log$last_updated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")

  if (!is.null(audit_log)) {
    tryCatch(
      log_ai_decision(audit_log, "researcher_review", "memo_added",
                      memo_id    = memo$id,
                      memo_type  = memo$type,
                      memo_chars = nchar(memo$body),
                      author     = memo$author),
      error = function(e) NULL
    )
  }

  log
}

#' Read a memo from a ResearcherReflectionLog by id
#'
#' Returns NULL when no memo with the given id exists -- callers should
#' handle the NULL case explicitly rather than relying on an error,
#' because read-by-id is sometimes used as an existence check.
#'
#' @param log A \code{ResearcherReflectionLog}.
#' @param id Character: a memo id.
#' @return The \code{Memo} object, or NULL when not found.
#' @export
read_memo <- function(log, id) {
  validate_class(log, "ResearcherReflectionLog")
  if (length(log$memos) == 0L) return(NULL)
  for (m in log$memos) {
    if (identical(m$id, id)) return(m)
  }
  NULL
}

#' List memos in a ResearcherReflectionLog as a tibble
#'
#' Filterable summary view: one row per memo, columns are the schema
#' fields (id, timestamp, author, type, n_linked_codes, n_linked_themes,
#' n_linked_entries, body_chars, body_preview). Useful for the Mode 1
#' report's memos timeline and for programmatic introspection.
#'
#' @param log A \code{ResearcherReflectionLog}.
#' @param type Optional character: filter to memos of this type.
#' @param author Optional character: filter to memos by this author.
#' @param linked_theme Optional character: filter to memos linked to
#'   this theme.
#' @return A tibble (zero-row when no memos / nothing matches).
#' @export
list_memos <- function(log, type = NULL, author = NULL,
                          linked_theme = NULL) {
  validate_class(log, "ResearcherReflectionLog")
  if (length(log$memos) == 0L) {
    return(tibble::tibble(
      id = character(0), timestamp = character(0), author = character(0),
      type = character(0), n_linked_codes = integer(0),
      n_linked_themes = integer(0), n_linked_entries = integer(0),
      body_chars = integer(0), body_preview = character(0)
    ))
  }

  rows <- lapply(log$memos, function(m) {
    preview <- if (nchar(m$body) > 100L)
                 paste0(substr(m$body, 1L, 97L), "...")
               else m$body
    tibble::tibble(
      id              = m$id,
      timestamp       = m$timestamp,
      author          = m$author,
      type            = m$type,
      n_linked_codes  = length(m$linked_codes),
      n_linked_themes = length(m$linked_themes),
      n_linked_entries = length(m$linked_entries),
      body_chars      = nchar(m$body),
      body_preview    = preview
    )
  })
  out <- do.call(rbind, rows)

  if (!is.null(type)) out <- out[out$type == type, , drop = FALSE]
  if (!is.null(author)) out <- out[out$author == author, , drop = FALSE]
  if (!is.null(linked_theme)) {
    keep <- vapply(log$memos, function(m)
                     linked_theme %in% m$linked_themes, logical(1))
    keep <- keep[match(out$id,
                         vapply(log$memos, function(m) m$id, character(1)))]
    out <- out[keep, , drop = FALSE]
  }

  tibble::as_tibble(out)
}

# ==============================================================================
# Markdown round-trip with YAML frontmatter
# ==============================================================================

#' Serialize a Memo to a Markdown string with YAML frontmatter
#'
#' The frontmatter carries
#' the schema fields; the body follows after the closing \code{---}.
#' YAML is written with explicit quoting on fields that may contain
#' special characters so the round-trip is lossless even with
#' apostrophes, colons, etc.
#'
#' @param memo A \code{Memo} object.
#' @param methodology_mode Optional character: methodology mode to stamp
#'   into the frontmatter as \code{methodology_mode}. Per AC4
#'   (methodology stamped on every output), Mode 1 memos persisted to
#'   disk should carry the mode declaration so a memo lifted out of its
#'   run directory still self-identifies. Defaults to
#'   \code{"reflexive_scaffold"} (Mode 1) since memos are a Mode 1
#'   construct; pass NULL to omit the field.
#' @param run_id Optional character: run id to stamp into the
#'   frontmatter alongside methodology_mode. NULL omits the field.
#' @return Character: the full Markdown content (frontmatter + body).
#' @export
memo_to_markdown <- function(memo,
                              methodology_mode = "reflexive_scaffold",
                              run_id = NULL) {
  validate_class(memo, "Memo")

  # Build YAML frontmatter manually -- yaml::as.yaml is convenient but
  # produces inconsistent quoting on fields with special chars; manual
  # construction with explicit single-quote wrapping is safer.
  yaml_lines <- c(
    "---",
    sprintf("id: %s", .yaml_quote(memo$id)),
    sprintf("timestamp: %s", .yaml_quote(memo$timestamp)),
    sprintf("author: %s", .yaml_quote(memo$author)),
    sprintf("type: %s", .yaml_quote(memo$type)),
    sprintf("schema_version: %s", .yaml_quote(memo$schema_version)),
    sprintf("linked_codes: %s",
            .yaml_array(memo$linked_codes)),
    sprintf("linked_themes: %s",
            .yaml_array(memo$linked_themes)),
    sprintf("linked_entries: %s",
            .yaml_array(memo$linked_entries)),
    sprintf("linked_prior_memo: %s",
            if (is.na(memo$linked_prior_memo)) "null"
            else .yaml_quote(memo$linked_prior_memo))
  )
  # Per AC4, persist methodology + run_id on every
  # memo so a memo file lifted out of its run dir still self-identifies.
  if (!is.null(methodology_mode) && nzchar(methodology_mode)) {
    yaml_lines <- c(yaml_lines,
      sprintf("methodology_mode: %s", .yaml_quote(methodology_mode)))
  }
  if (!is.null(run_id) && nzchar(run_id)) {
    yaml_lines <- c(yaml_lines,
      sprintf("run_id: %s", .yaml_quote(run_id)))
  }
  yaml_lines <- c(yaml_lines,
    "---",
    "",
    memo$body,
    ""  # trailing newline for POSIX-friendliness
  )
  paste(yaml_lines, collapse = "\n")
}

#' YAML-quote a string for safe single-line frontmatter use
#' @keywords internal
.yaml_quote <- function(s) {
  if (is.null(s) || is.na(s)) return("null")
  s <- as.character(s)[1L]
  # Single-quote with embedded single-quotes doubled per YAML spec
  paste0("'", gsub("'", "''", s, fixed = TRUE), "'")
}

#' YAML-format a character vector as an array
#' @keywords internal
.yaml_array <- function(v) {
  if (length(v) == 0L) return("[]")
  paste0("[", paste(vapply(v, .yaml_quote, character(1)),
                     collapse = ", "), "]")
}

#' Parse a Markdown-with-YAML-frontmatter string back into a Memo
#'
#' Inverse of \code{\link{memo_to_markdown}}. Handles the YAML
#' frontmatter via \code{yaml::yaml.load} and treats everything after
#' the closing \code{---} as the memo body. Returns the Memo with its
#' S3 class restored.
#'
#' @param md_text Character: full Markdown content.
#' @return A \code{Memo} object.
#' @export
markdown_to_memo <- function(md_text) {
  if (!is.character(md_text) || length(md_text) != 1L) {
    stop("markdown_to_memo: md_text must be a single string", call. = FALSE)
  }
  lines <- strsplit(md_text, "\n", fixed = TRUE)[[1L]]
  if (length(lines) < 4L || !identical(trimws(lines[1L]), "---")) {
    stop("markdown_to_memo: input does not start with a YAML frontmatter '---' block",
         call. = FALSE)
  }
  # Find the closing --- line
  close_idx <- which(trimws(lines[-1L]) == "---")[1L]
  if (is.na(close_idx)) {
    stop("markdown_to_memo: YAML frontmatter not terminated by '---'",
         call. = FALSE)
  }
  yaml_block <- paste(lines[2L:close_idx], collapse = "\n")
  body_lines <- if ((close_idx + 2L) <= length(lines))
                  lines[(close_idx + 2L):length(lines)]
                else character(0)
  # Strip a single leading blank line if present -- memo_to_markdown
  # always emits one between the closing '---' and the body for
  # readability, but it's a serializer convention, not data. Without
  # this strip the body round-trip would acquire a leading "\n" each
  # save/load cycle.
  if (length(body_lines) > 0L && !nzchar(body_lines[1L])) {
    body_lines <- body_lines[-1L]
  }
  body <- paste(body_lines, collapse = "\n")
  # Trailing-whitespace handling lives in make_memo (called below) --
  # the constructor canonicalizes body so the round-trip is byte-
  # equivalent without this parser doing extra work.

  fm <- yaml::yaml.load(yaml_block)
  if (!is.list(fm)) {
    stop("markdown_to_memo: YAML frontmatter did not parse to a list",
         call. = FALSE)
  }

  # Coerce each field with NULL-safety -- a malformed frontmatter (e.g.,
  # missing optional fields) should produce a defensible Memo rather
  # than crashing.
  prior <- fm$linked_prior_memo
  prior <- if (is.null(prior) || identical(prior, NA) ||
                identical(tolower(as.character(prior)), "null"))
             NULL
           else as.character(prior)[1L]

  make_memo(
    body              = body,
    type              = fm$type %||% "theoretical",
    author            = fm$author %||% "researcher",
    linked_codes      = as.character(unlist(fm$linked_codes %||% character(0))),
    linked_themes     = as.character(unlist(fm$linked_themes %||% character(0))),
    linked_entries    = as.character(unlist(fm$linked_entries %||% character(0))),
    linked_prior_memo = prior,
    timestamp         = fm$timestamp %||% NULL,
    id                = fm$id %||% NULL
  )
}

# ==============================================================================
# Persistence: round-trip an entire ResearcherReflectionLog$memos to disk
# ==============================================================================

#' Persist all memos in a ResearcherReflectionLog to disk
#'
#' Writes one \code{outputs/<run>/memos/<memo_id>.md} per memo with full
#' YAML frontmatter. Idempotent: re-calling re-writes existing files
#' (memo content is immutable so re-writes produce byte-equivalent
#' output -- useful for replay-equivalence checks).
#'
#' Per AC4 ("methodology stamped on every output"), the memos directory
#' is a canonical Mode 1 artifact -- the integrity check expects
#' \code{memos/} to exist when any memos have been authored, and the
#' Mode 1 report renders a "Researcher Reflexive Memos" section
#' driven by the on-disk files.
#'
#' \strong{Overwrite policy}: if a memo's \code{.md} file has been
#' edited externally (e.g., manually fixed up in a text editor),
#' calling \code{persist_memos} again will overwrite that file
#' \emph{without warning}. The in-memory \code{Memo} is the
#' authoritative version. To preserve external edits, load the memos
#' back via \code{\link{load_memos}} before calling
#' \code{persist_memos} -- the load round-trip pulls any external
#' edits into the in-memory log first.
#'
#' @param log A \code{ResearcherReflectionLog}.
#' @param run_dir Path to the run output directory.
#' @param methodology_mode Optional character: methodology mode to
#'   stamp into each memo's YAML frontmatter (per AC4). Defaults to
#'   \code{"reflexive_scaffold"} since memos are a Mode 1 construct.
#'   Pass NULL to omit.
#' @param run_id Optional character: run id to stamp alongside the
#'   mode. Defaults to \code{basename(run_dir)} so a typical
#'   \code{run_mode1} call writes the right id automatically.
#' @return Invisibly: a character vector of the written file paths.
#' @export
persist_memos <- function(log, run_dir,
                            methodology_mode = "reflexive_scaffold",
                            run_id = NULL) {
  validate_class(log, "ResearcherReflectionLog")
  if (!is.character(run_dir) || length(run_dir) != 1L || !nzchar(run_dir)) {
    stop("persist_memos: run_dir must be a single non-empty string",
         call. = FALSE)
  }
  if (length(log$memos) == 0L) return(invisible(character(0)))

  memos_dir <- file.path(run_dir, "memos")
  if (!dir.exists(memos_dir)) {
    dir.create(memos_dir, recursive = TRUE, showWarnings = FALSE)
  }

  if (is.null(run_id) || !nzchar(run_id)) {
    run_id <- basename(run_dir)
  }

  paths <- character(0)
  for (m in log$memos) {
    if (!inherits(m, "Memo")) next
    path <- file.path(memos_dir, paste0(m$id, ".md"))
    md_text <- memo_to_markdown(m,
                                  methodology_mode = methodology_mode,
                                  run_id = run_id)
    tryCatch({
      writeLines(md_text, path, useBytes = TRUE)
      paths <- c(paths, path)
    }, error = function(e) {
      log_warn("persist_memos: could not write {path}: {e$message}")
    })
  }

  log_info("Persisted {length(paths)} memo(s) to {memos_dir}")
  invisible(paths)
}

#' Load memos from a run output directory back into Memo objects
#'
#' Reads every \code{memos/*.md} file in \code{run_dir} and parses it
#' via \code{\link{markdown_to_memo}}. Used by \code{run_mode1()} on
#' the resume path so a previously persisted memo set survives across
#' interrupted runs.
#'
#' @param run_dir Path to the run output directory.
#' @return List of \code{Memo} objects (zero-length when no memos
#'   exist or the memos directory is missing).
#' @export
load_memos <- function(run_dir) {
  if (!is.character(run_dir) || length(run_dir) != 1L || !nzchar(run_dir)) {
    stop("load_memos: run_dir must be a single non-empty string",
         call. = FALSE)
  }
  memos_dir <- file.path(run_dir, "memos")
  if (!dir.exists(memos_dir)) return(list())

  files <- list.files(memos_dir, pattern = "\\.md$", full.names = TRUE)
  if (length(files) == 0L) return(list())

  out <- list()
  for (f in files) {
    md_text <- tryCatch(
      paste(readLines(f, warn = FALSE, encoding = "UTF-8"),
            collapse = "\n"),
      error = function(e) {
        log_warn("load_memos: could not read {f}: {e$message}")
        NULL
      }
    )
    if (is.null(md_text)) next
    memo <- tryCatch(markdown_to_memo(md_text),
                       error = function(e) {
                         log_warn("load_memos: could not parse {f}: {e$message}")
                         NULL
                       })
    if (!is.null(memo)) out[[length(out) + 1L]] <- memo
  }
  # Order by timestamp so the timeline view is chronological.
  # Secondary sort by id when timestamps tie. Without the
  # secondary key, order() falls back to position-in-input which
  # depends on list.files() return order (alphabetical on most
  # filesystems but not contractually so). The secondary id sort
  # makes the result deterministic regardless of filesystem behavior.
  if (length(out) > 1L) {
    ts <- vapply(out, function(m) m$timestamp, character(1))
    ids <- vapply(out, function(m) m$id, character(1))
    out <- out[order(ts, ids)]
  }
  out
}
