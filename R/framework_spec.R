# ==============================================================================
# Framework Specification (Sprint-4 M3.1)
# ==============================================================================
# Mode 3 (Framework Applied) requires a researcher-supplied theoretical
# framework that the AI applies verbatim. The framework specifies:
#   - constructs (e.g., TPB's "attitude", "subjective norm", "perceived
#     behavioral control") -- these become the only codes the model is
#     allowed to use
#   - example indicators per construct (phrasings the AI looks for)
#   - epistemic stance (constructionist / positivist / mixed) -- governs
#     how the framework is applied
#   - anomaly handling policy (extend / revise / bracket) -- what to do
#     with entries that don't fit any construct
#
# This module loads + validates the framework spec from YAML/JSON and
# returns a typed FrameworkSpec S3 object. Pre-built frameworks (TPB,
# COM-B, TDF) ship in inst/extdata/frameworks/ -- users can point at
# those by name or supply their own file.
#
# AC8 (modes are configurations of one architecture, never separate code
# paths) is the load-bearing constraint: this module emits a typed object
# that the coding + theming dispatch in R/09_coding.R / R/13_themes.R
# consume identically to how Mode 2 consumes a generated codebook --
# Mode 3 doesn't FORK the pipeline, it just provides a different source
# of construct labels.
# ==============================================================================

#' Current schema version for FrameworkSpec
#' @keywords internal
.FRAMEWORK_SPEC_SCHEMA_VERSION <- "1.0.0"

#' Valid epistemic-stance enum values
#' @keywords internal
.VALID_EPISTEMIC_STANCES <- c("constructionist", "positivist", "mixed")

#' Valid anomaly-handling policy enum values
#'
#' @keywords internal
#' @details
#'   "extend"  -- add the anomaly as a new construct (Vila-Henninger 2024
#'                "abductive coding" mode). Researcher must explicitly
#'                accept each new construct.
#'   "revise"  -- modify an existing construct's definition to absorb the
#'                anomaly. Records a framework-revision entry.
#'   "bracket" -- mark the anomaly as "out of scope for this framework"
#'                without modifying the framework. Most positivist.
.VALID_ANOMALY_POLICIES <- c("extend", "revise", "bracket")

#' Load + validate a theoretical framework specification
#'
#' Reads YAML or JSON, validates against the M3.1 schema, and returns
#' a typed \code{FrameworkSpec} S3 object. The schema requires:
#' \itemize{
#'   \item \code{framework$name} -- non-empty character
#'   \item \code{framework$constructs} -- non-empty list, each with
#'     \code{id} (unique), \code{name}, \code{description}, optional
#'     \code{example_indicators}
#'   \item \code{framework$epistemic_stance} -- one of
#'     \code{"constructionist"}, \code{"positivist"}, \code{"mixed"}
#'   \item \code{framework$anomaly_handling} -- one of \code{"extend"},
#'     \code{"revise"}, \code{"bracket"}
#' }
#' Optional fields: \code{citations}, \code{code_definitions}.
#'
#' Construct \code{id} values must be unique within a framework (the
#' coding pipeline keys constructs by id). Validation errors point at
#' the specific construct that failed so users with a malformed spec
#' get an actionable message.
#'
#' @param path Path to a YAML or JSON file (extension determines parser).
#'   Special values: when \code{path} is one of the built-in framework
#'   names (e.g., \code{"tpb"}, \code{"comb"}, \code{"tdf"}), loads from
#'   \code{inst/extdata/frameworks/}.
#' @return A \code{FrameworkSpec} S3 object.
#' @seealso \code{\link{list_builtin_frameworks}};
#'   \code{\link{archive_framework_spec}} (writes a verbatim copy +
#'   sha256 to a Mode 3 run dir); \code{\link{framework_prompt_block}}
#'   (formats the spec for AI system prompts).
#' @examples
#' # Load a built-in framework by alias
#' tpb <- load_framework_spec("tpb")
#' print(tpb)
#'
#' list_builtin_frameworks()
#' # [1] "tpb"  "comb" "tdf"
#'
#' \dontrun{
#' # Load a custom spec from disk (YAML or JSON)
#' my_framework <- load_framework_spec("path/to/my_framework.yaml")
#' }
#' @export
load_framework_spec <- function(path) {
  if (!is.character(path) || length(path) != 1L || !nzchar(path)) {
    stop("load_framework_spec: `path` must be a single non-empty string",
         call. = FALSE)
  }

  # Built-in framework alias: load from package inst/extdata
  builtin_path <- .resolve_builtin_framework(path)
  if (!is.null(builtin_path)) path <- builtin_path

  if (!file.exists(path)) {
    stop(sprintf("load_framework_spec: file not found: %s", path),
         call. = FALSE)
  }

  raw <- .read_framework_file(path)
  spec <- .validate_framework_spec(raw, source_path = path)
  spec
}

#' Resolve a built-in framework name to its inst/extdata path
#'
#' Returns the file path for built-in framework aliases, or NULL when
#' the input is not an alias. Aliases:
#' \itemize{
#'   \item \code{"tpb"} -- Theory of Planned Behavior
#'   \item \code{"comb"} -- COM-B (Capability-Opportunity-Motivation)
#'   \item \code{"tdf"} -- Theoretical Domains Framework
#' }
#' @keywords internal
.resolve_builtin_framework <- function(name) {
  builtins <- c(
    tpb  = "tpb.yaml",
    comb = "comb.yaml",
    tdf  = "tdf.yaml"
  )
  key <- tolower(name)
  if (!key %in% names(builtins)) return(NULL)
  path <- system.file("extdata", "frameworks", builtins[[key]],
                       package = "pakhom")
  if (!nzchar(path) || !file.exists(path)) {
    log_warn("Built-in framework '{key}' resolved but file missing at {path}")
    return(NULL)
  }
  path
}

#' Read a framework spec file (YAML or JSON, by extension)
#' @keywords internal
.read_framework_file <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("yaml", "yml")) {
    tryCatch(yaml::yaml.load_file(path),
             error = function(e) stop(sprintf(
               "Could not parse framework YAML at %s: %s", path, e$message
             ), call. = FALSE))
  } else if (ext == "json") {
    tryCatch(jsonlite::read_json(path, simplifyVector = FALSE),
             error = function(e) stop(sprintf(
               "Could not parse framework JSON at %s: %s", path, e$message
             ), call. = FALSE))
  } else {
    stop(sprintf(
      "Unsupported framework spec extension: '%s' (expected yaml/yml/json)",
      ext
    ), call. = FALSE)
  }
}

#' Validate the parsed raw framework spec and build a FrameworkSpec object
#' @keywords internal
.validate_framework_spec <- function(raw, source_path = NA_character_) {
  if (!is.list(raw) || is.null(raw$framework)) {
    stop("Framework spec missing top-level `framework:` block", call. = FALSE)
  }
  fw <- raw$framework

  # Required: name
  if (is.null(fw$name) || !is.character(fw$name) || !nzchar(fw$name)) {
    stop("framework$name must be a non-empty string", call. = FALSE)
  }

  # Required: constructs (non-empty list)
  if (is.null(fw$constructs) || !is.list(fw$constructs) ||
      length(fw$constructs) == 0L) {
    stop("framework$constructs must be a non-empty list", call. = FALSE)
  }

  # Validate each construct + collect IDs for uniqueness check
  validated_constructs <- vector("list", length(fw$constructs))
  ids <- character(length(fw$constructs))
  for (i in seq_along(fw$constructs)) {
    c_in <- fw$constructs[[i]]
    if (!is.list(c_in)) {
      stop(sprintf("constructs[[%d]] must be a named list", i), call. = FALSE)
    }
    if (is.null(c_in$id) || !is.character(c_in$id) ||
        length(c_in$id) != 1L || !nzchar(c_in$id)) {
      stop(sprintf("constructs[[%d]]$id must be a non-empty string", i),
           call. = FALSE)
    }
    if (is.null(c_in$name) || !is.character(c_in$name) ||
        length(c_in$name) != 1L || !nzchar(c_in$name)) {
      stop(sprintf("constructs[[%d]]$name (id=%s) must be a non-empty string",
                   i, c_in$id), call. = FALSE)
    }
    if (is.null(c_in$description) || !is.character(c_in$description) ||
        length(c_in$description) != 1L) {
      stop(sprintf("constructs[[%d]]$description (id=%s) must be a string",
                   i, c_in$id), call. = FALSE)
    }
    indicators <- c_in$example_indicators %||% character(0)
    if (!is.character(indicators) && !is.list(indicators)) {
      stop(sprintf(
        "constructs[[%d]]$example_indicators (id=%s) must be a character vector or list",
        i, c_in$id
      ), call. = FALSE)
    }
    indicators <- as.character(unlist(indicators))
    ids[i] <- c_in$id
    validated_constructs[[i]] <- list(
      id           = c_in$id,
      name         = c_in$name,
      description  = c_in$description,
      example_indicators = indicators
    )
  }

  # Construct ID uniqueness
  if (anyDuplicated(ids) > 0L) {
    dup <- ids[duplicated(ids)][1L]
    stop(sprintf("Duplicate construct id: '%s' (must be unique within framework)",
                 dup), call. = FALSE)
  }

  # Epistemic stance (default: constructionist for safety)
  stance <- fw$epistemic_stance %||% "constructionist"
  if (!stance %in% .VALID_EPISTEMIC_STANCES) {
    stop(sprintf(
      "framework$epistemic_stance '%s' invalid; expected one of: %s",
      stance, paste(.VALID_EPISTEMIC_STANCES, collapse = ", ")
    ), call. = FALSE)
  }

  # Anomaly handling policy (default: bracket for safety)
  anomaly <- fw$anomaly_handling %||% "bracket"
  if (!anomaly %in% .VALID_ANOMALY_POLICIES) {
    stop(sprintf(
      "framework$anomaly_handling '%s' invalid; expected one of: %s",
      anomaly, paste(.VALID_ANOMALY_POLICIES, collapse = ", ")
    ), call. = FALSE)
  }

  # Optional citations
  citations <- if (is.null(fw$citations)) character(0) else
                 as.character(unlist(fw$citations))

  obj <- list(
    name              = fw$name,
    citations         = citations,
    epistemic_stance  = stance,
    anomaly_handling  = anomaly,
    constructs        = validated_constructs,
    construct_ids     = ids,
    source_path       = source_path,
    schema_version    = .FRAMEWORK_SPEC_SCHEMA_VERSION
  )
  class(obj) <- "FrameworkSpec"
  obj
}

#' Print method for FrameworkSpec
#' @param x A FrameworkSpec object
#' @param ... Ignored
#' @export
print.FrameworkSpec <- function(x, ...) {
  cat(sprintf("FrameworkSpec: %s\n", x$name))
  cat(sprintf("  Epistemic stance: %s\n", x$epistemic_stance))
  cat(sprintf("  Anomaly policy:   %s\n", x$anomaly_handling))
  cat(sprintf("  Constructs:       %d\n", length(x$constructs)))
  for (c in x$constructs) {
    desc <- if (nchar(c$description) > 80)
              paste0(substr(c$description, 1, 77), "...")
            else c$description
    cat(sprintf("    - %s (%s): %s\n", c$id, c$name, desc))
  }
  if (length(x$citations) > 0L) {
    cat(sprintf("  Citations:        %d\n", length(x$citations)))
    for (cit in x$citations) cat(sprintf("    %s\n", cit))
  }
  if (!is.na(x$source_path)) {
    cat(sprintf("  Source:           %s\n", x$source_path))
  }
  invisible(x)
}

#' Archive a Mode 3 framework spec into the run output directory
#'
#' Phase 32 (audit H1 + H2): a Mode 3 run loads
#' \code{config$methodology$framework_spec_path} into a typed
#' \code{FrameworkSpec} but never copies the source spec into the run
#' outputs. The HTML report's methodology stamp says "M3 - Framework
#' Applied" but a reviewer cannot reconstruct WHICH framework was used
#' (TPB? COM-B? TDF? a custom YAML?), what its citations are, or what
#' its anomaly handling policy says. Without the archive, replay /
#' methodology-paper provenance is broken.
#'
#' This helper writes a verbatim copy of the source spec to
#' \code{outputs/<run>/framework_applied.yaml} (or .json -- preserved
#' from source extension), computes a deterministic SHA-256 of the
#' file's bytes, and returns a metadata list suitable for stamping into
#' \code{run_metadata.json} via \code{init_run_state(...)}.
#'
#' Per AC4 ("methodology stamped on every output"), the archive is
#' mandatory for any Mode 3 run -- absence of the archive is a coverage
#' failure flagged by \code{verify_run_integrity}.
#'
#' @param spec A \code{FrameworkSpec} object (must carry a non-NA
#'   \code{source_path}). For built-in frameworks the source_path is
#'   the \code{system.file()} resolution at load time.
#' @param run_dir Path to the run output directory. Created if missing.
#' @param run_id Optional character: run id used for the AC4
#'   methodology stamp prepended to the archive (YAML/JSON comment).
#'   Phase 37 audit added the stamp; \code{run_id = NULL} omits the
#'   `| run: <id>` portion of the stamp.
#' @return Named list with \code{path} (path of the archived file
#'   under run_dir), \code{hash} (sha256 hex string of the ORIGINAL
#'   source spec -- not the post-stamp archive bytes -- so
#'   replay-equivalence is anchored to the source spec the user
#'   supplied), \code{name} (framework$name), \code{epistemic_stance},
#'   \code{anomaly_handling}, \code{n_constructs}, \code{schema_version},
#'   suitable to splat into \code{init_run_state(...)}.
#' @seealso \code{\link{load_framework_spec}};
#'   \code{\link{init_run_state}} (consumes the metadata).
#' @examples
#' spec <- load_framework_spec("tpb")
#' tmp <- tempfile()
#' dir.create(tmp)
#' arch <- archive_framework_spec(spec, tmp)
#' arch$relative_path  # "framework_applied.yaml"
#' nchar(arch$hash) == 64L  # TRUE -- sha256 hex string
#' file.exists(arch$path)   # TRUE
#' @export
archive_framework_spec <- function(spec, run_dir, run_id = NULL) {
  validate_class(spec, "FrameworkSpec")
  if (!is.character(run_dir) || length(run_dir) != 1L || !nzchar(run_dir)) {
    stop("archive_framework_spec: run_dir must be a single non-empty string",
         call. = FALSE)
  }
  src <- spec$source_path
  if (is.null(src) || !is.character(src) || length(src) != 1L ||
      is.na(src) || !nzchar(src) || !file.exists(src)) {
    stop(sprintf(
      "archive_framework_spec: spec$source_path '%s' is missing or unreadable; ",
      src %||% "<NA>"
    ),
    "the spec must have been loaded from a real file (load_framework_spec ",
    "sets source_path on read).", call. = FALSE)
  }
  if (!dir.exists(run_dir)) {
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Preserve the source extension; default to .yaml when the source
  # somehow lost its extension. The destination file name is fixed
  # ("framework_applied") so verify_run_integrity has a deterministic
  # path to check.
  ext <- tolower(tools::file_ext(src))
  if (!nzchar(ext) || !ext %in% c("yaml", "yml", "json")) {
    ext <- "yaml"
  }
  dest_name <- paste0("framework_applied.", ext)
  dest <- file.path(run_dir, dest_name)

  # Phase 37 audit (AC4 MEDIUM): compute sha256 over the SOURCE bytes
  # before any copy / stamp, so the returned hash anchors replay-
  # equivalence to the user-supplied spec rather than to the post-
  # stamp archive. The archived file gets a methodology comment
  # prepended (idempotent, structurally invisible to YAML/JSON
  # parsers) so AC4 ("methodology stamped on every output") holds at
  # the artifact level too.
  hash <- tryCatch(
    digest::digest(file = src, algo = "sha256", serialize = FALSE),
    error = function(e) {
      log_warn("archive_framework_spec: could not compute sha256 of source {src}: {e$message}")
      NA_character_
    }
  )

  ok <- file.copy(src, dest, overwrite = TRUE)
  if (!isTRUE(ok)) {
    stop(sprintf(
      "archive_framework_spec: failed to copy %s -> %s", src, dest
    ), call. = FALSE)
  }

  # Stamp the archived file (YAML accepts `#` comments natively; JSON
  # has no comment syntax so we wrap the JSON in stamp_methodology_json's
  # envelope).
  tryCatch({
    if (ext %in% c("yaml", "yml")) {
      body <- readLines(dest, warn = FALSE)
      # Idempotency: skip if first line already a methodology stamp
      if (length(body) == 0L || !grepl("^# methodology:", body[1L])) {
        header_lines <- c(
          sprintf("# methodology: %s%s",
                  methodology_label("framework_applied"),
                  if (!is.null(run_id) && nzchar(run_id))
                    sprintf(" | run: %s", run_id) else ""),
          sprintf("# source-sha256: %s", hash %||% "n/a"),
          "#"
        )
        writeLines(c(header_lines, body), dest)
      }
    } else if (identical(ext, "json")) {
      stamp_methodology_json(dest, "framework_applied", run_id = run_id)
    }
  }, error = function(e) {
    log_warn("archive_framework_spec: stamp failed for {dest}: {e$message}")
  })

  log_info("Mode 3 framework archived: {dest_name} (sha256 {substr(hash, 1, 12)}..., {length(spec$constructs)} constructs)")

  list(
    path             = dest,
    relative_path    = dest_name,
    hash             = hash,
    name             = spec$name,
    epistemic_stance = spec$epistemic_stance,
    anomaly_handling = spec$anomaly_handling,
    n_constructs     = length(spec$constructs),
    construct_ids    = spec$construct_ids,
    schema_version   = spec$schema_version
  )
}

#' List the built-in frameworks shipped with pakhom
#'
#' Returns a character vector of built-in framework aliases. Each alias
#' resolves via \code{\link{load_framework_spec}}.
#'
#' @return Character vector of alias names.
#' @export
list_builtin_frameworks <- function() {
  c("tpb", "comb", "tdf")
}

#' Build the prompt block describing the framework's constructs
#'
#' Formats the framework as a system-prompt section that the Mode 3
#' coding pipeline injects so the AI knows which constructs are
#' permitted code names + how to apply them. The block is kept compact
#' (one line per construct + one line per indicator) so it doesn't
#' dominate the context window.
#'
#' @param spec A FrameworkSpec object.
#' @return Character: the prompt block.
#' @export
framework_prompt_block <- function(spec) {
  validate_class(spec, "FrameworkSpec")
  lines <- c(
    sprintf("# THEORETICAL FRAMEWORK: %s", spec$name),
    sprintf("# (epistemic stance: %s; anomaly handling: %s)",
            spec$epistemic_stance, spec$anomaly_handling),
    "#",
    "# You will apply this framework verbatim to the entry. The constructs",
    "# below are the ONLY permitted code names. Do NOT invent new constructs.",
    "# When an entry segment does NOT fit any construct, code it as 'anomaly'",
    "# with a one-sentence reason rather than forcing a fit.",
    ""
  )
  for (c in spec$constructs) {
    lines <- c(lines, sprintf("- [%s] %s: %s", c$id, c$name, c$description))
    if (length(c$example_indicators) > 0L) {
      example_str <- paste(sprintf('"%s"', c$example_indicators), collapse = ", ")
      lines <- c(lines, sprintf("  Example indicators: %s", example_str))
    }
  }
  paste(lines, collapse = "\n")
}
