# ==============================================================================
# QDPX Export — Interoperability with ATLAS.ti / NVivo / MAXQDA
# ==============================================================================
# Exports a ProgressiveCodingState (codebook + coded segments) and optionally a
# ThemeSet (hierarchical theme > subtheme > code structure) to the open QDPX
# exchange format.  QDPX is a ZIP archive containing:
#   - project.qde  (XML codebook + coding references)
#   - sources/      (plain-text source files, one per entry)
#
# Uses xml2 for XML construction and utils::zip() for the archive.
# ==============================================================================

# ==============================================================================
# GUID generation
# ==============================================================================

#' Generate a unique GUID for QDPX elements
#'
#' Produces a deterministic-looking but unique identifier prefixed with "TA-"
#' (for pakhom).  Uniqueness is ensured by combining the current timestamp,
#' a random integer, and an optional tag.
#'
#' @param tag Optional character string appended for readability
#' @return Character scalar, e.g. "TA-20250520143012-472913-code"
#' @keywords internal
.qdpx_guid <- function(tag = NULL) {
  base <- paste0(
    "TA-",
    format(Sys.time(), "%Y%m%d%H%M%S", tz = "UTC"),
    "-",
    sample(100000:999999, 1)
  )
  if (!is.null(tag) && nchar(tag) > 0) {
    base <- paste0(base, "-", gsub("[^a-zA-Z0-9_-]", "", tag))
  }
  base
}

# ==============================================================================
# XML helpers
# ==============================================================================

#' Strip XML-1.0-illegal control characters from a string
#'
#' The XML 1.0 specification (section 2.2) forbids the C0 control characters
#' except tab (0x09), line feed (0x0A) and carriage return (0x0D). Such
#' characters cannot be represented in XML at all -- not even as a numeric
#' character reference -- so any corpus-, AI- or user-derived text must have
#' them removed before it is serialised into the .qde. libxml2 handles them
#' inconsistently across versions (some silently substitute U+FFFD, others
#' emit an un-parseable reference or the raw byte), which made an export
#' readable on one platform yet rejected as malformed on another. Stripping
#' them up front guarantees a well-formed, portable document. Tab/LF/CR and
#' every printable character (accented letters, CJK, emoji) are preserved.
#'
#' @param x Character string or vector
#' @return The input with XML-illegal control characters removed
#' @keywords internal
.strip_invalid_xml_chars <- function(x) {
  if (is.null(x) || length(x) == 0) return(x)
  x <- enc2utf8(as.character(x))
  # Build the illegal-character class from code points so this source stays
  # pure ASCII rather than embedding raw control bytes: 0x01-0x08, 0x0B, 0x0C
  # and 0x0E-0x1F (every C0 control except the XML-legal tab/LF/CR).
  illegal <- paste(intToUtf8(c(1:8, 11, 12, 14:31), multiple = TRUE), collapse = "")
  gsub(paste0("[", illegal, "]"), "", x, perl = TRUE)
}

#' Escape a string for safe inclusion as XML text content
#'
#' @param x Character string
#' @return Escaped character string
#' @keywords internal
.xml_escape <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x[1])) return("")
  x <- .strip_invalid_xml_chars(as.character(x[1]))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&apos;", x, fixed = TRUE)
  x
}

#' Add a \code{<Code>} XML node to a parent element
#'
#' @param parent xml2 node to attach to
#' @param guid Character GUID
#' @param name Character code/theme name
#' @param is_codable Logical — TRUE for leaf codes, FALSE for grouping nodes
#' @param description Optional character description
#' @return The newly created xml2 node (invisibly)
#' @keywords internal
.add_code_node <- function(parent, guid, name, is_codable = TRUE,
                           description = NULL) {
  node <- xml2::xml_add_child(parent, "Code",
                               guid = guid,
                               name = .strip_invalid_xml_chars(name),
                               isCodable = tolower(as.character(is_codable)))
  if (!is.null(description) && nchar(description) > 0) {
    desc_node <- xml2::xml_add_child(node, "Description")
    xml2::xml_set_text(desc_node, .strip_invalid_xml_chars(description))
  }
  invisible(node)
}

# ==============================================================================
# Code-to-theme mapping helpers
# ==============================================================================

#' Build a reverse map from code_name -> list(theme, subtheme) using ThemeSet
#'
#' Walks the ThemeSet structure and its merge_history (if present) to figure out
#' which theme and subtheme each code belongs to.
#'
#' @param theme_set ThemeSet object
#' @param coding_state ProgressiveCodingState (for codebook key -> name lookup)
#' @return Named list keyed by code_name, each value a list with
#'   \code{theme_name} and \code{subtheme_name} (the latter may be NULL).
#' @keywords internal
.build_code_hierarchy <- function(theme_set, coding_state) {
  hierarchy <- list()

  merge_history <- theme_set$merge_history
  code_to_theme_map <- if (!is.null(merge_history)) {
    merge_history$code_to_theme_map %||% list()
  } else {
    list()
  }
  code_to_subtheme_map <- if (!is.null(merge_history)) {
    merge_history$code_to_subtheme_map %||% list()
  } else {
    list()
  }

  # Walk codebook entries

  for (code_key in names(coding_state$codebook)) {
    code_name <- coding_state$codebook[[code_key]]$code_name %||% code_key
    theme_name <- code_to_theme_map[[code_key]]
    subtheme_name <- code_to_subtheme_map[[code_key]]

    # Fallback: if no merge_history map, walk Theme -> Subtheme -> Code
    # hierarchy directly. Subthemes hold Code S3 objects rather
    # than bare strings, so resolve subtheme membership by Code$name.
    # subthemes may now contain nested sub-
    # subthemes; check both the subtheme's direct codes AND any nested
    # children before attributing membership.
    if (is.null(theme_name)) {
      # Recursive helper: collect all code names under a Subtheme,
      # including nested sub-subthemes.
      collect_all_names <- function(st) {
        if (!inherits(st, "Subtheme")) return(character(0))
        out <- subtheme_code_names(st)
        for (child in st$subthemes %||% list()) {
          out <- c(out, collect_all_names(child))
        }
        out
      }
      for (t in theme_set$themes) {
        if (!(code_name %in% theme_codes(t))) next
        theme_name <- t$name
        for (s in t$subthemes %||% list()) {
          if (!inherits(s, "Subtheme")) next
          if (is.na(s$name) || nchar(s$name %||% "") == 0L) next
          if (code_name %in% collect_all_names(s)) {
            subtheme_name <- s$name
            break
          }
        }
        break
      }
    }

    hierarchy[[code_name]] <- list(
      theme_name = theme_name,
      subtheme_name = subtheme_name
    )
  }

  hierarchy
}

# ==============================================================================
# Source file writer
# ==============================================================================

#' Write plain-text source files into the sources/ directory
#'
#' @param data Tibble with std_id, std_text columns
#' @param sources_dir Path to sources/ directory inside the staging area
#' @return Named character vector: entry_id -> file path (relative to archive root)
#' @keywords internal
.write_source_files <- function(data, sources_dir) {
  dir.create(sources_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- character(nrow(data))
  names(paths) <- as.character(data$std_id)

  for (i in seq_len(nrow(data))) {
    entry_id <- as.character(data$std_id[i])
    entry_text <- as.character(data$std_text[i])
    if (is.na(entry_text)) entry_text <- ""

    fname <- paste0("entry_", entry_id, ".txt")
    fpath <- file.path(sources_dir, fname)
    writeLines(entry_text, fpath, useBytes = TRUE)
    paths[entry_id] <- paste0("sources/", fname)
  }

  paths
}

# ==============================================================================
# XML document builder
# ==============================================================================

#' Build the project.qde XML document
#'
#' @param coding_state ProgressiveCodingState
#' @param data Tibble with std_id, std_text columns
#' @param source_paths Named character vector from \code{.write_source_files}
#' @param theme_set Optional ThemeSet for hierarchical codes
#' @param study_name Character study name
#' @return xml2 document
#' @keywords internal
.build_qde_xml <- function(coding_state, data, source_paths,
                            theme_set = NULL, study_name = "pakhom export",
                            output_path = NULL) {

  # QDPX creationDateTime in UTC so the value is unambiguous when imported
  # into NVivo / MAXQDA across timezones.
  creation_dt <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  project_guid <- .qdpx_guid("project")
  user_guid <- .qdpx_guid("user")

  # Create root document
  doc <- xml2::read_xml(paste0(
    '<?xml version="1.0" encoding="UTF-8"?>',
    '<Project name="', .xml_escape(study_name), '" ',
    'origin="pakhom" ',
    'creatingUserGuid="', user_guid, '" ',
    'creationDateTime="', creation_dt, '">',
    '</Project>'
  ))

  project_node <- xml2::xml_root(doc)

  # T0.1 + T1.7 (AC4) transparency: stamp the project's Description with
  # the methodology mode AND Tier-0 verification stats so a reviewer
  # importing this QDPX into ATLAS.ti / NVivo / MAXQDA sees the
  # methodology declaration AND the anti-fabrication stats at the project
  # level. Per AC4 (methodology stamped on every output), QDPX is itself
  # an output that gets the methodology stamp.
  tryCatch({
    # Methodology mode is read from the coding_state's run metadata when
    # available; otherwise stamps "Methodology not declared". This avoids
    # threading mode through every QDPX call site at the cost of a tiny
    # filesystem read.
    meth_mode <- tryCatch({
      ms <- attr(coding_state, "methodology_mode")
      if (!is.null(ms)) ms else NULL
    }, error = function(e) NULL)
    desc_parts <- character(0)
    if (!is.null(meth_mode)) {
      desc_parts <- c(desc_parts, sprintf("Methodology: %s.",
                                            methodology_label(meth_mode)))
    } else {
      desc_parts <- c(desc_parts, "Methodology: not declared.")
    }
    desc_parts <- c(desc_parts, "Generated by pakhom.")

    prov_stats <- compute_quote_provenance_stats(coding_state)
    if (!is.null(prov_stats) && !identical(prov_stats$total, 0L)) {
      total <- prov_stats$total
      v_rate <- prov_stats$verification_rate %||% NA_real_
      v_pct  <- if (is.na(v_rate)) "n/a" else sprintf("%.1f%%", 100 * v_rate)
      n_api  <- prov_stats$n_citations_api %||% 0L

      # report pre-rejection fabrication count
      # honestly. compute_quote_provenance_stats's fabrication_rate is
      # computed over SURVIVING quotes only (i.e. close to zero by
      # construction since fabricated quotes are dropped before
      # surfacing). An earlier QDPX <Description> reported that
      # near-zero rate as "fabrication rate 0.0%" -- which lied to a
      # reviewer importing the QDPX about whether fabrication
      # detection actually fired. Now read fabrication_log.csv to
      # surface the pre-rejection count + rate alongside the
      # post-rejection verification rate, parallel to the same
      # fix in the T0.1 dashboard.
      # output_path is the .qdpx file path passed by export_qdpx; the
      # fabrication_log.csv lives in the same directory.
      output_dir <- if (!is.null(output_path)) dirname(output_path) else "."
      fab_log_path <- file.path(output_dir, "fabrication_log.csv")
      n_caught <- .count_pre_rejection_fabrications(
        fabrication_log_path = fab_log_path
      )
      n_attempts <- if (!is.null(n_caught)) total + n_caught else total
      caught_pct <- if (!is.null(n_caught) && n_attempts > 0L) {
        sprintf("%.1f%%", 100 * n_caught / n_attempts)
      } else "0.0%"

      desc_parts <- c(desc_parts,
        sprintf("Quote provenance: %d AI-attributed verbatim claims surviving verification (of %d attempts);",
                 total, n_attempts),
        sprintf("verification rate %s (post-rejection); fabrication caught + excluded: %s (%s of attempts).",
                 v_pct,
                 if (is.null(n_caught)) "n/a"
                 else format(n_caught, big.mark = ","),
                 caught_pct),
        sprintf("Anthropic Citations API path: %d/%d surviving quotes.",
                 n_api, total)
      )
    }
    desc_parts <- c(desc_parts,
      "Per Jowsey et al. 2025 (PLOS One, doi:10.1371/journal.pone.0330217),",
      "AI-assisted thematic analysis tools must report quote-fabrication",
      "rates and corpus-coverage transparency."
    )
    desc_node <- xml2::xml_add_child(project_node, "Description")
    xml2::xml_set_text(desc_node, paste(desc_parts, collapse = " "))
  }, error = function(e) {
    # Non-fatal: keep building the QDPX even if verification stats fail
    log_debug("QDPX: skipping Tier-0 description ({e$message})")
  })

  # ------------------------------------------------------------------
  # 1. CodeBook
  # ------------------------------------------------------------------
  codebook_node <- xml2::xml_add_child(project_node, "CodeBook")
  codes_node <- xml2::xml_add_child(codebook_node, "Codes")

  # Build GUID map for every code in the codebook (keyed by code_key)
  code_guid_map <- list()
  for (code_key in names(coding_state$codebook)) {
    code_guid_map[[code_key]] <- .qdpx_guid("code")
  }

  if (!is.null(theme_set) && length(theme_set$themes) > 0) {
    # --- Hierarchical: Theme > (Subtheme) > Code ---
    hierarchy <- .build_code_hierarchy(theme_set, coding_state)
    placed_codes <- character(0)

    for (theme in theme_set$themes) {
      theme_guid <- .qdpx_guid("theme")
      theme_node <- .add_code_node(codes_node,
                                    guid = theme_guid,
                                    name = theme$name,
                                    is_codable = FALSE,
                                    description = theme$description)

      # Determine which codes belong to each subtheme (if any)
      subtheme_codes <- list()  # subtheme_name -> list of code_keys
      orphan_codes <- character(0)  # codes in this theme but no subtheme

      for (code_key in names(coding_state$codebook)) {
        code_name <- coding_state$codebook[[code_key]]$code_name %||% code_key
        h <- hierarchy[[code_name]]
        if (is.null(h) || is.null(h$theme_name)) next
        if (h$theme_name != theme$name) next

        if (!is.null(h$subtheme_name) && nchar(h$subtheme_name) > 0) {
          subtheme_codes[[h$subtheme_name]] <- c(
            subtheme_codes[[h$subtheme_name]], code_key
          )
        } else {
          orphan_codes <- c(orphan_codes, code_key)
        }
        placed_codes <- c(placed_codes, code_key)
      }

      # Add subtheme nodes
      if (length(subtheme_codes) > 0) {
        for (sname in names(subtheme_codes)) {
          # Find subtheme description if available.
          # subthemes_structured is now a list of Subtheme S3 objects;
          # add an inherits() guard so any non-S3 element (defensive
          # against hand-built ThemeSets) is skipped rather than crashing
          # on `s$name` access.
          sdesc <- ""
          for (s in theme$subthemes %||% list()) {
            if (!inherits(s, "Subtheme")) next
            if (identical(s$name, sname)) {
              sdesc <- s$description %||% ""
              break
            }
          }
          sub_guid <- .qdpx_guid("subtheme")
          sub_node <- .add_code_node(theme_node,
                                      guid = sub_guid,
                                      name = sname,
                                      is_codable = FALSE,
                                      description = sdesc)

          for (ck in subtheme_codes[[sname]]) {
            cb <- coding_state$codebook[[ck]]
            .add_code_node(sub_node,
                           guid = code_guid_map[[ck]],
                           name = cb$code_name %||% ck,
                           is_codable = TRUE,
                           description = cb$description)
          }
        }
      }

      # Add orphan codes directly under the theme
      for (ck in orphan_codes) {
        cb <- coding_state$codebook[[ck]]
        .add_code_node(theme_node,
                       guid = code_guid_map[[ck]],
                       name = cb$code_name %||% ck,
                       is_codable = TRUE,
                       description = cb$description)
      }
    }

    # Any codes not placed under a theme go at the root level
    unplaced <- setdiff(names(coding_state$codebook), placed_codes)
    if (length(unplaced) > 0) {
      log_info("QDPX export: {length(unplaced)} codes not mapped to any theme -- added at root level")
      for (ck in unplaced) {
        cb <- coding_state$codebook[[ck]]
        .add_code_node(codes_node,
                       guid = code_guid_map[[ck]],
                       name = cb$code_name %||% ck,
                       is_codable = TRUE,
                       description = cb$description)
      }
    }

  } else {
    # --- Flat code list (no theme hierarchy) ---
    for (code_key in names(coding_state$codebook)) {
      cb <- coding_state$codebook[[code_key]]
      .add_code_node(codes_node,
                     guid = code_guid_map[[code_key]],
                     name = cb$code_name %||% code_key,
                     is_codable = TRUE,
                     description = cb$description)
    }
  }

  # ------------------------------------------------------------------
  # 2. Sources + Coding references
  # ------------------------------------------------------------------
  sources_node <- xml2::xml_add_child(project_node, "Sources")

  for (i in seq_len(nrow(data))) {
    entry_id <- as.character(data$std_id[i])
    entry_text <- as.character(data$std_text[i])
    if (is.na(entry_text)) entry_text <- ""

    source_name <- paste0("entry_", entry_id)
    source_path <- source_paths[entry_id]
    source_guid <- .qdpx_guid("source")

    src_node <- xml2::xml_add_child(sources_node, "TextSource",
                                     guid = source_guid,
                                     name = source_name,
                                     plainTextPath = source_path,
                                     creationDateTime = creation_dt)

    # Add Coding elements for this entry
    er <- coding_state$entry_results[[entry_id]]
    if (is.null(er) || isTRUE(er$skipped) || length(er$coded_segments) == 0) {
      next
    }

    for (seg in er$coded_segments) {
      code_key <- seg$code_key
      if (is.null(code_key) || !(code_key %in% names(code_guid_map))) next

      # Determine text positions
      start_pos <- seg$start_char
      end_pos <- seg$end_char

      # If positions are missing, try to locate the segment text in the entry
      if (is.null(start_pos) || is.na(start_pos) ||
          is.null(end_pos) || is.na(end_pos)) {
        seg_text <- seg$text %||% ""
        if (nchar(seg_text) > 0) {
          match_pos <- regexpr(seg_text, entry_text, fixed = TRUE)
          if (match_pos > 0) {
            start_pos <- as.integer(match_pos - 1L)  # 0-based
            end_pos <- start_pos + nchar(seg_text)
          } else {
            # Fallback: cover entire entry
            start_pos <- 0L
            end_pos <- nchar(entry_text)
          }
        } else {
          start_pos <- 0L
          end_pos <- nchar(entry_text)
        }
      }

      # Ensure positions are valid integers
      start_pos <- max(0L, as.integer(start_pos))
      end_pos <- min(nchar(entry_text), as.integer(end_pos))
      if (end_pos <= start_pos) {
        end_pos <- nchar(entry_text)
      }

      coding_guid <- .qdpx_guid("coding")
      coding_node <- xml2::xml_add_child(src_node, "Coding",
                                          guid = coding_guid,
                                          creatingUser = "pakhom")

      xml2::xml_add_child(coding_node, "CodeRef",
                           targetGUID = code_guid_map[[code_key]])

      xml2::xml_add_child(coding_node, "TextSelection",
                           startPosition = as.character(start_pos),
                           endPosition = as.character(end_pos))
    }
  }

  doc
}

# ==============================================================================
# Main export function
# ==============================================================================

#' Export coding results to QDPX format
#'
#' Creates a QDPX file (ZIP archive) that can be imported into ATLAS.ti, NVivo,
#' MAXQDA, and other qualitative data analysis software that supports the QDPX
#' exchange standard.
#'
#' The archive contains:
#' \itemize{
#'   \item \code{project.qde} — XML file with the codebook structure and all
#'     coding references (text selections linked to codes).
#'   \item \code{sources/} — directory of plain-text files, one per entry.
#' }
#'
#' When a \code{theme_set} is provided the codebook is exported hierarchically:
#' Theme (non-codable) > Subtheme (non-codable) > Code (codable leaf).
#' Without a theme set, codes are exported as a flat list.
#'
#' @param coding_state \code{ProgressiveCodingState} object containing
#'   \code{$codebook} and \code{$entry_results}.
#' @param data Tibble with at least \code{std_id} and \code{std_text} columns.
#' @param output_path File path for the \code{.qdpx} file to create.
#' @param theme_set Optional \code{ThemeSet} object.
#'   If provided, builds a hierarchical code tree (Theme > Subtheme > Code).
#' @param study_name Character string used as the project name inside the
#'   QDPX file.  Defaults to \code{"pakhom export"}.
#' @param methodology_mode Optional character (T1.7 / AC4): when supplied,
#'   the QDPX project's Description is stamped with the methodology mode
#'   alongside the Tier-0 verification stats. NULL preserves legacy
#'   behavior (no methodology stamp).
#' @return The \code{output_path} (invisibly), or stops with an error.
#' @export
export_qdpx <- function(coding_state, data, output_path,
                         theme_set = NULL,
                         study_name = "pakhom export",
                         methodology_mode = NULL) {

  # --- Input validation -------------------------------------------------------
  # For CRAN, xml2 moved to Suggests. QDPX export is the
  # only module that touches XML, so the guard lives here. The rest of
  # the pipeline (HTML report, CSV exports, etc.) doesn't need xml2.
  if (!requireNamespace("xml2", quietly = TRUE)) {
    stop("export_qdpx requires the 'xml2' package. Install with: ",
         "install.packages('xml2'). The rest of pakhom works without it; ",
         "QDPX export is only needed for QDA-software interoperability ",
         "(NVivo, ATLAS.ti, MAXQDA).", call. = FALSE)
  }
  if (!inherits(coding_state, "ProgressiveCodingState")) {
    stop("coding_state must be a ProgressiveCodingState object")
  }
  # T1.7 (AC4): attach methodology mode to coding_state so .build_qde_xml's
  # Description block can stamp the project. Using attr() rather than
  # threading through every helper is intentional -- methodology mode is a
  # cross-cutting concern that doesn't belong on the data argument.
  if (!is.null(methodology_mode)) {
    attr(coding_state, "methodology_mode") <- methodology_mode
  }
  if (!is.data.frame(data) || !all(c("std_id", "std_text") %in% names(data))) {
    stop("data must be a data frame with 'std_id' and 'std_text' columns")
  }
  if (!is.null(theme_set) && !inherits(theme_set, "ThemeSet")) {
    stop("theme_set must be a ThemeSet object or NULL")
  }
  if (!is.character(output_path) || length(output_path) != 1 || nchar(output_path) == 0) {
    stop("output_path must be a non-empty file path string")
  }

  # Ensure output path ends with .qdpx
  if (!grepl("\\.qdpx$", output_path, ignore.case = TRUE)) {
    output_path <- paste0(output_path, ".qdpx")
  }

  n_codes <- length(coding_state$codebook)
  n_entries <- nrow(data)
  log_info("QDPX export: {n_codes} codes, {n_entries} entries -> {output_path}")

  if (n_codes == 0) {
    log_warn("QDPX export: codebook is empty -- export will contain sources only")
  }

  # --- Create staging directory -----------------------------------------------
  staging_dir <- tempfile(pattern = "qdpx_")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging_dir, recursive = TRUE), add = TRUE)

  # --- Write source text files ------------------------------------------------
  sources_dir <- file.path(staging_dir, "sources")
  source_paths <- .write_source_files(data, sources_dir)
  log_info("QDPX export: wrote {length(source_paths)} source files")

  # --- Build and write project.qde --------------------------------------------
  qde_doc <- .build_qde_xml(
    coding_state = coding_state,
    data = data,
    source_paths = source_paths,
    theme_set = theme_set,
    study_name = study_name,
    # pass the .qdpx path so the description
    # can read fabrication_log.csv from the same directory.
    output_path = output_path
  )

  qde_path <- file.path(staging_dir, "project.qde")
  xml2::write_xml(qde_doc, qde_path)
  log_info("QDPX export: wrote project.qde")

  # --- Create ZIP archive -----------------------------------------------------
  output_path <- normalizePath(output_path, mustWork = FALSE)
  output_dir <- dirname(output_path)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Remove existing file if present (zip() appends by default on some systems)
  if (file.exists(output_path)) {
    file.remove(output_path)
  }

  # Gather files to zip (relative paths within staging_dir)
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(staging_dir)

  files_to_zip <- c(
    "project.qde",
    list.files("sources", full.names = TRUE)
  )

  zip_result <- tryCatch(
    utils::zip(zipfile = output_path, files = files_to_zip, flags = "-q"),
    error = function(e) {
      log_error("QDPX export: ZIP creation failed -- {conditionMessage(e)}")
      stop("Failed to create QDPX archive: ", conditionMessage(e), call. = FALSE)
    }
  )

  if (!file.exists(output_path)) {
    stop("QDPX export: ZIP file was not created at ", output_path, call. = FALSE)
  }

  file_size_kb <- round(file.info(output_path)$size / 1024, 1)
  log_info("QDPX export complete: {output_path} ({file_size_kb} KB)")

  invisible(output_path)
}
