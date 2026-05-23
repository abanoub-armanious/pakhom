# ==============================================================================
# Theme Hierarchy S3 Classes — Themes -> Subthemes -> Codes -> Segments
# ==============================================================================
# Phase 51 rewrite. Themes own first-class Subthemes which own Code objects;
# each Code carries its own coded_segments inline so a saved themes.json is
# self-contained for reproducibility (no coding_state companion file required).
#
# Back-compat (used by existing callers and by older themes.json fixtures):
# - theme$codes_included        denormalised character vector of code NAMES
# - theme$subthemes_structured  alias for theme$subthemes
# Both are recomputed automatically whenever the canonical theme$subthemes
# changes via .recompute_theme_denorm().
# ==============================================================================

#' Required fields for each theme within a ThemeSet
.THEME_REQUIRED_FIELDS <- c("id", "name", "description")

#' Default values for optional theme fields
.THEME_DEFAULTS <- list(
  prevalence = "medium",
  sentiment_tendency = "neutral",
  supporting_quotes = character(0),
  # Phase 58 Tier 7 M-25/AF-34: parallel structured records carrying
  # text + entry_id + source_table + std_author + sentiment_score +
  # position label. Empty list() default; populated by enrich_themes
  # when representative quotes are selected.
  supporting_quote_records = list(),
  keywords = character(0),
  narrative = "",
  entry_count = 0L
)

# ==============================================================================
# Code S3 — atomic leaf in the theme hierarchy
# ==============================================================================

#' Create a Code S3 object
#'
#' Atomic leaf in the theme hierarchy. Carries name, description, type,
#' frequency, entry_ids, and coded_segments inline so a saved ThemeSet is
#' self-contained: a researcher with just themes.json can verify every quote
#' without needing the original coding_state.
#'
#' @param key Code key (codebook lookup key, e.g., "med_helps")
#' @param name Human-readable code name (e.g., "Medication helps binge control")
#' @param description Code description
#' @param type Code type (e.g., "descriptive", "framework_construct", "anomaly")
#' @param frequency How many entries are coded with this code
#' @param entry_ids Character vector of entry std_ids
#' @param coded_segments List of coded-segment records (each with entry_id,
#'   text, offsets, QuoteProvenance)
#' @return Code S3 object
#' @export
create_code_object <- function(key, name = NULL, description = "",
                                 type = "descriptive", frequency = 0L,
                                 entry_ids = character(0),
                                 coded_segments = list()) {
  obj <- list(
    key            = as.character(key),
    name           = as.character(name %||% key),
    description    = as.character(description %||% ""),
    type           = as.character(type %||% "descriptive"),
    frequency      = as.integer(frequency %||% 0L),
    entry_ids      = as.character(entry_ids %||% character(0)),
    coded_segments = if (is.null(coded_segments)) list() else coded_segments
  )
  class(obj) <- "Code"
  obj
}

#' Hydrate a Code S3 from a coding_state codebook entry
#' @param key Codebook key
#' @param coding_state ProgressiveCodingState
#' @return Code S3 (stub if key not found in codebook)
#' @keywords internal
.code_from_codebook <- function(key, coding_state) {
  cb <- coding_state$codebook[[key]]
  if (is.null(cb)) return(create_code_object(key = key))
  create_code_object(
    key            = key,
    name           = cb$code_name %||% key,
    description    = cb$description %||% "",
    type           = cb$type %||% "descriptive",
    frequency      = cb$frequency %||% 0L,
    entry_ids      = unique(cb$entry_ids %||% character(0)),
    coded_segments = cb$coded_segments %||% list()
  )
}

#' Print method for Code
#' @param x Code object
#' @param ... ignored
#' @export
print.Code <- function(x, ...) {
  cat(sprintf("<Code> %s (key=%s, type=%s, n=%d)\n",
              x$name, x$key, x$type, length(x$entry_ids)))
  invisible(x)
}

# ==============================================================================
# Subtheme S3 — first-class container for codes within a Theme
# ==============================================================================

#' Create a Subtheme S3 object
#'
#' First-class container that holds a set of Code objects within a Theme.
#' Use NA_character_ for name when codes are not yet sub-grouped (a "virtual"
#' subtheme that will be populated by Phase 52's clustering).
#'
#' @param name Subtheme name; NA_character_ for virtual/ungrouped
#' @param description Subtheme description
#' @param codes List of Code S3 objects (or character vector of code names —
#'   coerced to stub Codes for use in tests / non-coding-state contexts)
#' @param subthemes List of nested Subtheme S3 objects (or raw lists
#'   coerced via recursive create_subtheme call). Phase 58 Tier 1 C-12
#'   added nested subthemes to support depth-N HAC walker decomposition.
#'   Empty list = leaf Subtheme (no nested children).
#' @return Subtheme S3 object
#' @export
create_subtheme <- function(name = NA_character_, description = "",
                              codes = list(), subthemes = list()) {
  if (is.character(codes)) {
    codes <- lapply(codes, function(cn) create_code_object(key = cn, name = cn))
  } else if (is.list(codes)) {
    codes <- lapply(codes, function(c) {
      if (inherits(c, "Code")) return(c)
      if (is.list(c) && !is.null(c$key)) {
        return(create_code_object(
          key            = c$key,
          name           = c$name %||% c$key,
          description    = c$description %||% "",
          type           = c$type %||% "descriptive",
          frequency      = c$frequency %||% 0L,
          entry_ids      = c$entry_ids %||% character(0),
          coded_segments = c$coded_segments %||% list()
        ))
      }
      if (is.character(c) && length(c) == 1L) {
        return(create_code_object(key = c, name = c))
      }
      stop("Cannot coerce subtheme code: ", paste(class(c), collapse = "/"))
    })
  } else {
    stop("Subtheme codes must be a list or character vector")
  }

  # Phase 58 Tier 1 C-12: subthemes can now nest. Empty list when the
  # Subtheme is a leaf in the hierarchy (no further decomposition).
  # Coerce nested Subtheme records (raw list form from walker output)
  # into Subtheme S3 objects recursively.
  if (length(subthemes) > 0L) {
    if (!is.list(subthemes)) stop("Subtheme subthemes must be a list")
    subthemes <- lapply(subthemes, function(s) {
      if (inherits(s, "Subtheme")) return(s)
      if (is.list(s)) {
        return(create_subtheme(
          name        = s$name        %||% NA_character_,
          description = s$description %||% "",
          codes       = s$codes       %||% list(),
          subthemes   = s$subthemes   %||% list()
        ))
      }
      stop("Cannot coerce nested subtheme: ", paste(class(s), collapse = "/"))
    })
  }

  obj <- list(
    name        = if (is.na(name)) NA_character_ else as.character(name),
    description = as.character(description %||% ""),
    codes       = codes,
    subthemes   = subthemes
  )
  class(obj) <- "Subtheme"
  obj
}

#' Number of DIRECT codes in a Subtheme (excludes nested sub-subthemes)
#'
#' Phase 58 Tier 1 audit LOW-3/6 documentation: returns the count of
#' codes attached DIRECTLY to this Subtheme. Codes in nested
#' sub-subthemes are NOT counted. Use \code{subtheme_n_codes_total()}
#' for the depth-recursive count.
#'
#' @param subtheme Subtheme S3
#' @return Integer; direct-code count (depth-0).
#' @export
subtheme_n_codes <- function(subtheme) {
  validate_class(subtheme, "Subtheme")
  length(subtheme$codes)
}

#' Number of codes in a Subtheme INCLUDING nested sub-subthemes
#'
#' Phase 58 Tier 1 audit LOW-3 addition: depth-recursive code count.
#' Walks the Subtheme tree and sums direct-code counts at every depth.
#' Use \code{subtheme_n_codes()} for the depth-0 (direct only) count.
#'
#' @param subtheme Subtheme S3
#' @return Integer; total code count across every nested depth.
#' @export
subtheme_n_codes_total <- function(subtheme) {
  validate_class(subtheme, "Subtheme")
  length(.subtheme_codes_recursive(subtheme))
}

#' Code names (display) within a Subtheme
#' @param subtheme Subtheme S3
#' @return Character vector
#' @export
subtheme_code_names <- function(subtheme) {
  validate_class(subtheme, "Subtheme")
  if (length(subtheme$codes) == 0L) return(character(0))
  vapply(subtheme$codes, function(c) c$name, character(1))
}

#' Code keys within a Subtheme
#' @param subtheme Subtheme S3
#' @return Character vector
#' @export
subtheme_code_keys <- function(subtheme) {
  validate_class(subtheme, "Subtheme")
  if (length(subtheme$codes) == 0L) return(character(0))
  vapply(subtheme$codes, function(c) c$key, character(1))
}

#' Number of nested subthemes within a Subtheme
#'
#' Phase 58 Tier 1 C-12 introduced nested Subthemes so the HAC walker
#' can produce hierarchical decomposition (e.g. a 200-code subtheme
#' broken into sub-subthemes via depth-N recursion). Returns 0 for
#' leaf Subthemes.
#'
#' @param subtheme Subtheme S3
#' @return Integer; depth-1 nested subtheme count.
#' @export
subtheme_n_subthemes <- function(subtheme) {
  validate_class(subtheme, "Subtheme")
  length(subtheme$subthemes %||% list())
}

#' Print method for Subtheme
#' @param x Subtheme object
#' @param ... ignored
#' @export
print.Subtheme <- function(x, ...) {
  nm <- if (is.na(x$name)) "<virtual / ungrouped>" else x$name
  cat(sprintf("<Subtheme> %s (%d codes)\n", nm, length(x$codes)))
  invisible(x)
}

# ==============================================================================
# Theme-level getters (work on a single theme — a plain list with class fields)
# ==============================================================================

#' Collect Code S3 objects from a Subtheme AND all its nested sub-subthemes
#'
#' Phase 58 Tier 1 C-12 introduced nested Subthemes. This helper walks
#' the depth-N tree so callers that want a flat list of every Code under
#' a Subtheme don't have to recurse manually.
#'
#' @keywords internal
.subtheme_codes_recursive <- function(subtheme) {
  if (!inherits(subtheme, "Subtheme")) return(list())
  out <- subtheme$codes
  for (child in subtheme$subthemes %||% list()) {
    out <- c(out, .subtheme_codes_recursive(child))
  }
  out
}

#' Flatten Code S3 objects across all subthemes (and sub-subthemes) of a theme
#'
#' Phase 58 Tier 1 C-12: now recurses through nested Subthemes so codes
#' in sub-subthemes are included. Pre-Phase-58 ThemeSets without
#' nesting are unaffected (the recursion bottoms out at depth 1).
#'
#' @param theme A theme list (one element of theme_set$themes)
#' @return List of Code S3 objects
#' @export
theme_code_objects <- function(theme) {
  if (is.null(theme$subthemes) || length(theme$subthemes) == 0L) return(list())
  out <- list()
  for (s in theme$subthemes) {
    if (inherits(s, "Subtheme")) {
      out <- c(out, .subtheme_codes_recursive(s))
    }
  }
  out
}

#' Flatten code names across all subthemes of a theme (back-compat with codes_included)
#' @param theme A theme list
#' @return Character vector of code NAMES
#' @export
theme_codes <- function(theme) {
  objs <- theme_code_objects(theme)
  if (length(objs) == 0L) return(character(0))
  unname(vapply(objs, function(c) c$name, character(1)))
}

#' Flatten code keys across all subthemes of a theme
#' @param theme A theme list
#' @return Character vector of code KEYS
#' @export
theme_code_keys <- function(theme) {
  objs <- theme_code_objects(theme)
  if (length(objs) == 0L) return(character(0))
  unname(vapply(objs, function(c) c$key, character(1)))
}

#' Flatten coded_segments across all codes of a theme
#' @param theme A theme list
#' @return Flat list of segment records
#' @export
theme_segments <- function(theme) {
  objs <- theme_code_objects(theme)
  out <- list()
  for (c in objs) {
    if (length(c$coded_segments) > 0L) {
      out <- c(out, c$coded_segments)
    }
  }
  out
}

#' Number of TOP-LEVEL real subthemes in a theme (excludes virtual wrappers)
#'
#' Phase 58 Tier 1 AF-3: this counter is unchanged from Phase 51 -- it
#' counts only the immediate (depth-1) named subthemes of the theme.
#' Virtual (NA-named) subthemes are excluded; nested sub-subthemes are
#' NOT counted. For "all real subthemes at every depth" use
#' \code{theme_n_subthemes_total()}. For "raw structural count including
#' virtual wrappers" use \code{length(theme$subthemes)}.
#'
#' @param theme A theme list
#' @return Integer; depth-1 named subtheme count.
#' @export
theme_n_subthemes <- function(theme) {
  if (is.null(theme$subthemes) || length(theme$subthemes) == 0L) return(0L)
  # Count only named (non-virtual) top-level subthemes
  named <- vapply(theme$subthemes, function(s) {
    inherits(s, "Subtheme") && !is.na(s$name) && nchar(s$name) > 0L
  }, logical(1))
  sum(named)
}

#' Total real (named) subthemes across every depth of a theme
#'
#' Phase 58 Tier 1 AF-3: with C-12's recursive walker, subthemes can
#' nest. This getter counts every named subtheme regardless of depth so
#' downstream consumers can report the "true" decomposition size of a
#' theme.
#'
#' @param theme A theme list
#' @return Integer; named-subtheme count across all depths.
#' @export
theme_n_subthemes_total <- function(theme) {
  if (is.null(theme$subthemes) || length(theme$subthemes) == 0L) return(0L)
  count <- 0L
  walk <- function(s) {
    if (!inherits(s, "Subtheme")) return(invisible(NULL))
    if (!is.na(s$name) && nchar(s$name) > 0L) count <<- count + 1L
    for (child in s$subthemes %||% list()) walk(child)
  }
  for (s in theme$subthemes) walk(s)
  count
}

#' Recompute denormalised back-compat fields on a theme from its subthemes
#' @keywords internal
.recompute_theme_denorm <- function(theme) {
  theme$codes_included        <- theme_codes(theme)
  theme$subthemes_structured  <- theme$subthemes
  theme
}

# ==============================================================================
# ThemeSet S3 — top-level container
# ==============================================================================

#' Create a ThemeSet object (canonical internal representation)
#'
#' Accepts both the new hierarchy shape (themes with first-class Subtheme S3
#' objects) and the legacy flat shape (themes with codes_included character
#' vectors). Legacy input is wrapped into a single virtual Subtheme per theme.
#'
#' @param themes List of theme lists. Each theme requires id and name; codes
#'   and subthemes follow either the new (subthemes = list of Subtheme S3) or
#'   legacy (codes_included = character vector, subthemes = character vector)
#'   shape.
#' @param thematic_map Character description of inter-theme relationships
#' @param analysis_notes Character reflexive notes
#' @param review_notes List of review results (or NULL)
#' @param split_history List tracking any theme splits performed
#' @return ThemeSet S3 object
#' @export
create_theme_set <- function(themes, thematic_map = "",
                             analysis_notes = "", review_notes = NULL,
                             split_history = NULL) {
  stopifnot(is.list(themes))

  themes <- lapply(seq_along(themes), function(i) {
    t <- themes[[i]]
    t$id <- as.integer(t$id %||% i)
    if (is.null(t$name) || is.na(t$name) || nchar(t$name) == 0) {
      stop(sprintf("Theme %d is missing a 'name' field", i))
    }
    if (is.null(t$description)) t$description <- ""

    # Determine the canonical theme$subthemes (list of Subtheme S3)
    has_new_subthemes <- is.list(t$subthemes) && length(t$subthemes) > 0L &&
                          all(vapply(t$subthemes, inherits, logical(1), "Subtheme"))

    if (!has_new_subthemes) {
      legacy_codes <- t$codes_included
      if (is.list(legacy_codes)) legacy_codes <- as.character(unlist(legacy_codes))
      if (is.null(legacy_codes)) legacy_codes <- character(0)
      legacy_codes <- legacy_codes[!is.na(legacy_codes) & nchar(legacy_codes) > 0L]

      # Where did the caller record subthemes?
      legacy_sts <- t$subthemes_structured %||% t$subthemes

      st_objs <- .legacy_subthemes_to_objects(legacy_sts, legacy_codes)
      if (length(st_objs) == 0L) {
        # No usable subtheme info — wrap all codes in one virtual Subtheme
        st_objs <- list(create_subtheme(
          name = NA_character_, description = "", codes = legacy_codes
        ))
      }
      t$subthemes <- st_objs
    }

    # Apply field defaults (skip subthemes/codes — handled above; subthemes_structured + codes_included are denormalised below)
    for (field in names(.THEME_DEFAULTS)) {
      if (is.null(t[[field]])) t[[field]] <- .THEME_DEFAULTS[[field]]
    }

    # Recompute back-compat denormalised fields
    t <- .recompute_theme_denorm(t)

    # Flatten other list-type fields that came from jsonlite
    if (is.list(t$supporting_quotes)) t$supporting_quotes <- as.character(unlist(t$supporting_quotes))
    if (is.list(t$keywords))          t$keywords          <- as.character(unlist(t$keywords))

    t
  })

  obj <- list(
    themes         = themes,
    thematic_map   = thematic_map %||% "",
    analysis_notes = analysis_notes %||% "",
    review_notes   = review_notes,
    split_history  = split_history %||% list()
  )
  class(obj) <- "ThemeSet"
  obj
}

#' Convert legacy subtheme representations into a list of Subtheme S3 objects
#'
#' Handles the formats jsonlite emits:
#' - data.frame with name + description columns (simplifyVector = TRUE)
#' - list-of-lists with $name + $description
#' - plain character vector of subtheme names (no code mapping known)
#' - existing list of Subtheme S3 (passes through)
#'
#' If no per-subtheme code mapping is present, returns an empty list and the
#' caller wraps all codes in a single virtual Subtheme.
#' @keywords internal
.legacy_subthemes_to_objects <- function(legacy_sts, all_code_names) {
  if (is.null(legacy_sts)) return(list())
  if (is.character(legacy_sts) && length(legacy_sts) == 0L) return(list())

  # Already Subtheme S3 list — pass through
  if (is.list(legacy_sts) && length(legacy_sts) > 0L &&
      all(vapply(legacy_sts, inherits, logical(1), "Subtheme"))) {
    return(legacy_sts)
  }

  # data.frame from jsonlite simplifyVector = TRUE
  if (is.data.frame(legacy_sts) && "name" %in% names(legacy_sts)) {
    return(lapply(seq_len(nrow(legacy_sts)), function(r) {
      .one_legacy_subtheme(as.list(legacy_sts[r, , drop = FALSE]), all_code_names)
    }))
  }

  # list-of-lists with $name
  if (is.list(legacy_sts) && length(legacy_sts) > 0L &&
      is.list(legacy_sts[[1]]) && !is.null(legacy_sts[[1]]$name)) {
    return(lapply(legacy_sts, function(s) .one_legacy_subtheme(s, all_code_names)))
  }

  # Plain character vector — no code mapping known; caller will fall back to virtual
  list()
}

#' Build one Subtheme S3 from a legacy list/row representation
#' @keywords internal
.one_legacy_subtheme <- function(s, all_code_names) {
  raw_codes <- s$codes %||% s$code_keys %||% s$code_names
  if (is.list(raw_codes)) raw_codes <- as.character(unlist(raw_codes))
  if (is.null(raw_codes)) raw_codes <- character(0)
  raw_codes <- raw_codes[!is.na(raw_codes) & nchar(raw_codes) > 0L]

  use_codes <- if (length(raw_codes) > 0L) raw_codes else all_code_names

  create_subtheme(
    name        = s$name %||% NA_character_,
    description = s$description %||% "",
    codes       = use_codes
  )
}

#' Normalize raw AI theme output to canonical ThemeSet
#'
#' Call this immediately after fromJSON() on any AI response that produces
#' themes. Handles both data.frame and list formats transparently and the
#' legacy (flat codes_included) wire format.
#'
#' @param raw_result Parsed JSON from AI (may be df or list)
#' @return ThemeSet S3 object
#' @export
normalize_theme_result <- function(raw_result) {
  if (inherits(raw_result, "ThemeSet")) return(raw_result)

  themes_raw     <- NULL
  thematic_map   <- ""
  analysis_notes <- ""

  if (is.list(raw_result) && !is.data.frame(raw_result)) {
    themes_raw     <- raw_result$themes %||% raw_result
    thematic_map   <- raw_result$thematic_map %||% ""
    analysis_notes <- raw_result$analysis_notes %||% ""
  } else if (is.data.frame(raw_result)) {
    themes_raw <- raw_result
  }

  if (is.null(themes_raw)) {
    stop("Cannot normalize theme result: no themes found in input")
  }

  if (is.data.frame(themes_raw)) {
    themes_list <- lapply(seq_len(nrow(themes_raw)), function(i) {
      row <- as.list(themes_raw[i, , drop = FALSE])
      for (nm in names(row)) {
        if (is.list(row[[nm]]) && length(row[[nm]]) == 1L) {
          row[[nm]] <- row[[nm]][[1]]
        }
      }
      row
    })
  } else if (is.list(themes_raw)) {
    themes_list <- themes_raw
    if (!is.null(themes_raw$name)) themes_list <- list(themes_raw)
  } else {
    stop("Cannot normalize theme result: unexpected type ", class(themes_raw))
  }

  create_theme_set(themes = themes_list, thematic_map = thematic_map,
                    analysis_notes = analysis_notes)
}

#' Extract theme names from ThemeSet
#' @param theme_set ThemeSet object
#' @return Character vector of theme names
#' @export
theme_names <- function(theme_set) {
  validate_class(theme_set, "ThemeSet")
  vapply(theme_set$themes, function(t) t$name, character(1))
}

#' Get the number of themes
#' @param theme_set ThemeSet object
#' @return Integer
#' @export
n_themes <- function(theme_set) {
  validate_class(theme_set, "ThemeSet")
  length(theme_set$themes)
}

#' Convert ThemeSet to tibble for export/inspection
#'
#' Flattens to a per-theme tibble (one row per theme). Subtheme structure is
#' summarized via subtheme name + description columns; per-subtheme detail is
#' available through the hierarchy (subtheme_name resolves to first
#' subtheme$name, etc.). Per-subtheme detail tables are produced separately
#' by the report renderer.
#'
#' @param theme_set ThemeSet object
#' @return tibble with one row per theme
#' @export
theme_set_to_tibble <- function(theme_set) {
  validate_class(theme_set, "ThemeSet")

  tibble(
    id = vapply(theme_set$themes, function(t) t$id, integer(1)),
    name = vapply(theme_set$themes, function(t) t$name, character(1)),
    description = vapply(theme_set$themes, function(t) t$description, character(1)),
    prevalence = vapply(theme_set$themes, function(t) t$prevalence, character(1)),
    sentiment_tendency = vapply(theme_set$themes, function(t) t$sentiment_tendency, character(1)),
    entry_count = vapply(theme_set$themes, function(t) as.integer(t$entry_count), integer(1)),
    n_codes = vapply(theme_set$themes, function(t) length(theme_codes(t)), integer(1)),
    # Phase 58 Tier 1 audit MEDIUM-1 followup: CSV form now exposes
    # BOTH counters so consumers reading the tibble see the full
    # decomposition shape, not just the depth-1 count. JSON form is
    # canonical (see R/17_report.R) but the CSV is the most common
    # downstream format and shouldn't underrepresent the nesting.
    n_subthemes = vapply(theme_set$themes, function(t) theme_n_subthemes(t), integer(1)),
    n_subthemes_total = vapply(theme_set$themes, function(t) theme_n_subthemes_total(t), integer(1)),
    codes_included = vapply(theme_set$themes, function(t) {
      paste(theme_codes(t), collapse = "; ")
    }, character(1)),
    subthemes = vapply(theme_set$themes, function(t) {
      paste(.subtheme_names_no_virtual(t), collapse = "; ")
    }, character(1)),
    subtheme_descriptions = vapply(theme_set$themes, function(t) {
      .subtheme_name_desc_pairs(t)
    }, character(1)),
    keywords = vapply(theme_set$themes, function(t) {
      paste(t$keywords, collapse = "; ")
    }, character(1)),
    narrative = vapply(theme_set$themes, function(t) {
      t$narrative %||% ""
    }, character(1)),
    supporting_quotes = vapply(theme_set$themes, function(t) {
      paste(t$supporting_quotes, collapse = " | ")
    }, character(1))
  )
}

#' Subtheme names omitting virtual (NA-named) wrappers
#' @keywords internal
.subtheme_names_no_virtual <- function(theme) {
  if (is.null(theme$subthemes) || length(theme$subthemes) == 0L) return(character(0))
  nms <- vapply(theme$subthemes, function(s) {
    if (inherits(s, "Subtheme")) s$name %||% NA_character_ else NA_character_
  }, character(1))
  nms[!is.na(nms) & nchar(nms) > 0L]
}

#' Subtheme name+description pairs for tibble serialization
#' @keywords internal
.subtheme_name_desc_pairs <- function(theme) {
  if (is.null(theme$subthemes) || length(theme$subthemes) == 0L) return("")
  pairs <- character(0)
  for (s in theme$subthemes) {
    if (!inherits(s, "Subtheme")) next
    if (is.na(s$name) || nchar(s$name %||% "") == 0L) next
    pairs <- c(pairs, paste0(s$name, ": ", s$description %||% ""))
  }
  paste(pairs, collapse = "; ")
}

#' Print method for ThemeSet
#' @param x ThemeSet object
#' @param ... Additional arguments (ignored)
#' @export
print.ThemeSet <- function(x, ...) {
  cat(sprintf("ThemeSet with %d themes:\n", n_themes(x)))
  for (t in x$themes) {
    n_sub <- theme_n_subthemes(t)
    n_codes <- length(theme_codes(t))
    sub_str <- if (n_sub > 0L) sprintf("%d subthemes, ", n_sub) else ""
    cat(sprintf("  [%d] %s (%s prevalence, %s sentiment, %s%d codes)\n",
                t$id, t$name, t$prevalence, t$sentiment_tendency,
                sub_str, n_codes))
  }
  if (nchar(x$thematic_map) > 0) {
    cat(sprintf("\nThematic map: %s\n", substr(x$thematic_map, 1, 200)))
  }
  invisible(x)
}

#' Remove themes with zero assigned entries after enrichment
#'
#' Call after enrich_themes() to drop themes that no data was mapped to.
#' Re-numbers theme IDs sequentially after pruning.
#'
#' @param theme_set ThemeSet object (enriched, with entry_count populated)
#' @return ThemeSet with empty themes removed
#' @export
prune_empty_themes <- function(theme_set) {
  validate_class(theme_set, "ThemeSet")

  keep_idx <- vapply(theme_set$themes, function(t) {
    (t$entry_count %||% 0L) > 0L
  }, logical(1))

  n_removed <- sum(!keep_idx)
  if (n_removed > 0) {
    removed_names <- vapply(theme_set$themes[!keep_idx], function(t) t$name, character(1))
    log_info("Pruning {n_removed} empty theme(s): {paste(removed_names, collapse = ', ')}")
    theme_set$themes <- theme_set$themes[keep_idx]
    for (i in seq_along(theme_set$themes)) theme_set$themes[[i]]$id <- i
  }

  theme_set
}

#' Rebuild code-to-theme mapping after researcher restructuring
#'
#' After the researcher modifies the theme structure (reassigning codes,
#' creating/splitting themes), the merge_history$code_to_theme_map becomes
#' stale. This function rebuilds it by walking the canonical hierarchy
#' (theme$subthemes -> Subtheme$codes -> Code$key) and resolving code names
#' back to code keys via the codebook for back-compat with legacy callers
#' that recorded code names rather than keys.
#'
#' @param theme_set ThemeSet with modified themes
#' @param coding_state ProgressiveCodingState for code name -> key resolution
#' @return ThemeSet with updated merge_history$code_to_theme_map
#' @keywords internal
rebuild_code_to_theme_map <- function(theme_set, coding_state) {
  validate_class(theme_set, "ThemeSet")

  # Reverse lookup: code_name -> code_key
  name_to_key <- list()
  for (key in names(coding_state$codebook)) {
    cn <- coding_state$codebook[[key]]$code_name
    name_to_key[[cn]]   <- key
    name_to_key[[key]]  <- key
  }

  resolve_key <- function(code_or_key) {
    if (is.null(code_or_key)) return(NULL)
    if (!is.null(name_to_key[[code_or_key]])) return(name_to_key[[code_or_key]])
    NULL
  }

  code_to_theme    <- list()
  code_to_subtheme <- list()

  # Phase 58 Tier 1 C-12: subthemes can now nest. The cascade attributes
  # each code to its TOP-LEVEL subtheme name (the immediate child of
  # the theme) regardless of how deep the code lives. This preserves
  # the legacy code_to_subtheme_map contract -- downstream consumers
  # see a flat code -> top-level-subtheme map; sub-subtheme detail
  # lives in themes.json's structured field.
  walk_subtheme <- function(s, theme_name, top_level_subtheme_name) {
    if (!inherits(s, "Subtheme")) return(invisible(NULL))
    for (code in s$codes) {
      k <- resolve_key(code$key) %||% resolve_key(code$name)
      if (is.null(k)) next
      code_to_theme[[k]] <<- theme_name
      if (!is.null(top_level_subtheme_name)) {
        code_to_subtheme[[k]] <<- top_level_subtheme_name
      }
    }
    for (child in s$subthemes %||% list()) {
      walk_subtheme(child, theme_name, top_level_subtheme_name)
    }
  }

  for (theme in theme_set$themes) {
    if (is.null(theme$subthemes) || length(theme$subthemes) == 0L) next
    for (s in theme$subthemes) {
      if (!inherits(s, "Subtheme")) next
      top_name <- if (!is.na(s$name) && nchar(s$name %||% "") > 0L) {
        s$name
      } else NULL
      walk_subtheme(s, theme$name, top_name)
    }
  }

  if (is.null(theme_set$merge_history)) theme_set$merge_history <- list()
  theme_set$merge_history$code_to_theme_map    <- code_to_theme
  theme_set$merge_history$code_to_subtheme_map <- code_to_subtheme

  theme_set
}
