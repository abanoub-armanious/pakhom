# ==============================================================================
# ThemeSet S3 Class — Canonical Theme Data Representation
# ==============================================================================
# Eliminates the dual-format (data.frame vs list) bug from the old script.
# Every AI response producing themes goes through normalize_theme_result()
# immediately after fromJSON(). All downstream code works exclusively with
# ThemeSet objects — no is.data.frame() checks needed anywhere else.
# ==============================================================================

#' Required fields for each theme within a ThemeSet
.THEME_REQUIRED_FIELDS <- c("id", "name", "description", "codes_included")

#' Default values for optional theme fields
.THEME_DEFAULTS <- list(
  prevalence = "medium",
  sentiment_tendency = "neutral",
  subthemes = character(0),
  subthemes_structured = NULL,
  supporting_quotes = character(0),
  keywords = character(0),
  narrative = "",
  entry_count = 0L
)

#' Create a ThemeSet object (canonical internal representation)
#'
#' @param themes List of theme lists, each with at minimum: id, name,
#'   description, codes_included
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

  # Ensure each theme has required fields and defaults

  themes <- lapply(seq_along(themes), function(i) {
    t <- themes[[i]]
    t$id <- as.integer(t$id %||% i)
    if (is.null(t$name) || is.na(t$name) || nchar(t$name) == 0) {
      stop(sprintf("Theme %d is missing a 'name' field", i))
    }

    if (is.null(t$description)) t$description <- ""
    if (is.null(t$codes_included)) t$codes_included <- character(0)

    # Flatten codes_included if nested
    t$codes_included <- as.character(unlist(t$codes_included))
    t$codes_included <- t$codes_included[!is.na(t$codes_included) & nchar(t$codes_included) > 0]

    # Apply defaults for optional fields (skip subthemes — handled above)
    for (field in names(.THEME_DEFAULTS)) {
      if (field %in% c("subthemes", "subthemes_structured")) next
      if (is.null(t[[field]])) {
        t[[field]] <- .THEME_DEFAULTS[[field]]
      }
    }
    # Only apply subtheme defaults if not already set by the branching above
    if (is.null(t$subthemes)) t$subthemes <- character(0)
    if (!"subthemes_structured" %in% names(t)) t$subthemes_structured <- NULL

    # Handle structured subthemes (objects with name + description)
    # jsonlite::fromJSON with simplifyVector=TRUE turns [{name:...,description:...}]
    # into a data.frame, not a list-of-lists. Must handle both formats,
    # plus plain character vectors from simplified JSON arrays.
    if (is.character(t$subthemes) && length(t$subthemes) > 0) {
      # Plain character vector of subtheme names (e.g., from simplifyVector)
      t$subthemes_structured <- NULL
    } else if (is.data.frame(t$subthemes) && "name" %in% names(t$subthemes)) {
      # data.frame format from jsonlite (most common)
      t$subthemes_structured <- lapply(seq_len(nrow(t$subthemes)), function(r) {
        as.list(t$subthemes[r, , drop = FALSE])
      })
      t$subthemes <- as.character(t$subthemes$name)
    } else if (is.list(t$subthemes) && length(t$subthemes) > 0 &&
        !is.null(t$subthemes[[1]]) && is.list(t$subthemes[[1]]) &&
        !is.null(t$subthemes[[1]]$name)) {
      # list-of-lists format (from simplifyVector=FALSE or manual construction)
      t$subthemes_structured <- t$subthemes
      t$subthemes <- vapply(t$subthemes, function(s) {
        s$name %||% as.character(s)
      }, character(1))
    } else if (is.list(t$subthemes)) {
      t$subthemes <- as.character(unlist(t$subthemes))
      t$subthemes_structured <- NULL
    }

    # Flatten other list-type fields
    if (is.list(t$supporting_quotes)) t$supporting_quotes <- as.character(unlist(t$supporting_quotes))
    if (is.list(t$keywords)) t$keywords <- as.character(unlist(t$keywords))

    t
  })

  obj <- list(
    themes = themes,
    thematic_map = thematic_map %||% "",
    analysis_notes = analysis_notes %||% "",
    review_notes = review_notes,
    split_history = split_history %||% list()
  )
  class(obj) <- "ThemeSet"
  obj
}

#' Normalize raw AI theme output to canonical ThemeSet
#'
#' Call this immediately after fromJSON() on any AI response that produces
#' themes. Handles both data.frame and list formats transparently.
#'
#' @param raw_result Parsed JSON from AI (may be df or list)
#' @return ThemeSet S3 object (always list-based internally)
#' @export
normalize_theme_result <- function(raw_result) {
  if (inherits(raw_result, "ThemeSet")) return(raw_result)

  # Extract the themes array from various wrapper formats

  themes_raw <- NULL
  thematic_map <- ""
  analysis_notes <- ""

  if (is.list(raw_result) && !is.data.frame(raw_result)) {
    themes_raw <- raw_result$themes %||% raw_result
    thematic_map <- raw_result$thematic_map %||% ""
    analysis_notes <- raw_result$analysis_notes %||% ""
  } else if (is.data.frame(raw_result)) {
    themes_raw <- raw_result
  }

  if (is.null(themes_raw)) {
    stop("Cannot normalize theme result: no themes found in input")
  }

  # Convert data.frame rows to list-of-lists
  if (is.data.frame(themes_raw)) {
    themes_list <- lapply(seq_len(nrow(themes_raw)), function(i) {
      row <- as.list(themes_raw[i, , drop = FALSE])
      # Unbox single-element list columns
      for (nm in names(row)) {
        if (is.list(row[[nm]]) && length(row[[nm]]) == 1) {
          row[[nm]] <- row[[nm]][[1]]
        }
      }
      row
    })
  } else if (is.list(themes_raw)) {
    themes_list <- themes_raw
    # Handle case where it's a single theme not wrapped in a list
    if (!is.null(themes_raw$name)) {
      themes_list <- list(themes_raw)
    }
  } else {
    stop("Cannot normalize theme result: unexpected type ", class(themes_raw))
  }

  create_theme_set(
    themes = themes_list,
    thematic_map = thematic_map,
    analysis_notes = analysis_notes
  )
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
    n_codes = vapply(theme_set$themes, function(t) length(t$codes_included), integer(1)),
    codes_included = vapply(theme_set$themes, function(t) {
      paste(t$codes_included, collapse = "; ")
    }, character(1)),
    subthemes = vapply(theme_set$themes, function(t) {
      paste(t$subthemes, collapse = "; ")
    }, character(1)),
    subtheme_descriptions = vapply(theme_set$themes, function(t) {
      if (!is.null(t$subthemes_structured)) {
        paste(vapply(t$subthemes_structured, function(s) {
          paste0(s$name %||% "", ": ", s$description %||% "")
        }, character(1)), collapse = "; ")
      } else {
        ""
      }
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

#' Print method for ThemeSet
#' @param x ThemeSet object
#' @param ... Additional arguments (ignored)
#' @export
print.ThemeSet <- function(x, ...) {
  cat(sprintf("ThemeSet with %d themes:\n", n_themes(x)))
  for (t in x$themes) {
    cat(sprintf("  [%d] %s (%s prevalence, %s sentiment, %d codes)\n",
                t$id, t$name, t$prevalence, t$sentiment_tendency,
                length(t$codes_included)))
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

    # Re-number IDs sequentially
    for (i in seq_along(theme_set$themes)) {
      theme_set$themes[[i]]$id <- i
    }
  }

  theme_set
}

#' Rebuild code-to-theme mapping after researcher restructuring
#'
#' After the researcher modifies the theme structure (reassigning codes,
#' creating/splitting themes), the merge_history$code_to_theme_map becomes
#' stale. This function rebuilds it from the current ThemeSet by walking
#' all themes and resolving code names back to code keys via the codebook.
#'
#' @param theme_set ThemeSet with modified themes
#' @param coding_state ProgressiveCodingState for code name → key resolution
#' @return ThemeSet with updated merge_history$code_to_theme_map
#' @keywords internal
rebuild_code_to_theme_map <- function(theme_set, coding_state) {
  validate_class(theme_set, "ThemeSet")

  # Build reverse lookup: code_name -> code_key
  name_to_key <- list()
  for (key in names(coding_state$codebook)) {
    cn <- coding_state$codebook[[key]]$code_name
    name_to_key[[cn]] <- key
    # Also map by key itself (codes_included may use either)
    name_to_key[[key]] <- key
  }

  code_to_theme <- list()
  code_to_subtheme <- list()

  for (theme in theme_set$themes) {
    for (code_name in theme$codes_included) {
      key <- name_to_key[[code_name]]
      if (!is.null(key)) {
        code_to_theme[[key]] <- theme$name
      }
    }
    # Also handle subthemes_structured if present
    if (!is.null(theme$subthemes_structured)) {
      for (st in theme$subthemes_structured) {
        for (code_name in st$codes %||% character(0)) {
          key <- name_to_key[[code_name]]
          if (!is.null(key)) {
            code_to_subtheme[[key]] <- st$name
          }
        }
      }
    }
  }

  if (is.null(theme_set$merge_history)) {
    theme_set$merge_history <- list()
  }
  theme_set$merge_history$code_to_theme_map <- code_to_theme
  theme_set$merge_history$code_to_subtheme_map <- code_to_subtheme

  theme_set
}

