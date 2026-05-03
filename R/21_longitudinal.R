# ==============================================================================
# Within-Run Longitudinal / Temporal Analysis
# ==============================================================================
# Analyses how themes emerge and shift in prevalence across time within a
# single analysis run.  Requires the standardised `std_timestamp` column
# produced by standardize_data().  When timestamps are absent or
# unparseable the module returns a stub result with has_temporal_data = FALSE.
# ==============================================================================

# -- Colour palette (matches report style) -------------------------------------
.TEMPORAL_PALETTE <- c(

  "#3498DB", "#9B59B6", "#E74C3C", "#27AE60",
  "#F39C12", "#1ABC9C", "#E67E22", "#34495E",
  "#16A085", "#8E44AD", "#2980B9", "#C0392B",
  "#D35400", "#7D3C98", "#2ECC71", "#F1C40F"
)

# -- Common date formats to attempt -------------------------------------------
.DATE_FORMATS <- c(

  "%Y-%m-%dT%H:%M:%S",        # ISO 8601 (no tz)

  "%Y-%m-%d %H:%M:%S",        # common datetime

  "%Y-%m-%d",                  # date only

  "%m/%d/%Y %H:%M:%S",        # US datetime

  "%m/%d/%Y",                  # US date
  "%d/%m/%Y %H:%M:%S",        # EU datetime
  "%d/%m/%Y",                  # EU date
  "%Y/%m/%d",                  # alt date
  "%b %d, %Y",                # abbreviated month
  "%B %d, %Y"                 # full month name
)

# ==============================================================================
# Internal: parse timestamps robustly
# ==============================================================================

#' Attempt to parse a character vector of timestamps
#'
#' Tries multiple common datetime formats via \code{as.POSIXct()} and returns
#' the first format that successfully parses the majority of non-NA values.
#' Falls back to \code{NA} for entries that cannot be parsed.
#'
#' @param x Character vector of timestamp strings
#' @return POSIXct vector (NA where parsing failed)
#' @keywords internal
.parse_timestamps <- function(x) {
  x <- as.character(x)
  x[x == "" | x == "NA"] <- NA_character_
  non_na <- !is.na(x)

  if (sum(non_na) == 0L) return(rep(as.POSIXct(NA), length(x)))

  # Strip trailing "Z" (UTC marker) and fractional seconds for cleaner parsing

  cleaned <- x
  cleaned[non_na] <- sub("\\.[0-9]+", "", cleaned[non_na])
  cleaned[non_na] <- sub("Z$", "", cleaned[non_na])
  # Strip timezone offsets like +00:00 or -05:00

  cleaned[non_na] <- sub("[+-][0-9]{2}:[0-9]{2}$", "", cleaned[non_na])

  best_parsed <- NULL
  best_n      <- 0L

  for (fmt in .DATE_FORMATS) {
    parsed <- suppressWarnings(as.POSIXct(cleaned, format = fmt, tz = "UTC"))
    n_ok   <- sum(!is.na(parsed[non_na]))
    if (n_ok > best_n) {
      best_n      <- n_ok
      best_parsed <- parsed
      if (n_ok == sum(non_na)) break
    }
  }

  if (is.null(best_parsed) || best_n == 0L) {
    return(rep(as.POSIXct(NA), length(x)))
  }

  best_parsed
}

# ==============================================================================
# Internal: detect appropriate time period granularity
# ==============================================================================

#' Determine period type from a vector of parsed timestamps
#'
#' Calculates the date span and selects granularity:
#' \itemize{
#'   \item < 30 days: \code{"daily"}
#'   \item 30 days -- 6 months: \code{"weekly"}
#'   \item 6 months -- 2 years: \code{"monthly"}
#'   \item > 2 years: \code{"quarterly"}
#' }
#'
#' @param timestamps POSIXct vector (NAs removed internally)
#' @return Character string: one of \code{"daily"}, \code{"weekly"},
#'   \code{"monthly"}, \code{"quarterly"}
#' @keywords internal
.detect_time_periods <- function(timestamps) {
  ts <- timestamps[!is.na(timestamps)]
  if (length(ts) < 2L) return("daily")

  span_days <- as.numeric(difftime(max(ts), min(ts), units = "days"))

  if (span_days < 30) {
    "daily"
  } else if (span_days < 180) {
    "weekly"
  } else if (span_days < 730) {
    "monthly"
  } else {
    "quarterly"
  }
}

# ==============================================================================
# Internal: assign each row to a period label
# ==============================================================================

#' Convert a POSIXct vector into period labels
#'
#' @param timestamps POSIXct vector
#' @param period_type One of \code{"daily"}, \code{"weekly"}, \code{"monthly"},
#'   \code{"quarterly"}
#' @return Character vector of period labels (same length as input)
#' @keywords internal
.assign_period_labels <- function(timestamps, period_type) {
  dates <- as.Date(timestamps)

  switch(period_type,
    daily     = format(dates, "%Y-%m-%d"),
    weekly    = {
      # ISO week: Monday-based
      wk  <- as.integer(format(dates, "%V"))
      yr  <- as.integer(format(dates, "%G"))
      sprintf("%04d-W%02d", yr, wk)
    },
    monthly   = format(dates, "%Y-%m"),
    quarterly = {
      yr <- as.integer(format(dates, "%Y"))
      qt <- (as.integer(format(dates, "%m")) - 1L) %/% 3L + 1L
      sprintf("%04d-Q%d", yr, qt)
    },
    format(dates, "%Y-%m-%d")
  )
}

# ==============================================================================
# Internal: compute theme prevalence per period
# ==============================================================================

#' Compute theme prevalence for each time period
#'
#' Groups entries by period, then counts how many entries belong to each theme
#' within that period.
#'
#' @param data Tibble with \code{std_timestamp}, \code{emerged_themes} (and/or
#'   \code{theme_membership_*} columns), and a \code{.period} column already
#'   attached.
#' @param theme_set ThemeSet object
#' @param period_type Character period type (for column reference only)
#' @return Tibble with columns: \code{period}, \code{theme_name},
#'   \code{n_entries}, \code{pct_of_period}, \code{total_in_period}
#' @keywords internal
.compute_theme_prevalence <- function(data, theme_set, period_type) {
  valid_themes <- theme_names(theme_set)

  # Detect available membership columns
  membership_cols <- paste0("theme_membership_", make.names(valid_themes))
  has_membership  <- any(membership_cols %in% names(data))

  periods <- sort(unique(data$.period))
  results <- vector("list", length(periods) * length(valid_themes))
  idx     <- 0L

  for (p in periods) {
    rows_in_period <- data[data$.period == p, , drop = FALSE]
    total_in_period <- nrow(rows_in_period)

    for (tn in valid_themes) {
      idx <- idx + 1L

      if (has_membership) {
        col_name <- paste0("theme_membership_", make.names(tn))
        if (col_name %in% names(rows_in_period)) {
          n_entries <- sum(rows_in_period[[col_name]] == 1L, na.rm = TRUE)
        } else {
          n_entries <- 0L
        }
      } else if ("emerged_themes" %in% names(rows_in_period)) {
        # Fallback: parse semicolon-delimited emerged_themes
        n_entries <- sum(
          grepl(tn, rows_in_period$emerged_themes, fixed = TRUE),
          na.rm = TRUE
        )
      } else {
        n_entries <- 0L
      }

      pct <- if (total_in_period > 0) n_entries / total_in_period * 100 else 0

      results[[idx]] <- tibble(
        period          = p,
        theme_name      = tn,
        n_entries       = as.integer(n_entries),
        pct_of_period   = round(pct, 2),
        total_in_period = as.integer(total_in_period)
      )
    }
  }

  bind_rows(results[seq_len(idx)])
}

# ==============================================================================
# Internal: compute theme emergence timeline
# ==============================================================================

#' Compute when each theme first appeared in the data
#'
#' For each theme, finds the earliest \code{std_timestamp} among entries that
#' belong to that theme.  When \code{coding_state} is supplied, also records
#' the timestamp of the first constituent code creation.
#'
#' @param data Tibble with \code{.parsed_ts} and theme assignment columns
#' @param theme_set ThemeSet object
#' @param coding_state ProgressiveCodingState (or NULL)
#' @return Tibble with columns: \code{theme_name}, \code{first_appearance_date},
#'   \code{first_code_date}, \code{n_codes_at_emergence}
#' @keywords internal
.compute_theme_emergence <- function(data, theme_set, coding_state) {
  valid_themes <- theme_names(theme_set)
  membership_cols <- paste0("theme_membership_", make.names(valid_themes))
  has_membership  <- any(membership_cols %in% names(data))

  results <- vector("list", length(valid_themes))

  for (i in seq_along(valid_themes)) {
    tn <- valid_themes[i]

    # Find rows belonging to this theme
    if (has_membership) {
      col_name <- paste0("theme_membership_", make.names(tn))
      if (col_name %in% names(data)) {
        mask <- !is.na(data[[col_name]]) & data[[col_name]] == 1L
      } else {
        mask <- rep(FALSE, nrow(data))
      }
    } else if ("emerged_themes" %in% names(data)) {
      mask <- grepl(tn, data$emerged_themes, fixed = TRUE) & !is.na(data$emerged_themes)
    } else {
      mask <- rep(FALSE, nrow(data))
    }

    theme_rows <- data[mask, , drop = FALSE]

    first_appearance <- if (nrow(theme_rows) > 0 && any(!is.na(theme_rows$.parsed_ts))) {
      min(theme_rows$.parsed_ts, na.rm = TRUE)
    } else {
      NA
    }

    # Track first code creation date from coding_state
    first_code_date    <- NA
    n_codes_at_emergence <- 0L

    if (!is.null(coding_state) && inherits(coding_state, "ProgressiveCodingState")) {
      # Get codes belonging to this theme from the ThemeSet
      theme_obj  <- theme_set$themes[[i]]
      theme_codes <- tolower(theme_obj$codes_included)

      if (length(theme_codes) > 0 && !is.null(coding_state$saturation$code_birth_log)) {
        birth_log <- coding_state$saturation$code_birth_log
        # Find entries where these codes were first created
        code_births <- vapply(theme_codes, function(code_key) {
          entry_idx <- birth_log[[code_key]]
          if (is.null(entry_idx)) return(NA_integer_)
          as.integer(entry_idx)
        }, integer(1))

        code_births <- code_births[!is.na(code_births)]

        if (length(code_births) > 0) {
          earliest_entry_idx <- min(code_births)
          n_codes_at_emergence <- sum(code_births == earliest_entry_idx)

          # Map entry index back to timestamp
          if (earliest_entry_idx <= nrow(data) && !is.na(data$.parsed_ts[earliest_entry_idx])) {
            first_code_date <- data$.parsed_ts[earliest_entry_idx]
          }
        }
      }
    }

    results[[i]] <- tibble(
      theme_name           = tn,
      first_appearance_date = as.character(
        if (!is.na(first_appearance)) as.Date(first_appearance) else NA
      ),
      first_code_date      = as.character(
        if (!is.na(first_code_date)) as.Date(first_code_date) else NA
      ),
      n_codes_at_emergence = n_codes_at_emergence
    )
  }

  bind_rows(results)
}

# ==============================================================================
# Main exported function
# ==============================================================================

#' Analyse temporal patterns in theme prevalence within a single run
#'
#' Requires the data to contain a \code{std_timestamp} column (character,
#' parseable as dates).  Detects the appropriate time granularity, computes
#' theme prevalence per period, and builds an emergence timeline showing when
#' each theme first appeared in the dataset.
#'
#' @param data Tibble with at least \code{std_timestamp} and theme assignment
#'   columns (\code{emerged_themes} and/or \code{theme_membership_*}).
#' @param theme_set ThemeSet object
#' @param coding_state ProgressiveCodingState (or NULL)
#' @return A list with elements:
#'   \describe{
#'     \item{prevalence_over_time}{Tibble: period, theme_name, n_entries,
#'       pct_of_period, total_in_period}
#'     \item{emergence_timeline}{Tibble: theme_name, first_appearance_date,
#'       first_code_date, n_codes_at_emergence}
#'     \item{period_type}{Character: "daily", "weekly", "monthly", or
#'       "quarterly"}
#'     \item{has_temporal_data}{Logical: TRUE when usable timestamps exist}
#'   }
#' @export
analyze_temporal_patterns <- function(data, theme_set, coding_state = NULL) {
  validate_class(theme_set, "ThemeSet", "analyze_temporal_patterns")

  # -- Guard: no timestamp column at all ------------------------------------
  if (!"std_timestamp" %in% names(data)) {
    log_warn("No std_timestamp column found -- skipping temporal analysis")
    return(.empty_temporal_result())
  }

  # -- Parse timestamps -----------------------------------------------------
  parsed <- .parse_timestamps(data$std_timestamp)
  n_ok   <- sum(!is.na(parsed))

  if (n_ok == 0L) {
    log_warn("All timestamps are NA or unparseable -- skipping temporal analysis")
    return(.empty_temporal_result())
  }

  pct_parsed <- round(n_ok / nrow(data) * 100, 1)
  log_info("Parsed {n_ok}/{nrow(data)} timestamps ({pct_parsed}%)")

  if (n_ok < nrow(data)) {
    log_warn("{nrow(data) - n_ok} entries have missing/unparseable timestamps")
  }

  data$.parsed_ts <- parsed

  # -- Handle degenerate case: all timestamps identical ---------------------
  unique_ts <- unique(parsed[!is.na(parsed)])
  if (length(unique_ts) <= 1L) {
    log_warn("All entries share the same timestamp -- temporal trends not meaningful")
    period_type <- "daily"
    data$.period <- .assign_period_labels(parsed, period_type)

    prevalence <- .compute_theme_prevalence(data, theme_set, period_type)
    emergence  <- .compute_theme_emergence(data, theme_set, coding_state)

    return(list(
      prevalence_over_time = prevalence,
      emergence_timeline   = emergence,
      period_type          = period_type,
      has_temporal_data    = TRUE
    ))
  }

  # -- Determine period granularity -----------------------------------------
  period_type <- .detect_time_periods(parsed)
  log_info("Temporal granularity: {period_type} (span covers {round(as.numeric(difftime(max(parsed, na.rm = TRUE), min(parsed, na.rm = TRUE), units = 'days')))} days)")

  data$.period <- .assign_period_labels(parsed, period_type)

  # -- Compute analyses -----------------------------------------------------
  prevalence <- .compute_theme_prevalence(data, theme_set, period_type)
  emergence  <- .compute_theme_emergence(data, theme_set, coding_state)

  log_info("Temporal analysis complete: {nrow(prevalence)} prevalence rows, {nrow(emergence)} themes tracked")


  list(
    prevalence_over_time = prevalence,
    emergence_timeline   = emergence,
    period_type          = period_type,
    has_temporal_data    = TRUE
  )
}

# ==============================================================================
# Helper: empty stub result
# ==============================================================================

#' Return an empty temporal result stub
#' @return List matching the shape of a successful temporal analysis
#' @keywords internal
.empty_temporal_result <- function() {
  list(
    prevalence_over_time = tibble(
      period          = character(0),
      theme_name      = character(0),
      n_entries       = integer(0),
      pct_of_period   = numeric(0),
      total_in_period = integer(0)
    ),
    emergence_timeline = tibble(
      theme_name            = character(0),
      first_appearance_date = character(0),
      first_code_date       = character(0),
      n_codes_at_emergence  = integer(0)
    ),
    period_type       = NA_character_,
    has_temporal_data = FALSE
  )
}

# ==============================================================================
# Exported: generate temporal plots
# ==============================================================================

#' Generate PNG plots for temporal analysis results
#'
#' Creates two publication-quality plots:
#' \enumerate{
#'   \item \code{temporal_prevalence.png} -- line chart showing theme
#'     prevalence (\%) over time periods (one coloured line per theme).
#'   \item \code{temporal_emergence.png} -- lollipop/timeline chart showing
#'     when each theme first appeared in the data.
#' }
#'
#' @param temporal_results List returned by \code{\link{analyze_temporal_patterns}}
#' @param output_dir Directory where PNGs will be saved (created if needed)
#' @return Invisible character vector of file paths written
#' @export
generate_temporal_plots <- function(temporal_results, output_dir) {
  if (!isTRUE(temporal_results$has_temporal_data)) {
    log_warn("No temporal data available -- skipping plot generation")
    return(invisible(character(0)))
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- character(0)

  # --- 1. Prevalence over time (line chart) ---------------------------------
  prev <- temporal_results$prevalence_over_time
  if (nrow(prev) > 0 && length(unique(prev$period)) > 1) {
    prev_path <- file.path(output_dir, "temporal_prevalence.png")

    n_themes <- length(unique(prev$theme_name))
    palette  <- rep_len(.TEMPORAL_PALETTE, n_themes)

    # Order periods chronologically (they sort lexicographically by design)
    prev$period <- factor(prev$period, levels = sort(unique(prev$period)))

    p1 <- ggplot2::ggplot(prev, ggplot2::aes(
      x = .data$period,
      y = .data$pct_of_period,
      colour = .data$theme_name,
      group  = .data$theme_name
    )) +
      ggplot2::geom_line(linewidth = 0.9, na.rm = TRUE) +
      ggplot2::geom_point(size = 2, na.rm = TRUE) +
      ggplot2::scale_colour_manual(values = palette) +
      ggplot2::labs(
        title    = "Theme Prevalence Over Time",
        subtitle = paste0("Period type: ", temporal_results$period_type),
        x        = "Time Period",
        y        = "% of Entries in Period",
        colour   = "Theme"
      ) +
      ggplot2::theme_minimal(base_family = "sans") +
      ggplot2::theme(
        plot.background   = ggplot2::element_rect(fill = "white", colour = NA),
        panel.background  = ggplot2::element_rect(fill = "white", colour = NA),
        panel.grid.major  = ggplot2::element_line(colour = "#EAECEE", linewidth = 0.4),
        panel.grid.minor  = ggplot2::element_blank(),
        axis.line         = ggplot2::element_line(colour = "#2C3E50", linewidth = 0.3),
        axis.text         = ggplot2::element_text(colour = "#7F8C8D", size = 9),
        axis.text.x       = ggplot2::element_text(angle = 45, hjust = 1),
        axis.title        = ggplot2::element_text(colour = "#2C3E50", size = 11,
                                                  face = "bold"),
        plot.title        = ggplot2::element_text(colour = "#2C3E50", size = 15,
                                                  face = "bold",
                                                  margin = ggplot2::margin(b = 8)),
        plot.subtitle     = ggplot2::element_text(colour = "#7F8C8D", size = 11,
                                                  margin = ggplot2::margin(b = 12)),
        legend.position   = "bottom",
        legend.background = ggplot2::element_rect(fill = "white", colour = NA),
        legend.title      = ggplot2::element_text(colour = "#2C3E50", face = "bold",
                                                  size = 9),
        legend.text       = ggplot2::element_text(colour = "#7F8C8D", size = 9),
        plot.margin       = ggplot2::margin(15, 15, 15, 15)
      )

    # Determine sensible dimensions
    plot_width  <- max(900, 60 * length(unique(prev$period)))
    plot_width  <- min(plot_width, 2400)  # cap

    grDevices::png(prev_path, width = plot_width, height = 700, res = 120)
    tryCatch(
      print(p1),
      error = function(e) log_warn("Prevalence plot rendering failed: {e$message}")
    )
    grDevices::dev.off()

    log_info("Temporal prevalence plot saved: {prev_path}")
    paths <- c(paths, prev_path)
  } else {
    log_info("Insufficient periods for prevalence plot (need > 1)")
  }

  # --- 2. Emergence timeline (lollipop chart) -------------------------------
  emerg <- temporal_results$emergence_timeline
  emerg <- emerg[!is.na(emerg$first_appearance_date), , drop = FALSE]

  if (nrow(emerg) > 0) {
    emerg_path <- file.path(output_dir, "temporal_emergence.png")

    emerg$appearance_date <- as.Date(emerg$first_appearance_date)

    # Sort by date
    emerg <- emerg[order(emerg$appearance_date), , drop = FALSE]
    emerg$theme_name <- factor(emerg$theme_name, levels = rev(emerg$theme_name))

    n_themes <- nrow(emerg)
    palette  <- rep_len(.TEMPORAL_PALETTE, n_themes)

    p2 <- ggplot2::ggplot(emerg, ggplot2::aes(
      x     = .data$appearance_date,
      y     = .data$theme_name,
      colour = .data$theme_name
    )) +
      ggplot2::geom_segment(
        ggplot2::aes(
          x    = min(emerg$appearance_date),
          xend = .data$appearance_date,
          y    = .data$theme_name,
          yend = .data$theme_name
        ),
        linewidth = 0.6,
        na.rm = TRUE
      ) +
      ggplot2::geom_point(size = 4, na.rm = TRUE) +
      ggplot2::scale_colour_manual(values = palette, guide = "none") +
      ggplot2::scale_x_date(date_labels = "%Y-%m-%d") +
      ggplot2::labs(
        title    = "Theme Emergence Timeline",
        subtitle = "Date each theme first appeared in the data",
        x        = "Date",
        y        = NULL
      ) +
      ggplot2::theme_minimal(base_family = "sans") +
      ggplot2::theme(
        plot.background  = ggplot2::element_rect(fill = "white", colour = NA),
        panel.background = ggplot2::element_rect(fill = "white", colour = NA),
        panel.grid.major.x = ggplot2::element_line(colour = "#EAECEE",
                                                    linewidth = 0.4),
        panel.grid.major.y = ggplot2::element_blank(),
        panel.grid.minor   = ggplot2::element_blank(),
        axis.line          = ggplot2::element_line(colour = "#2C3E50",
                                                    linewidth = 0.3),
        axis.text          = ggplot2::element_text(colour = "#7F8C8D", size = 10),
        axis.title         = ggplot2::element_text(colour = "#2C3E50", size = 11,
                                                    face = "bold"),
        plot.title         = ggplot2::element_text(colour = "#2C3E50", size = 15,
                                                    face = "bold",
                                                    margin = ggplot2::margin(b = 8)),
        plot.subtitle      = ggplot2::element_text(colour = "#7F8C8D", size = 11,
                                                    margin = ggplot2::margin(b = 12)),
        plot.margin        = ggplot2::margin(15, 15, 15, 15)
      )

    plot_height <- max(500, 40 * n_themes + 150)

    grDevices::png(emerg_path, width = 1000, height = plot_height, res = 120)
    tryCatch(
      print(p2),
      error = function(e) log_warn("Emergence plot rendering failed: {e$message}")
    )
    grDevices::dev.off()

    log_info("Temporal emergence plot saved: {emerg_path}")
    paths <- c(paths, emerg_path)
  } else {
    log_info("No emergence dates available for timeline plot")
  }

  invisible(paths)
}
