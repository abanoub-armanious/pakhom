# ==============================================================================
# Report Helper Functions -- Statistics Aggregation & AI Synthesis
# ==============================================================================
# Per-theme + overall statistics aggregation, metric-column detection, the
# paper-style per-subtheme summary tables, and the (optional) AI executive
# summary used by the HTML report.
# ==============================================================================

#' Does each entry's emerged_themes list contain theme `tn` exactly?
#'
#' \code{emerged_themes} is a "; "-delimited list of EXACT theme names. The old
#' test, \code{grepl(tn, emerged_themes, fixed = TRUE)}, matched on substrings,
#' so a theme whose name is contained in another's (e.g. "Sleep" inside "Sleep
#' Problems") produced false-positive membership and inflated every per-theme
#' count, correlation, and prevalence figure built from it. This does an exact,
#' token-level membership test instead. NA / empty entries are not members.
#'
#' @param emerged_themes Character vector (one "; "-joined list per entry).
#' @param tn Single exact theme name to test for.
#' @return Logical vector, same length as \code{emerged_themes}.
#' @keywords internal
.entry_in_theme <- function(emerged_themes, tn) {
  if (length(emerged_themes) == 0L) return(logical(0))
  vapply(emerged_themes, function(s) {
    if (is.na(s) || !nzchar(s)) return(FALSE)
    tn %in% trimws(strsplit(s, ";", fixed = TRUE)[[1]])
  }, logical(1), USE.NAMES = FALSE)
}

#' Aggregate per-theme statistics for report
#'
#' @param data tibble with theme_membership_* or emerged_themes columns
#' @param theme_set ThemeSet object
#' @param consolidated ConsolidatedCodes list (or NULL)
#' @param quotes_per_theme Integer; number of representative quotes to
#'   select per theme. Wired through from
#'   \code{config$analysis$themes$quotes_per_theme}; defaults to 3.
#' @param config Optional ThematicConfig or config list. When supplied,
#'   \code{config$data$column_mappings$metric_columns} is used as the
#'   explicit metric allowlist for the per-subtheme paper-style tables
#' When NULL or empty, metrics auto-detect from the data
#'   via \code{\link{.detect_metric_columns}}.
#' @param metric_interpretation Optional \code{MetricInterpretation} (from the
#'   Methodology Assistant), threaded to \code{.compute_subtheme_statistics} so
#'   each interpreted column additionally gets the AI's requested primitives.
#'   NULL (legacy / Mode 1) -> only the legacy Median(MAD)+Mean(SD) battery.
#' @return Named list of theme stats (one per theme). Each theme entry
#'   carries a \code{subtheme_stats} list with one element
#'   per real subtheme: n, per-metric Median(MAD) + Mean(SD), and
#'   metric-tagged example quotes -- the paper-style per-subtheme rows
#'   the report renders into a table inside each theme's card.
#' @export
aggregate_theme_statistics <- function(data, theme_set, consolidated = NULL,
                                         quotes_per_theme = 3L,
                                         config = NULL,
                                         metric_interpretation = NULL) {
  validate_class(theme_set, "ThemeSet")

  theme_stats <- list()
  total <- nrow(data)

  # dataset-agnostic metric column detection. Used for paper-
  # style per-subtheme summary tables + metric-tagged example quotes.
  metric_cols <- .detect_metric_columns(data, config)

  for (t in theme_set$themes) {
    tn <- t$name
    # Use multi-label membership columns to find all entries belonging to this theme
    safe_col <- paste0("theme_membership_", make.names(tn))
    if (safe_col %in% names(data)) {
      entries <- data[data[[safe_col]] == 1L, ]
    } else if ("emerged_themes" %in% names(data)) {
      entries <- data[!is.na(data$emerged_themes) &
                       .entry_in_theme(data$emerged_themes, tn), ]
    } else {
      entries <- data[0, ]
    }
    n <- nrow(entries)

    # subthemes are first-class Subtheme S3 objects under the
    # new hierarchy. The report renderer wants only named (non-virtual)
    # subthemes, so filter out NA-named virtual wrappers here at the
    # aggregation boundary.
    real_subtheme_names  <- .subtheme_names_no_virtual(t)
    real_subtheme_objs   <- Filter(function(s) {
      inherits(s, "Subtheme") && !is.na(s$name) && nchar(s$name %||% "") > 0L
    }, t$subthemes %||% list())

    # preserve theme_kind from apply_framework_themes so the
    # report renderer can section framework themes vs emergent themes vs
    # the bracket-policy Anomaly catch-all. Default to "framework" for
    # Mode 2 themes (which don't carry the field).
    theme_kind <- t$theme_kind %||% "framework"

    # Guard: empty themes get NA stats instead of NaN
    if (n == 0) {
      theme_stats[[tn]] <- list(
        name = tn,
        description = t$description %||% "",
        n_entries = 0L,
        pct_of_total = 0,
        sentiment = list(mean = NA_real_, sd = NA_real_, median = NA_real_,
                         pct_negative = 0, pct_positive = 0),
        intensity = list(mean = NA_real_, sd = NA_real_),
        emotions = tibble(emotion = character(), n = integer(), pct = numeric()),
        participant_spread = .empty_participant_spread(),
        keywords = t$keywords %||% character(0),
        subthemes = real_subtheme_names,
        subthemes_structured = real_subtheme_objs,
        subtheme_stats = list(),
        metric_cols = metric_cols,
        temporal_panel = NULL,   # no entries -> no temporal panel
        prevalence = t$prevalence %||% "unknown",
        theme_kind = theme_kind,
        quotes_with_context = list()
      )
      next
    }

    # Participant spread (T0.2 -- Jowsey "Frankenstein" answer): per-theme
    # counts of distinct contributors + Gini of the per-contributor entry-count
    # distribution + share of the most prolific contributor. Lets the report
    # surface themes that look prevalent but actually rest on one heavy poster.
    participant_spread <- .compute_participant_spread(entries)

    # Sentiment stats. Guard the column as aggregate_overall_statistics does: a
    # standalone/hand-edited data frame may lack sentiment_score or carry it as
    # strings, which would make mean()/sd() error ("non-numeric argument").
    ss <- if ("sentiment_score" %in% names(entries)) {
      suppressWarnings(as.numeric(entries$sentiment_score))
    } else numeric(0)
    has_sent <- any(!is.na(ss))
    sent <- list(
      mean = if (has_sent) round(mean(ss, na.rm = TRUE), 2) else NA_real_,
      sd = if (has_sent) round(sd(ss, na.rm = TRUE), 2) else NA_real_,
      median = if (has_sent) round(median(ss, na.rm = TRUE), 2) else NA_real_,
      pct_negative = if (has_sent) round(100 * sum(ss < -0.2, na.rm = TRUE) / max(n, 1), 1) else NA_real_,
      pct_positive = if (has_sent) round(100 * sum(ss > 0.2, na.rm = TRUE) / max(n, 1), 1) else NA_real_
    )

    # Intensity stats (same column guard)
    ei <- if ("emotion_intensity" %in% names(entries)) {
      suppressWarnings(as.numeric(entries$emotion_intensity))
    } else numeric(0)
    has_ei <- any(!is.na(ei))
    intensity <- list(
      mean = if (has_ei) round(mean(ei, na.rm = TRUE), 2) else NA_real_,
      sd = if (has_ei) round(sd(ei, na.rm = TRUE), 2) else NA_real_
    )

    # Emotion distribution (multi-label: split all_emotions and count each)
    emotions <- .count_multi_label_emotions(entries)

    # Representative quotes: prefer excerpt-based quotes from an enriched ThemeSet
    enriched_quotes <- t$supporting_quotes
    if (!is.null(enriched_quotes) && length(enriched_quotes) >= 1 &&
        all(nchar(enriched_quotes) > 0, na.rm = TRUE)) {
      # Build quotes_with_context from enriched supporting_quotes
      # Quotes were selected at positions 1, ceil(n/2), n in sentiment-sorted order
      sent_sorted <- entries |> arrange(sentiment_score)
      n_sorted <- nrow(sent_sorted)
      quote_labels <- c("most_negative", "median", "most_positive")[seq_along(enriched_quotes)]
      # Map quote indices to the same positions used in enrich_themes()
      if (length(enriched_quotes) >= 3 && n_sorted >= 3) {
        meta_indices <- c(1, ceiling(n_sorted / 2), n_sorted)
      } else {
        meta_indices <- seq_len(min(length(enriched_quotes), n_sorted))
      }
      # Resolve EACH representative quote's true sentiment + emotion
      # from its SOURCE ENTRY via the entry_id-linked supporting_quote_records
      # (built in the same order as supporting_quotes), rather than re-deriving by
      # sentiment-sort index. Index re-derivation could attach the wrong displayed
      # number to a quote once #8's theme-specificity selection picks an entry that
      # is not at position 1/median/n of the full sort. Falls back to the index
      # position for legacy state files that lack records or an entry_id.
      enriched_recs <- t$supporting_quote_records
      quotes <- list()
      for (qi in seq_along(enriched_quotes)) {
        rec <- if (!is.null(enriched_recs) && qi <= length(enriched_recs)) enriched_recs[[qi]] else NULL
        eid <- if (!is.null(rec)) rec$entry_id else NULL
        erow <- NULL
        if (!is.null(eid) && !is.na(eid) && "std_id" %in% names(entries)) {
          hit <- entries[!is.na(entries$std_id) & entries$std_id == eid, , drop = FALSE]
          if (nrow(hit) >= 1L) erow <- hit[1, ]
        }
        if (is.null(erow)) {
          idx <- if (qi <= length(meta_indices)) meta_indices[qi] else 1
          erow <- sent_sorted[min(idx, n_sorted), ]
        }
        lbl <- (if (!is.null(rec)) rec$position else NULL) %||% quote_labels[qi]
        emo <- if ("all_emotions" %in% names(erow)) erow$all_emotions %||% NA_character_ else NA_character_
        quotes[[lbl]] <- list(
          text = enriched_quotes[qi],
          sentiment = round(erow$sentiment_score, 2),
          emotion = emo
        )
      }
    } else {
      quotes <- .select_representative_quotes(entries, n_quotes = quotes_per_theme)
    }

    # per-subtheme paper-style statistics. For each REAL
    # (non-virtual) subtheme of this theme, compute n + Median(MAD) +
    # Mean(SD) for each auto-detected metric column + metric-tagged
    # example quotes. The renderer turns this list into a table inside
    # the theme's card. Themes without subthemes (or with only the
    # virtual NA-named wrapper) get an empty list.
    subtheme_stats <- .compute_subtheme_statistics(
      theme         = t,
      data          = data,
      metric_cols   = metric_cols,
      quotes_per_subtheme = quotes_per_theme,
      metric_interpretation = metric_interpretation
    )

    theme_stats[[tn]] <- list(
      name = tn,
      description = t$description %||% "",
      n_entries = n,
      pct_of_total = round(100 * n / max(total, 1), 1),
      sentiment = sent,
      intensity = intensity,
      emotions = emotions,
      participant_spread = participant_spread,
      # theme$keywords is capped at 8 frequency-ranked codes upstream.
      # The fallback fires only when a cached legacy theme_set
      # is resumed (no $keywords on the object). In that case frequency
      # ranking cannot be recovered (codebook not in scope here), so it caps
      # at 8 to match the modern count contract and leaves the order as the
      # theme's leaf order. The legacy resume warner
      # (.warn_legacy_coding_resume) covers the corresponding QuoteProvenance
      # case; cached theme_sets without $keywords are a separate older
      # vintage that this fallback documents rather than silently degrades.
      keywords = t$keywords %||% theme_codes(t)[seq_len(min(8L, length(theme_codes(t))))],
      subthemes = real_subtheme_names,
      subthemes_structured = real_subtheme_objs,
      subtheme_stats = subtheme_stats,
      metric_cols = metric_cols,
      # per-theme posting-time patterns over this theme's entry
      # timestamps, driven by the AI's temporal interpretations (NULL when no
      # temporal column was interpreted / Mode 1 / legacy).
      temporal_panel = .compute_theme_temporal_panel(entries, metric_interpretation),
      prevalence = t$prevalence %||% "unknown",
      theme_kind = theme_kind,
      quotes_with_context = quotes
    )
  }

  theme_stats
}

# ==============================================================================
# dataset-agnostic metric helpers + per-subtheme stats
# ==============================================================================
# The paper-style per-theme tables in publication-quality
# analyses show one row per subtheme with: n, Median(MAD) <metric>,
# Mean(SD) <metric>, Examples of comments tagged [metric: value; ...].
# pakhom implements that surface while staying dataset-
# agnostic: metrics auto-detect from any column_map$metrics override or
# fall back to "any numeric column the package didn't engineer."

#' Detect metric columns in a data frame, dataset-agnostically
#'
#' Returns the names of numeric columns in \code{data} that can sensibly
#' be summarized as quantitative metrics in per-theme + per-subtheme
#' tables. Package-internal columns (those pakhom engineers itself --
#' sentiment_score, emotion_intensity, theme_membership_*, etc.) are
#' excluded; everything else numeric is a candidate.
#'
#' Explicit override path: when \code{config$data$column_mappings$metric_columns}
#' is non-empty, those names are used verbatim (intersected with the data's
#' columns to avoid referencing missing fields). This matches the explicit
#' \code{detect_columns()} mapping path in \code{R/07_data_loading.R}.
#'
#' Mirrors (and consolidates) the metric-column detection used by
#' \code{prepare_correlation_data} (R/14_correlations.R).
#'
#' @param data tibble with the analytical data (post-cascade)
#' @param config Optional ThematicConfig or config list
#' @param explicit Optional character vector of metric column names to
#'   use verbatim (intersected with the data's columns). Bypasses the
#'   config dig.
#'
#' @section Caveat -- sentiment_score collision:
#'   The auto-detect path excludes \code{sentiment_score} (the package-
#'   engineered column from R/10_sentiment.R) by name. If a user's
#'   corpus happens to have its own \code{sentiment_score} numeric
#'   column that they want treated AS A METRIC, the auto-detect silently
#'   drops it. Workaround: supply an explicit override via
#'   \code{config$data$column_mappings$metric_columns} (or the direct
#'   \code{explicit=} arg) -- the override path returns the requested
#'   columns verbatim, bypassing the internal-column exclusion. The
#'   same collision applies to any other internal name
#'   (\code{emotion_intensity}, \code{n_themes}, etc.).
#' @return Character vector of metric column names (possibly empty)
#' @keywords internal
.detect_metric_columns <- function(data, config = NULL, explicit = NULL) {
  # Direct injection path (used by callers with a pre-resolved override)
  if (!is.null(explicit) && length(explicit) > 0L) {
    return(intersect(as.character(explicit), names(data)))
  }

  # Config-dig path
  if (!is.null(config)) {
    if (inherits(config, "ThematicConfig")) {
      explicit <- config$data$column_mappings$metric_columns
    } else if (is.list(config)) {
      explicit <- config$data$column_mappings$metric_columns %||%
                    config$column_mappings$metric_columns %||%
                    config$metric_columns
    }
  }
  if (!is.null(explicit) && length(explicit) > 0L) {
    return(intersect(as.character(explicit), names(data)))
  }

  # Auto-detect: numeric, not package-internal, not theme_membership_*
  internal_cols <- c(
    "std_id", "std_text", "std_author", "std_timestamp", "original_text",
    "sentiment_score", "emotion_intensity", "confidence",
    "all_emotions", "emerged_themes", "n_themes", "source_table",
    "subtheme_assignments", ".parsed_ts"
  )
  is_internal <- function(nm) {
    nm %in% internal_cols || grepl("^theme_membership_", nm)
  }
  keep <- vapply(names(data), function(nm) {
    !is_internal(nm) && is.numeric(data[[nm]])
  }, logical(1))
  names(data)[keep]
}

#' Find the Methodology Assistant's interpretation record for a metric column
#'
#' Looks up the AI's per-metric interpretation (from
#' \code{MetricInterpretation$metrics}) by exact \code{column_name}. Only numeric
#' metric columns are matched here; timestamp columns are handled by the
#' per-theme temporal panel. Returns NULL when no record exists --
#' the per-column fallback to the legacy stats battery (audit notes N1/N2).
#'
#' @keywords internal
.metric_interpretation_record <- function(metric_interpretation, column_name) {
  if (is.null(metric_interpretation) ||
      is.null(metric_interpretation$metrics) ||
      length(metric_interpretation$metrics) == 0L) {
    return(NULL)
  }
  for (rec in metric_interpretation$metrics) {
    if (identical(rec$column_name, column_name)) return(rec)
  }
  NULL
}

#' Compute the AI's requested primitives for one column's values
#'
#' Dispatches each requested primitive through the allowlist
#' \code{compute_metric_stat()} (which fails honestly on an unknown name and
#' parses datetime-string timestamps), returning the interpretation note
#' alongside the per-primitive results for the report renderer.
#' Step 3.5: each primitive's \code{args} (a named list) is threaded
#' to \code{compute_metric_stat()}. The live discovery schema is args-free, so
#' discovery records carry no args (empty list -> unchanged behavior); a pinned
#' record can carry e.g. \code{list(q = 0.9)} for \code{prim_quantile} or
#' \code{list(bin_width_days = 30)} for \code{prim_entries_over_time}.
#'
#' @keywords internal
.compute_requested_primitives <- function(rec, values) {
  stats_list <- lapply(rec$requested_primitives %||% list(), function(p) {
    compute_metric_stat(p$primitive, values, args = p$args %||% list())
  })
  list(
    column_description  = rec$column_description %||% "",
    interpretation_note = rec$interpretation_note %||% "",
    # Carry the AI's per-column reliability floor so the renderer can
    # MARK (not hide) spread/shape cells computed on fewer entries than this.
    min_reliable_n      = rec$min_reliable_n %||% NA_integer_,
    requested           = stats_list
  )
}

# ---- Step 3: per-theme temporal panel ----------------------------------------
# The AI's temporal interpretations (metric_interpretation$temporal_columns) had
# ZERO consumers before 61.4 -- recorded and archived, never computed. The panel
# applies each requested temporal primitive to THIS theme's entry timestamps, so
# a reviewer sees when a theme's entries were actually posted (e.g. evening
# clustering of distress) alongside the AI's interpretation note.

# Enumerate every "YYYY-MM" from first to last inclusive (base R, no lubridate).
.enumerate_year_months <- function(first_ym, last_ym) {
  f <- suppressWarnings(as.Date(paste0(first_ym, "-01")))
  l <- suppressWarnings(as.Date(paste0(last_ym,  "-01")))
  if (is.na(f) || is.na(l) || l < f) return(character(0))
  out <- character(0); d <- f
  while (d <= l) {
    out <- c(out, format(d, "%Y-%m"))
    d <- seq(d, by = "month", length.out = 2L)[2L]   # robust month step
  }
  out
}

# Zero-fill the open-ended timeline distributions (L2). The cyclic distributions
# (hour/day/month-of-year) already cover all bins via factor levels, so only the
# two open-ended timelines need densifying: an omitted middle bin would look
# contiguous and overstate continuity. Display-only: the backend primitive stays
# pure (it returns observed bins); the panel densifies for an honest, continuous
# reading. Fail-safe: returns the value unchanged if the grid can't be rebuilt.
.densify_temporal_distribution <- function(primitive, value, args = list()) {
  if (is.null(value) || length(value) == 0L || is.null(names(value))) return(value)
  # SAFETY INVARIANT (both branches): densifying must never DROP an observed
  # bin. If the reconstructed continuous grid fails to contain every observed
  # label -- which can happen if the grid arithmetic ever diverges from the
  # primitive's own bin arithmetic (a fractional bin_width_days, a Date-space vs
  # POSIXct-space rounding edge, a locale surprise) -- the zero-fill is abandoned
  # and return the primitive's value verbatim. Better an un-densified but
  # complete panel than a continuous-looking one that silently lost a real bin.
  tryCatch({
    if (identical(primitive, "prim_entries_by_month")) {
      full <- .enumerate_year_months(names(value)[1], names(value)[length(value)])
      if (length(full) == 0L) return(value)
      if (!all(names(value) %in% full)) return(value)          # never drop a bin
      out <- stats::setNames(rep(0, length(full)), full)
      out[names(value)] <- as.numeric(value)
      out
    } else if (identical(primitive, "prim_entries_over_time")) {
      bw <- suppressWarnings(as.numeric(args$bin_width_days))
      if (length(bw) != 1L || is.na(bw) || bw <= 0) return(value)
      ds <- suppressWarnings(as.Date(names(value)))
      if (any(is.na(ds))) return(value)
      grid <- seq(min(ds), max(ds), by = sprintf("%d days", as.integer(round(bw))))
      labs <- format(grid, "%Y-%m-%d")
      if (!all(names(value) %in% labs)) return(value)          # never drop a bin
      out <- stats::setNames(rep(0, length(labs)), labs)
      out[names(value)] <- as.numeric(value)
      out
    } else {
      value
    }
  }, error = function(e) value)
}

#' Compute the per-theme temporal panel
#'
#' For each timestamp column the Methodology Assistant interpreted, applies the
#' AI's requested temporal primitives to \code{entries[[column_name]]} (this
#' theme's rows). Continuous-timeline distributions are zero-filled (L2) so empty
#' bins are visible rather than silently skipped. Returns NULL when there is no
#' temporal interpretation, no entries, or the named column is absent (so the
#' renderer simply omits the panel).
#'
#' @param entries Tibble of this theme's entries.
#' @param metric_interpretation A \code{MetricInterpretation}, or NULL.
#' @return A list of per-column panels (each: \code{column_name},
#'   \code{column_description}, \code{interpretation_note}, \code{requested} =
#'   per-primitive \code{compute_metric_stat()} records, densified), or NULL.
#' @keywords internal
.compute_theme_temporal_panel <- function(entries, metric_interpretation) {
  if (is.null(metric_interpretation)) return(NULL)
  tcols <- metric_interpretation$temporal_columns %||% list()
  if (length(tcols) == 0L || is.null(entries) || nrow(entries) == 0L) return(NULL)

  panels <- list()
  for (rec in tcols) {
    cn <- rec$column_name %||% NA_character_
    if (is.na(cn) || !cn %in% names(entries)) next
    raw <- entries[[cn]]
    stats_list <- lapply(rec$requested_primitives %||% list(), function(p) {
      r <- compute_metric_stat(p$primitive, raw, args = p$args %||% list())
      if (isTRUE(r$available) && identical(r$shape, "distribution")) {
        r$value <- .densify_temporal_distribution(p$primitive, r$value,
                                                   args = p$args %||% list())
      }
      r
    })
    if (length(stats_list) == 0L) next
    panels[[length(panels) + 1L]] <- list(
      column_name         = cn,
      column_description  = rec$column_description %||% "",
      interpretation_note = rec$interpretation_note %||% "",
      requested           = stats_list
    )
  }
  if (length(panels) == 0L) return(NULL)
  panels
}

#' Compute per-subtheme statistics for a theme (paper-style)
#'
#' For each REAL (non-virtual, named) subtheme of \code{theme}, returns
#' a record carrying:
#' \itemize{
#'   \item \code{name}: subtheme name
#'   \item \code{description}: subtheme description (1-2 sentences)
#'   \item \code{n}: entries that contributed to this subtheme
#'   \item \code{metric_stats}: list keyed by metric column name; each
#'     entry has \code{median}, \code{mad}, \code{mean}, \code{sd}
#'   \item \code{example_quotes}: character vector of representative
#'     quotes tagged with per-entry metric values
#'     (e.g. \samp{"<quote text>... [<metric_a>: 8; <metric_b>: 12]"})
#' }
#'
#' Virtual NA-named subtheme wrappers (added by the ThemeSet hierarchy
#' for themes without AI-clustered subthemes) are skipped. Themes with
#' only virtual subthemes return an empty list -- the renderer falls
#' back to the theme-level summary in that case.
#'
#' Membership: an entry belongs to subtheme S if its
#' \code{subtheme_assignments} column (populated by
#' \code{cascade_theme_assignments}) contains S's name. When that
#' column is absent the fallback is "every entry in the theme is in
#' every subtheme" (degenerate but non-fatal -- the table still renders,
#' just without entry-level filtering).
#'
#' @param theme A theme list (one element of \code{theme_set$themes})
#' @param data Analytical tibble with theme_membership_* +
#'   subtheme_assignments columns
#' @param metric_cols Character vector of metric column names
#'   (from \code{.detect_metric_columns})
#' @param quotes_per_subtheme Integer; default 3
#' @param metric_interpretation Optional \code{MetricInterpretation} (from the
#'   Methodology Assistant). When supplied, each interpreted column ALSO gets the
#'   AI's requested primitives computed into \code{ai_metric_stats}; the legacy
#'   \code{metric_stats} battery is always computed (renderer fallback). NULL
#'   (legacy / Mode 1) -> only the legacy battery.
#' @return Named list (one per real subtheme) of stat records. Each record adds
#'   \code{ai_metric_stats}: a list keyed by metric column name
#'   (only for interpreted columns) of \code{column_description},
#'   \code{interpretation_note}, and \code{requested} (per-primitive
#'   \code{compute_metric_stat()} results).
#' @keywords internal
.compute_subtheme_statistics <- function(theme, data, metric_cols,
                                            quotes_per_subtheme = 3L,
                                            metric_interpretation = NULL) {
  if (is.null(theme$subthemes) || length(theme$subthemes) == 0L) {
    return(list())
  }

  # Restrict to entries belonging to THIS theme. Reuse the same
  # membership signal aggregate_theme_statistics uses.
  tn <- theme$name
  safe_col <- paste0("theme_membership_", make.names(tn))
  theme_entries <- if (safe_col %in% names(data)) {
    data[data[[safe_col]] == 1L, ]
  } else if ("emerged_themes" %in% names(data)) {
    data[!is.na(data$emerged_themes) &
           .entry_in_theme(data$emerged_themes, tn), ]
  } else {
    data[0, ]
  }

  out <- list()
  for (s in theme$subthemes) {
    if (!inherits(s, "Subtheme")) next
    snm <- s$name %||% NA_character_
    if (is.na(snm) || nchar(snm %||% "") == 0L) next  # virtual wrapper

    # This loop iterates only
    # depth-1 subthemes (theme$subthemes). Nested sub-subthemes
    # (introduced by C-12) are NOT broken out as separate rows in the
    # paper-style summary table -- their codes contribute to the
    # parent subtheme's row instead. This matches the cascade design:
    # subtheme_assignments stores only the top-level subtheme name. The
    # full nested decomposition lives in themes.json and the HTML
    # renderer's indented subtheme list. If a downstream consumer
    # needs per-sub-subtheme stats, they can compute them from
    # themes.json + theme_entries/*.csv directly.
    #
    # Entries within this subtheme: filter the theme's entries by the
    # subtheme_assignments column (semicolon-separated names; same
    # serialization cascade_theme_assignments produces).
    # Exact token membership against the ";"-joined subtheme_assignments list
    # (reuse .entry_in_theme, the same exact-match helper aggregate_theme_
    # statistics uses for themes). A raw substring grepl(snm, ...) would treat
    # any subtheme whose name is a substring of another ("Sleep" in "Sleep
    # quality") as a member, inflating n, medians, and the example quotes in
    # the paper-style per-subtheme table.
    sub_entries <- if ("subtheme_assignments" %in% names(theme_entries)) {
      theme_entries[.entry_in_theme(theme_entries$subtheme_assignments, snm), ]
    } else {
      theme_entries  # fallback: theme-wide
    }
    n_sub <- nrow(sub_entries)

    # Per-metric stats. The legacy Median(MAD)+Mean(SD) battery is ALWAYS
    # computed (the current renderer reads it; the report prefers the AI stats
    # when present). The report ALSO computes the Methodology Assistant's
    # requested primitives for any column it interpreted (matched by name);
    # columns with no interpretation record keep ONLY the legacy battery
    # (per-column fallback, audit notes N1/N2).
    metric_stats    <- list()
    ai_metric_stats <- list()
    for (mc in metric_cols) {
      if (!mc %in% names(sub_entries)) next
      vals <- suppressWarnings(as.numeric(sub_entries[[mc]]))
      vals <- vals[!is.na(vals)]
      if (length(vals) == 0L) {
        metric_stats[[mc]] <- list(median = NA_real_, mad = NA_real_,
                                     mean = NA_real_, sd = NA_real_,
                                     n_observed = 0L)
      } else {
        metric_stats[[mc]] <- list(
          median     = round(stats::median(vals), 2),
          mad        = round(stats::mad(vals), 2),
          mean       = round(mean(vals), 2),
          sd         = round(stats::sd(vals), 2),
          n_observed = length(vals)
        )
      }
      rec <- .metric_interpretation_record(metric_interpretation, mc)
      if (!is.null(rec)) {
        # Pass the RAW column (compute_metric_stat cleans + parses datetime
        # strings itself, the 61.2 H2 fix) rather than the NA-stripped vals.
        ai_metric_stats[[mc]] <- .compute_requested_primitives(rec, sub_entries[[mc]])
      }
    }

    # Example quotes tagged with metric values per entry
    example_quotes <- .select_metric_tagged_quotes(
      entries = sub_entries,
      metric_cols = metric_cols,
      n_quotes = quotes_per_subtheme
    )

    out[[snm]] <- list(
      name            = snm,
      description     = s$description %||% "",
      n               = n_sub,
      metric_stats    = metric_stats,
      ai_metric_stats = ai_metric_stats,
      example_quotes  = example_quotes
    )
  }
  out
}

#' Select representative example quotes tagged with per-entry metric values
#'
#' Picks up to \code{n_quotes} entries using the same sentiment-positioned
#' selection as \code{.select_representative_quotes} (so the quotes span
#' the sentiment range when available), then formats each as the entry's
#' text followed by a bracketed metric-tag block:
#' \samp{"<quote text>" [<metric_a>: 8; <metric_b>: 12]}
#'
#' When the entry is missing a metric, that metric is omitted from the
#' tag (rather than printing "NA"); when all metrics are missing the
#' tag itself is omitted.
#'
#' @keywords internal
.select_metric_tagged_quotes <- function(entries, metric_cols, n_quotes = 3L) {
  if (nrow(entries) == 0L) return(character(0))

  text_col <- if ("std_text" %in% names(entries)) "std_text"
              else if ("original_text" %in% names(entries)) "original_text"
              else NA_character_
  if (is.na(text_col)) return(character(0))

  # Sentiment-positioned selection when sentiment_score is available and
  # there are enough entries to span the range; else first-N. Pick
  # ROW INDICES (not just text) so the metric-tag can read the source
  # row's metric columns alongside the text -- something the existing
  # .select_representative_quotes helper doesn't preserve.
  if ("sentiment_score" %in% names(entries) &&
      any(!is.na(entries$sentiment_score)) &&
      nrow(entries) >= 2L) {
    ord <- order(entries$sentiment_score, na.last = NA)
    n_valid <- length(ord)
    target_slots <- if (n_quotes >= 3L && n_valid >= 3L) {
      c(ord[1], ord[ceiling(n_valid / 2L)], ord[n_valid])
    } else if (n_quotes >= 2L && n_valid >= 2L) {
      c(ord[1], ord[n_valid])
    } else {
      ord[1]
    }
    picked_idx <- target_slots[seq_len(min(length(target_slots), n_quotes))]
  } else {
    picked_idx <- seq_len(min(n_quotes, nrow(entries)))
  }

  vapply(picked_idx, function(i) {
    if (is.na(i)) return(NA_character_)
    row <- entries[i, , drop = FALSE]
    # word-boundary-aware truncation with
    # visible ellipsis instead of a bare substr cut. An earlier version
    # quote was cut at exactly 280 chars even mid-word, then the
    # metric tag was butted up against the partial word (real
    # examples: "I've gained 7 pounds since Ma[score: 5]"). Now:
    # truncate to <= 280 chars, back off to the last whitespace
    # boundary if necessary, append " ..." marker.
    raw_text <- as.character(row[[text_col]] %||% "")
    q <- .truncate_quote_word_boundary(raw_text, max_chars = 280L)
    if (nchar(q) == 0L) return(NA_character_)
    tag <- .format_metric_tag(row, metric_cols)
    if (nzchar(tag)) paste0(q, " ", tag) else q
  }, character(1)) |> stats::na.omit() |> as.character()
}

#' Word-boundary-aware quote truncation with visible ellipsis
#'
#' a quote longer than \code{max_chars} is cut
#' back to the LAST whitespace character before the limit, then a
#' " ..." marker is appended so the reader sees the truncation. Falls
#' back to a hard substr cut + "..." when the entry contains no
#' whitespace within the budget (rare; typically single-token URLs or
#' user handles).
#'
#' @param text Input string (may be NA).
#' @param max_chars Total budget INCLUDING the " ..." marker.
#' @return Truncated character.
#' @keywords internal
.truncate_quote_word_boundary <- function(text, max_chars = 280L) {
  if (is.na(text) || !nzchar(text)) return("")
  text <- as.character(text)
  if (nchar(text) <= max_chars) return(text)
  # Reserve 4 chars for the " ..." marker (space + 3-dot ellipsis)
  budget <- max_chars - 4L
  if (budget <= 0L) {
    return(substr(text, 1L, max(1L, max_chars - 3L)))
  }
  cut <- substr(text, 1L, budget)
  last_ws <- regexpr("\\s\\S*$", cut, perl = TRUE)
  if (last_ws > 0L) {
    # Cut at the last whitespace so a word isn't broken. The
    # boundary-equality case (last_ws + match.length - 1 == budget) used to
    # have a separate branch with identical behaviour -- collapsed
    # to keep the contract single-pathed.
    cut <- substr(cut, 1L, last_ws - 1L)
  }
  cut <- trimws(cut)
  if (nchar(cut) == 0L) {
    # Hard-cut fall-through (no whitespace in budget, e.g. a long URL).
    # Use the same " ..." marker as the normal path so the docstring's
    # 4-char budget invariant is honoured.
    return(paste0(substr(text, 1L, budget), " ..."))
  }
  paste0(cut, " ...")
}

#' Format a bracketed metric-tag block for one entry row
#'
#' \samp{[<metric_a>: 8; <metric_b>: 12]} -- one metric=value pair per metric
#' column the entry has a non-NA value for, semicolon-separated, wrapped
#' in square brackets. Returns the empty string when the row is missing
#' all metric values (so the renderer can omit the tag entirely).
#'
#' @keywords internal
.format_metric_tag <- function(row, metric_cols) {
  if (is.null(row) || is.null(metric_cols) || length(metric_cols) == 0L) return("")
  parts <- character(0)
  for (mc in metric_cols) {
    if (!mc %in% names(row)) next
    val <- row[[mc]]
    if (length(val) == 0L || is.na(val[1])) next
    parts <- c(parts, sprintf("%s: %s", mc, .format_metric_value(val[1])))
  }
  if (length(parts) == 0L) return("")
  paste0("[", paste(parts, collapse = "; "), "]")
}

#' Pretty-print a single metric value (round numeric; preserve int)
#' @keywords internal
.format_metric_value <- function(x) {
  if (is.numeric(x)) {
    # Integer-looking values print without trailing .0
    if (abs(x - round(x)) < 1e-9) format(as.integer(round(x)))
    else format(round(x, 2))
  } else {
    as.character(x)
  }
}

#' Format a per-metric Median(MAD) or Mean(SD) summary as a string
#'
#' "8.0 (1.5)" -- one summary statistic + its variability measure in
#' parentheses. Returns "n/a" when the value is NA so the renderer
#' produces a meaningful cell rather than NaN/NA artifacts.
#'
#' @keywords internal
.format_metric_summary <- function(center, spread, digits = 2L) {
  if (is.na(center) || is.na(spread)) return("n/a")
  sprintf("%s (%s)",
          format(round(as.numeric(center), digits), nsmall = digits),
          format(round(as.numeric(spread), digits), nsmall = digits))
}

# ---- Render the AI analyst's chosen primitives ---------------------------------
# These render a compute_metric_stat() record (the AI's chosen primitive +
# result) into report cells. They are shared by the per-subtheme summary table
# (.build_subtheme_summary_table) and the per-theme temporal panel. The catalog
# the AI chose from is backend scaffolding; what these surface is the AI's OWN
# recorded decision (the primitive it named + its free-form interpretation note),
# never a fixed taxonomy imposed on the researcher.

#' Pretty-print a primitive name for a report header
#'
#' Strips the \code{prim_} prefix and turns underscores into spaces, so
#' \code{prim_hour_of_day_distribution} reads as "hour of day distribution".
#' Reflects the AI's chosen primitive name verbatim (dataset-agnostic; there is
#' no hardcoded label lookup -- an AI-named primitive the catalog lacks still
#' prints its requested name).
#'
#' @param p Character primitive name (e.g. \code{"prim_p90"}).
#' @return A display string.
#' @keywords internal
.pretty_primitive_name <- function(p) {
  gsub("_+", " ", sub("^prim_", "", as.character(p)[1] %||% ""))
}

#' Format one compute_metric_stat() record as an HTML table cell
#'
#' Renders a single primitive result for the per-subtheme + temporal tables:
#' \itemize{
#'   \item available scalar -> the rounded value (via [.format_metric_value]);
#'   \item available distribution -> up to \code{max_items} "label: count" pairs
#'     (descending count) with an honest "(+k more)" suffix when truncated;
#'   \item unavailable (fail-honest, design requirement R4) -> an em dash, with
#'     the gap reason in the cell's \code{title} -- NEVER a substituted statistic;
#'   \item NULL / NA / empty -> "n/a".
#' }
#' Distribution labels are HTML-escaped. The em-dash branch is how the report
#' surfaces "the AI requested X but X is not in the catalog" without inventing a
#' number (the whole point of fail-honest).
#'
#' @param prec One element of \code{ai_metric_stats[[col]]$requested} (a record
#'   returned by [compute_metric_stat]); may be NULL.
#' @param max_items Maximum distribution pairs to show before truncating.
#' @param natural_order When TRUE, keep the distribution's existing name order
#'   (used by the temporal panel so chronological/clock order is preserved);
#'   when FALSE (default) sort by descending count (used by the per-subtheme
#'   metric table, where "most common first" is the honest compact summary).
#' @return A character HTML cell string.
#' @keywords internal
.format_primitive_result <- function(prec, max_items = 6L, natural_order = FALSE) {
  if (is.null(prec)) return("n/a")
  if (!isTRUE(prec$available)) {
    return(sprintf('<span class="prim-unavailable" title="%s">&mdash;</span>',
                   .html_esc(prec$reason %||% "requested primitive unavailable")))
  }
  val <- prec$value
  if (identical(prec$shape, "distribution")) {
    if (is.null(val) || length(val) == 0L) return("n/a")
    nm <- names(val)
    if (is.null(nm)) nm <- as.character(seq_along(val))
    idx <- if (isTRUE(natural_order)) seq_along(val)
           else order(as.numeric(val), decreasing = TRUE)
    shown <- idx[seq_len(min(length(idx), max_items))]
    pairs <- vapply(shown, function(i)
      sprintf("%s: %s", .html_esc(nm[i]), .format_metric_value(val[[i]])),
      character(1))
    out <- paste(pairs, collapse = ", ")
    extra <- length(val) - length(shown)
    if (extra > 0L) out <- paste0(out, sprintf(" (+%d more)", extra))
    out
  } else {
    if (length(val) == 0L || is.na(val[1])) return("n/a")
    .format_metric_value(val[[1]])
  }
}

# ==============================================================================
# Participant spread
# ==============================================================================
# T0.2 answers Jowsey et al. 2025's empirical finding that "none of the
# Copilot outputs reported the participant spread." Per-theme metrics:
#   n_distinct_contributors -- how many unique authors contributed to this
#     theme's entries
#   contributor_gini        -- Gini coefficient of per-contributor entry-count
#     distribution. 0.0 = each contributor has the same number of entries
#     (perfectly equal); 1.0 = one contributor has all entries.
#   top_contributor_share   -- fraction of theme's entries from the single
#     most prolific contributor. Quick "is this one person's theme?" check.
#
# Without these, themes that look prevalent (n_entries high) can secretly
# be one heavy poster repeating themselves. The Frankenstein paper made
# this opacity non-negotiable; pakhom's Tier-0 commitment is to surface it
# regardless of mode.
#
# Author column comes from std_author (set in R/07_data_loading.R based on
# config column_mappings). When std_author is absent (anonymous data,
# legacy runs, datasets that didn't supply an author column) the metrics
# return their empty shape -- the report renders a "contributor data
# unavailable" notice rather than crashing or silently dropping the
# section.

#' Compute participant spread metrics for a theme's entries
#'
#' @param entries tibble of entries belonging to this theme (must have
#'   a \code{std_author} column when participant spread is desired; when
#'   the column is missing or all NA, returns the empty-shape list).
#' @return Named list:
#'   \itemize{
#'     \item \code{n_distinct_contributors}: integer count of unique non-NA
#'       \code{std_author} values
#'     \item \code{contributor_gini}: Gini coefficient (\code{NA_real_} when
#'       there are no contributors or only one)
#'     \item \code{top_contributor_share}: fraction of entries from the
#'       single most prolific contributor (\code{NA_real_} when no
#'       contributors)
#'     \item \code{available}: logical -- TRUE when \code{std_author}
#'       was usable, FALSE when absent or all NA. Lets downstream
#'       rendering distinguish "no data" from "data shows even spread".
#'   }
#' @keywords internal
.compute_participant_spread <- function(entries) {
  if (!"std_author" %in% names(entries)) {
    return(.empty_participant_spread())
  }
  authors <- entries$std_author
  authors_nonna <- authors[!is.na(authors) & nzchar(as.character(authors))]
  if (length(authors_nonna) == 0L) {
    return(.empty_participant_spread())
  }

  counts <- as.integer(table(authors_nonna))
  n_contrib <- length(counts)
  total_entries <- sum(counts)

  # Gini is undefined for n_contrib == 0; conventionally 0 for n_contrib == 1
  # (no inequality possible with one value). NA is returned in both cases so
  # the dashboard distinguishes "1 contributor (Gini meaningless)" from
  # "many contributors, perfectly even (Gini = 0)".
  gini <- if (n_contrib >= 2L) .gini_coefficient(counts) else NA_real_
  top_share <- max(counts) / total_entries

  list(
    n_distinct_contributors = n_contrib,
    contributor_gini        = gini,
    top_contributor_share   = top_share,
    available               = TRUE
  )
}

#' Empty-shape result for the participant-spread sub-list
#'
#' Used in two cases: themes with zero entries (no contributors to count),
#' and entries datasets that don't carry an \code{std_author} column.
#' Keeping a stable shape across those cases simplifies downstream rendering.
#' @keywords internal
.empty_participant_spread <- function() {
  list(
    n_distinct_contributors = 0L,
    contributor_gini        = NA_real_,
    top_contributor_share   = NA_real_,
    available               = FALSE
  )
}

#' Gini coefficient of a non-negative numeric vector
#'
#' Standard sample Gini based on the mean absolute difference, normalized
#' to the unit interval. Returns 0 when all values are identical, NA when the input
#' is empty / contains negatives / has zero sum.
#'
#' Implemented inline (rather than depending on \code{ineq}) because (a)
#' Gini is a one-line formula not worth a 21st imported package and (b)
#' pakhom's CRAN dependency footprint is already noted as accept-as-noise.
#'
#' @param x Non-negative numeric vector (per-contributor entry counts).
#' @return Numeric Gini in the unit interval, or \code{NA_real_} on degenerate
#'   inputs.
#' @keywords internal
.gini_coefficient <- function(x) {
  if (length(x) == 0L) return(NA_real_)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)
  total <- sum(x)
  if (total == 0) return(NA_real_)
  n <- length(x)
  # Sorted-values closed-form: G = (2 * sum(i * x[i])) / (n * sum(x)) - (n+1)/n
  # where x is sorted ascending and i is 1-indexed. Equivalent to the
  # mean-absolute-difference definition; numerically more stable for large n.
  x_sorted <- sort(x)
  weighted <- sum(seq_along(x_sorted) * x_sorted)
  g <- (2 * weighted) / (n * total) - (n + 1) / n
  # Numerical clamp -- closed-form can produce -1e-16 for perfectly equal x
  max(0, min(1, g))
}

#' Aggregate overall analysis statistics for report
#'
#' @param data tibble with all analysis columns
#' @param theme_set ThemeSet object
#' @param consolidated ConsolidatedCodes list (or NULL)
#' @param learning_context LearningContext object (or NULL)
#' @param config ThematicConfig object (or NULL)
#' @return List of overall statistics
#' @export
aggregate_overall_statistics <- function(data, theme_set, consolidated = NULL,
                                          learning_context = NULL, config = NULL) {
  total <- nrow(data)

  # Theme distribution counted from the multi-label membership columns.
  # iterate the ORIGINAL theme_set$themes names
  # instead of round-tripping membership column names through
  # make.names(). The pre-fix path used `sub("^theme_membership_",
  # "", ...)` + `gsub("\\.", " ", ...)` which only recovered space
  # characters. make.names() also collapses hyphens, apostrophes,
  # slashes, colons, commas, and parentheses to periods, so 71 of
  # 417 themes in a large run were never named correctly in the
  # dashboard or Appendix B (every missing theme had a non-period
  # special character in its name).
  membership_cols <- grep("^theme_membership_", names(data), value = TRUE)
  if (length(membership_cols) > 0 && !is.null(theme_set) &&
      inherits(theme_set, "ThemeSet") && length(theme_set$themes) > 0) {
    # Direct iteration: for each known theme, compute its safe column
    # name via make.names() and read counts off that column. Themes
    # without a corresponding column (rare; usually a Mode 3 anomaly
    # bracket case) get a 0 count.
    theme_data <- lapply(theme_set$themes, function(t) {
      orig_name <- t$name
      safe_col <- paste0("theme_membership_", make.names(orig_name))
      n_entries <- if (safe_col %in% names(data)) {
        as.integer(sum(data[[safe_col]] == 1L, na.rm = TRUE))
      } else 0L
      list(name = orig_name, n = n_entries)
    })
    themes_df <- tibble(
      theme_name = vapply(theme_data, function(x) x$name, character(1)),
      n = vapply(theme_data, function(x) x$n, integer(1)),
      pct = round(100 * vapply(theme_data, function(x) x$n, integer(1)) /
                    max(total, 1), 1)
    ) |> arrange(desc(n))
  } else if (length(membership_cols) > 0) {
    # Legacy fallback (no theme_set available): the lossy round-trip
    # path. Only fires when callers don't supply theme_set, which
    # would have been a regression in the production callsite but is
    # still possible in some test fixtures.
    theme_counts <- vapply(membership_cols, function(col) sum(data[[col]] == 1L, na.rm = TRUE), integer(1))
    theme_labels <- sub("^theme_membership_", "", names(theme_counts))
    theme_labels <- gsub("\\.", " ", theme_labels)
    themes_df <- tibble(
      theme_name = theme_labels,
      n = as.integer(theme_counts),
      pct = round(100 * theme_counts / max(total, 1), 1)
    ) |> arrange(desc(n))
  } else if ("emerged_themes" %in% names(data)) {
    # Fallback: parse semicolon-separated emerged_themes
    all_themes <- unlist(strsplit(data$emerged_themes[!is.na(data$emerged_themes)], ";\\s*"))
    theme_tbl <- table(trimws(all_themes))
    # names(table(character(0))) returns NULL, and
    # tibble(theme_name = NULL, ...) drops the column entirely --
    # which then made the downstream `pull(theme_name)` in
    # generate_report() error with "object 'theme_name' not found".
    # The bug fired in any Mode 3 run where the AI's coded constructs
    # didn't match any framework construct (so apply_framework_themes
    # produced an empty theme_set, cascade_theme_assignments left
    # emerged_themes all-NA, and aggregate_overall_statistics fell
    # into this branch with empty data). Coerce names() to
    # character(0) so the column always exists.
    theme_names_safe <- names(theme_tbl) %||% character(0)
    themes_df <- tibble(
      theme_name = theme_names_safe,
      n = as.integer(theme_tbl),
      pct = round(100 * as.integer(theme_tbl) / max(total, 1), 1)
    ) |> arrange(desc(n))
  } else {
    themes_df <- tibble(theme_name = character(), n = integer(), pct = numeric())
  }

  # Sentiment summary. Guard the column: a standalone generate_report() call or
  # a hand-edited checkpoint may lack sentiment_score (or carry it as strings).
  # Without the guard, mean()/sd() on a missing or character column raise
  # "non-numeric argument to mathematical function" and abort the report.
  ss <- if ("sentiment_score" %in% names(data)) {
    suppressWarnings(as.numeric(data$sentiment_score))
  } else {
    numeric(0)
  }
  has_sent <- any(!is.na(ss))
  sent <- list(
    mean   = if (has_sent) round(mean(ss, na.rm = TRUE), 2) else NA_real_,
    sd     = if (has_sent) round(sd(ss, na.rm = TRUE), 2) else NA_real_,
    median = if (has_sent) round(median(ss, na.rm = TRUE), 2) else NA_real_,
    pct_negative = if (has_sent) round(100 * sum(ss < -0.2, na.rm = TRUE) / max(total, 1), 1) else NA_real_,
    pct_positive = if (has_sent) round(100 * sum(ss > 0.2, na.rm = TRUE) / max(total, 1), 1) else NA_real_
  )

  # Emotion distribution (multi-label: split all_emotions and count each)
  emotions <- .count_multi_label_emotions(data)

  # Source breakdown
  source_breakdown <- if ("source_table" %in% names(data)) {
    data |>
      count(source_table) |>
      mutate(pct = round(100 * n / sum(n), 1))
  } else {
    NULL
  }

  # Coding stats
  coding <- list(
    total_unique_codes = if (!is.null(consolidated)) nrow(consolidated$codes) else 0L
  )

  # Learning context stats (expanded for transparency)
  learning <- if (!is.null(learning_context) && inherits(learning_context, "LearningContext") &&
                  learning_context$n_studies > 0) {
    list(
      n_studies = learning_context$n_studies,
      study_names = learning_context$study_names,
      context_characters = nchar(learning_context$for_coding) +
                           nchar(learning_context$for_theming) +
                           nchar(learning_context$for_review),
      coding_chars = nchar(learning_context$for_coding),
      theming_chars = nchar(learning_context$for_theming),
      review_chars = nchar(learning_context$for_review),
      reflection = learning_context$for_report %||% "",
      # Include truncated excerpts of the actual content for report transparency
      coding_excerpt = substr(learning_context$for_coding, 1, 2000),
      theming_excerpt = substr(learning_context$for_theming, 1, 2000),
      review_excerpt = substr(learning_context$for_review, 1, 2000)
    )
  } else {
    NULL
  }

  list(
    total_entries = total,
    n_themes = n_themes(theme_set),
    themes = themes_df,
    sentiment = sent,
    emotions = emotions,
    source_breakdown = source_breakdown,
    coding = coding,
    learning = learning,
    research_focus = config$study$research_focus %||% "",
    research_context = config$study$research_context %||% "",
    analysis_date = Sys.Date()
  )
}

#' Generate AI-powered executive summary and conclusion
#'
#' @param overall_stats Overall statistics list
#' @param theme_stats Per-theme statistics list
#' @param correlations_df Correlations tibble
#' @param insights Insights list
#' @param theme_set ThemeSet object
#' @param provider AIProvider object (or NULL for fallback)
#' @param config ThematicConfig (or NULL). The reflexivity_block is read from
#'   \code{config$study} and injected into the synthesis system prompt; pass
#'   NULL to omit reflexivity framing.
#' @param audit_log An optional AuditLog object. When provided, the
#'   executive-summary synthesis AI call is recorded as an \code{ai_request}
#'   audit decision with full provenance.
#' @param response_cache An optional ResponseCache object. When
#'   provided, the raw API response is written to the cache and referenced
#'   from the audit log.
#' @return List with executive_summary and conclusion strings
generate_ai_synthesis <- function(overall_stats, theme_stats, correlations_df,
                                   insights, theme_set, provider = NULL,
                                   config = NULL,
                                   audit_log = NULL,
                                   response_cache = NULL) {
  # Build summary context
  theme_lines <- vapply(names(theme_stats), function(tn) {
    ts <- theme_stats[[tn]]
    sprintf("- %s: %d entries (%.1f%%), sentiment=%.2f",
            tn, ts$n_entries, ts$pct_of_total, ts$sentiment$mean)
  }, character(1))

  n_sig <- if (!is.null(correlations_df)) sum(correlations_df$significant, na.rm = TRUE) else 0

  # Include learning context in the synthesis input
  learning_str <- ""
  if (!is.null(overall_stats$learning) && overall_stats$learning$n_studies > 0) {
    learning_str <- paste0(
      "\nInformed by ", overall_stats$learning$n_studies, " previous studies: ",
      paste(overall_stats$learning$study_names, collapse = ", "),
      " (", format(overall_stats$learning$context_characters, big.mark = ","),
      " chars of learning context)\n"
    )
  }

  context <- paste0(
    "Study: ", overall_stats$research_focus, "\n",
    "Total entries: ", overall_stats$total_entries, "\n",
    "Mean sentiment: ", overall_stats$sentiment$mean, "\n",
    learning_str,
    "Themes:\n", paste(theme_lines, collapse = "\n"), "\n",
    "Significant correlations: ", n_sig, "\n"
  )

  if (!is.null(provider)) {
    result <- tryCatch({
      reflexivity <- if (!is.null(config)) config$study$reflexivity_block %||% "" else ""
      system_prompt <- paste0(
        "Write a concise executive summary (2-3 paragraphs) and conclusion ",
        "(1-2 paragraphs) for a thematic analysis report.\n\n",
        reflexivity,
        "The executive summary should:\n",
        "1. State the research question and key findings\n",
        "2. If the analysis was informed by previous studies, briefly note how ",
        "prior analyses shaped the current one\n",
        "3. Evaluate whether the identified themes adequately capture the research focus\n",
        "4. Note any aspects of the research question that are well-addressed and any gaps\n\n",
        "Provide both fields (executive_summary, conclusion). The response ",
        "shape is enforced by the structured-output schema."
      )

      ai_result <- ai_complete(provider, context, system_prompt,
                                task = "synthesis",
                                response_schema = .synthesis_schema())
      if (!is.null(audit_log)) {
        log_ai_request(audit_log, "synthesis", ai_result, response_cache,
                        purpose = "executive_summary")
      }
      parse_json_safely(ai_result$content)
    }, error = function(e) {
      log_warn("AI synthesis failed: {e$message}")
      NULL
    })

    if (!is.null(result)) return(result)
    log_warn("AI synthesis returned NULL -- using statistical fallback")
  } else {
    log_warn("No AI provider available for synthesis -- using statistical fallback")
  }

  # Fallback: generate without AI. Guard against an NA sentiment mean (a
  # zero-coded / zero-theme corpus, or a run with no sentiment step): a bare
  # `if (NA < -0.2)` would raise "missing value where TRUE/FALSE needed".
  sm <- overall_stats$sentiment$mean
  sentiment_phrase <- if (is.na(sm)) "not scored"
                      else if (sm < -0.2) "predominantly negative"
                      else if (sm > 0.2) "predominantly positive"
                      else "mixed"
  sentiment_detail <- if (is.na(sm)) {
    "."
  } else {
    paste0(" (M = ", sm, ", SD = ", overall_stats$sentiment$sd, "), with ",
           overall_stats$sentiment$pct_negative,
           "% of entries showing negative sentiment.")
  }
  list(
    executive_summary = paste0(
      "This thematic analysis examined ", overall_stats$total_entries,
      " entries, identifying ", overall_stats$n_themes,
      " distinct themes. The overall sentiment was ",
      sentiment_phrase, sentiment_detail
    ),
    conclusion = paste0(
      "The analysis identified ", overall_stats$n_themes,
      " themes with ", n_sig,
      " significant correlations, providing insight into the research focus: ",
      overall_stats$research_focus, "."
    )
  )
}

#' Interpret correlation results for reporting
#'
#' @param correlations_df Correlations tibble
#' @param theme_stats Per-theme statistics list
#' @return List with summary text
interpret_correlations <- function(correlations_df, theme_stats) {
  if (is.null(correlations_df) || nrow(correlations_df) == 0) {
    return(list(summary = "No correlations were computed for this analysis."))
  }

  n_total <- nrow(correlations_df)

  # Detect whether this is a newer result (has meaningful_effect + p_raw)
  # or a legacy / fixture-generated result (no meaningful_effect column).
  has_meaningful <- "meaningful_effect" %in% names(correlations_df)
  has_p_raw <- "p_raw" %in% names(correlations_df)

  # Headline summary: effect-size-first framing for the new path,
  # back-compat phrasing for the legacy path.
  if (has_meaningful) {
    n_meaningful <- sum(correlations_df$meaningful_effect, na.rm = TRUE)
    n_sig <- sum(correlations_df$significant, na.rm = TRUE)
    # the publication-relevant headline is the
    # intersection -- pairs that are BOTH statistically distinguishable
    # AND large enough to matter substantively. An earlier report
    # quoted just `significant` (Bonferroni), which on a 5,000-entry
    # corpus passes hundreds of trivial-magnitude pairs (|r| < 0.10)
    # along with the substantive ones. The intersection number is the
    # one a journal reviewer cares about; the standalone counts remain
    # in the methodology appendix for full transparency.
    n_meaningful_and_sig <- sum(
      correlations_df$meaningful_effect & correlations_df$significant,
      na.rm = TRUE
    )

    summary_text <- paste0(
      "Of ", n_total, " exploratory associations examined, **",
      n_meaningful_and_sig, "** are both statistically significant after ",
      "Bonferroni adjustment (alpha = 0.05) AND have a meaningful effect ",
      "size (|r| >= 0.10, Cohen's small-effect threshold). For full ",
      "transparency: ", n_meaningful, " pairs cross the effect-size ",
      "threshold and ", n_sig, " pairs cross the significance threshold; ",
      "the intersection above is the publication-relevant subset.\n\n",
      "_These associations are exploratory: themes were inductively derived ",
      "from the same data the correlations are computed on, so p-values are ",
      "best read as descriptive diagnostics rather than confirmatory tests ",
      "(cf. Rothman 1990, Epidemiology 1(1):43-46). Effect sizes (Cohen's r ",
      "conventions: 0.10 small, 0.30 medium, 0.50 large) and 95% confidence ",
      "intervals are the primary inferential signals. P-values under three ",
      "regimes (raw, Benjamini-Hochberg FDR, Bonferroni FWER) are reported ",
      "in the full table for transparency._"
    )

    pool <- correlations_df |>
      filter(meaningful_effect & significant) |>
      arrange(desc(abs(correlation)))
    label <- "The strongest associations (meaningful effect AND Bonferroni-significant):"
  } else {
    n_sig <- sum(correlations_df$significant, na.rm = TRUE)
    summary_text <- paste0(
      "Of ", n_total, " correlation pairs tested, **", n_sig,
      "** were statistically significant after Bonferroni adjustment (p < 0.05)."
    )
    pool <- correlations_df |>
      filter(significant) |>
      arrange(desc(abs(correlation)))
    label <- "The strongest correlations were:"
  }

  if (nrow(pool) > 0) {
    top <- pool |> head(3)
    top_descriptions <- vapply(seq_len(nrow(top)), function(i) {
      r <- top[i, ]
      direction <- if (r$correlation > 0) "positive" else "negative"
      v1 <- gsub("theme_membership_", "", r$var1)
      v2 <- gsub("theme_membership_", "", r$var2)
      v1 <- tools::toTitleCase(gsub("[_.]", " ", v1))
      v2 <- tools::toTitleCase(gsub("[_.]", " ", v2))

      # 95% CI when available
      ci_text <- if (!is.null(r$ci_lower) && !is.na(r$ci_lower) &&
                     !is.null(r$ci_upper) && !is.na(r$ci_upper)) {
        sprintf(" [%.3f, %.3f]", r$ci_lower, r$ci_upper)
      } else {
        ""
      }

      # Tiered p-values for newer results; single p for legacy
      p_text <- if (has_p_raw) {
        sprintf(" (p_raw = %.3f, p_BH = %.3f, p_Bonf = %.3f)",
                r$p_raw, r$p_bh, r$p_bonferroni)
      } else {
        sprintf(" (p = %.3f)", r$p_value)
      }

      sprintf("**%s** and **%s** -- r = %.3f%s, %s %s effect%s",
              v1, v2, r$correlation, ci_text, direction, r$effect_size, p_text)
    }, character(1))

    summary_text <- paste0(
      summary_text, "\n\n", label, "\n\n",
      paste0("- ", top_descriptions, collapse = "\n")
    )
  }

  list(summary = summary_text)
}

#' Get interpretation text for an emotion
#'
#' @param emotion Emotion name string
#' @return Interpretation string
get_emotion_interpretation <- function(emotion) {
  interpretations <- list(
    sadness = "reflects grief, loss, or emotional pain within the community",
    anger = "indicates frustration, resentment, or perceived injustice",
    fear = "suggests anxiety, worry, or concern about outcomes",
    disgust = "reveals aversion or strong negative reactions",
    joy = "represents positive experiences, hope, or satisfaction",
    surprise = "indicates unexpected findings or reactions",
    trust = "reflects confidence, reliability, or faith in processes",
    anticipation = "suggests forward-looking expectations or planning",
    frustration = "indicates ongoing difficulty or dissatisfaction with circumstances",
    anxiety = "reflects worry, unease, or nervousness about situations",
    hope = "suggests optimism or positive expectations for the future",
    shame = "indicates self-directed negative emotions or stigma",
    guilt = "reflects self-blame or moral distress",
    confusion = "suggests uncertainty or difficulty understanding situations",
    resignation = "indicates acceptance or giving up on change",
    relief = "suggests release from distress or positive resolution",
    gratitude = "reflects thankfulness for support or improvement",
    empathy = "indicates understanding of others' experiences"
  )

  interpretations[[tolower(emotion)]] %||%
    paste0("reflects ", tolower(emotion), "-related emotional responses")
}

#' Generate downloads appendix section
#'
#' @param export_files List of export file paths
#' @param theme_stats Per-theme statistics list
#' @return R Markdown string
generate_downloads_section <- function(export_files, theme_stats) {
  content <- paste0(
    "# Appendix C: Data Downloads {#data-downloads}\n\n",
    "All analysis outputs are available for download:\n\n",
    "| File | Description |\n",
    "|------|-------------|\n"
  )

  if (!is.null(export_files$sentiment_file)) {
    content <- paste0(content,
      "| [Sentiment Scores](", basename(export_files$sentiment_file),
      ") | Sentiment and emotion data for all entries |\n"
    )
  }

  if (!is.null(export_files$codes_file)) {
    content <- paste0(content,
      "| [Consolidated Codes](", basename(export_files$codes_file),
      ") | All codes with frequencies and types |\n"
    )
  }

  if (!is.null(export_files$correlations_file)) {
    content <- paste0(content,
      "| [Correlations](", basename(export_files$correlations_file),
      ") | All correlation pairs with p-values (circular/analyst-internal pairs retained but flagged `excluded_from_findings`) |\n"
    )
  }

  if (!is.null(export_files$themes_file)) {
    content <- paste0(content,
      "| [Themes](", basename(export_files$themes_file),
      ") | Theme definitions and metadata |\n"
    )
  }

  # Per-theme CSV files
  if (!is.null(export_files$theme_csv_files)) {
    for (tn in names(export_files$theme_csv_files)) {
      info <- export_files$theme_csv_files[[tn]]
      n_entries <- theme_stats[[tn]]$n_entries %||% "?"
      content <- paste0(content,
        "| [", .html_esc(tn), "](", info$relative_path,
        ") | ", n_entries, " entries for this theme |\n"
      )
    }
  }

  paste0(content, "\n")
}

# ==============================================================================
# Internal helpers
# ==============================================================================

#' Select representative quotes spanning sentiment range
#' @keywords internal
.select_representative_quotes <- function(entries, n_quotes = 3) {
  if (nrow(entries) == 0) return(list())

  text_col <- if ("original_text" %in% names(entries)) "original_text" else "std_text"
  valid <- entries |>
    filter(!is.na(.data[[text_col]]), nchar(.data[[text_col]]) > 50)

  if (nrow(valid) == 0) return(list())

  # Analytic fit: representative quotes should be drawn from entries
  # CHARACTERISTIC of this theme, not from diffuse posts coded into many themes. A
  # heavily multi-coded extreme-sentiment post is the "most negative" (or "most
  # positive") member of MANY themes at once, so pure sentiment-extremity selection
  # makes the SAME diffuse post the lead quote for several themes -- where it
  # illustrates none of them well (verified: one "I feel like giving up" post led 5
  # of 9 themes, including a Recovery theme). When theme-membership breadth is
  # available, restrict the candidate pool to the more theme-specific entries (at or
  # below median breadth) BEFORE sentiment-positioning -- but only when that still
  # leaves >= 3 rows to span the sentiment range. Multi-coding is fully preserved in
  # the theme's membership + counts; this changes only which entries become the
  # representative QUOTES. Sentiment labels stay honest (relative to the theme's
  # characteristic entries, which is what the quotes are meant to illustrate),
  # author-spread (T0.2) and provenance (T0.1) are untouched, and it falls back to
  # all entries when breadth is unavailable or too few specific entries exist.
  tm_cols <- grep("^theme_membership_", names(valid), value = TRUE)
  if (length(tm_cols) >= 2L) {
    breadth <- rowSums(as.matrix(valid[, tm_cols, drop = FALSE]) == 1L, na.rm = TRUE)
    if (any(breadth > 0L)) {
      thresh <- stats::median(breadth[breadth > 0L])
      specific <- valid[breadth > 0L & breadth <= thresh, , drop = FALSE]
      if (nrow(specific) >= 3L) valid <- specific
    }
  }

  valid <- valid |> arrange(sentiment_score)
  has_authors <- "std_author" %in% names(valid)
  n_valid <- nrow(valid)

  # Sentiment-positioned target slots (most negative, median, most positive).
  # NULL means "this slot can't be filled at this n_valid."
  targets <- list(
    most_negative = if (n_valid >= 1L) 1L else NULL,
    median        = if (n_valid >= 3L) ceiling(n_valid / 2L) else NULL,
    most_positive = if (n_valid >= 3L) n_valid else if (n_valid >= 2L) 2L else NULL
  )

  quotes <- list()
  taken_indices <- integer(0)
  taken_authors <- character(0)

  for (label in names(targets)) {
    target_idx <- targets[[label]]
    if (is.null(target_idx)) next

    # T0.2 spread-aware selection: prefer a row whose author is not already
    # represented in this theme's quotes. Search expands outward from the
    # target sentiment position so it keeps the "most negative / median /
    # most positive" framing as close as possible while diversifying. Falls
    # back to target_idx when no alternative exists (single-contributor
    # theme, no author data, all unique authors already taken).
    chosen_idx <- .pick_quote_with_spread(
      valid_df       = valid,
      target_idx     = target_idx,
      taken_indices  = taken_indices,
      taken_authors  = taken_authors,
      has_authors    = has_authors
    )

    q <- valid[chosen_idx, ]
    # Guard against missing all_emotions column -- some pipelines (test
    # fixtures, sentiment-failure paths) skip emotion analysis. Direct
    # `q$all_emotions` triggers an "Unknown or uninitialised column"
    # warning when the column doesn't exist; use names()-check instead.
    emotion_val <- if ("all_emotions" %in% names(q)) {
      q$all_emotions %||% NA_character_
    } else NA_character_
    # record entry_id + source_table +
    # author alongside text + sentiment so a downstream consumer can
    # reconstruct the link from quote-text back to its source entry.
    # An earlier version had this as a bare text string -- T0.2 provenance was
    # partially missing because the renderer couldn't trace a published
    # quote back to its row in the data tibble.
    entry_id_val <- if ("std_id" %in% names(q)) {
      as.character(q$std_id %||% NA_character_)
    } else NA_character_
    source_table_val <- if ("source_table" %in% names(q)) {
      as.character(q$source_table %||% NA_character_)
    } else NA_character_
    author_val <- if (has_authors) {
      as.character(q$std_author %||% NA_character_)
    } else NA_character_
    quotes[[label]] <- list(
      text         = truncate_text(q[[text_col]], 300),
      sentiment    = round(q$sentiment_score, 2),
      emotion      = emotion_val,
      entry_id     = entry_id_val,
      source_table = source_table_val,
      author       = author_val
    )
    taken_indices <- c(taken_indices, chosen_idx)
    if (has_authors) {
      a <- q$std_author
      if (!is.null(a) && !is.na(a) && nzchar(as.character(a))) {
        taken_authors <- c(taken_authors, as.character(a))
      }
    }
  }

  quotes
}

#' Pick a row near a sentiment-target index, preferring a new contributor
#'
#' Search order: target_idx, then expanding outward (target-1, target+1,
#' target-2, target+2, ...) until finding a row that (a) hasn't been taken
#' already by index, AND (b) is from an author not yet represented (or has
#' no author data, which is treated as "neutral" and accepted). When no winner
#' is found, falls back to target_idx so the caller still gets SOMETHING --
#' single-contributor or no-author-data themes degrade to the original
#' behavior. This is the per-slot half of T0.2 spread-aware quote selection.
#' @keywords internal
.pick_quote_with_spread <- function(valid_df, target_idx, taken_indices,
                                     taken_authors, has_authors) {
  n <- nrow(valid_df)
  if (target_idx < 1L || target_idx > n) return(target_idx)

  # When there's no author data, take the first non-taken-by-index
  # row (preserves the old behavior for datasets without author columns).
  is_acceptable <- function(idx) {
    if (idx %in% taken_indices) return(FALSE)
    if (!has_authors) return(TRUE)
    a <- valid_df$std_author[idx]
    if (is.null(a) || is.na(a) || !nzchar(as.character(a))) return(TRUE)
    !(as.character(a) %in% taken_authors)
  }

  # Target first, then expanding outward in alternating directions.
  if (is_acceptable(target_idx)) return(target_idx)
  for (offset in seq_len(n)) {
    for (direction in c(-1L, 1L)) {
      cand <- target_idx + direction * offset
      if (cand < 1L || cand > n) next
      if (is_acceptable(cand)) return(cand)
    }
  }
  # Nothing acceptable -- fallback to target_idx (single-contributor theme,
  # all authors already taken). Caller will end up with a duplicate-author
  # quote, which is the natural outcome when spread is genuinely unavailable.
  target_idx
}

#' Count emotion occurrences across multi-label all_emotions column
#'
#' Splits semicolon-separated all_emotions values and counts each emotion.
#' An entry expressing "sadness; anger" contributes one count to each.
#' @param entries tibble with all_emotions column
#' @return tibble with emotion, n, pct columns
#' @keywords internal
.count_multi_label_emotions <- function(entries) {
  n <- nrow(entries)
  if (n == 0) return(tibble::tibble(emotion = character(), n = integer(), pct = numeric()))

  if ("all_emotions" %in% names(entries)) {
    raw <- entries$all_emotions[!is.na(entries$all_emotions)]
  } else {
    return(tibble::tibble(emotion = character(), n = integer(), pct = numeric()))
  }

  if (length(raw) == 0) {
    return(tibble::tibble(emotion = character(), n = integer(), pct = numeric()))
  }

  # Split on semicolons, trim, and count
  all_labels <- unlist(strsplit(raw, ";\\s*"))
  all_labels <- trimws(all_labels)
  all_labels <- all_labels[nchar(all_labels) > 0]

  if (length(all_labels) == 0) {
    return(tibble::tibble(emotion = character(), n = integer(), pct = numeric()))
  }

  tbl <- sort(table(all_labels), decreasing = TRUE)
  # Percentages are of ALL entries in scope (n = nrow(entries)), not just the
  # entries that carry an emotion label -- this matches the adjacent sentiment
  # percentages (which use all entries) and avoids inflating prevalence when a
  # subset of entries went unlabelled. One entry can contribute to several
  # emotions, so columns need not sum to 100.
  tibble::tibble(
    emotion = names(tbl),
    n = as.integer(tbl),
    pct = round(100 * as.integer(tbl) / max(n, 1), 1)
  )
}

#' Count pre-rejection fabrications for the T0.1 dashboard (V-5 helper)
#'
#' Counts fabrication-log
#' entries using readr's RFC-4180 parser instead of `readLines() +
#' length(lines) - 1L`. Coded segments routinely contain newlines
#' (Reddit posts), and the FabricationLog writes the exact_text field
#' as RFC-4180 quoted-with-embedded-newlines via .csv_quote
#' (R/quote_provenance.R:861). The pre-fix line-counting approach
#' counted a single 3-line fabricated quote as 3 fabrications.
#'
#' @param fabrication_log_path Absolute path to fabrication_log.csv, or NULL.
#' @param n_fabricated_caught Explicit override (e.g. from
#'   FabricationLog$state$n_logged); takes priority when supplied.
#' @return Integer N caught, or NULL if neither source is available.
#' @keywords internal
.count_pre_rejection_fabrications <- function(fabrication_log_path = NULL,
                                                n_fabricated_caught = NULL) {
  if (!is.null(n_fabricated_caught)) return(as.integer(n_fabricated_caught))
  if (is.null(fabrication_log_path)) return(NULL)
  if (!file.exists(fabrication_log_path)) return(NULL)
  tryCatch({
    # readr handles the methodology comment-header lines + RFC-4180
    # quoted multi-line fields correctly. NULL show_col_types silences
    # the column-type message.
    df <- readr::read_csv(fabrication_log_path,
                          comment = "#",
                          show_col_types = FALSE)
    as.integer(nrow(df))
  }, error = function(e) NULL)
}

# ==============================================================================
# Tier-0 Data Integrity Dashboard
# ==============================================================================

#' Build the Tier-0 data integrity dashboard markdown for the report
#'
#' Renders a markdown card that summarizes the run's anti-fabrication
#' verification work: how many AI-attributed verbatim claims were checked,
#' how many verified exactly vs fuzzy (with method breakdown), how many
#' fabrications were dropped, and a relative path link to the
#' \code{fabrication_log.csv} when fabrications occurred.
#'
#' This dashboard is the user-visible artifact of T0.1's universal
#' verification contract -- it makes the package's anti-fabrication work
#' empirically inspectable from the report itself, addressing the
#' transparency dimension of Jowsey et al. 2025's critique. When no
#' verification was run (pre-T0.1 runs, or runs that skipped coding), the
#' dashboard renders a "Verification not available" notice rather than
#' silently omitting -- absence of the badge would be its own integrity
#' signal that should not be sent.
#'
#' @param stats Named list returned by
#'   \code{\link{compute_quote_provenance_stats}}.
#' @param fabrication_log_relpath Optional relative path to
#'   \code{fabrication_log.csv} (relative to the report HTML's directory).
#'   When NULL or no fabrications occurred, no link is rendered.
#' @param config ThematicConfig object (or NULL) used for the
#'   Citations API bypass footnote.
#' @param fabrication_log_path absolute path to
#'   \code{fabrication_log.csv}. When supplied, the dashboard counts
#'   pre-rejection fabrications from this file (the surviving
#'   population in \code{stats} is post-rejection, so it always
#'   reports 0 caught fabrications by itself). Pass \code{NULL} on
#'   legacy callers that don't have the path; the dashboard falls
#'   back to the surviving-population count.
#' @param n_fabricated_caught explicit count of
#'   pre-rejection fabrications (from
#'   \code{FabricationLog$state$n_logged}). Overrides
#'   \code{fabrication_log_path} when supplied. Pass \code{NULL} to
#'   skip.
#' @return A character string of markdown content (one card).
#' @keywords internal
.build_tier0_dashboard <- function(stats,
                                    fabrication_log_relpath = "fabrication_log.csv",
                                    config = NULL,
                                    fabrication_log_path = NULL,
                                    n_fabricated_caught = NULL) {
  # When fabrications were caught
  # AND every attribution was dropped (stats$total == 0L because the
  # surviving population is empty), the pre-fix early-return rendered
  # "verification did not run" -- a strictly worse lie than V-5's
  # "no fabrications detected" because it implies the defense never
  # fired at all. Compute the pre-rejection count FIRST; only fall
  # through to the empty-stats branch when BOTH stats and the fab log
  # are empty.
  pre_caught <- .count_pre_rejection_fabrications(
    fabrication_log_path = fabrication_log_path,
    n_fabricated_caught  = n_fabricated_caught
  )
  if ((is.null(stats) || identical(stats$total, 0L)) &&
      (is.null(pre_caught) || pre_caught == 0L)) {
    # Even on the empty-stats path the dashboard
    # still renders the Mode-3-bypass footnote when applicable, so a
    # reviewer reading the dashboard for a Mode 3 + Anthropic run with
    # zero verbatim claims sees the architectural reason for the
    # absence (the Citations API is precluded by the tool_use schema)
    # rather than just "verification did not run."
    return(paste0(
      '<div class="tier0-dashboard tier0-empty">\n\n',
      '## Data Integrity Dashboard (T0.1)\n\n',
      'Quote-provenance verification did not run for this report ',
      '(pre-T0.1 run, no coding step, or no AI-attributed verbatim claims). ',
      'Future runs of this study will include a verification dashboard here.\n\n',
      .tier0_citations_api_bypass_footnote(config),
      '</div>\n\n'
    ))
  }
  # When stats$total is 0 but fabrications WERE caught, render an
  # explicit "all attempts dropped" dashboard with the caught count.
  if (is.null(stats) || identical(stats$total, 0L)) {
    return(paste0(
      '<div class="tier0-dashboard">\n\n',
      '## Data Integrity Dashboard (T0.1)\n\n',
      '**', pre_caught, '** fabricated quote attribution',
      if (pre_caught == 1L) ' was' else 's were',
      ' CAUGHT by the verification ladder and DROPPED from the codebook ',
      '(100% pre-rejection fabrication rate; 0 surviving verbatim claims). ',
      'See [fabrication_log.csv](', fabrication_log_relpath,
      ') for per-fabrication audit detail.\n\n',
      .tier0_citations_api_bypass_footnote(config),
      '</div>\n\n'
    ))
  }

  total <- stats$total
  by_status <- stats$by_status
  by_method <- stats$by_method
  # Safe accessor: by_status[["key"]] errors on missing names; single-bracket
  # subset returns NA, so coerce that to 0L.
  count_or_zero <- function(vec, key) {
    if (key %in% names(vec)) as.integer(vec[[key]]) else 0L
  }
  n_exact      <- count_or_zero(by_status, "verified_exact")
  n_fuzzy      <- count_or_zero(by_status, "verified_fuzzy")
  n_fabricated <- count_or_zero(by_status, "fabricated")
  n_drifted    <- count_or_zero(by_status, "drifted")
  n_verified   <- n_exact + n_fuzzy

  pct_verified <- 100 * n_verified / max(total, 1)
  pct_fabricated <- 100 * n_fabricated / max(total, 1)

  # Method breakdown line for the fuzzy class -- which ladder steps recovered
  # how many quotes
  method_str <- if (length(by_method) > 0) {
    methods <- names(by_method)
    counts  <- as.integer(by_method)
    paste(
      paste0(methods, " = ", counts),
      collapse = ", "
    )
  } else {
    "n/a"
  }

  # compute PRE-rejection fabrication count via
  # the shared helper. See .count_pre_rejection_fabrications below.
  n_caught_resolved <- .count_pre_rejection_fabrications(
    fabrication_log_path = fabrication_log_path,
    n_fabricated_caught  = n_fabricated_caught
  )
  n_caught <- n_caught_resolved %||% n_fabricated

  # Fabrication line: only render the CSV link when there ARE fabrications
  # (most runs will have zero -- that's the goal). Path is relative because
  # the report HTML and the CSV both live in the same run output directory.
  # Survivor count uses n_verified
  # (exact + fuzzy), NOT total. `total` includes drifted + unverified
  # entries which are NOT verified-against-source -- claiming them as
  # "verified" was a small instance of the same lie V-5 fixed.
  fab_line <- if (n_caught > 0L) {
    rate_pct <- round(100 * n_caught / max(n_caught + total, 1L), 2)
    paste0(
      "**", n_caught, "** fabricated quote attribution",
      if (n_caught == 1L) " was" else "s were",
      " CAUGHT by the verification ladder and DROPPED from the codebook ",
      "(", rate_pct, "% pre-rejection fabrication rate; **", n_verified,
      "** surviving verbatim claims verified against the analytic source text",
      if (n_drifted + (total - n_verified - n_drifted) > 0L) {
        paste0("; ", total - n_verified, " of the ", total, " survivors ",
                "are drifted or pending review and NOT counted as verified")
      } else "",
      "). ",
      "See [fabrication_log.csv](", fabrication_log_relpath,
      ") for per-fabrication audit detail.\n\n"
    )
  } else {
    if (n_verified == total) {
      paste0(
        "**No fabrications detected.** Every one of the **", total, "** ",
        "AI-attributed verbatim claims verified against the analytic ",
        "source text (the verification ladder did not need to drop any).\n\n"
      )
    } else {
      paste0(
        "**No fabrications detected.** Of the **", total, "** AI-attributed ",
        "verbatim claims, **", n_verified, "** verified against the ",
        "analytic source text; the remaining ", total - n_verified, " are drifted or ",
        "pending review (see drift section below) and NOT counted as ",
        "verified.\n\n"
      )
    }
  }

  # Same noun-verb agreement for drifted line
  drift_line <- if (n_drifted > 0) {
    paste0(
      "**", n_drifted, "** quote",
      if (n_drifted == 1L) " was" else "s were",
      " marked **drifted** -- the source document SHA-256 differed ",
      "between attribution time and verification time, suggesting the ",
      "underlying data was edited between runs. Drifted quotes are ",
      "excluded from rendering pending researcher review.\n\n"
    )
  } else {
    ""
  }

  # Citation-source breakdown: distinguishes the PREVENTION layer (Anthropic
  # Citations API: server-side-grounded quote spans) from the DETECTION-only
  # layer (model_freeform: model wrote a verbatim claim, ladder verified
  # offline). Both are admissible into the codebook; citations are strictly
  # stronger evidence. The dashboard makes the distinction visible.
  source_block <- .build_tier0_source_block(stats, config = config)

  paste0(
    '<div class="tier0-dashboard">\n\n',
    '## Data Integrity Dashboard (T0.1)\n\n',
    'This run subjected every AI-attributed verbatim claim to a ',
    'verification ladder (strict string match -> normalized match -> substring ',
    'search, plus an optional embedding-similarity step when an embedding ',
    'provider is configured) before admitting it into the codebook. ',
    'For Anthropic-provider runs, the verification ladder runs on top of the ',
    '**Anthropic Citations API** -- which is the package\'s prevention layer ',
    '(model returns server-side-guaranteed offsets into source documents ',
    'instead of free-form quotes). The two layers compose: the API prevents ',
    'fabrications, the ladder catches anything the API misses (corpus drift, ',
    'encoding issues). Together they address the Jowsey et al. 2025 critique ',
    '(doi:10.1371/journal.pone.0330217) that LLM-for-thematic-analysis tools ',
    'cannot be trusted to refrain from fabricating Frankenstein quotes that ',
    'look verbatim but exist in no source.\n\n',
    '**', total, '** AI-attributed verbatim claim',
    if (total == 1L) " was" else "s were",
    ' checked.\n\n',
    '- **', n_verified, '** verified (',
    sprintf("%.1f%%", pct_verified), ') -- ',
    n_exact, ' exact + ', n_fuzzy, ' fuzzy.\n',
    '- Verification methods: ', method_str, '.\n\n',
    source_block,
    fab_line,
    drift_line,
    'See `R/quote_provenance.R` in the package source for the ',
    'verification-ladder implementation and the per-step threshold ',
    'rationale (documented in the ladder constants and `verify_quote`).\n\n',
    '</div>\n\n'
  )
}

#' Render the citation-source breakdown sub-block for the Tier-0 dashboard
#'
#' Distinguishes Anthropic Citations API quotes (PREVENTION layer:
#' server-side-grounded offsets) from model_freeform quotes (DETECTION-only
#' layer: model wrote a verbatim claim, ladder verified offline). Renders
#' the per-source count and per-source verification rate so the dashboard
#' shows both reliability dimensions at once.
#'
#' Returns "" (empty string) when there are no citation_source values
#' (degenerate state -- shouldn't happen for normal runs but the dashboard
#' should not crash on an unusual stats object).
#' @keywords internal
.build_tier0_source_block <- function(stats, config = NULL) {
  by_source <- stats$by_citation_source
  if (length(by_source) == 0L) {
    # When the source breakdown has nothing to show, fall through to
    # only the Mode-3-bypass footnote (or empty when not Mode 3 +
    # Anthropic). Empty-source state is fine on its own; the footnote
    # explains the deliberate architectural constraint when applicable.
    return(.tier0_citations_api_bypass_footnote(config))
  }

  rate_by_source <- stats$verification_rate_by_source %||%
                      stats::setNames(numeric(0), character(0))

  # Display order: citations API first (it's the headline win), then any
  # other sources alphabetically. This makes the dashboard's "what
  # happened" story read top-to-bottom: best evidence first.
  source_order <- c(
    "anthropic_citations_api",
    sort(setdiff(names(by_source), "anthropic_citations_api"))
  )
  source_order <- intersect(source_order, names(by_source))

  pretty_label <- function(s) {
    switch(s,
      "anthropic_citations_api" = "Anthropic Citations API (prevention + detection)",
      "model_freeform"          = "Model freeform (detection only)",
      "human_supplied"          = "Human-supplied",
      "pipeline_derived"        = "Pipeline-derived",
      s
    )
  }

  total <- sum(by_source)

  lines <- vapply(source_order, function(s) {
    n_src <- as.integer(by_source[[s]])
    pct_src <- 100 * n_src / max(total, 1)
    rate_src <- rate_by_source[[s]] %||% NA_real_
    rate_str <- if (is.na(rate_src)) "rate n/a" else
                sprintf("%.1f%% verified", 100 * rate_src)
    sprintf("- **%s**: %d (%.1f%%) -- %s",
            pretty_label(s), n_src, pct_src, rate_str)
  }, character(1))

  paste0(
    "**Citation source breakdown:**\n",
    paste(lines, collapse = "\n"),
    "\n\n",
    .tier0_citations_api_bypass_footnote(config)
  )
}

#' Footnote explaining the Mode 3 + Anthropic Citations API silent bypass
#'
#' When \code{config$methodology$mode}
#' is \code{"framework_applied"} (Mode 3) AND
#' \code{config$ai$provider} is \code{"anthropic"}, the Citations API
#' path in \code{R/02_ai_providers.R} is deliberately dropped. The
#' constraint is structural at the Anthropic API level: forced
#' \code{tool_use} schema (which Mode 3 requires to constrain coding
#' to framework constructs) and the Citations API output format are
#' mutually exclusive on the same response. The Mode 3 coding pipeline
#' therefore relies on the verification ladder's DETECTION-only path
#' (model_freeform + offline string match) instead of the API's
#' PREVENTION layer.
#'
#' Without this footnote, a reviewer reading the Tier-0 dashboard for
#' a Mode 3 + Anthropic run would see only \emph{"Model freeform
#' (detection only)"} and reasonably wonder why the Anthropic-specific
#' prevention layer is missing -- they could infer a bug rather than a
#' deliberate architectural constraint. The footnote makes the
#' architectural reason explicit.
#'
#' Returns "" when the trigger condition does not apply (Mode 1 / Mode
#' 2 runs, or non-Anthropic providers, or NULL config).
#' @keywords internal
.tier0_citations_api_bypass_footnote <- function(config = NULL) {
  if (is.null(config)) return("")
  meth_mode <- .config_methodology_mode(config)
  provider  <- tryCatch(config$ai$provider,      error = function(e) NULL)
  if (!identical(meth_mode, "framework_applied")) return("")
  if (!identical(provider, "anthropic")) return("")

  paste0(
    '<div class="tier0-footnote tier0-mode3-citations-bypass">\n',
    '<strong>Note (Mode 3 + Anthropic):</strong> the Citations API ',
    '(prevention layer) is <em>structurally precluded</em> in this run. ',
    'Mode 3 (Framework Applied) forces a <code>tool_use</code> response ',
    'schema so the model can only return constructs from the framework ',
    '(plus an <code>anomaly</code> bucket). The Anthropic Citations API ',
    'output format is mutually exclusive with forced <code>tool_use</code> ',
    'on the same response, so the API silently drops citation spans for ',
    'Mode 3 calls. The Mode 3 pipeline therefore relies on the ',
    'verification ladder (detection-only) for quote provenance -- the ',
    'same ladder Modes 1 and 2 use as a backstop, but here it is the ',
    'sole layer rather than a backstop. Future phases may explore a ',
    'hybrid schema (constrained constructs + paired citation offsets) ',
    'as a research spike. For now this footnote is the honest disclosure ',
    'rather than silent omission.\n',
    '</div>\n\n'
  )
}
