# ==============================================================================
# Report Helper Functions -- Statistics Aggregation & AI Synthesis
# ==============================================================================
# These functions were undefined in the old script (Bug #8). Now implemented.
# ==============================================================================

#' Aggregate per-theme statistics for report
#'
#' @param data tibble with theme_membership_* or emerged_themes columns
#' @param theme_set ThemeSet object
#' @param consolidated ConsolidatedCodes list (or NULL)
#' @return Named list of theme stats (one per theme)
#' @export
aggregate_theme_statistics <- function(data, theme_set, consolidated = NULL) {
  validate_class(theme_set, "ThemeSet")

  theme_stats <- list()
  total <- nrow(data)

  for (t in theme_set$themes) {
    tn <- t$name
    # Use multi-label membership columns to find all entries belonging to this theme
    safe_col <- paste0("theme_membership_", make.names(tn))
    if (safe_col %in% names(data)) {
      entries <- data[data[[safe_col]] == 1L, ]
    } else if ("emerged_themes" %in% names(data)) {
      entries <- data[!is.na(data$emerged_themes) &
                       grepl(tn, data$emerged_themes, fixed = TRUE), ]
    } else {
      entries <- data[0, ]
    }
    n <- nrow(entries)

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
        keywords = t$keywords %||% character(0),
        subthemes = t$subthemes %||% character(0),
        subthemes_structured = t$subthemes_structured,
        prevalence = t$prevalence %||% "unknown",
        quotes_with_context = list()
      )
      next
    }

    # Sentiment stats
    sent <- list(
      mean = round(mean(entries$sentiment_score, na.rm = TRUE), 2),
      sd = round(sd(entries$sentiment_score, na.rm = TRUE), 2),
      median = round(median(entries$sentiment_score, na.rm = TRUE), 2),
      pct_negative = round(100 * sum(entries$sentiment_score < -0.2, na.rm = TRUE) / max(n, 1), 1),
      pct_positive = round(100 * sum(entries$sentiment_score > 0.2, na.rm = TRUE) / max(n, 1), 1)
    )

    # Intensity stats
    intensity <- list(
      mean = round(mean(entries$emotion_intensity, na.rm = TRUE), 2),
      sd = round(sd(entries$emotion_intensity, na.rm = TRUE), 2)
    )

    # Emotion distribution (multi-label: split all_emotions and count each)
    emotions <- .count_multi_label_emotions(entries)

    # Representative quotes — prefer excerpt-based quotes from enriched ThemeSet
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
      quotes <- list()
      for (qi in seq_along(enriched_quotes)) {
        idx <- if (qi <= length(meta_indices)) meta_indices[qi] else 1
        idx <- min(idx, n_sorted)
        matching_row <- sent_sorted[idx, ]
        quotes[[quote_labels[qi]]] <- list(
          text = enriched_quotes[qi],
          sentiment = round(matching_row$sentiment_score, 2),
          emotion = matching_row$all_emotions %||% NA_character_
        )
      }
    } else {
      quotes <- .select_representative_quotes(entries, n_quotes = 3)
    }

    theme_stats[[tn]] <- list(
      name = tn,
      description = t$description %||% "",
      n_entries = n,
      pct_of_total = round(100 * n / max(total, 1), 1),
      sentiment = sent,
      intensity = intensity,
      emotions = emotions,
      keywords = t$keywords %||% t$codes_included[seq_len(min(5, length(t$codes_included)))],
      subthemes = t$subthemes %||% character(0),
      subthemes_structured = t$subthemes_structured,
      prevalence = t$prevalence %||% "unknown",
      quotes_with_context = quotes
    )
  }

  theme_stats
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

  # Theme distribution — count from multi-label membership columns
  membership_cols <- grep("^theme_membership_", names(data), value = TRUE)
  if (length(membership_cols) > 0) {
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
    themes_df <- tibble(
      theme_name = names(theme_tbl),
      n = as.integer(theme_tbl),
      pct = round(100 * as.integer(theme_tbl) / max(total, 1), 1)
    ) |> arrange(desc(n))
  } else {
    themes_df <- tibble(theme_name = character(), n = integer(), pct = numeric())
  }

  # Sentiment summary
  sent <- list(
    mean = round(mean(data$sentiment_score, na.rm = TRUE), 2),
    sd = round(sd(data$sentiment_score, na.rm = TRUE), 2),
    median = round(median(data$sentiment_score, na.rm = TRUE), 2),
    pct_negative = round(100 * sum(data$sentiment_score < -0.2, na.rm = TRUE) / max(total, 1), 1),
    pct_positive = round(100 * sum(data$sentiment_score > 0.2, na.rm = TRUE) / max(total, 1), 1)
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
#' @param audit_log An optional AuditLog object (T1.4). When provided, the
#'   executive-summary synthesis AI call is recorded as an \code{ai_request}
#'   audit decision with full provenance.
#' @param response_cache An optional ResponseCache object (T1.4). When
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

  # Fallback: generate without AI
  list(
    executive_summary = paste0(
      "This thematic analysis examined ", overall_stats$total_entries,
      " entries, identifying ", overall_stats$n_themes,
      " distinct themes. The overall sentiment was ",
      if (overall_stats$sentiment$mean < -0.2) "predominantly negative"
      else if (overall_stats$sentiment$mean > 0.2) "predominantly positive"
      else "mixed",
      " (M = ", overall_stats$sentiment$mean,
      ", SD = ", overall_stats$sentiment$sd, "), with ",
      overall_stats$sentiment$pct_negative, "% of entries showing negative sentiment."
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

  # Detect whether this is a post-OS.2 result (has meaningful_effect + p_raw)
  # or a legacy / fixture-generated result (no meaningful_effect column).
  has_meaningful <- "meaningful_effect" %in% names(correlations_df)
  has_p_raw <- "p_raw" %in% names(correlations_df)

  # Headline summary: effect-size-first framing for the new path,
  # back-compat phrasing for the legacy path.
  if (has_meaningful) {
    n_meaningful <- sum(correlations_df$meaningful_effect, na.rm = TRUE)
    n_sig <- sum(correlations_df$significant, na.rm = TRUE)

    summary_text <- paste0(
      "Of ", n_total, " exploratory associations examined, **", n_meaningful,
      "** had a meaningful effect size (|r| >= 0.10, Cohen's small-effect ",
      "threshold) and **", n_sig, "** survived Bonferroni adjustment at ",
      "alpha = 0.05.\n\n",
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
      filter(meaningful_effect) |>
      arrange(desc(abs(correlation)))
    label <- "The strongest associations (by effect size):"
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

      # Tiered p-values when post-OS.2 result; single p for legacy
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

#' Generate a theme detail section for appendix
#'
#' @param theme_name Theme name
#' @param ts Theme statistics (from aggregate_theme_statistics)
#' @param theme_index Numeric index
#' @param theme_csv_info CSV file info list (or NULL)
#' @return R Markdown string for the section
generate_theme_detail_section <- function(theme_name, ts, theme_index, theme_csv_info) {
  anchor_id <- paste0("appendix-", make_anchor_id(theme_name))

  content <- paste0(
    '## ', theme_index, '. ', .html_esc(theme_name), ' {#', anchor_id, '}\n\n',
    '<a class="appendix-back-link" href="#theme-summary-', theme_index, '">Back to summary</a>\n\n',
    '**Description:** ', .html_esc(ts$description %||% ""), '\n\n',
    '**Entries:** ', ts$n_entries, ' (', ts$pct_of_total, '% of total)\n\n',
    '**Sentiment:** Mean = ', ts$sentiment$mean,
    ' (SD = ', ts$sentiment$sd, ')\n\n'
  )

  # Subthemes
  if (length(ts$subthemes) > 0 && !all(is.na(unlist(ts$subthemes)))) {
    subs <- vapply(ts$subthemes, function(s) {
      if (is.null(s)) return(NA_character_)
      # Handle structured subthemes (list with $name) or plain strings
      val <- if (is.list(s)) s$name %||% as.character(s[[1]]) else as.character(s)
      if (length(val) != 1) val <- paste(val, collapse = " ")
      if (is.na(val) || nchar(val) == 0) NA_character_ else val
    }, character(1))
    subs <- subs[!is.na(subs)]
    if (length(subs) > 0) {
      content <- paste0(content, "**Subthemes:** ", paste(vapply(subs, .html_esc, character(1)), collapse = ", "), "\n\n")
    }
  }

  # Emotions table
  if (nrow(ts$emotions) > 0) {
    content <- paste0(content,
      "### Emotion Distribution\n\n",
      "| Emotion | Count | Percentage |\n",
      "|---------|------:|----------:|\n"
    )
    for (i in seq_len(min(5, nrow(ts$emotions)))) {
      row <- ts$emotions[i, ]
      content <- paste0(content,
        "| ", row$emotion, " | ", row$n, " | ", row$pct, "% |\n"
      )
    }
    content <- paste0(content, "\n")
  }

  # Download link
  if (!is.null(theme_csv_info)) {
    content <- paste0(content,
      '<div class="download-box">\n',
      '<a href="', theme_csv_info$relative_path, '" class="download-link" download>',
      'Download all ', ts$n_entries, ' entries as CSV</a>\n',
      '</div>\n\n'
    )
  }

  content
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
      ") | All correlation pairs with p-values |\n"
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

  valid <- valid |> arrange(sentiment_score)

  # Pick negative, neutral, positive representatives
  quotes <- list()

  if (nrow(valid) >= 1) {
    q <- valid[1, ]
    quotes$most_negative <- list(
      text = truncate_text(q[[text_col]], 300),
      sentiment = round(q$sentiment_score, 2),
      emotion = q$all_emotions %||% NA_character_
    )
  }

  if (nrow(valid) >= 3) {
    mid <- ceiling(nrow(valid) / 2)
    q <- valid[mid, ]
    quotes$median <- list(
      text = truncate_text(q[[text_col]], 300),
      sentiment = round(q$sentiment_score, 2),
      emotion = q$all_emotions %||% NA_character_
    )

    q <- valid[nrow(valid), ]
    quotes$most_positive <- list(
      text = truncate_text(q[[text_col]], 300),
      sentiment = round(q$sentiment_score, 2),
      emotion = q$all_emotions %||% NA_character_
    )
  } else if (nrow(valid) == 2) {
    q <- valid[2, ]
    quotes$most_positive <- list(
      text = truncate_text(q[[text_col]], 300),
      sentiment = round(q$sentiment_score, 2),
      emotion = q$all_emotions %||% NA_character_
    )
  }

  quotes
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
  # Percentages relative to number of entries (not total labels),
  # since one entry can have multiple emotions
  n_entries_with_emotion <- length(raw)
  tibble::tibble(
    emotion = names(tbl),
    n = as.integer(tbl),
    pct = round(100 * as.integer(tbl) / max(n_entries_with_emotion, 1), 1)
  )
}

# ==============================================================================
# Tier-0 Data Integrity Dashboard (Sprint-4 T0.1 part 3)
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
#' signal that we don't want to send.
#'
#' @param stats Named list returned by
#'   \code{\link{compute_quote_provenance_stats}}.
#' @param fabrication_log_relpath Optional relative path to
#'   \code{fabrication_log.csv} (relative to the report HTML's directory).
#'   When NULL or no fabrications occurred, no link is rendered.
#' @return A character string of markdown content (one card).
#' @keywords internal
.build_tier0_dashboard <- function(stats,
                                    fabrication_log_relpath = "fabrication_log.csv") {
  if (is.null(stats) || identical(stats$total, 0L)) {
    return(paste0(
      '<div class="tier0-dashboard tier0-empty">\n\n',
      '## Data Integrity Dashboard (T0.1)\n\n',
      'Quote-provenance verification did not run for this report ',
      '(pre-T0.1 run, no coding step, or no AI-attributed verbatim claims). ',
      'Future runs of this study will include a verification dashboard here.\n\n',
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

  # Fabrication line: only render the CSV link when there ARE fabrications
  # (most runs will have zero -- that's the goal). Path is relative because
  # the report HTML and the CSV both live in the same run output directory.
  fab_line <- if (n_fabricated > 0) {
    paste0(
      "**", n_fabricated, "** fabricated quote",
      if (n_fabricated == 1L) " was" else "s were",
      " detected by the verification ladder and DROPPED from the codebook ",
      "(see [fabrication_log.csv](", fabrication_log_relpath,
      ") for the audit trail).\n\n"
    )
  } else {
    paste0(
      "**No fabrications detected.** Every AI-attributed verbatim claim ",
      "verified against the source corpus.\n\n"
    )
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

  paste0(
    '<div class="tier0-dashboard">\n\n',
    '## Data Integrity Dashboard (T0.1)\n\n',
    'This run subjected every AI-attributed verbatim claim to a four-step ',
    'verification ladder (strict string match -> normalized match -> substring ',
    'search -> embedding similarity) before admitting it into the codebook. ',
    'This addresses the Jowsey et al. 2025 critique ',
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
    fab_line,
    drift_line,
    'See [`R/quote_provenance.R`](https://github.com/) for the verification ',
    'ladder implementation and the methodology paper for the empirical ',
    'justification of each ladder step\'s threshold.\n\n',
    '</div>\n\n'
  )
}
