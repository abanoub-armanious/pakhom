# ==============================================================================
# Mode 1 (Reflexive Scaffold) Report Renderer -- Sprint-4 phase 31
# ==============================================================================
# Mode 1's analog of generate_report() in R/17_report.R. The existing report
# is wired to coding_state + sentiment + correlations + AI synthesis -- none
# of which exist in Mode 1. Rather than balloon the existing function with
# mode-conditional branching (option B from the phase 31 design discussion),
# this file builds a Mode 1-specific Rmd that:
#
#   - stamps the methodology declaration at the top (AC4) -- via
#     stamp_methodology_html() shared with Modes 2/3
#   - renders the Tier-0 Data Integrity Dashboard adapted to provocation
#     provenance stats (every provocation that cited a verbatim quote runs
#     through the same verification ladder used in Modes 2/3)
#   - renders the Mode 1 Provocation Coverage card (T0.3 in Mode 1 shape)
#     -- via render_tier0_coverage_card() S3 dispatch on the
#     ProvocationCoverage class
#   - renders the per-theme participant spread card (T0.2) -- via the
#     existing .build_participant_spread_card helper
#   - renders the per-theme provocations grouped by category, each with
#     cited quote + verification status + reason
#   - renders a deterministic executive summary (counts + flags); a future
#     phase can layer an AI-synthesis call on top
#   - includes the run-integrity card so reviewers see what artifacts the
#     run produced
#
# Helpers reused from Modes 2/3 (no duplication):
#   stamp_methodology_html, .build_tier0_dashboard,
#   render_tier0_coverage_card, .build_participant_spread_card,
#   .html_esc, theme_set_to_tibble
# ==============================================================================

# ==============================================================================
# Provocation provenance stats (Mode 1 analog of compute_quote_provenance_stats)
# ==============================================================================

#' Aggregate verification stats across all provocations in a reflection log
#'
#' Mode 1's analog of \code{\link{compute_quote_provenance_stats}}. Walks
#' \code{reflection_log$provocations}, extracts each provocation's
#' \code{$provenance} field (a \code{QuoteProvenance} object built and
#' verified by the per-category function -- see
#' R/provocateur.R::.citation_to_provocation), and feeds them through
#' \code{\link{quote_provenance_summary}}.
#'
#' Provocations from observational categories (absent_voice, parts of
#' assumption_surfacing) carry NULL provenance because the AI is reasoning
#' ABOUT the data rather than quoting it; those are excluded from the
#' verification stats (the Tier-0 dashboard's domain is verbatim claims).
#'
#' @param reflection_log A \code{ResearcherReflectionLog}, or NULL.
#' @return The list returned by \code{\link{quote_provenance_summary}}.
#' @export
compute_provocation_provenance_stats <- function(reflection_log) {
  if (is.null(reflection_log) ||
      !inherits(reflection_log, "ResearcherReflectionLog") ||
      length(reflection_log$provocations) == 0L) {
    return(quote_provenance_summary(list()))
  }
  quotes <- list()
  for (p in reflection_log$provocations) {
    if (!is.null(p$provenance) && inherits(p$provenance, "QuoteProvenance")) {
      quotes[[length(quotes) + 1L]] <- p$provenance
    }
  }
  quote_provenance_summary(quotes)
}

# ==============================================================================
# Mode 1 deterministic executive summary
# ==============================================================================

#' Build a deterministic Mode 1 executive summary
#'
#' No AI call -- counts + flags from the reflection log + theme stats.
#' Surfaces: total provocations, top-2 categories by emit count, themes
#' that attracted the most disconfirming evidence, themes flagged by
#' participant-spread concentration, fabrication count.
#' @keywords internal
.build_mode1_executive_summary <- function(reflection_log, theme_set,
                                              theme_stats, coverage,
                                              prov_stats) {
  n_provs <- length(reflection_log$provocations)
  n_themes <- length(theme_stats)

  # Top categories by emit count
  if (n_provs > 0L) {
    cats <- vapply(reflection_log$provocations,
                    function(p) p$category, character(1))
    cat_tbl <- sort(table(cats), decreasing = TRUE)
    top_cats <- if (length(cat_tbl) > 0L) {
      paste(vapply(seq_len(min(3L, length(cat_tbl))), function(i) {
        sprintf("%s (%d)", names(cat_tbl)[i], cat_tbl[[i]])
      }, character(1)), collapse = ", ")
    } else "none"
  } else {
    top_cats <- "none"
  }

  # Themes that attracted the most disconfirming evidence -- the
  # provocation category most directly tied to "is the theme actually
  # supported by the data?" Per Sarkar 2024 / patterns doc, this is the
  # most epistemically loaded category and worth surfacing first.
  disconfirming_by_theme <- if (n_provs > 0L) {
    df_provs <- Filter(function(p)
                          identical(p$category, "disconfirming_evidence"),
                        reflection_log$provocations)
    if (length(df_provs) > 0L) {
      tn_v <- vapply(df_provs, function(p) p$theme_name, character(1))
      sort(table(tn_v), decreasing = TRUE)
    } else stats::setNames(integer(0), character(0))
  } else stats::setNames(integer(0), character(0))

  top_disconfirmed <- if (length(disconfirming_by_theme) > 0L) {
    paste(vapply(seq_len(min(3L, length(disconfirming_by_theme))), function(i) {
      sprintf("%s (%d)",
              .html_esc(names(disconfirming_by_theme)[i]),
              disconfirming_by_theme[[i]])
    }, character(1)), collapse = "; ")
  } else "none"

  # Concentration warnings from participant spread. Audit B test gap
  # (phase 31): theme names can contain HTML-active characters
  # (researcher-supplied input). Route every theme-name interpolation
  # through .html_esc so the rendered Rmd doesn't smuggle a script tag
  # via the exec summary -- the test-mode1-report.R XSS test pinned this.
  concentration_flags <- character(0)
  for (tn in names(theme_stats)) {
    ps <- theme_stats[[tn]]$participant_spread
    if (isTRUE(ps$available) && !is.na(ps$top_contributor_share) &&
        ps$top_contributor_share > 0.5) {
      concentration_flags <- c(concentration_flags, sprintf(
        "%s (top contributor share %.0f%%)",
        .html_esc(tn), 100 * ps$top_contributor_share
      ))
    }
  }
  concentration_block <- if (length(concentration_flags) > 0L) {
    paste0(
      "**Participant-concentration flags:** ",
      paste(concentration_flags, collapse = "; "),
      ". Treat findings on these themes as a small number of voices, ",
      "not a community pattern.\n\n"
    )
  } else ""

  # Fabrication line. Audit B (phase 31): distinguish three cases
  # honestly:
  #   (a) verbatim claims existed AND fabrications were dropped --
  #       report the count
  #   (b) verbatim claims existed AND none were fabricated -- "no
  #       fabrications detected"
  #   (c) no verbatim claims at all (e.g., a Mode 1 run that only used
  #       absent_voice / observational categories) -- there's nothing
  #       to verify, so claiming "no fabrications" is misleading
  fab_count <- if (!is.null(prov_stats) && !is.null(prov_stats$by_status)) {
    if ("fabricated" %in% names(prov_stats$by_status))
      as.integer(prov_stats$by_status[["fabricated"]]) else 0L
  } else 0L
  total_verbatim <- if (!is.null(prov_stats)) prov_stats$total %||% 0L else 0L
  fab_line <- if (fab_count > 0L) {
    sprintf(
      "**%d fabricated provocation%s dropped** by the verification ladder ",
      fab_count, if (fab_count == 1L) " was" else "s were"
    )
  } else if (total_verbatim > 0L) {
    "**No fabrications detected** "
  } else {
    paste0(
      "**No verbatim claims to verify** -- this run's provocations are ",
      "from observational categories (absent_voice / assumption_surfacing ",
      "erased terms). No fabrication-detection signal applies. "
    )
  }

  # Coverage line
  coverage_line <- if (!is.null(coverage) && isTRUE(coverage$no_silent_skip)) {
    sprintf(
      paste0(
        "All %d theme(s) were challenged across all %d requested ",
        "provocation categor%s -- coverage verified."
      ),
      coverage$n_themes_input, coverage$n_categories_requested,
      if (coverage$n_categories_requested == 1L) "y" else "ies"
    )
  } else if (!is.null(coverage)) {
    paste0(
      "Coverage is incomplete (silent skip detected). See the ",
      "Provocation Coverage card below."
    )
  } else "Coverage not computed."

  paste0(
    "This Mode 1 (Reflexive Scaffold) run challenged ", n_themes,
    " researcher-authored theme(s) with ", n_provs, " AI-extracted ",
    "provocation(s). The AI's role here is Socratic gadfly (Sarkar 2024) ",
    "-- it surfaces counter-narratives, absent voices, alternative ",
    "framings, disconfirming evidence, and assumption-surfacing terms ",
    "extracted from the corpus. Theme authorship belongs to the ",
    "researcher. ", coverage_line, "\n\n",
    "**Top provocation categories:** ", top_cats, ".\n\n",
    "**Themes attracting the most disconfirming evidence:** ",
    top_disconfirmed, ". These are the themes most worth re-examining ",
    "against the cited counter-evidence below.\n\n",
    concentration_block,
    fab_line,
    "(Mode 1 verification: every provocation that cited a verbatim ",
    "quote ran through the same four-step verification ladder used in ",
    "Modes 2 and 3 -- per AC7, T0.1 is universal across modes.)\n\n"
  )
}

# ==============================================================================
# Mode 1 per-theme provocation section
# ==============================================================================

#' Build the per-theme provocations section of the Mode 1 report
#' @keywords internal
.build_mode1_provocation_section <- function(theme_stats, reflection_log) {
  if (length(theme_stats) == 0L) {
    return(paste0(
      "# Provocations by Theme\n\n",
      "No themes were processed in this run.\n\n"
    ))
  }

  content <- paste0(
    "# Provocations by Theme\n\n",
    "Each theme below was challenged across the requested provocation ",
    "categories. Each provocation is a verbatim citation from the corpus ",
    "(except observational categories like *absent voice*) that the AI ",
    "surfaced for the researcher's consideration. The AI does NOT ",
    "interpret -- it selects evidence the researcher's framing would ",
    "want to engage with.\n\n",
    "*Verification status* on each cited quote reflects the four-step ",
    "verification ladder (AC7 / T0.1 universal). Fabricated provocations ",
    "are dropped silently and recorded in `fabrication_log.csv` -- they ",
    "do not appear here.\n\n"
  )

  # Sort themes by provocation count (descending) so the most-contested
  # themes are at the top
  theme_order <- names(theme_stats)
  if (length(theme_order) > 1L) {
    counts <- vapply(theme_order,
                       function(tn) theme_stats[[tn]]$provocations$total,
                       integer(1))
    theme_order <- theme_order[order(-counts)]
  }

  for (tn in theme_order) {
    ts <- theme_stats[[tn]]
    safe_tn <- .html_esc(tn)
    content <- paste0(content,
      "## ", safe_tn, "\n\n",
      sprintf("**%d supporting entries** &middot; %d provocation%s issued\n\n",
              ts$n_entries,
              ts$provocations$total,
              if (ts$provocations$total == 1L) "" else "s")
    )

    # Phase 55: per-theme metric stats line (Mode 1 light touch). One
    # Median(MAD) + Mean(SD) per auto-detected metric column. Mode 1
    # themes are researcher-supplied + flat (no AI subthemes), so
    # there's no per-subtheme table -- just a one-liner per theme.
    if (length(ts$metric_cols %||% character(0)) > 0L) {
      stat_parts <- character(0)
      for (mc in ts$metric_cols) {
        ms <- ts$metric_stats[[mc]] %||% list()
        if (is.null(ms$n_observed) || ms$n_observed == 0L) next
        stat_parts <- c(stat_parts, sprintf(
          "%s: Median(MAD) = %s; Mean(SD) = %s",
          .html_esc(mc),
          .format_metric_summary(ms$median, ms$mad),
          .format_metric_summary(ms$mean, ms$sd)
        ))
      }
      if (length(stat_parts) > 0L) {
        content <- paste0(content,
          "**Metric summary:** ",
          paste(stat_parts, collapse = " &middot; "),
          "\n\n"
        )
      }
    }

    # T0.2 participant spread card
    content <- paste0(content,
      .build_participant_spread_card(ts$participant_spread)
    )

    # Provocations grouped by category (ordered by emit count desc)
    items <- ts$provocations$items
    if (length(items) == 0L) {
      content <- paste0(content,
        "_No provocations were emitted for this theme. This may be a ",
        "legitimate analytic outcome (the AI found no qualifying ",
        "evidence in the requested categories) or it may indicate the ",
        "supporting-entry sample was too narrow. See the Provocation ",
        "Coverage card for the attempt-vs-emit breakdown._\n\n"
      )
      next
    }

    by_cat <- split(items, vapply(items, function(p) p$category, character(1)))
    cat_order <- names(by_cat)[order(-vapply(by_cat, length, integer(1)))]

    for (cn in cat_order) {
      cat_items <- by_cat[[cn]]
      content <- paste0(content,
        "### ", .html_esc(cn), " (", length(cat_items), ")\n\n"
      )
      for (p in cat_items) {
        content <- paste0(content,
          .render_provocation_block(p)
        )
      }
    }
  }

  content
}

# ==============================================================================
# Researcher Reflexive Memos section (Phase 33 / M1.3)
# ==============================================================================

#' Build the Researcher Reflexive Memos section of the Mode 1 report
#'
#' Per AC6 (symmetric obligations across modes), Mode 1's burden parity
#' against Modes 2/3 is delivered through reflexive memos at pause
#' points. This section renders the memo timeline chronologically with
#' per-memo metadata (type, links to provocations / themes / entries)
#' so a reviewer can read the researcher's analytic trail alongside
#' the provocations that prompted it.
#'
#' Empty-memo state renders an explicit notice rather than silent
#' omission -- per AC4 the absence of memos in a Mode 1 run is itself
#' transparency-relevant. A Mode 1 run with zero memos is valid (the
#' researcher may have used the run for provocation generation only)
#' but the report says so.
#' @keywords internal
.build_mode1_memo_section <- function(reflection_log) {
  memos <- reflection_log$memos %||% list()
  typed_memos <- Filter(function(m) inherits(m, "Memo"), memos)

  header <- paste0(
    "\n# Researcher Reflexive Memos (M1.3 / AC6)\n\n",
    "Per AC6 (symmetric obligations across modes), Mode 1's burden ",
    "parity against Modes 2 and 3 is delivered through reflexive memos. ",
    "Memos are *researcher-authored* analytic notes -- the AI does NOT ",
    "write them. This section renders the memo timeline chronologically; ",
    "each memo links to the provocations, themes, codes, or entries it ",
    "responds to. Memos are persisted as Markdown files with YAML ",
    "frontmatter under `memos/` so they round-trip into NVivo / ATLAS.ti ",
    "via QDPX export.\n\n"
  )

  if (length(typed_memos) == 0L) {
    return(paste0(
      header,
      "_No memos were authored during this run. Per AC4, the absence is ",
      "reported rather than silently omitted -- a Mode 1 run with zero ",
      "memos is valid (the run may have been used for provocation ",
      "generation only) but does not deliver the researcher-side ",
      "reflexive burden that AC6 calls for. Consider authoring memos in ",
      "response to the provocations above; use `add_memo(reflection_log, ",
      "...)` to add memos programmatically or write Markdown files ",
      "directly under `memos/` (one per memo, with YAML frontmatter ",
      "matching the M1.3 schema -- see `?make_memo`)._\n\n"
    ))
  }

  # Sort chronologically (timestamp ascending) so the timeline reads
  # earliest -> latest.
  ts <- vapply(typed_memos, function(m) m$timestamp, character(1))
  typed_memos <- typed_memos[order(ts)]

  # By-type rollup
  type_counts <- table(vapply(typed_memos, function(m) m$type, character(1)))
  rollup_lines <- vapply(names(type_counts), function(tn) {
    sprintf("- **%s**: %d", .html_esc(tn), type_counts[[tn]])
  }, character(1))

  body <- paste0(
    header,
    "**Memos written: ", length(typed_memos), "**. Breakdown by type:\n",
    paste(rollup_lines, collapse = "\n"), "\n\n"
  )

  for (m in typed_memos) {
    body <- paste0(body, .render_memo_block(m))
  }
  body
}

#' Render a single memo as an HTML block
#' @keywords internal
.render_memo_block <- function(m) {
  if (!inherits(m, "Memo")) return("")
  type_class <- sprintf("memo-type-%s", .html_esc(m$type))
  meta_parts <- character(0)
  meta_parts <- c(meta_parts, sprintf("<span class=\"memo-timestamp\">%s</span>",
                                          .html_esc(m$timestamp)))
  meta_parts <- c(meta_parts, sprintf("<span class=\"memo-author\">%s</span>",
                                          .html_esc(m$author)))
  if (length(m$linked_themes) > 0L) {
    meta_parts <- c(meta_parts, sprintf(
      "<span class=\"memo-link\">themes: %s</span>",
      .html_esc(paste(m$linked_themes, collapse = ", "))
    ))
  }
  if (length(m$linked_codes) > 0L) {
    meta_parts <- c(meta_parts, sprintf(
      "<span class=\"memo-link\">codes: %s</span>",
      .html_esc(paste(m$linked_codes, collapse = ", "))
    ))
  }
  if (length(m$linked_entries) > 0L) {
    n_entries <- length(m$linked_entries)
    preview <- if (n_entries > 3L)
                 paste0(paste(head(m$linked_entries, 3L),
                                collapse = ", "),
                          sprintf(", +%d more", n_entries - 3L))
               else paste(m$linked_entries, collapse = ", ")
    meta_parts <- c(meta_parts, sprintf(
      "<span class=\"memo-link\">entries: %s</span>",
      .html_esc(preview)
    ))
  }
  if (!is.na(m$linked_prior_memo)) {
    meta_parts <- c(meta_parts, sprintf(
      "<span class=\"memo-link\">extends: <code>%s</code></span>",
      .html_esc(m$linked_prior_memo)
    ))
  }
  meta <- paste(meta_parts, collapse = " &middot; ")

  # Body is researcher-supplied Markdown -- escape the raw text and
  # emit it inside a <pre>-like wrapper so the Markdown renderer
  # treats it as a literal block. We don't render the Markdown to HTML
  # here because the researcher's content might use formatting that
  # collides with the surrounding Rmd structure (e.g., a "## Heading"
  # would create a duplicate level-2 heading in the report TOC). The
  # wrapper preserves the content faithfully without restructuring.
  body_esc <- .html_esc(m$body)

  paste0(
    sprintf('<div class="memo-block %s">\n', type_class),
    sprintf('<div class="memo-header"><strong>[%s]</strong> <code>%s</code></div>\n',
            .html_esc(m$type), .html_esc(m$id)),
    sprintf('<div class="memo-meta">%s</div>\n', meta),
    sprintf('<div class="memo-body"><pre style="white-space: pre-wrap; background: transparent; border: none; padding: 0;">%s</pre></div>\n',
            body_esc),
    "</div>\n\n"
  )
}

#' Render a single provocation as an HTML block (called from the per-theme section)
#' @keywords internal
.render_provocation_block <- function(p) {
  reason <- .html_esc(as.character(p$reason %||% ""))
  prov   <- p$provenance
  if (!is.null(prov) && inherits(prov, "QuoteProvenance")) {
    quote_txt <- .html_esc(as.character(prov$exact_text %||% ""))
    if (nchar(quote_txt) > 600L) {
      quote_txt <- paste0(substr(quote_txt, 1L, 597L), "...")
    }
    src_id <- .html_esc(as.character(prov$source_doc_id %||% ""))
    vstatus <- as.character(prov$verification_status %||% "unknown")
    vstatus_class <- if (vstatus %in% c("verified_exact", "verified_fuzzy"))
                       "prov-verified" else "prov-unverified"
    paste0(
      '<div class="provocation-block">\n',
      '<div class="provocation-quote">&ldquo;', quote_txt, '&rdquo;</div>\n',
      '<div class="provocation-meta">',
      '<span class="prov-source">Cited entry: ', src_id, '</span>',
      ' &middot; <span class="', vstatus_class, '">',
      .html_esc(vstatus), '</span>',
      '</div>\n',
      '<div class="provocation-reason"><strong>Why this challenges the theme:</strong> ',
      reason, '</div>\n',
      '</div>\n\n'
    )
  } else {
    # Observational provocation -- no verbatim citation. Render the
    # reason + any extra fields (alternative_term, erased_term,
    # dimension, etc.).
    extras <- p$extra %||% list()
    extra_str <- if (length(extras) > 0L) {
      parts <- vapply(names(extras), function(k) {
        sprintf("%s: %s", .html_esc(k),
                .html_esc(as.character(extras[[k]])[1L]))
      }, character(1))
      paste0(' <span class="provocation-extra">(',
             paste(parts, collapse = "; "), ')</span>')
    } else ""
    paste0(
      '<div class="provocation-block provocation-observational">\n',
      '<div class="provocation-reason">', reason, extra_str, '</div>\n',
      '</div>\n\n'
    )
  }
}

# ==============================================================================
# Mode 1 Rmd content builder
# ==============================================================================

#' @keywords internal
.build_mode1_rmd_content <- function(data, theme_set, reflection_log,
                                       coverage, theme_stats, config,
                                       run_id, prov_stats,
                                       integrity = NULL,
                                       self_contained = TRUE) {

  meth_mode <- .config_methodology_mode(config) %||% "reflexive_scaffold"
  research_focus <- config$study$research_focus %||% "Mode 1 reflexive analysis"
  safe_focus <- gsub("'", "''", .html_esc(research_focus))
  sc_flag <- if (isTRUE(self_contained)) "true" else "false"

  content <- paste0(
    "---\n",
    "title: 'Reflexive Scaffold (Mode 1) Analysis Report'\n",
    "subtitle: '", safe_focus, "'\n",
    "date: '", Sys.Date(), "'\n",
    "output:\n",
    "  html_document:\n",
    "    toc: true\n",
    "    toc_depth: 3\n",
    "    toc_float:\n",
    "      collapsed: true\n",
    "      smooth_scroll: true\n",
    "    theme: flatly\n",
    "    highlight: pygments\n",
    "    self_contained: ", sc_flag, "\n",
    "    css: styles.css\n",
    "---\n\n"
  )

  content <- paste0(content,
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,\n",
    "                      fig.width = 10, fig.height = 5.5, dpi = 150,\n",
    "                      error = TRUE)\n",
    "```\n\n"
  )

  # Methodology stamp -- shared helper from R/output_stamping.R
  content <- paste0(content,
    stamp_methodology_html(meth_mode, run_id = run_id), "\n"
  )

  # Executive summary (deterministic)
  content <- paste0(content,
    '<div class="hero-section">\n',
    "\n# Executive Summary\n\n",
    .build_mode1_executive_summary(reflection_log, theme_set,
                                     theme_stats, coverage, prov_stats),
    "</div>\n\n"
  )

  # Tier-0 verification dashboard (T0.1) -- adapted to provocation provenances
  content <- paste0(content,
    .build_tier0_dashboard(prov_stats,
                            fabrication_log_relpath = "fabrication_log.csv")
  )

  # Tier-0 coverage card (T0.3 in Mode 1 shape) -- S3 dispatch
  content <- paste0(content,
    render_tier0_coverage_card(coverage)
  )

  # Skipped themes -- if any
  if (!is.null(reflection_log$skipped_themes) &&
      nrow(reflection_log$skipped_themes) > 0L) {
    rows <- vapply(seq_len(nrow(reflection_log$skipped_themes)), function(i) {
      sprintf("- **%s** (%s, %s)",
              .html_esc(reflection_log$skipped_themes$theme_name[i]),
              .html_esc(reflection_log$skipped_themes$reason[i]),
              .html_esc(reflection_log$skipped_themes$skipped_at[i]))
    }, character(1))
    content <- paste0(content,
      "\n# Themes Explicitly Skipped\n\n",
      "The following theme(s) were skipped before the provocateur loop ",
      "began. The skip is recorded with a reason -- per AC4, absence is ",
      "reported rather than silently omitted.\n\n",
      paste(rows, collapse = "\n"), "\n\n"
    )
  }

  # Per-theme provocations
  content <- paste0(content,
    .build_mode1_provocation_section(theme_stats, reflection_log)
  )

  # Phase 33 (M1.3): Researcher Reflexive Memos section. Renders the
  # researcher's memo timeline (chronological by timestamp). Per AC6
  # (symmetric obligations across modes), memos are Mode 1's burden-
  # parity counterpart to Modes 2/3's codebook + theme review pause-
  # points. The section appears after provocations because memos are
  # most often written in response to provocations -- the reading
  # order mirrors the research workflow.
  content <- paste0(content,
    .build_mode1_memo_section(reflection_log)
  )

  # Run integrity card -- shows reviewer what artifacts the run produced
  if (!is.null(integrity)) {
    miss_block <- if (length(integrity$missing) == 0L) {
      paste0(
        '<div class="integrity-ok">All ', length(integrity$expected),
        ' expected file(s) present.</div>'
      )
    } else {
      paste0(
        '<div class="integrity-warn">Missing: ',
        .html_esc(paste(integrity$missing, collapse = ", ")),
        '</div>'
      )
    }
    content <- paste0(content,
      "\n# Run Integrity\n\n", miss_block, "\n\n"
    )
  }

  # Footer: methodology stamp again at bottom for completeness
  content <- paste0(content,
    "\n---\n\n*Generated by pakhom Mode 1 (Reflexive Scaffold). Run id: ",
    .html_esc(run_id %||% ""), ". Methodology mode locked at run start; ",
    "see run_metadata.json for the canonical declaration.*\n"
  )

  content
}

# ==============================================================================
# generate_mode1_report -- top-level Mode 1 report renderer
# ==============================================================================

#' Generate the Mode 1 (Reflexive Scaffold) HTML analysis report
#'
#' Mode 1's analog of \code{\link{generate_report}}. Builds an Rmd, copies
#' the shared CSS, and renders to HTML via \code{rmarkdown::render}.
#' Called from \code{\link{run_mode1}}; can also be called directly with
#' a previously-saved reflection_log + theme_set if a report needs to be
#' re-rendered after the fact.
#'
#' @param data Tibble: standardized corpus.
#' @param theme_set ThemeSet: researcher-authored themes.
#' @param reflection_log ResearcherReflectionLog: provocateur output.
#' @param coverage ProvocationCoverage (or NULL).
#' @param theme_stats Named list returned by
#'   \code{\link{compute_mode1_theme_stats}}.
#' @param config ThematicConfig (or list).
#' @param provider Optional AIProvider (currently unused; kept for
#'   signature parity + future AI-synthesis layer).
#' @param audit_log Optional AuditLog (currently unused; kept for parity).
#' @param response_cache Optional ResponseCache (currently unused).
#' @param fabrication_log Optional FabricationLog (currently unused).
#' @param output_file Path to the HTML output file.
#' @param self_contained Logical; if TRUE (default), produces a single
#'   self-contained HTML.
#' @return Path to the generated HTML on success, NULL on failure.
#' @export
generate_mode1_report <- function(data, theme_set, reflection_log,
                                    coverage = NULL, theme_stats = NULL,
                                    config = NULL, provider = NULL,
                                    audit_log = NULL, response_cache = NULL,
                                    fabrication_log = NULL,
                                    output_file = "analysis_report.html",
                                    self_contained = TRUE) {

  if (!inherits(theme_set, "ThemeSet")) {
    stop("generate_mode1_report: theme_set must be a ThemeSet object",
         call. = FALSE)
  }
  if (!inherits(reflection_log, "ResearcherReflectionLog")) {
    stop("generate_mode1_report: reflection_log must be a ",
         "ResearcherReflectionLog object", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("generate_mode1_report: data must be a data.frame / tibble",
         call. = FALSE)
  }
  if (is.null(theme_stats)) {
    theme_stats <- compute_mode1_theme_stats(data, theme_set, reflection_log)
  }

  log_info("Generating Mode 1 HTML report...")
  tic("Mode 1 report generation")

  output_dir <- dirname(output_file)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  prov_stats <- compute_provocation_provenance_stats(reflection_log)

  integrity <- tryCatch(verify_run_integrity(output_dir, config),
                          error = function(e) NULL)

  rmd_content <- .build_mode1_rmd_content(
    data           = data,
    theme_set      = theme_set,
    reflection_log = reflection_log,
    coverage       = coverage,
    theme_stats    = theme_stats,
    config         = config,
    run_id         = basename(output_dir),
    prov_stats     = prov_stats,
    integrity      = integrity,
    self_contained = self_contained
  )

  rmd_file <- gsub("\\.html$", ".Rmd", output_file)
  rmd_content <- paste(rmd_content, collapse = "\n")
  writeLines(rmd_content, rmd_file)
  log_info("Mode 1 R Markdown file written: {rmd_file}")

  # Copy shared CSS so the styling matches Modes 2/3
  css_src <- system.file("rmd", "styles.css", package = "pakhom")
  if (nchar(css_src) > 0L && file.exists(css_src)) {
    file.copy(css_src, file.path(output_dir, "styles.css"), overwrite = TRUE)
  }

  # Pandoc availability (shared with the Mode 2/3 path)
  if (!rmarkdown::pandoc_available()) {
    rstudio_pandoc <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"
    if (dir.exists(rstudio_pandoc)) {
      Sys.setenv(RSTUDIO_PANDOC = rstudio_pandoc)
      log_info("Using RStudio-bundled pandoc: {rstudio_pandoc}")
    }
  }

  abs_output_dir <- normalizePath(output_dir, mustWork = TRUE)
  abs_rmd_file <- normalizePath(rmd_file, mustWork = TRUE)
  tryCatch({
    rmarkdown::render(
      abs_rmd_file,
      output_file = basename(output_file),
      output_dir  = abs_output_dir,
      knit_root_dir = abs_output_dir,
      quiet = TRUE
    )
    log_info("Mode 1 HTML report generated: {output_file}")
  }, error = function(e) {
    log_error("Could not render Mode 1 HTML report: {e$message}")
    log_info("R Markdown file saved for manual rendering: {rmd_file}")
  })

  toc()

  if (!file.exists(output_file)) {
    log_error("Mode 1 report file not created: {output_file}")
    return(NULL)
  }
  output_file
}
