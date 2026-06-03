# ==============================================================================
# Research-question coverage (Phase 63)
# ==============================================================================
#
# A late, post-hoc AI pass that answers the question a researcher most cares
# about: "did my analysis address the things I set out to study, and where did
# each land?" It runs AFTER the final themes exist, reads the named facets out of
# the researcher's OWN research focus + concepts, and judges -- per facet -- where
# its content landed across the themes.
#
# PRINCIPLES (do not weaken):
#   * The AI judges; the package renders. The package never classifies the
#     researcher's content or invents facets.
#   * Clustering is untouched -- this REPORTS where the emergent grouping placed
#     each facet; it never imposes structure or forces a theme per facet (C1/C2).
#   * A facet that DISPERSES across themes is a normal, valid inductive outcome,
#     never a coverage failure. The framing says so explicitly and points to
#     Mode 3 for guaranteed per-facet coverage.
# ==============================================================================

.RESEARCH_COVERAGE_SCHEMA_VERSION <- "1.0.0"

#' Construct a ResearchCoverage S3 object (Phase 63)
#'
#' @param facets List of per-facet coverage records (each: facet, coverage_level,
#'   supporting_codes, landed_in_themes, coverage_note).
#' @param overall_note A brief honest summary of overall RQ coverage.
#' @param source "ai" (discovery) or "pinned" (replay), like the methodology
#'   articulations.
#' @return A \code{ResearchCoverage} S3 list.
#' @keywords internal
new_research_coverage <- function(facets = list(), overall_note = "",
                                  source = "ai") {
  structure(list(
    schema_version = .RESEARCH_COVERAGE_SCHEMA_VERSION,
    facets         = facets %||% list(),
    overall_note   = .scalar_chr(overall_note),
    source         = match.arg(source, c("ai", "pinned"))
  ), class = "ResearchCoverage")
}

# Valid coverage levels (the AI's structural self-assessment -- an AI-judgment
# enum like the saturation verdict + cluster decision, NOT a package taxonomy of
# the researcher's content). Used only to coerce/validate the AI's own output.
.RESEARCH_COVERAGE_LEVELS <- c("central", "dispersed", "peripheral", "not_surfaced")

#' Coerce one AI facet-coverage record into a clean internal record
#' @keywords internal
.coerce_coverage_facet <- function(rec) {
  if (is.null(rec) || !is.list(rec)) return(NULL)
  facet <- .scalar_chr(rec$facet %||% "")
  if (!nzchar(facet)) return(NULL)
  lvl <- as.character(rec$coverage_level %||% "")[1]
  if (!lvl %in% .RESEARCH_COVERAGE_LEVELS) {
    # Defensive: strict mode enforces the enum, but never trust the wire. An
    # unrecognised level is treated as the most conservative descriptive bucket
    # rather than dropped (we never silently invent a stronger claim).
    lvl <- if (length(.as_char_vec(rec$landed_in_themes)) > 0L) "dispersed" else "peripheral"
  }
  list(
    facet            = facet,
    coverage_level   = lvl,
    supporting_codes = .as_char_vec(rec$supporting_codes),
    landed_in_themes = .as_char_vec(rec$landed_in_themes),
    coverage_note    = .scalar_chr(rec$coverage_note %||% "")
  )
}

#' Build the theme -> codes structure block for the coverage prompt
#'
#' One block per theme: name + (truncated) description + its code names and
#' (truncated) descriptions. Code names/descriptions come from the Code S3
#' objects; falls back to bare code names when descriptions are unavailable.
#' @keywords internal
.build_theme_structure_block <- function(theme_set) {
  themes <- theme_set$themes %||% list()
  if (length(themes) == 0L) return("(no themes)")
  blocks <- vapply(themes, function(th) {
    nm   <- as.character(th$name %||% "(unnamed theme)")[1]
    desc <- as.character(th$description %||% "")[1]
    objs <- tryCatch(theme_code_objects(th), error = function(e) list())
    code_lines <- if (length(objs) > 0L) {
      vapply(objs, function(co) {
        cn <- as.character(co$name %||% co$key %||% "")[1]
        cd <- as.character(co$description %||% "")[1]
        if (nzchar(cd)) sprintf("    - %s: %s", cn, substr(cd, 1, 160))
        else            sprintf("    - %s", cn)
      }, character(1))
    } else {
      nms <- tryCatch(theme_codes(th), error = function(e) character(0))
      if (length(nms) == 0L) "    - (no codes)" else sprintf("    - %s", nms)
    }
    paste0("THEME: ", nm,
           if (nzchar(desc)) paste0("\n  ", substr(desc, 1, 220)) else "",
           "\n", paste(code_lines, collapse = "\n"))
  }, character(1))
  paste(blocks, collapse = "\n\n")
}

#' Assess research-question coverage over the final themes (Phase 63)
#'
#' One AI call. Reads the named facets / sub-questions out of the researcher's own
#' \code{research_focus} + \code{concepts} and judges, per facet, where its
#' content landed across \code{theme_set}'s themes (central / dispersed /
#' peripheral / not_surfaced) with an honest note. Fails loudly on an empty or
#' unparseable response (no silent fallback). Returns an empty coverage (no AI
#' call) when there are no themes to assess.
#'
#' @param research_focus Character; the study's research focus (required).
#' @param concepts Character vector of the researcher's stated concepts (or NULL).
#' @param theme_set The final \code{ThemeSet} (after enrich/prune).
#' @param provider An \code{AIProvider}.
#' @param audit_log,response_cache,methodology_override As elsewhere.
#' @return A \code{ResearchCoverage} S3 object.
#' @keywords internal
assess_research_coverage <- function(research_focus, concepts, theme_set, provider,
                                     audit_log = NULL, response_cache = NULL,
                                     methodology_override = NULL) {
  if (is.null(research_focus) || !nzchar(research_focus)) {
    stop("assess_research_coverage: research_focus is required.", call. = FALSE)
  }
  themes <- theme_set$themes %||% list()
  if (length(themes) == 0L) {
    # Nothing to assess -> empty coverage, no AI call (honest + cheap).
    return(new_research_coverage(facets = list(), overall_note = "", source = "ai"))
  }

  concepts_str <- {
    cv <- .as_char_vec(concepts)
    if (length(cv) > 0L) paste(cv, collapse = ", ") else "(none stated)"
  }
  structure_block <- .build_theme_structure_block(theme_set)

  system_prompt <- paste0(
    "You are an expert qualitative methodologist assessing whether an inductive ",
    "thematic analysis ADDRESSED the things its researcher set out to study, and ",
    "WHERE each landed. You are shown the research focus, the researcher's stated ",
    "concepts, and the final theme structure (each theme and its codes).\n\n",
    "Read the named facets / sub-questions OUT OF the researcher's own focus and ",
    "concepts -- do NOT invent facets they did not state, and do NOT force the ",
    "themes into a fixed taxonomy. For each facet, judge where its content landed ",
    "across the themes, citing code and theme names verbatim.\n\n",
    "CRITICAL framing: in inductive thematic analysis a named facet that ",
    "DISPERSES across several themes (rather than forming its own) is a NORMAL, ",
    "VALID outcome -- the facet IS addressed; it simply was not the organizing ",
    "principle of a standalone theme. Treat 'dispersed' as a sound emergent ",
    "result, never a deficiency. Be HONEST when the corpus did not surface a ",
    "facet at all: mark it 'not_surfaced' and say so plainly (do not imply a flaw ",
    "in the method, and do not overclaim coverage)."
  )
  user_prompt <- paste0(
    "RESEARCH FOCUS:\n", research_focus, "\n\n",
    "RESEARCHER'S STATED CONCEPTS: ", concepts_str, "\n\n",
    "FINAL THEME STRUCTURE (theme -> its codes):\n", structure_block, "\n\n",
    "Return, in `facets`, one record per named facet / sub-question you read from ",
    "the focus + concepts -- facet, coverage_level (central | dispersed | ",
    "peripheral | not_surfaced), supporting_codes, landed_in_themes, coverage_note ",
    "-- using code and theme names verbatim from the structure above. Then an ",
    "`overall_note` summarizing RQ coverage honestly. If the focus states no ",
    "separable facets, return an empty `facets` array and say so in overall_note."
  )
  max_tokens <- min(16000L, 2000L + 250L * as.integer(length(themes)))
  ai_result <- ai_complete(
    provider, user_prompt, system_prompt,
    task = "methodology", temperature = 0, max_tokens = max_tokens,
    response_schema = .research_coverage_schema(),
    methodology_override = methodology_override
  )
  if (!is.null(audit_log)) {
    log_ai_request(audit_log, "methodology_assistant", ai_result, response_cache,
                   level = "RESEARCH_COVERAGE")
  }
  parsed <- .parse_methodology_json(ai_result$content)
  if (is.null(parsed) || is.null(parsed$facets)) {
    stop("assess_research_coverage: the AI returned an empty or unparseable coverage assessment. Aborting (no silent fallback).",
         call. = FALSE)
  }
  facets <- Filter(Negate(is.null),
                   lapply(.as_record_list(parsed$facets, "facet"), .coerce_coverage_facet))
  coverage <- new_research_coverage(
    facets = facets,
    overall_note = parsed$overall_note %||% "",
    source = "ai")

  if (!is.null(audit_log)) {
    n_ns <- sum(vapply(facets, function(f) identical(f$coverage_level, "not_surfaced"), logical(1)))
    log_ai_decision(audit_log, "methodology_assistant", "research_coverage",
                    n_facets = length(facets),
                    n_not_surfaced = n_ns,
                    overall_note = coverage$overall_note)
  }
  coverage
}

# ---- serialization + archive -------------------------------------------------

#' Serialize a ResearchCoverage to a plain list (Phase 63)
#' @keywords internal
research_coverage_to_list <- function(coverage) {
  stopifnot(inherits(coverage, "ResearchCoverage"))
  list(
    schema_version = coverage$schema_version,
    source         = coverage$source,
    overall_note   = coverage$overall_note,
    facets         = lapply(coverage$facets, function(f) list(
      facet            = f$facet,
      coverage_level   = f$coverage_level,
      supporting_codes = as.list(f$supporting_codes),
      landed_in_themes = as.list(f$landed_in_themes),
      coverage_note    = f$coverage_note
    ))
  )
}

#' Markdown rendering of a ResearchCoverage (for the run-dir archive)
#' @keywords internal
format_research_coverage_md <- function(coverage) {
  lines <- c("# Research-question coverage", "",
             "_Where each named focus facet landed across the inductive themes._",
             "_Dispersion across themes is a valid inductive outcome, not a gap._", "")
  if (nzchar(coverage$overall_note %||% "")) {
    lines <- c(lines, paste0("**Overall:** ", coverage$overall_note), "")
  }
  if (length(coverage$facets) == 0L) {
    lines <- c(lines, "_No separable named facets were identified in the focus._")
  } else {
    for (f in coverage$facets) {
      lines <- c(lines,
        sprintf("## %s  _(%s)_", f$facet, f$coverage_level),
        if (length(f$landed_in_themes) > 0L)
          paste0("- Landed in: ", paste(f$landed_in_themes, collapse = "; ")) else
          "- Landed in: (not surfaced)",
        if (length(f$supporting_codes) > 0L)
          paste0("- Codes: ", paste(f$supporting_codes, collapse = "; ")) else NULL,
        if (nzchar(f$coverage_note)) paste0("- ", f$coverage_note) else NULL,
        "")
    }
  }
  paste(lines, collapse = "\n")
}

#' Archive a ResearchCoverage to run_dir/rules/research_coverage.md and .json
#' @keywords internal
archive_research_coverage <- function(coverage, run_dir) {
  if (is.null(coverage) || is.null(run_dir)) return(invisible(NULL))
  rules_dir <- file.path(run_dir, "rules")
  if (!dir.exists(rules_dir)) dir.create(rules_dir, recursive = TRUE, showWarnings = FALSE)
  tryCatch({
    writeLines(format_research_coverage_md(coverage),
               file.path(rules_dir, "research_coverage.md"))
    jsonlite::write_json(research_coverage_to_list(coverage),
                         file.path(rules_dir, "research_coverage.json"),
                         auto_unbox = TRUE, null = "null", pretty = TRUE)
  }, error = function(e) log_warn("Could not archive research coverage: {e$message}"))
  invisible(NULL)
}

#' Load a ResearchCoverage from run_dir/rules/research_coverage.json (Phase 63)
#'
#' Fallback for a direct \code{generate_report()} / resume render that did not
#' thread the in-memory object (parallel to
#' \code{.load_methodology_articulations_from_run_dir}). Returns NULL when the
#' archive is absent or unreadable.
#' @keywords internal
.load_research_coverage_from_run_dir <- function(run_dir) {
  if (is.null(run_dir)) return(NULL)
  p <- file.path(run_dir, "rules", "research_coverage.json")
  if (!file.exists(p)) return(NULL)
  lst <- tryCatch(jsonlite::read_json(p, simplifyVector = FALSE),
                  error = function(e) NULL)
  if (is.null(lst) || is.null(lst$facets)) return(NULL)
  facets <- Filter(Negate(is.null),
                   lapply(.as_record_list(lst$facets, "facet"), .coerce_coverage_facet))
  new_research_coverage(facets = facets,
                        overall_note = lst$overall_note %||% "",
                        source = lst$source %||% "pinned")
}

# ---- report section ----------------------------------------------------------

# Human-readable label + CSS class for each coverage level. "dispersed" is framed
# as a valid outcome, never a deficiency.
.research_coverage_level_label <- function(lvl) {
  switch(lvl,
    central      = "Central (own theme)",
    dispersed    = "Dispersed across themes",
    peripheral   = "Peripheral",
    not_surfaced = "Not surfaced in corpus",
    lvl)
}

#' Render the Research-Question Coverage report section (Phase 63)
#'
#' Returns an HTML block: a short framing preamble + one row per named facet
#' (facet, coverage level, the themes it landed in, the AI's note) + the overall
#' note. Returns "" when there is no coverage object or no facets (vague focus),
#' so the section is simply omitted (byte-identical back-compat).
#'
#' @param coverage A \code{ResearchCoverage} S3 object, or NULL.
#' @return Character HTML string.
#' @keywords internal
.build_research_coverage_section <- function(coverage) {
  if (is.null(coverage) || !inherits(coverage, "ResearchCoverage")) return("")
  facets <- coverage$facets %||% list()
  if (length(facets) == 0L) return("")  # no separable facets -> omit (honest)

  rows <- vapply(facets, function(f) {
    # Defense-in-depth: the badge's CSS class is interpolated from `lvl`, so the
    # render layer itself guarantees it is one of the known tokens rather than
    # trusting the upstream coerce. A stray value falls back to a safe token (it
    # cannot reach here through any current path -- the schema enum + the coerce
    # both constrain it -- but the renderer never relies on that).
    lvl   <- f$coverage_level
    if (!isTRUE(lvl %in% .RESEARCH_COVERAGE_LEVELS)) lvl <- "peripheral"
    label <- .research_coverage_level_label(lvl)
    themes_txt <- if (length(f$landed_in_themes) > 0L)
      .html_esc(paste(f$landed_in_themes, collapse = ", ")) else
      "<em>(not surfaced)</em>"
    codes_txt <- if (length(f$supporting_codes) > 0L)
      sprintf("<div class=\"rc-codes\">codes: %s</div>",
              .html_esc(paste(f$supporting_codes, collapse = ", "))) else ""
    sprintf(paste0("<tr><td class=\"rc-facet\">%s</td>",
                   "<td><span class=\"rc-badge rc-%s\">%s</span></td>",
                   "<td>%s%s</td><td class=\"rc-note\">%s</td></tr>"),
            .html_esc(f$facet), lvl, .html_esc(label),
            themes_txt, codes_txt, .html_esc(f$coverage_note))
  }, character(1))

  overall <- if (nzchar(coverage$overall_note %||% ""))
    sprintf("<p class=\"rc-overall\"><strong>Overall:</strong> %s</p>",
            .html_esc(coverage$overall_note)) else ""

  paste0(
    "<h2>Research-question coverage</h2>\n",
    "<div class=\"research-coverage-section\">\n",
    "<p class=\"rc-preamble\">Where each named facet of the research focus landed ",
    "across the inductively generated themes. A facet that is <em>dispersed</em> ",
    "across several themes &mdash; rather than forming its own &mdash; is a normal, ",
    "valid outcome of inductive thematic analysis: its content is present in the ",
    "codes; it simply was not the organizing principle of a standalone theme. This ",
    "is a coverage map, not a score. For guaranteed coverage of pre-specified ",
    "facets, use Mode 3 (framework-applied), where those facets are the framework ",
    "constructs.</p>\n",
    overall,
    "<table class=\"research-coverage-table\">\n",
    "<thead><tr><th>Focus facet</th><th>Where it landed</th>",
    "<th>Themes &amp; codes</th><th>Note</th></tr></thead>\n",
    "<tbody>", paste(rows, collapse = "\n"), "</tbody>\n",
    "</table>\n</div>\n\n"
  )
}
