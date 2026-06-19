# ==============================================================================
# Methodological Transparency Report
# ==============================================================================
# Bundles the run's transparency artifacts into a single
# self-contained HTML report so a methodology paper reviewer can pick up
# ONE file and verify:
#
#   1. Run metadata (methodology mode, finalization status, schema version)
#   2. Lincoln & Guba (1985) credibility / dependability / confirmability /
#      transferability mapping with citations to specific decisions logged
#   3. Reflexivity scaffold (researcher_positionality + research_paradigm
#      + reflexive_notes, with Olmos-Vega AMEE Guide 149 framing)
#   4. T0.1 quote provenance summary (pre-rejection fabrication count +
#      verification rate post-rejection)
#   5. T0.3 corpus coverage summary (funnel from input -> LLM-processed
#      -> coded; saturation status; skip-reason taxonomy)
#   6. Audit log summary (one row per decision_type x step combo)
#   7. Theme set summary (count, kinds, top themes by prevalence)
#
# Framing per the rewrite plan: "AI does the bookkeeping so the human does
# the reflexivity, here is the receipt for everything."
#
# Outputs:
#   * <output_path>.html  -- self-contained HTML for human review
#   * <output_path>.json  -- machine-readable companion (same content,
#                            structured for tool-agnostic reuse)
#
# Idempotent + safe to re-run; reads from disk artifacts only (no
# re-execution of the pipeline). Per AC4, the report is methodology-
# stamped at the top.
# ==============================================================================

#' Schema version for the transparency report
#'
#' \itemize{
#'   \item 1.0.0: initial schema. Sections:
#'     run_metadata, lincoln_guba, reflexivity, quote_provenance,
#'     corpus_coverage, audit_summary, theme_set.
#' }
#' @keywords internal
.TRANSPARENCY_REPORT_SCHEMA_VERSION <- "1.0.0"

#' Bundle a run's transparency artifacts into a single report
#'
#' Generates a self-contained HTML methodological-transparency report
#' for a pakhom run, plus a machine-readable JSON companion. Per AC4
#' (methodology stamped on every output), the report is mode-stamped at
#' the top; per the transparency spec (anti-Jowsey compliance), it maps every
#' pipeline step to a Lincoln & Guba (1985) credibility / dependability
#' / confirmability / transferability checkpoint citing the exact
#' decisions logged.
#'
#' The bundler reads ONLY from disk artifacts produced by a completed
#' (or in-progress) pakhom run -- it never re-executes the pipeline,
#' never calls an AI provider, and is safe to invoke any number of
#' times. Missing artifacts degrade gracefully (the corresponding
#' section renders an "unavailable" notice rather than crashing).
#'
#' @param run_dir Path to the run output directory (the directory
#'   containing run_metadata.json + ai_decisions.jsonl etc.).
#' @param output_path Optional path for the HTML output. Defaults to
#'   \code{file.path(run_dir, "transparency_report.html")}. The JSON
#'   companion is written alongside (same basename, .json extension).
#' @return Invisible list with \code{html_path}, \code{json_path}, and
#'   the parsed \code{report_data} (the machine-readable contents).
#' @references
#'   Lincoln, Y. S. & Guba, E. G. (1985). Naturalistic inquiry.
#'     Sage Publications.
#'   Olmos-Vega, F. M. et al. (2023). A practical guide to reflexivity
#'     in qualitative research: AMEE Guide No. 149.
#'   Jowsey et al. (2025). PLOS One doi:10.1371/journal.pone.0330217.
#' @export
bundle_transparency_report <- function(run_dir,
                                          output_path = NULL) {
  if (!dir.exists(run_dir)) {
    stop("run_dir does not exist: ", run_dir, call. = FALSE)
  }
  if (is.null(output_path)) {
    output_path <- file.path(run_dir, "transparency_report.html")
  }
  json_path <- sub("\\.html?$", ".json", output_path, ignore.case = TRUE)
  if (identical(json_path, output_path)) {
    json_path <- paste0(output_path, ".json")
  }

  # ---- Gather all artifacts -------------------------------------------------
  report_data <- list(
    schema_version           = .TRANSPARENCY_REPORT_SCHEMA_VERSION,
    generated_at             = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z",
                                        tz = "UTC"),
    run_dir                  = normalizePath(run_dir, mustWork = TRUE),
    run_metadata             = .tr_read_run_metadata(run_dir),
    reflexivity              = .tr_read_reflexivity(run_dir),
    quote_provenance_summary = .tr_summarize_quote_provenance(run_dir),
    corpus_coverage          = .tr_read_corpus_coverage(run_dir),
    audit_log_summary        = .tr_summarize_audit_log(run_dir),
    theme_set_summary        = .tr_summarize_theme_set(run_dir),
    lincoln_guba_mapping     = NULL  # populated after the others
  )
  # The mapping references the other sections so it's computed last
  report_data$lincoln_guba_mapping <- .tr_build_lincoln_guba(report_data)

  # ---- Write JSON companion -------------------------------------------------
  jsonlite::write_json(report_data, json_path, pretty = TRUE,
                        auto_unbox = TRUE, null = "null", force = TRUE)
  meth_mode <- report_data$run_metadata$methodology_mode
  if (!is.null(meth_mode) && !is.na(meth_mode)) {
    tryCatch(
      stamp_methodology_json(json_path, meth_mode,
                              run_id = basename(run_dir)),
      error = function(e) log_debug(
        "transparency_report JSON stamp skipped: {e$message}"
      )
    )
  }

  # ---- Render HTML -----------------------------------------------------------
  html_content <- .tr_render_html(report_data)
  writeLines(html_content, output_path)

  log_info("Transparency report bundled: {output_path}")
  invisible(list(
    html_path   = output_path,
    json_path   = json_path,
    report_data = report_data
  ))
}

# ==============================================================================
# Artifact readers
# ==============================================================================

#' Unwrap a methodology-stamped JSON envelope
#'
#' \code{stamp_methodology_json}
#' (R/output_stamping.R:240-268) wraps the original payload in
#' \code{\{"_methodology_stamp": ..., "_payload": <original>\}}. The
#' transparency bundler reads disk artifacts that may or may not have
#' been stamped (coverage_card.json IS stamped when produced via
#' \code{write_corpus_coverage} with a non-null methodology mode; that
#' callsite always stamps). Earlier readers accessed
#' fields like \code{cov$n_processed} directly -- which returned NULL
#' on every real run because the data was under \code{_payload}. The
#' fixture tests passed only because the synthetic stub skipped the
#' stamp. This helper is the single source of truth for unwrapping;
#' all readers route through it.
#' @keywords internal
.tr_unwrap_payload <- function(x) {
  if (is.list(x) && !is.null(x[["_payload"]])) {
    return(x[["_payload"]])
  }
  x
}

#' Read run_metadata.json with graceful fallback
#' @keywords internal
.tr_read_run_metadata <- function(run_dir) {
  meta <- tryCatch(read_run_metadata(run_dir),
                    error = function(e) NULL)
  meta <- .tr_unwrap_payload(meta)
  if (is.null(meta)) {
    return(list(
      available        = FALSE,
      methodology_mode = NA_character_,
      is_finalized     = FALSE,
      created_at       = NA_character_,
      finalized_at     = NA_character_,
      prompt_template_version = NA_character_,
      schema_version   = NA_character_
    ))
  }
  list(
    available                = TRUE,
    run_id                   = meta$run_id %||% NA_character_,
    methodology_mode         = meta$methodology_mode %||% NA_character_,
    is_finalized             = isTRUE(meta$is_finalized),
    created_at               = meta$created_at %||% NA_character_,
    finalized_at             = meta$finalized_at %||% NA_character_,
    mode_locked_at           = meta$mode_locked_at %||% NA_character_,
    parent_run_id            = meta$parent_run_id %||% NA_character_,
    mode_changed_from        = meta$mode_changed_from %||% NA_character_,
    schema_version           = meta$schema_version %||% NA_character_,
    prompt_template_version  = meta$prompt_template_version %||% NA_character_
  )
}

#' Read reflexivity scaffold from run_metadata or config artifact
#' @keywords internal
.tr_read_reflexivity <- function(run_dir) {
  # Pull from run_metadata if present (the config is archived there);
  # otherwise fall back to scanning the saved archived config under
  # the run directory.
  meta <- tryCatch(read_run_metadata(run_dir),
                    error = function(e) NULL)
  meta <- .tr_unwrap_payload(meta)
  study <- meta$study %||% list()
  list(
    available            = !is.null(meta),
    researcher_positionality = study$researcher_positionality %||% NA_character_,
    research_paradigm        = study$research_paradigm %||% NA_character_,
    reflexive_notes          = study$reflexive_notes %||% NA_character_,
    completeness             = .tr_reflexivity_completeness(study)
  )
}

#' Score reflexivity scaffold completeness (0/3 .. 3/3)
#' @keywords internal
.tr_reflexivity_completeness <- function(study) {
  fields <- c("researcher_positionality", "research_paradigm", "reflexive_notes")
  filled <- vapply(fields, function(f) {
    v <- study[[f]]
    !is.null(v) && !is.na(v) && nzchar(as.character(v))
  }, logical(1))
  list(
    score         = as.integer(sum(filled)),
    max_score     = length(fields),
    filled_fields = fields[filled],
    missing_fields = fields[!filled]
  )
}

#' Summarize quote provenance from the audit log + fabrication log
#' @keywords internal
.tr_summarize_quote_provenance <- function(run_dir) {
  fab_path <- file.path(run_dir, "fabrication_log.csv")
  n_caught <- .count_pre_rejection_fabrications(
    fabrication_log_path = fab_path
  )
  audit_path <- file.path(run_dir, "ai_decisions.jsonl")
  n_verified_exact <- 0L
  n_verified_fuzzy <- 0L
  n_drifted <- 0L
  n_code_assignment <- 0L
  n_qv_coding <- 0L        # quote_verified records from the coding path
  n_qv_provocateur <- 0L   # quote_verified records from Mode 1 provocations
  if (file.exists(audit_path)) {
    lines <- tryCatch(readLines(audit_path, warn = FALSE),
                       error = function(e) character(0))
    for (ln in lines) {
      rec <- tryCatch(jsonlite::fromJSON(ln, simplifyVector = TRUE),
                       error = function(e) NULL)
      if (is.null(rec)) next
      dt <- rec$decision_type %||% NA_character_
      if (identical(dt, "quote_drifted")) {
        n_drifted <- n_drifted + 1L
      } else if (identical(dt, "code_assignment")) {
        n_code_assignment <- n_code_assignment + 1L
      } else if (identical(dt, "quote_verified")) {
        if (!is.null(rec$provocation_category)) {
          n_qv_provocateur <- n_qv_provocateur + 1L
        } else {
          n_qv_coding <- n_qv_coding + 1L
        }
        vs <- rec$verification_status %||% NA_character_
        if (identical(vs, "verified_exact")) n_verified_exact <- n_verified_exact + 1L
        if (identical(vs, "verified_fuzzy")) n_verified_fuzzy <- n_verified_fuzzy + 1L
      }
    }
  }
  # Count-robust combination: real quote_verified records are preferred,
  # but a run started before they existed and resumed after (the audit log
  # appends across resumes) has code_assignment records for ALL admitted
  # segments and quote_verified only for the post-resume tail -- so take
  # the larger of the two coding-side tallies rather than switching on
  # zero/nonzero. (For pure new runs the two are equal; for pure legacy
  # logs quote_verified is 0 and the proxy carries the count.)
  n_verifications <- n_drifted + max(n_code_assignment, n_qv_coding) +
    n_qv_provocateur
  verification_count_source <- if (n_qv_coding == 0L && n_qv_provocateur == 0L) {
    if (n_code_assignment > 0L) "code_assignment_proxy" else "no_verified_records"
  } else if (n_qv_coding >= n_code_assignment) {
    "quote_verified"
  } else {
    "mixed"
  }
  list(
    available           = file.exists(audit_path) || file.exists(fab_path),
    n_verifications     = as.integer(n_verifications),
    n_fabrications_caught = if (is.null(n_caught)) NA_integer_ else as.integer(n_caught),
    n_drifted           = as.integer(n_drifted),
    n_verified_exact    = as.integer(n_verified_exact),
    n_verified_fuzzy    = as.integer(n_verified_fuzzy),
    verification_count_source = verification_count_source,
    # Denominator is ALL attributed quotes -- survivors (n_verifications, which
    # includes drifted + admitted) PLUS the caught fabrications -- so the rate
    # is caught / total_attributed, NOT the survivor-biased caught / survivors.
    fabrication_rate    = if (is.null(n_caught) || n_verifications + (n_caught %||% 0L) == 0L) NA_real_
                          else (n_caught %||% 0L) / (n_verifications + (n_caught %||% 0L))
  )
}

#' Read coverage_card.json with graceful fallback
#' @keywords internal
.tr_read_corpus_coverage <- function(run_dir) {
  path <- file.path(run_dir, "coverage_card.json")
  if (!file.exists(path)) {
    return(list(available = FALSE))
  }
  cov <- tryCatch(
    jsonlite::read_json(path, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(cov)) return(list(available = FALSE))
  cov <- .tr_unwrap_payload(cov)
  cov$available <- TRUE
  cov
}

#' Summarize the audit log via the existing summarize_audit_log helper
#'
#' Adapts \code{summarize_audit_log()}'s public field names
#' (\code{total_decisions}, \code{decisions_by_type},
#' \code{decisions_by_step}) to the shorter names used in the
#' transparency report (total, by_decision_type, by_step) so the report
#' surface stays compact and stable across future audit-summary
#' refactors.
#' @keywords internal
.tr_summarize_audit_log <- function(run_dir) {
  s <- tryCatch(summarize_audit_log(run_dir),
                 error = function(e) NULL)
  if (is.null(s) || (s$total_decisions %||% 0L) == 0L) {
    return(list(available = FALSE))
  }
  list(
    available             = TRUE,
    total                 = as.integer(s$total_decisions %||% 0L),
    by_decision_type      = as.list(s$decisions_by_type %||% list()),
    by_step               = as.list(s$decisions_by_step %||% list()),
    total_ai_requests     = as.integer(s$total_ai_requests %||% 0L),
    total_tokens_used     = as.integer(s$total_tokens_used %||% 0L),
    methodology_modes_observed = as.character(s$methodology_modes_observed %||% character(0))
  )
}

#' Summarize themes.json (count, kinds, top 5 by entry_count)
#' @keywords internal
.tr_summarize_theme_set <- function(run_dir) {
  path <- file.path(run_dir, "themes.json")
  if (!file.exists(path)) return(list(available = FALSE))
  themes <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) NULL
  )
  themes <- .tr_unwrap_payload(themes)
  if (is.null(themes) || length(themes) == 0L) return(list(available = FALSE))

  kinds <- vapply(themes, function(t) t$theme_kind %||% "framework",
                   character(1))
  n_entries <- vapply(themes, function(t) as.integer(t$entry_count %||% 0L),
                       integer(1))
  ord <- order(-n_entries)
  top <- lapply(utils::head(ord, 5L), function(i) {
    list(name = themes[[i]]$name %||% NA_character_,
         n_entries = n_entries[[i]],
         theme_kind = kinds[[i]])
  })
  list(
    available     = TRUE,
    n_themes      = length(themes),
    kind_counts   = as.list(table(kinds)),
    total_entries = sum(n_entries),
    top_themes    = top
  )
}

# ==============================================================================
# Lincoln & Guba (1985) mapping
# ==============================================================================

#' Build the Lincoln & Guba (1985) trustworthiness mapping
#'
#' Maps pakhom's architectural commitments to the four classic
#' naturalistic-inquiry criteria from Lincoln & Guba 1985:
#' \itemize{
#'   \item \strong{Credibility} (~internal validity) -- T0.1 quote
#'     verification, methodology-rules injection, framework grounding.
#'   \item \strong{Dependability} (~reliability) -- audit log + AC9
#'     stamping + parent_run_id soft-lock.
#'   \item \strong{Confirmability} (~objectivity) -- reflexivity
#'     scaffold (Olmos-Vega AMEE 149) + per-cluster organizing-concept
#'     rationale (audit-logged).
#'   \item \strong{Transferability} (~external validity) -- AC4 output
#'     stamping, cross-run comparison, QDPX export.
#' }
#' Each criterion's "evidence" field cites the specific decisions
#' logged in this run so a reviewer can independently verify.
#' @keywords internal
.tr_build_lincoln_guba <- function(rd) {
  meta <- rd$run_metadata
  audit <- rd$audit_log_summary
  qp <- rd$quote_provenance_summary
  cov <- rd$corpus_coverage
  refl <- rd$reflexivity

  list(
    credibility = list(
      criterion = "Credibility (internal validity equivalent)",
      pakhom_mechanisms = c(
        "T0.1 quote provenance verification ladder (R/quote_provenance.R)",
        "Anti-fabrication enforcement: drop + log every fabricated quote",
        "Framework grounding: TPB / COM-B / TDF citations stamped in Mode 3",
        "AI saturation arbiter (replaces hardcoded thresholds)"
      ),
      run_evidence = list(
        n_verifications      = qp$n_verifications,
        n_fabrications_caught = qp$n_fabrications_caught,
        fabrication_rate     = qp$fabrication_rate,
        methodology_mode     = meta$methodology_mode
      )
    ),
    dependability = list(
      criterion = "Dependability (reliability equivalent)",
      pakhom_mechanisms = c(
        "AC9: every AI call audit-logged with methodology stamp",
        "AC10: stage-gating via filesystem state (checkpoints/*.rds)",
        "Soft-lock and parent_run_id linkage for auditable re-runs",
        "Schema versioning on every persisted artifact"
      ),
      run_evidence = list(
        audit_log_records         = audit$total,
        is_finalized              = meta$is_finalized,
        parent_run_id             = meta$parent_run_id,
        prompt_template_version   = meta$prompt_template_version,
        methodology_modes_observed = audit$methodology_modes_observed
      )
    ),
    confirmability = list(
      criterion = "Confirmability (objectivity equivalent)",
      pakhom_mechanisms = c(
        # Audit followup H-1: each entry is ONE logical bullet (the
        # renderer wraps each in <li>). Pre-followup the continuation
        # lines starting with "  +" / "  concept" produced six <li>
        # items when the author intended three.
        "Reflexivity scaffold (researcher_positionality + research_paradigm + reflexive_notes) injected to AI system prompt every turn (Olmos-Vega AMEE Guide 149)",
        "Per-cluster articulation: the AI records a central-organizing-concept rationale for each grouping decision (audit-logged, inspectable)",
        "Researcher review pause-points (codebook + themes)"
      ),
      run_evidence = list(
        reflexivity_completeness = refl$completeness,
        missing_reflexivity      = refl$completeness$missing_fields
      )
    ),
    transferability = list(
      criterion = "Transferability (external validity equivalent)",
      pakhom_mechanisms = c(
        "AC4 methodology stamping on every output (CSV/JSON/PNG/HTML)",
        "QDPX export for QDA-software interoperability (NVivo/ATLAS.ti/MAXQDA)",
        "Cross-run comparison (compare_runs)",
        "Paper-style per-subtheme tables with metric-tagged quotes"
      ),
      run_evidence = list(
        coverage_n_processed = cov$n_processed %||% NA_integer_,
        coverage_n_coded     = cov$n_coded %||% NA_integer_,
        coverage_rate        = cov$coverage_rate %||% NA_real_,
        stop_reason          = cov$stop_reason %||% NA_character_
      )
    )
  )
}

# ==============================================================================
# HTML rendering
# ==============================================================================

#' Render the transparency report as self-contained HTML
#' @keywords internal
.tr_render_html <- function(rd) {
  meta <- rd$run_metadata
  refl <- rd$reflexivity
  qp <- rd$quote_provenance_summary
  cov <- rd$corpus_coverage
  audit <- rd$audit_log_summary
  themes <- rd$theme_set_summary

  meth_label <- if (!is.na(meta$methodology_mode)) {
    tryCatch(methodology_label(meta$methodology_mode),
             error = function(e) meta$methodology_mode)
  } else "Methodology not declared"

  body <- paste0(
    "<!DOCTYPE html>\n<html lang='en'>\n<head>\n",
    "<meta charset='UTF-8'>\n",
    "<title>pakhom Transparency Report -- ", .html_esc(basename(rd$run_dir)),
    "</title>\n",
    .tr_inline_css(),
    "</head>\n<body>\n",
    "<div class='wrap'>\n",
    "<h1>Methodological Transparency Report</h1>\n",
    "<p class='subtitle'>pakhom run: <code>", .html_esc(basename(rd$run_dir)),
    "</code></p>\n",
    "<p class='subtitle'>Generated: ", .html_esc(rd$generated_at), "</p>\n",
    "<p class='subtitle'>Schema version: ",
    .html_esc(rd$schema_version), "</p>\n",
    .tr_methodology_section(meta, meth_label),
    .tr_reflexivity_section(refl),
    .tr_lincoln_guba_section(rd$lincoln_guba_mapping),
    .tr_quote_provenance_section(qp),
    .tr_corpus_coverage_section(cov),
    .tr_audit_summary_section(audit),
    .tr_theme_set_section(themes),
    .tr_footer(),
    "</div>\n</body>\n</html>\n"
  )
  body
}

#' @keywords internal
.tr_inline_css <- function() {
  paste0(
    "<style>\n",
    "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', ",
    "sans-serif; color: #2C3E50; max-width: 920px; margin: 2rem auto; ",
    "padding: 0 1.5rem; line-height: 1.6; }\n",
    ".wrap h1 { color: #2C3E50; border-bottom: 3px solid #3498DB; ",
    "padding-bottom: 0.5rem; }\n",
    ".wrap h2 { color: #2C3E50; margin-top: 2rem; border-left: 4px solid ",
    "#3498DB; padding-left: 0.75rem; }\n",
    ".wrap h3 { color: #34495E; margin-top: 1.5rem; }\n",
    ".subtitle { color: #7F8C8D; font-size: 0.9rem; margin: 0.3rem 0; }\n",
    ".lg-card { background: #F8F9FA; border-left: 4px solid #3498DB; ",
    "padding: 1rem 1.25rem; margin: 1rem 0; border-radius: 6px; }\n",
    ".lg-criterion { font-weight: 600; color: #2C3E50; font-size: 1.05rem; }\n",
    ".lg-mechanisms { font-size: 0.9rem; margin: 0.5rem 0; }\n",
    ".lg-evidence { background: rgba(52, 152, 219, 0.06); padding: 0.5rem ",
    "0.75rem; border-radius: 4px; font-family: 'Monaco', monospace; ",
    "font-size: 0.82rem; color: #34495E; }\n",
    ".stamp { background: rgba(52, 152, 219, 0.08); border-left: 3px solid ",
    "#3498DB; padding: 0.5rem 0.85rem; margin: 1rem 0; font-size: 0.85rem; }\n",
    ".na-note { color: #7F8C8D; font-style: italic; font-size: 0.85rem; }\n",
    "table { border-collapse: collapse; width: 100%; margin: 1rem 0; ",
    "font-size: 0.9rem; }\n",
    "th, td { padding: 0.4rem 0.75rem; text-align: left; border-bottom: 1px ",
    "solid #E5E7EB; }\n",
    "th { background: #F3F4F6; }\n",
    "code { background: rgba(0,0,0,0.04); padding: 0.1rem 0.35rem; ",
    "border-radius: 3px; font-size: 0.85rem; }\n",
    ".citation { font-size: 0.78rem; color: #7F8C8D; }\n",
    "</style>\n"
  )
}

#' @keywords internal
.tr_methodology_section <- function(meta, meth_label) {
  # Small helper: NULL or NA -> FALSE; a present non-NA value -> TRUE
  .present <- function(x) !is.null(x) && length(x) > 0L && !is.na(x) && nzchar(as.character(x))
  paste0(
    "<div class='stamp'>\n",
    "<strong>Methodology:</strong> ", .html_esc(meth_label), "<br/>\n",
    "<strong>Run ID:</strong> <code>",
    .html_esc(meta$run_id %||% NA_character_), "</code><br/>\n",
    "<strong>Finalized:</strong> ",
    if (isTRUE(meta$is_finalized)) "Yes" else "No",
    if (.present(meta$finalized_at)) paste0(" (", .html_esc(meta$finalized_at), ")") else "",
    "<br/>\n",
    "<strong>Created:</strong> ", .html_esc(meta$created_at %||% NA_character_),
    "<br/>\n",
    if (.present(meta$parent_run_id))
      paste0("<strong>Parent run:</strong> <code>",
              .html_esc(meta$parent_run_id), "</code> (mode changed from <em>",
              .html_esc(meta$mode_changed_from %||% "?"), "</em>)<br/>\n")
    else "",
    "<strong>Prompt template version:</strong> <code>",
    .html_esc(meta$prompt_template_version %||% "unknown"),
    "</code>\n",
    "</div>\n"
  )
}

#' @keywords internal
.tr_reflexivity_section <- function(refl) {
  comp <- refl$completeness
  paste0(
    "<h2>Reflexivity scaffold</h2>\n",
    "<p>Per Olmos-Vega et al. (AMEE Guide 149), positionality + paradigm + ",
    "reflexive notes are injected to every AI prompt turn. ",
    if (is.null(comp)) "<span class='na-note'>(unavailable)</span>"
    else paste0("This run filled ", comp$score, "/", comp$max_score,
                 " reflexivity fields."),
    "</p>\n",
    "<table>\n<thead><tr><th>Field</th><th>Value</th></tr></thead>\n<tbody>\n",
    "<tr><td>researcher_positionality</td><td>",
    .html_esc(refl$researcher_positionality %||% NA_character_),
    "</td></tr>\n",
    "<tr><td>research_paradigm</td><td>",
    .html_esc(refl$research_paradigm %||% NA_character_),
    "</td></tr>\n",
    "<tr><td>reflexive_notes</td><td>",
    .html_esc(refl$reflexive_notes %||% NA_character_),
    "</td></tr>\n",
    "</tbody></table>\n",
    "<p class='citation'>Olmos-Vega FM et al. (2023). A practical guide to ",
    "reflexivity in qualitative research. Medical Teacher.</p>\n"
  )
}

#' @keywords internal
.tr_lincoln_guba_section <- function(lg) {
  parts <- character(0)
  parts <- c(parts,
    "<h2>Lincoln &amp; Guba (1985) trustworthiness mapping</h2>\n",
    "<p>The four classic naturalistic-inquiry criteria mapped to pakhom's ",
    "architectural commitments, with run-specific evidence:</p>\n"
  )
  for (key in c("credibility", "dependability", "confirmability", "transferability")) {
    entry <- lg[[key]]
    if (is.null(entry)) next
    # Evidence values can be NULL, NA, scalar, vector, or nested list.
    # Collapse vectors to comma-separated strings BEFORE escape so
    # .html_esc never sees a multi-element input (which would trip
    # the `is.null(x) || is.na(x)` scalar guard).
    .fmt_evidence <- function(v) {
      if (is.null(v) || length(v) == 0L) return("<em>(unavailable)</em>")
      if (length(v) == 1L && is.na(v)) return("<em>(unavailable)</em>")
      if (is.list(v)) {
        # Nested list (e.g., completeness sub-record) -> compact JSON
        return(.html_esc(jsonlite::toJSON(v, auto_unbox = TRUE,
                                            null = "null")))
      }
      # Multi-element vector -> collapse with commas
      .html_esc(paste(as.character(v), collapse = ", "))
    }
    parts <- c(parts,
      "<div class='lg-card'>\n",
      "<div class='lg-criterion'>", .html_esc(entry$criterion), "</div>\n",
      "<div class='lg-mechanisms'>",
      "<strong>Mechanisms:</strong><ul>\n",
      paste0("<li>", vapply(entry$pakhom_mechanisms, .html_esc, character(1)),
              "</li>\n", collapse = ""),
      "</ul></div>\n",
      "<div class='lg-evidence'>\n",
      "<strong>This run's evidence:</strong><br/>\n",
      paste0(
        names(entry$run_evidence), ": ",
        vapply(entry$run_evidence, .fmt_evidence, character(1)),
        collapse = "<br/>"
      ),
      "\n</div>\n</div>\n"
    )
  }
  parts <- c(parts,
    "<p class='citation'>Lincoln, Y. S. &amp; Guba, E. G. (1985). ",
    "Naturalistic inquiry. Sage Publications.</p>\n"
  )
  paste0(parts, collapse = "")
}

#' @keywords internal
.tr_quote_provenance_section <- function(qp) {
  if (!isTRUE(qp$available)) {
    return(paste0(
      "<h2>T0.1: Quote provenance</h2>\n",
      "<p class='na-note'>Provenance data not available for this run.</p>\n"
    ))
  }
  rate_str <- if (is.na(qp$fabrication_rate)) "n/a"
              else sprintf("%.2f%%", 100 * qp$fabrication_rate)
  paste0(
    "<h2>T0.1: Quote provenance</h2>\n",
    "<p>Anti-fabrication enforcement: every AI-attributed verbatim claim ",
    "passes a verification ladder (string match -> normalized match -> ",
    "substring search, plus an optional embedding-cosine step when an ",
    "embedding provider is configured). Fabrications are ",
    "dropped from rendering and logged with the failed step.</p>\n",
    "<table>\n<thead><tr><th>Metric</th><th>Value</th></tr></thead>\n<tbody>\n",
    "<tr><td>",
    # Label the count honestly when it rests (partly) on the legacy
    # code_assignment proxy rather than real quote_verified records.
    switch(qp$verification_count_source %||% "quote_verified",
      code_assignment_proxy = "Verifications run (proxy: admitted segments + drifted)",
      mixed = "Verifications run (partly proxy: run predates per-quote records)",
      "Verifications run"),
    "</td><td>",
    format(qp$n_verifications, big.mark = ","), "</td></tr>\n",
    if (identical(qp$verification_count_source %||% "", "quote_verified")) {
      paste0(
        "<tr><td>Verified exact (offset match)</td><td>",
        format(qp$n_verified_exact %||% 0L, big.mark = ","), "</td></tr>\n",
        "<tr><td>Verified fuzzy (normalized / substring / embedding)</td><td>",
        format(qp$n_verified_fuzzy %||% 0L, big.mark = ","), "</td></tr>\n"
      )
    } else "",
    "<tr><td>Fabrications caught + excluded</td><td>",
    if (is.na(qp$n_fabrications_caught)) "n/a"
    else format(qp$n_fabrications_caught, big.mark = ","),
    "</td></tr>\n",
    "<tr><td>Drifted (source SHA mismatch)</td><td>",
    format(qp$n_drifted, big.mark = ","), "</td></tr>\n",
    "<tr><td>Pre-rejection fabrication rate</td><td>",
    .html_esc(rate_str), "</td></tr>\n",
    "</tbody></table>\n",
    "<p class='citation'>Per Jowsey et al. 2025 (PLOS One, ",
    "doi:10.1371/journal.pone.0330217), AI-assisted thematic analysis tools ",
    "must report quote-fabrication rates.</p>\n"
  )
}

#' @keywords internal
.tr_corpus_coverage_section <- function(cov) {
  if (!isTRUE(cov$available)) {
    return(paste0(
      "<h2>T0.3: Corpus coverage</h2>\n",
      "<p class='na-note'>Coverage card not available for this run.</p>\n"
    ))
  }
  paste0(
    "<h2>T0.3: Corpus coverage funnel</h2>\n",
    "<p>Empirical answer to Jowsey 2025's 'Frankenstein' finding (Copilot ",
    "drew themes from only the first 2-3 pages of data). pakhom processes ",
    "entries strictly one at a time; the funnel is the proof.</p>\n",
    "<table>\n<thead><tr><th>Stage</th><th>Entries</th></tr></thead>\n<tbody>\n",
    "<tr><td>Input to coding</td><td>",
    format(cov$n_input_to_coding %||% NA_integer_, big.mark = ","),
    "</td></tr>\n",
    "<tr><td>LLM-processed</td><td>",
    format(cov$n_processed %||% NA_integer_, big.mark = ","),
    "</td></tr>\n",
    "<tr><td>Coded</td><td>",
    format(cov$n_coded %||% NA_integer_, big.mark = ","), "</td></tr>\n",
    "<tr><td>Skipped (AI-judged or call failure)</td><td>",
    format(cov$n_skipped %||% NA_integer_, big.mark = ","), "</td></tr>\n",
    "</tbody></table>\n",
    "<p><strong>Stop reason:</strong> <code>",
    .html_esc(cov$stop_reason %||% "unknown"), "</code></p>\n",
    "<p><strong>No silent truncation (entry-level coverage):</strong> ",
    if (isTRUE(cov$no_silent_truncation)) "Yes (verified)" else "<em>flagged</em>",
    "</p>\n",
    "<p><strong>Saturation reached:</strong> ",
    if (isTRUE(cov$saturation_reached)) "Yes (AI arbiter)" else "No",
    "</p>\n"
  )
}

#' @keywords internal
.tr_audit_summary_section <- function(audit) {
  if (!isTRUE(audit$available)) {
    return(paste0(
      "<h2>AC9: Audit log</h2>\n",
      "<p class='na-note'>Audit log not available for this run.</p>\n"
    ))
  }
  by_type <- audit$by_decision_type
  paste0(
    "<h2>AC9: Audit log summary</h2>\n",
    "<p>Every AI decision recorded as one JSONL line in ",
    "<code>ai_decisions.jsonl</code> with methodology stamp. Total: ",
    format(audit$total, big.mark = ","), " records.</p>\n",
    "<table>\n<thead><tr><th>Decision type</th><th>Count</th></tr></thead>\n<tbody>\n",
    paste0(
      "<tr><td>", vapply(names(by_type), .html_esc, character(1)),
      "</td><td>", format(unlist(by_type), big.mark = ","), "</td></tr>\n",
      collapse = ""
    ),
    "</tbody></table>\n",
    "<p class='citation'>Methodology modes observed in audit: ",
    paste(vapply(audit$methodology_modes_observed, .html_esc, character(1)),
          collapse = ", "),
    "</p>\n"
  )
}

#' @keywords internal
.tr_theme_set_section <- function(themes) {
  if (!isTRUE(themes$available)) {
    return(paste0(
      "<h2>Theme set</h2>\n",
      "<p class='na-note'>themes.json not available for this run.</p>\n"
    ))
  }
  paste0(
    "<h2>Theme set summary</h2>\n",
    "<p>Total themes: ", format(themes$n_themes, big.mark = ","),
    "; total entries (sum of theme_membership): ",
    format(themes$total_entries, big.mark = ","), ".</p>\n",
    "<p>Theme kinds: ",
    paste0(names(themes$kind_counts), " = ",
            format(unlist(themes$kind_counts), big.mark = ","),
            collapse = "; "),
    "</p>\n",
    "<h3>Top 5 themes by prevalence</h3>\n",
    "<table>\n<thead><tr><th>Theme</th><th>Kind</th><th>Entries</th></tr></thead>\n<tbody>\n",
    paste0(
      vapply(themes$top_themes, function(t) {
        sprintf("<tr><td>%s</td><td>%s</td><td>%s</td></tr>",
                 .html_esc(t$name %||% ""),
                 .html_esc(t$theme_kind %||% ""),
                 format(t$n_entries %||% 0L, big.mark = ","))
      }, character(1)),
      collapse = "\n"
    ),
    "\n</tbody></table>\n"
  )
}

#' @keywords internal
.tr_footer <- function() {
  # Audit followup L-3: softened the AC4 claim. The HTML carries an
  # inline methodology stamp (the <div class='stamp'> block at the
  # top); the file itself is NOT wrapped in the AC4 file-level
  # envelope (that pattern applies to JSON / CSV outputs where a
  # downstream consumer might parse the file without seeing the
  # inline stamp).
  paste0(
    "<hr/>\n",
    "<p class='citation'>Generated by pakhom. ",
    "This report carries an inline methodology stamp at the top per ",
    "AC4; reviewers importing it should treat the report as the ",
    "receipt for the run's transparency artifacts, not a substitute ",
    "for the artifacts themselves (audit log, fabrication log, ",
    "coverage card, themes.json) -- all live in the same run ",
    "directory. The JSON companion (transparency_report.json) is ",
    "wrapped in the AC4 file-level envelope.</p>\n"
  )
}
