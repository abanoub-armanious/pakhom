# ==============================================================================
# Methodology Assistant (Phase 61.2)
# ==============================================================================
#
# Step 2.5 of the pipeline: once, before coding, the AI inspects the research
# focus + a corpus sample + the available metric/timestamp columns and the
# metric-primitive catalog (R/metric_primitives.R), and ARTICULATES -- in free
# form -- (a) a relevance criterion that operationalizes what is on-focus for
# THIS study, and (b) per-column interpretations naming which primitives are
# honest to compute and how to read them.
#
# This is the "AI as analyst" half of "AI as analyst with calculator." The
# package supplies the calculator (the catalog) and the prompt scaffolding; the
# AI does the analytical reasoning. Nothing here is a menu the researcher (or
# the AI) picks from to pre-classify data:
#   * The relevance criterion, column descriptions and interpretation notes are
#     all free text the AI generates.
#   * Primitive requests are FREE STRINGS (see .metric_intelligence_schema): the
#     AI may name a computation the catalog lacks, and the pipeline fails
#     honestly (R4) rather than substituting a misleading statistic.
# The researcher configures only research_focus + mode + data. Every AI
# articulation is archived (methodology_articulations.{md,json}) and can be
# pinned via config$study$inferred_methodology for replay-equivalent
# confirmatory runs (R7).
#
# All functions @keywords internal: the pipeline (Phase 61.3) calls them; the
# researcher never does.
# ==============================================================================

# ---- internal: parse a methodology AI response into nested lists -------------

# parse_json_safely() simplifies arrays-of-objects into data.frames, which is
# awkward for the nested metric records here. Parse with simplifyVector = FALSE
# so everything stays predictable nested lists. Strips the same markdown fences
# parse_json_safely() does. Returns NULL on failure (callers fail loudly).
.parse_methodology_json <- function(content) {
  if (is.null(content) || length(content) != 1L || is.na(content) ||
      !nzchar(trimws(content))) {
    return(NULL)
  }
  cleaned <- trimws(content)
  cleaned <- gsub("^```json\\s*", "", cleaned)
  cleaned <- gsub("^```\\s*", "", cleaned)
  cleaned <- gsub("\\s*```$", "", cleaned)
  tryCatch(
    jsonlite::fromJSON(cleaned, simplifyVector = FALSE),
    error = function(e) NULL
  )
}

# Coerce a (possibly list-of-length-1-strings) field to a plain character vector.
.as_char_vec <- function(x) {
  if (is.null(x) || length(x) == 0L) return(character(0))
  as.character(unlist(x, use.names = FALSE))
}

# Coerce to a length-1 character, mapping NULL/NA/empty to a default. (`%||%`
# only catches NULL, so `as.character(NULL)[1] %||% ""` would leak NA.)
.scalar_chr <- function(x, default = "") {
  x <- as.character(x)
  if (length(x) == 0L || is.na(x[1])) default else x[1]
}

# Normalize a parsed field to a list-of-records. Under simplifyVector = FALSE a
# JSON array of objects is already a list, but a single JSON object (which a
# reasoning model bypassing strict schema may emit instead of a 1-element array)
# is a named list -- detect that via its distinguishing key and wrap it, so the
# caller's lapply iterates records, not the single object's fields. NULL/empty
# -> list(); a genuinely empty/unparseable response is still caught upstream.
.as_record_list <- function(x, key) {
  if (is.null(x) || !is.list(x)) return(list())
  if (!is.null(x[[key]])) return(list(x))   # single record -> wrap
  x
}

# Coerce one parsed column record (metric or temporal) into the canonical shape.
.coerce_column_record <- function(rec) {
  if (is.null(rec) || !is.list(rec) || is.null(rec$column_name)) return(NULL)
  prims <- lapply(.as_record_list(rec$requested_primitives, "primitive"), function(p) {
    list(primitive = as.character(p$primitive %||% NA_character_)[1],
         rationale = as.character(p$rationale %||% "")[1])
  })
  prims <- Filter(function(p) !is.na(p$primitive) && nzchar(p$primitive), prims)
  list(
    column_name         = as.character(rec$column_name %||% NA_character_)[1],
    column_description  = as.character(rec$column_description %||% "")[1],
    requested_primitives = prims,
    interpretation_note = as.character(rec$interpretation_note %||% "")[1]
  )
}

# lapply .coerce_column_record over a (possibly single-object) field, dropping
# any records that failed to coerce (e.g. missing column_name).
.coerce_records <- function(x) {
  Filter(Negate(is.null),
         lapply(.as_record_list(x, "column_name"), .coerce_column_record))
}

# ---- S3 constructors ---------------------------------------------------------

#' @keywords internal
new_relevance_criterion <- function(research_focus_paraphrase = "",
                                    relevance_criterion = "",
                                    on_focus_examples = character(0),
                                    off_focus_examples = character(0),
                                    discrimination_principle = "",
                                    source = "ai") {
  structure(list(
    research_focus_paraphrase = .scalar_chr(research_focus_paraphrase),
    relevance_criterion       = .scalar_chr(relevance_criterion),
    on_focus_examples         = .as_char_vec(on_focus_examples),
    off_focus_examples        = .as_char_vec(off_focus_examples),
    discrimination_principle  = .scalar_chr(discrimination_principle),
    source                    = match.arg(source, c("ai", "pinned"))
  ), class = "RelevanceCriterion")
}

#' @keywords internal
new_metric_interpretation <- function(metrics = list(),
                                      temporal_columns = list(),
                                      source = "ai") {
  structure(list(
    metrics          = metrics %||% list(),
    temporal_columns = temporal_columns %||% list(),
    source           = match.arg(source, c("ai", "pinned"))
  ), class = "MetricInterpretation")
}

#' @keywords internal
new_methodology_articulations <- function(relevance,
                                          metric_interpretation,
                                          research_focus = "",
                                          source = "ai") {
  stopifnot(inherits(relevance, "RelevanceCriterion"))
  stopifnot(inherits(metric_interpretation, "MetricInterpretation"))
  structure(list(
    relevance             = relevance,
    metric_interpretation = metric_interpretation,
    research_focus        = .scalar_chr(research_focus),
    source                = match.arg(source, c("ai", "pinned")),
    schema_version        = "1.0.0"
  ), class = "MethodologyArticulations")
}

# ---- column detection + prompt builders --------------------------------------

#' Detect timestamp columns the methodology assistant should interpret
#'
#' Currently the standardized \code{std_timestamp} column (set by data loading),
#' returned only when it parses to at least one finite instant.
#' @keywords internal
.detect_temporal_columns <- function(data) {
  if (is.null(data) || !"std_timestamp" %in% names(data)) return(character(0))
  # .parse_timestamps is robust + non-throwing (multi-format, NA on failure);
  # the bare as.POSIXct(character) ERRORS on an ambiguous/garbage cell, and
  # suppressWarnings does not catch errors -- one bad row would crash Step 2.5.
  ts <- .parse_timestamps(as.character(data$std_timestamp))
  if (all(is.na(ts))) return(character(0))
  "std_timestamp"
}

# A deterministic, spread-out sample of entry texts for the relevance prompt.
# Deterministic (evenly-spaced indices, no RNG) so two discovery runs see the
# same sample; the relevance articulation is pinned for replay anyway.
.build_corpus_sample_block <- function(data, n_sample = 12L, max_chars = 300L) {
  text_col <- if ("std_text" %in% names(data)) "std_text"
              else if ("original_text" %in% names(data)) "original_text"
              else return("(no text column available)")
  texts <- as.character(data[[text_col]])
  texts <- texts[!is.na(texts) & nzchar(texts)]
  # Distinct entries only: real corpora contain runs of identical text
  # (e.g. "[removed]", boilerplate, bot posts); showing duplicates would waste
  # the relevance sample's slots. (Metric VALUE samples keep their repeats --
  # frequency is signal for numbers, not for this text sample.) unique() keeps
  # first-occurrence order, so the evenly-spaced pick stays deterministic.
  texts <- unique(texts)
  if (length(texts) == 0L) return("(no entry text available)")
  idx <- unique(round(seq(1, length(texts), length.out = min(n_sample, length(texts)))))
  picked <- texts[idx]
  picked <- vapply(picked, function(t) {
    if (nchar(t) > max_chars) paste0(substr(t, 1L, max_chars), " ...") else t
  }, character(1))
  paste(sprintf("%d. %s", seq_along(picked), picked), collapse = "\n")
}

# Per-column block: name + a deterministic spread of raw sample values. We show
# RAW values (not pre-computed stats) so the AI reasons about shape itself --
# pre-summarizing would be us doing the analysis for it.
.build_metric_columns_block <- function(data, metric_cols, temporal_cols,
                                        n_values = 12L) {
  n_rows <- nrow(data)
  fmt_vals <- function(col, temporal = FALSE) {
    v <- data[[col]]
    if (temporal) {
      v <- as.character(v)
    }
    keep <- !is.na(v)
    n_obs <- sum(keep)
    v <- v[keep]
    if (length(v) == 0L) return(sprintf("- %s: (no non-missing values)", col))
    idx <- unique(round(seq(1, length(v), length.out = min(n_values, length(v)))))
    sample_vals <- v[idx]
    if (!temporal) {
      sample_vals <- vapply(sample_vals, function(x) {
        if (abs(x - round(x)) < 1e-9) format(as.integer(round(x))) else format(round(x, 3))
      }, character(1))
    }
    sprintf("- %s: sample values [%s] (n_rows=%d, non-missing=%d)",
            col, paste(sample_vals, collapse = ", "), n_rows, n_obs)
  }
  parts <- character(0)
  if (length(metric_cols) > 0L) {
    parts <- c(parts, "NUMERIC METRIC COLUMNS:",
               vapply(metric_cols, fmt_vals, character(1), temporal = FALSE))
  } else {
    parts <- c(parts, "NUMERIC METRIC COLUMNS: (none detected)")
  }
  if (length(temporal_cols) > 0L) {
    parts <- c(parts, "", "TIMESTAMP COLUMNS:",
               vapply(temporal_cols, fmt_vals, character(1), temporal = TRUE))
  } else {
    parts <- c(parts, "", "TIMESTAMP COLUMNS: (none detected)")
  }
  paste(parts, collapse = "\n")
}

# Warn (loudly, by name) about any requested primitive the catalog lacks. Used
# on BOTH the discovery and pinned paths -- consistent with R4 fail-honest: we
# surface the gap (for the report + maintainers) but do NOT drop the request or
# substitute a different statistic. A WARN (not stop) because an unknown name is
# a legitimate fail-honest signal, not necessarily a config error.
.warn_unknown_primitives <- function(metric_interpretation, context = "") {
  available <- metric_catalog_names()
  requested <- unlist(lapply(
    c(metric_interpretation$metrics, metric_interpretation$temporal_columns),
    function(rec) vapply(rec$requested_primitives, function(p) p$primitive, character(1))
  ), use.names = FALSE)
  unknown <- unique(requested[!requested %in% available])
  if (length(unknown) > 0L) {
    logger::log_warn(sprintf(
      "Methodology Assistant%s requested %d primitive(s) not in the catalog: %s. These will be reported as unavailable (fail-honest); no statistic is substituted. Consider contributing the primitive(s).",
      if (nzchar(context)) paste0(" (", context, ")") else "",
      length(unknown), paste(unknown, collapse = ", ")))
  }
  invisible(unknown)
}

# ---- AI callers --------------------------------------------------------------

#' Articulate the study's relevance criterion (Phase 61.2)
#'
#' One AI call. Given the research focus + a corpus sample, the AI articulates a
#' free-form relevance criterion (+ on/off-focus examples) that the coding step
#' injects in place of loose "applicable" language. Fails LOUDLY on an empty or
#' unparseable response -- there is no degenerate fallback (a silent fallback is
#' the focus-drift pathology this module exists to fix).
#'
#' @return A \code{RelevanceCriterion} S3 object.
#' @keywords internal
articulate_relevance_criterion <- function(research_focus, corpus_sample, provider,
                                           audit_log = NULL, response_cache = NULL,
                                           methodology_override = NULL) {
  if (is.null(research_focus) || !nzchar(research_focus)) {
    stop("articulate_relevance_criterion: research_focus is required.", call. = FALSE)
  }
  system_prompt <- paste0(
    "You are an expert qualitative methodologist. Given a study's research ",
    "focus and a sample of its corpus, articulate -- in your own words -- what ",
    "makes a text segment RELEVANT to this focus. Your articulation ",
    "operationalizes inclusion/exclusion for coding: on-focus content is coded; ",
    "adjacent-but-off-focus content is not.\n\n",
    "Reason from THIS focus and THIS corpus. Do NOT impose a generic relevance ",
    "taxonomy. The aim is a criterion specific enough that a coder applies it ",
    "consistently and excludes content that is merely topically adjacent."
  )
  user_prompt <- paste0(
    "RESEARCH FOCUS:\n", research_focus, "\n\n",
    "CORPUS SAMPLE:\n", corpus_sample, "\n\n",
    "Articulate:\n",
    "1. research_focus_paraphrase: restate the focus in your own words.\n",
    "2. relevance_criterion: a paragraph a coder applies to decide if a segment ",
    "is on-focus (injected verbatim into the coding instructions).\n",
    "3. on_focus_examples: 2-4 short hypothetical on-focus fragments.\n",
    "4. off_focus_examples: 2-4 short fragments that are ADJACENT but NOT ",
    "on-focus (the discriminating cases).\n",
    "5. discrimination_principle: one sentence distinguishing on- from off-focus."
  )
  ai_result <- ai_complete(
    provider, user_prompt, system_prompt,
    task = "coding", temperature = 0, max_tokens = 2000,
    response_schema = .relevance_criterion_schema(),
    methodology_override = methodology_override
  )
  if (!is.null(audit_log)) {
    log_ai_request(audit_log, "methodology_assistant", ai_result, response_cache,
                   level = "RELEVANCE_CRITERION")
  }
  parsed <- .parse_methodology_json(ai_result$content)
  if (is.null(parsed) || is.null(parsed$relevance_criterion) ||
      !nzchar(as.character(parsed$relevance_criterion)[1])) {
    stop("articulate_relevance_criterion: the AI returned an empty or unparseable relevance criterion. Aborting before coding rather than proceeding with no criterion (no silent fallback).",
         call. = FALSE)
  }
  new_relevance_criterion(
    research_focus_paraphrase = parsed$research_focus_paraphrase %||% "",
    relevance_criterion       = parsed$relevance_criterion,
    on_focus_examples         = .as_char_vec(parsed$on_focus_examples),
    off_focus_examples        = .as_char_vec(parsed$off_focus_examples),
    discrimination_principle  = parsed$discrimination_principle %||% "",
    source                    = "ai"
  )
}

#' Interpret each metric / timestamp column (Phase 61.2)
#'
#' One AI call (skipped entirely when there are no metric or timestamp columns).
#' For each column the AI reads sampled raw values, describes it in free form,
#' and requests the primitives that are honest for it (by name -- free string,
#' so it may request something the catalog lacks; R4). Fails loudly on an
#' unparseable response.
#'
#' @return A \code{MetricInterpretation} S3 object.
#' @keywords internal
interpret_metrics <- function(data, research_focus, metric_cols = NULL,
                              temporal_cols = NULL, provider,
                              audit_log = NULL, response_cache = NULL,
                              methodology_override = NULL) {
  if (is.null(metric_cols))   metric_cols   <- .detect_metric_columns(data)
  if (is.null(temporal_cols)) temporal_cols <- .detect_temporal_columns(data)

  # Nothing to interpret -> empty interpretation, no AI call (honest + cheap).
  if (length(metric_cols) == 0L && length(temporal_cols) == 0L) {
    return(new_metric_interpretation(source = "ai"))
  }

  system_prompt <- paste0(
    "You are an expert data analyst deciding how to HONESTLY summarize each ",
    "measured column in a dataset, given the study's research focus. For each ",
    "column you are shown its name and a sample of its raw values; you also ",
    "have a catalog of computational primitives (a calculator) you request by ",
    "name.\n\n",
    "For each column decide: (1) what it represents and how its values behave ",
    "(shape, scale, skew, bounds, zeros; cadence for timestamps) -- reason from ",
    "the ACTUAL values, not from a fixed list of column 'kinds'; (2) which ",
    "primitives are honest for it (a right-skewed count must not be summarized ",
    "by a mean; a bounded ratio is not an unbounded count) -- request only what ",
    "is defensible; (3) what a reader should take from the results given the ",
    "focus.\n\n",
    "You are the analyst -- do not pick from a menu, reason about each real ",
    "column. If a column needs a computation this catalog lacks, name it ",
    "anyway: the pipeline records the gap honestly rather than substituting a ",
    "misleading statistic."
  )
  user_prompt <- paste0(
    "RESEARCH FOCUS:\n", research_focus, "\n\n",
    format_metric_catalog(), "\n\n",
    "COLUMNS:\n", .build_metric_columns_block(data, metric_cols, temporal_cols),
    "\n\nReturn one record per numeric metric column in `metrics`, and one per ",
    "timestamp column in `temporal_columns` (each: column_name, ",
    "column_description, requested_primitives [primitive + rationale], ",
    "interpretation_note). Request only honest summaries."
  )
  # The RESPONSE carries one record (description + N primitives + rationales +
  # note) per column, so the token budget must scale with column count or a
  # wide dataset truncates and (correctly) aborts. ~300 tokens/column + headroom.
  n_cols <- length(metric_cols) + length(temporal_cols)
  metric_max_tokens <- min(16000L, 1500L + 300L * as.integer(n_cols))
  ai_result <- ai_complete(
    provider, user_prompt, system_prompt,
    task = "coding", temperature = 0, max_tokens = metric_max_tokens,
    response_schema = .metric_intelligence_schema(),
    methodology_override = methodology_override
  )
  if (!is.null(audit_log)) {
    log_ai_request(audit_log, "methodology_assistant", ai_result, response_cache,
                   level = "METRIC_INTELLIGENCE")
  }
  parsed <- .parse_methodology_json(ai_result$content)
  if (is.null(parsed) || (is.null(parsed$metrics) && is.null(parsed$temporal_columns))) {
    stop("interpret_metrics: the AI returned an empty or unparseable metric interpretation. Aborting (no silent fallback to a naive stats battery).",
         call. = FALSE)
  }
  mi <- new_metric_interpretation(
    metrics          = .coerce_records(parsed$metrics),
    temporal_columns = .coerce_records(parsed$temporal_columns),
    source           = "ai"
  )
  .warn_unknown_primitives(mi, context = "discovery")
  mi
}

# ---- orchestrator ------------------------------------------------------------

#' Run the Methodology Assistant (Step 2.5 orchestrator, Phase 61.2)
#'
#' Either loads PINNED articulations from \code{config$study$inferred_methodology}
#' (replay mode -- no AI calls, fully deterministic) or makes the two AI calls
#' (discovery mode). Archives the result to
#' \code{run_dir/rules/methodology_articulations.\{md,json\}} when \code{run_dir}
#' is given. Returns a \code{MethodologyArticulations} bundle that Phase 61.3
#' attaches to the coding state.
#'
#' @return A \code{MethodologyArticulations} S3 object.
#' @keywords internal
run_methodology_assistant <- function(data, config, provider,
                                      audit_log = NULL, response_cache = NULL,
                                      run_dir = NULL, methodology_override = NULL) {
  research_focus <- config$study$research_focus %||% ""
  if (!nzchar(research_focus)) {
    stop("run_methodology_assistant: study.research_focus is required.", call. = FALSE)
  }

  pinned <- config$study$inferred_methodology
  if (!is.null(pinned)) {
    logger::log_info("Methodology Assistant: loading PINNED articulations (replay mode); skipping AI calls.")
    art <- load_pinned_methodology(pinned, research_focus = research_focus)
    if (!is.null(run_dir)) archive_methodology_articulations(art, run_dir)
    return(art)
  }

  logger::log_info("Methodology Assistant: articulating relevance criterion + metric interpretations (discovery mode).")
  corpus_sample <- .build_corpus_sample_block(data)
  relevance <- articulate_relevance_criterion(
    research_focus, corpus_sample, provider, audit_log, response_cache,
    methodology_override)
  if (!is.null(audit_log)) {
    log_ai_decision(audit_log, "methodology_assistant", "relevance_criterion",
                    relevance_criterion = relevance$relevance_criterion,
                    discrimination_principle = relevance$discrimination_principle,
                    n_on_focus = length(relevance$on_focus_examples),
                    n_off_focus = length(relevance$off_focus_examples))
  }

  metric_cols   <- .detect_metric_columns(data, config)
  temporal_cols <- .detect_temporal_columns(data)
  metric_interp <- interpret_metrics(
    data, research_focus, metric_cols, temporal_cols, provider,
    audit_log, response_cache, methodology_override)
  if (!is.null(audit_log)) {
    log_ai_decision(audit_log, "methodology_assistant", "metric_interpretation",
                    n_metrics = length(metric_interp$metrics),
                    n_temporal = length(metric_interp$temporal_columns))
  }

  art <- new_methodology_articulations(
    relevance = relevance, metric_interpretation = metric_interp,
    research_focus = research_focus, source = "ai")
  if (!is.null(run_dir)) archive_methodology_articulations(art, run_dir)
  art
}

# ---- replay path -------------------------------------------------------------

#' Load pinned methodology articulations from a config block (Phase 61.2)
#'
#' Reconstructs a \code{MethodologyArticulations} from a
#' \code{config$study$inferred_methodology} block (copied from a prior run's
#' \code{methodology_articulations.json}). No AI call -- this is the
#' replay-equivalent path (R7). Warns about any pinned primitive the catalog
#' lacks (fail-honest, consistent with the discovery path).
#'
#' @keywords internal
load_pinned_methodology <- function(inferred_block, research_focus = NULL) {
  if (is.null(inferred_block) || !is.list(inferred_block)) {
    stop("load_pinned_methodology: inferred_methodology block must be a list.", call. = FALSE)
  }
  if (is.null(inferred_block$relevance_criterion) ||
      !nzchar(as.character(inferred_block$relevance_criterion)[1])) {
    stop("load_pinned_methodology: inferred_methodology is missing relevance_criterion.", call. = FALSE)
  }
  relevance <- new_relevance_criterion(
    research_focus_paraphrase = inferred_block$research_focus_paraphrase %||%
                                  (research_focus %||% ""),
    relevance_criterion       = inferred_block$relevance_criterion,
    on_focus_examples         = .as_char_vec(inferred_block$on_focus_examples),
    off_focus_examples        = .as_char_vec(inferred_block$off_focus_examples),
    discrimination_principle  = inferred_block$discrimination_principle %||% "",
    source                    = "pinned"
  )
  mi <- new_metric_interpretation(
    metrics          = .coerce_records(inferred_block$metrics),
    temporal_columns = .coerce_records(inferred_block$temporal_columns),
    source           = "pinned"
  )
  .warn_unknown_primitives(mi, context = "pinned replay")
  new_methodology_articulations(
    relevance = relevance, metric_interpretation = mi,
    research_focus = research_focus %||% relevance$research_focus_paraphrase,
    source = "pinned")
}

# ---- serialization + archive -------------------------------------------------

.column_record_to_list <- function(rec) {
  list(
    column_name          = rec$column_name,
    column_description   = rec$column_description,
    requested_primitives = lapply(rec$requested_primitives, function(p) {
      list(primitive = p$primitive, rationale = p$rationale)
    }),
    interpretation_note  = rec$interpretation_note
  )
}

#' Serialize a MethodologyArticulations to a plain list (Phase 61.2)
#'
#' The list shape is the canonical \code{inferred_methodology} block: flat
#' relevance fields + \code{metrics} + \code{temporal_columns}. JSON-encode with
#' \code{jsonlite::write_json(auto_unbox = TRUE)} (the character-vector example
#' fields are wrapped in \code{as.list()} so single-element arrays survive
#' auto_unbox, per the package convention). The result can be copied verbatim
#' into \code{config$study$inferred_methodology} to replay the run.
#'
#' @keywords internal
methodology_articulations_to_list <- function(art) {
  stopifnot(inherits(art, "MethodologyArticulations"))
  list(
    schema_version            = art$schema_version,
    source                    = art$source,
    research_focus            = art$research_focus,
    research_focus_paraphrase = art$relevance$research_focus_paraphrase,
    relevance_criterion       = art$relevance$relevance_criterion,
    on_focus_examples         = as.list(art$relevance$on_focus_examples),
    off_focus_examples        = as.list(art$relevance$off_focus_examples),
    discrimination_principle  = art$relevance$discrimination_principle,
    metrics                   = lapply(art$metric_interpretation$metrics,
                                       .column_record_to_list),
    temporal_columns          = lapply(art$metric_interpretation$temporal_columns,
                                       .column_record_to_list)
  )
}

#' Reconstruct a MethodologyArticulations from a plain list (Phase 61.2)
#' @keywords internal
methodology_articulations_from_list <- function(lst, default_source = "pinned") {
  src <- lst$source %||% default_source
  relevance <- new_relevance_criterion(
    research_focus_paraphrase = lst$research_focus_paraphrase %||% "",
    relevance_criterion       = lst$relevance_criterion %||% "",
    on_focus_examples         = .as_char_vec(lst$on_focus_examples),
    off_focus_examples        = .as_char_vec(lst$off_focus_examples),
    discrimination_principle  = lst$discrimination_principle %||% "",
    source                    = src
  )
  mi <- new_metric_interpretation(
    metrics          = .coerce_records(lst$metrics),
    temporal_columns = .coerce_records(lst$temporal_columns),
    source           = src
  )
  new_methodology_articulations(
    relevance = relevance, metric_interpretation = mi,
    research_focus = lst$research_focus %||% "", source = src)
}

#' Render methodology articulations as human-readable markdown (Phase 61.2)
#' @keywords internal
format_methodology_articulations_md <- function(art) {
  stopifnot(inherits(art, "MethodologyArticulations"))
  rel <- art$relevance
  bullet <- function(xs) if (length(xs) == 0L) "  (none)" else
    paste(sprintf("  - %s", xs), collapse = "\n")
  col_md <- function(rec) {
    prims <- if (length(rec$requested_primitives) == 0L) "  (none requested)" else
      paste(vapply(rec$requested_primitives, function(p)
        sprintf("  - `%s`: %s", p$primitive, p$rationale), character(1)),
        collapse = "\n")
    paste0(
      "### ", rec$column_name, "\n\n",
      rec$column_description, "\n\n",
      "**Requested primitives:**\n", prims, "\n\n",
      "**Interpretation:** ", rec$interpretation_note, "\n")
  }
  lines <- c(
    "# Methodology Articulations (Phase 61 Methodology Assistant)",
    "",
    sprintf("- Source: **%s** (%s)", art$source,
            if (identical(art$source, "ai")) "AI-articulated at run start"
            else "pinned for replay"),
    sprintf("- Research focus: %s", art$research_focus),
    "",
    "## Relevance criterion",
    "",
    sprintf("**Focus (AI paraphrase):** %s", rel$research_focus_paraphrase),
    "",
    rel$relevance_criterion,
    "",
    "**On-focus examples:**", bullet(rel$on_focus_examples),
    "",
    "**Off-focus (adjacent) examples:**", bullet(rel$off_focus_examples),
    "",
    sprintf("**Discrimination principle:** %s", rel$discrimination_principle),
    "",
    "## Metric interpretations",
    ""
  )
  metrics <- art$metric_interpretation$metrics
  temporal <- art$metric_interpretation$temporal_columns
  if (length(metrics) == 0L) {
    lines <- c(lines, "(no numeric metric columns interpreted)", "")
  } else {
    lines <- c(lines, unlist(lapply(metrics, col_md), use.names = FALSE))
  }
  lines <- c(lines, "## Temporal interpretations", "")
  if (length(temporal) == 0L) {
    lines <- c(lines, "(no timestamp columns interpreted)", "")
  } else {
    lines <- c(lines, unlist(lapply(temporal, col_md), use.names = FALSE))
  }
  paste(lines, collapse = "\n")
}

#' Archive methodology articulations under \code{run_dir/rules/} (Phase 61.2)
#'
#' Writes \code{methodology_articulations.md} (human/peer-review record) and
#' \code{methodology_articulations.json} (machine-readable; copyable into
#' \code{config$study$inferred_methodology} for replay). Best-effort: logs and
#' returns NULL on failure rather than aborting the run.
#'
#' @return Named list of the two written paths (invisibly), or NULL on failure.
#' @keywords internal
archive_methodology_articulations <- function(art, run_dir) {
  stopifnot(inherits(art, "MethodologyArticulations"))
  if (is.null(run_dir)) return(NULL)
  tryCatch({
    rules_dir <- file.path(run_dir, "rules")
    if (!dir.exists(rules_dir)) {
      dir.create(rules_dir, recursive = TRUE, showWarnings = FALSE)
    }
    md_path   <- file.path(rules_dir, "methodology_articulations.md")
    json_path <- file.path(rules_dir, "methodology_articulations.json")
    writeLines(format_methodology_articulations_md(art), md_path)
    jsonlite::write_json(methodology_articulations_to_list(art), json_path,
                         auto_unbox = TRUE, pretty = TRUE, null = "null")
    logger::log_info("Methodology articulations archived: {md_path}")
    invisible(list(md = md_path, json = json_path))
  }, error = function(e) {
    logger::log_warn("Could not archive methodology articulations: {e$message}")
    NULL
  })
}

# ---- coding-prompt injection (consumed by Phase 61.3) ------------------------

#' Build the relevance-criterion block injected into the coding system prompt
#'
#' Phase 61.3 inserts this in place of the coding prompt's loose "applicable"
#' language. Returns "" when no usable criterion is present (caller keeps its
#' prior wording).
#'
#' @keywords internal
relevance_criterion_prompt_block <- function(relevance) {
  if (is.null(relevance) || !inherits(relevance, "RelevanceCriterion") ||
      !nzchar(relevance$relevance_criterion)) {
    return("")
  }
  ex <- function(xs) if (length(xs) == 0L) "" else
    paste(sprintf("- %s", xs), collapse = "\n")
  parts <- c(
    "## RELEVANCE CRITERION FOR THIS STUDY",
    relevance$relevance_criterion
  )
  if (length(relevance$on_focus_examples) > 0L) {
    parts <- c(parts, "", "## ON-FOCUS EXAMPLES", ex(relevance$on_focus_examples))
  }
  if (length(relevance$off_focus_examples) > 0L) {
    parts <- c(parts, "", "## OFF-FOCUS EXAMPLES (adjacent but NOT relevant)",
               ex(relevance$off_focus_examples))
  }
  if (nzchar(relevance$discrimination_principle)) {
    parts <- c(parts, "", paste0("Discrimination principle: ",
                                 relevance$discrimination_principle))
  }
  parts <- c(parts, "",
             "Code only segments that meet this relevance criterion. Adjacent context that does not directly satisfy it should NOT be coded.")
  paste(parts, collapse = "\n")
}
