# ==============================================================================
# Report Generation -- Rmd-based HTML Report with Exports
# ==============================================================================
# Replaces ~2000 lines of inline HTML string concatenation from the old script.
# Uses an external Rmd template + CSS file from inst/rmd/.
# ==============================================================================

#' Escape strings for safe HTML embedding
#' @param x Character string to escape
#' @return HTML-safe string
#' @keywords internal
.html_esc <- function(x) {
  if (is.null(x) || is.na(x)) return("")
  x <- as.character(x)
  if (requireNamespace("htmltools", quietly = TRUE)) {
    # htmltools::htmlEscape doesn't escape quotes by default
    out <- as.character(htmltools::htmlEscape(x))
    out <- gsub('"', "&quot;", out, fixed = TRUE)
    gsub("'", "&#39;", out, fixed = TRUE)
  } else {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    x <- gsub(">", "&gt;", x, fixed = TRUE)
    x <- gsub('"', "&quot;", x, fixed = TRUE)
    x <- gsub("'", "&#39;", x, fixed = TRUE)
    x
  }
}

#' Escape a value for safe embedding in a Markdown (pipe) table cell
#'
#' Applies \code{.html_esc} (so AI- or user-supplied text cannot inject HTML
#' once pandoc renders the table) and escapes the pipe character -- a literal
#' \code{"|"} would otherwise split the row into extra columns and corrupt the
#' table structure. Newlines are collapsed so a multi-line value cannot break
#' the single-row cell.
#' @keywords internal
.md_cell <- function(x) {
  x <- .html_esc(x)
  x <- gsub("[\r\n]+", " ", x)
  gsub("|", "\\|", x, fixed = TRUE)
}

# Allowlisted URL schemes for links embedded in AI-generated prose (consumed by
# .url_scheme_unsafe / .defang_unsafe_links below). Everything else --
# javascript:, data:, vbscript:, file:, ... -- is defanged, because pandoc turns
# [text](url) into <a href="url"> WITHOUT vetting the scheme and DECODES HTML
# entities in the URL first, so [x](javascript&colon;..) or [x](&#106;avascript:..)
# would otherwise render as a live, clickable javascript: href.
.SAFE_URL_SCHEMES <- c("http", "https", "mailto", "ftp", "ftps", "tel")

#' Decode the HTML character references pandoc/browsers resolve inside a URL
#'
#' Enough to reveal a link's true scheme: decimal (\code{&#106;}), hex
#' (\code{&#x6a;}), and the named refs that can disguise a scheme or its colon.
#' Iterates to a fixed point so multi-layer encodings (\code{&amp;#106;}) are
#' caught. Used ONLY to DECIDE whether a link is safe -- the original prose is
#' what gets kept or dropped, so over-decoding here cannot corrupt output.
#' @keywords internal
#' @noRd
.decode_url_entities <- function(s) {
  if (!grepl("&", s, fixed = TRUE)) return(s)
  named <- c(colon = ":", sol = "/", quest = "?", num = "#", period = ".",
             lpar = "(", rpar = ")", commat = "@", Tab = " ", NewLine = " ",
             amp = "&")
  for (.it in 1:5) {
    before <- s
    for (nm in names(named)) s <- gsub(paste0("&", nm, ";"), named[[nm]], s, fixed = TRUE)
    repeat {
      m <- regexpr("&#[xX]?[0-9A-Fa-f]+;", s, perl = TRUE)
      if (m[1] == -1L) break
      tok  <- regmatches(s, m)
      body <- sub(";$", "", sub("^&#", "", tok))
      code <- if (grepl("^[xX]", body)) strtoi(sub("^[xX]", "", body), 16L)
              else suppressWarnings(as.integer(body))
      ch <- if (is.na(code) || code < 1L || code > 0x10FFFF) ""
            else tryCatch(intToUtf8(code), error = function(e) "")
      s <- sub("&#[xX]?[0-9A-Fa-f]+;", ch, s, perl = TRUE)
    }
    if (identical(s, before)) break
  }
  s
}

#' Is a markdown link destination's URL scheme unsafe to render?
#'
#' Decodes entity obfuscation, strips the whitespace/control chars browsers
#' ignore, then allowlists the scheme (\code{.SAFE_URL_SCHEMES}). Scheme-less
#' (relative / anchor / query) destinations are safe; an absolute URL whose
#' scheme is not allowlisted -- or is malformed after decoding -- is unsafe.
#' @keywords internal
#' @noRd
.url_scheme_unsafe <- function(dest) {
  d <- .decode_url_entities(dest)
  d <- gsub("[[:space:][:cntrl:]]", "", d)
  d <- tolower(d)
  first_seg <- sub("[/?#].*$", "", d)
  if (!grepl(":", first_seg, fixed = TRUE)) return(FALSE)   # no scheme -> relative -> safe
  scheme <- sub(":.*$", "", first_seg)
  if (!grepl("^[a-z][a-z0-9+.-]*$", scheme)) return(TRUE)   # obfuscated / malformed scheme
  !(scheme %in% .SAFE_URL_SCHEMES)
}

# Inline markdown link or image: [text](dest ...) / ![alt](dest ...). The dest
# is the run up to the first whitespace or paren; the tail captures any title.
.MD_LINK_RE   <- "(!?)\\[([^]]*)\\]\\(\\s*([^()[:space:]]*)([^)]*)\\)"
# Reference-style definition at line start: [label]: dest "title".
.MD_REFDEF_RE <- "(?m)^([ ]{0,3}\\[[^]]+\\]:[ \\t]*)(\\S+)"

#' Defang markdown links/images whose URL scheme is unsafe
#'
#' Replaces an unsafe inline link/image with its visible text, and rewrites an
#' unsafe reference definition's URL to \code{#}. Safe links (http/https/mailto/
#' relative/...) are left byte-for-byte unchanged, so legitimate citations and
#' URLs with balanced parens are never mangled. Runs after \code{<>}-escaping,
#' so the only links present use \code{[]()} syntax (raw \code{<url>} autolinks
#' are already neutralized).
#' @keywords internal
#' @noRd
.defang_unsafe_links <- function(x) {
  vapply(x, function(s) {
    if (grepl("](", s, fixed = TRUE)) {
      gm <- gregexpr(.MD_LINK_RE, s, perl = TRUE)[[1]]
      if (gm[1] != -1L) for (full in regmatches(s, list(gm))[[1]]) {
        g <- regmatches(full, regexec(.MD_LINK_RE, full, perl = TRUE))[[1]]
        if (.url_scheme_unsafe(g[4])) s <- sub(full, g[3], s, fixed = TRUE)
      }
    }
    if (grepl("]:", s, fixed = TRUE)) {
      gm <- gregexpr(.MD_REFDEF_RE, s, perl = TRUE)[[1]]
      if (gm[1] != -1L) for (full in regmatches(s, list(gm))[[1]]) {
        g <- regmatches(full, regexec(.MD_REFDEF_RE, full, perl = TRUE))[[1]]
        if (.url_scheme_unsafe(g[3])) s <- sub(full, paste0(g[2], "#"), s, fixed = TRUE)
      }
    }
    s
  }, character(1), USE.NAMES = FALSE)
}

#' Neutralize HTML in AI-generated prose while preserving intended Markdown
#'
#' AI free-text (executive summary, conclusion, implications, key findings,
#' saturation articulation/rationale, learning reflection, correlation
#' narrative) is interpolated into the report as \emph{Markdown} -- so it must
#' keep working bold/links/lists/blockquotes -- but it can echo prompt-injected
#' corpus content like \samp{<script>}, \samp{<img onerror=...>}, or a
#' \samp{[click](javascript:...)} link. Running the full \code{.html_esc} here
#' would also escape the quotes/ampersands the Markdown relies on. Instead,
#' neutralize only what enables injection:
#' \itemize{
#'   \item \code{<} -> \code{&lt;} and \code{>} -> \code{&gt;} (no tag can open)
#'   \item a \emph{bare} \code{&} (not already starting an HTML entity) ->
#'     \code{&amp;}, so existing entities and the package's own \code{&bull;}/\code{&mdash;}
#'     are left intact and not double-escaped.
#'   \item markdown links/images whose URL scheme is not http/https/mailto/...
#'     (e.g. \code{javascript:}, \code{data:}, including the entity-obfuscated
#'     variants pandoc would decode) are defanged: pandoc does not vet link
#'     schemes, so this stops a clickable \code{javascript:} href in the report.
#' }
#' \code{**bold**}, safe \code{[text](url)}, lists, and \code{>} blockquote
#' markers the renderer prepends all keep working; an injected \code{<tag>} or
#' \code{javascript:} link cannot.
#' @param x Character string of AI-generated prose (NULL/NA -> "").
#' @return HTML-tag-neutralized, Markdown-preserving string.
#' @keywords internal
.sanitize_ai_prose <- function(x) {
  if (is.null(x) || all(is.na(x))) return("")
  x <- as.character(x)
  x[is.na(x)] <- ""
  # Strip ASCII control characters first, EXCEPT tab (\t) and newline (\n)
  # which legitimately structure Markdown. pandoc silently DELETES some of
  # these -- notably a carriage return -- from a link destination, while
  # .MD_LINK_RE's whitespace-stopping capture treats the same character as the
  # end of the URL. That mismatch let an embedded control char hide a scheme
  # from the allowlist (a CR in "[x](java<CR>script:...)") yet have pandoc
  # reassemble it into a live javascript: href in the rendered report. Removing
  # the control characters up front makes the sanitizer and pandoc agree.
  x <- gsub("[\x01-\x08\x0B-\x1F\x7F]", "", x, perl = TRUE)
  # Bare & first (a & not already part of a &name; / &#123; / &#xAF; entity),
  # so the &lt;/&gt; introduced next aren't themselves re-amped.
  x <- gsub("&(?!#?[A-Za-z0-9]+;)", "&amp;", x, perl = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  # Defang javascript:/data:/vbscript:/... links so pandoc can't render them as
  # live hrefs; http/https/mailto/relative links keep working.
  x <- .defang_unsafe_links(x)
  x
}

#' Export all analysis results to files
#'
#' @param data tibble with all analysis columns
#' @param theme_set ThemeSet object
#' @param correlations_df Correlations tibble
#' @param insights Insights list
#' @param consolidated ConsolidatedCodes list
#' @param output_dir Output directory path
#' @param methodology_mode Optional methodology mode (T1.7). When
#'   non-NULL, every CSV produced is stamped with a comment header
#'   identifying the mode and run id (per AC4). NULL skips stamping --
#'   used by tests / legacy callers.
#' @return List of export file paths
#' @export
export_results <- function(data, theme_set, correlations_df, insights,
                            consolidated, output_dir,
                            methodology_mode = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  export_files <- list()
  # T1.7 (AC4): methodology stamp on every CSV produced by this run.
  # Helper closes over methodology_mode + output_dir so the call sites
  # below stay one-line.
  .stamp <- function(path) {
    if (is.null(methodology_mode) || !file.exists(path)) return(invisible(NULL))
    tryCatch(stamp_methodology_csv(path, methodology_mode,
                                     run_id = basename(output_dir)),
             error = function(e) log_debug("CSV stamp skipped: {e$message}"))
  }

  # --- Sentiment scores CSV ---
  sentiment_file <- file.path(output_dir, "sentiment_scores.csv")
  sentiment_cols <- intersect(
    c("std_id", "std_text", "sentiment_score", "all_emotions",
      "emotion_intensity", "confidence", "emerged_themes", "n_themes"),
    names(data)
  )
  readr::write_csv(data[, sentiment_cols, drop = FALSE], sentiment_file)
  .stamp(sentiment_file)
  export_files$sentiment_file <- sentiment_file
  log_info("Exported sentiment scores: {sentiment_file}")

  # --- Codes CSV (renamed from "consolidated_codes.csv"; the
  # old filename violated C-tenet 2 because it implied codes are
  # consolidated/merged, when in fact pakhom preserves codes as atomic
  # leaves throughout clustering. New filename is honest.) ---
  codes_file <- file.path(output_dir, "codes.csv")
  if (!is.null(consolidated) && !is.null(consolidated$codes) && nrow(consolidated$codes) > 0) {
    readr::write_csv(consolidated$codes, codes_file)
    log_info("Exported {nrow(consolidated$codes)} codes: {codes_file}")
  } else {
    readr::write_csv(tibble(code_text = character(), frequency = integer(), code_type = character()),
                      codes_file)
    log_warn("No codes to export")
  }
  .stamp(codes_file)
  export_files$codes_file <- codes_file

  # --- Correlations CSV ---
  correlations_file <- file.path(output_dir, "correlations.csv")
  if (!is.null(correlations_df) && nrow(correlations_df) > 0) {
    readr::write_csv(correlations_df, correlations_file)
    log_info("Exported {nrow(correlations_df)} correlation pairs: {correlations_file}")
  } else {
    readr::write_csv(tibble(var1 = character(), var2 = character(),
                             correlation = numeric(), p_value = numeric(),
                             significant = logical(), effect_size = character()),
                      correlations_file)
  }
  .stamp(correlations_file)
  export_files$correlations_file <- correlations_file

  # --- Themes JSON ---
  # Bypass theme_set_to_tibble() for JSON. The tibble form
  # collapses codes_included/subthemes/keywords into ";"-delimited strings
  # (R/12_theme_data.R::theme_set_to_tibble), which is correct for CSV
  # cells but WRONG for JSON consumers. An audit caught this:
  # downstream tools (and the plan-comparison agents) saw codes_included
  # as a single string of length 1 and assumed the merge tree had
  # corrupted the codes. The merge tree is fine; the JSON serialization
  # just needed to preserve the in-memory character-vector shape. Wrap
  # array-typed fields in I() so jsonlite::auto_unbox doesn't collapse
  # length-1 vectors to scalars.
  themes_file <- file.path(output_dir, "themes.json")
  themes_json <- lapply(theme_set$themes, function(t) {
    # walk the canonical Theme -> Subtheme -> Code hierarchy.
    # Flat codes_included + flat subthemes character vectors stay for
    # back-compat with downstream consumers; subthemes_structured carries
    # the full hierarchy with per-code metadata (key, name, desc, freq,
    # n_segments). Full coded_segments + QuoteProvenance live in the
    # per-theme detail files (see export_theme_entry_csvs).
    flat_code_names <- theme_codes(t)
    flat_subtheme_names <- .subtheme_names_no_virtual(t)

    # serialize the full subtheme tree (subthemes
    # can now nest). Recursive walker emits nested $subthemes alongside
    # the direct $codes at each level; consumers that don't know about
    # the nesting (earlier readers) still get codes + name + desc.
    serialize_subtheme <- function(s) {
      if (!inherits(s, "Subtheme")) return(NULL)
      list(
        name        = if (is.na(s$name)) NA_character_ else s$name,
        description = s$description %||% "",
        codes = lapply(s$codes %||% list(), function(c) {
          list(
            key         = c$key %||% "",
            name        = c$name %||% "",
            description = c$description %||% "",
            type        = c$type %||% "descriptive",
            frequency   = as.integer(c$frequency %||% 0L),
            entry_ids   = I(as.character(c$entry_ids %||% character(0))),
            n_segments  = length(c$coded_segments %||% list())
          )
        }),
        subthemes = Filter(
          Negate(is.null),
          lapply(s$subthemes %||% list(), serialize_subtheme)
        )
      )
    }
    structured <- lapply(t$subthemes %||% list(), serialize_subtheme)
    structured <- Filter(Negate(is.null), structured)

    # three subtheme counters expose the full
    # decomposition shape to consumers. n_subthemes preserves the
    # earlier semantics (depth-1 real subthemes only) for back-
    # compat. n_subthemes_total counts every named subtheme at every
    # depth (the "true" decomposition size when C-12's nested walker
    # produces sub-subthemes). n_subthemes_structured matches
    # length(subthemes_structured) exactly, including virtual NA-named
    # wrappers -- the value that surprised an audit when it
    # didn't match n_subthemes.
    list(
      id                       = as.integer(t$id %||% 0L),
      name                     = t$name %||% "",
      description              = t$description %||% "",
      prevalence               = t$prevalence %||% NA_character_,
      sentiment_tendency       = t$sentiment_tendency %||% NA_character_,
      entry_count              = as.integer(t$entry_count %||% 0L),
      n_codes                  = length(flat_code_names),
      n_subthemes              = length(flat_subtheme_names),
      n_subthemes_total        = theme_n_subthemes_total(t),
      n_subthemes_structured   = length(structured),
      codes_included           = I(flat_code_names),
      subthemes                = I(flat_subtheme_names),
      subthemes_structured     = structured,
      keywords                 = I(as.character(t$keywords %||% character(0))),
      narrative                = t$narrative %||% "",
      supporting_quotes        = I(as.character(t$supporting_quotes %||% character(0))),
      # Persist the
      # structured supporting_quote_records to disk so downstream
      # consumers (themes.json readers, comparison runs, cross-run
      # T0.2 audits) can trace each rendered quote back to its source
      # entry. Pre-followup the field was in-memory only -- defeating
      # the M-25 purpose since any persistent consumer saw only the
      # bare-string supporting_quotes legacy field. Empty list when
      # the theme has no representative quotes (default-shaped
      # themes from create_theme_set hydration).
      supporting_quote_records = t$supporting_quote_records %||% list()
    )
  })
  jsonlite::write_json(themes_json, themes_file, pretty = TRUE,
                        auto_unbox = TRUE, null = "null", force = TRUE)
  if (!is.null(methodology_mode)) {
    tryCatch(stamp_methodology_json(themes_file, methodology_mode,
                                      run_id = basename(output_dir)),
             error = function(e) log_debug("JSON stamp skipped: {e$message}"))
  }
  export_files$themes_file <- themes_file
  log_info("Exported themes: {themes_file}")

  # --- Per-theme CSV files ---
  theme_csv_files <- export_theme_entry_csvs(data, theme_set, output_dir,
                                              methodology_mode = methodology_mode)
  export_files$theme_csv_files <- theme_csv_files

  # --- Correlation plot ---
  export_files$plot_file <- file.path(output_dir, "correlation_plot.png")

  log_info("All results exported to: {output_dir}")
  export_files
}

#' Verify that a run directory contains all expected output files
#'
#' Checks for the core data files that every completed run should contain,
#' plus conditional files based on config settings.
#'
#' @param run_dir Path to the run directory
#' @param config ThematicConfig (or list) used for the run, to check conditional outputs
#' @return List with `expected` (all expected files), `present` (found files),
#'   `missing` (expected but not found), `complete` (logical)
#' @export
verify_run_integrity <- function(run_dir, config = list()) {
  # dispatch on methodology mode. Mode 1 (Reflexive Scaffold)
  # produces a different artifact set from Modes 2/3 (no sentiment, no
  # correlations, no theme_entries directory) -- a unified expected
  # list would silently mark every Mode 1 run as incomplete.
  meth_mode <- .config_methodology_mode(config)
  if (identical(meth_mode, "reflexive_scaffold")) {
    return(.verify_run_integrity_mode1(run_dir, config))
  }

  # Core files every completed run must have.
  # analysis_report.Rmd was incorrectly listed as unconditional. The
  # Rmd is produced only when output$generate_report=TRUE (the writer
  # lives inside generate_report's body). Listing it here caused
  # spurious "Run integrity: 1 file(s) missing -- analysis_report.Rmd"
  # warnings on every legitimate generate_report=FALSE run.
  expected <- c(
    "sentiment_scores.csv",
    "codes.csv",  # renamed from "consolidated_codes.csv" (C2)
    "correlations.csv",
    "themes.json",
    "theme_entries",
    # Tier-0 + Tier-1 outputs that MUST be present in any
    # complete run. Per AC4 (methodology stamped on every output),
    # integrity check must verify these exist -- otherwise a run that
    # silently lost the audit trail would still report complete=TRUE.
    "run_metadata.json",            # T1.5: REDCap-style state record
    "rules/methodology_rules.md",   # T1.6: archived rules text
    "fabrication_log.csv",          # T0.1: anti-fabrication audit trail
    "ai_decisions.jsonl"            # T1.4: AI decision audit log
  )

  # Conditional files based on config
  if (isTRUE(config$output$generate_report)) {
    expected <- c(expected,
                   "analysis_report.html",
                   "analysis_report.Rmd",
                   "styles.css",
                   "theme_details")
  }
  # correlation_plot.png is conditionally produced. Even when
  # generate_correlation_plot=TRUE, create_correlation_plot legitimately
  # skips when the matrix has <2 variables (small samples / sparse
  # theme-membership). Expecting the file unconditionally surfaces a
  # false-positive integrity warning. Treat the plot as expected only
  # when correlations.csv carries at least one data row -- if the
  # correlation stage produced no pairs, the plot is correctly absent.
  if (isTRUE(config$output$generate_correlation_plot)) {
    corr_csv <- file.path(run_dir, "correlations.csv")
    has_corr_rows <- tryCatch({
      if (file.exists(corr_csv)) {
        df <- readr::read_csv(corr_csv, show_col_types = FALSE,
                                comment = "#")
        nrow(df) > 0L
      } else FALSE
    }, error = function(e) FALSE)
    if (has_corr_rows) {
      expected <- c(expected, "correlation_plot.png")
    }
  }
  # T1.4: when raw-response capture is enabled (default TRUE), the
  # response-cache directory MUST exist for the raw-response audit
  # trail to be complete.
  # Honor config$audit$response_cache_dir so a customized cache dir
  # doesn't surface as a false-positive missing artifact.
  if (isTRUE(config$audit$capture_raw_responses %||% TRUE)) {
    expected <- c(expected, config$audit$response_cache_dir %||% "api_responses")
  }
  # Mode 3 must have an archived framework
  # spec at outputs/<run>/framework_applied.{yaml|yml|json}. The extension
  # is dynamic (preserved from the source spec); accept either as
  # satisfying the archive requirement.
  framework_present <- if (identical(meth_mode, "framework_applied")) {
    yaml_exists <- file.exists(file.path(run_dir, "framework_applied.yaml"))
    yml_exists  <- file.exists(file.path(run_dir, "framework_applied.yml"))
    json_exists <- file.exists(file.path(run_dir, "framework_applied.json"))
    yaml_exists || yml_exists || json_exists
  } else NA  # not applicable for non-Mode-3 runs

  present <- expected[file.exists(file.path(run_dir, expected))]
  missing <- setdiff(expected, present)

  # Add framework_applied.* to expected/present/missing so the integrity
  # report reflects it. Done here (not via the standard `expected` list
  # above) because the extension is dynamic.
  if (identical(meth_mode, "framework_applied")) {
    expected <- c(expected, "framework_applied.{yaml|yml|json}")
    if (isTRUE(framework_present)) {
      present <- c(present, "framework_applied.{yaml|yml|json}")
    } else {
      missing <- c(missing, "framework_applied.{yaml|yml|json}")
    }
  }

  list(
    expected = expected,
    present = present,
    missing = missing,
    complete = length(missing) == 0
  )
}

#' Export CSV files for each theme's entries
#'
#' @param data tibble with theme_membership_* or emerged_themes columns
#' @param theme_set ThemeSet object
#' @param output_dir Output directory
#' @param methodology_mode Optional methodology mode (T1.7). When
#'   non-NULL, every CSV produced is stamped with a comment header
#'   identifying the mode and run id (per AC4). NULL skips stamping --
#'   used by tests / legacy callers.
#' @return Named list of file info per theme
export_theme_entry_csvs <- function(data, theme_set, output_dir,
                                      methodology_mode = NULL) {
  theme_dir <- file.path(output_dir, "theme_entries")
  dir.create(theme_dir, recursive = TRUE, showWarnings = FALSE)

  theme_csv_files <- list()
  # T0.2: include std_author so per-theme CSVs preserve the contributor data
  # the participant-spread metrics on the dashboard were computed from. Per
  # AC4 (methodology stamped on every output), Tier-0-relevant columns
  # propagate to all output artifacts -- silent omission would let a
  # downstream consumer recompute participant spread from the wrong shape.
  # subtheme_assignments was missing from this
  # export whitelist even though cascade_theme_assignments populates the
  # column in analytic_data. Downstream consumers (paper-style
  # subtheme tables, researcher manual review) had no way to reconstruct
  # which subtheme each entry belonged to from the per-theme CSV alone.
  # Adding it here makes the per-theme + master CSVs self-describing.
  export_cols <- intersect(
    c("std_id", "std_text", "std_author", "sentiment_score", "all_emotions",
      "emotion_intensity", "emerged_themes", "n_themes", "subtheme_assignments",
      "source_table"),
    names(data)
  )

  for (tn in theme_names(theme_set)) {
    # Use multi-label membership column to find all entries in this theme
    safe_col <- paste0("theme_membership_", make.names(tn))
    if (safe_col %in% names(data)) {
      entries <- data[data[[safe_col]] == 1L, ]
    } else if ("emerged_themes" %in% names(data)) {
      entries <- data[!is.na(data$emerged_themes) &
                       .entry_in_theme(data$emerged_themes, tn), ]
    } else {
      next
    }
    if (nrow(entries) == 0) next

    safe_name <- make_safe_filename(tn)
    csv_path <- file.path(theme_dir, paste0(safe_name, ".csv"))
    readr::write_csv(entries[, intersect(export_cols, names(entries)), drop = FALSE], csv_path)
    # T1.7 (AC4): stamp the file with the methodology mode so any
    # downstream consumer parsing the CSV sees the declaration up-front.
    # The stamp is a comment-style header line; readr::read_csv with
    # comment = "#" strips it transparently.
    if (!is.null(methodology_mode)) {
      tryCatch(stamp_methodology_csv(csv_path, methodology_mode,
                                       run_id = basename(output_dir)),
               error = function(e) log_debug("CSV stamp skipped: {e$message}"))
    }

    theme_csv_files[[tn]] <- list(
      file_path = csv_path,
      relative_path = file.path("theme_entries", paste0(safe_name, ".csv"))
    )
  }

  # Master CSV with all entries that have any theme assignment
  master_path <- file.path(theme_dir, "all_entries_by_theme.csv")
  master_data <- data |>
    filter(!is.na(emerged_themes)) |>
    arrange(emerged_themes) |>
    select(any_of(export_cols))
  readr::write_csv(master_data, master_path)
  if (!is.null(methodology_mode)) {
    tryCatch(stamp_methodology_csv(master_path, methodology_mode,
                                     run_id = basename(output_dir)),
             error = function(e) log_debug("CSV stamp skipped: {e$message}"))
  }

  log_info("Exported {length(theme_csv_files)} theme CSV files + master CSV")
  theme_csv_files
}

#' Export per-theme paper-style subtheme-summary CSVs
#'
#' Complement to \code{export_theme_entry_csvs}: for each theme, writes
#' a CSV with ONE ROW PER REAL SUBTHEME with paper-style columns
#' (Subtheme name, description, n, Median+MAD + Mean+SD per auto-
#' detected metric, examples of comments tagged with metric values).
#'
#' Output structure:
#' \itemize{
#'   \item \code{theme_summaries/<safe_theme_name>.csv} -- one per theme
#'         with non-empty subtheme_stats
#'   \item \code{theme_summaries/all_subthemes.csv} -- master with
#'         theme_name + subtheme rows from every theme
#' }
#'
#' Themes with no real subthemes (only the virtual NA-named wrapper)
#' OR with empty subtheme_stats are skipped -- they're already covered
#' by the per-entry CSVs and the theme card.
#'
#' @param theme_stats Per-theme stats list from
#'   \code{aggregate_theme_statistics()} (must carry
#'   \code{subtheme_stats} + \code{metric_cols}).
#' @param output_dir Run directory.
#' @param methodology_mode Optional methodology mode for AC4 stamping.
#' @return Named list of file info per theme.
#' @export
export_theme_subtheme_summary_csvs <- function(theme_stats, output_dir,
                                                 methodology_mode = NULL) {
  summ_dir <- file.path(output_dir, "theme_summaries")
  dir.create(summ_dir, recursive = TRUE, showWarnings = FALSE)

  files <- list()
  master_rows <- list()

  for (tn in names(theme_stats)) {
    ts <- theme_stats[[tn]]
    st_stats <- ts$subtheme_stats %||% list()
    if (length(st_stats) == 0L) next  # no real subthemes / virtual-only
    metric_cols <- ts$metric_cols %||% character(0)

    # Build a row-wise tibble: one row per subtheme.
    rows <- lapply(names(st_stats), function(snm) {
      s <- st_stats[[snm]]
      row <- list(
        theme           = tn,
        subtheme        = s$name %||% snm,
        description     = s$description %||% "",
        n               = as.integer(s$n %||% 0L)
      )
      for (mc in metric_cols) {
        ms <- s$metric_stats[[mc]] %||% list()
        row[[paste0(mc, "_median")]] <- as.numeric(ms$median %||% NA_real_)
        row[[paste0(mc, "_mad")]]    <- as.numeric(ms$mad    %||% NA_real_)
        row[[paste0(mc, "_mean")]]   <- as.numeric(ms$mean   %||% NA_real_)
        row[[paste0(mc, "_sd")]]     <- as.numeric(ms$sd     %||% NA_real_)
        row[[paste0(mc, "_n_obs")]]  <- as.integer(ms$n_observed %||% 0L)
      }
      row$examples_of_comments <- paste(s$example_quotes %||% character(0),
                                          collapse = " || ")
      row
    })
    df <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))

    safe_name <- make_safe_filename(tn)
    csv_path <- file.path(summ_dir, paste0(safe_name, ".csv"))
    readr::write_csv(df, csv_path)
    if (!is.null(methodology_mode)) {
      tryCatch(stamp_methodology_csv(csv_path, methodology_mode,
                                       run_id = basename(output_dir)),
               error = function(e) log_debug("CSV stamp skipped: {e$message}"))
    }
    files[[tn]] <- list(
      file_path     = csv_path,
      relative_path = file.path("theme_summaries", paste0(safe_name, ".csv"))
    )
    master_rows[[tn]] <- df
  }

  if (length(master_rows) > 0L) {
    # Some themes may have different metric column sets when the data
    # changes across themes (e.g., test fixtures). dplyr::bind_rows
    # handles the union safely; rbind would error on column mismatch.
    master_df <- dplyr::bind_rows(master_rows)
    master_path <- file.path(summ_dir, "all_subthemes.csv")
    readr::write_csv(master_df, master_path)
    if (!is.null(methodology_mode)) {
      tryCatch(stamp_methodology_csv(master_path, methodology_mode,
                                       run_id = basename(output_dir)),
               error = function(e) log_debug("CSV stamp skipped: {e$message}"))
    }
  }

  log_info("Exported {length(files)} per-theme subtheme-summary CSV(s)")
  files
}

#' Generate the full HTML analysis report
#'
#' Builds an Rmd file from data and renders it to HTML.
#'
#' @param data tibble with all analysis columns
#' @param theme_set ThemeSet object
#' @param correlations_df Correlations tibble
#' @param insights Insights list
#' @param export_files List of export file paths
#' @param consolidated ConsolidatedCodes list (or NULL)
#' @param learning_context LearningContext object (or NULL)
#' @param provider AIProvider object (or NULL)
#' @param config ThematicConfig object (or NULL)
#' @param output_file Path for the HTML report
#' @param irr_result Inter-rater reliability result list (or NULL)
#' @param comparison_result ComparisonResult object from compare_runs() (or NULL)
#' @param self_contained If TRUE (default), produce a self-contained HTML file
#'   with all resources embedded. Set to FALSE for faster rendering and smaller
#'   file size (external CSS/JS will be referenced).
#' @param coding_results Legacy CodingResults list as returned by
#'   \code{as_coding_results}. Used to populate per-theme entry tables with
#'   the codes assigned to each entry. Pass NULL to omit the codes column.
#' @param coding_state ProgressiveCodingState with saturation data (or NULL)
#' @param excerpt_verification Optional list returned by \code{verify_excerpts}
#'   containing substring_stats and (optionally) coherence_stats. When
#'   provided, the report's data-quality appendix shows excerpt validation
#'   results.
#' @param theme_group_tests Optional tibble returned by
#'   \code{compare_theme_groups} (Mann-Whitney U tests). When provided, the
#'   correlation section gains a 'Theme Group Comparisons' subsection.
#' @param cooccurrence_tests Optional tibble returned by
#'   \code{test_theme_cooccurrence} (chi-square / Fisher tests). When
#'   provided, the correlation section gains a 'Theme Co-occurrence'
#'   subsection.
#' @param audit_log Optional \code{AuditLog} object (T1.4) forwarded to
#'   \code{generate_ai_synthesis} so the executive-summary AI call is
#'   recorded as an \code{ai_request} audit decision.
#' @param response_cache Optional \code{ResponseCache} object (T1.4)
#'   forwarded to \code{generate_ai_synthesis} so the raw API response is
#'   written to the cache and referenced from the audit log.
#' @param coverage Optional \code{CorpusCoverage} object (T0.3) from
#'   \code{\link{compute_corpus_coverage}}. When provided, the report
#'   renders a Tier-0 corpus-coverage card asserting that every entry
#'   surviving preprocessing reached the LLM (entry-level coverage;
#'   within-entry truncation is measured and disclosed when tracked).
#'   When NULL the card renders an explicit "coverage not computed"
#'   notice rather than silently omitting -- absence is itself a
#'   transparency signal per AC4.
#' @param framework_spec Optional \code{FrameworkSpec} object (Mode 3
#'   only). When provided AND \code{config$methodology$mode} is
#'   \code{"framework_applied"}, the report renders a Framework
#'   Declaration section with the framework's name, citations,
#'   epistemic stance, anomaly handling policy, and full constructs
#'   list. NULL on Mode 1 / Mode 2 runs.
#' @param framework_archive Optional named list returned by
#'   \code{\link{archive_framework_spec}} carrying the archived
#'   framework's path + sha256 hash. When provided alongside
#'   \code{framework_spec}, the Framework Declaration section
#'   includes the sha256 fingerprint and a link to the archived spec.
#' @param metric_interpretation Optional \code{MetricInterpretation} (from the
#'   Methodology Assistant). Threaded to \code{aggregate_theme_statistics} so
#'   per-subtheme stats are computed via the AI's chosen primitives (per-column,
#'   matched by name) with the legacy battery as fallback. NULL -> legacy only.
#' @param methodology_articulations Optional \code{MethodologyArticulations}
#'   bundle. Drives the report's "Methodology Setup" section
#'   (relevance criterion + per-metric interpretations + per-theme temporal
#'   panel). When NULL, the renderer falls back to reading the archived
#'   \code{rules/methodology_articulations.json} under the output dir if present;
#'   when neither is available the section is omitted. When supplied and
#'   \code{metric_interpretation} is NULL, the metric interpretation is derived
#'   from this bundle.
#' @param research_coverage Optional \code{ResearchCoverage} object (Mode 2
#'   only). Drives the "Research-question coverage" section -- where each named
#'   focus facet landed across the themes. When NULL, the renderer falls back to
#'   the archived \code{rules/research_coverage.json} under the output dir if
#'   present; when neither is available (or no separable facets were found) the
#'   section is omitted.
#' @param temporal_results Optional list from
#'   \code{\link{analyze_temporal_patterns}}. Drives the "Longitudinal
#'   Patterns" section (period granularity, theme prevalence over time,
#'   theme emergence). Each chart is embedded only when its PNG exists in
#'   the output directory (the prevalence chart requires more than one time
#'   period; the emergence chart at least one dated theme). NULL -> the
#'   section is omitted.
#' @return Path to generated HTML report
#' @export
generate_report <- function(data, theme_set, correlations_df, insights,
                             export_files, consolidated = NULL,
                             learning_context = NULL, provider = NULL,
                             config = NULL, output_file = "analysis_report.html",
                             irr_result = NULL, comparison_result = NULL,
                             self_contained = TRUE, coding_results = NULL,
                             coding_state = NULL,
                             excerpt_verification = NULL,
                             theme_group_tests = NULL,
                             cooccurrence_tests = NULL,
                             audit_log = NULL,
                             response_cache = NULL,
                             coverage = NULL,
                             framework_spec = NULL,
                             framework_archive = NULL,
                             metric_interpretation = NULL,
                             methodology_articulations = NULL,
                             research_coverage = NULL,
                             temporal_results = NULL) {
  validate_class(theme_set, "ThemeSet")

  # when the full articulations bundle is supplied but the
  # metric_interpretation wasn't passed separately, derive it from the bundle
  # so the per-subtheme stats path (61.3b) and the Methodology Setup section
  # (61.4) share one source. Passing metric_interpretation explicitly (the
  # current pipeline wiring) still wins -- this only fills a NULL.
  if (is.null(metric_interpretation) &&
      inherits(methodology_articulations, "MethodologyArticulations")) {
    metric_interpretation <- methodology_articulations$metric_interpretation
  }

  # Validate inputs
  stopifnot(
    is.data.frame(data),
    is.data.frame(correlations_df) || is.null(correlations_df)
  )

  log_info("Generating HTML report...")
  tic("Report generation")

  # Aggregate statistics. Thread the user's
  # quotes_per_theme config through; was previously hardcoded to 3
  # at R/16_report_helpers.R:106 + R/13_themes.R:793 even when the
  # user set a different value in config$analysis$themes$quotes_per_theme.
  theme_stats <- aggregate_theme_statistics(data, theme_set, consolidated,
                                              quotes_per_theme = config$analysis$themes$quotes_per_theme %||% 3L,
                                              config = config,
                                              metric_interpretation = metric_interpretation)
  overall_stats <- aggregate_overall_statistics(data, theme_set, consolidated,
                                                 learning_context, config)

  # AI synthesis
  ai_synthesis <- generate_ai_synthesis(overall_stats, theme_stats, correlations_df,
                                         insights, theme_set, provider,
                                         config = config,
                                         audit_log = audit_log,
                                         response_cache = response_cache)

  # Correlation interpretation
  corr_interpretation <- interpret_correlations(correlations_df, theme_stats)

  # Theme ordering. Under Mode 3 the theme_set may contain both
  # framework themes (deductive, primary) and emergent themes (inductive,
  # derived from anomaly residuals). Order them by kind first
  # (framework -> emergent -> anomaly_bracket) and within each kind by
  # prevalence DESC. Mode 2 themes default to theme_kind = "framework"
  # via aggregate_theme_statistics, so this ordering is a no-op for them.
  .theme_kind_rank <- function(k) {
    switch(k %||% "framework",
      framework = 0L, emergent = 1L, anomaly_bracket = 2L, 3L)
  }
  themes_with_kind <- overall_stats$themes
  themes_with_kind$theme_kind_rank <- vapply(
    themes_with_kind$theme_name,
    function(tn) .theme_kind_rank(theme_stats[[tn]]$theme_kind),
    integer(1)
  )
  theme_order <- themes_with_kind |>
    arrange(.data$theme_kind_rank, desc(.data$n)) |>
    pull(.data$theme_name)

  # Determine output paths
  output_dir <- dirname(output_file)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  rmd_file <- gsub("\\.html$", ".Rmd", output_file)

  # Build the R Markdown content
  rmd_content <- .build_rmd_content(
    overall_stats = overall_stats,
    theme_stats = theme_stats,
    theme_order = theme_order,
    ai_synthesis = ai_synthesis,
    corr_interpretation = corr_interpretation,
    insights = insights,
    export_files = export_files,
    config = config,
    irr_result = irr_result,
    comparison_result = comparison_result,
    self_contained = self_contained,
    theme_group_tests = theme_group_tests,
    cooccurrence_tests = cooccurrence_tests,
    excerpt_verification = excerpt_verification,
    coding_state = coding_state,
    coverage = coverage,
    run_id = basename(output_dir),
    framework_spec = framework_spec,
    framework_archive = framework_archive,
    # the methodology articulations bundle drives the
    # "Methodology Setup" section + per-theme temporal panel.
    methodology_articulations = methodology_articulations,
    # pass output_dir so the T0.1 dashboard can
    # find fabrication_log.csv to count pre-rejection fabrications.
    output_dir = output_dir,
    # research-question coverage section (Mode 2).
    research_coverage = research_coverage,
    # longitudinal patterns section (charts gated on on-disk existence).
    temporal_results = temporal_results
  )

  # Write Rmd (collapse to single string to prevent duplication)
  rmd_content <- paste(rmd_content, collapse = "\n")
  writeLines(rmd_content, rmd_file)
  log_info("R Markdown file written: {rmd_file}")

  # Copy CSS to output directory
  css_src <- system.file("rmd", "styles.css", package = "pakhom")
  if (nchar(css_src) > 0 && file.exists(css_src)) {
    file.copy(css_src, file.path(output_dir, "styles.css"), overwrite = TRUE)
  }

  # Generate separate theme detail HTML files. Compute the methodology mode
  # here (AC4) so each standalone detail page carries the same methodology
  # badge as the main report -- mirroring the per-theme CSVs below.
  meth_mode <- .config_methodology_mode(config)
  theme_detail_files <- .generate_theme_detail_htmls(
    theme_stats, theme_order, export_files, output_dir,
    data = data, coding_results = coding_results,
    methodology_mode = meth_mode
  )

  # paper-style per-theme subtheme summary CSVs. One CSV per
  # theme (one row per subtheme; columns include Median(MAD) + Mean(SD)
  # per auto-detected metric + examples-of-comments) plus a master
  # all_subthemes.csv. Methodology-stamped per AC4 when in a real run.
  tryCatch(
    export_theme_subtheme_summary_csvs(
      theme_stats     = theme_stats,
      output_dir      = output_dir,
      methodology_mode = meth_mode
    ),
    error = function(e) {
      log_warn("Per-theme subtheme-summary CSV export skipped: {e$message}")
      list()
    }
  )

  # Ensure pandoc is available (RStudio bundles it, but CLI runs may not find it)
  if (!rmarkdown::pandoc_available()) {
    rstudio_pandoc <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"
    if (dir.exists(rstudio_pandoc)) {
      # CRAN politeness: set RSTUDIO_PANDOC only for this render and restore the
      # caller's value on exit, rather than permanently mutating their session env.
      old_pandoc <- Sys.getenv("RSTUDIO_PANDOC", unset = NA_character_)
      on.exit(
        if (is.na(old_pandoc)) Sys.unsetenv("RSTUDIO_PANDOC")
        else Sys.setenv(RSTUDIO_PANDOC = old_pandoc),
        add = TRUE
      )
      Sys.setenv(RSTUDIO_PANDOC = rstudio_pandoc)
      log_info("Using RStudio-bundled pandoc: {rstudio_pandoc}")
    }
  }

  # Render (use normalizePath to ensure absolute paths for CLI contexts)
  abs_output_dir <- normalizePath(output_dir, mustWork = TRUE)
  abs_rmd_file <- normalizePath(rmd_file, mustWork = TRUE)
  tryCatch({
    rmarkdown::render(
      abs_rmd_file,
      output_file = basename(output_file),
      output_dir = abs_output_dir,
      knit_root_dir = abs_output_dir,
      quiet = TRUE
    )
    log_info("HTML report generated: {output_file}")
  }, error = function(e) {
    log_error("Could not render HTML report: {e$message}")
    log_info("R Markdown file saved for manual rendering: {rmd_file}")
  })

  toc()

  # Return NULL if the report file was not actually created
  if (!file.exists(output_file)) {
    log_error("Report file not created: {output_file}")
    return(NULL)
  }

  output_file
}

# ==============================================================================
# Internal: Build R Markdown content
# ==============================================================================

.build_rmd_content <- function(overall_stats, theme_stats, theme_order,
                                ai_synthesis, corr_interpretation, insights,
                                export_files, config, irr_result = NULL,
                                comparison_result = NULL,
                                self_contained = TRUE,
                                theme_group_tests = NULL,
                                cooccurrence_tests = NULL,
                                excerpt_verification = NULL,
                                coding_state = NULL,
                                coverage = NULL,
                                run_id = NULL,
                                framework_spec = NULL,
                                framework_archive = NULL,
                                methodology_articulations = NULL,
                                output_dir = NULL,
                                research_coverage = NULL,
                                temporal_results = NULL) {

  theme_count <- length(theme_stats)

  # --- YAML header ---
  safe_focus <- gsub("'", "''", .html_esc(overall_stats$research_focus))
  sc_flag <- if (isTRUE(self_contained)) "true" else "false"
  content <- paste0(
    "---\n",
    "title: 'Thematic Analysis Report'\n",
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
    # mathjax: null keeps the report genuinely offline: rmarkdown otherwise
    # injects its default MathJax from a remote CDN even when self_contained,
    # contradicting the no-CDN / offline claim. The report contains no LaTeX
    # math, so disabling it has no visible effect.
    "    mathjax: null\n",
    "    css: styles.css\n",
    "---\n\n"
  )

  # --- Setup chunk ---
  content <- paste0(content,
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,\n",
    "                      fig.width = 10, fig.height = 5.5, dpi = 150,\n",
    "                      error = TRUE)\n",
    "library(dplyr)\n",
    "library(ggplot2)\n",
    "library(readr)\n",
    "library(knitr)\n",
    "library(scales)\n",
    "has_dt <- requireNamespace('DT', quietly = TRUE)\n\n",
    .ggplot_theme_code(),
    "\n```\n\n"
  )

  # --- Executive Summary ---
  sentiment_class <- if (is.na(overall_stats$sentiment$mean)) "neutral"
    else if (overall_stats$sentiment$mean < .SENTIMENT_NEGATIVE_THRESHOLD) "negative"
    else if (overall_stats$sentiment$mean > .SENTIMENT_POSITIVE_THRESHOLD) "positive"
    else "neutral"

  # T1.7 (AC4): methodology stamp at the top of the report so a reviewer
  # who picks up the rendered HTML sees the mode declaration before the
  # substantive analysis. Built from config (the canonical source) with
  # a fallback "Unknown methodology" for legacy runs without a config.
  meth_mode <- .config_methodology_mode(config)
  content <- paste0(content,
    stamp_methodology_html(meth_mode, run_id = run_id), '\n'
  )

  # Epistemic-honesty caveat for the standalone HTML artifact: in Mode 2/3 the
  # codes + themes + summary are AI-generated and researcher-gated. A reviewer
  # who picks up the rendered report detached from the docs should see this
  # in-body (Mode 1's themes are researcher-authored, so it does not apply there).
  ai_artifact_caveat <- if ((config$methodology$mode %||% "") %in%
                            c("codebook_collaborative", "framework_applied")) {
    paste0('<p class="ai-artifact-caveat"><em>The codes, themes, and this summary ',
           'are AI-generated analytic artifacts produced under the declared ',
           'methodology, intended for researcher review and curation -- not a ',
           'substitute for the researcher\'s own immersion and interpretive ',
           'judgement. The provenance, coverage, and methodology disclosures below ',
           'document how they were produced.</em></p>\n\n')
  } else ""
  content <- paste0(content,
    '<div class="hero-section">\n',
    '\n# Executive Summary\n\n',
    .sanitize_ai_prose(ai_synthesis$executive_summary), '\n',
    '</div>\n\n',
    ai_artifact_caveat,
    .build_metrics_dashboard(overall_stats, theme_count, sentiment_class,
                             comparison_result = comparison_result),
    '\n\n'
  )

  # T0.1 part 3: Tier-0 Data Integrity Dashboard. Renders immediately under
  # the executive summary so reviewers see verification results before
  # reading themes. Reads coding_state's per-segment $provenance fields
  # (populated by pipeline wiring); falls back gracefully when coding_state
  # is missing / pre-T0.1.
  tier0_stats <- compute_quote_provenance_stats(coding_state)
  # pass the absolute fabrication-log path so the
  # dashboard can count pre-rejection fabrications (the surviving
  # population in `tier0_stats` is post-rejection, so it always
  # reports 0 caught fabrications regardless of how many the ladder
  # actually dropped during coding). output_dir is NULL for legacy
  # callers / tests that don't supply it; in that case the dashboard
  # falls back to the (legacy, post-rejection) stats path.
  fab_log_abs <- if (!is.null(output_dir)) {
    file.path(output_dir, "fabrication_log.csv")
  } else NULL
  content <- paste0(content,
    .build_tier0_dashboard(tier0_stats,
                           fabrication_log_relpath = "fabrication_log.csv",
                           config = config,
                           fabrication_log_path = fab_log_abs)
  )

  # T0.3 corpus coverage assertion. Pairs with T0.1: T0.1 says "no
  # fabrications", T0.3 says "no silent truncation". Both are Tier-0
  # transparency cards rendered before the substantive analysis so
  # reviewers see the integrity claims first. coverage is NULL on
  # legacy/test report calls -- the generic dispatches to the
  # "unavailable" default rather than crashing or omitting silently.
  content <- paste0(content,
    render_tier0_coverage_card(coverage)
  )

  # Framework Declaration section. Renders
  # ONLY when Mode 3 + the framework_spec is present. Carries the
  # framework's name, citations, epistemic stance, anomaly handling
  # policy, and full constructs list -- so a reviewer reading the
  # report knows exactly which theoretical framework was applied,
  # along with the sha256 hash of the archived framework_applied.yaml
  # for replay-equivalence checks. Per AC4, this is mandatory for any
  # Mode 3 run; absence (e.g., archive failed earlier) renders an
  # explicit "framework archive not available" notice.
  if (identical(meth_mode, "framework_applied")) {
    content <- paste0(content,
      .build_framework_declaration(framework_spec, framework_archive)
    )
  }

  # Methodology Setup section. The AI analyst's run-start
  # articulations -- the relevance criterion that operationalized "on-focus"
  # for this study, plus the per-metric interpretations (which primitives are
  # honest for each column + how to read them). This is the peer-review
  # transparency artifact for the "AI as analyst with calculator" architecture:
  # a reviewer sees the AI's interpretive framework before the findings. Sits
  # alongside the Framework Declaration as a "declaration of method" section.
  # Source resolution: prefer the in-memory bundle; else fall back to the
  # archived rules/methodology_articulations.json under the run dir (so a
  # resume / direct generate_report() call still surfaces it). Omitted when
  # neither is available (legacy / Mode 1 / earlier runs).
  ms_art <- methodology_articulations
  if (is.null(ms_art) && !is.null(output_dir)) {
    ms_art <- .load_methodology_articulations_from_run_dir(output_dir)
  }
  if (!is.null(ms_art)) {
    content <- paste0(content, .build_methodology_setup_section(ms_art))
  }

  # Research-question coverage section -- where each named focus facet
  # landed across the themes. Prefer the in-memory object; else the archived
  # rules/research_coverage.json (resume / direct generate_report()). The
  # builder returns "" when there is no object or no separable facets, so this
  # is a no-op (byte-identical) on Mode 3 / legacy / vague-focus runs.
  rc_cov <- research_coverage
  if (is.null(rc_cov) && !is.null(output_dir)) {
    rc_cov <- .load_research_coverage_from_run_dir(output_dir)
  }
  content <- paste0(content, .build_research_coverage_section(rc_cov))

  # Inline data overview context into executive summary (Issue 4)
  rc <- overall_stats$research_context
  rc_text <- if (!is.null(rc) && nzchar(trimws(rc))) {
    paste0(" using data from ", .html_esc(rc))
  } else {
    ""
  }
  content <- paste0(content,
    "This analysis examines **", .html_esc(overall_stats$research_focus),
    "**", rc_text,
    ". The analysis was conducted on ", format(overall_stats$analysis_date, "%B %d, %Y"), ".\n\n"
  )
  if (!is.null(overall_stats$source_breakdown)) {
    content <- paste0(content,
      "| Source | Count | Percentage |\n",
      "|--------|------:|----------:|\n"
    )
    for (i in seq_len(nrow(overall_stats$source_breakdown))) {
      row <- overall_stats$source_breakdown[i, ]
      content <- paste0(content,
        "| ", .md_cell(row$source_table), " | ", format(row$n, big.mark = ","),
        " | ", row$pct, "% |\n"
      )
    }
    content <- paste0(content, "\n")
  }
  if (!is.null(overall_stats$learning)) {
    content <- paste0(content,
      "The AI analysis was informed by **", overall_stats$learning$n_studies,
      " previous studies** (",
      format(overall_stats$learning$context_characters, big.mark = ","),
      " characters of learning context).\n\n"
    )
  }
  content <- paste0(content,
    '<div class="download-box">\n',
    '**Quick Download:** <a href="theme_entries/all_entries_by_theme.csv" class="download-link" download>',
    'Download all ', overall_stats$total_entries, ' entries as CSV</a>\n',
    '</div>\n\n'
  )

  # --- Learning Transparency ---
  if (!is.null(overall_stats$learning)) {
    content <- paste0(content, .build_learning_transparency(overall_stats$learning))
  }

  # --- Emotional Landscape ---
  content <- paste0(content, .build_emotional_landscape(overall_stats, export_files))

  # --- Thematic Analysis ---
  # pass config so the section honors
  # analysis$themes$max_inline_themes (top-N inline cards + compact
  # rows for the remainder). Prevents pandoc OOM at scale.
  content <- paste0(content, .build_thematic_section(
    theme_stats, theme_order, theme_count, export_files, config = config
  ))

  # --- Correlation Analysis ---
  content <- paste0(content, .build_correlation_section(corr_interpretation, export_files,
                                                         theme_group_tests = theme_group_tests,
                                                         cooccurrence_tests = cooccurrence_tests))

  # --- Cross-Run Comparison ---
  if (!is.null(comparison_result) && inherits(comparison_result, "ComparisonResult")) {
    content <- paste0(content, .build_comparison_section(comparison_result))
  }

  # --- Synthesis & Conclusion (merged, Issue 12) ---
  content <- paste0(content, .build_synthesis_section(insights, ai_synthesis = ai_synthesis))

  # --- Human Verification / IRR ---
  if (!is.null(irr_result) && !is.null(irr_result$irr_stats)) {
    content <- paste0(content, .build_irr_section(irr_result))
  }

  # --- Thematic Saturation ---
  if (!is.null(coding_state) && !is.null(coding_state$saturation)) {
    content <- paste0(content, .build_saturation_section(coding_state))
  }

  # --- Longitudinal Patterns ---
  if (!is.null(temporal_results) && isTRUE(temporal_results$has_temporal_data)) {
    content <- paste0(content,
                      .build_longitudinal_section(temporal_results, output_dir))
  }

  # --- Appendix A: Methodology ---
  content <- paste0(content, .build_methodology_appendix(overall_stats, export_files, config,
                                                          excerpt_verification = excerpt_verification))

  # --- Appendix B: Theme Details ---
  content <- paste0(content,
    "# Appendix B: Theme Details {#theme-details-appendix}\n\n",
    "Each theme has its own interactive detail page with full entry data, searchable tables, ",
    "and downloadable CSVs.\n\n"
  )
  theme_idx <- 0
  for (tn in theme_order) {
    if (!tn %in% names(theme_stats)) next
    theme_idx <- theme_idx + 1
    ts <- theme_stats[[tn]]
    safe_fn <- make_safe_filename(tn)
    sent_mean <- if (!is.null(ts$sentiment$mean) && !is.na(ts$sentiment$mean)) {
      ts$sentiment$mean
    } else {
      0
    }
    sent_label <- if (sent_mean < .SENTIMENT_NEGATIVE_THRESHOLD) "negative"
      else if (sent_mean > .SENTIMENT_POSITIVE_THRESHOLD) "positive"
      else "mixed"
    content <- paste0(content,
      theme_idx, ". **[", .html_esc(tn), "](theme_details/theme_", safe_fn, ".html)** -- ",
      ts$n_entries, " entries, ", sent_label, " sentiment (mean: ", round(sent_mean, 2), ")\n"
    )
  }
  content <- paste0(content, "\n")

  # --- Appendix C: Downloads ---
  content <- paste0(content, generate_downloads_section(export_files, theme_stats))

  # --- Footer ---
  content <- paste0(content,
    "---\n\n",
    "<p style='text-align: center; color: var(--text-muted); font-size: 0.85rem; margin-top: 3rem;'>",
    "Report generated on ",
    format(Sys.time(), "%Y-%m-%d at %H:%M:%S", tz = "UTC"), " UTC",
    " using ", tryCatch(
      paste0("pakhom v", as.character(utils::packageVersion("pakhom"))),
      error = function(e) "pakhom"
    ), " by <a href='https://www.linkedin.com/in/abanoubarmanious/' target='_blank' style='color: inherit;'>Abanoub J. Armanious, MS</a></p>\n"
  )

  content
}

# ==============================================================================
# Internal: Section builders
# ==============================================================================

.build_metrics_dashboard <- function(stats, n_themes, sentiment_class,
                                     comparison_result = NULL) {
  # Minimal inline SVG icons (24x24, currentColor)
  icon_entries <- '<span class="metric-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M14 2H6a2 2 0 0 0-2 2v16a2 2 0 0 0 2 2h12a2 2 0 0 0 2-2V8z"/><polyline points="14 2 14 8 20 8"/><line x1="16" y1="13" x2="8" y2="13"/><line x1="16" y1="17" x2="8" y2="17"/><polyline points="10 9 9 9 8 9"/></svg></span>'
  icon_themes <- '<span class="metric-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><rect x="3" y="3" width="7" height="7"/><rect x="14" y="3" width="7" height="7"/><rect x="14" y="14" width="7" height="7"/><rect x="3" y="14" width="7" height="7"/></svg></span>'
  icon_codes <- '<span class="metric-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M20.59 13.41l-7.17 7.17a2 2 0 0 1-2.83 0L2 12V2h10l8.59 8.59a2 2 0 0 1 0 2.82z"/><line x1="7" y1="7" x2="7.01" y2="7"/></svg></span>'
  icon_sentiment <- '<span class="metric-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><line x1="12" y1="20" x2="12" y2="10"/><line x1="18" y1="20" x2="18" y2="4"/><line x1="6" y1="20" x2="6" y2="16"/></svg></span>'
  icon_negative <- '<span class="metric-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="23 18 13.5 8.5 8.5 13.5 1 6"/><polyline points="17 18 23 18 23 12"/></svg></span>'
  icon_positive <- '<span class="metric-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><polyline points="23 6 13.5 15.5 8.5 10.5 1 18"/><polyline points="17 6 23 6 23 12"/></svg></span>'

  cards <- paste0(
    '<div class="metrics-grid">\n',
    '<div class="metric-card">\n',
    icon_entries, '\n',
    '<div class="metric-value">', format(stats$total_entries, big.mark = ","), '</div>\n',
    '<div class="metric-label">Entries Analyzed</div>\n',
    '</div>\n',
    '<div class="metric-card">\n',
    icon_themes, '\n',
    '<div class="metric-value">', n_themes, '</div>\n',
    '<div class="metric-label">Themes Identified</div>\n',
    '</div>\n',
    '<div class="metric-card">\n',
    icon_codes, '\n',
    '<div class="metric-value">', stats$coding$total_unique_codes, '</div>\n',
    '<div class="metric-label">Unique Codes</div>\n',
    '</div>\n',
    '<div class="metric-card ', sentiment_class, '">\n',
    icon_sentiment, '\n',
    '<div class="metric-value">', stats$sentiment$mean, '</div>\n',
    '<div class="metric-label">Mean Sentiment</div>\n',
    '</div>\n',
    '<div class="metric-card negative">\n',
    icon_negative, '\n',
    '<div class="metric-value">', stats$sentiment$pct_negative, '%</div>\n',
    '<div class="metric-label">Negative</div>\n',
    '</div>\n',
    '<div class="metric-card positive">\n',
    icon_positive, '\n',
    '<div class="metric-value">', stats$sentiment$pct_positive, '%</div>\n',
    '<div class="metric-label">Positive</div>\n',
    '</div>\n'
  )

  # Issue 13: optional stability metric card from cross-run comparison
  if (!is.null(comparison_result)) {
    stability_info <- tryCatch({
      # Prefer theme stability rate (how many entries kept same theme across runs)
      if (!is.null(comparison_result$entry_migration) &&
          !is.na(comparison_result$entry_migration$stability_rate)) {
        list(
          value = round(comparison_result$entry_migration$stability_rate * 100, 1),
          label = "Theme Stability"
        )
      } else if (!is.null(comparison_result$sample_overlap) &&
                 !is.null(comparison_result$sample_overlap$pairwise) &&
                 !is.na(comparison_result$sample_overlap$pairwise$jaccard_index %||% NA)) {
        list(
          value = round(comparison_result$sample_overlap$pairwise$jaccard_index * 100, 1),
          label = "Sample Overlap (Jaccard)"
        )
      } else {
        NULL
      }
    }, error = function(e) NULL)

    if (!is.null(stability_info)) {
      icon_stability <- '<span class="metric-icon"><svg xmlns="http://www.w3.org/2000/svg" width="24" height="24" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M12 22s8-4 8-10V5l-8-3-8 3v7c0 6 8 10 8 10z"/></svg></span>'
      cards <- paste0(cards,
        '<div class="metric-card">\n',
        icon_stability, '\n',
        '<div class="metric-value">', stability_info$value, '%</div>\n',
        '<div class="metric-label">', stability_info$label, '</div>\n',
        '</div>\n'
      )
    }
  }

  paste0(cards, '</div>\n')
}


.build_irr_section <- function(irr_result) {
  stats <- irr_result$irr_stats

  alpha_val    <- stats$krippendorff_alpha %||% NA_real_
  alpha_interp <- stats$alpha_interpretation %||% "N/A"
  ci_lo <- stats$alpha_ci_low %||% NA_real_
  ci_hi <- stats$alpha_ci_high %||% NA_real_
  ci_str <- if (!is.na(ci_lo) && !is.na(ci_hi))
    paste0(" (95% CI [", ci_lo, ", ", ci_hi, "])") else ""
  n_codes <- stats$n_codes %||% NA_integer_
  kappa_label <- if (!is.na(n_codes))
    paste0("Mean per-code Cohen's kappa (", n_codes, " codes)") else
    "Mean per-code Cohen's kappa"

  content <- paste0(
    "# Inter-Rater Reliability\n\n",
    "A human verification step was performed to assess coding agreement between ",
    "the AI system and a human researcher. The two coders were aligned by entry ",
    "id, and codes were matched with conservative fuzzy comparison (Jaro-Winkler ",
    "distance up to 0.15) that bridges spelling and inflection differences but ",
    "treats distinct wordings as genuine disagreements.\n\n",
    "*Scope of this statistic.* This compares the AI's coding against a single ",
    "human coder on a sampled subset of entries (see *Entries Compared* below); ",
    "it is an AI-vs-human agreement check, not a multi-coder reliability study, ",
    "and coefficients estimated on a small sample carry wide uncertainty (the ",
    "bootstrap interval on alpha quantifies this). Agreement is computed on ",
    "fuzzy-canonicalized code labels, so it reflects conceptual rather than ",
    "verbatim-string agreement.\n\n",
    "## Agreement Statistics\n\n",
    "| Metric | Value | Interpretation |\n",
    "|--------|------:|----------------|\n",
    "| Krippendorff's alpha (set-based, Jaccard) | ", alpha_val, ci_str, " | ", alpha_interp, " |\n",
    "| ", kappa_label, " | ", stats$cohens_kappa, " | ", stats$kappa_interpretation, " |\n",
    "| Percent Agreement (exact code set) | ", stats$percent_agreement, "% | |\n",
    "| Mean Jaccard Similarity | ", stats$jaccard_similarity, " | |\n",
    "| Entries Compared | ", stats$n_entries, " | |\n\n"
  )

  # Interpretation -- the set-based Krippendorff alpha is the recommended metric.
  if (!is.na(alpha_val)) {
    content <- paste0(content,
      "## Interpretation\n\n",
      "**Krippendorff's alpha** of **", alpha_val, "**", ci_str, " indicates **",
      tolower(alpha_interp), "** agreement (Krippendorff, 2011). ",
      "Computed with a Jaccard set-distance, alpha is the recommended reliability ",
      "metric for multi-label coding: it scores each entry's whole code *set*, so -- ",
      "unlike a flattened code-by-code matrix -- it is not inflated by the number ",
      "of distinct codes in the codebook. Krippendorff recommends alpha >= 0.667 ",
      "for tentative conclusions and >= 0.800 for reliable conclusions.\n\n",
      "The **mean per-code Cohen's kappa** of **", stats$cohens_kappa, "** (",
      tolower(stats$kappa_interpretation), "; Landis & Koch, 1977) averages a ",
      "separate present/absent kappa over each code and is provided as a ",
      "supplementary, per-code view. ",
      "The **mean Jaccard similarity** of **", stats$jaccard_similarity,
      "** is the average overlap between the code sets assigned by each rater, and ",
      "**percent agreement** is the share of entries where the two coders chose the ",
      "exact same code set.\n\n"
    )
  } else if (!is.na(stats$cohens_kappa)) {
    content <- paste0(content,
      "## Interpretation\n\n",
      "The **mean per-code Cohen's kappa** of **", stats$cohens_kappa, "** indicates **",
      tolower(stats$kappa_interpretation), " agreement** (Landis & Koch, 1977). ",
      "The mean Jaccard similarity of **", stats$jaccard_similarity,
      "** represents the average overlap between code sets assigned by each rater.\n\n"
    )
  }

  content
}

.build_learning_transparency <- function(learning) {
  content <- paste0(
    "# Learning from Previous Studies\n\n",
    "Before analyzing the current dataset, the AI system studied **", learning$n_studies,
    " previous manually-coded analyses**: ", paste(learning$study_names, collapse = ", "),
    ". These prior analyses provided calibration data that shaped every major stage ",
    "of the current pipeline.\n\n"
  )

  # How learning was used across pipeline stages
  content <- paste0(content,
    "## How Prior Analyses Guided Each Pipeline Stage\n\n",
    '<div class="callout callout-neutral">\n',
    "The learning context was injected into three pipeline stages to ensure the AI's ",
    "analytical behavior matches the depth, granularity, and specificity demonstrated ",
    "by the human researchers in the previous studies.\n",
    "</div>\n\n",
    "| Pipeline Stage | How Prior Studies Were Used | Context Size |\n",
    "|----------------|---------------------------|-------------:|\n",
    "| **Initial Coding** | Prior theme examples and raw data excerpts set the expected ",
    "level of code specificity and analytical depth | ", format(learning$coding_chars, big.mark = ","), " chars |\n",
    "| **Theme Generation** | Prior thematic structures guided the AI toward producing ",
    "themes at comparable granularity to human-identified themes | ", format(learning$theming_chars, big.mark = ","), " chars |\n",
    "| **Theme Review** | Human-generated themes served as a specificity benchmark -- ",
    "AI themes too vague compared to the prior studies were flagged for revision | ",
    format(learning$review_chars, big.mark = ","), " chars |\n",
    "| **Total** | | **", format(learning$context_characters, big.mark = ","), " chars** |\n\n"
  )

  # Show AI reflection on what was learned
  if (nchar(learning$reflection) > 0) {
    content <- paste0(content,
      "## What the AI Learned and How It Applied Those Findings\n\n",
      .sanitize_ai_prose(learning$reflection), "\n\n"
    )
  }

  # Show specific learning content excerpts
  has_excerpts <- !is.null(learning$coding_excerpt) && nchar(learning$coding_excerpt) > 0
  if (has_excerpts) {
    content <- paste0(content,
      "## Specific Learning Context Provided to the AI\n\n",
      "The following excerpts show what was actually sent to the AI from the prior ",
      "studies. Each pipeline stage received a tailored slice of the learning context ",
      "optimized for its specific analytical task. This transparency is critical for ",
      "assessing whether the AI had adequate calibration data.\n\n"
    )

    content <- paste0(content,
      "*Calibrates code specificity and analytical depth.*\n\n",
      "<details>\n",
      "<summary><strong>Coding Context Excerpt</strong> (click to expand)</summary>\n\n",
      "```\n", learning$coding_excerpt, "\n```\n\n",
      "</details>\n\n"
    )
  }
  if (!is.null(learning$theming_excerpt) && nchar(learning$theming_excerpt) > 0) {
    content <- paste0(content,
      "*Guides theme granularity comparable to human analyses.*\n\n",
      "<details>\n",
      "<summary><strong>Theming Context Excerpt</strong> (click to expand)</summary>\n\n",
      "```\n", learning$theming_excerpt, "\n```\n\n",
      "</details>\n\n"
    )
  }
  if (!is.null(learning$review_excerpt) && nchar(learning$review_excerpt) > 0) {
    content <- paste0(content,
      "*Specificity benchmark for theme quality validation.*\n\n",
      "<details>\n",
      "<summary><strong>Review Calibration Excerpt</strong> (click to expand)</summary>\n\n",
      "```\n", learning$review_excerpt, "\n```\n\n",
      "</details>\n\n"
    )
  }

  content
}

.build_emotional_landscape <- function(stats, export_files) {
  content <- paste0(
    "# Emotional Landscape\n\n",
    "## Sentiment Distribution\n\n",
    "```{r sentiment-histogram}\n",
    "data <- read_csv('", basename(export_files$sentiment_file), "', show_col_types = FALSE, comment = '#')\n\n",
    "ggplot(data, aes(x = sentiment_score)) +\n",
    "  geom_histogram(bins = 35, fill = '#3498DB', color = 'white', alpha = 0.85) +\n",
    "  geom_vline(xintercept = mean(data$sentiment_score, na.rm = TRUE),\n",
    "             color = '#E74C3C', linetype = 'dashed', linewidth = 1) +\n",
    "  annotate('label', x = mean(data$sentiment_score, na.rm = TRUE), y = Inf,\n",
    "           label = paste0('Mean: ', round(mean(data$sentiment_score, na.rm = TRUE), 2)),\n",
    "           vjust = 1.5, fill = '#E74C3C', color = 'white', fontface = 'bold', size = 3.5) +\n",
    "  labs(title = 'Distribution of Sentiment Scores',\n",
    "       subtitle = 'Vertical line indicates mean sentiment across all entries',\n",
    "       x = 'Sentiment Score (-1 = Very Negative, +1 = Very Positive)',\n",
    "       y = 'Number of Entries') +\n",
    "  theme_report() +\n",
    "  scale_x_continuous(breaks = seq(-1, 1, 0.25))\n",
    "```\n\n"
  )

  # Methodological note on bimodal distributions (Issue 5)
  content <- paste0(content,
    '<div class="callout callout-neutral">\n',
    '<strong>Methodological Note:</strong> Bimodal sentiment distributions are common in ',
    'health and support communities, where entries naturally cluster around distress narratives ',
    'and recovery/positive-experience narratives. Additionally, the coupled emotion-sentiment ',
    'prompt architecture (which elicits both emotion and sentiment simultaneously) may ',
    'amplify polarity. Interpret distribution shape with both factors in mind.\n',
    '</div>\n\n'
  )

  # Emotional tone interpretation
  affect <- if (is.na(stats$sentiment$mean)) "**mixed emotional states**"
    else if (stats$sentiment$mean < .SENTIMENT_NEGATIVE_THRESHOLD) "**negative affect**"
    else if (stats$sentiment$mean > .SENTIMENT_POSITIVE_THRESHOLD) "**positive affect**"
    else "**mixed emotional states**"

  content <- paste0(content,
    "## Interpreting the Emotional Tone\n\n",
    "The sentiment distribution reveals a community predominantly experiencing ",
    affect, " (mean = ", stats$sentiment$mean, ", SD = ", stats$sentiment$sd,
    "). Specifically, **", stats$sentiment$pct_negative,
    "%** of entries showed negative sentiment, while **",
    stats$sentiment$pct_positive, "%** were positive.\n\n"
  )

  # Emotion bar chart
  content <- paste0(content,
    "## Emotion Distribution (Multi-Label)\n\n",
    "Entries may express multiple emotions simultaneously. The chart below counts ",
    "each emotion independently -- an entry expressing both sadness and anger ",
    "contributes to both counts.\n\n",
    "```{r emotion-bar}\n",
    "# Multi-label emotion counting: split all_emotions on semicolons\n",
    "emo_col <- 'all_emotions'\n",
    "raw_emo <- data[[emo_col]][!is.na(data[[emo_col]])]\n",
    "all_labels <- trimws(unlist(strsplit(raw_emo, ';\\\\s*')))\n",
    "all_labels <- all_labels[nchar(all_labels) > 0]\n",
    "emo_tbl <- sort(table(all_labels), decreasing = TRUE)\n",
    "n_entries_with_emo <- length(raw_emo)\n",
    "emotion_data <- tibble::tibble(\n",
    "  emotion = names(emo_tbl),\n",
    "  n = as.integer(emo_tbl),\n",
    "  pct = round(100 * as.integer(emo_tbl) / max(n_entries_with_emo, 1), 1)\n",
    ")\n\n",
    "n_emotions <- nrow(emotion_data)\n",
    "emotion_colors <- colorRampPalette(report_colors)(n_emotions)\n",
    "names(emotion_colors) <- emotion_data$emotion\n\n",
    "ggplot(emotion_data, aes(x = reorder(emotion, n), y = n, fill = emotion)) +\n",
    "  geom_col(alpha = 0.9, width = 0.7) +\n",
    "  geom_text(aes(label = paste0(pct, '%')), hjust = -0.15, size = 3.2, fontface = 'bold') +\n",
    "  coord_flip() +\n",
    "  labs(title = 'Emotion Distribution (Multi-Label)',\n",
    "       subtitle = 'Entries may express multiple emotions; percentages are of all entries in scope',\n",
    "       x = '', y = 'Number of Occurrences') +\n",
    "  theme_report() +\n",
    "  theme(legend.position = 'none') +\n",
    "  scale_fill_manual(values = emotion_colors) +\n",
    "  expand_limits(y = max(emotion_data$n) * 1.12)\n",
    "```\n\n"
  )

  # Emotion table
  if (nrow(stats$emotions) > 0) {
    content <- paste0(content,
      "### Emotion Breakdown\n\n",
      "| Emotion | Count | Percentage | Interpretation |\n",
      "|---------|------:|----------:|-----------------|\n"
    )
    for (i in seq_len(min(8, nrow(stats$emotions)))) {
      row <- stats$emotions[i, ]
      interp <- get_emotion_interpretation(row$emotion)
      content <- paste0(content,
        "| ", .md_cell(row$emotion), " | ", format(row$n, big.mark = ","),
        " | ", row$pct, "% | ", .md_cell(stringr::str_to_sentence(interp)), " |\n"
      )
    }
    content <- paste0(content, "\n")
  }

  content
}

.build_thematic_section <- function(theme_stats, theme_order, n_themes, export_files,
                                     config = NULL) {
  # Honest-failure guard (robustness audit): a corpus that yields ZERO themes
  # (empty corpus, 0 on-focus entries, or a Mode-3 run with no matched
  # constructs and no anomalies) must DISCLOSE that rather than emit the
  # theme-distribution / sentiment-by-theme chunks, which crash on the all-NA
  # emerged_themes column (read back as logical -> strsplit error) and leave
  # `## Error` boxes in the published report's headline section.
  if (is.null(n_themes) || length(n_themes) == 0L || is.na(n_themes) ||
      n_themes < 1L || length(theme_order) == 0L) {
    return(paste0(
      "# Thematic Analysis\n\n",
      "No themes emerged from this corpus. This is an honest outcome rather than ",
      "an error: it typically means no entries were on-focus for the stated ",
      "research question (see the Corpus Coverage and Saturation sections for the ",
      "coded / examined / sampled counts), or a Mode-3 framework matched no ",
      "constructs. No theme distribution, per-theme statistics, or sentiment-by-",
      "theme comparison is shown because there are no themes to summarize.\n\n"
    ))
  }
  # top-N inlining + tabular index for remainder.
  # Themes beyond max_inline_themes are rendered as compact one-line
  # rows linking to their per-theme detail HTMLs. Default 30 keeps the
  # main Rmd well under pandoc's working-set limit even on corpora
  # producing hundreds of themes.
  max_inline_themes <- as.integer(
    config$analysis$themes$max_inline_themes %||% 30L
  )
  if (is.na(max_inline_themes) || max_inline_themes < 1L) {
    # Defensive: malformed user config falls back to package default,
    # not to "no cap". A user setting it to 0 or NA would otherwise
    # silently produce an all-compact report.
    max_inline_themes <- 30L
  }
  content <- paste0(
    "# Thematic Analysis\n\n",
    "The analysis identified **", n_themes, " distinct themes** through an iterative process of ",
    "coding, consolidation, and refinement.\n\n",
    "*Click \"View Full Details\" on any theme to see complete entries and statistics.*\n\n",
    "```{r theme-distribution}\n",
    "# Count entries per theme using multi-label membership columns\n",
    "membership_cols <- grep('^theme_membership_', names(data), value = TRUE)\n",
    "if (length(membership_cols) > 0) {\n",
    "  theme_counts <- vapply(membership_cols, function(col) sum(data[[col]] == 1L, na.rm = TRUE), integer(1))\n",
    "  theme_labels <- sub('^theme_membership_', '', names(theme_counts))\n",
    "  theme_labels <- gsub('\\\\.', ' ', theme_labels)\n",
    "  theme_data <- tibble::tibble(theme_name = theme_labels, n = as.integer(theme_counts))\n",
    "} else {\n",
    "  all_themes <- unlist(strsplit(data$emerged_themes[!is.na(data$emerged_themes)], ';\\\\s*'))\n",
    "  theme_tbl <- sort(table(trimws(all_themes)), decreasing = TRUE)\n",
    "  # names(table(character(0))) returns NULL,\n",
    "  # and tibble(theme_name = NULL, ...) drops the column. Coerce\n",
    "  # to character(0) so theme_name always exists (mirrors the\n",
    "  # aggregate_overall_statistics fix in R/16_report_helpers.R).\n",
    "  theme_data <- tibble::tibble(\n",
    "    theme_name = if (length(theme_tbl) > 0L) names(theme_tbl) else character(0),\n",
    "    n = as.integer(theme_tbl))\n",
    "}\n",
    "theme_data <- theme_data |> dplyr::filter(n > 0) |> dplyr::arrange(dplyr::desc(n))\n",
    "theme_data$pct <- round(100 * theme_data$n / nrow(data), 1)\n\n",
    "theme_colors <- colorRampPalette(report_colors)(nrow(theme_data))\n",
    "names(theme_colors) <- theme_data$theme_name\n\n",
    "ggplot(theme_data, aes(x = reorder(theme_name, n), y = n, fill = theme_name)) +\n",
    "  geom_col(alpha = 0.9, width = 0.75) +\n",
    "  geom_text(aes(label = paste0(n, ' (', pct, '%)')), hjust = -0.05, size = 3.2, fontface = 'bold') +\n",
    "  coord_flip() +\n",
    "  labs(title = 'Theme Distribution (Multi-Label)',\n",
    "       subtitle = 'Entries may appear under multiple themes',\n",
    "       x = '', y = 'Number of Entries') +\n",
    "  theme_report() +\n",
    "  theme(legend.position = 'none') +\n",
    "  scale_fill_manual(values = theme_colors) +\n",
    "  expand_limits(y = max(theme_data$n) * 1.15)\n",
    "```\n\n",
    "## Sentiment Comparison Across Themes\n\n",
    "```{r sentiment-boxplot-thematic}\n",
    "# Build long-format data for multi-label sentiment boxplot\n",
    "membership_cols <- grep('^theme_membership_', names(data), value = TRUE)\n",
    "if (length(membership_cols) > 0) {\n",
    "  long_data <- do.call(rbind, lapply(membership_cols, function(col) {\n",
    "    entries <- data[data[[col]] == 1L & !is.na(data$sentiment_score), ]\n",
    "    if (nrow(entries) == 0) return(NULL)\n",
    "    tn <- gsub('\\\\.', ' ', sub('^theme_membership_', '', col))\n",
    "    tibble::tibble(theme_name = tn, sentiment_score = entries$sentiment_score)\n",
    "  }))\n",
    "} else {\n",
    "  long_data <- tibble::tibble(theme_name = character(), sentiment_score = numeric())\n",
    "}\n",
    "if (!is.null(long_data) && nrow(long_data) > 0) {\n",
    "n_themes_plot <- length(unique(long_data$theme_name))\n",
    "ggplot(long_data,\n",
    "       aes(x = reorder(theme_name, sentiment_score, FUN = median),\n",
    "           y = sentiment_score, fill = theme_name)) +\n",
    "  geom_boxplot(alpha = 0.8, outlier.alpha = 0.4, outlier.size = 1.5) +\n",
    "  geom_hline(yintercept = 0, linetype = 'dashed', color = '#7F8C8D', linewidth = 0.5) +\n",
    "  coord_flip() +\n",
    "  labs(title = 'Sentiment Distribution by Theme',\n",
    "       subtitle = 'Boxplots showing median, IQR, and outliers (multi-label)',\n",
    "       x = '', y = 'Sentiment Score') +\n",
    "  theme_report() +\n",
    "  theme(legend.position = 'none') +\n",
    "  scale_fill_manual(values = colorRampPalette(report_colors)(n_themes_plot))\n",
    "}\n",
    "```\n\n"
  )

  # Theme cards. Inject a section header when the theme_kind
  # changes (framework -> emergent -> anomaly_bracket). For Mode 2 runs
  # (all theme_kind = "framework") the header injection never fires.
  # themes beyond max_inline_themes render compact
  # (one-line summary + link). The threshold counts across kinds so the
  # cap is a global budget, not per-kind.
  # Dedupe theme_order so an upstream caller
  # that accidentally repeats a name doesn't render the same theme
  # twice (which would also break the n_compact arithmetic). Pipeline
  # invariants exclude duplicates in practice but defensive dedup
  # cheap insurance.
  theme_order <- unique(theme_order)
  theme_index <- 0
  last_kind <- NA_character_
  compact_header_emitted <- FALSE
  total_rendered <- length(intersect(theme_order, names(theme_stats)))
  n_compact <- max(0L, total_rendered - max_inline_themes)
  for (tn in theme_order) {
    if (!tn %in% names(theme_stats)) next
    theme_index <- theme_index + 1
    ts <- theme_stats[[tn]]
    csv_info <- export_files$theme_csv_files[[tn]]
    safe_fn <- make_safe_filename(tn)

    cur_kind <- ts$theme_kind %||% "framework"
    is_compact <- theme_index > max_inline_themes

    sent_class <- if (is.na(ts$sentiment$mean)) "neutral"
      else if (ts$sentiment$mean < .SENTIMENT_NEGATIVE_THRESHOLD) "negative"
      else if (ts$sentiment$mean > .SENTIMENT_POSITIVE_THRESHOLD) "positive"
      else "neutral"

    # At the inline-to-compact
    # boundary, emit the "## Additional themes" header BEFORE the
    # theme_kind transition banner. Pre-followup the two banners
    # stacked with no content in between when framework count exactly
    # equaled max_inline_themes (i.e. the first compact theme was also
    # the first emergent / anomaly_bracket theme). Reordering puts the
    # cap-explanation banner first; the kind banner follows naturally
    # inside the compact section and the reader sees coherent section
    # boundaries.
    if (is_compact && !compact_header_emitted) {
      content <- paste0(content,
        '<div class="additional-themes-header" style="margin-top: 2.5rem;">\n',
        '## Additional themes\n\n',
        '<p class="theme-description"><em>',
        n_compact, ' theme(s) ranked beyond the top ',
        max_inline_themes, ' are listed below as compact rows linking ',
        'to their full per-theme detail pages. This keeps the main HTML ',
        'renderable at scale (pandoc OOMs on inline cards for >400 themes). ',
        'Every theme retains complete provenance, ',
        'entries, and the paper-style subtheme summary on its detail page.',
        '</em></p>\n',
        '</div>\n\n'
      )
      compact_header_emitted <- TRUE
    }

    if (!identical(cur_kind, last_kind)) {
      header_html <- switch(cur_kind,
        framework = "",  # default; framework themes come first, no header needed
        emergent  = paste0(
          '<div class="emergent-section-header" style="margin-top: 2.5rem;">\n',
          '## Emergent themes\n\n',
          '<p class="theme-description"><em>Patterns the framework did not ',
          'anticipate. These themes are surfaced inductively from the ',
          'segments that resisted the framework (per <code>anomaly_handling = ',
          '"extend"</code>). The framework themes above remain primary; this ',
          'section is an abductive complement (Vila-Henninger 2024).</em></p>\n',
          '</div>\n\n'
        ),
        anomaly_bracket = paste0(
          '<div class="anomaly-section-header" style="margin-top: 2.5rem;">\n',
          '## Bracketed anomalies\n\n',
          '<p class="theme-description"><em>Segments that resist the framework, ',
          'surfaced as a single catch-all per <code>anomaly_handling = ',
          '"bracket"</code>. Switch the framework spec to <code>"extend"</code> ',
          'or <code>"revise"</code> to cluster these inductively into emergent ',
          'themes.</em></p>\n',
          '</div>\n\n'
        ),
        ""
      )
      content <- paste0(content, header_html)
      last_kind <- cur_kind
    }

    # compact branch. Themes beyond the inline cap
    # render a single-line card with badge + name + n + sentiment +
    # detail link + CSV link. The per-theme detail HTML still has the
    # full provenance + entries table + paper-style subtheme table
    # The "## Additional themes" section header
    # was already emitted above (audit followup H1 ordering).
    if (is_compact) {
      # Format sentiment the same way the
      # inline metric card does (rounded by the upstream aggregate but
      # printed without further rounding here). NA falls through to
      # "NA" verbatim, matching inline behavior; pre-followup the
      # compact branch printed "n/a" instead, an inconsistency in
      # the same report.
      csv_link <- if (!is.null(csv_info))
        paste0(' &middot; <a href="', csv_info$relative_path,
                '" download>CSV</a>')
      else ""
      content <- paste0(content,
        '<div class="theme-card-compact" id="theme-summary-', theme_index,
        '">\n',
        '<span class="theme-badge theme-badge-compact">', theme_index,
        '</span> ',
        '<a href="theme_details/theme_', safe_fn,
        '.html" class="theme-compact-link" target="_blank"><strong>',
        .html_esc(tn), '</strong></a> &mdash; ',
        '<span class="compact-n">', ts$n_entries, ' entries (',
        ts$pct_of_total, '%)</span> &middot; ',
        '<span class="compact-sent text-', sent_class, '">sentiment ',
        ts$sentiment$mean,
        '</span>',
        csv_link,
        '\n</div>\n\n'
      )
      next
    }

    content <- paste0(content,
      '<div class="theme-card theme-', theme_index, '" id="theme-summary-', theme_index, '">\n\n',
      '## <span class="theme-badge">', theme_index, '</span> ', .html_esc(tn), '\n\n',
      '<p class="theme-description">', .html_esc(ts$description %||% ""), '</p>\n\n',
      '<div class="theme-meta">\n',
      '<div class="theme-meta-item">\n',
      '<span class="theme-meta-value">', ts$n_entries, ' (', ts$pct_of_total, '%)</span>\n',
      '<span class="theme-meta-label">Entries</span>\n',
      '</div>\n',
      '<div class="theme-meta-item">\n',
      '<span class="theme-meta-value text-', sent_class, '">', ts$sentiment$mean, '</span>\n',
      '<span class="theme-meta-label">Mean Sentiment</span>\n',
      '</div>\n',
      '<div class="theme-meta-item">\n',
      '<span class="theme-meta-value">', ts$sentiment$pct_negative, '% / ', ts$sentiment$pct_positive, '%</span>\n',
      '<span class="theme-meta-label">Neg / Pos</span>\n',
      '</div>\n',
      '<div class="theme-meta-item">\n',
      '<span class="theme-meta-value">', ts$intensity$mean, '</span>\n',
      '<span class="theme-meta-label">Intensity</span>\n',
      '</div>\n',
      '</div>\n\n'
    )

    # Keywords -- capped at 8 frequency-ranked codes
    # upstream in 13_themes.R (`keyword_cap <- 8L`); renderer respects the
    # whole curated set rather than truncating again. An earlier bug was
    # `seq_len(min(5, ...))` here, silently dropping 3 of the 8 (caught by
    # the high-effort code review).
    if (!is.null(ts$keywords) && length(ts$keywords) > 0 && !all(is.na(ts$keywords))) {
      pills <- vapply(ts$keywords[seq_along(ts$keywords)], function(k) {
        paste0('<span class="keyword-pill">', .html_esc(k), '</span>')
      }, character(1))
      content <- paste0(content,
        '<div class="keywords-container">\n',
        paste(pills, collapse = "\n"), '\n',
        '</div>\n\n'
      )
    }

    # per-subtheme paper-style table. One row per real
    # subtheme: name, n, Median(MAD) + Mean(SD) for each auto-detected
    # metric column, examples-of-comments column with quotes tagged
    # [metric: value; ...]. Matches the target publication paper
    # layout. Skipped when the theme has no real subthemes (only the
    # virtual NA-named wrapper from the hierarchy) or when no metrics
    # were auto-detected.
    content <- paste0(content,
      .build_subtheme_summary_table(ts))

    # per-theme temporal panel (posting-time rhythms + the AI's
    # temporal interpretation note). Emitted in the non-compact branch, so it
    # inherits the max_inline_themes gating automatically -- overflow
    # themes still get the panel on their detail page. Returns "" when this
    # theme carries no temporal panel (Mode 1 / legacy / no temporal column).
    content <- paste0(content,
      .build_temporal_panel(ts))

    # T0.2 participant distribution: count, Gini, top contributor share, with
    # a concentration warning when one author dominates. Renders an
    # "unavailable" variant when std_author isn't present (preserves the
    # absence-as-signal pattern -- silent omission would itself be a
    # methodology problem per Jowsey 2025).
    content <- paste0(content,
      .build_participant_spread_card(ts$participant_spread))

    # Representative quotes
    content <- paste0(content, "### Representative Voices\n\n")
    if (!is.null(ts$quotes_with_context) && length(ts$quotes_with_context) > 0) {
      for (quote_type in names(ts$quotes_with_context)) {
        q <- ts$quotes_with_context[[quote_type]]
        if (is.null(q$text) || is.na(q$text)) next

        # NA-guard the sentiment BEFORE the comparisons. This runs in the
        # Rmd-string-BUILDING phase (not a knitr chunk), so an NA here would
        # abort the whole report build -- error=TRUE only catches failures
        # *inside* rendered chunks. Mirrors the detail-page guard below.
        q_sent <- suppressWarnings(as.numeric(q$sentiment %||% NA_real_))

        qclass <- if (is.na(q_sent)) "neutral"
          else if (q_sent < .SENTIMENT_NEGATIVE_THRESHOLD) "negative"
          else if (q_sent > .SENTIMENT_POSITIVE_THRESHOLD) "positive"
          else "neutral"

        slabel <- if (is.na(q_sent)) "Neutral/Mixed"
          else if (q_sent < -0.3) "High Distress"
          else if (q_sent < 0) "Moderate Distress"
          else if (q_sent < 0.3) "Neutral/Mixed"
          else "Positive"

        content <- paste0(content,
          '<div class="quote-box ', qclass, '">\n',
          .html_esc(gsub("\n", " ", q$text)), '\n',
          '<div class="quote-meta">\n',
          '<span class="sentiment-pill ', qclass, '">', slabel, '</span>\n',
          'Sentiment: ', if (is.na(q_sent)) "N/A" else round(q_sent, 2),
          ' &bull; ', .html_esc(q$emotion %||% "N/A"), '\n',
          '</div>\n',
          '</div>\n\n'
        )
      }
    }

    # Detail link (safe_fn already computed at loop top for the
    # compact-row pathway)
    content <- paste0(content,
      '<a href="theme_details/theme_', safe_fn, '.html" class="drill-down-link" target="_blank">',
      'View Full Details: ', ts$n_entries, ' Entries</a>\n'
    )

    # CSV link
    if (!is.null(csv_info)) {
      content <- paste0(content,
        '<span class="csv-link-small"><a href="', csv_info$relative_path,
        '" download>Download CSV</a></span>\n'
      )
    }

    content <- paste0(content, '\n</div>\n\n')
  }

  content
}

# ==============================================================================
# Methodology Setup section (AI analyst's run-start articulations)
# ==============================================================================

#' Load archived methodology articulations from a run directory
#'
#' Reads \code{<run_dir>/rules/methodology_articulations.json} (written by
#' \code{archive_methodology_articulations()} at Step 2.5) and reconstructs the
#' \code{MethodologyArticulations} bundle. Best-effort: returns NULL when the
#' file is absent or unreadable (so the report simply omits the section rather
#' than failing). This is the fallback that lets a resume run -- or a direct
#' \code{generate_report()} call that didn't thread the in-memory bundle -- still
#' surface the AI analyst's articulations for peer review.
#'
#' @param run_dir The report output directory (the run dir).
#' @return A \code{MethodologyArticulations}, or NULL.
#' @keywords internal
.load_methodology_articulations_from_run_dir <- function(run_dir) {
  if (is.null(run_dir)) return(NULL)
  json_path <- file.path(run_dir, "rules", "methodology_articulations.json")
  if (!file.exists(json_path)) return(NULL)
  tryCatch({
    lst <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
    methodology_articulations_from_list(lst,
      default_source = lst$source %||% "ai")
  }, error = function(e) {
    log_warn("Could not load archived methodology articulations: {e$message}")
    NULL
  })
}

#' Render one interpreted-column record as an HTML table row
#'
#' Shared by the metric + temporal interpretation tables in the Methodology
#' Setup section. Shows the column name, the AI's free-form description, the
#' exact primitives it requested (as catalog identifiers + rationales -- the
#' replay-auditable artifact), and the interpretation note.
#'
#' @keywords internal
.methodology_column_row <- function(rec) {
  prims <- rec$requested_primitives %||% list()
  prim_html <- if (length(prims) == 0L) {
    "<em>(none requested)</em>"
  } else {
    paste(vapply(prims, function(p) {
      pn <- as.character(p$primitive %||% "")[1]
      rat <- as.character(p$rationale %||% "")[1]
      sprintf("<code>%s</code>%s", .html_esc(pn),
              if (nzchar(rat)) paste0(" &mdash; ", .html_esc(rat)) else "")
    }, character(1)), collapse = "<br>")
  }
  # the AI's free-form provenance/relevance judgment, shown inline.
  # Empty (pre-62 archives / AI returned nothing) -> an em dash, never fabricated.
  prov <- rec$metric_provenance %||% ""
  prov_html <- if (nzchar(prov)) .html_esc(prov) else "&mdash;"
  sprintf("<tr><td><strong>%s</strong></td><td>%s</td><td>%s</td><td>%s</td><td>%s</td></tr>",
          .html_esc(rec$column_name %||% ""),
          .html_esc(rec$column_description %||% ""),
          prim_html,
          .html_esc(rec$interpretation_note %||% ""),
          prov_html)
}

#' Classify a metric column as substantive vs source/platform metadata
#'
#' Routes the AI's OWN free-form provenance prose into one of two report groups.
#' This is NOT a hardcoded taxonomy of column "kinds" the researcher configures
#' (which the load-bearing principle forbids) and NOT a classification of the
#' column itself -- it reads the AI's prose judgment and surfaces it as a header.
#' Signals are drawn from the wording the schema/prompt asks the AI to use
#' ("metadata", "platform", "reception", "incidental", "engagement"). When the AI
#' gave no provenance (empty -> pre-62 archives), the metric is left UNGROUPED
#' (rendered in a single neutral table), so old runs render exactly as before.
#'
#' @return "metadata", "substantive", or "" (ungrouped/unknown).
#' @keywords internal
.metric_provenance_group <- function(rec) {
  prov <- tolower(rec$metric_provenance %||% "")
  if (!nzchar(prov)) return("")
  meta_signals <- c("metadata", "platform", "reception", "incidental",
                    "engagement", "not the phenomenon", "not a measure of",
                    "how the data was collected", "view count", "upvote",
                    "comment count")
  if (any(vapply(meta_signals, function(s) grepl(s, prov, fixed = TRUE),
                 logical(1)))) "metadata" else "substantive"
}

#' Render the Methodology Setup section
#'
#' The "AI as analyst" transparency artifact: the relevance criterion that
#' operationalized on-focus coding for this study, the on/off-focus examples and
#' discrimination principle, and the per-metric + per-temporal interpretations
#' (which primitives the AI judged honest for each column, and how to read them).
#' A source badge marks AI-articulated vs pinned-replay provenance.
#'
#' Returns "" when the bundle carries no usable relevance criterion AND no
#' interpreted columns (nothing meaningful to show).
#'
#' @param art A \code{MethodologyArticulations} bundle.
#' @return Character HTML string for the section.
#' @keywords internal
.build_methodology_setup_section <- function(art) {
  if (is.null(art) || !inherits(art, "MethodologyArticulations")) return("")
  rel <- art$relevance
  mi  <- art$metric_interpretation
  metrics  <- mi$metrics %||% list()
  temporal <- mi$temporal_columns %||% list()

  has_criterion <- !is.null(rel) && nzchar(rel$relevance_criterion %||% "")
  if (!has_criterion && length(metrics) == 0L && length(temporal) == 0L) {
    return("")
  }

  src <- art$source %||% "ai"
  badge <- if (identical(src, "pinned")) "pinned replay" else "AI-articulated"

  parts <- c(
    '<div class="methodology-setup-section">',
    sprintf('<h2>Methodology Setup <span class="ms-source-badge">%s</span></h2>',
            .html_esc(badge)),
    paste0('<p>Before coding, the AI analyst articulated how to focus this ',
           'study and how to honestly summarize each measured column. These ',
           'decisions are recorded here for peer review and can be pinned to ',
           're-apply the same methodology choices in a confirmatory run.</p>')
  )

  if (has_criterion) {
    parts <- c(parts, "<h3>Relevance criterion</h3>")
    if (nzchar(rel$research_focus_paraphrase %||% "")) {
      parts <- c(parts, sprintf('<p><strong>Focus (AI paraphrase):</strong> %s</p>',
                                .html_esc(rel$research_focus_paraphrase)))
    }
    parts <- c(parts, sprintf('<p class="ms-criterion">%s</p>',
                              .html_esc(rel$relevance_criterion)))

    on_ex  <- rel$on_focus_examples  %||% character(0)
    off_ex <- rel$off_focus_examples %||% character(0)
    if (length(on_ex) > 0L || length(off_ex) > 0L) {
      ex_block <- function(title, xs) {
        if (length(xs) == 0L) return("")
        items <- paste(vapply(xs, function(x)
          sprintf("<li>%s</li>", .html_esc(x)), character(1)), collapse = "")
        sprintf("<div><strong>%s</strong><ul>%s</ul></div>", title, items)
      }
      parts <- c(parts, '<div class="ms-examples">',
                 ex_block("On-focus examples", on_ex),
                 ex_block("Off-focus (adjacent) examples", off_ex),
                 '</div>')
    }
    if (nzchar(rel$discrimination_principle %||% "")) {
      parts <- c(parts, sprintf('<p><strong>Discrimination principle:</strong> %s</p>',
                                .html_esc(rel$discrimination_principle)))
    }
  }

  col_table <- function(heading, recs, caption = "") {
    if (length(recs) == 0L) return(character(0))
    rows <- paste(vapply(recs, .methodology_column_row, character(1)),
                  collapse = "\n")
    c(sprintf("<h3>%s</h3>", heading),
      if (nzchar(caption)) sprintf("<p class=\"ms-group-caption\"><em>%s</em></p>", caption) else character(0),
      "<table>",
      paste0("<thead><tr><th>Column</th><th>What it represents</th>",
             "<th>Requested primitives</th><th>How to read</th>",
             "<th>Relevance to focus</th></tr></thead>"),
      paste0("<tbody>", rows, "</tbody>"),
      "</table>")
  }
  # group the metric interpretations by the AI's OWN provenance prose
  # into "Substantive measures" vs "Source / engagement metadata" -- the grouping
  # IS the methodological signal (the tool refuses to conflate platform reception
  # with the phenomenon). When NO metric carries provenance (pre-62 archives /
  # AI returned none), fall back to the original single neutral table so old runs
  # render unchanged.
  if (length(metrics) > 0L) {
    groups <- vapply(metrics, .metric_provenance_group, character(1))
    if (all(groups == "")) {
      parts <- c(parts, col_table("Metric interpretations", metrics))
    } else {
      subst <- metrics[groups %in% c("substantive", "")]
      meta  <- metrics[groups == "metadata"]
      parts <- c(parts, col_table(
        "Metric interpretations &mdash; substantive measures", subst,
        caption = "Measures the AI judged to bear on the phenomenon under study."))
      parts <- c(parts, col_table(
        "Metric interpretations &mdash; source / engagement metadata", meta,
        caption = paste0("The AI judged these to reflect how the data was ",
                         "collected or received (e.g. platform engagement), not ",
                         "the phenomenon itself. Read the per-theme statistics as ",
                         "reception/salience signals, not prevalence or severity.")))
    }
  }
  # deterministic small-n reliability caveat for the per-subtheme
  # spread statistics. EXPLAIN, don't gate (parallel to the correlation section's
  # exploratory caveat + the metadata caveat above): no value is ever suppressed,
  # there is no n-floor/threshold, and the n is shown beside every statistic so the
  # reader judges. A fixed note is the reliable vehicle -- eliciting this via the
  # AI's free-form interpretation_note proved unreliable on real data (gpt-4o
  # omitted it across re-validation runs even when explicitly asked).
  if (length(metrics) > 0L) {
    parts <- c(parts, paste0(
      '<p class="ms-smalln-caveat"><strong>Reading spread at small n:</strong> ',
      'where this report shows spread statistics (e.g. MAD, IQR, p90) for a ',
      'subtheme with few entries, read them as indicative, not precise &mdash; ',
      'they summarize that handful of observations, not a population. Nothing is ',
      'suppressed: the number of entries (n) is shown beside every statistic so ',
      'you can weigh each accordingly.</p>'))
  }
  parts <- c(parts, col_table("Temporal interpretations", temporal))

  parts <- c(parts, "</div>", "")
  paste(parts, collapse = "\n")
}

# ==============================================================================
# per-theme temporal panel (posting-time rhythms)
# ==============================================================================

#' Render the per-theme temporal panel
#'
#' Surfaces when a theme's entries were posted, using the temporal primitives the
#' AI analyst requested (hour/day/month rhythms, span, cadence, volume timeline)
#' computed over THIS theme's timestamps, plus the AI's interpretation note.
#' Distribution results render in natural (chronological / clock) order; scalars
#' render as a single value. A fail-honest em dash marks any requested primitive
#' the catalog lacks. Returns "" when the theme carries no temporal panel
#' (NULL on Mode 1 / legacy / no-temporal-interpretation runs).
#'
#' @param ts Per-theme stats object (carries \code{temporal_panel}).
#' @return Character HTML string for the panel block.
#' @keywords internal
.build_temporal_panel <- function(ts) {
  panels <- ts$temporal_panel %||% list()
  if (length(panels) == 0L) return("")

  parts <- c('<div class="temporal-panel">',
             "<h3>Posting-time patterns</h3>")
  for (pan in panels) {
    reqs <- pan$requested %||% list()
    if (length(reqs) == 0L) next
    # One row per requested primitive: pretty name + result. Distributions keep
    # natural order (chronological / clock); show more items than the metric
    # table since the timeline IS the point (L1: still truncates with an honest
    # "(+k more)" so a multi-year series can't blow up the cell).
    rows <- vapply(reqs, function(prec) {
      sprintf("<tr><td><strong>%s</strong></td><td>%s</td></tr>",
              .html_esc(.pretty_primitive_name(prec$primitive %||% "")),
              .format_primitive_result(prec, max_items = 24L, natural_order = TRUE))
    }, character(1))

    col_lab <- gsub("_+", " ", pan$column_name %||% "")
    parts <- c(parts,
      sprintf('<p class="tp-col"><strong>%s</strong>%s</p>',
              .html_esc(col_lab),
              if (nzchar(pan$column_description %||% ""))
                paste0(" &mdash; ", .html_esc(pan$column_description)) else ""),
      "<table>",
      "<thead><tr><th>Pattern</th><th>Value</th></tr></thead>",
      paste0("<tbody>", paste(rows, collapse = ""), "</tbody>"),
      "</table>")
    if (nzchar(pan$interpretation_note %||% "")) {
      parts <- c(parts, sprintf('<p class="tp-note"><em>How to read:</em> %s</p>',
                                .html_esc(pan$interpretation_note)))
    }
  }
  parts <- c(parts, "</div>", "")
  paste(parts, collapse = "\n")
}

# ==============================================================================
# paper-style per-subtheme summary table
# ==============================================================================

#' Render the per-theme, per-subtheme summary table
#'
#' Returns an HTML/Markdown block containing a table with one row per
#' real (non-virtual) subtheme of the theme:
#' \itemize{
#'   \item \strong{Subtheme} -- subtheme name + description (truncated)
#'   \item \strong{n} -- entries in this subtheme
#'   \item For each auto-detected metric column: two cells, Median(MAD)
#'     and Mean(SD), formatted as "<center> (<spread>)".
#'   \item \strong{Examples of comments} -- up to N representative quotes
#'     (sentiment-positioned when sentiment_score is available),
#'     each tagged with the source entry's metric values as
#'     \samp{[<metric_a>: 8; <metric_b>: 12]}.
#' }
#'
#' Returns the empty string when:
#' \itemize{
#'   \item the theme has no real subthemes (only the virtual NA-named
#'     wrapper from the subtheme hierarchy), \emph{OR}
#'   \item the dataset has no detectable metric columns AND no real
#'     subthemes.
#' }
#' If subthemes exist but no metric columns do, the table renders with
#' just the Subtheme, n, and Examples columns -- still useful for
#' surfacing the hierarchy.
#'
#' @param ts Per-theme stats object from \code{aggregate_theme_statistics}
#'   (must carry \code{subtheme_stats} + \code{metric_cols}).
#' @return Character HTML+markdown string for the table block.
#' @keywords internal
.build_subtheme_summary_table <- function(ts) {
  st_stats <- ts$subtheme_stats %||% list()
  if (length(st_stats) == 0L) return("")  # virtual-only or no subthemes

  metric_cols <- ts$metric_cols %||% character(0)

  # Pretty-print metric column names (underscores -> spaces) so e.g. a
  # "drug_rating" column heads as "drug rating" while the underlying data
  # column stays canonical. Paper-style convention; dataset-agnostic.
  .pretty_metric <- function(mc) gsub("_+", " ", mc)

  # build a per-column rendering plan. A column the Methodology
  # Assistant interpreted (carries ai_metric_stats with >= 1 requested
  # primitive) renders one column PER chosen primitive plus an interpretation
  # note beneath the table; a column with no AI record (or an empty request)
  # falls back to the legacy Median(MAD)+Mean(SD) battery (per-column, audit
  # notes N1/N2). The AI request list is global per column, so it is read from
  # the first subtheme that carries it. When NO column has an AI plan the
  # output is byte-identical to the pre-61.4 legacy table (back-compat).
  plan <- list()
  for (mc in metric_cols) {
    rec <- NULL
    for (snm in names(st_stats)) {
      r <- st_stats[[snm]]$ai_metric_stats[[mc]]
      if (!is.null(r)) { rec <- r; break }
    }
    if (!is.null(rec) && length(rec$requested %||% list()) > 0L) {
      prim_names <- vapply(rec$requested,
                           function(p) as.character(p$primitive %||% "")[1],
                           character(1))
      unavailable <- vapply(rec$requested, function(p)
        if (!isTRUE(p$available)) as.character(p$primitive %||% "")[1] else NA_character_,
        character(1))
      unavailable <- unique(unavailable[!is.na(unavailable) & nzchar(unavailable)])
      plan[[mc]] <- list(mode = "ai", primitives = prim_names,
                         note = rec$interpretation_note %||% "",
                         desc = rec$column_description %||% "",
                         unavailable = unavailable)
    } else {
      plan[[mc]] <- list(mode = "legacy")
    }
  }
  has_ai     <- any(vapply(plan, function(p) identical(p$mode, "ai"), logical(1)))
  # has_legacy drives the caption's "Median(MAD)/Mean(SD)" clause. TRUE when any
  # column uses the legacy battery, OR when there are no metric columns at all
  # (preserves the exact pre-61.4 caption for the no-metric table).
  has_legacy <- length(metric_cols) == 0L ||
                any(vapply(plan, function(p) identical(p$mode, "legacy"), logical(1)))

  # Header row
  header_cells <- c("Subtheme", "n")
  for (mc in metric_cols) {
    if (identical(plan[[mc]]$mode, "ai")) {
      for (pn in plan[[mc]]$primitives) {
        header_cells <- c(header_cells,
          .html_esc(sprintf("%s %s", .pretty_primitive_name(pn), .pretty_metric(mc))))
      }
    } else {
      header_cells <- c(header_cells,
                         sprintf("Median(MAD) %s", .html_esc(.pretty_metric(mc))),
                         sprintf("Mean(SD) %s",    .html_esc(.pretty_metric(mc))))
    }
  }
  header_cells <- c(header_cells, "Examples of comments")
  header_row <- paste0("<tr><th>",
                       paste(header_cells, collapse = "</th><th>"),
                       "</th></tr>")

  # Build one body row per subtheme
  body_rows <- vapply(names(st_stats), function(snm) {
    s <- st_stats[[snm]]
    cells <- c(
      sprintf("<div class=\"st-name\"><strong>%s</strong></div>%s",
               .html_esc(s$name %||% snm),
               if (nzchar(s$description %||% ""))
                 paste0("<div class=\"st-desc\"><em>",
                         .html_esc(s$description), "</em></div>")
               else ""),
      format(as.integer(s$n %||% 0L))
    )

    for (mc in metric_cols) {
      if (identical(plan[[mc]]$mode, "ai")) {
        # One cell per chosen primitive, matched by primitive NAME (in the
        # header plan's order) against THIS subtheme's computed results. In the
        # production path every subtheme of a theme shares one interpretation
        # record per column (.compute_subtheme_statistics applies the same
        # .metric_interpretation_record to each), so the lists are already
        # identical in length + order and only the values differ. Matching by
        # name -- rather than by raw position -- additionally hardens the table
        # against any future caller whose subthemes differ in primitive order:
        # a value can never land under the wrong header (three independent
        # An audit flagged the positional version as a latent
        # transposition risk; this closes it). Duplicate primitive names (e.g. a
        # pinned prim_quantile at two q's) are consumed left-to-right; a plan
        # primitive absent from this subtheme renders "n/a". The cell count
        # always equals length(plan primitives), so rows stay header-aligned.
        reqs <- s$ai_metric_stats[[mc]]$requested %||% list()
        floor_n <- suppressWarnings(as.integer(
          s$ai_metric_stats[[mc]]$min_reliable_n %||% NA))
        used <- rep(FALSE, length(reqs))
        for (pn in plan[[mc]]$primitives) {
          hit <- which(!used & vapply(reqs, function(r)
            identical(as.character(r$primitive %||% NA_character_)[1], pn),
            logical(1)))
          prec <- if (length(hit) > 0L) { used[hit[1]] <- TRUE; reqs[[hit[1]]] } else NULL
          cell <- .format_primitive_result(prec)
          # MARK a spread/shape statistic computed on fewer entries
          # than the analyst's per-column reliability floor (min_reliable_n). The
          # value + its n stay shown -- this marks, it does not gate/hide. The
          # threshold is the AI's number (not a package cutoff); only dispersion/
          # shape estimators are eligible (robust centers + counts are never marked).
          n_obs <- suppressWarnings(as.integer(prec$n_observed %||% NA))
          if (!is.null(prec) && isTRUE(prec$available) &&
              .metric_primitive_small_n_sensitive(pn) &&
              !is.na(floor_n) && floor_n > 0L &&
              !is.na(n_obs) && n_obs < floor_n) {
            cell <- paste0(cell, sprintf(
              "<sup class=\"smalln-flag\" title=\"n=%d below the analyst's reliability floor of %d for this spread/shape measure; read as indicative\">&dagger;</sup>",
              n_obs, floor_n))
          }
          cells <- c(cells, cell)
        }
      } else {
        mstats <- s$metric_stats[[mc]] %||% list()
        cells <- c(cells,
          .format_metric_summary(mstats$median %||% NA_real_,
                                   mstats$mad %||% NA_real_),
          .format_metric_summary(mstats$mean %||% NA_real_,
                                   mstats$sd %||% NA_real_))
      }
    }

    # Examples-of-comments: stack quotes vertically
    quotes <- s$example_quotes %||% character(0)
    quotes_block <- if (length(quotes) == 0L) {
      "<em>(no representative quotes)</em>"
    } else {
      paste(vapply(quotes, function(q) {
        paste0("<div class=\"st-quote\">", .html_esc(q), "</div>")
      }, character(1)), collapse = "")
    }
    cells <- c(cells, quotes_block)

    paste0("<tr><td>",
            paste(cells, collapse = "</td><td>"),
            "</td></tr>")
  }, character(1))

  # Caption. The legacy clause is preserved verbatim so the no-AI table is
  # byte-identical to the pre-61.4 output; the AI clause is added only when at
  # least one column shows AI-chosen primitives.
  caption <- paste0(
    # Clarify that the bracketed metric tags after
    # each example quote are the SOURCE ENTRY's metric values, NOT subtheme
    # aggregates. Without this preface a reader could confuse a per-entry
    # "[<metric>: 8]" with an aggregate cell.
    "<p class=\"subtheme-table-caption\"><em>",
    if (has_legacy) paste0("Median(MAD) and Mean(SD) columns are subtheme aggregates -- ",
                           "read spread (MAD, SD) as indicative, not precise, when n is small ",
                           "(the n is shown beside each); ") else "",
    if (has_ai) paste0("primitive-named columns are the AI analyst's chosen ",
                       "summaries for that column (interpretation notes below); ") else "",
    "the bracketed values after each example comment are that source ",
    "entry's metric values.</em></p>\n\n"
  )

  # Per-column interpretation notes: the AI analyst's free-form
  # reading of each interpreted column, shown beneath the table. Surfaces any
  # fail-honest gap (a requested primitive the catalog lacks) by name.
  notes_html <- ""
  if (has_ai) {
    note_items <- character(0)
    for (mc in metric_cols) {
      pl <- plan[[mc]]
      if (!identical(pl$mode, "ai")) next
      unavail_txt <- if (length(pl$unavailable) > 0L)
        sprintf(" <span class=\"prim-gap\">Requested unavailable primitive(s): %s &mdash; no statistic computed (fail-honest; contribute the primitive or report the gap).</span>",
                .html_esc(paste(pl$unavailable, collapse = ", "))) else ""
      note_items <- c(note_items, sprintf(
        "<li><strong>%s</strong>%s%s%s</li>",
        .html_esc(.pretty_metric(mc)),
        if (nzchar(pl$desc)) paste0(" &mdash; ", .html_esc(pl$desc)) else "",
        if (nzchar(pl$note)) paste0(" <em>How to read:</em> ", .html_esc(pl$note)) else "",
        unavail_txt))
    }
    if (length(note_items) > 0L)
      notes_html <- paste0(
        "<div class=\"metric-interpretation-notes\">\n",
        "<p class=\"mi-caption\"><em>Metric interpretations (AI analyst):</em></p>\n",
        "<ul>", paste(note_items, collapse = ""), "</ul>\n</div>\n")
  }

  # Footnote explaining the small-n marker, shown only when at least
  # one spread/shape cell was marked. Explain-don't-gate: nothing is suppressed,
  # and the threshold is the analyst's per-column number, not a fixed cutoff.
  smalln_footnote <- if (any(grepl("class=\"smalln-flag\"", body_rows, fixed = TRUE))) {
    paste0(
      "<p class=\"smalln-footnote\"><sup>&dagger;</sup> A spread or distribution-",
      "shape statistic (e.g. SD, MAD, IQR, skewness) computed on fewer entries ",
      "than the analyst judged necessary for a reliable estimate of that measure ",
      "for this column. Read it as indicative, not precise &mdash; the value and ",
      "its n are shown (nothing is suppressed), and the threshold is the analyst's ",
      "per-column judgement, not a fixed cutoff.</p>\n")
  } else ""

  paste0(
    "<h3>Subthemes (per-subtheme summary)</h3>\n\n",
    caption,
    "<div class=\"subtheme-table-wrapper\">\n",
    "<table class=\"subtheme-summary-table\">\n",
    "<thead>", header_row, "</thead>\n",
    "<tbody>", paste(body_rows, collapse = "\n"), "</tbody>\n",
    "</table>\n</div>\n\n",
    smalln_footnote,
    notes_html
  )
}

# ==============================================================================
# Participant distribution card
# ==============================================================================

#' Render the per-theme Participant Distribution card
#'
#' Empirical answer to Jowsey et al. 2025's Frankenstein finding that "none
#' of the Copilot outputs reported the participant spread". Three metrics
#' are surfaced as a meta card:
#' \itemize{
#'   \item \code{n_distinct_contributors} -- count of unique authors
#'   \item \code{contributor_gini} -- Gini coefficient (0 = even, 1 = one
#'     contributor takes everything)
#'   \item \code{top_contributor_share} -- fraction from the most prolific
#'     contributor (the "is this one person's theme?" check)
#' }
#'
#' Concentration warnings:
#' \itemize{
#'   \item When \code{n_distinct_contributors == 1}, renders a "single
#'     contributor" notice -- the theme has zero participant spread.
#'   \item When \code{top_contributor_share > 0.5} (one contributor owns
#'     more than half), renders a caution banner.
#' }
#'
#' Unavailable variant: when \code{participant_spread$available} is FALSE
#' (no \code{std_author} column in the data, or no non-NA author values
#' for this theme), renders a "Participant data not available" notice.
#' Silent omission is rejected because the absence itself carries
#' methodological signal (a Tier-0 universal that explicitly cannot be
#' computed must say so).
#'
#' @param ps participant_spread sub-list from
#'   \code{aggregate_theme_statistics()} (or NULL/missing on legacy stats).
#' @return Character HTML/markdown string for the card.
#' @keywords internal
.build_participant_spread_card <- function(ps) {
  if (is.null(ps)) {
    # Legacy stats objects predate T0.2 -- treat as unavailable rather
    # than crashing.
    return(paste0(
      '<div class="participant-spread-card participant-spread-unavailable">\n',
      '<div class="ps-header">Participant Distribution</div>\n',
      '<p class="ps-unavailable-note">Author data not available for ',
      'this analysis run.</p>\n',
      '</div>\n\n'
    ))
  }

  if (!isTRUE(ps$available)) {
    return(paste0(
      '<div class="participant-spread-card participant-spread-unavailable">\n',
      '<div class="ps-header">Participant Distribution</div>\n',
      '<p class="ps-unavailable-note">Author data not available for ',
      'this dataset; participant-spread metrics cannot be computed. ',
      'Per Tier-0 transparency policy this absence is reported rather ',
      'than silently omitted.</p>\n',
      '</div>\n\n'
    ))
  }

  n_contrib <- ps$n_distinct_contributors %||% 0L
  gini      <- ps$contributor_gini      %||% NA_real_
  top_share <- ps$top_contributor_share %||% NA_real_

  gini_str  <- if (is.na(gini))      "n/a" else sprintf("%.2f", gini)
  share_str <- if (is.na(top_share)) "n/a" else sprintf("%.0f%%",
                                                         100 * top_share)

  # Concentration warning -- threshold tuned to flag themes that look
  # prevalent but actually lean on one heavy poster. The single-contributor
  # case is its own message because n=1 means top_share=1.0 by definition
  # and the count itself is the warning.
  warn_msg <- NULL
  share_warn_class <- ""
  if (n_contrib == 1L) {
    warn_msg <- paste0(
      "Single contributor only. This theme has no participant spread; ",
      "treat findings as a single voice, not a community pattern."
    )
    share_warn_class <- "ps-warn"
  } else if (!is.na(top_share) && top_share > 0.5) {
    warn_msg <- paste0(
      sprintf("%s of this theme's entries come from one contributor", share_str),
      " (top contributor share > 50%). Consider whether the theme reflects ",
      "a community pattern or a single user's framing."
    )
    share_warn_class <- "ps-warn"
  }

  paste0(
    '<div class="participant-spread-card">\n',
    '<div class="ps-header">Participant Distribution</div>\n',
    '<div class="ps-stats">\n',
    '<div class="ps-stat">',
    '<span class="ps-value">', n_contrib, '</span>',
    '<span class="ps-label">Distinct contributors</span>',
    '</div>\n',
    '<div class="ps-stat">',
    '<span class="ps-value">', gini_str, '</span>',
    '<span class="ps-label">Gini coefficient</span>',
    '</div>\n',
    '<div class="ps-stat">',
    '<span class="ps-value ', share_warn_class, '">', share_str, '</span>',
    '<span class="ps-label">Top contributor share</span>',
    '</div>\n',
    '</div>\n',
    if (!is.null(warn_msg)) paste0(
      '<div class="ps-warning">', .html_esc(warn_msg), '</div>\n'
    ) else "",
    '</div>\n\n'
  )
}


# ==============================================================================
# Skip-reason taxonomy clustering
# ==============================================================================

#' Cluster free-text skip reasons into a coarse taxonomy
#'
#' AI-generated skip reasons (\code{coding_state$entry_results[[id]]$skip_reason})
#' are short free-text justifications produced by the coding model when it
#' judges an entry off-topic / non-applicable. On a 5,000-entry run the
#' A large run produced 580 distinct reason strings, almost all
#' paraphrases of "the entry does not contain..." in slightly different
#' wording. Rendering one HTML bullet per distinct string produced an
#' unreadable 580-bullet list AND contributed measurably to pandoc OOM
#' during HTML render (C-3).
#'
#' This helper buckets reasons into ~7 broad categories via case-insensitive
#' keyword regex, first-match-wins. Categories are aggregated by total
#' count; each carries up to 3 verbatim examples (most-frequent first)
#' so the reader can still sample original wording.
#'
#' @param skip_reasons Named integer vector from \code{coverage$skip_reasons}
#'   (names = verbatim reason strings; values = counts).
#' @return List of category records, each with \code{label}, \code{count}
#'   (total entries in this category), \code{n_distinct} (distinct
#'   reason strings), and \code{examples} (character vector, up to 3).
#'   Sorted by total count, descending.
#' @keywords internal
.cluster_skip_reasons <- function(skip_reasons) {
  if (is.null(skip_reasons) || length(skip_reasons) == 0L) {
    return(list())
  }

  # Order matters: first-match-wins. Broad-first patterns would absorb
  # everything; specific patterns come first so they pull the obvious
  # cases out before "off-topic" sweeps the rest. The "Other" bucket
  # catches anything that doesn't match a known pattern.
  patterns <- list(
    # AI-call breakdowns must surface as failures, not hide in "Other".
    # Anchored to the ONE failure string the code generates
    # (R/09_coding.R failure path) -- generic keywords like "network" or
    # "timeout" would re-bucket legitimate free-text AI skip reasons
    # ("discusses a social network...") as failures.
    "AI call failure (network / API / parse)" =
      "^ai response parse failure$",
    "Media-only (image / video / GIF / emoji)" =
      "\\b(gif|emoji|sticker|emoticon|image|images|photo|video|videos|media[ -]?only|attachment|attachments|reaction[- ]?image)\\b",
    "Duplicate / near-duplicate" =
      "\\b(duplicate|reposted|repost|already (covered|posted|discussed)|same as|identical|previously (mentioned|posted))\\b",
    "Metadata / tag / link only" =
      "(\\bsubreddit (tag|reference|mention)|/r/|\\btag(s|ged)? only|\\blink(s)?( only)?\\b|\\burl(s)?( only)?\\b|\\bmention(s)? only|\\bmetadata\\b|^@\\S+\\s*$)",
    "Quote / reply with no original content" =
      "(\\bquote (only|of)\\b|\\bquoting\\b|\\breply (to|that has)\\b|just a (reply|response|quote)|paraphrase of|forwarded)",
    "Question without contributable content" =
      "(\\bonly a question\\b|\\bquestion without\\b|\\bjust asking\\b|\\basking for (help|advice|info|information|opinions)|\\bwondering (if|how|when|what|where|why)|\\bseeking (advice|help|info|input))",
    "Too short / no substantive content" =
      "(\\btoo short\\b|\\binsufficient (content|text|detail|context)|\\bno (substantive|meaningful|relevant|usable) content|\\b(empty|blank) (entry|post|comment)|\\bstub\\b|\\bfewer than\\b|\\bless than [0-9]|\\bbrief (comment|reply)|\\bone[- ](word|liner)|\\bvery brief\\b)",
    "Off-topic / not about research focus" =
      "(\\boff[ -]?topic\\b|\\bnot (about|related|relevant|on[- ]?topic)|\\bunrelated\\b|\\birrelevant\\b|\\bdoes not (relate|pertain|concern|address|discuss|mention|involve|cover)|\\boutside (the )?(scope|topic|focus|study)|\\bdoesn'?t (relate|discuss|mention|address)|\\bnot pertinent\\b|\\bno (?:relevant )?information about\\b)"
  )

  reasons <- as.character(names(skip_reasons))
  counts  <- as.integer(skip_reasons)
  # NA / empty reasons get a stable label so they cluster rather than
  # silently fold into the wrong category.
  reasons[is.na(reasons) | !nzchar(reasons)] <- "(unspecified)"
  reasons_lower <- tolower(reasons)

  cat_label <- rep("Other / unspecified", length(reasons))
  for (label in names(patterns)) {
    unmatched <- cat_label == "Other / unspecified"
    if (!any(unmatched)) break
    hits <- unmatched & grepl(patterns[[label]], reasons_lower, perl = TRUE)
    cat_label[hits] <- label
  }

  # Aggregate per category.
  result <- list()
  for (label in unique(cat_label)) {
    in_cat <- cat_label == label
    cat_count <- sum(counts[in_cat])
    cat_reasons <- reasons[in_cat]
    cat_counts <- counts[in_cat]

    # Examples = top-3 by count within the category.
    ord <- order(-cat_counts)
    examples <- utils::head(cat_reasons[ord], 3L)

    result[[label]] <- list(
      label      = label,
      count      = as.integer(cat_count),
      n_distinct = length(cat_reasons),
      examples   = as.character(examples)
    )
  }

  # Preserve a sensible across-category ordering: by total count desc,
  # but keep "Other / unspecified" at the end regardless of size so a
  # reader expecting a known taxonomy sees the known categories first.
  others <- result[names(result) == "Other / unspecified"]
  known  <- result[names(result) != "Other / unspecified"]
  if (length(known) > 0L) {
    known <- known[order(-vapply(known, function(x) x$count, integer(1)))]
  }
  c(known, others)
}


# ==============================================================================
# Corpus coverage card
# ==============================================================================

#' Render the Tier-0 corpus-coverage assertion card
#'
#' Empirical answer to Jowsey et al. 2025's Frankenstein finding that
#' Microsoft Copilot "drew themes from only the first 2-3 pages of data."
#' pakhom processes entries strictly one at a time; this card surfaces the
#' funnel from preprocessed data to LLM-processed entries to coded entries
#' and asserts the headline \code{no_silent_truncation} claim explicitly.
#'
#' Pairs with the T0.1 verification dashboard: T0.1 says "no fabrications",
#' T0.3 says "no silent truncation". Both are Tier-0 transparency cards
#' rendered above the substantive analysis so reviewers see the integrity
#' claims first.
#'
#' Unavailable variant: when \code{coverage} is NULL (legacy report call,
#' or coverage computation failed) the card renders an explicit
#' "coverage data unavailable" notice rather than omitting silently. Per
#' AC4 (methodology stamped on every output), absence of the card is
#' itself a failure signal, so it is shown.
#'
#' @param coverage A \code{CorpusCoverage} object from
#'   \code{\link{compute_corpus_coverage}}, or NULL.
#' @return Character HTML/markdown string for the card.
#' @keywords internal
.build_corpus_coverage_card <- function(coverage) {
  # this name is preserved as a thin compat wrapper around
  # the new render_tier0_coverage_card generic. Existing tests in
  # test-corpus_coverage.R + test-tier0-smoke.R call this under
  # pakhom:::.build_corpus_coverage_card; routing through the generic
  # keeps their assertions valid while letting Mode 1 dispatch via
  # render_tier0_coverage_card.ProvocationCoverage.
  render_tier0_coverage_card(coverage)
}

#' @rdname render_tier0_coverage_card
#' @export
render_tier0_coverage_card.CorpusCoverage <- function(x, ...) {
  coverage <- x
  ok <- isTRUE(coverage$no_silent_truncation)
  # distinguish intentional saturation-triggered early-stop
  # (coverage$stop_reason == "saturation_arbiter_reached") from genuine
  # silent truncation. Both share ok=TRUE only when the unprocessed tail
  # EXACTLY equals the post-saturation tail (verified in
  # compute_corpus_coverage). Banner styling + language differs.
  saturation_stop <- isTRUE(coverage$saturation_reached) &&
                      identical(coverage$stop_reason, "saturation_arbiter_reached")
  banner_class <- if (ok && saturation_stop) "coverage-banner-saturated"
                  else if (ok)                "coverage-banner-ok"
                  else                         "coverage-banner-warn"
  banner_msg <- if (ok && saturation_stop) {
    paste0(
      "AI saturation arbiter judged the codebook saturated after examining ",
      format(coverage$reached_at_entry, big.mark = ","), " of the ",
      format(coverage$n_input_to_coding, big.mark = ","), " sampled entries",
      ". Coding stopped intentionally; the unprocessed tail (",
      format(coverage$n_unprocessed, big.mark = ","),
      " entries) was excluded by design, NOT by silent truncation. ",
      "See the Saturation Analysis section for the AI arbiter's ",
      "articulation + rationale."
    )
  } else if (ok) {
    paste0(
      "All ",
      format(coverage$n_input_to_coding, big.mark = ","),
      " entries from the preprocessed dataset reached the LLM ",
      "(entry-level coverage).",
      # Guarded: legacy coverage objects have NULL fields, untracked runs
      # have NA -- a bare `> 0` would crash report generation for both.
      if (isTRUE(coverage$truncation_tracked %||% FALSE) &&
          isTRUE((coverage$n_entries_truncated %||% 0L) > 0L)) {
        paste0(
          " ",
          format(coverage$n_entries_truncated, big.mark = ","),
          " entries exceeded the per-entry character cap and were sent ",
          "truncated; see the volume line below."
        )
      } else {
        ""
      }
    )
  } else if (coverage$n_input_to_coding == 0L) {
    "Empty dataset: coding step received zero entries."
  } else {
    paste0(
      format(coverage$n_unprocessed, big.mark = ","),
      " of ",
      format(coverage$n_input_to_coding, big.mark = ","),
      " entries did NOT reach the LLM. Coverage is incomplete; ",
      "investigate before publishing."
    )
  }

  # Funnel rows -- only show pre-coding rows when data is available for them
  funnel_rows <- character(0)
  if (!is.na(coverage$n_raw_loaded)) {
    funnel_rows <- c(funnel_rows, sprintf(
      '<tr><td>Raw rows loaded</td><td>%s</td><td></td></tr>',
      format(coverage$n_raw_loaded, big.mark = ",")
    ))
  }
  if (!is.na(coverage$n_after_preprocessing)) {
    drop_pp <- if (!is.na(coverage$n_raw_loaded))
      format(coverage$n_raw_loaded - coverage$n_after_preprocessing,
             big.mark = ",")
    else ""
    drop_label <- if (nzchar(drop_pp))
      sprintf("%s removed (preprocessing: dedup + length filter)", drop_pp)
    else "Preprocessed entries"
    funnel_rows <- c(funnel_rows, sprintf(
      '<tr><td>After preprocessing</td><td>%s</td><td>%s</td></tr>',
      format(coverage$n_after_preprocessing, big.mark = ","),
      drop_label
    ))
  }
  if (!is.na(coverage$test_mode_sample_size)) {
    funnel_rows <- c(funnel_rows, sprintf(
      '<tr><td>Test-mode sub-sample</td><td>%s</td><td>%s</td></tr>',
      format(coverage$test_mode_sample_size, big.mark = ","),
      "Random sub-sample (test mode enabled)"
    ))
  }
  funnel_rows <- c(funnel_rows, sprintf(
    '<tr class="coverage-row-input"><td>Input to coding step</td><td>%s</td><td>%s</td></tr>',
    format(coverage$n_input_to_coding, big.mark = ","),
    "Entries fed to progressive sequential coding"
  ))
  funnel_rows <- c(funnel_rows, sprintf(
    '<tr class="coverage-row-llm"><td>LLM-processed</td><td>%s</td><td>%s</td></tr>',
    format(coverage$n_processed, big.mark = ","),
    if (ok) "All input entries reached the LLM"
    else sprintf("Gap: %s entries did not reach the LLM",
                 format(coverage$n_unprocessed, big.mark = ","))
  ))
  funnel_rows <- c(funnel_rows, sprintf(
    '<tr><td>&nbsp;&nbsp;-- of those, coded</td><td>%s</td><td>%s</td></tr>',
    format(coverage$n_coded, big.mark = ","),
    "Received at least one code"
  ))
  funnel_rows <- c(funnel_rows, sprintf(
    '<tr><td>&nbsp;&nbsp;-- of those, skipped</td><td>%s</td><td>%s</td></tr>',
    format(coverage$n_skipped, big.mark = ","),
    # Skips are not all AI judgments (an AI-call failure also records a
    # skip); point at the breakdown rather than asserting a single cause.
    # The breakdown block is only rendered when skips exist.
    if ((coverage$n_skipped %||% 0L) > 0L) {
      "Skipped (see skip-reason breakdown below)"
    } else {
      "Skipped"
    }
  ))

  # Skip-reason breakdown -- only render when skips exist. Free-text
  # reasons are clustered into ~7 broad
  # categories via keyword matching, then rendered as category +
  # total count + up to 3 verbatim examples (most-frequent first).
  # An earlier section emitted one <li> per distinct reason --
  # a large full run produced 580 distinct strings (mostly
  # paraphrases of "the entry does not contain...") and the resulting
  # 580-bullet list was a measurable contributor to HTML render OOM
  # (C-3) and a major reader-cognitive-load issue.
  skip_block <- ""
  if (length(coverage$skip_reasons) > 0L) {
    clustered <- .cluster_skip_reasons(coverage$skip_reasons)
    cat_rows <- vapply(clustered, function(cat) {
      examples_html <- if (length(cat$examples) > 0L) {
        paste0(
          '<ul class="coverage-skip-examples">',
          paste(vapply(
            cat$examples,
            function(ex) sprintf('<li>%s</li>', .html_esc(ex)),
            character(1)
          ), collapse = ""),
          '</ul>'
        )
      } else ""
      sprintf(
        '<li><strong>%s</strong>: %s entries (%d distinct reason%s)%s</li>',
        .html_esc(cat$label),
        format(cat$count, big.mark = ","),
        cat$n_distinct,
        ifelse(cat$n_distinct == 1L, "", "s"),
        examples_html
      )
    }, character(1))

    skip_block <- paste0(
      '<div class="coverage-skip-reasons">\n',
      '<div class="coverage-subheader">Skip reasons (clustered)</div>\n',
      '<p class="coverage-skip-caption"><em>Free-text reasons clustered by ',
      'keyword into broad categories; up to 3 verbatim ',
      'examples per category, most-frequent first.</em></p>\n',
      '<ul class="coverage-skip-categories">',
      paste(cat_rows, collapse = ""),
      '</ul>\n',
      '</div>\n'
    )
  }

  paste0(
    '<div class="coverage-card">\n',
    '<div class="coverage-header">Corpus Coverage (T0.3)</div>\n',
    '<div class="coverage-banner ', banner_class, '">', banner_msg, '</div>\n',
    '<div class="coverage-funnel-wrapper">\n',
    '<table class="coverage-funnel">\n',
    '<thead><tr><th>Stage</th><th>Entries</th><th>Note</th></tr></thead>\n',
    '<tbody>\n', paste(funnel_rows, collapse = "\n"), '\n</tbody>\n',
    '</table>\n',
    '</div>\n',
    # Volume line. When within-entry truncation was tracked, distinguish
    # what was SENT to the LLM from the source-text size (chars_sent can
    # also fall below the total via pre-AI too-short skips, whose text
    # counts in the source size but was never sent). Legacy/untracked
    # states keep the source-size figures, labeled as such.
    if (isTRUE(coverage$truncation_tracked %||% FALSE)) {
      sprintf(
        '<div class="coverage-volume">%s of %s characters of source text sent to the LLM (%s words / %s bytes in source).</div>\n',
        format(coverage$chars_sent_to_llm, big.mark = ","),
        format(coverage$chars_processed, big.mark = ","),
        format(coverage$words_processed, big.mark = ","),
        format(coverage$bytes_processed, big.mark = ",")
      )
    } else {
      sprintf(
        '<div class="coverage-volume">%s words (%s characters / %s bytes) of source text (per-entry characters sent to the LLM were not tracked in this run\'s coding state).</div>\n',
        format(coverage$words_processed, big.mark = ","),
        format(coverage$chars_processed, big.mark = ","),
        format(coverage$bytes_processed, big.mark = ",")
      )
    },
    skip_block,
    '<p class="coverage-citation">Addresses Jowsey et al. 2025 ',
    '(doi:10.1371/journal.pone.0330217), which found that Microsoft ',
    'Copilot drew themes from only the first 2-3 pages of data. ',
    'pakhom processes entries strictly one at a time; this funnel is ',
    'the empirical proof of entry-level corpus coverage in the LLM call ',
    'path (within-entry truncation against the per-entry cap is measured ',
    'and disclosed above).</p>\n',
    '</div>\n\n'
  )
}


# ==============================================================================
# Framework Declaration card
# ==============================================================================

#' Render the Mode 3 Framework Declaration section
#'
#' Mode 3 reports previously stamped the
#' methodology mode at the top ("M3 - Framework Applied") but never
#' surfaced WHICH theoretical framework was applied. A reviewer reading
#' a report could not reconstruct whether the analysis used the Theory
#' of Planned Behavior, COM-B, the Theoretical Domains Framework, or
#' the researcher's own custom YAML -- which broke the methodology
#' paper provenance chain and made cross-run comparison opaque. This
#' helper renders the framework's identity (name + sha256 hash), its
#' epistemic stance, anomaly handling policy, and the full constructs
#' list with example indicators so the report is self-describing.
#'
#' Per AC4 ("methodology stamped on every output"), this section is
#' mandatory for any Mode 3 run. Absence (e.g., archive failed earlier
#' in the pipeline) renders an explicit "framework archive not
#' available" notice rather than silently omitting -- the absence is
#' itself a transparency signal.
#'
#' @param spec A \code{FrameworkSpec} object (from
#'   \code{\link{load_framework_spec}}). NULL falls through to the
#'   unavailable variant.
#' @param archive Named list returned by
#'   \code{\link{archive_framework_spec}} carrying \code{path},
#'   \code{relative_path}, and \code{hash}. NULL is acceptable but
#'   the rendered section will lack the file-link + sha256 fingerprint.
#' @return Character HTML/markdown string for the section.
#' @keywords internal
.build_framework_declaration <- function(spec, archive = NULL) {
  if (is.null(spec) || !inherits(spec, "FrameworkSpec")) {
    return(paste0(
      '<div class="framework-card framework-unavailable">\n\n',
      '## Theoretical Framework (Mode 3 / AC4)\n\n',
      'The Mode 3 framework spec was not available to the report ',
      'renderer. This is a transparency failure -- a Mode 3 run that ',
      'cannot identify its framework should not be treated as a ',
      'reproducible methodology paper artifact. Investigate the ',
      'pipeline log for an archive_framework_spec error, then re-run.\n\n',
      '</div>\n\n'
    ))
  }

  # Header line: framework name + (optional) sha256 short fingerprint
  hash_str <- if (!is.null(archive) && !is.null(archive$hash) &&
                    !is.na(archive$hash) && nzchar(archive$hash))
                sprintf(' <span class="framework-hash">sha256: %s</span>',
                        substr(archive$hash, 1, 12))
              else ""
  archive_link <- if (!is.null(archive) && !is.null(archive$relative_path))
                    sprintf(' &middot; <a href="%s">archived spec</a>',
                            .html_esc(archive$relative_path))
                  else ""

  # Citations block -- one per line, escaped
  citations_block <- if (length(spec$citations) > 0L) {
    cit_lines <- vapply(spec$citations, function(cit) {
      sprintf("- %s", .html_esc(cit))
    }, character(1))
    paste0(
      "**Citations:**\n",
      paste(cit_lines, collapse = "\n"),
      "\n\n"
    )
  } else {
    "**Citations:** (none recorded in spec)\n\n"
  }

  # Per-construct rows. Limit example_indicators preview to first 3
  # so the section stays readable on long frameworks (TDF has 14+).
  construct_rows <- vapply(spec$constructs, function(c) {
    indicators <- c$example_indicators %||% character(0)
    indicator_str <- if (length(indicators) > 0L) {
      preview <- head(indicators, 3L)
      tail_str <- if (length(indicators) > 3L)
                    sprintf(", ... (+%d more)", length(indicators) - 3L)
                  else ""
      esc_preview <- vapply(preview, .html_esc, character(1))
      sprintf('<em>e.g.,</em> %s%s',
              paste(sprintf('"%s"', esc_preview), collapse = "; "),
              tail_str)
    } else "<em>(no example indicators in spec)</em>"
    sprintf(
      paste0(
        '<tr><td class="framework-construct-id"><code>%s</code></td>',
        '<td><strong>%s</strong></td>',
        '<td>%s<br><small>%s</small></td></tr>'
      ),
      .html_esc(c$id),
      .html_esc(c$name),
      .html_esc(c$description),
      indicator_str
    )
  }, character(1))

  # Stance + policy plain-language explainers so a reviewer doesn't
  # have to look them up.
  stance_explainer <- switch(spec$epistemic_stance,
    "constructionist" = "treats constructs as researcher-developed lenses; expects to fit not all data perfectly",
    "positivist"      = "treats constructs as universal categories; brackets data that doesn't fit",
    "mixed"           = "applies constructs as primary but tolerates legitimate revision when data demands it",
    "(unknown stance)"
  )
  # anomaly_handling drives BEHAVIOR. Each policy is a real
  # methodological stance toward segments that resist the framework, not
  # a documentation-only enum as it was earlier.
  anomaly_explainer <- switch(spec$anomaly_handling,
    "extend"  = "anomaly segments are clustered inductively into a section of emergent themes parallel to the framework themes (abductive coding; Vila-Henninger 2024). The framework spec itself is NOT mutated -- the analysis output gains a new section; the framework remains fixed at run start (AC2)",
    "revise"  = "same as `extend` PLUS a `framework_review.csv` artifact is written with one row per anomaly segment and editable columns, so the researcher can decide whether to update the framework spec for a future run",
    "bracket" = "anomaly segments are surfaced as a single \"Anomaly (non-fitting)\" theme; no inductive clustering. Right when the framework is mature and bracketing is the methodologically intended stance",
    "(unknown policy)"
  )

  paste0(
    '<div class="framework-card">\n\n',
    '## Theoretical Framework (Mode 3 / AC4)\n\n',
    sprintf('### %s%s%s\n\n',
            .html_esc(spec$name), hash_str, archive_link),
    citations_block,
    sprintf(
      paste0(
        '**Epistemic stance:** `%s` &mdash; %s.\n\n',
        '**Anomaly handling:** `%s` &mdash; %s.\n\n'
      ),
      .html_esc(spec$epistemic_stance), stance_explainer,
      .html_esc(spec$anomaly_handling), anomaly_explainer
    ),
    sprintf(
      '**Constructs (%d):** the AI was constrained to apply ONLY these labels (plus an `anomaly` bucket per the policy above). Free-form coding was disabled.\n\n',
      length(spec$constructs)
    ),
    '<div class="framework-constructs-wrapper">\n',
    '<table class="framework-constructs">\n',
    '<thead><tr><th>ID</th><th>Construct</th><th>Description &amp; example indicators</th></tr></thead>\n',
    '<tbody>\n', paste(construct_rows, collapse = "\n"), '\n</tbody>\n',
    '</table>\n',
    '</div>\n\n',
    if (!is.null(archive)) paste0(
      '<p class="framework-citation"><em>This declaration is the canonical ',
      'record of the framework applied for the run. The archived spec ',
      'file (linked above) is byte-equivalent to what was loaded by ',
      '<code>load_framework_spec()</code>; its sha256 fingerprint is ',
      'stamped into <code>run_metadata.json</code> for cross-run ',
      'comparison and replay-equivalence checks.</em></p>\n'
    ) else paste0(
      '<p class="framework-citation"><em>The framework spec was loaded ',
      'in-process but not archived to the run output directory ',
      '(archive_framework_spec() was not called or failed). The ',
      'declaration above reflects the in-memory FrameworkSpec object; ',
      'replay-equivalence is not assertable without the archive file.</em></p>\n'
    ),
    '</div>\n\n'
  )
}


.build_correlation_section <- function(corr_interpretation, export_files,
                                       theme_group_tests = NULL,
                                       cooccurrence_tests = NULL) {
  content <- paste0(
    "# Correlation Analysis\n\n",
    "## Overview\n\n"
  )

  if (!is.null(corr_interpretation)) {
    content <- paste0(content, .sanitize_ai_prose(corr_interpretation$summary), "\n\n")
  }

  # correlation_plot.png is conditionally produced (skipped
  # when the correlation matrix has <2 variables, e.g. small samples).
  # Reference the image only when it actually exists on disk; otherwise
  # render a one-line note explaining the absence so the reader doesn't
  # see a broken-image icon in the HTML.
  plot_path <- export_files$plot_file %||%
                 file.path(dirname(export_files$correlations_file %||% "."),
                            "correlation_plot.png")
  plot_block <- if (!is.null(plot_path) && file.exists(plot_path)) {
    paste0("![Correlation Matrix](", basename(plot_path), ")\n\n",
           "*Cells display correlation coefficients; intensity reflects |r|. ",
           "Effect sizes and 95% confidence intervals are the primary inferential ",
           "tools (see Overview above for the exploratory-framing rationale).*\n\n")
  } else {
    paste0("*No correlation plot was produced for this run. The correlation ",
           "matrix had fewer than 2 variables -- typically because the analytic ",
           "sample is too small for theme-membership pairs to overlap. The ",
           "table below still reports any pair-level associations that did ",
           "compute.*\n\n")
  }
  content <- paste0(content,
    "## Correlation Matrix\n\n",
    plot_block,
    "## Exploratory Associations\n\n",
    "_Sorted by absolute effect size (|r|). The table reports correlations ",
    "with their 95% confidence intervals and three p-value adjustments (raw, ",
    "Benjamini-Hochberg FDR, Bonferroni FWER) for transparency. Treat these as ",
    "hypothesis-generating; themes were inductively derived from this corpus._\n\n",
    "```{r correlation-table}\n",
    "correlations <- read_csv('", basename(export_files$correlations_file), "', show_col_types = FALSE, comment = '#')\n\n",
    "# Filter by meaningful effect (|r| >= 0.10) when available, else legacy 'significant'\n",
    "flag_col <- if ('meaningful_effect' %in% names(correlations)) 'meaningful_effect' else 'significant'\n",
    "# Robustness: a focused corpus can yield ZERO correlation pairs -> header-only\n",
    "# correlations.csv -> readr types every column as empty CHARACTER, so\n",
    "# filter()/arrange(abs())/mutate() error and (with error=TRUE) cascade\n",
    "# ('sig_corrs not found') through the rest of the chunk. Guard: render a\n",
    "# graceful note instead of a stack of R error tracebacks when there is\n",
    "# nothing to report.\n",
    "if (nrow(correlations) == 0 || !flag_col %in% names(correlations)) {\n",
    "  cat('\\n_No exploratory associations met the reporting threshold for this corpus (too few variables or correlation pairs to test)._\\n')\n",
    "} else {\n",
    "sig_corrs <- correlations |>\n",
    "  filter(.data[[flag_col]]) |>\n",
    "  arrange(desc(abs(correlation))) |>\n",
    "  mutate(\n",
    "    var1 = gsub('theme_membership_', '', var1),\n",
    "    var2 = gsub('theme_membership_', '', var2),\n",
    "    var1 = gsub('_', ' ', var1),\n",
    "    var2 = gsub('_', ' ', var2),\n",
    "    var1 = gsub('\\\\.', ' ', var1),\n",
    "    var2 = gsub('\\\\.', ' ', var2),\n",
    "    var1 = tools::toTitleCase(var1),\n",
    "    var2 = tools::toTitleCase(var2),\n",
    "    Direction = ifelse(correlation > 0, 'Positive', 'Negative'),\n",
    "    `Effect Size` = effect_size\n",
    "  )\n",
    "select_cols <- c('Variable 1' = 'var1', 'Variable 2' = 'var2',\n",
    "       'Correlation' = 'correlation', 'Direction' = 'Direction',\n",
    "       'Effect Size' = 'Effect Size')\n",
    "if ('ci_lower' %in% names(sig_corrs) && 'ci_upper' %in% names(sig_corrs)) {\n",
    "  sig_corrs$`95% CI` <- sprintf('[%.3f, %.3f]', sig_corrs$ci_lower, sig_corrs$ci_upper)\n",
    "  select_cols <- c(select_cols, '95% CI' = '95% CI')\n",
    "}\n",
    "# Tiered p-values for newer results; single p_value for legacy data.\n",
    "if (all(c('p_raw', 'p_bh', 'p_bonferroni') %in% names(sig_corrs))) {\n",
    "  select_cols <- c(select_cols,\n",
    "    'p (raw)' = 'p_raw', 'p (BH FDR)' = 'p_bh', 'p (Bonf)' = 'p_bonferroni')\n",
    "} else {\n",
    "  select_cols <- c(select_cols, 'P-value' = 'p_value')\n",
    "}\n",
    "if ('method' %in% names(sig_corrs)) {\n",
    "  sig_corrs$Method <- tools::toTitleCase(sig_corrs$method)\n",
    "  select_cols <- c(select_cols, 'Method' = 'Method')\n",
    "}\n",
    "sig_corrs <- sig_corrs |> select(!!!select_cols)\n\n",
    "if (has_dt) {\n",
    "  numeric_cols <- intersect(c('Correlation', 'p (raw)', 'p (BH FDR)',\n",
    "                              'p (Bonf)', 'P-value'), names(sig_corrs))\n",
    "  DT::datatable(sig_corrs,\n",
    "                options = list(pageLength = 10, dom = 'ftp', scrollX = TRUE),\n",
    "                rownames = FALSE,\n",
    "                class = 'compact stripe') |>\n",
    "    DT::formatRound(columns = numeric_cols, digits = 3)\n",
    "} else {\n",
    "  knitr::kable(sig_corrs, digits = 3)\n",
    "}\n",
    "}\n",
    "```\n\n"
  )

  # --- Theme Group Comparisons (Mann-Whitney U) ---
  if (!is.null(theme_group_tests) && is.data.frame(theme_group_tests) && nrow(theme_group_tests) > 0) {
    has_tiered_p <- all(c("p_raw", "p_bh", "p_bonferroni") %in% names(theme_group_tests))
    # Surface n_members +
    # n_non_members + effect_size in the rendered table. Pre-followup
    # H-17 emitted these columns in the tibble but the renderer never
    # showed them -- defeating the whole point of H-17. M-1 defensive
    # guard against resume from an older checkpoint (tibble without
    # these columns falls back to the legacy rendered shape).
    has_n_members <- all(c("n_members", "n_non_members") %in%
                            names(theme_group_tests))
    has_effect_size <- "effect_size" %in% names(theme_group_tests)
    content <- paste0(content,
      "## Theme Group Comparisons\n\n",
      "Mann-Whitney U tests comparing continuous variables (sentiment, emotion ",
      "intensity) between entries assigned to each theme versus those not ",
      "assigned. Effect sizes (Cohen's r conventions: 0.10 small, 0.30 medium, ",
      "0.50 large; below 0.10 negligible) are the primary inferential signals; ",
      "p-values under three regimes (raw, Benjamini-Hochberg FDR, Bonferroni ",
      "FWER) are reported for transparency. Sample sizes (n_members + ",
      "n_non_members) accompany every test so power differences are visible. ",
      "Sorted by absolute effect size.\n\n",
      "```{r theme-group-tests}\n",
      "tgt <- tibble::tibble(\n",
      "  Theme = ", deparse1(theme_group_tests$theme), ",\n",
      "  Variable = ", deparse1(theme_group_tests$variable), ",\n",
      if (has_n_members) paste0(
        "  `n (Members)` = ", deparse1(as.integer(theme_group_tests$n_members)), ",\n",
        "  `n (Non-members)` = ", deparse1(as.integer(theme_group_tests$n_non_members)), ",\n"
      ) else "",
      "  `Mean (Members)` = ", deparse1(round(theme_group_tests$mean_members, 3)), ",\n",
      "  `Mean (Non-members)` = ", deparse1(round(theme_group_tests$mean_non_members, 3)), ",\n",
      "  `W Statistic` = ", deparse1(theme_group_tests$w_statistic), ",\n",
      "  `Effect Size (r)` = ", deparse1(round(theme_group_tests$effect_r, 3)),
      if (has_effect_size) paste0(",\n",
        "  `Magnitude` = ", deparse1(theme_group_tests$effect_size)
      ) else "",
      if (has_tiered_p) paste0(",\n",
        "  `p (raw)` = ", deparse1(signif(theme_group_tests$p_raw, 4)), ",\n",
        "  `p (BH FDR)` = ", deparse1(signif(theme_group_tests$p_bh, 4)), ",\n",
        "  `p (Bonf)` = ", deparse1(signif(theme_group_tests$p_bonferroni, 4))
      ) else paste0(",\n",
        "  `P-value` = ", deparse1(signif(theme_group_tests$p_adjusted, 4))
      ), "\n",
      ")\n",
      "if (has_dt) {\n",
      "  DT::datatable(tgt,\n",
      "                options = list(pageLength = 10, dom = 'ftp', scrollX = TRUE),\n",
      "                rownames = FALSE,\n",
      "                class = 'compact stripe',\n",
      "                caption = 'Mann-Whitney U: Theme Members vs Non-Members')\n",
      "} else {\n",
      "  knitr::kable(tgt, digits = 3, caption = 'Mann-Whitney U: Theme Members vs Non-Members')\n",
      "}\n",
      "```\n\n"
    )
  }

  # --- Theme Co-occurrence (Chi-square / Fisher's exact) ---
  if (!is.null(cooccurrence_tests) && is.data.frame(cooccurrence_tests) && nrow(cooccurrence_tests) > 0) {
    has_tiered_p <- all(c("p_raw", "p_bh", "p_bonferroni") %in% names(cooccurrence_tests))
    # Surface effect_size column
    # when present. M-3: note count of pairs with degenerate (NA)
    # Cramer's V separately, so the reader knows the headline applies
    # to interpretable rows only.
    has_effect_size <- "effect_size" %in% names(cooccurrence_tests)
    n_na_cramers <- sum(is.na(cooccurrence_tests$cramers_v))
    na_cramers_note <- if (n_na_cramers > 0L) {
      sprintf(paste0(" Note: %d pair(s) had a degenerate contingency table ",
                      "(one row or column all zero); their Cramer's V is ",
                      "undefined and they are excluded from the meaningful-",
                      "effect headline count."), n_na_cramers)
    } else ""
    content <- paste0(content,
      "## Theme Co-occurrence\n\n",
      "Chi-square tests of independence (or Fisher's exact test when expected ",
      "frequencies < 5) examining whether theme co-occurrence patterns differ ",
      "from what would be expected by chance. Cramer's V (effect size) is the ",
      "primary inferential signal; p-values under three regimes (raw, ",
      "Benjamini-Hochberg FDR, Bonferroni FWER) are reported for transparency. ",
      "Sorted by |Cramer's V|.", na_cramers_note, "\n\n",
      "```{r theme-cooccurrence}\n",
      "cooc <- tibble::tibble(\n",
      "  `Theme 1` = ", deparse1(cooccurrence_tests$theme1), ",\n",
      "  `Theme 2` = ", deparse1(cooccurrence_tests$theme2), ",\n",
      "  `Observed Both` = ", deparse1(cooccurrence_tests$observed_both), ",\n",
      "  `Expected Both` = ", deparse1(round(cooccurrence_tests$expected_both, 1)), ",\n",
      "  Statistic = ", deparse1(round(cooccurrence_tests$statistic, 3)), ",\n",
      "  `Cramer's V` = ", deparse1(round(cooccurrence_tests$cramers_v, 3)),
      if (has_effect_size) paste0(",\n",
        "  `Magnitude` = ", deparse1(cooccurrence_tests$effect_size)
      ) else "",
      if (has_tiered_p) paste0(",\n",
        "  `p (raw)` = ", deparse1(signif(cooccurrence_tests$p_raw, 4)), ",\n",
        "  `p (BH FDR)` = ", deparse1(signif(cooccurrence_tests$p_bh, 4)), ",\n",
        "  `p (Bonf)` = ", deparse1(signif(cooccurrence_tests$p_bonferroni, 4))
      ) else paste0(",\n",
        "  `P-value` = ", deparse1(signif(cooccurrence_tests$p_adjusted, 4))
      ), "\n",
      ")\n",
      "if (has_dt) {\n",
      "  DT::datatable(cooc,\n",
      "                options = list(pageLength = 10, dom = 'ftp', scrollX = TRUE),\n",
      "                rownames = FALSE,\n",
      "                class = 'compact stripe',\n",
      "                caption = 'Theme Co-occurrence: Chi-Square / Fisher Tests')\n",
      "} else {\n",
      "  knitr::kable(cooc, digits = 3, caption = 'Theme Co-occurrence: Chi-Square / Fisher Tests')\n",
      "}\n",
      "```\n\n"
    )
  }

  content
}

.build_synthesis_section <- function(insights, ai_synthesis = NULL) {
  content <- "# Synthesis & Conclusion\n\n"

  # Key findings
  if (!is.null(insights$key_findings) && length(insights$key_findings) > 0) {
    content <- paste0(content, "## Key Findings\n\n")
    findings <- insights$key_findings

    if (is.data.frame(findings)) {
      for (i in seq_len(min(5, nrow(findings)))) {
        content <- paste0(content,
          "### ", i, ". ", .sanitize_ai_prose(findings$insight[i]), "\n\n",
          .sanitize_ai_prose(findings$explanation[i]), "\n\n"
        )
      }
    } else if (is.list(findings)) {
      for (i in seq_along(findings)) {
        f <- findings[[i]]
        insight_text <- if (is.list(f)) f$insight else as.character(f)
        explanation <- if (is.list(f)) f$explanation %||% "" else ""
        content <- paste0(content, "### ", i, ". ", .sanitize_ai_prose(insight_text), "\n\n")
        if (nchar(explanation) > 0) {
          content <- paste0(content, .sanitize_ai_prose(explanation), "\n\n")
        }
      }
    }
  }

  if (!is.null(insights$theoretical_implications) ||
      !is.null(insights$practical_implications)) {
    content <- paste0(content,
      "> _The implications below are **exploratory and hypothesis-generating**, read ",
      "from cross-sectional associations in a single corpus. They are not causal ",
      "claims and require confirmatory testing._\n\n")
  }
  if (!is.null(insights$theoretical_implications)) {
    content <- paste0(content,
      "## Theoretical Implications\n\n",
      .sanitize_ai_prose(insights$theoretical_implications), "\n\n"
    )
  }

  if (!is.null(insights$practical_implications)) {
    content <- paste0(content,
      "## Practical Implications\n\n",
      .sanitize_ai_prose(insights$practical_implications), "\n\n"
    )
  }

  # Append conclusion into synthesis section (Issue 12)
  if (!is.null(ai_synthesis) && !is.null(ai_synthesis$conclusion)) {
    content <- paste0(content,
      "## Conclusion\n\n",
      .sanitize_ai_prose(ai_synthesis$conclusion), "\n\n"
    )
  }

  content
}

# ==============================================================================
# Saturation section
# ==============================================================================

.build_saturation_section <- function(coding_state) {
  sat <- coding_state$saturation
  curve <- sat$curve

  content <- "# Thematic Saturation Analysis\n\n"

  if (isTRUE(sat$reached)) {
    content <- paste0(content,
      # #7a: label the three distinct counts (coded / examined / sampled) so the
      # banner's "examined N of M sampled" and this line do not read as a
      # contradiction. reached_at_coded = entries that received >=1 code;
      # reached_at_entry = entries examined before the arbiter stopped;
      # total_entries_at_saturation = the sampled total fed to coding.
      "Thematic saturation was **reached** after coding **",
      sat$reached_at_coded, "** of the ",
      if (!is.null(sat$reached_at_entry) && !is.na(sat$reached_at_entry)) {
        paste0("**", sat$reached_at_entry, "** entries examined (",
               sat$total_entries_at_saturation, " sampled)")
      } else {
        paste0(sat$total_entries_at_saturation, " entries sampled")
      },
      ". At that point, the codebook contained **",
      length(coding_state$codebook), "** unique codes.\n\n"
    )

    # saturation is now AI-arbited (R/saturation_arbiter.R).
    # The earlier triangulation (code_creation_rate + slope_ratio +
    # ai_self_assessment signal booleans) is gone; the report renders
    # the AI's articulation + rationale instead. Fall back to the
    # legacy signal list when reading an older state file that
    # predates the arbiter (back-compat: replay of earlier runs).
    # Sanitize the AI free-text up front (neutralizes injected <tags> while
    # preserving Markdown) BEFORE prepending the "> " blockquote markers,
    # so the markers stay literal and the content can't inject HTML.
    articulation <- .sanitize_ai_prose(as.character(sat$ai_articulation %||% ""))
    rationale    <- .sanitize_ai_prose(as.character(sat$ai_rationale %||% ""))
    if (nzchar(articulation) || nzchar(rationale)) {
      content <- paste0(content,
        "## How the AI arbiter judged saturation\n\n",
        "Per the commitment that the AI decides when to stop (no ",
        "hardcoded thresholds), an AI saturation arbiter judged the ",
        "codebook trajectory + composition at every ",
        "`max(20, ceiling(n_corpus / 50))`-entry checkpoint and ",
        "returned a 3-valued verdict (`reached` / `not_yet` / ",
        "`uncertain`) with a structured articulation and rationale. ",
        "The verdict at the stopping point was **`reached`**.\n\n",
        if (nzchar(articulation)) paste0(
          "**Articulation** (the AI's description of what it observed):\n\n",
          "> ", gsub("\n", "\n> ", articulation, fixed = TRUE), "\n\n"
        ) else "",
        if (nzchar(rationale)) paste0(
          "**Rationale** (the AI's justification for declaring saturation):\n\n",
          "> ", gsub("\n", "\n> ", rationale, fixed = TRUE), "\n\n"
        ) else ""
      )
    } else if (isTRUE(sat$signals$ai_self_assessment) ||
               isTRUE(sat$signals$code_creation_rate) ||
               isTRUE(sat$signals$slope_ratio)) {
      # Back-compat: earlier state file -- describe the legacy
      # convergent signals that triggered saturation. Not produced by
      # any current run, but a state file from an older run may have
      # them.
      signals <- c()
      if (isTRUE(sat$signals$code_creation_rate)) {
        signals <- c(signals, "new code creation rate dropped below threshold")
      }
      if (isTRUE(sat$signals$slope_ratio)) {
        signals <- c(signals, "Inductive Thematic Saturation ratio reached threshold (De Paoli & Mathis, 2024)")
      }
      if (isTRUE(sat$signals$ai_self_assessment)) {
        signals <- c(signals, "AI self-assessment reported no novel patterns remaining")
      }
      content <- paste0(content,
        "Saturation was triggered by the following convergent signals ",
        "(an earlier triangulation): ",
        paste(signals, collapse = "; "), ".\n\n"
      )
    }

    sat_ratio_val <- as.numeric(sat$saturation_ratio %||% NA_real_)
    if (!is.na(sat_ratio_val) && sat_ratio_val > 0) {
      content <- paste0(content,
        "The saturation ratio (codes / coded entries) was **",
        sat_ratio_val, "**, indicating that on average one new code was created ",
        "for every ", round(1 / sat_ratio_val), " coded entries.\n\n"
      )
    }
  } else {
    content <- paste0(content,
      "Thematic saturation was **not reached** during this analysis. ",
      "All ", length(coding_state$entries_processed), " entries were processed, ",
      "yielding ", length(coding_state$codebook), " unique codes.\n\n"
    )
  }

  # Saturation curve (generated inline via R code in the Rmd)
  content <- paste0(content, "## Saturation Curve\n\n")
  {
    if (nrow(curve) > 0) {
      content <- paste0(content,
        "```{r saturation-curve, echo=FALSE, fig.width=9, fig.height=5.5}\n",
        "curve_data <- data.frame(\n",
        "  entries_coded = c(", paste(curve$entries_coded, collapse = ", "), "),\n",
        "  n_codes = c(", paste(curve$n_codes, collapse = ", "), "),\n",
        "  new_codes = c(", paste(curve$new_codes_in_window, collapse = ", "), ")\n",
        ")\n",
        "par(mar = c(5, 5, 4, 5))\n",
        "plot(curve_data$entries_coded, curve_data$n_codes,\n",
        "     type = 'l', lwd = 2.5, col = '#2c3e50',\n",
        "     xlab = 'Entries Coded', ylab = 'Cumulative Unique Codes',\n",
        "     main = 'Thematic Saturation Curve', las = 1, bty = 'l')\n",
        "par(new = TRUE)\n",
        "plot(curve_data$entries_coded, curve_data$new_codes,\n",
        "     type = 'l', lwd = 1.5, col = '#e74c3c', lty = 2,\n",
        "     axes = FALSE, xlab = '', ylab = '')\n",
        "axis(side = 4, col = '#e74c3c', col.axis = '#e74c3c', las = 1)\n",
        "mtext('New Codes per Window', side = 4, line = 3, col = '#e74c3c')\n"
      )

      if (isTRUE(sat$reached)) {
        sat_idx <- which.min(abs(curve$entries_coded - sat$reached_at_coded))
        content <- paste0(content,
          "abline(v = ", sat$reached_at_coded, ", col = '#e67e22', lty = 3, lwd = 1.5)\n",
          "points(", sat$reached_at_coded, ", ", curve$n_codes[sat_idx],
          ", pch = 19, col = '#e67e22', cex = 2)\n",
          "text(", sat$reached_at_coded, ", ", curve$n_codes[sat_idx],
          ", labels = paste0('Saturation\\n(', ", sat$reached_at_coded,
          ", ' entries, ', ", curve$n_codes[sat_idx], ", ' codes)'),\n",
          "     pos = 4, col = '#e67e22', cex = 0.8, font = 2)\n"
        )
      }

      content <- paste0(content,
        "legend('right',\n",
        "  legend = c('Cumulative codes', 'New codes/window'",
        if (isTRUE(sat$reached)) ", 'Saturation point'" else "",
        "),\n",
        "  col = c('#2c3e50', '#e74c3c'",
        if (isTRUE(sat$reached)) ", '#e67e22'" else "",
        "),\n",
        "  lty = c(1, 2",
        if (isTRUE(sat$reached)) ", NA" else "",
        "),\n",
        "  pch = c(NA, NA",
        if (isTRUE(sat$reached)) ", 19" else "",
        "),\n",
        "  lwd = c(2.5, 1.5",
        if (isTRUE(sat$reached)) ", NA" else "",
        "),\n",
        "  cex = 0.75, bg = 'white')\n",
        "```\n\n"
      )
    }
  }

  # Methodological note for paper
  content <- paste0(content,
    "## Methodological Note\n\n",
    "> **Suggested text for methods section:** "
  )

  # Pre-fix the suggested methods text, which
  # (a) said "All 8216 entries were coded" using length(entries_processed)
  # when actually entries_processed includes skipped entries (3182 of
  # 8216 were skipped on a large run; only 5034 coded), and (b) cited
  # the earlier saturation heuristics (Guest 2020 + De Paoli 2024)
  # instead of the actual AI saturation arbiter. Both
  # generate journal-reviewer-misleading methods text.
  n_coded <- length(coding_state$entries_processed) -
              length(coding_state$entries_skipped %||% integer(0))
  n_processed <- length(coding_state$entries_processed)
  n_skipped <- length(coding_state$entries_skipped %||% integer(0))

  if (isTRUE(sat$reached)) {
    art <- coding_state$saturation$ai_articulation %||% NA_character_
    art_str <- if (!is.na(art) && nzchar(trimws(art))) {
      # AI free-text -> neutralize injected HTML (Markdown preserved).
      paste0(" The arbiter articulated: \"",
             .sanitize_ai_prose(substr(art, 1, 280)), "\"")
    } else ""
    content <- paste0(content,
      "Thematic saturation was assessed via an AI-judged arbiter ",
      "(pakhom's implementation) that reviewed codebook ",
      "trajectory metrics at a cadence auto-scaled to corpus size. ",
      "At each check the AI was shown the new-codes-in-window trend, ",
      "the reuse density, and a verbal trajectory, and was asked to ",
      "return a 3-valued verdict (`reached` | `not_yet` | `uncertain`) ",
      "with a written articulation justifying the decision. Per the C1 ",
      "commitment, no hardcoded thresholds gate the verdict; the AI ",
      "is the sole judge.", art_str, " Saturation was reached after ",
      "coding ", sat$reached_at_coded, " of ",
      # #7a: same coded/examined/sampled labeling as the section opening, so the
      # suggested methods paragraph the researcher copies into their paper does not
      # imply only `reached_at_coded` of the SAMPLE were examined.
      if (!is.null(sat$reached_at_entry) && !is.na(sat$reached_at_entry)) {
        paste0("the ", sat$reached_at_entry, " entries examined (",
               sat$total_entries_at_saturation, " sampled)")
      } else {
        paste0(sat$total_entries_at_saturation, " entries sampled")
      },
      ", at which point ", length(coding_state$codebook),
      " unique codes had been identified.\n\n"
    )
  } else {
    content <- paste0(content,
      n_coded, " of ", n_processed, " entries were coded ",
      "(", n_skipped, " skipped as off-topic or insufficient ",
      "content). Thematic saturation was monitored by an AI-judged ",
      "arbiter (pakhom's implementation) which reviewed the ",
      "codebook trajectory at a cadence auto-scaled to corpus size; ",
      "the arbiter did not declare saturation, indicating that novel ",
      "codes continued to emerge through the end of the run.\n\n"
    )
  }

  content
}

#' Build the Longitudinal Patterns report section
#'
#' Rendered only when temporal analysis produced data
#' (\code{has_temporal_data}). Each chart is embedded only when its PNG
#' actually exists on disk: the prevalence chart requires more than one
#' time period and the emergence chart at least one dated theme, and with
#' \code{self_contained} rendering a reference to a missing local image
#' aborts the whole pandoc render -- losing the entire report.
#' @keywords internal
.build_longitudinal_section <- function(temporal_results, output_dir) {
  prev <- temporal_results$prevalence_over_time
  emer <- temporal_results$emergence_timeline
  n_periods <- if (!is.null(prev) && nrow(prev) > 0) {
    length(unique(prev$period))
  } else {
    0L
  }

  content <- paste0(
    "## Longitudinal Patterns\n\n",
    "Temporal granularity: **",
    .html_esc(as.character(temporal_results$period_type %||% "unknown")),
    "** (", n_periods, " period", if (n_periods == 1L) "" else "s",
    " observed).\n\n"
  )

  has_prev_png <- !is.null(output_dir) &&
    file.exists(file.path(output_dir, "temporal_prevalence.png"))
  content <- paste0(content, if (has_prev_png) {
    "![Theme prevalence over time](temporal_prevalence.png)\n\n"
  } else {
    paste0("*No prevalence chart was produced: all entries fall in a ",
           "single time period.*\n\n")
  })

  has_emer_png <- !is.null(output_dir) &&
    file.exists(file.path(output_dir, "temporal_emergence.png"))
  content <- paste0(content, if (has_emer_png) {
    "![Theme emergence timeline](temporal_emergence.png)\n\n"
  } else {
    paste0("*No emergence chart was produced: no theme had a dated ",
           "first appearance.*\n\n")
  })

  # emergence_timeline carries first_appearance_date as an ISO date string
  # (sortable lexicographically); see .compute_theme_emergence.
  if (!is.null(emer) && nrow(emer) > 0 &&
      "first_appearance_date" %in% names(emer)) {
    em <- emer[!is.na(emer$first_appearance_date), , drop = FALSE]
    if (nrow(em) > 0) {
      em <- em[order(em$first_appearance_date), , drop = FALSE]
      shown <- utils::head(em, 10L)
      content <- paste0(content,
        "**Theme emergence** (first dated appearance",
        if (nrow(em) > 10L) paste0("; first 10 of ", nrow(em)) else "",
        "):\n\n",
        "| Theme | First appearance |\n",
        "|-------|------------------|\n",
        paste0("| ", .html_esc(as.character(shown$theme_name)), " | ",
               .html_esc(as.character(shown$first_appearance_date)), " |\n",
               collapse = ""),
        "\n"
      )
    }
  }

  content
}

.build_methodology_appendix <- function(stats, export_files, config,
                                         excerpt_verification = NULL) {
  # Describe the ACTUAL methodology mode -- pakhom is three-mode by
  # architecture (AC2: M1 reflexive scaffold / M2 codebook collaborative / M3
  # framework applied), so the appendix must NOT hardcode "reflexive thematic
  # analysis" for every run. A Mode-2 run is codebook TA, for which an evolving
  # codebook + AI-judged saturation + run-to-run stability ARE appropriate (Braun &
  # Clarke reserve the no-codebook / no-saturation stance for REFLEXIVE TA). This
  # makes the methodology label internally consistent with the features reported.
  meth_phrase <- switch(config$methodology$mode %||% "",
    "reflexive_scaffold"     = "AI-assisted **reflexive** thematic analysis (Braun & Clarke's reflexive TA)",
    "codebook_collaborative" = "AI-assisted **codebook** thematic analysis (Braun & Clarke's codebook orientation -- an evolving, AI-collaborative codebook with a full provenance/audit trail)",
    "framework_applied"      = "AI-assisted **framework-applied** (deductive) thematic analysis (a pre-specified framework mapped onto the corpus, with an abductive pass for non-fitting data)",
    "AI-assisted thematic analysis")
  # The theme-generation step describes the algorithm that ACTUALLY runs. The
  # production theme generator is the embedding-free multi-pass AI clustering;
  # the retired HAC-on-embeddings path was removed, and a config that pins the
  # deprecated algorithm = "v1" now dispatches to this same generator. So the
  # appendix always emits the multi-pass description -- matching what the
  # pipeline logs and what actually executed.
  theme_step_desc <- "**Multi-pass AI clustering with label-after-clustering** -- the AI sees all codes at once and proposes a partition into conceptual clusters; passes repeat (grouping clusters into larger clusters) until the AI declares convergence (no hardcoded pass count); a separate post-convergence pass assigns theme + subtheme labels. Embedding-free; clustering depth is the AI's dynamic call"
  theme_algo_row <- "Multi-pass AI clustering: AI-proposed partitions until AI-declared convergence, then a separate labeling pass (embedding-free; no count thresholds)"
  theme_algo_short <- "the multi-pass AI clustering"
  content <- paste0(
    "# Appendix A: Methodology\n\n",
    "## Analysis Process\n\n",
    "This analysis employed ", meth_phrase, ", using a progressive sequential coding pipeline:\n\n",
    "1. **Learning from prior studies** -- codebook structures and coding conventions from previous manual analyses\n",
    "2. **Progressive sequential coding** -- each entry read individually; applicable text coded with existing or novel codes\n",
    "3. **AI-judged saturation arbitration** -- an AI arbiter reviews codebook trajectory metrics at an auto-scaled cadence and returns a 3-valued verdict; no hardcoded thresholds\n",
    "4. **Code-aware sentiment analysis** -- sentiment scored on coded entries using assigned codes as context\n",
    "5. ", theme_step_desc, "\n",
    "6. **Deterministic code-path cascading** -- entries mapped to themes through their codes (no AI re-reading)\n",
    "7. **Correlation analysis** -- statistical associations between themes, sentiment, and metadata\n\n",
    "## Top Codes\n\n",
    "```{r code-table}\n",
    "codes <- read_csv('", basename(export_files$codes_file), "', show_col_types = FALSE, comment = '#')\n\n",
    "codes_display <- codes |>\n",
    "  arrange(desc(frequency)) |>\n",
    "  head(30) |>\n",
    "  select(Code = code_text, Type = code_type, Frequency = frequency)\n\n",
    "if (has_dt) {\n",
    "  DT::datatable(codes_display,\n",
    "                options = list(pageLength = 10, dom = 'ftp'),\n",
    "                rownames = FALSE,\n",
    "                class = 'compact stripe',\n",
    "                caption = 'Top 30 Consolidated Codes by Frequency')\n",
    "} else {\n",
    "  knitr::kable(codes_display, caption = 'Top 30 Consolidated Codes by Frequency')\n",
    "}\n",
    "```\n\n"
  )

  # Config table (guard against NULL config)
  if (is.null(config)) config <- list()
  provider_name <- config$ai$provider %||% "openai"
  model_name <- config$ai[[provider_name]]$models$primary %||% "N/A"

  # theme range / max proportion / multi-label rows replaced by
  # the algorithm row. Per C1 (AI decides when to stop) those knobs no
  # longer gate behavior; the methodology appendix now reports the
  # algorithm itself rather than dead config values.
  fast_model <- config$ai[[provider_name]]$models$fast %||% model_name
  reasoning_model <- config$ai[[provider_name]]$models$reasoning %||% "N/A"
  corr_method <- config$analysis$correlations$method %||% "spearman"
  p_adjust <- config$analysis$correlations$adjust_method %||% "bonferroni"
  dynamic_corr <- if (isTRUE(config$analysis$correlations$dynamic_method)) "Yes (per-pair)" else "No"

  content <- paste0(content,
    "## AI Models and Configuration\n\n",
    "| Parameter | Value |\n",
    "|-----------|-------|\n",
    "| AI Provider | ", provider_name, " |\n",
    "| Primary Model | ", model_name, " |\n",
    "| Fast Model (sentiment) | ", fast_model, " |\n",
    "| Reasoning Model (themes, review) | ", reasoning_model, " |\n",
    "| Theme generation | ", theme_algo_row, " |\n",
    "| Correlation Method | ", corr_method, " |\n",
    "| Dynamic Method Selection | ", dynamic_corr, " |\n",
    "| P-Value Adjustment | ", p_adjust, " |\n\n"
  )

  # Note on dynamic correlation method selection
  if (isTRUE(config$analysis$correlations$dynamic_method)) {
    content <- paste0(content,
      "**Dynamic Correlation Method Selection:** When enabled, the correlation method ",
      "is selected per variable pair based on variable types. Binary-binary pairs use ",
      "Pearson (phi coefficient), binary-continuous pairs use Pearson (point-biserial), ",
      "continuous pairs use Pearson if both pass Shapiro-Wilk normality test (otherwise Spearman), ",
      "and any pair involving an ordinal variable (e.g. AI-elicited sentiment / intensity scores) ",
      "uses Spearman rank correlation.\n\n"
    )
  }

  # Token limits table -- restrict to tasks actually used by the v1.0
  # pipeline so legacy keys (consolidation/assignment/relevance from the
  # pre-1.0 architecture) carried over in user configs are not shown.
  # callsite-level temperature overrides for the
  # tasks that pin temperature for replay-equivalence (R7). The
  # config-level table previously showed the CONFIG default (e.g.
  # theming = 0.4) but the runtime calls ai_complete with explicit
  # temperature=0 for theme clustering (R/theme_algorithm_v2.R) and at
  # .ai_judge_saturation + .refresh_code_description. Showing
  # the config value misleads readers about the effective temperature.
  runtime_temp_overrides <- list(
    theming               = 0,  # R/theme_algorithm_v2.R clustering decisions
    saturation_check      = 0,  # R/saturation_arbiter.R::.ai_judge_saturation
    description_refresh   = 0   # R/09_coding.R::.refresh_code_description
  )
  v1_tasks <- c("coding", "theming", "sentiment", "review", "insight", "synthesis")
  max_tokens <- config$ai$max_tokens
  if (!is.null(max_tokens) && length(max_tokens) > 0) {
    active_tasks <- intersect(v1_tasks, names(max_tokens))
    if (length(active_tasks) > 0) {
      content <- paste0(content,
        "## Token Limits per Task\n\n",
        "Temperature shown is the EFFECTIVE runtime value. Tasks where ",
        "the call-site explicitly overrides the config default (for ",
        "replay-equivalence) are marked with `*`.\n\n",
        "| Task | Max Tokens | Temperature |\n",
        "|------|----------:|------------:|\n"
      )
      temps <- config$ai$temperature %||% list()
      for (task_name in active_tasks) {
        config_temp <- temps[[task_name]] %||% "default"
        if (task_name %in% names(runtime_temp_overrides)) {
          temp_str <- paste0(runtime_temp_overrides[[task_name]], "*")
        } else {
          temp_str <- as.character(config_temp)
        }
        content <- paste0(content,
          "| ", task_name, " | ",
          format(max_tokens[[task_name]], big.mark = ","), " | ",
          temp_str, " |\n"
        )
      }
      content <- paste0(content,
        "\n*Call-site override: temperature pinned to 0 regardless of config to ",
        "minimize run-to-run variance. Note: LLM inference is not guaranteed ",
        "bit-identical across runs (especially on providers without a seed ",
        "parameter), and coding / sentiment / insight / synthesis run at higher ",
        "temperatures and are non-deterministic by design.\n\n"
      )
    }
  }

  # Methodological notes
  content <- paste0(content,
    "## Statistical Notes\n\n",
    "**Correlation method:** The default Spearman rank correlation is used between ",
    "continuous sentiment scores and binary theme membership variables. When correlating ",
    "binary (0/1) membership with continuous variables, researchers may also consider ",
    "Pearson correlation (equivalent to point-biserial correlation) as an alternative. ",
    "The `method` parameter in the configuration allows switching between methods.\n\n",
    "**Multiple testing:** Bonferroni and BH-FDR corrections are applied within the ",
    "correlation analysis over the SUBSTANTIVE variable pairs only -- circular / ",
    "analyst-internal pairs are excluded from the correction family and reported ",
    "separately with their raw p-values. However, the full analysis pipeline involves ",
    "multiple sequential decision points (saturation arbitration via the AI ",
    "judge, per-pass theme clustering via ", theme_algo_short, ", and ",
    "deterministic theme cascading). Each decision introduces potential for cumulative ",
    "error. Readers should interpret individual findings within this context and ",
    "prioritize patterns that replicate across runs.\n\n",
    "**Theme group comparisons:** Mann-Whitney U tests (non-parametric) compare continuous ",
    "variables between theme members and non-members. Effect size is the rank-biserial ",
    "correlation, r = 2U/(n1*n2) - 1 (range -1 to 1; the sign indicates the direction of ",
    "the difference, the magnitude follows Cohen's r conventions). P-values are ",
    "Bonferroni-adjusted across all tests.\n\n",
    "**Theme co-occurrence:** Chi-square tests of independence assess whether theme pairs ",
    "co-occur more or less often than expected by chance (computed without Yates' continuity ",
    "correction, so Cramer's V is the primary effect-size signal). Fisher's exact test is ",
    "substituted when any expected cell frequency falls below 5. Effect size is reported as ",
    "Cramer's V.\n\n"
  )

  # Excerpt verification results
  if (!is.null(excerpt_verification)) {
    content <- paste0(content, "## Data Quality: Excerpt Verification\n\n")

    ss <- excerpt_verification$substring_stats
    if (!is.null(ss) && ss$total > 0) {
      content <- paste0(content,
        "**Substring Validation:** ", ss$valid, " of ", ss$total,
        " coded excerpts (", ss$pct_valid, "%) are verbatim substrings of their (cleaned) source text.\n\n"
      )
      if (ss$invalid > 0) {
        content <- paste0(content,
          "<div class='callout callout-neutral'>\n",
          ss$invalid, " excerpt(s) could not be matched as exact substrings. ",
          "These may have been paraphrased by the AI coder or truncated during processing.\n",
          "</div>\n\n"
        )
      }
    }

    cs <- excerpt_verification$coherence_stats
    if (!is.null(cs)) {
      content <- paste0(content,
        "**Theme-Excerpt Coherence:** AI spot-check of ", cs$n_checked,
        " random excerpt-theme pairings yielded a mean coherence score of **",
        cs$mean_score, "/5**"
      )
      if (cs$n_low_coherence > 0) {
        content <- paste0(content,
          " (", cs$n_low_coherence, " pair(s) scored 2 or below).\n\n")
      } else {
        content <- paste0(content, ".\n\n")
      }
    }
  }

  content
}

# ==============================================================================
# Internal: Cross-Run Comparison Section
# ==============================================================================

.build_comparison_section <- function(comparison) {
  content <- paste0(
    "# Cross-Run Comparison {.tabset}\n\n",
    "This section compares the current analysis run against **",
    comparison$n_runs - 1, " previous run(s)**.\n\n"
  )

  # Disclose WHICH models were compared so a reader can verify the headline
  # inter-model reliability statistics are between two distinct models (and
  # not, e.g., two runs of the same model).
  if (isTRUE(comparison$is_inter_model)) {
    if (!is.null(comparison$compared_models) &&
        length(comparison$compared_models) == 2L) {
      cm <- comparison$compared_models
      cr <- comparison$compared_run_ids %||% c("", "")
      content <- paste0(content,
        "**Models compared:** ", .html_esc(cm[1]),
        " (run ", .html_esc(cr[1]), ") vs ", .html_esc(cm[2]),
        " (run ", .html_esc(cr[2]), ").\n\n")
    } else if (!is.null(comparison$unique_models)) {
      content <- paste0(content,
        "**Models across runs:** ",
        .html_esc(paste(comparison$unique_models, collapse = ", ")), ".\n\n")
    }
  } else if (!is.null(comparison$models_used) &&
             length(comparison$models_used) > 0L) {
    content <- paste0(content,
      "**Model:** ",
      .html_esc(paste(unique(unlist(comparison$models_used)), collapse = ", ")),
      " -- all runs used the same model, so this is a same-model stability ",
      "check, not an inter-model comparison.\n\n")
  }

  # --- 1. Sample Overlap ---
  if (!is.null(comparison$sample_overlap)) {
    so <- comparison$sample_overlap
    pw <- so$pairwise

    content <- paste0(content,
      "## Sample Overlap\n\n",
      "<div class='metrics-grid'>\n",
      "<div class='metric-card'><div class='metric-value'>",
        sprintf("%.1f%%", pw$pct_shared),
        "</div><div class='metric-label'>Entries Shared</div></div>\n",
      "<div class='metric-card'><div class='metric-value'>",
        pw$n_new,
        "</div><div class='metric-label'>New Entries</div></div>\n",
      "<div class='metric-card'><div class='metric-value'>",
        pw$n_dropped,
        "</div><div class='metric-label'>Dropped Entries</div></div>\n",
      "</div>\n\n"
    )

    # Interpretation banner
    interp_class <- switch(so$interpretation,
      "identical sample" = "positive",
      "mostly same sample" = "positive",
      "overlapping samples" = "neutral",
      "largely different samples" = "negative",
      "neutral"
    )
    content <- paste0(content,
      "<div class='callout callout-", interp_class, "'>\n",
      "<strong>Sample Assessment:</strong> ",
      .html_esc(tools::toTitleCase(so$interpretation)),
      " (Jaccard index: ", sprintf("%.3f", pw$jaccard_index), ")",
      if (so$text_changes > 0) paste0(
        ". Note: ", so$text_changes, " shared entries had text changes (re-preprocessing detected)."
      ) else "",
      "\n</div>\n\n"
    )

    # Source composition table if available
    if (nrow(so$per_run) > 0 && !all(is.na(so$per_run$posts_pct))) {
      content <- paste0(content,
        "**Source Composition Across Runs:**\n\n",
        "| Run | Total | Posts | Comments | Posts % |\n",
        "|-----|-------|-------|----------|--------|\n"
      )
      for (i in seq_len(nrow(so$per_run))) {
        r <- so$per_run[i, ]
        content <- paste0(content,
          "| ", .html_esc(r$run_id), " | ", r$total_entries,
          " | ", ifelse(is.na(r$n_from_posts), "&mdash;", r$n_from_posts),
          " | ", ifelse(is.na(r$n_from_comments), "&mdash;", r$n_from_comments),
          " | ", ifelse(is.na(r$posts_pct), "&mdash;", paste0(r$posts_pct, "%")),
          " |\n"
        )
      }
      content <- paste0(content, "\n")
    }
  }

  # --- 2. Sentiment Drift ---
  if (!is.null(comparison$sentiment_drift)) {
    sd <- comparison$sentiment_drift

    content <- paste0(content,
      "## Sentiment Drift\n\n"
    )

    # Summary metrics
    if (!is.null(sd$summary) && !is.na(sd$summary$mean_shift)) {
      shift_dir <- if (sd$summary$mean_shift > 0.05) "more positive" else
        if (sd$summary$mean_shift < -0.05) "more negative" else "stable"

      content <- paste0(content,
        "<div class='metrics-grid'>\n",
        "<div class='metric-card'><div class='metric-value'>",
          sprintf("%+.3f", sd$summary$mean_shift),
          "</div><div class='metric-label'>Mean Shift</div></div>\n",
        "<div class='metric-card'><div class='metric-value'>",
          sprintf("%.1f%%", sd$summary$reclassification_rate %||% 0),
          "</div><div class='metric-label'>Emotion Reclassification</div></div>\n",
        "<div class='metric-card'><div class='metric-value'>",
          sd$summary$n_shared_entries,
          "</div><div class='metric-label'>Entries Compared</div></div>\n",
        "</div>\n\n",
        "Sentiment has been **", shift_dir, "** between runs.\n\n"
      )
    }

    # Per-run sentiment trend table
    if (nrow(sd$per_run) > 0) {
      content <- paste0(content,
        "**Sentiment Summary Per Run:**\n\n",
        "| Run | Mean | Median | SD | Top Emotions | % Negative | % Positive |\n",
        "|-----|------|--------|----|--------------------|------------|------------|\n"
      )
      for (i in seq_len(nrow(sd$per_run))) {
        r <- sd$per_run[i, ]
        content <- paste0(content,
          "| ", .html_esc(r$run_id),
          " | ", sprintf("%.3f", r$mean_sentiment),
          " | ", sprintf("%.3f", r$median_sentiment),
          " | ", sprintf("%.3f", r$sd_sentiment),
          " | ", .html_esc(r$top_emotions %||% "\u2014"),
          " | ", sprintf("%.1f%%", r$pct_negative),
          " | ", sprintf("%.1f%%", r$pct_positive),
          " |\n"
        )
      }
      content <- paste0(content, "\n")
    }

    # Sentiment drift plot (if 2+ runs)
    if (nrow(sd$per_run) >= 2) {
      content <- paste0(content,
        "```{r sentiment-drift-plot, fig.width=8, fig.height=4, echo=FALSE}\n",
        "sentiment_trend <- data.frame(\n",
        "  run = c(", paste0('"', sd$per_run$run_id, '"', collapse = ", "), "),\n",
        "  mean_sentiment = c(", paste(sd$per_run$mean_sentiment, collapse = ", "), "),\n",
        "  stringsAsFactors = FALSE\n",
        ")\n",
        "sentiment_trend$run <- factor(sentiment_trend$run, levels = sentiment_trend$run)\n",
        "ggplot(sentiment_trend, aes(x = run, y = mean_sentiment, group = 1)) +\n",
        "  geom_line(color = '#4477AA', linewidth = 1.2) +\n",
        "  geom_point(color = '#4477AA', size = 3) +\n",
        "  geom_hline(yintercept = 0, linetype = 'dashed', color = '#999') +\n",
        "  labs(title = 'Mean Sentiment Across Runs', x = NULL, y = 'Mean Sentiment') +\n",
        "  theme_report() +\n",
        "  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))\n",
        "```\n\n"
      )
    }
  }

  # --- 3. Code Stability ---
  if (!is.null(comparison$code_stability)) {
    cs <- comparison$code_stability

    content <- paste0(content,
      "## Code Stability\n\n"
    )

    if (!is.null(cs$stability)) {
      content <- paste0(content,
        "<div class='metrics-grid'>\n",
        "<div class='metric-card'><div class='metric-value'>",
          sprintf("%.1f%%", cs$stability$jaccard_overall * 100),
          "</div><div class='metric-label'>Code Set Overlap</div></div>\n",
        "<div class='metric-card'><div class='metric-value'>",
          sprintf("%.1f%%", cs$stability$churn_rate * 100),
          "</div><div class='metric-label'>Churn Rate</div></div>\n",
        "<div class='metric-card'><div class='metric-value'>",
          cs$stability$n_stable %||% 0,
          " / ", (cs$stability$n_renamed %||% 0),
          " / ", cs$stability$n_new %||% 0,
          " / ", cs$stability$n_dropped %||% 0,
          "</div><div class='metric-label'>Stable / Renamed / New / Dropped</div></div>\n",
        "</div>\n\n"
      )
    }

    # Renamed codes table
    if (!is.null(cs$pairwise$renamed) && nrow(cs$pairwise$renamed) > 0) {
      content <- paste0(content,
        "**Renamed Codes (high similarity, different text):**\n\n",
        "| Previous | Current | Similarity |\n",
        "|----------|---------|------------|\n"
      )
      for (i in seq_len(min(10, nrow(cs$pairwise$renamed)))) {
        r <- cs$pairwise$renamed[i, ]
        content <- paste0(content,
          "| ", .html_esc(r$code_prev), " | ", .html_esc(r$code_curr),
          " | ", sprintf("%.1f%%", r$similarity * 100), " |\n"
        )
      }
      content <- paste0(content, "\n")
    }
  }

  # --- 4. Theme Evolution ---
  if (!is.null(comparison$theme_evolution) && !is.null(comparison$theme_evolution$pairwise)) {
    te <- comparison$theme_evolution$pairwise

    content <- paste0(content,
      "## Theme Evolution\n\n"
    )

    # Persisted themes
    if (nrow(te$persisted) > 0) {
      content <- paste0(content,
        "**Persisted Themes** (matched across runs):\n\n",
        "| Previous | Current | Name Sim | Code Overlap | Entries (prev &rarr; curr) | Sentiment |\n",
        "|----------|---------|----------|-------------|----------------------|----------|\n"
      )
      for (i in seq_len(nrow(te$persisted))) {
        r <- te$persisted[i, ]
        ec_change <- if (!is.na(r$entry_count_prev) && !is.na(r$entry_count_curr)) {
          paste0(r$entry_count_prev, " &rarr; ", r$entry_count_curr)
        } else "&mdash;"
        sent_change <- if (!is.na(r$sentiment_prev) && !is.na(r$sentiment_curr)) {
          paste0(r$sentiment_prev, " &rarr; ", r$sentiment_curr)
        } else "&mdash;"

        content <- paste0(content,
          "| ", .html_esc(r$theme_prev), " | ", .html_esc(r$theme_curr),
          " | ", sprintf("%.0f%%", r$name_sim * 100),
          " | ", sprintf("%.0f%%", r$code_jaccard * 100),
          " | ", ec_change,
          " | ", sent_change,
          " |\n"
        )
      }
      content <- paste0(content, "\n")
    }

    # New themes
    if (nrow(te$new) > 0) {
      content <- paste0(content,
        "**New Themes** (not in previous run):\n\n"
      )
      for (i in seq_len(nrow(te$new))) {
        content <- paste0(content,
          "- <span class='comparison-badge comparison-badge-new'>NEW</span> **",
          .html_esc(te$new$theme_name[i]), "**",
          if (!is.na(te$new$entry_count[i])) paste0(" (", te$new$entry_count[i], " entries)") else "",
          "\n"
        )
      }
      content <- paste0(content, "\n")
    }

    # Disappeared themes
    if (nrow(te$disappeared) > 0) {
      content <- paste0(content,
        "**Disappeared Themes** (in previous run, not current):\n\n"
      )
      for (i in seq_len(nrow(te$disappeared))) {
        content <- paste0(content,
          "- <span class='comparison-badge comparison-badge-gone'>GONE</span> **",
          .html_esc(te$disappeared$theme_name[i]), "**",
          if (!is.na(te$disappeared$entry_count[i])) paste0(" (had ", te$disappeared$entry_count[i], " entries)") else "",
          "\n"
        )
      }
      content <- paste0(content, "\n")
    }
  }

  # --- 5. Entry Migration ---
  if (!is.null(comparison$entry_migration) && !is.na(comparison$entry_migration$stability_rate)) {
    em <- comparison$entry_migration

    content <- paste0(content,
      "## Entry Migration\n\n",
      "<div class='metrics-grid'>\n",
      "<div class='metric-card'><div class='metric-value'>",
        sprintf("%.1f%%", em$stability_rate * 100),
        "</div><div class='metric-label'>Theme Stability</div></div>\n",
      "<div class='metric-card'><div class='metric-value'>",
        em$n_stable,
        "</div><div class='metric-label'>Stable Entries</div></div>\n",
      "<div class='metric-card'><div class='metric-value'>",
        em$n_migrated,
        "</div><div class='metric-label'>Migrated Entries</div></div>\n",
      "</div>\n\n"
    )

    # Migration heatmap
    if (nrow(em$matrix) > 0) {
      # Serialize matrix data for the ggplot chunk
      mat_str <- paste0(
        "data.frame(\n",
        "  theme_prev = c(", paste0('"', em$matrix$theme_prev, '"', collapse = ", "), "),\n",
        "  theme_curr = c(", paste0('"', em$matrix$theme_curr, '"', collapse = ", "), "),\n",
        "  n_entries = c(", paste(em$matrix$n_entries, collapse = ", "), "),\n",
        "  stringsAsFactors = FALSE\n",
        ")"
      )

      content <- paste0(content,
        "```{r migration-heatmap, fig.width=10, fig.height=7, echo=FALSE}\n",
        "migration_data <- ", mat_str, "\n",
        "# Truncate long theme names\n",
        "migration_data$theme_prev <- ifelse(nchar(migration_data$theme_prev) > 30,\n",
        "  paste0(substr(migration_data$theme_prev, 1, 27), '...'), migration_data$theme_prev)\n",
        "migration_data$theme_curr <- ifelse(nchar(migration_data$theme_curr) > 30,\n",
        "  paste0(substr(migration_data$theme_curr, 1, 27), '...'), migration_data$theme_curr)\n",
        "ggplot(migration_data, aes(x = theme_curr, y = theme_prev, fill = n_entries)) +\n",
        "  geom_tile(color = 'white', linewidth = 0.5) +\n",
        "  geom_text(aes(label = n_entries), color = 'black', size = 3.5) +\n",
        "  scale_fill_gradient(low = '#f0f4ff', high = '#4477AA', name = 'Entries') +\n",
        "  labs(title = 'Entry Migration Between Runs',\n",
        "       x = 'Current Run Themes', y = 'Previous Run Themes') +\n",
        "  theme_report() +\n",
        "  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),\n",
        "        axis.text.y = element_text(size = 8))\n",
        "```\n\n"
      )
    }
  }

  # --- 6. Correlation Stability ---
  if (!is.null(comparison$correlation_stability)) {
    cors <- comparison$correlation_stability

    content <- paste0(content,
      "## Correlation Stability\n\n"
    )

    if (nrow(cors$persistent) > 0) {
      # Denominator = runs that exported correlation data (n_corr_runs),
      # not all compared runs; %||% falls back for comparison objects
      # serialized before n_corr_runs existed.
      corr_denom <- cors$n_corr_runs %||% comparison$n_runs
      content <- paste0(content,
        "**Persistent Correlations** (significant in all ", corr_denom,
        " runs with correlation data):\n\n",
        "| Variable Pair | Mean r | Runs Significant |\n",
        "|--------------|--------|------------------|\n"
      )
      for (i in seq_len(nrow(cors$persistent))) {
        r <- cors$persistent[i, ]
        content <- paste0(content,
          "| ", .html_esc(r$var1), " &harr; ", .html_esc(r$var2),
          " | ", sprintf("%.3f", r$mean_correlation),
          " | ", r$n_runs_significant, "/", corr_denom,
          " |\n"
        )
      }
      content <- paste0(content, "\n")
    }

    n_inter <- nrow(cors$intermittent)
    n_spec <- nrow(cors$run_specific)
    if (n_inter > 0 || n_spec > 0) {
      content <- paste0(content,
        "Additionally: **", n_inter, "** intermittent correlation(s) ",
        "and **", n_spec, "** run-specific correlation(s) were identified.\n\n"
      )
    }

    if (nrow(cors$persistent) == 0 && n_inter == 0 && n_spec == 0) {
      content <- paste0(content,
        "No significant correlations found across runs for comparison.\n\n"
      )
    }
  }

  # --- 7. Run Dashboard ---
  if (!is.null(comparison$dashboard) && nrow(comparison$dashboard) > 0) {
    db <- comparison$dashboard

    content <- paste0(content,
      "## Run Dashboard\n\n",
      "| Run | Date | Entries | Themes | Mean Sent. | Emotion | Sig. Corr. | Codes |\n",
      "|-----|------|---------|--------|-----------|---------|------------|-------|\n"
    )
    for (i in seq_len(nrow(db))) {
      r <- db[i, ]
      content <- paste0(content,
        "| ", .html_esc(r$run_id),
        " | ", .html_esc(r$date),
        " | ", r$total_entries,
        " | ", r$n_themes,
        " | ", sprintf("%.3f", r$mean_sentiment),
        " | ", .html_esc(r$top_emotions %||% "\u2014"),
        " | ", r$n_significant_correlations,
        " | ", r$n_codes,
        " |\n"
      )
    }
    content <- paste0(content, "\n")
  }

  content
}

# ==============================================================================
# Internal: ggplot2 theme code for Rmd setup chunk
# ==============================================================================

.ggplot_theme_code <- function() {
  '
# Custom ggplot2 theme matching report style
theme_report <- function() {
  theme_minimal(base_family = "sans") +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#EAECEE", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "#2C3E50", linewidth = 0.3),
      axis.ticks = element_line(color = "#BDC3C7", linewidth = 0.3),
      axis.text = element_text(color = "#7F8C8D", size = 10),
      axis.title = element_text(color = "#2C3E50", size = 11, face = "bold"),
      plot.title = element_text(color = "#2C3E50", size = 15, face = "bold",
                                margin = margin(b = 8)),
      plot.subtitle = element_text(color = "#7F8C8D", size = 11,
                                   margin = margin(b = 12)),
      legend.position = "bottom",
      legend.background = element_rect(fill = "white", color = NA),
      legend.title = element_text(color = "#2C3E50", face = "bold", size = 9),
      legend.text = element_text(color = "#7F8C8D", size = 9),
      plot.margin = margin(15, 15, 15, 15)
    )
}

report_colors <- c(
  "#3498DB", "#9B59B6", "#E74C3C", "#27AE60",
  "#F39C12", "#1ABC9C", "#E67E22", "#34495E",
  "#16A085", "#8E44AD"
)

# Expand report_colors palette to n colors using interpolation
expand_report_colors <- function(n) {
  if (n <= length(report_colors)) {
    return(report_colors[seq_len(n)])
  }
  grDevices::colorRampPalette(report_colors)(n)
}

sentiment_colors <- c(
  "negative" = "#E74C3C",
  "neutral" = "#F39C12",
  "positive" = "#27AE60"
)
'
}

# ==============================================================================
# Internal: Generate separate theme detail HTML files
# ==============================================================================

.generate_theme_detail_htmls <- function(theme_stats, theme_order, export_files,
                                          output_dir, data = NULL,
                                          coding_results = NULL,
                                          methodology_mode = NULL) {
  detail_dir <- file.path(output_dir, "theme_details")
  dir.create(detail_dir, recursive = TRUE, showWarnings = FALSE)

  # Vendored client-side libraries for the interactive entries table (jQuery +
  # DataTables, both MIT-licensed; see inst/COPYRIGHTS). Copied in beside the
  # detail pages so each report is fully self-contained and renders its tables
  # OFFLINE -- no CDN, no version rot, no MITM surface. Referenced by local
  # filename in the <head> below. file.copy is an overwrite-safe no-op when the
  # target already exists; a missing source just degrades the table to static
  # (the DataTables init script tolerates its absence).
  for (.asset in c("jquery-3.7.1.min.js",
                   "jquery.dataTables.min.js",
                   "jquery.dataTables.min.css")) {
    .asset_src <- system.file("rmd", .asset, package = "pakhom")
    if (nzchar(.asset_src) && file.exists(.asset_src)) {
      file.copy(.asset_src, file.path(detail_dir, .asset), overwrite = TRUE)
    }
  }

  generated <- list()

  # AC4: each standalone theme-detail page carries the same methodology badge
  # (mode + run id) as the main report. A NULL mode -- legacy/test callers with
  # no methodology block -- renders no badge. The badge uses the same
  # .methodology-stamp class and the page already links ../styles.css, so it is
  # styled identically to the main report.
  meth_stamp <- if (!is.null(methodology_mode)) {
    stamp_methodology_html(methodology_mode, run_id = basename(output_dir))
  } else ""

  for (tn in theme_order) {
    if (!tn %in% names(theme_stats)) next
    ts <- theme_stats[[tn]]
    safe_name <- make_safe_filename(tn)
    detail_file <- file.path(detail_dir, paste0("theme_", safe_name, ".html"))

    html <- paste0(
      '<!DOCTYPE html>\n<html lang="en">\n<head>\n',
      '<meta charset="UTF-8">\n',
      '<meta name="viewport" content="width=device-width, initial-scale=1.0">\n',
      '<title>', .html_esc(tn), ' -- Theme Details</title>\n',
      '<link rel="stylesheet" href="../styles.css">\n',
      # jQuery + DataTables are VENDORED (bundled in inst/rmd/, copied into this
      # theme_details/ dir above) and referenced by LOCAL filename -- no CDN.
      # Each report is thus fully self-contained: interactive tables render
      # offline, forever, with no network dependency, no CDN version-rot, and no
      # compromised/MITM'd-payload surface. Licences + provenance (incl. the
      # SHA-256 of each bundled file) are recorded in inst/COPYRIGHTS. (The init
      # script below no-ops gracefully if an asset is ever absent -- the table
      # degrades to static rather than erroring.)
      '<link rel="stylesheet" href="jquery.dataTables.min.css">\n',
      '<script src="jquery-3.7.1.min.js"></script>\n',
      '<script src="jquery.dataTables.min.js"></script>\n',
      '<style>\n',
      '#entries-table { table-layout: fixed; width: 100% !important; }\n',
      '#entries-table th:nth-child(1) { width: 40%; }\n',
      '#entries-table th:nth-child(2) { width: 10%; }\n',
      '#entries-table th:nth-child(3) { width: 12%; }\n',
      '#entries-table th:nth-child(4) { width: 13%; }\n',
      '#entries-table th:nth-child(5) { width: 25%; }\n',
      '#entries-table td:first-child { max-width: 400px; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }\n',
      '#entries-table td:first-child:hover { white-space: normal; word-wrap: break-word; }\n',
      '</style>\n',
      '</head>\n<body>\n',
      '<div style="max-width: 900px; margin: 2rem auto; padding: 0 1.5rem;">\n',
      '<a href="../analysis_report.html" class="appendix-back-link">Back to Report</a>\n',
      '<h1>', .html_esc(tn), '</h1>\n',
      meth_stamp,
      '<p class="theme-description">', .html_esc(ts$description %||% ""), '</p>\n',
      '<div class="theme-meta">\n',
      '<div class="theme-meta-item"><span class="theme-meta-value">', ts$n_entries, '</span>',
      '<span class="theme-meta-label">Total Entries</span></div>\n',
      '<div class="theme-meta-item"><span class="theme-meta-value">', ts$sentiment$mean, '</span>',
      '<span class="theme-meta-label">Mean Sentiment</span></div>\n',
      '<div class="theme-meta-item"><span class="theme-meta-value">', ts$intensity$mean, '</span>',
      '<span class="theme-meta-label">Mean Intensity</span></div>\n',
      '</div>\n'
    )

    # Subthemes (ts$subthemes_structured is a list of Subtheme S3
    # objects with virtual NA-named wrappers already filtered out by
    # aggregate_theme_statistics, so this is a clean render).
    # nested sub-subthemes render as indented
    # children below their parent. Recursive emitter preserves the
    # tree's depth budget (max 3 by default; deeper trees just keep
    # indenting).
    if (!is.null(ts$subthemes_structured) && length(ts$subthemes_structured) > 0) {
      render_subtheme_html <- function(s, depth) {
        s_name <- s$name %||% ""
        s_desc <- s$description %||% ""
        if (is.na(s_name) || nchar(s_name) == 0L) return("")
        indent_px <- 16 * (depth - 1L)
        out <- paste0(
          '<div class="subtheme-item" style="margin-bottom: 0.5rem; ',
          'margin-left: ', indent_px, 'px;">\n',
          '<strong>', .html_esc(s_name), '</strong>',
          if (nchar(s_desc) > 0) paste0(' &mdash; ', .html_esc(s_desc)) else "",
          '\n</div>\n'
        )
        for (child in s$subthemes %||% list()) {
          out <- paste0(out, render_subtheme_html(child, depth + 1L))
        }
        out
      }
      html <- paste0(html, '<h2>Subthemes</h2>\n<div class="subthemes-list">\n')
      for (s in ts$subthemes_structured) {
        html <- paste0(html, render_subtheme_html(s, 1L))
      }
      html <- paste0(html, '</div>\n')
    } else if (length(ts$subthemes) > 0 && !all(is.na(ts$subthemes))) {
      subs <- ts$subthemes[!is.na(ts$subthemes) & nchar(as.character(ts$subthemes)) > 0]
      if (length(subs) > 0) {
        html <- paste0(html, '<h2>Subthemes</h2>\n<div>\n')
        for (s in subs) {
          html <- paste0(html, '<span class="keyword-pill">', .html_esc(s), '</span>\n')
        }
        html <- paste0(html, '</div>\n')
      }
    }

    # render the paper-style per-subtheme summary
    # table (Subtheme | n | Median(MAD) | Mean(SD) per metric | examples
    # of comments) on each per-theme detail page too. An earlier
    # table only appeared in the main report -- so when a reader clicked
    # "View Full Details" they LOST the paper-style breakdown that's the
    # most important paper-style output. The renderer returns "" when the
    # theme has no real subthemes (virtual-only) or no detectable
    # metrics, so the section disappears gracefully when not applicable.
    subtheme_table_html <- .build_subtheme_summary_table(ts)
    if (nzchar(subtheme_table_html)) {
      html <- paste0(html,
        '<div class="detail-subtheme-summary">\n',
        subtheme_table_html,
        '</div>\n'
      )
    }

    # per-theme temporal panel on the detail page too (so a reader
    # who clicked through keeps the posting-time breakdown). "" when absent.
    temporal_panel_html <- .build_temporal_panel(ts)
    if (nzchar(temporal_panel_html)) {
      html <- paste0(html,
        '<div class="detail-temporal-panel">\n',
        temporal_panel_html,
        '</div>\n'
      )
    }

    # Keywords
    if (!is.null(ts$keywords) && length(ts$keywords) > 0) {
      html <- paste0(html, '<h2>Keywords</h2>\n<div class="keywords-container">\n')
      for (k in ts$keywords) {
        html <- paste0(html, '<span class="keyword-pill">', .html_esc(k), '</span>\n')
      }
      html <- paste0(html, '</div>\n')
    }

    # Quotes
    html <- paste0(html, '<h2>Representative Quotes</h2>\n')
    if (!is.null(ts$quotes_with_context) && length(ts$quotes_with_context) > 0) {
      for (qt in names(ts$quotes_with_context)) {
        q <- ts$quotes_with_context[[qt]]
        if (is.null(q$text) || is.na(q$text)) next
        q_sent <- q$sentiment %||% 0
        qclass <- if (is.na(q_sent)) "neutral" else if (q_sent < .SENTIMENT_NEGATIVE_THRESHOLD) "negative" else if (q_sent > .SENTIMENT_POSITIVE_THRESHOLD) "positive" else "neutral"
        html <- paste0(html,
          '<div class="quote-box ', qclass, '">\n',
          .html_esc(gsub("\n", " ", q$text)), '\n',
          '<div class="quote-meta">\n',
          '<span class="sentiment-pill ', qclass, '">Sentiment: ', round(q_sent, 2), '</span>\n',
          ' Emotion: ', .html_esc(q$emotion %||% "N/A"), '\n',
          '</div>\n</div>\n'
        )
      }
    }

    # Interactive entry table (Issue 8)
    if (!is.null(data)) {
      safe_col <- paste0("theme_membership_", make.names(tn))
      if (safe_col %in% names(data)) {
        theme_entries <- data[data[[safe_col]] == 1L, ]
      } else if ("emerged_themes" %in% names(data)) {
        theme_entries <- data[!is.na(data$emerged_themes) &
                               .entry_in_theme(data$emerged_themes, tn), ]
      } else {
        theme_entries <- data[0, ]
      }
      if (nrow(theme_entries) > 0) {
        text_col <- if ("original_text" %in% names(theme_entries)) "original_text" else "std_text"

        html <- paste0(html, '<h2>All Entries</h2>\n',
          '<table id="entries-table" class="display" style="width:100%">\n',
          '<thead><tr><th>Text</th><th>Sentiment</th><th>Emotion</th>',
          '<th>Sent. Confidence</th><th>Codes</th></tr></thead>\n<tbody>\n')

        entry_excerpts <- if (!is.null(coding_results)) coding_results$entry_excerpts else NULL

        for (ri in seq_len(nrow(theme_entries))) {
          row <- theme_entries[ri, ]
          entry_id <- as.character(row$std_id)
          full_text <- as.character(row[[text_col]])
          # Use the word-boundary
          # helper so the per-theme entries table doesn't cut mid-word
          # either (the helper was first applied at the metric-tagged
          # quote site in R/16_report_helpers.R; the audit caught that
          # this parallel site at the per-theme detail HTML was still
          # doing a hard substr).
          display_text <- .html_esc(
            .truncate_quote_word_boundary(full_text, max_chars = 200L)
          )
          sent_val <- round(row$sentiment_score %||% 0, 2)
          emotion <- .html_esc(row$all_emotions %||% "N/A")
          conf <- round(row$confidence %||% 0, 2)

          # Get codes for this entry
          codes_str <- ""
          if (!is.null(entry_excerpts) && !is.null(entry_excerpts[[entry_id]])) {
            code_names <- vapply(entry_excerpts[[entry_id]], function(x) x$code %||% "", character(1))
            codes_str <- .html_esc(paste(unique(code_names), collapse = "; "))
          }

          html <- paste0(html,
            '<tr><td>', display_text, '</td><td>', sent_val,
            '</td><td>', emotion, '</td><td>', conf,
            '</td><td>', codes_str, '</td></tr>\n')
        }

        html <- paste0(html, '</tbody>\n</table>\n')
      }
    }

    # DataTable init script (graceful offline fallback)
    html <- paste0(html,
      '<script>\n',
      'document.addEventListener("DOMContentLoaded", function() {\n',
      '  if (typeof jQuery !== "undefined" && jQuery.fn.DataTable) {\n',
      '    jQuery("#entries-table").DataTable({pageLength: 25, scrollX: true});\n',
      '  }\n',
      '});\n',
      '</script>\n')

    # Download link
    csv_info <- export_files$theme_csv_files[[tn]]
    if (!is.null(csv_info)) {
      csv_rel <- paste0("../", csv_info$relative_path)
      html <- paste0(html,
        '<div class="download-box">\n',
        '<a href="', csv_rel, '" class="download-link" download>',
        'Download All ', ts$n_entries, ' Entries as CSV</a>\n',
        '</div>\n'
      )
    }

    html <- paste0(html, '</div>\n</body>\n</html>')
    writeLines(html, detail_file)

    generated[[tn]] <- list(
      file_path = detail_file,
      relative_path = paste0("theme_details/theme_", safe_name, ".html")
    )
  }

  log_info("Generated {length(generated)} theme detail HTML files")
  generated
}
