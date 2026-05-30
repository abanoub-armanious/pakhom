#!/usr/bin/env Rscript
# ==============================================================================
# Phase 61.5 PRE-FLIGHT SMOKE -- the cheap real-corpus run before the full
# empirical re-validation. ~50 entries, review points OFF, the binge-eating x
# sleep x medication corpus, the REAL OpenAI API. ~$1-2, ~3-5 min.
#
# It drives the project's actual root config.yaml (the config a real user has --
# complete with data$tables, preprocessing, models, rate limits) and overrides
# only what a dry-run needs: absolute DB path, test_mode sampling, review points
# OFF, and an output dir inside the worktree.
#
# Purpose (what mocks can't cover):
#   * Live Step 2.5: real articulate_relevance_criterion() + interpret_metrics()
#     against gpt-4o on REAL heavy-tailed (score), bounded (upvote_ratio) and
#     temporal (created_utc) columns.
#   * The AUDITED path: run_analysis creates a real audit_log, so this exercises
#     log_ai_decision/log_ai_request("methodology_assistant", ...) -- the exact
#     class of landmine that crashed prior phases (C1 / f6b5005), which the
#     NULL-audit-log unit tests do not reach.
#   * The Phase 61.4 report surfaces END-TO-END on real data: Methodology Setup
#     section, AI-chosen primitive columns in the per-subtheme table, and the
#     per-theme temporal panel.
#
# Usage:  Rscript scripts/phase61_preflight_smoke.R [sample_size]   (default 50)
# Exits non-zero with a clear FAIL line if any pre-flight assertion fails, so it
# can gate the full 61.5 spend.
# ==============================================================================

# CRITICAL: load the WORKTREE package code, NOT the installed build. A prior
# smoke used library(pakhom) and silently loaded a stale pre-Phase-61 install
# from the R library -- so Step 2.5 never ran and ~$1-2 was spent on the wrong
# code. load_all() the package source so the run exercises THIS worktree.
.find_pkg_dir <- function() {
  d <- getwd()
  for (i in 1:8) {
    desc <- file.path(d, "DESCRIPTION")
    if (file.exists(desc) &&
        any(grepl("^Package:\\s*pakhom", readLines(desc, warn = FALSE)))) return(d)
    d <- dirname(d)
  }
  stop("Could not locate the pakhom package source dir (DESCRIPTION with Package: pakhom).")
}
PKG_DIR <- .find_pkg_dir()
suppressMessages(suppressWarnings(pkgload::load_all(PKG_DIR, quiet = TRUE)))
# Fail LOUDLY if we somehow still don't have the Phase 61 surface (the exact
# failure mode the prior run hit) -- never spend API on stale code again.
if (!exists("run_methodology_assistant", where = asNamespace("pakhom")))
  stop("Loaded pakhom lacks run_methodology_assistant -- Phase 61 code not loaded. Aborting before any API spend.")
cat(sprintf("[smoke] loaded worktree package from: %s\n", PKG_DIR))

args <- commandArgs(trailingOnly = TRUE)
SAMPLE_SIZE <- if (length(args) >= 1L) as.integer(args[[1]]) else 50L

# --- locate project root (.Renviron + DB + config.yaml live at the repo root) -
find_root <- function() {
  env <- Sys.getenv("PAKHOM_PROJECT_ROOT")
  if (nzchar(env) && file.exists(file.path(env, "RedditBingeEating_SleepData.db"))) return(env)
  d <- getwd()
  for (i in 1:8) {
    if (file.exists(file.path(d, "RedditBingeEating_SleepData.db"))) return(d)
    d <- dirname(d)
  }
  stop("Could not locate project root (RedditBingeEating_SleepData.db).")
}
PROJECT_ROOT <- find_root()
Sys.setenv(PAKHOM_PROJECT_ROOT = PROJECT_ROOT)
readRenviron(file.path(PROJECT_ROOT, ".Renviron"))
if (!nzchar(Sys.getenv("OPENAI_API_KEY")))
  stop("OPENAI_API_KEY not set after readRenviron(", PROJECT_ROOT, "/.Renviron)")
rs_pandoc <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"
if (dir.exists(rs_pandoc)) Sys.setenv(RSTUDIO_PANDOC = rs_pandoc)

DB_ABS  <- file.path(PROJECT_ROOT, "RedditBingeEating_SleepData.db")
CFG_ABS <- file.path(PROJECT_ROOT, "config.yaml")
stopifnot(file.exists(DB_ABS), file.exists(CFG_ABS))
cat(sprintf("[smoke] project root: %s\n[smoke] sample_size: %d\n[smoke] config: %s\n",
            PROJECT_ROOT, SAMPLE_SIZE, CFG_ABS))

# --- drive the REAL root config.yaml, overriding only dry-run knobs -----------
overrides <- list(
  "data.database"                       = DB_ABS,                 # absolute, hermetic
  "analysis.test_mode.enabled"          = TRUE,
  "analysis.test_mode.sample_size"      = SAMPLE_SIZE,
  "analysis.test_mode.seed"             = 42L,
  "analysis.review_points.after_coding" = FALSE,
  "analysis.review_points.after_themes" = FALSE,
  "analysis.themes.algorithm"           = "v2",
  "learning.enabled"                    = FALSE,                  # skip manuscript learning for the smoke
  "output.results_dir"                  = file.path(getwd(), "outputs", "results")
)

t0 <- Sys.time()
res <- run_analysis(CFG_ABS, config_overrides = overrides)
elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
cat(sprintf("[smoke] run_analysis finished in %s min\n", elapsed))

# --- locate the run dir ------------------------------------------------------
run_dir <- res$output_dir
if (is.null(run_dir) || !dir.exists(run_dir)) {
  base <- file.path(getwd(), "outputs", "results")
  runs <- list.dirs(base, recursive = FALSE)
  run_dir <- runs[which.max(file.info(runs)$mtime)]
}
cat(sprintf("[smoke] run dir: %s\n", run_dir))

# --- assertions (Phase 61 specific) ------------------------------------------
fails <- character(0)
ok <- function(label, cond) {
  cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", label))
  if (!isTRUE(cond)) fails <<- c(fails, label)
}

md   <- file.path(run_dir, "rules", "methodology_articulations.md")
json <- file.path(run_dir, "rules", "methodology_articulations.json")
ok("methodology_articulations.md archived", file.exists(md))
ok("methodology_articulations.json archived", file.exists(json))
art <- if (file.exists(json)) tryCatch(jsonlite::fromJSON(json, simplifyVector = FALSE),
                                       error = function(e) NULL) else NULL
ok("articulations JSON parses", !is.null(art))

rc <- art$relevance_criterion %||% ""
ok("relevance_criterion is non-trivial (>= 60 chars)", nchar(rc) >= 60L)
cat(sprintf("    relevance_criterion: %s\n", substr(rc, 1, 200)))

metrics <- art$metrics %||% list()
ok("AI interpreted >= 1 metric column", length(metrics) >= 1L)
prim_names <- unique(unlist(lapply(metrics, function(m)
  vapply(m$requested_primitives %||% list(),
         function(p) as.character(p$primitive %||% ""), character(1)))))
ok("AI requested >= 1 catalog primitive", length(prim_names) >= 1L)
cat(sprintf("    interpreted columns: %s\n",
            paste(vapply(metrics, function(m) m$column_name %||% "?", character(1)),
                  collapse = ", ")))
cat(sprintf("    requested primitives: %s\n", paste(prim_names, collapse = ", ")))
catalog_names <- tryCatch(pakhom:::metric_catalog_names(), error = function(e) character(0))
unknown <- if (length(catalog_names)) setdiff(prim_names, catalog_names) else character(0)
cat(sprintf("    requested-but-not-in-catalog (R4 gaps, informational): %s\n",
            if (length(unknown)) paste(unknown, collapse = ", ") else "(none)"))

tcols <- art$temporal_columns %||% list()
ok("AI interpreted the timestamp column", length(tcols) >= 1L)

html_path <- file.path(run_dir, "analysis_report.html")
ok("analysis_report.html rendered", file.exists(html_path))
if (file.exists(html_path)) {
  html <- paste(readLines(html_path, warn = FALSE), collapse = "\n")
  ok("report has Methodology Setup section", grepl("Methodology Setup", html, fixed = TRUE))
  ok("report shows the AI relevance criterion", grepl("Relevance criterion", html, fixed = TRUE))
  ok("report has a per-subtheme summary table", grepl("subtheme-summary-table", html, fixed = TRUE))
  ok("report shows >= 1 AI primitive column or fail-honest cell",
     grepl("class=\"prim-unavailable\"|>median |>p[0-9]|>mean |>iqr |>skewness |metric-interpretation-notes", html))
  ok("report has the temporal panel (Posting-time patterns)",
     grepl("Posting-time patterns", html, fixed = TRUE))
}

ts <- res$theme_set
n_themes <- if (!is.null(ts)) length(ts$themes) else NA_integer_
ok("theme_set produced >= 1 theme", !is.na(n_themes) && n_themes >= 1L)
cat(sprintf("    themes produced: %s\n", n_themes))

# token tally from the audit log. Each audited AI call records usage_prompt +
# usage_completion (+ usage_total). gpt-4o pricing: $2.50/1M in, $10/1M out.
adlog <- file.path(run_dir, "ai_decisions.jsonl")
if (file.exists(adlog)) {
  ls <- readLines(adlog, warn = FALSE)
  pt <- ct <- 0
  for (l in ls) {
    o <- tryCatch(jsonlite::fromJSON(l), error = function(e) NULL)
    if (is.null(o)) next
    p <- suppressWarnings(as.numeric(o$usage_prompt))
    cc <- suppressWarnings(as.numeric(o$usage_completion))
    if (length(p) == 1L && !is.na(p)) pt <- pt + p
    if (length(cc) == 1L && !is.na(cc)) ct <- ct + cc
  }
  cat(sprintf("    tokens: prompt=%.0f completion=%.0f  est cost $%.3f (gpt-4o $2.50/1M in + $10/1M out)\n",
              pt, ct, pt / 1e6 * 2.5 + ct / 1e6 * 10))
}

cat("\n========================================\n")
if (length(fails) == 0L) {
  cat(sprintf("SMOKE PASS -- all assertions green (%s min, sample=%d)\n", elapsed, SAMPLE_SIZE))
  cat(sprintf("Inspect: %s\n", html_path))
  quit(status = 0L)
} else {
  cat(sprintf("SMOKE FAIL -- %d assertion(s) failed:\n", length(fails)))
  for (f in fails) cat("  - ", f, "\n", sep = "")
  cat(sprintf("Run dir for debugging: %s\n", run_dir))
  quit(status = 1L)
}
