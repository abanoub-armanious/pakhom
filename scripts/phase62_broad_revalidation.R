#!/usr/bin/env Rscript
# ==============================================================================
# Phase 62.5 BROAD-FOCUS RE-VALIDATION -- the load-bearing empirical test.
#
# A broad research focus is the ONLY config that yields a large codebook ->
# multi-pass clustering -> REAL (named) subthemes -> the per-subtheme AI-primitive
# table renders live. This run replicates the pre-62 baseline broad run
# (run_2026-05-30_213824_M2: 63 codes / 12 themes / 21 real subthemes) so the
# Phase 62 deltas can be checked against it:
#   * 62.1 + 62.5a: metric provenance grouping (substantive vs source/engagement
#     metadata) renders, and the AI's per-column judgment is sensible on the real
#     Reddit engagement columns (score/num_comments/upvote_ratio -> metadata).
#   * 62.2: the D-7 placeholder marker is gone; clustering NOT degraded vs baseline.
#   * 62.3: display quotes end at word boundaries.
#
# Same sample (seed 42, 250 entries), v2 algorithm, review points OFF, so the only
# moving parts vs the baseline are the Phase 62 code changes.
#
# Usage:  Rscript scripts/phase62_broad_revalidation.R [sample_size]   (default 250)
# Cost:   ~$2-3 on gpt-4o.  Loads WORKTREE code via pkgload::load_all (61.5 lesson).
# ==============================================================================

.find_pkg_dir <- function() {
  d <- getwd()
  for (i in 1:8) {
    desc <- file.path(d, "DESCRIPTION")
    if (file.exists(desc) &&
        any(grepl("^Package:\\s*pakhom", readLines(desc, warn = FALSE)))) return(d)
    d <- dirname(d)
  }
  stop("Could not locate the pakhom package source dir.")
}
PKG_DIR <- .find_pkg_dir()
suppressMessages(suppressWarnings(pkgload::load_all(PKG_DIR, quiet = TRUE)))
if (!exists("run_methodology_assistant", where = asNamespace("pakhom")))
  stop("Loaded pakhom lacks run_methodology_assistant -- Phase 61 code not loaded. Aborting before any API spend.")
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
cat(sprintf("[broad] loaded worktree package from: %s\n", PKG_DIR))

find_root <- function() {
  d <- getwd()
  for (i in 1:8) {
    if (file.exists(file.path(d, "RedditBingeEating_SleepData.db"))) return(d)
    d <- dirname(d)
  }
  stop("Could not locate project root (DB).")
}
PROJECT_ROOT <- find_root()
readRenviron(file.path(PROJECT_ROOT, ".Renviron"))
if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) stop("OPENAI_API_KEY not set.")
rs_pandoc <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"
if (dir.exists(rs_pandoc)) Sys.setenv(RSTUDIO_PANDOC = rs_pandoc)

args <- commandArgs(trailingOnly = TRUE)
SAMPLE_SIZE <- if (length(args) >= 1L) as.integer(args[[1]]) else 250L
DB_ABS  <- file.path(PROJECT_ROOT, "RedditBingeEating_SleepData.db")
CFG_ABS <- file.path(PROJECT_ROOT, "config.yaml")

BROAD_FOCUS <- paste0(
  "the lived experience of binge eating: emotional triggers, behaviors, ",
  "physical effects, and recovery")

cat(sprintf("[broad] sample_size=%d  focus=%s\n", SAMPLE_SIZE, BROAD_FOCUS))

overrides <- list(
  "data.database"                       = DB_ABS,
  "study.research_focus"                = BROAD_FOCUS,
  "analysis.test_mode.enabled"          = TRUE,
  "analysis.test_mode.sample_size"      = SAMPLE_SIZE,
  "analysis.test_mode.seed"             = 42L,
  "analysis.review_points.after_coding" = FALSE,
  "analysis.review_points.after_themes" = FALSE,
  "analysis.themes.algorithm"           = "v2",
  "learning.enabled"                    = FALSE,
  "output.results_dir"                  = file.path(getwd(), "outputs", "results")
)

t0 <- Sys.time()
res <- run_analysis(CFG_ABS, config_overrides = overrides)
cat(sprintf("[broad] run_analysis finished in %.1f min\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

run_dir <- res$output_dir
cat(sprintf("[broad] RUN DIR: %s\n", run_dir))

# --- relevance criterion ------------------------------------------------------
art_json <- file.path(run_dir, "rules", "methodology_articulations.json")
if (file.exists(art_json)) {
  art <- jsonlite::fromJSON(art_json, simplifyVector = FALSE)
  cat("\n===== RELEVANCE CRITERION (AI) =====\n")
  cat(substr(art$relevance_criterion %||% "(none)", 1, 400), "\n")
  cat("\n===== METRIC PROVENANCE per column (62.1 + 62.5a) =====\n")
  for (m in art$metrics %||% list())
    cat(sprintf("  - %-14s %s\n", paste0(m$column_name %||% "?", ":"),
                substr(m$metric_provenance %||% "(none)", 1, 160)))
  for (m in art$temporal_columns %||% list())
    cat(sprintf("  - %-14s %s\n", paste0(m$column_name %||% "?", ":"),
                substr(m$metric_provenance %||% "(none)", 1, 160)))
}

# --- codes + themes + real (named) subthemes ----------------------------------
codes_csv <- file.path(run_dir, "codes.csv")
n_codes <- if (file.exists(codes_csv)) {
  cc <- tryCatch(utils::read.csv(codes_csv, comment.char = "#", stringsAsFactors = FALSE),
                 error = function(e) NULL)
  if (!is.null(cc)) nrow(cc) else NA_integer_
} else NA_integer_

real_subs <- function(th) {
  subs <- th$subthemes %||% th$subthemes_structured %||% list()
  sum(vapply(subs, function(s) {
    nm <- tryCatch(s$name %||% NA_character_, error = function(e) NA_character_)
    !is.na(nm) && nzchar(nm) && !identical(toupper(nm), "NA")
  }, logical(1)))
}

cat("\n===== THEMES (assess coherence + on-focus; compare baseline 12 themes / 21 real subthemes) =====\n")
ts <- res$theme_set
total_subs <- 0L
if (!is.null(ts)) {
  for (i in seq_along(ts$themes)) {
    th <- ts$themes[[i]]
    ns <- real_subs(th); total_subs <- total_subs + ns
    cat(sprintf("  %2d. %-45s [%d real subtheme(s)]\n",
                i, substr(th$name %||% "(unnamed)", 1, 45), ns))
  }
}
cat(sprintf("\n[broad] TOTAL: codes=%s  themes=%d  real subthemes=%d  (baseline: 63 / 12 / 21)\n",
            as.character(n_codes), if (!is.null(ts)) length(ts$themes) else 0L, total_subs))

# --- cost ---------------------------------------------------------------------
adlog <- file.path(run_dir, "ai_decisions.jsonl")
if (file.exists(adlog)) {
  ls <- readLines(adlog, warn = FALSE); pt <- ct <- 0
  for (l in ls) {
    o <- tryCatch(jsonlite::fromJSON(l), error = function(e) NULL); if (is.null(o)) next
    p <- suppressWarnings(as.numeric(o$usage_prompt)); cc <- suppressWarnings(as.numeric(o$usage_completion))
    if (length(p) == 1L && !is.na(p)) pt <- pt + p
    if (length(cc) == 1L && !is.na(cc)) ct <- ct + cc
  }
  cat(sprintf("[broad] tokens: prompt=%.0f completion=%.0f  est cost $%.3f (gpt-4o $2.50/1M in + $10/1M out)\n",
              pt, ct, pt / 1e6 * 2.5 + ct / 1e6 * 10))
}
cat("[broad] DONE.\n")
