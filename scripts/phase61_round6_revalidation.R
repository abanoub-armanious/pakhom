#!/usr/bin/env Rscript
# ==============================================================================
# Phase 61.5 EMPIRICAL RE-VALIDATION -- Round 6 (the load-bearing focus-drift test)
#
# Round 6 focus ("emotional triggers driving binge-eating episodes") was the
# WORST focus-drift case in Phase 60.9: only 1 of 7 themes was directly on-focus
# (the other 6 were corpus topology -- recovery, behavior management, etc.).
# Phase 61's fix is the AI-articulated relevance criterion injected into coding
# (Step 2.5 -> 09_coding prompt). This run re-creates Round 6 on the Phase 61
# architecture at 250 entries to measure whether on-focus improves (target >5/7).
#
# Usage:  Rscript scripts/phase61_round6_revalidation.R [sample_size]   (default 250)
# Cost:   ~$0.028/entry on gpt-4o -> ~$7 for 250. ~12 min wall-time.
# Output: prints the run dir + the AI relevance criterion + every theme name, so
#         the on-focus ratio can be assessed (as the Phase 60.9 deep dive did).
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
cat(sprintf("[r6] loaded worktree package from: %s\n", PKG_DIR))

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

ROUND6_FOCUS <- paste0(
  "Emotional triggers driving binge-eating episodes and the affective ",
  "experience surrounding loss-of-control eating")

cat(sprintf("[r6] sample_size=%d  focus=%s\n", SAMPLE_SIZE, ROUND6_FOCUS))

overrides <- list(
  "data.database"                       = DB_ABS,
  "study.research_focus"                = ROUND6_FOCUS,
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
cat(sprintf("[r6] run_analysis finished in %.1f min\n",
            as.numeric(difftime(Sys.time(), t0, units = "mins"))))

run_dir <- res$output_dir
cat(sprintf("[r6] RUN DIR: %s\n", run_dir))

# --- report the substance for on-focus assessment ----------------------------
art_json <- file.path(run_dir, "rules", "methodology_articulations.json")
if (file.exists(art_json)) {
  art <- jsonlite::fromJSON(art_json, simplifyVector = FALSE)
  cat("\n===== RELEVANCE CRITERION (AI-articulated) =====\n")
  cat(art$relevance_criterion %||% "(none)", "\n")
  cat("\n--- on-focus examples ---\n")
  for (e in art$on_focus_examples %||% list()) cat("  +", e, "\n")
  cat("--- off-focus examples ---\n")
  for (e in art$off_focus_examples %||% list()) cat("  -", e, "\n")
}

cat("\n===== THEMES PRODUCED (assess on-focus ratio) =====\n")
ts <- res$theme_set
if (!is.null(ts)) {
  for (i in seq_along(ts$themes)) {
    th <- ts$themes[[i]]
    nm <- th$name %||% "(unnamed)"
    desc <- th$description %||% ""
    cat(sprintf("  %d. %s\n     %s\n", i, nm, substr(desc, 1, 160)))
  }
  cat(sprintf("\n[r6] total themes: %d\n", length(ts$themes)))
}

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
  cat(sprintf("[r6] tokens: prompt=%.0f completion=%.0f  est cost $%.3f (gpt-4o $2.50/1M in + $10/1M out)\n",
              pt, ct, pt / 1e6 * 2.5 + ct / 1e6 * 10))
}
cat("[r6] DONE.\n")
