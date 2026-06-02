#!/usr/bin/env Rscript
# ==============================================================================
# Phase 63 -- FULL-PIPELINE driver for the v2 singleton-merge tune (#3).
#
# Runs ONE real run_analysis() on the real corpus + real OpenAI API and records
# its theme-structure metrics + cost + steer-stamp into outputs/phase63/
# manifest.jsonl. Three uses:
#   1. CODEBOOK generation (Mode 2): codes a cell once -> progressive_coding.rds
#      that phase63_ab_validation.R then re-themes k times per arm. (purpose=codebook)
#   2. Mode-3 before/after: the v2 clustering for Mode 3 lives inside the
#      emergent/anomaly pass (not reachable via a top-level generate_themes_iterative
#      call), so the steer is A/B'd here by running the FULL pipeline old vs new.
#      (purpose=mode3)
#   3. Confirmatory: a new-code full run to confirm the steer behaves in situ +
#      the report still renders. (purpose=confirm)
#
# Usage:
#   Rscript scripts/phase63_full_run.R label=broad450 mode=codebook_collaborative \
#       size=450 purpose=codebook focus='the lived experience of binge eating: ...'
#   Rscript scripts/phase63_full_run.R label=m3tdf mode=framework_applied size=250 \
#       purpose=mode3 framework=tdf focus='...'
#
# Loads WORKTREE code via pkgload::load_all (never library() -- the 61.5 lesson).
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
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
# Fail LOUDLY before any API spend if the v2 surface isn't the loaded code.
if (!exists("generate_themes_phase60", where = asNamespace("pakhom")))
  stop("Loaded pakhom lacks generate_themes_phase60 -- v2 code not loaded. Aborting before any API spend.")
source(file.path(PKG_DIR, "scripts", "phase63_metrics.R"))
cat(sprintf("[p63-full] loaded worktree package from: %s\n", PKG_DIR))

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
readRenviron(file.path(PROJECT_ROOT, ".Renviron"))
if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) stop("OPENAI_API_KEY not set.")
rs_pandoc <- "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"
if (dir.exists(rs_pandoc)) Sys.setenv(RSTUDIO_PANDOC = rs_pandoc)

# --- args: key=value ----------------------------------------------------------
kv <- list()
for (a in commandArgs(trailingOnly = TRUE)) {
  if (grepl("=", a, fixed = TRUE)) {
    kv[[sub("=.*", "", a)]] <- sub("^[^=]*=", "", a)
  }
}
LABEL     <- kv$label   %||% "run"
MODE      <- kv$mode    %||% "codebook_collaborative"
SIZE      <- as.integer(kv$size %||% "250")
PURPOSE   <- kv$purpose %||% "codebook"
FOCUS     <- kv$focus   %||% ""
FRAMEWORK <- kv$framework %||% ""

DB_ABS  <- file.path(PROJECT_ROOT, "RedditBingeEating_SleepData.db")
CFG_ABS <- file.path(PROJECT_ROOT, "config.yaml")
P63_DIR <- file.path(getwd(), "outputs", "phase63")
dir.create(P63_DIR, recursive = TRUE, showWarnings = FALSE)

overrides <- list(
  "data.database"                       = DB_ABS,
  "methodology.mode"                    = MODE,
  "analysis.test_mode.enabled"          = TRUE,
  "analysis.test_mode.sample_size"      = SIZE,
  "analysis.test_mode.seed"             = 42L,
  "analysis.review_points.after_coding" = FALSE,
  "analysis.review_points.after_themes" = FALSE,
  "analysis.themes.algorithm"           = "v2",
  "learning.enabled"                    = FALSE,
  "output.results_dir"                  = file.path(P63_DIR, "runs")
)
if (nzchar(FOCUS)) overrides[["study.research_focus"]] <- FOCUS
if (identical(MODE, "framework_applied")) {
  fpath <- file.path(PKG_DIR, "inst", "extdata", "frameworks", paste0(FRAMEWORK, ".yaml"))
  if (!file.exists(fpath)) stop(sprintf("framework spec not found: %s", fpath))
  overrides[["methodology.framework_spec_path"]] <- fpath
  # anomaly_handling defaults to "extend" (Phase 54) -> emergent v2 clustering.
}

cat(sprintf("[p63-full] label=%s mode=%s size=%d purpose=%s\n", LABEL, MODE, SIZE, PURPOSE))
cat(sprintf("[p63-full] focus=%s\n", if (nzchar(FOCUS)) FOCUS else "(root config default)"))

t0 <- Sys.time()
res <- run_analysis(CFG_ABS, config_overrides = overrides)
elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
run_dir <- res$output_dir
cat(sprintf("[p63-full] run_analysis finished in %s min -> %s\n", elapsed, run_dir))

ts <- res$theme_set
overall  <- .p63_theme_metrics(ts)
emergent <- .p63_theme_metrics(ts, kinds = "emergent")   # Mode-3 path; empty otherwise

n_codes <- tryCatch({
  cc <- file.path(run_dir, "codes.csv")
  if (file.exists(cc)) nrow(utils::read.csv(cc, comment.char = "#", stringsAsFactors = FALSE)) else NA_integer_
}, error = function(e) NA_integer_)

cost  <- .p63_cost_from_audit(file.path(run_dir, "ai_decisions.jsonl"))
steer <- .p63_steer_present(PKG_DIR)
git_head <- tryCatch(trimws(system(sprintf("git -C '%s' rev-parse --short HEAD", PKG_DIR), intern = TRUE))[1],
                     error = function(e) NA_character_)

# --- console summary ----------------------------------------------------------
cat("\n===== PHASE 63 FULL-RUN SUMMARY =====\n")
cat(sprintf("  steer_present=%s  git=%s\n", steer, git_head))
cat(sprintf("  codes=%s  themes(overall)=%d  single-code rate=%.3f (%d single)  max-share=%.3f (n=%s)\n",
            as.character(n_codes), overall$n_themes, overall$single_code_rate %||% NA,
            overall$n_single, overall$max_share %||% NA, as.character(overall$max_theme_n)))
cat(sprintf("  converged_at_pass=%s  substantive_passes=%s\n",
            as.character(overall$converged_at), as.character(overall$n_passes)))
if (emergent$n_themes > 0L) {
  cat(sprintf("  [EMERGENT] themes=%d  single-code rate=%.3f (%d single)  max-share=%.3f\n",
              emergent$n_themes, emergent$single_code_rate %||% NA, emergent$n_single,
              emergent$max_share %||% NA))
}
cat(sprintf("  theme sizes (overall): %s\n", paste(overall$sizes, collapse = " ")))
cat(sprintf("  est cost: $%.3f (prompt=%.0f completion=%.0f)\n", cost$usd, cost$prompt, cost$completion))

# --- append manifest line -----------------------------------------------------
rec <- list(
  label = LABEL, purpose = PURPOSE, mode = MODE, size = SIZE, focus = FOCUS,
  framework = FRAMEWORK, reusable = identical(MODE, "codebook_collaborative"),
  run_dir = run_dir, n_codes = n_codes, steer_present = steer, git_head = git_head,
  overall = list(n_themes = overall$n_themes, total_codes = overall$total_codes,
                 single_code_rate = overall$single_code_rate, n_single = overall$n_single,
                 max_share = overall$max_share, max_theme_n = overall$max_theme_n,
                 converged_at = overall$converged_at, n_passes = overall$n_passes,
                 sizes = overall$sizes),
  emergent = list(n_themes = emergent$n_themes, single_code_rate = emergent$single_code_rate,
                  n_single = emergent$n_single, max_share = emergent$max_share,
                  sizes = emergent$sizes),
  cost_usd = cost$usd, elapsed_min = elapsed,
  overrides = overrides, timestamp = format(Sys.time(), tz = "UTC")
)
mf <- file.path(P63_DIR, "manifest.jsonl")
cat(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null"), "\n",
    file = mf, append = TRUE, sep = "")
cat(sprintf("[p63-full] appended manifest record (label=%s) -> %s\n", LABEL, mf))
cat("[p63-full] DONE.\n")
