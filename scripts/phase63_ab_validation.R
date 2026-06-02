#!/usr/bin/env Rscript
# ==============================================================================
# Phase 63 -- CONTROLLED A/B re-theming harness for the v2 singleton-merge tune.
#
# The clean controlled experiment for #3. v2 is pure multi-pass AI partitioning
# (NO embeddings/HAC), so with a FIXED coding_state + fixed research_focus +
# concepts the ONLY variable between arms is the ANTI-BIAS-GUIDANCE steer text;
# all run-to-run variation is the AI's temperature-0 residual non-determinism.
#
# It loads each codebook cell's cached progressive_coding.rds, rebuilds the EXACT
# config the pipeline passes to theming (load_config + the line 115-119 propagation
# of reflexivity_block/researcher_positionality), and calls generate_themes_iterative
# K times -- recording single_code_rate + max_share + theme count + convergence
# per replicate. response_cache=NULL so every replicate is an INDEPENDENT API call
# (we MEASURE the temp-0 variance, we do not suppress it).
#
# The ARM (old vs new) is whatever theme_algorithm_v2.R currently contains:
#   * run BEFORE the edit  -> arm=old (steer absent)
#   * run AFTER  the edit  -> arm=new (steer present)
# The harness detects the steer from source and ABORTS if it disagrees with the
# arm= you asked for (so you can never mislabel an arm).
#
# Usage:  Rscript scripts/phase63_ab_validation.R arm=old k=5
#         Rscript scripts/phase63_ab_validation.R arm=new k=5
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
if (!exists("generate_themes_phase60", where = asNamespace("pakhom")))
  stop("Loaded pakhom lacks generate_themes_phase60 -- v2 code not loaded. Aborting before any API spend.")
source(file.path(PKG_DIR, "scripts", "phase63_metrics.R"))

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
CFG_ABS <- file.path(PROJECT_ROOT, "config.yaml")

kv <- list()
for (a in commandArgs(trailingOnly = TRUE)) {
  if (grepl("=", a, fixed = TRUE)) kv[[sub("=.*", "", a)]] <- sub("^[^=]*=", "", a)
}
ARM <- kv$arm %||% "old"
K   <- as.integer(kv$k %||% "5")
stopifnot(ARM %in% c("old", "new"), K >= 1L)

# --- arm / steer cross-check (never mislabel an arm) --------------------------
steer <- .p63_steer_present(PKG_DIR)
expected <- if (identical(ARM, "new")) TRUE else FALSE
if (!identical(isTRUE(steer), expected)) {
  stop(sprintf("ARM=%s expects steer_present=%s but source has steer_present=%s. ABORTING (wrong code loaded for this arm).",
               ARM, expected, steer))
}
git_head <- tryCatch(trimws(system(sprintf("git -C '%s' rev-parse --short HEAD", PKG_DIR), intern = TRUE))[1],
                     error = function(e) NA_character_)
cat(sprintf("[p63-ab] arm=%s  steer_present=%s  git=%s  k=%d\n", ARM, steer, git_head, K))

# --- load the codebook cells from the manifest --------------------------------
P63_DIR <- file.path(getwd(), "outputs", "phase63")
mf <- file.path(P63_DIR, "manifest.jsonl")
if (!file.exists(mf)) stop(sprintf("manifest not found: %s -- run phase63_full_run.R (purpose=codebook) first.", mf))
recs <- lapply(readLines(mf, warn = FALSE), function(l) tryCatch(jsonlite::fromJSON(l, simplifyVector = FALSE),
                                                                 error = function(e) NULL))
recs <- Filter(function(r) !is.null(r) && identical(r$purpose, "codebook") && isTRUE(r$reusable), recs)
# de-dup by label, keeping the most recent codebook run for each label
seen <- character(0); cells <- list()
for (r in rev(recs)) if (!(r$label %in% seen)) { seen <- c(seen, r$label); cells[[length(cells) + 1L]] <- r }
cells <- rev(cells)
if (length(cells) == 0L) stop("No codebook cells in manifest.")
cat(sprintf("[p63-ab] %d codebook cell(s): %s\n", length(cells),
            paste(vapply(cells, function(r) r$label, character(1)), collapse = ", ")))

# one audit log for the whole arm -> total re-theming cost
audit_dir <- file.path(P63_DIR, paste0("audit_", ARM))
unlink(audit_dir, recursive = TRUE); dir.create(audit_dir, recursive = TRUE, showWarnings = FALSE)
audit_log <- tryCatch(init_audit_log(audit_dir, config = NULL), error = function(e) NULL)

lc <- .empty_learning_context()
rows <- list()

for (cell in cells) {
  label   <- cell$label
  run_dir <- cell$run_dir
  rds     <- file.path(run_dir, "checkpoints", "progressive_coding.rds")
  if (!file.exists(rds)) { cat(sprintf("  [SKIP] %s: no progressive_coding.rds at %s\n", label, rds)); next }
  coding_state <- tryCatch(readRDS(rds), error = function(e) NULL)
  if (is.null(coding_state) || !inherits(coding_state, "ProgressiveCodingState")) {
    cat(sprintf("  [SKIP] %s: progressive_coding.rds is not a ProgressiveCodingState\n", label)); next
  }

  # Rebuild the EXACT theming config the pipeline used (load_config replays the
  # cell's overrides; then the line 115-119 propagation).
  overrides <- cell$overrides
  config <- load_config(CFG_ABS, overrides = overrides)
  concepts <- config$study$concepts
  rb <- .build_reflexivity_block(config$study)
  config$analysis$themes$reflexivity_block        <- rb
  config$analysis$themes$researcher_positionality <- config$study$researcher_positionality
  provider <- create_ai_provider(config$ai$provider, config)
  rf <- config$study$research_focus

  cat(sprintf("\n[p63-ab] CELL %s  (focus=%s)\n", label, substr(rf, 1, 60)))
  for (rep in seq_len(K)) {
    t0 <- Sys.time()
    ts <- tryCatch(
      generate_themes_iterative(
        coding_state, provider, config$analysis$themes,
        learning_context = lc, research_focus = rf, concepts = concepts,
        audit_log = audit_log, response_cache = NULL, live_tracker = NULL),
      error = function(e) { cat(sprintf("    rep %d ERROR: %s\n", rep, conditionMessage(e))); NULL })
    m <- .p63_theme_metrics(ts)
    secs <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
    cat(sprintf("    rep %d: themes=%2d  single=%.3f (%d)  max-share=%.3f  passes=%s  conv@%s  [%.0fs]\n",
                rep, m$n_themes, m$single_code_rate %||% NA, m$n_single, m$max_share %||% NA,
                as.character(m$n_passes), as.character(m$converged_at), secs))
    rows[[length(rows) + 1L]] <- list(
      arm = ARM, label = label, rep = rep, n_themes = m$n_themes, total_codes = m$total_codes,
      single_code_rate = m$single_code_rate, n_single = m$n_single,
      max_share = m$max_share, max_theme_n = m$max_theme_n,
      converged_at = m$converged_at, n_passes = m$n_passes,
      sizes = m$sizes, names = m$names)
  }
}

cost <- .p63_cost_from_audit(file.path(audit_dir, "ai_decisions.jsonl"))

# --- per-cell aggregate -------------------------------------------------------
cat("\n===== PHASE 63 A/B SUMMARY (arm=", ARM, ") =====\n", sep = "")
agg <- list()
for (label in unique(vapply(rows, function(r) r$label, character(1)))) {
  rr  <- Filter(function(r) identical(r$label, label), rows)
  scr <- vapply(rr, function(r) r$single_code_rate %||% NA_real_, numeric(1))
  ms  <- vapply(rr, function(r) r$max_share %||% NA_real_, numeric(1))
  nt  <- vapply(rr, function(r) as.numeric(r$n_themes), numeric(1))
  cat(sprintf("  %-12s  single-code rate: mean=%.3f range[%.3f,%.3f]   max-share: mean=%.3f range[%.3f,%.3f]   themes: mean=%.1f range[%g,%g]\n",
              label, mean(scr, na.rm = TRUE), min(scr, na.rm = TRUE), max(scr, na.rm = TRUE),
              mean(ms, na.rm = TRUE), min(ms, na.rm = TRUE), max(ms, na.rm = TRUE),
              mean(nt, na.rm = TRUE), min(nt, na.rm = TRUE), max(nt, na.rm = TRUE)))
  agg[[label]] <- list(single_code_rate_mean = mean(scr, na.rm = TRUE),
                       single_code_rate_range = c(min(scr, na.rm = TRUE), max(scr, na.rm = TRUE)),
                       max_share_mean = mean(ms, na.rm = TRUE),
                       max_share_range = c(min(ms, na.rm = TRUE), max(ms, na.rm = TRUE)),
                       n_themes_mean = mean(nt, na.rm = TRUE))
}
cat(sprintf("\n[p63-ab] arm=%s re-theming cost: $%.3f (prompt=%.0f completion=%.0f, %d reps x %d cells)\n",
            ARM, cost$usd, cost$prompt, cost$completion, K, length(cells)))

out <- file.path(P63_DIR, sprintf("abresults_%s.json", ARM))
jsonlite::write_json(list(arm = ARM, steer_present = steer, git_head = git_head, k = K,
                          cost_usd = cost$usd, rows = rows, agg = agg,
                          timestamp = format(Sys.time(), tz = "UTC")),
                     out, auto_unbox = TRUE, null = "null", pretty = TRUE)
cat(sprintf("[p63-ab] wrote %s\n[p63-ab] DONE.\n", out))
