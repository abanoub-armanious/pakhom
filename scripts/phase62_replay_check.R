#!/usr/bin/env Rscript
# ==============================================================================
# Phase 62.5 -- REPLAY-EQUIVALENCE CHECK for metric_provenance (62.1). NO API.
#
# Confirms the R7 back-compat hinge end-to-end, at both the serialization and the
# orchestrator level (the existing unit test at test-methodology-assistant.R:261
# exercises the replay path with a provenance-FREE fixture; this closes the gap):
#   * a NEW pinned block that INCLUDES metric_provenance round-trips + replays
#     (orchestrator returns source="pinned", ZERO AI calls -- ai_complete is
#     overridden to throw);
#   * an OLD pinned block that OMITS the field loads with provenance="" and
#     replays deterministically (the `%||% ""` default in .coerce_column_record);
#   * BC-3: an empty provenance is NOT serialized (the archive stays byte-identical
#     to a pre-62 archive -- no stray key leaks);
#   * serialize -> load -> serialize round-trips the metric/temporal content
#     identically (deterministic replay archive).
#
# Loads WORKTREE code via pkgload::load_all (never library()) -- the 61.5 lesson.
# Usage:  Rscript scripts/phase62_replay_check.R    (exits non-zero on any FAIL)
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
  stop("Loaded pakhom lacks run_methodology_assistant -- Phase 61 code not loaded.")
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
cat(sprintf("[replay] loaded worktree package from: %s\n", PKG_DIR))

n_assert <- 0L
fails <- character(0)
ok <- function(label, cond) {
  n_assert <<- n_assert + 1L
  cat(sprintf("  [%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", label))
  if (!isTRUE(cond)) fails <<- c(fails, label)
}

PROV <- paste0("Reddit net upvotes (score): platform engagement metadata; reflects ",
               "reception/visibility, not the phenomenon under study.")
CRIT <- "A segment is on-focus if it links medication to sleep or binge eating."

mk_art <- function(prov) {
  rel <- pakhom:::new_relevance_criterion(
    research_focus_paraphrase = "para",
    relevance_criterion = CRIT,
    on_focus_examples = c("pills keep me up", "cravings dropped"),
    off_focus_examples = c("nice weather"),
    discrimination_principle = "link present vs absent",
    source = "ai")
  mi <- pakhom:::new_metric_interpretation(
    metrics = list(
      list(column_name = "score", column_description = "heavy-tailed upvote count",
           requested_primitives = list(
             list(primitive = "prim_median", rationale = "robust"),
             list(primitive = "prim_p90", rationale = "tail")),
           interpretation_note = "median + p90",
           metric_provenance = prov)),
    temporal_columns = list(
      list(column_name = "std_timestamp", column_description = "timestamps",
           requested_primitives = list(
             list(primitive = "prim_hour_of_day_distribution", rationale = "rhythm")),
           interpretation_note = "evening peak")),
    source = "ai")
  pakhom:::new_methodology_articulations(rel, mi, research_focus = "focus", source = "ai")
}

# ---- (1)(2) serialization: provenance present serialized; empty omitted (BC-3) ----
ser_with    <- pakhom:::methodology_articulations_to_list(mk_art(PROV))
ser_without <- pakhom:::methodology_articulations_to_list(mk_art(""))
ok("serialize WITH provenance -> field present + equals the AI prose",
   identical(ser_with$metrics[[1]]$metric_provenance, PROV))
ok("BC-3: serialize with EMPTY provenance -> field OMITTED (byte-identical archive)",
   is.null(ser_without$metrics[[1]]$metric_provenance))

# ---- (3)(4) load_pinned_methodology: present preserved; absent -> "" (R7) ----
loaded_with <- pakhom:::load_pinned_methodology(ser_with, research_focus = "focus")
ok("load pinned WITH provenance -> preserved on the loaded record",
   identical(loaded_with$metric_interpretation$metrics[[1]]$metric_provenance, PROV))
ok("loaded WITH provenance -> source == 'pinned'", identical(loaded_with$source, "pinned"))

old_block <- ser_with
old_block$metrics[[1]]$metric_provenance <- NULL    # as a pre-62 archive would lack it
loaded_old <- pakhom:::load_pinned_methodology(old_block, research_focus = "focus")
ok("R7: OLD pinned block WITHOUT provenance -> loads as \"\" (deterministic default)",
   identical(loaded_old$metric_interpretation$metrics[[1]]$metric_provenance, ""))

# ---- (5)(6) ORCHESTRATOR replay: source=pinned, ZERO AI calls (ai_complete throws) ----
ns <- asNamespace("pakhom")
orig_ai <- get("ai_complete", envir = ns)
set_ai <- function(fn) {
  if (bindingIsLocked("ai_complete", ns)) unlockBinding("ai_complete", ns)
  assign("ai_complete", fn, envir = ns)
}
set_ai(function(...) stop("ai_complete MUST NOT be called in replay mode"))
mini    <- data.frame(std_text = "x", stringsAsFactors = FALSE)  # unused in replay
cfg_with <- list(study = list(research_focus = "focus", inferred_methodology = ser_with))
cfg_old  <- list(study = list(research_focus = "focus", inferred_methodology = old_block))
res_with <- tryCatch(pakhom:::run_methodology_assistant(mini, cfg_with, provider = list()),
                     error = function(e) e)
res_old  <- tryCatch(pakhom:::run_methodology_assistant(mini, cfg_old, provider = list()),
                     error = function(e) e)
set_ai(orig_ai)   # restore

ok("orchestrator replay (WITH provenance): ZERO AI calls + returns source=pinned",
   inherits(res_with, "MethodologyArticulations") && identical(res_with$source, "pinned"))
ok("orchestrator replay (WITH provenance): provenance preserved through the orchestrator",
   inherits(res_with, "MethodologyArticulations") &&
     identical(res_with$metric_interpretation$metrics[[1]]$metric_provenance, PROV))
ok("orchestrator replay (OLD block, no provenance): ZERO AI calls + source=pinned",
   inherits(res_old, "MethodologyArticulations") && identical(res_old$source, "pinned"))
ok("orchestrator replay (OLD block): provenance loads as \"\"",
   inherits(res_old, "MethodologyArticulations") &&
     identical(res_old$metric_interpretation$metrics[[1]]$metric_provenance, ""))

# ---- (7) relevance criterion + column counts survive replay ----
ok("replay preserves the relevance criterion verbatim + metric/temporal counts",
   inherits(res_with, "MethodologyArticulations") &&
     identical(res_with$relevance$relevance_criterion, CRIT) &&
     length(res_with$metric_interpretation$metrics) == 1L &&
     length(res_with$metric_interpretation$temporal_columns) == 1L)

# ---- (8)(9) idempotent serialize->load->serialize (deterministic replay archive) ----
# Compare CONTENT (metrics + temporal), not the `source` label (load always sets
# it to "pinned" -- that flip is correct, not a round-trip failure).
content_json <- function(lst) jsonlite::toJSON(
  list(metrics = lst$metrics, temporal_columns = lst$temporal_columns,
       relevance_criterion = lst$relevance_criterion), auto_unbox = TRUE)
rt_with <- pakhom:::methodology_articulations_to_list(
  pakhom:::load_pinned_methodology(ser_with, research_focus = "focus"))
rt_old  <- pakhom:::methodology_articulations_to_list(
  pakhom:::load_pinned_methodology(old_block, research_focus = "focus"))
ok("serialize->load->serialize byte-identical content (WITH provenance)",
   identical(content_json(ser_with), content_json(rt_with)))
ok("serialize->load->serialize byte-identical content (OLD block; no key leaks)",
   identical(content_json(old_block), content_json(rt_old)))

cat("\n========================================\n")
if (length(fails) == 0L) {
  cat(sprintf("REPLAY CHECK PASS -- %d/%d assertions green (metric_provenance R7 back-compat holds end-to-end)\n",
              n_assert, n_assert))
  quit(status = 0L)
} else {
  cat(sprintf("REPLAY CHECK FAIL -- %d/%d assertion(s) failed:\n", length(fails), n_assert))
  for (f in fails) cat("  - ", f, "\n", sep = "")
  quit(status = 1L)
}
