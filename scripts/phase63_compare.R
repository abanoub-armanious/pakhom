#!/usr/bin/env Rscript
# ==============================================================================
# Phase 63 -- OLD vs NEW arm comparison + decision rule for the v2 singleton tune.
#
# Reads outputs/phase63/abresults_old.json + abresults_new.json (written by
# phase63_ab_validation.R) and renders the before/after decision table.
#
# DECISION RULE (ship the steer iff ALL hold, aggregated across cells):
#   (1) EFFICACY:  new single-code-rate mean <= old single-code-rate mean
#                  (strictly falls where old > 0; non-inferior where old == 0).
#   (2) SAFETY:    new max-share mean does NOT rise above the OLD arm's observed
#                  max-share range (no kitchen-sink / over-merge regression).
#   (3) QUALITATIVE fit audit (done separately by reading cluster rationales).
#
# No API. Pure read + arithmetic.
# Usage:  Rscript scripts/phase63_compare.R
# ==============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
P63 <- file.path(getwd(), "outputs", "phase63")
of <- file.path(P63, "abresults_old.json"); nf <- file.path(P63, "abresults_new.json")
if (!file.exists(of) || !file.exists(nf))
  stop(sprintf("Need both arms. old:%s new:%s", file.exists(of), file.exists(nf)))
old <- jsonlite::fromJSON(of, simplifyVector = FALSE)
new <- jsonlite::fromJSON(nf, simplifyVector = FALSE)

cat(sprintf("OLD: steer=%s git=%s k=%s cost=$%.3f\n", old$steer_present, old$git_head, old$k, old$cost_usd %||% NA))
cat(sprintf("NEW: steer=%s git=%s k=%s cost=$%.3f\n", new$steer_present, new$git_head, new$k, new$cost_usd %||% NA))
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a
stopifnot(isFALSE(old$steer_present), isTRUE(new$steer_present))

labels <- intersect(names(old$agg), names(new$agg))
cat("\n", strrep("=", 110), "\n", sep = "")
cat(sprintf("%-12s | %-26s | %-26s | %-9s | %s\n",
            "cell", "single-code rate (mean[lo,hi])", "max-share (mean[lo,hi])", "themes", "verdict"))
cat(strrep("-", 110), "\n", sep = "")

all_pass <- TRUE; verdicts <- list()
for (L in labels) {
  o <- old$agg[[L]]; n <- new$agg[[L]]
  o_scr <- o$single_code_rate_mean; n_scr <- n$single_code_rate_mean
  o_ms  <- o$max_share_mean;        n_ms  <- n$max_share_mean
  o_msr <- unlist(o$max_share_range); # [lo,hi] of old max-share
  # (1) efficacy: single-code rate must not rise (and ideally falls when old>0)
  eff_ok <- is.finite(n_scr) && is.finite(o_scr) && (n_scr <= o_scr + 1e-9)
  eff_falls <- is.finite(o_scr) && o_scr > 1e-9 && is.finite(n_scr) && n_scr < o_scr - 1e-9
  # (2) safety: new mean max-share must not exceed old observed max (+small tol)
  saf_ok <- is.finite(n_ms) && length(o_msr) == 2 && n_ms <= max(o_msr) + 0.02
  v <- if (eff_ok && saf_ok) (if (eff_falls) "PASS (falls)" else "PASS (non-inf)") else
       if (!saf_ok) "FAIL (over-merge)" else "FAIL (scr up)"
  if (!(eff_ok && saf_ok)) all_pass <- FALSE
  verdicts[[L]] <- v
  cat(sprintf("%-12s | %.3f -> %.3f             | %.3f -> %.3f             | %.1f->%.1f | %s\n",
              L, o_scr, n_scr, o_ms, n_ms, o$n_themes_mean, n$n_themes_mean, v))
}
cat(strrep("=", 110), "\n", sep = "")
cat(sprintf("\nAGGREGATE VERDICT (counts only; the qualitative fit audit is separate): %s\n",
            if (all_pass) "PASS -- single-code rate non-inferior/falls AND no over-merge in every cell" else
                          "REVIEW -- at least one cell failed; inspect above"))
cat("Reminder: ship requires this PASS *and* the qualitative cluster-rationale audit (genuine specific-instance merges, no concept destruction).\n")
