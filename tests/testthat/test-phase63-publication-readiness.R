# Phase 63 audit-followup: publication-readiness fixes surfaced by the
# whole-package methodology + transparency audits. These pin the fixes so a
# future change cannot silently regress the methods text or the small-n caveats.

.pr_cfg <- function(algo = "v2", mode = "codebook_collaborative") {
  list(methodology = list(mode = mode),
       analysis = list(themes = list(algorithm = algo),
                       correlations = list(method = "spearman", adjust_method = "bonferroni",
                                           dynamic_method = FALSE)),
       ai = list(provider = "openai",
                 openai = list(models = list(primary = "gpt-4o", fast = "gpt-4o-mini",
                                             reasoning = "gpt-4o"))),
       study = list(research_focus = "x"))
}
.pr_appendix <- function(cfg) .build_methodology_appendix(
  stats = list(), export_files = list(codes_file = "codes.csv"), config = cfg)

test_that("methodology appendix describes the ACTUAL theme algorithm (v2 default, v1 when pinned)", {
  ap_v2 <- .pr_appendix(.pr_cfg("v2"))
  # The methods text a researcher pastes into a paper must describe v2 (the
  # production default), NOT the retired Phase-52 HAC-on-embeddings path.
  expect_match(ap_v2, "Multi-pass AI clustering", fixed = TRUE)
  expect_false(grepl("ward.D2", ap_v2, fixed = TRUE))
  expect_false(grepl("cosine embeddings", ap_v2, fixed = TRUE))
  expect_match(ap_v2, "embedding-free", fixed = TRUE)
  # default (algorithm unset) is v2
  cfg_default <- .pr_cfg("v2"); cfg_default$analysis$themes$algorithm <- NULL
  expect_match(.pr_appendix(cfg_default), "Multi-pass AI clustering", fixed = TRUE)
  expect_false(grepl("ward.D2", .pr_appendix(cfg_default), fixed = TRUE))
  # a v1-pinned run still gets an ACCURATE legacy description
  ap_v1 <- .pr_appendix(.pr_cfg("v1"))
  expect_match(ap_v1, "ward.D2", fixed = TRUE)
  expect_false(grepl("Multi-pass AI clustering", ap_v1, fixed = TRUE))
  # the Statistical-Notes decision-point list is algorithm-accurate too
  expect_match(ap_v2, "the multi-pass AI clustering", fixed = TRUE)
  expect_match(ap_v1, "the HAC + AI tree walk", fixed = TRUE)
  # published methods text carries no internal dev-process labels
  expect_false(grepl("Phase 60", ap_v2, fixed = TRUE))
  expect_false(grepl("Phase 52", ap_v1, fixed = TRUE))
})

test_that("per-subtheme table carries a small-n caveat on the legacy battery path too", {
  # Legacy path: a metric column with NO ai_metric_stats renders Median(MAD)/Mean(SD).
  # The 62.5d dagger only fires on the AI path, so the legacy spread must carry its
  # own small-n caveat (closes the audit's transparency LOW: a fragile SD on a
  # pre-Phase-61 resume could otherwise appear with no caveat).
  ts <- list(metric_cols = "score", subtheme_stats = list(
    Sub = list(name = "Sub", description = "d", n = 4,
               metric_stats = list(score = list(median = 5, mad = 2, mean = 5.5, sd = 3, n_observed = 4)),
               ai_metric_stats = list(),
               example_quotes = character(0))))
  html <- .build_subtheme_summary_table(ts)
  expect_match(html, "Median(MAD)", fixed = TRUE)              # legacy battery shown
  expect_match(html, "indicative, not precise", fixed = TRUE)  # ...with the small-n caveat
  expect_match(html, "the n is shown", fixed = TRUE)           # ...and nothing hidden
})

test_that("methodology appendix states the ACTUAL Mann-Whitney effect size (rank-biserial, not |Z|/sqrt(N)) [H1]", {
  ap <- .pr_appendix(.pr_cfg())
  # the production code computes rank-biserial 2U/(n1*n2)-1; the appendix must say so
  expect_match(ap, "rank-biserial", fixed = TRUE)
  # the retired (Phase-58-replaced) |Z|/sqrt(N) derivation must NOT reappear
  expect_false(grepl("|Z|", ap, fixed = TRUE))
})

test_that(".build_thematic_section discloses 0 themes honestly instead of broken chunks [robustness]", {
  # A 0-theme corpus (empty / 0 on-focus / Mode-3 no-codes-no-anomalies) must
  # DISCLOSE rather than emit the theme-distribution / sentiment-by-theme chunks,
  # which crash on the all-NA emerged_themes column and leave `## Error` boxes.
  sec <- .build_thematic_section(theme_stats = list(), theme_order = character(0),
                                 n_themes = 0L, export_files = list(), config = NULL)
  expect_match(sec, "No themes emerged", fixed = TRUE)
  expect_false(grepl("theme-distribution", sec, fixed = TRUE))  # the crashing chunk is gone
  expect_false(grepl("strsplit", sec, fixed = TRUE))
  expect_false(grepl("```{r", sec, fixed = TRUE))               # no executable chunks at all
  # also fires defensively when n_themes is reported but theme_order is empty
  sec2 <- .build_thematic_section(theme_stats = list(), theme_order = character(0),
                                  n_themes = 5L, export_files = list(), config = NULL)
  expect_match(sec2, "No themes emerged", fixed = TRUE)
})
