# Phase 58 Tier 5 visual smoke test
#
# Renders a small synthetic .build_thematic_section + render_tier0_coverage_card
# + .generate_theme_detail_htmls fixture so we can visually verify the
# C-3 / C-10 / V-7 / AH-8/V-2 / AH-9/V-1 / H-23 changes look right.
#
# Run:
#   cd pakhom
#   Rscript scripts/dev/tier5_visual_smoke.R /tmp/tier5_smoke

devtools::load_all(".")

args <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1L) args[[1L]] else file.path(tempdir(), "tier5_smoke")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cat("Output dir:", out_dir, "\n\n")

# ----- 1. Synthetic theme_stats (5 framework + 3 emergent + 1 anomaly) -----
make_ts <- function(name, n, sentiment_mean, kind, with_subtheme = TRUE) {
  list(
    description = paste(name, "-- demo theme for Tier 5 visual smoke"),
    n_entries = n,
    pct_of_total = round(100 * n / 1000, 1),
    sentiment = list(mean = sentiment_mean, pct_negative = 30, pct_positive = 25),
    intensity = list(mean = 0.4),
    keywords = c("alpha", "beta", "gamma"),
    quotes_with_context = list(
      most_negative = list(text = "Felt awful after binge", sentiment = -0.8, emotion = "sadness"),
      median = list(text = "Tried smaller portions today", sentiment = 0.0, emotion = "neutral"),
      most_positive = list(text = "Made it through the day without binging", sentiment = 0.6, emotion = "joy")
    ),
    subthemes_structured = if (with_subtheme) {
      list(structure(
        list(name = paste(name, "/ sub-1"),
              description = "A demo subtheme",
              codes = list()),
        class = "Subtheme"
      ))
    } else list(),
    subtheme_stats = if (with_subtheme) {
      stats::setNames(
        list(list(
          name = paste(name, "/ sub-1"),
          description = "A demo subtheme",
          n = floor(n * 0.6),
          metric_stats = list(score = list(median = 8, mad = 1.2, mean = 7.6, sd = 0.9)),
          example_quotes = c(
            paste0("Quote 1 from ", name, " [score: 8]"),
            paste0("Quote 2 from ", name, " [score: 9]")
          )
        )),
        paste(name, "/ sub-1")
      )
    } else list(),
    metric_cols = if (with_subtheme) c("score") else character(0),
    theme_kind = kind,
    participant_spread = list(
      available = TRUE,
      n_distinct_contributors = floor(n / 4),
      contributor_gini = 0.4,
      top_contributor_share = 0.15
    )
  )
}

# We need MORE than 30 themes to trigger the compact branch -- use 35.
theme_names <- c(
  paste0("Framework ", 1:20),
  paste0("Emergent ", 1:10),
  "Anomaly Catch-all"
)
n_total <- length(theme_names)
theme_stats <- stats::setNames(
  lapply(seq_along(theme_names), function(i) {
    nm <- theme_names[[i]]
    kind <- if (i <= 20) "framework"
            else if (i <= 30) "emergent"
            else "anomaly_bracket"
    make_ts(nm,
            n = round(120 - i * 2),  # decreasing prevalence
            sentiment_mean = sin(i / 3) * 0.5,
            kind = kind,
            with_subtheme = (i %% 3 != 0))
  }),
  theme_names
)
cat(sprintf("Built %d synthetic themes\n", length(theme_stats)))

# ----- 2. Test the C-3 compact section at cap = 5 -----
config_compact <- list(
  analysis = list(themes = list(max_inline_themes = 5L))
)
section_html <- pakhom:::.build_thematic_section(
  theme_stats = theme_stats,
  theme_order = theme_names,
  n_themes    = n_total,
  export_files = list(theme_csv_files = list()),
  config = config_compact
)
writeLines(section_html, file.path(out_dir, "thematic_section_cap5.md"))

# Count inline vs compact cards
inline_cards <- length(gregexpr('class="theme-card theme-', section_html, fixed = TRUE)[[1L]])
compact_rows <- length(gregexpr('class="theme-card-compact"', section_html, fixed = TRUE)[[1L]])
cat(sprintf("  inline cards: %d (expected 5)\n", inline_cards))
cat(sprintf("  compact rows: %d (expected %d)\n", compact_rows, n_total - 5))
stopifnot(inline_cards == 5L)
stopifnot(compact_rows == n_total - 5)

# Verify "Additional themes" header appears
stopifnot(grepl("Additional themes", section_html, fixed = TRUE))
cat("  Additional themes header: present\n")

# Verify "Emergent themes" Phase 54 header appears
stopifnot(grepl("Emergent themes", section_html, fixed = TRUE))
cat("  Emergent themes header: present\n")

# Verify "Bracketed anomalies" Phase 54 header appears
stopifnot(grepl("Bracketed anomalies", section_html, fixed = TRUE))
cat("  Bracketed anomalies header: present\n")

# Verify additional themes header appears BEFORE the kind transition
# header at boundary (Tier 5 audit followup H1).
pos_additional <- regexpr("## Additional themes", section_html, fixed = TRUE)
pos_emergent_in_section <- regexpr("## Emergent themes", section_html, fixed = TRUE)
stopifnot(as.integer(pos_additional) < as.integer(pos_emergent_in_section))
cat("  Header ordering at boundary: additional-themes BEFORE emergent (H1 followup correct)\n")

# ----- 3. V-7 skip-reason clustering -----
skip_reasons <- stats::setNames(
  as.integer(c(15, 12, 10, 8, 6, 5, 4, 3, 2, 1)),
  c(
    "Entry is off-topic and unrelated to the focus",
    "Comment does not relate to the research question",
    "Off-topic anecdote about a different topic",
    "Too short -- only 4 words",
    "GIF reply only, no text content",
    "Just asking a question without any content",
    "Subreddit tag only: /r/binge_eating",
    "Duplicate of an earlier post",
    "Reply to another user with no original content",
    "Some unparseable wording the regex won't catch"
  )
)
clustered <- pakhom:::.cluster_skip_reasons(skip_reasons)
cat(sprintf("\nSkip-reason clustering -- %d categories from %d distinct reasons\n",
            length(clustered), length(skip_reasons)))
for (cat_name in names(clustered)) {
  ce <- clustered[[cat_name]]
  cat(sprintf("  %-50s %4d entries (%d distinct)\n", cat_name, ce$count, ce$n_distinct))
}

# ----- 4. H-23: detail HTML embeds Phase 55 table -----
detail_dir <- file.path(out_dir, "theme_details")
dir.create(detail_dir, recursive = TRUE, showWarnings = FALSE)
# Single-theme subset for fast smoke
one_theme <- list("Framework 1" = theme_stats[["Framework 1"]])
generated <- pakhom:::.generate_theme_detail_htmls(
  theme_stats = one_theme,
  theme_order = c("Framework 1"),
  export_files = list(theme_csv_files = list()),
  output_dir = out_dir,
  data = NULL,
  coding_results = NULL
)
detail_path <- generated[["Framework 1"]]$file_path
detail_html <- paste(readLines(detail_path), collapse = "\n")
stopifnot(grepl("detail-subtheme-summary", detail_html, fixed = TRUE))
stopifnot(grepl("subtheme-summary-table", detail_html, fixed = TRUE))
stopifnot(grepl("Median(MAD) score", detail_html, fixed = TRUE))
cat(sprintf("\nH-23 detail HTML for 'Framework 1': %s\n", detail_path))
cat("  detail-subtheme-summary div: present\n")
cat("  Phase 55 paper-style table: present\n")
cat("  metric column header: present\n")

# ----- 5. C-10 lollipop -----
set.seed(7L)
n <- 50L
cm <- matrix(runif(n * n, -1, 1), nrow = n)
cm <- (cm + t(cm)) / 2; diag(cm) <- 1
rownames(cm) <- colnames(cm) <- paste0("var", seq_len(n))
pa <- matrix(runif(n * n, 0, 1), nrow = n)
rownames(pa) <- colnames(pa) <- rownames(cm)
results <- list(correlation_matrix = cm, p_adjusted = pa)
lolli_path <- file.path(out_dir, "correlation_lollipop.png")
create_correlation_plot(results, lolli_path, max_inline_vars = 30L)
stopifnot(file.exists(lolli_path))
cat(sprintf("\nC-10 lollipop PNG: %s (%.1f KB)\n", lolli_path, file.info(lolli_path)$size / 1024))

# ----- 6. AH-9/V-1 theme_network -----
set.seed(11L)
n_themes <- 40L
n_entries <- 200L
mat <- matrix(0L, nrow = n_entries, ncol = n_themes)
for (j in seq_len(n_themes)) {
  n_in <- max(0L, 100L - 2L * j)
  if (n_in > 0L) mat[seq_len(min(n_in, n_entries)), j] <- 1L
}
data <- data.frame(mat); names(data) <- paste0("theme_membership_T", seq_len(n_themes))
theme_set <- structure(
  list(themes = lapply(seq_len(n_themes), function(j)
    list(name = paste0("T", j), codes_included = character()))),
  class = "ThemeSet"
)
net_path <- file.path(out_dir, "theme_network.png")
create_theme_network(data, theme_set, output_path = net_path,
                      min_cooccurrence = 1L, max_inline_themes = 10L)
stopifnot(file.exists(net_path))
cat(sprintf("AH-9/V-1 theme_network PNG: %s (%.1f KB; filtered 40 -> 10)\n",
            net_path, file.info(net_path)$size / 1024))

# Re-render to test replay-equivalence (Tier 5 cross-tier audit J2)
net_path2 <- file.path(out_dir, "theme_network_replay.png")
create_theme_network(data, theme_set, output_path = net_path2,
                      min_cooccurrence = 1L, max_inline_themes = 10L)
md5_1 <- tools::md5sum(net_path)
md5_2 <- tools::md5sum(net_path2)
cat(sprintf("Replay-equivalence: PNG md5 match = %s (J2 fix verified)\n",
            identical(unname(md5_1), unname(md5_2))))
stopifnot(identical(unname(md5_1), unname(md5_2)))

# ----- 7. Tier 6 statistical hygiene smoke (added by Tier 6 cross-tier audit) -----
cat("\n=== Tier 6 statistical hygiene smoke ===\n")
# (a) H-13: VADER-shaped sentiment classifies as ordinal
sentiment_cd <- tibble::tibble(
  binary_theme   = rep(c(0L, 1L), 30L),
  sentiment_score = round(runif(60L, -1, 1), 1)  # 21-level grid
)
types <- detect_variable_types(sentiment_cd)
stopifnot(types[["binary_theme"]] == "binary")
stopifnot(types[["sentiment_score"]] == "ordinal")
cat("  H-13: sentiment_score classifies as ordinal (was continuous pre-Tier-6)\n")

# (b) .select_pair_method: binary x ordinal -> spearman
m <- pakhom:::.select_pair_method(
  sentiment_cd$binary_theme, sentiment_cd$sentiment_score,
  "binary", "ordinal"
)
stopifnot(m == "spearman")
cat("  H-13: binary x ordinal routes to Spearman (was Pearson pre-Tier-6)\n")

# (c) H-15: interpret_correlations headline reports the intersection
test_df <- tibble::tibble(
  var1 = c("a", "b", "c", "d"),
  var2 = c("x", "y", "z", "w"),
  correlation = c(0.5, 0.3, 0.05, 0.02),
  p_value = c(1e-10, 0.5, 1e-10, 0.7),
  p_raw = c(1e-10, 0.5, 1e-10, 0.7),
  p_bh = c(1e-10, 0.5, 1e-10, 0.7),
  p_bonferroni = c(1e-10, 0.5, 1e-10, 0.7),
  effect_size = c("large", "medium", "negligible", "negligible"),
  significant = c(TRUE, FALSE, TRUE, FALSE),
  meaningful_effect = c(TRUE, TRUE, FALSE, FALSE),
  method = rep("spearman", 4L),
  ci_lower = c(0.4, 0.2, 0.04, 0.01),
  ci_upper = c(0.6, 0.4, 0.06, 0.03)
)
res <- interpret_correlations(test_df, theme_stats = list())
stopifnot(grepl("meaningful effect AND Bonferroni-significant", res$summary, fixed = TRUE))
cat("  H-15: headline filters on meaningful AND significant intersection\n")

# ----- 8. Tier 7 T0.1 verification fidelity smoke -----
cat("\n=== Tier 7 T0.1 verification fidelity smoke ===\n")

# (a) V-6/L-3: schema prompt embeds entry text verbatim in <entry_text>
#     fences (was JSON-escaped pre-Tier-7)
raw_text <- 'Quote contains "embedded" and \\ slash.'
prompt <- pakhom:::.build_progressive_schema_user_prompt(raw_text)
stopifnot(grepl("<entry_text>", prompt, fixed = TRUE))
stopifnot(grepl(raw_text, prompt, fixed = TRUE))  # text appears verbatim
cat("  V-6/L-3: entry text embedded verbatim in <entry_text> fence\n")

# (b) H-T7-3 audit followup: adversarial </entry_text> in entry doesn't
#     break the fence
adv_text <- "Mentions </entry_text> literally."
adv_prompt <- pakhom:::.build_progressive_schema_user_prompt(adv_text)
n_close <- length(gregexpr("</entry_text>", adv_prompt, fixed = TRUE)[[1L]])
stopifnot(n_close == 1L)
cat("  H-T7-3: adversarial </entry_text> in entry produces single fence\n")

# (c) M-13/E-19: verify_quote populates verification_failure_reason on
#     fabricated quote
src <- "Source text says actual content."
q <- make_quote("d1", "test", src, 0L, 10L, "FAKE CONTENT")
v <- verify_quote(q, src, provider = NULL)
stopifnot(identical(v$verification_status, "fabricated"))
stopifnot(!is.na(v$verification_failure_reason))
cat(sprintf("  M-13/E-19: fabricated quote carries reason '%s'\n",
              v$verification_failure_reason))

# (d) L-2/M-24: .normalize_quote_text collapses NBSP + smart apostrophe
nbsp_text <- "I’m hungry"   # smart apostrophe + NBSP
ascii_text <- "I'm hungry"
stopifnot(identical(
  pakhom:::.normalize_quote_text(nbsp_text),
  pakhom:::.normalize_quote_text(ascii_text)
))
cat("  L-2/M-24: smart apostrophe + NBSP normalize identically to ASCII\n")

# (e) M-25/AF-34 + C-T7-1 followup: themes.json supports
#     supporting_quote_records roundtrip
tj <- list(list(
  id = 1L, name = "T", description = "",
  codes_included = I("c1"), subthemes = I(character(0)),
  subthemes_structured = list(), keywords = I("k"),
  narrative = "", supporting_quotes = I("Q"),
  supporting_quote_records = list(
    list(text = "Q", entry_id = "e1", source_table = "posts",
          std_author = "alice", sentiment_score = 0.5,
          position = "most_negative")
  )
))
tj_path <- file.path(out_dir, "themes_t7_roundtrip.json")
jsonlite::write_json(tj, tj_path, pretty = TRUE, auto_unbox = TRUE,
                      null = "null", force = TRUE)
back <- jsonlite::read_json(tj_path)
stopifnot("supporting_quote_records" %in% names(back[[1L]]))
stopifnot(back[[1L]]$supporting_quote_records[[1L]]$entry_id == "e1")
cat("  M-25/AF-34 + C-T7-1: supporting_quote_records roundtrips through themes.json\n")

# (f) Schema version bumped to 1.1.0 (Tier 7 cross-tier polish)
stopifnot(identical(pakhom:::.QUOTE_PROVENANCE_SCHEMA_VERSION, "1.1.0"))
cat("  Schema version: QuoteProvenance v1.1.0 (was 1.0.0 pre-Tier-7)\n")

# ----- 9. Tier 8 polish + carry-forwards smoke -----
cat("\n=== Tier 8 polish + carry-forwards smoke ===\n")

# (a) H-9 dedupe: fresh coding state has last_arbiter_n_coded = -1L
cs_fresh <- create_coding_state()
stopifnot(identical(cs_fresh$saturation$last_arbiter_n_coded, -1L))
cat("  H-9 + audit followup MEDIUM-3: last_arbiter_n_coded pre-init = -1L\n")

# (b) H-11 audit log schema_version
cs_audit_td <- file.path(out_dir, "audit_test")
dir.create(cs_audit_td, recursive = TRUE, showWarnings = FALSE)
audit <- init_audit_log(cs_audit_td, config = NULL)
log_ai_decision(audit, "coding", "code_assignment",
                  entry_id = "e1", code_name = "test_code")
close_audit_log(audit)
audit_lines <- readLines(file.path(cs_audit_td, "ai_decisions.jsonl"))
audit_rec <- jsonlite::fromJSON(audit_lines[1L])
stopifnot(audit_rec$schema_version == "1.0.0")
cat("  H-11: ai_decisions.jsonl record carries schema_version = 1.0.0\n")

# (c) H-10 coverage_card.json roundtrip
cov_td <- file.path(out_dir, "coverage_test")
dir.create(cov_td, recursive = TRUE, showWarnings = FALSE)
cov_obj <- structure(list(
  n_input_to_coding = 100L, n_processed = 100L, n_unprocessed = 0L,
  n_skipped = 0L, n_coded = 100L,
  skip_reasons = stats::setNames(integer(0), character(0)),
  words_processed = 1000L, coverage_rate = 1.0,
  no_silent_truncation = TRUE, stop_reason = "all_entries_processed",
  saturation_reached = FALSE, reached_at_entry = NA_integer_
), class = c("CorpusCoverage", "Tier0Coverage"))
cov_path <- write_corpus_coverage(cov_obj, cov_td, methodology_mode = NULL)
stopifnot(file.exists(cov_path))
cov_back <- jsonlite::read_json(cov_path, simplifyVector = TRUE)
stopifnot("schema_version" %in% names(cov_back))
cat("  H-10: write_corpus_coverage roundtrips with schema_version present\n")

# (d) H-26 keywords cap to top-8 by codebook frequency
keyword_codebook <- setNames(
  lapply(1:15, function(i) list(code_name = paste0("c_", i),
                                  description = "", type = "descriptive",
                                  frequency = 16L - i,
                                  entry_ids = paste0("e", i),
                                  coded_segments = list())),
  paste0("c_", 1:15)
)
kw_cs <- list(codebook = keyword_codebook, entry_results = list())
class(kw_cs) <- "ProgressiveCodingState"
kw_ts <- create_theme_set(list(list(id = 1L, name = "TK",
                                      description = "",
                                      codes_included = paste0("c_", 1:15))))
kw_data <- tibble::tibble(std_id = paste0("e", 1:15),
                            std_text = rep("x", 15L),
                            sentiment_score = rep(0, 15L))
kw_enr <- suppressWarnings(enrich_themes(kw_ts, kw_data, coding_state = kw_cs))
stopifnot(length(kw_enr$themes[[1L]]$keywords) == 8L)
cat("  H-26: enrich_themes caps keywords to top-8 by codebook frequency\n")

# (e) M-21 description fallback wired correctly (audit followup CRITICAL-1)
stub_decision <- list(
  decision = "coherent_theme",
  central_organizing_concept = "Real articulation here",
  proposed_description = ""  # forces fallback
)
desc <- pakhom:::.derive_theme_description(
  leaf_indices = 1L,
  codes = list(list(name = "x", frequency = 1L)),
  articulation = stub_decision$central_organizing_concept
)
stopifnot(desc == "Real articulation here")
cat("  M-21 + audit followup CRITICAL-1: fallback uses central_organizing_concept\n")

cat("\n--- All visual smoke checks PASSED ---\n")
