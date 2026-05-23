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

cat("\n--- All visual smoke checks PASSED ---\n")
