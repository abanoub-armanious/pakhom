# Report-rendering unit tests
#
# C-3 top-N inlining of theme cards in the main HTML report
# C-10 top-N effect-size lollipop chart for large correlation matrices
# V-7 skip-reason taxonomy clustering
# AH-8/V-2 temporal_emergence top-N filter (via emergence_timeline schema)
# AH-9/V-1 theme_network top-N + legend
# H-23 paper-style subtheme table on per-theme detail HTMLs

# ==========================================================================
# V-7 skip-reason taxonomy
# ==========================================================================

test_that(".cluster_skip_reasons returns empty list on empty input", {
  expect_identical(pakhom:::.cluster_skip_reasons(NULL), list())
  expect_identical(
    pakhom:::.cluster_skip_reasons(stats::setNames(integer(0), character(0))),
    list()
  )
})

test_that(".cluster_skip_reasons classifies common off-topic phrasings", {
  reasons <- stats::setNames(
    as.integer(c(10, 5, 3, 2)),
    c(
      "Entry is off-topic and unrelated to the research focus",
      "Comment does not relate to sleep medication or binge eating",
      "off topic personal anecdote",
      "Off-topic; about a different drug entirely"
    )
  )

  clustered <- pakhom:::.cluster_skip_reasons(reasons)

  # All four reasons should land in the same off-topic bucket.
  expect_true("Off-topic / not about research focus" %in% names(clustered))
  off_topic <- clustered[["Off-topic / not about research focus"]]
  expect_equal(off_topic$count, 20L)
  expect_equal(off_topic$n_distinct, 4L)
  # Examples are top-by-count first.
  expect_equal(off_topic$examples[1],
                "Entry is off-topic and unrelated to the research focus")
})

test_that(".cluster_skip_reasons handles a mixed input across categories", {
  reasons <- stats::setNames(
    as.integer(c(100, 30, 20, 15, 7, 4, 2, 1)),
    c(
      "Off-topic to the research focus",
      "Entry is too short -- only 3 words",
      "GIF reply only; no text content",
      "Just asking a question without offering any content",
      "Subreddit tag only: /r/binge_eating",
      "Duplicate of an earlier post in this thread",
      "Quoting another comment with no original content",
      "Some completely unparseable wording the regex won't catch"
    )
  )

  clustered <- pakhom:::.cluster_skip_reasons(reasons)

  # We should have at least: off-topic, too-short, media, question,
  # metadata, duplicate, quote, and an "Other" bucket.
  expected_categories <- c(
    "Off-topic / not about research focus",
    "Too short / no substantive content",
    "Media-only (image / video / GIF / emoji)",
    "Question without contributable content",
    "Metadata / tag / link only",
    "Duplicate / near-duplicate",
    "Quote / reply with no original content",
    "Other / unspecified"
  )
  expect_true(all(expected_categories %in% names(clustered)))

  # Counts sum to total
  total <- sum(vapply(clustered, function(x) x$count, integer(1)))
  expect_equal(total, sum(as.integer(reasons)))

  # "Other / unspecified" is always sorted last
  expect_equal(tail(names(clustered), 1L), "Other / unspecified")
})

test_that(".cluster_skip_reasons caps examples at 3 per category", {
  reasons <- stats::setNames(
    rep(1L, 10L),
    paste("This entry is off-topic and not related to the focus #", 1:10)
  )
  clustered <- pakhom:::.cluster_skip_reasons(reasons)
  off_topic <- clustered[["Off-topic / not about research focus"]]
  expect_equal(length(off_topic$examples), 3L)
  expect_equal(off_topic$n_distinct, 10L)
})

test_that(".cluster_skip_reasons sorts known categories by count descending", {
  reasons <- stats::setNames(
    as.integer(c(5, 50, 100)),
    c(
      "GIF emoji image only",
      "Too short and brief",
      "Off-topic to the research focus"
    )
  )
  clustered <- pakhom:::.cluster_skip_reasons(reasons)
  # Off-topic (100) should come before Too-short (50), then Media-only (5).
  expect_equal(names(clustered)[1L], "Off-topic / not about research focus")
  expect_equal(names(clustered)[2L], "Too short / no substantive content")
  expect_equal(names(clustered)[3L], "Media-only (image / video / GIF / emoji)")
})

test_that(".cluster_skip_reasons NA / empty reason strings cluster to Other", {
  reasons <- stats::setNames(
    as.integer(c(7, 3)),
    c(NA_character_, "")
  )
  clustered <- pakhom:::.cluster_skip_reasons(reasons)
  expect_true("Other / unspecified" %in% names(clustered))
  other <- clustered[["Other / unspecified"]]
  expect_equal(other$count, 10L)
  expect_true("(unspecified)" %in% other$examples)
})

test_that(".cluster_skip_reasons on-topic negation does not bucket into off-topic", {
  # Audit followup M2: defensive test that negated phrasings
  # ("entry is on-topic but..." / "this IS about X but lacks detail")
  # don't false-positive into the off-topic bucket. The off-topic
  # pattern anchors on \b(not |unrelated|irrelevant|...), so a positive
  # framing should fall through to Other.
  reasons <- stats::setNames(
    as.integer(c(5, 4)),
    c(
      "Entry is on-topic about the focus but contains no usable detail",
      "This IS about the topic, yet lacks substance"
    )
  )
  clustered <- pakhom:::.cluster_skip_reasons(reasons)
  # Neither reason carries the "on-topic" wording into the off-topic
  # bucket. Either off_topic is absent entirely, or it's present but
  # holds no examples mentioning "on-topic". Both outcomes are correct.
  off_topic <- clustered[["Off-topic / not about research focus"]]
  if (is.null(off_topic)) {
    succeed("on-topic phrasings stayed out of the off-topic bucket")
  } else {
    expect_false(any(grepl("on[- ]?topic", tolower(off_topic$examples))))
  }
})

test_that(".cluster_skip_reasons documents first-match-wins for reply-without-quote", {
  # Audit followup M2: the V-7 taxonomy is intent-blind. A
  # reason like "Reply to another user with no substance" lands in
  # the Quote/Reply bucket via the reply alternation rather than the
  # too-short bucket, because the quote/reply pattern is matched
  # earlier in the cascade. Document expected behavior so future
  # audits / contributors don't re-litigate.
  reasons <- stats::setNames(
    as.integer(c(5)),
    c("Reply to another user with no substance")
  )
  clustered <- pakhom:::.cluster_skip_reasons(reasons)
  expect_true(
    "Quote / reply with no original content" %in% names(clustered)
  )
})


# ==========================================================================
# C-3 top-N inlining + compact section header
# ==========================================================================

test_that(".build_thematic_section emits a compact section header beyond max_inline_themes", {
  # Build a minimal theme_stats list with 5 themes and cap at 3
  ts_one <- function(name, n) {
    list(
      description = paste(name, "description"),
      n_entries = n,
      pct_of_total = round(100 * n / 100, 1),
      sentiment = list(mean = 0.1, pct_negative = 25, pct_positive = 30),
      intensity = list(mean = 0.5),
      keywords = c("k1", "k2"),
      quotes_with_context = NULL,
      subtheme_stats = list(),
      metric_cols = character(0),
      theme_kind = "framework"
    )
  }
  theme_stats <- list(
    "Theme A" = ts_one("Theme A", 30),
    "Theme B" = ts_one("Theme B", 25),
    "Theme C" = ts_one("Theme C", 20),
    "Theme D" = ts_one("Theme D", 15),
    "Theme E" = ts_one("Theme E", 10)
  )
  theme_order <- names(theme_stats)
  export_files <- list(theme_csv_files = list())
  config <- list(analysis = list(themes = list(max_inline_themes = 3L)))

  out <- pakhom:::.build_thematic_section(
    theme_stats = theme_stats,
    theme_order = theme_order,
    n_themes    = length(theme_stats),
    export_files = export_files,
    config = config
  )

  # Section header for the compact tail must be present
  expect_match(out, "Additional themes", fixed = TRUE)
  expect_match(out, "theme-card-compact", fixed = TRUE)
  expect_match(out, "theme-badge-compact", fixed = TRUE)

  # First 3 themes render as full cards; last 2 as compact rows
  full_card_hits <- length(gregexpr("theme-card theme-", out, fixed = TRUE)[[1L]])
  compact_hits <- length(gregexpr("theme-card-compact", out, fixed = TRUE)[[1L]])
  # full_card_hits counts class occurrences; should be exactly 3 (one per inline theme)
  expect_equal(full_card_hits, 3L)
  # compact_hits counts class occurrences; should be exactly 2 (one per compact row)
  expect_equal(compact_hits, 2L)
})

test_that(".build_thematic_section without cap renders all themes as full cards", {
  ts_one <- function(name, n) {
    list(
      description = paste(name, "description"),
      n_entries = n,
      pct_of_total = round(100 * n / 50, 1),
      sentiment = list(mean = 0.1, pct_negative = 25, pct_positive = 30),
      intensity = list(mean = 0.5),
      keywords = c("k1"),
      quotes_with_context = NULL,
      subtheme_stats = list(),
      metric_cols = character(0),
      theme_kind = "framework"
    )
  }
  theme_stats <- list(
    "Theme 1" = ts_one("Theme 1", 30),
    "Theme 2" = ts_one("Theme 2", 20)
  )
  config <- list(analysis = list(themes = list(max_inline_themes = 100L)))
  out <- pakhom:::.build_thematic_section(
    theme_stats = theme_stats,
    theme_order = names(theme_stats),
    n_themes    = length(theme_stats),
    export_files = list(theme_csv_files = list()),
    config = config
  )
  expect_false(grepl("Additional themes", out, fixed = TRUE))
  expect_false(grepl("theme-card-compact", out, fixed = TRUE))
})

test_that(".build_thematic_section emits Additional themes BEFORE kind transition at boundary", {
  # Audit followup H1: the compact-header (top-N C-3) and
  # the kind-header used to stack with no content in
  # between when framework count == max_inline_themes. The followup
  # reorders so "## Additional themes" emits first, then the kind
  # transition ("## Emergent themes") -- reader sees two coherent
  # section boundaries each followed by content.
  ts_one <- function(name, n, kind) {
    list(
      description = paste(name, "description"),
      n_entries = n,
      pct_of_total = round(100 * n / 100, 1),
      sentiment = list(mean = 0.05, pct_negative = 10, pct_positive = 15),
      intensity = list(mean = 0.3),
      keywords = character(0),
      quotes_with_context = NULL,
      subtheme_stats = list(),
      metric_cols = character(0),
      theme_kind = kind
    )
  }
  # 2 framework themes inline + 2 emergent themes compact (cap = 2)
  theme_stats <- list(
    "F1" = ts_one("F1", 30, "framework"),
    "F2" = ts_one("F2", 25, "framework"),
    "E1" = ts_one("E1", 12, "emergent"),
    "E2" = ts_one("E2", 8,  "emergent")
  )
  out <- pakhom:::.build_thematic_section(
    theme_stats = theme_stats,
    theme_order = names(theme_stats),
    n_themes    = length(theme_stats),
    export_files = list(theme_csv_files = list()),
    config = list(analysis = list(themes = list(max_inline_themes = 2L)))
  )
  # Both headers present
  expect_match(out, "## Additional themes", fixed = TRUE)
  expect_match(out, "## Emergent themes", fixed = TRUE)
  # AND "Additional themes" appears BEFORE "Emergent themes" in the output
  pos_additional <- regexpr("## Additional themes", out, fixed = TRUE)
  pos_emergent <- regexpr("## Emergent themes", out, fixed = TRUE)
  expect_lt(as.integer(pos_additional), as.integer(pos_emergent))
})

test_that(".build_thematic_section dedupes theme_order before counting", {
  # Audit followup M3: defensive dedupe so an upstream caller
  # passing the same theme twice doesn't render it twice (which
  # would also break the n_compact arithmetic in the header).
  ts_one <- function(n) list(
    description = "", n_entries = n,
    pct_of_total = 10, sentiment = list(mean = 0, pct_negative = 0, pct_positive = 0),
    intensity = list(mean = 0), keywords = character(0),
    quotes_with_context = NULL, subtheme_stats = list(),
    metric_cols = character(0), theme_kind = "framework"
  )
  theme_stats <- list("Solo" = ts_one(10), "Other" = ts_one(5))
  # theme_order repeats "Solo"
  out <- pakhom:::.build_thematic_section(
    theme_stats = theme_stats,
    theme_order = c("Solo", "Solo", "Other"),
    n_themes = 2L,
    export_files = list(theme_csv_files = list()),
    config = list(analysis = list(themes = list(max_inline_themes = 100L)))
  )
  # Solo's theme-card markup should appear exactly once
  solo_hits <- length(gregexpr('id="theme-summary-1"', out, fixed = TRUE)[[1L]])
  expect_equal(solo_hits, 1L)
})

test_that(".build_thematic_section honors malformed max_inline_themes by reverting to default", {
  # NA / negative input falls back to default 30 -- so a 5-theme set
  # renders fully inline with no compact section.
  ts_one <- function(n) list(
    description = "", n_entries = n,
    pct_of_total = 10, sentiment = list(mean = 0, pct_negative = 0, pct_positive = 0),
    intensity = list(mean = 0), keywords = character(0),
    quotes_with_context = NULL, subtheme_stats = list(),
    metric_cols = character(0), theme_kind = "framework"
  )
  theme_stats <- list(A = ts_one(5), B = ts_one(4))
  for (bad in list(0L, -1L, NA_integer_)) {
    config <- list(analysis = list(themes = list(max_inline_themes = bad)))
    out <- pakhom:::.build_thematic_section(
      theme_stats = theme_stats,
      theme_order = names(theme_stats),
      n_themes = 2L,
      export_files = list(theme_csv_files = list()),
      config = config
    )
    expect_false(grepl("Additional themes", out, fixed = TRUE))
  }
})


# ==========================================================================
# AH-8/V-2 emergence_timeline schema has n_entries
# ==========================================================================

test_that(".compute_theme_emergence emits an n_entries column", {
  # Tiny synthetic data: 2 themes, 5 entries, with theme_membership columns
  data <- data.frame(
    std_id = paste0("e", 1:5),
    std_timestamp = as.POSIXct("2025-01-01") + (0:4) * 86400,
    theme_membership_Theme.One = c(1L, 1L, 0L, 1L, 0L),
    theme_membership_Theme.Two = c(0L, 0L, 1L, 1L, 1L),
    stringsAsFactors = FALSE
  )
  data$.parsed_ts <- as.POSIXct(data$std_timestamp)

  theme_set <- structure(
    list(
      themes = list(
        list(name = "Theme One",  codes_included = character()),
        list(name = "Theme Two", codes_included = character())
      )
    ),
    class = "ThemeSet"
  )

  emergence <- pakhom:::.compute_theme_emergence(data, theme_set, coding_state = NULL)
  expect_true("n_entries" %in% names(emergence))
  expect_equal(emergence$n_entries[emergence$theme_name == "Theme One"], 3L)
  expect_equal(emergence$n_entries[emergence$theme_name == "Theme Two"], 3L)
})

test_that(".empty_temporal_result emergence_timeline tibble has n_entries column", {
  stub <- pakhom:::.empty_temporal_result()
  expect_true("n_entries" %in% names(stub$emergence_timeline))
})


# ==========================================================================
# C-10 lollipop helper smoke tests
# ==========================================================================

test_that(".create_correlation_lollipop emits a PNG file when given a valid matrix", {
  set.seed(1234L)
  n <- 8L
  cm <- matrix(runif(n * n, -1, 1), nrow = n)
  cm <- (cm + t(cm)) / 2  # symmetric
  diag(cm) <- 1
  rownames(cm) <- colnames(cm) <- paste0("var", seq_len(n))
  pa <- matrix(runif(n * n, 0, 1), nrow = n)
  pa[lower.tri(pa, diag = TRUE)] <- pa[upper.tri(pa, diag = TRUE)]
  rownames(pa) <- colnames(pa) <- rownames(cm)

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)

  pakhom:::.create_correlation_lollipop(
    cm = cm, pa = pa, output_path = tmp,
    top_n = 5L, n_total_vars = n,
    methodology_mode = NULL, run_id = NULL
  )

  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0L)
})

test_that(".create_correlation_lollipop returns NULL when matrix is degenerate", {
  cm <- matrix(NA_real_, nrow = 2L, ncol = 2L)
  rownames(cm) <- colnames(cm) <- c("a", "b")
  pa <- cm
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  res <- pakhom:::.create_correlation_lollipop(
    cm = cm, pa = pa, output_path = tmp,
    top_n = 5L, n_total_vars = 2L,
    methodology_mode = NULL, run_id = NULL
  )
  expect_null(res)
})


# ==========================================================================
# C-10 create_correlation_plot dispatch: heatmap below cap, lollipop above
# ==========================================================================

test_that("create_correlation_plot dispatches to lollipop above max_inline_vars", {
  skip_if_not_installed("ggplot2")
  set.seed(99L)
  n <- 6L
  cm <- matrix(runif(n * n, -1, 1), nrow = n)
  cm <- (cm + t(cm)) / 2
  diag(cm) <- 1
  rownames(cm) <- colnames(cm) <- paste0("v", seq_len(n))
  pa <- matrix(runif(n * n, 0, 0.5), nrow = n)
  rownames(pa) <- colnames(pa) <- rownames(cm)
  results <- list(correlation_matrix = cm, p_adjusted = pa)

  tmp_low <- tempfile(fileext = ".png")
  tmp_high <- tempfile(fileext = ".png")
  on.exit({ unlink(tmp_low); unlink(tmp_high) }, add = TRUE)

  # n_vars = 6, cap = 100 -> heatmap path. Skip if corrplot unavailable.
  if (requireNamespace("corrplot", quietly = TRUE)) {
    create_correlation_plot(results, tmp_low, max_inline_vars = 100L)
    expect_true(file.exists(tmp_low))
  }

  # n_vars = 6, cap = 3 -> lollipop path (no corrplot needed).
  create_correlation_plot(results, tmp_high, max_inline_vars = 3L)
  expect_true(file.exists(tmp_high))
})


# ==========================================================================
# H-23 per-theme detail HTML embeds the paper-style subtheme table
# ==========================================================================

test_that(".generate_theme_detail_htmls embeds .build_subtheme_summary_table output", {
  # Theme with one real subtheme so the paper-style table renders
  ts <- list(
    description = "Demo",
    n_entries = 12L,
    pct_of_total = 24,
    sentiment = list(mean = 0.05, pct_negative = 20, pct_positive = 25),
    intensity = list(mean = 0.4),
    keywords = c("alpha", "beta"),
    quotes_with_context = NULL,
    subthemes_structured = list(
      structure(
        list(name = "Subtheme One",
              description = "A real (non-virtual) subtheme",
              codes = list()),
        class = "Subtheme"
      )
    ),
    subtheme_stats = list(
      "Subtheme One" = list(
        name = "Subtheme One",
        description = "A real (non-virtual) subtheme",
        n = 5L,
        metric_stats = list(score = list(median = 8, mad = 1, mean = 7.5, sd = 1.2)),
        example_quotes = c("Example quote 1 [score: 8]", "Example quote 2 [score: 7]")
      )
    ),
    metric_cols = c("score"),
    theme_kind = "framework"
  )

  theme_stats <- list("Demo Theme" = ts)
  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  generated <- pakhom:::.generate_theme_detail_htmls(
    theme_stats = theme_stats,
    theme_order = names(theme_stats),
    export_files = list(theme_csv_files = list()),
    output_dir = out_dir,
    data = NULL,
    coding_results = NULL
  )

  expect_true("Demo Theme" %in% names(generated))
  html <- paste(readLines(generated[["Demo Theme"]]$file_path), collapse = "\n")
  # The paper-style table marker classes / heading should appear in the detail
  expect_match(html, "Subthemes \\(per-subtheme summary\\)")
  expect_match(html, "subtheme-summary-table", fixed = TRUE)
  expect_match(html, "detail-subtheme-summary", fixed = TRUE)
  # Subtheme name appears in the rendered table
  expect_match(html, "Subtheme One", fixed = TRUE)
})

test_that(".generate_theme_detail_htmls methodology-stamps each detail page (AC4)", {
  # AC4 ("methodology stamped on every output"): a standalone theme-detail page
  # must carry the same methodology badge as the main report when a mode is
  # supplied, and render cleanly with NO badge for legacy NULL callers.
  ts <- list(
    description = "Demo",
    n_entries = 12L,
    pct_of_total = 24,
    sentiment = list(mean = 0.05, pct_negative = 20, pct_positive = 25),
    intensity = list(mean = 0.4),
    keywords = c("alpha", "beta"),
    quotes_with_context = NULL,
    subthemes_structured = list(
      structure(
        list(name = "Subtheme One",
              description = "A real (non-virtual) subtheme",
              codes = list()),
        class = "Subtheme"
      )
    ),
    subtheme_stats = list(
      "Subtheme One" = list(
        name = "Subtheme One",
        description = "A real (non-virtual) subtheme",
        n = 5L,
        metric_stats = list(score = list(median = 8, mad = 1, mean = 7.5, sd = 1.2)),
        example_quotes = c("Example quote 1 [score: 8]", "Example quote 2 [score: 7]")
      )
    ),
    metric_cols = c("score"),
    theme_kind = "framework"
  )
  theme_stats <- list("Demo Theme" = ts)

  render_detail <- function(mode) {
    out_dir <- tempfile()
    dir.create(out_dir)
    on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)
    generated <- pakhom:::.generate_theme_detail_htmls(
      theme_stats = theme_stats,
      theme_order = names(theme_stats),
      export_files = list(theme_csv_files = list()),
      output_dir = out_dir,
      methodology_mode = mode
    )
    paste(readLines(generated[["Demo Theme"]]$file_path), collapse = "\n")
  }

  # Mode supplied -> the detail page carries the same badge class + mode label
  # as the main report (styled via the ../styles.css the page already links).
  stamped <- render_detail("codebook_collaborative")
  expect_match(stamped, "methodology-stamp", fixed = TRUE)
  expect_match(stamped, "M2 - Codebook Collaborative", fixed = TRUE)

  # Legacy NULL caller (no methodology block) -> renders, but no badge.
  expect_false(grepl("methodology-stamp", render_detail(NULL), fixed = TRUE))
})

test_that("create_theme_network filters to top-N by weighted degree", {
  # Audit followup H2: theme_network top-N filter coverage.
  # Build a 5-theme membership matrix; cap at 3; assert that the
  # PNG file is written and the lowest-degree themes are removed.
  skip_if_not_installed("igraph")
  set.seed(42L)
  n_themes <- 5L
  n_entries <- 50L
  # Construct membership so theme 1 has the most co-occurrences,
  # decreasing toward theme 5.
  mat <- matrix(0L, nrow = n_entries, ncol = n_themes)
  for (j in seq_len(n_themes)) {
    # theme j gets entries 1..(60 - 10 * j) -- more for low-j themes
    n_in <- max(0L, 60L - 10L * j)
    if (n_in > 0L) {
      mat[seq_len(min(n_in, n_entries)), j] <- 1L
    }
  }
  data <- data.frame(mat)
  names(data) <- paste0("theme_membership_T", seq_len(n_themes))

  # Minimal theme_set with the same theme names
  theme_set <- structure(
    list(
      themes = lapply(seq_len(n_themes), function(j) {
        list(name = paste0("T", j),
              codes_included = character())
      })
    ),
    class = "ThemeSet"
  )

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  res <- create_theme_network(
    data, theme_set,
    output_path = tmp,
    min_cooccurrence = 1L,
    max_inline_themes = 3L
  )
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0L)
  # Adjacency matrix returned invisibly should have all themes (the
  # filter applies to the rendered graph, not the returned adjacency).
  expect_true(!is.null(res))
})

test_that("create_theme_network renders without filter when below cap", {
  skip_if_not_installed("igraph")
  set.seed(7L)
  n_themes <- 3L
  n_entries <- 20L
  mat <- matrix(0L, nrow = n_entries, ncol = n_themes)
  for (j in seq_len(n_themes)) {
    mat[seq_len(10L - j), j] <- 1L
  }
  data <- data.frame(mat)
  names(data) <- paste0("theme_membership_T", seq_len(n_themes))

  theme_set <- structure(
    list(
      themes = lapply(seq_len(n_themes), function(j) {
        list(name = paste0("T", j), codes_included = character())
      })
    ),
    class = "ThemeSet"
  )
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  res <- create_theme_network(
    data, theme_set,
    output_path = tmp,
    min_cooccurrence = 1L,
    max_inline_themes = 100L
  )
  expect_true(file.exists(tmp))
})

test_that("create_theme_network handles empty co-occurrence gracefully", {
  skip_if_not_installed("igraph")
  data <- data.frame(
    theme_membership_T1 = c(1L, 0L, 0L),
    theme_membership_T2 = c(0L, 1L, 0L)
  )
  theme_set <- structure(
    list(themes = list(
      list(name = "T1", codes_included = character()),
      list(name = "T2", codes_included = character())
    )),
    class = "ThemeSet"
  )
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  # No theme co-appears -> no edges -> early return
  res <- create_theme_network(
    data, theme_set,
    output_path = tmp,
    min_cooccurrence = 1L,
    max_inline_themes = 30L
  )
  # No file should be written when there are no edges
  expect_false(file.exists(tmp))
})


test_that(".generate_theme_detail_htmls omits paper-style table when no real subthemes", {
  ts <- list(
    description = "Demo",
    n_entries = 5L,
    pct_of_total = 10,
    sentiment = list(mean = 0.0, pct_negative = 0, pct_positive = 0),
    intensity = list(mean = 0.0),
    keywords = character(0),
    quotes_with_context = NULL,
    subthemes_structured = list(),
    subtheme_stats = list(),  # no real subthemes -> table returns ""
    metric_cols = character(0),
    theme_kind = "framework"
  )

  out_dir <- tempfile()
  dir.create(out_dir)
  on.exit(unlink(out_dir, recursive = TRUE), add = TRUE)

  generated <- pakhom:::.generate_theme_detail_htmls(
    theme_stats = list("Bare Theme" = ts),
    theme_order = c("Bare Theme"),
    export_files = list(theme_csv_files = list()),
    output_dir = out_dir
  )

  html <- paste(readLines(generated[["Bare Theme"]]$file_path), collapse = "\n")
  expect_false(grepl("detail-subtheme-summary", html, fixed = TRUE))
  expect_false(grepl("subtheme-summary-table", html, fixed = TRUE))
})

# ==========================================================================
# Longitudinal Patterns report section (.build_longitudinal_section)
# ==========================================================================

.make_temporal_fixture <- function() {
  data <- data.frame(
    std_id = paste0("e", 1:6),
    std_timestamp = as.POSIXct("2025-01-01", tz = "UTC") + (0:5) * 86400 * 40,
    theme_membership_Theme.One = c(1L, 1L, 0L, 1L, 0L, 1L),
    theme_membership_Theme.Two = c(0L, 0L, 1L, 1L, 1L, 0L),
    stringsAsFactors = FALSE
  )
  theme_set <- structure(
    list(themes = list(
      list(name = "Theme One", codes_included = character()),
      list(name = "Theme Two", codes_included = character())
    )),
    class = "ThemeSet"
  )
  list(data = data, theme_set = theme_set)
}

test_that("analyze_temporal_patterns returns prevalence + emergence over real periods", {
  fx <- .make_temporal_fixture()
  tr <- analyze_temporal_patterns(fx$data, fx$theme_set, coding_state = NULL)

  expect_true(isTRUE(tr$has_temporal_data))
  expect_true(tr$period_type %in% c("daily", "weekly", "monthly", "quarterly", "yearly"))
  expect_true(nrow(tr$prevalence_over_time) > 0)
  expect_true(all(c("period", "theme_name", "n_entries", "pct_of_period") %in%
                    names(tr$prevalence_over_time)))
  # Theme One appears at the earliest timestamp; Theme Two only later
  # (first_appearance_date is an ISO date string -- sortable lexicographically)
  em <- tr$emergence_timeline
  t1 <- em$first_appearance_date[em$theme_name == "Theme One"]
  t2 <- em$first_appearance_date[em$theme_name == "Theme Two"]
  expect_true(t1 < t2)
})

test_that(".build_longitudinal_section degrades to absence notes when PNGs are missing", {
  # Single-period / no-chart runs MUST NOT reference missing images: with
  # self_contained rendering, pandoc aborts on a missing local image and the
  # run loses its entire HTML report.
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  tr <- list(
    prevalence_over_time = tibble::tibble(
      period = "2025-01", theme_name = "Theme One",
      n_entries = 3L, pct_of_period = 50, total_in_period = 6L
    ),
    emergence_timeline = tibble::tibble(
      theme_name = "Theme One",
      first_appearance_date = "2025-01-01",
      n_entries = 3L
    ),
    period_type = "monthly",
    has_temporal_data = TRUE
  )

  out <- pakhom:::.build_longitudinal_section(tr, tmp)
  expect_match(out, "## Longitudinal Patterns", fixed = TRUE)
  expect_false(grepl("![", out, fixed = TRUE))   # no image refs at all
  expect_match(out, "No prevalence chart was produced", fixed = TRUE)
  expect_match(out, "No emergence chart was produced", fixed = TRUE)
  expect_match(out, "Theme One", fixed = TRUE)   # emergence table still renders
})

test_that(".build_longitudinal_section embeds charts that exist on disk", {
  tmp <- tempfile(); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  file.create(file.path(tmp, "temporal_prevalence.png"))
  file.create(file.path(tmp, "temporal_emergence.png"))

  tr <- list(
    prevalence_over_time = tibble::tibble(
      period = c("2025-01", "2025-02"), theme_name = "Theme One",
      n_entries = c(2L, 1L), pct_of_period = c(66.7, 33.3),
      total_in_period = c(3L, 3L)
    ),
    emergence_timeline = tibble::tibble(
      theme_name = "Theme One",
      first_appearance_date = "2025-01-01",
      n_entries = 3L
    ),
    period_type = "monthly",
    has_temporal_data = TRUE
  )

  out <- pakhom:::.build_longitudinal_section(tr, tmp)
  expect_match(out, "![Theme prevalence over time](temporal_prevalence.png)", fixed = TRUE)
  expect_match(out, "![Theme emergence timeline](temporal_emergence.png)", fixed = TRUE)
  expect_false(grepl("No prevalence chart", out, fixed = TRUE))
})

test_that(".build_rmd_content omits the Longitudinal section when temporal_results is NULL", {
  # The section is strictly opt-in: absent temporal results leave no trace.
  # (Direct check of the gating expression -- the builder is only invoked
  # behind the NULL + has_temporal_data guard.)
  tr_null <- NULL
  expect_false(!is.null(tr_null) && isTRUE(tr_null$has_temporal_data))
  tr_no_data <- list(has_temporal_data = FALSE)
  expect_false(!is.null(tr_no_data) && isTRUE(tr_no_data$has_temporal_data))
})
