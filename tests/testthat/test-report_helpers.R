# Tests for report helper functions (20_report_helpers.R)

test_that("get_emotion_interpretation returns known interpretation", {
  result <- get_emotion_interpretation("sadness")
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  expect_true(grepl("grief|loss|pain", result))
})

test_that("get_emotion_interpretation handles uppercase input", {
  result <- get_emotion_interpretation("ANGER")
  expect_type(result, "character")
  expect_true(grepl("frustration|resentment|injustice", result))
})

test_that("get_emotion_interpretation handles unknown emotions", {
  result <- get_emotion_interpretation("bewilderment")
  expect_type(result, "character")
  expect_true(grepl("bewilderment", result))
})

test_that("get_emotion_interpretation covers all built-in emotions", {
  known_emotions <- c("sadness", "anger", "fear", "disgust", "joy",
                       "surprise", "trust", "anticipation", "frustration",
                       "anxiety", "hope", "shame", "guilt", "confusion",
                       "resignation", "relief", "gratitude", "empathy")
  for (em in known_emotions) {
    result <- get_emotion_interpretation(em)
    expect_true(nchar(result) > 0, info = paste("Empty result for", em))
    # Known emotions should NOT have the fallback pattern
    expect_false(grepl(paste0("reflects ", em, "-related"), result),
                 info = paste("Fallback used for known emotion:", em))
  }
})

test_that("aggregate_overall_statistics returns required fields", {
  data <- tibble::tibble(
    std_id = paste0("e", 1:5),
    std_text = paste("Text", 1:5),
    emerged_themes = c("A", "A", "B", "B", "A"),
    theme_membership_A = c(1L, 1L, 0L, 0L, 1L),
    theme_membership_B = c(0L, 0L, 1L, 1L, 0L),
    sentiment_score = c(0.5, -0.3, 0.1, -0.8, 0.2),
    all_emotions = c("joy", "sadness", "neutral", "anger", "hope")
  )
  theme_set <- create_theme_set(list(
    list(name = "A", description = "Theme A", codes_included = "c1"),
    list(name = "B", description = "Theme B", codes_included = "c2")
  ))
  result <- aggregate_overall_statistics(data, theme_set)
  expect_type(result, "list")
  expect_equal(result$total_entries, 5)
  expect_equal(result$n_themes, 2)
  expect_true(!is.null(result$sentiment))
  expect_true(!is.null(result$sentiment$mean))
  expect_true(!is.null(result$sentiment$sd))
  expect_true(!is.null(result$emotions))
  expect_true(!is.null(result$themes))
})

test_that("aggregate_overall_statistics handles missing optional columns", {
  data <- tibble::tibble(
    std_id = paste0("e", 1:3),
    std_text = paste("Text", 1:3),
    sentiment_score = c(0.1, -0.2, 0.3)
  )
  theme_set <- create_theme_set(list(
    list(name = "Only", description = "Only theme", codes_included = "c1")
  ))
  # No emerged_themes or all_emotions columns
  result <- aggregate_overall_statistics(data, theme_set)
  expect_equal(result$total_entries, 3)
  expect_equal(result$n_themes, 1)
  # themes df should be empty since no emerged_themes column
  expect_equal(nrow(result$themes), 0)
})

test_that("C-11 audit followup LOW #6: theme names with special chars round-trip via theme_set", {
  # Earlier, the theme distribution path used
  # sub("^theme_membership_", "", names(theme_counts)) +
  # gsub("\\.", " ", theme_labels) to recover the original theme name
  # from the membership column name. make.names() collapses MANY chars
  # to periods (-, ', /, :, ',', (, ) etc.) -- the reverse mapping only
  # handles space. 71 of 417 themes in an early run were therefore
  # MISSING from the dashboard (every theme with a hyphen, apostrophe,
  # slash, colon, comma, or paren in its name).
  # The C-11 fix iterates theme_set$themes directly and computes
  # safe_col = paste0("theme_membership_", make.names(t$name)) for each
  # ORIGINAL name.
  special_names <- c(
    "Scheduling's Role in Workload",                # apostrophe
    "Self-Compassion and Recovery",                  # hyphen
    "Stress, Body Weight & Physical Health",         # comma + ampersand
    "Focus (Acute) Patterns / Onset Times"           # parens + slash
  )
  safe_cols <- paste0("theme_membership_", make.names(special_names))
  data <- tibble::tibble(
    std_id          = paste0("e", 1:4),
    std_text        = paste("Text", 1:4),
    sentiment_score = c(0.1, -0.1, 0.0, 0.2)  # aggregate_overall_statistics needs this
  )
  for (i in seq_along(safe_cols)) {
    col <- safe_cols[i]
    data[[col]] <- as.integer(seq_along(data$std_id) == i)
  }
  theme_set <- create_theme_set(lapply(special_names, function(n) {
    list(name = n, description = "", codes_included = "c1")
  }))
  result <- aggregate_overall_statistics(data, theme_set)
  # Every ORIGINAL theme name appears in the dashboard's themes_df
  # (pre-fix, names like "Scheduling's Role in Workload" would have
  # been corrupted to "Scheduling s Role in Workload").
  for (n in special_names) {
    expect_true(n %in% result$themes$theme_name,
                info = sprintf("theme '%s' missing from dashboard themes_df", n))
  }
  # Every theme is counted exactly once.
  expect_equal(sum(result$themes$n), 4L)
})

# ==============================================================================
# T0.2: Participant spread per theme
# ==============================================================================
# T0.2 answers Jowsey 2025's "Frankenstein" finding that "none of the
# Copilot outputs reported the participant spread". Three metrics added to
# every theme's stats:
#   - n_distinct_contributors (count of unique authors)
#   - contributor_gini (0 = perfectly even, 1 = one contributor takes all)
#   - top_contributor_share (fraction from the most prolific contributor)
# Plus spread-aware representative quote selection so the displayed quotes
# don't all come from one heavy poster.

# ---- .gini_coefficient ------------------------------------------------------

test_that(".gini_coefficient is 0 for perfectly equal distributions", {
  expect_equal(pakhom:::.gini_coefficient(c(5L, 5L, 5L, 5L)), 0)
  expect_equal(pakhom:::.gini_coefficient(c(1L, 1L)), 0)
})

test_that(".gini_coefficient approaches 1 as inequality grows", {
  # One contributor with 100, others with 1 -- highly unequal
  high_inequality <- c(100L, rep(1L, 50L))
  g <- pakhom:::.gini_coefficient(high_inequality)
  expect_true(g > 0.6)
  expect_true(g < 1)

  # Perfectly unequal: one contributor gets everything
  # G_max for n contributors is (n-1)/n
  perfect_unequal <- c(100L, rep(0L, 9L))
  g <- pakhom:::.gini_coefficient(perfect_unequal)
  # n=10 -> max Gini = 9/10 = 0.9 (the formula returns the sample-Gini)
  expect_equal(round(g, 1), 0.9)
})

test_that(".gini_coefficient classic textbook example: c(1, 2, 3, 4, 5) -> ~0.27", {
  # Standard worked example: counts 1,2,3,4,5 has Gini = 4/15 ~= 0.267
  expect_equal(round(pakhom:::.gini_coefficient(c(1L, 2L, 3L, 4L, 5L)), 3),
               round(4 / 15, 3))
})

test_that(".gini_coefficient handles degenerate inputs", {
  expect_identical(pakhom:::.gini_coefficient(integer(0)), NA_real_)
  expect_identical(pakhom:::.gini_coefficient(c(0L, 0L, 0L)), NA_real_)
  # NA in input
  expect_identical(pakhom:::.gini_coefficient(c(1L, NA_integer_, 3L)),
                   NA_real_)
  # Negative values shouldn't occur for counts but guard anyway
  expect_identical(pakhom:::.gini_coefficient(c(1L, -1L, 3L)), NA_real_)
})

test_that(".gini_coefficient clamps tiny floating-point negatives to 0", {
  # Closed-form can produce -1e-16 for perfectly equal inputs due to
  # floating-point round-off. The implementation clamps to [0, 1].
  g <- pakhom:::.gini_coefficient(rep(1.0, 100))
  expect_true(g >= 0)
  expect_true(g <= 1)
})

# ---- .compute_participant_spread --------------------------------------------

test_that(".compute_participant_spread on entries without std_author returns unavailable", {
  entries <- tibble::tibble(std_id = paste0("e", 1:3),
                            std_text = paste("text", 1:3),
                            sentiment_score = c(0.1, 0.2, 0.3))
  ps <- pakhom:::.compute_participant_spread(entries)
  expect_false(ps$available)
  expect_equal(ps$n_distinct_contributors, 0L)
  expect_identical(ps$contributor_gini, NA_real_)
  expect_identical(ps$top_contributor_share, NA_real_)
})

test_that(".compute_participant_spread on all-NA std_author returns unavailable", {
  entries <- tibble::tibble(std_id = paste0("e", 1:3),
                            std_author = c(NA_character_, NA_character_, NA_character_),
                            sentiment_score = c(0.1, 0.2, 0.3))
  ps <- pakhom:::.compute_participant_spread(entries)
  expect_false(ps$available)
})

test_that(".compute_participant_spread on empty-string std_author returns unavailable", {
  entries <- tibble::tibble(std_author = c("", "", ""),
                            sentiment_score = c(0.1, 0.2, 0.3))
  ps <- pakhom:::.compute_participant_spread(entries)
  expect_false(ps$available)
})

test_that(".compute_participant_spread: even spread -> Gini 0, top share 1/n", {
  entries <- tibble::tibble(
    std_author = c("alice", "bob", "carol", "dave"),
    sentiment_score = c(0.1, 0.2, 0.3, 0.4)
  )
  ps <- pakhom:::.compute_participant_spread(entries)
  expect_true(ps$available)
  expect_equal(ps$n_distinct_contributors, 4L)
  expect_equal(ps$contributor_gini, 0)
  expect_equal(ps$top_contributor_share, 0.25)
})

test_that(".compute_participant_spread: single contributor -> Gini NA, top share 1.0", {
  entries <- tibble::tibble(
    std_author = c("alice", "alice", "alice"),
    sentiment_score = c(0.1, 0.2, 0.3)
  )
  ps <- pakhom:::.compute_participant_spread(entries)
  expect_true(ps$available)
  expect_equal(ps$n_distinct_contributors, 1L)
  # Gini undefined for n=1 -- documented as NA so dashboard distinguishes
  # "1 contributor (Gini meaningless)" from "perfectly even (Gini = 0)"
  expect_identical(ps$contributor_gini, NA_real_)
  expect_equal(ps$top_contributor_share, 1.0)
})

test_that(".compute_participant_spread: heavy-poster theme -> high Gini + high top share", {
  entries <- tibble::tibble(
    std_author = c(rep("heavy", 8), "alice", "bob"),  # 8/10 from one poster
    sentiment_score = seq(0.1, 1.0, length.out = 10)
  )
  ps <- pakhom:::.compute_participant_spread(entries)
  expect_equal(ps$n_distinct_contributors, 3L)
  expect_equal(ps$top_contributor_share, 0.8)
  expect_true(ps$contributor_gini > 0.4)  # substantial inequality
})

test_that(".compute_participant_spread ignores NA authors mixed with real ones", {
  entries <- tibble::tibble(
    std_author = c("alice", "alice", NA_character_, "bob"),
    sentiment_score = c(0.1, 0.2, 0.3, 0.4)
  )
  ps <- pakhom:::.compute_participant_spread(entries)
  # Only alice + bob counted; NA filtered out
  expect_equal(ps$n_distinct_contributors, 2L)
  expect_equal(ps$top_contributor_share, 2 / 3)  # alice has 2 of 3 non-NA
})

test_that(".empty_participant_spread shape stays stable", {
  empty <- pakhom:::.empty_participant_spread()
  expect_named(empty, c("n_distinct_contributors", "contributor_gini",
                         "top_contributor_share", "available"))
  expect_equal(empty$n_distinct_contributors, 0L)
  expect_false(empty$available)
})

# ---- aggregate_theme_statistics with participant_spread ---------------------

test_that("aggregate_theme_statistics adds participant_spread to each theme", {
  data <- tibble::tibble(
    std_id = paste0("e", 1:6),
    std_text = paste("text", 1:6),
    std_author = c("alice", "bob", "carol", "alice", "dave", "eve"),
    emerged_themes = c("A", "A", "A", "B", "B", "B"),
    theme_membership_A = c(1L, 1L, 1L, 0L, 0L, 0L),
    theme_membership_B = c(0L, 0L, 0L, 1L, 1L, 1L),
    sentiment_score = c(0.5, -0.3, 0.1, -0.8, 0.2, 0.4),
    emotion_intensity = c(0.4, 0.6, 0.3, 0.8, 0.5, 0.4),
    all_emotions = c("joy", "sad", "neutral", "anger", "hope", "joy")
  )
  ts <- create_theme_set(list(
    list(name = "A", description = "", codes_included = "c1"),
    list(name = "B", description = "", codes_included = "c2")
  ))
  stats <- aggregate_theme_statistics(data, ts)

  # Both themes have participant_spread populated
  expect_true(!is.null(stats[["A"]]$participant_spread))
  expect_true(!is.null(stats[["B"]]$participant_spread))
  expect_true(stats[["A"]]$participant_spread$available)
  expect_equal(stats[["A"]]$participant_spread$n_distinct_contributors, 3L)
  expect_equal(stats[["B"]]$participant_spread$n_distinct_contributors, 3L)
})

test_that("aggregate_theme_statistics empty theme has empty-shape participant_spread", {
  data <- tibble::tibble(
    std_id = paste0("e", 1:3),
    std_text = paste("text", 1:3),
    std_author = c("alice", "bob", "carol"),
    theme_membership_A = c(0L, 0L, 0L),  # no entries match
    sentiment_score = c(0.1, 0.2, 0.3),
    emotion_intensity = c(0.5, 0.6, 0.4),
    all_emotions = c("joy", "sad", "joy")
  )
  ts <- create_theme_set(list(
    list(name = "A", description = "", codes_included = "c1")
  ))
  stats <- aggregate_theme_statistics(data, ts)
  expect_equal(stats[["A"]]$n_entries, 0L)
  expect_false(stats[["A"]]$participant_spread$available)
})

test_that("aggregate_theme_statistics on data without std_author column reports unavailable", {
  data <- tibble::tibble(
    std_id = paste0("e", 1:3),
    std_text = paste("text", 1:3),
    theme_membership_A = c(1L, 1L, 1L),
    sentiment_score = c(0.1, 0.2, 0.3),
    emotion_intensity = c(0.5, 0.6, 0.4),
    all_emotions = c("joy", "sad", "joy")
  )
  ts <- create_theme_set(list(
    list(name = "A", description = "", codes_included = "c1")
  ))
  stats <- aggregate_theme_statistics(data, ts)
  expect_false(stats[["A"]]$participant_spread$available)
  # n_entries still computed (3), just no spread metrics
  expect_equal(stats[["A"]]$n_entries, 3L)
})

# ---- Spread-aware representative quote selection ----------------------------

test_that(".select_representative_quotes prefers different contributors when authors differ", {
  set.seed(42)
  # 6 entries: 3 from "heavy" (with varied sentiment), 3 from singletons
  entries <- tibble::tibble(
    std_text = paste("This is entry text long enough to pass the 50-char filter, entry", 1:6),
    std_author = c("heavy", "heavy", "heavy", "alice", "bob", "carol"),
    sentiment_score = c(-0.9, -0.6, -0.3, 0.0, 0.4, 0.8),
    all_emotions = c("anger", "fear", "sadness", "neutral", "hope", "joy")
  )
  quotes <- pakhom:::.select_representative_quotes(entries, n_quotes = 3)
  expect_length(quotes, 3L)
  # The 3 sentiment-positioned slots: sorted by sentiment, position 1 (most
  # negative = heavy at -0.9), median = position 3 (heavy at -0.3 -- but
  # taken_authors already has heavy, so should expand outward), position 6
  # (most positive = carol at 0.8).
  # We verify spread-aware-ness by checking that the chosen quotes are
  # NOT all "heavy"-authored.
  chosen_texts <- vapply(quotes, function(q) q$text, character(1))
  # At least 2 different authors should be represented (we can't directly
  # see authors in the output, but indirect check: the median quote's
  # sentiment shouldn't be -0.3 (heavy) -- spread-aware should have moved
  # it to alice/bob).
  median_q <- quotes$median
  expect_false(round(median_q$sentiment, 1) == -0.3)
})

test_that(".select_representative_quotes single-contributor falls back to original behavior", {
  entries <- tibble::tibble(
    std_text = paste("This is entry text long enough to pass the 50-char filter, entry", 1:5),
    std_author = c("alice", "alice", "alice", "alice", "alice"),
    sentiment_score = c(-0.8, -0.4, 0.0, 0.4, 0.8),
    all_emotions = c("anger", "sadness", "neutral", "hope", "joy")
  )
  quotes <- pakhom:::.select_representative_quotes(entries, n_quotes = 3)
  expect_length(quotes, 3L)
  # All 3 from the same contributor (no alternative). Sentiments at the
  # extremes should be intact.
  expect_equal(round(quotes$most_negative$sentiment, 1), -0.8)
  expect_equal(round(quotes$most_positive$sentiment, 1), 0.8)
})

test_that(".select_representative_quotes works without std_author column (legacy data)", {
  entries <- tibble::tibble(
    std_text = paste("This is entry text long enough to pass the 50-char filter, entry", 1:5),
    sentiment_score = c(-0.8, -0.4, 0.0, 0.4, 0.8),
    all_emotions = c("anger", "sadness", "neutral", "hope", "joy")
  )
  quotes <- pakhom:::.select_representative_quotes(entries, n_quotes = 3)
  expect_length(quotes, 3L)
  # Without author info, behavior is the original sentiment-positional
  # selection: position 1 (-0.8), median position 3 (0.0), last (0.8).
  expect_equal(round(quotes$most_negative$sentiment, 1), -0.8)
  expect_equal(round(quotes$median$sentiment,        1),  0.0)
  expect_equal(round(quotes$most_positive$sentiment, 1),  0.8)
})

test_that(".select_representative_quotes prefers theme-characteristic entries over diffuse multi-theme posts (#8)", {
  # e1 is the GLOBALLY most-negative entry but is coded into 4 themes (a diffuse
  # post); e5-e8 are specific to ThisTheme. Pre-#8, sentiment-extremity made e1 the
  # lead for every theme it touched. Post-#8, the lead is drawn from the
  # theme-characteristic (low-breadth) entries so the exemplar actually fits.
  entries <- tibble::tibble(
    std_id          = paste0("e", 1:8),
    std_text        = paste("On-theme entry text long enough to pass the fifty-character filter, item", 1:8),
    std_author      = paste0("auth", 1:8),
    sentiment_score = c(-0.9, -0.3, -0.2, -0.1, 0.0, 0.1, 0.2, 0.3),
    all_emotions    = rep("sadness", 8),
    theme_membership_ThisTheme = rep(1L, 8),
    theme_membership_OtherA    = c(1L, 1L, 1L, 1L, 0L, 0L, 0L, 0L),  # e1-e4 diffuse
    theme_membership_OtherB    = c(1L, 1L, 0L, 0L, 0L, 0L, 0L, 0L),
    theme_membership_OtherC    = c(1L, 0L, 0L, 0L, 0L, 0L, 0L, 0L)   # e1 in 4 themes
  )
  # breadth = c(4,3,2,2,1,1,1,1); median 1.5 -> specific pool = e5..e8 (breadth 1)
  quotes <- pakhom:::.select_representative_quotes(entries, n_quotes = 3)
  expect_length(quotes, 3L)
  expect_false(quotes$most_negative$entry_id == "e1")                 # diffuse extreme NOT the lead
  expect_true(quotes$most_negative$entry_id %in% c("e5", "e6", "e7", "e8"))  # lead is theme-specific
  # CONTROL: with no theme_membership_* columns the filter is skipped and the old
  # sentiment-extremity behavior is preserved (e1 leads) -- graceful fallback.
  q0 <- pakhom:::.select_representative_quotes(
    entries[, c("std_id", "std_text", "std_author", "sentiment_score", "all_emotions")],
    n_quotes = 3)
  expect_equal(q0$most_negative$entry_id, "e1")
})

test_that(".pick_quote_with_spread expands outward correctly", {
  valid_df <- tibble::tibble(
    std_text = letters[1:7],
    std_author = c("a", "a", "b", "c", "c", "d", "e"),
    sentiment_score = seq(-1, 1, length.out = 7)
  )
  # Target index 2 (author "a"), but author "a" already taken -> should
  # walk outward and find index 1 (also "a", still taken) then 3 ("b",
  # not taken) -- returns 3.
  chosen <- pakhom:::.pick_quote_with_spread(
    valid_df = valid_df, target_idx = 2L,
    taken_indices = integer(0), taken_authors = "a",
    has_authors = TRUE
  )
  expect_equal(chosen, 3L)
})

test_that(".pick_quote_with_spread returns target_idx when no acceptable alternative", {
  valid_df <- tibble::tibble(
    std_author = c("a", "a", "a"),
    sentiment_score = c(-1, 0, 1)
  )
  chosen <- pakhom:::.pick_quote_with_spread(
    valid_df = valid_df, target_idx = 2L,
    taken_indices = integer(0), taken_authors = "a",  # all rows are "a"
    has_authors = TRUE
  )
  # Falls back to target_idx
  expect_equal(chosen, 2L)
})

# ---- .build_participant_spread_card -----------------------------------------

test_that(".build_participant_spread_card renders metrics for available data", {
  ps <- list(n_distinct_contributors = 5L, contributor_gini = 0.42,
             top_contributor_share = 0.30, available = TRUE)
  html <- pakhom:::.build_participant_spread_card(ps)
  expect_match(html, "Participant Distribution")
  expect_match(html, ">5<", fixed = TRUE)        # contributor count
  expect_match(html, "0\\.42")                    # Gini
  expect_match(html, "30%")                       # top share
  # No warning for moderate concentration
  expect_false(grepl("ps-warning", html))
})

test_that(".build_participant_spread_card warns when one contributor dominates", {
  ps <- list(n_distinct_contributors = 3L, contributor_gini = 0.7,
             top_contributor_share = 0.75, available = TRUE)
  html <- pakhom:::.build_participant_spread_card(ps)
  expect_match(html, "ps-warning")
  expect_match(html, "75% of this theme")
})

test_that(".build_participant_spread_card renders single-contributor warning", {
  ps <- list(n_distinct_contributors = 1L, contributor_gini = NA_real_,
             top_contributor_share = 1.0, available = TRUE)
  html <- pakhom:::.build_participant_spread_card(ps)
  expect_match(html, "Single contributor")
  expect_match(html, "ps-warning")
})

test_that(".build_participant_spread_card renders unavailable variant when author data missing", {
  ps <- pakhom:::.empty_participant_spread()
  html <- pakhom:::.build_participant_spread_card(ps)
  expect_match(html, "participant-spread-unavailable")
  expect_match(html, "Author data not available")
  expect_false(grepl("ps-warning", html))
})

test_that(".build_participant_spread_card on NULL renders unavailable (legacy stats)", {
  html <- pakhom:::.build_participant_spread_card(NULL)
  expect_match(html, "participant-spread-unavailable")
  # Slightly different message for the legacy case but still indicates absence
  expect_match(html, "Author data not available")
})
