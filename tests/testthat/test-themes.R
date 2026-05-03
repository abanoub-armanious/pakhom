# Tests for iterative theme generation and deterministic cascading
# (16_themes.R)

test_that("cascade_theme_assignments maps entries through codes deterministically", {
  # Create mock coding state
  state <- create_coding_state()
  state$codebook[["med_helps"]] <- list(
    code_name = "Medication helps binge control", description = "",
    type = "descriptive", frequency = 3L,
    entry_ids = c("e1", "e2", "e3"), coded_segments = list()
  )
  state$codebook[["sleep_bad"]] <- list(
    code_name = "Sleep disruption", description = "",
    type = "descriptive", frequency = 2L,
    entry_ids = c("e1", "e4"), coded_segments = list()
  )
  state$entry_results[["e1"]] <- list(codes_assigned = c("med_helps", "sleep_bad"), skipped = FALSE)
  state$entry_results[["e2"]] <- list(codes_assigned = c("med_helps"), skipped = FALSE)
  state$entry_results[["e3"]] <- list(codes_assigned = c("med_helps"), skipped = FALSE)
  state$entry_results[["e4"]] <- list(codes_assigned = c("sleep_bad"), skipped = FALSE)

  # Create theme set with merge history
  ts <- create_theme_set(list(
    list(id = 1, name = "Medication Benefits", description = "",
         codes_included = "Medication helps binge control"),
    list(id = 2, name = "Sleep Problems", description = "",
         codes_included = "Sleep disruption")
  ))
  ts$merge_history <- list(
    passes = list(),
    code_to_theme_map = list(med_helps = "Medication Benefits",
                              sleep_bad = "Sleep Problems"),
    code_to_subtheme_map = list()
  )

  data <- tibble::tibble(
    std_id = c("e1", "e2", "e3", "e4"),
    std_text = c("text1", "text2", "text3", "text4")
  )

  result <- cascade_theme_assignments(data, state, ts)

  # e1 has both codes -> assigned to both themes (multi-label)
  expect_true(grepl("Medication Benefits", result$emerged_themes[1]))
  # e2, e3 -> Medication Benefits
  expect_equal(result$emerged_themes[2], "Medication Benefits")
  expect_equal(result$emerged_themes[3], "Medication Benefits")
  # e4 -> Sleep Problems
  expect_equal(result$emerged_themes[4], "Sleep Problems")
  # e1 should be assigned to both themes (multi-label)
  expect_true(grepl("Sleep Problems", result$emerged_themes[1]))
  expect_true(grepl("Medication Benefits", result$emerged_themes[1]))
  # n_themes: e1 has 2, others have 1
  expect_equal(result$n_themes[1], 2L)
  expect_equal(result$n_themes[2], 1L)
})

test_that("cascade_theme_assignments handles skipped entries", {
  state <- create_coding_state()
  state$entry_results[["e1"]] <- list(codes_assigned = character(0), skipped = TRUE, skip_reason = "N/A")

  ts <- create_theme_set(list(
    list(id = 1, name = "Theme A", description = "", codes_included = "code1")
  ))
  ts$merge_history <- list(
    passes = list(),
    code_to_theme_map = list(x = "Theme A"),
    code_to_subtheme_map = list()
  )

  data <- tibble::tibble(std_id = "e1", std_text = "text")
  result <- cascade_theme_assignments(data, state, ts)

  expect_true(is.na(result$emerged_themes[1]))
})

test_that("enrich_themes computes entry counts and sentiment", {
  ts <- create_theme_set(list(
    list(id = 1, name = "Positive Theme", description = "", codes_included = "a"),
    list(id = 2, name = "Negative Theme", description = "", codes_included = "b")
  ))

  # Data must have theme_membership_* columns (set by cascade_theme_assignments)
  data <- tibble::tibble(
    std_id = c("e1", "e2", "e3"),
    std_text = c("text1", "text2", "text3"),
    emerged_themes = c("Positive Theme", "Positive Theme", "Negative Theme"),
    sentiment_score = c(0.5, 0.8, -0.6),
    theme_membership_Positive.Theme = c(1L, 1L, 0L),
    theme_membership_Negative.Theme = c(0L, 0L, 1L)
  )

  enriched <- enrich_themes(ts, data)
  expect_equal(enriched$themes[[1]]$entry_count, 2)
  expect_equal(enriched$themes[[2]]$entry_count, 1)
  expect_equal(enriched$themes[[1]]$sentiment_tendency, "positive")
  expect_equal(enriched$themes[[2]]$sentiment_tendency, "negative")
})

# T0.2: enrich_themes populates supporting_quotes via the spread-aware,
# sentiment-positioned selector (was previously random sampling -- which
# meant the report's most_negative/median/most_positive labels lied AND
# the participant-spread logic in .select_representative_quotes was
# effectively dead code on the production path).

test_that("enrich_themes uses sentiment-positioned (not random) supporting_quotes", {
  ts <- create_theme_set(list(
    list(id = 1, name = "T", description = "", codes_included = "a")
  ))
  set.seed(123)
  data <- tibble::tibble(
    std_id   = paste0("e", 1:7),
    std_text = paste0("This is a quote long enough to pass the 50-character filter, entry ",
                      1:7, ". Lorem ipsum."),
    emerged_themes = rep("T", 7),
    sentiment_score = c(-0.9, -0.6, -0.3, 0.0, 0.3, 0.6, 0.9),
    theme_membership_T = rep(1L, 7)
  )
  enriched <- enrich_themes(ts, data)
  qs <- enriched$themes[[1]]$supporting_quotes
  expect_length(qs, 3L)
  # Sentiment-positioned selection: first quote should be from the
  # most-negative entry (e1, sentiment -0.9), last from most-positive
  # (e7, sentiment 0.9). Middle is the median.
  expect_match(qs[1], "entry 1\\.")
  expect_match(qs[3], "entry 7\\.")
})

test_that("enrich_themes prefers different contributors for supporting_quotes (T0.2 spread-aware)", {
  ts <- create_theme_set(list(
    list(id = 1, name = "T", description = "", codes_included = "a")
  ))
  data <- tibble::tibble(
    std_id   = paste0("e", 1:6),
    std_text = paste0("This is a quote long enough to pass the 50-character filter, entry ",
                      1:6, ". Lorem ipsum dolor."),
    std_author = c("heavy", "heavy", "heavy", "alice", "bob", "carol"),
    emerged_themes = rep("T", 6),
    sentiment_score = c(-0.9, -0.5, -0.2, 0.1, 0.4, 0.8),
    theme_membership_T = rep(1L, 6)
  )
  enriched <- enrich_themes(ts, data)
  qs <- enriched$themes[[1]]$supporting_quotes
  expect_length(qs, 3L)
  # Most-negative slot (sentiment -0.9, author heavy): heavy is taken.
  # Median slot (target idx 3 in sentiment-sorted = -0.2 by heavy):
  # spread-aware should walk outward and find alice/bob/carol instead.
  # Most-positive slot (carol at 0.8): carol is fresh -> selected.
  # The middle quote text should NOT come from "entry 3" (heavy at -0.2);
  # spread-aware picks an alternative author's row.
  expect_false(grepl("entry 3\\.", qs[2], fixed = FALSE))
})

test_that("enrich_themes handles single-entry themes gracefully", {
  ts <- create_theme_set(list(
    list(id = 1, name = "T", description = "", codes_included = "a")
  ))
  data <- tibble::tibble(
    std_id   = "e1",
    std_text = "This is the only entry, long enough to pass filtering filters.",
    emerged_themes = "T",
    sentiment_score = 0.5,
    theme_membership_T = 1L
  )
  enriched <- enrich_themes(ts, data)
  # 1 entry -> 1 supporting quote (most_negative slot only)
  expect_length(enriched$themes[[1]]$supporting_quotes, 1L)
})

test_that("enrich_themes handles entries below 50-char text filter (no supporting_quotes)", {
  ts <- create_theme_set(list(
    list(id = 1, name = "T", description = "", codes_included = "a")
  ))
  data <- tibble::tibble(
    std_id   = c("e1", "e2"),
    std_text = c("short", "also short"),  # < 50 chars; .select_representative_quotes filters out
    emerged_themes = c("T", "T"),
    sentiment_score = c(-0.5, 0.5),
    theme_membership_T = c(1L, 1L)
  )
  enriched <- enrich_themes(ts, data)
  # No quotes pass the filter -> supporting_quotes stays NULL/unset
  expect_true(is.null(enriched$themes[[1]]$supporting_quotes) ||
              length(enriched$themes[[1]]$supporting_quotes) == 0L)
})


test_that("theme_membership columns are created", {
  state <- create_coding_state()
  state$codebook[["a"]] <- list(code_name = "Code A", frequency = 1L,
                                 entry_ids = "e1", coded_segments = list())
  state$entry_results[["e1"]] <- list(codes_assigned = "a", skipped = FALSE)

  ts <- create_theme_set(list(
    list(id = 1, name = "Theme One", description = "", codes_included = "Code A")
  ))
  ts$merge_history <- list(
    passes = list(),
    code_to_theme_map = list(a = "Theme One"),
    code_to_subtheme_map = list()
  )

  data <- tibble::tibble(std_id = "e1", std_text = "text")
  result <- cascade_theme_assignments(data, state, ts)

  membership_col <- paste0("theme_membership_", make.names("Theme One"))
  expect_true(membership_col %in% names(result))
  expect_equal(result[[membership_col]][1], 1L)
})
