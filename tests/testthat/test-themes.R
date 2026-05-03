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
