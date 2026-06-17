# Regression tests for cross-cutting peripheral correctness fixes.

# ==============================================================================
# .entry_in_theme: EXACT theme membership in a "; "-delimited emerged_themes
# (replaces a substring grepl that gave false positives across 7 modules)
# ==============================================================================
test_that(".entry_in_theme matches whole theme names, not substrings", {
  em <- c("Focus Problems; Anxiety",   # has "Focus Problems", NOT "Focus"
          "Focus",                      # exactly "Focus"
          "Anxiety; Focus",             # "Focus" as the 2nd token
          NA_character_,                # NA -> not a member
          "")                           # empty -> not a member

  # The crux: "Focus" must NOT match the entry whose only theme is "Focus
  # Problems" (the old grepl(tn, ..., fixed=TRUE) wrongly did).
  expect_equal(pakhom:::.entry_in_theme(em, "Focus"),
               c(FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(pakhom:::.entry_in_theme(em, "Focus Problems"),
               c(TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(pakhom:::.entry_in_theme(em, "Anxiety"),
               c(TRUE, FALSE, TRUE, FALSE, FALSE))
})

test_that(".entry_in_theme tolerates whitespace and empty input", {
  expect_equal(pakhom:::.entry_in_theme(c("A ; B", "B;C"), "B"),
               c(TRUE, TRUE))                       # trims around ';'
  expect_equal(pakhom:::.entry_in_theme(character(0), "X"), logical(0))
  expect_equal(pakhom:::.entry_in_theme("Other", "Missing"), FALSE)
})

# ==============================================================================
# .parse_timestamps: day/month handling (the ambiguity-aware parser)
# ==============================================================================
test_that(".parse_timestamps parses ISO-8601 unambiguously", {
  out <- pakhom:::.parse_timestamps(c("2024-01-15", "2024-02-20"))
  expect_equal(as.character(as.Date(out)), c("2024-01-15", "2024-02-20"))
})

test_that(".parse_timestamps respects day>12 to pick EU order", {
  # "13/05/2024" only parses under %d/%m/%Y -> 13 May (unambiguous, no warning).
  out <- pakhom:::.parse_timestamps(c("13/05/2024", "20/06/2024"))
  expect_equal(as.character(as.Date(out)), c("2024-05-13", "2024-06-20"))
})

test_that(".parse_timestamps does not crash on ambiguous slash dates", {
  # "03/05/2024" parses under both %m/%d and %d/%m; the parser must still
  # return a valid date (US order wins, with an ambiguity warning logged).
  out <- pakhom:::.parse_timestamps(c("03/05/2024", "04/06/2024"))
  expect_false(any(is.na(out)))
  expect_equal(as.character(as.Date(out[1])), "2024-03-05")  # US order chosen
})

test_that(".parse_timestamps returns NA vector when nothing parses", {
  out <- pakhom:::.parse_timestamps(c("not a date", "also not"))
  expect_true(all(is.na(out)))
  expect_equal(length(out), 2L)
})
