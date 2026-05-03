# Tests for ThemeSet S3 class (15_theme_data.R)

test_that("create_theme_set creates valid ThemeSet", {
  ts <- create_theme_set(list(
    list(name = "Theme A", description = "Desc A", codes_included = c("code1", "code2")),
    list(name = "Theme B", description = "Desc B", codes_included = c("code3"))
  ))

  expect_s3_class(ts, "ThemeSet")
  expect_equal(n_themes(ts), 2)
  expect_equal(theme_names(ts), c("Theme A", "Theme B"))
})

test_that("normalize_theme_result handles list-of-lists input", {
  input <- list(themes = list(
    list(name = "T1", description = "D1", codes_included = list("a", "b")),
    list(name = "T2", description = "D2", codes_included = list("c"))
  ))

  ts <- normalize_theme_result(input)
  expect_s3_class(ts, "ThemeSet")
  expect_equal(n_themes(ts), 2)
  expect_equal(ts$themes[[1]]$name, "T1")
  expect_type(ts$themes[[1]]$codes_included, "character")
})

test_that("normalize_theme_result handles data.frame input", {
  input <- list(themes = data.frame(
    name = c("T1", "T2"),
    description = c("D1", "D2"),
    stringsAsFactors = FALSE
  ))

  ts <- normalize_theme_result(input)
  expect_s3_class(ts, "ThemeSet")
  expect_equal(n_themes(ts), 2)
  # Should always produce list-of-lists, never a data.frame
  expect_false(is.data.frame(ts$themes))
})

test_that("normalize_theme_result fills default fields", {
  input <- list(themes = list(
    list(name = "Minimal Theme")
  ))

  ts <- normalize_theme_result(input)
  t <- ts$themes[[1]]
  expect_equal(t$name, "Minimal Theme")
  expect_true(!is.null(t$description))
  expect_true(!is.null(t$prevalence))
  expect_true(!is.null(t$sentiment_tendency))
  expect_type(t$codes_included, "character")
  expect_type(t$keywords, "character")
  expect_type(t$subthemes, "character")
  expect_type(t$supporting_quotes, "character")
})

test_that("theme_set_to_tibble produces correct tibble", {
  ts <- create_theme_set(list(
    list(name = "A", description = "DA", codes_included = c("c1")),
    list(name = "B", description = "DB", codes_included = c("c2", "c3"))
  ))

  df <- theme_set_to_tibble(ts)
  expect_s3_class(df, "tbl_df")
  expect_equal(nrow(df), 2)
  expect_true("name" %in% names(df))
  expect_true("description" %in% names(df))
})

test_that("theme_set_to_tibble returns correct structure", {
  ts <- create_theme_set(list(
    list(name = "Theme X", description = "D", codes_included = c("code_a", "code_b")),
    list(name = "Theme Y", description = "D", codes_included = c("code_c"))
  ))

  tbl <- theme_set_to_tibble(ts)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 2)
  expect_true("Theme X" %in% tbl$name)
  expect_equal(tbl$n_codes[1], 2)
  expect_equal(tbl$n_codes[2], 1)
})

test_that("prune_empty_themes removes zero-entry themes", {
  ts <- create_theme_set(list(
    list(name = "Has Entries", description = "D", codes_included = "c1"),
    list(name = "Empty", description = "D", codes_included = "c2")
  ))
  ts$themes[[1]]$entry_count <- 5L
  ts$themes[[2]]$entry_count <- 0L

  pruned <- prune_empty_themes(ts)
  expect_equal(n_themes(pruned), 1)
  expect_equal(theme_names(pruned), "Has Entries")
})

test_that("create_theme_set handles character vector subthemes", {
  ts <- create_theme_set(list(
    list(name = "A", description = "D", codes_included = "c1",
         subthemes = c("sub1", "sub2"))
  ))
  expect_equal(ts$themes[[1]]$subthemes, c("sub1", "sub2"))
  expect_null(ts$themes[[1]]$subthemes_structured)
})

test_that("theme_set_to_tibble handles numeric IDs", {
  ts <- create_theme_set(list(
    list(id = 1.0, name = "A", description = "D", codes_included = "c1")
  ))
  df <- theme_set_to_tibble(ts)
  expect_s3_class(df, "tbl_df")
  expect_type(df$id, "integer")
})

test_that("theme_set_to_tibble includes narrative and supporting_quotes", {
  ts <- create_theme_set(list(
    list(name = "A", description = "D", codes_included = "c1",
         narrative = "Test narrative", supporting_quotes = c("q1", "q2"))
  ))
  df <- theme_set_to_tibble(ts)
  expect_true("narrative" %in% names(df))
  expect_true("supporting_quotes" %in% names(df))
  expect_equal(df$narrative, "Test narrative")
  expect_equal(df$supporting_quotes, "q1 | q2")
})
