# Tests for ThemeSet S3 class (R/12_theme_data.R)
# Themes carry first-class Subtheme S3 objects holding Code S3
# objects. Legacy codes_included input is wrapped via the back-compat path
# into a single virtual (NA-named) Subtheme.

test_that("create_theme_set creates valid ThemeSet", {
  ts <- create_theme_set(list(
    list(name = "Theme A", description = "Desc A", codes_included = c("code1", "code2")),
    list(name = "Theme B", description = "Desc B", codes_included = c("code3"))
  ))

  expect_s3_class(ts, "ThemeSet")
  expect_equal(n_themes(ts), 2)
  expect_equal(theme_names(ts), c("Theme A", "Theme B"))
  # Legacy input is wrapped in a virtual Subtheme; canonical theme$subthemes
  # is now a list of Subtheme S3 objects.
  expect_type(ts$themes[[1]]$subthemes, "list")
  expect_s3_class(ts$themes[[1]]$subthemes[[1]], "Subtheme")
  # Denormalised back-compat field continues to flatten to a character vector.
  expect_equal(ts$themes[[1]]$codes_included, c("code1", "code2"))
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
  # codes_included is the denormalised character vector;
  # subthemes is a list of Subtheme S3 (always >= 1, virtual when no
  # AI clustering produced subthemes).
  expect_type(t$codes_included, "character")
  expect_type(t$keywords, "character")
  expect_type(t$subthemes, "list")
  expect_s3_class(t$subthemes[[1]], "Subtheme")
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

test_that("create_theme_set handles character vector subthemes (legacy)", {
  # A plain character vector of subtheme names with no per-subtheme
  # code mapping is degenerate input. The back-compat path drops the names
  # and wraps all theme codes in one virtual Subtheme — the alternative
  # (preserve names with no codes) creates orphan subthemes that render
  # awkwardly in reports. The clustering pass populates real subthemes
  # with codes.
  ts <- create_theme_set(list(
    list(name = "A", description = "D", codes_included = "c1",
         subthemes = c("sub1", "sub2"))
  ))
  expect_type(ts$themes[[1]]$subthemes, "list")
  expect_length(ts$themes[[1]]$subthemes, 1L)
  expect_s3_class(ts$themes[[1]]$subthemes[[1]], "Subtheme")
  expect_true(is.na(ts$themes[[1]]$subthemes[[1]]$name))
})

test_that("create_theme_set preserves Subtheme S3 input verbatim", {
  st <- create_subtheme(name = "S1", description = "D1",
                          codes = c("code_a", "code_b"))
  ts <- create_theme_set(list(
    list(name = "T", description = "D", subthemes = list(st))
  ))
  expect_s3_class(ts$themes[[1]]$subthemes[[1]], "Subtheme")
  expect_equal(ts$themes[[1]]$subthemes[[1]]$name, "S1")
  expect_equal(subtheme_code_names(ts$themes[[1]]$subthemes[[1]]),
               c("code_a", "code_b"))
  # Denormalised codes_included reflects the canonical hierarchy
  expect_equal(ts$themes[[1]]$codes_included, c("code_a", "code_b"))
})

test_that("create_code_object + create_subtheme build typed S3", {
  code <- create_code_object(key = "k1", name = "Code 1",
                                description = "desc",
                                type = "descriptive",
                                frequency = 7L,
                                entry_ids = c("e1", "e2"))
  expect_s3_class(code, "Code")
  expect_equal(code$key, "k1")
  expect_equal(code$frequency, 7L)

  sub <- create_subtheme(name = "S", description = "sd",
                          codes = list(code))
  expect_s3_class(sub, "Subtheme")
  expect_equal(subtheme_n_codes(sub), 1L)
  expect_equal(subtheme_code_keys(sub), "k1")
})

test_that("theme_codes / theme_code_objects walk the hierarchy", {
  ts <- create_theme_set(list(
    list(name = "T", description = "D",
         subthemes = list(
           create_subtheme(name = "S1", codes = c("a", "b")),
           create_subtheme(name = "S2", codes = c("c"))
         ))
  ))
  expect_equal(theme_codes(ts$themes[[1]]), c("a", "b", "c"))
  expect_equal(theme_code_keys(ts$themes[[1]]), c("a", "b", "c"))
  expect_length(theme_code_objects(ts$themes[[1]]), 3L)
  expect_equal(theme_n_subthemes(ts$themes[[1]]), 2L)
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
