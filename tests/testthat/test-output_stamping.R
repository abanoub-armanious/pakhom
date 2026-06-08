# Tests for output stamping (T1.7, R/output_stamping.R).
# Per AC4 (methodology stamped on every output), every artifact pakhom
# produces should carry the methodology declaration. The stamping API
# centralizes that so individual writers don't have to roll their own.

# ---- methodology_short_code ------------------------------------------------

test_that("methodology_short_code maps the three modes to M1/M2/M3", {
  expect_equal(methodology_short_code("reflexive_scaffold"),     "M1")
  expect_equal(methodology_short_code("codebook_collaborative"), "M2")
  expect_equal(methodology_short_code("framework_applied"),      "M3")
})

test_that("methodology_short_code returns 'M?' for unknown / NULL / NA / empty", {
  expect_equal(methodology_short_code(NULL),       "M?")
  expect_equal(methodology_short_code(NA),         "M?")
  expect_equal(methodology_short_code(""),         "M?")
  expect_equal(methodology_short_code("free_for_all"), "M?")
})

# ---- methodology_label -----------------------------------------------------

test_that("methodology_label produces human-readable Mode N labels", {
  expect_equal(methodology_label("reflexive_scaffold"),
               "M1 - Reflexive Scaffold")
  expect_equal(methodology_label("codebook_collaborative"),
               "M2 - Codebook Collaborative")
  expect_equal(methodology_label("framework_applied"),
               "M3 - Framework Applied")
})

test_that("methodology_label flags unknown modes visibly", {
  expect_equal(methodology_label(NULL), "Unknown methodology")
  expect_match(methodology_label("future_mode"), "Unknown methodology \\(future_mode\\)")
})

# ---- methodology_description_short -----------------------------------------

test_that("methodology_description_short returns a one-liner per mode", {
  expect_match(methodology_description_short("reflexive_scaffold"),
               "AI extractive only")
  expect_match(methodology_description_short("codebook_collaborative"),
               "AI proposes codes")
  expect_match(methodology_description_short("framework_applied"),
               "framework verbatim")
})

test_that("methodology_description_short tags missing/unknown modes", {
  expect_equal(methodology_description_short(NULL),
               "Methodology not declared.")
  expect_equal(methodology_description_short("free_for_all"),
               "Methodology not declared.")
})

# ---- run_id_with_mode ------------------------------------------------------

test_that("run_id_with_mode appends mode short-code suffix", {
  expect_equal(run_id_with_mode("run_2026-05-03_103415", "reflexive_scaffold"),
               "run_2026-05-03_103415_M1")
  expect_equal(run_id_with_mode("run_2026-05-03_103415", "framework_applied"),
               "run_2026-05-03_103415_M3")
  expect_equal(run_id_with_mode("run_2026-05-03_103415", NULL),
               "run_2026-05-03_103415_M?")
})

# ---- stamp_methodology_html ------------------------------------------------

test_that("stamp_methodology_html renders the badge with mode + description + run id", {
  html <- stamp_methodology_html("reflexive_scaffold", run_id = "run_test")
  expect_match(html, "methodology-stamp")
  expect_match(html, "M1 - Reflexive Scaffold")
  expect_match(html, "AI extractive only")
  expect_match(html, "run_test")
})

test_that("stamp_methodology_html omits the run id when not supplied", {
  html <- stamp_methodology_html("reflexive_scaffold", run_id = NULL)
  expect_false(grepl("methodology-run-id", html, fixed = TRUE) &&
               grepl("run [^&]", html))
})

test_that("stamp_methodology_html escapes HTML-special characters in run_id", {
  html <- stamp_methodology_html("reflexive_scaffold",
                                   run_id = "<script>alert(1)</script>")
  expect_false(grepl("<script>", html, fixed = TRUE))
  expect_match(html, "&lt;script&gt;")
})

# ---- methodology_csv_header_lines + stamp_methodology_csv ------------------

test_that("methodology_csv_header_lines builds the comment-style header", {
  lines <- methodology_csv_header_lines("reflexive_scaffold", run_id = "r1")
  expect_length(lines, 2L)  # header + separator
  expect_match(lines[1], "^# methodology: M1 - Reflexive Scaffold")
  expect_match(lines[1], "run: r1")
  expect_equal(lines[2], "#")
})

test_that("stamp_methodology_csv prepends the comment header to a real file", {
  d <- withr::local_tempdir()
  csv <- file.path(d, "data.csv")
  writeLines(c("col1,col2", "a,1", "b,2"), csv)

  stamp_methodology_csv(csv, "reflexive_scaffold", run_id = "r1")
  body <- readLines(csv)
  expect_equal(body[1], "# methodology: M1 - Reflexive Scaffold | run: r1")
  expect_equal(body[2], "#")
  expect_equal(body[3], "col1,col2")
  expect_equal(body[4], "a,1")

  # Idempotent: re-stamping doesn't double-prepend
  stamp_methodology_csv(csv, "reflexive_scaffold", run_id = "r1")
  body2 <- readLines(csv)
  expect_equal(length(body2), length(body))
  expect_equal(body2[1], body[1])
})

test_that("stamp_methodology_csv is read-roundtrip-safe with comment-aware parsers", {
  d <- withr::local_tempdir()
  csv <- file.path(d, "data.csv")
  writeLines(c("col1,col2", "a,1", "b,2"), csv)
  stamp_methodology_csv(csv, "reflexive_scaffold")
  # readr::read_csv with comment = "#" strips the stamp transparently
  parsed <- readr::read_csv(csv, comment = "#", show_col_types = FALSE)
  expect_equal(nrow(parsed), 2L)
  expect_equal(parsed$col1, c("a", "b"))
})

test_that("stamp_methodology_csv silently no-ops when file doesn't exist", {
  d <- withr::local_tempdir()
  expect_silent(invisible(stamp_methodology_csv(
    file.path(d, "absent.csv"), "reflexive_scaffold")))
})

# ---- stamp_methodology_console + methodology_plot_caption ------------------

test_that("stamp_methodology_console returns the banner with mode and run id", {
  expect_match(stamp_methodology_console("reflexive_scaffold", "r1"),
               "\\[methodology: M1 - Reflexive Scaffold\\]")
  expect_match(stamp_methodology_console("reflexive_scaffold", "r1"),
               "\\[run: r1\\]")
})

test_that("methodology_plot_caption is a one-liner ggplot caption", {
  cap <- methodology_plot_caption("reflexive_scaffold", "r1")
  expect_match(cap, "pakhom M1 - Reflexive Scaffold - run r1")
  cap2 <- methodology_plot_caption("framework_applied")
  expect_match(cap2, "^pakhom M3 - Framework Applied$")
})

# ---- AC4 contract test: stampers don't return empty / no-op silently --------

test_that("AC4 enforcement: every stamper returns a non-empty stamp for every valid mode", {
  # Per AC4, ABSENCE of the methodology stamp is itself a transparency
  # failure. The stampers must produce a visible artifact for every
  # legitimate mode; regressions that turn one of them into a no-op
  # should fail this test.
  for (mode in c("reflexive_scaffold", "codebook_collaborative",
                  "framework_applied")) {
    expect_true(nzchar(methodology_short_code(mode)),
                info = sprintf("short_code(%s) must not be empty", mode))
    expect_true(nzchar(methodology_label(mode)),
                info = sprintf("label(%s) must not be empty", mode))
    expect_true(nzchar(methodology_description_short(mode)),
                info = sprintf("description_short(%s) must not be empty", mode))
    expect_true(nzchar(stamp_methodology_html(mode)),
                info = sprintf("stamp_methodology_html(%s) must not be empty", mode))
    expect_true(nzchar(stamp_methodology_console(mode)),
                info = sprintf("stamp_methodology_console(%s) must not be empty", mode))
    expect_true(nzchar(methodology_plot_caption(mode)),
                info = sprintf("plot_caption(%s) must not be empty", mode))
  }
})
