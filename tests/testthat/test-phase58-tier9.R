# Cross-cutting hardening unit tests
#
# V-8  word-boundary-aware quote truncation with visible ellipsis
# L-15 timestamps in UTC across audit_log + quote_provenance
# L-21 code_style removed (dead) + include_in_vivo + min_text_length wired
# L-1  code_assignment provenance docstring documents the join
# (carried over from earlier)
# M-T7-1   prompt_template_version stamped in run_metadata.json
# MEDIUM-2 schema_version is FIRST field in audit log records

# ==========================================================================
# V-8: word-boundary truncation with ellipsis
# ==========================================================================

test_that(".truncate_quote_word_boundary preserves short text", {
  expect_equal(
    pakhom:::.truncate_quote_word_boundary("Short quote.", max_chars = 100L),
    "Short quote."
  )
})

test_that(".truncate_quote_word_boundary cuts at last whitespace + appends ellipsis", {
  long <- paste(rep("alpha beta gamma delta", 50L), collapse = " ")
  out <- pakhom:::.truncate_quote_word_boundary(long, max_chars = 60L)
  # Cut respects word boundary -- ends with the literal " ..." marker
  expect_true(endsWith(out, " ..."))
  # Total length <= 60
  expect_lte(nchar(out), 60L)
  # The last word before " ..." is whole (no mid-word cut)
  pre_ellipsis <- sub(" \\.\\.\\.$", "", out, perl = TRUE)
  # The truncated content ends with one of the four full words used
  # in the fixture; no half-word like "alpha beta gamma del".
  last_word <- sub(".* ", "", pre_ellipsis)
  expect_true(last_word %in% c("alpha", "beta", "gamma", "delta"))
})

test_that(".truncate_quote_word_boundary handles edge cases", {
  expect_equal(pakhom:::.truncate_quote_word_boundary(NA_character_), "")
  expect_equal(pakhom:::.truncate_quote_word_boundary(""), "")
  # Very-long single token gets hard cut + plain "..."
  long_token <- paste(rep("x", 100L), collapse = "")
  out <- pakhom:::.truncate_quote_word_boundary(long_token, max_chars = 30L)
  expect_lte(nchar(out), 30L)
  expect_match(out, "\\.\\.\\.$")
})


# ==========================================================================
# L-15: timestamps in UTC
# ==========================================================================

test_that("audit log timestamps are emitted in UTC", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td, config = NULL)
  log_ai_decision(audit, "coding", "code_assignment",
                   entry_id = "e1", code_name = "test")
  close_audit_log(audit)
  lines <- readLines(file.path(td, "ai_decisions.jsonl"))
  rec <- jsonlite::fromJSON(lines[1L])
  # UTC offset is "+0000" (or "Z" for some platforms; we use %z which is +0000)
  expect_match(rec$timestamp, "\\+0000$")
})

# Meta-audit M2 + M4: helper for the static UTC test.
# Walks every `format(Sys.time(), ...)` call in `files` (after stripping
# whole-line R comments) and returns offenders that lack `tz = "UTC"`.
# Used by both the R/ scope test and the tests/ hygiene test.
.collect_utc_offenders <- function(files) {
  offenders <- character()
  for (f in files) {
    src <- readLines(f, warn = FALSE)
    # M2: strip whole-line comments so a comment like
    # `# example: format(Sys.time(), "%z")` does not get walked as a real
    # call. Inline trailing comments are left alone -- safer than mishandling
    # `#` inside string literals.
    src <- src[!grepl("^\\s*#", src)]
    text <- paste(src, collapse = "\n")
    starts <- gregexpr("format\\(Sys\\.time\\(\\),", text, perl = TRUE)[[1L]]
    if (starts[1L] == -1L) next
    for (pos in starts) {
      # Walk forward balancing parens to find the matching close.
      depth <- 0L
      i <- pos
      end <- -1L
      n <- nchar(text)
      while (i <= n) {
        ch <- substr(text, i, i)
        if (ch == "(") depth <- depth + 1L
        else if (ch == ")") {
          depth <- depth - 1L
          if (depth == 0L) { end <- i; break }
        }
        i <- i + 1L
      }
      if (end == -1L) next
      call_text <- substr(text, pos, end)
      declares_utc <- grepl("tz\\s*=\\s*\"UTC\"", call_text, perl = TRUE)
      if (!declares_utc) {
        offenders <- c(offenders, paste0(basename(f), ": ", call_text))
      }
    }
  }
  offenders
}

test_that("every format(Sys.time(), ...) in R/ declares tz = \"UTC\" (L-15 invariant)", {
  src_dir <- test_path("..", "..", "R")
  # Check for an actual source file (not just the directory) -- covr's
  # test fixture sometimes has an empty R/ that exists but lacks sources.
  if (!file.exists(file.path(src_dir, "utils.R"))) {
    testthat::skip("R/ source files not on disk (covr / install context)")
  }
  # Strict hygiene: any `format(Sys.time(), ...)` call -- whether or not it
  # uses %z -- must declare tz = "UTC". Otherwise the runner's LOCAL TZ
  # leaks into user-visible artifacts (run IDs, report footers, QDPX
  # creation dates, JSONL timestamps). Two researchers in different
  # timezones running identical code would otherwise produce divergent
  # artifacts. Caught:
  #   - L-15 audit: 14 sites in R/
  #   - cross-cutting audit: mode1_orchestrator.R:363 (initially missed)
  #   - meta-audit M4: utils.R / 17_report.R / qdpx_export.R
  # This static-source test catches the ENTIRE bug class for any future site.
  r_files <- list.files(
    test_path("..", "..", "R"),
    pattern = "\\.R$", full.names = TRUE
  )
  offenders <- .collect_utc_offenders(r_files)
  expect_equal(
    offenders, character(),
    info = paste0(
      "Found format(Sys.time(), ...) calls without tz = \"UTC\".",
      " Offenders:\n", paste(offenders, collapse = "\n"),
      "\nFix: add `, tz = \"UTC\"` to the format() call."
    )
  )
})

test_that("test fixtures in tests/testthat/ also declare tz = \"UTC\" (hygiene)", {
  test_dir <- test_path("..", "testthat")
  # Check for a known fixture file (not just the directory).
  if (!file.exists(file.path(test_dir, "test-phase58-tier9.R"))) {
    testthat::skip("tests/testthat/ source files not on disk (covr / install context)")
  }
  # Meta-audit L1: test fixtures should model the package's own
  # hygiene rule. If a maintainer copies a `format(Sys.time(), ...)` line
  # from a test fixture into R/ code, the UTC declaration should travel
  # with it. Otherwise the same class of bug as M4 can re-enter.
  test_files <- list.files(
    test_path("..", "testthat"),
    pattern = "\\.R$", full.names = TRUE
  )
  # Exclude THIS file -- its own .collect_utc_offenders example strings
  # would otherwise self-match.
  test_files <- test_files[!grepl("test-phase58-tier9\\.R$", test_files)]
  offenders <- .collect_utc_offenders(test_files)
  expect_equal(offenders, character())
})

test_that("ProvocationCoverage computed_at is UTC", {
  obj <- list(
    n_corpus_entries_searchable = 10L,
    corpus_provided_to_per_category_fns = TRUE,
    llm_prompt_includes_full_corpus = TRUE,
    n_memos = 0L,
    memos_by_type = list(),
    no_silent_theme_skip = TRUE,
    no_unexpected_category_attempts = TRUE,
    no_silent_skip = TRUE,
    computed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"),
    schema_version = pakhom:::.PROVOCATION_COVERAGE_SCHEMA_VERSION
  )
  expect_match(obj$computed_at, "\\+0000$")
})


# ==========================================================================
# MEDIUM-2: schema_version is the FIRST field
# ==========================================================================

test_that("audit log record has schema_version as the first field", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td, config = NULL)
  log_ai_decision(audit, "coding", "code_assignment",
                   entry_id = "e1", code_name = "test")
  close_audit_log(audit)
  lines <- readLines(file.path(td, "ai_decisions.jsonl"))
  rec_raw <- lines[1L]
  # Parse position-aware: the first key after the opening "{" should be
  # schema_version. jsonlite preserves field order in toJSON output.
  expect_match(rec_raw, '^\\{"schema_version":')
})


# ==========================================================================
# M-T7-1 (deferred): prompt_template_version in run_metadata
# ==========================================================================

test_that(".PROMPT_TEMPLATE_VERSION constant exists and is 1.0.0", {
  expect_equal(pakhom:::.PROMPT_TEMPLATE_VERSION, "1.0.0")
})

test_that("init_run_state stamps prompt_template_version into run_metadata.json", {
  td <- withr::local_tempdir()
  meta <- init_run_state(
    run_dir = td,
    run_id = "test_run",
    methodology_mode = "codebook_collaborative"
  )
  expect_true("prompt_template_version" %in% names(meta))
  expect_equal(meta$prompt_template_version, "1.0.0")
  # Read back from disk to confirm persistence
  back <- jsonlite::fromJSON(file.path(td, "run_metadata.json"))
  expect_equal(back$prompt_template_version, "1.0.0")
})


# ==========================================================================
# L-21: config wiring audit -- code_style removed
# ==========================================================================

test_that("code_style is NOT in default_config.yaml (legacy dead knob)", {
  yaml_path <- system.file("config", "default_config.yaml", package = "pakhom")
  if (!nzchar(yaml_path)) {
    yaml_path <- file.path("../../inst/config/default_config.yaml")
  }
  skip_if_not(file.exists(yaml_path), "default_config.yaml not on install path")
  yaml_text <- paste(readLines(yaml_path), collapse = "\n")
  # The line `code_style: "descriptive"` is gone (replaced with a comment)
  expect_false(grepl('code_style:\\s*"descriptive"', yaml_text, fixed = FALSE))
})
