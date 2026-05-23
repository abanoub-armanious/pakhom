# Phase 58 Tier 9 unit tests
#
# V-8  word-boundary-aware quote truncation with visible ellipsis
# L-15 timestamps in UTC across audit_log + quote_provenance
# L-21 code_style removed (dead) + include_in_vivo + min_text_length wired
# L-1  code_assignment provenance docstring documents the join
# (prior-tier deferrals)
# Tier 5 M-T7-1   prompt_template_version stamped in run_metadata.json
# Tier 8 MEDIUM-2 schema_version is FIRST field in audit log records

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


# ==========================================================================
# Tier 8 MEDIUM-2: schema_version is the FIRST field
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
# Tier 5 M-T7-1 (deferred): prompt_template_version in run_metadata
# ==========================================================================

test_that(".PROMPT_TEMPLATE_VERSION constant exists and is phase58_tier7", {
  expect_equal(pakhom:::.PROMPT_TEMPLATE_VERSION, "phase58_tier7")
})

test_that("init_run_state stamps prompt_template_version into run_metadata.json", {
  td <- withr::local_tempdir()
  meta <- init_run_state(
    run_dir = td,
    run_id = "test_run",
    methodology_mode = "codebook_collaborative"
  )
  expect_true("prompt_template_version" %in% names(meta))
  expect_equal(meta$prompt_template_version, "phase58_tier7")
  # Read back from disk to confirm persistence
  back <- jsonlite::fromJSON(file.path(td, "run_metadata.json"))
  expect_equal(back$prompt_template_version, "phase58_tier7")
})


# ==========================================================================
# L-21: config wiring audit -- code_style removed
# ==========================================================================

test_that("code_style is NOT in default_config.yaml (was dead since Phase 50e)", {
  yaml_path <- system.file("config", "default_config.yaml", package = "pakhom")
  if (!nzchar(yaml_path)) {
    yaml_path <- file.path("../../inst/config/default_config.yaml")
  }
  skip_if_not(file.exists(yaml_path), "default_config.yaml not on install path")
  yaml_text <- paste(readLines(yaml_path), collapse = "\n")
  # The line `code_style: "descriptive"` is gone (replaced with a comment)
  expect_false(grepl('code_style:\\s*"descriptive"', yaml_text, fixed = FALSE))
})
