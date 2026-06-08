# Coverage followup: branch tests for the 2 next-lowest-
# coverage user-facing files surfaced by covr:
#   - R/methodology_decision_aid.R (50% pre-followup)
#   - R/03_json_utils.R (41% pre-followup)
# This file adds the missing branch + error-path tests without duplicating
# the existing happy-path tests in test-config.R and test-json_utils.R.

# ==========================================================================
# methodology_decision_aid: print-only path + error paths
# ==========================================================================

test_that("methodology_decision_aid prints comparison + returns NULL when no args (T1.3 print-only)", {
  # The print-only branch: interactive = FALSE AND every criterion NULL.
  # Sanity: prints to stdout and returns NULL invisibly.
  out <- capture.output({
    res <- methodology_decision_aid(
      interactive = FALSE,
      ta_family = NULL,
      has_apriori_framework = NULL,
      wants_irr = NULL
    )
  })
  expect_null(res)
  combined <- paste(out, collapse = "\n")
  # Comparison surfaces all three modes by name
  expect_match(combined, "reflexive_scaffold", fixed = TRUE)
  expect_match(combined, "codebook_collaborative", fixed = TRUE)
  expect_match(combined, "framework_applied", fixed = TRUE)
  # AC3 (no default mode) is the rationale for this helper
  expect_match(combined, "No default", fixed = TRUE)
})

test_that("methodology_decision_aid errors when ta_family missing in non-interactive mode (other criteria supplied)", {
  # Non-interactive but some-but-not-all criteria supplied -> falls through
  # to .recommend_methodology_mode, which requires ta_family.
  expect_error(
    methodology_decision_aid(
      interactive = FALSE,
      ta_family = NULL,
      has_apriori_framework = FALSE,
      wants_irr = NULL
    ),
    "ta_family"
  )
})

test_that("methodology_decision_aid errors on invalid ta_family (match.arg gate)", {
  expect_error(
    methodology_decision_aid(
      interactive = FALSE,
      ta_family = "nonexistent_family",
      has_apriori_framework = TRUE,
      wants_irr = FALSE
    ),
    "should be one of"
  )
})

test_that("methodology_decision_aid surfaces methodology-incongruence advice when reflexive + IRR (alternative populated)", {
  rec <- methodology_decision_aid(
    interactive = FALSE,
    ta_family = "reflexive",
    has_apriori_framework = FALSE,
    wants_irr = TRUE
  )
  # The IRR + reflexive mismatch is the canonical T1.3 example of "scaffold
  # the choice itself" -- the recommendation is codebook_collaborative
  # BUT the alternative (reflexive_scaffold) is surfaced for the user.
  expect_equal(rec$recommended_mode, "codebook_collaborative")
  expect_equal(rec$alternative, "reflexive_scaffold")
  expect_match(rec$reasoning, "RTARG|Braun")
})

test_that("methodology_decision_aid recommends framework_applied for content with positivist alternative surfaced", {
  rec <- methodology_decision_aid(
    interactive = FALSE,
    ta_family = "content",
    has_apriori_framework = TRUE,
    wants_irr = TRUE
  )
  expect_equal(rec$recommended_mode, "framework_applied")
  # Content analysis: the helper notes the constructionist alternative.
  expect_equal(rec$alternative, "codebook_collaborative")
  expect_match(rec$reasoning, "Mayring")
})


# ==========================================================================
# .repair_close_brackets: bracket-balancing edge cases
# ==========================================================================

test_that(".repair_close_brackets is a no-op on well-formed JSON", {
  ok <- '{"a": 1, "b": [2, 3]}'
  expect_equal(pakhom:::.repair_close_brackets(ok), ok)
})

test_that(".repair_close_brackets respects string contexts (brackets inside strings not counted)", {
  # Brackets inside string literals must NOT be balanced -- they're just
  # part of the string content.
  s <- '{"text": "this has [brackets] and {braces}", "n": 1}'
  expect_equal(pakhom:::.repair_close_brackets(s), s)
})

test_that(".repair_close_brackets respects escape sequences inside strings", {
  # \\ doesn't escape the closing quote that follows it (only the second \\
  # does). The bracket walker must skip the escape pair correctly.
  s <- '{"escaped_brace": "\\\\}", "ok": 1}'
  expect_equal(pakhom:::.repair_close_brackets(s), s)
})

test_that(".repair_close_brackets closes a truncated object", {
  truncated <- '{"a": 1, "b": [2, 3'
  result <- pakhom:::.repair_close_brackets(truncated)
  expect_match(result, "\\]\\}$", perl = TRUE)
})


# ==========================================================================
# .repair_truncated_element: truncate-to-last-safe-point
# ==========================================================================

test_that(".repair_truncated_element returns original when no safe point found", {
  # < 10 chars or no safe truncation point -> return original unchanged.
  short <- '{"a"'
  expect_equal(pakhom:::.repair_truncated_element(short), short)
})

test_that(".repair_truncated_element cuts at last complete element of a truncated array", {
  # A truncated array like [{a},{b},{c -- the helper should truncate to
  # [{a},{b}] (or [{a},{b}] equivalent after .repair_close_brackets).
  truncated <- '[{"id":1},{"id":2},{"id":3'
  out <- pakhom:::.repair_truncated_element(truncated)
  # Result must parse as valid JSON (the whole point of the repair).
  parsed <- tryCatch(jsonlite::fromJSON(out), error = function(e) NULL)
  expect_false(is.null(parsed))
  # And include at least the first complete element.
  expect_true(length(parsed) >= 1L)
})


# ==========================================================================
# .repair_find_valid_subset: extract complete top-level object
# ==========================================================================

test_that(".repair_find_valid_subset finds the embedded valid JSON in surrounding prose", {
  surrounded <- 'Here is some text. {"key": "value", "n": 42} And more text after.'
  out <- pakhom:::.repair_find_valid_subset(surrounded)
  parsed <- jsonlite::fromJSON(out)
  expect_equal(parsed$key, "value")
  expect_equal(parsed$n, 42)
})

test_that(".repair_find_valid_subset returns original when no opening brace", {
  no_brace <- "just plain text with no braces"
  expect_equal(pakhom:::.repair_find_valid_subset(no_brace), no_brace)
})

test_that(".repair_find_valid_subset handles nested objects correctly (matches braces with depth)", {
  nested <- '{"outer": {"inner": "value", "list": [1,2,3]}, "top": true}'
  out <- pakhom:::.repair_find_valid_subset(nested)
  # Should extract the whole object (depth returns to 0 only at the outer })
  parsed <- jsonlite::fromJSON(out)
  expect_equal(parsed$outer$inner, "value")
  expect_true(parsed$top)
})


# ==========================================================================
# parse_json_safely: edge cases not covered by happy-path tests
# ==========================================================================

test_that("parse_json_safely returns NULL on NA input", {
  expect_null(parse_json_safely(NA_character_))
})

test_that("parse_json_safely returns NULL on whitespace-only input", {
  expect_null(parse_json_safely("   \n\t  "))
})

test_that("parse_json_safely warns + uses first element when given a vector", {
  out <- parse_json_safely(c('{"a": 1}', '{"b": 2}'))
  expect_equal(out$a, 1)
  expect_null(out$b)
})

test_that("parse_json_safely returns NULL when expected_key is missing", {
  # Strategy 0 parses but expected_key absent -> .try_parse returns NULL,
  # then repair strategies try and also fail to add the key.
  expect_null(parse_json_safely('{"x": 1}', expected_key = "themes"))
})

test_that("parse_json_safely respects max_repair_attempts = 0 (no repairs)", {
  # Garbage JSON, no repair attempts -> NULL immediately after Strategy 0.
  expect_null(parse_json_safely("not json at all {{}}", max_repair_attempts = 0))
})

test_that("parse_json_safely recovers via Strategy 3 (find valid subset) on embedded JSON", {
  # Wrapped in conversational prose. Strategy 0-2 fail; Strategy 3 extracts.
  noisy <- 'Sure, here is the analysis: {"theme": "Adherence", "n": 5} -- hope this helps!'
  out <- parse_json_safely(noisy)
  expect_equal(out$theme, "Adherence")
  expect_equal(out$n, 5)
})

test_that("parse_json_safely returns NULL when all repair strategies exhausted", {
  # Completely unrecoverable input.
  expect_null(parse_json_safely("this is plain prose with no json structure at all"))
})

test_that("parse_json_safely strips ```json fences (markdown code-block wrapper)", {
  fenced <- "```json\n{\"a\": 1, \"b\": [2, 3]}\n```"
  out <- parse_json_safely(fenced)
  expect_equal(out$a, 1)
  expect_equal(out$b, c(2L, 3L))
})

test_that("parse_json_safely strips generic ``` fences (no language tag)", {
  fenced <- "```\n{\"x\": \"y\"}\n```"
  out <- parse_json_safely(fenced)
  expect_equal(out$x, "y")
})
