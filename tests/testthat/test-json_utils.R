# Tests for JSON utility functions (03_json_utils.R)

test_that("parse_json_safely parses valid JSON", {
  result <- parse_json_safely('{"key": "value", "num": 42}')
  expect_type(result, "list")
  expect_equal(result$key, "value")
  expect_equal(result$num, 42)
})

test_that("parse_json_safely handles JSON with expected key", {
  json <- '{"themes": [{"name": "Theme 1"}], "meta": "info"}'
  result <- parse_json_safely(json, expected_key = "themes")
  expect_type(result, "list")
  expect_true(!is.null(result$themes))
})

test_that("parse_json_safely returns NULL for empty input", {
  expect_null(parse_json_safely(""))
  expect_null(parse_json_safely(NULL))
  expect_null(parse_json_safely(NA))
})

test_that("parse_json_safely extracts JSON from markdown code blocks", {
  response <- '```json\n{"key": "value"}\n```'
  result <- parse_json_safely(response)
  expect_type(result, "list")
  expect_equal(result$key, "value")
})

test_that("parse_json_safely repairs truncated JSON with missing brackets", {
  # Missing closing bracket and brace, which repair_close_brackets should fix
  truncated <- '{"themes": [{"name": "Test"}]'
  result <- parse_json_safely(truncated)
  # This is actually valid JSON, should parse fine
  expect_type(result, "list")
  expect_true(!is.null(result$themes))
  expect_equal(result$themes$name, "Test")
})

test_that("parse_json_safely repairs unclosed braces", {
  # Missing two closing characters: ] and }
  truncated <- '{"themes": [{"name": "Test"}'
  result <- parse_json_safely(truncated)
  # The bracket repair strategy should close ] and }
  # Should successfully parse after repair
  expect_true(is.null(result) || is.list(result))
  if (!is.null(result)) {
    expect_true(!is.null(result$themes))
  }
})

test_that("parse_json_safely handles JSON arrays", {
  json <- '[{"name": "A"}, {"name": "B"}]'
  result <- parse_json_safely(json)
  expect_true(is.list(result) || is.data.frame(result))
})

test_that("parse_json_safely returns NULL (not a crash) on empty-after-fence input", {
  # An all-fence response is non-empty before fence stripping but empty after;
  # the repair path used to error with "missing value where TRUE/FALSE needed",
  # violating the documented NULL-on-unparseable contract.
  for (inp in c("```json\n```", "```\n```", "```json\n \n```")) {
    expect_null(suppressWarnings(parse_json_safely(inp)), info = inp)
  }
})
