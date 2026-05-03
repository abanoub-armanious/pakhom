# Tests for structured output schemas (Sprint-4 T1.2)
# R/structured_schemas.R defines 6 task schemas + a .validate_schema()
# helper. The schemas are consumed by ai_complete(response_schema = ...)
# which enforces them server-side via OpenAI strict json_schema or
# Anthropic forced tool-use.

# ---- All six schemas validate as well-formed -------------------------------

test_that("all six task schemas pass .validate_schema", {
  schemas <- list(
    coding     = pakhom:::.coding_schema(),
    saturation = pakhom:::.saturation_schema(),
    sentiment  = pakhom:::.sentiment_schema(),
    theming    = pakhom:::.theming_schema(),
    insight    = pakhom:::.insight_schema(),
    synthesis  = pakhom:::.synthesis_schema()
  )
  for (nm in names(schemas)) {
    expect_silent(pakhom:::.validate_schema(schemas[[nm]]))
  }
})

# ---- Round-trip serialization: every schema serializes to valid JSON -------

test_that("schemas serialize via jsonlite without losing structure", {
  schemas <- list(
    pakhom:::.coding_schema(),
    pakhom:::.saturation_schema(),
    pakhom:::.sentiment_schema(),
    pakhom:::.theming_schema(),
    pakhom:::.insight_schema(),
    pakhom:::.synthesis_schema()
  )
  for (s in schemas) {
    json <- jsonlite::toJSON(s, auto_unbox = TRUE)
    parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    expect_equal(parsed$type, "object")
    expect_false(parsed$additionalProperties)
    # required must round-trip as a JSON array even if length 1 (the bug
    # this guards against: c("x") would auto-unbox to "x", breaking the
    # schema). All 6 task schemas use list() so this should be a JSON array.
    expect_type(parsed$required, "list")
    expect_true(length(parsed$required) >= 1)
  }
})

# ---- .validate_schema catches common mistakes ------------------------------

test_that(".validate_schema rejects missing additionalProperties = FALSE", {
  bad <- list(type = "object",
              required   = list("x"),
              properties = list(x = list(type = "string")))
  # Missing additionalProperties (treated as TRUE by JSON Schema default)
  expect_error(pakhom:::.validate_schema(bad), "additionalProperties")
})

test_that(".validate_schema rejects required as character vector (auto_unbox trap)", {
  # required = c("x") would auto-unbox to "x" under jsonlite, breaking
  # the schema. The validator catches this by requiring list().
  bad <- list(type = "object",
              additionalProperties = FALSE,
              required   = c("x"),
              properties = list(x = list(type = "string")))
  expect_error(pakhom:::.validate_schema(bad), "must be a list\\(\\)")
})

test_that(".validate_schema rejects optional fields (OpenAI strict mode)", {
  # OpenAI strict mode forbids optional fields: every property must be in
  # required. The validator enforces this.
  bad <- list(type = "object",
              additionalProperties = FALSE,
              required   = list("x"),
              properties = list(x = list(type = "string"),
                                y = list(type = "string")))
  expect_error(pakhom:::.validate_schema(bad), "not in required")
})

test_that(".validate_schema rejects required referencing unknown property", {
  bad <- list(type = "object",
              additionalProperties = FALSE,
              required   = list("x", "y"),
              properties = list(x = list(type = "string")))
  expect_error(pakhom:::.validate_schema(bad), "unknown properties")
})

test_that(".validate_schema rejects enum as character vector", {
  # Same auto_unbox trap as required.
  bad <- list(type = "object",
              additionalProperties = FALSE,
              required   = list("x"),
              properties = list(x = list(type = "string", enum = c("a", "b"))))
  expect_error(pakhom:::.validate_schema(bad), "enum must be a list")
})

test_that(".validate_schema recurses into nested objects and arrays", {
  # The nested object inside `coded_segments` items must satisfy the same
  # constraints. Insert a violation in a nested object and expect the path
  # in the error message.
  bad <- pakhom:::.coding_schema()
  bad$properties$coded_segments$items$additionalProperties <- TRUE
  expect_error(pakhom:::.validate_schema(bad),
               "coded_segments.items: additionalProperties")
})

# ---- Schema-specific shape assertions --------------------------------------

test_that(".coding_schema has the four code_type enum values", {
  s <- pakhom:::.coding_schema()
  enum <- s$properties$coded_segments$items$properties$code_type$enum
  expect_setequal(unlist(enum),
                  c("descriptive", "emotional", "process", "in_vivo"))
})

test_that(".sentiment_schema reflects the emotion_categories argument", {
  s <- pakhom:::.sentiment_schema(
    emotion_categories = c("joy", "sadness", "neutral")
  )
  enum <- s$properties$results$items$properties$emotions$items$enum
  expect_setequal(unlist(enum), c("joy", "sadness", "neutral"))
})

test_that(".theming_schema marks merge-only fields as nullable", {
  s <- pakhom:::.theming_schema()
  # merge_into / updated_label / updated_description are nullable because
  # they're meaningless when action = "standalone" but strict mode requires
  # all properties present.
  expect_true("null" %in% unlist(s$properties$merge_into$type))
  expect_true("null" %in% unlist(s$properties$updated_label$type))
  expect_true("null" %in% unlist(s$properties$updated_description$type))
  # rationale is required and non-null in both branches.
  expect_equal(s$properties$rationale$type, "string")
})

# ---- prompt_hash includes response_schema (T1.2 hash-key extension) -------

test_that(".compute_prompt_hash distinguishes requests by response_schema", {
  base <- pakhom:::.compute_prompt_hash(
    "p", "s", "gpt-4o", 0.3, 1000L, TRUE,
    response_schema = NULL
  )
  with_coding <- pakhom:::.compute_prompt_hash(
    "p", "s", "gpt-4o", 0.3, 1000L, TRUE,
    response_schema = pakhom:::.coding_schema()
  )
  with_sentiment <- pakhom:::.compute_prompt_hash(
    "p", "s", "gpt-4o", 0.3, 1000L, TRUE,
    response_schema = pakhom:::.sentiment_schema()
  )

  expect_false(identical(base, with_coding))
  expect_false(identical(base, with_sentiment))
  expect_false(identical(with_coding, with_sentiment))
})

test_that(".compute_prompt_hash is deterministic across schema instances", {
  # Calling the schema function multiple times produces identical R lists,
  # so the prompt_hash should be stable. Critical for replay_run cache hits
  # to work correctly across runs.
  h1 <- pakhom:::.compute_prompt_hash(
    "p", NULL, "gpt-4o", 0.3, 1000L, FALSE,
    response_schema = pakhom:::.coding_schema()
  )
  h2 <- pakhom:::.compute_prompt_hash(
    "p", NULL, "gpt-4o", 0.3, 1000L, FALSE,
    response_schema = pakhom:::.coding_schema()
  )
  expect_identical(h1, h2)
})

test_that(".compute_prompt_hash with NULL schema matches pre-T1.2 calls", {
  # Pre-T1.2 callers (no response_schema arg) get the same hash as T1.2
  # callers passing response_schema = NULL explicitly. NULL is the default
  # and is hashed as JSON null.
  h_implicit <- pakhom:::.compute_prompt_hash("p", "s", "gpt-4o", 0.3, 1000L, FALSE)
  h_explicit <- pakhom:::.compute_prompt_hash("p", "s", "gpt-4o", 0.3, 1000L, FALSE,
                                                    response_schema = NULL)
  expect_identical(h_implicit, h_explicit)
})
