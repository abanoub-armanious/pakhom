# Tests for AI provider abstraction (02_ai_providers.R)

test_that("create_ai_provider returns AIProvider for openai", {
  withr::local_envvar(OPENAI_API_KEY = "sk-test-fake-key-for-unit-tests")

  provider <- create_ai_provider("openai")
  expect_s3_class(provider, "AIProvider")
  expect_equal(provider$provider, "openai")
  expect_equal(provider$key_env$key, "sk-test-fake-key-for-unit-tests")
  expect_equal(provider$models$primary, "gpt-4o")
  expect_equal(provider$models$fast, "gpt-4o-mini")
})

test_that("create_ai_provider returns AIProvider for anthropic", {
  withr::local_envvar(ANTHROPIC_API_KEY = "sk-ant-test-fake-key")

  provider <- create_ai_provider("anthropic")
  expect_s3_class(provider, "AIProvider")
  expect_equal(provider$provider, "anthropic")
  expect_equal(provider$key_env$key, "sk-ant-test-fake-key")
})

test_that("create_ai_provider errors on missing API key", {
  withr::local_envvar(OPENAI_API_KEY = "")

  expect_error(
    create_ai_provider("openai"),
    "API key not found"
  )
})

test_that("create_ai_provider errors on invalid provider", {
  expect_error(
    create_ai_provider("invalid_provider"),
    "provider"
  )
})

test_that("create_ai_provider uses config api_key over env var", {
  withr::local_envvar(OPENAI_API_KEY = "env-key")

  config <- list(openai = list(api_key = "config-key"))
  provider <- create_ai_provider("openai", config)
  expect_equal(provider$key_env$key, "config-key")
})

test_that("reasoning model detection works", {
  # .is_reasoning_model is internal, test via namespace
  is_reasoning <- pakhom:::.is_reasoning_model
  expect_true(is_reasoning("o1-preview"))
  expect_true(is_reasoning("o1-mini"))
  expect_true(is_reasoning("o3-mini"))
  expect_true(is_reasoning("o4-mini"))
  expect_false(is_reasoning("gpt-4o"))
  expect_false(is_reasoning("gpt-4o-mini"))
  expect_false(is_reasoning("claude-sonnet-4-20250514"))
})

test_that("task-specific temperature and max_tokens resolve correctly", {
  withr::local_envvar(OPENAI_API_KEY = "sk-test-key")

  config <- list(
    openai = list(api_key = "sk-test-key"),
    max_tokens = list(coding = 3000, sentiment = 1500),
    temperature = list(coding = 0.2, sentiment = 0.1)
  )
  provider <- create_ai_provider("openai", config)
  expect_equal(provider$max_tokens$coding, 3000)
  expect_equal(provider$max_tokens$sentiment, 1500)
  expect_equal(provider$temperature$coding, 0.2)
  expect_equal(provider$temperature$sentiment, 0.1)
})

test_that("default rate limits are set per provider", {
  withr::local_envvar(OPENAI_API_KEY = "sk-test", ANTHROPIC_API_KEY = "sk-ant-test")

  openai <- create_ai_provider("openai")
  anthropic <- create_ai_provider("anthropic")

  expect_equal(openai$rate_limits$batch_size, 20)
  expect_equal(anthropic$rate_limits$batch_size, 10)
  expect_true(openai$rate_limits$requests_per_minute > anthropic$rate_limits$requests_per_minute)
})

test_that("print.AIProvider works without error", {
  withr::local_envvar(OPENAI_API_KEY = "sk-test-key")

  provider <- create_ai_provider("openai")
  expect_output(print(provider), "AIProvider")
  expect_output(print(provider), "gpt-4o")
})

test_that("create_ai_provider initializes context_window", {
  withr::local_envvar(OPENAI_API_KEY = "sk-test-key")

  p <- create_ai_provider("openai")
  expect_true(!is.null(p$context_window))
  expect_true(is.integer(p$context_window) || is.numeric(p$context_window))
  expect_equal(p$context_window, 128000L)
})

# ==============================================================================
# Sprint-4 T1.1: Structured return helpers for ai_complete()
# ==============================================================================
# .compute_prompt_hash, .normalize_usage_openai, .normalize_usage_anthropic,
# .normalize_anthropic_finish are internal helpers that shape the structured
# list returned by ai_complete(). Tests live here (not in a separate file)
# because they test internal helpers of the AI provider module.

test_that(".compute_prompt_hash returns a 64-char SHA-256 hex digest", {
  h <- pakhom:::.compute_prompt_hash(
    "hello", "system msg", "gpt-4o", 0.3, 1000L, FALSE
  )
  expect_type(h, "character")
  expect_equal(nchar(h), 64L)
  expect_match(h, "^[0-9a-f]{64}$")
})

test_that(".compute_prompt_hash is deterministic for identical inputs", {
  h1 <- pakhom:::.compute_prompt_hash(
    "same prompt", "same system", "gpt-4o", 0.3, 1000L, FALSE
  )
  h2 <- pakhom:::.compute_prompt_hash(
    "same prompt", "same system", "gpt-4o", 0.3, 1000L, FALSE
  )
  expect_identical(h1, h2)
})

test_that(".compute_prompt_hash treats NULL system_prompt as empty string", {
  # NULL system prompts should produce the same hash as explicit "" so that
  # callers that omit the arg vs pass "" don't break the cache.
  h_null  <- pakhom:::.compute_prompt_hash(
    "p", NULL, "gpt-4o", 0.3, 1000L, FALSE
  )
  h_empty <- pakhom:::.compute_prompt_hash(
    "p", "",   "gpt-4o", 0.3, 1000L, FALSE
  )
  expect_identical(h_null, h_empty)
})

test_that(".compute_prompt_hash distinguishes prompt, model, temperature, max_tokens, json_mode", {
  base <- pakhom:::.compute_prompt_hash(
    "prompt A", "system", "gpt-4o", 0.3, 1000L, FALSE
  )
  expect_false(identical(base, pakhom:::.compute_prompt_hash(
    "prompt B", "system", "gpt-4o", 0.3, 1000L, FALSE)))
  expect_false(identical(base, pakhom:::.compute_prompt_hash(
    "prompt A", "different system", "gpt-4o", 0.3, 1000L, FALSE)))
  expect_false(identical(base, pakhom:::.compute_prompt_hash(
    "prompt A", "system", "claude-sonnet-4-20250514", 0.3, 1000L, FALSE)))
  expect_false(identical(base, pakhom:::.compute_prompt_hash(
    "prompt A", "system", "gpt-4o", 0.7, 1000L, FALSE)))
  expect_false(identical(base, pakhom:::.compute_prompt_hash(
    "prompt A", "system", "gpt-4o", 0.3, 2000L, FALSE)))
  expect_false(identical(base, pakhom:::.compute_prompt_hash(
    "prompt A", "system", "gpt-4o", 0.3, 1000L, TRUE)))
})

test_that(".normalize_usage_openai extracts standard payload", {
  usage <- list(prompt_tokens = 120L, completion_tokens = 35L,
                total_tokens = 155L)
  result <- pakhom:::.normalize_usage_openai(usage)
  expect_equal(result$prompt_tokens, 120L)
  expect_equal(result$completion_tokens, 35L)
  expect_equal(result$total_tokens, 155L)
  expect_type(result$prompt_tokens, "integer")
})

test_that(".normalize_usage_openai returns NA_integer_ on NULL or missing fields", {
  result_null <- pakhom:::.normalize_usage_openai(NULL)
  expect_identical(result_null$prompt_tokens, NA_integer_)
  expect_identical(result_null$completion_tokens, NA_integer_)
  expect_identical(result_null$total_tokens, NA_integer_)

  # Partial payload (missing total_tokens)
  result_partial <- pakhom:::.normalize_usage_openai(
    list(prompt_tokens = 50L, completion_tokens = 10L)
  )
  expect_equal(result_partial$prompt_tokens, 50L)
  expect_equal(result_partial$completion_tokens, 10L)
  expect_identical(result_partial$total_tokens, NA_integer_)
})

test_that(".normalize_usage_anthropic remaps input/output to OpenAI-style names and computes total", {
  usage <- list(input_tokens = 200L, output_tokens = 50L)
  result <- pakhom:::.normalize_usage_anthropic(usage)
  expect_equal(result$prompt_tokens, 200L)      # input -> prompt
  expect_equal(result$completion_tokens, 50L)   # output -> completion
  expect_equal(result$total_tokens, 250L)       # computed
  expect_type(result$total_tokens, "integer")
})

test_that(".normalize_usage_anthropic propagates NA when either count is missing", {
  result_null <- pakhom:::.normalize_usage_anthropic(NULL)
  expect_identical(result_null$prompt_tokens, NA_integer_)
  expect_identical(result_null$completion_tokens, NA_integer_)
  expect_identical(result_null$total_tokens, NA_integer_)

  # Output missing -> total should be NA
  result_partial <- pakhom:::.normalize_usage_anthropic(
    list(input_tokens = 100L)
  )
  expect_equal(result_partial$prompt_tokens, 100L)
  expect_identical(result_partial$completion_tokens, NA_integer_)
  expect_identical(result_partial$total_tokens, NA_integer_)
})

test_that(".normalize_anthropic_finish maps stop_reason to canonical finish_reason", {
  expect_equal(pakhom:::.normalize_anthropic_finish("end_turn"),      "stop")
  expect_equal(pakhom:::.normalize_anthropic_finish("max_tokens"),    "length")
  expect_equal(pakhom:::.normalize_anthropic_finish("stop_sequence"), "stop")
  expect_equal(pakhom:::.normalize_anthropic_finish("tool_use"),      "tool_use")
  # Unknown values pass through unchanged for forward compatibility
  expect_equal(pakhom:::.normalize_anthropic_finish("future_value"),  "future_value")
})
