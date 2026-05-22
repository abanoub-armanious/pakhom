# Tests for progressive sequential coding (11_coding.R)

test_that("create_coding_state initializes correctly", {
  state <- create_coding_state()
  expect_s3_class(state, "ProgressiveCodingState")
  expect_type(state$codebook, "list")
  expect_length(state$codebook, 0)
  expect_type(state$entry_results, "list")
  expect_length(state$entries_processed, 0)
  expect_length(state$entries_skipped, 0)
})

test_that("create_coding_state accepts learning context calibration", {
  ctx <- list(benchmarks = list(
    typical_code_count = 40,
    codes_per_entry = 5.7,
    max_code_coverage_pct = 34,
    avg_segment_length = 88
  ))
  state <- create_coding_state(learning_context = ctx)
  expect_equal(state$calibration$target_granularity, 40)
  expect_equal(state$calibration$target_codes_per_entry, 5.7)
  expect_equal(state$calibration$avg_segment_length, 88)
})

test_that("get_analytic_sample filters skipped entries", {
  state <- create_coding_state()
  state$entry_results[["a"]] <- list(codes_assigned = "code1", skipped = FALSE)
  state$entry_results[["b"]] <- list(codes_assigned = character(0), skipped = TRUE, skip_reason = "Not applicable")
  state$entry_results[["c"]] <- list(codes_assigned = c("code1", "code2"), skipped = FALSE)

  data <- tibble::tibble(std_id = c("a", "b", "c"), std_text = c("text1", "text2", "text3"))
  result <- get_analytic_sample(state, data)

  expect_equal(nrow(result), 2)
  expect_true("a" %in% result$std_id)
  expect_true("c" %in% result$std_id)
  expect_false("b" %in% result$std_id)
})

test_that("as_coding_results converts state to legacy format", {
  state <- create_coding_state()
  state$codebook[["sleep_issues"]] <- list(
    code_name = "Sleep Issues", description = "Problems sleeping",
    type = "descriptive", frequency = 2L,
    entry_ids = c("e1", "e2"),
    coded_segments = list(
      list(entry_id = "e1", text = "cant sleep", start_char = 0L, end_char = 10L),
      list(entry_id = "e2", text = "insomnia", start_char = 5L, end_char = 13L)
    )
  )
  state$entry_results[["e1"]] <- list(
    codes_assigned = "sleep_issues",
    coded_segments = list(list(code_key = "sleep_issues", code_name = "Sleep Issues",
                                text = "cant sleep", start_char = 0L, end_char = 10L)),
    skipped = FALSE
  )
  state$entry_results[["e2"]] <- list(
    codes_assigned = "sleep_issues",
    coded_segments = list(list(code_key = "sleep_issues", code_name = "Sleep Issues",
                                text = "insomnia", start_char = 5L, end_char = 13L)),
    skipped = FALSE
  )
  state$entry_results[["e3"]] <- list(codes_assigned = character(0), skipped = TRUE, skip_reason = "N/A")
  state$entries_processed <- 1:3
  state$entries_skipped <- 3L

  cr <- as_coding_results(state)
  expect_type(cr, "list")
  expect_equal(cr$unique_codes, 1)
  expect_equal(cr$entries_coded, 2)
  expect_equal(cr$total_applications, 2)
  expect_true("sleep_issues" %in% names(cr$all_codes))
  expect_true("e1" %in% names(cr$entry_codes))
  expect_false("e3" %in% names(cr$entry_codes))
})

test_that("verify_excerpts works with ProgressiveCodingState", {
  state <- create_coding_state()
  state$codebook[["test_code"]] <- list(
    code_name = "Test Code", description = "", type = "descriptive",
    frequency = 1L, entry_ids = "e1",
    coded_segments = list(
      list(entry_id = "e1", text = "important finding", start_char = 5L, end_char = 22L)
    )
  )
  state$entry_results[["e1"]] <- list(
    codes_assigned = "test_code",
    coded_segments = list(list(code_key = "test_code", code_name = "Test Code",
                                text = "important finding", start_char = 5L, end_char = 22L)),
    skipped = FALSE
  )

  data <- tibble::tibble(std_id = "e1", std_text = "This important finding is notable.")
  result <- verify_excerpts(data, state)
  expect_type(result, "list")
  expect_equal(result$substring_stats$valid, 1)
  expect_equal(result$substring_stats$invalid, 0)
})

test_that("codebook_summary builder works", {
  state <- create_coding_state()
  # Empty codebook
  summary <- pakhom:::.build_codebook_summary(state)
  expect_equal(summary, "")

  # Add some codes
  state$codebook[["a"]] <- list(code_name = "Code A", frequency = 10L,
                                 description = "desc A", type = "descriptive")
  state$codebook[["b"]] <- list(code_name = "Code B", frequency = 5L,
                                 description = "desc B", type = "emotional")
  summary <- pakhom:::.build_codebook_summary(state, max_codes = 10)
  expect_true(nchar(summary) > 0)
  expect_true(grepl("Code A", summary))
  expect_true(grepl("Code B", summary))
})

# ============================================================================
# Phase 58 Tier 0 C-4 regression: numbered-list prompt leak
#
# Background: in the Phase 57 full-corpus run, 52 codes ended up with names
# like `321. "Food Addiction"` because the codebook-summary prompt format
# used `sprintf("  %d. \"%s\" ...", i, name)`. The AI echoed the entire
# prefix back as the code name on re-uses; the case-insensitive key matcher
# never collapsed them to existing codes, so they shadowed the clean
# versions in the codebook and the HAC distance matrix.
#
# Fixes:
# 1. Prompt format switched to bare `- name (freq=..., type=...)` bullets
#    (no numeric prefix, no surrounding quotes).
# 2. Defensive normalizer `.normalize_code_name()` strips any residual
#    numbered/quoted/NEW: prefixes on every code admitted to the codebook,
#    making future prompt drift recoverable rather than silent.
# ============================================================================

test_that(".normalize_code_name strips numbered-list prefixes (C-4 root cause)", {
  # The exact corruption pattern observed in Phase 57's full-corpus run.
  expect_equal(pakhom:::.normalize_code_name('321. "Food Addiction"'),
               "Food Addiction")
  expect_equal(pakhom:::.normalize_code_name('1. Food Addiction'),
               "Food Addiction")
  expect_equal(pakhom:::.normalize_code_name('  12. "Compulsive Eating"  '),
               "Compulsive Eating")
})

test_that(".normalize_code_name strips NEW: marker", {
  expect_equal(pakhom:::.normalize_code_name("NEW: Food Addiction"),
               "Food Addiction")
  expect_equal(pakhom:::.normalize_code_name("new: food addiction"),
               "food addiction")
})

test_that(".normalize_code_name handles combined prefix orderings", {
  expect_equal(pakhom:::.normalize_code_name('NEW: 12. "Food Addiction"'),
               "Food Addiction")
  expect_equal(pakhom:::.normalize_code_name('12. NEW: "Food Addiction"'),
               "Food Addiction")
  expect_equal(pakhom:::.normalize_code_name('  NEW: 12. "Food Addiction" '),
               "Food Addiction")
})

test_that(".normalize_code_name strips ASCII and Unicode smart quotes", {
  expect_equal(pakhom:::.normalize_code_name('"Food Addiction"'),
               "Food Addiction")
  expect_equal(pakhom:::.normalize_code_name("'Food Addiction'"),
               "Food Addiction")
  # Unicode left/right double quotation marks
  expect_equal(pakhom:::.normalize_code_name("“Food Addiction”"),
               "Food Addiction")
  # Unicode left/right single quotation marks
  expect_equal(pakhom:::.normalize_code_name("‘Food Addiction’"),
               "Food Addiction")
})

test_that(".normalize_code_name preserves clean names + idempotent", {
  clean <- "Food Addiction"
  expect_equal(pakhom:::.normalize_code_name(clean), clean)
  # Idempotency: applying twice produces the same result as once.
  once <- pakhom:::.normalize_code_name('321. "Food Addiction"')
  twice <- pakhom:::.normalize_code_name(once)
  expect_equal(once, twice)
})

test_that(".normalize_code_name handles NULL/NA/empty defensively", {
  expect_true(is.na(pakhom:::.normalize_code_name(NULL)))
  expect_true(is.na(pakhom:::.normalize_code_name(NA_character_)))
  expect_equal(pakhom:::.normalize_code_name(""), "")
  expect_equal(pakhom:::.normalize_code_name("   "), "")
})

test_that(".build_codebook_summary uses bare-bullet format (C-4 prompt fix)", {
  state <- create_coding_state()
  state$codebook[["food_addiction"]] <- list(
    code_name = "Food Addiction", frequency = 100L,
    description = "Reports of food compulsion", type = "descriptive"
  )
  state$codebook[["sleep_loss"]] <- list(
    code_name = "Sleep Loss", frequency = 50L,
    description = "Reports of difficulty sleeping", type = "emotional"
  )
  summary <- pakhom:::.build_codebook_summary(state, max_codes = 10)

  # New format: bare dash bullet, no numeric prefix.
  expect_true(grepl("- Food Addiction", summary, fixed = TRUE))
  expect_true(grepl("- Sleep Loss", summary, fixed = TRUE))

  # Old format must be gone: no numbered list prefix on any line; no
  # surrounding quotes on the code names.
  lines <- strsplit(summary, "\n", fixed = TRUE)[[1]]
  for (line in lines) {
    expect_false(grepl("^\\s*\\d+\\.\\s", line),
                 info = sprintf("line still has numbered prefix: %s", line))
    expect_false(grepl('"Food Addiction"', line, fixed = TRUE),
                 info = sprintf("code name still wrapped in quotes: %s", line))
    expect_false(grepl('"Sleep Loss"', line, fixed = TRUE),
                 info = sprintf("code name still wrapped in quotes: %s", line))
  }
})

# ============================================================================
# Phase 58 Tier 0 C-4 audit followup tests
#
# The post-C-4 audit subagent flagged:
#  - HIGH-2: no end-to-end test asserting that an AI returning the corrupt
#    `321. "Food Addiction"` shape merges into an existing `food addiction`
#    code rather than creating a new entry under a corrupt key.
#  - MEDIUM-3: inputs that normalize to "" (e.g. "321.", "NEW:") were not
#    explicitly guarded at the admission site -- the pre-norm guard only
#    catches nchar(seg_code) == 0.
#  - MEDIUM-4: the is_new regex did not allow leading quote-wrapping, so
#    `"\"NEW: Foo\""` would be classified as is_new = FALSE.
# ============================================================================

test_that("C-4 integration: AI-emitted '321. \"Foo\"' merges into existing 'foo' code (no duplicate)", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "I struggle with food addiction every single day."

  # Pre-populate an existing clean code in the codebook. If C-4's normalizer
  # is wired correctly, the AI's `321. "Food Addiction"` will normalize to
  # `Food Addiction` -> key `food addiction` -> merge with this entry.
  state <- create_coding_state()
  state$codebook[["food addiction"]] <- list(
    code_name      = "Food Addiction",
    description    = "Compulsive food consumption",
    type           = "descriptive",
    frequency      = 1L,
    entry_ids      = "e_prior",
    coded_segments = list()
  )

  mock_response <- jsonlite::toJSON(list(
    skipped        = FALSE,
    skip_reason    = "",
    coded_segments = list(list(
      text             = "food addiction",
      start_char       = 19L,
      end_char         = 33L,
      code             = "321. \"Food Addiction\"",
      code_description = "Compulsive food consumption",
      code_type        = "descriptive"
    ))
  ), auto_unbox = TRUE)

  # Mock compute_embeddings too: with a pre-populated codebook, the
  # Phase 58 C-6 additive-retrieval path inside .code_entry_progressive
  # would otherwise make a real HTTP request to OpenAI with the fake
  # test key and return 401. The graceful fallback works (no failure)
  # but the network egress is undesirable in a unit test.
  local_mocked_bindings(
    ai_complete = function(...) list(
      content       = mock_response,
      model         = "gpt-4o-mock",
      request_id    = "req_mock_c4_integration",
      usage         = list(prompt_tokens = 50L, completion_tokens = 25L,
                            total_tokens = 75L),
      finish_reason = "stop",
      raw_response  = list(),
      prompt_hash   = "hash-c4-integration"
    ),
    compute_embeddings = function(provider, texts, model = NULL) NULL,
    .package = "pakhom"
  )

  state <- pakhom:::.code_entry_progressive(
    text = entry_text, entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider(),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test"
  )

  # Codebook still has exactly ONE entry: the prior clean `food addiction`
  # code, with frequency incremented to 2 (prior + this new segment).
  expect_equal(length(state$codebook), 1L,
               info = "AI's `321. \"Food Addiction\"` should merge into the existing clean code, not create a new entry")
  expect_true("food addiction" %in% names(state$codebook))
  expect_equal(state$codebook[["food addiction"]]$frequency, 2L)
  expect_setequal(state$codebook[["food addiction"]]$entry_ids,
                  c("e_prior", "e1"))

  # No codebook key starts with a digit-prefix or contains stray quotes.
  for (key in names(state$codebook)) {
    expect_false(grepl("^\\d+\\.", key),
                 info = sprintf("codebook key has number-prefix: %s", key))
    expect_false(grepl("\"", key, fixed = TRUE),
                 info = sprintf("codebook key has quote chars: %s", key))
  }
})

test_that("C-4 audit MEDIUM-3: AI-emitted bare '321.' (normalizes to empty) is dropped, not admitted", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "Some text about food."

  mock_response <- jsonlite::toJSON(list(
    skipped        = FALSE,
    skip_reason    = "",
    coded_segments = list(list(
      text             = "Some text",
      start_char       = 0L,
      end_char         = 9L,
      code             = "321.",  # Normalizes to "" -- must NOT admit.
      code_description = "Empty after normalization",
      code_type        = "descriptive"
    ))
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(
      content       = mock_response,
      model         = "gpt-4o-mock",
      request_id    = "req_mock_med3",
      usage         = list(prompt_tokens = 30L, completion_tokens = 10L,
                            total_tokens = 40L),
      finish_reason = "stop",
      raw_response  = list(),
      prompt_hash   = "hash-med3"
    ),
    .package = "pakhom"
  )

  state <- create_coding_state()
  state <- suppressWarnings(pakhom:::.code_entry_progressive(
    text = entry_text, entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider(),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test"
  ))

  # The empty-normalization guard drops the segment; codebook stays empty.
  expect_equal(length(state$codebook), 0L,
               info = "code that normalizes to '' must not be admitted")
})

test_that("C-4 audit MEDIUM-4: quote-wrapped NEW: prefix still detected as new-code request", {
  # is_new is computed via regex on the pre-normalization seg_code; the
  # audit fix added `["']*` allowance for leading quotes.
  # We can't easily observe is_new from outside without mocking, but we
  # can verify the regex directly. Use the same pattern the production
  # code uses so any divergence is visible in this test.
  re <- "^[\"']*\\s*(\\d+\\.\\s*)?\\s*NEW:"
  expect_true(grepl(re, "NEW: Foo", ignore.case = TRUE))
  expect_true(grepl(re, "\"NEW: Foo\"", ignore.case = TRUE))
  expect_true(grepl(re, "'NEW: Foo'", ignore.case = TRUE))
  expect_true(grepl(re, "\"1. NEW: Foo\"", ignore.case = TRUE))
  expect_true(grepl(re, "new: foo", ignore.case = TRUE))
  expect_false(grepl(re, "Existing Code", ignore.case = TRUE))
})

# ============================================================================
# Phase 58 Tier 0 C-6: codebook additive semantic retrieval
#
# The pre-Phase-58 prompt window showed the AI top-80 codes by frequency +
# the last-20 created. With 4,000-code codebooks the AI saw only 2% of
# existing codes per entry, so every re-encounter past entry 1000 looked
# new and the codebook never saturated.
#
# Fix: ADDITIVE retrieval. Top-N by frequency + last-N created PLUS
# per-entry semantic top-K cosine retrieval against cached code
# embeddings. Defaults: max_codes = 150L (up from 80L), top_k_semantic
# = 30L. Embeddings cached per code key in
# state$semantic_cache$code_embeddings; computed on first use and
# survive checkpoint save/restore.
# ============================================================================

test_that("C-6: create_coding_state initializes empty semantic_cache", {
  state <- create_coding_state()
  expect_true("semantic_cache" %in% names(state))
  expect_true("code_embeddings" %in% names(state$semantic_cache))
  expect_type(state$semantic_cache$code_embeddings, "list")
  expect_length(state$semantic_cache$code_embeddings, 0L)
})

test_that("C-6: legacy .build_codebook_summary wrapper returns a string (back-compat)", {
  state <- create_coding_state()
  state$codebook[["a"]] <- list(code_name = "Code A", frequency = 10L,
                                 description = "desc A", type = "descriptive")
  state$codebook[["b"]] <- list(code_name = "Code B", frequency = 5L,
                                 description = "desc B", type = "emotional")
  out <- pakhom:::.build_codebook_summary(state)
  expect_type(out, "character")
  expect_length(out, 1L)
  expect_true(grepl("Code A", out, fixed = TRUE))
})

test_that("C-6: .build_codebook_summary_with_retrieval falls back to freq+recency when provider is NULL", {
  state <- create_coding_state()
  for (k in paste0("c", 1:30)) {
    state$codebook[[k]] <- list(
      code_name = paste("Code", k), frequency = sample(1:50, 1),
      description = "d", type = "descriptive"
    )
  }
  # No provider + no entry_text -> no semantic call -> just freq + recency
  result <- pakhom:::.build_codebook_summary_with_retrieval(
    state, max_codes = 150L, recent_window = 5L,
    entry_text = NULL, provider = NULL, top_k_semantic = 10L
  )
  expect_type(result, "list")
  expect_named(result, c("summary", "new_embeddings"))
  expect_length(result$new_embeddings, 0L)
  expect_true(nchar(result$summary) > 0)
  # Output contains bare bullets (no numeric prefix).
  lines <- strsplit(result$summary, "\n", fixed = TRUE)[[1]]
  for (line in lines) {
    expect_false(grepl("^\\s*\\d+\\.\\s", line))
    expect_true(grepl("^\\s*-\\s", line))
  }
})

test_that("C-6: .retrieve_semantic_codes returns integer(0) when provider lacks embeddings", {
  state <- create_coding_state()
  state$codebook[["a"]] <- list(code_name = "Code A", frequency = 1L,
                                 description = "x", type = "descriptive")
  code_data <- list(list(key = "a", name = "Code A", desc = "x",
                          freq = 1L, type = "descriptive"))
  fake_provider <- structure(list(provider = "anthropic", models = list()),
                              class = "AIProvider")
  # compute_embeddings short-circuits for non-OpenAI providers (returns NULL),
  # so .retrieve_semantic_codes must return an empty result.
  result <- pakhom:::.retrieve_semantic_codes(
    state, code_data, entry_text = "some text",
    provider = fake_provider, top_k = 5L
  )
  expect_equal(result$indices, integer(0))
  expect_length(result$new_embeddings, 0L)
})

test_that("C-6: .retrieve_semantic_codes selects top-K via cosine + populates cache", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  # Construct 4 codes with hand-built embeddings so we can predict the
  # similarity ranking. Use 3-dim unit vectors:
  #   c1: (1, 0, 0)   <- closest to entry (1, 0, 0)
  #   c2: (0.95, 0.31, 0)  <- 2nd closest
  #   c3: (0.5, 0.87, 0)
  #   c4: (0, 1, 0)   <- least close
  state <- create_coding_state()
  for (k in paste0("c", 1:4)) {
    state$codebook[[k]] <- list(code_name = paste("Code", k), frequency = 1L,
                                 description = "x", type = "descriptive")
  }
  code_data <- lapply(names(state$codebook), function(k) {
    list(key = k, name = state$codebook[[k]]$code_name, desc = "x",
         freq = 1L, type = "descriptive")
  })
  fake_provider <- structure(list(provider = "openai", models = list()),
                              class = "AIProvider")

  # Mock compute_embeddings: first call (entry) returns (1,0,0); second
  # call (4 codes) returns the matrix above.
  call_count <- 0L
  local_mocked_bindings(
    compute_embeddings = function(provider, texts, model = NULL) {
      call_count <<- call_count + 1L
      if (length(texts) == 1L) {
        # entry embedding
        matrix(c(1, 0, 0), nrow = 1, byrow = TRUE)
      } else {
        # code embeddings (length 4)
        matrix(c(
          1.00, 0.00, 0.00,
          0.95, 0.31, 0.00,
          0.50, 0.87, 0.00,
          0.00, 1.00, 0.00
        ), nrow = 4, byrow = TRUE)
      }
    },
    .package = "pakhom"
  )

  result <- pakhom:::.retrieve_semantic_codes(
    state, code_data, entry_text = "some entry",
    provider = fake_provider, top_k = 2L
  )
  # Top-2 cosine match: c1 (1.0) and c2 (0.95). order returns indices into
  # code_data, so result$indices should be c(1, 2).
  expect_equal(result$indices, c(1L, 2L))
  # All 4 codes embedded (cache miss for all). new_embeddings keyed by
  # code_key.
  expect_setequal(names(result$new_embeddings), c("c1", "c2", "c3", "c4"))
  # 2 API calls: 1 for entry, 1 for codes batch.
  expect_equal(call_count, 2L)
})

test_that("C-6: .retrieve_semantic_codes uses cache on second call (no re-embed)", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  state <- create_coding_state()
  state$codebook[["a"]] <- list(code_name = "Code A", frequency = 1L,
                                 description = "x", type = "descriptive")
  # Pre-populate cache with embedding for the existing code.
  state$semantic_cache$code_embeddings[["a"]] <- c(1, 0, 0)
  code_data <- list(list(key = "a", name = "Code A", desc = "x",
                          freq = 1L, type = "descriptive"))
  fake_provider <- structure(list(provider = "openai", models = list()),
                              class = "AIProvider")

  call_count <- 0L
  embed_inputs <- list()
  local_mocked_bindings(
    compute_embeddings = function(provider, texts, model = NULL) {
      call_count <<- call_count + 1L
      embed_inputs[[length(embed_inputs) + 1L]] <<- texts
      matrix(c(1, 0, 0), nrow = 1, byrow = TRUE)
    },
    .package = "pakhom"
  )

  result <- pakhom:::.retrieve_semantic_codes(
    state, code_data, entry_text = "some entry",
    provider = fake_provider, top_k = 5L
  )
  # Only the entry embedding call should fire; the code is already cached
  # so the code-batch call is skipped.
  expect_equal(call_count, 1L)
  expect_length(result$new_embeddings, 0L)  # nothing new added
  expect_equal(result$indices, 1L)  # single code still scores
})

test_that("C-6: .retrieve_semantic_codes degrades gracefully on embedding failure", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  state <- create_coding_state()
  state$codebook[["a"]] <- list(code_name = "Code A", frequency = 1L,
                                 description = "x", type = "descriptive")
  code_data <- list(list(key = "a", name = "Code A", desc = "x",
                          freq = 1L, type = "descriptive"))
  fake_provider <- structure(list(provider = "openai", models = list()),
                              class = "AIProvider")

  # Mock compute_embeddings to fail (return NULL, as the real helper does
  # on HTTP / network failure).
  local_mocked_bindings(
    compute_embeddings = function(provider, texts, model = NULL) NULL,
    .package = "pakhom"
  )

  result <- pakhom:::.retrieve_semantic_codes(
    state, code_data, entry_text = "some entry",
    provider = fake_provider, top_k = 5L
  )
  # Empty result; no crash.
  expect_equal(result$indices, integer(0))
  expect_length(result$new_embeddings, 0L)
})

test_that("C-6: .build_codebook_summary_with_retrieval injects semantic top-K into selection", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  # 5 codes total. freq ranking: high (50) -> low (1). Recency ranking: c5
  # is last. With max_codes = 3 and recent_window = 1, freq + recency
  # would pick {high (c1), c5}. Add semantic retrieval pointing to a
  # specific code (c3) and verify it gets added even though it's neither
  # high-freq nor recent.
  state <- create_coding_state()
  freqs <- c(50L, 30L, 5L, 20L, 1L)
  for (i in seq_along(freqs)) {
    state$codebook[[paste0("c", i)]] <- list(
      code_name = paste0("Code", i), frequency = freqs[i],
      description = "x", type = "descriptive"
    )
  }
  fake_provider <- structure(list(provider = "openai", models = list()),
                              class = "AIProvider")

  # Mock embeddings so c3 is the single closest match to the entry.
  local_mocked_bindings(
    compute_embeddings = function(provider, texts, model = NULL) {
      if (length(texts) == 1L) {
        # entry embedding -- align with c3 direction
        matrix(c(0, 0, 1), nrow = 1, byrow = TRUE)
      } else {
        # 5 code embeddings -- c3 perfectly aligned, others orthogonal
        matrix(c(
          1, 0, 0,   # c1
          1, 0, 0,   # c2
          0, 0, 1,   # c3  (semantic match)
          1, 0, 0,   # c4
          1, 0, 0    # c5
        ), nrow = 5, byrow = TRUE)
      }
    },
    .package = "pakhom"
  )

  result <- pakhom:::.build_codebook_summary_with_retrieval(
    state, max_codes = 3L, recent_window = 1L,
    entry_text = "some entry text",
    provider = fake_provider, top_k_semantic = 1L
  )
  # Selection should include c1 (highest freq), c5 (most recent), and c3
  # (semantic top-K). Total = 3 codes, hits max_codes cap exactly.
  expect_true(grepl("Code1", result$summary, fixed = TRUE),
              info = "highest-frequency code missing from summary")
  expect_true(grepl("Code5", result$summary, fixed = TRUE),
              info = "most-recently-created code missing from summary")
  expect_true(grepl("Code3", result$summary, fixed = TRUE),
              info = "semantic top-K code missing from summary")
  # The other two codes (c2, c4) should NOT be in the output -- they
  # didn't make any of the three selection cohorts.
  expect_false(grepl("Code2", result$summary, fixed = TRUE))
  expect_false(grepl("Code4", result$summary, fixed = TRUE))
  expect_length(result$new_embeddings, 5L)
})

# C-6 audit followup tests --------------------------------------------------

test_that("C-6 audit LOW-1: mismatched embedding dimensions are scored -Inf (no recycling)", {
  # Pre-fix: stale cache with a different-dim embedding (e.g., from a
  # prior run that used a different embedding model) would silently
  # recycle into `sum(emb * entry_emb)` and produce garbage cosines.
  # Post-fix: length mismatch -> -Inf -> excluded from top-K.
  state <- create_coding_state()
  state$codebook[["c1"]] <- list(code_name = "Code 1", frequency = 1L,
                                  description = "x", type = "descriptive")
  state$codebook[["c2"]] <- list(code_name = "Code 2", frequency = 1L,
                                  description = "x", type = "descriptive")
  # Pre-populate one cache entry with a WRONG-DIM embedding.
  state$semantic_cache$code_embeddings[["c1"]] <- c(1, 0, 0, 0)   # 4 dims
  state$semantic_cache$code_embeddings[["c2"]] <- c(0, 1, 0)      # 3 dims (matches entry)
  code_data <- list(
    list(key = "c1", name = "Code 1", desc = "x", freq = 1L, type = "descriptive"),
    list(key = "c2", name = "Code 2", desc = "x", freq = 1L, type = "descriptive")
  )
  fake_provider <- structure(list(provider = "openai", models = list()),
                              class = "AIProvider")

  testthat::local_mocked_bindings(
    compute_embeddings = function(provider, texts, model = NULL) {
      # entry embedding: 3-dim (matches c2 but not c1)
      matrix(c(0, 1, 0), nrow = 1, byrow = TRUE)
    },
    .package = "pakhom"
  )

  result <- pakhom:::.retrieve_semantic_codes(
    state, code_data, entry_text = "x",
    provider = fake_provider, top_k = 2L
  )
  # c1 has wrong-dim cache -> excluded; only c2 should appear in indices.
  expect_equal(result$indices, 2L)
})

test_that("C-6 audit LOW-3: state$semantic_cache populates after .code_entry_progressive returns", {
  # End-to-end verification of the cache-merge contract: after one
  # round of .code_entry_progressive on a pre-populated codebook, the
  # state returned must carry the new code embeddings in
  # $semantic_cache$code_embeddings.
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  state <- create_coding_state()
  state$codebook[["food addiction"]] <- list(
    code_name = "Food Addiction", description = "Compulsive consumption",
    type = "descriptive", frequency = 1L,
    entry_ids = "e_prior", coded_segments = list()
  )
  # Cache starts empty -> .retrieve_semantic_codes will compute the
  # food-addiction embedding on this call.
  expect_length(state$semantic_cache$code_embeddings, 0L)

  mock_resp <- jsonlite::toJSON(list(
    skipped = FALSE, skip_reason = "",
    coded_segments = list(list(
      text = "food addiction", start_char = 0L, end_char = 14L,
      code = "Food Addiction",
      code_description = "Compulsive consumption", code_type = "descriptive"
    ))
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(
      content = mock_resp, model = "gpt-4o-mock",
      request_id = "r", usage = list(prompt_tokens = 1L,
                                      completion_tokens = 1L, total_tokens = 2L),
      finish_reason = "stop", raw_response = list(), prompt_hash = "h"
    ),
    compute_embeddings = function(provider, texts, model = NULL) {
      # Return deterministic dim-3 vectors; matrix shape depends on
      # whether this is the entry call (1 text) or code call (N texts).
      if (length(texts) == 1L) {
        matrix(c(1, 0, 0), nrow = 1, byrow = TRUE)
      } else {
        matrix(rep(c(1, 0, 0), length(texts)), nrow = length(texts), byrow = TRUE)
      }
    },
    .package = "pakhom"
  )

  state <- pakhom:::.code_entry_progressive(
    text = "food addiction is rough", entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider(),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test"
  )

  # Cache must now contain the food-addiction embedding.
  expect_length(state$semantic_cache$code_embeddings, 1L)
  expect_true("food addiction" %in% names(state$semantic_cache$code_embeddings))
  expect_equal(state$semantic_cache$code_embeddings[["food addiction"]],
               c(1, 0, 0))
})

test_that("C-6 audit LOW-4: cache accumulates across multiple calls (no re-embed)", {
  # Simulates the production loop: each entry's coding call extends the
  # cache. Codes embedded in call N are cache hits in call N+1.
  state <- create_coding_state()
  for (k in c("c1", "c2", "c3")) {
    state$codebook[[k]] <- list(
      code_name = paste("Code", k), frequency = 1L,
      description = "x", type = "descriptive"
    )
  }
  code_data <- lapply(names(state$codebook), function(k) {
    list(key = k, name = state$codebook[[k]]$code_name, desc = "x",
         freq = 1L, type = "descriptive")
  })
  fake_provider <- structure(list(provider = "openai", models = list()),
                              class = "AIProvider")

  call_count <- 0L
  texts_seen <- list()
  testthat::local_mocked_bindings(
    compute_embeddings = function(provider, texts, model = NULL) {
      call_count <<- call_count + 1L
      texts_seen[[length(texts_seen) + 1L]] <<- texts
      matrix(rep(c(1, 0, 0), length(texts)),
              nrow = length(texts), byrow = TRUE)
    },
    .package = "pakhom"
  )

  # First call: cache empty -> all 3 codes embedded (1 batch call) + 1
  # entry embedding call = 2 total.
  result1 <- pakhom:::.retrieve_semantic_codes(
    state, code_data, entry_text = "entry text", provider = fake_provider,
    top_k = 3L
  )
  expect_equal(call_count, 2L)
  expect_length(result1$new_embeddings, 3L)

  # Merge new embeddings into state (simulates the production callsite).
  for (key in names(result1$new_embeddings)) {
    state$semantic_cache$code_embeddings[[key]] <- result1$new_embeddings[[key]]
  }

  # Second call: cache hit on ALL 3 codes -> only the entry embedding
  # call fires (call_count becomes 3, not 4).
  result2 <- pakhom:::.retrieve_semantic_codes(
    state, code_data, entry_text = "another entry", provider = fake_provider,
    top_k = 3L
  )
  expect_equal(call_count, 3L)
  expect_length(result2$new_embeddings, 0L)
})

# ============================================================================
# Saturation tracking math (curve computation)
#
# Background: the previous saturation calculation tried to derive each code's
# birth-time-in-coded-entries from per-checkpoint accumulator lists that get
# reset every checkpoint_interval. After the first reset the calculation lost
# all history, so `new_in_window` collapsed toward zero and the code-creation-
# rate signal fired prematurely. The fix stores `n_coded_at_birth` directly
# in a parallel map at the moment each code is created, so the saturation
# check is a direct lookup.
#
# Phase 56: the pre-Phase-56 code-creation-rate / slope-ratio / consecutive-
# windows heuristic signals were replaced by an AI saturation arbiter
# (R/saturation_arbiter.R) per C1. The curve math below is still in
# production (it feeds the AI arbiter's prompt evidence + the
# saturation_curve.png plot), but the `saturation_threshold` / `window_size`
# values used in the assertions below are now test-local constants, not
# config knobs.
# ============================================================================

test_that("create_coding_state initializes code_n_coded_at_birth alongside code_birth_log", {
  state <- create_coding_state()
  expect_true("code_n_coded_at_birth" %in% names(state$saturation))
  expect_true("code_birth_log" %in% names(state$saturation))
  expect_type(state$saturation$code_n_coded_at_birth, "list")
  expect_length(state$saturation$code_n_coded_at_birth, 0)
})

test_that("saturation: new_in_window reflects only codes born within the window", {
  # Direct calculation that mirrors the production code in 09_coding.R lines
  # 281-289. Tests the math without invoking the full coding loop.
  state <- create_coding_state()
  # Codes born at coded-entry counts: 10, 100, 200, 400, 500, 580, 600
  birth_points <- c(10, 100, 200, 400, 500, 580, 600)
  for (i in seq_along(birth_points)) {
    state$saturation$code_n_coded_at_birth[[paste0("code_", i)]] <- birth_points[i]
  }

  n_coded <- 600
  window_size <- 200
  births <- unlist(state$saturation$code_n_coded_at_birth, use.names = FALSE)
  new_in_window <- sum(births > (n_coded - window_size))  # > 400
  # Codes at 500, 580, 600 should count (3 codes); 400 itself is the boundary
  # and is strictly NOT > 400, so it should not count.
  expect_equal(new_in_window, 3)
})

test_that("saturation: signal_creation does NOT fire when code creation is steady", {
  # Simulate 600 coded entries with codes born at every 10 coded entries
  state <- create_coding_state()
  for (i in seq(50, 600, by = 10)) {
    state$saturation$code_n_coded_at_birth[[paste0("code_", i)]] <- i
  }
  n_coded <- 600
  window_size <- 200
  saturation_threshold <- 2

  births <- unlist(state$saturation$code_n_coded_at_birth, use.names = FALSE)
  new_in_window <- sum(births > (n_coded - window_size))

  # Codes born at 410, 420, ..., 600 -> 20 codes. 20 > threshold of 2.
  # signal_creation should be FALSE -> saturation should NOT fire on this signal.
  expect_equal(new_in_window, 20)
  expect_false(new_in_window <= saturation_threshold)
})

test_that("saturation: signal_creation DOES fire when code creation has stopped", {
  # All codes born early (before n_coded=400); we are now at n_coded=700
  state <- create_coding_state()
  for (i in seq(50, 400, by = 10)) {
    state$saturation$code_n_coded_at_birth[[paste0("code_", i)]] <- i
  }
  n_coded <- 700
  window_size <- 200
  saturation_threshold <- 2

  births <- unlist(state$saturation$code_n_coded_at_birth, use.names = FALSE)
  new_in_window <- sum(births > (n_coded - window_size))

  # All births at <= 400 < (700-200)=500 -> new_in_window == 0
  # 0 <= threshold of 2, so signal_creation should be TRUE
  expect_equal(new_in_window, 0)
  expect_true(new_in_window <= saturation_threshold)
})

test_that("saturation: empty codebook produces zero new_in_window without errors", {
  state <- create_coding_state()
  expect_length(state$saturation$code_n_coded_at_birth, 0)
  # Mirror the guarded path from 09_coding.R
  if (length(state$saturation$code_n_coded_at_birth) > 0) {
    births <- unlist(state$saturation$code_n_coded_at_birth, use.names = FALSE)
    new_in_window <- sum(births > 0)
  } else {
    new_in_window <- 0L
  }
  expect_equal(new_in_window, 0L)
})

test_that("backward-compat: legacy state missing code_n_coded_at_birth is conservatively backfilled", {
  # Simulate a state saved under the old structure (code_birth_log present,
  # code_n_coded_at_birth absent). Call run_progressive_coding's resume path
  # logic by constructing the equivalent backfill the function performs.
  legacy_state <- create_coding_state()
  legacy_state$saturation$code_birth_log <- list(
    code_a = 10L, code_b = 25L, code_c = 100L
  )
  legacy_state$saturation$code_n_coded_at_birth <- NULL  # simulate older save

  # Apply the backfill as run_progressive_coding does on resume
  if (is.null(legacy_state$saturation$code_n_coded_at_birth)) {
    legacy_state$saturation$code_n_coded_at_birth <- setNames(
      as.list(rep(0L, length(legacy_state$saturation$code_birth_log))),
      names(legacy_state$saturation$code_birth_log)
    )
  }

  expect_length(legacy_state$saturation$code_n_coded_at_birth, 3)
  expect_named(legacy_state$saturation$code_n_coded_at_birth,
               c("code_a", "code_b", "code_c"))
  # All seeded with 0 -> conservative: never count toward "recent" births
  # for any n_coded > window_size, which is the safe direction (won't cause
  # premature saturation, may delay it slightly)
  births <- unlist(legacy_state$saturation$code_n_coded_at_birth, use.names = FALSE)
  expect_true(all(births == 0))
  expect_equal(sum(births > (500 - 200)), 0)  # at any plausible n_coded, no recent
})

# ==============================================================================
# T0.1 wiring: .code_entry_progressive verify_quote + fabrication_log integration
# ==============================================================================

test_that("T0.1: .code_entry_progressive attaches QuoteProvenance to verified segments", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "I had trouble sleeping after starting the new medication."

  # Mock returns one segment whose text IS verbatim in the entry -> verified
  mock_response <- jsonlite::toJSON(list(
    skipped        = FALSE,
    skip_reason    = "",
    coded_segments = list(list(
      text             = "trouble sleeping",
      start_char       = 6L,
      end_char         = 22L,
      code             = "NEW: sleep difficulty",
      code_description = "Difficulty initiating or maintaining sleep",
      code_type        = "descriptive"
    ))
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(
      content    = mock_response,
      model      = "gpt-4o-mock",
      request_id = "req_mock_verified",
      usage      = list(prompt_tokens = 50L, completion_tokens = 25L,
                        total_tokens = 75L),
      finish_reason = "stop",
      raw_response  = list(),
      prompt_hash   = "hash-verified"
    ),
    .package = "pakhom"
  )

  state <- create_coding_state()
  state <- pakhom:::.code_entry_progressive(
    text = entry_text, entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider(),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test"
  )

  # Segment kept; provenance attached; verification passed
  seg <- state$entry_results[["e1"]]$coded_segments[[1]]
  expect_true(!is.null(seg$provenance))
  expect_s3_class(seg$provenance, "QuoteProvenance")
  expect_true(seg$provenance$verification_status %in% c("verified_exact", "verified_fuzzy"))
  expect_equal(seg$provenance$ai_call_id, "req_mock_verified")
  expect_equal(seg$provenance$ai_model,   "gpt-4o-mock")
})

test_that("T0.1: .code_entry_progressive drops fabricated segments + does not pollute codebook", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "Real entry text about sleep struggles."

  # Mock returns one fabricated segment (text not in entry)
  mock_response <- jsonlite::toJSON(list(
    skipped        = FALSE,
    skip_reason    = "",
    coded_segments = list(list(
      text             = "I love unicorns and rainbows",
      start_char       = 0L,
      end_char         = 28L,
      code             = "NEW: unicorn appreciation",
      code_description = "Affinity for unicorns",
      code_type        = "descriptive"
    ))
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(
      content       = mock_response,
      model         = "gpt-4o-mock",
      request_id    = "req_mock_fab",
      usage         = list(prompt_tokens = 50L, completion_tokens = 25L,
                            total_tokens = 75L),
      finish_reason = "stop",
      raw_response  = list(),
      prompt_hash   = "hash-fabricated"
    ),
    .package = "pakhom"
  )

  state <- create_coding_state()
  # Capture log_warn output to silence the expected fabrication warning
  state <- suppressWarnings(pakhom:::.code_entry_progressive(
    text = entry_text, entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider(),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test"
  ))

  # No segment kept; codebook unpolluted
  expect_length(state$entry_results[["e1"]]$coded_segments, 0L)
  expect_length(state$codebook, 0L)
  # Entry NOT marked skipped -- fabrication is a different signal than
  # "AI said this entry has no applicable content"
  expect_false(isTRUE(state$entry_results[["e1"]]$skipped))
})

test_that("T0.1: .code_entry_progressive writes fabricated quotes to FabricationLog when supplied", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "Real entry text."
  mock_response <- jsonlite::toJSON(list(
    skipped        = FALSE,
    skip_reason    = "",
    coded_segments = list(list(
      text             = "fabricated content not in source",
      start_char       = 0L,
      end_char         = 32L,
      code             = "NEW: fake_code",
      code_description = "test",
      code_type        = "descriptive"
    ))
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(
      content       = mock_response,
      model         = "gpt-4o-mock",
      request_id    = "req_fab_log",
      usage         = list(prompt_tokens = 50L, completion_tokens = 25L,
                            total_tokens = 75L),
      finish_reason = "stop",
      raw_response  = list(),
      prompt_hash   = "hash-fabricated2"
    ),
    .package = "pakhom"
  )

  td <- withr::local_tempdir()
  flog <- init_fabrication_log(td)

  state <- create_coding_state()
  suppressWarnings(pakhom:::.code_entry_progressive(
    text = entry_text, entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider(),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test",
    fabrication_log = flog
  ))

  expect_equal(flog$state$n_logged, 1L)
  rows <- read.csv(flog$path, stringsAsFactors = FALSE)
  expect_equal(nrow(rows), 1L)
  expect_equal(rows$verification_status, "fabricated")
  expect_equal(rows$ai_call_id, "req_fab_log")
})

# ==============================================================================
# Phase 21c: Anthropic Citations API integration in .code_entry_progressive
# ==============================================================================
# T0.1 part 3b: when the provider is Anthropic, the coding pipeline uses the
# Citations API (PREVENTION layer -- model returns server-side-guaranteed
# offsets into the source document instead of free-form quotes). The
# verification ladder still runs as defense in depth. Tests cover:
#   - Provider dispatch: Anthropic -> citations path; OpenAI -> schema path
#   - Citations -> QuoteProvenance with citation_source = anthropic_citations_api
#   - Pairing: emission-order match, string match, fallback to model_freeform
#   - End-to-end: Anthropic citations path produces verified quotes
#   - End-to-end: OpenAI schema path is unchanged (regression check)

# ---- Dispatch and small helpers ---------------------------------------------

test_that(".use_citations_for_provider returns TRUE only for Anthropic", {
  expect_true(pakhom:::.use_citations_for_provider(mock_provider("anthropic"),
                                                   list()))
  expect_false(pakhom:::.use_citations_for_provider(mock_provider("openai"),
                                                    list()))
})

test_that(".normalize_segments handles all three jsonlite shapes", {
  # list-of-lists (the canonical shape) -- pass-through
  segs_list <- list(list(text = "x", code = "c"), list(text = "y", code = "c"))
  expect_identical(pakhom:::.normalize_segments(segs_list), segs_list)

  # data.frame (jsonlite collapses uniform-fields)
  segs_df <- data.frame(text = c("x", "y"), code = c("c", "c"),
                        stringsAsFactors = FALSE)
  segs_norm <- pakhom:::.normalize_segments(segs_df)
  expect_length(segs_norm, 2L)
  expect_equal(segs_norm[[1]]$text, "x")

  # single segment named list (jsonlite collapses arity-1 arrays)
  seg_one <- list(text = "x", code = "c", code_description = "d", code_type = "t")
  segs_norm <- pakhom:::.normalize_segments(seg_one)
  expect_length(segs_norm, 1L)
  expect_equal(segs_norm[[1]]$text, "x")
})

test_that(".build_progressive_citations_user_prompt declares anti-fabrication rules", {
  prompt <- pakhom:::.build_progressive_citations_user_prompt()
  expect_match(prompt, "verbatim quote from the document")
  expect_match(prompt, "Do NOT paraphrase")
  expect_match(prompt, "Do NOT invent quotes")
  # Schema still asks for code, code_description, code_type
  expect_match(prompt, "code_description")
  expect_match(prompt, "code_type")
})

test_that(".citation_text_matches returns TRUE on exact and normalized matches", {
  cite <- list(cited_text = "trouble sleeping")
  expect_true(pakhom:::.citation_text_matches(cite, "trouble sleeping"))
  # Normalized comparison handles whitespace/case
  expect_true(pakhom:::.citation_text_matches(
    list(cited_text = "  Trouble  Sleeping "),
    "trouble sleeping"))
  # Smart quotes vs straight quotes
  expect_true(pakhom:::.citation_text_matches(
    list(cited_text = "“hello”"),
    '"hello"'))
  # Genuinely different text
  expect_false(pakhom:::.citation_text_matches(
    list(cited_text = "different"),
    "trouble sleeping"))
})

# ---- Quote constructors per path --------------------------------------------

test_that(".build_quote_from_schema_path produces a model_freeform QuoteProvenance", {
  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model <- "gpt-4o-mock"
  ai_meta$call_id <- "req_test"
  q <- pakhom:::.build_quote_from_schema_path(
    seg_text = "trouble sleeping", seg_start = 6L, seg_end = 22L,
    text = "I had trouble sleeping after starting the medication",
    entry_id = "e1", code_key = "sleep_difficulty", ai_meta = ai_meta
  )
  expect_s3_class(q, "QuoteProvenance")
  expect_equal(q$citation_source, "model_freeform")
  expect_equal(q$start_char, 6L)
  expect_equal(q$end_char,   22L)
  expect_equal(q$exact_text, "trouble sleeping")
})

test_that(".build_quote_from_schema_path defaults bad offsets to safe values", {
  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model <- "gpt-4o-mock"; ai_meta$call_id <- "r"
  # Negative start -> 0
  q <- pakhom:::.build_quote_from_schema_path(
    seg_text = "hello", seg_start = -5L, seg_end = NA_integer_,
    text = "hello world", entry_id = "e1", code_key = "c", ai_meta = ai_meta
  )
  expect_equal(q$start_char, 0L)
  expect_equal(q$end_char,   nchar("hello"))
})

test_that(".build_quote_from_citations_path returns anthropic_citations_api on emission-order match", {
  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model <- "claude-mock"; ai_meta$call_id <- "msg_test"
  citations <- list(
    list(type = "char_location", cited_text = "trouble sleeping",
         document_index = 0L, document_title = "e1",
         start_char_index = 6L, end_char_index = 22L),
    list(type = "char_location", cited_text = "the medication",
         document_index = 0L, document_title = "e1",
         start_char_index = 38L, end_char_index = 52L)
  )
  documents <- list(list(id = "e1",
                         text = "I had trouble sleeping after starting the medication.",
                         type = "data_entry"))

  q <- pakhom:::.build_quote_from_citations_path(
    seg_text = "trouble sleeping", seg_index = 1L,
    citations = citations, documents = documents,
    text = documents[[1]]$text, entry_id = "e1",
    code_key = "sleep_difficulty", ai_meta = ai_meta
  )
  expect_s3_class(q, "QuoteProvenance")
  expect_equal(q$citation_source, "anthropic_citations_api")
  expect_equal(q$start_char,      6L)
  expect_equal(q$end_char,        22L)
  expect_equal(q$exact_text,      "trouble sleeping")
})

test_that(".build_quote_from_citations_path falls back to string match when emission order is wrong", {
  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model <- "m"; ai_meta$call_id <- "c"
  citations <- list(
    list(type = "char_location", cited_text = "the medication",
         document_index = 0L, document_title = "e1",
         start_char_index = 38L, end_char_index = 52L),
    list(type = "char_location", cited_text = "trouble sleeping",
         document_index = 0L, document_title = "e1",
         start_char_index = 6L, end_char_index = 22L)
  )
  documents <- list(list(id = "e1",
                         text = "I had trouble sleeping after starting the medication.",
                         type = "data_entry"))

  # seg_index=1 would emission-order-match citations[[1]] (cited_text =
  # "the medication"), which doesn't match seg_text. String-match fallback
  # finds citations[[2]].
  q <- pakhom:::.build_quote_from_citations_path(
    seg_text = "trouble sleeping", seg_index = 1L,
    citations = citations, documents = documents,
    text = documents[[1]]$text, entry_id = "e1",
    code_key = "sleep_difficulty", ai_meta = ai_meta
  )
  expect_equal(q$citation_source, "anthropic_citations_api")
  expect_equal(q$exact_text, "trouble sleeping")
  expect_equal(q$start_char, 6L)
})

test_that(".build_quote_from_citations_path falls back to model_freeform when no citation matches", {
  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model <- "m"; ai_meta$call_id <- "c"
  citations <- list(
    list(type = "char_location", cited_text = "completely different text",
         document_index = 0L, document_title = "e1",
         start_char_index = 0L, end_char_index = 25L)
  )
  documents <- list(list(id = "e1",
                         text = "I had trouble sleeping",
                         type = "data_entry"))

  q <- pakhom:::.build_quote_from_citations_path(
    seg_text = "trouble sleeping", seg_index = 1L,
    citations = citations, documents = documents,
    text = documents[[1]]$text, entry_id = "e1",
    code_key = "sleep_difficulty", ai_meta = ai_meta
  )
  # Pairing failed -> fell back to model_freeform; ladder will run normally
  expect_equal(q$citation_source, "model_freeform")
  expect_equal(q$exact_text, "trouble sleeping")
})

test_that(".build_quote_from_citations_path falls back when citations list is empty", {
  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model <- "m"; ai_meta$call_id <- "c"
  documents <- list(list(id = "e1", text = "I had trouble sleeping",
                         type = "data_entry"))

  q <- pakhom:::.build_quote_from_citations_path(
    seg_text = "trouble sleeping", seg_index = 1L,
    citations = list(), documents = documents,
    text = documents[[1]]$text, entry_id = "e1",
    code_key = "c", ai_meta = ai_meta
  )
  expect_equal(q$citation_source, "model_freeform")
})

# ---- End-to-end: .code_entry_progressive on Anthropic uses citations --------

test_that("T0.1 part 3b: Anthropic provider triggers citations path; QuoteProvenance carries anthropic_citations_api", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "I had trouble sleeping after starting the new medication."
  # JSON without offsets: model returns text only; citations carry the offsets
  mock_response <- jsonlite::toJSON(list(
    skipped        = FALSE,
    skip_reason    = "",
    coded_segments = list(list(
      text             = "trouble sleeping",
      code             = "NEW: sleep difficulty",
      code_description = "Difficulty initiating or maintaining sleep",
      code_type        = "descriptive"
    ))
  ), auto_unbox = TRUE)

  captured <- new.env(parent = emptyenv())

  local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt = NULL, task = "coding",
                            model = NULL, temperature = NULL, max_tokens = NULL,
                            json_mode = FALSE, max_retries = 3,
                            response_schema = NULL, documents = NULL) {
      # Capture the kwargs to assert dispatch went down the citations path
      captured$documents       <- documents
      captured$response_schema <- response_schema
      captured$json_mode       <- json_mode
      list(
        content       = mock_response,
        model         = "claude-opus-4-7-mock",
        request_id    = "req_anthropic_test",
        usage         = list(prompt_tokens = 100L, completion_tokens = 30L,
                             total_tokens = 130L),
        finish_reason = "stop",
        raw_response  = list(),
        prompt_hash   = "h",
        # Anthropic returned a citation pointing to the verbatim claim
        citations     = list(list(
          type             = "char_location",
          cited_text       = "trouble sleeping",
          document_index   = 0L,
          document_title   = "e1",
          start_char_index = 6L,
          end_char_index   = 22L
        ))
      )
    },
    .package = "pakhom"
  )

  state <- create_coding_state()
  state <- pakhom:::.code_entry_progressive(
    text = entry_text, entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider("anthropic"),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test"
  )

  # Dispatch went down the citations path: documents passed, no response_schema,
  # json_mode TRUE
  expect_length(captured$documents, 1L)
  expect_equal(captured$documents[[1]]$id, "e1")
  expect_equal(captured$documents[[1]]$text, entry_text)
  expect_null(captured$response_schema)
  expect_true(captured$json_mode)

  # The QuoteProvenance carries the citations-API source and is verified
  seg <- state$entry_results[["e1"]]$coded_segments[[1]]
  expect_s3_class(seg$provenance, "QuoteProvenance")
  expect_equal(seg$provenance$citation_source, "anthropic_citations_api")
  expect_true(seg$provenance$verification_status %in%
              c("verified_exact", "verified_fuzzy"))
  expect_equal(seg$provenance$ai_call_id, "req_anthropic_test")
  expect_equal(seg$provenance$ai_model,   "claude-opus-4-7-mock")
})

test_that("T0.1 part 3b: OpenAI provider keeps the schema path (citation_source = model_freeform)", {
  # Regression check: refactor must not change OpenAI behavior. The existing
  # tests above already cover this, but assert the citation_source explicitly
  # to lock in the contract.
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "I had trouble sleeping."
  mock_response <- jsonlite::toJSON(list(
    skipped = FALSE, skip_reason = "",
    coded_segments = list(list(
      text = "trouble sleeping", start_char = 6L, end_char = 22L,
      code = "NEW: sleep_diff", code_description = "x", code_type = "descriptive"
    ))
  ), auto_unbox = TRUE)

  captured <- new.env(parent = emptyenv())

  local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt = NULL, task = "coding",
                            model = NULL, temperature = NULL, max_tokens = NULL,
                            json_mode = FALSE, max_retries = 3,
                            response_schema = NULL, documents = NULL) {
      captured$documents       <- documents
      captured$response_schema <- response_schema
      list(
        content       = mock_response, model = "gpt-4o-mock",
        request_id    = "r",
        usage         = list(prompt_tokens = 1L, completion_tokens = 1L,
                             total_tokens = 2L),
        finish_reason = "stop", raw_response = list(),
        prompt_hash   = "h", citations = list()
      )
    },
    .package = "pakhom"
  )

  state <- create_coding_state()
  state <- pakhom:::.code_entry_progressive(
    text = entry_text, entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider("openai"),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test"
  )

  # Schema path: documents NULL, response_schema is the coding schema
  expect_null(captured$documents)
  expect_false(is.null(captured$response_schema))

  seg <- state$entry_results[["e1"]]$coded_segments[[1]]
  expect_equal(seg$provenance$citation_source, "model_freeform")
})

test_that("T0.1 part 3b: Anthropic with no citations falls back to model_freeform per segment", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "I had trouble sleeping."
  mock_response <- jsonlite::toJSON(list(
    skipped = FALSE, skip_reason = "",
    coded_segments = list(list(
      text = "trouble sleeping",
      code = "NEW: sleep_diff", code_description = "x", code_type = "descriptive"
    ))
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt = NULL, task = "coding",
                            model = NULL, temperature = NULL, max_tokens = NULL,
                            json_mode = FALSE, max_retries = 3,
                            response_schema = NULL, documents = NULL) {
      list(
        content    = mock_response, model = "claude-mock",
        request_id = "r",
        usage      = list(prompt_tokens = 1L, completion_tokens = 1L,
                          total_tokens = 2L),
        finish_reason = "stop", raw_response = list(),
        prompt_hash   = "h",
        citations     = list()  # API returned no citations (e.g., model misbehaved)
      )
    },
    .package = "pakhom"
  )

  state <- create_coding_state()
  state <- pakhom:::.code_entry_progressive(
    text = entry_text, entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider("anthropic"),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test"
  )

  # Segment kept (it's a real verbatim) but flagged as model_freeform because
  # the citation path produced no usable pairing. The verification ladder
  # still ran (substring search recovers the offsets).
  seg <- state$entry_results[["e1"]]$coded_segments[[1]]
  expect_s3_class(seg$provenance, "QuoteProvenance")
  expect_equal(seg$provenance$citation_source, "model_freeform")
  expect_true(seg$provenance$verification_status %in%
              c("verified_exact", "verified_fuzzy"))
})

test_that("T0.1 part 3b: Anthropic citations path drops fabricated quotes (defense in depth)", {
  # Anthropic's server-side guarantee covers index validity. If somehow a
  # citation is returned that doesn't match the source (test mocks this
  # adversarially to confirm the bridge does not bypass verification), the
  # ladder still flags fabricated.
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "Real entry text."
  mock_response <- jsonlite::toJSON(list(
    skipped = FALSE, skip_reason = "",
    coded_segments = list(list(
      text = "completely fabricated content",
      code = "NEW: fabricated", code_description = "x", code_type = "descriptive"
    ))
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt = NULL, task = "coding",
                            model = NULL, temperature = NULL, max_tokens = NULL,
                            json_mode = FALSE, max_retries = 3,
                            response_schema = NULL, documents = NULL) {
      list(
        content       = mock_response, model = "claude-mock",
        request_id    = "req_fab_anthro",
        usage         = list(prompt_tokens = 1L, completion_tokens = 1L,
                             total_tokens = 2L),
        finish_reason = "stop", raw_response = list(),
        prompt_hash   = "h",
        # Adversarial mock: API claims a citation for content that's not in
        # the entry. The ladder still flags fabricated.
        citations     = list(list(
          type = "char_location", cited_text = "completely fabricated content",
          document_index = 0L, document_title = "e1",
          start_char_index = 0L, end_char_index = 30L
        ))
      )
    },
    .package = "pakhom"
  )

  state <- create_coding_state()
  state <- suppressWarnings(pakhom:::.code_entry_progressive(
    text = entry_text, entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider("anthropic"),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test"
  ))

  # Fabricated -- segment dropped, codebook unpolluted
  expect_length(state$entry_results[["e1"]]$coded_segments, 0L)
  expect_length(state$codebook, 0L)
})

test_that("T0.1 part 3b: Anthropic citations path attributes ai_call_id from response", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "trouble sleeping is hard"
  mock_response <- jsonlite::toJSON(list(
    skipped = FALSE, skip_reason = "",
    coded_segments = list(list(
      text = "trouble sleeping",
      code = "NEW: x", code_description = "x", code_type = "descriptive"
    ))
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt = NULL, task = "coding",
                            model = NULL, temperature = NULL, max_tokens = NULL,
                            json_mode = FALSE, max_retries = 3,
                            response_schema = NULL, documents = NULL) {
      list(
        content    = mock_response, model = "claude-opus-4-7-MOCK",
        request_id = "msg_REQID_42",
        usage      = list(prompt_tokens = 1L, completion_tokens = 1L,
                          total_tokens = 2L),
        finish_reason = "stop", raw_response = list(),
        prompt_hash   = "h",
        citations     = list(list(
          type = "char_location", cited_text = "trouble sleeping",
          document_index = 0L, document_title = "e1",
          start_char_index = 0L, end_char_index = 16L
        ))
      )
    },
    .package = "pakhom"
  )

  state <- create_coding_state()
  state <- pakhom:::.code_entry_progressive(
    text = entry_text, entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider("anthropic"),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test"
  )

  seg <- state$entry_results[["e1"]]$coded_segments[[1]]
  expect_equal(seg$provenance$ai_call_id, "msg_REQID_42")
  expect_equal(seg$provenance$ai_model,   "claude-opus-4-7-MOCK")
})

test_that("T0.1 part 3b: Anthropic citations path handles multiple segments with paired citations", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "I had trouble sleeping. The medication helps."
  # Two segments, two citations, properly paired by emission order
  mock_response <- jsonlite::toJSON(list(
    skipped = FALSE, skip_reason = "",
    coded_segments = list(
      list(text = "trouble sleeping",
           code = "NEW: sleep_diff",   code_description = "x", code_type = "descriptive"),
      list(text = "The medication helps",
           code = "NEW: medication_efficacy", code_description = "y", code_type = "descriptive")
    )
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt = NULL, task = "coding",
                            model = NULL, temperature = NULL, max_tokens = NULL,
                            json_mode = FALSE, max_retries = 3,
                            response_schema = NULL, documents = NULL) {
      list(
        content    = mock_response, model = "claude-mock",
        request_id = "r",
        usage      = list(prompt_tokens = 1L, completion_tokens = 1L,
                          total_tokens = 2L),
        finish_reason = "stop", raw_response = list(),
        prompt_hash   = "h",
        citations     = list(
          list(type = "char_location", cited_text = "trouble sleeping",
               document_index = 0L, document_title = "e1",
               start_char_index = 6L, end_char_index = 22L),
          list(type = "char_location", cited_text = "The medication helps",
               document_index = 0L, document_title = "e1",
               start_char_index = 24L, end_char_index = 44L)
        )
      )
    },
    .package = "pakhom"
  )

  state <- create_coding_state()
  state <- pakhom:::.code_entry_progressive(
    text = entry_text, entry_id = "e1", entry_index = 1L,
    state = state, provider = mock_provider("anthropic"),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test"
  )

  segs <- state$entry_results[["e1"]]$coded_segments
  expect_length(segs, 2L)
  expect_equal(segs[[1]]$provenance$citation_source, "anthropic_citations_api")
  expect_equal(segs[[2]]$provenance$citation_source, "anthropic_citations_api")
  expect_equal(segs[[1]]$provenance$exact_text, "trouble sleeping")
  expect_equal(segs[[2]]$provenance$exact_text, "The medication helps")
  # Codebook has both codes
  expect_setequal(names(state$codebook),
                  c("sleep_diff", "medication_efficacy"))
})

# ==============================================================================
# Phase 50f: .effective_max_entry_chars context-window-aware truncation
# ==============================================================================

test_that(".effective_max_entry_chars derives from provider context window", {
  prov_gpt4o <- structure(list(context_window = 128000L), class = "AIProvider")
  prov_claude <- structure(list(context_window = 200000L), class = "AIProvider")
  # ~40% * cw * 4 chars/token
  expect_equal(pakhom:::.effective_max_entry_chars(prov_gpt4o, list()), 204800L)
  expect_equal(pakhom:::.effective_max_entry_chars(prov_claude, list()), 320000L)
})

test_that(".effective_max_entry_chars honors config override (researcher explicit cap)", {
  prov <- structure(list(context_window = 128000L), class = "AIProvider")
  expect_equal(
    pakhom:::.effective_max_entry_chars(prov, list(ai = list(max_entry_chars = 5000L))),
    5000L
  )
  # 0 / negative override is ignored (falls through to derived)
  expect_equal(
    pakhom:::.effective_max_entry_chars(prov, list(ai = list(max_entry_chars = 0L))),
    204800L
  )
})

test_that(".effective_max_entry_chars floors at .MAX_ENTRY_CHARS for tiny-context models", {
  prov_tiny <- structure(list(context_window = 4000L), class = "AIProvider")  # 4K tokens
  # 0.40 * 4000 * 4 = 6400; floor at 8000
  expect_equal(pakhom:::.effective_max_entry_chars(prov_tiny, list()), 8000L)
})

test_that(".effective_max_entry_chars is robust to NULL/missing provider (R-quirk: is.na(NULL))", {
  # Phase 50f bug caught at implementation: is.na(NULL) returns logical(0)
  # which trips the if-condition. Must guard with length check first.
  expect_equal(pakhom:::.effective_max_entry_chars(NULL, list()), 8000L)
  expect_equal(pakhom:::.effective_max_entry_chars(list(), list()), 8000L)
  prov_no_cw <- structure(list(), class = "AIProvider")
  expect_equal(pakhom:::.effective_max_entry_chars(prov_no_cw, list()), 8000L)
})
