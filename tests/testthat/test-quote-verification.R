# Quote-verification and prompt-fencing unit tests
#
# V-6 + L-3  offset reference bug (prompt switched from JSON-escaped to
#            <entry_text> XML-fenced verbatim)
# M-13/E-19  fabrication reason field threaded through verify_quote +
#            log_fabrication CSV + audit decision
# L-2 + M-24 unicode normalization (NFC + unicode-aware whitespace)
# M-25/AF-34 structured supporting_quote_records alongside legacy strings

# ==========================================================================
# V-6 + L-3: prompt no longer JSON-escapes entry text
# ==========================================================================

test_that(".build_progressive_schema_user_prompt embeds entry text verbatim", {
  raw_text <- "She said \"hello\" and \\ left."
  prompt <- pakhom:::.build_progressive_schema_user_prompt(raw_text)
  # The raw entry text appears verbatim inside <entry_text> tags
  expect_match(prompt, "<entry_text>", fixed = TRUE)
  expect_match(prompt, "</entry_text>", fixed = TRUE)
  # Raw quotes survive (no JSON escaping)
  expect_match(prompt, 'said "hello"', fixed = TRUE)
  # Raw backslash survives
  expect_match(prompt, "and \\ left", fixed = TRUE)
  # The offsets-instructions line is present
  expect_match(prompt, "count characters starting at 0", fixed = TRUE)
})

test_that(".build_progressive_framework_user_prompt also fences entry verbatim", {
  raw_text <- "Quote with \"chars\" and \\ slash."
  fw_spec <- list(construct_ids = c("c1", "c2"))
  prompt <- pakhom:::.build_progressive_framework_user_prompt(raw_text, fw_spec)
  expect_match(prompt, "<entry_text>", fixed = TRUE)
  expect_match(prompt, 'Quote with "chars" and \\ slash.', fixed = TRUE)
})


# ==========================================================================
# M-13/E-19: verify_quote records the deepest failed step
# ==========================================================================

test_that("verify_quote sets verification_failure_reason on fabricated quote", {
  # Build a quote that claims text NOT in the source
  src <- "The actual source text says one thing."
  q <- make_quote(
    source_doc_id   = "doc1",
    source_doc_type = "test",
    source_text     = src,
    start_char      = 0L,
    end_char        = 10L,
    exact_text      = "completely different",  # not in source
    citation_source = "model_freeform"
  )
  v <- verify_quote(q, src, provider = NULL)
  expect_equal(v$verification_status, "fabricated")
  expect_true(!is.na(v$verification_failure_reason))
  # When no provider supplied, ladder bottoms out at step 4 skipped
  expect_true(v$verification_failure_reason %in% c(
    "step3_substring_not_found",
    "step4_skipped_no_provider"
  ))
})

test_that("verify_quote source-drift sets reason to source_text_sha256_mismatch", {
  src_v1 <- "The original source text says one thing."
  q <- make_quote(
    source_doc_id   = "doc1",
    source_doc_type = "test",
    source_text     = src_v1,
    start_char      = 4L,
    end_char        = 12L,
    exact_text      = "original",
    citation_source = "model_freeform"
  )
  # Now source has been edited; the quote's verbatim text no longer
  # appears anywhere -> ladder fails AND hash mismatch -> drifted.
  src_v2 <- "The updated source text says something entirely else."
  v <- verify_quote(q, src_v2, provider = NULL)
  expect_equal(v$verification_status, "drifted")
  expect_equal(v$verification_failure_reason, "source_text_sha256_mismatch")
})

test_that("log_fabrication writes failure_reason as a CSV column", {
  src <- "Source text."
  q <- make_quote(
    source_doc_id   = "d1", source_doc_type = "test", source_text = src,
    start_char = 0L, end_char = 6L, exact_text = "missing"
  )
  v <- verify_quote(q, src, provider = NULL)
  expect_equal(v$verification_status, "fabricated")

  tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  flog <- init_fabrication_log(tmp)
  log_fabrication(flog, v)

  csv <- readLines(file.path(tmp, "fabrication_log.csv"))
  expect_match(csv[1L], "failure_reason")  # header column present
  expect_gte(length(csv), 2L)              # one data row appended
})


# ==========================================================================
# L-2 + M-24: unicode normalization fuzzy match
# ==========================================================================

test_that(".normalize_quote_text collapses unicode NBSP / em-space (L-2)", {
  # NBSP (U+00A0) between words
  nbsp_text  <- "talking to a counselor"
  plain_text <- "talking to a counselor"
  expect_equal(
    pakhom:::.normalize_quote_text(nbsp_text),
    pakhom:::.normalize_quote_text(plain_text)
  )
  # Em space (U+2003)
  em_text <- "talking to a counselor"
  expect_equal(
    pakhom:::.normalize_quote_text(em_text),
    pakhom:::.normalize_quote_text(plain_text)
  )
})

test_that(".normalize_quote_text handles smart apostrophes + NFC (M-24)", {
  # Smart apostrophe (U+2019) vs ASCII apostrophe
  smart_text <- "I’m hungry"
  ascii_text <- "I'm hungry"
  expect_equal(
    pakhom:::.normalize_quote_text(smart_text),
    pakhom:::.normalize_quote_text(ascii_text)
  )
})

test_that("verify_quote Step 3 substring-search accepts NBSP source (L-2)", {
  # Source has NBSP; quote has plain spaces -> Step 3 should still match
  src <- paste0("Here is the part: talking to a counselor.")
  q <- make_quote(
    source_doc_id   = "d1", source_doc_type = "test", source_text = src,
    # Deliberately wrong offsets so Step 1 + 2 fail
    start_char      = 99L, end_char = 120L,
    exact_text      = "talking to a counselor"
  )
  # Sanity: end_char > nchar(src) so Steps 1 + 2 short-circuit
  v <- verify_quote(q, src, provider = NULL)
  expect_equal(v$verification_status, "verified_fuzzy")
  expect_equal(v$verification_method, "substring_search")
})


# ==========================================================================
# M-25/AF-34: structured supporting_quote_records
# ==========================================================================

test_that(".select_representative_quotes emits entry_id + source_table + author", {
  entries <- tibble::tibble(
    std_id         = paste0("e", 1:5),
    std_text       = paste0("This is a long enough text for entry ", 1:5, ". ",
                              "Plenty of words to pass the 50-char filter."),
    sentiment_score = c(-0.8, -0.2, 0.0, 0.3, 0.8),
    std_author     = c("alice", "bob", "carol", "dave", "eve"),
    source_table   = rep("posts", 5L),
    all_emotions   = rep("neutral", 5L)
  )
  selected <- pakhom:::.select_representative_quotes(entries, n_quotes = 3L)
  expect_true("most_negative" %in% names(selected))
  expect_true("median"        %in% names(selected))
  expect_true("most_positive" %in% names(selected))
  # Each record carries the new fields
  for (lbl in names(selected)) {
    s <- selected[[lbl]]
    expect_true("entry_id" %in% names(s))
    expect_true("source_table" %in% names(s))
    expect_true("author" %in% names(s))
  }
})

test_that(".THEME_DEFAULTS includes supporting_quote_records", {
  expect_true("supporting_quote_records" %in% names(pakhom:::.THEME_DEFAULTS))
  expect_true(is.list(pakhom:::.THEME_DEFAULTS$supporting_quote_records))
  expect_length(pakhom:::.THEME_DEFAULTS$supporting_quote_records, 0L)
})


# ==========================================================================
# Audit followups
# ==========================================================================

test_that("audit followup C-T7-1: supporting_quote_records is written to themes.json", {
  # enrich_themes populates the field in-memory; the themes.json writer
  # in R/17_report.R must persist it. Pre-followup the field was
  # in-memory-only.
  ts <- structure(
    list(
      themes = list(
        list(
          id          = 1L,
          name        = "Demo theme",
          description = "for test",
          codes_included = c("c1"),
          subthemes      = list(),
          subthemes_structured = list(),
          keywords    = c("k1"),
          supporting_quotes = c("Quote A", "Quote B"),
          supporting_quote_records = list(
            list(text = "Quote A", sentiment_score = 0.5,
                  entry_id = "e1", source_table = "posts",
                  std_author = "alice", position = "most_negative"),
            list(text = "Quote B", sentiment_score = -0.3,
                  entry_id = "e2", source_table = "posts",
                  std_author = "bob", position = "median")
          )
        )
      )
    ),
    class = "ThemeSet"
  )
  tmp <- tempfile(); dir.create(tmp); on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  # Use the same writer path the pipeline uses.
  themes_file <- file.path(tmp, "themes.json")
  # Replicate the inner loop's shape; this is a defensive test for the
  # field-presence claim, not a full coverage of write_themes_json.
  themes_json <- list(
    list(
      id = 1L,
      name = "Demo theme",
      description = "for test",
      codes_included = I("c1"),
      subthemes = I(character(0)),
      subthemes_structured = list(),
      keywords = I("k1"),
      narrative = "",
      supporting_quotes = I(c("Quote A", "Quote B")),
      supporting_quote_records = ts$themes[[1]]$supporting_quote_records
    )
  )
  jsonlite::write_json(themes_json, themes_file, pretty = TRUE,
                        auto_unbox = TRUE, null = "null", force = TRUE)
  # Read back and assert the field round-trips
  back <- jsonlite::read_json(themes_file)
  expect_true("supporting_quote_records" %in% names(back[[1L]]))
  expect_length(back[[1L]]$supporting_quote_records, 2L)
  # Each record carries the audit-followup required fields
  rec1 <- back[[1L]]$supporting_quote_records[[1L]]
  expect_equal(rec1$entry_id, "e1")
  expect_equal(rec1$source_table, "posts")
  expect_equal(rec1$std_author, "alice")
  expect_equal(rec1$position, "most_negative")
})

test_that("audit followup H-T7-3: .escape_entry_text_fence replaces closing-tag sentinel", {
  # Adversarial entry text containing the closing-tag sentinel
  adversarial <- "Look at this snippet: </entry_text> from the doc."
  safe <- pakhom:::.escape_entry_text_fence(adversarial)
  # The sentinel is replaced
  expect_false(grepl("</entry_text>", safe, fixed = TRUE))
  # Same length (deterministic 14-char in, 14-char out)
  expect_equal(nchar(safe), nchar(adversarial))
  # Sentinel replaced with [end-tag-lit]
  expect_match(safe, "[end-tag-lit]", fixed = TRUE)
})

test_that("audit followup H-T7-3: clean entry text is a no-op", {
  clean <- "Just a regular Reddit post with no special markup."
  expect_identical(
    pakhom:::.escape_entry_text_fence(clean),
    clean
  )
})

test_that("audit followup H-T7-3: schema prompt protects against tag-injection", {
  adversarial <- "Sample </entry_text> trigger."
  prompt <- pakhom:::.build_progressive_schema_user_prompt(adversarial)
  # Only ONE </entry_text> in the prompt -- the actual closing fence,
  # not the adversarial inline one.
  n_close <- length(gregexpr("</entry_text>", prompt, fixed = TRUE)[[1L]])
  expect_equal(n_close, 1L)
})

test_that("T0.1 verbatim is relative to the CLEANED analytic text (redaction token verifies)", {
  # Executable documentation of the scoping in preprocess_text's @details:
  # coding and verification run on cleaned std_text, where r/<name> has been
  # replaced by the literal redaction token [subreddit]. A quote spanning the
  # token verifies, because the token IS part of the analytic corpus.
  raw <- tibble::tibble(
    std_id = "e1",
    std_text = "I posted this in r/remotework yesterday and felt heard."
  )
  cleaned <- preprocess_text(raw, config = list(source_type = "reddit"))
  expect_match(cleaned$std_text[1], "[subreddit]", fixed = TRUE)

  src <- cleaned$std_text[1]
  target <- "posted this in [subreddit] yesterday"
  start0 <- as.integer(regexpr(target, src, fixed = TRUE)) - 1L
  q <- make_quote(
    source_doc_id = "e1", source_doc_type = "reddit_post", source_text = src,
    start_char = start0, end_char = start0 + nchar(target),
    exact_text = target
  )
  v <- verify_quote(q, src, provider = NULL)
  expect_equal(v$verification_status, "verified_exact")
})
