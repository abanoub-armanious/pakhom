# Tests for the quote provenance + verification ladder module
# (Sprint-4 T0.1, R/quote_provenance.R)
# These tests are the empirical contract for the package's anti-fabrication
# guarantee. Edits should add cases, not weaken assertions.

# Helper: known source text with a quote at known offsets [13, 27)
# H e l l o   w o r l d  .    T  h  i  s     i  s     t  h  e     s  o  ...
# 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27
SRC <- "Hello world. This is the source text."
# substr(src, start_char + 1, end_char) with start_char=13, end_char=27 in
# 0-indexed exclusive-end terms == substr(src, 14, 27) in R 1-indexed inclusive
# == "This is the so" (14 chars).
QUOTE_TEXT <- "This is the so"

# ---- make_quote -------------------------------------------------------------

test_that("make_quote constructs a QuoteProvenance with all schema fields", {
  q <- make_quote(source_doc_id = "doc1", source_doc_type = "reddit_post",
                  source_text = SRC, start_char = 13L, end_char = 27L,
                  exact_text = QUOTE_TEXT)
  expect_s3_class(q, "QuoteProvenance")
  # Schema field presence
  expected_fields <- c("quote_id", "source_doc_id", "source_doc_type",
                       "source_text_sha256", "start_char", "end_char",
                       "exact_text", "ai_paraphrase", "attributed_theme_id",
                       "attributed_code_id", "ai_model", "ai_call_id",
                       "citation_source", "verification_status",
                       "verification_method", "verification_score",
                       "verified_at", "schema_version")
  expect_setequal(names(q), expected_fields)
  # Newly-constructed quote starts unverified
  expect_equal(q$verification_status, "unverified")
  expect_identical(q$verification_method, NA_character_)
  expect_identical(q$verification_score, NA_real_)
})

test_that("make_quote computes deterministic quote_id from positional fingerprint", {
  q1 <- make_quote("doc1", "test", SRC, 13L, 27L, QUOTE_TEXT)
  q2 <- make_quote("doc1", "test", SRC, 13L, 27L, QUOTE_TEXT)
  expect_identical(q1$quote_id, q2$quote_id)
  # Different exact_text -> different ID
  q3 <- make_quote("doc1", "test", SRC, 13L, 27L, "Different text")
  expect_false(identical(q1$quote_id, q3$quote_id))
  # Different doc -> different ID
  q4 <- make_quote("doc2", "test", SRC, 13L, 27L, QUOTE_TEXT)
  expect_false(identical(q1$quote_id, q4$quote_id))
  # Different offsets -> different ID
  q5 <- make_quote("doc1", "test", SRC, 14L, 27L, QUOTE_TEXT)
  expect_false(identical(q1$quote_id, q5$quote_id))
})

test_that("make_quote computes source_text_sha256 over the FULL source text", {
  q1 <- make_quote("doc1", "test", SRC, 13L, 27L, QUOTE_TEXT)
  q2 <- make_quote("doc1", "test", paste0(SRC, " extra"), 13L, 27L, QUOTE_TEXT)
  expect_false(identical(q1$source_text_sha256, q2$source_text_sha256))
  # SHA-256 is 64 hex chars
  expect_equal(nchar(q1$source_text_sha256), 64L)
})

test_that("make_quote validates inputs and rejects malformed ones", {
  expect_error(
    make_quote("doc1", "test", SRC, -1L, 5L, "x"),
    "start_char must be >= 0"
  )
  expect_error(
    make_quote("doc1", "test", SRC, 5L, 5L, "x"),
    "end_char must be > start_char"
  )
  expect_error(
    make_quote("doc1", "test", SRC, 13L, 27L, QUOTE_TEXT,
               citation_source = "made_up_source"),
    "citation_source"
  )
})

# ---- verify_quote ladder steps ---------------------------------------------

test_that("verify_quote step 1: exact string match at recorded offsets -> verified_exact", {
  q <- make_quote("doc1", "test", SRC, 13L, 27L, QUOTE_TEXT)
  v <- verify_quote(q, SRC)
  expect_equal(v$verification_status, "verified_exact")
  expect_equal(v$verification_method, "string_match")
  expect_equal(v$verification_score, 1.0)
  expect_false(is.na(v$verified_at))
})

test_that("verify_quote step 2: normalized match at recorded offsets -> verified_fuzzy", {
  # Source has straight quotes; quote was recorded with smart quotes at the
  # same offsets. Normalized match recovers because chartr collapses smart->straight.
  src <- 'He said "hello" today.'
  # Smart quote variant of quote text at same offsets [8, 15)
  smart <- paste0("“", "hello", "”")
  q <- make_quote("doc1", "test", src, 8L, 15L, smart)
  v <- verify_quote(q, src)
  expect_equal(v$verification_status, "verified_fuzzy")
  expect_equal(v$verification_method, "normalized_match")
  expect_equal(v$verification_score, 0.95)
})

test_that("verify_quote step 2: case-insensitive normalization", {
  src <- "Hello world."
  q <- make_quote("doc1", "test", src, 0L, 5L, "HELLO")
  v <- verify_quote(q, src)
  expect_equal(v$verification_status, "verified_fuzzy")
  expect_equal(v$verification_method, "normalized_match")
})

test_that("verify_quote step 2: whitespace normalization", {
  src <- "Hello world."
  q <- make_quote("doc1", "test", src, 0L, 11L, "Hello   world")
  v <- verify_quote(q, src)
  expect_equal(v$verification_status, "verified_fuzzy")
  expect_equal(v$verification_method, "normalized_match")
})

test_that("verify_quote step 3: substring search recovers from offset drift", {
  # Quote text IS in source but offsets are wrong. Should match via substring.
  q <- make_quote("doc1", "test", SRC, 0L, 5L, "source text")
  v <- verify_quote(q, SRC)
  expect_equal(v$verification_status, "verified_fuzzy")
  expect_equal(v$verification_method, "substring_search")
  expect_equal(v$verification_score, 0.85)
})

test_that("verify_quote ladder fails -> fabricated when source hash matches", {
  # The quote was attributed against the same source we're verifying against,
  # but the text genuinely doesn't exist in the source -> fabrication, not drift.
  q <- make_quote("doc1", "test", SRC, 0L, 30L, "I love unicorns and rainbows")
  v <- verify_quote(q, SRC)
  expect_equal(v$verification_status, "fabricated")
  expect_identical(v$verification_method, NA_character_)
  expect_identical(v$verification_score, NA_real_)
  # verified_at is set even on fabrication so we know when it was checked
  expect_false(is.na(v$verified_at))
})

test_that("verify_quote ladder fails + source hash differs -> drifted", {
  # The quote was attributed against a different source than what we're
  # verifying against. Source has been edited since attribution.
  q <- make_quote("doc1", "test", "ORIGINAL SOURCE TEXT", 0L, 5L, "missing")
  v <- verify_quote(q, "completely different new source text")
  expect_equal(v$verification_status, "drifted")
  expect_identical(v$verification_method, NA_character_)
})

test_that("verify_quote step 4 (embedding) is skipped silently when provider is NULL", {
  # Without provider, ladder stops at step 3. A quote that would only match
  # via embedding falls through to fabricated/drifted as if step 4 didn't exist.
  q <- make_quote("doc1", "test", SRC, 0L, 5L, "paraphrased version")
  v <- verify_quote(q, SRC, provider = NULL)
  expect_equal(v$verification_status, "fabricated")
})

# ---- verify_quotes (batch) -------------------------------------------------

test_that("verify_quotes batch-verifies against a corpus lookup", {
  q1 <- make_quote("doc1", "test", SRC, 13L, 27L, QUOTE_TEXT)
  q2 <- make_quote("doc2", "test", "Different doc text",
                    0L, 9L, "Different")
  q3 <- make_quote("doc3", "test", "Third doc",
                    0L, 5L, "fabricated quote")
  corpus <- list(doc1 = SRC, doc2 = "Different doc text", doc3 = "Third doc")
  results <- verify_quotes(list(q1, q2, q3), corpus)
  expect_equal(results[[1]]$verification_status, "verified_exact")
  expect_equal(results[[2]]$verification_status, "verified_exact")
  expect_equal(results[[3]]$verification_status, "fabricated")
})

test_that("verify_quotes flags missing-from-corpus quotes as drifted", {
  q1 <- make_quote("doc1", "test", SRC, 13L, 27L, QUOTE_TEXT)
  # Corpus does not include doc1 -> drifted (source no longer accessible)
  results <- verify_quotes(list(q1), corpus_lookup = list())
  expect_equal(results[[1]]$verification_status, "drifted")
})

test_that("verify_quotes passes through non-QuoteProvenance items unchanged", {
  q1 <- make_quote("doc1", "test", SRC, 13L, 27L, QUOTE_TEXT)
  results <- verify_quotes(list(q1, "not a quote", 42),
                            corpus_lookup = list(doc1 = SRC))
  expect_equal(results[[1]]$verification_status, "verified_exact")
  # Non-QuoteProvenance items pass through unchanged (no verification_status)
  expect_identical(results[[2]], "not a quote")
  expect_identical(results[[3]], 42)
})

# ---- Fabrication log -------------------------------------------------------

test_that("init_fabrication_log creates the CSV with header row", {
  td <- withr::local_tempdir()
  flog <- init_fabrication_log(td)
  expect_s3_class(flog, "FabricationLog")
  expect_true(file.exists(flog$path))
  header <- readLines(flog$path)[1]
  # Verify header lists the expected columns
  expect_match(header, "timestamp,quote_id,source_doc_id")
  expect_match(header, "verification_status$")
})

test_that("log_fabrication appends one row per fabricated quote", {
  td <- withr::local_tempdir()
  flog <- init_fabrication_log(td)

  fab1 <- make_quote("doc1", "test", SRC, 0L, 30L, "fake quote one")
  fab1 <- verify_quote(fab1, SRC)
  expect_equal(fab1$verification_status, "fabricated")

  fab2 <- make_quote("doc2", "test", SRC, 0L, 30L, "fake quote two")
  fab2 <- verify_quote(fab2, SRC)
  expect_equal(fab2$verification_status, "fabricated")

  log_fabrication(flog, fab1)
  log_fabrication(flog, fab2)

  rows <- readLines(flog$path)
  expect_equal(length(rows), 3L)        # header + 2 rows
  expect_equal(flog$state$n_logged, 2L)
})

test_that("log_fabrication silently no-ops on non-fabricated quotes", {
  # The CSV is for fabrications only. Drifted, unverified, and verified
  # quotes have other render-time treatments and don't go in here.
  td <- withr::local_tempdir()
  flog <- init_fabrication_log(td)

  q_verified <- verify_quote(
    make_quote("doc1", "test", SRC, 13L, 27L, QUOTE_TEXT),
    SRC
  )
  expect_equal(q_verified$verification_status, "verified_exact")

  log_fabrication(flog, q_verified)
  expect_equal(flog$state$n_logged, 0L)
  expect_equal(length(readLines(flog$path)), 1L)  # header only
})

test_that("log_fabrication CSV-quotes values containing commas + quotes", {
  td <- withr::local_tempdir()
  flog <- init_fabrication_log(td)

  # Fabricate a quote whose text contains a comma and a quote -- verify CSV
  # quoting/escaping is RFC 4180 compliant so the row reads back correctly.
  src <- "Some innocent source text without the bad quote."
  fab <- make_quote("doc1", "test", src, 0L, 30L,
                    'Tricky text, with a "comma" and quote')
  fab <- verify_quote(fab, src)
  expect_equal(fab$verification_status, "fabricated")

  log_fabrication(flog, fab)
  rows <- read.csv(flog$path, stringsAsFactors = FALSE)
  expect_equal(nrow(rows), 1L)
  expect_equal(rows$exact_text, 'Tricky text, with a "comma" and quote')
})

test_that("log_fabrication counter survives pass-by-value (env-backed state)", {
  # Same regression as ResponseCache + AuditLog: state must mutate from a
  # callee. Verify by writing from a separate function.
  td <- withr::local_tempdir()
  flog <- init_fabrication_log(td)
  write_n <- function(flog, n) {
    for (i in seq_len(n)) {
      fab <- make_quote(sprintf("d%d", i), "t", "src text", 0L, 5L,
                        sprintf("missing-%d", i))
      fab <- verify_quote(fab, "src text")
      log_fabrication(flog, fab)
    }
  }
  write_n(flog, 4)
  expect_equal(flog$state$n_logged, 4L)
})

# ---- quote_provenance_summary ----------------------------------------------

test_that("quote_provenance_summary aggregates by status + computes rates", {
  q1 <- verify_quote(make_quote("d1", "t", SRC, 13L, 27L, QUOTE_TEXT), SRC)  # verified_exact
  q2 <- verify_quote(make_quote("d1", "t", SRC, 0L,  5L, "Hello"), SRC)        # verified_exact
  q3 <- verify_quote(make_quote("d1", "t", SRC, 0L, 30L, "Hello world."), SRC) # verified_fuzzy via substring
  q4 <- verify_quote(make_quote("d1", "t", SRC, 0L, 10L, "fake quote"), SRC)   # fabricated
  q5 <- verify_quote(make_quote("d1", "t", "OLD SRC", 0L, 5L, "missing"),
                      "DIFFERENT SOURCE")                                       # drifted

  summary <- quote_provenance_summary(list(q1, q2, q3, q4, q5))
  expect_equal(summary$total, 5L)
  expect_equal(unname(summary$by_status[["verified_exact"]]), 2L)
  expect_equal(unname(summary$by_status[["verified_fuzzy"]]), 1L)
  expect_equal(unname(summary$by_status[["fabricated"]]), 1L)
  expect_equal(unname(summary$by_status[["drifted"]]), 1L)
  # verification_rate counts BOTH verified_exact and verified_fuzzy as verified
  expect_equal(summary$verification_rate, 3/5)
  expect_equal(summary$fabrication_rate, 1/5)
  expect_equal(summary$drift_rate, 1/5)
})

test_that("quote_provenance_summary returns empty-shape on zero quotes", {
  summary <- quote_provenance_summary(list())
  expect_equal(summary$total, 0L)
  expect_length(summary$by_status, 0L)
  expect_identical(summary$verification_rate, NA_real_)
  expect_identical(summary$fabrication_rate, NA_real_)
})

# ---- print method ----------------------------------------------------------

test_that("print.QuoteProvenance reports key fields without error", {
  q <- verify_quote(
    make_quote("doc1", "test", SRC, 13L, 27L, QUOTE_TEXT),
    SRC
  )
  expect_output(print(q), "QuoteProvenance")
  expect_output(print(q), "verified_exact")
  expect_output(print(q), "doc1")
})

# ---- compute_quote_provenance_stats (T0.1 part 3 dashboard input) -----------

test_that("compute_quote_provenance_stats walks coding_state and aggregates", {
  # Build a fake coding_state shaped like .code_entry_progressive's output:
  # codebook entries with coded_segments, each segment optionally carrying
  # $provenance.
  q1 <- verify_quote(make_quote("d1", "test", SRC, 13L, 27L, QUOTE_TEXT), SRC)
  q2 <- verify_quote(make_quote("d2", "test", SRC, 0L,  5L, "Hello"), SRC)
  q3 <- verify_quote(make_quote("d3", "test", SRC, 0L, 30L, "fake quote"), SRC)
  state <- list(
    codebook = list(
      code_a = list(coded_segments = list(
        list(text = QUOTE_TEXT, provenance = q1)
      )),
      code_b = list(coded_segments = list(
        list(text = "Hello",  provenance = q2),
        list(text = "fake quote", provenance = q3)  # fabricated; should still aggregate
      ))
    )
  )
  stats <- compute_quote_provenance_stats(state)
  expect_equal(stats$total, 3L)
  expect_equal(unname(stats$by_status[["verified_exact"]]), 2L)
  expect_equal(unname(stats$by_status[["fabricated"]]), 1L)
  expect_equal(stats$verification_rate, 2/3)
  expect_equal(stats$fabrication_rate, 1/3)
})

test_that("compute_quote_provenance_stats returns empty-summary on NULL or missing coding_state", {
  empty1 <- compute_quote_provenance_stats(NULL)
  expect_equal(empty1$total, 0L)
  expect_identical(empty1$verification_rate, NA_real_)

  empty2 <- compute_quote_provenance_stats(list(codebook = list()))
  expect_equal(empty2$total, 0L)
})

test_that("compute_quote_provenance_stats skips segments missing $provenance (pre-T0.1 back-compat)", {
  # Pre-T0.1 coding states don't have $provenance on segments. The aggregator
  # should skip them rather than crash, returning an empty summary if no
  # segments have provenance.
  state <- list(
    codebook = list(
      legacy_code = list(coded_segments = list(
        list(text = "no provenance here"),
        list(text = "also no provenance")
      ))
    )
  )
  stats <- compute_quote_provenance_stats(state)
  expect_equal(stats$total, 0L)
})

# ---- .build_tier0_dashboard (T0.1 part 3 report rendering) ----------------

test_that(".build_tier0_dashboard renders the empty-state notice when no quotes", {
  empty <- compute_quote_provenance_stats(NULL)
  md <- pakhom:::.build_tier0_dashboard(empty)
  expect_match(md, "Data Integrity Dashboard")
  expect_match(md, "did not run for this report")
  expect_match(md, 'class="tier0-dashboard tier0-empty"')
})

test_that(".build_tier0_dashboard renders verified counts + method breakdown", {
  q1 <- verify_quote(make_quote("d1", "test", SRC, 13L, 27L, QUOTE_TEXT), SRC)
  q2 <- verify_quote(make_quote("d2", "test", SRC, 0L,  5L, "Hello"), SRC)
  state <- list(codebook = list(c = list(coded_segments = list(
    list(provenance = q1), list(provenance = q2)
  ))))
  md <- pakhom:::.build_tier0_dashboard(compute_quote_provenance_stats(state))
  expect_match(md, "\\*\\*2\\*\\*.*verbatim claims were checked")
  expect_match(md, "verified.*100\\.0%")
  expect_match(md, "No fabrications detected")
})

test_that(".build_tier0_dashboard renders fabrication CSV link when fabrications occurred", {
  q1 <- verify_quote(make_quote("d1", "test", SRC, 13L, 27L, QUOTE_TEXT), SRC)
  q_fab <- verify_quote(make_quote("d2", "test", SRC, 0L, 30L, "fake quote"), SRC)
  state <- list(codebook = list(c = list(coded_segments = list(
    list(provenance = q1), list(provenance = q_fab)
  ))))
  md <- pakhom:::.build_tier0_dashboard(compute_quote_provenance_stats(state),
                                            fabrication_log_relpath = "fabrication_log.csv")
  expect_match(md, "1.* fabricated quote was detected")
  expect_match(md, "\\[fabrication_log.csv\\]\\(fabrication_log.csv\\)")
  expect_match(md, "DROPPED from the codebook")
})

test_that(".build_tier0_dashboard handles singular vs plural correctly", {
  # Singular "1 fabricated quote was" / plural "2 fabricated quotes were"
  q_fab1 <- verify_quote(make_quote("d1", "t", SRC, 0L, 30L, "fake quote one"), SRC)
  state1 <- list(codebook = list(c = list(coded_segments = list(list(provenance = q_fab1)))))
  md1 <- pakhom:::.build_tier0_dashboard(compute_quote_provenance_stats(state1))
  expect_match(md1, "1.* fabricated quote was")

  q_fab2 <- verify_quote(make_quote("d2", "t", SRC, 0L, 30L, "fake quote two"), SRC)
  state2 <- list(codebook = list(c = list(coded_segments = list(
    list(provenance = q_fab1), list(provenance = q_fab2)
  ))))
  md2 <- pakhom:::.build_tier0_dashboard(compute_quote_provenance_stats(state2))
  expect_match(md2, "2.* fabricated quotes were")
})
