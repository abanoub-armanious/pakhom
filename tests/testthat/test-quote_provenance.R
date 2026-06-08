# Tests for the quote provenance + verification ladder module
# (T0.1, R/quote_provenance.R)
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
                       # M-13/E-19: failure_reason field
                       # populated on fabricated / drifted; NA otherwise.
                       "verification_failure_reason",
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
  # M-13/E-19: failure_reason column appended after
  # verification_status.
  expect_match(header, "verification_status,failure_reason$")
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

test_that("init_fabrication_log stamps the CSV when methodology_mode is set, and appends survive the stamp", {
  # Audit A HIGH: fabrication_log.csv was a user-facing
  # run-dir artifact that AC4 missed -- the audit log flagged it as
  # the only T0.1 artifact without a methodology stamp.
  td <- withr::local_tempdir()
  flog <- init_fabrication_log(td, methodology_mode = "framework_applied")
  lines <- readLines(flog$path)
  expect_match(lines[1], "^# methodology: M3 - Framework Applied")
  expect_equal(lines[2], "#")
  expect_match(lines[3], "^timestamp,quote_id")

  # Appending a row must not disturb the stamp lines.
  fab <- make_quote("doc1", "test", SRC, 0L, 30L, "fake quote")
  fab <- verify_quote(fab, SRC)
  log_fabrication(flog, fab)
  lines2 <- readLines(flog$path)
  expect_equal(length(lines2), 4L)            # 2 stamp + header + 1 row
  expect_match(lines2[1], "^# methodology:")  # stamp survived append
  expect_match(lines2[3], "^timestamp,quote_id")

  # Reading with comment = "#" gives a parseable tibble (the path a
  # downstream methodology-paper analysis script would use).
  df <- readr::read_csv(flog$path, show_col_types = FALSE, comment = "#")
  expect_equal(nrow(df), 1L)
  expect_true("verification_status" %in% names(df))
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
  # V-5: text updated to "CAUGHT by the verification ladder
  # and DROPPED from the codebook" (was "detected and DROPPED"). Honest
  # post-rejection framing -- the new dashboard distinguishes pre-rejection
  # (caught + dropped) fabrications from the post-rejection (surviving)
  # population. This test simulates a state where the fabricated quote
  # was NOT actually dropped (still attached to a segment), so the n_caught
  # path falls back to counting the surviving fabricated status.
  q1 <- verify_quote(make_quote("d1", "test", SRC, 13L, 27L, QUOTE_TEXT), SRC)
  q_fab <- verify_quote(make_quote("d2", "test", SRC, 0L, 30L, "fake quote"), SRC)
  state <- list(codebook = list(c = list(coded_segments = list(
    list(provenance = q1), list(provenance = q_fab)
  ))))
  md <- pakhom:::.build_tier0_dashboard(compute_quote_provenance_stats(state),
                                            fabrication_log_relpath = "fabrication_log.csv")
  expect_match(md, "1.* fabricated quote attribution was CAUGHT")
  expect_match(md, "\\[fabrication_log.csv\\]\\(fabrication_log.csv\\)")
  expect_match(md, "DROPPED from the codebook")
})

test_that(".build_tier0_dashboard handles singular vs plural correctly", {
  # V-5: singular "1 attribution was CAUGHT" / plural
  # "2 attributions were CAUGHT" (was "1 quote was detected" / "2 quotes
  # were detected").
  q_fab1 <- verify_quote(make_quote("d1", "t", SRC, 0L, 30L, "fake quote one"), SRC)
  state1 <- list(codebook = list(c = list(coded_segments = list(list(provenance = q_fab1)))))
  md1 <- pakhom:::.build_tier0_dashboard(compute_quote_provenance_stats(state1))
  expect_match(md1, "1.* fabricated quote attribution was")

  q_fab2 <- verify_quote(make_quote("d2", "t", SRC, 0L, 30L, "fake quote two"), SRC)
  state2 <- list(codebook = list(c = list(coded_segments = list(
    list(provenance = q_fab1), list(provenance = q_fab2)
  ))))
  md2 <- pakhom:::.build_tier0_dashboard(compute_quote_provenance_stats(state2))
  expect_match(md2, "2.* fabricated quote attributions were")
})

test_that("V-5: dashboard reports pre-rejection fabrication count via fabrication_log_path", {
  # Production callsite (R/17_report.R::build_analysis_report) passes
  # fabrication_log_path so the dashboard counts fabrications the
  # ladder DROPPED during coding (which aren't in the surviving
  # codebook population at all). Without this signal the dashboard
  # reports "No fabrications detected" -- the V-5 false-negative.
  td <- withr::local_tempdir()
  fab_path <- file.path(td, "fabrication_log.csv")
  # Simulate a fabrication log with 3 fabrications + the methodology
  # header + the CSV column header.
  writeLines(c(
    "# methodology_mode: codebook_collaborative",
    "# run_id: smoke-test",
    "timestamp,quote_id,source_doc_id,attributed_theme_id,attributed_code_id,ai_model,ai_call_id,exact_text,verification_status",
    "2026-05-22T00:00:00+0000,q1,d1,NA,c1,gpt-4o,req_001,fake_text_1,fabricated",
    "2026-05-22T00:00:01+0000,q2,d2,NA,c2,gpt-4o,req_002,fake_text_2,fabricated",
    "2026-05-22T00:00:02+0000,q3,d3,NA,c3,gpt-4o,req_003,fake_text_3,fabricated"
  ), fab_path)
  # Surviving population: 100 verified attributions, 0 fabricated
  # (the 3 fabrications never made it into this stats object because
  # they were dropped at coding time).
  stats <- list(
    total = 100L,
    by_status = c(verified_exact = 50L, verified_fuzzy = 50L),
    by_method = c(exact = 50L, substring_search = 50L)
  )
  md <- pakhom:::.build_tier0_dashboard(
    stats, fabrication_log_relpath = "fabrication_log.csv",
    fabrication_log_path = fab_path
  )
  # Now honestly reports 3 caught fabrications + 100 verified survivors.
  expect_match(md, "3.* fabricated quote attributions were CAUGHT")
  expect_match(md, "100.*surviving verbatim claims verified",
               info = "should explicitly state surviving population count")
  expect_no_match(md, "No fabrications detected")
})

test_that("V-5: dashboard reports zero fabrications honestly when none caught", {
  td <- withr::local_tempdir()
  fab_path <- file.path(td, "fabrication_log.csv")
  # Empty fabrication log (header rows only, no data rows).
  writeLines(c(
    "# methodology_mode: codebook_collaborative",
    "timestamp,quote_id,source_doc_id,attributed_theme_id,attributed_code_id,ai_model,ai_call_id,exact_text,verification_status"
  ), fab_path)
  stats <- list(
    total = 50L,
    by_status = c(verified_exact = 25L, verified_fuzzy = 25L),
    by_method = c(exact = 25L, substring_search = 25L)
  )
  md <- pakhom:::.build_tier0_dashboard(
    stats, fabrication_log_relpath = "fabrication_log.csv",
    fabrication_log_path = fab_path
  )
  expect_match(md, "No fabrications detected")
  # Per V-5 fix, the "no fabrications" path also names the surviving population.
  expect_match(md, "50.*AI-attributed verbatim claims")
})

# ==============================================================================
# Anthropic Citations API bridge
# ==============================================================================
# Tests for make_quote_from_citation() and make_quotes_from_citations(),
# which convert Anthropic Citations API output into QuoteProvenance objects
# with citation_source = "anthropic_citations_api". The bridge is the second
# half of T0.1's prevention layer: the provider layer captures citations
# at the source; this bridge produces verifiable QuoteProvenance objects
# from them.

# Helper: a citation pointing at QUOTE_TEXT in SRC.
# Recall SRC = "Hello world. This is the source text." with QUOTE_TEXT at [13, 27)
.cit_char <- function(doc_index = 0L, start = 13L, end = 27L,
                       cited = QUOTE_TEXT, title = "Doc 1") {
  list(
    type             = "char_location",
    cited_text       = cited,
    document_index   = doc_index,
    document_title   = title,
    start_char_index = start,
    end_char_index   = end
  )
}

.docs_one <- function() {
  list(list(id = "doc1", text = SRC, title = "Doc 1"))
}

# ---- make_quote_from_citation -----------------------------------------------

test_that("make_quote_from_citation builds QuoteProvenance with anthropic_citations_api source", {
  q <- make_quote_from_citation(.cit_char(), .docs_one(),
                                 attributed_code_id = "cod_42",
                                 attributed_theme_id = "thm_07",
                                 ai_model = "claude-opus-4-7",
                                 ai_call_id = "msg_test")
  expect_s3_class(q, "QuoteProvenance")
  expect_equal(q$citation_source,    "anthropic_citations_api")
  expect_equal(q$source_doc_id,      "doc1")
  expect_equal(q$source_doc_type,    "data_entry")  # default
  expect_equal(q$start_char,         13L)
  expect_equal(q$end_char,           27L)
  expect_equal(q$exact_text,         QUOTE_TEXT)
  expect_equal(q$attributed_code_id, "cod_42")
  expect_equal(q$attributed_theme_id, "thm_07")
  expect_equal(q$ai_model,           "claude-opus-4-7")
  expect_equal(q$ai_call_id,         "msg_test")
  # Newly-constructed quote starts unverified (caller chains verify_quote)
  expect_equal(q$verification_status, "unverified")
})

test_that("make_quote_from_citation maps document_index 0 to documents[[1]]", {
  docs <- list(
    list(id = "doc_a", text = "first doc text", title = "A"),
    list(id = "doc_b", text = SRC,              title = "B")
  )
  cite <- .cit_char(doc_index = 1L)  # 0-indexed -> documents[[2]]
  q <- make_quote_from_citation(cite, docs)
  expect_equal(q$source_doc_id, "doc_b")
})

test_that("make_quote_from_citation respects per-document type override", {
  docs <- list(list(id = "p1", text = SRC, title = "P1", type = "reddit_post"))
  q <- make_quote_from_citation(.cit_char(), docs)
  expect_equal(q$source_doc_type, "reddit_post")
})

test_that("make_quote_from_citation lets caller override source_doc_type_default", {
  q <- make_quote_from_citation(.cit_char(), .docs_one(),
                                 source_doc_type_default = "interview_segment")
  expect_equal(q$source_doc_type, "interview_segment")
})

test_that("make_quote_from_citation errors on out-of-range document_index", {
  expect_error(
    make_quote_from_citation(.cit_char(doc_index = 5L), .docs_one()),
    "document_index .* out of range"
  )
  expect_error(
    make_quote_from_citation(.cit_char(doc_index = -1L), .docs_one()),
    "document_index .* out of range"
  )
})

test_that("make_quote_from_citation errors on NA document_index", {
  cite <- .cit_char()
  cite$document_index <- NA_integer_
  expect_error(make_quote_from_citation(cite, .docs_one()), "document_index")
})

test_that("make_quote_from_citation errors when documents is missing or empty", {
  expect_error(
    make_quote_from_citation(.cit_char(), list()),
    "non-empty list"
  )
  expect_error(
    make_quote_from_citation(.cit_char(), NULL),
    "non-empty list"
  )
})

test_that("make_quote_from_citation errors when document is missing $id or $text", {
  expect_error(
    make_quote_from_citation(.cit_char(),
                              list(list(text = SRC))),  # missing id
    "missing \\$id or \\$text"
  )
  expect_error(
    make_quote_from_citation(.cit_char(),
                              list(list(id = "x"))),    # missing text
    "missing \\$id or \\$text"
  )
})

test_that("make_quote_from_citation errors with helpful message on page_location", {
  cite <- list(
    type              = "page_location",
    cited_text        = "pdf content",
    document_index    = 0L,
    document_title    = "PDF",
    start_page_number = 1L,
    end_page_number   = 2L
  )
  expect_error(
    make_quote_from_citation(cite, list(list(id = "p", text = "x"))),
    "page_location citations \\(PDF inputs\\) are not yet supported"
  )
})

test_that("make_quote_from_citation errors with helpful message on content_block_location", {
  cite <- list(
    type              = "content_block_location",
    cited_text        = "block content",
    document_index    = 0L,
    document_title    = "Custom",
    start_block_index = 0L,
    end_block_index   = 1L
  )
  expect_error(
    make_quote_from_citation(cite, list(list(id = "p", text = "x"))),
    "content_block_location citations .* are not yet supported"
  )
})

test_that("make_quote_from_citation errors on unknown citation type", {
  cite <- list(
    type           = "future_location_type",
    cited_text     = "x",
    document_index = 0L
  )
  expect_error(
    make_quote_from_citation(cite, .docs_one()),
    "Unknown Anthropic citation type: future_location_type"
  )
})

test_that("make_quote_from_citation errors on missing citation type", {
  expect_error(
    make_quote_from_citation(list(document_index = 0L), .docs_one()),
    "type"
  )
})

# ---- Bridge feeds the verification ladder cleanly --------------------------

test_that("citations bridge -> verify_quote yields verified_exact for honest spans", {
  # End-to-end: cited text matches source at offsets -> ladder step 1 passes
  q <- make_quote_from_citation(.cit_char(), .docs_one())
  q <- verify_quote(q, SRC)
  expect_equal(q$verification_status, "verified_exact")
  expect_equal(q$verification_method, "string_match")
  expect_equal(q$verification_score,  1.0)
})

test_that("citations bridge -> verify_quote catches API misalignment via substring fallback", {
  # Anthropic guarantees indices, but defense in depth: if some future API
  # bug returns a misaligned offset, the ladder's substring step recovers
  # via verified_fuzzy. This test simulates that scenario by constructing
  # the citation with a wrong start_char but a real cited_text.
  cite <- .cit_char(start = 5L, end = 19L, cited = QUOTE_TEXT)
  # SRC at [5, 19) is " world. This i" -- not QUOTE_TEXT
  q <- make_quote_from_citation(cite, .docs_one())
  q <- verify_quote(q, SRC)
  # Strict step fails (offsets don't match exact_text); normalized also
  # fails; substring search finds QUOTE_TEXT in SRC -> verified_fuzzy
  expect_equal(q$verification_status, "verified_fuzzy")
  expect_equal(q$verification_method, "substring_search")
})

test_that("citations bridge -> verify_quote flags genuinely fabricated content as fabricated", {
  # If somehow Anthropic's guarantee is violated (or our test mocks bad
  # behavior), the ladder still marks fabricated. This documents that the
  # bridge does NOT bypass the verification ladder.
  cite <- .cit_char(start = 0L, end = 30L,
                     cited = "this string is not in the source at all xx")
  q <- make_quote_from_citation(cite, .docs_one())
  q <- verify_quote(q, SRC)
  expect_equal(q$verification_status, "fabricated")
})

# ---- make_quotes_from_citations (batch) -------------------------------------

test_that("make_quotes_from_citations returns list in same order as input", {
  cites <- list(
    .cit_char(start = 0L,  end = 5L,  cited = "Hello"),
    .cit_char(start = 13L, end = 27L, cited = QUOTE_TEXT)
  )
  qs <- make_quotes_from_citations(cites, .docs_one(),
                                     ai_model = "m", ai_call_id = "c")
  expect_length(qs, 2L)
  expect_s3_class(qs[[1]], "QuoteProvenance")
  expect_equal(qs[[1]]$exact_text, "Hello")
  expect_equal(qs[[2]]$exact_text, QUOTE_TEXT)
  # Both share the metadata applied uniformly
  expect_equal(qs[[1]]$ai_model, "m")
  expect_equal(qs[[2]]$ai_model, "m")
  expect_equal(qs[[1]]$citation_source, "anthropic_citations_api")
})

test_that("make_quotes_from_citations returns empty list on empty input", {
  expect_identical(make_quotes_from_citations(list(),    .docs_one()), list())
  expect_identical(make_quotes_from_citations(NULL,      .docs_one()), list())
})

test_that("make_quotes_from_citations resolves multi-document corpora correctly", {
  docs <- list(
    list(id = "doc_a", text = "Apple banana cherry.", title = "A"),
    list(id = "doc_b", text = SRC,                    title = "B")
  )
  cites <- list(
    .cit_char(doc_index = 0L, start = 0L,  end = 5L,  cited = "Apple"),
    .cit_char(doc_index = 1L, start = 13L, end = 27L, cited = QUOTE_TEXT)
  )
  qs <- make_quotes_from_citations(cites, docs)
  expect_equal(qs[[1]]$source_doc_id, "doc_a")
  expect_equal(qs[[2]]$source_doc_id, "doc_b")
})

test_that("make_quotes_from_citations propagates errors from individual citations", {
  cites <- list(.cit_char(),
                .cit_char(doc_index = 99L))  # second is out of range
  expect_error(
    make_quotes_from_citations(cites, .docs_one()),
    "document_index"
  )
})

# ==============================================================================
# citation_source breakdown in the summary + Tier-0 dashboard
# ==============================================================================
# T0.1 part 3b dashboard work: quote_provenance_summary now exposes per-source
# counts and per-source verification rates so the dashboard can distinguish
# the PREVENTION layer (Anthropic Citations API: server-side-grounded) from
# the DETECTION-only layer (model_freeform: model wrote a verbatim claim,
# ladder verified offline).

# Helper: build a list of QuoteProvenance objects with the desired
# citation_source / verification_status mix, deterministically.
.q_with_source_status <- function(source, status,
                                    src_id = "doc1",
                                    text = QUOTE_TEXT,
                                    source_text = SRC,
                                    start = 13L, end = 27L) {
  q <- make_quote(src_id, "test", source_text, start, end, text,
                   citation_source = source)
  q$verification_status <- status
  q$verification_method <- if (status == "verified_exact") "string_match"
                            else if (status == "verified_fuzzy") "substring_search"
                            else NA_character_
  q$verification_score  <- if (status == "verified_exact") 1.0
                            else if (status == "verified_fuzzy") 0.85
                            else NA_real_
  q
}

test_that("quote_provenance_summary computes by_citation_source breakdown", {
  quotes <- list(
    .q_with_source_status("anthropic_citations_api", "verified_exact"),
    .q_with_source_status("anthropic_citations_api", "verified_exact"),
    .q_with_source_status("anthropic_citations_api", "verified_fuzzy"),
    .q_with_source_status("model_freeform",          "verified_exact"),
    .q_with_source_status("model_freeform",          "fabricated")
  )
  s <- quote_provenance_summary(quotes)

  # Source counts present and correct
  expect_equal(s$by_citation_source[["anthropic_citations_api"]], 3L)
  expect_equal(s$by_citation_source[["model_freeform"]],          2L)

  # n_citations_api convenience accessor
  expect_equal(s$n_citations_api, 3L)
  # citations_api_rate = 3 / 5
  expect_equal(s$citations_api_rate, 0.6)
})

test_that("quote_provenance_summary computes per-source verification rates", {
  quotes <- list(
    # citations API: 3/3 verified
    .q_with_source_status("anthropic_citations_api", "verified_exact"),
    .q_with_source_status("anthropic_citations_api", "verified_exact"),
    .q_with_source_status("anthropic_citations_api", "verified_fuzzy"),
    # model_freeform: 1/2 verified (1 fabricated)
    .q_with_source_status("model_freeform",          "verified_exact"),
    .q_with_source_status("model_freeform",          "fabricated")
  )
  s <- quote_provenance_summary(quotes)
  expect_equal(s$verification_rate_by_source[["anthropic_citations_api"]], 1.0)
  expect_equal(s$verification_rate_by_source[["model_freeform"]],          0.5)
})

test_that("quote_provenance_summary on empty quotes returns NA rates and zero counts (preserves shape)", {
  s <- quote_provenance_summary(list())
  expect_equal(s$total, 0L)
  expect_equal(s$n_citations_api, 0L)
  expect_identical(s$citations_api_rate, NA_real_)
  expect_length(s$by_citation_source, 0L)
  expect_length(s$verification_rate_by_source, 0L)
})

test_that(".build_tier0_source_block renders citations API first, then alphabetical", {
  s <- list(
    by_citation_source = c(model_freeform = 2L,
                            anthropic_citations_api = 3L,
                            human_supplied = 1L),
    verification_rate_by_source = c(anthropic_citations_api = 1.0,
                                     model_freeform = 0.5,
                                     human_supplied = 1.0)
  )
  block <- pakhom:::.build_tier0_source_block(s)
  expect_match(block, "Citation source breakdown")
  expect_match(block, "Anthropic Citations API")
  expect_match(block, "Model freeform")
  expect_match(block, "Human-supplied")

  # Citations API line appears before model_freeform line
  pos_api <- regexpr("Anthropic Citations API", block)
  pos_freeform <- regexpr("Model freeform", block)
  expect_true(pos_api < pos_freeform)
})

test_that(".build_tier0_source_block shows percentages and verification rates", {
  s <- list(
    by_citation_source = c(anthropic_citations_api = 3L,
                            model_freeform = 1L),
    verification_rate_by_source = c(anthropic_citations_api = 1.0,
                                     model_freeform = 0.0)  # all fabricated
  )
  block <- pakhom:::.build_tier0_source_block(s)
  # 3 of 4 = 75%
  expect_match(block, "75\\.0%")
  # 1 of 4 = 25%
  expect_match(block, "25\\.0%")
  # citations_api: 100% verified
  expect_match(block, "100\\.0% verified")
  # model_freeform: 0% verified
  expect_match(block, "0\\.0% verified")
})

test_that(".build_tier0_source_block returns empty string when no source breakdown", {
  s <- list(by_citation_source = stats::setNames(integer(0), character(0)),
            verification_rate_by_source = stats::setNames(numeric(0), character(0)))
  expect_equal(pakhom:::.build_tier0_source_block(s), "")
})

test_that(".build_tier0_source_block falls back to source name on unknown labels", {
  s <- list(
    by_citation_source = c(future_source = 2L),
    verification_rate_by_source = c(future_source = 0.5)
  )
  block <- pakhom:::.build_tier0_source_block(s)
  expect_match(block, "future_source")
})

test_that(".build_tier0_dashboard now includes citation source breakdown and notes prevention layer", {
  quotes <- list(
    .q_with_source_status("anthropic_citations_api", "verified_exact"),
    .q_with_source_status("anthropic_citations_api", "verified_fuzzy"),
    .q_with_source_status("model_freeform",          "verified_exact")
  )
  s <- quote_provenance_summary(quotes)
  md <- pakhom:::.build_tier0_dashboard(s)

  # Mentions both layers
  expect_match(md, "Anthropic Citations API")
  expect_match(md, "prevention layer")
  expect_match(md, "Citation source breakdown")
  # Contains both source labels
  expect_match(md, "Anthropic Citations API \\(prevention \\+ detection\\)")
  expect_match(md, "Model freeform \\(detection only\\)")
})

# ==============================================================================
# Integration test: end-to-end coding run on Anthropic produces a dashboard
# with citation_source breakdown reflecting the run's prevention-layer use.
# ==============================================================================

test_that("End-to-end (mocked Anthropic): coding run -> stats -> dashboard shows citations API engaged", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "I had trouble sleeping. The medication helps a lot."
  mock_response <- jsonlite::toJSON(list(
    skipped = FALSE, skip_reason = "",
    coded_segments = list(
      list(text = "trouble sleeping",
           code = "NEW: sleep_diff", code_description = "x", code_type = "descriptive"),
      list(text = "The medication helps",
           code = "NEW: medication_efficacy", code_description = "y", code_type = "descriptive")
    )
  ), auto_unbox = TRUE)

  testthat::local_mocked_bindings(
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

  # Pipe through summary + dashboard
  stats <- compute_quote_provenance_stats(state)
  expect_equal(stats$total, 2L)
  expect_equal(stats$n_citations_api, 2L)
  expect_equal(stats$citations_api_rate, 1.0)
  expect_equal(stats$verification_rate_by_source[["anthropic_citations_api"]], 1.0)

  md <- pakhom:::.build_tier0_dashboard(stats)
  expect_match(md, "Anthropic Citations API")
  expect_match(md, "100\\.0% verified")
  # No fabrications
  expect_match(md, "No fabrications detected")
})

test_that("End-to-end (mocked OpenAI): dashboard shows model_freeform path engaged (regression)", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  entry_text <- "I had trouble sleeping after the medication."
  mock_response <- jsonlite::toJSON(list(
    skipped = FALSE, skip_reason = "",
    coded_segments = list(list(
      text = "trouble sleeping", start_char = 6L, end_char = 22L,
      code = "NEW: sleep_diff", code_description = "x", code_type = "descriptive"
    ))
  ), auto_unbox = TRUE)

  testthat::local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt = NULL, task = "coding",
                            model = NULL, temperature = NULL, max_tokens = NULL,
                            json_mode = FALSE, max_retries = 3,
                            response_schema = NULL, documents = NULL) {
      list(
        content    = mock_response, model = "gpt-4o-mock",
        request_id = "r",
        usage      = list(prompt_tokens = 1L, completion_tokens = 1L,
                          total_tokens = 2L),
        finish_reason = "stop", raw_response = list(),
        prompt_hash   = "h",
        citations     = list()
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

  stats <- compute_quote_provenance_stats(state)
  expect_equal(stats$total, 1L)
  expect_equal(stats$n_citations_api, 0L)
  expect_equal(stats$citations_api_rate, 0)
  # by_citation_source has model_freeform = 1
  expect_equal(stats$by_citation_source[["model_freeform"]], 1L)

  md <- pakhom:::.build_tier0_dashboard(stats)
  expect_match(md, "Model freeform \\(detection only\\)")
})
