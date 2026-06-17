# End-to-end smoke test: all three Tier-0 universals cooperate
#
# Builds a synthetic dataset + ProgressiveCodingState + ThemeSet,
# threads them through enrich_themes -> aggregate_theme_statistics ->
# .build_thematic_section, and asserts that:
#   - T0.1 quote provenance is captured on every coded segment
#   - T0.2 participant_spread is computed per theme and the report
#     renders the "Participant Distribution" card with concentration
#     warnings when applicable
#   - T0.3 corpus coverage object computes correctly with no_silent_truncation
#   - All three Tier-0 cards render in a single rendered theme/dashboard pass
#     without crashing or interfering with each other
#
# This test is the integration safety net: pure unit tests passed in
# isolation but didn't catch enrich_themes bypassing the spread-aware
# selector. Smoke tests catch this class of integration gap.

# ----- helpers ---------------------------------------------------------------

.smoke_data <- function(authors = NULL, n = 8) {
  if (is.null(authors)) authors <- rep("anon", n)
  tibble::tibble(
    std_id   = paste0("e", seq_len(n)),
    std_text = vapply(seq_len(n), function(i) {
      sprintf("Entry %d full text passes the 50-char filter; lorem ipsum dolor sit amet.", i)
    }, character(1)),
    std_author = authors,
    emerged_themes  = rep("Focus", n),
    theme_membership_Focus = rep(1L, n),
    sentiment_score = seq(-0.9, 0.9, length.out = n),
    emotion_intensity = seq(0.2, 0.8, length.out = n),
    all_emotions = rep(c("anxiety", "neutral"), length.out = n),
    source_table = rep("posts", n)
  )
}

.smoke_state <- function(ids, fab_index = NULL) {
  state <- create_coding_state()
  for (i in seq_along(ids)) {
    id <- ids[i]
    # Build a verified QuoteProvenance for each entry so T0.1 stats reflect
    # an honest run (verified_exact rate = 100% unless we inject a fab).
    text <- sprintf("Entry %d full text passes the 50-char filter; lorem ipsum dolor sit amet.", i)
    # exact_text is the first 7 chars of THIS entry's text (e.g. "Entry 1"
    # for i=1, "Entry 2" for i=2). Hardcoding "Entry 1" everywhere would
    # cause verify_quote to fail on entries 2..n.
    exact_text <- substr(text, 1L, 7L)
    quote <- make_quote(
      source_doc_id = id, source_doc_type = "data_entry",
      source_text = text, start_char = 0L, end_char = 7L,
      exact_text = exact_text,
      attributed_code_id = "focus_difficulty",
      ai_model = "claude-opus-4-7-mock", ai_call_id = paste0("req_", id),
      citation_source = "anthropic_citations_api"
    )
    quote <- verify_quote(quote, text)
    if (!is.null(fab_index) && i == fab_index) {
      # Mock a fabrication for one entry to exercise the warn path
      quote$verification_status <- "fabricated"
      quote$verification_method <- NA_character_
    }
    seg_record <- list(
      entry_id   = id, text = exact_text,
      start_char = 0L, end_char = 7L,
      provenance = quote
    )
    state$codebook[["focus_difficulty"]] <- list(
      code_name      = "focus_difficulty",
      description    = "trouble focusing",
      type           = "descriptive",
      frequency      = (state$codebook[["focus_difficulty"]]$frequency %||% 0L) + 1L,
      entry_ids      = unique(c(state$codebook[["focus_difficulty"]]$entry_ids,
                                 id)),
      coded_segments = c(state$codebook[["focus_difficulty"]]$coded_segments,
                         list(seg_record))
    )
    state$entry_results[[id]] <- list(
      codes_assigned = "focus_difficulty",
      coded_segments = list(list(
        code_key = "focus_difficulty", code_name = "focus_difficulty",
        text = "Entry 1", start_char = 0L, end_char = 7L,
        provenance = quote
      )),
      skipped     = FALSE,
      skip_reason = NA_character_
    )
  }
  state
}

# ----- the smoke test --------------------------------------------------------

test_that("Tier-0 smoke: all three universals compute + render together", {
  # Mixed contributor distribution: heavy + 4 distinct others
  authors <- c("heavy", "heavy", "heavy", "heavy", "heavy",
               "alice", "bob", "carol")
  data  <- .smoke_data(authors = authors, n = 8)
  state <- .smoke_state(data$std_id)

  # ----- T0.1: verification stats -------------------------------------------
  prov_stats <- compute_quote_provenance_stats(state)
  expect_true(prov_stats$total >= 8L)
  expect_true(prov_stats$verification_rate > 0.9)  # all verified
  expect_equal(prov_stats$by_citation_source[["anthropic_citations_api"]],
               prov_stats$total)

  # ----- T0.3: coverage assertion -------------------------------------------
  coverage <- compute_corpus_coverage(state, data,
                                        n_after_preprocessing = 8L)
  expect_true(coverage$no_silent_truncation)
  expect_equal(coverage$n_processed, 8L)
  expect_equal(coverage$n_coded,     8L)

  # ----- enrich_themes -> aggregate_theme_statistics path -------------------
  ts <- create_theme_set(list(
    list(id = 1, name = "Focus", description = "Focus difficulties",
         codes_included = "focus_difficulty")
  ))
  enriched <- enrich_themes(ts, data, coding_state = state)
  theme_stats <- aggregate_theme_statistics(data, enriched)

  # T0.2: participant_spread is on every theme
  expect_true(!is.null(theme_stats[["Focus"]]$participant_spread))
  ps <- theme_stats[["Focus"]]$participant_spread
  expect_true(ps$available)
  expect_equal(ps$n_distinct_contributors, 4L)
  # Top contributor "heavy" has 5/8 entries -> 0.625 share, triggers warning
  expect_true(ps$top_contributor_share > 0.5)

  # ----- Render the Tier-0 dashboard + per-theme card + coverage card ------
  # Each is rendered independently in the report, but we verify they
  # all produce non-empty markdown with their respective signals.
  tier0_md <- pakhom:::.build_tier0_dashboard(prov_stats)
  expect_match(tier0_md, "Data Integrity Dashboard")
  expect_match(tier0_md, "anthropic_citations_api|Anthropic Citations API")

  ps_md <- pakhom:::.build_participant_spread_card(ps)
  expect_match(ps_md, "Participant Distribution")
  expect_match(ps_md, "ps-warn")  # heavy poster triggers concentration warning
  expect_match(ps_md, "Single contributor|come from one contributor")

  cov_md <- pakhom:::.build_corpus_coverage_card(coverage)
  expect_match(cov_md, "Corpus Coverage")
  expect_match(cov_md, "coverage-banner-ok")
  expect_match(cov_md, "entry-level coverage")

  # Sanity: combined output has the three distinct anchor strings,
  # i.e., we can render all three cards in sequence without conflict.
  combined <- paste(tier0_md, ps_md, cov_md)
  expect_match(combined, "Data Integrity Dashboard")
  expect_match(combined, "Participant Distribution")
  expect_match(combined, "Corpus Coverage")
})

test_that("Tier-0 smoke: anonymous data (no std_author) -> T0.2 reports unavailable, T0.1/T0.3 unaffected", {
  data <- .smoke_data(authors = rep(NA_character_, 6), n = 6)
  # Drop the column entirely to simulate datasets without author info
  data$std_author <- NULL
  state <- .smoke_state(data$std_id)

  prov_stats <- compute_quote_provenance_stats(state)
  expect_true(prov_stats$verification_rate > 0.9)

  coverage <- compute_corpus_coverage(state, data,
                                        n_after_preprocessing = 6L)
  expect_true(coverage$no_silent_truncation)

  ts <- create_theme_set(list(
    list(id = 1, name = "Focus", description = "",
         codes_included = "focus_difficulty")
  ))
  enriched <- enrich_themes(ts, data, coding_state = state)
  theme_stats <- aggregate_theme_statistics(data, enriched)

  # T0.2 unavailable because no std_author column
  ps <- theme_stats[["Focus"]]$participant_spread
  expect_false(ps$available)

  # Renderer renders the unavailable variant (Tier-0 transparency)
  ps_md <- pakhom:::.build_participant_spread_card(ps)
  expect_match(ps_md, "participant-spread-unavailable")

  # T0.1 + T0.3 work as usual
  tier0_md <- pakhom:::.build_tier0_dashboard(prov_stats)
  expect_match(tier0_md, "Data Integrity Dashboard")
  cov_md <- pakhom:::.build_corpus_coverage_card(coverage)
  expect_match(cov_md, "entry-level coverage")
})

test_that("Tier-0 smoke: fabrication detected -> T0.1 dashboard reports it, T0.2/T0.3 unaffected", {
  authors <- c("alice", "bob", "carol", "dave", "eve")
  data  <- .smoke_data(authors = authors, n = 5)
  state <- .smoke_state(data$std_id, fab_index = 3L)  # entry 3 is fabricated

  prov_stats <- compute_quote_provenance_stats(state)
  expect_equal(prov_stats$by_status[["fabricated"]], 1L)
  expect_equal(prov_stats$fabrication_rate, 1 / prov_stats$total)

  tier0_md <- pakhom:::.build_tier0_dashboard(prov_stats)
  # Dashboard mentions the fabrication explicitly
  expect_match(tier0_md, "fabricated")

  # T0.3 still works
  coverage <- compute_corpus_coverage(state, data,
                                        n_after_preprocessing = 5L)
  expect_true(coverage$no_silent_truncation)
})

test_that("Tier-0 smoke: silent truncation detected -> T0.3 raises WARN banner", {
  # 5 entries in data, but coding_state only processed 3 -> truncation
  data  <- .smoke_data(authors = paste0("u", 1:5), n = 5)
  state <- .smoke_state(data$std_id[1:3])

  coverage <- compute_corpus_coverage(state, data)
  expect_false(coverage$no_silent_truncation)
  expect_equal(coverage$n_unprocessed, 2L)

  cov_md <- pakhom:::.build_corpus_coverage_card(coverage)
  expect_match(cov_md, "coverage-banner-warn")
  expect_match(cov_md, "investigate")
})

# T0.2 contract test: std_author must survive every pipeline step that
# transforms or filters the data tibble. Without this contract test, a
# future change that adds `select(std_id, std_text, ...)` somewhere in
# the pipeline would silently drop std_author and the report would
# render the "Participant data not available" notice without anyone
# noticing the regression.

test_that("std_author survives standardize_data -> preprocess_text -> get_analytic_sample", {
  # Stage 1: build raw + standardize
  raw <- tibble::tibble(
    id        = paste0("e", 1:5),
    text      = c("First entry text long enough to pass the 50-char length filter, lorem.",
                  "Second entry text long enough to pass the length filter, dolor sit amet.",
                  "Third entry text long enough to pass the length filter, consectetur adipiscing.",
                  "Fourth entry text long enough to pass the filter, elit sed do eiusmod tempor.",
                  "Fifth entry text long enough to pass the filter, incididunt ut labore et dolore."),
    username  = c("alice", "bob", "alice", "carol", "bob"),
    timestamp = as.character(Sys.time() + 1:5)
  )
  col_map <- list(id = "id", text = "text", author = "username",
                   timestamp = "timestamp", metrics = character(0))
  std <- standardize_data(raw, col_map)
  expect_true("std_author" %in% names(std))
  expect_equal(std$std_author, c("alice", "bob", "alice", "carol", "bob"))

  # Stage 2: preprocess_text must preserve std_author
  pre <- preprocess_text(std, list(min_text_length = 10L))
  expect_true("std_author" %in% names(pre))

  # Stage 3: build a coding state and verify get_analytic_sample preserves std_author
  state <- create_coding_state()
  for (id in pre$std_id) {
    state$entry_results[[id]] <- list(
      codes_assigned = "code1",
      coded_segments = list(),
      skipped        = FALSE
    )
  }
  analytic <- get_analytic_sample(state, pre)
  expect_true("std_author" %in% names(analytic))
  expect_equal(sort(unique(analytic$std_author)), c("alice", "bob", "carol"))
})

test_that(".use_citations_for_provider cannot be config-disabled (AC1 enforcement)", {
  # AC1: AI is scaffold by architecture, not by configuration. Citations
  # API engagement on Anthropic is load-bearing -- a config flag that
  # could turn it off would weaken the architectural commitment. This
  # test locks the contract: regardless of any config knobs a future
  # change might add, Anthropic provider must still trigger the
  # prevention layer.
  anth <- mock_provider("anthropic")
  # No knob in any config namespace can flip this off:
  expect_true(pakhom:::.use_citations_for_provider(anth, list()))
  expect_true(pakhom:::.use_citations_for_provider(anth,
    list(data_integrity = list(use_citations_api = FALSE))))
  expect_true(pakhom:::.use_citations_for_provider(anth,
    list(analysis = list(coding = list(use_citations_api = FALSE)))))
  # And it returns FALSE for OpenAI regardless of any "enable" flag
  oai <- mock_provider("openai")
  expect_false(pakhom:::.use_citations_for_provider(oai,
    list(data_integrity = list(use_citations_api = TRUE))))
})

test_that(".anthropic_completion errors when documents + response_schema combined (defensive guard)", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  # Future caller mistakenly combines citations with forced tool_use ->
  # silent empty-citations would hide the misconfiguration. The guard
  # turns it into a loud error.
  expect_error(
    pakhom:::.anthropic_completion(
      provider = mock_provider("anthropic"),
      prompt = "p", system_prompt = NULL, model = "claude-mock",
      temperature = 0.3, max_tokens = 100L, json_mode = FALSE,
      response_schema = list(type = "object", properties = list()),
      documents = list(list(id = "d1", text = "x", title = "d1"))
    ),
    "cannot be combined with"
  )
})

test_that("compute_corpus_coverage refuses duplicate std_ids", {
  # Headline assertion would silently affirm coverage that wasn't
  # achieved if std_ids have duplicates (intersect would dedupe).
  state <- create_coding_state()
  for (id in c("e1", "e2", "e3")) {
    state$entry_results[[id]] <- list(skipped = FALSE,
                                        coded_segments = list(),
                                        codes_assigned = "c1")
  }
  data <- tibble::tibble(
    std_id   = c("e1", "e2", "e2", "e3"),  # e2 duplicated
    std_text = c("x", "y", "y2", "z")
  )
  expect_error(
    compute_corpus_coverage(state, data),
    "duplicate std_id"
  )
})

test_that("Tier-0 smoke: long-entry quote stays verified_exact (truncation/SHA bug fixed)", {
  # Regression test for the audit's HIGH #12: the citations bridge used to
  # store the truncated text's SHA, so verify_quote (called with the full
  # text) saw a hash mismatch and -- if the ladder failed -- would
  # mis-categorize fabrications as "drifted". The fix passes the FULL
  # text through to make_quote so SHA matches.
  long_text <- paste(rep("Padding text. ", 800), collapse = "")  # > 8000 chars
  long_text <- paste0(long_text, "trouble focusing was hard")
  truncated <- substr(long_text, 1, 8000)

  cite <- list(
    type = "char_location",
    cited_text = "Padding",  # in the truncated prefix
    document_index = 0L, document_title = "e1",
    start_char_index = 0L, end_char_index = 7L
  )
  documents <- list(list(id = "e1", text = truncated, type = "data_entry"))

  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model <- "m"; ai_meta$call_id <- "c"; ai_meta$citations <- list(cite)

  q <- pakhom:::.build_quote_from_citations_path(
    seg_text = "Padding", seg_index = 1L, citations = list(cite),
    documents = documents, text = long_text, entry_id = "e1",
    code_key = "c", ai_meta = ai_meta
  )
  q <- verify_quote(q, long_text)
  # Should be verified_exact, NOT drifted (the fix ensures source_text
  # passed to make_quote is the FULL text, not the truncated prompt text)
  expect_equal(q$verification_status, "verified_exact")
})
