# Tests for the provocateur architecture (M1.1 + M1.2,
# R/provocateur.R). Mode 1 (Reflexive Scaffold) implements Sarkar 2024's
# "AI as Socratic gadfly" pattern: AI provides extractive provocations
# that surface counter-narratives, absent voices, alternative
# interpretations, disconfirming evidence, and assumption-surfacing
# challenges -- but does NOT name themes or codes (the researcher does).

# ---- ResearcherReflectionLog S3 ---------------------------------------------

test_that("create_reflection_log returns the documented shape", {
  log <- create_reflection_log()
  expect_s3_class(log, "ResearcherReflectionLog")
  expected_fields <- c(
    "provocations", "memos", "positionality_history",
    "reflexivity_collapse_flags", "researcher_authored_codes",
    "researcher_authored_themes",
    # Schema 1.1.0: T0.3 attempt + explicit-skip tracking
    "provocation_attempts", "skipped_themes",
    "config_hash", "created_at",
    "last_updated", "schema_version"
  )
  expect_setequal(names(log), expected_fields)
  expect_length(log$provocations, 0L)
  expect_equal(nrow(log$positionality_history), 0L)
  expect_equal(nrow(log$provocation_attempts), 0L)
  expect_equal(nrow(log$skipped_themes), 0L)
  expect_equal(log$schema_version, "1.2.0")
})

test_that("create_reflection_log accepts and stores config_hash", {
  log <- create_reflection_log(config_hash = "abc123")
  expect_equal(log$config_hash, "abc123")
})

test_that("print.ResearcherReflectionLog produces a structured summary", {
  log <- create_reflection_log()
  out <- capture.output(print(log))
  expect_true(any(grepl("ResearcherReflectionLog", out)))
  expect_true(any(grepl("Provocations:", out)))
  expect_true(any(grepl("Memos:", out)))
})

# ---- Provocation S3 -------------------------------------------------------

test_that("make_provocation rejects invalid category", {
  expect_error(
    make_provocation(category = "invented_category",
                       theme_name = "T", reason = "x", provenance = NULL),
    "Invalid provocation category"
  )
})

test_that("make_provocation accepts NULL provenance for observational categories", {
  # absent_voice is observational (no exact_text citation possible); the
  # constructor permits NULL provenance for these categories.
  p <- make_provocation(category = "absent_voice", theme_name = "T",
                          reason = "Adolescents underrepresented",
                          provenance = NULL)
  expect_s3_class(p, "Provocation")
  expect_null(p$provenance)
  expect_equal(p$category, "absent_voice")
})

test_that("make_provocation rejects non-QuoteProvenance provenance", {
  expect_error(
    make_provocation(category = "counter_narrative", theme_name = "T",
                       reason = "x", provenance = list(fake = TRUE)),
    "QuoteProvenance"
  )
})

test_that("print.Provocation shows category, theme, citation when present", {
  src <- "I plan to take medication every day from now on."
  q <- make_quote("e1", "data_entry", src, 0L, 6L, "I plan",
                   citation_source = "model_freeform")
  q <- verify_quote(q, src)
  p <- make_provocation(
    category   = "counter_narrative",
    theme_name = "Medication adherence",
    reason     = "Frames medication taking as routine, not contested",
    provenance = q
  )
  out <- capture.output(print(p))
  expect_true(any(grepl("counter_narrative", out)))
  expect_true(any(grepl("Medication adherence", out)))
  expect_true(any(grepl("e1", out)))
})

# ---- Provocation citation -> Provocation conversion ------------------------

test_that(".citation_to_provocation builds verified Provocation from real citation", {
  data <- tibble::tibble(
    std_id   = c("e1", "e2"),
    std_text = c("I plan to take my medication every day from now on.",
                  "My doctor told me to follow this regimen carefully.")
  )
  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model <- "claude-mock"; ai_meta$call_id <- "msg_test"

  cit <- list(entry_id = "e1", char_start = 0L, char_end = 6L,
              exact_text = "I plan",
              reason = "Patient frames adherence as voluntary plan")

  p <- pakhom:::.citation_to_provocation(
    cit = cit, theme_name = "Medication adherence",
    category = "counter_narrative", data = data, ai_meta = ai_meta
  )

  expect_s3_class(p, "Provocation")
  expect_equal(p$provenance$verification_status, "verified_exact")
  expect_equal(p$provenance$source_doc_id, "e1")
})

test_that(".citation_to_provocation drops citation referencing unknown entry_id", {
  data <- tibble::tibble(std_id = "e1", std_text = "I plan to take medication.")
  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model <- "m"; ai_meta$call_id <- "c"
  cit <- list(entry_id = "GHOST", char_start = 0L, char_end = 5L,
              exact_text = "ghost", reason = "made up")
  expect_null(suppressWarnings(pakhom:::.citation_to_provocation(
    cit, "T", "counter_narrative", data, ai_meta
  )))
})

test_that(".citation_to_provocation drops fabricated provocation citations (T0.1 enforcement)", {
  # Per AC7, fabricated provocations are dropped silently. The cited
  # entry exists but the exact_text doesn't appear in the source.
  data <- tibble::tibble(
    std_id = "e1",
    std_text = "I plan to take medication every day from now on."
  )
  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model <- "m"; ai_meta$call_id <- "c"
  cit <- list(entry_id = "e1", char_start = 0L, char_end = 30L,
              exact_text = "I love unicorns and rainbows",
              reason = "Definitely in the data")
  p <- suppressWarnings(pakhom:::.citation_to_provocation(
    cit, "T", "counter_narrative", data, ai_meta
  ))
  expect_null(p)
})

# ---- Five provocation categories: end-to-end with mocked AI ----------------

.smoke_data_with_themes <- function() {
  data <- tibble::tibble(
    std_id   = paste0("e", 1:6),
    std_text = c(
      "I plan to take my medication every day from now on.",
      "My doctor told me to follow this regimen carefully.",
      "I always forget my pills; the schedule is impossible to keep up.",
      "Side effects make me skip doses on weekends.",
      "Honestly I don't think medication helps me at all.",
      "Taking my meds makes me feel like a different person."
    ),
    std_author = c("alice", "bob", "carol", "dave", "eve", "frank"),
    sentiment_score = c(0.5, 0.4, -0.3, -0.5, -0.7, 0.1),
    emotion_intensity = rep(0.4, 6),
    all_emotions = rep("hope", 6),
    emerged_themes = rep("Adherence", 6),
    theme_membership_Adherence = rep(1L, 6)
  )
  data
}

.smoke_theme_set <- function() {
  create_theme_set(list(list(
    id = 1, name = "Adherence",
    description = "Medication adherence",
    codes_included = "med_routine"
  )))
}

test_that("provoke_counter_narrative returns verified provocations on mocked AI", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  data <- .smoke_data_with_themes()
  theme_entries <- data[1:3, ]
  # e5 = "Honestly I don't think medication helps me at all."
  # cite "medication helps" at the position it actually appears
  mock <- jsonlite::toJSON(list(provocations = list(
    list(entry_id = "e5", char_start = 25L, char_end = 41L,
         exact_text = "medication helps",
         reason = "Patient denies medication efficacy")
  )), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(
      content    = mock, model = "claude-mock", request_id = "r1",
      usage      = list(prompt_tokens = 1L, completion_tokens = 1L,
                        total_tokens = 2L),
      finish_reason = "stop", raw_response = list(),
      prompt_hash = "h", citations = list()
    ),
    .package = "pakhom"
  )

  provs <- provoke_counter_narrative(
    theme_name    = "Adherence",
    theme_entries = theme_entries,
    data          = data,
    provider      = mock_provider("anthropic"),
    n             = 5L
  )
  expect_length(provs, 1L)
  expect_s3_class(provs[[1]], "Provocation")
  expect_equal(provs[[1]]$category, "counter_narrative")
  expect_equal(provs[[1]]$provenance$source_doc_id, "e5")
  expect_true(provs[[1]]$provenance$verification_status %in%
              c("verified_exact", "verified_fuzzy"))
})

test_that("provoke_counter_narrative drops fabricated citations from output", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  data <- .smoke_data_with_themes()
  theme_entries <- data[1:3, ]
  # Mock returns one verified + one fabricated citation
  mock <- jsonlite::toJSON(list(provocations = list(
    list(entry_id = "e5", char_start = 25L, char_end = 41L,
         exact_text = "medication helps", reason = "verified"),
    list(entry_id = "e1", char_start = 0L, char_end = 50L,
         exact_text = "totally fabricated content not in source",
         reason = "fake")
  )), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(
      content    = mock, model = "m", request_id = "r",
      usage      = list(prompt_tokens = 1L, completion_tokens = 1L,
                        total_tokens = 2L),
      finish_reason = "stop", raw_response = list(),
      prompt_hash = "h", citations = list()
    ),
    .package = "pakhom"
  )

  provs <- suppressWarnings(provoke_counter_narrative(
    theme_name = "Adherence", theme_entries = theme_entries,
    data = data, provider = mock_provider("anthropic")
  ))
  # Only the verified provocation survives
  expect_length(provs, 1L)
  expect_equal(provs[[1]]$provenance$source_doc_id, "e5")
})

test_that("provoke_disconfirming_evidence returns verified provocations", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  data <- .smoke_data_with_themes()
  mock <- jsonlite::toJSON(list(provocations = list(
    list(entry_id = "e3", char_start = 19L, char_end = 27L,
         exact_text = "the sche",
         reason = "Schedule fails contradict adherence")
  )), auto_unbox = TRUE)
  local_mocked_bindings(
    ai_complete = function(...) list(
      content = mock, model = "m", request_id = "r",
      usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                   total_tokens = 2L),
      finish_reason = "stop", raw_response = list(),
      prompt_hash = "h", citations = list()
    ),
    .package = "pakhom"
  )
  provs <- provoke_disconfirming_evidence(
    theme_name = "Adherence", theme_entries = data[1:3, ],
    data = data, provider = mock_provider("anthropic")
  )
  expect_length(provs, 1L)
  expect_equal(provs[[1]]$category, "disconfirming_evidence")
})

test_that("provoke_alternative_interpretation returns N alternative names anchored on a quote", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  data <- .smoke_data_with_themes()
  mock <- jsonlite::toJSON(list(
    alternative_names = c("Routine maintenance", "Compliance under pressure"),
    shared_quotes = list(list(
      entry_id = "e1", char_start = 0L, char_end = 6L,
      exact_text = "I plan",
      reason = "Frames as plan; could read as compliance under pressure"
    ))
  ), auto_unbox = TRUE)
  local_mocked_bindings(
    ai_complete = function(...) list(
      content = mock, model = "m", request_id = "r",
      usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                   total_tokens = 2L),
      finish_reason = "stop", raw_response = list(),
      prompt_hash = "h", citations = list()
    ),
    .package = "pakhom"
  )
  provs <- provoke_alternative_interpretation(
    theme_name = "Adherence", theme_entries = data[1:3, ],
    data = data, provider = mock_provider("anthropic"),
    n_alternatives = 2L
  )
  expect_length(provs, 2L)
  expect_equal(provs[[1]]$category, "alternative_interpretation")
  expect_equal(provs[[1]]$extra$alternative_name, "Routine maintenance")
  expect_equal(provs[[2]]$extra$alternative_name, "Compliance under pressure")
})

test_that("provoke_absent_voice returns NULL-provenance Provocations with dimension info", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  data <- .smoke_data_with_themes()
  mock <- jsonlite::toJSON(list(absent_segments = list(
    list(dimension = "demographic",
         description = "No adolescent contributors",
         reason = "Theme support comes only from adults"),
    list(dimension = "temporal",
         description = "No entries from last 30 days",
         reason = "Recent perspectives missing")
  )), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(
      content = mock, model = "m", request_id = "r",
      usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                   total_tokens = 2L),
      finish_reason = "stop", raw_response = list(),
      prompt_hash = "h", citations = list()
    ),
    .package = "pakhom"
  )
  provs <- provoke_absent_voice(
    theme_name = "Adherence", theme_entries = data[1:3, ],
    data = data, provider = mock_provider("anthropic")
  )
  expect_length(provs, 2L)
  expect_null(provs[[1]]$provenance)
  expect_equal(provs[[1]]$category, "absent_voice")
  expect_equal(provs[[1]]$extra$dimension, "demographic")
})

test_that("provoke_assumption_surfacing returns alternative + erased terms", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  data <- .smoke_data_with_themes()
  mock <- jsonlite::toJSON(list(
    alternative_terms = list(
      list(term = "skip doses",
           example_entry_id = "e4",
           exact_text = "skip doses")
    ),
    erased_terms = list(
      list(term = "side effects",
           implication = "Researcher's framing focuses on routine, ignoring physical impact")
    )
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(
      content = mock, model = "m", request_id = "r",
      usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                   total_tokens = 2L),
      finish_reason = "stop", raw_response = list(),
      prompt_hash = "h", citations = list()
    ),
    .package = "pakhom"
  )
  provs <- provoke_assumption_surfacing(
    theme_name = "Adherence", theme_entries = data[1:3, ],
    data = data, provider = mock_provider("anthropic"),
    key_term = "adherence"
  )
  expect_gte(length(provs), 1L)
  # The alternative term provocation has provenance (citation present)
  alt <- Filter(function(p) !is.null(p$provenance), provs)
  expect_gte(length(alt), 1L)
  expect_equal(alt[[1]]$extra$alternative_term, "skip doses")
  # The erased term has NULL provenance
  erased <- Filter(function(p) is.null(p$provenance), provs)
  expect_gte(length(erased), 1L)
  expect_equal(erased[[1]]$extra$erased_term, "side effects")
})

# ---- Orchestrator: run_provocateur_questioning -----------------------------

test_that("run_provocateur_questioning iterates all categories per theme", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  data <- .smoke_data_with_themes()
  ts <- .smoke_theme_set()

  # Mock returns a single provocation per call (one per category)
  call_count <- new.env(parent = emptyenv()); call_count$n <- 0L
  cn_response <- jsonlite::toJSON(list(provocations = list(list(
    entry_id = "e5", char_start = 18L, char_end = 25L,
    exact_text = "doesn't",
    reason = "denies efficacy"
  ))), auto_unbox = TRUE)
  ai_response <- jsonlite::toJSON(list(absent_segments = list(list(
    dimension = "demographic", description = "no x", reason = "y"
  ))), auto_unbox = TRUE)
  alt_response <- jsonlite::toJSON(list(
    alternative_names = c("Compliance"),
    shared_quotes = list(list(entry_id = "e1", char_start = 0L, char_end = 6L,
                                exact_text = "I plan", reason = "anchor"))
  ), auto_unbox = TRUE)
  ass_response <- jsonlite::toJSON(list(
    alternative_terms = list(list(term = "skip", example_entry_id = "e4",
                                    exact_text = "skip")),
    erased_terms = list(list(term = "side", implication = "x"))
  ), auto_unbox = TRUE)

  responses <- list(cn_response, ai_response, alt_response, cn_response,
                     ass_response)

  local_mocked_bindings(
    ai_complete = function(...) {
      call_count$n <- call_count$n + 1L
      list(
        content = responses[[call_count$n]], model = "m",
        request_id = paste0("r", call_count$n),
        usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                     total_tokens = 2L),
        finish_reason = "stop", raw_response = list(),
        prompt_hash = "h", citations = list()
      )
    },
    .package = "pakhom"
  )

  log <- run_provocateur_questioning(
    data = data, theme_set = ts,
    provider = mock_provider("anthropic"),
    config = list()
  )

  expect_s3_class(log, "ResearcherReflectionLog")
  # Should have provocations from each category that ran
  cats <- vapply(log$provocations, function(p) p$category, character(1))
  expect_true(length(cats) > 0L)
})

test_that("run_provocateur_questioning rejects unknown category names", {
  data <- .smoke_data_with_themes()
  ts <- .smoke_theme_set()
  expect_error(
    run_provocateur_questioning(
      data = data, theme_set = ts,
      provider = mock_provider("anthropic"),
      categories = c("counter_narrative", "FAKE_CATEGORY")
    ),
    "Unknown provocation categories"
  )
})

test_that("run_provocateur_questioning skips themes with no supporting entries", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  # Theme membership column references a theme but no rows are flagged
  data <- tibble::tibble(
    std_id = paste0("e", 1:3),
    std_text = paste("text", 1:3),
    theme_membership_Empty = c(0L, 0L, 0L)
  )
  ts <- create_theme_set(list(list(
    id = 1, name = "Empty", description = "", codes_included = "x"
  )))

  call_count <- new.env(parent = emptyenv()); call_count$n <- 0L
  local_mocked_bindings(
    ai_complete = function(...) {
      call_count$n <- call_count$n + 1L
      list(content = "{}", model = "m", request_id = "r",
           usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                        total_tokens = 2L),
           finish_reason = "stop", raw_response = list(),
           prompt_hash = "h", citations = list())
    },
    .package = "pakhom"
  )

  log <- suppressWarnings(run_provocateur_questioning(
    data = data, theme_set = ts,
    provider = mock_provider("openai")
  ))

  expect_equal(call_count$n, 0L)  # zero AI calls because theme had no entries
  expect_length(log$provocations, 0L)
  # Schema 1.1.0: the explicit-skip is recorded with reason
  # so compute_mode1_coverage can distinguish it from a silent skip.
  expect_equal(nrow(log$skipped_themes), 1L)
  expect_equal(log$skipped_themes$theme_name, "Empty")
  expect_equal(log$skipped_themes$reason, "no_supporting_entries")
  # No attempts should be recorded because the theme was skipped before
  # the per-category loop even started.
  expect_equal(nrow(log$provocation_attempts), 0L)
})

test_that("run_provocateur_questioning surfaces missing-membership-input as a distinct skip reason", {
  # Finding: when input data carries NEITHER any
  # theme_membership_<safe_name> column NOR an emerged_themes column,
  # every theme would silently grade as "no supporting entries", which
  # masks a real input-shape misconfiguration. The improved error
  # surfaces the missing-input case as a distinct reason and prepends
  # an upfront log_warn pointing the user at the right input shapes.
  data <- tibble::tibble(
    std_id   = paste0("e", 1:5),
    std_text = paste("text", 1:5)
    # NO theme_membership_* column, NO emerged_themes column
  )
  ts <- create_theme_set(list(
    list(id = 1, name = "Theme A", description = "", codes_included = "x"),
    list(id = 2, name = "Theme B", description = "", codes_included = "y")
  ))

  log <- suppressWarnings(run_provocateur_questioning(
    data = data, theme_set = ts,
    provider = mock_provider("openai")
  ))

  expect_equal(nrow(log$skipped_themes), 2L)
  # The new, more-specific reason
  expect_true(all(log$skipped_themes$reason == "missing_membership_input"))
  expect_setequal(log$skipped_themes$theme_name, c("Theme A", "Theme B"))
})

test_that("run_provocateur_questioning records one attempt row per theme x category, regardless of n_emitted", {
  # T0.3 (Mode 1) pre-condition: a category that legitimately returns
  # zero provocations must still appear in provocation_attempts so the
  # coverage card can assert "every category was attempted." Conflating
  # "AI returned []" with "we never asked" would let silent skips hide.
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  data <- .smoke_data_with_themes()
  ts <- .smoke_theme_set()

  # Mock returns empty results for all calls (legitimate "no qualifying entries")
  empty_response <- jsonlite::toJSON(list(provocations = list()),
                                       auto_unbox = TRUE)
  local_mocked_bindings(
    ai_complete = function(...) list(
      content = empty_response, model = "m", request_id = "r",
      usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                   total_tokens = 2L),
      finish_reason = "stop", raw_response = list(),
      prompt_hash = "h", citations = list()
    ),
    .package = "pakhom"
  )

  log <- run_provocateur_questioning(
    data = data, theme_set = ts,
    provider = mock_provider("anthropic"),
    categories = c("counter_narrative", "disconfirming_evidence")
  )

  # No provocations emitted (empty AI responses), but every theme x
  # category combo with non-empty supporting entries got an attempt row.
  expect_length(log$provocations, 0L)
  n_themes_with_entries <- sum(vapply(ts$themes, function(t) {
    safe_col <- paste0("theme_membership_", make.names(t$name))
    if (safe_col %in% names(data)) sum(data[[safe_col]] == 1L) > 0L else FALSE
  }, logical(1)))
  expect_equal(nrow(log$provocation_attempts),
               n_themes_with_entries * 2L)
  expect_true(all(log$provocation_attempts$n_emitted == 0L))
})

test_that("run_provocateur_questioning supports resume via resume_log", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  data <- .smoke_data_with_themes()
  ts <- .smoke_theme_set()

  # Build a resume log with one prior provocation
  prior_log <- create_reflection_log()
  src <- "I plan to take my medication every day from now on."
  q <- make_quote("e1", "data_entry", src, 0L, 6L, "I plan",
                   citation_source = "model_freeform")
  q <- verify_quote(q, src)
  prior_log$provocations[[1]] <- make_provocation(
    category = "counter_narrative", theme_name = "Adherence",
    reason = "prior", provenance = q
  )

  mock <- jsonlite::toJSON(list(provocations = list()), auto_unbox = TRUE)
  local_mocked_bindings(
    ai_complete = function(...) list(
      content = mock, model = "m", request_id = "r",
      usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                   total_tokens = 2L),
      finish_reason = "stop", raw_response = list(),
      prompt_hash = "h", citations = list()
    ),
    .package = "pakhom"
  )

  log <- run_provocateur_questioning(
    data = data, theme_set = ts,
    provider = mock_provider("anthropic"),
    resume_log = prior_log,
    categories = "counter_narrative"
  )
  # Prior provocation preserved in resumed log
  expect_gte(length(log$provocations), 1L)
  expect_true(any(vapply(log$provocations,
                          function(p) identical(p$reason, "prior"),
                          logical(1))))
})

# ---- AC contract tests for Mode 1 -----------------------------------------

test_that("AC1: provocation_categories enum is fixed at five", {
  # AC1 (architecture not config): the five categories are architectural
  # primitives, not user-configurable. A regression that adds a 6th
  # without updating the spec must fail this test.
  expect_setequal(pakhom:::.VALID_PROVOCATION_CATEGORIES,
                   c("counter_narrative", "absent_voice",
                     "alternative_interpretation",
                     "disconfirming_evidence", "assumption_surfacing"))
})

test_that("AC7: every Provocation with a citation runs through verify_quote", {
  # Architectural commitment: provocations with citations are NOT
  # exempt from Tier-0 universals. Verified above end-to-end via the
  # fabricated-citation drop tests, but lock the contract here too.
  data <- tibble::tibble(std_id = "e1",
                          std_text = "Real text in the source.")
  ai_meta <- new.env(parent = emptyenv())
  ai_meta$model <- "m"; ai_meta$call_id <- "c"
  cit_real <- list(entry_id = "e1", char_start = 0L, char_end = 4L,
                    exact_text = "Real", reason = "verified")
  cit_fake <- list(entry_id = "e1", char_start = 0L, char_end = 25L,
                    exact_text = "fabricated content not in source",
                    reason = "fake")
  p_real <- pakhom:::.citation_to_provocation(
    cit_real, "T", "counter_narrative", data, ai_meta
  )
  p_fake <- suppressWarnings(pakhom:::.citation_to_provocation(
    cit_fake, "T", "counter_narrative", data, ai_meta
  ))
  expect_s3_class(p_real, "Provocation")
  expect_null(p_fake)
})
