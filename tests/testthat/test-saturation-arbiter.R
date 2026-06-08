# ==============================================================================
# AI saturation arbiter (R/saturation_arbiter.R)
# ==============================================================================
# Tests for .ai_judge_saturation, .saturation_cadence, .build_saturation_prompt,
# .format_saturation_curve_for_prompt, .sample_codebook_for_prompt, and the
# .saturation_decision_schema().
#
# The arbiter is the C1 replacement for the former multi-signal
# triangulation (six hardcoded knobs + 0.05 slope threshold). Tests cover
# the three verdict paths, articulation enforcement, circuit-breaker
# failure path, schema validity, cadence formula, and prompt assembly.
# ==============================================================================

# ---- Cadence formula --------------------------------------------------------

test_that(".saturation_cadence floors at 20 for tiny corpora", {
  expect_equal(pakhom:::.saturation_cadence(1L), 20L)
  expect_equal(pakhom:::.saturation_cadence(100L), 20L)
  expect_equal(pakhom:::.saturation_cadence(999L), 20L)
})

test_that(".saturation_cadence scales as ceiling(n/50) for larger corpora", {
  expect_equal(pakhom:::.saturation_cadence(1000L), 20L)  # 1000/50 = 20
  expect_equal(pakhom:::.saturation_cadence(2500L), 50L)  # 2500/50 = 50
  expect_equal(pakhom:::.saturation_cadence(9178L), 184L) # ceiling(9178/50)
  expect_equal(pakhom:::.saturation_cadence(50000L), 1000L)
})

# ---- Schema validity --------------------------------------------------------

test_that(".saturation_decision_schema is well-formed JSON Schema", {
  s <- pakhom:::.saturation_decision_schema()
  expect_silent(pakhom:::.validate_schema(s))
  expect_equal(s$type, "object")
  expect_false(s$additionalProperties)
  expect_setequal(unlist(s$required), c("articulation", "verdict", "rationale"))
  # Verdict enum must contain exactly the three valid values
  enum_vals <- unlist(s$properties$verdict$enum)
  expect_setequal(enum_vals, c("reached", "not_yet", "uncertain"))
})

test_that(".saturation_decision_schema round-trips via jsonlite", {
  s <- pakhom:::.saturation_decision_schema()
  json <- jsonlite::toJSON(s, auto_unbox = TRUE)
  parsed <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  expect_equal(parsed$type, "object")
  expect_false(parsed$additionalProperties)
  # required must round-trip as an array of length 3 (the auto-unbox trap
  # would collapse c("x") -> "x"; list() avoids it)
  expect_type(parsed$required, "list")
  expect_equal(length(parsed$required), 3L)
})

# ---- Prompt assembly helpers ------------------------------------------------

test_that(".format_saturation_curve_for_prompt handles empty curve", {
  out <- pakhom:::.format_saturation_curve_for_prompt(NULL)
  expect_true(grepl("no curve data yet", out))
  empty_curve <- data.frame(
    entries_coded = integer(0), entries_processed = integer(0),
    n_codes = integer(0), new_codes_in_window = integer(0),
    slope_ratio = numeric(0), timestamp = as.POSIXct(character(0))
  )
  out2 <- pakhom:::.format_saturation_curve_for_prompt(empty_curve)
  expect_true(grepl("no curve data yet", out2))
})

test_that(".format_saturation_curve_for_prompt renders the last N rows", {
  curve <- data.frame(
    entries_coded = c(50L, 100L, 150L, 200L, 250L, 300L, 350L, 400L),
    entries_processed = c(50L, 100L, 150L, 200L, 250L, 300L, 350L, 400L),
    n_codes = c(20L, 35L, 45L, 50L, 52L, 53L, 53L, 53L),
    new_codes_in_window = c(20L, 15L, 10L, 5L, 2L, 1L, 0L, 0L),
    slope_ratio = c(0.5, 0.4, 0.3, 0.2, 0.1, 0.05, 0.04, 0.04),
    timestamp = rep(Sys.time(), 8)
  )
  out <- pakhom:::.format_saturation_curve_for_prompt(curve, n_recent = 3L)
  # Last 3 rows only
  expect_true(grepl("entries_coded=350", out))
  expect_true(grepl("entries_coded=400", out))
  expect_true(grepl("entries_coded=300", out))
  # Earlier rows excluded
  expect_false(grepl("entries_coded=50,", out))
  # reuse_density = 1 - slope_ratio; the last row's slope is 0.04 -> 0.960
  expect_true(grepl("reuse_density=0\\.960", out))
})

test_that(".sample_codebook_for_prompt orders by frequency descending and truncates", {
  codebook <- list(
    a = list(code_name = "Alpha", frequency = 5L),
    b = list(code_name = "Beta",  frequency = 100L),
    c = list(code_name = "Gamma", frequency = 20L),
    d = list(code_name = "Delta", frequency = 1L)
  )
  out <- pakhom:::.sample_codebook_for_prompt(codebook, n = 2L)
  # Top 2 by frequency: Beta, Gamma
  expect_true(grepl("Beta \\(n=100\\)", out))
  expect_true(grepl("Gamma \\(n=20\\)", out))
  expect_false(grepl("Alpha", out))
  expect_false(grepl("Delta", out))
})

test_that(".sample_codebook_for_prompt handles empty codebook", {
  out <- pakhom:::.sample_codebook_for_prompt(list())
  expect_equal(out, "(empty codebook)")
})

test_that(".build_saturation_prompt includes research focus, progress, curve, codebook", {
  state <- create_coding_state()
  state$codebook[["a"]] <- list(code_name = "Alpha", frequency = 10L)
  state$saturation$curve <- data.frame(
    entries_coded = 100L, entries_processed = 100L,
    n_codes = 1L, new_codes_in_window = 0L,
    slope_ratio = 0.01, timestamp = Sys.time()
  )
  prompt <- pakhom:::.build_saturation_prompt(
    state, research_focus = "Sleep medication adherence",
    n_coded = 100L, n_corpus = 500L, n_done = 110L
  )
  expect_true(grepl("Sleep medication adherence", prompt))
  expect_true(grepl("entries_coded: 100", prompt))
  expect_true(grepl("total corpus size: 500", prompt))
  expect_true(grepl("current codebook size: 1", prompt))
  expect_true(grepl("Alpha", prompt))
  # 110/500 = 22% processed
  expect_true(grepl("22%", prompt))
})

# ---- AI arbiter: success paths ----------------------------------------------

# Helper: build a mock provider whose ai_complete returns a canned response
.mock_arbiter_provider <- function() {
  list(
    provider = "openai",
    models = list(primary = "gpt-mock"),
    temperature = list(saturation_check = 0),
    max_tokens = list(saturation_check = 500),
    methodology_rules = "",
    rate_limits = list(delay_between_batches = 0)
  ) |> structure(class = "AIProvider")
}

.mock_state_for_arbiter <- function() {
  state <- create_coding_state()
  state$codebook[["a"]] <- list(code_name = "Alpha", frequency = 10L)
  state$codebook[["b"]] <- list(code_name = "Beta",  frequency = 5L)
  state$saturation$curve <- data.frame(
    entries_coded = c(50L, 100L), entries_processed = c(50L, 100L),
    n_codes = c(1L, 2L), new_codes_in_window = c(1L, 0L),
    slope_ratio = c(0.1, 0.05), timestamp = c(Sys.time(), Sys.time())
  )
  state
}

test_that(".ai_judge_saturation returns 'reached' verdict on confident response", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  mock_response <- jsonlite::toJSON(list(
    articulation = paste0(
      "The codebook has been stable for the last 6 windows with ",
      "new_codes_in_window dropping to 0 and reuse_density at 0.95. ",
      "Recent additions are not surfacing new concepts."
    ),
    verdict      = "reached",
    rationale    = "Trajectory shows codebook plateau over 6 consecutive windows."
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(content = mock_response, model = "gpt-mock",
                                       request_id = "r1",
                                       usage = list(prompt_tokens = 100L,
                                                     completion_tokens = 50L,
                                                     total_tokens = 150L),
                                       finish_reason = "stop", raw_response = list(),
                                       prompt_hash = "h", citations = list()),
    .package = "pakhom"
  )

  out <- pakhom:::.ai_judge_saturation(
    state = .mock_state_for_arbiter(),
    provider = .mock_arbiter_provider(),
    research_focus = "test",
    n_coded = 200L, n_corpus = 500L, n_done = 220L
  )
  expect_true(out$success)
  expect_equal(out$verdict, "reached")
  expect_true(nchar(out$articulation) >= 30L)
})

test_that(".ai_judge_saturation returns 'not_yet' verdict when AI says so", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  mock_response <- jsonlite::toJSON(list(
    articulation = "Codebook is still adding 3-5 new codes per window; growth is steady.",
    verdict      = "not_yet",
    rationale    = "Recent windows continue producing new codes."
  ), auto_unbox = TRUE)
  local_mocked_bindings(
    ai_complete = function(...) list(content = mock_response, model = "gpt-mock",
                                       request_id = "r1",
                                       usage = list(prompt_tokens = 100L,
                                                     completion_tokens = 50L,
                                                     total_tokens = 150L),
                                       finish_reason = "stop", raw_response = list(),
                                       prompt_hash = "h", citations = list()),
    .package = "pakhom"
  )
  out <- pakhom:::.ai_judge_saturation(
    state = .mock_state_for_arbiter(),
    provider = .mock_arbiter_provider(),
    research_focus = "test",
    n_coded = 100L, n_corpus = 500L, n_done = 100L
  )
  expect_true(out$success)
  expect_equal(out$verdict, "not_yet")
})

test_that(".ai_judge_saturation returns 'uncertain' verdict when AI defers", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  mock_response <- jsonlite::toJSON(list(
    articulation = "Only one curve point so far; trajectory not yet informative.",
    verdict      = "uncertain",
    rationale    = "Insufficient evidence to judge saturation yet."
  ), auto_unbox = TRUE)
  local_mocked_bindings(
    ai_complete = function(...) list(content = mock_response, model = "gpt-mock",
                                       request_id = "r1",
                                       usage = list(prompt_tokens = 100L,
                                                     completion_tokens = 50L,
                                                     total_tokens = 150L),
                                       finish_reason = "stop", raw_response = list(),
                                       prompt_hash = "h", citations = list()),
    .package = "pakhom"
  )
  out <- pakhom:::.ai_judge_saturation(
    state = .mock_state_for_arbiter(),
    provider = .mock_arbiter_provider(),
    research_focus = "test",
    n_coded = 20L, n_corpus = 500L, n_done = 20L
  )
  expect_true(out$success)
  expect_equal(out$verdict, "uncertain")
})

# ---- AI arbiter: articulation enforcement (anti-vacuous) -----------

test_that(".ai_judge_saturation downgrades short articulation 'reached' -> 'not_yet'", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  # Articulation is < 30 chars; AI claims reached. Arbiter should downgrade.
  mock_response <- jsonlite::toJSON(list(
    articulation = "Done.",
    verdict      = "reached",
    rationale    = "Done."
  ), auto_unbox = TRUE)
  local_mocked_bindings(
    ai_complete = function(...) list(content = mock_response, model = "gpt-mock",
                                       request_id = "r1",
                                       usage = list(prompt_tokens = 1L,
                                                     completion_tokens = 1L,
                                                     total_tokens = 2L),
                                       finish_reason = "stop", raw_response = list(),
                                       prompt_hash = "h", citations = list()),
    .package = "pakhom"
  )
  out <- suppressWarnings(pakhom:::.ai_judge_saturation(
    state = .mock_state_for_arbiter(),
    provider = .mock_arbiter_provider(),
    research_focus = "test",
    n_coded = 100L, n_corpus = 500L, n_done = 100L
  ))
  expect_true(out$success)
  # 'reached' should have been forced to 'not_yet' because articulation < 30 chars
  expect_equal(out$verdict, "not_yet")
})

test_that(".ai_judge_saturation does NOT downgrade short articulation for 'not_yet' / 'uncertain'", {
  # The anti-vacuous rule only applies to 'reached' (the stopping verdict).
  # Short articulations for 'not_yet' or 'uncertain' are acceptable.
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  mock_response <- jsonlite::toJSON(list(
    articulation = "Brief.",
    verdict      = "not_yet",
    rationale    = "Brief."
  ), auto_unbox = TRUE)
  local_mocked_bindings(
    ai_complete = function(...) list(content = mock_response, model = "gpt-mock",
                                       request_id = "r1",
                                       usage = list(prompt_tokens = 1L,
                                                     completion_tokens = 1L,
                                                     total_tokens = 2L),
                                       finish_reason = "stop", raw_response = list(),
                                       prompt_hash = "h", citations = list()),
    .package = "pakhom"
  )
  out <- pakhom:::.ai_judge_saturation(
    state = .mock_state_for_arbiter(),
    provider = .mock_arbiter_provider(),
    research_focus = "test",
    n_coded = 100L, n_corpus = 500L, n_done = 100L
  )
  expect_true(out$success)
  expect_equal(out$verdict, "not_yet")
})

# ---- AI arbiter: failure paths ----------------------------------------------

test_that(".ai_judge_saturation returns uncertain + success=FALSE on AI error", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  local_mocked_bindings(
    ai_complete = function(...) stop("simulated provider outage"),
    .package = "pakhom"
  )
  out <- suppressWarnings(pakhom:::.ai_judge_saturation(
    state = .mock_state_for_arbiter(),
    provider = .mock_arbiter_provider(),
    research_focus = "test",
    n_coded = 100L, n_corpus = 500L, n_done = 100L
  ))
  expect_false(out$success)
  expect_equal(out$verdict, "uncertain")
})

test_that(".ai_judge_saturation returns success=FALSE on unknown verdict value", {
  # Defensive: the schema is enum-constrained, but if a provider returns
  # something out-of-enum (e.g., a buggy mock), the arbiter must not
  # accidentally accept it.
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  mock_response <- jsonlite::toJSON(list(
    articulation = "Long enough articulation for the anti-vacuous check to pass.",
    verdict      = "garbage_value",
    rationale    = "Buggy mock."
  ), auto_unbox = TRUE)
  local_mocked_bindings(
    ai_complete = function(...) list(content = mock_response, model = "gpt-mock",
                                       request_id = "r1",
                                       usage = list(prompt_tokens = 1L,
                                                     completion_tokens = 1L,
                                                     total_tokens = 2L),
                                       finish_reason = "stop", raw_response = list(),
                                       prompt_hash = "h", citations = list()),
    .package = "pakhom"
  )
  out <- suppressWarnings(pakhom:::.ai_judge_saturation(
    state = .mock_state_for_arbiter(),
    provider = .mock_arbiter_provider(),
    research_focus = "test",
    n_coded = 100L, n_corpus = 500L, n_done = 100L
  ))
  expect_false(out$success)
  expect_equal(out$verdict, "uncertain")
})

# ---- AI arbiter: audit log integration --------------------------------------

test_that(".ai_judge_saturation records saturation_judgment in audit log on success", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  mock_response <- jsonlite::toJSON(list(
    articulation = "Codebook flat for 5+ windows; reuse density 0.95. New codes have dropped to 0.",
    verdict      = "reached",
    rationale    = "Sustained flat trajectory."
  ), auto_unbox = TRUE)
  local_mocked_bindings(
    ai_complete = function(...) list(content = mock_response, model = "gpt-mock",
                                       request_id = "r1",
                                       usage = list(prompt_tokens = 100L,
                                                     completion_tokens = 50L,
                                                     total_tokens = 150L),
                                       finish_reason = "stop", raw_response = list(),
                                       prompt_hash = "h", citations = list()),
    .package = "pakhom"
  )

  tmp_dir <- withr::local_tempdir()
  audit <- init_audit_log(tmp_dir)
  withr::defer(close_audit_log(audit))

  out <- pakhom:::.ai_judge_saturation(
    state = .mock_state_for_arbiter(),
    provider = .mock_arbiter_provider(),
    research_focus = "test",
    n_coded = 200L, n_corpus = 500L, n_done = 220L,
    audit_log = audit
  )
  expect_true(out$success)
  expect_equal(out$verdict, "reached")

  # Audit log should contain one ai_request + one saturation_judgment record
  log_lines <- readLines(audit$path)
  decisions <- lapply(log_lines, jsonlite::fromJSON, simplifyVector = TRUE)
  types <- vapply(decisions, function(d) d$decision_type, character(1))
  expect_true("saturation_judgment" %in% types)
  sj_idx <- which(types == "saturation_judgment")
  expect_equal(decisions[[sj_idx]]$verdict, "reached")
  expect_true(nchar(decisions[[sj_idx]]$articulation_excerpt %||% "") >= 30L)
})
