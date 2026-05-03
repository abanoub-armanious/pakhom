# Tests for AI decision audit log (audit_log.R)
# Sprint-4 T1.4 expanded the schema with: methodology_mode auto-stamping,
# log_ai_request() helper, env-backed n_written counter, and ai_request stats
# in summarize_audit_log. The pre-T1.4 contract (init/log/close/summarize) is
# also exercised here.

# ---- Helpers ----------------------------------------------------------------

mk_cfg <- function(mode = "codebook_collaborative",
                   capture_raw = TRUE) {
  list(methodology = list(mode = mode),
       audit       = list(capture_raw_responses = capture_raw,
                          response_cache_dir = "api_responses"))
}

mk_ai_result <- function(prompt_hash = "test-hash",
                         model = "gpt-4o",
                         total_tokens = 100L) {
  list(
    content       = "test content",
    model         = model,
    usage         = list(prompt_tokens = 60L, completion_tokens = 40L,
                         total_tokens = total_tokens),
    finish_reason = "stop",
    raw_response  = list(id = "test-id", text = "raw payload"),
    prompt_hash   = prompt_hash,
    request_id    = "req_test"
  )
}

# ---- init_audit_log + close ------------------------------------------------

test_that("init_audit_log creates AuditLog with default fields", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td)
  on.exit(close_audit_log(audit))

  expect_s3_class(audit, "AuditLog")
  expect_equal(audit$output_dir, td)
  expect_true(file.exists(file.path(td, "ai_decisions.jsonl")))
  expect_null(audit$methodology_mode)  # no config passed
  expect_true(is.environment(audit$state))
  expect_equal(audit$state$n_written, 0L)
})

test_that("init_audit_log captures methodology_mode when config is provided", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td, config = mk_cfg("reflexive_scaffold"))
  on.exit(close_audit_log(audit))
  expect_equal(audit$methodology_mode, "reflexive_scaffold")
})

test_that("close_audit_log reports written count from env-backed counter", {
  # Pre-T1.4 regression: the n_written field was on a pass-by-value list and
  # always read 0 in close_audit_log's log message. T1.4 moves it to an
  # environment so increments from log_ai_decision survive.
  td <- withr::local_tempdir()
  audit <- init_audit_log(td)

  log_ai_decision(audit, "coding", "code_assignment",
                  entry_id = "e1", code_name = "test_code")
  log_ai_decision(audit, "coding", "new_code_created",
                  entry_id = "e1", code_name = "new_code")
  expect_equal(audit$state$n_written, 2L)

  # close_audit_log should pull the count from audit$state$n_written without
  # erroring (we can't easily assert the log message but we can confirm no
  # error surfaces).
  expect_silent(close_audit_log(audit))
})

# ---- log_ai_decision basic --------------------------------------------------

test_that("log_ai_decision writes one JSONL line per call with required fields", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td)
  log_ai_decision(audit, "coding", "code_assignment",
                  entry_id = "e1", code_name = "test")
  close_audit_log(audit)

  lines <- readLines(file.path(td, "ai_decisions.jsonl"))
  expect_equal(length(lines), 1)
  rec <- jsonlite::fromJSON(lines[1])
  expect_equal(rec$step, "coding")
  expect_equal(rec$decision_type, "code_assignment")
  expect_equal(rec$entry_id, "e1")
  expect_true(!is.null(rec$timestamp))
})

test_that("log_ai_decision rejects invalid step", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td)
  on.exit(close_audit_log(audit))
  expect_error(
    log_ai_decision(audit, "not_a_step", "code_assignment"),
    "Invalid audit step"
  )
})

test_that("log_ai_decision rejects invalid decision_type", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td)
  on.exit(close_audit_log(audit))
  expect_error(
    log_ai_decision(audit, "coding", "not_a_type"),
    "Invalid decision_type"
  )
})

# ---- T1.4: methodology_mode auto-stamping ----------------------------------

test_that("log_ai_decision auto-stamps methodology_mode when config was passed", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td, config = mk_cfg("framework_applied"))
  log_ai_decision(audit, "coding", "code_assignment",
                  entry_id = "e1", code_name = "test")
  close_audit_log(audit)

  rec <- jsonlite::fromJSON(readLines(file.path(td, "ai_decisions.jsonl"))[1])
  expect_equal(rec$methodology_mode, "framework_applied")
})

test_that("log_ai_decision omits methodology_mode field when no config was passed", {
  # Back-compat: pre-T1.4 callers (init_audit_log without config) get records
  # without the methodology_mode field. summarize_audit_log handles this
  # gracefully (methodology_modes_observed = character(0)).
  td <- withr::local_tempdir()
  audit <- init_audit_log(td)
  log_ai_decision(audit, "coding", "code_assignment",
                  entry_id = "e1", code_name = "test")
  close_audit_log(audit)

  raw_line <- readLines(file.path(td, "ai_decisions.jsonl"))[1]
  expect_false(grepl("methodology_mode", raw_line, fixed = TRUE))
})

# ---- T1.4: new decision_types accepted -------------------------------------

test_that("log_ai_decision accepts T1.4 new decision_types", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td)
  on.exit(close_audit_log(audit))

  new_types <- c("ai_request", "provocation_emitted", "memo_added",
                 "positionality_recorded", "reflexivity_collapse_detected",
                 "mode_changed", "quote_verified", "quote_fabricated")
  for (dt in new_types) {
    # Pair each new decision_type with a step it makes sense under.
    step <- switch(dt,
      "ai_request"                    = "coding",
      "provocation_emitted"           = "provocateur",
      "memo_added"                    = "memo",
      "positionality_recorded"        = "positionality",
      "reflexivity_collapse_detected" = "reflexivity",
      "mode_changed"                  = "mode_change",
      "quote_verified"                = "quote_verification",
      "quote_fabricated"              = "quote_verification"
    )
    expect_silent(log_ai_decision(audit, step, dt, payload = "ok"))
  }
})

test_that("log_ai_decision accepts T1.4 new audit_steps", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td)
  on.exit(close_audit_log(audit))

  new_steps <- c("provocateur", "memo", "positionality", "reflexivity",
                 "mode_change", "quote_verification")
  for (st in new_steps) {
    expect_silent(log_ai_decision(audit, st, "ai_request", payload = "ok"))
  }
})

# ---- T1.4: log_ai_request --------------------------------------------------

test_that("log_ai_request unpacks ai_complete return into audit record", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td, config = mk_cfg())
  ai_result <- mk_ai_result()

  log_ai_request(audit, "coding", ai_result, response_cache = NULL,
                  entry_id = "e1")
  close_audit_log(audit)

  rec <- jsonlite::fromJSON(readLines(file.path(td, "ai_decisions.jsonl"))[1])
  expect_equal(rec$decision_type, "ai_request")
  expect_equal(rec$model, "gpt-4o")
  expect_equal(rec$finish_reason, "stop")
  expect_equal(rec$prompt_hash, "test-hash")
  expect_equal(rec$request_id, "req_test")
  expect_equal(rec$usage_prompt, 60L)
  expect_equal(rec$usage_completion, 40L)
  expect_equal(rec$usage_total, 100L)
  expect_equal(rec$entry_id, "e1")
  # No cache provided -> raw_response_path serializes as JSON null, which
  # round-trips back as R NULL (length 0). Downstream consumers use %||% to
  # treat NULL/missing as "no cached response".
  expect_null(rec$raw_response_path)
})

test_that("log_ai_request writes to cache when response_cache provided", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td, config = mk_cfg())
  cache <- init_response_cache(td, config = mk_cfg())
  ai_result <- mk_ai_result(prompt_hash = "wired-hash")

  log_ai_request(audit, "sentiment", ai_result, response_cache = cache,
                  batch_idx = 3)
  close_audit_log(audit)

  # JSONL record references the cached file
  rec <- jsonlite::fromJSON(readLines(file.path(td, "ai_decisions.jsonl"))[1])
  expect_equal(rec$raw_response_path,
               file.path("api_responses", "wired-hash.json"))
  # Cache file actually exists with the payload
  expect_true(file.exists(file.path(td, "api_responses", "wired-hash.json")))
  # Counter incremented
  expect_equal(cache$state$n_written, 1L)
})

test_that("log_ai_request silently no-ops on NULL ai_result", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td)
  expect_silent(log_ai_request(audit, "coding", NULL))
  close_audit_log(audit)

  expect_equal(audit$state$n_written, 0L)
})

# ---- T1.4: summarize_audit_log new fields ---------------------------------

test_that("summarize_audit_log surfaces ai_request stats from the new fields", {
  td <- withr::local_tempdir()
  audit <- init_audit_log(td, config = mk_cfg("reflexive_scaffold"))
  cache <- init_response_cache(td, config = mk_cfg("reflexive_scaffold"))

  # 3 ai_requests across 2 models with known token counts
  log_ai_request(audit, "coding", mk_ai_result(prompt_hash = "h1",
                                                model = "gpt-4o",
                                                total_tokens = 100L),
                 cache, entry_id = "e1")
  log_ai_request(audit, "coding", mk_ai_result(prompt_hash = "h2",
                                                model = "gpt-4o",
                                                total_tokens = 150L),
                 cache, entry_id = "e2")
  log_ai_request(audit, "sentiment", mk_ai_result(prompt_hash = "h3",
                                                   model = "gpt-4o-mini",
                                                   total_tokens = 75L),
                 cache, batch_idx = 1)
  close_audit_log(audit)

  summary <- summarize_audit_log(td)
  expect_equal(summary$total_decisions, 3L)
  expect_equal(summary$total_ai_requests, 3L)
  expect_equal(summary$total_tokens_used, 325L)
  expect_setequal(names(summary$ai_requests_by_model),
                  c("gpt-4o", "gpt-4o-mini"))
  expect_equal(unname(summary$ai_requests_by_model[["gpt-4o"]]), 2L)
  expect_equal(unname(summary$ai_requests_by_model[["gpt-4o-mini"]]), 1L)
  expect_equal(summary$methodology_modes_observed, "reflexive_scaffold")
})

test_that("summarize_audit_log handles pre-T1.4 logs (no methodology, no ai_request)", {
  # Simulate a pre-T1.4 log: no config passed, only old decision types used.
  td <- withr::local_tempdir()
  audit <- init_audit_log(td)
  log_ai_decision(audit, "coding", "code_assignment", entry_id = "e1")
  log_ai_decision(audit, "coding", "new_code_created", entry_id = "e1",
                  code_name = "new")
  close_audit_log(audit)

  summary <- summarize_audit_log(td)
  expect_equal(summary$total_decisions, 2L)
  # T1.4 fields all present but empty/zero -- back-compat shape contract
  expect_equal(summary$total_ai_requests, 0L)
  expect_equal(summary$total_tokens_used, 0L)
  expect_length(summary$ai_requests_by_model, 0L)
  expect_length(summary$methodology_modes_observed, 0L)
})

test_that("summarize_audit_log .empty_audit_summary() includes T1.4 fields", {
  # When the audit log file does not exist, summarize_audit_log returns the
  # empty summary -- which must still expose the T1.4 fields with zero/empty
  # values so downstream consumers (report builders) can read them
  # unconditionally.
  td <- withr::local_tempdir()
  summary <- summarize_audit_log(td)  # no log file exists
  expect_equal(summary$total_decisions, 0L)
  expect_equal(summary$total_ai_requests, 0L)
  expect_equal(summary$total_tokens_used, 0L)
  expect_named(summary, c("total_decisions", "decisions_by_type",
                          "decisions_by_step", "new_codes_timeline",
                          "entries_skipped", "merge_decisions_accepted",
                          "merge_decisions_standalone",
                          "total_ai_requests", "total_tokens_used",
                          "ai_requests_by_model",
                          "methodology_modes_observed"))
})

test_that("methodology_modes_observed surfaces multiple values when mode changed mid-pipeline", {
  # Forward-looking: T1.5 will support mode_change mid-pipeline. The summary
  # field should already correctly aggregate distinct modes across records.
  td <- withr::local_tempdir()
  audit <- init_audit_log(td, config = mk_cfg("codebook_collaborative"))
  log_ai_decision(audit, "coding", "code_assignment", entry_id = "e1")
  close_audit_log(audit)

  # Manually append a record under a different methodology_mode (simulating
  # a mode change). In production this happens via T1.5's mode_change flow;
  # here we just edit the file directly to test the summary aggregation.
  con <- file(file.path(td, "ai_decisions.jsonl"), open = "a")
  writeLines(jsonlite::toJSON(list(
    timestamp = "2026-01-01T00:00:00.000+0000",
    step = "mode_change", decision_type = "mode_changed",
    methodology_mode = "framework_applied",
    parent_run_id = "prior-run-id"
  ), auto_unbox = TRUE), con)
  close(con)

  summary <- summarize_audit_log(td)
  expect_setequal(summary$methodology_modes_observed,
                  c("codebook_collaborative", "framework_applied"))
})
