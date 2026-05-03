# End-to-end tests for Mode 3 (Framework Applied) — Sprint-4 M3.x
#
# Mode 3 dispatches at two seams:
#   1. .code_entry_progressive: framework_spec is non-NULL -> AI applies
#      framework constructs verbatim; codebook is pre-populated with
#      construct ids; "anomaly" code captures non-fitting segments.
#   2. theming step (R/18_pipeline.R STEP 5): apply_framework_themes()
#      maps the codebook (which IS the framework) to a ThemeSet.
#
# Per AC2 (three modes; no fourth) and AC8 (modes are configurations of
# one architecture, never separate code paths), Mode 3 reuses the same
# per-segment processor as Mode 2 -- dispatch lives in the prompt +
# schema + codebook initialization, not in a forked code path.

# ---- run_progressive_coding pre-populates codebook with constructs ---------

test_that("run_progressive_coding (Mode 3) pre-populates codebook with framework constructs + anomaly bucket", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  spec <- load_framework_spec("tpb")
  data <- tibble::tibble(
    std_id   = c("e1", "e2"),
    std_text = c("I plan to take my medication every morning.",
                  "My doctor expects me to follow this regimen.")
  )

  # Mock ai_complete to return one construct application per entry.
  responses <- list(
    jsonlite::toJSON(list(
      skipped = FALSE, skip_reason = "",
      coded_segments = list(list(
        text = "I plan to take", start_char = 0L, end_char = 14L,
        construct_id = "intention", anomaly_reason = ""
      ))
    ), auto_unbox = TRUE),
    jsonlite::toJSON(list(
      skipped = FALSE, skip_reason = "",
      coded_segments = list(list(
        text = "doctor expects me", start_char = 3L, end_char = 20L,
        construct_id = "subjective_norm", anomaly_reason = ""
      ))
    ), auto_unbox = TRUE)
  )
  call_count <- new.env(parent = emptyenv())
  call_count$n <- 0L

  local_mocked_bindings(
    ai_complete = function(...) {
      call_count$n <- call_count$n + 1L
      list(
        content    = responses[[call_count$n]],
        model      = "claude-mock", request_id = paste0("req_", call_count$n),
        usage      = list(prompt_tokens = 1L, completion_tokens = 1L,
                          total_tokens = 2L),
        finish_reason = "stop", raw_response = list(),
        prompt_hash = "h", citations = list()
      )
    },
    .package = "pakhom"
  )

  state <- run_progressive_coding(
    data = data, provider = mock_provider("anthropic"),
    config = list(max_retries_per_entry = 1L,
                   saturation_enabled = FALSE),
    research_focus = "Medication adherence",
    framework_spec = spec
  )

  # Codebook keys are the framework's construct ids + "anomaly"
  expect_setequal(names(state$codebook),
                   c(spec$construct_ids, "anomaly"))

  # Frequencies reflect what the AI mock produced
  expect_equal(state$codebook[["intention"]]$frequency, 1L)
  expect_equal(state$codebook[["subjective_norm"]]$frequency, 1L)
  expect_equal(state$codebook[["anomaly"]]$frequency, 0L)

  # Untouched constructs still in codebook with frequency 0
  expect_equal(state$codebook[["attitude"]]$frequency, 0L)
})

test_that("run_progressive_coding (Mode 3) routes anomaly responses into the anomaly bucket", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  spec <- load_framework_spec("tpb")
  data <- tibble::tibble(
    std_id   = "e1",
    std_text = "I went to the store and bought a strange unrelated item."
  )

  mock_response <- jsonlite::toJSON(list(
    skipped = FALSE, skip_reason = "",
    coded_segments = list(list(
      text = "strange unrelated item", start_char = 33L, end_char = 55L,
      construct_id = "anomaly",
      anomaly_reason = "Off-topic content unrelated to behavior change."
    ))
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(
      content    = mock_response, model = "claude-mock", request_id = "r1",
      usage      = list(prompt_tokens = 1L, completion_tokens = 1L,
                        total_tokens = 2L),
      finish_reason = "stop", raw_response = list(),
      prompt_hash = "h", citations = list()
    ),
    .package = "pakhom"
  )

  state <- run_progressive_coding(
    data = data, provider = mock_provider("anthropic"),
    config = list(max_retries_per_entry = 1L, saturation_enabled = FALSE),
    research_focus = "TPB",
    framework_spec = spec
  )

  expect_equal(state$codebook[["anomaly"]]$frequency, 1L)
  # Anomaly segment description carries the reason
  anomaly_seg <- state$codebook[["anomaly"]]$coded_segments[[1]]
  expect_s3_class(anomaly_seg$provenance, "QuoteProvenance")
})

test_that("run_progressive_coding (Mode 3) refuses to invent new constructs (schema enforces enum)", {
  # The framework schema's `construct_id` is enum-constrained to the
  # spec's construct_ids + "anomaly". The AI provider's response_schema
  # validation would fail on any other value. We verify the schema
  # builder produces the right enum.
  spec <- load_framework_spec("tpb")
  schema <- pakhom:::.coding_schema_framework(spec$construct_ids)
  enum <- schema$properties$coded_segments$items$properties$construct_id$enum
  expect_setequal(unlist(enum),
                   c(spec$construct_ids, "anomaly"))
})

# ---- apply_framework_themes ------------------------------------------------

test_that("apply_framework_themes maps used constructs to themes; drops zero-frequency ones", {
  spec <- load_framework_spec("tpb")
  state <- create_coding_state()
  # Pre-populate codebook (mimicking a Mode 3 run)
  for (c in spec$constructs) {
    state$codebook[[c$id]] <- list(
      code_name = c$name, description = c$description,
      type = "framework_construct", frequency = 0L,
      entry_ids = character(0), coded_segments = list()
    )
  }
  state$codebook[["anomaly"]] <- list(
    code_name = "Anomaly (non-fitting)", description = "x",
    type = "anomaly", frequency = 0L,
    entry_ids = character(0), coded_segments = list()
  )
  # Two constructs received entries; rest are zero
  state$codebook[["intention"]]$frequency <- 3L
  state$codebook[["subjective_norm"]]$frequency <- 2L

  theme_set <- apply_framework_themes(state, spec)
  expect_s3_class(theme_set, "ThemeSet")
  # Only the 2 non-zero constructs become themes
  theme_names <- vapply(theme_set$themes, function(t) t$name, character(1))
  expect_setequal(theme_names,
                   c("Behavioral intention", "Subjective norm"))
})

test_that("apply_framework_themes adds an Anomaly theme when anomalies are present", {
  spec <- load_framework_spec("tpb")
  state <- create_coding_state()
  for (c in spec$constructs) {
    state$codebook[[c$id]] <- list(
      code_name = c$name, description = c$description,
      type = "framework_construct", frequency = 0L,
      entry_ids = character(0), coded_segments = list()
    )
  }
  state$codebook[["intention"]]$frequency <- 1L
  state$codebook[["anomaly"]] <- list(
    code_name = "Anomaly (non-fitting)", description = "x",
    type = "anomaly", frequency = 2L,  # has anomalies
    entry_ids = character(0), coded_segments = list()
  )

  theme_set <- apply_framework_themes(state, spec)
  theme_names <- vapply(theme_set$themes, function(t) t$name, character(1))
  expect_true("Anomaly (non-fitting)" %in% theme_names)
})

test_that("apply_framework_themes returns empty set when no constructs received entries", {
  spec <- load_framework_spec("tpb")
  state <- create_coding_state()
  for (c in spec$constructs) {
    state$codebook[[c$id]] <- list(
      code_name = c$name, description = c$description,
      type = "framework_construct", frequency = 0L,
      entry_ids = character(0), coded_segments = list()
    )
  }
  state$codebook[["anomaly"]] <- list(
    code_name = "Anomaly", description = "x", type = "anomaly",
    frequency = 0L, entry_ids = character(0), coded_segments = list()
  )

  theme_set <- apply_framework_themes(state, spec)
  expect_s3_class(theme_set, "ThemeSet")
  expect_length(theme_set$themes, 0L)
})

test_that("apply_framework_themes validates inputs", {
  spec <- load_framework_spec("tpb")
  expect_error(apply_framework_themes(list(), spec), "ProgressiveCodingState")
  expect_error(apply_framework_themes(create_coding_state(), list()),
               "FrameworkSpec")
})

test_that("apply_framework_themes preserves construct id as keywords pivot", {
  # A theme's keywords field is rendered in the report; for Mode 3,
  # keywords = the construct's example_indicators, so the report
  # surfaces the framework's signal phrases.
  spec <- load_framework_spec("tpb")
  state <- create_coding_state()
  for (c in spec$constructs) {
    state$codebook[[c$id]] <- list(
      code_name = c$name, description = c$description,
      type = "framework_construct", frequency = 0L,
      entry_ids = character(0), coded_segments = list()
    )
  }
  state$codebook[["intention"]]$frequency <- 1L

  theme_set <- apply_framework_themes(state, spec)
  intention_theme <- theme_set$themes[[1]]
  expect_equal(intention_theme$framework_construct_id, "intention")
  # Example indicators from TPB intention are surfaced
  expect_true(any(grepl("plan", intention_theme$keywords)))
})

# ---- AC2 / AC8 dispatch contract tests -------------------------------------

test_that("Mode 3 .code_entry_progressive dispatch chooses framework path when framework_spec is set", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  spec <- load_framework_spec("tpb")
  captured <- new.env(parent = emptyenv())
  local_mocked_bindings(
    ai_complete = function(provider, prompt, system_prompt, task = "coding",
                            model = NULL, temperature = NULL, max_tokens = NULL,
                            json_mode = FALSE, max_retries = 3,
                            response_schema = NULL, documents = NULL) {
      captured$response_schema <- response_schema
      captured$documents       <- documents
      captured$json_mode       <- json_mode
      captured$system_prompt   <- system_prompt
      list(
        content = jsonlite::toJSON(list(
          skipped = FALSE, skip_reason = "",
          coded_segments = list(list(
            text = "I plan", start_char = 0L, end_char = 6L,
            construct_id = "intention", anomaly_reason = ""
          ))
        ), auto_unbox = TRUE),
        model = "m", request_id = "r",
        usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                     total_tokens = 2L),
        finish_reason = "stop", raw_response = list(),
        prompt_hash = "h", citations = list()
      )
    },
    .package = "pakhom"
  )

  state <- create_coding_state()
  for (c in spec$constructs) {
    state$codebook[[c$id]] <- list(
      code_name = c$name, description = c$description,
      type = "framework_construct", frequency = 0L,
      entry_ids = character(0), coded_segments = list()
    )
  }
  state$codebook[["anomaly"]] <- list(
    code_name = "A", description = "", type = "anomaly", frequency = 0L,
    entry_ids = character(0), coded_segments = list()
  )

  pakhom:::.code_entry_progressive(
    text = "I plan to take medication.",
    entry_id = "e1", entry_index = 1L, state = state,
    provider = mock_provider("anthropic"),
    config = list(max_retries_per_entry = 1L),
    base_system_prompt = "test",
    framework_spec = spec
  )

  # Framework path: response_schema is the framework-constrained schema
  # (NOT the open .coding_schema()); documents is NULL (Mode 3 does not
  # use Citations API in the first iteration); json_mode is FALSE
  # because forced tool_use schema does the JSON enforcement.
  expect_false(is.null(captured$response_schema))
  expect_null(captured$documents)
  expect_false(captured$json_mode)
  # System prompt includes the framework block
  expect_match(captured$system_prompt, "THEORETICAL FRAMEWORK")
  expect_match(captured$system_prompt, "Theory of Planned Behavior")
})

test_that("Mode 3 dispatch uses framework even when provider is Anthropic (overrides citations path)", {
  # Per AC8 dispatch order: framework > citations > schema. Mode 3
  # takes precedence because the framework's enum constraint is more
  # restrictive than citations' free-form citation labeling.
  spec <- load_framework_spec("tpb")
  prov <- mock_provider("anthropic")
  expect_true(pakhom:::.use_citations_for_provider(prov, list()))
  # But the actual dispatch in .code_entry_progressive uses framework
  # when framework_spec is non-NULL; the documents arg is NULL in that
  # case (verified in the prior test).
})

# ---- BLOCKER fixes from phase 29 audit -------------------------------------

test_that("apply_framework_themes populates merge_history$code_to_theme_map (BLOCKER 1 regression)", {
  # The phase 29 audit caught that apply_framework_themes was returning
  # a ThemeSet without merge_history$code_to_theme_map populated, which
  # caused cascade_theme_assignments to bail out and produce
  # n_entries = 0 for every theme. Without this, Mode 3 was silently
  # broken end-to-end. This test pins the contract.
  spec <- load_framework_spec("tpb")
  state <- create_coding_state()
  for (c in spec$constructs) {
    state$codebook[[c$id]] <- list(
      code_name = c$name, description = c$description,
      type = "framework_construct", frequency = 0L,
      entry_ids = character(0), coded_segments = list()
    )
  }
  state$codebook[["intention"]]$frequency <- 1L
  state$codebook[["intention"]]$entry_ids <- "e1"
  state$codebook[["intention"]]$coded_segments <- list(list(
    entry_id = "e1", text = "I plan", start_char = 0L, end_char = 6L
  ))
  state$entry_results[["e1"]] <- list(
    codes_assigned = "intention",
    coded_segments = list(list(code_key = "intention", code_name = "Behavioral intention",
                                 text = "I plan", start_char = 0L, end_char = 6L)),
    skipped = FALSE, skip_reason = NA_character_
  )

  ts <- apply_framework_themes(state, spec)
  # merge_history must exist and contain a mapping from construct id ->
  # theme name. Without this, cascade_theme_assignments fails at
  # R/13_themes.R:559-565.
  expect_false(is.null(ts$merge_history))
  expect_false(is.null(ts$merge_history$code_to_theme_map))
  expect_equal(ts$merge_history$code_to_theme_map[["intention"]],
               "Behavioral intention")
})

test_that("Mode 3 end-to-end: cascade_theme_assignments populates theme_membership_* via apply_framework_themes", {
  # Integration regression test for the audit's BLOCKER 1: verify the
  # full chain apply_framework_themes -> cascade -> theme_membership_*.
  spec <- load_framework_spec("tpb")
  state <- create_coding_state()
  for (c in spec$constructs) {
    state$codebook[[c$id]] <- list(
      code_name = c$name, description = c$description,
      type = "framework_construct", frequency = 0L,
      entry_ids = character(0), coded_segments = list()
    )
  }
  state$codebook[["intention"]]$frequency <- 1L
  state$codebook[["intention"]]$entry_ids <- "e1"
  state$entry_results[["e1"]] <- list(
    codes_assigned = "intention",
    coded_segments = list(list(code_key = "intention", code_name = "Behavioral intention",
                                 text = "I plan", start_char = 0L, end_char = 6L)),
    skipped = FALSE, skip_reason = NA_character_
  )

  ts <- apply_framework_themes(state, spec)
  data <- tibble::tibble(std_id = "e1", std_text = "I plan to take medication.")
  data <- cascade_theme_assignments(data, state, ts)

  # cascade_theme_assignments should have produced a theme_membership_*
  # column AND assigned e1 to "Behavioral intention".
  expect_true("theme_membership_Behavioral.intention" %in% names(data))
  expect_equal(data$theme_membership_Behavioral.intention[1], 1L)
})

test_that("enrich_themes preserves Mode 3 keywords (HIGH 3 regression)", {
  # Phase 29 audit caught that enrich_themes unconditionally overwrote
  # keywords with codes_included, erasing the framework's
  # example_indicators that apply_framework_themes had set. The fix
  # detects Mode 3 themes via the framework_construct_id marker and
  # preserves their keywords.
  spec <- load_framework_spec("tpb")
  state <- create_coding_state()
  for (c in spec$constructs) {
    state$codebook[[c$id]] <- list(
      code_name = c$name, description = c$description,
      type = "framework_construct", frequency = 0L,
      entry_ids = character(0), coded_segments = list()
    )
  }
  state$codebook[["intention"]]$frequency <- 1L
  state$codebook[["intention"]]$entry_ids <- "e1"
  state$entry_results[["e1"]] <- list(
    codes_assigned = "intention",
    coded_segments = list(list(code_key = "intention", code_name = "Behavioral intention",
                                 text = "I plan", start_char = 0L, end_char = 6L)),
    skipped = FALSE, skip_reason = NA_character_
  )

  ts <- apply_framework_themes(state, spec)
  data <- tibble::tibble(std_id = "e1",
                          std_text = "I plan to take medication every day.",
                          sentiment_score = 0.5)
  data <- cascade_theme_assignments(data, state, ts)
  ts <- enrich_themes(ts, data, coding_state = state)

  # The "Behavioral intention" theme should still have the example
  # indicator phrases (e.g. "I plan to...") as keywords, NOT just the
  # construct id "intention" that codes_included contains.
  intention_theme <- ts$themes[[which(vapply(ts$themes, function(t) {
    identical(t$name, "Behavioral intention")
  }, logical(1)))]]
  # Example indicators from TPB intention include "I plan to..." etc.
  expect_true(any(grepl("plan", intention_theme$keywords)))
  # codes_included is just the construct id (single string)
  expect_equal(intention_theme$codes_included, "intention")
})

test_that("run_progressive_coding refuses Mode 3 resume from a Mode 2 state (BLOCKER 2)", {
  # Phase 29 audit caught that resuming a Mode 2 partial state under a
  # framework_spec arg would Frankenstein the codebook. Resume guard
  # added at R/09_coding.R now refuses.
  spec <- load_framework_spec("tpb")
  # Build a Mode 2-style resume state (free-form code keys, no framework
  # constructs)
  resume <- create_coding_state()
  resume$codebook[["med_helps"]] <- list(
    code_name = "med_helps", description = "x", type = "descriptive",
    frequency = 1L, entry_ids = "e1",
    coded_segments = list(list(text = "x", start_char = 0L, end_char = 1L))
  )
  data <- tibble::tibble(std_id = "e1", std_text = "x")

  expect_error(
    suppressMessages(suppressWarnings(
      run_progressive_coding(
        data = data,
        provider = mock_provider("openai"),
        config = list(saturation_enabled = FALSE),
        research_focus = "test",
        resume_state = resume,
        framework_spec = spec
      )
    )),
    "Mode 3 resume guard"
  )
})

# ---- Pipeline-level Mode 1 friendly error ----------------------------------

test_that("Pipeline source contains Mode 1 friendly-error block (dispatch is wired)", {
  # The dispatch at the top of run_analysis() refuses Mode 1 with a
  # friendly error pointing to Mode 2 / Mode 3 alternatives. End-to-end
  # invocation of run_analysis() requires too much config plumbing to
  # set up cleanly inside an isolated test, so we verify the source
  # contains the dispatch block. This is a smoke check rather than a
  # behavioral test; the pipeline's actual Mode 1 path is exercised by
  # the broader integration tests at the package-test layer.
  src_path <- system.file("R", "18_pipeline.R", package = "pakhom")
  if (!nzchar(src_path) || !file.exists(src_path)) {
    # devtools::load_all() context: read from the source dir
    candidates <- c("R/18_pipeline.R",
                     file.path("..", "..", "R", "18_pipeline.R"),
                     file.path(testthat::test_path(), "..", "..", "R",
                                "18_pipeline.R"))
    src_path <- Filter(file.exists, candidates)[1]
  }
  skip_if(is.na(src_path) || !nzchar(src_path) || !file.exists(src_path),
           "pipeline source not locatable in this run context")

  src <- paste(readLines(src_path), collapse = "\n")
  expect_match(src, "Mode 1 \\(Reflexive Scaffold\\)")
  # Source points users at the dedicated Mode 1 entry point (phase 31:
  # run_mode1 superseded run_provocateur_questioning as the canonical
  # Mode 1 orchestrator -- both must be referenced so the bare-loop
  # entry point and the scaffolded entry point are both visible)
  expect_match(src, "run_mode1")
  expect_match(src, "run_provocateur_questioning")
  expect_match(src, "codebook_collaborative")
  expect_match(src, "framework_applied")
})
