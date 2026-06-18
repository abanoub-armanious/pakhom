# End-to-end tests for Mode 3 (Framework Applied) covering M3.x
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
    std_text = c("I plan to take my scheduling every morning.",
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
    config = list(max_retries_per_entry = 1L),
    research_focus = "Schedule adherence",
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

test_that("run_progressive_coding (Mode 3) routes an OUT-OF-FRAMEWORK construct_id to the anomaly bucket (defensive)", {
  # The response schema enum-constrains construct_id, but if a provider/json_mode
  # path lets an out-of-framework id slip through, the runtime must NOT admit it
  # as a new construct (which would let the model invent constructs and drop
  # those entries out of every framework theme). It is re-routed to anomaly.
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5 for local_mocked_bindings")

  spec <- load_framework_spec("tpb")
  data <- tibble::tibble(std_id = "e1", std_text = "Some text the model mislabels.")

  mock_response <- jsonlite::toJSON(list(
    skipped = FALSE, skip_reason = "",
    coded_segments = list(list(
      text = "Some text the model", start_char = 0L, end_char = 19L,
      construct_id = "invented_construct",   # NOT a TPB construct and not "anomaly"
      anomaly_reason = ""
    ))
  ), auto_unbox = TRUE)

  local_mocked_bindings(
    ai_complete = function(...) list(
      content = mock_response, model = "claude-mock", request_id = "req_1",
      usage = list(prompt_tokens = 1L, completion_tokens = 1L, total_tokens = 2L),
      finish_reason = "stop", raw_response = list(), prompt_hash = "h",
      citations = list()
    ),
    .package = "pakhom"
  )

  state <- suppressWarnings(run_progressive_coding(
    data = data, provider = mock_provider("anthropic"),
    config = list(max_retries_per_entry = 1L),
    research_focus = "x", framework_spec = spec
  ))

  expect_false("invented_construct" %in% names(state$codebook))
  expect_setequal(names(state$codebook), c(spec$construct_ids, "anomaly"))
  expect_equal(state$codebook[["anomaly"]]$frequency, 1L)
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
    config = list(max_retries_per_entry = 1L),
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
    text = "I plan to take scheduling.",
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

# ---- BLOCKER fixes from an earlier audit ----------------------------------

test_that("apply_framework_themes populates merge_history$code_to_theme_map (BLOCKER 1 regression)", {
  # An earlier audit caught that apply_framework_themes was returning
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
  data <- tibble::tibble(std_id = "e1", std_text = "I plan to take scheduling.")
  data <- cascade_theme_assignments(data, state, ts)

  # cascade_theme_assignments should have produced a theme_membership_*
  # column AND assigned e1 to "Behavioral intention".
  expect_true("theme_membership_Behavioral.intention" %in% names(data))
  expect_equal(data$theme_membership_Behavioral.intention[1], 1L)
})

test_that("enrich_themes preserves Mode 3 keywords (HIGH 3 regression)", {
  # An earlier audit caught that enrich_themes unconditionally overwrote
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
                          std_text = "I plan to take scheduling every day.",
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
  # codes_included is now the denormalised character vector of
  # code NAMES across the theme's hierarchy. For Mode 3 framework themes
  # this is the construct's display name (which shows in the report and
  # JSON) rather than the construct id (technical key). The id is still
  # the canonical lookup key in coding_state$codebook + resolves through
  # rebuild_code_to_theme_map for cascade.
  expect_equal(intention_theme$codes_included, "Behavioral intention")
  expect_equal(theme_code_keys(intention_theme), "intention")
})

test_that("run_progressive_coding refuses Mode 3 resume from a Mode 2 state (BLOCKER 2)", {
  # An earlier audit caught that resuming a Mode 2 partial state under a
  # framework_spec arg would Frankenstein the codebook. Resume guard
  # added at R/09_coding.R now refuses.
  spec <- load_framework_spec("tpb")
  # Build a Mode 2-style resume state (free-form code keys, no framework
  # constructs)
  resume <- create_coding_state()
  resume$codebook[["tool_helps"]] <- list(
    code_name = "tool_helps", description = "x", type = "descriptive",
    frequency = 1L, entry_ids = "e1",
    coded_segments = list(list(text = "x", start_char = 0L, end_char = 1L))
  )
  data <- tibble::tibble(std_id = "e1", std_text = "x")

  expect_error(
    suppressMessages(suppressWarnings(
      run_progressive_coding(
        data = data,
        provider = mock_provider("openai"),
        config = list(),
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
  # Source points users at the dedicated Mode 1 entry point (historically,
  # run_mode1 superseded run_provocateur_questioning as the canonical
  # Mode 1 orchestrator -- both must be referenced so the bare-loop
  # entry point and the scaffolded entry point are both visible)
  expect_match(src, "run_mode1")
  expect_match(src, "run_provocateur_questioning")
  expect_match(src, "codebook_collaborative")
  expect_match(src, "framework_applied")
})

# ---- Framework Declaration section + Citations API footnote ----

test_that(".build_framework_declaration renders framework name + sha256 + citations + stance + policy + constructs", {
  spec <- load_framework_spec("tpb")
  d <- withr::local_tempdir()
  arch <- archive_framework_spec(spec, d)
  html <- pakhom:::.build_framework_declaration(spec, arch)
  expect_match(html, "Theoretical Framework \\(Mode 3 / AC4\\)")
  expect_match(html, "Theory of Planned Behavior")
  expect_match(html, "sha256:")
  # Hash short fingerprint = first 12 chars of arch$hash
  expect_match(html, substr(arch$hash, 1, 12), fixed = TRUE)
  # Citations -- TPB ships with the Ajzen 1991 + 2020 references
  expect_match(html, "Ajzen", fixed = TRUE)
  expect_match(html, "1991", fixed = TRUE)
  # Epistemic stance + plain-language explainer
  expect_match(html, "positivist")
  expect_match(html, "brackets data that doesn't fit")
  # Anomaly policy + plain-language explainer. The bracket
  # explainer no longer says "out-of-scope" (that was the earlier
  # phrasing); it now says "single 'Anomaly (non-fitting)' theme" to
  # match the actual behavior + contrasts it with extend/revise.
  expect_match(html, "bracket")
  expect_match(html, "single \"Anomaly")
  # All five TPB constructs surface
  for (cid in c("attitude", "subjective_norm",
                  "perceived_behavioral_control", "intention", "behavior")) {
    expect_match(html, sprintf("<code>%s</code>", cid))
  }
  # Archive link
  expect_match(html, sprintf('href="%s"', arch$relative_path))
})

test_that(".build_framework_declaration renders the unavailable variant when spec is NULL", {
  html <- pakhom:::.build_framework_declaration(NULL, NULL)
  expect_match(html, "framework-unavailable")
  expect_match(html, "transparency failure")
})

test_that(".build_framework_declaration renders the unavailable variant for non-FrameworkSpec input", {
  html <- pakhom:::.build_framework_declaration(list(name = "fake"), NULL)
  expect_match(html, "framework-unavailable")
})

test_that(".build_framework_declaration handles spec without an archive (no hash, no link)", {
  spec <- load_framework_spec("tpb")
  html <- pakhom:::.build_framework_declaration(spec, archive = NULL)
  expect_match(html, "Theory of Planned Behavior")
  expect_no_match(html, "sha256:")
  expect_no_match(html, "archived spec")
})

test_that(".build_framework_declaration HTML-escapes framework name + construct fields", {
  # Synthetic spec with researcher-supplied content that contains HTML-
  # active characters in name + construct description. The renderer must
  # escape every interpolation -- a malicious or careless framework spec
  # should not be able to inject arbitrary HTML / JS into a Mode 3
  # report. Audit-pattern fix ahead of time.
  d <- withr::local_tempdir()
  yaml_path <- file.path(d, "evil.yaml")
  writeLines(c(
    "framework:",
    "  name: '<script>alert(1)</script>'",
    "  citations:",
    "    - 'A & B (2024) <em>title</em>'",
    "  epistemic_stance: 'mixed'",
    "  anomaly_handling: 'extend'",
    "  constructs:",
    "    - id: c1",
    "      name: 'C <one>'",
    "      description: '\"first\" & only'",
    "      example_indicators:",
    "        - '<b>example</b>'"
  ), yaml_path)
  spec <- load_framework_spec(yaml_path)
  arch <- archive_framework_spec(spec, d)
  html <- pakhom:::.build_framework_declaration(spec, arch)
  expect_no_match(html, "<script>alert\\(1\\)</script>")
  # Escaped form must be present
  expect_match(html, "&lt;script&gt;")
  expect_match(html, "&amp;")
})

test_that(".tier0_citations_api_bypass_footnote fires only on Mode 3 + Anthropic", {
  fn_m3_anth <- pakhom:::.tier0_citations_api_bypass_footnote(list(
    methodology = list(mode = "framework_applied"),
    ai = list(provider = "anthropic")
  ))
  expect_match(fn_m3_anth, "structurally precluded")
  expect_match(fn_m3_anth, "tool_use", fixed = TRUE)

  # All other combos return ""
  fn_m3_oai <- pakhom:::.tier0_citations_api_bypass_footnote(list(
    methodology = list(mode = "framework_applied"),
    ai = list(provider = "openai")
  ))
  expect_equal(fn_m3_oai, "")

  fn_m2_anth <- pakhom:::.tier0_citations_api_bypass_footnote(list(
    methodology = list(mode = "codebook_collaborative"),
    ai = list(provider = "anthropic")
  ))
  expect_equal(fn_m2_anth, "")

  fn_m1_anth <- pakhom:::.tier0_citations_api_bypass_footnote(list(
    methodology = list(mode = "reflexive_scaffold"),
    ai = list(provider = "anthropic")
  ))
  expect_equal(fn_m1_anth, "")

  fn_null <- pakhom:::.tier0_citations_api_bypass_footnote(NULL)
  expect_equal(fn_null, "")
})

test_that(".build_tier0_source_block plumbs config through to bypass footnote", {
  # Build a stats object with quotes from model_freeform only (the
  # canonical Mode 3 + Anthropic shape), then verify the rendered
  # source block carries the bypass footnote when config indicates
  # Mode 3 + Anthropic.
  src <- "I plan to take my scheduling every day."
  q <- make_quote("e1", "data_entry", src, 0L, 6L, "I plan",
                    citation_source = "model_freeform")
  q <- verify_quote(q, src)
  stats <- quote_provenance_summary(list(q))

  html_m3 <- pakhom:::.build_tier0_source_block(stats, config = list(
    methodology = list(mode = "framework_applied"),
    ai = list(provider = "anthropic")
  ))
  expect_match(html_m3, "structurally precluded")

  html_m2 <- pakhom:::.build_tier0_source_block(stats, config = list(
    methodology = list(mode = "codebook_collaborative"),
    ai = list(provider = "anthropic")
  ))
  expect_no_match(html_m2, "structurally precluded")
})

test_that("verify_run_integrity expects framework_applied.{yaml|yml|json} when Mode 3", {
  d <- withr::local_tempdir()
  cfg <- list(methodology = list(mode = "framework_applied"),
                output = list(generate_report = FALSE),
                audit = list(capture_raw_responses = FALSE))
  res <- verify_run_integrity(d, cfg)
  expect_true("framework_applied.{yaml|yml|json}" %in% res$expected)
  expect_true("framework_applied.{yaml|yml|json}" %in% res$missing)
  expect_false(res$complete)

  # Now drop a fake framework_applied.yaml in run_dir
  writeLines("framework: { name: stub, constructs: [] }",
             file.path(d, "framework_applied.yaml"))
  # Drop the OTHER mandatory artifacts so we test only the framework path
  for (f in c("sentiment_scores.csv", "codes.csv",
                "correlations.csv", "themes.json", "analysis_report.Rmd",
                "run_metadata.json", "fabrication_log.csv",
                "ai_decisions.jsonl")) {
    writeLines("", file.path(d, f))
  }
  dir.create(file.path(d, "theme_entries"))
  dir.create(file.path(d, "rules"))
  writeLines("", file.path(d, "rules", "methodology_rules.md"))
  res2 <- verify_run_integrity(d, cfg)
  expect_false("framework_applied.{yaml|yml|json}" %in% res2$missing)
  expect_true("framework_applied.{yaml|yml|json}" %in% res2$present)
})

test_that("verify_run_integrity does NOT expect correlation_plot.png when correlations.csv has no rows", {
  # Small samples (or any run with no overlapping theme-pair
  # data) produce a 0-row correlations.csv and skip the plot. The
  # integrity check should treat the plot as expected only when there's
  # actually data to plot.
  d <- withr::local_tempdir()
  cfg <- list(methodology = list(mode = "codebook_collaborative"),
                output = list(generate_report = FALSE,
                                generate_correlation_plot = TRUE),
                audit = list(capture_raw_responses = FALSE))
  # Drop a header-only correlations.csv (the pipeline writes this when
  # there are no correlation pairs to extract)
  writeLines("var1,var2,correlation,p_value,significant,effect_size",
             file.path(d, "correlations.csv"))
  res <- verify_run_integrity(d, cfg)
  expect_false("correlation_plot.png" %in% res$expected)
})

test_that("verify_run_integrity DOES expect correlation_plot.png when correlations.csv has data rows", {
  d <- withr::local_tempdir()
  cfg <- list(methodology = list(mode = "codebook_collaborative"),
                output = list(generate_report = FALSE,
                                generate_correlation_plot = TRUE),
                audit = list(capture_raw_responses = FALSE))
  writeLines(c("var1,var2,correlation,p_value,significant,effect_size",
               "a,b,0.5,0.01,TRUE,medium"),
             file.path(d, "correlations.csv"))
  res <- verify_run_integrity(d, cfg)
  expect_true("correlation_plot.png" %in% res$expected)
  # And it gets flagged as missing because we didn't drop the PNG
  expect_true("correlation_plot.png" %in% res$missing)
})

test_that("verify_run_integrity reads stamped correlations.csv (round-trip with comment header)", {
  # Defense-in-depth: the new heuristic reads correlations.csv with
  # comment="#" so a stamped CSV doesn't poison the row-count.
  d <- withr::local_tempdir()
  cfg <- list(methodology = list(mode = "codebook_collaborative"),
                output = list(generate_report = FALSE,
                                generate_correlation_plot = TRUE),
                audit = list(capture_raw_responses = FALSE))
  csv_path <- file.path(d, "correlations.csv")
  writeLines(c("var1,var2,correlation,p_value,significant,effect_size",
               "a,b,0.5,0.01,TRUE,medium"),
             csv_path)
  stamp_methodology_csv(csv_path, "codebook_collaborative", run_id = "r1")
  res <- verify_run_integrity(d, cfg)
  # Stamped + has data row -> still expected
  expect_true("correlation_plot.png" %in% res$expected)
})

test_that("verify_run_integrity for Mode 2/Mode 3 differs only in framework expectation", {
  d <- withr::local_tempdir()
  cfg_m2 <- list(methodology = list(mode = "codebook_collaborative"),
                   output = list(generate_report = FALSE),
                   audit = list(capture_raw_responses = FALSE))
  cfg_m3 <- list(methodology = list(mode = "framework_applied"),
                   output = list(generate_report = FALSE),
                   audit = list(capture_raw_responses = FALSE))
  m2 <- verify_run_integrity(d, cfg_m2)
  m3 <- verify_run_integrity(d, cfg_m3)
  expect_false("framework_applied.{yaml|yml|json}" %in% m2$expected)
  expect_true("framework_applied.{yaml|yml|json}" %in% m3$expected)
  # All other expected files are identical
  expect_setequal(setdiff(m3$expected, m2$expected),
                    "framework_applied.{yaml|yml|json}")
})

test_that("Mode 3 report integration: generate_report writes an Rmd with Framework Declaration + bypass footnote", {
  # Audit H1: the previous version of this test
  # wrapped generate_report in a tryCatch that swallowed the error and
  # then skip()ped if the Rmd was never written. That made the test
  # silently pass for ANY future regression of the Mode 3 wiring --
  # the audit caught the silent skip. The fix here is twofold:
  #   1. Build a sufficient input set (full export_files, complete
  #      theme_set, sentiment data on the entries) so generate_report
  #      actually completes.
  #   2. Remove the skip-fallback: any failure to write the Rmd or
  #      missing Mode 3 content in the Rmd is now a hard test failure.
  skip_if_not(rmarkdown::pandoc_available() ||
                dir.exists("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"),
              "Requires pandoc")

  spec <- load_framework_spec("tpb")
  out_dir <- withr::local_tempdir()
  arch <- archive_framework_spec(spec, out_dir)

  # Build a richer data fixture: 6 entries with varied sentiment so
  # aggregate_overall_statistics's sentiment + emotion summaries
  # produce non-degenerate values that downstream Rmd builders
  # (e.g., .build_emotional_landscape) format without crashing.
  data <- tibble::tibble(
    std_id   = paste0("e", 1:6),
    std_text = c("I plan to take it",
                  "My doctor expects me to",
                  "I always forget",
                  "Side effects are tough",
                  "I really don't think it helps",
                  "I have control over my schedule"),
    std_author = paste0("user", 1:6),
    sentiment_score = c(0.5, 0.4, -0.3, -0.5, -0.7, 0.6),
    all_emotions = c("hope", "concern", "frustration", "frustration",
                       "doubt", "control"),
    emotion_intensity = c(0.5, 0.4, 0.6, 0.7, 0.8, 0.5),
    confidence = rep(0.9, 6),
    emerged_themes = c("intention", "subjective_norm", "intention",
                        "perceived_behavioral_control",
                        "attitude", "perceived_behavioral_control"),
    theme_membership_intention                    = c(1L, 0L, 1L, 0L, 0L, 0L),
    theme_membership_subjective_norm              = c(0L, 1L, 0L, 0L, 0L, 0L),
    theme_membership_perceived_behavioral_control = c(0L, 0L, 0L, 1L, 0L, 1L),
    theme_membership_attitude                     = c(0L, 0L, 0L, 0L, 1L, 0L)
  )
  ts <- create_theme_set(list(
    list(id = 1L, name = "intention", description = "Behavioral intention",
         codes_included = "intention"),
    list(id = 2L, name = "subjective_norm",
         description = "Perceived social pressure",
         codes_included = "subjective_norm"),
    list(id = 3L, name = "perceived_behavioral_control",
         description = "Perceived ease/difficulty",
         codes_included = "perceived_behavioral_control"),
    list(id = 4L, name = "attitude",
         description = "Favorable/unfavorable evaluation",
         codes_included = "attitude")
  ))
  # Enrich theme_set so per-theme stats (supporting_quotes, code_count)
  # don't crash downstream rendering helpers.
  for (i in seq_along(ts$themes)) {
    ts$themes[[i]]$supporting_quotes <- character(0)
  }

  cs <- create_coding_state()
  cs$entries_processed <- data$std_id
  for (sid in data$std_id) {
    cs$entry_results[[sid]] <- list(skipped = FALSE)
  }

  # Provide an export_files list that mirrors the shape
  # export_results() would produce; the Rmd builders read these
  # filenames into chunks (basename(...) calls). The files don't have
  # to exist on disk -- the Rmd builder just emits paths into the
  # rendered Rmd.
  export_files <- list(
    sentiment_file    = file.path(out_dir, "sentiment_scores.csv"),
    codes_file        = file.path(out_dir, "codes.csv"),
    correlations_file = file.path(out_dir, "correlations.csv"),
    themes_file       = file.path(out_dir, "themes.json"),
    plot_file         = file.path(out_dir, "correlation_plot.png"),
    theme_csv_files   = list()
  )

  cfg <- list(
    methodology = list(mode = "framework_applied",
                          framework_spec_path = spec$source_path),
    study = list(name = "framework-decl-test",
                   research_focus = "Mode 3 e2e smoke",
                   research_context = "test"),
    ai = list(provider = "anthropic"),
    output = list(generate_report = TRUE,
                    generate_correlation_plot = FALSE),
    audit = list(capture_raw_responses = FALSE)
  )

  # Drive generate_report. Audit H1 fix: do NOT wrap in tryCatch --
  # any error (including the previous "a character vector argument
  # expected" from missing export_files) must surface as a test
  # failure, not a silent skip.
  result <- generate_report(
    data = data, theme_set = ts, correlations_df = NULL,
    insights = list(), export_files = export_files,
    consolidated = NULL, learning_context = NULL,
    provider = NULL, config = cfg,
    output_file = file.path(out_dir, "test_report.html"),
    coding_state = cs,
    framework_spec = spec,
    framework_archive = arch
  )

  # The Rmd MUST exist (audit H1: no skip-fallback).
  rmd_path <- file.path(out_dir, "test_report.Rmd")
  expect_true(file.exists(rmd_path),
              info = "generate_report must write an Rmd; previous silent-skip is now a hard fail")

  rmd <- paste(readLines(rmd_path, warn = FALSE), collapse = "\n")
  # Framework Declaration section is present (Mode 3 specific)
  expect_match(rmd, "Theoretical Framework \\(Mode 3 / AC4\\)")
  expect_match(rmd, "Theory of Planned Behavior", fixed = TRUE)
  expect_match(rmd, substr(arch$hash, 1, 12), fixed = TRUE)
  # All five TPB constructs surface in the Rmd
  for (cid in c("attitude", "subjective_norm",
                  "perceived_behavioral_control", "intention", "behavior")) {
    expect_match(rmd, sprintf("<code>%s</code>", cid))
  }
  # Bypass footnote fires (Mode 3 + Anthropic)
  expect_match(rmd, "structurally precluded")
})

test_that("Mode 2 report does NOT include the Framework Declaration section", {
  # Mode 2 / Mode 1 reports should never render the Framework
  # Declaration -- the gating condition in .build_rmd_content is
  # `if (identical(meth_mode, "framework_applied"))`. Pin the
  # negative assertion so a future refactor that loosens the gate
  # surfaces in tests.
  skip_if_not(rmarkdown::pandoc_available() ||
                dir.exists("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"),
              "Requires pandoc")

  out_dir <- withr::local_tempdir()
  data <- tibble::tibble(
    std_id   = paste0("e", 1:3),
    std_text = letters[1:3],
    std_author = c("a", "b", "c"),
    sentiment_score = c(0.1, -0.1, 0.3),
    all_emotions = c("hope", "doubt", "hope"),
    emotion_intensity = rep(0.5, 3),
    confidence = rep(0.9, 3),
    emerged_themes = c("T1", "T1", "T2"),
    theme_membership_T1 = c(1L, 1L, 0L),
    theme_membership_T2 = c(0L, 0L, 1L)
  )
  ts <- create_theme_set(list(
    list(id = 1L, name = "T1", description = "first",  codes_included = "x"),
    list(id = 2L, name = "T2", description = "second", codes_included = "y")
  ))
  cs <- create_coding_state()
  cs$entries_processed <- data$std_id
  for (sid in data$std_id) cs$entry_results[[sid]] <- list(skipped = FALSE)

  export_files <- list(
    sentiment_file = file.path(out_dir, "sentiment_scores.csv"),
    codes_file     = file.path(out_dir, "codes.csv"),
    correlations_file = file.path(out_dir, "correlations.csv"),
    themes_file    = file.path(out_dir, "themes.json"),
    plot_file      = file.path(out_dir, "correlation_plot.png"),
    theme_csv_files = list()
  )
  cfg <- list(
    methodology = list(mode = "codebook_collaborative"),
    study = list(name = "test", research_focus = "y"),
    ai = list(provider = "openai"),
    output = list(generate_report = TRUE, generate_correlation_plot = FALSE),
    audit = list(capture_raw_responses = FALSE)
  )

  generate_report(
    data = data, theme_set = ts, correlations_df = NULL,
    insights = list(), export_files = export_files,
    consolidated = NULL, learning_context = NULL,
    provider = NULL, config = cfg,
    output_file = file.path(out_dir, "test_report.html"),
    coding_state = cs
    # framework_spec/framework_archive intentionally NULL on Mode 2
  )

  rmd_path <- file.path(out_dir, "test_report.Rmd")
  expect_true(file.exists(rmd_path))
  rmd <- paste(readLines(rmd_path, warn = FALSE), collapse = "\n")
  expect_no_match(rmd, "Theoretical Framework \\(Mode 3 / AC4\\)")
  expect_no_match(rmd, "framework-card")
  # The bypass footnote must also NOT fire on Mode 2 even with
  # provider != anthropic
  expect_no_match(rmd, "structurally precluded")
})
