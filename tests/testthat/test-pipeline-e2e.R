# End-to-end pipeline integration tests
# ==============================================================================
# These tests drive run_analysis() through the full pipeline (data load ->
# coding -> sentiment -> theme generation -> correlations -> report ->
# finalize_run) with a smart mock for ai_complete that returns task-
# appropriate JSON. The goal is to pin the architectural commitments
# (AC4 stamping, AC5 soft-lock, AC7 Tier-0 universals, AC8 mode-specific
# artifact divergence) against silent regression of the kind that has
# bitten the package three times before:
#   - dead-code paths (T0.2 spread-aware quote selection)
#   - silent overwrite of finalized runs on resume (AC5 violation)
#   - apply_framework_themes not populating merge_history (Mode 3 e2e)
# An earlier audit (MEDIUM #4) explicitly called out the gap: existing
# component-level tests would not catch any of these; these e2e tests close it.
# ==============================================================================

# ---- Smart ai_complete mock: returns task-appropriate JSON --------------

#' Smart mock for ai_complete that branches on `task` to return shape-
#' appropriate JSON for each pipeline call site. The mock is pragmatic --
#' minimal valid responses are enough to drive the pipeline through;
#' parse_json_safely + tryCatch in the production code handle any
#' minor schema gaps gracefully.
#'
#' \code{coding_code_name} (default `"NEW: mock_code"`) controls the
#' code label the coding-task path returns. Set to a framework
#' construct id (e.g., `"attitude"`) to exercise the Mode 3 happy-path
#' (apply_framework_themes -> non-empty theme_set -> cascade_theme_assignments
#' populates theme_membership_*). Audit H2: without this
#' lever, every Mode 3 e2e test would silently exercise only the
#' empty-theme-set branch -- exactly the path that hid the earlier
#' apply_framework_themes-not-populating-merge_history bug.
#' @keywords internal
.smart_mock_ai_complete <- function(known_entry_ids = c("e1", "e2", "e3", "e4", "e5"),
                                      coding_code_name = "NEW: mock_code") {
  # Return a function suitable for local_mocked_bindings(ai_complete = ...).
  # The closure captures known_entry_ids so the sentiment + coding
  # responses can reference real ids in the test fixture.
  call_count <- new.env(parent = emptyenv())
  call_count$n <- 0L

  build_response <- function(content) {
    list(
      content       = content,
      model         = "mock-model",
      request_id    = paste0("req-", as.integer(Sys.time()), "-",
                              call_count$n),
      usage         = list(prompt_tokens = 10L, completion_tokens = 5L,
                            total_tokens = 15L),
      finish_reason = "stop",
      raw_response  = list(),
      prompt_hash   = "hash",
      citations     = list()
    )
  }

  function(provider, prompt, system_prompt = NULL, task = "coding",
           model = NULL, temperature = NULL, max_tokens = NULL,
           json_mode = FALSE, max_retries = 3,
           response_schema = NULL, documents = NULL,
           methodology_override = NULL) {
    call_count$n <- call_count$n + 1L

    content <- switch(task,
      "coding" = {
        # Extract the actual entry text from the prompt so the
        # returned segment is verbatim (otherwise verify_quote drops
        # it as "fabricated" and the codebook stays empty -- exactly
        # the silent failure mode that masked the audit H2 finding).
        # The Mode 2 / Mode 3 prompts wrap
        # entry text in `<entry_text>...</entry_text>` fences (earlier
        # prompts used `Entry text: "..."`). Match the new fence
        # first, then fall back to the legacy format for back-compat
        # with older fixtures.
        m <- regmatches(prompt,
                          regexpr("<entry_text>[^<]+", prompt, perl = TRUE))
        slice <- if (length(m) > 0L && nzchar(m)) {
          inner <- sub("^<entry_text>", "", m)
          substr(inner, 1L, min(15L, nchar(inner)))
        } else {
          # Legacy `Entry text: "..."` format
          m2 <- regmatches(prompt,
                            regexpr('Entry text:\\s*"([^"]{1,200})', prompt))
          if (length(m2) > 0L && nzchar(m2)) {
            inner <- sub('^Entry text:\\s*"', "", m2)
            substr(inner, 1L, min(15L, nchar(inner)))
          } else {
            "test"  # fallback; will likely fail verify_quote in production
          }
        }
        # Include both Mode 2 fields (code, code_description, code_type)
        # and Mode 3 fields (construct_id, anomaly_reason) so a single
        # mock works across both modes. Audit H2: Mode 3's parser
        # reads `construct_id` (R/09_coding.R:858), Mode 2's reads
        # `code` (line 870). Setting both is harmless in either mode.
        # additionalProperties=FALSE in the schema is enforced only at
        # the LLM-provider level, which we bypass.
        jsonlite::toJSON(list(
          skipped = FALSE,
          skip_reason = "",
          coded_segments = list(list(
            text = slice,
            start_char = 0L,
            end_char = nchar(slice),
            # Mode 2 fields
            code = coding_code_name,
            code_description = "Test code from mock",
            code_type = "descriptive",
            confidence = 0.8,
            # Mode 3 fields
            construct_id = coding_code_name,
            anomaly_reason = ""
          ))
        ), auto_unbox = TRUE)
      },

      "sentiment" = jsonlite::toJSON(list(
        results = lapply(seq_along(known_entry_ids), function(i) {
          list(
            id                = i,
            sentiment_score   = 0.0,
            confidence        = 0.7,
            emotions          = list("neutral"),
            emotion_intensity = 0.5
          )
        })
      ), auto_unbox = TRUE),

      # Legacy single-call theming-task mock response (decision =
      # split_required). It keeps the mock pipeline functional --
      # resolving to one theme per code -- without requiring real
      # embeddings or AI judgment.
      "theming" = jsonlite::toJSON(list(
        central_organizing_concept = "mocked: no unifying principle",
        decision = "split_required",
        proposed_name = NULL,
        proposed_description = NULL,
        rationale = "mocked: defaulting to split for test coverage"
      ), auto_unbox = TRUE, null = "null"),

      "insight" = jsonlite::toJSON(list(
        key_findings = list(list(
          insight = "Mocked insight: low sentiment in some entries",
          explanation = "Mock-generated"
        )),
        theoretical_implications = "Mock theoretical implication.",
        practical_implications = "Mock practical implication."
      ), auto_unbox = TRUE),

      "synthesis" = jsonlite::toJSON(list(
        executive_summary = "Mock executive summary covering the analytic findings.",
        conclusion = "Mock conclusion."
      ), auto_unbox = TRUE),

      "review" = jsonlite::toJSON(list(provocations = list()),
                                    auto_unbox = TRUE),

      "saturation_check" = jsonlite::toJSON(list(
        saturated = FALSE,
        reason = "Mock: not saturated"
      ), auto_unbox = TRUE),

      # The Methodology Assistant (Step 2.5) makes two
      # task="methodology" calls -- a relevance-criterion call (its prompt
      # contains "CORPUS SAMPLE") and a metric-interpretation call. Return
      # shape-valid JSON for each so Step 2.5 does not (correctly) abort.
      "methodology" = if (grepl("CORPUS SAMPLE", prompt, fixed = TRUE)) {
        jsonlite::toJSON(list(
          research_focus_paraphrase = "Scheduling, focus, and overwork.",
          relevance_criterion = "A segment is on-focus when it concerns the research focus directly.",
          on_focus_examples  = list("scheduling affected my focus"),
          off_focus_examples = list("unrelated small talk"),
          discrimination_principle = "On-focus segments tie to the focus; adjacent ones do not."
        ), auto_unbox = TRUE)
      } else {
        jsonlite::toJSON(list(metrics = list(), temporal_columns = list()),
                         auto_unbox = TRUE)
      },

      # Default: empty object so any uncovered task degrades gracefully
      "{}"
    )

    build_response(content)
  }
}

# ---- Helpers (extend the existing create_test_db / create_test_config) ----

# Reuse helpers defined in test-pipeline.R (create_test_config, create_test_db)
# by including their definitions here (testthat sources files in alphabetical
# order; e2e file loads after pipeline file but the helpers may not be in
# scope when e2e runs in isolation). Define our own minimal helpers for
# robustness across run-this-file-alone invocations.

.e2e_create_test_db <- function(db_path, n_posts = 5) {
  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)
  DBI::dbExecute(db, "CREATE TABLE IF NOT EXISTS posts (
    post_id TEXT PRIMARY KEY, subreddit TEXT, title TEXT, body TEXT,
    author TEXT, score INTEGER, num_comments INTEGER,
    created_utc REAL, scraped_at TEXT, permalink TEXT
  )")
  posts <- data.frame(
    post_id      = paste0("p_", seq_len(n_posts)),
    subreddit    = "test",
    title        = paste0("Test title ", seq_len(n_posts)),
    body = c(
      "I have trouble focusing after taking my scheduling at night.",
      "Overwork episodes have decreased since starting program.",
      "The side effects of the policy make me feel exhausted all day.",
      "My deep-work quality improved significantly with the new schedule.",
      "I feel anxious about overworking and it affects my focus patterns."
    )[seq_len(n_posts)],
    author       = paste0("user_", seq_len(n_posts)),
    score        = seq_len(n_posts) * 10L,
    num_comments = seq_len(n_posts),
    created_utc  = as.numeric(Sys.time()) - seq_len(n_posts) * 3600,
    scraped_at   = as.character(Sys.time()),
    permalink    = paste0("/r/test/comments/p_", seq_len(n_posts), "/"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(db, "posts", posts, append = TRUE)
  invisible(db_path)
}

.e2e_config <- function(db_path, output_dir,
                          mode = "codebook_collaborative",
                          framework_spec_path = NULL,
                          generate_report = TRUE,
                          provider = "openai") {
  cfg <- list(
    methodology = list(
      mode = mode,
      framework_spec_path = framework_spec_path,
      mode_locked_at = NULL,
      parent_run_id = NULL,
      mode_changed_from = NULL
    ),
    study = list(
      name = "E2E Test",
      research_focus = "integration e2e",
      research_context = "integration tests",
      concepts = c("focus", "scheduling")
    ),
    ai = list(
      provider = provider,
      openai = list(api_key = "sk-test-fake-e2e",
                      models = list(primary = "gpt-4o", fast = "gpt-4o-mini"),
                      rate_limits = list(rpm = 500, tpm = 150000)),
      anthropic = list(api_key = "sk-ant-test-fake-e2e",
                         models = list(primary = "claude-sonnet-4-20250514"),
                         rate_limits = list(rpm = 500, tpm = 150000))
    ),
    data = list(
      database = db_path,
      tables = "posts",
      source_type = "reddit",
      # Audit M3: the production preprocess_text reads min_text_length,
      # NOT min_char. Using the wrong field name silently degrades to
      # the default. Use the right name in the e2e fixture so the
      # config we ship reflects production reality.
      preprocessing = list(min_text_length = 10, dedup_ratio = 0.9)
    ),
    learning = list(enabled = FALSE),
    analysis = list(
      test_mode = list(enabled = TRUE, sample_size = 3, seed = 42),
      sentiment = list(code_aware = TRUE, batch_size = 10,
                          dynamic_batching = FALSE),
      coding = list(progressive = TRUE, max_retries_per_entry = 1,
                       checkpoint_interval = 50),
      human_verification = list(enabled = FALSE),
      # Removed dead legacy knobs (merge_strategy,
      # max_merge_passes, min_merges_to_continue).
      themes = list(include_subthemes = FALSE),
      correlations = list(method = "spearman",
                              adjust_method = "bonferroni",
                              min_observations = 3, min_theme_entries = 1,
                              dynamic_method = FALSE),
      review_points = list(after_coding = FALSE, after_themes = FALSE,
                              max_iterations = 1L)
    ),
    output = list(
      results_dir = output_dir,
      generate_report = generate_report,
      generate_correlation_plot = FALSE,
      comparison_enabled = FALSE,
      export_qdpx = FALSE
    ),
    audit = list(capture_raw_responses = FALSE),
    logging = list(log_level = "WARN")
  )
  cfg
}

.e2e_write_config <- function(cfg, dir) {
  path <- file.path(dir, "config.yaml")
  yaml::write_yaml(cfg, path)
  path
}

# ---- AC4: Mode 2 e2e produces full artifact set + finalized -------------

test_that("Mode 2 e2e: run_analysis produces complete artifact set + finalize_run", {
  skip_if_not_installed("RSQLite")
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")

  tmp_dir <- withr::local_tempdir()
  db_path <- file.path(tmp_dir, "test.db")
  .e2e_create_test_db(db_path, n_posts = 5)
  output_dir <- file.path(tmp_dir, "outputs")
  dir.create(output_dir, recursive = TRUE)

  cfg <- .e2e_config(db_path, output_dir,
                       mode = "codebook_collaborative",
                       generate_report = FALSE)  # skip Rmd render -- we test
                                                  # the Rmd-render path separately
  config_path <- .e2e_write_config(cfg, tmp_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete        = .smart_mock_ai_complete(),
    .package = "pakhom"
  )

  result <- suppressWarnings(run_analysis(config_path))
  expect_false(is.null(result))
  expect_true(dir.exists(result$output_dir))

  # AC4: methodology mode stamped in run_metadata.json
  meta <- jsonlite::read_json(file.path(result$output_dir,
                                           "run_metadata.json"),
                                simplifyVector = TRUE)
  expect_equal(meta$methodology_mode, "codebook_collaborative")
  expect_equal(meta$is_finalized, TRUE)
  expect_false(is.null(meta$finalized_at))

  # AC4 / AC8 / AC7: every required Mode 2 artifact present
  d <- result$output_dir
  expect_true(file.exists(file.path(d, "sentiment_scores.csv")))
  expect_true(file.exists(file.path(d, "codes.csv")))
  expect_true(file.exists(file.path(d, "correlations.csv")))
  expect_true(file.exists(file.path(d, "themes.json")))
  expect_true(dir.exists(file.path(d, "theme_entries")))
  expect_true(file.exists(file.path(d, "run_metadata.json")))
  expect_true(file.exists(file.path(d, "rules", "methodology_rules.md")))
  expect_true(file.exists(file.path(d, "fabrication_log.csv")))
  expect_true(file.exists(file.path(d, "ai_decisions.jsonl")))
})

# ---- AC4: Mode 3 e2e produces framework_applied.yaml + Framework Declaration ---

test_that("Mode 3 e2e: run_analysis archives framework_spec + stamps run_metadata + renders Framework Declaration", {
  skip_if_not_installed("RSQLite")
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  skip_if_not(rmarkdown::pandoc_available() ||
                dir.exists("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"),
              "Requires pandoc for HTML report render")

  # Resolve TPB framework path (built-in)
  tpb_path <- system.file("extdata", "frameworks", "tpb.yaml",
                            package = "pakhom")
  if (!nzchar(tpb_path) || !file.exists(tpb_path)) {
    skip("TPB framework not installed; skipping Mode 3 e2e")
  }

  tmp_dir <- withr::local_tempdir()
  db_path <- file.path(tmp_dir, "test.db")
  .e2e_create_test_db(db_path, n_posts = 5)
  output_dir <- file.path(tmp_dir, "outputs")
  dir.create(output_dir, recursive = TRUE)

  cfg <- .e2e_config(db_path, output_dir,
                       mode = "framework_applied",
                       framework_spec_path = tpb_path,
                       generate_report = TRUE,
                       provider = "anthropic")
  config_path <- .e2e_write_config(cfg, tmp_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("anthropic"),
    ai_complete        = .smart_mock_ai_complete(),
    .package = "pakhom"
  )

  result <- suppressWarnings(run_analysis(config_path))
  expect_false(is.null(result))
  d <- result$output_dir

  # Audit H1 + H2: framework_applied.yaml is archived + hashed
  framework_archive_path <- file.path(d, "framework_applied.yaml")
  expect_true(file.exists(framework_archive_path))

  meta <- jsonlite::read_json(file.path(d, "run_metadata.json"),
                                simplifyVector = TRUE)
  expect_equal(meta$methodology_mode, "framework_applied")
  expect_false(is.null(meta$framework_name))
  expect_equal(meta$framework_name, "Theory of Planned Behavior")
  expect_match(meta$framework_hash, "^[0-9a-f]{64}$")
  expect_equal(meta$framework_n_constructs, 5L)

  # Framework Declaration appears in the rendered Rmd / HTML
  rmd_path <- file.path(d, "analysis_report.Rmd")
  expect_true(file.exists(rmd_path))
  rmd <- paste(readLines(rmd_path, warn = FALSE), collapse = "\n")
  expect_match(rmd, "Theoretical Framework \\(Mode 3 / AC4\\)")
  expect_match(rmd, "Theory of Planned Behavior", fixed = TRUE)
  expect_match(rmd, substr(meta$framework_hash, 1, 12), fixed = TRUE)

  # Mode 3 + Anthropic: bypass footnote fires
  expect_match(rmd, "structurally precluded")
})

# ---- AC4 propagation: methodology stamps on every CSV produced ----------

test_that("AC4: Mode 2 run_analysis stamps the methodology mode on every CSV output", {
  skip_if_not_installed("RSQLite")
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")

  tmp_dir <- withr::local_tempdir()
  db_path <- file.path(tmp_dir, "test.db")
  .e2e_create_test_db(db_path, n_posts = 5)
  output_dir <- file.path(tmp_dir, "outputs")
  dir.create(output_dir, recursive = TRUE)

  cfg <- .e2e_config(db_path, output_dir,
                       mode = "codebook_collaborative",
                       generate_report = FALSE)
  config_path <- .e2e_write_config(cfg, tmp_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete        = .smart_mock_ai_complete(),
    .package = "pakhom"
  )

  result <- suppressWarnings(run_analysis(config_path))
  d <- result$output_dir

  # AC4: every CSV produced by export_results carries the methodology
  # stamp as a comment-style header line at the top. Audit M1: tighten
  # the assertion -- the stamp shape is `# methodology: M2 - Codebook
  # Collaborative | run: ...`. A loose regex like
  # "methodology|M2|codebook_collaborative" would silently pass on a
  # CSV whose first row happened to contain any of those tokens for
  # unrelated reasons. Anchor on the exact prefix.
  csvs <- list.files(d, pattern = "\\.csv$", recursive = TRUE,
                       full.names = TRUE)
  expect_gt(length(csvs), 0L)
  for (csv in csvs) {
    first_line <- readLines(csv, n = 1L, warn = FALSE)
    if (length(first_line) == 0L) next
    # fabrication_log.csv is now also stamped (audit A finding):
    # init_fabrication_log writes the header then prepends the methodology
    # stamp; subsequent log_fabrication appends rows below. The header
    # comment lines aren't disturbed by the appends.
    expect_match(first_line, "^# methodology:",
                  info = sprintf("CSV without methodology stamp prefix: %s",
                                 basename(csv)))
    expect_match(first_line, "M2",
                  info = sprintf("CSV without M2 short-code: %s",
                                 basename(csv)))
  }
})

# ---- AC5: finalized run cannot be silently re-finalized ------------------

test_that("AC5: run_analysis refuses to resume into a finalized run with mode mismatch", {
  skip_if_not_installed("RSQLite")
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")

  tmp_dir <- withr::local_tempdir()
  db_path <- file.path(tmp_dir, "test.db")
  .e2e_create_test_db(db_path, n_posts = 5)
  output_dir <- file.path(tmp_dir, "outputs")
  dir.create(output_dir, recursive = TRUE)

  cfg_m2 <- .e2e_config(db_path, output_dir,
                          mode = "codebook_collaborative",
                          generate_report = FALSE)
  config_path_m2 <- .e2e_write_config(cfg_m2, tmp_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete        = .smart_mock_ai_complete(),
    .package = "pakhom"
  )

  result <- suppressWarnings(run_analysis(config_path_m2))
  expect_true(is_run_finalized(result$output_dir))

  # Switch the config to Mode 3 and try to resume into the finalized
  # run dir. Per AC5, this must refuse rather than silently overwrite.
  tpb_path <- system.file("extdata", "frameworks", "tpb.yaml",
                            package = "pakhom")
  cfg_m3 <- cfg_m2
  cfg_m3$methodology$mode <- "framework_applied"
  cfg_m3$methodology$framework_spec_path <- tpb_path
  config_path_m3 <- file.path(tmp_dir, "config_m3.yaml")
  yaml::write_yaml(cfg_m3, config_path_m3)

  expect_error(
    suppressWarnings(run_analysis(config_path_m3, resume = TRUE)),
    regex = "FINALIZED|mismatch|fork",
    ignore.case = TRUE
  )
})

# ---- AC8: cross-mode artifact divergence ---------------------------------

test_that("AC8: Mode 2 and Mode 3 produce overlapping but distinct artifact sets", {
  skip_if_not_installed("RSQLite")
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  tpb_path <- system.file("extdata", "frameworks", "tpb.yaml",
                            package = "pakhom")
  if (!nzchar(tpb_path)) skip("TPB not installed")

  tmp_dir <- withr::local_tempdir()
  db_path <- file.path(tmp_dir, "test.db")
  .e2e_create_test_db(db_path, n_posts = 5)
  out_m2 <- file.path(tmp_dir, "out_m2"); dir.create(out_m2, recursive = TRUE)
  out_m3 <- file.path(tmp_dir, "out_m3"); dir.create(out_m3, recursive = TRUE)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete        = .smart_mock_ai_complete(),
    .package = "pakhom"
  )

  cfg2 <- .e2e_config(db_path, out_m2,
                        mode = "codebook_collaborative",
                        generate_report = FALSE)
  cfg3 <- .e2e_config(db_path, out_m3,
                        mode = "framework_applied",
                        framework_spec_path = tpb_path,
                        generate_report = FALSE)

  cfg2_path <- file.path(tmp_dir, "cfg2.yaml")
  cfg3_path <- file.path(tmp_dir, "cfg3.yaml")
  yaml::write_yaml(cfg2, cfg2_path)
  yaml::write_yaml(cfg3, cfg3_path)

  res2 <- suppressWarnings(run_analysis(cfg2_path))
  res3 <- suppressWarnings(run_analysis(cfg3_path))

  # Mode 3 should have framework_applied.yaml; Mode 2 should NOT
  expect_true(file.exists(file.path(res3$output_dir,
                                       "framework_applied.yaml")))
  expect_false(file.exists(file.path(res2$output_dir,
                                        "framework_applied.yaml")))

  # Mode 3 metadata carries framework_* fields; Mode 2 metadata does not
  meta2 <- jsonlite::read_json(file.path(res2$output_dir,
                                            "run_metadata.json"),
                                  simplifyVector = TRUE)
  meta3 <- jsonlite::read_json(file.path(res3$output_dir,
                                            "run_metadata.json"),
                                  simplifyVector = TRUE)
  expect_null(meta2$framework_name)
  expect_equal(meta3$framework_name, "Theory of Planned Behavior")

  # Both modes have the universal Tier-0/Tier-1 artifacts
  for (d in c(res2$output_dir, res3$output_dir)) {
    expect_true(file.exists(file.path(d, "run_metadata.json")))
    expect_true(file.exists(file.path(d, "rules/methodology_rules.md")))
    expect_true(file.exists(file.path(d, "fabrication_log.csv")))
    expect_true(file.exists(file.path(d, "ai_decisions.jsonl")))
    expect_true(is_run_finalized(d))
  }

  # Run-dir suffix carries mode short-code (T1.7)
  expect_match(basename(res2$output_dir), "_M2$")
  expect_match(basename(res3$output_dir), "_M3$")
})

# ---- AC8: Mode 1 produces Mode-1-specific artifacts (not Mode 2/3 ones) -

test_that("AC8: run_mode1 produces Mode 1 artifact set (different from run_analysis Mode 2/3)", {
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  skip_if_not(rmarkdown::pandoc_available() ||
                dir.exists("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"),
              "Requires pandoc")

  tmp_dir <- withr::local_tempdir()

  data <- tibble::tibble(
    std_id = paste0("e", 1:6),
    std_text = c("plan to take", "doctor told me", "always forget",
                  "side effects", "doesn't help", "feel different"),
    std_author = c("a", "b", "c", "d", "e", "f"),
    theme_membership_Adoption = rep(1L, 6)
  )
  ts <- create_theme_set(list(list(
    id = 1, name = "Adoption", description = "Adoption theme",
    codes_included = "x"
  )))
  cfg <- list(
    methodology = list(mode = "reflexive_scaffold"),
    study = list(name = "M1 cross-mode test",
                   research_focus = "integration test"),
    ai = list(provider = "openai"),
    output = list(results_dir = tmp_dir, generate_report = TRUE),
    audit = list(capture_raw_responses = FALSE),
    logging = list(log_level = "WARN")
  )

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete        = function(...) list(
      content = jsonlite::toJSON(list(provocations = list()),
                                    auto_unbox = TRUE),
      model = "m", request_id = "r",
      usage = list(prompt_tokens = 1L, completion_tokens = 1L,
                    total_tokens = 2L),
      finish_reason = "stop", raw_response = list(),
      prompt_hash = "h", citations = list()
    ),
    .package = "pakhom"
  )

  result <- suppressWarnings(
    run_mode1(data = data, theme_set = ts, config = cfg,
                categories = "counter_narrative")
  )
  d <- result$output_dir

  # Mode 1-specific artifacts (NOT in Mode 2/3)
  expect_true(file.exists(file.path(d, "reflection_log.json")))
  expect_true(file.exists(file.path(d, "provocations.csv")))
  expect_true(file.exists(file.path(d, "provocation_attempts.csv")))
  expect_true(file.exists(file.path(d, "coverage_mode1.json")))

  # Mode 2/3 artifacts NOT present in Mode 1
  expect_false(file.exists(file.path(d, "sentiment_scores.csv")))
  expect_false(file.exists(file.path(d, "codes.csv")))
  expect_false(file.exists(file.path(d, "correlations.csv")))
  expect_false(file.exists(file.path(d, "framework_applied.yaml")))
  expect_false(dir.exists(file.path(d, "theme_entries")))

  # Universal Tier-0/Tier-1 artifacts present in all modes
  expect_true(file.exists(file.path(d, "run_metadata.json")))
  expect_true(file.exists(file.path(d, "rules/methodology_rules.md")))
  expect_true(file.exists(file.path(d, "fabrication_log.csv")))
  expect_true(file.exists(file.path(d, "ai_decisions.jsonl")))

  # Run-dir suffix
  expect_match(basename(d), "_M1$")

  # Finalized
  expect_true(is_run_finalized(d))
})

# ---- Mode 1 from public API only ------------------------------------------

# A Mode 1 user must be able to take a YAML config, get a standardized +
# preprocessed corpus, attach theme membership from their external coding
# tool, and run the provocateur loop -- WITHOUT calling pakhom:::
# internals (load_and_combine_tables, preprocess_text, detect_columns,
# standardize_data). An earlier smoke test caught
# that this required pakhom::: access before load_corpus_from_config()
# was exported. This test pins that public-API contract so any future
# helper de-export silently breaks the test.

test_that("Mode 1 public-API contract: config_path -> load_corpus_from_config -> run_mode1, no ::: required", {
  skip_if_not_installed("RSQLite")
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")

  tmp_dir <- withr::local_tempdir()
  db_path <- file.path(tmp_dir, "test.db")
  .e2e_create_test_db(db_path, n_posts = 5)
  output_dir <- file.path(tmp_dir, "outputs")
  dir.create(output_dir, recursive = TRUE)

  cfg <- .e2e_config(db_path, output_dir,
                       mode = "reflexive_scaffold",
                       generate_report = FALSE)
  # Disable test_mode sampling so we can attach theme_membership_*
  # against the full 5-row corpus without depending on which 3 rows
  # the sampler happened to pick.
  cfg$analysis$test_mode$enabled <- FALSE
  config_path <- .e2e_write_config(cfg, tmp_dir)

  # ---- ACCEPTANCE 1: load_corpus_from_config is the only loader used. ----
  # This is the entire user-side data-prep recipe; no pakhom::: anywhere.
  corpus <- suppressWarnings(load_corpus_from_config(config_path))

  # Standardized schema produced by the public helper:
  expect_true(is.data.frame(corpus))
  expect_true(all(c("std_id", "std_text") %in% names(corpus)))
  expect_true("std_author" %in% names(corpus))
  expect_true("source_table" %in% names(corpus))
  expect_gt(nrow(corpus), 0L)

  # ---- ACCEPTANCE 2: attach theme_membership_* using only the columns ----
  # exposed by load_corpus_from_config. In a real workflow std_id values
  # would come from the researcher's NVivo / ATLAS.ti / MAXQDA export;
  # here we synthesize a half-and-half split for testing.
  n_half <- ceiling(nrow(corpus) / 2)
  corpus$theme_membership_Adoption  <- as.integer(
    seq_len(nrow(corpus)) <= n_half
  )
  corpus$theme_membership_Resistance <- as.integer(
    seq_len(nrow(corpus)) >  n_half
  )

  # ---- ACCEPTANCE 3: build the ThemeSet using only public API. ----
  ts <- create_theme_set(list(
    list(id = 1L, name = "Adoption",
         description = "Researcher-authored: schedule adoption",
         codes_included = c("async_routine", "daily_batching")),
    list(id = 2L, name = "Resistance",
         description = "Researcher-authored: resistance",
         codes_included = c("skips_async", "tool_friction"))
  ))

  # ---- ACCEPTANCE 4: run_mode1 accepts the public-API-built fixture. ----
  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete        = function(...) list(
      content       = jsonlite::toJSON(list(provocations = list()),
                                          auto_unbox = TRUE),
      model         = "mock-model",
      request_id    = "req-mock",
      usage         = list(prompt_tokens = 1L, completion_tokens = 1L,
                            total_tokens = 2L),
      finish_reason = "stop",
      raw_response  = list(),
      prompt_hash   = "hash-mock",
      citations     = list()
    ),
    .package = "pakhom"
  )

  result <- suppressWarnings(
    run_mode1(data = corpus, theme_set = ts, config_path = config_path,
                categories = "counter_narrative")
  )

  # ---- ACCEPTANCE 5: the run produced a Mode-1 artifact set. ----
  d <- result$output_dir
  expect_true(dir.exists(d))
  expect_match(basename(d), "_M1$")
  expect_true(file.exists(file.path(d, "reflection_log.json")))
  expect_true(file.exists(file.path(d, "coverage_mode1.json")))
  expect_true(file.exists(file.path(d, "run_metadata.json")))
  expect_true(file.exists(file.path(d, "rules", "methodology_rules.md")))
  expect_true(file.exists(file.path(d, "fabrication_log.csv")))
  expect_true(is_run_finalized(d))
})

test_that("load_corpus_from_config: accepts both ThematicConfig and file path; preserves test_mode toggle", {
  skip_if_not_installed("RSQLite")

  tmp_dir <- withr::local_tempdir()
  db_path <- file.path(tmp_dir, "test.db")
  .e2e_create_test_db(db_path, n_posts = 5)
  output_dir <- file.path(tmp_dir, "outputs")
  dir.create(output_dir, recursive = TRUE)

  cfg_list <- .e2e_config(db_path, output_dir,
                             mode = "codebook_collaborative",
                             generate_report = FALSE)
  cfg_list$analysis$test_mode <- list(enabled = TRUE,
                                         sample_size = 2L,
                                         seed = 42L)
  config_path <- .e2e_write_config(cfg_list, tmp_dir)

  # Path form: parses YAML -> ThematicConfig internally.
  corpus_via_path <- suppressWarnings(load_corpus_from_config(config_path))
  # ThematicConfig form: skips the YAML parse step.
  cfg_obj <- suppressWarnings(load_config(config_path))
  corpus_via_obj  <- suppressWarnings(load_corpus_from_config(cfg_obj))
  expect_equal(corpus_via_path, corpus_via_obj)

  # test_mode default (TRUE) honors config: 5 rows -> sample_size 2.
  expect_equal(nrow(corpus_via_path), 2L)

  # apply_test_mode = FALSE returns the full preprocessed corpus.
  corpus_full <- suppressWarnings(
    load_corpus_from_config(cfg_obj, apply_test_mode = FALSE)
  )
  expect_gte(nrow(corpus_full), 3L)
  expect_true(all(c("std_id", "std_text", "std_author") %in% names(corpus_full)))
})

test_that("load_corpus_from_config: rejects invalid inputs with actionable errors", {
  # No database in config:
  cfg_no_db <- list(
    data = list(tables = "posts", source_type = "reddit")
  )
  expect_error(load_corpus_from_config(cfg_no_db),
               "config\\$data\\$database is required")

  # No tables in config:
  cfg_no_tables <- list(
    data = list(database = "x.db", source_type = "reddit")
  )
  expect_error(load_corpus_from_config(cfg_no_tables),
               "config\\$data\\$tables must list at least one table")

  # Not a ThematicConfig or path:
  expect_error(load_corpus_from_config(42),
               "must be a ThematicConfig")
})

# ---- AC5 same-mode finalized refusal --------------------------------------

test_that("AC5 (same-mode): run_analysis refuses to resume a same-mode finalized run", {
  # Companion to the earlier AC5 mismatch test: this one uses the SAME
  # mode (Mode 2 -> Mode 2) so it exercises the in-mode finalized-
  # resume refusal specifically. The `resume = TRUE` request would
  # otherwise silently overwrite analysis_report.html / CSVs / the
  # audit log on a finalized run.
  skip_if_not_installed("RSQLite")
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")

  tmp_dir <- withr::local_tempdir()
  db_path <- file.path(tmp_dir, "test.db")
  .e2e_create_test_db(db_path, n_posts = 5)
  output_dir <- file.path(tmp_dir, "outputs")
  dir.create(output_dir, recursive = TRUE)

  cfg <- .e2e_config(db_path, output_dir,
                       mode = "codebook_collaborative",
                       generate_report = FALSE)
  config_path <- .e2e_write_config(cfg, tmp_dir)

  call_log <- new.env(parent = emptyenv())
  call_log$tasks <- character(0)

  smart_mock <- .smart_mock_ai_complete()
  tracking_mock <- function(provider, prompt, system_prompt = NULL,
                              task = "coding", ...) {
    call_log$tasks <- c(call_log$tasks, task)
    smart_mock(provider, prompt, system_prompt, task = task, ...)
  }

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete        = tracking_mock,
    .package = "pakhom"
  )

  result1 <- suppressWarnings(run_analysis(config_path))
  expect_true(is_run_finalized(result1$output_dir))
  # Audit L1: assert the EXACT coding-call count expected from
  # test_mode$sample_size = 3. progressive coding makes one ai_complete
  # per entry. A regression that skips entries (or double-codes them)
  # would surface here.
  first_run_coding_calls <- sum(call_log$tasks == "coding")
  expect_equal(first_run_coding_calls, 3L)

  expect_error(
    suppressWarnings(run_analysis(config_path, resume = TRUE)),
    regex = "FINALIZED|finalized",
    ignore.case = TRUE
  )
})

# ---- Mode 2 e2e with generate_report = TRUE (full Rmd render path) -------

test_that("Mode 2 e2e WITH generate_report=TRUE produces analysis_report.html", {
  # Audit-pattern coverage: the earlier Mode 2 test had
  # generate_report=FALSE to avoid the Rmd render time + complexity.
  # This test exercises the full report pipeline (the bug-prone path
  # that surfaced the empty-themes theme_name lookup).
  skip_if_not_installed("RSQLite")
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  skip_if_not(rmarkdown::pandoc_available() ||
                dir.exists("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"),
              "Requires pandoc")

  tmp_dir <- withr::local_tempdir()
  db_path <- file.path(tmp_dir, "test.db")
  .e2e_create_test_db(db_path, n_posts = 5)
  output_dir <- file.path(tmp_dir, "outputs")
  dir.create(output_dir, recursive = TRUE)

  cfg <- .e2e_config(db_path, output_dir,
                       mode = "codebook_collaborative",
                       generate_report = TRUE)
  config_path <- .e2e_write_config(cfg, tmp_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete        = .smart_mock_ai_complete(),
    .package = "pakhom"
  )

  result <- suppressWarnings(run_analysis(config_path))
  d <- result$output_dir

  expect_true(file.exists(file.path(d, "analysis_report.html")))
  expect_true(file.exists(file.path(d, "analysis_report.Rmd")))
  expect_true(file.exists(file.path(d, "styles.css")))

  # Mode 2 reports do NOT carry the Mode 3 Framework Declaration
  rmd <- paste(readLines(file.path(d, "analysis_report.Rmd"),
                            warn = FALSE), collapse = "\n")
  expect_no_match(rmd, "Theoretical Framework \\(Mode 3 / AC4\\)")
  # Mode 2 + OpenAI: bypass footnote does NOT fire (only Mode 3 + Anthropic)
  expect_no_match(rmd, "structurally precluded")
  # Mode 2 methodology stamp
  expect_match(rmd, "M2 - Codebook Collaborative")
})

# ---- Audit H2: Mode 3 HAPPY-PATH (construct-matching codes) -------------

test_that("Mode 3 e2e (happy path): construct-matching codes -> apply_framework_themes populates merge_history -> theme_membership_* set", {
  # Audit H2: the previous Mode 3 e2e returned "NEW: mock_code"
  # for every coding call, which never matches a TPB construct id.
  # apply_framework_themes (R/13_themes.R:687) skips constructs whose
  # codebook entry is NULL, so the theme_set was empty and
  # cascade_theme_assignments left emerged_themes all-NA. The
  # bug-prone path (apply_framework_themes -> NON-empty theme_set ->
  # rebuild_code_to_theme_map -> cascade_theme_assignments populates
  # theme_membership_*) was never exercised. THIS test fires that path
  # by feeding the mock a construct-matching code so apply_framework_themes
  # produces a non-empty theme_set and cascade does its work. The
  # earlier "apply_framework_themes not populating merge_history"
  # silent failure lived precisely in this path -- test pins it.
  skip_if_not_installed("RSQLite")
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")
  skip_if_not(rmarkdown::pandoc_available() ||
                dir.exists("/Applications/RStudio.app/Contents/Resources/app/quarto/bin/tools/aarch64"),
              "Requires pandoc")

  tpb_path <- system.file("extdata", "frameworks", "tpb.yaml",
                            package = "pakhom")
  if (!nzchar(tpb_path)) skip("TPB not installed")

  tmp_dir <- withr::local_tempdir()
  db_path <- file.path(tmp_dir, "test.db")
  .e2e_create_test_db(db_path, n_posts = 5)
  output_dir <- file.path(tmp_dir, "outputs")
  dir.create(output_dir, recursive = TRUE)

  cfg <- .e2e_config(db_path, output_dir,
                       mode = "framework_applied",
                       framework_spec_path = tpb_path,
                       generate_report = TRUE,
                       provider = "anthropic")
  config_path <- .e2e_write_config(cfg, tmp_dir)

  # Mock returns "attitude" as the code for every coding call. In Mode 3,
  # the construct id "attitude" matches one of TPB's constructs, so the
  # codebook will have an "attitude" entry with non-zero frequency.
  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("anthropic"),
    ai_complete        = .smart_mock_ai_complete(
                            coding_code_name = "attitude"),
    .package = "pakhom"
  )

  result <- suppressWarnings(run_analysis(config_path))
  d <- result$output_dir

  # AC8 happy-path: theme_set is NON-empty
  expect_gt(length(result$theme_set$themes), 0L)
  # The "Attitude toward the behavior" construct must be the (only) theme
  # surfaced from apply_framework_themes.
  theme_names <- vapply(result$theme_set$themes,
                          function(t) t$name, character(1))
  expect_true("Attitude toward the behavior" %in% theme_names)

  # cascade_theme_assignments must have populated theme_membership_*
  membership_cols <- grep("^theme_membership_",
                            names(result$analytic_data), value = TRUE)
  expect_gt(length(membership_cols), 0L)
  # At least some entries got assigned (since coding produced
  # non-skipped results)
  total_assigned <- sum(vapply(membership_cols, function(col) {
    sum(result$analytic_data[[col]] == 1L, na.rm = TRUE)
  }, integer(1)))
  expect_gt(total_assigned, 0L)

  # merge_history must carry the code-to-theme map (the earlier
  # silent-failure regression pin)
  expect_false(is.null(result$theme_set$merge_history))
  expect_gt(length(result$theme_set$merge_history$code_to_theme_map),
              0L)
  expect_equal(
    result$theme_set$merge_history$code_to_theme_map[["attitude"]],
    "Attitude toward the behavior"
  )

  # Framework Declaration in the Rmd (sanity, already covered by the
  # other Mode 3 test but pin it here too on the happy path)
  rmd <- paste(readLines(file.path(d, "analysis_report.Rmd"),
                            warn = FALSE), collapse = "\n")
  expect_match(rmd, "Theoretical Framework \\(Mode 3 / AC4\\)")
  expect_match(rmd, "Theory of Planned Behavior", fixed = TRUE)
})

# ---- Audit AC2 + AC3: explicit-mode + no-fourth-mode ---------------------

test_that("AC2: run_analysis rejects an unknown methodology mode", {
  # AC2 ("Three modes; no fourth"): config validation must refuse any
  # methodology.mode that isn't one of the three canonical values.
  skip_if_not_installed("RSQLite")
  tmp_dir <- withr::local_tempdir()
  db_path <- file.path(tmp_dir, "test.db")
  .e2e_create_test_db(db_path, n_posts = 3)
  output_dir <- file.path(tmp_dir, "outputs"); dir.create(output_dir)
  cfg <- .e2e_config(db_path, output_dir,
                       mode = "novel_unsupported_mode",
                       generate_report = FALSE)
  cfg_path <- .e2e_write_config(cfg, tmp_dir)
  expect_error(
    suppressWarnings(run_analysis(cfg_path)),
    regex = "mode|invalid|unknown|reflexive_scaffold|codebook|framework",
    ignore.case = TRUE
  )
})

test_that("AC3: run_analysis rejects a config without an explicit methodology.mode", {
  # AC3 ("No default mode; explicit declaration mandatory"): a config
  # missing methodology$mode must fail validation. T1.3 enforcement is
  # pinned at the run_analysis level so a regression
  # that re-introduces a default is caught.
  skip_if_not_installed("RSQLite")
  tmp_dir <- withr::local_tempdir()
  db_path <- file.path(tmp_dir, "test.db")
  .e2e_create_test_db(db_path, n_posts = 3)
  output_dir <- file.path(tmp_dir, "outputs"); dir.create(output_dir)
  cfg <- .e2e_config(db_path, output_dir,
                       mode = "codebook_collaborative",
                       generate_report = FALSE)
  # Strip the methodology block entirely
  cfg$methodology <- NULL
  cfg_path <- .e2e_write_config(cfg, tmp_dir)
  expect_error(
    suppressWarnings(run_analysis(cfg_path)),
    regex = "methodology|mode",
    ignore.case = TRUE
  )
})

# ---- Production-bug regression test --------------------------------------

test_that("aggregate_overall_statistics: empty-emerged-themes path keeps theme_name column (audit-found bug)", {
  # Pin the production bug found by these e2e tests:
  # names(table(character(0))) returns NULL, and tibble(theme_name = NULL,
  # ...) drops the column entirely. The downstream pull(theme_name)
  # in generate_report errored with "object 'theme_name' not found"
  # for any Mode 3 run where the AI's coded constructs didn't match
  # any framework construct (apply_framework_themes -> empty theme_set
  # -> cascade_theme_assignments leaves all-NA emerged_themes).
  data <- tibble::tibble(
    std_id = paste0("e", 1:3),
    std_text = letters[1:3],
    sentiment_score = c(0.1, -0.1, 0.0),
    confidence = rep(0.9, 3),
    all_emotions = rep("neutral", 3),
    emotion_intensity = rep(0.5, 3),
    emerged_themes = NA_character_,  # <- the bug-triggering shape
    n_themes = 0L
  )
  ts <- create_theme_set(list())  # empty theme_set (Mode 3 no-match scenario)
  stats <- aggregate_overall_statistics(data, ts)
  # The fix: theme_name column must exist even when no themes
  expect_true("theme_name" %in% names(stats$themes))
  expect_equal(nrow(stats$themes), 0L)
  # And dplyr::pull works without erroring
  out <- dplyr::pull(stats$themes, theme_name)
  expect_equal(out, character(0))
})

# ---- AC4 / AC9: run_metadata.json carries the methodology rules hash ----

test_that("AC9: run_metadata.json captures the config_hash + methodology rules are written", {
  skip_if_not_installed("RSQLite")
  skip_if_not(exists("local_mocked_bindings", envir = asNamespace("testthat")),
              "Requires testthat >= 3.1.5")

  tmp_dir <- withr::local_tempdir()
  db_path <- file.path(tmp_dir, "test.db")
  .e2e_create_test_db(db_path, n_posts = 5)
  output_dir <- file.path(tmp_dir, "outputs")
  dir.create(output_dir, recursive = TRUE)

  cfg <- .e2e_config(db_path, output_dir,
                       mode = "codebook_collaborative",
                       generate_report = FALSE)
  config_path <- .e2e_write_config(cfg, tmp_dir)

  local_mocked_bindings(
    create_ai_provider = function(...) mock_provider("openai"),
    ai_complete        = .smart_mock_ai_complete(),
    .package = "pakhom"
  )

  result <- suppressWarnings(run_analysis(config_path))
  d <- result$output_dir

  meta <- jsonlite::read_json(file.path(d, "run_metadata.json"),
                                simplifyVector = TRUE)
  # AC9: rules archived under outputs/<run>/rules/methodology_rules.md
  rules_path <- file.path(d, "rules", "methodology_rules.md")
  expect_true(file.exists(rules_path))
  rules_text <- paste(readLines(rules_path, warn = FALSE),
                        collapse = "\n")
  # Mode 2 rules mention codebook collaborative
  expect_match(rules_text, "codebook_collaborative|Codebook Collaborative",
                ignore.case = TRUE)

  # config_hash captured in metadata for replay-equivalence
  expect_match(meta$config_hash, "^[0-9a-f]+$")

  # provider stamped (AC8 -- modes are configurations of one architecture
  # but each run records WHICH provider was used)
  expect_equal(meta$provider, "openai")
  expect_match(meta$model_primary, "gpt-")
})
