# Tests for config loading and validation (01_config.R)

test_that("default_config returns valid ThematicConfig", {
  cfg <- default_config("codebook_collaborative")
  expect_s3_class(cfg, "ThematicConfig")
  expect_true(!is.null(cfg$ai))
  expect_true(!is.null(cfg$data))
  expect_true(!is.null(cfg$analysis))
  expect_true(!is.null(cfg$output))
  expect_true(!is.null(cfg$study))
})

test_that("default_config has expected AI settings", {
  cfg <- default_config("codebook_collaborative")
  expect_true(cfg$ai$provider %in% c("openai", "anthropic"))
  # Use [[ for strict access; $ does prefix matching ("model" would silently
  # match "models") and would let stale field names pass undetected.
  expect_true(!is.null(cfg$ai$openai[["models"]]))
  expect_true(!is.null(cfg$ai$openai[["models"]][["primary"]]))
  expect_true(!is.null(cfg$ai$anthropic[["models"]]))
  expect_true(!is.null(cfg$ai$anthropic[["models"]][["primary"]]))
})

test_that("default_config has expected analysis settings", {
  cfg <- default_config("codebook_collaborative")
  # Phase 53 cleanup of Phase 52 audit deferral: min_themes / max_themes /
  # max_theme_proportion / multi_label_assignment / ai_batch_size are
  # removed (they were dead per C1 -- never gated algorithm behavior).
  # Phase 50e removed min/max_subthemes_per_theme as dead knobs.
  expect_null(cfg$analysis$themes$min_themes)
  expect_null(cfg$analysis$themes$max_themes)
  expect_null(cfg$analysis$themes$max_theme_proportion)
  expect_null(cfg$analysis$themes$multi_label_assignment)
  expect_null(cfg$analysis$themes$ai_batch_size)
  # Surviving theme knobs are display-only; verify they keep their defaults.
  expect_true(cfg$analysis$themes$include_subthemes)
  expect_true(cfg$analysis$themes$include_quotes)
  expect_equal(cfg$analysis$themes$quotes_per_theme, 3)
  expect_equal(cfg$analysis$themes$approach, "inductive")
  expect_true(cfg$analysis$correlations$use_multi_label)
})

test_that("default_config has new config fields", {
  cfg <- default_config("codebook_collaborative")
  expect_null(cfg$study$researcher_positionality)
  expect_null(cfg$data$explicit_columns)
  expect_type(cfg$data$preprocessing$custom_cleaning_rules, "list")
  # Phase 50e: removed `fallback_keyword_patterns` as dead knob.
  # New reflexivity fields
  expect_null(cfg$study$research_paradigm)
  expect_null(cfg$study$reflexive_notes)
  # Multi-model config
  expect_false(cfg$ai$multi_model$enabled)
  # Review format
  expect_equal(cfg$analysis$review_points$format, "csv")
})

test_that("default_config: multi-label entry assignment is structural (correlations dispatch)", {
  # Phase 53: multi_label_assignment under analysis$themes was a dead
  # knob and is removed. Multi-label behavior is now structural in
  # cascade_theme_assignments (entries flow into every theme that
  # contains any of their codes) and in correlations dispatch via
  # use_multi_label.
  cfg <- default_config("codebook_collaborative")
  expect_null(cfg$analysis$themes$multi_label_assignment)
  expect_true(cfg$analysis$correlations$use_multi_label)
})

test_that("validate_config errors on missing research_focus", {
  cfg <- default_config("codebook_collaborative")
  # default_config has empty research_focus, which validate_config rejects
  expect_error(validate_config(cfg), "research_focus")
})

test_that("validate_config errors on missing API key", {
  cfg <- default_config("codebook_collaborative")
  cfg$study$research_focus <- "Test focus for validation"
  # Satisfy other validation requirements so only the API key error fires
  cfg$output$results_dir <- tempdir()
  cfg$learning$enabled <- FALSE
  # With no API key set in config and no env var, should error about API key
  old_key <- Sys.getenv("OPENAI_API_KEY")
  on.exit(Sys.setenv(OPENAI_API_KEY = old_key))
  Sys.setenv(OPENAI_API_KEY = "")
  expect_error(validate_config(cfg), "API key|api.key|api_key", ignore.case = TRUE)
})

test_that("validate_config catches invalid provider", {
  cfg <- default_config("codebook_collaborative")
  cfg$study$research_focus <- "Test focus"
  cfg$ai$provider <- "invalid_provider"
  expect_error(validate_config(cfg), "provider")
})

# ==============================================================================
# Methodology declaration tests (T1.3 - multi-mode architecture)
# ==============================================================================

test_that("default_config(mode) sets methodology block correctly", {
  cfg <- default_config("codebook_collaborative")
  expect_true(!is.null(cfg$methodology))
  expect_equal(cfg$methodology$mode, "codebook_collaborative")
  expect_true(!is.null(cfg$methodology$mode_locked_at))
  expect_null(cfg$methodology$framework_spec_path)
  expect_null(cfg$methodology$parent_run_id)
})

test_that("default_config() with no methodology arg ERRORS per AC3", {
  # Phase 37 audit (AC HIGH): per AC3 ("no default mode; explicit
  # declaration mandatory"), the NULL-methodology path now hard-stops
  # rather than warn-and-default. The error message must point users
  # at the three valid modes + the decision aid. The previous
  # warn-and-default behavior was the lone AC3 violation in the
  # programmatic API (validate_config + YAML-load already enforced
  # AC3 cleanly).
  expect_error(
    default_config(),
    regexp = "AC3|mandatory|reflexive_scaffold|codebook_collaborative",
    ignore.case = TRUE
  )
})

test_that("default_config(mode) is silent when methodology is passed explicitly", {
  expect_silent(default_config("reflexive_scaffold"))
  expect_silent(default_config("codebook_collaborative"))
  expect_silent(default_config("framework_applied"))
})

test_that("default_config errors on invalid methodology mode", {
  expect_error(default_config("not_a_real_mode"), "Invalid methodology mode")
})

test_that("default_config has audit and memos config blocks", {
  cfg <- default_config("codebook_collaborative")
  expect_true(!is.null(cfg$audit))
  expect_true(cfg$audit$capture_raw_responses)
  expect_equal(cfg$audit$response_cache_dir, "api_responses")

  expect_true(!is.null(cfg$memos))
  expect_equal(cfg$memos$mandatory_for_modes, "reflexive_scaffold")
  expect_setequal(cfg$memos$prompt_at, c("after_coding", "after_themes"))
})

test_that("validate_config errors when methodology section is missing", {
  cfg <- default_config("codebook_collaborative")
  cfg$study$research_focus <- "Test focus"
  cfg$methodology <- NULL
  expect_error(validate_config(cfg), "methodology")
})

test_that("validate_config errors when methodology.mode is missing", {
  cfg <- default_config("codebook_collaborative")
  cfg$study$research_focus <- "Test focus"
  cfg$methodology$mode <- NULL
  expect_error(validate_config(cfg), "methodology.mode is required")
})

test_that("validate_config errors when methodology.mode is invalid", {
  cfg <- default_config("codebook_collaborative")
  cfg$study$research_focus <- "Test focus"
  cfg$methodology$mode <- "not_a_real_mode"
  expect_error(validate_config(cfg), "Invalid methodology mode")
})

test_that("validate_config errors when methodology.mode is framework_applied without framework_spec_path", {
  cfg <- default_config("codebook_collaborative")
  cfg$study$research_focus <- "Test focus"
  cfg$methodology$mode <- "framework_applied"
  cfg$methodology$framework_spec_path <- NULL
  expect_error(validate_config(cfg), "framework_spec_path is required")
})

test_that("validate_config accepts framework_applied with framework_spec_path", {
  cfg <- default_config("framework_applied")
  cfg$study$research_focus <- "Test focus"
  cfg$methodology$framework_spec_path <- "/some/path/framework.yaml"
  cfg$output$results_dir <- tempdir()
  cfg$learning$enabled <- FALSE
  cfg$data$database <- NULL
  # Should pass the methodology check. validate_config either errors (returns
  # the error message via tryCatch) or succeeds (returns the config list).
  # Either way, "methodology" should not appear in any error text. The
  # error-vs-success branch matters because grepl on a list returns a vector
  # (one logical per top-level config field); on a single string it returns
  # one logical -- the previous bare `expect_false(grepl(...))` worked only
  # in the error branch and broke under R CMD check when env vars made
  # validate_config succeed.
  result <- tryCatch(validate_config(cfg), error = function(e) conditionMessage(e))
  err_text <- if (is.character(result)) result else ""
  expect_false(grepl("methodology", err_text))
})

test_that("validate_config accepts all three valid modes", {
  for (mode in c("reflexive_scaffold", "codebook_collaborative", "framework_applied")) {
    cfg <- default_config(mode)
    cfg$study$research_focus <- "Test focus"
    if (mode == "framework_applied") {
      cfg$methodology$framework_spec_path <- "/dummy/path.yaml"
    }
    cfg$output$results_dir <- tempdir()
    cfg$data$database <- NULL
    cfg$learning$enabled <- FALSE
    # Set fake env var so API key check passes
    old_key <- Sys.getenv("OPENAI_API_KEY")
    Sys.setenv(OPENAI_API_KEY = "sk-test")
    on.exit(Sys.setenv(OPENAI_API_KEY = old_key), add = TRUE)
    expect_silent(validate_config(cfg))
  }
})

test_that("print.ThematicConfig displays methodology mode", {
  cfg <- default_config("codebook_collaborative")
  out <- capture.output(print(cfg))
  expect_true(any(grepl("Methodology:.*codebook_collaborative", out)))
})

test_that("print.ThematicConfig flags missing methodology declaration", {
  cfg <- default_config("codebook_collaborative")
  cfg$methodology$mode <- NULL
  out <- capture.output(print(cfg))
  expect_true(any(grepl("NOT DECLARED", out)))
})

# ==============================================================================
# validate_methodology_mode helper tests (T1.3)
# ==============================================================================

test_that("validate_methodology_mode accepts all three valid modes", {
  for (mode in c("reflexive_scaffold", "codebook_collaborative", "framework_applied")) {
    expect_silent(validate_methodology_mode(mode))
  }
})

test_that("validate_methodology_mode errors on NULL when allow_null = FALSE", {
  expect_error(validate_methodology_mode(NULL, allow_null = FALSE),
               "methodology.mode is required")
})

test_that("validate_methodology_mode silent on NULL when allow_null = TRUE", {
  expect_silent(validate_methodology_mode(NULL, allow_null = TRUE))
})

test_that("validate_methodology_mode errors on invalid string", {
  expect_error(validate_methodology_mode("bogus_mode"), "Invalid methodology mode")
})

test_that("validate_methodology_mode errors on non-character input", {
  expect_error(validate_methodology_mode(42), "single non-NA character string")
  expect_error(validate_methodology_mode(c("a", "b")), "single non-NA character string")
  expect_error(validate_methodology_mode(NA_character_), "single non-NA character string")
})

# ==============================================================================
# methodology_decision_aid tests (T1.3)
# ==============================================================================

test_that("methodology_decision_aid recommends reflexive_scaffold for reflexive TA", {
  rec <- methodology_decision_aid(
    interactive = FALSE,
    ta_family = "reflexive",
    has_apriori_framework = FALSE,
    wants_irr = FALSE
  )
  expect_equal(rec$recommended_mode, "reflexive_scaffold")
  expect_true(grepl("reflexive", rec$reasoning, ignore.case = TRUE))
})

test_that("methodology_decision_aid flags reflexive + IRR as incongruent", {
  rec <- methodology_decision_aid(
    interactive = FALSE,
    ta_family = "reflexive",
    has_apriori_framework = FALSE,
    wants_irr = TRUE
  )
  # Should suggest codebook_collaborative (IRR is appropriate there) but
  # surface the methodological incongruence in the reasoning.
  expect_equal(rec$recommended_mode, "codebook_collaborative")
  expect_true(grepl("incongruent|RTARG|Braun", rec$reasoning, ignore.case = TRUE))
  expect_equal(rec$alternative, "reflexive_scaffold")
})

test_that("methodology_decision_aid recommends codebook_collaborative for codebook TA", {
  rec <- methodology_decision_aid(
    interactive = FALSE, ta_family = "codebook",
    has_apriori_framework = FALSE, wants_irr = FALSE
  )
  expect_equal(rec$recommended_mode, "codebook_collaborative")
})

test_that("methodology_decision_aid recommends codebook_collaborative for template TA", {
  rec <- methodology_decision_aid(
    interactive = FALSE, ta_family = "template",
    has_apriori_framework = FALSE, wants_irr = FALSE
  )
  expect_equal(rec$recommended_mode, "codebook_collaborative")
})

test_that("methodology_decision_aid recommends framework_applied for framework analysis", {
  rec <- methodology_decision_aid(
    interactive = FALSE, ta_family = "framework",
    has_apriori_framework = TRUE, wants_irr = FALSE
  )
  expect_equal(rec$recommended_mode, "framework_applied")
  expect_true(grepl("framework_spec_path", rec$reasoning))
})

test_that("methodology_decision_aid handles content analysis via framework_applied", {
  rec <- methodology_decision_aid(
    interactive = FALSE, ta_family = "content",
    has_apriori_framework = TRUE, wants_irr = TRUE
  )
  expect_equal(rec$recommended_mode, "framework_applied")
  # The constructionist alternative should be surfaced
  expect_equal(rec$alternative, "codebook_collaborative")
})

test_that("methodology_decision_aid prints comparison when called print-only", {
  out <- capture.output(
    result <- methodology_decision_aid(interactive = FALSE)
  )
  expect_null(result)
  # All three modes should be mentioned in the printed comparison
  out_str <- paste(out, collapse = "\n")
  expect_true(grepl("reflexive_scaffold", out_str))
  expect_true(grepl("codebook_collaborative", out_str))
  expect_true(grepl("framework_applied", out_str))
})

test_that("methodology_decision_aid errors when ta_family is missing in non-interactive", {
  expect_error(
    methodology_decision_aid(interactive = FALSE,
                             has_apriori_framework = FALSE,
                             wants_irr = FALSE),
    "ta_family"
  )
})

# ============================================================================
# Phase 58 Tier 3 AH-5: deprecated-knob warnings
# ============================================================================

test_that("AH-5: .warn_deprecated_config_knobs flags pre-Phase-53 sequential-merge knobs", {
  cfg <- list(
    analysis = list(themes = list(
      merge_strategy        = "auto",
      max_merge_passes      = 5,
      stopping_criterion    = "convergence",
      min_merges_to_continue = 2,
      include_subthemes     = TRUE  # still valid; should NOT be flagged
    ))
  )
  flagged <- pakhom:::.warn_deprecated_config_knobs(cfg)
  expect_length(flagged, 4L)
  expect_true(any(grepl("merge_strategy", flagged)))
  expect_true(any(grepl("max_merge_passes", flagged)))
  expect_true(any(grepl("stopping_criterion", flagged)))
  expect_true(any(grepl("min_merges_to_continue", flagged)))
  expect_false(any(grepl("include_subthemes", flagged)))
})

test_that("AH-5: .warn_deprecated_config_knobs flags pre-Phase-56 saturation knobs", {
  cfg <- list(analysis = list(coding = list(
    saturation_enabled          = TRUE,
    saturation_window           = 100L,
    saturation_threshold        = 2L,
    saturation_confirmations    = 3L,
    min_coded_before_saturation = 50L,
    ai_assessment_interval      = 10L
  )))
  flagged <- pakhom:::.warn_deprecated_config_knobs(cfg)
  expect_length(flagged, 6L)
})

test_that("AH-5: empty config produces no deprecated-knob warnings", {
  flagged <- pakhom:::.warn_deprecated_config_knobs(list())
  expect_length(flagged, 0L)
})

# ============================================================================
# Phase 58 Tier 3 AH-4: reflexivity-scaffold warnings
# ============================================================================

test_that("AH-4: .warn_empty_reflexivity flags fully-empty reflexivity scaffold", {
  cfg <- list(study = list(
    research_focus = "test",
    researcher_positionality = NULL,
    research_paradigm = NULL,
    reflexive_notes = NULL
  ))
  empties <- pakhom:::.warn_empty_reflexivity(cfg)
  expect_true(all(empties))
})

test_that("AH-4: empty-string / NA / whitespace-only are treated as empty", {
  for (val in list("", "  ", NA_character_, NA)) {
    cfg <- list(study = list(
      researcher_positionality = val,
      research_paradigm        = val,
      reflexive_notes          = val
    ))
    empties <- pakhom:::.warn_empty_reflexivity(cfg)
    expect_true(all(empties),
                info = sprintf("value class %s should be empty", class(val)[1]))
  }
})

test_that("AH-4: partially-populated reflexivity produces info-level signal", {
  cfg <- list(study = list(
    researcher_positionality = "Researcher with 10 years experience",
    research_paradigm        = NULL,
    reflexive_notes          = NULL
  ))
  empties <- pakhom:::.warn_empty_reflexivity(cfg)
  expect_equal(sum(empties), 2L)
  expect_false(empties["positionality"])
})

test_that("AH-4: fully-populated reflexivity produces no warning", {
  cfg <- list(study = list(
    researcher_positionality = "Clinical psychologist",
    research_paradigm        = "critical realist",
    reflexive_notes          = "Some reflexive notes"
  ))
  empties <- pakhom:::.warn_empty_reflexivity(cfg)
  expect_false(any(empties))
})

# ============================================================================
# Phase 58 Tier 3 audit followup LOW-6: validate_config wires the warns through
# ============================================================================

test_that("AH-5 integration: validate_config calls deprecated-knob warn helper", {
  # Build a minimal-but-valid config carrying a deprecated knob; verify
  # validate_config completes (no error) AND logs a warn referencing
  # the knob. Pre-fix the helper existed but no test exercised the
  # call-site wiring at R/01_config.R:206.
  skip_if_not(file.exists("../../inst/config/default_config.yaml"),
              "default config not on test working dir")
  td <- withr::local_tempdir()
  db_path <- file.path(td, "test.db")
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  DBI::dbWriteTable(con, "posts", data.frame(post_id = "a", text = "x",
                                                stringsAsFactors = FALSE))
  DBI::dbDisconnect(con)
  Sys.setenv(OPENAI_API_KEY = "sk-test-fake-key")
  on.exit(Sys.unsetenv("OPENAI_API_KEY"), add = TRUE)

  cfg <- list(
    study = list(research_focus = "test focus", concepts = c("a", "b")),
    methodology = list(mode = "codebook_collaborative"),
    ai = list(provider = "openai",
              openai = list(api_key_env = "OPENAI_API_KEY")),
    data = list(database = db_path),
    output = list(results_dir = td),
    analysis = list(themes = list(merge_strategy = "auto"))  # deprecated
  )
  # Should not error; should emit a deprecated-knob warn.
  expect_no_error(validate_config(cfg))
})
