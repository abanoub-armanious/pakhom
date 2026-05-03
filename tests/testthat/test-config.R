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
  # Theme count constraints are NULL by default (data-driven)
  expect_null(cfg$analysis$themes$min_themes)
  expect_null(cfg$analysis$themes$max_themes)
  expect_null(cfg$analysis$themes$min_subthemes_per_theme)
  expect_null(cfg$analysis$themes$max_subthemes_per_theme)
  expect_true(cfg$analysis$themes$max_theme_proportion > 0)
  expect_true(cfg$analysis$themes$max_theme_proportion <= 1)
  expect_true(cfg$analysis$correlations$use_multi_label)
})

test_that("default_config has new config fields", {
  cfg <- default_config("codebook_collaborative")
  expect_null(cfg$study$researcher_positionality)
  expect_null(cfg$data$explicit_columns)
  expect_type(cfg$data$preprocessing$custom_cleaning_rules, "list")
  expect_null(cfg$analysis$coding$fallback_keyword_patterns)
  # New reflexivity fields
  expect_null(cfg$study$research_paradigm)
  expect_null(cfg$study$reflexive_notes)
  # Multi-model config
  expect_false(cfg$ai$multi_model$enabled)
  # Review format
  expect_equal(cfg$analysis$review_points$format, "csv")
})

test_that("default_config has multi-label assignment enabled", {
  cfg <- default_config("codebook_collaborative")
  expect_true(cfg$analysis$themes$multi_label_assignment)
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

test_that("default_config() with no methodology arg warns and falls back to codebook_collaborative", {
  expect_warning(
    cfg <- default_config(),
    regexp = "methodology not specified"
  )
  expect_equal(cfg$methodology$mode, "codebook_collaborative")
  expect_true(!is.null(cfg$methodology$mode_locked_at))
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
