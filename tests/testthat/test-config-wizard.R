# ==============================================================================
# Tests for the config wizards (R/20_shiny_wizard.R + R/01_config.R)
#
# Regression coverage for the publication-blocking bug where BOTH wizards
# produced configs that failed validate_config():
#   * Shiny config_wizard_app() -> .build_config_from_inputs() never emitted
#     the mandatory `methodology` block, so loading the saved config errored
#     with "Missing required config section: 'methodology'".
#   * CLI config_wizard() called create_config() with `data_path=` /
#     `config_path=` (not real params -> swallowed by ... and dropped) and
#     never declared a methodology mode.
#
# These tests build a config via each path for all three methodology modes
# and assert load_config()/validate_config() succeeds with methodology$mode
# set. The Shiny builder is exercised with `input` mocked as a plain named
# list (it only does input[[id]] / input$id access, so no Shiny needed).
# config_wizard() itself is an interactive readline() shell that cannot be
# driven non-interactively (base interactive()/readline() are unmockable by
# testthat), so the CLI path is covered through create_config() -- the
# function it delegates to with the now-corrected argument names -- plus a
# direct unit test of its .parse_methodology_choice() helper.
# ==============================================================================

.WIZARD_MODES <- c("reflexive_scaffold", "codebook_collaborative", "framework_applied")

# Minimal mock of the Shiny `input` object for .build_config_from_inputs().
# Only fields that matter for validation are set; everything else falls back
# to the builder's own defaults (val() returns the default for missing keys).
.mock_wizard_input <- function(methodology_mode, framework_spec_path = NULL,
                               research_focus = "How does X relate to Y?") {
  input <- list(
    methodology_mode = methodology_mode,
    study_name = "Wizard Test Study",
    research_focus = research_focus,
    positionality = "Test analyst with domain expertise",
    ai_provider = "openai",
    api_key_env = "OPENAI_API_KEY",
    source_type = "generic",
    results_dir = "outputs/results"
  )
  if (!is.null(framework_spec_path)) {
    input$framework_spec_path <- framework_spec_path
  }
  input
}

# Round-trip a config list through YAML + load_config() and return the
# validated ThematicConfig. A fake API key env var satisfies validate_config()'s
# key check; the empty database path skips the file-existence check.
.load_wizard_config <- function(config) {
  td <- withr::local_tempdir(.local_envir = parent.frame())
  path <- file.path(td, "config.yaml")
  yaml::write_yaml(config, path)
  withr::local_envvar(OPENAI_API_KEY = "sk-test-fake-key",
                      .local_envir = parent.frame())
  load_config(path)
}

# ------------------------------------------------------------------------------
# Shiny path: .build_config_from_inputs()
# ------------------------------------------------------------------------------

test_that("Shiny builder emits a methodology block (bug 1 regression)", {
  # Pre-fix this returned a config with NO `methodology` key at all.
  input <- .mock_wizard_input("codebook_collaborative")
  config <- pakhom:::.build_config_from_inputs(input)
  expect_false(is.null(config$methodology))
  expect_equal(config$methodology$mode, "codebook_collaborative")
})

test_that("Shiny builder -> load_config succeeds with methodology$mode set, all 3 modes", {
  for (mode in .WIZARD_MODES) {
    fsp <- if (mode == "framework_applied") "tpb" else NULL
    input <- .mock_wizard_input(mode, framework_spec_path = fsp)

    config <- pakhom:::.build_config_from_inputs(input)
    expect_equal(config$methodology$mode, mode, info = mode)

    cfg <- .load_wizard_config(config)
    expect_s3_class(cfg, "ThematicConfig")
    expect_equal(cfg$methodology$mode, mode, info = mode)
    expect_equal(cfg$study$research_focus, "How does X relate to Y?", info = mode)
    expect_equal(cfg$study$researcher_positionality,
                 "Test analyst with domain expertise", info = mode)
  }
})

test_that("Shiny builder carries framework_spec_path through for Mode 3", {
  input <- .mock_wizard_input("framework_applied", framework_spec_path = "tpb")
  config <- pakhom:::.build_config_from_inputs(input)
  expect_equal(config$methodology$framework_spec_path, "tpb")

  cfg <- .load_wizard_config(config)
  expect_equal(cfg$methodology$framework_spec_path, "tpb")
})

test_that("Shiny builder: Mode 3 without a spec path yields an actionable error", {
  # The UI blocks advancing without a spec, but the builder still emits the
  # key so a missing path fails validate_config() with the right message
  # rather than being silently dropped.
  input <- .mock_wizard_input("framework_applied", framework_spec_path = NULL)
  config <- pakhom:::.build_config_from_inputs(input)
  expect_true("framework_spec_path" %in% names(config$methodology))

  td <- withr::local_tempdir()
  path <- file.path(td, "config.yaml")
  yaml::write_yaml(config, path)
  withr::local_envvar(OPENAI_API_KEY = "sk-test-fake-key")
  expect_error(load_config(path), "framework_spec_path")
})

# ------------------------------------------------------------------------------
# CLI path: create_config() (the function config_wizard() delegates to) +
# the .parse_methodology_choice() helper used by config_wizard().
# ------------------------------------------------------------------------------

test_that("CLI path: create_config writes a loadable config, all 3 modes (bug 2 regression)", {
  for (mode in .WIZARD_MODES) {
    td <- withr::local_tempdir()
    cfg_path <- file.path(td, "config.yaml")
    db_path <- file.path(td, "data.db")
    file.create(db_path)  # validate_config checks the database file exists
    fsp <- if (mode == "framework_applied") "tpb" else NULL

    # These are exactly the arguments config_wizard() now passes:
    # methodology=, database_path= (not data_path=), output_path= (not
    # config_path=), plus framework_spec_path for Mode 3.
    create_config(
      methodology = mode,
      study_name = "CLI Wizard Study",
      research_focus = "How does X relate to Y?",
      database_path = db_path,
      output_path = cfg_path,
      framework_spec_path = fsp,
      provider = "openai"
    )
    expect_true(file.exists(cfg_path), info = mode)

    withr::local_envvar(OPENAI_API_KEY = "sk-test-fake-key")
    cfg <- load_config(cfg_path)
    expect_s3_class(cfg, "ThematicConfig")
    expect_equal(cfg$methodology$mode, mode, info = mode)
    # The database path landed in config$data$database (the real bug was it
    # being dropped because `data_path=` is not a create_config() parameter).
    expect_equal(normalizePath(cfg$data$database), normalizePath(db_path), info = mode)
    # No bogus top-level keys from mis-routed arguments.
    expect_null(cfg$data_path, info = mode)
    expect_null(cfg$config_path, info = mode)
  }
})

test_that(".parse_methodology_choice maps numbers and names; blank yields NULL (AC3)", {
  expect_equal(pakhom:::.parse_methodology_choice("1"), "reflexive_scaffold")
  expect_equal(pakhom:::.parse_methodology_choice("2"), "codebook_collaborative")
  expect_equal(pakhom:::.parse_methodology_choice("3"), "framework_applied")
  # AC3 (no default mode): blank / whitespace input does NOT resolve to a mode --
  # there is no default. It returns NULL so config_wizard() re-prompts.
  expect_null(pakhom:::.parse_methodology_choice(""))
  expect_null(pakhom:::.parse_methodology_choice("   "))
  # Canonical names pass through; surrounding whitespace tolerated.
  expect_equal(pakhom:::.parse_methodology_choice("reflexive_scaffold"), "reflexive_scaffold")
  expect_equal(pakhom:::.parse_methodology_choice("  framework_applied  "), "framework_applied")
  # Unrecognized -> NULL so config_wizard() can raise an actionable error.
  expect_null(pakhom:::.parse_methodology_choice("bogus"))
  expect_null(pakhom:::.parse_methodology_choice("4"))
})

test_that("the Shiny wizard's methodology radio starts UNSELECTED (AC3: no default mode)", {
  skip_if_not_installed("shiny")
  ui <- pakhom:::.ui_step_methodology()
  html <- as.character(htmltools::renderTags(ui)$html)
  # No radio option may be pre-checked: a preselected mode would let a
  # user click through and inherit a methodology they never chose.
  expect_false(grepl('checked="checked"', html, fixed = TRUE))
  expect_true(grepl('name="methodology_mode"', html, fixed = TRUE))
})

test_that(".build_config_from_inputs writes scraping.enabled in both states", {
  base <- list(methodology_mode = "codebook_collaborative", research_focus = "x",
               ai_provider = "openai")

  # Disabled must still write enabled = FALSE so unchecking the box turns off a
  # previously-enabled block on a merge-save.
  off <- pakhom:::.build_config_from_inputs(base)
  expect_false(off$scraping$enabled)

  on <- pakhom:::.build_config_from_inputs(c(base, list(
    scraping_enabled = TRUE,
    scraping_subreddits = "productivity, remotework",
    scraping_posts = 250, scraping_comments = TRUE,
    scraping_sort = "top", scraping_time = "year")))
  expect_true(on$scraping$enabled)
  expect_equal(unlist(on$scraping$subreddits), c("productivity", "remotework"))
  expect_equal(on$scraping$posts_per_subreddit, 250)
  expect_true(on$scraping$include_comments)
  expect_equal(on$scraping$sort_by, "top")
  expect_equal(on$scraping$time_filter, "year")
  # Credentials must never be written into the config (env-only).
  expect_null(on$scraping$reddit_client_id)
  expect_null(on$scraping$reddit_client_secret)
})

test_that("the Shiny wizard exposes a Reddit scraping step", {
  skip_if_not_installed("shiny")
  ui <- pakhom:::.ui_step_scraping()
  html <- as.character(htmltools::renderTags(ui)$html)
  expect_true(grepl("scraping_enabled", html, fixed = TRUE))
  expect_true(grepl("scraping_subreddits", html, fixed = TRUE))
})

test_that("create_config writes a scraping block from dot-path overrides", {
  cfg <- tempfile(fileext = ".yaml"); on.exit(unlink(cfg), add = TRUE)
  suppressMessages(create_config(
    methodology = "reflexive_scaffold", study_name = "S", research_focus = "x",
    output_path = cfg,
    scraping.enabled = TRUE, scraping.subreddits = list("AskReddit"),
    scraping.posts_per_subreddit = 100L))
  rt <- yaml::read_yaml(cfg)
  expect_true(rt$scraping$enabled)
  expect_equal(unlist(rt$scraping$subreddits), "AskReddit")
  expect_equal(rt$scraping$posts_per_subreddit, 100L)
})

test_that(".drop_null_leaves removes NULL leaves at every level", {
  x <- list(a = 1, b = NULL, c = list(d = NULL, e = 2), f = list(g = NULL))
  out <- pakhom:::.drop_null_leaves(x)
  expect_equal(out$a, 1)
  expect_null(out$b)            # removed
  expect_false("b" %in% names(out))
  expect_equal(out$c$e, 2)
  expect_false("d" %in% names(out$c))
  expect_equal(length(out$f), 0L)  # g dropped; f becomes empty
})

test_that("a blank wizard field does not delete an existing value on merge", {
  # Simulate the save-handler merge: existing file has a database path and a
  # scraping block; the wizard is re-run with the Data field blank and scraping
  # disabled. The merge must keep the database and turn scraping off.
  existing <- list(
    data = list(database = "my_data.db", source_type = "reddit"),
    scraping = list(enabled = TRUE, subreddits = list("productivity"))
  )
  wiz <- pakhom:::.build_config_from_inputs(list(
    methodology_mode = "codebook_collaborative", research_focus = "x",
    ai_provider = "openai"))  # database_path blank, scraping disabled

  merged <- utils::modifyList(existing, pakhom:::.drop_null_leaves(wiz))
  expect_equal(merged$data$database, "my_data.db")     # preserved, not deleted
  expect_false(merged$scraping$enabled)                # turned off
  expect_equal(unlist(merged$scraping$subreddits), "productivity")  # preserved
})

# ==============================================================================
# Wizard pre-load: a re-run round-trips an existing config (residual fix)
# ==============================================================================
test_that(".wizard_input_defaults is the faithful inverse of .build_config_from_inputs", {
  # build(defaults(build(input))) must equal build(input) for every managed
  # field, so re-opening the wizard and saving round-trips the config.
  inp <- list(
    methodology_mode = "framework_applied", framework_spec_path = "tpb",
    study_name = "My Study", research_focus = "RQ here", research_context = "forum data",
    concepts = "a, b, c", positionality = "my background",
    ai_provider = "anthropic", api_key_env = "ANTHROPIC_API_KEY",
    model_primary = "claude-sonnet-4-20250514", model_fast = "claude-haiku-4-5-20251001",
    rpm = 4000, tpm = 700000, batch_delay = 0.7,
    database_path = "data/x.db", tables = "posts, comments", source_type = "generic",
    min_text_length = 15, max_text_length = 9000,
    remove_urls = FALSE, remove_mentions = TRUE, remove_hashtags = TRUE,
    scraping_enabled = TRUE, scraping_subreddits = "productivity, remotework",
    scraping_posts = 300, scraping_comments = FALSE, scraping_sort = "top", scraping_time = "year",
    learning_enabled = TRUE, learning_base_dir = "manual",
    max_manuscript_chars = 12000, max_raw_samples = 3,
    test_mode = TRUE, test_sample_size = 50, test_seed = 7,
    checkpoint_interval = 25, max_retries = 2, include_in_vivo = FALSE,
    review_codes = TRUE, review_themes = TRUE,
    irr_enabled = TRUE, irr_sample = 30, irr_seed = 11,
    corr_method = "pearson", corr_adjust = "holm", corr_min_obs = 20, corr_min_theme = 3,
    results_dir = "out/res", gen_report = FALSE, gen_corr_plot = FALSE,
    gen_theme_details = FALSE, export_csv = FALSE, export_json = TRUE,
    gen_comparison = FALSE, log_level = "DEBUG"
  )
  cfg1 <- pakhom:::.build_config_from_inputs(inp)
  cfg2 <- pakhom:::.build_config_from_inputs(pakhom:::.wizard_input_defaults(cfg1))
  expect_identical(cfg2, cfg1)
})

test_that(".wizard_input_defaults(NULL) preserves AC3 and the built-in defaults", {
  d <- pakhom:::.wizard_input_defaults(NULL)
  expect_null(d$methodology_mode)          # AC3: no default mode
  expect_equal(d$study_name, "Untitled Study")
  expect_equal(d$ai_provider, "openai")
  expect_equal(d$source_type, "reddit")
  expect_false(d$scraping_enabled)
  expect_equal(d$scraping_posts, 500)
})

test_that("a pre-loaded value is rendered as the input default", {
  skip_if_not_installed("shiny")
  d <- pakhom:::.wizard_input_defaults(list(study = list(name = "Loaded Study Name")))
  html <- as.character(htmltools::renderTags(pakhom:::.ui_step_study(d))$html)
  expect_true(grepl("Loaded Study Name", html, fixed = TRUE))
})
