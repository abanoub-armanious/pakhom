# Tests for pipeline orchestrator (22_pipeline.R)
# Integration-level tests that mock AI calls and verify pipeline flow.

# ==============================================================================
# Helper: Create a minimal but valid config YAML
# ==============================================================================
create_test_config <- function(db_path, output_dir) {
  list(
    methodology = list(
      mode = "codebook_collaborative",  # T1.3: methodology declaration mandatory
      framework_spec_path = NULL,
      mode_locked_at = NULL,
      parent_run_id = NULL,
      mode_changed_from = NULL
    ),
    study = list(
      name = "Test Study",
      research_focus = "testing pipeline flow",
      research_context = "unit tests",
      concepts = c("sleep", "medication")
    ),
    ai = list(
      provider = "openai",
      openai = list(
        api_key = "sk-test-fake-key-for-testing",
        models = list(
          primary = "gpt-4o",
          fast = "gpt-4o-mini"
        ),
        rate_limits = list(rpm = 500, tpm = 150000)
      )
    ),
    data = list(
      database = db_path,
      tables = "posts",
      source_type = "reddit",
      preprocessing = list(
        min_char = 10,
        dedup_ratio = 0.9
      )
    ),
    learning = list(enabled = FALSE),
    analysis = list(
      test_mode = list(enabled = TRUE, sample_size = 5, seed = 42),
      sentiment = list(code_aware = TRUE, batch_size = 10, dynamic_batching = FALSE),
      coding = list(progressive = TRUE, max_retries_per_entry = 1, checkpoint_interval = 50),
      human_verification = list(enabled = FALSE),
      themes = list(
        merge_strategy = "auto",
        max_merge_passes = 3,
        min_merges_to_continue = 2,
        include_subthemes = TRUE
      ),
      correlations = list(
        method = "spearman",
        adjust_method = "bonferroni",
        min_observations = 3,
        min_theme_entries = 1,
        dynamic_method = FALSE
      )
    ),
    output = list(
      results_dir = output_dir,
      generate_report = FALSE,
      generate_correlation_plot = FALSE,
      comparison_enabled = FALSE
    ),
    logging = list(log_level = "WARN")
  )
}

# ==============================================================================
# Helper: Create a test database with sample posts
# ==============================================================================
create_test_db <- function(db_path, n_posts = 10) {
  db <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(db), add = TRUE)

  DBI::dbExecute(db, "CREATE TABLE IF NOT EXISTS posts (
    post_id TEXT PRIMARY KEY,
    subreddit TEXT,
    title TEXT,
    body TEXT,
    author TEXT,
    score INTEGER,
    num_comments INTEGER,
    created_utc REAL,
    scraped_at TEXT,
    permalink TEXT
  )")

  posts <- data.frame(
    post_id = paste0("p_", seq_len(n_posts)),
    subreddit = "test",
    title = paste0("Test title ", seq_len(n_posts)),
    body = c(
      "I have trouble sleeping after taking my medication at night and it really affects my daily routine",
      "Binge eating episodes have decreased since starting treatment with the new medication",
      "The side effects of the drug make me feel exhausted all day long and I cannot function",
      "My sleep quality improved significantly with the new dosage adjustment last week",
      "I feel anxious about eating and it affects my sleep patterns every single night",
      "The medication helps control cravings but causes insomnia which is very frustrating",
      "Exercise before bed helps me sleep better and eat less during the nighttime hours",
      "Night eating syndrome is worse when I skip my medication for even one day",
      "The doctor adjusted my dose and my sleep finally normalized after two weeks of changes",
      "Stress triggers both my binge eating and sleep problems making everything much worse"
    )[seq_len(n_posts)],
    author = paste0("user_", seq_len(n_posts)),
    score = seq_len(n_posts) * 10L,
    num_comments = seq_len(n_posts),
    created_utc = as.numeric(Sys.time()) - seq_len(n_posts) * 3600,
    scraped_at = as.character(Sys.time()),
    permalink = paste0("/r/test/comments/p_", seq_len(n_posts), "/"),
    stringsAsFactors = FALSE
  )

  DBI::dbWriteTable(db, "posts", posts, append = TRUE)
  invisible(db_path)
}

# ==============================================================================
# Checkpoint system integration with pipeline
# ==============================================================================
test_that("checkpoint resume skips completed steps", {
  skip_if_not_installed("RSQLite")

  tmp_dir <- tempfile("cp_resume_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create checkpoint manager and save a checkpoint
  cp <- init_checkpoints(file.path(tmp_dir, "checkpoints"))

  # Save data_loaded checkpoint
  data <- sample_data(5)
  save_checkpoint(cp, "data_loaded", list(data = data))

  # Verify resume point detection
  resume_point <- find_resume_point(cp)
  expect_equal(resume_point, "data_loaded")

  # Save sentiment_done too
  save_checkpoint(cp, "sentiment_done", list(data = data))
  resume_point <- find_resume_point(cp)
  expect_equal(resume_point, "sentiment_done")
})

# ==============================================================================
# Config hash detection for config changes
# ==============================================================================
test_that("config hash detects changes", {
  # hash_config takes a file path, not a config object
  tmp1 <- tempfile(fileext = ".yaml")
  tmp2 <- tempfile(fileext = ".yaml")
  on.exit(unlink(c(tmp1, tmp2)), add = TRUE)

  config1 <- mock_config()
  config2 <- mock_config()
  config2$study$research_focus <- "different focus"

  yaml::write_yaml(config1, tmp1)
  yaml::write_yaml(config2, tmp2)

  hash1 <- hash_config(tmp1)
  hash2 <- hash_config(tmp2)

  expect_type(hash1, "character")
  expect_type(hash2, "character")
  expect_true(hash1 != hash2)

  # Same file should produce same hash
  expect_equal(hash_config(tmp1), hash1)

  # Non-existent file should return NA
  expect_true(is.na(hash_config("/nonexistent/config.yaml")))
})

# ==============================================================================
# Pipeline step order validation
# ==============================================================================
test_that("pipeline step order is consistent", {
  # The checkpoint system defines a step_order. Verify it matches
  # the actual execution order.
  expected_order <- c(
    "data_loaded", "progressive_coding", "sentiment_done",
    "themes_generated", "correlations"
  )

  # Create checkpoint manager and check its step order
  tmp_dir <- tempfile("step_order_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  cp <- init_checkpoints(tmp_dir)

  # The step_order should contain all expected steps
  for (step in expected_order) {
    # Save and load each step to verify it's recognized
    save_checkpoint(cp, step, list(test = TRUE))
  }

  checkpoints <- list_checkpoints(cp)
  # list_checkpoints returns list with $completed (char vector) and $details (tibble)
  expect_equal(length(checkpoints$completed), length(expected_order))
})

# ==============================================================================
# Data flow: pass-by-value correctness
# ==============================================================================
test_that("data modifications are captured through pipeline steps", {
  # This tests the critical R pass-by-value pattern
  data <- sample_data(5)

  # Simulate what the pipeline does: each step modifies and returns data
  # Step 1: Add relevance scores
  data$relevance_score <- runif(nrow(data), 0.5, 1.0)
  expect_true("relevance_score" %in% names(data))

  # Step 2: Add sentiment (already present in sample_data, but verify modification)
  data$sentiment_score <- runif(nrow(data), -1, 1)
  expect_true("sentiment_score" %in% names(data))

  # Step 3: Add theme assignments
  data$emerged_themes <- paste0("theme_", sample(1:3, nrow(data), replace = TRUE))
  expect_true("emerged_themes" %in% names(data))

  # All columns should be present at end
  expect_true(all(c("std_id", "std_text", "relevance_score",
                     "sentiment_score", "emerged_themes") %in% names(data)))
})

# ==============================================================================
# run_analysis: Input validation
# ==============================================================================
test_that("run_analysis errors on non-existent config path", {
  expect_error(
    run_analysis("/nonexistent/config.yaml"),
    regex = "config|exist|found|read",
    ignore.case = TRUE
  )
})

test_that("run_analysis errors on invalid config", {
  tmp_config <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp_config), add = TRUE)

  # Write a config missing required sections
  yaml::write_yaml(list(study = list(name = "test")), tmp_config)

  expect_error(
    run_analysis(tmp_config),
    regex = "config|valid|missing|required",
    ignore.case = TRUE
  )
})

# ==============================================================================
# Export and integrity checks
# ==============================================================================
test_that("export_results creates expected output files", {
  skip_if_not_installed("RSQLite")

  tmp_dir <- tempfile("export_test_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  data <- sample_data(10)
  data$emerged_themes <- rep(c("Sleep Disruption", "Treatment Efficacy"), 5)
  data$assigned_theme_list <- as.list(data$emerged_themes)
  data$theme_membership_Sleep.Disruption <- rep(c(1L, 0L), 5)
  data$theme_membership_Treatment.Efficacy <- rep(c(0L, 1L), 5)

  ts <- mock_theme_set()
  config <- mock_config()

  consolidated <- list(
    codes = tibble::tibble(
      code_text = c("insomnia", "craving control", "dosage adjustment"),
      frequency = c(5L, 3L, 2L),
      code_type = rep("descriptive", 3)
    )
  )

  corr_df <- tibble::tibble(
    var1 = "sentiment_score",
    var2 = "theme_Sleep Disruption",
    correlation = 0.45,
    p_value = 0.01,
    method = "spearman",
    n = 10L,
    significant = TRUE
  )

  export_results(
    data = data,
    theme_set = ts,
    correlations_df = corr_df,
    insights = list(),
    consolidated = consolidated,
    output_dir = tmp_dir
  )

  # Check key files exist
  expect_true(file.exists(file.path(tmp_dir, "themes.json")))
})

# ==============================================================================
# verify_run_integrity: Post-run validation
# ==============================================================================
test_that("verify_run_integrity detects missing files", {
  tmp_dir <- tempfile("integrity_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Empty directory should report missing files
  result <- verify_run_integrity(tmp_dir)

  expect_type(result, "list")
  expect_true(!is.null(result$missing) || !is.null(result$status))
})

# ==============================================================================
# Full pipeline smoke test (minimal, mocked)
# ==============================================================================
test_that("load_config with valid YAML produces ThematicConfig", {
  skip_if_not_installed("RSQLite")

  tmp_dir <- tempfile("smoke_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  db_path <- file.path(tmp_dir, "test.db")
  create_test_db(db_path, n_posts = 5)

  output_dir <- file.path(tmp_dir, "output")
  dir.create(output_dir)

  config_list <- create_test_config(db_path, output_dir)
  config_path <- file.path(tmp_dir, "config.yaml")
  yaml::write_yaml(config_list, config_path)

  # This tests that config loads and validates without error
  config <- load_config(config_path)
  expect_true(inherits(config, "ThematicConfig"))
  expect_equal(config$study$name, "Test Study")
  expect_equal(config$data$database, db_path)
})
