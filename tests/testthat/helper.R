# ==============================================================================
# Shared test fixtures and helpers
# ==============================================================================

# A no-op logger appender used as the test-suite baseline. Many tests
# deliberately exercise error / warning / failure code paths (a nonexistent IRR
# sheet, a simulated AI-call failure, a provider misconfiguration), which call
# log_error()/log_warn()/log_info() and then PASS. logger's default console
# appender would write those to stderr, where GitHub Actions surfaces them as
# ##[error]/##[warning]/##[notice] annotations -- making a clean run (R CMD
# check Status: OK, testthat FAIL 0) look like it carries dozens of "errors".
# setup.R installs this appender for the suite; restore to it (not the console
# appender) when a test temporarily swaps the appender to capture log output.
.pakhom_test_silent_appender <- function(lines) invisible(NULL)

#' Create a mock AIProvider object for testing (no real API calls)
#'
#' Mirrors the structure produced by `create_ai_provider()` so tests can
#' exercise downstream code paths without hitting the real API. Update this
#' alongside any change to the real provider's structure.
mock_provider <- function(provider_type = "openai") {
  key_env <- new.env(parent = emptyenv())
  key_env$key <- if (provider_type == "openai") "sk-test-fake-key-for-unit-tests"
                 else "sk-ant-test-fake-key-for-unit-tests"
  structure(list(
    provider = provider_type,
    key_env = key_env,
    models = list(
      primary = if (provider_type == "openai") "gpt-4o" else "claude-sonnet-4-20250514",
      fast = if (provider_type == "openai") "gpt-4o-mini" else "claude-sonnet-4-20250514",
      reasoning = if (provider_type == "openai") "o3-mini" else NULL,
      embedding = if (provider_type == "openai") "text-embedding-3-small" else NULL
    ),
    rate_limits = list(
      requests_per_minute = if (provider_type == "openai") 5000 else 1000,
      tokens_per_minute = if (provider_type == "openai") 800000 else 400000,
      batch_size = if (provider_type == "openai") 20 else 10,
      delay_between_batches = if (provider_type == "openai") 0.5 else 1.0
    ),
    anthropic_api_version = "2023-06-01",
    max_tokens = list(
      coding = 2000,
      theming = 4000,
      sentiment = 1500,
      review = 3000,
      insight = 1500,
      synthesis = 2000
    ),
    temperature = list(
      coding = 0.3,
      theming = 0.4,
      sentiment = 0.1,
      review = 0.3,
      insight = 0.4,
      synthesis = 0.4
    ),
    context_window = if (provider_type == "openai") 128000L else 200000L
  ), class = "AIProvider")
}

#' Create a minimal sample dataset for testing
sample_data <- function(n = 10) {
  withr::local_seed(42)
  tibble::tibble(
    std_id = paste0("entry_", seq_len(n)),
    std_text = c(
      "I have trouble focusing in the afternoon after a morning of back-to-back calls",
      "Overtime hours have decreased since we adopted the new scheduling policy",
      "The constant context-switching leaves me feeling drained all day",
      "My focus improved significantly once I blocked out deep-work mornings",
      "I feel anxious about deadlines and it affects my evenings",
      "Async messaging helps cut meetings but makes me check email late",
      "A short walk before work helps me concentrate and avoid burnout",
      "Overwork is worse on the weeks I skip my planning routine",
      "My manager adjusted my workload and my hours normalized",
      "Tight deadlines trigger both my overtime and my stress"
    )[seq_len(n)],
    sentiment_score = round(runif(n, -1, 1), 2),
    all_emotions = sample(c("sadness", "anxiety", "frustration", "hope", "neutral"),
                          n, replace = TRUE),
    emotion_intensity = round(runif(n, 0, 1), 2),
    confidence = round(runif(n, 0.5, 1), 2),
    source_table = sample(c("posts", "comments"), n, replace = TRUE)
  )
}

#' Create a minimal ThemeSet for testing
mock_theme_set <- function() {
  themes <- list(
    list(
      name = "Focus Fragmentation",
      description = "Effects of meeting load on deep focus",
      codes_included = c("context switching", "deep work", "meeting load"),
      subthemes = c("Interruption-driven distraction", "Schedule fragmentation"),
      keywords = c("focus", "meetings", "interruptions"),
      narrative = "This theme captures focus-related experiences.",
      supporting_quotes = list("I can't concentrate after a morning of calls")
    ),
    list(
      name = "Policy Effectiveness",
      description = "Perceived effectiveness of the scheduling policy",
      codes_included = c("workload control", "schedule adjustment", "side effects"),
      subthemes = c("Positive outcomes", "Adjustment burden"),
      keywords = c("policy", "scheduling", "workload"),
      narrative = "This theme reflects experiences with the new policy.",
      supporting_quotes = list("The new schedule really helped me")
    )
  )
  create_theme_set(themes)
}

#' Create a minimal config list for testing
mock_config <- function() {
  list(
    study = list(
      research_focus = "scheduling-policy effects on focus and overwork",
      concepts = c("scheduling", "focus", "overwork")
    ),
    ai = list(
      provider = "openai",
      max_tokens = list(coding = 2000, sentiment = 1500),
      temperature = list(coding = 0.3, sentiment = 0.1)
    ),
    analysis = list(
      themes = list(
        # Removed dead theme knobs (min_themes, max_themes,
        # multi_label_assignment, max_theme_proportion). Per C1 the
        # algorithm has no count thresholds; a later audit cleanup found
        # multi_label_assignment had no effect and was display-only.
        include_subthemes = TRUE
      ),
      correlations = list(
        method = "spearman",
        adjust_method = "bonferroni",
        min_observations = 30,
        min_theme_entries = 5
      )
    )
  )
}
