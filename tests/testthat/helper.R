# ==============================================================================
# Shared test fixtures and helpers
# ==============================================================================

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
      "I have trouble sleeping after taking my medication at night",
      "Binge eating episodes have decreased since starting treatment",
      "The side effects of the drug make me feel exhausted all day",
      "My sleep quality improved significantly with the new dosage",
      "I feel anxious about eating and it affects my sleep patterns",
      "The medication helps control cravings but causes insomnia",
      "Exercise before bed helps me sleep better and eat less",
      "Night eating syndrome is worse when I skip my medication",
      "The doctor adjusted my dose and my sleep normalized",
      "Stress triggers both my binge eating and sleep problems"
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
      name = "Sleep Disruption",
      description = "Medication effects on sleep quality",
      codes_included = c("insomnia", "sleep quality", "medication timing"),
      subthemes = c("Drug-induced insomnia", "Sleep architecture changes"),
      keywords = c("sleep", "insomnia", "night"),
      narrative = "This theme captures sleep-related experiences.",
      supporting_quotes = list("I can't sleep after taking my pills")
    ),
    list(
      name = "Treatment Efficacy",
      description = "Perceived effectiveness of medication",
      codes_included = c("craving control", "dosage adjustment", "side effects"),
      subthemes = c("Positive outcomes", "Side effect burden"),
      keywords = c("medication", "treatment", "dose"),
      narrative = "This theme reflects experiences with treatment.",
      supporting_quotes = list("The medication really helped me")
    )
  )
  create_theme_set(themes)
}

#' Create a minimal config list for testing
mock_config <- function() {
  list(
    study = list(
      research_focus = "medication effects on sleep and binge eating",
      concepts = c("medication", "sleep", "binge eating")
    ),
    ai = list(
      provider = "openai",
      max_tokens = list(coding = 2000, sentiment = 1500),
      temperature = list(coding = 0.3, sentiment = 0.1)
    ),
    analysis = list(
      themes = list(
        min_themes = 3, max_themes = 10,
        multi_label_assignment = TRUE,
        # Phase 50e: removed `membership_threshold = 0.15` (was dead).
        max_theme_proportion = 0.60
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
