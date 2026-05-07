# ==============================================================================
# Configuration System — YAML-based with validation and defaults
# ==============================================================================

#' Load analysis configuration from YAML file
#'
#' @param config_path Path to YAML config file
#' @param overrides Named list of overrides (dot-separated keys,
#'   e.g., list("ai.provider" = "anthropic"))
#' @return A validated ThematicConfig S3 object
#' @export
load_config <- function(config_path, overrides = list()) {
  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path)
  }

  config <- yaml::read_yaml(config_path)

  # Apply overrides
  for (key in names(overrides)) {
    parts <- strsplit(key, "\\.")[[1]]
    config <- .set_nested(config, parts, overrides[[key]])
  }

  # Merge with defaults for any missing fields
  defaults <- .config_defaults()
  config <- .merge_defaults(config, defaults)

  # Resolve relative paths relative to config file directory
  config_dir <- normalizePath(dirname(config_path), mustWork = TRUE)
  config <- .resolve_paths(config, config_dir)

  config <- structure(config, class = "ThematicConfig")
  validate_config(config)
  config
}

#' Create a default configuration object
#'
#' Returns a working starter config. \strong{Methodology mode is the
#' load-bearing architectural decision in pakhom} -- it determines AI
#' agency, output stamping, and reporting requirements. Pass \code{methodology}
#' explicitly:
#'
#' \preformatted{
#'   default_config("reflexive_scaffold")      # Mode 1: AI as provocateur
#'   default_config("codebook_collaborative")  # Mode 2: AI proposes, researcher gates
#'   default_config("framework_applied")       # Mode 3: AI applies researcher's framework
#' }
#'
#' Calling \code{default_config()} with no argument emits a warning and falls
#' back to \code{"codebook_collaborative"} (the mode that best matches v1.x
#' behavior and serves the largest existing user population). The warning
#' exists by design: per Spool 2011 (>95\% of users never change defaults),
#' a silent default would let users inherit a methodology without conscious
#' choice -- contrary to pakhom's architectural commitment that
#' methodology declaration must be explicit. Run
#' \code{\link{methodology_decision_aid}} for guidance on choosing.
#'
#' Note: \code{.config_defaults()} (internal) returns the bare schema with
#' \code{methodology$mode = NULL}, so user-supplied YAMLs that omit the
#' methodology section fail validation with a clear error rather than
#' silently inheriting a default. \code{default_config()} is the only
#' entry point that pre-fills mode (and only with the warning above).
#'
#' @param methodology One of \code{"reflexive_scaffold"},
#'   \code{"codebook_collaborative"}, or \code{"framework_applied"}.
#'   \strong{Mandatory}: per AC3 ("no default mode; explicit
#'   declaration mandatory"), \code{methodology = NULL} produces an
#'   error rather than silently defaulting. Run
#'   \code{methodology_decision_aid()} for guidance on the choice.
#' @return A ThematicConfig S3 object with all defaults
#' @seealso \code{\link{methodology_decision_aid}} for guidance on choosing
#'   a methodology mode; \code{\link{validate_methodology_mode}} for the
#'   underlying validator.
#' @export
default_config <- function(methodology = NULL) {
  # Phase 37 audit (AC HIGH): the previous behavior here violated AC3
  # ("no default mode; explicit declaration mandatory") -- a NULL
  # methodology argument would warn-and-default to
  # codebook_collaborative, exactly the silent-default failure mode
  # AC3 commits the package against. The validate_config + YAML-load
  # paths already enforced AC3 cleanly; the programmatic
  # default_config() entry point was the lone exception. Now matches
  # validate_methodology_mode(allow_null = FALSE) symmetric with the
  # rest of the package.
  if (is.null(methodology)) {
    stop(
      "default_config(): methodology argument is mandatory per AC3 ",
      "(no default mode; explicit declaration mandatory).\n\n",
      "Choose one of:\n",
      "  - 'reflexive_scaffold'      (Mode 1: AI as provocateur)\n",
      "  - 'codebook_collaborative'  (Mode 2: AI proposes, researcher gates)\n",
      "  - 'framework_applied'       (Mode 3: AI applies your framework)\n\n",
      "Run methodology_decision_aid() for guidance, or see ",
      "vignette('methodology-modes') for worked examples of each mode.",
      call. = FALSE
    )
  }
  validate_methodology_mode(methodology, allow_null = FALSE,
                            caller = "default_config")
  config <- .config_defaults()
  config$methodology$mode <- methodology
  config$methodology$mode_locked_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
  structure(config, class = "ThematicConfig")
}

#' Validate configuration completeness and correctness
#' @param config A ThematicConfig object
#' @return The config invisibly, or stops with informative errors
validate_config <- function(config) {
  errors <- character(0)

  # Required top-level sections (methodology added in Sprint-4 / T1.3)
  for (section in c("study", "ai", "data", "analysis", "output", "methodology")) {
    if (is.null(config[[section]])) {
      errors <- c(errors, sprintf("Missing required config section: '%s'", section))
    }
  }
  if (length(errors) > 0) stop(paste(errors, collapse = "\n"))

  # Validate methodology mode (T1.3). Mode is mandatory; framework_applied
  # additionally requires framework_spec_path. Mode-specific behavior is
  # routed at the orchestrator (R/18_pipeline.R) and downstream functions.
  tryCatch(
    validate_methodology_mode(config$methodology$mode, allow_null = FALSE,
                              caller = "validate_config"),
    error = function(e) errors <<- c(errors, conditionMessage(e))
  )
  if (identical(config$methodology$mode, .METHODOLOGY_MODE_FRAMEWORK_APPLIED)) {
    if (is.null(config$methodology$framework_spec_path) ||
        !nzchar(config$methodology$framework_spec_path)) {
      errors <- c(errors,
        "methodology.framework_spec_path is required when mode = 'framework_applied'")
    }
  }

  # Validate study
  if (is.null(config$study$research_focus) || is.na(config$study$research_focus) || nchar(config$study$research_focus) == 0) {
    errors <- c(errors, "study.research_focus is required")
  }

  # Validate AI provider
  provider <- config$ai$provider
  if (is.null(provider) || !provider %in% c("openai", "anthropic")) {
    errors <- c(errors, sprintf("ai.provider must be 'openai' or 'anthropic', got '%s'",
                                provider %||% "NULL"))
  }

  # Validate API key is set (direct key or env var)
  # Use [[ ]] to avoid R's partial matching ($api_key matching $api_key_env)
  if (!is.null(provider) && provider %in% c("openai", "anthropic")) {
    provider_cfg <- config$ai[[provider]]
    direct_key <- provider_cfg[["api_key"]] %||% ""
    has_direct_key <- nchar(direct_key) > 0 && direct_key != "PASTE_YOUR_KEY_HERE"

    # Warn if API key appears to be pasted directly (security risk)
    if (has_direct_key && grepl("^sk-", direct_key)) {
      log_warn("API key detected in config file. For security, use an environment variable instead:")
      log_warn("  1. Remove the key from config.yaml (set api_key: \"\")")
      log_warn("  2. Set: Sys.setenv({toupper(paste0(provider, '_API_KEY'))} = 'your-key')")
      log_warn("  Or add to .Renviron: {toupper(paste0(provider, '_API_KEY'))}=your-key")
    }

    if (!has_direct_key) {
      key_env <- provider_cfg[["api_key_env"]] %||% toupper(paste0(provider, "_API_KEY"))
      if (nchar(Sys.getenv(key_env)) == 0) {
        errors <- c(errors, sprintf(
          "API key not configured. Either set ai.%s.api_key in config.yaml or set env var: Sys.setenv(%s = 'your-key')",
          provider, key_env %||% toupper(paste0(provider, "_API_KEY"))
        ))
      }
    }
  }

  # Validate data paths
  if (!is.null(config$data$database)) {
    db_path <- config$data$database
    if (!file.exists(db_path)) {
      errors <- c(errors, sprintf("Database file not found: %s", db_path))
    }
  }

  # Validate learning paths if enabled
  if (isTRUE(config$learning$enabled)) {
    base_dir <- config$learning$base_dir
    if (is.null(base_dir) || !dir.exists(base_dir)) {
      errors <- c(errors, sprintf(
        "learning.base_dir does not exist: '%s'. Use an absolute path.",
        base_dir %||% "NULL"
      ))
    }
  }

  # Validate output dir
  output_dir <- config$output$results_dir
  if (is.null(output_dir)) {
    errors <- c(errors, "output.results_dir is required")
  }

  if (length(errors) > 0) {
    stop("Configuration validation failed:\n  - ", paste(errors, collapse = "\n  - "))
  }

  invisible(config)
}

#' Print method for ThematicConfig
#' @param x ThematicConfig object
#' @param ... Additional arguments (ignored)
#' @export
print.ThematicConfig <- function(x, ...) {
  cat("ThematicConfig\n")
  cat(sprintf("  Study:        %s\n", x$study$name %||% "(unnamed)"))
  cat(sprintf("  Focus:        %s\n", truncate_text(x$study$research_focus, 60)))
  mode_label <- x$methodology$mode %||% "(NOT DECLARED -- run methodology_decision_aid())"
  cat(sprintf("  Methodology:  %s\n", mode_label))
  cat(sprintf("  Provider:     %s\n", x$ai$provider))
  cat(sprintf("  Database:     %s\n", x$data$database %||% "(not set)"))
  cat(sprintf("  Learning:     %s\n", if (isTRUE(x$learning$enabled)) "enabled" else "disabled"))
  cat(sprintf("  Output:       %s\n", x$output$results_dir %||% "(not set)"))
  invisible(x)
}

# ==============================================================================
# Internal helpers
# ==============================================================================

.config_defaults <- function() {
  list(
    # Methodology declaration (T1.3 -- multi-mode architecture).
    # Mode is intentionally NULL in the bare schema so user-supplied configs
    # that omit it error clearly at validate_config(). default_config() (the
    # public starter) sets mode = "codebook_collaborative" for ergonomics.
    methodology = list(
      mode = NULL,                    # MUST be set: see .VALID_METHODOLOGY_MODES
      framework_spec_path = NULL,     # required when mode = "framework_applied"
      mode_locked_at = NULL,          # ISO timestamp set on first run
      parent_run_id = NULL,           # set if this run is linked to a prior re-declaration
      mode_changed_from = NULL        # set if methodology was changed mid-pipeline (audit)
    ),

    # Audit configuration (T1.4 / OS.5 -- raw-response capture for replay_run).
    # Defaults are conservative; users running in cost-sensitive environments
    # can disable raw response capture but lose replay_run() reproducibility.
    audit = list(
      capture_raw_responses = TRUE,
      response_cache_dir = "api_responses"
    ),

    # Memos configuration (M1.3 -- reflexive memos as data).
    # Mandatory in reflexive_scaffold mode; optional in other modes. NULL
    # means "derive from mode" (resolved by the orchestrator when the run
    # starts based on mandatory_for_modes).
    memos = list(
      enabled = NULL,
      mandatory_for_modes = c("reflexive_scaffold"),
      prompt_at = c("after_coding", "after_themes")
    ),

    study = list(
      name = "Untitled Study",
      research_focus = "",
      research_context = "",
      concepts = NULL,  # e.g., c("medication", "binge eating", "sleep")
      researcher_positionality = NULL,  # e.g., "Clinical psychologist with 10 years experience in eating disorders"
      research_paradigm = NULL,  # e.g., "critical realist", "social constructionist", "pragmatist"
      reflexive_notes = NULL  # Free-text researcher reflections on their approach and assumptions
    ),

    ai = list(
      provider = "openai",
      # Phase 50f: per-entry text character cap. NULL = auto (derive
      # from provider$context_window: ~40% of context, ~4 chars/token,
      # floored at 8000). Positive integer = explicit override.
      # Replaces the legacy hardcoded .MAX_ENTRY_CHARS = 8000L which
      # silently truncated long-form entries (interviews, multi-paragraph
      # posts) regardless of model context window.
      max_entry_chars = NULL,
      multi_model = list(
        enabled = FALSE,
        models = list()  # list of list(provider, model, api_key_env)
      ),
      openai = list(
        api_key_env = "OPENAI_API_KEY",
        models = list(
          primary = "gpt-4o",
          fast = "gpt-4o-mini",
          embedding = "text-embedding-3-small"
        ),
        rate_limits = list(
          requests_per_minute = 5000,
          tokens_per_minute = 800000,
          batch_size = 20,
          delay_between_batches = 0.5
        ),
        context_window = 128000
      ),
      anthropic = list(
        api_key_env = "ANTHROPIC_API_KEY",
        models = list(
          primary = "claude-sonnet-4-20250514",
          fast = "claude-sonnet-4-20250514"
        ),
        rate_limits = list(
          requests_per_minute = 1000,
          tokens_per_minute = 400000,
          batch_size = 10,
          delay_between_batches = 1.0
        ),
        context_window = 200000
      ),
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
      )
    ),

    data = list(
      source_type = "reddit",
      database = NULL,
      tables = NULL,
      custom_query = NULL,
      explicit_columns = NULL,  # e.g., list(text_column = "body", id_column = "post_id")
      preprocessing = list(
        remove_urls = TRUE,
        remove_mentions = TRUE,
        remove_hashtags = FALSE,
        lowercase = FALSE,
        min_text_length = 10,
        max_text_length = 10000,
        custom_cleaning_rules = list()  # list of list(pattern, replacement, description)
      ),
      column_mappings = list(
        reddit = list(
          # ORDER MATTERS: detect_columns picks the FIRST candidate that
          # exists. Reddit comments tables typically carry both
          # `comment_id` (the row's own id) AND `post_id` (the parent
          # post's id). If `post_id` came first, every comment under the
          # same parent would collapse onto a single std_id, silently
          # corrupting coding_state$entry_results. Phase 39 finding.
          id = c("comment_id", "post_id", "id"),
          text = c("text", "comment_body", "body", "selftext"),
          author = c("author", "username"),
          timestamp = c("created_utc", "created", "date"),
          metrics = c("score", "num_comments", "upvote_ratio")
        ),
        drugscom = list(
          id = c("id", "review_id"),
          text = c("review", "text", "comment"),
          author = c("username", "author"),
          timestamp = c("date", "created"),
          metrics = c("rating", "likes", "useful_count")
        ),
        generic = list(
          id = c("id"),
          text = c("text", "content", "body", "comment"),
          author = c("author", "user", "username"),
          timestamp = c("date", "timestamp", "created"),
          metrics = c("score", "rating", "likes")
        )
      )
    ),

    learning = list(
      # Default: disabled. The previous default (enabled = TRUE with
      # base_dir = NULL) was self-inconsistent -- validate_config rejects
      # any config where learning is enabled but base_dir is missing
      # (R/01_config.R:107-115), so calling default_config() and then
      # validate_config() on the result would have errored. Most users
      # also won't have prior coded studies sitting on disk; learning is
      # a power-user feature that should be opted into deliberately by
      # setting enabled: true and providing a base_dir.
      enabled = FALSE,
      base_dir = NULL,
      folder_pattern = "study$",
      manuscript_filenames = c("finalized themes", "manuscript", "analysis"),
      raw_data_subfolder = "raw data",
      max_manuscript_chars = 12000,
      max_raw_samples = 5
    ),

    analysis = list(
      dynamic_batching = TRUE,  # Enable token-aware batch sizing
      sentiment = list(
        include_confidence = TRUE,
        include_emotions = TRUE,
        emotion_categories = c("joy", "sadness", "anger", "fear",
                                "surprise", "disgust", "trust", "anticipation"),
        batch_size = 15
      ),
      coding = list(
        # Phase 50e: removed 5 dead config knobs that were declared but
        # never read by any R/ code path: min_code_frequency,
        # code_style, use_fallback_extraction, fallback_keyword_patterns,
        # clean_excerpts. Audit confirmed each had zero non-declaration
        # references in R/.
        include_in_vivo = TRUE,    # Live: gates "in vivo codes" prompt
                                    #   sentence at R/09_coding.R:1190
        max_retries_per_entry = 2,
        # Coding guideline parameters (NULL = use benchmarks from prior analyses, or package defaults)
        segment_length_min = NULL,   # Min coded segment length in characters (default: 30)
        segment_length_max = NULL,   # Max coded segment length in characters (default: 200)
        code_label_min_words = NULL,  # Min code label length in words (default: 3)
        code_label_max_words = NULL,  # Max code label length in words (default: 8)
        saturation_enabled = TRUE,
        saturation_window = 200L,
        saturation_threshold = 2L,
        saturation_confirmations = 3L,
        min_coded_before_saturation = 500L,
        ai_assessment_interval = 200L
      ),
      themes = list(
        # Phase 53 cleanup of Phase 52 audit deferral: removed five more
        # dead theme knobs (min_themes, max_themes, max_theme_proportion,
        # multi_label_assignment, ai_batch_size). Per C1 (AI decides when
        # to stop) the count + proportion knobs were never gating real
        # algorithm behavior -- they were display-only in the methodology
        # appendix and validation glue. multi_label_assignment had no
        # effect (cascade_theme_assignments always produces multi-label).
        # ai_batch_size was never read (theming is one AI call per HAC
        # node, not a batched task). Phase 50e removed the prior 8 dead
        # knobs (min_subthemes_per_theme, max_subthemes_per_theme,
        # review_iterations, auto_split_dominant, enrich_missing_fields,
        # strict_assignment_validation, max_rebalance_iterations,
        # membership_threshold).
        include_subthemes = TRUE,
        include_quotes = TRUE,
        quotes_per_theme = 3,
        approach = "inductive"
      ),
      correlations = list(
        method = "spearman",
        p_threshold = 0.05,
        adjust_method = "bonferroni",
        min_observations = 30,
        min_theme_entries = 5,
        use_multi_label = TRUE
      ),
      test_mode = list(
        enabled = FALSE,
        sample_size = 100L,
        seed = 42L
      ),
      human_verification = list(
        enabled = FALSE,
        sample_size = 20L,
        seed = 42L
      ),
      review_points = list(
        after_coding = FALSE,
        after_themes = FALSE,
        format = "csv",  # "csv" or "qdpx"
        max_iterations = 3L  # max recursive review cycles before forcing continue
      )
    ),

    output = list(
      results_dir = NULL,
      checkpoint_dir = NULL,
      generate_report = TRUE,
      generate_theme_details = TRUE,
      generate_correlation_plot = TRUE,
      export_csv = TRUE,
      export_json = TRUE,
      comparison_enabled = TRUE,
      comparison_similarity_threshold = 0.75,
      export_qdpx = TRUE  # Export QDPX file for QDA software interoperability
    ),

    logging = list(
      log_level = "INFO"  # DEBUG, INFO, WARN, ERROR
    ),

    scraping = list(
      enabled = FALSE,
      subreddits = NULL,
      posts_per_subreddit = 500,
      include_comments = TRUE,
      sort_by = "new",
      time_filter = "all",
      reddit_client_id = NULL,
      reddit_client_secret = NULL,
      # User-agent built dynamically from DESCRIPTION so default_config()'s
      # output stays consistent with the installed package version.
      # End users should override this with their actual Reddit username.
      reddit_user_agent = sprintf(
        "pakhom/%s (by u/YourRedditUsername)",
        tryCatch(as.character(utils::packageVersion("pakhom")),
                 error = function(e) "dev")
      )
    )
  )
}

#' Resolve relative file/directory paths relative to the config file location
#' @param config Config list
#' @param config_dir Absolute path to the directory containing config.yaml
#' @return Config with resolved absolute paths
#' @keywords internal
.resolve_paths <- function(config, config_dir) {
  resolve <- function(p) {
    if (is.null(p) || nchar(p) == 0) return(p)
    if (startsWith(p, "/") || startsWith(p, "~")) return(p)
    # On Windows, check for drive letter
    if (.Platform$OS.type == "windows" && grepl("^[A-Za-z]:", p)) return(p)
    normalizePath(file.path(config_dir, p), mustWork = FALSE)
  }

  if (!is.null(config$data$database))
    config$data$database <- resolve(config$data$database)
  if (!is.null(config$learning$base_dir))
    config$learning$base_dir <- resolve(config$learning$base_dir)
  if (!is.null(config$output$results_dir))
    config$output$results_dir <- resolve(config$output$results_dir)
  if (!is.null(config$output$checkpoint_dir))
    config$output$checkpoint_dir <- resolve(config$output$checkpoint_dir)

  config
}

#' Recursively merge user config with defaults (defaults fill gaps)
#' @keywords internal
.merge_defaults <- function(user, defaults) {
  for (key in names(defaults)) {
    if (is.null(user[[key]])) {
      user[[key]] <- defaults[[key]]
    } else if (is.list(defaults[[key]]) && is.list(user[[key]])) {
      user[[key]] <- .merge_defaults(user[[key]], defaults[[key]])
    }
  }
  user
}

#' Set a value at a nested path in a list
#' @keywords internal
.set_nested <- function(lst, path, value) {
  if (length(path) == 1) {
    lst[[path]] <- value
  } else {
    if (is.null(lst[[path[1]]])) lst[[path[1]]] <- list()
    lst[[path[1]]] <- .set_nested(lst[[path[1]]], path[-1], value)
  }
  lst
}

# ==============================================================================
# Config Generation Helpers
# ==============================================================================

#' Create a minimal configuration file
#'
#' Generates a valid YAML config with sensible defaults and writes it to disk.
#' Per T1.3 (phase 25-27) the \code{methodology} block is mandatory in
#' every config -- this helper writes it for you given the
#' \code{methodology} argument.
#'
#' @param methodology Methodology mode (mandatory): one of
#'   \code{"reflexive_scaffold"} (Mode 1), \code{"codebook_collaborative"}
#'   (Mode 2; the default), or \code{"framework_applied"} (Mode 3).
#'   Mode 3 also requires \code{framework_spec_path}.
#' @param study_name Study name (default \code{"Untitled Study"}).
#' @param research_focus Research focus string. Required when no
#'   \code{...} override supplies it.
#' @param framework_spec_path Path to a framework spec YAML/JSON OR a
#'   built-in alias (\code{"tpb"}, \code{"comb"}, \code{"tdf"}). Required
#'   when \code{methodology = "framework_applied"}; ignored otherwise.
#' @param database_path Path to the SQLite database (alias for the
#'   internal \code{data$database} field). Modes 2/3 require a database;
#'   Mode 1 may use a tibble passed directly to \code{run_mode1()} so
#'   the database can be NULL for Mode 1 configs.
#' @param output_path Where to save the YAML config file (default
#'   \code{"config.yaml"}). The output_path alias matches the
#'   README + vignette quickstart calls.
#' @param concepts Character vector of core research concepts
#'   (informs progressive coding prompts).
#' @param source_type Data source type: \code{"reddit"}, \code{"twitter"},
#'   \code{"generic"}, \code{"clinical"} (default \code{"generic"}).
#' @param output_dir Directory for analysis results (default
#'   \code{"outputs"}).
#' @param provider AI provider: \code{"openai"} or \code{"anthropic"}
#'   (default \code{"openai"}).
#' @param ... Additional overrides as dot-path = value pairs
#'   (e.g., \code{analysis.test_mode.enabled = TRUE}).
#' @return The path to the created config file (invisibly).
#' @examples
#' \dontrun{
#' # Mode 2 (Codebook Collaborative)
#' create_config(
#'   methodology = "codebook_collaborative",
#'   study_name = "My Study",
#'   research_focus = "How does X relate to Y?",
#'   database_path = "my_data.db",
#'   output_path = "config.yaml"
#' )
#'
#' # Mode 3 (Framework Applied) with a built-in framework
#' create_config(
#'   methodology = "framework_applied",
#'   framework_spec_path = "tpb",
#'   study_name = "TPB analysis",
#'   research_focus = "Behavioral intention -> behavior",
#'   database_path = "my_data.db",
#'   output_path = "config.yaml"
#' )
#'
#' # Mode 1 (Reflexive Scaffold) -- corpus is supplied at run_mode1() time
#' create_config(
#'   methodology = "reflexive_scaffold",
#'   study_name = "Reflexive analysis",
#'   research_focus = "Provocation against my coded themes",
#'   output_path = "config.yaml"
#' )
#' }
#' @export
create_config <- function(methodology = "codebook_collaborative",
                          study_name = "Untitled Study",
                          research_focus = NULL,
                          framework_spec_path = NULL,
                          database_path = NULL,
                          output_path = "config.yaml",
                          concepts = NULL,
                          source_type = "generic",
                          output_dir = "outputs",
                          provider = "openai",
                          ...) {
  # Phase 37 audit (CRITICAL #1): the previous signature had no
  # methodology arg, so create_config() produced configs that failed
  # validation immediately (T1.3 requires methodology$mode + structured
  # methodology block). The README + vignette Quick Start docs already
  # used kwargs that didn't exist (methodology, framework_spec_path,
  # database_path, output_path) -- a copy-paste of those docs hit
  # "Configuration validation failed" on every kwarg. Rewriting the
  # signature to accept those documented kwargs aligns the function
  # with the published docs AND writes a config that validate_config()
  # accepts on first try.
  validate_methodology_mode(methodology, allow_null = FALSE,
                              caller = "create_config")

  if (identical(methodology, "framework_applied") &&
      (is.null(framework_spec_path) || !nzchar(framework_spec_path))) {
    stop("create_config: methodology = 'framework_applied' (Mode 3) ",
         "requires framework_spec_path. Pass a built-in alias ",
         "('tpb', 'comb', 'tdf') or a path to a custom YAML/JSON spec.",
         call. = FALSE)
  }

  # research_focus is required for Modes 2/3 but optional for Mode 1
  # (the corpus + theme_set are passed at run_mode1() time, not config
  # time). Empty string is a default-to-fill-in placeholder.
  if (is.null(research_focus) || !nzchar(research_focus)) {
    if (identical(methodology, "reflexive_scaffold")) {
      research_focus <- ""
    } else {
      stop("create_config: research_focus is required for Modes 2 + 3. ",
           "(Mode 1 -- reflexive_scaffold -- accepts an empty focus ",
           "since the corpus + theme_set are passed at run_mode1() ",
           "time rather than config time.)",
           call. = FALSE)
    }
  }

  config <- list(
    methodology = list(
      mode = methodology,
      framework_spec_path = framework_spec_path,
      mode_locked_at = NULL,
      parent_run_id = NULL,
      mode_changed_from = NULL
    ),
    study = list(
      name = study_name,
      research_focus = research_focus,
      research_context = "",
      concepts = as.list(concepts)
    ),
    ai = list(
      provider = provider
    ),
    data = list(
      source_type = source_type,
      database = database_path
    ),
    analysis = list(
      themes = list(approach = "inductive")
    ),
    output = list(
      results_dir = output_dir,
      generate_report = TRUE
    )
  )

  # Apply ... overrides
  overrides <- list(...)
  for (key in names(overrides)) {
    parts <- strsplit(key, "\\.")[[1]]
    config <- .set_nested(config, parts, overrides[[key]])
  }

  header <- paste0(
    "# =============================================================================\n",
    "# pakhom Configuration -- ", study_name, "\n",
    "# Methodology mode: ", methodology, "\n",
    "# =============================================================================\n",
    "# Generated by create_config(). Edit as needed.\n",
    "# Full documentation: see ?load_config + vignette('methodology-modes').\n",
    "# =============================================================================\n\n"
  )

  yaml_text <- yaml::as.yaml(config, indent = 2, indent.mapping.sequence = TRUE)
  writeLines(paste0(header, yaml_text), output_path)

  log_info("Config written to: {output_path}")
  invisible(output_path)
}

#' Interactive configuration wizard
#'
#' Guides the user through creating a config.yaml via CLI prompts.
#' Only works in interactive mode.
#'
#' @param output_path Where to save the config (default "config.yaml")
#' @return The path to the created config file (invisibly)
#' @export
config_wizard <- function(output_path = "config.yaml") {
  if (!interactive()) {
    stop("config_wizard() requires an interactive R session")
  }

  cat("pakhom Configuration Wizard\n")
  cat("================================\n")
  cat("This wizard will help you create a config.yaml file.\n\n")

  study_name <- readline("Study name: ")
  if (nchar(study_name) == 0) study_name <- "Untitled Study"

  research_focus <- readline("Research focus (required): ")
  if (nchar(research_focus) == 0) stop("Research focus is required")

  concepts_raw <- readline("Core concepts (comma-separated, or blank): ")
  concepts <- if (nchar(concepts_raw) > 0) {
    trimws(strsplit(concepts_raw, ",")[[1]])
  } else {
    NULL
  }

  data_path <- readline("Path to database file (.db or .csv): ")
  if (nchar(data_path) == 0) data_path <- NULL

  source_type <- readline("Data source type [reddit/twitter/generic/clinical] (default: generic): ")
  if (nchar(source_type) == 0) source_type <- "generic"

  provider <- readline("AI provider [openai/anthropic] (default: openai): ")
  if (nchar(provider) == 0) provider <- "openai"

  output_dir <- readline("Output directory (default: outputs): ")
  if (nchar(output_dir) == 0) output_dir <- "outputs"

  positionality <- readline("Researcher positionality statement (optional, Enter to skip): ")

  overrides <- list()
  if (nchar(positionality) > 0) {
    overrides[["study.researcher_positionality"]] <- positionality
  }

  do.call(create_config, c(list(
    study_name = study_name,
    research_focus = research_focus,
    concepts = concepts,
    data_path = data_path,
    source_type = source_type,
    output_dir = output_dir,
    provider = provider,
    config_path = output_path
  ), overrides))

  cat("\nConfig saved to: ", output_path, "\n")
  cat("Next steps:\n")
  cat("  1. Add your API key to the config file\n")
  cat("  2. Review and adjust parameters as needed\n")
  cat("  3. Run: pakhom::run_analysis('", output_path, "')\n\n", sep = "")

  invisible(output_path)
}
