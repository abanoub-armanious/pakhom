# ==============================================================================
# Configuration System — YAML-based with validation and defaults
# ==============================================================================

#' Load analysis configuration from YAML file
#'
#' @param config_path Path to YAML config file.
#' @param overrides Named list of overrides applied after the YAML
#'   is parsed. Two equivalent styles are supported and may be mixed:
#'   \itemize{
#'     \item \strong{Dot-path keys} --
#'       \code{list("ai.provider" = "anthropic",
#'       "study.research_focus" = "x")}
#'     \item \strong{Nested named lists} --
#'       \code{list(ai = list(provider = "anthropic"),
#'       study = list(research_focus = "x"))}
#'   }
#'   Both styles deep-merge: sibling fields under the same parent key
#'   are preserved, not clobbered. (Previously the nested style
#'   silently replaced the entire parent block -- e.g., passing
#'   \code{list(study = list(researcher_positionality = "..."))} would
#'   drop \code{study$research_focus} and surface as the misleading
#'   "study.research_focus is required" validation error.)
#'
#'   Leaves are everything that is not a non-empty named list:
#'   atomic vectors (\code{c("a", "b")}), NULL, empty \code{list()},
#'   unnamed/positional lists (e.g.,
#'   \code{custom_cleaning_rules = list(list(pattern = ...))}), and
#'   data.frames. Use a positional list deliberately when you mean
#'   "replace this whole block".
#' @return A validated ThematicConfig S3 object
#' @export
load_config <- function(config_path, overrides = list()) {
  if (!file.exists(config_path)) {
    stop("Config file not found: ", config_path)
  }

  config <- yaml::read_yaml(config_path)

  # Apply overrides. Accept both dot-path keys
  # (list("study.research_focus" = "x")) AND nested named lists
  # (list(study = list(research_focus = "x"))) -- the nested style was
  # a silent foot-gun in an earlier version because the loop
  # walked names() and used .set_nested at the top key, which
  # clobbered the entire study block. .flatten_overrides() recurses
  # into named lists and emits dot-paths so both styles deep-merge.
  flat_overrides <- .flatten_overrides(overrides)
  for (key in names(flat_overrides)) {
    parts <- strsplit(key, "\\.")[[1]]
    config <- .set_nested(config, parts, flat_overrides[[key]])
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
#' Calling \code{default_config()} with no argument is an ERROR: the
#' \code{methodology} argument is mandatory (AC3 -- no default mode; explicit
#' declaration mandatory). There is deliberately no fallback default. Spool
#' 2011 (>95\% of users never change defaults) is precisely why: a silent
#' default would let users inherit a methodology without conscious choice, so
#' pakhom requires the mode to be declared explicitly rather than supplying one
#' for them. Run \code{\link{methodology_decision_aid}} for guidance on choosing.
#'
#' Note: \code{.config_defaults()} (internal) returns the bare schema with
#' \code{methodology$mode = NULL}, so user-supplied YAMLs that omit the
#' methodology section fail validation with a clear error rather than
#' silently inheriting a default. \code{default_config()} is the only
#' entry point that pre-fills mode (when methodology is supplied explicitly).
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
  # The previous behavior here violated AC3
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
  config$methodology$mode_locked_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  structure(config, class = "ThematicConfig")
}

#' Validate configuration completeness and correctness
#' @param config A ThematicConfig object
#' @return The config invisibly, or stops with informative errors
validate_config <- function(config) {
  errors <- character(0)

  # Required top-level sections (methodology added per T1.3)
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

  # warn about deprecated knobs the user still
  # carries in their personal config.yaml. A user's project-root
  # config.yaml was found to carry four
  # surviving knobs from the earlier sequential-merge algorithm
  # in the user's project-root config.yaml. The package no longer
  # reads them but they confuse anyone who edits the file later.
  .warn_deprecated_config_knobs(config)

  # warn about empty reflexivity scaffold.
  # Olmos-Vega AMEE Guide 149 recommends positionality + paradigm +
  # reflexive notes be present in every turn of qualitative analysis.
  # A run was observed where all three were NULL ->
  # silent reflexivity gap in the AI prompts. Warn (not error) so
  # users on small / exploratory runs aren't blocked, but the
  # methodology paper can no longer claim reflexive practice.
  .warn_empty_reflexivity(config)

  if (length(errors) > 0) {
    stop("Configuration validation failed:\n  - ", paste(errors, collapse = "\n  - "))
  }

  invisible(config)
}

#' Warn about deprecated config knobs
#'
#' Walks the user's config for keys that the package no longer reads
#' (18 knobs total: the sequential-merge theme knobs, the theme-count
#' knobs, the theme-membership knobs, and the pre-arbiter saturation
#' knobs -- all removed before the 1.0.0 release). Each surviving stale
#' knob in a user's personal config.yaml is logged as a warning so it
#' can be cleaned up.
#'
#' @keywords internal
.warn_deprecated_config_knobs <- function(config) {
  deprecated <- list(
    # Sequential-merge theme algorithm knobs (the merge step was removed):
    list(path = c("analysis", "themes", "merge_strategy"),
         reason = "AI-judged clustering replaced the sequential merge."),
    list(path = c("analysis", "themes", "max_merge_passes"),
         reason = "Same as merge_strategy: no merge passes in the current algorithm."),
    list(path = c("analysis", "themes", "stopping_criterion"),
         reason = "The AI decides cluster structure (C1: no count thresholds)."),
    list(path = c("analysis", "themes", "min_merges_to_continue"),
         reason = "No merge-pass loop to gate."),
    # Theme-count knobs (C1: the AI decides how many themes emerge):
    list(path = c("analysis", "themes", "min_themes"),
         reason = "C1: no count thresholds."),
    list(path = c("analysis", "themes", "max_themes"),
         reason = "C1: no count thresholds."),
    list(path = c("analysis", "themes", "max_theme_proportion"),
         reason = "C1: no count thresholds."),
    list(path = c("analysis", "themes", "multi_label_assignment"),
         reason = "Cascade always produces multi-label assignments."),
    list(path = c("analysis", "themes", "ai_batch_size"),
         reason = "Theming is AI-judged per cluster, not batched."),
    # Theme-membership knobs (the cascade is deterministic):
    list(path = c("analysis", "themes", "membership_threshold"),
         reason = "Cascade is deterministic; no membership-score gate."),
    list(path = c("analysis", "themes", "max_rebalance_iterations"),
         reason = "No rebalance loop; the clustering algorithm converges on its own."),
    list(path = c("analysis", "themes", "review_iterations"),
         reason = "Researcher review is pause-based, not a fixed-iteration loop (`review_points.max_iterations` is the live knob)."),
    # Pre-arbiter saturation knobs (replaced by the AI saturation arbiter):
    list(path = c("analysis", "coding", "saturation_enabled"),
         reason = "Replaced by AI saturation arbiter (no user knob)."),
    list(path = c("analysis", "coding", "saturation_window"),
         reason = "AI arbiter judges saturation; no window threshold."),
    list(path = c("analysis", "coding", "saturation_threshold"),
         reason = "AI arbiter judges saturation; no count threshold."),
    list(path = c("analysis", "coding", "saturation_confirmations"),
         reason = "AI arbiter judges saturation; no confirmation count."),
    list(path = c("analysis", "coding", "min_coded_before_saturation"),
         reason = "AI arbiter has its own cadence (.saturation_cadence)."),
    list(path = c("analysis", "coding", "ai_assessment_interval"),
         reason = "AI arbiter has its own cadence (.saturation_cadence).")
  )

  # Helper: walk path through a nested list, returning TRUE if every
  # element exists (final value may be NULL).
  has_path <- function(cfg, path) {
    cur <- cfg
    for (p in path) {
      if (!is.list(cur)) return(FALSE)
      if (!(p %in% names(cur))) return(FALSE)
      cur <- cur[[p]]
    }
    TRUE
  }

  stale_found <- character(0)
  for (d in deprecated) {
    if (has_path(config, d$path)) {
      stale_found <- c(stale_found, paste0(
        paste(d$path, collapse = "."), " (no longer used; ", d$reason, ")"
      ))
    }
  }

  if (length(stale_found) > 0L) {
    log_warn(paste0(
      "Found ", length(stale_found), " deprecated knob(s) in your ",
      "config.yaml that pakhom no longer reads. These are safe to ",
      "remove from your file."
    ))
    for (s in stale_found) log_warn(paste0("  - ", s))
  }

  # The legacy v1 HAC theme algorithm was removed (v2 multi-pass clustering is
  # the only engine now). Two knobs only the v1 walker ever read are therefore
  # inert; warn if a user has explicitly set one so the wasted effort is
  # visible. The check fires only when the knob differs from its default, so a
  # freshly-loaded default config stays quiet.
  v1_only <- list(
    list(path = c("analysis", "themes", "max_subtheme_depth"),
         default = 3L,
         reason = "subtheme depth is derived from the v2 multi-pass clustering tree, not a fixed recursion limit"),
    list(path = c("analysis", "themes", "max_codes_per_subtheme"),
         default = 25L,
         reason = "subtheme membership is determined by the v2 penultimate-pass clusters")
  )
  inert_found <- character(0)
  for (k in v1_only) {
    if (!has_path(config, k$path)) next
    val <- config
    for (p in k$path) val <- val[[p]]
    if (!isTRUE(all.equal(val, k$default))) {
      inert_found <- c(inert_found, paste0(
        paste(k$path, collapse = "."), " = ", as.character(val),
        " (no effect; the v1 algorithm was removed -- ", k$reason, ")"
      ))
    }
  }
  if (length(inert_found) > 0L) {
    log_warn(paste0(
      "Found ", length(inert_found), " setting(s) in your config that no ",
      "longer have any effect (the legacy v1 theme algorithm was removed); ",
      "you can safely remove them:"
    ))
    for (s in inert_found) log_warn(paste0("  - ", s))
  }

  invisible(stale_found)
}

#' Warn when the reflexivity scaffold is empty
#'
#' Olmos-Vega et al. (AMEE Guide 149) recommend positionality +
#' paradigm + reflexive notes be present in every turn of qualitative
#' analysis. pakhom injects \code{study.researcher_positionality},
#' \code{study.research_paradigm}, and \code{study.reflexive_notes}
#' into the AI system prompt at every coding / theming / saturation
#' call (see R/methodology_rules.R). When all three are empty,
#' \code{.reflexivity_block_for()} returns the empty string and
#' the AI prompt CONTAINS NO REFLEXIVITY BLOCK AT ALL -- a
#' methodology paper relying on this run cannot honestly claim
#' reflexive practice.
#'
#' @keywords internal
.warn_empty_reflexivity <- function(config) {
  is_empty <- function(x) {
    is.null(x) ||
      (length(x) == 1L && (is.na(x) || !nzchar(trimws(as.character(x)[1]))))
  }
  fields <- list(
    positionality = config$study$researcher_positionality,
    paradigm      = config$study$research_paradigm,
    reflexive_notes = config$study$reflexive_notes
  )
  empties <- vapply(fields, is_empty, logical(1))
  if (all(empties)) {
    log_warn(paste0(
      "Reflexivity scaffold is EMPTY: study.researcher_positionality, ",
      "study.research_paradigm, and study.reflexive_notes are all unset. ",
      "pakhom will OMIT the reflexivity block from the AI prompt entirely ",
      "(no placeholder text is substituted). A methodology paper relying ",
      "on this run cannot honestly claim reflexive practice (Olmos-Vega ",
      "AMEE Guide 149). Set at least one of these fields in your ",
      "config.yaml before a publication run."
    ))
  } else if (any(empties)) {
    missing_names <- names(empties)[empties]
    log_info(paste0(
      "Reflexivity scaffold partially populated. Missing: ",
      paste(paste0("study.", missing_names), collapse = ", "),
      ". Pipeline continues; consider filling these before a ",
      "publication run."
    ))
  }
  invisible(empties)
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

    # Audit configuration (T1.4 -- raw-response capture for replay_run).
    # Defaults are conservative; users running in cost-sensitive environments
    # can disable raw response capture but forfeit the cached responses the
    # planned replay_run() will need.
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
      concepts = NULL,  # e.g., c("remote work", "wellbeing", "work-life balance")
      researcher_positionality = NULL,  # e.g., "Organizational psychologist studying remote-work practices"
      research_paradigm = NULL,  # e.g., "critical realist", "social constructionist", "pragmatist"
      reflexive_notes = NULL,  # Free-text researcher reflections on their approach and assumptions
      # replay-pin for the Methodology Assistant (Step 2.5). NULL =
      # the AI articulates the relevance criterion + per-metric interpretations
      # live; a non-NULL block (copied from a prior run's
      # methodology_articulations.json) skips those Step-2.5 AI calls and
      # re-applies the pinned methodology decisions deterministically (the rest
      # of the run -- coding, sentiment, synthesis -- still queries the model).
      inferred_methodology = NULL
    ),

    ai = list(
      provider = "openai",
      # per-entry text character cap. NULL = auto (derive
      # from provider$context_window: ~40% of context, ~4 chars/token,
      # floored at 8000). Positive integer = explicit override.
      # Replaces the legacy hardcoded .MAX_ENTRY_CHARS = 8000L which
      # silently truncated long-form entries (interviews, multi-paragraph
      # posts) regardless of model context window.
      max_entry_chars = NULL,
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
      # default source_type changed from "reddit"
      # to "generic" per C4 (dataset-agnostic). Previously the runtime
      # fallback assumed Reddit shape, which silently mis-detected column
      # mappings on non-Reddit corpora when users forgot to set
      # source_type. The shipped YAML template at
      # inst/config/default_config.yaml mirrors this change; users with
      # actual Reddit data should set source_type: "reddit" explicitly.
      source_type = "generic",
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
          # corrupting coding_state$entry_results.
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
          # generic source-type has no known schema; metric detection
          # falls through to .detect_metric_columns() auto-detect
          # (any numeric column that isn't internal or theme_membership_*).
          # Per-source-type metric hints belong only where the source has
          # a known schema (reddit, drugscom).
          metrics = character(0)
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
        # Removed 5 dead config knobs that were declared but
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
        # description-refresh pass cadence + filter.
        # Every `description_refresh_interval` new codes admitted, the
        # AI re-describes every code with `frequency >=
        # description_refresh_min_freq` using
        # `description_refresh_sample_segments` of its coded_segments.
        # Mode 3 (framework) skips the refresh entirely.
        description_refresh_interval        = 100L,
        description_refresh_min_freq        = 50L,
        description_refresh_sample_segments = 5L
        # removed the six earlier saturation knobs
        # (saturation_enabled, saturation_window, saturation_threshold,
        # saturation_confirmations, min_coded_before_saturation,
        # ai_assessment_interval). Per C1 ("AI decides when to stop"),
        # the AI saturation arbiter in R/saturation_arbiter.R is the
        # sole decision; cadence is auto-scaled by corpus size via
        # .saturation_cadence(). No user-facing knobs.
      ),
      themes = list(
        # algorithm selector. "v2" (the default) uses the
        # multi-pass clustering + label-after-clustering implementation
        # in R/theme_algorithm_v2.R. The earlier "v1" HAC + tree-walk
        # algorithm has been removed (it produced 87-92% single-code
        # themes at scale); pinning algorithm = "v1" now falls back to
        # v2 with a deprecation warning. NEW USERS: leave as "v2".
        algorithm = "v2",
        # Five dead theme knobs were removed (min_themes, max_themes,
        # max_theme_proportion, multi_label_assignment, ai_batch_size).
        # Per C1 (AI decides when to stop) the count + proportion knobs
        # were never gating real algorithm behavior -- they were
        # display-only in the methodology appendix and validation glue.
        # multi_label_assignment had no effect (cascade_theme_assignments
        # always produces multi-label). ai_batch_size was never read
        # (theming is one AI call per cluster, not a batched task). An
        # earlier cleanup removed the prior 8 dead
        # knobs (min_subthemes_per_theme, max_subthemes_per_theme,
        # review_iterations, auto_split_dominant, enrich_missing_fields,
        # strict_assignment_validation, max_rebalance_iterations,
        # membership_threshold).
        include_subthemes = TRUE,
        include_quotes = TRUE,
        quotes_per_theme = 3,
        approach = "inductive",
        # Legacy subtheme-nesting depth knob (read only by the removed
        # v1 HAC walker). Under v2 subtheme depth is derived from the
        # multi-pass clustering tree, so this knob is inert; it is kept
        # at its default so existing configs stay quiet.
        max_subtheme_depth     = 3L,
        # Legacy companion to max_subtheme_depth (also read only by the
        # removed v1 walker); inert under v2 and kept at its default.
        max_codes_per_subtheme = 25L,
        # cap on the number of themes rendered as
        # full inline cards in the main HTML report. Themes ranked
        # beyond this cap render as compact one-line rows linking to
        # per-theme detail HTMLs. Without this cap a >400-theme run
        # produces a 12+ MB Rmd that pandoc cannot render (OOM). The
        # per-theme detail HTMLs are unaffected -- every theme retains
        # full provenance + entry data on its own page. Set very high
        # (e.g. 10000L) to disable on small corpora.
        max_inline_themes = 30L,
        # cap on themes shown on the
        # temporal_emergence.png lollipop chart. An earlier version
        # rendered EVERY theme (a large run produced 4,059 codes /
        # 417 themes -> a 2.8 MB vertical wall of text). The filter
        # selects the top-N themes by cumulative entry count; the
        # n_entries column on emergence_timeline enables this ranking.
        max_inline_themes_temporal = 30L
      ),
      correlations = list(
        method = "spearman",
        p_threshold = 0.05,
        adjust_method = "bonferroni",
        min_observations = 30,
        min_theme_entries = 5,
        use_multi_label = TRUE,
        # variable-count threshold above which
        # correlation_plot.png renders as a top-N effect-size lollipop
        # (ranking pairs by |r|) instead of a full corrplot heatmap.
        # An earlier unconditional heatmap scaled to 14,280x14,280
        # pixels on a 228-variable run -- 4.8 MB and browser-illegible.
        # Lollipop stays publication-readable regardless of variable
        # count.
        max_inline_vars = 30L,
        # node-count threshold above which
        # theme_network.png is filtered to the top-N most-connected
        # themes before plotting. An earlier plot rendered 417
        # themes as an unreadable hairball with no legend; filtering
        # to top-30 + adding a legend restores publication quality.
        max_inline_themes_network = 30L
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

#' Flatten a (possibly nested) overrides list into dot-path key/value pairs
#'
#' Both styles of override are supported:
#' \itemize{
#'   \item Dot-path keys: \code{list("study.research_focus" = "x")}
#'   \item Nested named lists: \code{list(study = list(research_focus = "x"))}
#' }
#'
#' Without this helper, the override loop in \code{\link{load_config}}
#' treated a nested list as an atomic leaf, so passing
#' \code{list(study = list(researcher_positionality = "..."))} silently
#' clobbered the entire \code{study} block -- dropping
#' \code{research_focus} (and every other field) and surfacing as the
#' misleading error "study.research_focus is required". This was
#' caught during the Mode 1 smoke test.
#'
#' Recursion rule: a value is recursed into when it is a non-empty
#' named list (\code{is.list} TRUE, \code{length > 0}, has
#' \code{names()}, and not a data.frame). Atomic vectors, NULL, empty
#' lists, unnamed/positional lists (e.g.,
#' \code{custom_cleaning_rules = list(list(pattern = ...))}), and
#' data.frames are treated as leaves so a config value that legitimately
#' is a list is not mis-walked.
#' Mixed-named lists (some entries with empty names) are recursed into
#' so the recursive top-of-function name check can fire an error with
#' the full dot-path prefix for localization.
#'
#' Duplicate flattened keys can arise when a user targets the same
#' dot-path via both styles in a single call (e.g.,
#' \code{list("ai.provider" = "openai", ai = list(provider = "anthropic"))}
#' would emit two \code{ai.provider} entries). The helper deduplicates
#' before returning, keeping the LAST occurrence -- this matches the
#' intuitive "what I wrote later overrides what I wrote earlier"
#' semantic. (Plain \code{list[[key]]} access in R returns the FIRST
#' match for duplicate names, which would produce the opposite -- and
#' surprising -- behavior if dedup were skipped.)
#'
#' @param overrides A named list of overrides (possibly nested).
#' @param prefix Internal recursion accumulator -- the dot-path built
#'   so far. Leave as \code{NULL} at top level.
#' @return A flat named list whose names are dot-paths and whose values
#'   are leaves.
#' @keywords internal
.flatten_overrides <- function(overrides, prefix = NULL) {
  if (length(overrides) == 0L) return(list())

  nms <- names(overrides)
  # NA names sneak through nzchar() (treated as TRUE under the default
  # keepNA setting), and `list[[NA]]` returns NULL -- which would
  # silently drop the user's value. Reject NA names explicitly.
  if (is.null(nms) || any(is.na(nms)) || any(!nzchar(nms))) {
    stop(
      ".flatten_overrides: every override entry must be named. ",
      "Got entries with empty, missing, or NA names at ",
      if (is.null(prefix)) "top level" else sprintf("prefix '%s'", prefix),
      ".",
      call. = FALSE
    )
  }

  flat <- list()
  for (key in nms) {
    value <- overrides[[key]]
    full_key <- if (is.null(prefix)) key else paste0(prefix, ".", key)

    # Recurse only into non-empty named lists (data.frames excluded
    # because they have names() = column names and is.list() TRUE but
    # walking them as a config tree would be wrong). Lists with
    # any names get recursed -- if the names are mixed (some empty),
    # the recursive call's top-of-function check fires the error with
    # the full prefix so the user can localize the typo.
    is_recurseable_block <-
      is.list(value) &&
      length(value) > 0L &&
      !is.data.frame(value) &&
      !is.null(names(value))

    if (is_recurseable_block) {
      flat <- c(flat, .flatten_overrides(value, prefix = full_key))
    } else {
      # Bracket-single + list(.) idiom preserves NULL leaves, which
      # the [[<-]] idiom would silently drop. Atomic vectors, empty
      # lists, positional (unnamed) lists, and data.frames also flow
      # through here as leaves.
      flat[full_key] <- list(value)
    }
  }

  # Deduplicate: when a user mixes both override styles for the same
  # dot-path (e.g., "ai.provider" = "x" AND ai = list(provider = "y")),
  # both entries land in `flat` with identical names. The downstream
  # loops in load_config / create_config read values with [[key]],
  # which returns the FIRST match for duplicate names -- the opposite
  # of the "last write wins" semantic users expect when overriding.
  # Keep the last occurrence so later writes dominate.
  if (length(flat) > 0L) {
    flat <- flat[!duplicated(names(flat), fromLast = TRUE)]
  }

  flat
}

# ==============================================================================
# Config Generation Helpers
# ==============================================================================

#' Create a minimal configuration file
#'
#' Generates a valid YAML config with sensible defaults and writes it to disk.
#' Per T1.3 the \code{methodology} block is mandatory in
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
  # The previous signature had no
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

  # Apply ... overrides. Accepts both dot-path and nested-list styles
  # (see .flatten_overrides + load_config notes).
  overrides <- list(...)
  flat_overrides <- .flatten_overrides(overrides)
  for (key in names(flat_overrides)) {
    parts <- strsplit(key, "\\.")[[1]]
    config <- .set_nested(config, parts, flat_overrides[[key]])
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

#' Map a CLI methodology choice to a canonical mode name
#'
#' Accepts a menu number ("1"/"2"/"3"), a canonical mode name, or blank
#' (Enter = default). Returns the canonical mode string, or \code{NULL} for
#' an unrecognized choice so the caller can raise an actionable error.
#'
#' @param raw The raw user input (a single string from \code{readline()}).
#' @param default Mode returned when \code{raw} is blank.
#' @keywords internal
.parse_methodology_choice <- function(raw, default = .METHODOLOGY_MODE_CODEBOOK_COLLABORATIVE) {
  raw <- trimws(raw)
  if (nchar(raw) == 0) return(default)  # Enter = default (Mode 2)
  switch(raw,
    "1" = .METHODOLOGY_MODE_REFLEXIVE_SCAFFOLD,
    "2" = .METHODOLOGY_MODE_CODEBOOK_COLLABORATIVE,
    "3" = .METHODOLOGY_MODE_FRAMEWORK_APPLIED,
    reflexive_scaffold = .METHODOLOGY_MODE_REFLEXIVE_SCAFFOLD,
    codebook_collaborative = .METHODOLOGY_MODE_CODEBOOK_COLLABORATIVE,
    framework_applied = .METHODOLOGY_MODE_FRAMEWORK_APPLIED,
    NULL
  )
}

#' Interactive configuration wizard
#'
#' Guides the user through creating a config.yaml via CLI prompts. The first
#' prompt is the (mandatory) methodology mode -- reflexive_scaffold,
#' codebook_collaborative, or framework_applied -- so the generated config
#' passes [validate_config()]. This is the text-based companion to the
#' web-based [config_wizard_app()]. Only works in interactive mode.
#'
#' @param output_path Where to save the config (default "config.yaml")
#' @return The path to the created config file (invisibly)
#' @seealso [config_wizard_app()] for the web-based wizard;
#'   [methodology_decision_aid()] for help choosing a methodology mode.
#' @export
config_wizard <- function(output_path = "config.yaml") {
  if (!interactive()) {
    stop("config_wizard() requires an interactive R session")
  }

  cat("pakhom Configuration Wizard\n")
  cat("================================\n")
  cat("This wizard will help you create a config.yaml file.\n\n")

  # Methodology mode -- the load-bearing architectural choice (T1.3). It is
  # mandatory: a config without it fails validate_config(). Prompt for it
  # first, mirroring the Shiny wizard's Methodology step.
  cat("Methodology mode -- determines AI behaviors, mandatory artifacts, and\n")
  cat("report sections. Run methodology_decision_aid() if you are unsure.\n")
  cat("  1. reflexive_scaffold      (Mode 1) inductive reflexive TA; AI as provocateur\n")
  cat("  2. codebook_collaborative  (Mode 2) AI proposes codes, researcher gates; shared codebook\n")
  cat("  3. framework_applied       (Mode 3) deductive coding against a framework you supply\n")
  methodology_raw <- readline("Choose [1/2/3 or name] (default: 2): ")
  methodology <- .parse_methodology_choice(methodology_raw)
  if (is.null(methodology)) {
    stop("Invalid methodology choice: '", trimws(methodology_raw), "'. Choose ",
         "1/2/3 or one of: reflexive_scaffold, codebook_collaborative, ",
         "framework_applied.", call. = FALSE)
  }

  framework_spec_path <- NULL
  if (identical(methodology, "framework_applied")) {
    framework_spec_path <- trimws(readline(
      "Framework spec (built-in alias tpb/comb/tdf or path to YAML/JSON): "))
    if (nchar(framework_spec_path) == 0) {
      stop("framework_applied (Mode 3) requires a framework spec (alias or path).",
           call. = FALSE)
    }
  }

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

  # NB: pass create_config()'s real parameter names. Earlier this used
  # `data_path =` / `config_path =`, which are NOT create_config() params --
  # they fell into `...`, were flattened to bogus top-level keys, and the
  # database path + output path were silently lost. Use database_path= and
  # output_path=, and pass the methodology declaration captured above.
  wizard_args <- list(
    methodology = methodology,
    study_name = study_name,
    research_focus = research_focus,
    concepts = concepts,
    database_path = data_path,
    source_type = source_type,
    output_dir = output_dir,
    provider = provider,
    output_path = output_path
  )
  if (!is.null(framework_spec_path)) {
    wizard_args$framework_spec_path <- framework_spec_path
  }
  do.call(create_config, c(wizard_args, overrides))

  cat("\nConfig saved to: ", output_path, "\n")
  cat("Next steps:\n")
  cat("  1. Add your API key to the config file\n")
  cat("  2. Review and adjust parameters as needed\n")
  cat("  3. Run: pakhom::run_analysis('", output_path, "')\n\n", sep = "")

  invisible(output_path)
}
