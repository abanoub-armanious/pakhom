# ==============================================================================
# Shiny Config Wizard — interactive web-based configuration builder
# ==============================================================================

#' Launch the interactive configuration wizard
#'
#' Opens a Shiny app that walks you through every configuration option
#' with descriptions, validation, and sensible defaults. When finished,
#' it writes a validated `config.yaml` to disk.
#'
#' This is the web-based companion to the CLI-based [config_wizard()].
#' Both produce identical YAML output.
#'
#' @param output_path Where to save the generated config (default "config.yaml").
#'   The user can also change this in the UI.
#' @return The path to the created config file (invisibly). Returns NULL if the
#'   user closes the app without saving.
#' @export
config_wizard_app <- function(output_path = "config.yaml") {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is required for config_wizard_app().\n",
         "Install it with: install.packages('shiny')")
  }

  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style(shiny::HTML("
      body { background: #f8f9fa; }
      .main-container { max-width: 800px; margin: 0 auto; padding: 20px; }
      .section-card {
        background: white; border-radius: 8px; padding: 24px;
        margin-bottom: 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.1);
      }
      .section-title { color: #2c3e50; margin-top: 0; border-bottom: 2px solid #3498db; padding-bottom: 8px; }
      .help-text { color: #6c757d; font-size: 0.9em; margin-top: 4px; }
      .required-star { color: #e74c3c; }
      .btn-generate { font-size: 1.1em; padding: 12px 32px; }
      .yaml-preview { font-family: 'Fira Code', 'Consolas', monospace; font-size: 0.85em; }
      .step-indicator { display: flex; justify-content: center; margin-bottom: 20px; }
      .step { padding: 8px 16px; margin: 0 4px; border-radius: 20px; background: #e9ecef; color: #6c757d; font-size: 0.85em; }
      .step.active { background: #3498db; color: white; font-weight: bold; }
      .step.done { background: #27ae60; color: white; }
      .nav-buttons { display: flex; justify-content: space-between; margin-top: 16px; }
    "))),

    shiny::div(class = "main-container",
      shiny::h2("pakhom Configuration Wizard",
                 style = "text-align: center; color: #2c3e50; margin-bottom: 4px;"),
      shiny::p("Build your config.yaml step by step",
               style = "text-align: center; color: #7f8c8d; margin-bottom: 4px;"),
      shiny::p(shiny::HTML("By <a href='https://www.linkedin.com/in/abanoubarmanious/' target='_blank' style='color: #95a5a6; text-decoration: underline;'>Abanoub J. Armanious, MS</a>"),
               style = "text-align: center; color: #95a5a6; font-size: 0.85em; margin-bottom: 20px;"),

      # Step indicators
      shiny::uiOutput("step_indicator"),

      # Dynamic step content
      shiny::uiOutput("step_content"),

      # Navigation buttons
      shiny::div(class = "nav-buttons",
        shiny::uiOutput("nav_back"),
        shiny::uiOutput("nav_next")
      )
    )
  )

  server <- function(input, output, session) {
    step <- shiny::reactiveVal(1L)
    total_steps <- 7L
    step_labels <- c("Study", "AI Provider", "Data", "Learning",
                     "Analysis", "Output", "Review & Save")
    saved_path <- shiny::reactiveVal(NULL)

    # --- Step indicator ---
    output$step_indicator <- shiny::renderUI({
      current <- step()
      tags <- lapply(seq_len(total_steps), function(i) {
        cls <- if (i < current) "step done" else if (i == current) "step active" else "step"
        shiny::span(class = cls, step_labels[i])
      })
      shiny::div(class = "step-indicator", tags)
    })

    # --- Navigation ---
    output$nav_back <- shiny::renderUI({
      if (step() > 1L) {
        shiny::actionButton("btn_back", "Back", class = "btn btn-outline-secondary")
      }
    })
    output$nav_next <- shiny::renderUI({
      if (step() < total_steps) {
        shiny::actionButton("btn_next", "Next", class = "btn btn-primary")
      } else {
        shiny::actionButton("btn_save", "Save Config", class = "btn btn-success btn-generate")
      }
    })

    shiny::observeEvent(input$btn_back, { step(max(1L, step() - 1L)) })
    shiny::observeEvent(input$btn_next, {
      # Validate required fields on step 1
      if (step() == 1L) {
        if (is.null(input$research_focus) || nchar(trimws(input$research_focus)) == 0) {
          shiny::showNotification("Research focus is required.", type = "error")
          return()
        }
      }
      step(min(total_steps, step() + 1L))
    })

    # --- Step content ---
    output$step_content <- shiny::renderUI({
      switch(as.character(step()),
        "1" = .ui_step_study(),
        "2" = .ui_step_ai(input),
        "3" = .ui_step_data(),
        "4" = .ui_step_learning(),
        "5" = .ui_step_analysis(),
        "6" = .ui_step_output(),
        "7" = .ui_step_review(input, output_path)
      )
    })

    # --- YAML preview ---
    output$yaml_preview <- shiny::renderText({
      config <- .build_config_from_inputs(input)
      yaml::as.yaml(config, indent = 2, indent.mapping.sequence = TRUE)
    })

    # --- Save ---
    shiny::observeEvent(input$btn_save, {
      config <- .build_config_from_inputs(input)
      save_to <- if (!is.null(input$output_path) && nchar(trimws(input$output_path)) > 0) {
        trimws(input$output_path)
      } else {
        output_path
      }
      header <- paste0(
        "# =============================================================================\n",
        "# pakhom Configuration -- ", config$study$name, "\n",
        "# =============================================================================\n",
        "# Generated by config_wizard_app(). Edit as needed.\n",
        "# Full documentation: see ?load_config or the getting-started vignette.\n",
        "# =============================================================================\n\n"
      )
      yaml_text <- yaml::as.yaml(config, indent = 2, indent.mapping.sequence = TRUE)
      writeLines(paste0(header, yaml_text), save_to)
      saved_path(save_to)
      shiny::showNotification(
        paste("Config saved to", save_to),
        type = "message", duration = 5
      )
      shiny::stopApp(save_to)
    })
  }

  result <- shiny::runApp(
    shiny::shinyApp(ui, server),
    launch.browser = TRUE,
    quiet = TRUE
  )

  invisible(result)
}


# ==============================================================================
# UI builders for each step
# ==============================================================================

#' @keywords internal
.ui_step_study <- function() {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "Study Details"),
    shiny::p(class = "help-text",
             "These settings define your research question and guide the AI's analysis."),

    shiny::textInput("study_name", "Study Name", value = "Untitled Study",
                     width = "100%"),
    shiny::div(class = "help-text", "A short label for your study (used in report headers)."),

    shiny::br(),
    shiny::textAreaInput("research_focus",
                         shiny::HTML("Research Focus <span class='required-star'>*</span>"),
                         rows = 3, width = "100%",
                         placeholder = "Your specific research question. e.g., 'How do online communities discuss [topic X]?' or 'What experiences shape attitudes toward [phenomenon Y]?'"),
    shiny::div(class = "help-text",
               "The most important setting. Be as specific as possible about your research question."),

    shiny::br(),
    shiny::textAreaInput("research_context", "Research Context", rows = 2, width = "100%",
                         placeholder = "Brief description of your data source and population. e.g., 'Online forum discussions', 'Survey responses from undergraduate students', 'Interview transcripts from healthcare workers'"),
    shiny::div(class = "help-text", "Where does your data come from? What is the broader context?"),

    shiny::br(),
    shiny::textInput("concepts", "Core Concepts (comma-separated)", width = "100%",
                     placeholder = "2-5 key concepts central to your research question (any domain)"),
    shiny::div(class = "help-text", "2-5 key concepts that the AI uses for targeted coding and theme generation."),

    shiny::br(),
    shiny::textAreaInput("positionality", "Researcher Positionality (optional)", rows = 2, width = "100%",
                         placeholder = "Your relevant expertise, training, and perspective on the research question (any field)"),
    shiny::div(class = "help-text",
               "Your perspective and background. Helps the AI calibrate its analytical lens.")
  )
}

#' @keywords internal
.ui_step_ai <- function(input) {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "AI Provider"),
    shiny::p(class = "help-text",
             "Choose your AI provider and model settings. Both produce high-quality results."),

    shiny::selectInput("ai_provider", "Provider",
                       choices = c("OpenAI" = "openai", "Anthropic" = "anthropic"),
                       selected = "openai", width = "50%"),
    shiny::div(class = "help-text",
               "OpenAI GPT-4o is recommended for most studies. Anthropic Claude is an excellent alternative."),

    shiny::br(),
    shiny::textInput("api_key_env", "API Key Environment Variable",
                     value = "OPENAI_API_KEY", width = "60%"),
    shiny::div(class = "help-text",
               "The name of the environment variable holding your API key (set in .Renviron)."),

    shiny::br(),
    shiny::h4("Models"),
    shiny::fluidRow(
      shiny::column(6,
        shiny::textInput("model_primary", "Primary Model", value = "gpt-4o", width = "100%"),
        shiny::div(class = "help-text", "Used for complex tasks: coding, theming, merge passes.")
      ),
      shiny::column(6,
        shiny::textInput("model_fast", "Fast Model", value = "gpt-4o-mini", width = "100%"),
        shiny::div(class = "help-text", "Used for batch operations: sentiment analysis.")
      )
    ),

    shiny::br(),
    shiny::h4("Rate Limits"),
    shiny::fluidRow(
      shiny::column(4,
        shiny::numericInput("rpm", "Requests/min", value = 5000, min = 1, width = "100%")
      ),
      shiny::column(4,
        shiny::numericInput("tpm", "Tokens/min", value = 800000, min = 1000, width = "100%")
      ),
      shiny::column(4,
        shiny::numericInput("batch_delay", "Batch Delay (sec)", value = 0.5, min = 0, step = 0.1, width = "100%")
      )
    ),
    shiny::div(class = "help-text",
               "Reduce these if you hit rate limits. Defaults work for most paid API tiers.")
  )
}

#' @keywords internal
.ui_step_data <- function() {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "Data Source"),
    shiny::p(class = "help-text",
             "Point the package at your SQLite database and tell it what kind of data it is."),

    shiny::textInput("database_path", "Database Path (.db file)", width = "100%",
                     placeholder = "e.g., my_data.db"),
    shiny::div(class = "help-text", "Relative to where config.yaml is saved, or an absolute path."),

    shiny::br(),
    shiny::textInput("tables", "Table Name(s)", value = "posts", width = "60%",
                     placeholder = "e.g., posts, comments"),
    shiny::div(class = "help-text",
               "Comma-separated if using multiple tables. The package merges them automatically."),

    shiny::br(),
    shiny::selectInput("source_type", "Data Source Type",
                       choices = c("Reddit" = "reddit", "Twitter" = "twitter",
                                   "Clinical" = "clinical", "Generic" = "generic"),
                       selected = "reddit", width = "50%"),
    shiny::div(class = "help-text",
               "Controls column auto-detection and preprocessing. Use 'Generic' if unsure."),

    shiny::br(),
    shiny::h4("Preprocessing"),
    shiny::fluidRow(
      shiny::column(4,
        shiny::numericInput("min_text_length", "Min Text Length", value = 10, min = 0, width = "100%"),
        shiny::div(class = "help-text", "Entries shorter than this are dropped.")
      ),
      shiny::column(4,
        shiny::numericInput("max_text_length", "Max Text Length", value = 10000, min = 100, width = "100%"),
        shiny::div(class = "help-text", "Entries are truncated to this length.")
      ),
      shiny::column(4,
        shiny::numericInput("dedup_ratio", "Dedup Similarity", value = 0.9, min = 0, max = 1, step = 0.05, width = "100%"),
        shiny::div(class = "help-text", "Entries above this similarity are deduplicated.")
      )
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::checkboxInput("remove_urls", "Remove URLs", value = TRUE)),
      shiny::column(4, shiny::checkboxInput("remove_mentions", "Remove @mentions", value = TRUE)),
      shiny::column(4, shiny::checkboxInput("remove_hashtags", "Remove #hashtags", value = FALSE))
    )
  )
}

#' @keywords internal
.ui_step_learning <- function() {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "Learning from Previous Studies"),
    shiny::p(class = "help-text",
             "If you have completed manual thematic analyses, the AI can learn from them ",
             "to produce more consistent, calibrated results."),

    shiny::checkboxInput("learning_enabled", "Enable manuscript learning", value = FALSE),

    shiny::conditionalPanel(
      condition = "input.learning_enabled",
      shiny::textInput("learning_base_dir", "Manuscripts Directory", width = "100%",
                       placeholder = "e.g., manual analyses"),
      shiny::div(class = "help-text",
                 "Folder containing subfolders (ending in 'study') with manuscript.docx files."),

      shiny::br(),
      shiny::numericInput("max_manuscript_chars", "Max Manuscript Chars", value = 18000,
                          min = 1000, max = 50000, step = 1000, width = "50%"),
      shiny::div(class = "help-text",
                 "Maximum characters to extract from each manuscript. Higher = more context but more cost."),

      shiny::br(),
      shiny::numericInput("max_raw_samples", "Max Raw Data Samples", value = 5,
                          min = 0, max = 20, width = "50%"),
      shiny::div(class = "help-text", "Number of raw data files to include as exemplars per study.")
    )
  )
}

#' @keywords internal
.ui_step_analysis <- function() {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "Analysis Settings"),
    shiny::p(class = "help-text",
             "Fine-tune how each pipeline step behaves."),

    # Test mode
    shiny::h4("Test Mode"),
    shiny::fluidRow(
      shiny::column(4, shiny::checkboxInput("test_mode", "Enable test mode", value = FALSE)),
      shiny::column(4, shiny::numericInput("test_sample_size", "Sample Size", value = 100, min = 5, width = "100%")),
      shiny::column(4, shiny::numericInput("test_seed", "Random Seed", value = 42, width = "100%"))
    ),
    shiny::div(class = "help-text",
               "Test mode runs the pipeline on a small subset. Great for validating your setup before a full run."),

    shiny::hr(),

    # Coding (progressive)
    shiny::h4("Progressive Coding"),
    shiny::fluidRow(
      shiny::column(4, shiny::numericInput("checkpoint_interval", "Checkpoint Interval", value = 50,
                                           min = 10, max = 500, step = 10, width = "100%")),
      shiny::column(4, shiny::numericInput("max_retries", "Max Retries per Entry", value = 1,
                                           min = 0, max = 5, width = "100%")),
      shiny::column(4, shiny::checkboxInput("include_in_vivo", "Include In Vivo Codes", value = TRUE))
    ),
    shiny::div(class = "help-text",
               "Entries are processed one at a time. The AI codes applicable text segments and skips irrelevant entries."),

    shiny::hr(),

    # Thematic saturation
    shiny::h4("Thematic Saturation Detection"),
    shiny::fluidRow(
      shiny::column(4, shiny::checkboxInput("saturation_enabled", "Enable Saturation Detection", value = TRUE)),
      shiny::column(4, shiny::numericInput("saturation_window", "Window Size (coded entries)", value = 200,
                                           min = 50, max = 1000, step = 50, width = "100%")),
      shiny::column(4, shiny::numericInput("saturation_threshold", "New Codes Threshold", value = 2,
                                           min = 0, max = 10, width = "100%"))
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::numericInput("saturation_confirmations", "Consecutive Windows", value = 3,
                                           min = 1, max = 10, width = "100%")),
      shiny::column(4, shiny::numericInput("min_coded_before_sat", "Min Coded Before Saturation", value = 500,
                                           min = 100, max = 5000, step = 100, width = "100%")),
      shiny::column(4, shiny::numericInput("ai_assessment_interval", "AI Assessment Every N Coded", value = 200,
                                           min = 50, max = 1000, step = 50, width = "100%"))
    ),
    shiny::div(class = "help-text",
               "Stops coding when the codebook stabilizes. Uses triangulated signals: code creation rate, ",
               "reuse stability (slope ratio), and AI self-assessment. Disable to code all entries regardless."),

    shiny::hr(),

    # Themes (iterative bottom-up)
    shiny::h4("Theme Generation (Iterative Merge)"),
    shiny::fluidRow(
      shiny::column(4, shiny::selectInput("merge_strategy", "Merge Strategy",
                                          choices = c("Auto (convergence)" = "auto",
                                                      "Manual (pause each pass)" = "manual"),
                                          selected = "auto", width = "100%")),
      shiny::column(4, shiny::numericInput("max_merge_passes", "Max Merge Passes", value = 5,
                                           min = 1, max = 10, width = "100%")),
      shiny::column(4, shiny::numericInput("min_merges_continue", "Min Merges to Continue", value = 2,
                                           min = 1, max = 10, width = "100%"))
    ),
    shiny::div(class = "help-text",
               "Codes are iteratively merged bottom-up. Themes and subthemes emerge from the merge depth."),

    shiny::hr(),

    # Review points
    shiny::h4("Researcher Review Points"),
    shiny::p(class = "help-text",
             "Pause the pipeline at critical decision points so you can curate the AI's output."),
    shiny::fluidRow(
      shiny::column(6, shiny::checkboxInput("review_codes", "After Progressive Coding", value = FALSE)),
      shiny::column(6, shiny::checkboxInput("review_themes", "After Theme Generation", value = FALSE))
    ),

    shiny::hr(),

    # Human verification
    shiny::h4("Human Verification (IRR)"),
    shiny::fluidRow(
      shiny::column(4, shiny::checkboxInput("irr_enabled", "Enable IRR", value = FALSE)),
      shiny::column(4, shiny::numericInput("irr_sample", "Sample Size", value = 20, min = 5, width = "100%")),
      shiny::column(4, shiny::numericInput("irr_seed", "Random Seed", value = 42, width = "100%"))
    ),
    shiny::div(class = "help-text",
               "Inter-rater reliability: exports a sample for independent human coding, then computes agreement metrics."),

    shiny::hr(),

    # Correlations
    shiny::h4("Correlation Analysis"),
    shiny::fluidRow(
      shiny::column(3, shiny::selectInput("corr_method", "Method",
                                          choices = c("Spearman" = "spearman", "Pearson" = "pearson",
                                                      "Kendall" = "kendall"),
                                          selected = "spearman", width = "100%")),
      shiny::column(3, shiny::selectInput("corr_adjust", "P-value Adjustment",
                                          choices = c("Bonferroni" = "bonferroni", "Holm" = "holm",
                                                      "BH (FDR)" = "BH", "None" = "none"),
                                          selected = "bonferroni", width = "100%")),
      shiny::column(3, shiny::numericInput("corr_min_obs", "Min Observations", value = 30, min = 5, width = "100%")),
      shiny::column(3, shiny::numericInput("corr_min_theme", "Min Theme Entries", value = 5, min = 1, width = "100%"))
    )
  )
}

#' @keywords internal
.ui_step_output <- function() {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "Output Settings"),
    shiny::p(class = "help-text", "Control what gets generated and where."),

    shiny::textInput("results_dir", "Results Directory", value = "outputs/results", width = "100%"),
    shiny::div(class = "help-text", "All outputs (report, CSVs, JSON) are saved here under a timestamped run folder."),

    shiny::br(),
    shiny::fluidRow(
      shiny::column(4, shiny::checkboxInput("gen_report", "Generate HTML Report", value = TRUE)),
      shiny::column(4, shiny::checkboxInput("gen_corr_plot", "Generate Correlation Plot", value = TRUE)),
      shiny::column(4, shiny::checkboxInput("gen_comparison", "Enable Run Comparison", value = TRUE))
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::checkboxInput("export_csv", "Export CSVs", value = TRUE)),
      shiny::column(4, shiny::checkboxInput("export_json", "Export JSON", value = TRUE)),
      shiny::column(4, shiny::checkboxInput("gen_theme_details", "Export Theme Details", value = TRUE))
    ),

    shiny::br(),
    shiny::selectInput("log_level", "Log Level",
                       choices = c("DEBUG", "INFO", "WARN", "ERROR"),
                       selected = "INFO", width = "30%"),
    shiny::div(class = "help-text", "DEBUG shows everything. INFO is recommended for normal use.")
  )
}

#' @keywords internal
.ui_step_review <- function(input, default_path) {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "Review & Save"),
    shiny::p(class = "help-text",
             "Review the generated YAML below. You can edit it after saving."),

    shiny::textInput("output_path", "Save As", value = default_path, width = "60%"),
    shiny::div(class = "help-text", "Path where config.yaml will be written."),

    shiny::br(),
    shiny::h4("Generated Configuration"),
    shiny::verbatimTextOutput("yaml_preview", placeholder = TRUE)
  )
}


# ==============================================================================
# Config builder — assembles inputs into a config list
# ==============================================================================

#' @keywords internal
.build_config_from_inputs <- function(input) {
  # Helper to get input value with fallback
  val <- function(id, default = NULL) {
    v <- input[[id]]
    if (is.null(v) || (is.character(v) && nchar(trimws(v)) == 0)) default else v
  }

  # Parse concepts
  concepts_raw <- val("concepts", "")
  concepts <- if (nchar(concepts_raw) > 0) {
    as.list(trimws(strsplit(concepts_raw, ",")[[1]]))
  } else {
    NULL
  }

  # Parse tables
  tables_raw <- val("tables", "posts")
  tables_vec <- trimws(strsplit(tables_raw, ",")[[1]])
  tables <- if (length(tables_vec) == 1) tables_vec else as.list(tables_vec)

  # Determine provider block
  provider <- val("ai_provider", "openai")
  api_env <- val("api_key_env", if (provider == "openai") "OPENAI_API_KEY" else "ANTHROPIC_API_KEY")

  provider_block <- list(
    api_key_env = api_env,
    models = list(
      primary = val("model_primary", if (provider == "openai") "gpt-4o" else "claude-sonnet-4-20250514"),
      fast = val("model_fast", if (provider == "openai") "gpt-4o-mini" else "claude-sonnet-4-20250514")
    ),
    rate_limits = list(
      requests_per_minute = val("rpm", 5000),
      tokens_per_minute = val("tpm", 800000),
      delay_between_batches = val("batch_delay", 0.5)
    )
  )

  ai_block <- list(provider = provider)
  ai_block[[provider]] <- provider_block

  # Build config
  config <- list(
    study = list(
      name = val("study_name", "Untitled Study"),
      research_focus = val("research_focus", ""),
      research_context = val("research_context", ""),
      concepts = concepts
    ),
    ai = ai_block,
    data = list(
      database = val("database_path"),
      tables = tables,
      source_type = val("source_type", "generic"),
      preprocessing = list(
        min_text_length = val("min_text_length", 10),
        max_text_length = val("max_text_length", 10000),
        remove_urls = isTRUE(input$remove_urls),
        remove_mentions = isTRUE(input$remove_mentions),
        remove_hashtags = isTRUE(input$remove_hashtags)
      )
    ),
    analysis = list(
      test_mode = list(
        enabled = isTRUE(input$test_mode),
        sample_size = val("test_sample_size", 100),
        seed = val("test_seed", 42)
      ),
      coding = list(
        progressive = TRUE,
        include_in_vivo = isTRUE(input$include_in_vivo),
        max_retries_per_entry = val("max_retries", 1),
        checkpoint_interval = val("checkpoint_interval", 50),
        saturation_enabled = isTRUE(input$saturation_enabled),
        saturation_window = val("saturation_window", 200),
        saturation_threshold = val("saturation_threshold", 2),
        saturation_confirmations = val("saturation_confirmations", 3),
        min_coded_before_saturation = val("min_coded_before_sat", 500),
        ai_assessment_interval = val("ai_assessment_interval", 200)
      ),
      themes = list(
        merge_strategy = val("merge_strategy", "auto"),
        max_merge_passes = val("max_merge_passes", 5),
        min_merges_to_continue = val("min_merges_continue", 2),
        include_subthemes = TRUE,
        include_quotes = TRUE
      ),
      review_points = list(
        after_coding = isTRUE(input$review_codes),
        after_themes = isTRUE(input$review_themes)
      ),
      human_verification = list(
        enabled = isTRUE(input$irr_enabled),
        sample_size = val("irr_sample", 20),
        seed = val("irr_seed", 42)
      ),
      correlations = list(
        method = val("corr_method", "spearman"),
        adjust_method = val("corr_adjust", "bonferroni"),
        min_observations = val("corr_min_obs", 30),
        min_theme_entries = val("corr_min_theme", 5)
      )
    ),
    output = list(
      results_dir = val("results_dir", "outputs/results"),
      generate_report = isTRUE(input$gen_report),
      generate_correlation_plot = isTRUE(input$gen_corr_plot),
      generate_theme_details = isTRUE(input$gen_theme_details),
      export_csv = isTRUE(input$export_csv),
      export_json = isTRUE(input$export_json),
      comparison_enabled = isTRUE(input$gen_comparison)
    ),
    logging = list(
      log_level = val("log_level", "INFO")
    )
  )

  # Add positionality if provided
  pos <- val("positionality")
  if (!is.null(pos)) config$study$researcher_positionality <- pos

  # Add theme min/max if provided (NA means unset)
  min_t <- input$min_themes
  max_t <- input$max_themes
  if (!is.null(min_t) && !is.na(min_t)) config$analysis$themes$min_themes <- min_t
  if (!is.null(max_t) && !is.na(max_t)) config$analysis$themes$max_themes <- max_t

  # Add learning if enabled
  if (isTRUE(input$learning_enabled)) {
    config$learning <- list(
      enabled = TRUE,
      base_dir = val("learning_base_dir", "manual analyses"),
      max_manuscript_chars = val("max_manuscript_chars", 18000),
      max_raw_samples = val("max_raw_samples", 5)
    )
  } else {
    config$learning <- list(enabled = FALSE)
  }

  config
}
