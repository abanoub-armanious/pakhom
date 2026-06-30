# ==============================================================================
# Shiny Config Wizard: interactive web-based configuration builder
# ==============================================================================

#' Launch the interactive configuration wizard
#'
#' Opens a Shiny app that walks you through every configuration option
#' with descriptions and sensible defaults. The step gates enforce the fields
#' [validate_config()] requires (an explicit methodology mode, a research
#' focus, a data path, and a framework spec for Mode 3), so the `config.yaml`
#' it writes loads cleanly.
#'
#' This is the web-based companion to the CLI-based [config_wizard()]. Both
#' write an equivalent `config.yaml` (the same required sections and methodology
#' block); exact field defaults may differ between the two entry points.
#'
#' @param output_path Where to save the generated config (default "config.yaml").
#'   The user can also change this in the UI.
#' @param .return_app Internal testing hook. When \code{TRUE}, returns the Shiny
#'   app object (for \code{shiny::testServer}) instead of launching it.
#' @return The path to the created config file (invisibly). Returns NULL if the
#'   user closes the app without saving.
#' @seealso [config_wizard()] for the CLI-based wizard and [create_config()] for
#'   building a config programmatically; [load_config()] to load the result.
#' @export
config_wizard_app <- function(output_path = "config.yaml", .return_app = FALSE) {
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
    total_steps <- 9L
    step_labels <- c("Methodology", "Study", "AI Provider", "Data", "Scraping",
                     "Learning", "Analysis", "Output", "Review & Save")
    saved_path <- shiny::reactiveVal(NULL)

    # Pre-load an existing config (if the target file already exists) so a
    # re-run starts from the saved values instead of reverting every field to
    # its built-in default. dflts is keyed 1:1 by input id.
    existing_cfg <- if (file.exists(output_path)) {
      tryCatch(yaml::read_yaml(output_path), error = function(e) NULL)
    } else {
      NULL
    }
    dflts <- .wizard_input_defaults(existing_cfg)

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

    # Keep the API-key env var + model fields in sync with the chosen provider.
    # Without this, selecting Anthropic left the OpenAI key var + gpt-4o models,
    # silently producing an OpenAI-shaped config under the anthropic block --
    # exactly the path the methods paper's OpenAI-vs-Anthropic comparison uses.
    # Track the provider so the sync only fires on an actual USER change. The
    # AI step's inputs are created lazily (renderUI), so when the user first
    # reaches it the pre-loaded provider materializes and would otherwise be
    # seen as a "change" -- stomping the pre-loaded model/key fields with the
    # provider defaults. Seeding this with the pre-loaded provider prevents that.
    prev_provider <- shiny::reactiveVal(dflts$ai_provider)
    shiny::observeEvent(input$ai_provider, {
      prov <- input$ai_provider %||% "openai"
      if (identical(prov, prev_provider())) return()  # materialization, not a change
      prev_provider(prov)
      dm <- .default_models(prov)
      key_env <- if (identical(prov, "anthropic")) "ANTHROPIC_API_KEY" else "OPENAI_API_KEY"
      shiny::updateTextInput(session, "api_key_env", value = key_env)
      shiny::updateTextInput(session, "model_primary", value = dm$primary)
      shiny::updateTextInput(session, "model_fast", value = dm$fast %||% dm$primary)
    }, ignoreInit = TRUE)

    shiny::observeEvent(input$btn_back, { step(max(1L, step() - 1L)) })
    shiny::observeEvent(input$btn_next, {
      # Step 1 (Methodology): framework_applied (Mode 3) requires a
      # framework spec, mirroring create_config()/validate_config(). Block
      # advancing without one so the saved config can't fail validation.
      if (step() == 1L) {
        if (is.null(input$methodology_mode)) {
          shiny::showNotification(
            "Choose a methodology mode -- the declaration is mandatory (no default).",
            type = "error")
          return()
        }
        if (identical(input$methodology_mode, "framework_applied") &&
            (is.null(input$framework_spec_path) ||
             nchar(trimws(input$framework_spec_path)) == 0)) {
          shiny::showNotification(
            "Framework mode requires a framework spec (a built-in alias like 'tpb' or a path).",
            type = "error")
          return()
        }
      }
      # Step 2 (Study): research focus is required (validate_config() rejects
      # an empty study.research_focus for every mode).
      if (step() == 2L) {
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
        "1" = .ui_step_methodology(dflts),
        "2" = .ui_step_study(dflts),
        "3" = .ui_step_ai(input, dflts),
        "4" = .ui_step_data(dflts),
        "5" = .ui_step_scraping(dflts),
        "6" = .ui_step_learning(dflts),
        "7" = .ui_step_analysis(dflts),
        "8" = .ui_step_output(dflts),
        "9" = .ui_step_review(input, output_path)
      )
    })

    # --- YAML preview ---
    output$yaml_preview <- shiny::renderText({
      config <- .build_config_from_inputs(input)
      yaml::as.yaml(config, indent = 2, indent.mapping.sequence = TRUE)
    })

    # --- Save ---
    shiny::observeEvent(input$btn_save, {
      # Build + write inside tryCatch so a build error (e.g. a missing
      # methodology backstop) or an unwritable output path surfaces as an
      # in-app notification rather than crashing the Shiny session.
      result <- tryCatch({
        config <- .build_config_from_inputs(input)
        save_to <- if (!is.null(input$output_path) && nchar(trimws(input$output_path)) > 0) {
          trimws(input$output_path)
        } else {
          output_path
        }
        # Merge the wizard output over an existing file rather than overwriting
        # it wholesale, so hand-edited sections the wizard does not surface are
        # preserved across re-runs.
        if (file.exists(save_to)) {
          existing <- tryCatch(yaml::read_yaml(save_to), error = function(e) NULL)
          # Drop NULL leaves first so a blank wizard field does not delete the
          # corresponding value already in the file.
          if (is.list(existing)) {
            config <- utils::modifyList(existing, .drop_null_leaves(config))
            config <- .normalize_merged_config(config)
          }
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
        save_to
      }, error = function(e) {
        shiny::showNotification(
          paste0("Could not save config: ", conditionMessage(e)),
          type = "error", duration = NULL)
        NULL
      })
      if (is.null(result)) return(invisible(NULL))
      saved_path(result)
      shiny::showNotification(
        paste("Config saved to", result),
        type = "message", duration = 5
      )
      shiny::stopApp(result)
    })
  }

  app <- shiny::shinyApp(ui, server)
  # Testability hook: return the app object (for shiny::testServer) instead of
  # launching it. Internal use only.
  if (isTRUE(.return_app)) return(app)

  result <- shiny::runApp(
    app,
    launch.browser = TRUE,
    quiet = TRUE
  )

  invisible(result)
}


# ==============================================================================
# UI builders for each step
# ==============================================================================

#' @keywords internal
.ui_step_methodology <- function(dflts = .wizard_input_defaults()) {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "Methodology Mode"),
    shiny::p(class = "help-text",
             "The most consequential choice in pakhom: it determines which AI ",
             "behaviors are permitted, which artifacts are mandatory, and which ",
             "report sections are generated. This declaration is required -- a ",
             "config without it cannot be loaded. Unsure which fits your study? ",
             "Run ", shiny::code("methodology_decision_aid()"), " for guidance, ",
             "or see ", shiny::code("vignette('methodology-modes')"), "."),

    shiny::radioButtons("methodology_mode", NULL,
      choiceNames = list(
        shiny::HTML("<b>Reflexive scaffold</b> (Mode 1) &mdash; inductive, bottom-up reflexive thematic analysis. The AI acts as a provocateur (questions, counter-narratives); the researcher does the meaning-making. Reflexive memos + positionality are mandatory. <i>(Braun &amp; Clarke reflexive TA.)</i>"),
        shiny::HTML("<b>Codebook collaborative</b> (Mode 2) &mdash; the AI proposes codes across the corpus; the researcher accepts / edits / rejects each. A shared codebook is the deliverable; inter-rater reliability is available. <i>(Codebook / template TA.)</i>"),
        shiny::HTML("<b>Framework applied</b> (Mode 3) &mdash; deductive coding against a predefined framework you supply (TPB, COM-B, TDF, or a custom spec); entries that resist the framework are flagged. <i>(Framework / theoretical analysis.)</i>")
      ),
      choiceValues = c("reflexive_scaffold", "codebook_collaborative", "framework_applied"),
      # AC3 (no default mode): start UNSELECTED so the researcher makes a
      # conscious choice -- a preselected mode would let a user click
      # through and inherit a methodology they never chose, the exact
      # silent default create_config()/the CLI wizard refuse. A pre-loaded
      # config restores its own explicitly-chosen mode.
      selected = dflts$methodology_mode %||% character(0), width = "100%"),

    shiny::conditionalPanel(
      condition = "input.methodology_mode == 'framework_applied'",
      shiny::br(),
      shiny::textInput("framework_spec_path", "Framework Specification", width = "100%",
                       value = dflts$framework_spec_path,
                       placeholder = "Built-in alias (tpb, comb, tdf) or a path to a custom YAML/JSON spec"),
      shiny::div(class = "help-text",
                 "Required for framework_applied. Use a built-in alias ('tpb', ",
                 "'comb', 'tdf') or a path to your own framework spec file.")
    )
  )
}

#' @keywords internal
.ui_step_study <- function(dflts = .wizard_input_defaults()) {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "Study Details"),
    shiny::p(class = "help-text",
             "These settings define your research question and guide the AI's analysis."),

    shiny::textInput("study_name", "Study Name", value = dflts$study_name,
                     width = "100%"),
    shiny::div(class = "help-text", "A short label for your study (used in report headers)."),

    shiny::br(),
    shiny::textAreaInput("research_focus",
                         shiny::HTML("Research Focus <span class='required-star'>*</span>"),
                         value = dflts$research_focus,
                         rows = 3, width = "100%",
                         placeholder = "Your specific research question. e.g., 'How do online communities discuss [topic X]?' or 'What experiences shape attitudes toward [phenomenon Y]?'"),
    shiny::div(class = "help-text",
               "The most important setting. Be as specific as possible about your research question."),

    shiny::br(),
    shiny::textAreaInput("research_context", "Research Context", value = dflts$research_context,
                         rows = 2, width = "100%",
                         placeholder = "Brief description of your data source and population. e.g., 'Online forum discussions', 'Survey responses from undergraduate students', 'Interview transcripts from healthcare workers'"),
    shiny::div(class = "help-text", "Where does your data come from? What is the broader context?"),

    shiny::br(),
    shiny::textInput("concepts", "Core Concepts (comma-separated)", width = "100%",
                     value = dflts$concepts,
                     placeholder = "2-5 key concepts central to your research question (any domain)"),
    shiny::div(class = "help-text", "2-5 key concepts that the AI uses for targeted coding and theme generation."),

    shiny::br(),
    shiny::textAreaInput("positionality", "Researcher Positionality (optional)", value = dflts$positionality,
                         rows = 2, width = "100%",
                         placeholder = "Your relevant expertise, training, and perspective on the research question (any field)"),
    shiny::div(class = "help-text",
               "Your perspective and background. Helps the AI calibrate its analytical lens.")
  )
}

#' @keywords internal
.ui_step_ai <- function(input, dflts = .wizard_input_defaults()) {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "AI Provider"),
    shiny::p(class = "help-text",
             "Choose your AI provider and model settings. Both produce high-quality results."),

    shiny::selectInput("ai_provider", "Provider",
                       choices = c("OpenAI" = "openai", "Anthropic" = "anthropic"),
                       selected = dflts$ai_provider, width = "50%"),
    shiny::div(class = "help-text",
               "OpenAI GPT-4o is recommended for most studies. Anthropic Claude is an excellent alternative."),

    shiny::br(),
    shiny::textInput("api_key_env", "API Key Environment Variable",
                     value = dflts$api_key_env, width = "60%"),
    shiny::div(class = "help-text",
               "The name of the environment variable holding your API key (set in .Renviron)."),

    shiny::div(
      style = paste0("margin-top: 10px; padding: 10px 12px; background: #f0f7ff; ",
                     "border-left: 3px solid #4a90d9; border-radius: 4px; font-size: 0.9em;"),
      shiny::tags$strong("Your privacy. "),
      "pakhom runs entirely on your own machine and collects no telemetry, and ",
      "nothing about you or your data is sent to its authors. Your text is ",
      "transmitted only to the AI provider you select above (over HTTPS), and ",
      "solely to perform the analysis you request. Your API key is read from your ",
      "environment and is never written to logs, audit records, or output files."),

    shiny::br(),
    shiny::h4("Models"),
    shiny::fluidRow(
      shiny::column(6,
        shiny::textInput("model_primary", "Primary Model", value = dflts$model_primary, width = "100%"),
        shiny::div(class = "help-text", "Used for complex tasks: coding, theming, merge passes.")
      ),
      shiny::column(6,
        shiny::textInput("model_fast", "Fast Model", value = dflts$model_fast, width = "100%"),
        shiny::div(class = "help-text", "Used for batch operations: sentiment analysis.")
      )
    ),

    shiny::br(),
    shiny::h4("Rate Limits"),
    shiny::fluidRow(
      shiny::column(4,
        shiny::numericInput("rpm", "Requests/min", value = dflts$rpm, min = 1, width = "100%")
      ),
      shiny::column(4,
        shiny::numericInput("tpm", "Tokens/min", value = dflts$tpm, min = 1000, width = "100%")
      ),
      shiny::column(4,
        shiny::numericInput("batch_delay", "Batch Delay (sec)", value = dflts$batch_delay, min = 0, step = 0.1, width = "100%")
      )
    ),
    shiny::div(class = "help-text",
               "Reduce these if you hit rate limits. Defaults work for most paid API tiers.")
  )
}

#' @keywords internal
.ui_step_data <- function(dflts = .wizard_input_defaults()) {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "Data Source"),
    shiny::p(class = "help-text",
             "Point the package at your SQLite database and tell it what kind of data it is."),

    shiny::textInput("database_path", "Database Path (.db file)", width = "100%",
                     value = dflts$database_path,
                     placeholder = "e.g., my_data.db"),
    shiny::div(class = "help-text", "Relative to where config.yaml is saved, or an absolute path."),

    shiny::br(),
    shiny::textInput("tables", "Table Name(s)", value = dflts$tables, width = "60%",
                     placeholder = "e.g., posts, comments"),
    shiny::div(class = "help-text",
               "Comma-separated if using multiple tables. The package merges them automatically."),

    shiny::br(),
    shiny::selectInput("source_type", "Data Source Type",
                       choices = c("Reddit" = "reddit", "Twitter" = "twitter",
                                   "Clinical" = "clinical", "Generic" = "generic"),
                       selected = dflts$source_type, width = "50%"),
    shiny::div(class = "help-text",
               "Controls column auto-detection and preprocessing. Use 'Generic' if unsure."),

    shiny::br(),
    shiny::h4("Preprocessing"),
    shiny::fluidRow(
      shiny::column(4,
        shiny::numericInput("min_text_length", "Min Text Length", value = dflts$min_text_length, min = 0, width = "100%"),
        shiny::div(class = "help-text", "Entries shorter than this are dropped.")
      ),
      shiny::column(4,
        shiny::numericInput("max_text_length", "Max Text Length", value = dflts$max_text_length, min = 100, width = "100%"),
        shiny::div(class = "help-text", "Entries are truncated to this length.")
      ),
      shiny::column(4,
        shiny::numericInput("dedup_ratio", "Dedup Similarity", value = dflts$dedup_ratio, min = 0, max = 1, step = 0.05, width = "100%"),
        shiny::div(class = "help-text", "Entries above this similarity are deduplicated.")
      )
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::checkboxInput("remove_urls", "Remove URLs", value = dflts$remove_urls)),
      shiny::column(4, shiny::checkboxInput("remove_mentions", "Remove @mentions", value = dflts$remove_mentions)),
      shiny::column(4, shiny::checkboxInput("remove_hashtags", "Remove #hashtags", value = dflts$remove_hashtags))
    )
  )
}

#' @keywords internal
.ui_step_scraping <- function(dflts = .wizard_input_defaults()) {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "Reddit Scraping (optional)"),
    shiny::p(class = "help-text",
             "If your corpus is already in a SQLite database, skip this. ",
             "Otherwise the built-in scraper can collect posts and full ",
             "comment trees from subreddits into the database above."),

    shiny::checkboxInput("scraping_enabled", "Enable Reddit scraping", value = dflts$scraping_enabled),

    shiny::conditionalPanel(
      condition = "input.scraping_enabled",
      shiny::div(class = "help-text",
                 "Reddit requires Responsible Builder approval for API access. ",
                 "Store credentials in .Renviron (REDDIT_CLIENT_ID, ",
                 "REDDIT_CLIENT_SECRET); they are never written to this config."),
      shiny::br(),
      shiny::textInput("scraping_subreddits", "Subreddits", width = "100%",
                       value = dflts$scraping_subreddits,
                       placeholder = "comma-separated, without 'r/' (e.g. productivity, remotework)"),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(4,
          shiny::numericInput("scraping_posts", "Posts per subreddit",
                              value = dflts$scraping_posts, min = 1, width = "100%"),
          shiny::div(class = "help-text", "Counts only NEW posts on re-runs.")
        ),
        shiny::column(4,
          shiny::selectInput("scraping_sort", "Sort by",
            choices = c("new", "hot", "top", "rising"), selected = dflts$scraping_sort, width = "100%")
        ),
        shiny::column(4,
          shiny::selectInput("scraping_time", "Time filter (top only)",
            choices = c("hour", "day", "week", "month", "year", "all"),
            selected = dflts$scraping_time, width = "100%")
        )
      ),
      shiny::checkboxInput("scraping_comments", "Include comments", value = dflts$scraping_comments)
    )
  )
}

#' @keywords internal
.ui_step_learning <- function(dflts = .wizard_input_defaults()) {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "Learning from Previous Studies"),
    shiny::p(class = "help-text",
             "If you have completed manual thematic analyses, the AI can learn from them ",
             "to produce more consistent, calibrated results."),

    shiny::checkboxInput("learning_enabled", "Enable manuscript learning", value = dflts$learning_enabled),

    shiny::conditionalPanel(
      condition = "input.learning_enabled",
      shiny::textInput("learning_base_dir", "Manuscripts Directory", width = "100%",
                       value = dflts$learning_base_dir,
                       placeholder = "e.g., manual analyses"),
      shiny::div(class = "help-text",
                 "Folder containing subfolders (ending in 'study') with manuscript.docx files."),

      shiny::br(),
      shiny::numericInput("max_manuscript_chars", "Max Manuscript Chars", value = dflts$max_manuscript_chars,
                          min = 1000, max = 50000, step = 1000, width = "50%"),
      shiny::div(class = "help-text",
                 "Maximum characters to extract from each manuscript. Higher = more context but more cost."),

      shiny::br(),
      shiny::numericInput("max_raw_samples", "Max Raw Data Samples", value = dflts$max_raw_samples,
                          min = 0, max = 20, width = "50%"),
      shiny::div(class = "help-text", "Number of raw data files to include as exemplars per study.")
    )
  )
}

#' @keywords internal
.ui_step_analysis <- function(dflts = .wizard_input_defaults()) {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "Analysis Settings"),
    shiny::p(class = "help-text",
             "Fine-tune how each pipeline step behaves."),

    # Test mode
    shiny::h4("Test Mode"),
    shiny::fluidRow(
      shiny::column(4, shiny::checkboxInput("test_mode", "Enable test mode", value = dflts$test_mode)),
      shiny::column(4, shiny::numericInput("test_sample_size", "Sample Size", value = dflts$test_sample_size, min = 5, width = "100%")),
      shiny::column(4, shiny::numericInput("test_seed", "Random Seed", value = dflts$test_seed, width = "100%"))
    ),
    shiny::div(class = "help-text",
               "Test mode runs the pipeline on a small subset. Great for validating your setup before a full run."),

    shiny::hr(),

    # Coding (progressive)
    shiny::h4("Progressive Coding"),
    shiny::fluidRow(
      shiny::column(4, shiny::numericInput("checkpoint_interval", "Checkpoint Interval", value = dflts$checkpoint_interval,
                                           min = 10, max = 500, step = 10, width = "100%")),
      shiny::column(4, shiny::numericInput("max_retries", "Max Retries per Entry", value = dflts$max_retries,
                                           min = 0, max = 5, width = "100%")),
      shiny::column(4, shiny::checkboxInput("include_in_vivo", "Include In Vivo Codes", value = dflts$include_in_vivo))
    ),
    shiny::div(class = "help-text",
               "Entries are processed one at a time. The AI codes applicable text segments and skips irrelevant entries."),

    shiny::hr(),

    # Thematic saturation (AI-arbited).
    # Per C1 ("AI decides when to stop"), saturation is judged by an AI
    # arbiter whose cadence auto-scales with corpus size; there are no
    # user-tunable knobs. This help block is informational only.
    shiny::h4("Thematic Saturation"),
    shiny::div(class = "help-text",
               "Saturation is judged by an AI arbiter. The AI ",
               "evaluates the recent code-growth trajectory + codebook ",
               "composition at an adaptive cadence scaled to the coded ",
               "entries, and returns one of: reached / not_yet / uncertain. ",
               "Coding stops at the first 'reached' verdict. No tunable ",
               "thresholds -- per C1, the AI decides when to stop."),

    shiny::hr(),

    # removed the "Theme Generation (Iterative Merge)" UI
    # block. The earlier sequential pairwise insertion algorithm
    # (max_merge_passes / min_merges_continue / merge_strategy) was
    # replaced by AI-judged clustering, which has no merge-pass
    # parameters. Per C1 the AI decides the clustering; there's
    # nothing for the wizard to gate.

    shiny::hr(),

    # Review points
    shiny::h4("Researcher Review Points"),
    shiny::p(class = "help-text",
             "Pause the pipeline at critical decision points so you can curate the AI's output."),
    shiny::fluidRow(
      shiny::column(6, shiny::checkboxInput("review_codes", "After Progressive Coding", value = dflts$review_codes)),
      shiny::column(6, shiny::checkboxInput("review_themes", "After Theme Generation", value = dflts$review_themes))
    ),

    shiny::hr(),

    # Human verification
    shiny::h4("Human Verification (IRR)"),
    shiny::fluidRow(
      shiny::column(4, shiny::checkboxInput("irr_enabled", "Enable IRR", value = dflts$irr_enabled)),
      shiny::column(4, shiny::numericInput("irr_sample", "Sample Size", value = dflts$irr_sample, min = 5, width = "100%")),
      shiny::column(4, shiny::numericInput("irr_seed", "Random Seed", value = dflts$irr_seed, width = "100%"))
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
                                          selected = dflts$corr_method, width = "100%")),
      shiny::column(3, shiny::selectInput("corr_adjust", "P-value Adjustment",
                                          choices = c("Bonferroni" = "bonferroni", "Holm" = "holm",
                                                      "BH (FDR)" = "BH", "None" = "none"),
                                          selected = dflts$corr_adjust, width = "100%")),
      shiny::column(3, shiny::numericInput("corr_min_obs", "Min Observations", value = dflts$corr_min_obs, min = 5, width = "100%")),
      shiny::column(3, shiny::numericInput("corr_min_theme", "Min Theme Entries", value = dflts$corr_min_theme, min = 1, width = "100%"))
    )
  )
}

#' @keywords internal
.ui_step_output <- function(dflts = .wizard_input_defaults()) {
  shiny::div(class = "section-card",
    shiny::h3(class = "section-title", "Output Settings"),
    shiny::p(class = "help-text", "Control what gets generated and where."),

    shiny::textInput("results_dir", "Results Directory", value = dflts$results_dir, width = "100%"),
    shiny::div(class = "help-text", "All outputs (report, CSVs, JSON) are saved here under a timestamped run folder."),

    shiny::br(),
    shiny::fluidRow(
      shiny::column(4, shiny::checkboxInput("gen_report", "Generate HTML Report", value = dflts$gen_report)),
      shiny::column(4, shiny::checkboxInput("gen_corr_plot", "Generate Correlation Plot", value = dflts$gen_corr_plot)),
      shiny::column(4, shiny::checkboxInput("gen_comparison", "Enable Run Comparison", value = dflts$gen_comparison))
    ),
    shiny::fluidRow(
      shiny::column(4, shiny::checkboxInput("export_csv", "Export CSVs", value = dflts$export_csv)),
      shiny::column(4, shiny::checkboxInput("export_json", "Export JSON", value = dflts$export_json)),
      shiny::column(4, shiny::checkboxInput("gen_theme_details", "Export Theme Details", value = dflts$gen_theme_details))
    ),

    shiny::br(),
    shiny::selectInput("log_level", "Log Level",
                       choices = c("DEBUG", "INFO", "WARN", "ERROR"),
                       selected = dflts$log_level, width = "30%"),
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
# Config builder: assembles the inputs into a config list
# ==============================================================================

#' Input defaults for the wizard, derived from an existing config
#'
#' The inverse of \code{.build_config_from_inputs}: given a previously-saved
#' config (or NULL for a fresh session), returns the value each wizard input
#' should default to, keyed 1:1 by input id. Threading this into the UI builders
#' lets a re-run round-trip untouched fields instead of reverting them to
#' built-in defaults. \code{.build_config_from_inputs(.wizard_input_defaults(cfg))}
#' reproduces \code{cfg} for the fields the wizard manages.
#' @keywords internal
#' @noRd
.wizard_input_defaults <- function(existing = NULL) {
  g <- function(path, default = NULL) {
    cur <- existing
    for (k in strsplit(path, ".", fixed = TRUE)[[1]]) {
      if (!is.list(cur)) return(default)
      v <- cur[[k]]
      if (is.null(v)) return(default)
      cur <- v
    }
    cur
  }
  csv <- function(x, default = "") {
    if (is.null(x) || length(x) == 0) return(default)
    paste(unlist(x), collapse = ", ")
  }
  prov <- g("ai.provider", "openai")
  dm <- .default_models(prov)
  key_env_default <- if (identical(prov, "anthropic")) "ANTHROPIC_API_KEY" else "OPENAI_API_KEY"

  list(
    # Methodology (AC3: no default mode -- NULL when absent)
    methodology_mode    = g("methodology.mode"),
    framework_spec_path = g("methodology.framework_spec_path", ""),
    # Study
    study_name       = g("study.name", "Untitled Study"),
    research_focus   = g("study.research_focus", ""),
    research_context = g("study.research_context", ""),
    concepts         = csv(g("study.concepts")),
    positionality    = g("study.researcher_positionality", ""),
    # AI provider
    ai_provider   = prov,
    api_key_env   = g(paste0("ai.", prov, ".api_key_env"), key_env_default),
    model_primary = g(paste0("ai.", prov, ".models.primary"), dm$primary),
    model_fast    = g(paste0("ai.", prov, ".models.fast"), dm$fast %||% dm$primary),
    rpm           = g(paste0("ai.", prov, ".rate_limits.requests_per_minute"), 5000),
    tpm           = g(paste0("ai.", prov, ".rate_limits.tokens_per_minute"), 800000),
    batch_delay   = g(paste0("ai.", prov, ".rate_limits.delay_between_batches"), 0.5),
    # Data
    database_path   = g("data.database", ""),
    tables          = csv(g("data.tables"), "posts"),
    source_type     = g("data.source_type", "reddit"),
    min_text_length = g("data.preprocessing.min_text_length", 10),
    max_text_length = g("data.preprocessing.max_text_length", 10000),
    dedup_ratio     = 0.9,  # shown in the UI but not written by the builder
    remove_urls     = isTRUE(g("data.preprocessing.remove_urls", TRUE)),
    remove_mentions = isTRUE(g("data.preprocessing.remove_mentions", TRUE)),
    remove_hashtags = isTRUE(g("data.preprocessing.remove_hashtags", FALSE)),
    # Scraping
    scraping_enabled    = isTRUE(g("scraping.enabled", FALSE)),
    scraping_subreddits = csv(g("scraping.subreddits")),
    scraping_posts      = g("scraping.posts_per_subreddit", 500),
    scraping_comments   = isTRUE(g("scraping.include_comments", TRUE)),
    scraping_sort       = g("scraping.sort_by", "new"),
    scraping_time       = g("scraping.time_filter", "all"),
    # Learning
    learning_enabled     = isTRUE(g("learning.enabled", FALSE)),
    learning_base_dir    = g("learning.base_dir", "manual analyses"),
    max_manuscript_chars = g("learning.max_manuscript_chars", 18000),
    max_raw_samples      = g("learning.max_raw_samples", 5),
    # Analysis
    test_mode           = isTRUE(g("analysis.test_mode.enabled", FALSE)),
    test_sample_size    = g("analysis.test_mode.sample_size", 100),
    test_seed           = g("analysis.test_mode.seed", 42),
    checkpoint_interval = g("analysis.coding.checkpoint_interval", 50),
    max_retries         = g("analysis.coding.max_retries_per_entry", 1),
    include_in_vivo     = isTRUE(g("analysis.coding.include_in_vivo", TRUE)),
    review_codes        = isTRUE(g("analysis.review_points.after_coding", FALSE)),
    review_themes       = isTRUE(g("analysis.review_points.after_themes", FALSE)),
    irr_enabled         = isTRUE(g("analysis.human_verification.enabled", FALSE)),
    irr_sample          = g("analysis.human_verification.sample_size", 20),
    irr_seed            = g("analysis.human_verification.seed", 42),
    corr_method         = g("analysis.correlations.method", "spearman"),
    corr_adjust         = g("analysis.correlations.adjust_method", "bonferroni"),
    corr_min_obs        = g("analysis.correlations.min_observations", 30),
    corr_min_theme      = g("analysis.correlations.min_theme_entries", 5),
    # Output
    results_dir       = g("output.results_dir", "outputs/results"),
    gen_report        = isTRUE(g("output.generate_report", TRUE)),
    gen_corr_plot     = isTRUE(g("output.generate_correlation_plot", TRUE)),
    gen_theme_details = isTRUE(g("output.generate_theme_details", TRUE)),
    export_csv        = isTRUE(g("output.export_csv", TRUE)),
    export_json       = isTRUE(g("output.export_json", TRUE)),
    gen_comparison    = isTRUE(g("output.comparison_enabled", TRUE)),
    # Logging
    log_level = g("logging.log_level", "INFO")
  )
}

#' @keywords internal
.build_config_from_inputs <- function(input) {
  # Helper to get input value with fallback
  val <- function(id, default = NULL) {
    v <- input[[id]]
    if (is.null(v) || (is.character(v) && nchar(trimws(v)) == 0)) default else v
  }

  # Parse concepts (drop empty tokens from "a,,b" or a trailing comma)
  concepts_raw <- val("concepts", "")
  concepts <- if (nchar(concepts_raw) > 0) {
    toks <- trimws(strsplit(concepts_raw, ",")[[1]])
    toks <- toks[nzchar(toks)]
    if (length(toks) > 0) as.list(toks) else NULL
  } else {
    NULL
  }

  # Parse tables (drop empty tokens)
  tables_raw <- val("tables", "posts")
  tables_vec <- trimws(strsplit(tables_raw, ",")[[1]])
  tables_vec <- tables_vec[nzchar(tables_vec)]
  if (length(tables_vec) == 0) tables_vec <- "posts"
  tables <- if (length(tables_vec) == 1) tables_vec else as.list(tables_vec)

  # Determine provider block
  provider <- val("ai_provider", "openai")
  api_env <- val("api_key_env", if (provider == "openai") "OPENAI_API_KEY" else "ANTHROPIC_API_KEY")

  provider_block <- list(
    api_key_env = api_env,
    models = list(
      primary = val("model_primary", if (provider == "openai") "gpt-4o" else "claude-sonnet-4-20250514"),
      fast = val("model_fast", if (provider == "openai") "gpt-4o-mini" else "claude-haiku-4-5-20251001")
    ),
    rate_limits = list(
      requests_per_minute = val("rpm", 5000),
      tokens_per_minute = val("tpm", 800000),
      delay_between_batches = val("batch_delay", 0.5)
    )
  )

  ai_block <- list(provider = provider)
  ai_block[[provider]] <- provider_block

  # Methodology block: mandatory. Without it, the saved
  # config fails validate_config() with "Missing required config section:
  # 'methodology'". The Methodology step captures the mode; framework_applied
  # (Mode 3) additionally captures framework_spec_path.
  # AC3 (no default mode): do NOT silently fall back to a methodology here. The
  # UI already blocks advancing past the Methodology step without an explicit
  # choice; this is the build-layer backstop -- a missing mode is an error, not
  # a silent Mode 2.
  meth_mode <- val("methodology_mode", NULL)
  if (is.null(meth_mode) || !nzchar(meth_mode)) {
    stop("No methodology mode selected. The wizard requires an explicit ",
         "methodology choice (AC3: no default mode).", call. = FALSE)
  }
  methodology_block <- list(mode = meth_mode)
  if (identical(meth_mode, "framework_applied")) {
    # Single-bracket + list() preserves an explicit NULL leaf (the `$<- NULL`
    # idiom would DROP the key). The key is therefore always present for
    # Mode 3, so an omitted path surfaces as the actionable
    # validate_config() error rather than a silently dropped field.
    methodology_block["framework_spec_path"] <- list(val("framework_spec_path"))
  }

  # Build config
  config <- list(
    methodology = methodology_block,
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
        checkpoint_interval = val("checkpoint_interval", 50)
        # the earlier saturation knobs (saturation_enabled,
        # saturation_window, saturation_threshold, saturation_confirmations,
        # min_coded_before_saturation, ai_assessment_interval) removed.
        # The AI saturation arbiter is the sole decision; cadence
        # auto-scales by corpus size.
      ),
      themes = list(
        # removed dead merge-pass knobs
        # (merge_strategy, max_merge_passes, min_merges_to_continue).
        # The AI-judged clustering has no merge passes; the AI decides
        # how codes are grouped.
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

  # theme min/max inputs were dead per C1 (AI decides when to
  # stop). The wizard UI may still render them for display; this glue is
  # removed so they don't get wired into the config.

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

  # Add scraping if enabled. Credentials are deliberately omitted -- they come
  # from .Renviron (REDDIT_CLIENT_ID / REDDIT_CLIENT_SECRET), never the config.
  if (isTRUE(input$scraping_enabled)) {
    subs_raw <- val("scraping_subreddits", "")
    subs <- trimws(strsplit(subs_raw, ",")[[1]])
    subs <- subs[nzchar(subs)]
    config$scraping <- list(
      enabled = TRUE,
      subreddits = if (length(subs) > 0) as.list(subs) else NULL,
      posts_per_subreddit = val("scraping_posts", 500),
      include_comments = isTRUE(input$scraping_comments),
      sort_by = val("scraping_sort", "new"),
      time_filter = val("scraping_time", "all")
    )
  } else {
    # Always emit an explicit enabled flag so unchecking the box writes
    # scraping.enabled = FALSE rather than leaving a prior enabled block intact.
    config$scraping <- list(enabled = FALSE)
  }

  config
}

#' Recursively drop NULL leaves from a nested list
#'
#' Used before merging the wizard output over an existing config so a field
#' left blank (which serializes to NULL) does not delete the value already in
#' the file. Returns the list with every NULL element removed at every level.
#' @keywords internal
#' @noRd
.drop_null_leaves <- function(x) {
  if (!is.list(x)) return(x)
  x <- lapply(x, .drop_null_leaves)
  x[!vapply(x, is.null, logical(1))]
}

#' Clear stale subtree keys a per-key merge leaves behind
#'
#' The wizard fully owns the \code{ai} and \code{methodology} subtrees, but
#' \code{modifyList} merges per key, so switching provider or leaving framework
#' mode would otherwise retain the old provider's block or a now-orphaned
#' \code{framework_spec_path}. This drops those so the saved config stays
#' internally consistent with the active provider and mode.
#' @keywords internal
#' @noRd
.normalize_merged_config <- function(config) {
  prov <- config$ai$provider
  if (is.list(config$ai) && !is.null(prov)) {
    config$ai <- config$ai[intersect(names(config$ai), c("provider", prov))]
  }
  if (is.list(config$methodology) &&
      !identical(config$methodology$mode, "framework_applied")) {
    config$methodology$framework_spec_path <- NULL
  }
  config
}
