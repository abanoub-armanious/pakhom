# ==============================================================================
# Data Loading — SQLite, Column Detection, Standardization
# ==============================================================================

#' Explore a SQLite database schema
#'
#' @param db_path Path to .db file
#' @return List with table_names, table_info (list of column details per table),
#'   row_counts (named integer vector)
#' @importFrom DBI dbConnect dbDisconnect dbListTables dbListFields dbGetQuery dbQuoteIdentifier
#' @importFrom RSQLite SQLite
#' @export
explore_database <- function(db_path) {
  if (!file.exists(db_path)) stop("Database not found: ", db_path)

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  tables <- dbListTables(con)

  table_info <- list()
  row_counts <- integer(0)

  for (tbl in tables) {
    fields <- dbListFields(con, tbl)
    safe_tbl <- DBI::dbQuoteIdentifier(con, tbl)
    count <- dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM %s", safe_tbl))$n
    row_counts[tbl] <- count

    table_info[[tbl]] <- list(
      columns = fields,
      row_count = count
    )

    log_info("Table '{tbl}': {count} rows, columns: {paste(fields, collapse=', ')}")
  }

  list(
    table_names = tables,
    table_info = table_info,
    row_counts = row_counts
  )
}

#' Load data from a SQLite database
#'
#' @param db_path Path to database file
#' @param table_name Table to load (NULL = auto-detect largest text table)
#' @param query Custom SQL query (overrides table_name)
#' @return tibble of raw data
#' @export
load_data <- function(db_path, table_name = NULL, query = NULL) {
  if (!file.exists(db_path)) stop("Database not found: ", db_path)

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  if (!is.null(query)) {
    log_info("Executing custom query...")
    data <- as_tibble(dbGetQuery(con, query))
    log_info("Loaded {nrow(data)} rows from custom query")
    return(data)
  }

  if (is.null(table_name)) {
    tables <- dbListTables(con)
    # Prefer content tables
    content_names <- c("posts", "comments", "submissions", "replies",
                        "reviews", "entries", "data")
    match <- tables[tolower(tables) %in% content_names]
    skipped <- setdiff(tables, tables[tolower(tables) %in% content_names])
    if (length(match) > 0) {
      table_name <- match[1]
      if (length(skipped) > 0) {
        log_warn("Auto-excluded {length(skipped)} table(s) not matching content names: {paste(skipped, collapse = ', ')}. Use table_name argument to override.")
      }
    } else {
      # Pick largest table
      counts <- vapply(tables, function(t) {
        safe_t <- DBI::dbQuoteIdentifier(con, t)
        dbGetQuery(con, sprintf("SELECT COUNT(*) as n FROM %s", safe_t))$n
      }, numeric(1))
      table_name <- tables[which.max(counts)]
      log_warn("No standard content tables found ({paste(content_names, collapse = ', ')}). Falling back to largest table: '{table_name}'")
    }
    log_info("Auto-selected table: '{table_name}'")
  }

  safe_table <- DBI::dbQuoteIdentifier(con, table_name)
  data <- as_tibble(dbGetQuery(con, sprintf("SELECT * FROM %s", safe_table)))
  log_info("Loaded {nrow(data)} rows from table '{table_name}'")
  data
}

#' Load and preprocess a corpus from a pakhom configuration
#'
#' One-call corpus preparation: reads the database, applies the
#' configured column mapping, standardizes column names, runs
#' preprocessing, and (optionally) applies test_mode sampling. Returns
#' a tibble with the canonical \code{std_id}, \code{std_text},
#' \code{std_author}, \code{std_timestamp}, \code{original_text} columns
#' (plus any configured metric columns and \code{source_table}).
#'
#' This is the canonical public entry point for users who need the
#' standardized + preprocessed corpus as a stand-alone object,
#' particularly for Mode 1 (\code{\link{run_mode1}}), which requires
#' the researcher to attach \code{theme_membership_*} columns from
#' their external coding workflow (NVivo, ATLAS.ti, MAXQDA, etc.)
#' before invoking the provocateur loop. \code{\link{run_analysis}}
#' (Modes 2/3) calls this same function internally during its Step 2,
#' so the loading code path is canonically identical whether the
#' corpus is consumed by \code{run_analysis} or by \code{run_mode1}.
#'
#' Previously users had to call the internal trio
#' (\code{pakhom:::load_and_combine_tables} +
#' \code{pakhom:::detect_columns} + \code{pakhom:::preprocess_text})
#' to construct a Mode 1 \code{data} argument from a YAML config,
#' which broke the package's no-\code{:::} contract for replicable
#' workflows. This helper closes that gap.
#'
#' @param config Either a \code{ThematicConfig} object (e.g., from
#'   \code{\link{load_config}}) or a length-1 character path to a YAML
#'   config file. If a path, \code{load_config()} is called with no
#'   overrides.
#' @param apply_test_mode Logical; if \code{TRUE} (the default) and
#'   the config's \code{analysis$test_mode$enabled} is set, sample the
#'   corpus down to \code{analysis$test_mode$sample_size} rows using
#'   \code{analysis$test_mode$seed}. Pass \code{FALSE} to skip
#'   sampling and return the full preprocessed corpus regardless of
#'   the test_mode config -- useful when the same config drives both
#'   a test_mode dry-run and a full Mode 1 ingestion.
#' @return A tibble of standardized + preprocessed entries with
#'   \code{std_id}, \code{std_text}, \code{std_author},
#'   \code{std_timestamp}, \code{original_text}, plus any metric
#'   columns identified by the column mapping, plus \code{source_table}.
#' @seealso \code{\link{load_config}} (parse a YAML config to
#'   ThematicConfig); \code{\link{run_mode1}} (Mode 1 entry point that
#'   consumes the returned tibble after the researcher attaches
#'   \code{theme_membership_*} columns); \code{\link{run_analysis}}
#'   (Modes 2/3 entry point that loads the corpus internally via this
#'   function); \code{vignette("methodology-modes")} for a Mode 1
#'   worked example.
#' @examples
#' \dontrun{
#'   # Mode 1 (Reflexive Scaffold) workflow: load the standardized
#'   # corpus from config, attach researcher-authored theme
#'   # memberships, then run the provocateur loop.
#'   cfg    <- load_config("config.yaml")   # methodology = reflexive_scaffold
#'   corpus <- load_corpus_from_config(cfg)
#'
#'   # Attach theme_membership_* columns from your external coding
#'   # tool (NVivo / ATLAS.ti / MAXQDA export). In Mode 1 pakhom
#'   # never authors themes -- you do, in your own workflow.
#'   corpus$theme_membership_Adherence  <- as.integer(corpus$std_id %in% ids_a)
#'   corpus$theme_membership_Resistance <- as.integer(corpus$std_id %in% ids_r)
#'
#'   themes <- create_theme_set(list(
#'     list(id = 1, name = "Adherence",
#'          description = "Researcher-authored: medication adherence",
#'          codes_included = c("med_routine", "daily_pills"))
#'   ))
#'
#'   result <- run_mode1(data = corpus, theme_set = themes, config = cfg)
#' }
#' @export
load_corpus_from_config <- function(config, apply_test_mode = TRUE) {
  if (is.character(config) && length(config) == 1L) {
    config <- load_config(config)
  }
  if (!inherits(config, "ThematicConfig") && !is.list(config)) {
    stop("load_corpus_from_config: `config` must be a ThematicConfig ",
         "object (from load_config()) or a character path to a YAML ",
         "config file", call. = FALSE)
  }
  if (is.null(config$data) || is.null(config$data$database)) {
    stop("load_corpus_from_config: config$data$database is required",
         call. = FALSE)
  }
  if (is.null(config$data$tables) || length(config$data$tables) == 0L) {
    stop("load_corpus_from_config: config$data$tables must list at ",
         "least one table", call. = FALSE)
  }

  db_path <- config$data$database
  tables  <- config$data$tables

  # Match run_analysis Step 2 exactly so the loading path stays
  # canonical. Multi-table loads use load_and_combine_tables;
  # single-table loads need an explicit source_table column for
  # downstream consumers (export_theme_entry_csvs,
  # aggregate_overall_statistics) which silently degrade without it.
  if (length(tables) > 1L) {
    data <- load_and_combine_tables(db_path, tables,
                                     source_type = config$data$source_type,
                                     config = config$data)
  } else {
    raw <- load_data(db_path, tables[1])
    col_map <- detect_columns(raw, config$data$source_type, config$data)
    data <- standardize_data(raw, col_map)
    data$source_table <- tables[1]
  }

  preprocess_config <- config$data$preprocessing
  preprocess_config$source_type <- config$data$source_type
  data <- preprocess_text(data, preprocess_config)
  log_info("Corpus loaded: {nrow(data)} entries")

  if (isTRUE(apply_test_mode) && isTRUE(config$analysis$test_mode$enabled)) {
    test_n <- config$analysis$test_mode$sample_size %||% 100
    test_seed <- config$analysis$test_mode$seed %||% 42
    if (test_n < nrow(data)) {
      set.seed(test_seed)
      data <- data[sample(nrow(data), test_n), ]
      log_info("TEST MODE: sampled {test_n} entries (seed={test_seed})")
    }
  }

  data
}

#' Load and combine multiple tables from a SQLite database
#'
#' Each table is independently column-mapped and standardized, then combined.
#'
#' @param db_path Path to database
#' @param table_names Character vector of table names
#' @param source_type Platform type for column mapping
#' @param config Full ThematicConfig (for column_mappings)
#' @return Standardized tibble with source_table column
load_and_combine_tables <- function(db_path, table_names, source_type = "reddit",
                                     config = NULL) {
  if (!file.exists(db_path)) stop("Database not found: ", db_path)

  con <- dbConnect(SQLite(), db_path)
  on.exit(dbDisconnect(con), add = TRUE)

  all_tables <- dbListTables(con)
  valid_tables <- table_names[table_names %in% all_tables]

  if (length(valid_tables) == 0) {
    stop("None of the specified tables exist: ", paste(table_names, collapse = ", "))
  }

  standardized <- list()

  for (tbl in valid_tables) {
    log_info("Loading table: {tbl}")
    safe_tbl <- DBI::dbQuoteIdentifier(con, tbl)
    raw <- as_tibble(dbGetQuery(con, sprintf("SELECT * FROM %s", safe_tbl)))

    if (nrow(raw) == 0) {
      log_warn("Table '{tbl}' is empty, skipping")
      next
    }

    col_map <- detect_columns(raw, source_type, config)
    std <- standardize_data(raw, col_map) |>
      mutate(source_table = tbl)

    standardized[[tbl]] <- std
    log_info("  Loaded {nrow(raw)} rows from '{tbl}'")
  }

  if (length(standardized) == 0) stop("No data loaded from any table")

  # union-then-NA-fill, not intersect-then-drop.
  #
  # The earlier path used `Reduce(intersect, lapply(standardized, names))`
  # and dropped any column not shared across every input table. For the
  # canonical Reddit shape (posts table has num_comments + upvote_ratio;
  # comments table doesn't) this silently dropped two of the three metric
  # columns from the analytic data, so paper-style subtheme
  # tables and correlations were only ever scored on `score`. dplyr's
  # `bind_rows()` already does NA-fill for missing columns; all that's needed is
  # to stop pre-filtering. Log columns that were NA-filled so the user
  # has explicit signal about partial coverage.
  per_table_cols <- lapply(standardized, names)
  all_cols       <- Reduce(union, per_table_cols)
  shared_cols    <- Reduce(intersect, per_table_cols)
  partial_cols   <- setdiff(all_cols, shared_cols)
  if (length(partial_cols) > 0L) {
    log_info(paste0(
      "Columns present in some input tables but not others (NA-filled in ",
      "combined data): ", paste(partial_cols, collapse = ", ")
    ))
  }
  combined <- bind_rows(standardized)

  # Defense-in-depth: downstream callers (coding_state,
  # fabrication log, per-theme exports, quote provenance) all use
  # std_id as a primary key. If duplicates slip through (e.g., a
  # column-mapping misconfiguration that pulls a non-unique field, or
  # legitimate id collisions across input tables), every consumer
  # silently corrupts. Detect duplicates here and auto-recover by
  # prefixing std_id with the source_table name. Cannot silently
  # continue.
  if ("std_id" %in% names(combined)) {
    n_dup <- sum(duplicated(combined$std_id))
    if (n_dup > 0L) {
      log_warn(paste0(
        "Detected {n_dup} duplicate std_id value(s) after combining tables. ",
        "Auto-recovering by prefixing std_id with source_table ",
        "(e.g., 'posts:abc123', 'comments:def456'). If this is unexpected, ",
        "check your column_mappings -- a non-unique id column may have ",
        "been picked. To suppress: set explicit_columns$id_column to a ",
        "row-unique field in your config."
      ))
      if ("source_table" %in% names(combined)) {
        combined$std_id <- paste0(combined$source_table, ":", combined$std_id)
        # Re-check; if duplicates persist after prefixing, REFUSE -- the
        # input is structurally broken (same id appearing twice within a
        # single table, which standardize_data should not allow).
        n_dup2 <- sum(duplicated(combined$std_id))
        if (n_dup2 > 0L) {
          stop(sprintf(paste0(
            "After auto-prefixing std_id with source_table, %d duplicate(s) ",
            "remain. This means a single source table has internally ",
            "duplicate id values -- the input data is corrupt. Inspect the ",
            "table that contains the duplicates and fix at the source."
          ), n_dup2), call. = FALSE)
        }
      } else {
        # No source_table column means prefixing isn't safe. Refuse
        # rather than ship corruption.
        stop(sprintf(paste0(
          "%d duplicate std_id value(s) after combining and no ",
          "source_table column available to disambiguate. Set ",
          "explicit_columns$id_column to a row-unique field."
        ), n_dup), call. = FALSE)
      }
    }
  }

  log_info("Combined {nrow(combined)} entries from {length(standardized)} tables")
  combined
}

#' Detect and map columns based on platform type
#'
#' Searches for known column name patterns and returns a mapping.
#'
#' @param data tibble to inspect
#' @param source_type Platform identifier ("reddit", "drugscom", "generic")
#' @param config ThematicConfig (uses data.column_mappings if present)
#' @return Named list with id, text, author, timestamp, metrics mappings
detect_columns <- function(data, source_type = "reddit", config = NULL) {
  available_cols <- names(data)

  # --- Check for explicit column mapping first ---
  explicit <- NULL
  if (!is.null(config)) {
    if (inherits(config, "ThematicConfig")) {
      explicit <- config$data$explicit_columns
    } else if (is.list(config)) {
      explicit <- config$explicit_columns
      if (is.null(explicit) && is.list(config$data)) {
        explicit <- config$data$explicit_columns
      }
    }
  }

  if (!is.null(explicit) && !is.null(explicit$text_column)) {
    log_info("Using explicit column mapping from config")
    if (!explicit$text_column %in% available_cols) {
      stop("Explicit text_column '", explicit$text_column,
           "' not found. Available: ", paste(available_cols, collapse = ", "))
    }
    mapping <- list(
      id = explicit$id_column %||% NA_character_,
      text = explicit$text_column,
      author = explicit$author_column %||% NA_character_,
      timestamp = explicit$timestamp_column %||% NA_character_,
      metrics = explicit$metric_columns %||% character(0)
    )
    # Validate referenced columns exist
    for (field in c("id", "author", "timestamp")) {
      val <- mapping[[field]]
      if (!is.na(val) && !val %in% available_cols) {
        log_warn("Explicit {field} column '{val}' not found, setting to NA")
        mapping[[field]] <- NA_character_
      }
    }
    mapping$metrics <- mapping$metrics[mapping$metrics %in% available_cols]
    log_info("Column mapping: id={mapping$id}, text={mapping$text}, author={mapping$author}")
    return(mapping)
  }

  # --- Auto-detection fallback ---
  # Get mapping candidates — config may be ThematicConfig or the $data subsection
  col_mappings <- NULL
  if (!is.null(config)) {
    if (inherits(config, "ThematicConfig")) {
      col_mappings <- config$data$column_mappings
    } else if (is.list(config)) {
      col_mappings <- config$column_mappings
      if (is.null(col_mappings) && is.list(config$data)) {
        col_mappings <- config$data$column_mappings
      }
    }
  }

  if (!is.null(col_mappings) && !is.null(col_mappings[[source_type]])) {
    candidates <- col_mappings[[source_type]]
  } else {
    candidates <- .default_column_mappings()[[source_type]]
    if (is.null(candidates)) candidates <- .default_column_mappings()$generic
  }

  mapping <- list()

  # Match each field to the first available column
  for (field in c("id", "text", "author", "timestamp")) {
    matched <- FALSE
    for (candidate in candidates[[field]]) {
      # Case-insensitive match
      match_idx <- which(tolower(available_cols) == tolower(candidate))
      if (length(match_idx) > 0) {
        mapping[[field]] <- available_cols[match_idx[1]]
        matched <- TRUE
        break
      }
    }
    if (!matched) {
      if (field == "text") {
        stop("Could not find a text column. Available: ", paste(available_cols, collapse = ", "))
      }
      mapping[[field]] <- NA_character_
    }
  }

  # Metrics: find all available
  metric_matches <- character(0)
  for (candidate in candidates$metrics) {
    match_idx <- which(tolower(available_cols) == tolower(candidate))
    if (length(match_idx) > 0) {
      metric_matches <- c(metric_matches, available_cols[match_idx[1]])
    }
  }
  mapping$metrics <- metric_matches

  log_info("Column mapping: id={mapping$id}, text={mapping$text}, author={mapping$author}")
  mapping
}

#' Standardize data to common schema
#'
#' @param data Raw tibble
#' @param column_map Result of detect_columns()
#' @return tibble with std_id, std_text, std_author, std_timestamp, + original metrics
standardize_data <- function(data, column_map) {
  std <- data

  # Map required columns
  if (!is.na(column_map$id)) {
    std$std_id <- as.character(std[[column_map$id]])
    # std_id is the primary key for coding, quote provenance, IRR joins, and
    # cross-run comparison; a non-unique id column silently corrupts all of
    # them (entries collide on the key). Fail loudly rather than mis-key. (The
    # multi-table combine path auto-recovers by prefixing with source_table; a
    # single table has no such fallback, so point the user at the fix.)
    n_dup <- sum(duplicated(std$std_id))
    if (n_dup > 0L) {
      dups <- unique(std$std_id[duplicated(std$std_id)])
      stop(sprintf(
        paste0("standardize_data: id column '%s' has %d duplicate value(s) ",
               "(e.g. %s). std_id must be row-unique -- it is the primary key ",
               "for coding, quote provenance, and cross-run joins. Point ",
               "explicit_columns$id_column at a row-unique field, or omit it to ",
               "auto-number rows."),
        column_map$id, n_dup, paste(utils::head(dups, 3), collapse = ", ")),
        call. = FALSE)
    }
  } else {
    std$std_id <- as.character(seq_len(nrow(std)))
  }

  std$std_text <- as.character(std[[column_map$text]])

  if (!is.na(column_map$author)) {
    std$std_author <- as.character(std[[column_map$author]])
  } else {
    std$std_author <- NA_character_
  }

  if (!is.na(column_map$timestamp)) {
    ts_col <- std[[column_map$timestamp]]
    # Detect Unix epoch timestamps (numeric values in plausible range)
    if (is.numeric(ts_col) && length(ts_col) > 0) {
      sample_val <- ts_col[!is.na(ts_col)][1]
      if (!is.na(sample_val) && sample_val > 1e9 && sample_val < 2e10) {
        log_info("Detected Unix epoch timestamps in '{column_map$timestamp}', converting to POSIXct")
        std$std_timestamp <- as.character(as.POSIXct(ts_col, origin = "1970-01-01", tz = "UTC"))
      } else {
        std$std_timestamp <- as.character(ts_col)
      }
    } else {
      std$std_timestamp <- as.character(ts_col)
    }
  } else {
    std$std_timestamp <- NA_character_
  }

  # Keep original text for quotes in reports
  std$original_text <- std$std_text

  # Select standardized + metric columns
  keep_cols <- c("std_id", "std_text", "std_author", "std_timestamp", "original_text")
  keep_cols <- c(keep_cols, column_map$metrics)
  keep_cols <- keep_cols[keep_cols %in% names(std)]

  std |> select(all_of(keep_cols))
}

#' Default column mappings for common platforms
#' @keywords internal
.default_column_mappings <- function() {
  defaults <- .config_defaults()
  defaults$data$column_mappings
}
