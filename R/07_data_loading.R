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

  # Phase 58 Tier 0 C-9: union-then-NA-fill, not intersect-then-drop.
  #
  # The pre-Phase-58 path used `Reduce(intersect, lapply(standardized, names))`
  # and dropped any column not shared across every input table. For the
  # canonical Reddit shape (posts table has num_comments + upvote_ratio;
  # comments table doesn't) this silently dropped two of the three metric
  # columns from the analytic data, so Phase 55 paper-style subtheme
  # tables and correlations were only ever scored on `score`. dplyr's
  # `bind_rows()` already does NA-fill for missing columns; we just need
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

  # Defense-in-depth (phase 39): downstream callers (coding_state,
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
        # No source_table column means we can't prefix safely. Refuse
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
