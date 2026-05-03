# Load and combine multiple tables from a SQLite database

Each table is independently column-mapped and standardized, then
combined.

## Usage

``` r
load_and_combine_tables(
  db_path,
  table_names,
  source_type = "reddit",
  config = NULL
)
```

## Arguments

- db_path:

  Path to database

- table_names:

  Character vector of table names

- source_type:

  Platform type for column mapping

- config:

  Full ThematicConfig (for column_mappings)

## Value

Standardized tibble with source_table column
