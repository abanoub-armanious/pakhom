# Load data from a SQLite database

Load data from a SQLite database

## Usage

``` r
load_data(db_path, table_name = NULL, query = NULL)
```

## Arguments

- db_path:

  Path to database file

- table_name:

  Table to load (NULL = auto-detect largest text table)

- query:

  Custom SQL query (overrides table_name)

## Value

tibble of raw data
