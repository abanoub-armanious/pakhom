# Detect and map columns based on platform type

Searches for known column name patterns and returns a mapping.

## Usage

``` r
detect_columns(data, source_type = "reddit", config = NULL)
```

## Arguments

- data:

  tibble to inspect

- source_type:

  Platform identifier ("reddit", "drugscom", "generic")

- config:

  ThematicConfig (uses data.column_mappings if present)

## Value

Named list with id, text, author, timestamp, metrics mappings
