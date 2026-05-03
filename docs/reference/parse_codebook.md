# Parse a QDA software codebook export (NVivo, ATLAS.ti, MAXQDA, or generic)

Auto-detects the QDA tool format from column headers and sheet
structure, then extracts code names, hierarchy, frequencies, and
descriptions into a standardized tibble.

## Usage

``` r
parse_codebook(path)
```

## Arguments

- path:

  Path to codebook file (.xlsx, .xls, or .csv)

## Value

tibble with columns: code_name, parent_code, frequency, n_sources,
description, hierarchy_level. Returns NULL if parsing fails.
