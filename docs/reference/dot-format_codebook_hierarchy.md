# Format a codebook tibble as a human-readable hierarchy string

Works with both deep QDPX codebooks (with hierarchy_level, is_codable,
is_discarded columns) and simpler codebooks (with parent_code column).
Dynamically adapts to whatever structure is available.

## Usage

``` r
.format_codebook_hierarchy(cb, max_chars = 5000)
```

## Arguments

- cb:

  Codebook tibble (either codebook_full from QDPX or basic codebook)

- max_chars:

  Maximum characters in output

## Value

Character string with formatted hierarchy
