# Remove themes with zero assigned entries after enrichment

Call after enrich_themes() to drop themes that no data was mapped to.
Re-numbers theme IDs sequentially after pruning.

## Usage

``` r
prune_empty_themes(theme_set)
```

## Arguments

- theme_set:

  ThemeSet object (enriched, with entry_count populated)

## Value

ThemeSet with empty themes removed
