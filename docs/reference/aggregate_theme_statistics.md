# Aggregate per-theme statistics for report

Aggregate per-theme statistics for report

## Usage

``` r
aggregate_theme_statistics(data, theme_set, consolidated = NULL)
```

## Arguments

- data:

  tibble with theme_membership\_\* or emerged_themes columns

- theme_set:

  ThemeSet object

- consolidated:

  ConsolidatedCodes list (or NULL)

## Value

Named list of theme stats (one per theme)
