# Enrich themes with entry counts, sentiment, and quotes

Enrich themes with entry counts, sentiment, and quotes

## Usage

``` r
enrich_themes(theme_set, data, coding_state = NULL, quotes_per_theme = 3L)
```

## Arguments

- theme_set:

  ThemeSet object

- data:

  Tibble with theme_membership\_\* and sentiment columns

- coding_state:

  ProgressiveCodingState (optional)

- quotes_per_theme:

  Integer; number of representative quotes to select per theme. Wired
  through from `config$analysis$themes$quotes_per_theme`; defaults to 3.

## Value

Enriched ThemeSet
