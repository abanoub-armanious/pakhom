# Fuzzy-match themes between two runs

Fuzzy-match themes between two runs

## Usage

``` r
.match_themes_pairwise(themes_a, themes_b, threshold = 0.75)
```

## Arguments

- themes_a:

  tibble of themes from previous run

- themes_b:

  tibble of themes from current run

- threshold:

  Combined similarity threshold

## Value

List with persisted, new, disappeared
