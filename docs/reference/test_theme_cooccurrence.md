# Test theme co-occurrence patterns with chi-square tests of independence

For each pair of themes, tests whether co-occurrence is significantly
different from expected by chance.

## Usage

``` r
test_theme_cooccurrence(data, theme_set, min_expected = 5)
```

## Arguments

- data:

  Tibble with theme_membership\_\* columns

- theme_set:

  ThemeSet object

- min_expected:

  Minimum expected cell count for chi-square (default 5)

## Value

Tibble with co-occurrence test results
