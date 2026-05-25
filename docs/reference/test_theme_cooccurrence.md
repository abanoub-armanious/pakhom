# Test theme co-occurrence patterns with chi-square tests of independence

For each pair of themes, tests whether co-occurrence is significantly
different from expected by chance.

## Usage

``` r
test_theme_cooccurrence(
  data,
  theme_set,
  min_expected = 5,
  min_theme_entries = 5L,
  min_observed_both = 1L
)
```

## Arguments

- data:

  Tibble with theme_membership\_\* columns

- theme_set:

  ThemeSet object

- min_expected:

  Minimum expected cell count for chi-square (default 5)

- min_theme_entries:

  Integer; themes with fewer than this many positive entries are
  excluded. Default 5L, matching the correlation matrix + theme-group
  test default.

- min_observed_both:

  Integer; M-10 polish. Pairs whose observed co-occurrence is below this
  count are skipped (Fisher tests on zero-co-occurrence pairs are
  uninterpretable). Default 1L.

## Value

Tibble with co-occurrence test results

## Details

Phase 58 Tier 6 H-16: applies the same `min_theme_entries` filter that
`prepare_correlation_data` and `compare_theme_groups` use, so the three
statistical layers report counts over a consistent theme cohort.
Pre-Phase-58 this function admitted every theme regardless of frequency,
which produced thousands of degenerate Fisher tests on rare themes (the
Phase 57 audit found 99.1% of Fisher pairs had `observed_both = 0`).
