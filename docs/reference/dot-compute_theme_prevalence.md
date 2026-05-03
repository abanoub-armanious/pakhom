# Compute theme prevalence for each time period

Groups entries by period, then counts how many entries belong to each
theme within that period.

## Usage

``` r
.compute_theme_prevalence(data, theme_set, period_type)
```

## Arguments

- data:

  Tibble with `std_timestamp`, `emerged_themes` (and/or
  `theme_membership_*` columns), and a `.period` column already
  attached.

- theme_set:

  ThemeSet object

- period_type:

  Character period type (for column reference only)

## Value

Tibble with columns: `period`, `theme_name`, `n_entries`,
`pct_of_period`, `total_in_period`
