# Compute when each theme first appeared in the data

For each theme, finds the earliest `std_timestamp` among entries that
belong to that theme. When `coding_state` is supplied, also records the
timestamp of the first constituent code creation.

## Usage

``` r
.compute_theme_emergence(data, theme_set, coding_state)
```

## Arguments

- data:

  Tibble with `.parsed_ts` and theme assignment columns

- theme_set:

  ThemeSet object

- coding_state:

  ProgressiveCodingState (or NULL)

## Value

Tibble with columns: `theme_name`, `first_appearance_date`,
`first_code_date`, `n_codes_at_emergence`
