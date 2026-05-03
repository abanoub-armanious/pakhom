# Analyse temporal patterns in theme prevalence within a single run

Requires the data to contain a `std_timestamp` column (character,
parseable as dates). Detects the appropriate time granularity, computes
theme prevalence per period, and builds an emergence timeline showing
when each theme first appeared in the dataset.

## Usage

``` r
analyze_temporal_patterns(data, theme_set, coding_state = NULL)
```

## Arguments

- data:

  Tibble with at least `std_timestamp` and theme assignment columns
  (`emerged_themes` and/or `theme_membership_*`).

- theme_set:

  ThemeSet object

- coding_state:

  ProgressiveCodingState (or NULL)

## Value

A list with elements:

- prevalence_over_time:

  Tibble: period, theme_name, n_entries, pct_of_period, total_in_period

- emergence_timeline:

  Tibble: theme_name, first_appearance_date, first_code_date,
  n_codes_at_emergence

- period_type:

  Character: "daily", "weekly", "monthly", or "quarterly"

- has_temporal_data:

  Logical: TRUE when usable timestamps exist
