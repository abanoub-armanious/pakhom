# Compute participant spread metrics for a theme's entries

Compute participant spread metrics for a theme's entries

## Usage

``` r
.compute_participant_spread(entries)
```

## Arguments

- entries:

  tibble of entries belonging to this theme (must have a `std_author`
  column when participant spread is desired; when the column is missing
  or all NA, returns the empty-shape list).

## Value

Named list:

- `n_distinct_contributors`: integer count of unique non-NA `std_author`
  values

- `contributor_gini`: Gini coefficient (`NA_real_` when there are no
  contributors or only one)

- `top_contributor_share`: fraction of entries from the single most
  prolific contributor (`NA_real_` when no contributors)

- `available`: logical – TRUE when `std_author` was usable, FALSE when
  absent or all NA. Lets downstream rendering distinguish "no data" from
  "data shows even spread".
