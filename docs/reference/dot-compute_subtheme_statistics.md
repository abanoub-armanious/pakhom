# Compute per-subtheme statistics for a theme (paper-style)

For each REAL (non-virtual, named) subtheme of `theme`, returns a record
carrying:

- `name`: subtheme name

- `description`: subtheme description (1-2 sentences)

- `n`: entries that contributed to this subtheme

- `metric_stats`: list keyed by metric column name; each entry has
  `median`, `mad`, `mean`, `sd`

- `example_quotes`: character vector of representative quotes tagged
  with per-entry metric values (e.g.
  `"<quote text>... [<metric_a>: 8; <metric_b>: 12]"`)

## Usage

``` r
.compute_subtheme_statistics(
  theme,
  data,
  metric_cols,
  quotes_per_subtheme = 3L
)
```

## Arguments

- theme:

  A theme list (one element of `theme_set$themes`)

- data:

  Analytical tibble with theme_membership\_\* + subtheme_assignments
  columns

- metric_cols:

  Character vector of metric column names (from
  `.detect_metric_columns`)

- quotes_per_subtheme:

  Integer; default 3

## Value

Named list (one per real subtheme) of stat records

## Details

Virtual NA-named subtheme wrappers (added by the ThemeSet hierarchy for
themes without AI-clustered subthemes) are skipped. Themes with only
virtual subthemes return an empty list – the renderer falls back to the
theme-level summary in that case.

Membership: an entry belongs to subtheme S if its `subtheme_assignments`
column (populated by `cascade_theme_assignments`) contains S's name.
When that column is absent we fall back to "every entry in the theme is
in every subtheme" (degenerate but non-fatal – the table still renders,
just without entry-level filtering).
