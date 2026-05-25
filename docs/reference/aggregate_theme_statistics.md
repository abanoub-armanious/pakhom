# Aggregate per-theme statistics for report

Aggregate per-theme statistics for report

## Usage

``` r
aggregate_theme_statistics(
  data,
  theme_set,
  consolidated = NULL,
  quotes_per_theme = 3L,
  config = NULL
)
```

## Arguments

- data:

  tibble with theme_membership\_\* or emerged_themes columns

- theme_set:

  ThemeSet object

- consolidated:

  ConsolidatedCodes list (or NULL)

- quotes_per_theme:

  Integer; number of representative quotes to select per theme. Wired
  through from `config$analysis$themes$quotes_per_theme`; defaults to 3.

- config:

  Optional ThematicConfig or config list. When supplied,
  `config$data$column_mappings$metric_columns` is used as the explicit
  metric allowlist for the per-subtheme paper-style tables (Phase 55).
  When NULL or empty, metrics auto-detect from the data via
  [`.detect_metric_columns`](https://abanoub-armanious.github.io/pakhom/reference/dot-detect_metric_columns.md).

## Value

Named list of theme stats (one per theme). Each theme entry carries a
`subtheme_stats` list (Phase 55) with one element per real subtheme: n,
per-metric Median(MAD) + Mean(SD), and metric-tagged example quotes –
the paper-style per-subtheme rows the report renders into a table inside
each theme's card.
