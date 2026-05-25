# Compute per-theme statistics for a Mode 1 run

Mode 1's analog of `aggregate_theme_statistics` for Mode 2/3. Mode 1 has
no sentiment / emotions / intensity (the AI didn't run coding or
sentiment over the corpus – the researcher did), so this helper returns
only what is meaningful in Mode 1: per-theme entry count, T0.2
participant spread, and provocation rollups (count by category + total +
drop count).

## Usage

``` r
compute_mode1_theme_stats(
  data,
  theme_set,
  reflection_log,
  quotes_per_theme = 3L,
  config = NULL
)
```

## Arguments

- data:

  Tibble with std_id, std_text, plus theme_membership\_\* columns or an
  emerged_themes column. Must carry std_author when T0.2 spread is
  desired.

- theme_set:

  Researcher-authored ThemeSet.

- reflection_log:

  Populated ResearcherReflectionLog (post provocateur loop).

- quotes_per_theme:

  Integer; number of representative quotes to select per theme. Wired
  through from `config$analysis$themes$quotes_per_theme`; defaults to 3.

- config:

  Optional ThematicConfig (or config list). When supplied (Phase 55),
  `config$data$column_mappings$metric_columns` is used to detect
  dataset-specific metric columns for the per-theme Median(MAD) +
  Mean(SD) summary line in the Mode 1 report. When NULL or empty,
  metrics auto-detect from the data via
  [`.detect_metric_columns`](https://abanoub-armanious.github.io/pakhom/reference/dot-detect_metric_columns.md).

## Value

Named list keyed by theme name. Each value carries `n_entries`,
`participant_spread`, `provocations` (count by category + total),
`quotes` (raw representative quotes – NOT sentiment-sorted because Mode
1 has no sentiment), plus Phase 55 fields: `metric_cols` (character
vector) and `metric_stats` (named list of per-metric Median/MAD/Mean/SD/
n_observed records).
