# Detect metric columns in a data frame, dataset-agnostically

Returns the names of numeric columns in `data` that can sensibly be
summarized as quantitative metrics in per-theme + per-subtheme tables.
Package-internal columns (those pakhom engineers itself –
sentiment_score, emotion_intensity, theme_membership\_\*, etc.) are
excluded; everything else numeric is a candidate.

## Usage

``` r
.detect_metric_columns(data, config = NULL, explicit = NULL)
```

## Arguments

- data:

  tibble with the analytical data (post-cascade)

- config:

  Optional ThematicConfig or config list

- explicit:

  Optional character vector of metric column names to use verbatim
  (intersected with the data's columns). Bypasses the config dig. Used
  by `compute_correlations` which already has a flat
  `config$metric_columns` field at hand.

## Value

Character vector of metric column names (possibly empty)

## Details

Explicit override path: when
`config$data$column_mappings$metric_columns` is non-empty, those names
are used verbatim (intersected with the data's columns to avoid
referencing missing fields). This matches the explicit
[`detect_columns()`](https://abanoub-armanious.github.io/pakhom/reference/detect_columns.md)
mapping path in `R/07_data_loading.R`.

Mirrors (and consolidates) the inline detection in
`compute_correlations` (R/14_correlations.R:82-106; Phase 50b). Future
cleanup: refactor that site to call this helper.

## Caveat – sentiment_score collision

The auto-detect path excludes `sentiment_score` (the package- engineered
column from R/10_sentiment.R) by name. If a user's corpus happens to
have its own `sentiment_score` numeric column that they want treated AS
A METRIC, the auto-detect silently drops it. Workaround: supply an
explicit override via `config$data$column_mappings$metric_columns` (or
the direct `explicit=` arg) – the override path returns the requested
columns verbatim, bypassing the internal-column exclusion. The same
collision applies to any other internal name (`emotion_intensity`,
`n_themes`, etc.).
