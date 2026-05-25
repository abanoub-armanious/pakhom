# Export per-theme paper-style subtheme-summary CSVs (Phase 55)

Complement to `export_theme_entry_csvs`: for each theme, writes a CSV
with ONE ROW PER REAL SUBTHEME with paper-style columns (Subtheme name,
description, n, Median+MAD + Mean+SD per auto- detected metric, examples
of comments tagged with metric values).

## Usage

``` r
export_theme_subtheme_summary_csvs(
  theme_stats,
  output_dir,
  methodology_mode = NULL
)
```

## Arguments

- theme_stats:

  Per-theme stats list from
  [`aggregate_theme_statistics()`](https://abanoub-armanious.github.io/pakhom/reference/aggregate_theme_statistics.md)
  (Phase 55+: must carry `subtheme_stats` + `metric_cols`).

- output_dir:

  Run directory.

- methodology_mode:

  Optional methodology mode for AC4 stamping.

## Value

Named list of file info per theme.

## Details

Output structure:

- `theme_summaries/<safe_theme_name>.csv` – one per theme with non-empty
  subtheme_stats

- `theme_summaries/all_subthemes.csv` – master with theme_name +
  subtheme rows from every theme

Themes with no real subthemes (only the virtual NA-named wrapper) OR
with empty subtheme_stats are skipped – they're already covered by the
per-entry CSVs and the theme card.
