# Render the per-theme, per-subtheme summary table (Phase 55)

Returns an HTML/Markdown block containing a table with one row per real
(non-virtual) subtheme of the theme:

- **Subtheme** – subtheme name + description (truncated)

- **n** – entries in this subtheme

- For each auto-detected metric column: two cells, Median(MAD) and
  Mean(SD), formatted as "

  ()".

- **Examples of comments** – up to N representative quotes
  (sentiment-positioned when sentiment_score is available), each tagged
  with the source entry's metric values as
  `[<metric_a>: 8; <metric_b>: 12]`.

## Usage

``` r
.build_subtheme_summary_table(ts)
```

## Arguments

- ts:

  Per-theme stats object from `aggregate_theme_statistics` (must carry
  `subtheme_stats` + `metric_cols`, Phase 55+).

## Value

Character HTML+markdown string for the table block.

## Details

Returns the empty string when:

- the theme has no real subthemes (only the virtual NA-named wrapper
  from the Phase 51 hierarchy), *OR*

- the dataset has no detectable metric columns AND no real subthemes.

If subthemes exist but no metric columns do, the table renders with just
the Subtheme, n, and Examples columns – still useful for surfacing the
hierarchy.
