# Top-N effect-size lollipop chart for large correlation matrices

Phase 58 Tier 5 C-10 fallback: when the variable count exceeds the
heatmap legibility threshold (`max_inline_vars`), render the top-N
unique pairs by `|r|` as a horizontal lollipop. Pairs are extracted from
the upper triangle of the correlation matrix (each pair appears once).
Significance, when available, is encoded by point color
(Bonferroni-adjusted `p < 0.05` vs not).

## Usage

``` r
.create_correlation_lollipop(
  cm,
  pa,
  output_path,
  top_n,
  n_total_vars,
  methodology_mode = NULL,
  run_id = NULL
)
```

## Arguments

- cm:

  Correlation matrix (rownames and colnames already humanized by the
  caller).

- pa:

  Adjusted-p matrix aligned to `cm`; NAs treated as non-significant.

- output_path:

  File path for PNG output.

- top_n:

  Integer; number of top pairs to show.

- n_total_vars:

  Integer; total variables in the underlying matrix (used in the
  subtitle to make the filter explicit).

- methodology_mode:

  AC4 caption.

- run_id:

  AC4 caption.
