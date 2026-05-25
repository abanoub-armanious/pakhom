# Create correlation plot

Renders a correlation heatmap for small matrices, OR a top-N effect-size
lollipop chart for large matrices (Phase 58 Tier 5 C-10). Pre-Phase-58
the corrplot heatmap was unconditional, producing a 14,280x14,280 PNG
(4.8 MB, browser-illegible) on the 228-variable Phase 57 saturation run.
Above the `max_inline_vars` threshold the function now switches to a
ggplot2 horizontal lollipop showing the top-N pairs ranked by absolute
correlation, with significance encoded by point color.

## Usage

``` r
create_correlation_plot(
  results,
  output_path,
  methodology_mode = NULL,
  run_id = NULL,
  max_inline_vars = 30L
)
```

## Arguments

- results:

  CorrelationResults from calculate_correlations()

- output_path:

  File path for PNG output

- methodology_mode:

  Optional character (T1.7 / AC4): when supplied, adds a footer caption
  identifying the methodology mode + run.

- run_id:

  Optional character: run identifier.

- max_inline_vars:

  Integer; correlation matrices with more variables than this render as
  a top-N lollipop instead of a heatmap. Default 30L.
