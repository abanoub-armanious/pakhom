# Build a caption string suitable for use as a ggplot watermark

Designed to be passed to `ggplot2::labs(caption = ...)` so every plot
the report generates carries the methodology stamp. Caption is small,
gray, italic by ggplot's default theme – visible but unobtrusive.

## Usage

``` r
methodology_plot_caption(mode, run_id = NULL)
```

## Arguments

- mode:

  Character methodology mode.

- run_id:

  Optional run identifier.

## Value

Character (single line).
