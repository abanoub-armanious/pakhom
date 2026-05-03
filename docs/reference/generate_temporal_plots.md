# Generate PNG plots for temporal analysis results

Creates two publication-quality plots:

1.  `temporal_prevalence.png` – line chart showing theme prevalence (\\

2.  `temporal_emergence.png` – lollipop/timeline chart showing when each
    theme first appeared in the data.

## Usage

``` r
generate_temporal_plots(temporal_results, output_dir)
```

## Arguments

- temporal_results:

  List returned by
  [`analyze_temporal_patterns`](analyze_temporal_patterns.md)

- output_dir:

  Directory where PNGs will be saved (created if needed)

## Value

Invisible character vector of file paths written
