# Generate PNG plots for temporal analysis results

Creates two publication-quality plots:

1.  `temporal_prevalence.png` – line chart showing theme prevalence (\\

2.  `temporal_emergence.png` – lollipop/timeline chart showing when each
    theme first appeared in the data.

## Usage

``` r
generate_temporal_plots(
  temporal_results,
  output_dir,
  methodology_mode = NULL,
  run_id = NULL,
  max_inline_themes = 30L
)
```

## Arguments

- temporal_results:

  List returned by
  [`analyze_temporal_patterns`](https://abanoub-armanious.github.io/pakhom/reference/analyze_temporal_patterns.md)

- output_dir:

  Directory where PNGs will be saved (created if needed)

- methodology_mode:

  Optional character (T1.7 / AC4): when supplied, adds a caption to each
  plot identifying the mode + run id.

- run_id:

  Optional character: run identifier.

- max_inline_themes:

  Integer; the temporal emergence chart filters to the top-N themes by
  entry count when more themes than this exist. Phase 58 Tier 5
  AH-8/V-2: pre-Phase-58 the chart rendered every theme (4,059 codes on
  the Phase 57 saturation run -\> 2.8 MB vertical wall of text). Default
  30L. Set very high (e.g. 10000L) to disable filtering.

## Value

Invisible character vector of file paths written
