# Generate saturation curve plot

Creates a PNG plot showing cumulative codes vs coded entries, with the
saturation point marked if reached.

## Usage

``` r
generate_saturation_plot(
  state,
  output_dir,
  methodology_mode = NULL,
  run_id = NULL
)
```

## Arguments

- state:

  ProgressiveCodingState with saturation data

- output_dir:

  Directory to save the plot

- methodology_mode:

  Optional character (T1.7 / AC4): when supplied, adds a footer caption
  to the plot identifying the mode + run.

- run_id:

  Optional character: run identifier for the footer.

## Value

Path to the generated PNG, or NULL
