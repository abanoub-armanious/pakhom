# Generate saturation curve plot

Creates a PNG plot showing cumulative codes vs coded entries, with the
saturation point marked if reached.

## Usage

``` r
generate_saturation_plot(state, output_dir)
```

## Arguments

- state:

  ProgressiveCodingState with saturation data

- output_dir:

  Directory to save the plot

## Value

Path to the generated PNG, or NULL
