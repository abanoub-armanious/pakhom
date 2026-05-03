# Compare runs that used different AI models for inter-model reliability

A convenience wrapper around [`compare_runs`](compare_runs.md) that
validates runs used different models and focuses output on agreement
metrics suitable for reporting inter-model reliability in publications.

## Usage

``` r
compare_models(results_dir, config = NULL)
```

## Arguments

- results_dir:

  Path to the parent directory containing run folders

- config:

  ThematicConfig object (or NULL)

## Value

A ComparisonResult with inter-model agreement metrics, or NULL
