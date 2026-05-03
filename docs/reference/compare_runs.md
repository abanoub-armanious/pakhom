# Compare the current run against all previous runs

Discovers all timestamped run directories, loads their exported
artifacts, and performs seven comparison analyses covering sample
overlap, sentiment drift, code stability, theme evolution, entry
migration, correlation stability, and a run summary dashboard.

## Usage

``` r
compare_runs(current_dir, results_base, config = NULL)
```

## Arguments

- current_dir:

  Path to the current run's output directory

- results_base:

  Path to the parent directory containing all run folders

- config:

  ThematicConfig object (or NULL)

## Value

A ComparisonResult S3 object, or NULL if fewer than 2 runs exist
