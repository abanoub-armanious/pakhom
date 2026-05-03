# Find the most recent run folder in the results directory

Searches for timestamped run folders (run_YYYY-MM-DD_HHMMSS) at the top
level of the results directory. Skips ghost directories (those with no
completed checkpoints or output files beyond the manifest).

## Usage

``` r
find_latest_run(results_base)
```

## Arguments

- results_base:

  Base results directory containing run folders

## Value

Character: folder name of most recent run, or NULL
