# List available analysis runs

Scans a results directory for timestamped run folders and returns a
summary tibble with run IDs, dates, paths, and output-schema versions.
The `schema_compatible` column flags whether each run can participate in
[`compare_runs`](compare_runs.md) given the current package's schema
version.

## Usage

``` r
list_available_runs(results_base)
```

## Arguments

- results_base:

  Path to the parent directory containing all run folders

## Value

A tibble with columns: run_id, date, path, schema_version,
schema_compatible
