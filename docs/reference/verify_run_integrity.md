# Verify that a run directory contains all expected output files

Checks for the core data files that every completed run should contain,
plus conditional files based on config settings.

## Usage

``` r
verify_run_integrity(run_dir, config = list())
```

## Arguments

- run_dir:

  Path to the run directory

- config:

  ThematicConfig (or list) used for the run, to check conditional
  outputs

## Value

List with `expected` (all expected files), `present` (found files),
`missing` (expected but not found), `complete` (logical)
