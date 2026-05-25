# Read and parse run_metadata.json for a run directory

Returns NULL when the file is missing (e.g., a run directory created by
a very old pakhom version, or a partially-initialized run that crashed
before run_metadata.json was written). Returns NULL on parse error too,
so callers can treat "no metadata" and "corrupt metadata" identically
(both mean: fall back to the no-prior-state code path). Errors do warn
so the user knows the file existed but couldn't be read.

## Usage

``` r
read_run_metadata(run_dir)
```

## Arguments

- run_dir:

  Path to a run output directory.

## Value

Named list of run metadata, or NULL when missing/unreadable.
