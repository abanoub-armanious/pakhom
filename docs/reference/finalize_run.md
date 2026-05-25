# Mark a run as finalized

Sets `is_finalized = TRUE` and `finalized_at` on the run's metadata.
Idempotent: re-finalizing a finalized run is a no-op (logs debug and
returns the existing metadata unchanged).

## Usage

``` r
finalize_run(run_dir)
```

## Arguments

- run_dir:

  Path to a run output directory.

## Value

The updated metadata list, or NULL if the run has no metadata to
finalize.

## Details

Called once per run – typically as the last step of `run_analysis` after
the report is generated. Per AC5, finalization is the moment methodology
is locked for the canonical output.
