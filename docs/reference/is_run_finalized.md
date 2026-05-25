# Check whether a run directory is finalized

A finalized run cannot have its methodology silently re-declared. Per
AC5 (soft-lock with audit trail; methodology change creates new run),
callers check this flag before mutating methodology-affecting state on a
prior run; if TRUE, they fork via
[`clone_run_with_new_mode`](https://abanoub-armanious.github.io/pakhom/reference/clone_run_with_new_mode.md).

## Usage

``` r
is_run_finalized(run_dir)
```

## Arguments

- run_dir:

  Path to a run output directory.

## Value

Logical scalar.

## Details

Returns FALSE for a missing run_metadata.json – "no metadata" means "not
finalized" by definition (the canonical record doesn't exist yet, so no
claim has been made).
