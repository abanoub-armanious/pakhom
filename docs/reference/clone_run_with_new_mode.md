# Clone a run directory with a new methodology mode

Implements the REDCap dev/production fork: when a researcher needs to
re-analyze the same data under a different methodology, the prior run is
preserved (frozen as the original record) and a NEW run directory is
created with `parent_run_id` pointing at the prior run. The new run
inherits nothing from the old run except the linkage in metadata; the
new run's outputs are computed fresh.

## Usage

``` r
clone_run_with_new_mode(
  run_dir,
  new_mode,
  new_run_dir,
  new_run_id = basename(new_run_dir)
)
```

## Arguments

- run_dir:

  Path to the parent run directory (must have a `run_metadata.json`).

- new_mode:

  Methodology mode for the cloned run. Must differ from the parent's
  mode (otherwise this is a re-init, not a fork; the function errors
  with a helpful message).

- new_run_dir:

  Path to the new run directory (must NOT already exist; the function
  refuses to overwrite to avoid accidental destruction of an active
  run).

- new_run_id:

  Stable identifier for the new run. Defaults to the basename of
  `new_run_dir`.

## Value

The new run's metadata list.

## Details

The new run directory is created at `new_run_dir` and contains only the
initial `run_metadata.json`. Pipeline orchestration is the caller's
responsibility – this function does NOT copy data, checkpoints, or any
other artifacts from the parent run.
