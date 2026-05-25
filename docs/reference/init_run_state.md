# Initialize the run-metadata record for a new (or resumed) run

Builds the run_metadata.json content from the supplied run details and
writes it to `run_dir`. Idempotent on resume: when `run_metadata.json`
already exists, returns the existing metadata WITHOUT overwriting (so
finalized markers / parent_run_id aren't clobbered by a casual re-init).
To recreate, delete the file first.

## Usage

``` r
init_run_state(
  run_dir,
  run_id,
  methodology_mode,
  parent_run_id = NULL,
  mode_changed_from = NULL,
  ...
)
```

## Arguments

- run_dir:

  Path to the run directory.

- run_id:

  Stable run identifier (often the basename of `run_dir`).

- methodology_mode:

  Character: the declared mode for this run.

- parent_run_id:

  Optional character: when this run is a fork of a prior run with a
  different methodology, the parent's run_id.

- mode_changed_from:

  Optional character: the methodology mode of the parent run (when
  forked). NULL when this is a fresh run.

- ...:

  Additional fields written verbatim into run_metadata.json (e.g.,
  `provider = "anthropic"`, `study_name = "..."`).

## Value

The metadata list (existing or newly written).
