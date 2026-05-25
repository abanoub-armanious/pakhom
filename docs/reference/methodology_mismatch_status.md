# Detect a methodology mismatch between a config and an existing run

Compares the methodology mode in `config` against the `methodology_mode`
stored in `run_dir`'s run_metadata.json. Used at pipeline start to
decide between resume (modes match), error (modes differ + run
finalized), or fork (modes differ + run still active and the user
explicitly requested a fork).

## Usage

``` r
methodology_mismatch_status(run_dir, config)
```

## Arguments

- run_dir:

  Path to a run output directory.

- config:

  A ThematicConfig (or list with a `methodology$mode` slot) representing
  the methodology declared for the current invocation.

## Value

A character classifier:

- `"no_metadata"` – run_dir has no metadata; treat as fresh.

- `"match"` – methodology agrees with stored.

- `"mismatch_active"` – modes differ, run not finalized.

- `"mismatch_finalized"` – modes differ, run already finalized; caller
  must fork or refuse.
