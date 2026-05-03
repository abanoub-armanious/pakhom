# Invalidate a checkpoint and all downstream checkpoints

Used when the researcher requests a loop-back (e.g., revise_codebook
disposition after theme review). Deletes the named step and everything
after it in step_order so the pipeline re-runs those steps.

## Usage

``` r
invalidate_checkpoints_from(manager, from_step)
```

## Arguments

- manager:

  CheckpointManager

- from_step:

  Character: the step to invalidate (inclusive).

## Value

Updated manager (manifest rewritten)
