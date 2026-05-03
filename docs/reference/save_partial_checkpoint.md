# Save partial checkpoint within a step (for long-running batch operations)

Save partial checkpoint within a step (for long-running batch
operations)

## Usage

``` r
save_partial_checkpoint(manager, step_name, data, progress_idx)
```

## Arguments

- manager:

  CheckpointManager object

- step_name:

  Step identifier

- data:

  Partial results so far

- progress_idx:

  Index of last completed item
