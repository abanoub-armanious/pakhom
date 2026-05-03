# Run human verification / IRR process

Samples entries, exports blank coding sheets and codebook, then checks
for a completed human coding sheet. If found, computes agreement stats.

## Usage

``` r
run_human_verification(
  data,
  coding_state,
  config = list(),
  output_dir = ".",
  checkpoint = NULL
)
```

## Arguments

- data:

  tibble with std_text, std_id columns

- coding_state:

  ProgressiveCodingState (provides codebook and AI codes)

- config:

  Human verification config section

- output_dir:

  Output directory path

- checkpoint:

  CheckpointManager (or NULL)

## Value

List with status ("exported", "completed"), irr_stats, sample_ids
