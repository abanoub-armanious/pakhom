# Export progressive codebook for researcher review

Exports a CSV where the researcher can review codes created during
progressive coding: keep, delete, merge, split, or rename codes. On
resume (when the reviewed file exists), applies modifications to the
ProgressiveCodingState.

## Usage

``` r
review_progressive_codebook(
  coding_state,
  output_dir,
  audit_log = NULL,
  irr_result = NULL
)
```

## Arguments

- coding_state:

  ProgressiveCodingState

- output_dir:

  Pipeline output directory

## Value

List with status ("exported" or "applied") and updated coding_state
