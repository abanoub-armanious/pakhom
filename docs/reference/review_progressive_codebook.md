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
  irr_result = NULL,
  methodology_mode = NULL
)
```

## Arguments

- coding_state:

  ProgressiveCodingState

- output_dir:

  Pipeline output directory

- audit_log:

  Optional AuditLog

- irr_result:

  Optional IRR result list

- methodology_mode:

  Optional methodology mode (T1.7 / AC4). When non-NULL, the exported
  review CSV is stamped with a comment header identifying the mode and
  run id. NULL skips stamping (legacy / test callers).

## Value

List with status ("exported" or "applied") and updated coding_state
