# Write framework_review.csv for the "revise" anomaly_handling policy

One row per anomaly segment. Columns:

- `entry_id`: original corpus entry the segment came from

- `segment_text`: the segment that didn't fit the framework

- `emergent_theme`: the emergent theme name the inductive pass assigned
  this segment to (or NA when the inductive pass produced no theme)

- `emergent_code`: the per-segment inductive code name

- `suggested_construct_edit` *(blank)*: column for the researcher to
  fill – what change to the framework spec would let this segment fit a
  (new or revised) construct?

- `accepted` *(blank)*: column for the researcher to mark TRUE/FALSE
  after deciding whether to act on the suggestion

## Usage

``` r
.write_framework_review_csv(
  output_dir,
  coding_state,
  framework_spec,
  emergent_themes,
  audit_log = NULL
)
```
