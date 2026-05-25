# Build a synthetic ProgressiveCodingState scoped to anomaly segments

Consolidates per-segment inductive codes (from
`.inductive_code_anomaly_segments`) by code_name, packing each unique
inductive code as a codebook entry whose `coded_segments` list contains
the anomaly segments labeled with that code. The resulting state can be
passed to `generate_themes_iterative` (Phase 52 HAC + AI-judged tree
walk) as if it were a normal Mode 2 run scoped to just the anomaly
residuals.

## Usage

``` r
.build_synthetic_state_from_emergent_codes(anomaly_segments, segment_codes)
```
