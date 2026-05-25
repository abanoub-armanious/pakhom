# Detect variable types for dynamic correlation method selection

Classifies each column as "binary", "ordinal", or "continuous". The
ordinal threshold defaults to `<=21 unique values`, which covers (a) the
VADER-shaped sentiment scale `[-1, 1]` quantized at 0.1 (21 distinct
levels), (b) the Likert-style 5/7/9/11-point scales common in survey
research, and (c) AI-elicited intensity / confidence scores on a small
integer grid. The pre-Phase-58 threshold of 7 silently classified VADER
sentiment as *continuous* on the Phase 57 run, which then dispatched to
Pearson (point-biserial for binary x quantized-sentiment pairs) –
methodologically wrong for an ordinal support. Spearman is correct when
either variable is rank-orderable but not interval-scaled (H-13).

## Usage

``` r
detect_variable_types(corr_data, ordinal_max = 21L)
```

## Arguments

- corr_data:

  Numeric tibble from prepare_correlation_data()

- ordinal_max:

  Integer; upper bound on distinct values for the ordinal
  classification. Default 21L. Datasets with finer-grained ordinal
  scales (e.g. 0-50 Likert) can override.

## Value

Named character vector with types per column
