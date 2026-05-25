# Batch-generate inductive codes for anomaly segments via the AI

One AI call per chunk of up to `.EMERGENT_BATCH_SIZE` segments. The
prompt anchors the AI to the framework name + constructs so its
inductive codes are scoped to "what the framework didn't capture."
temperature=0 for replay-equivalence.

## Usage

``` r
.EMERGENT_BATCH_SIZE
```

## Format

An object of class `integer` of length 1.

## Details

Returns a list parallel to `segments`: for each segment, a
`(code_name, code_description)` pair. NULL entries indicate the AI
returned no code for that segment (rare but possible).
