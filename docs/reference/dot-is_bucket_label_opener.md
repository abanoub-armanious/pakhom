# Phase 58 C-1: bucket-label opener detection

Returns TRUE when the articulation opens with a phrase that signals a
list-of-things rather than a unifying principle. Used as one of the
articulation-quality gates in .evaluate_cluster.

## Usage

``` r
.is_bucket_label_opener(articulation)
```

## Arguments

- articulation:

  Character scalar; the raw articulation string.

## Value

Logical TRUE if articulation should be rejected as bucket-y.
