# Compute code co-occurrence matrix from coding state

Counts how many entries contain each pair of codes simultaneously.

## Usage

``` r
.compute_code_cooccurrence(coding_state)
```

## Arguments

- coding_state:

  ProgressiveCodingState

## Value

Named list keyed by sorted "code_a\|code_b" pair-key, each value the
integer count of entries in which the two codes co-occurred.
