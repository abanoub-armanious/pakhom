# Number of nested subthemes within a Subtheme

Phase 58 Tier 1 C-12 introduced nested Subthemes so the HAC walker can
produce hierarchical decomposition (e.g. a 200-code subtheme broken into
sub-subthemes via depth-N recursion). Returns 0 for leaf Subthemes.

## Usage

``` r
subtheme_n_subthemes(subtheme)
```

## Arguments

- subtheme:

  Subtheme S3

## Value

Integer; depth-1 nested subtheme count.
