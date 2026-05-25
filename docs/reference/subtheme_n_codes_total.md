# Number of codes in a Subtheme INCLUDING nested sub-subthemes

Phase 58 Tier 1 audit LOW-3 addition: depth-recursive code count. Walks
the Subtheme tree and sums direct-code counts at every depth. Use
[`subtheme_n_codes()`](https://abanoub-armanious.github.io/pakhom/reference/subtheme_n_codes.md)
for the depth-0 (direct only) count.

## Usage

``` r
subtheme_n_codes_total(subtheme)
```

## Arguments

- subtheme:

  Subtheme S3

## Value

Integer; total code count across every nested depth.
