# Number of DIRECT codes in a Subtheme (excludes nested sub-subthemes)

Phase 58 Tier 1 audit LOW-3/6 documentation: returns the count of codes
attached DIRECTLY to this Subtheme. Codes in nested sub-subthemes are
NOT counted. Use
[`subtheme_n_codes_total()`](https://abanoub-armanious.github.io/pakhom/reference/subtheme_n_codes_total.md)
for the depth-recursive count.

## Usage

``` r
subtheme_n_codes(subtheme)
```

## Arguments

- subtheme:

  Subtheme S3

## Value

Integer; direct-code count (depth-0).
