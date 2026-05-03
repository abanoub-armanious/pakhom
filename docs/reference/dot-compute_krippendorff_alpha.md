# Compute Krippendorff's alpha for binary nominal data (2 raters)

Implements Krippendorff's alpha for nominal data with 2 coders. More
robust than Cohen's kappa for sparse binary matrices and handles the
prevalence/bias problem better (Krippendorff, 2011).

## Usage

``` r
.compute_krippendorff_alpha(rater1, rater2, n_codes, n_entries)
```

## Arguments

- rater1:

  Binary integer vector (flattened: n_entries \* n_codes)

- rater2:

  Binary integer vector (same length as rater1)

- n_codes:

  Number of unique codes

- n_entries:

  Number of entries coded

## Value

Krippendorff's alpha (numeric)
