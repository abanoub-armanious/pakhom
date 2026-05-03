# Compute confidence interval for a correlation using Fisher z-transformation

Approximates a confidence interval when cor.test does not provide one
(i.e., for Spearman and Kendall correlations). Uses the Fisher
z-transformation: z = atanh(r), SE = 1/sqrt(n-3), then back-transforms.

## Usage

``` r
.fisher_z_ci(r, n, conf_level = 0.95)
```

## Arguments

- r:

  Observed correlation coefficient

- n:

  Number of observations

- conf_level:

  Confidence level (default 0.95)

## Value

Numeric vector of length 2: c(lower, upper), or c(NA, NA) if n \< 4
