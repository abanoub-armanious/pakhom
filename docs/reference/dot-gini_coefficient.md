# Gini coefficient of a non-negative numeric vector

Standard sample Gini based on the mean absolute difference, normalized
to the unit interval. Returns 0 when all values are identical, NA when
the input is empty / contains negatives / has zero sum.

## Usage

``` r
.gini_coefficient(x)
```

## Arguments

- x:

  Non-negative numeric vector (per-contributor entry counts).

## Value

Numeric Gini in the unit interval, or `NA_real_` on degenerate inputs.

## Details

Implemented inline (rather than depending on `ineq`) because (a) Gini is
a one-line formula not worth a 21st imported package and (b) pakhom's
CRAN dependency footprint is already noted as accept-as-noise.
