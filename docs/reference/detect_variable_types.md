# Detect variable types for dynamic correlation method selection

Classifies each column as "binary", "ordinal" (\<=7 unique values), or
"continuous".

## Usage

``` r
detect_variable_types(corr_data)
```

## Arguments

- corr_data:

  Numeric tibble from prepare_correlation_data()

## Value

Named character vector with types per column
