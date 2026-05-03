# Extract significant correlations as tidy tibble

Extract significant correlations as tidy tibble

## Usage

``` r
extract_significant(results, p_threshold = 0.05, corr_data = NULL)
```

## Arguments

- results:

  CorrelationResults from calculate_correlations()

- p_threshold:

  Significance threshold (default 0.05)

- corr_data:

  Optional numeric tibble for computing confidence intervals via
  cor.test

## Value

tibble: var1, var2, correlation, p_value, significant, effect_size
