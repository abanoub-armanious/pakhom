# Calculate correlation matrix with p-values

Calculate correlation matrix with p-values

## Usage

``` r
calculate_correlations(
  corr_data,
  method = "spearman",
  adjust_method = "bonferroni",
  var_types = NULL,
  dynamic_method = FALSE
)
```

## Arguments

- corr_data:

  Numeric tibble from prepare_correlation_data()

- method:

  "spearman" or "pearson" (used when dynamic_method is FALSE)

- adjust_method:

  P-value adjustment method (e.g., "bonferroni")

- var_types:

  Optional named character vector from detect_variable_types()

- dynamic_method:

  If TRUE, select method per variable pair based on types

## Value

CorrelationResults list
