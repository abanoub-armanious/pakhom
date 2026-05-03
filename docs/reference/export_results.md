# Export all analysis results to files

Export all analysis results to files

## Usage

``` r
export_results(
  data,
  theme_set,
  correlations_df,
  insights,
  consolidated,
  output_dir
)
```

## Arguments

- data:

  tibble with all analysis columns

- theme_set:

  ThemeSet object

- correlations_df:

  Correlations tibble

- insights:

  Insights list

- consolidated:

  ConsolidatedCodes list

- output_dir:

  Output directory path

## Value

List of export file paths
