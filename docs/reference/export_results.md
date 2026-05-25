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
  output_dir,
  methodology_mode = NULL
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

- methodology_mode:

  Optional methodology mode (T1.7). When non-NULL, every CSV produced is
  stamped with a comment header identifying the mode and run id (per
  AC4). NULL skips stamping – used by tests / legacy callers.

## Value

List of export file paths
