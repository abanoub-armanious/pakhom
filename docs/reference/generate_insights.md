# Generate AI insights from correlation findings

Generate AI insights from correlation findings

## Usage

``` r
generate_insights(
  correlations_df,
  theme_set,
  provider,
  research_focus = "",
  config = list()
)
```

## Arguments

- correlations_df:

  Significant correlations tibble

- theme_set:

  ThemeSet object

- provider:

  AIProvider object

- research_focus:

  Research focus string

- config:

  Correlation config section (e.g. `config$analysis$correlations`). Used
  here for the reflexivity_block injected into the insight system
  prompt; pass an empty list to skip.

## Value

Insights list
