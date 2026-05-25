# Generate AI insights from correlation findings

Generate AI insights from correlation findings

## Usage

``` r
generate_insights(
  correlations_df,
  theme_set,
  provider,
  research_focus = "",
  config = list(),
  audit_log = NULL,
  response_cache = NULL
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

- audit_log:

  An optional AuditLog object (T1.4). When provided, the
  insight-generation AI call is recorded as an `ai_request` audit
  decision with full provenance.

- response_cache:

  An optional ResponseCache object (T1.4). When provided, the raw API
  response is written to the cache and referenced from the audit log.

## Value

Insights list
