# Generate AI-powered executive summary and conclusion

Generate AI-powered executive summary and conclusion

## Usage

``` r
generate_ai_synthesis(
  overall_stats,
  theme_stats,
  correlations_df,
  insights,
  theme_set,
  provider = NULL,
  config = NULL,
  audit_log = NULL,
  response_cache = NULL
)
```

## Arguments

- overall_stats:

  Overall statistics list

- theme_stats:

  Per-theme statistics list

- correlations_df:

  Correlations tibble

- insights:

  Insights list

- theme_set:

  ThemeSet object

- provider:

  AIProvider object (or NULL for fallback)

- config:

  ThematicConfig (or NULL). The reflexivity_block is read from
  `config$study` and injected into the synthesis system prompt; pass
  NULL to omit reflexivity framing.

- audit_log:

  An optional AuditLog object (T1.4). When provided, the
  executive-summary synthesis AI call is recorded as an `ai_request`
  audit decision with full provenance.

- response_cache:

  An optional ResponseCache object (T1.4). When provided, the raw API
  response is written to the cache and referenced from the audit log.

## Value

List with executive_summary and conclusion strings
