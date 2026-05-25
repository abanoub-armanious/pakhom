# Generate AI reflection on what was learned from previous studies

Asks the AI to summarize what patterns, themes, and analytical
approaches it extracted from the previous manual analyses, and how that
knowledge will guide the current analysis. The reflection is stored in
the learning context for inclusion in the final report.

## Usage

``` r
generate_learning_reflection(
  learning_context,
  provider = NULL,
  audit_log = NULL,
  response_cache = NULL
)
```

## Arguments

- learning_context:

  LearningContext object

- provider:

  AIProvider object (or NULL for plain-text fallback)

- audit_log:

  An optional AuditLog object (from
  [`init_audit_log`](https://abanoub-armanious.github.io/pakhom/reference/init_audit_log.md)).
  When provided, the AI reflection call is recorded as an `ai_request`
  audit decision (T1.4) with full provenance (model, usage,
  prompt_hash). Pass `NULL` to skip.

- response_cache:

  An optional ResponseCache object (from
  [`init_response_cache`](https://abanoub-armanious.github.io/pakhom/reference/init_response_cache.md)).
  When provided, the raw API response is written to the cache and
  referenced from the audit log.

## Value

Updated LearningContext with `for_report` populated
