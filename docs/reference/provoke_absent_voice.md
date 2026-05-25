# Absent-voice provocation

List demographic / temporal / linguistic / topical segments of the
corpus that are underrepresented in the entries supporting theme X.
Observational rather than evidence-based; no exact_text citations (the
model is reasoning ABOUT absences, not quoting present voices). Returns
Provocation objects with NULL provenance and the dimension info in the
`extra` field.

## Usage

``` r
provoke_absent_voice(
  theme_name,
  theme_entries,
  data,
  provider,
  n = 5L,
  audit_log = NULL,
  response_cache = NULL,
  fabrication_log = NULL
)
```

## Arguments

- theme_name:

  Character: the theme to challenge.

- theme_entries:

  Tibble: entries the researcher believes support the theme (must have
  std_id and std_text).

- data:

  Tibble: the FULL corpus (the model searches this).

- provider:

  AIProvider object.

- n:

  Integer: maximum provocations to return (default 5).

- audit_log:

  Optional AuditLog.

- response_cache:

  Optional ResponseCache.

- fabrication_log:

  Optional FabricationLog.
