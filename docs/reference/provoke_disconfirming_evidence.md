# Disconfirming-evidence provocation

Find the n entries in the corpus that most strongly contradict theme X.
Same extractive shape as counter_narrative.

## Usage

``` r
provoke_disconfirming_evidence(
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
