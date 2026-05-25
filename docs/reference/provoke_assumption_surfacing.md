# Assumption-surfacing provocation

Given the researcher's theme name + a key term they use, list other
terms participants in the corpus use for the same construct (with
citations) and identify a term participants use that the researcher's
framing erases.

## Usage

``` r
provoke_assumption_surfacing(
  theme_name,
  theme_entries,
  data,
  provider,
  key_term = NULL,
  n_alternatives = 3L,
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

- key_term:

  Character: the researcher's term to challenge (e.g., "binge-eating").
  Defaults to the theme name itself.

- n_alternatives:

  Integer: how many alternative-term provocations to request from the
  model (default 3).

- audit_log:

  Optional AuditLog.

- response_cache:

  Optional ResponseCache.

- fabrication_log:

  Optional FabricationLog.
