# Alternative-interpretation provocation

Given theme name + 3 supporting quotes, generate methodologically
defensible alternative theme names that the same quotes could support.
The model does NOT say which is better.

## Usage

``` r
provoke_alternative_interpretation(
  theme_name,
  theme_entries,
  data,
  provider,
  n_alternatives = 2L,
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

- n_alternatives:

  Integer: how many alternative names to return (default 2 per
  Drosos/Sarkar 2025).

- audit_log:

  Optional AuditLog.

- response_cache:

  Optional ResponseCache.

- fabrication_log:

  Optional FabricationLog.
