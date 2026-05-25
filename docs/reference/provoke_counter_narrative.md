# Counter-narrative provocation

Given researcher-supplied theme name + supporting entries, AI returns up
to `n` entries from the corpus that frame the same construct as not-Y.
Per Sarkar 2024 / patterns doc: extractive only – the model returns
entries (not arguments), and a one-sentence reason per entry.

## Usage

``` r
provoke_counter_narrative(
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

## Value

List of `Provocation` objects (verified, non-fabricated).
