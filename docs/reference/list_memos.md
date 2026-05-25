# List memos in a ResearcherReflectionLog as a tibble

Filterable summary view: one row per memo, columns are the schema fields
(id, timestamp, author, type, n_linked_codes, n_linked_themes,
n_linked_entries, body_chars, body_preview). Useful for the Mode 1
report's memos timeline and for programmatic introspection.

## Usage

``` r
list_memos(log, type = NULL, author = NULL, linked_theme = NULL)
```

## Arguments

- log:

  A `ResearcherReflectionLog`.

- type:

  Optional character: filter to memos of this type.

- author:

  Optional character: filter to memos by this author.

- linked_theme:

  Optional character: filter to memos linked to this theme.

## Value

A tibble (zero-row when no memos / nothing matches).
