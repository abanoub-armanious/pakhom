# Construct a Memo S3 object

Per SPRINT4_DESIGN.md M1.3 (line 277-298). The body is the researcher's
free-text Markdown; the header fields capture the memo's position in the
analytic timeline and its links to other artifacts.

## Usage

``` r
make_memo(
  body,
  type = "theoretical",
  author = "researcher",
  linked_codes = character(0),
  linked_themes = character(0),
  linked_entries = character(0),
  linked_prior_memo = NULL,
  timestamp = NULL,
  id = NULL
)
```

## Arguments

- body:

  Character: free-text Markdown content (the memo's body).

- type:

  Memo type; one of `"operational"`, `"coding"`, `"theoretical"`,
  `"positionality"` (default `"theoretical"` per Charmaz convention –
  the most common form when no other type is specified).

- author:

  Character: memo author. Defaults to `"researcher"` so
  single-researcher analyses don't have to set it; multi-researcher
  teams should record explicitly.

- linked_codes:

  Optional character vector of code ids the memo references.

- linked_themes:

  Optional character vector of theme names the memo references.

- linked_entries:

  Optional character vector of entry std_ids the memo cites.

- linked_prior_memo:

  Optional character: memo_id of an antecedent memo this one extends or
  revises (forms a chain for the timeline view).

- timestamp:

  Optional ISO-8601 timestamp (defaults to now).

- id:

  Optional explicit id (defaults to a generated one).

## Value

A `Memo` S3 object.
