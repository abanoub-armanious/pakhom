# Phase 58 C-1: tautological-articulation detection

Returns TRUE when the articulation reuses more than 70% of the content
words from the proposed theme name (after removing English stop words
and theme-rendering boilerplate like "theme", "captures", "various"). A
tautological articulation restates the name without adding a unifying
principle.

## Usage

``` r
.is_tautological_articulation(articulation, proposed_name)
```

## Arguments

- articulation:

  Character scalar; the raw articulation string.

- proposed_name:

  Character scalar; the AI's proposed theme name.

## Value

Logical TRUE if articulation should be rejected as tautological.
