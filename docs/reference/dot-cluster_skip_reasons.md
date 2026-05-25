# Cluster free-text skip reasons into a coarse taxonomy

AI-generated skip reasons
(`coding_state$entry_results[[id]]$skip_reason`) are short free-text
justifications produced by the coding model when it judges an entry
off-topic / non-applicable. On a 5,000-entry run the Phase 57 audit
observed 580 distinct reason strings, almost all paraphrases of "the
entry does not contain..." in slightly different wording. Rendering one
HTML bullet per distinct string produced an unreadable 580-bullet list
AND contributed measurably to pandoc OOM during HTML render (C-3).

## Usage

``` r
.cluster_skip_reasons(skip_reasons)
```

## Arguments

- skip_reasons:

  Named integer vector from `coverage$skip_reasons` (names = verbatim
  reason strings; values = counts).

## Value

List of category records, each with `label`, `count` (total entries in
this category), `n_distinct` (distinct reason strings), and `examples`
(character vector, up to 3). Sorted by total count, descending.

## Details

This helper buckets reasons into ~7 broad categories via
case-insensitive keyword regex, first-match-wins. Categories are
aggregated by total count; each carries up to 3 verbatim examples
(most-frequent first) so the reader can still sample original wording.
