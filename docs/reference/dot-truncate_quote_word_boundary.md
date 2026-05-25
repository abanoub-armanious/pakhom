# Word-boundary-aware quote truncation with visible ellipsis

Phase 58 Tier 9 V-8: a quote longer than `max_chars` is cut back to the
LAST whitespace character before the limit, then a " ..." marker is
appended so the reader sees the truncation. Falls back to a hard substr
cut + "..." when the entry contains no whitespace within the budget
(rare; typically single-token URLs or user handles).

## Usage

``` r
.truncate_quote_word_boundary(text, max_chars = 280L)
```

## Arguments

- text:

  Input string (may be NA).

- max_chars:

  Total budget INCLUDING the " ..." marker.

## Value

Truncated character.
