# Pick a row near a sentiment-target index, preferring a new contributor

Search order: target_idx, then expanding outward (target-1, target+1,
target-2, target+2, ...) until we find a row that (a) hasn't been taken
already by index, AND (b) is from an author not yet represented (or has
no author data, which we treat as "neutral" and accept). When no winner
is found, falls back to target_idx so the caller still gets SOMETHING –
single-contributor or no-author-data themes degrade to the original
behavior. This is the per-slot half of T0.2 spread-aware quote
selection.

## Usage

``` r
.pick_quote_with_spread(
  valid_df,
  target_idx,
  taken_indices,
  taken_authors,
  has_authors
)
```
