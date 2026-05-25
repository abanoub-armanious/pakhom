# Walk the HAC tree top-down to identify themes

Recursive divisive walk. At each internal node, the AI sees a summary of
the codes under it (with the most-distant pair highlighted) and decides:
coherent_theme – this subtree's codes are one theme; stop recursing.
split_required – recurse into both children, building separate themes
from them. atomic_outlier – this subtree's codes are essentially one
concept (often a leaf or near-leaf); make a theme of it and stop
recursing.

## Usage

``` r
.walk_for_themes(
  hac_node_idx,
  hac,
  codes,
  distance_matrix,
  co_occurrence,
  walk_ctx
)
```
