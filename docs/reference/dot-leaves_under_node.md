# HAC node helper: map a node index to the leaf code indices under it

[`stats::hclust`](https://rdrr.io/r/stats/hclust.html) encodes the
dendrogram as a (n-1) x 2 merge matrix. Negative entries are leaf
indices (1..n); positive entries are internal node indices referring to
earlier rows of `hac$merge`. This helper resolves an "internal node
index" (1..n-1) to the set of leaf indices under it. We pass
internal-node indices throughout the tree walk because they uniquely
identify subtrees.

## Usage

``` r
.leaves_under_node(hac, node_idx)
```
