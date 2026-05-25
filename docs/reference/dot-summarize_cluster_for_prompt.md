# Summarize a cluster for the AI prompt

Layout:

- Codes list: full per-code (name, freq, description) when N \<= 50;
  top-N by frequency + count of remaining + the 3 most-distant pairs
  when N \> 50.

- Quantitative context: mean intra-cluster distance, max-distant pair
  (always shown – bias mitigation), top co-occurring pairs.

## Usage

``` r
.summarize_cluster_for_prompt(
  cluster_leaves,
  codes,
  distance_matrix,
  co_occurrence
)
```
