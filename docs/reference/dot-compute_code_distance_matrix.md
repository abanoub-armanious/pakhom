# Compute pairwise code distance matrix

Cosine distance via OpenAI embeddings on "name: description" strings is
the preferred metric. When embeddings are unavailable (non-OpenAI
provider, network failure, embedding API error) we fall back to Jaccard
distance on the entry-id sets – codes that frequently co-occur on the
same entries are treated as similar.

## Usage

``` r
.compute_code_distance_matrix(codes, coding_state, provider)
```

## Details

Both metrics produce a symmetric `dist` object compatible with
[`stats::hclust`](https://rdrr.io/r/stats/hclust.html). The chosen
metric is recorded as an attribute for downstream audit-log stamping.
