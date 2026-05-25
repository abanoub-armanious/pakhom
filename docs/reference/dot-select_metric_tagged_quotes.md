# Select representative example quotes tagged with per-entry metric values

Picks up to `n_quotes` entries using the same sentiment-positioned
selection as `.select_representative_quotes` (so the quotes span the
sentiment range when available), then formats each as the entry's text
followed by a bracketed metric-tag block:
`"<quote text>" [<metric_a>: 8; <metric_b>: 12]`

## Usage

``` r
.select_metric_tagged_quotes(entries, metric_cols, n_quotes = 3L)
```

## Details

When the entry is missing a metric, that metric is omitted from the tag
(rather than printing "NA"); when all metrics are missing the tag itself
is omitted.
