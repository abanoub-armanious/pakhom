# Format a bracketed metric-tag block for one entry row

`[<metric_a>: 8; <metric_b>: 12]` – one metric=value pair per metric
column the entry has a non-NA value for, semicolon-separated, wrapped in
square brackets. Returns the empty string when the row is missing all
metric values (so the renderer can omit the tag entirely).

## Usage

``` r
.format_metric_tag(row, metric_cols)
```
