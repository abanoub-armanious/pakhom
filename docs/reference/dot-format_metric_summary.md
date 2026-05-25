# Format a per-metric Median(MAD) or Mean(SD) summary as a string

"8.0 (1.5)" – one summary statistic + its variability measure in
parentheses. Returns "n/a" when the value is NA so the renderer produces
a meaningful cell rather than NaN/NA artifacts.

## Usage

``` r
.format_metric_summary(center, spread, digits = 2L)
```
