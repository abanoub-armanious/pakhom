# Group anomaly segments by entry_id for cascade lookup

Used by cascade_theme_assignments under Mode 3 extend/revise. Walks
`coding_state$codebook[["anomaly"]]$coded_segments` once and indexes
them by entry_id so per-entry segment routing is O(1) per entry rather
than O(N) where N is the total anomaly segment count.

## Usage

``` r
.group_anomaly_segments_by_entry(coding_state)
```
