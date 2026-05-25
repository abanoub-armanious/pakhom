# Normalize the AI's coded_segments payload into a uniform list-of-lists

jsonlite may return a data.frame (when all segments share the same
fields) or a single named list (when only one segment). Downstream code
expects a list of named lists, so this helper coerces both shapes.

## Usage

``` r
.normalize_segments(segments_raw)
```
