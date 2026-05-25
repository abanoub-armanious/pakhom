# Coerce parse_json_safely output into a list-of-lists

jsonlite simplifies a JSON array of uniform-shape objects into a
data.frame; iterating with `for (cit in df)` then walks COLUMNS (atomic
vectors), which breaks the per-citation `cit$entry_id` access. This
helper normalizes both the data.frame and named-list shapes back to a
list-of-lists where each element is one citation.

## Usage

``` r
.normalize_provocations_payload(payload)
```
