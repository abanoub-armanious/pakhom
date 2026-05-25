# Collect Code S3 objects from a Subtheme AND all its nested sub-subthemes

Phase 58 Tier 1 C-12 introduced nested Subthemes. This helper walks the
depth-N tree so callers that want a flat list of every Code under a
Subtheme don't have to recurse manually.

## Usage

``` r
.subtheme_codes_recursive(subtheme)
```
