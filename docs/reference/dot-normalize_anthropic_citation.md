# Normalize a single citation into the canonical pakhom shape

Coerces numeric index fields to integer (jsonlite::fromJSON returns them
as numeric by default), handles missing-field defaults, and preserves
Anthropic's field names verbatim so the bridge in 21b doesn't need a
field-name lookup table.

## Usage

``` r
.normalize_anthropic_citation(cite)
```
