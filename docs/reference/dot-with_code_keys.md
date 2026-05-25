# Add code_keys to theme records for the live cluster snapshot

Theme records produced by the walks carry `code_indices` (positions in
the codes list); the live snapshot writer wants the actual codebook keys
for human-readable output. This helper resolves the mapping.

## Usage

``` r
.with_code_keys(themes_raw, codes)
```
