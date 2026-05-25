# Build a segment-identity -\> emergent-theme-name map for cascade fan-out

Walks the emergent theme records (each carrying first-class Subtheme S3
-\> Code S3 -\> coded_segments) and produces a flat list keyed by
`.segment_identity_key(segment)` mapping to the emergent theme the
segment ended up in. Used by `cascade_theme_assignments` to route the
entries whose anomaly segments landed in each emergent theme into the
correct `theme_membership_*` columns.

## Usage

``` r
.build_anomaly_segment_to_theme_map(emergent_themes_raw)
```

## Details

Phase 54 audit CRITICAL-8: without this map the cascade can only route
"anomaly" once (to a single theme), which under extend/revise policies
means emergent themes render with entry_count = 0 – a silent data-loss
bug that defeats the whole purpose of Phase 54.
