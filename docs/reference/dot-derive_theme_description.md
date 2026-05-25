# Derive a fallback theme description when the AI omits one

Phase 58 Tier 8 M-21/AF-29: pre-Tier-8 a coherent_theme verdict with
`proposed_description = ""` silently produced a theme with an empty
description (5 themes on the Phase 57 run). Downstream renderers then
displayed blank theme cards. The fallback derives a short summary from
(a) the AI's articulation (when non-empty) and (b) the theme's top-3
codes by frequency. Worst-case output is "Theme grouping: , , " – not
poetry, but provably non-empty.

## Usage

``` r
.derive_theme_description(leaf_indices, codes, articulation = NULL)
```
