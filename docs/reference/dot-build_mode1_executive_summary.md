# Build a deterministic Mode 1 executive summary

No AI call – counts + flags from the reflection log + theme stats.
Surfaces: total provocations, top-2 categories by emit count, themes
that attracted the most disconfirming evidence, themes flagged by
participant-spread concentration, fabrication count.

## Usage

``` r
.build_mode1_executive_summary(
  reflection_log,
  theme_set,
  theme_stats,
  coverage,
  prov_stats
)
```
