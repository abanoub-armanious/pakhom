# Phase 58 Tier 3 AH-3: prominent equal-weight roster of prior studies

Returns a markdown-formatted listing of every study with a brief
one-line characterization (n_codes, top theme count). Prepended to
`ctx$for_theming` so every study name appears at the TOP of the AI's
context window with equal prominence. Phase 57 audit found that without
this, the AI's "synthesis reflection" referenced whichever study was
iterated first (Dayvigo, 3x) and never named Ozempic or Vyvanse despite
all three being in the context.

## Usage

``` r
.build_study_roster(studies)
```

## Details

Returns "" when fewer than 2 studies are available (a single study
trivially gets full attention; no balancing needed).
