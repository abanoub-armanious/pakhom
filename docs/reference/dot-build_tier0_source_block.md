# Render the citation-source breakdown sub-block for the Tier-0 dashboard

Distinguishes Anthropic Citations API quotes (PREVENTION layer:
server-side-grounded offsets) from model_freeform quotes (DETECTION-only
layer: model wrote a verbatim claim, ladder verified offline). Renders
the per-source count and per-source verification rate so the dashboard
shows both reliability dimensions at once.

## Usage

``` r
.build_tier0_source_block(stats, config = NULL)
```

## Details

Returns "" (empty string) when there are no citation_source values
(degenerate state – shouldn't happen for normal runs but the dashboard
should not crash on an unusual stats object).
