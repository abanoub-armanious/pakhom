# Warn when the reflexivity scaffold is empty (Phase 58 Tier 3 AH-4)

Olmos-Vega et al. (AMEE Guide 149) recommend positionality + paradigm +
reflexive notes be present in every turn of qualitative analysis. pakhom
injects `study.researcher_positionality`, `study.research_paradigm`, and
`study.reflexive_notes` into the AI system prompt at every coding /
theming / saturation call (see R/methodology_rules.R). When all three
are empty, `.reflexivity_block_for()` returns the empty string and the
AI prompt CONTAINS NO REFLEXIVITY BLOCK AT ALL – a methodology paper
relying on this run cannot honestly claim reflexive practice.

## Usage

``` r
.warn_empty_reflexivity(config)
```
