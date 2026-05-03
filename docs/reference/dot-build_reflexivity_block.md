# Build a standardized reflexivity text block from study config

Combines researcher_positionality, research_paradigm, and
reflexive_notes into a single text block suitable for injection into AI
system prompts. Returns empty string if no reflexivity fields are set.

## Usage

``` r
.build_reflexivity_block(study_config)
```

## Arguments

- study_config:

  The study section of ThematicConfig

## Value

Character string (may be empty)
