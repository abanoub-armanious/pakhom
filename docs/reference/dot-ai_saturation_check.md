# AI self-assessment for saturation

Asks the AI whether it has encountered novel patterns recently that
don't fit existing codes. Returns TRUE if the AI reports no novel
patterns.

## Usage

``` r
.ai_saturation_check(state, provider, research_focus)
```

## Arguments

- state:

  ProgressiveCodingState with current codebook

- provider:

  AIProvider object

- research_focus:

  Research focus string

## Value

Logical: TRUE if AI reports no novel patterns (saturation signal)
