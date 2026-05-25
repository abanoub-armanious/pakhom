# Render the Tier-0 coverage card for a coverage object

S3 generic; the renderer the HTML report calls. Method dispatched on the
object's class. NULL is bypassed and routed to a fixed "unavailable"
card so the call site in `generate_report` / `generate_mode1_report`
does not need to branch.

## Usage

``` r
# S3 method for class 'CorpusCoverage'
render_tier0_coverage_card(x, ...)

render_tier0_coverage_card(x, ...)

# Default S3 method
render_tier0_coverage_card(x, ...)

# S3 method for class 'ProvocationCoverage'
render_tier0_coverage_card(x, ...)
```

## Arguments

- x:

  A coverage object (CorpusCoverage, ProvocationCoverage), NULL, or any
  other object (returns the unavailable variant).

- ...:

  Method-specific arguments.

## Value

Character HTML/markdown string for the card.
