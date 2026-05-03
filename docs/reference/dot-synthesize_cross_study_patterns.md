# Synthesize structural facts across multiple codebooks

Produces a domain-neutral, evidence-based summary of the prior
codebooks' actual contents (theme names, hierarchy depth, coding style
benchmarks).

## Usage

``` r
.synthesize_cross_study_patterns(hierarchies, studies, benchmarks)
```

## Arguments

- hierarchies:

  Named list of codebook hierarchy data

- studies:

  PreviousStudies object

- benchmarks:

  Computed coding benchmarks (or NULL)

## Value

Character string describing structural facts (no opinions)

## Details

### Design notes

An earlier version of this function (pre-1.0.0) injected hardcoded
medication/health-research opinions into the AI's learning context: a
regex list that matched theme names to predefined "recurring categories"
(side effects, treatment efficacy, dosage timing, etc.) and an
unconditional narrative-arc claim ("themes were organized to tell a
coherent story: starting with direct treatment effects, moving to side
effects and complications, then broader implications...") that fired
whenever any prior codebook had theme descriptions. Both biased the AI
toward medication-research framings regardless of the user's actual
research domain. They have been removed.

What this function does now: list the actual top-level theme names from
each prior codebook so the AI can see what was studied without being
told what the patterns "are". The numerical benchmarks (segment length,
codes per entry, discarded-code percentage) are kept because they're
domain-independent calibration data.
