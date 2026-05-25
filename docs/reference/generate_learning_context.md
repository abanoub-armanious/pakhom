# Generate task-specific learning context from previous analyses

Produces a LearningContext object with separate slices for coding,
theming, and review prompts. Uses a CODEBOOK-FIRST approach: the
codebook hierarchy (themes, subthemes, codes, descriptions, frequencies,
and entry-level coding examples) is the primary learning source.
Manuscripts are supplementary, used only when codebook descriptions are
lacking.

## Usage

``` r
generate_learning_context(
  studies,
  max_codebook_chars = 20000L,
  max_manuscript_chars = 12000L,
  max_raw_samples = 5L
)
```

## Arguments

- studies:

  PreviousStudies object

- max_codebook_chars:

  Max characters for codebook context per study

- max_manuscript_chars:

  Max characters for manuscript supplements per study

- max_raw_samples:

  Max raw data examples per study

## Value

LearningContext S3 object
