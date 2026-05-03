# Generate AI reflection on what was learned from previous studies

Asks the AI to summarize what patterns, themes, and analytical
approaches it extracted from the previous manual analyses, and how that
knowledge will guide the current analysis. The reflection is stored in
the learning context for inclusion in the final report.

## Usage

``` r
generate_learning_reflection(learning_context, provider = NULL)
```

## Arguments

- learning_context:

  LearningContext object

- provider:

  AIProvider object (or NULL for plain-text fallback)

## Value

Updated LearningContext with `for_report` populated
