# Ask the AI to evaluate a cluster

Builds the cluster summary prompt (with bias-mitigation context: most-
distant pair, full per-code list when small, top-N + extremes when
large) and calls
[`ai_complete()`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md)
with the
[`.theme_decision_schema()`](https://abanoub-armanious.github.io/pakhom/reference/dot-theme_decision_schema.md).

## Usage

``` r
.articulation_min_chars(n_codes)
```

## Arguments

- n_codes:

  Number of leaf codes in the cluster being evaluated.

## Value

Integer minimum-character count required of a coherent_theme
articulation for a cluster of this size.

## Details

Returns a structured decision record (decision, name, description,
rationale, articulation). Records the decision in walk_state and the
audit_log.
