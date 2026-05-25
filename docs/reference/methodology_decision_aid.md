# Methodology decision aid

Helps researchers choose between the three methodology modes
(`reflexive_scaffold`, `codebook_collaborative`, `framework_applied`) by
surfacing the decision-relevant differences. Operates in three modes:

- `interactive = TRUE` (default in interactive R sessions): prompts the
  researcher with a short series of questions and returns a recommended
  mode plus reasoning.

- `interactive = FALSE` with criteria supplied: returns a recommendation
  deterministically based on the supplied criteria.

- Neither: prints a comparison of the three modes for the researcher to
  read; returns NULL invisibly.

## Usage

``` r
methodology_decision_aid(
  interactive = base::interactive(),
  ta_family = NULL,
  has_apriori_framework = NULL,
  wants_irr = NULL
)
```

## Arguments

- interactive:

  Logical; whether to prompt for input. Defaults to
  [`interactive()`](https://rdrr.io/r/base/interactive.html) – TRUE in
  console sessions, FALSE in scripts.

- ta_family:

  Optional character: one of "reflexive", "codebook", "template",
  "framework", "content". Used in non-interactive mode.

- has_apriori_framework:

  Optional logical: whether the researcher has a pre-existing
  theoretical framework to apply.

- wants_irr:

  Optional logical: whether the researcher wants inter-rater reliability
  statistics in the output.

## Value

A list with elements `recommended_mode` (character), `reasoning`
(character), `alternative` (character or NA). Invisibly NULL when called
in print-only mode.

## Details

Per Sprint-4 design (AC3), there is no default methodology mode in
[`validate_config()`](https://abanoub-armanious.github.io/pakhom/reference/validate_config.md);
this function exists so users can make an informed choice rather than
picking arbitrarily.

## Examples

``` r
if (FALSE) { # \dontrun{
# Interactive (when running in a console):
result <- methodology_decision_aid()

# Non-interactive (deterministic):
result <- methodology_decision_aid(
  interactive = FALSE,
  ta_family = "reflexive",
  has_apriori_framework = FALSE
)

# Print-only comparison:
methodology_decision_aid(interactive = FALSE)
} # }
```
