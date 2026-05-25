# Validate a methodology mode declaration

Sprint-4 multi-mode architecture: every run must declare its
methodological posture (reflexive_scaffold / codebook_collaborative /
framework_applied). The declaration determines which AI behaviors are
permitted, which artifacts are mandatory, and which report sections are
generated. There is intentionally no default; missing declarations
produce an actionable error pointing to the decision aid.

## Usage

``` r
validate_methodology_mode(
  mode,
  allow_null = FALSE,
  caller = "validate_methodology_mode"
)
```

## Arguments

- mode:

  Character scalar; the methodology mode name

- allow_null:

  If TRUE, NULL is accepted (used by .config_defaults() which is a bare
  schema; user-facing validate_config() always passes allow_null =
  FALSE)

- caller:

  Name of the calling function (for error messages)
