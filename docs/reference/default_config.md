# Create a default configuration object

Returns a working starter config. **Methodology mode is the load-bearing
architectural decision in pakhom** – it determines AI agency, output
stamping, and reporting requirements. Pass `methodology` explicitly:

## Usage

``` r
default_config(methodology = NULL)
```

## Arguments

- methodology:

  One of `"reflexive_scaffold"`, `"codebook_collaborative"`, or
  `"framework_applied"`. **Mandatory**: per AC3 ("no default mode;
  explicit declaration mandatory"), `methodology = NULL` produces an
  error rather than silently defaulting. Run
  [`methodology_decision_aid()`](https://abanoub-armanious.github.io/pakhom/reference/methodology_decision_aid.md)
  for guidance on the choice.

## Value

A ThematicConfig S3 object with all defaults

## Details


      default_config("reflexive_scaffold")      # Mode 1: AI as provocateur
      default_config("codebook_collaborative")  # Mode 2: AI proposes, researcher gates
      default_config("framework_applied")       # Mode 3: AI applies researcher's framework

Calling `default_config()` with no argument emits a warning and falls
back to `"codebook_collaborative"` (the mode that best matches v1.x
behavior and serves the largest existing user population). The warning
exists by design: per Spool 2011 (\>95\\ a silent default would let
users inherit a methodology without conscious choice – contrary to
pakhom's architectural commitment that methodology declaration must be
explicit. Run
[`methodology_decision_aid`](https://abanoub-armanious.github.io/pakhom/reference/methodology_decision_aid.md)
for guidance on choosing.

Note: `.config_defaults()` (internal) returns the bare schema with
`methodology$mode = NULL`, so user-supplied YAMLs that omit the
methodology section fail validation with a clear error rather than
silently inheriting a default. `default_config()` is the only entry
point that pre-fills mode (and only with the warning above).

## See also

[`methodology_decision_aid`](https://abanoub-armanious.github.io/pakhom/reference/methodology_decision_aid.md)
for guidance on choosing a methodology mode;
[`validate_methodology_mode`](https://abanoub-armanious.github.io/pakhom/reference/validate_methodology_mode.md)
for the underlying validator.
