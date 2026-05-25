# Count pre-rejection fabrications for the T0.1 dashboard (V-5 helper)

Phase 58 Tier 4 audit MEDIUM \#4 followup: counts fabrication-log
entries using readr's RFC-4180 parser instead of
`readLines() + length(lines) - 1L`. Coded segments routinely contain
newlines (Reddit posts), and the FabricationLog writes the exact_text
field as RFC-4180 quoted-with-embedded-newlines via .csv_quote
(R/quote_provenance.R:861). The pre-fix line-counting approach counted a
single 3-line fabricated quote as 3 fabrications.

## Usage

``` r
.count_pre_rejection_fabrications(
  fabrication_log_path = NULL,
  n_fabricated_caught = NULL
)
```

## Arguments

- fabrication_log_path:

  Absolute path to fabrication_log.csv, or NULL.

- n_fabricated_caught:

  Explicit override (e.g. from FabricationLog\$state\$n_logged); takes
  priority when supplied.

## Value

Integer N caught, or NULL if neither source is available.
