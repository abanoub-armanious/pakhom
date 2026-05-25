# Select appropriate correlation method for a variable pair

Phase 58 Tier 6 H-13 hardening: binary x ordinal pairs now route through
Spearman (which yields the rank-biserial coefficient in that degenerate
case). Pre-Phase-58 they routed to Pearson via the general
binary+non-binary rule, which produces point-biserial – a coefficient
that assumes the non-binary side is interval-scaled. For AI-elicited
sentiment / intensity / Likert scores the support is genuinely ordinal,
not interval, and point-biserial is methodologically suspect. Binary x
continuous remains Pearson (point-biserial is appropriate when the
support genuinely is continuous).

## Usage

``` r
.select_pair_method(x, y, type_x, type_y)
```

## Arguments

- x:

  Numeric vector

- y:

  Numeric vector

- type_x:

  Variable type ("binary", "ordinal", "continuous")

- type_y:

  Variable type ("binary", "ordinal", "continuous")

## Value

Character: "pearson" or "spearman"
