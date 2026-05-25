# Compute multiple p-value adjustments (raw, BH FDR, Bonferroni)

Internal helper computing raw, Benjamini-Hochberg FDR, and Bonferroni
adjustments simultaneously over a vector of p-values. Used by
`calculate_correlations`, `compare_theme_groups`, and
`test_theme_cooccurrence` to provide a tiered presentation aligned with
the package's exploratory-analysis framing.

## Usage

``` r
.compute_p_adjustments(p_values)
```

## Arguments

- p_values:

  Numeric vector of raw p-values (NAs preserved)

## Value

Named list with three numeric vectors of the same length: `raw`, `bh`,
`bonferroni`

## Details

Rationale: themes are inductively derived from the same data the
correlations are computed on, so single-method p-adjustment can mislead.
Reporting raw + BH + Bonferroni alongside effect sizes lets reviewers
judge associations under multiple inferential regimes (cf. Rothman 1990,
Epidemiology 1(1):43-46; ScienceDirect S0895435625000216, J Clin
Epidemiol 2025; PMC12359981 on intra-correlation pitfalls for BH).
