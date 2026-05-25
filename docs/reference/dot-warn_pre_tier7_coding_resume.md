# Warn when resuming from a pre-Tier-7 coding checkpoint

Phase 58 Tier 7 cross-tier audit (paralleling the Tier 6 helper at the
correlations checkpoint): a pre-Tier-7 coding state carries
`QuoteProvenance` objects produced before the V-6/L-3 offset fix
(JSON-escape bug) and the M-13/E-19 failure-reason field. Detect by
inspecting the first quote's field set. Emit a `log_warn` explaining the
methodology-era drift and how to realign (delete progressive_coding from
checkpoint.rds and re-run from step 3). Silent on fresh runs (no
checkpoint = no warning).

## Usage

``` r
.warn_pre_tier7_coding_resume(coding_state)
```

## Arguments

- coding_state:

  A loaded ProgressiveCodingState, possibly NULL.

## Value

Invisible NULL.
