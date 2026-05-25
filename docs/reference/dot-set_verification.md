# Set verification fields on a quote

Phase 58 Tier 7 M-13/E-19: optional `failure_reason` populates
`verification_failure_reason` for fabricated / drifted statuses. NA when
the status is verified\_\* (the field carries meaning only when
verification failed).

## Usage

``` r
.set_verification(
  quote,
  status,
  method,
  score,
  verified_at,
  failure_reason = NA_character_
)
```
