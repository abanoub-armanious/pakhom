# Append a fabricated quote to the fabrication log

Silently no-ops on a non-FabricationLog `flog`, on a NULL quote, or on a
quote whose verification_status is not `"fabricated"` (the
single-purpose CSV is for fabrications only; `"drifted"` and
`"unverified"` have other render-time treatments).

## Usage

``` r
log_fabrication(flog, quote)
```

## Arguments

- flog:

  A FabricationLog from
  [`init_fabrication_log`](https://abanoub-armanious.github.io/pakhom/reference/init_fabrication_log.md).

- quote:

  A QuoteProvenance with verification_status = "fabricated".

## Value

Invisibly returns `flog`.
