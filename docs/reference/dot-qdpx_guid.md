# Generate a unique GUID for QDPX elements

Produces a deterministic-looking but unique identifier prefixed with
"TA-" (for pakhom). Uniqueness is ensured by combining the current
timestamp, a random integer, and an optional tag.

## Usage

``` r
.qdpx_guid(tag = NULL)
```

## Arguments

- tag:

  Optional character string appended for readability

## Value

Character scalar, e.g. "TA-20250520143012-472913-code"
