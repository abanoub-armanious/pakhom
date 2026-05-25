# Resolve a built-in framework name to its inst/extdata path

Returns the file path for built-in framework aliases, or NULL when the
input is not an alias. Aliases:

- `"tpb"` – Theory of Planned Behavior

- `"comb"` – COM-B (Capability-Opportunity-Motivation)

- `"tdf"` – Theoretical Domains Framework

## Usage

``` r
.resolve_builtin_framework(name)
```
