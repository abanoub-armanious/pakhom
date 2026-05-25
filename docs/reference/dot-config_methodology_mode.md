# Read `config$methodology$mode` defensively

Several pipeline + report sites need to read the methodology mode out of
a config that may be a bare list (where `$methodology` is absent) or a
partially-built ThematicConfig. This helper centralizes the
tryCatch-on-NULL pattern so future drift in one site can't diverge from
the others.

## Usage

``` r
.config_methodology_mode(config)
```

## Arguments

- config:

  A list or ThematicConfig.

## Value

Character scalar mode, or `NULL`.

## Details

Returns `NULL` when the field is missing, NA, or empty – the "unknown
methodology" path the various stamping helpers handle.
