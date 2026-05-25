# Build a console banner string for the methodology mode

Used at run start / end to print a one-line banner identifying the mode
in force. Not directly printed – callers
[`cat()`](https://rdrr.io/r/base/cat.html) or `log_info()` the result so
output formatting (prefixes, colors from the logger) is consistent.

## Usage

``` r
stamp_methodology_console(mode, run_id = NULL)
```

## Arguments

- mode:

  Character methodology mode.

- run_id:

  Optional run identifier.

## Value

Character (single line).
