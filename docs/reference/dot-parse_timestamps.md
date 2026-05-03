# Attempt to parse a character vector of timestamps

Tries multiple common datetime formats via
[`as.POSIXct()`](https://rdrr.io/r/base/as.POSIXlt.html) and returns the
first format that successfully parses the majority of non-NA values.
Falls back to `NA` for entries that cannot be parsed.

## Usage

``` r
.parse_timestamps(x)
```

## Arguments

- x:

  Character vector of timestamp strings

## Value

POSIXct vector (NA where parsing failed)
