# Determine period type from a vector of parsed timestamps

Calculates the date span and selects granularity:

- \< 30 days: `"daily"`

- 30 days – 6 months: `"weekly"`

- 6 months – 2 years: `"monthly"`

- \> 2 years: `"quarterly"`

## Usage

``` r
.detect_time_periods(timestamps)
```

## Arguments

- timestamps:

  POSIXct vector (NAs removed internally)

## Value

Character string: one of `"daily"`, `"weekly"`, `"monthly"`,
`"quarterly"`
