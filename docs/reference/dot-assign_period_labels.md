# Convert a POSIXct vector into period labels

Convert a POSIXct vector into period labels

## Usage

``` r
.assign_period_labels(timestamps, period_type)
```

## Arguments

- timestamps:

  POSIXct vector

- period_type:

  One of `"daily"`, `"weekly"`, `"monthly"`, `"quarterly"`

## Value

Character vector of period labels (same length as input)
