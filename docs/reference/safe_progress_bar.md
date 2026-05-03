# Create a progress bar that works in non-interactive/background mode

When R runs without a terminal (e.g., background jobs), the progress
package can hang. This returns a no-op progress bar in that case.

## Usage

``` r
safe_progress_bar(format, total)
```

## Arguments

- format:

  Progress bar format string

- total:

  Total number of ticks

## Value

A progress_bar object or a no-op list with a \$tick() method
