# Compute empirical coding benchmarks from parsed QDA codebooks

Aggregates statistics across all parsed codebooks to establish
data-driven thresholds for code specificity, consolidation targets, and
theme structure.

## Usage

``` r
compute_coding_benchmarks(studies)
```

## Arguments

- studies:

  PreviousStudies object with \$codebook fields populated

## Value

List of benchmarks, or NULL if no codebooks available
