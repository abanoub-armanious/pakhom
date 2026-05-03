# Build segment length guideline from benchmarks, config, or defaults

Priority: (1) empirical benchmarks from prior analyses (averaged across
codebooks), (2) user-configured values, (3) package defaults.

## Usage

``` r
.build_segment_length_guideline(learning_context, config)
```

## Arguments

- learning_context:

  LearningContext (or NULL)

- config:

  Coding config section

## Value

Character string for the prompt
