# Verify coded excerpts against source text

Verify coded excerpts against source text

## Usage

``` r
verify_excerpts(data, coding_results, provider = NULL, sample_size = 20)
```

## Arguments

- data:

  Tibble with std_text, std_id columns

- coding_results:

  CodingResults list (or ProgressiveCodingState)

- provider:

  AIProvider (optional, for coherence check)

- sample_size:

  Number of entries to check coherence for

## Value

List with substring_stats, coherence_stats, issues
