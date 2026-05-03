# Estimate token count for text

Tries the `tiktoken` R package for accurate BPE token counts. Falls back
to a 4-characters-per-token heuristic if tiktoken is unavailable or
errors.

## Usage

``` r
estimate_tokens(text, model = "gpt-4o")
```

## Arguments

- text:

  Character string(s) to estimate

- model:

  Model name for tiktoken encoding (default: "gpt-4o")

## Value

Integer vector of estimated token counts
