# Normalize Anthropic usage payload to the canonical pakhom shape

Anthropic returns `input_tokens` and `output_tokens` (no total). Maps to
the OpenAI-style `prompt_tokens`/`completion_tokens` naming and computes
the total. If either count is missing the total is `NA_integer_` (NA
propagates through integer addition).

## Usage

``` r
.normalize_usage_anthropic(usage)
```

## Arguments

- usage:

  Parsed list from Anthropic response `$usage`

## Value

list with integer fields prompt_tokens, completion_tokens, total_tokens
