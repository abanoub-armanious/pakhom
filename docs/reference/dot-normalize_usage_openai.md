# Normalize OpenAI usage payload to the canonical pakhom shape

Normalize OpenAI usage payload to the canonical pakhom shape

## Usage

``` r
.normalize_usage_openai(usage)
```

## Arguments

- usage:

  Parsed list from OpenAI response `$usage`

## Value

list with integer fields prompt_tokens, completion_tokens, total_tokens
(NA_integer\_ when payload missing or fields absent)
