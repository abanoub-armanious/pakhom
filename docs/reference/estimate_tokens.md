# Estimate token count for text

Uses a script-aware character-to-token heuristic (~4 chars/token for
Latin/Cyrillic scripts, ~1.5 chars/token for CJK; mixed scripts use a
weighted average). Sufficient for batch-size budgeting where a small
over- or under-estimate is harmless.

## Usage

``` r
estimate_tokens(text, model = "gpt-4o")
```

## Arguments

- text:

  Character string(s) to estimate

- model:

  Reserved for future per-model tuning (currently unused).

## Value

Integer vector of estimated token counts
