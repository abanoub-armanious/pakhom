# Compute the effective per-entry character cap given a provider + config

Phase 50f replacement for the hardcoded `.MAX_ENTRY_CHARS = 8000L`.
Resolution order:

1.  If `config$ai$max_entry_chars` is a positive integer, use it
    verbatim (researcher's explicit override).

2.  Otherwise, derive from `provider$context_window` (in tokens) by
    reserving ~60\\ completion and assigning the remaining ~40\\ Convert
    tokens to chars at ~4 chars/token (English averages 4.0-4.5 chars
    per BPE token; 4 is a conservative under-estimate so we don't
    over-fill context).

3.  Floor at 8000L (the legacy default) so behavior is never worse than
    the prior hardcode for very-small-context models.

## Usage

``` r
.effective_max_entry_chars(provider, config = list())
```

## Arguments

- provider:

  AIProvider object with `$context_window`

- config:

  Pipeline config (reads `config$ai$max_entry_chars`)

## Value

Integer character cap.
