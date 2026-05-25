# Codebook-summary builder (legacy interface, frequency + recency only)

Back-compat wrapper around .build_codebook_summary_with_retrieval that
returns just the summary string. Used by existing tests and any caller
that doesn't have an entry_text / provider to do semantic retrieval.
Default `max_codes = 80` preserves the pre-Phase-58 behavior for callers
that don't override it; the production callsite in
`.code_entry_progressive` uses the with_retrieval variant directly with
`max_codes = 150`.

## Usage

``` r
.build_codebook_summary(state, max_codes = 80, recent_window = 20)
```
