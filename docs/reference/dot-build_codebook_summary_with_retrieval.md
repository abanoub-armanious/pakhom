# Phase 58 Tier 0 C-6: codebook summary with additive semantic retrieval

Variant of `.build_codebook_summary` that performs additional top-K
semantic retrieval against the current entry's text on top of the
frequency + recency selection. Returns a list so the caller can persist
any newly-computed code embeddings into the coding state's cache (the
function takes `state` by value – mutating
`state$semantic_cache$code_embeddings` here would be invisible to the
caller).

## Usage

``` r
.build_codebook_summary_with_retrieval(
  state,
  max_codes = 150L,
  recent_window = 20L,
  entry_text = NULL,
  provider = NULL,
  top_k_semantic = 30L
)
```

## Value

list(summary = , new_embeddings = )
