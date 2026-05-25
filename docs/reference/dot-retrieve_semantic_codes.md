# Phase 58 Tier 0 C-6: per-entry semantic top-K retrieval against codebook

Computes cosine similarity between the current entry's embedding and
each code's `name: description` embedding, returning the top-K indices.
Uses + populates `state$semantic_cache$code_embeddings` so each code is
embedded at most once across the full coding run. Returns `integer(0)`
when the provider doesn't support embeddings, when no
provider/entry_text is supplied, or when the API call fails. Always
degrades gracefully – the caller falls back to frequency-only.

## Usage

``` r
.retrieve_semantic_codes(state, code_data, entry_text, provider, top_k)
```

## Arguments

- state:

  ProgressiveCodingState carrying \$codebook + \$semantic_cache.

- code_data:

  List of per-code records (key/name/desc/freq/type), matching the same
  row-order indices used by the caller.

- entry_text:

  Current entry's raw text (single character scalar).

- provider:

  AIProvider used to compute embeddings.

- top_k:

  Maximum number of semantic-retrieval indices to return.

## Value

list(indices = integer, new_embeddings = named list of vectors)
