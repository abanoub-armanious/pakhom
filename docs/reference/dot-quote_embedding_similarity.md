# Compute embedding cosine similarity between a quote and a source text

Used by the verification ladder's step 4. Embeds both texts via the
provider's embedding endpoint, then takes the cosine of the resulting
vectors. Returns NA_real\_ if embeddings are unavailable for the
provider (Anthropic doesn't currently expose embeddings).

## Usage

``` r
.quote_embedding_similarity(quote_text, source_text, provider)
```
