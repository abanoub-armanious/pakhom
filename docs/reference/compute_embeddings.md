# Compute text embeddings via AI provider

Calls the embedding model to compute vector representations of text.
Currently supports OpenAI's text-embedding models. Falls back gracefully
if the provider doesn't support embeddings.

## Usage

``` r
compute_embeddings(provider, texts, model = NULL)
```

## Arguments

- provider:

  AIProvider object

- texts:

  Character vector of texts to embed

- model:

  Embedding model override (NULL uses provider default)

## Value

Numeric matrix (rows = texts, cols = dimensions), or NULL on failure
