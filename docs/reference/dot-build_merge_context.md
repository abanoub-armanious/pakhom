# Build merge context string with co-occurrence and embedding similarity

Provides the AI with additional quantitative evidence to inform merge
decisions.

## Usage

``` r
.build_merge_context(new_item, clusters, co_occurrence, code_similarity)
```

## Arguments

- new_item:

  The item being placed

- clusters:

  Current clusters

- co_occurrence:

  Co-occurrence list from .compute_code_cooccurrence

- code_similarity:

  Cosine similarity matrix from embeddings (or NULL)

## Value

Character string to insert into the merge prompt (may be empty)
