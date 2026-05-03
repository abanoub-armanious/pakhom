# Compute dynamic batch indices based on token budget

Splits a set of text entries into batches that respect a maximum token
budget per batch. Longer entries get fewer per batch; shorter entries
get more. The fixed `max_batch_size` acts as a ceiling.

## Usage

``` r
compute_dynamic_batches(
  texts,
  max_batch_tokens,
  max_batch_size = 50,
  chars_per_entry = 1500
)
```

## Arguments

- texts:

  Character vector of text entries

- max_batch_tokens:

  Maximum tokens per batch (for the entries portion)

- max_batch_size:

  Hard ceiling on entries per batch (fallback/safety)

- chars_per_entry:

  Max characters that will be used per entry in the prompt (e.g., 1500
  for relevance, 800 for sentiment). Entries are virtually truncated to
  this length for token estimation.

## Value

List of integer vectors, each containing row indices for one batch
