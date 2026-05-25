# Verify a batch of quotes against a corpus

Convenience wrapper that looks up each quote's source text in
`corpus_lookup` (a named list keyed by source_doc_id) and runs
[`verify_quote`](https://abanoub-armanious.github.io/pakhom/reference/verify_quote.md)
on each. Quotes whose source is missing from the corpus are marked
`"drifted"`.

## Usage

``` r
verify_quotes(quotes, corpus_lookup, provider = NULL)
```

## Arguments

- quotes:

  List of QuoteProvenance objects.

- corpus_lookup:

  Named list: source_doc_id -\> source_text.

- provider:

  Optional AIProvider for the embedding ladder step.

## Value

List of QuoteProvenance objects, each with verification fields
populated.
