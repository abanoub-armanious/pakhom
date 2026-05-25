# Verify a quote against its source text via the four-step ladder

Runs the verification ladder in order; the first match wins and sets
`verification_status` accordingly. Steps:

1.  Strict offline string match (status `"verified_exact"`, method
    `"string_match"`, score 1.0)

2.  Normalized match: whitespace collapsed, smart quotes ASCII'd, NFC
    normalization, case-folded (status `"verified_fuzzy"`, method
    `"normalized_match"`, score 0.95)

3.  Substring search fallback: looks for normalized exact_text anywhere
    in the source; if found, corrects start_char/end_char (status
    `"verified_fuzzy"`, method `"substring_search"`, score 0.85)

4.  Embedding cosine similarity: requires `provider`; computes cosine
    between quote and source-text embeddings; matches if \>=
    `.QUOTE_EMBEDDING_VERIFICATION_THRESHOLD` (status
    `"verified_fuzzy"`, method `"embedding_cosine"`, score = cosine
    value). Skipped silently if `provider` is NULL or doesn't support
    embeddings (the previous status sticks).

## Usage

``` r
verify_quote(quote, source_text, provider = NULL)
```

## Arguments

- quote:

  A `QuoteProvenance` object from
  [`make_quote`](https://abanoub-armanious.github.io/pakhom/reference/make_quote.md).

- source_text:

  Character. Current source document text (re-fetched at verification
  time; may differ from the text used at attribution time – that's how
  drift is detected).

- provider:

  Optional AIProvider. When supplied, enables the embedding-similarity
  step (4). When NULL, the ladder stops at step 3.

## Value

The `QuoteProvenance` object with verification fields updated.
`verified_at` is set to
[`Sys.time()`](https://rdrr.io/r/base/Sys.time.html) ISO-8601.

## Details

Drift detection: before running the ladder, the source text is re-hashed
and compared to the quote's `source_text_sha256`. If the hashes differ
AND none of the ladder steps match, status becomes `"drifted"` (the
corpus changed since attribution). This is distinguished from
`"fabricated"` because it suggests the quote was real once but the
source has been edited.

Failure mode: if all ladder steps fail and there is no drift, status
becomes `"fabricated"`. Caller should write the quote to the
fabrication_log via
[`log_fabrication`](https://abanoub-armanious.github.io/pakhom/reference/log_fabrication.md).
