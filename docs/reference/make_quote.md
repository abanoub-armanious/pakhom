# Construct a Quote provenance object

Builds a structured quote with all provenance metadata fields populated
(or set to sensible NA defaults). The quote is created in the
`"unverified"` state; pass it through
[`verify_quote`](https://abanoub-armanious.github.io/pakhom/reference/verify_quote.md)
to run the four-step verification ladder.

## Usage

``` r
make_quote(
  source_doc_id,
  source_doc_type,
  source_text,
  start_char,
  end_char,
  exact_text,
  ai_paraphrase = NA_character_,
  attributed_theme_id = NA_character_,
  attributed_code_id = NA_character_,
  ai_model = NA_character_,
  ai_call_id = NA_character_,
  citation_source = "pipeline_derived"
)
```

## Arguments

- source_doc_id:

  Character. Identifier of the source document (e.g., `"post_abc123"`,
  `"comment_def456"`). Pulled from `data$std_id` in the standard
  pipeline.

- source_doc_type:

  Character. Document type (e.g., `"reddit_post"`, `"reddit_comment"`,
  `"interview_segment"`).

- source_text:

  Character. The FULL source document text. Used to compute
  `source_text_sha256`; not stored on the quote.

- start_char:

  Integer. 0-indexed inclusive start offset (matches Anthropic Citations
  API conventions).

- end_char:

  Integer. 0-indexed exclusive end offset.

- exact_text:

  Character. The verbatim slice from the source
  (`substr(source_text, start_char + 1, end_char)`). Stored directly to
  avoid recomputing during rendering.

- ai_paraphrase:

  Optional character. AI's paraphrase of the quote, if any.
  `NA_character_` when no paraphrase exists.

- attributed_theme_id:

  Optional character. Theme ID this quote supports.

- attributed_code_id:

  Optional character. Code ID this quote supports.

- ai_model:

  Optional character. Model that produced the attribution (e.g.,
  `"gpt-4o-2024-08-06"`).

- ai_call_id:

  Optional character. `request_id` of the AI call that produced the
  quote. Joins to the audit log's `request_id`.

- citation_source:

  One of `.VALID_QUOTE_CITATION_SOURCES`. Default `"pipeline_derived"`
  (the safest assumption when unknown).

## Value

A `QuoteProvenance` S3 object (a list with class).

## Details

`quote_id` is computed deterministically as
`paste0("qte_", sha1(source_doc_id + start_char + end_char + exact_text))`
so the same quote always has the same ID across runs – critical for
replay (OS.5) and cross-run comparison.

`source_text_sha256` is a hash of the FULL source document text at
attribution time. If the source corpus changes between runs (researcher
edits a post, re-scrapes data, etc.), `verify_quote` can detect the
drift by re-hashing.
