# Construct a QuoteProvenance from a single Anthropic citation

Bridges one Anthropic citation object (the shape produced by
`R/02_ai_providers.R::.anthropic_extract_citations`) to a
`QuoteProvenance` object with
`citation_source = "anthropic_citations_api"`. The constructed quote is
in the `"unverified"` state; chain through
[`verify_quote`](https://abanoub-armanious.github.io/pakhom/reference/verify_quote.md)
to run the four-step verification ladder.

## Usage

``` r
make_quote_from_citation(
  citation,
  documents,
  attributed_theme_id = NA_character_,
  attributed_code_id = NA_character_,
  ai_model = NA_character_,
  ai_call_id = NA_character_,
  ai_paraphrase = NA_character_,
  source_doc_type_default = "data_entry"
)
```

## Arguments

- citation:

  A single citation object from `ai_complete()$citations`. Must have
  `type`, `document_index`, and the type-specific span fields populated.

- documents:

  The same documents list passed to
  [`ai_complete()`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md).
  Each element must have `$id` and `$text`; an optional `$type` field
  overrides the default `source_doc_type`.

- attributed_theme_id, attributed_code_id:

  Optional attribution metadata. The bridge cannot infer these from the
  citation alone – the caller pairs each citation with the code/theme it
  supports.

- ai_model, ai_call_id:

  The model and request_id from the
  [`ai_complete()`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md)
  response that produced this citation. Stored on the QuoteProvenance
  for audit log linkage.

- ai_paraphrase:

  Optional paraphrase, if the AI rephrased rather than directly quoted.
  Defaults to `NA_character_` since Citations API returns verbatim
  slices.

- source_doc_type_default:

  Default `source_doc_type` when the document doesn't specify `$type`.
  Defaults to `"data_entry"` matching pakhom's coding pipeline
  convention.

## Value

A `QuoteProvenance` object (unverified state) with
`citation_source = "anthropic_citations_api"`.

## Details

Supported citation types:

- `char_location` (plain text source) – maps directly to
  `start_char`/`end_char`. The `cited_text` is stored as `exact_text`;
  the verification ladder confirms it matches
  `source[start_char:end_char]` byte-for-byte.

- `page_location` (PDF source) – not yet supported. PDF inputs aren't
  part of pakhom's current data model. Errors with a clear message
  rather than silently producing a malformed quote.

- `content_block_location` (custom_content source) – not yet supported
  for the same reason. Phase 21a uses plain_text source exclusively; if
  a future caller switches to custom_content, the bridge needs a
  block-index-to-char-offset mapping (caller would supply per-document
  block boundaries).

Document lookup: `citation$document_index` is 0-indexed into the
`documents` list passed to
[`ai_complete()`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md).
The bridge converts to 1-indexed for R.
