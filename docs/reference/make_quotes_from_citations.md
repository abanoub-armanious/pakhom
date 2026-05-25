# Construct QuoteProvenance objects from a list of Anthropic citations

Batch convenience over
[`make_quote_from_citation`](https://abanoub-armanious.github.io/pakhom/reference/make_quote_from_citation.md)
when all citations share the same attribution metadata (typical for a
single AI call's output where one entry was passed and one code/theme is
being attributed). For per-citation distinct attribution, callers should
use [`Map()`](https://rdrr.io/r/base/funprog.html) or iterate manually.

## Usage

``` r
make_quotes_from_citations(
  citations,
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

- citations:

  List of citation objects (`ai_complete()$citations`).

- documents:

  Same documents list passed to
  [`ai_complete()`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md).

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

List of `QuoteProvenance` objects (unverified). Empty list when
`citations` is empty.

## Details

Returns a list in the same order as `citations` so callers can zip the
result with parallel structures (e.g., a per-segment code list). Empty
input returns [`list()`](https://rdrr.io/r/base/list.html) (not an
error).
