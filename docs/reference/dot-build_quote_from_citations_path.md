# Build a QuoteProvenance for the citations path (offsets from Anthropic)

Pairs the model's segment with the corresponding citation by:

1.  Emission-order match (`citations[[seg_index]]` – the most common
    success case when the model emits one citation per segment).

2.  Cited-text string match (handles cases where the model emits
    citations in a different order than segments, or extra commentary
    citations interleave with the JSON).

3.  Fallback to the schema path's freeform constructor, leaving the
    verification ladder to recover offsets via substring search.
    citation_source becomes `"model_freeform"` so the dashboard
    distinguishes citation-API-grounded quotes from those that fell
    back.

## Usage

``` r
.build_quote_from_citations_path(
  seg_text,
  seg_index,
  citations,
  documents,
  text,
  entry_id,
  code_key,
  ai_meta
)
```
