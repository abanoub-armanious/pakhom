# Extract citations from a parsed Anthropic response content array

Walks the `parsed$content` list and collects every citation attached to
a text block. Each citation is preserved with Anthropic's field names
exactly (no remapping) so the bridge in `R/quote_provenance.R` can
dispatch on `type`:

- `char_location`: `start_char_index`, `end_char_index` (0-indexed,
  exclusive end) – pakhom's QuoteProvenance schema uses the same
  convention.

- `page_location`: `start_page_number`, `end_page_number` (1-indexed,
  exclusive end) – PDF sources.

- `content_block_location`: `start_block_index`, `end_block_index`
  (0-indexed, exclusive end) – custom_content sources.

All three types share `type`, `cited_text`, `document_index`,
`document_title`.

## Usage

``` r
.anthropic_extract_citations(parsed_content)
```

## Arguments

- parsed_content:

  The `parsed$content` list from the API response (may be a list of
  blocks, or empty).

## Value

List of citation objects in the order they were emitted across all text
blocks. Empty list when no citations were returned.

## Details

Robustness: skips non-text blocks (tool_use blocks have no citations);
skips text blocks whose `citations` is NULL or empty; preserves unknown
citation types unchanged for forward compatibility.
