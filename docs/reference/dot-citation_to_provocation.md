# Convert one parsed citation row to a Provocation

Takes the AI's `{entry_id, char_start, char_end, exact_text, reason}`
citation, looks up the source text from `data`, builds a
QuoteProvenance, runs verify_quote (T0.1 universal), and assembles a
Provocation. Returns NULL when the citation is fabricated – per AC7,
fabricated provocations are dropped silently from the provocation list
(and logged to the audit log if supplied).

## Usage

``` r
.citation_to_provocation(
  cit,
  theme_name,
  category,
  data,
  ai_meta,
  audit_log = NULL,
  fabrication_log = NULL
)
```
