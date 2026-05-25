# Build the schema-path user prompt (existing T1.2 flow)

Phase 58 Tier 7 V-6 / L-3: the pre-Tier-7 implementation JSON-escaped
the entry text via `jsonlite::toJSON(truncated_text, auto_unbox = TRUE)`
then stripped the outer quotes and wrapped the result in literal quote
marks. This made embedded
`"} / \code{\} characters appear as 2-character escape sequences in the prompt -- so the AI's emitted \code{start_char} / \code{end_char} offsets referenced the ESCAPED form, but \code{verify_quote} re-fetches the UN-escaped source and tries to match at the same indices. Every entry with a single \code{"`
silently produced an off-by-one verification failure that Step 3
substring-search papered over, driving the Phase 57 run to 99.89%
verified_fuzzy / 0.11% verified_exact. Fenced fence the entry text with
explicit XML-style delimiters and pass the text verbatim, so the AI sees
exactly the same character offsets that the verifier will check.

## Usage

``` r
.build_progressive_schema_user_prompt(truncated_text)
```
