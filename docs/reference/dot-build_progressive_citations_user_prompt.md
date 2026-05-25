# Build the citations-path user prompt (T0.1 part 3b)

The model receives the entry as a document content block (passed
alongside this prompt by .anthropic_completion when documents is set).
The prompt instructs JSON-mode output where each segment's `text` field
is a verbatim quote from the document; the Anthropic API attaches a
citation to each verbatim quote, producing server-side-guaranteed
character offsets into the source. The model is explicitly instructed
NOT to invent quotes – the QuoteProvenance bridge cross-checks the
model's claim against Anthropic's citation span, and the verification
ladder runs as defense in depth.

## Usage

``` r
.build_progressive_citations_user_prompt()
```
