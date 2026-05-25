# Check whether a citation's cited_text equals a segment's claimed text

Uses normalized comparison (whitespace + smart quotes + case) so trivial
formatting differences in the model's JSON encoding don't cause spurious
fallback to model_freeform. The verification ladder will further verify
the byte identity once the QuoteProvenance is built.

## Usage

``` r
.citation_text_matches(citation, seg_text)
```
