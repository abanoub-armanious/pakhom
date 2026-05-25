# Process one coded segment from either the schema or citations path

Builds a path-appropriate `QuoteProvenance` (free-form via `make_quote`
or citation-bridged via `make_quote_from_citation`), runs the
verification ladder, and – if not fabricated – updates the codebook and
accumulators.

## Usage

``` r
.handle_one_coded_segment(
  seg,
  seg_index,
  use_citations,
  use_framework = FALSE,
  framework_spec = NULL,
  ai_meta,
  documents,
  text,
  entry_id,
  state,
  acc,
  audit_log,
  fabrication_log
)
```

## Value

Updated `state` (immutable per-call; the codebook is updated when a
non-fabricated segment is processed).

## Details

Mutates `acc$entry_codes` and `acc$entry_segments` via environment
reference so the caller doesn't have to thread them through return
values.
