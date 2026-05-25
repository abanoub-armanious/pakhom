# Unwrap a methodology-stamped JSON envelope

Phase 58 audit followup C-T8A-1: `stamp_methodology_json`
(R/output_stamping.R:240-268) wraps the original payload in
`{"_methodology_stamp": ..., "_payload": <original>}`. The transparency
bundler reads disk artifacts that may or may not have been stamped
(coverage_card.json IS stamped when produced via `write_corpus_coverage`
with a non-null methodology mode; the Tier 8 H-10 callsite always
stamps). Pre-followup readers accessed fields like `cov$n_processed`
directly – which returned NULL on every real run because the data was
under `_payload`. The fixture tests passed only because the synthetic
stub skipped the stamp. This helper is the single source of truth for
unwrapping; all readers route through it.

## Usage

``` r
.tr_unwrap_payload(x)
```
