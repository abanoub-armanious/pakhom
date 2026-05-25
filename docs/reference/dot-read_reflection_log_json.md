# Read a reflection_log.json back into a ResearcherReflectionLog

Audit A H3 (phase 31): a previous version used `simplifyVector = TRUE`,
which collapsed `provocations` (a list of uniform-shape objects) into a
row-frame AND stripped the `Provocation` / `QuoteProvenance` S3 class
tags from the nested elements. On resume, downstream code that gates on
`inherits(p, "Provocation")` or
`inherits(p$provenance, "QuoteProvenance")` (notably
`.provocation_to_row` and the per-category provocation functions) would
silently emit NA-cited rows. The fix here is two-step: (1) read with
simplifyVector=FALSE so the provocations list keeps its list-of-lists
shape; (2) explicitly re-class each provocation + its provenance after
the read. The data.frame slots (provocation_attempts / skipped_themes /
positionality_history) are then re-coerced back to data.frames since
simplifyVector=FALSE leaves them as lists.

## Usage

``` r
.read_reflection_log_json(path)
```
