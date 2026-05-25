# Mode 1 expected-files helper for verify_run_integrity

Mode 1 produces a different artifact set from Modes 2/3 (no sentiment,
no correlations, no theme_entries CSVs). Universal Tier-0 + Tier-1
artifacts are still required (run_metadata, methodology rules,
fabrication log, audit log). Mode 1-specific outputs: reflection_log
JSON + provocations CSV + provocation_attempts CSV + coverage JSON.

## Usage

``` r
.verify_run_integrity_mode1(run_dir, config = list())
```
