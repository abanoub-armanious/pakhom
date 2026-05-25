# Schema version for run_metadata.json (T1.4 / T1.5)

Consumers (replay_run, summarize_audit_log) read this to gate
back-compat reads. Bumped when the metadata schema changes incompatibly.

## Usage

``` r
.RUN_METADATA_SCHEMA_VERSION
```

## Format

An object of class `character` of length 1.
