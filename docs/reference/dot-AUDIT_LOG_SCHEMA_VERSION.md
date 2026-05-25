# Audit log schema version

Phase 58 Tier 8 H-11: every audit record stamps this version so a
downstream replayer / cross-run comparator can detect schema drift.
Pre-Tier-8 the ai_decisions.jsonl was the only first-class artifact
lacking a version stamp (live tracker artifacts all carry
schema_version="1.0.0"). Bump this constant when the record schema
changes incompatibly.

## Usage

``` r
.AUDIT_LOG_SCHEMA_VERSION
```

## Format

An object of class `character` of length 1.

## Details

- 1.0.0 (Phase 58 Tier 8): initial stamping. Schema includes timestamp,
  step, decision_type, methodology_mode (when set), plus arbitrary
  user-supplied `...` fields.
