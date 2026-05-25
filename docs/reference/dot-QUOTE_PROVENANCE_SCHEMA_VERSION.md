# Current schema version for the quote provenance object

- 1.0.0 (pre-Phase-58-Tier-7): base schema – quote_id, source_doc_id,
  source_doc_type, source_text_sha256, start_char, end_char, exact_text,
  ai_paraphrase, attributed_theme_id, attributed_code_id, ai_model,
  ai_call_id, citation_source, verification_status, verification_method,
  verification_score, verified_at, schema_version.

- 1.1.0 (Phase 58 Tier 7 M-13/E-19): adds `verification_failure_reason`
  – structured attribution for fabricated / drifted quotes naming the
  deepest failed ladder step (step1_offset_mismatch,
  step2_normalized_mismatch, step3_substring_not_found,
  step4_embedding_below_threshold, source_text_sha256_mismatch, etc.).
  NA on verified or unverified quotes.

Bumping the version lets downstream tools (replay_run, cross-run
compare_runs) detect that a loaded QuoteProvenance was produced under a
different schema generation.

## Usage

``` r
.QUOTE_PROVENANCE_SCHEMA_VERSION
```

## Format

An object of class `character` of length 1.
