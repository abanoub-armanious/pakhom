# AI-driven refresh of a single high-frequency code's description

Phase 58 Tier 2 C-5: re-prompts the AI with a sample of the segments the
code has accumulated and asks for a description that captures the SHARED
conceptual core across them. The pre-Phase-58 codebook anchored each
description to the FIRST segment that created the code, so
high-frequency codes (e.g. Compulsive Eating Behavior, freq=1127)
carried descriptions that described only one of many distinct meanings
the code accumulated.

## Usage

``` r
.refresh_code_description(
  provider,
  code_name,
  current_description,
  sample_segments,
  audit_log = NULL,
  response_cache = NULL,
  methodology_override = NULL
)
```

## Details

Returns NULL on AI failure (caller leaves description unchanged but
still bumps last_description_refresh_at to avoid retrying on every
cadence).
