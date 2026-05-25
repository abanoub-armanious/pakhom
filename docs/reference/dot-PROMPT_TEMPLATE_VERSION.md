# Prompt-template generation marker (Phase 58 Tier 9, M-T7-1)

Stamped into `run_metadata.json$prompt_template_version` so a future
OS.5 replayer can detect when a loaded cache was produced under a
different prompt template generation. The value maps 1:1 to the most
recent Phase 58 tier that materially changed the prompt body. Bump
whenever a prompt rewrite shifts the AI-visible character offsets or the
response schema (e.g. Phase 58 Tier 7's `<entry_text>` fence change).

## Usage

``` r
.PROMPT_TEMPLATE_VERSION
```

## Format

An object of class `character` of length 1.

## Details

- `"pre_phase58"` – pre-Phase-58 prompt (JSON-escaped `Entry text:`
  wrapping)

- `"phase58_tier7"` – current generation, post-V-6/L-3 prompt rewrite
  (`<entry_text>...</entry_text>` fence with verbatim text; offset
  arithmetic preserved through `.escape_entry_text_fence`)
