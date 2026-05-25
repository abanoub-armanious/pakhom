# Footnote explaining the Mode 3 + Anthropic Citations API silent bypass

Phase 32 (audit MEDIUM \#5 / C3): when `config$methodology$mode` is
`"framework_applied"` (Mode 3) AND `config$ai$provider` is
`"anthropic"`, the Citations API path in `R/02_ai_providers.R` is
deliberately dropped. The constraint is structural at the Anthropic API
level: forced `tool_use` schema (which Mode 3 requires to constrain
coding to framework constructs) and the Citations API output format are
mutually exclusive on the same response. The Mode 3 coding pipeline
therefore relies on the verification ladder's DETECTION-only path
(model_freeform + offline string match) instead of the API's PREVENTION
layer.

## Usage

``` r
.tier0_citations_api_bypass_footnote(config = NULL)
```

## Details

Without this footnote, a reviewer reading the Tier-0 dashboard for a
Mode 3 + Anthropic run would see only *"Model freeform (detection
only)"* and reasonably wonder why the Anthropic-specific prevention
layer is missing – they could infer a bug rather than a deliberate
architectural constraint. The footnote makes the architectural reason
explicit.

Returns "" when the trigger condition does not apply (Mode 1 / Mode 2
runs, or non-Anthropic providers, or NULL config).
