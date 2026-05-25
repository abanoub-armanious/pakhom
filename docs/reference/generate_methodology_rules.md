# Generate the methodology-rules text for a config

Returns a single character string suitable for prepending to a system
prompt. The string includes mode-specific rules + universal Tier-0
rules. When `config$methodology$mode` is NULL or invalid, returns the
universal-rules-only string with a warning – this is a soft fallback
rather than a hard error because legacy/test contexts may instantiate
ai_complete without a full config and we don't want to break those
paths.

## Usage

``` r
generate_methodology_rules(config, inductive_pass = FALSE)
```

## Arguments

- config:

  A ThematicConfig (or list with the same shape).

- inductive_pass:

  Logical. When TRUE, select the inductive-pass rule variant (Phase 54
  abductive emergent-themes pass). Default FALSE.

## Value

Character: the rules block, prefixed with a header. Empty string when
nothing meaningful can be generated.

## Details

Phase 56: `inductive_pass = TRUE` selects an alternate mode rule variant
for the Phase 54 abductive emergent-themes pass. The default Mode 3 rule
says "Do NOT generate new framework constructs during coding"; under the
inductive pass that instruction directly contradicts the prompt asking
the AI to inductively code anomaly segments. The variant omits the "do
not generate" clause and instructs the AI to generate inductive codes
for anomaly residuals. Only Mode 3 has a meaningful inductive variant;
for other modes the flag is a no-op.
