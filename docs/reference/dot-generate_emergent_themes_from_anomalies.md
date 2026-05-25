# Generate emergent themes from a Mode 3 run's anomaly segments

Entry point for the "extend" / "revise" anomaly-handling policies.
Returns a list of theme records (with subthemes + descriptions) that
`apply_framework_themes` merges into the final ThemeSet, tagged with
`theme_kind = "emergent"`. The framework themes proper are unaffected.

## Usage

``` r
.generate_emergent_themes_from_anomalies(
  coding_state,
  framework_spec,
  provider,
  audit_log = NULL,
  response_cache = NULL,
  live_tracker = NULL,
  methodology_override = NULL
)
```

## Details

Edge cases:

- 0 anomaly segments: returns empty list (caller should not normally
  reach this; `apply_framework_themes` short-circuits).

- 1 anomaly segment: returns one single-code emergent theme directly (no
  AI call – HAC degenerate case).

- N=2 anomaly segments: minimum viable HAC tree (1 internal node).
