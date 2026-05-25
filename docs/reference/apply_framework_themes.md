# Apply framework constructs as themes + handle anomalies (Mode 3)

Generates the framework themes from the framework spec, then dispatches
on `framework_spec$anomaly_handling` to decide what happens to segments
that didn't fit any framework construct (the "anomaly" code's segments).

## Usage

``` r
apply_framework_themes(
  coding_state,
  framework_spec,
  provider = NULL,
  output_dir = NULL,
  audit_log = NULL,
  response_cache = NULL,
  live_tracker = NULL,
  config = NULL
)
```

## Arguments

- coding_state:

  A `ProgressiveCodingState` from a Mode 3 run. The codebook keys are
  construct_ids (plus "anomaly").

- framework_spec:

  A loaded `FrameworkSpec`.

- provider:

  Optional `AIProvider`. Required when `anomaly_handling` is `"extend"`
  or `"revise"` AND anomalies are present (the inductive pass calls the
  AI). When NULL under those policies, falls back to bracket behavior
  with a warning.

- output_dir:

  Optional run directory path. Required for `"revise"` policy to write
  `framework_review.csv`.

- audit_log:

  Optional `AuditLog` threaded into the inductive pass's AI calls.

- response_cache:

  Optional `ResponseCache` threaded through.

- live_tracker:

  Optional `LiveTracker` threaded through.

- config:

  Optional `ThematicConfig` (Phase 56). When supplied and the framework
  spec's anomaly_handling is `"extend"` or `"revise"`, the inductive
  emergent-themes pass receives a methodology rules override (the
  inductive-pass variant of the Mode 3 rule, computed via
  `generate_methodology_rules(config, inductive_pass = TRUE)`) so the AI
  doesn't see the contradictory "Do NOT generate new framework
  constructs" rule from the deductive default. NULL (the default) falls
  through to the provider's default rules – safe for legacy/test
  callers; the inductive pass will see the deductive rule alongside its
  inductive prompt (the Phase 54 deferral iii contradiction the override
  resolves).

## Value

A `ThemeSet` S3 object with framework themes and (under
"extend"/"revise") emergent themes. Themes carry a `theme_kind` field of
`"framework"` \| `"emergent"` \| `"anomaly_bracket"` so the report
renderer can section them.

## Details

Per AC8 (modes are configurations of one architecture, never separate
code paths): the returned ThemeSet has the same shape as one produced by
[`generate_themes_iterative()`](https://abanoub-armanious.github.io/pakhom/reference/generate_themes_iterative.md),
so all downstream consumers (cascade_theme_assignments,
aggregate_theme_statistics, report rendering) work without modification.

**Anomaly policy dispatch** (Phase 54):

- `"bracket"`: legacy pre-Phase-54 behavior. Appends a single "Anomaly
  (non-fitting)" theme containing all non-fitting segments.

- `"extend"` (default): runs an abductive inductive pass on the anomaly
  segments, producing a section of **emergent themes** parallel to the
  framework themes. Each emergent theme is tagged
  `theme_kind = "emergent"` so the report renderer surfaces it
  separately. The framework is NOT mutated – AC2's "framework fixed at
  run start" invariant is intact; the analysis output gains a new
  section, that's all.

- `"revise"`: same as `"extend"` plus writes `framework_review.csv` to
  `output_dir` (one row per anomaly segment + suggested-edit columns for
  the researcher). The existing `after_themes` review pause point
  (configured via `config$analysis$review_points$after_themes`) is the
  integration point where the researcher inspects the CSV alongside
  framework + emergent themes and decides whether to edit the framework
  spec for a future run. A dedicated `after_framework_coding` pause is
  deferred (requires resumable runs with in-flight spec edits, beyond
  current checkpoint scope).

The framework themes themselves are unchanged – AI is still constrained
to apply constructs verbatim during the main coding pass
(R/09_coding.R); the emergent-themes path operates only on segments that
the AI ALREADY classified as "anomaly" during deductive coding. The
deductive integrity of the framework analysis is preserved; the
inductive pass operates on deductive coding's residuals.
