# Generate themes via HAC + AI-judged divisive tree walk

Phase 52 algorithm. Computes pairwise distance between codes (cosine on
code-name embeddings; Jaccard fallback on entry-id sets when embeddings
are unavailable), runs hierarchical agglomerative clustering (HAC,
ward.D2 linkage), then walks the resulting dendrogram top-down with an
AI judge at every internal node deciding coherent_theme / split_required
/ atomic_outlier. For each identified theme, walks one level deeper for
subthemes.

## Usage

``` r
generate_themes_iterative(
  coding_state,
  provider,
  config = list(),
  learning_context = NULL,
  research_focus = "",
  concepts = NULL,
  audit_log = NULL,
  response_cache = NULL,
  live_tracker = NULL,
  methodology_override = NULL
)
```

## Arguments

- coding_state:

  `ProgressiveCodingState`

- provider:

  `AIProvider` object

- config:

  Theme config section (most legacy knobs are now ignored; the algorithm
  has no merge-pass parameters)

- learning_context:

  Optional `LearningContext`

- research_focus:

  Research focus string

- concepts:

  Optional character vector of core research concepts

- audit_log:

  Optional `AuditLog` for recording each AI decision

- response_cache:

  Optional `ResponseCache` for raw response capture

- live_tracker:

  Optional `LiveTracker` (Phase 53). When provided, the cluster snapshot
  is rewritten after every AI decision so a researcher can
  `cat outputs/<run>/live/code_to_cluster.json` mid-run.

- methodology_override:

  Optional character (Phase 56). When non-NULL, replaces the provider's
  default methodology rules in every internal `ai_complete` call for
  this walk. Used by the Phase 54 emergent-themes pass to inject the
  Mode 3 inductive variant; NULL for normal Mode 2 + Mode 3 deductive
  callers.

## Value

`ThemeSet` S3 object with merge_history attached. The
merge_history\$tree_walk field carries the HAC tree + per-node decisions
for replay (Phase 52 audit trail).

## Details

The function name retains its pre-Phase-52 form for back-compat with the
single production caller (R/18_pipeline.R) and existing test fixtures. A
future cleanup phase may rename to `generate_themes()`.
