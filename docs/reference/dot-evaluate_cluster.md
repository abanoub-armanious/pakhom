# Ask the AI to evaluate a cluster as a candidate theme or subtheme

Builds the cluster-summary prompt (with bias-mitigation context: most-
distant pair, full per-code list when small, top-N + extremes when
large) and calls
[`ai_complete()`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md)
with the
[`.theme_decision_schema()`](https://abanoub-armanious.github.io/pakhom/reference/dot-theme_decision_schema.md).
Post-validates the articulation via Phase 58 Tier 0 C-1's quality gates
(length / bucket-label opener / tautology). Returns a structured
decision record (decision, name, description, rationale, articulation).
Records the decision in `walk_state` and the `audit_log`.

## Usage

``` r
.evaluate_cluster(
  cluster_leaves,
  node_idx,
  level_label,
  parent_label = NULL,
  codes,
  distance_matrix,
  co_occurrence,
  walk_ctx
)
```
