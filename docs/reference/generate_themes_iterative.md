# Generate themes through iterative bottom-up merging

Starting from individual codes, the AI groups codes with similar
narratives into clusters through multiple passes. Each pass merges
clusters that share higher-level patterns. Stops when no more productive
merges exist.

## Usage

``` r
generate_themes_iterative(
  coding_state,
  provider,
  config = list(),
  learning_context = NULL,
  research_focus = "",
  concepts = NULL,
  audit_log = NULL
)
```

## Arguments

- coding_state:

  ProgressiveCodingState

- provider:

  AIProvider object

- config:

  Theme config section

- learning_context:

  LearningContext (or NULL)

- research_focus:

  Research focus string

- concepts:

  Character vector of core research concepts (or NULL)

- audit_log:

  An AuditLog object (from `init_audit_log`) for recording each merge
  decision (merge or standalone) and the final theme structure, or NULL
  to disable audit logging for this step.

## Value

ThemeSet S3 object with merge_history attached
