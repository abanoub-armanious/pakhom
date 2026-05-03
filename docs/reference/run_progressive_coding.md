# Run progressive sequential coding on all entries

Processes entries strictly one at a time. For each entry, the AI reads
the text and codes applicable segments using existing codes or creating
new ones. Entries with no applicable content are skipped.

## Usage

``` r
run_progressive_coding(
  data,
  provider,
  config = list(),
  learning_context = NULL,
  research_focus = "",
  checkpoint = NULL,
  concepts = NULL,
  resume_state = NULL,
  audit_log = NULL
)
```

## Arguments

- data:

  Tibble with std_text, std_id columns

- provider:

  AIProvider object

- config:

  Coding config section

- learning_context:

  LearningContext object (or NULL)

- research_focus:

  Research focus string

- checkpoint:

  CheckpointManager (or NULL)

- concepts:

  Character vector of core research concepts (or NULL)

- resume_state:

  ProgressiveCodingState from a previous partial run (or NULL)

- audit_log:

  An AuditLog object (from `init_audit_log`) for recording each coding
  decision (entry skipped, code assigned, new code created), or NULL to
  disable audit logging for this step.

## Value

ProgressiveCodingState with all entries processed
