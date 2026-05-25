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
  audit_log = NULL,
  response_cache = NULL,
  fabrication_log = NULL,
  framework_spec = NULL,
  live_tracker = NULL
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

- response_cache:

  An optional ResponseCache object (from
  [`init_response_cache`](https://abanoub-armanious.github.io/pakhom/reference/init_response_cache.md)).
  When provided, raw API responses for each per-entry coding
  ai_complete() call are written to the cache and a reference is
  recorded in the audit log (T1.4). Pass `NULL` to skip raw-response
  capture.

- fabrication_log:

  An optional FabricationLog object (from
  [`init_fabrication_log`](https://abanoub-armanious.github.io/pakhom/reference/init_fabrication_log.md)).
  T0.1 verification ALWAYS runs – each per-segment AI-attributed
  verbatim text is checked against the entry via the four-step ladder,
  and fabricated segments are dropped regardless of whether a log is
  supplied. When `fabrication_log` is non-NULL, fabrications are also
  written to `outputs/<run>/fabrication_log.csv` as a CSV audit artifact
  for the methodology paper's KPI. Pass `NULL` to skip the CSV (the
  default for tests + non-pipeline callers).

- framework_spec:

  Optional `FrameworkSpec` object (from
  [`load_framework_spec`](https://abanoub-armanious.github.io/pakhom/reference/load_framework_spec.md)).
  When supplied (Mode 3 / Framework Applied), the codebook is
  pre-populated with the framework's constructs and the AI is
  constrained to apply them verbatim (no NEW: prefix path). Anomaly
  segments go to a dedicated "anomaly" key. NULL preserves Mode 2
  (free-form codebook) behavior.

- live_tracker:

  Optional `LiveTracker` (Phase 53; from
  [`init_live_tracker`](https://abanoub-armanious.github.io/pakhom/reference/init_live_tracker.md)).
  When provided, every coded segment streams to
  `outputs/<run>/live/code_assignments.jsonl` and `codebook_live.json`
  is rewritten after every entry so a researcher can `tail -F` or `cat`
  those files mid-run. Pass `NULL` (default) to disable.

## Value

ProgressiveCodingState with all entries processed
