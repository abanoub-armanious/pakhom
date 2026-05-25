# Record one (entry, code, segment) assignment to the live tracker

Appends a JSONL line to `code_assignments.jsonl`. Safe to call with
`tracker = NULL` (no-op).

## Usage

``` r
live_record_assignment(
  tracker,
  entry_id,
  code_key,
  code_name,
  segment,
  is_new_code = FALSE,
  entry_index = NA_integer_
)
```

## Arguments

- tracker:

  A `LiveTracker` or NULL

- entry_id:

  Character entry id (std_id)

- code_key:

  Codebook key (sanitized id)

- code_name:

  Human-readable code name

- segment:

  List with `text`, `start_char`, `end_char`

- is_new_code:

  Logical; whether this assignment created the code

- entry_index:

  Integer position of the entry in the input (for ordering)

## Value

The (possibly updated) tracker, invisibly.

## Details

Phase 58 Tier 9 L-1/F-6: schema relationship to the audit log.
`code_assignment` events live in TWO places by design:

- `outputs/<run>/live/code_assignments.jsonl` (this writer): append-only
  EVENT log. One JSONL line per (entry_id, code_key, segment) triple.
  Carries text + offsets + verification_status inline so a researcher
  can `tail -F` the file mid-run.

- `outputs/<run>/ai_decisions.jsonl` (`R/audit_log.R`): append-only
  DECISION log. One JSONL line per audit event; `step = "coding"` +
  `decision_type = "code_assignment"` records the same logical event.
  Carries methodology stamp + AI metadata (model, request_id,
  methodology_mode) but NOT segment text or offsets.

The two are joined by `(entry_id, code_key)`. The split is intentional:
the audit log is the methodology-of-record (provenance, AC9 stamping,
replay) while the live tracker is the researcher's real-time view
(text + offsets + verification status for `tail -F`).
Pre-Phase-58-Tier-9 the docstring was silent on this; downstream
consumers had to infer the join from the field names. Now documented.
