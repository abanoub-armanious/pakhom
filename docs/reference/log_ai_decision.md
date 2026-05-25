# Record a single AI decision in the audit log

Appends one JSON line to the JSONL audit file.

## Usage

``` r
log_ai_decision(audit, step, decision_type, ...)
```

## Arguments

- audit:

  An `AuditLog` object returned by
  [`init_audit_log`](https://abanoub-armanious.github.io/pakhom/reference/init_audit_log.md).

- step:

  Character. Pipeline step — one of `"coding"`, `"sentiment"`,
  `"theming"`, `"saturation"`, `"insight"`, or `"synthesis"`.

- decision_type:

  Character. Type of decision — one of `"code_assignment"`,
  `"new_code_created"`, `"entry_skipped"`, `"merge_decision"`,
  `"sentiment_assignment"`, `"saturation_signal"`, `"theme_structure"`,
  or `"insight_generation"`.

- ...:

  Additional named fields to include in the JSON record (e.g.
  `entry_id`, `code_name`, `rationale`, `model`, `tokens_used`).

## Value

Invisibly returns `audit` (for pipe-friendly usage).
