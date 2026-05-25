# Summarize the audit log via the existing summarize_audit_log helper

Adapts
[`summarize_audit_log()`](https://abanoub-armanious.github.io/pakhom/reference/summarize_audit_log.md)'s
public field names (`total_decisions`, `decisions_by_type`,
`decisions_by_step`) to the shorter names used in the transparency
report (total, by_decision_type, by_step) so the report surface stays
compact and stable across future audit-summary refactors.

## Usage

``` r
.tr_summarize_audit_log(run_dir)
```
