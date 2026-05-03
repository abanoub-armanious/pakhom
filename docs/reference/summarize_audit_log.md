# Summarize the AI decision audit log

Reads `{output_dir}/ai_decisions.jsonl` and produces a summary of all
recorded decisions. Useful for post-analysis review and reporting.

## Usage

``` r
summarize_audit_log(output_dir)
```

## Arguments

- output_dir:

  Character. The same output directory passed to
  [`init_audit_log`](init_audit_log.md).

## Value

A named list with:

- total_decisions:

  Integer — total number of logged decisions.

- decisions_by_type:

  Named integer vector — counts per `decision_type`.

- decisions_by_step:

  Named integer vector — counts per pipeline `step`.

- new_codes_timeline:

  A `data.frame` with columns `timestamp` and `cumulative_codes`,
  showing the running total of new codes over time.

- entries_skipped:

  Integer — number of `entry_skipped` decisions.

- merge_decisions_accepted:

  Integer — merge decisions where `action == "merge"`.

- merge_decisions_standalone:

  Integer — merge decisions where `action == "standalone"`.
