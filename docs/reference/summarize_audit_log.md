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
  [`init_audit_log`](https://abanoub-armanious.github.io/pakhom/reference/init_audit_log.md).

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

- total_ai_requests:

  Integer (T1.4) — count of `ai_request` records (one per `ai_complete`
  call).

- total_tokens_used:

  Integer (T1.4) — sum of `usage_total` across all `ai_request` records
  (NA values dropped from the sum).

- ai_requests_by_model:

  Named integer vector (T1.4) — `ai_request` counts per model name.

- methodology_modes_observed:

  Character vector (T1.4) — unique non-NA values of the
  `methodology_mode` field across all records. Should normally be
  length-1 or length-0; length \>1 indicates a run where the methodology
  was changed mid-pipeline (T1.5 mode_change flow).

## Details

Sprint-4 T1.4 additions to the returned list: `total_ai_requests`,
`total_tokens_used`, `ai_requests_by_model`, and
`methodology_modes_observed`. Older audit logs missing these fields
return zero/empty values for the new keys; pre-T1.4 records still
surface in `decisions_by_type`/`decisions_by_step` as before.
