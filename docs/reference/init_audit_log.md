# Initialize the AI decision audit log

Opens (or creates) a JSONL file at `{output_dir}/ai_decisions.jsonl` and
returns an `AuditLog` S3 object that can be passed to
[`log_ai_decision`](https://abanoub-armanious.github.io/pakhom/reference/log_ai_decision.md)
throughout the pipeline.

## Usage

``` r
init_audit_log(output_dir, config = NULL)
```

## Arguments

- output_dir:

  Character. Base output directory for the current run.

- config:

  A ThematicConfig (or NULL). When non-NULL, `config$methodology$mode`
  is captured and auto-stamped on every subsequent audit record.

## Value

An `AuditLog` S3 object (a list with class attribute).

## Details

If the file already exists it is opened in append mode so that resumed
runs continue the same log.

Sprint-4 T1.4 additions:

- Accepts `config` so methodology metadata can flow into every audit
  record. When `config$methodology$mode` is set,
  [`log_ai_decision`](https://abanoub-armanious.github.io/pakhom/reference/log_ai_decision.md)
  auto-stamps it on every JSONL record. This is the load-bearing change
  for cross-mode comparison: every decision in the log is unambiguously
  attributable to the methodology it was made under.

- Counter state (`n_written`) is held in an internal environment so
  increments from
  [`log_ai_decision`](https://abanoub-armanious.github.io/pakhom/reference/log_ai_decision.md)
  mutate correctly across function calls. (Pre-T1.4, `n_written` was a
  plain list field that suffered R's pass-by-value semantics and stayed
  at 0 forever – a latent bug in `close_audit_log`'s "N decisions
  recorded" log message. Fixed here.)
