# Initialize the AI decision audit log

Opens (or creates) a JSONL file at `{output_dir}/ai_decisions.jsonl` and
returns an `AuditLog` S3 object that can be passed to
[`log_ai_decision`](log_ai_decision.md) throughout the pipeline.

## Usage

``` r
init_audit_log(output_dir)
```

## Arguments

- output_dir:

  Character. Base output directory for the current run.

## Value

An `AuditLog` S3 object (a list with class attribute).

## Details

If the file already exists it is opened in append mode so that resumed
runs continue the same log.
