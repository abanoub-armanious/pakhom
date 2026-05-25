# Record an AI request with the structured response from `ai_complete`

Convenience wrapper around
[`log_ai_decision`](https://abanoub-armanious.github.io/pakhom/reference/log_ai_decision.md)
that records an `"ai_request"` decision and unpacks the structured
fields from
[`ai_complete`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md)'s
return value (T1.1) into the audit record: model, finish_reason,
prompt_hash, request_id, and per-record token usage.

## Usage

``` r
log_ai_request(audit, step, ai_result, response_cache = NULL, ...)
```

## Arguments

- audit:

  An `AuditLog` object.

- step:

  Pipeline step (e.g., `"coding"`, `"sentiment"`). Must be one of
  `.valid_audit_steps`.

- ai_result:

  The list returned by
  [`ai_complete`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md).

- response_cache:

  Optional
  [`init_response_cache`](https://abanoub-armanious.github.io/pakhom/reference/init_response_cache.md)
  object. When provided, the raw_response is written to the cache and
  the path is recorded in the JSONL record.

- ...:

  Additional caller-specific named fields to include in the record
  (e.g., `entry_id = "abc"`, `batch_idx = 3`).

## Value

Invisibly returns `audit`.

## Details

If a `ResponseCache` is provided (Sprint-4 T1.4), the `raw_response` is
also written to the cache (content-addressable by `prompt_hash`) and the
cache path is recorded in the audit log entry. This separation keeps the
JSONL file lightweight while preserving the full API response for replay
(OS.5).

Silently no-ops on `NULL ai_result` (e.g., when `ai_complete` threw and
the caller's tryCatch returned NULL) so callers can wrap calls in
`tryCatch` and still call `log_ai_request` unconditionally.
