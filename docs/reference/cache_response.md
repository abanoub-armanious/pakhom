# Write a raw API response to the cache, indexed by prompt_hash

If the cache is disabled, returns `NA_character_` without writing. If a
file with the same prompt_hash already exists (i.e., the same request
was made earlier in the run), the write is skipped (deduplication) and
the existing relative path is returned. Otherwise, writes the
`raw_response` as pretty-printed JSON.

## Usage

``` r
cache_response(cache, ai_result)
```

## Arguments

- cache:

  A ResponseCache object from
  [`init_response_cache`](https://abanoub-armanious.github.io/pakhom/reference/init_response_cache.md).

- ai_result:

  The structured list returned by
  [`ai_complete`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md).
  Must contain `prompt_hash` (used as the key) and `raw_response` (the
  payload to cache).

## Value

Character. Path to the cached response file, RELATIVE to `output_dir`
(so audit log records remain portable when the run directory is moved).
`NA_character_` if the cache is disabled, `ai_result` is malformed, or
the write fails.

## Details

Errors during write are caught and logged; the function returns
`NA_character_` on failure rather than propagating, because audit-log
capture should never break the analysis pipeline.
