# Read a cached raw response by prompt_hash

Looks up a previously-cached response. Used by `replay_run()` (OS.5,
future) to reproduce a prior run's AI calls from on-disk artifacts.

## Usage

``` r
read_cached_response(cache, prompt_hash)
```

## Arguments

- cache:

  A ResponseCache object

- prompt_hash:

  Character SHA-256 hex digest (from `ai_result$prompt_hash` or an audit
  log record).

## Value

The parsed `raw_response` list that was cached, or `NULL` if the cache
is disabled or no matching file exists.
