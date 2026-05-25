# Initialize a content-addressable response cache

Creates the cache directory under the run output directory and returns a
`ResponseCache` S3 object that can be passed to
[`cache_response`](https://abanoub-armanious.github.io/pakhom/reference/cache_response.md)
and
[`read_cached_response`](https://abanoub-armanious.github.io/pakhom/reference/read_cached_response.md).

## Usage

``` r
init_response_cache(output_dir, config = NULL)
```

## Arguments

- output_dir:

  Character. Run output directory (where ai_decisions.jsonl lives). The
  cache lives at `output_dir/response_cache_dir/`.

- config:

  A ThematicConfig (or NULL). Reads `config$audit$capture_raw_responses`
  (default TRUE) and `config$audit$response_cache_dir` (default
  `"api_responses"`).

## Value

A ResponseCache S3 object.

## Details

If `config$audit$capture_raw_responses` is `FALSE` (a power-user
opt-out), the cache is created in disabled mode: write/read calls are
no-ops and the cache directory is not created. This matches the
conservative default in
[`default_config()`](https://abanoub-armanious.github.io/pakhom/reference/default_config.md)
(`capture_raw_responses = TRUE`).
