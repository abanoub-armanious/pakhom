# Compute a deterministic SHA-256 hash of an AI request's inputs

Hashes the JSON serialization of the request rather than the R object
itself, so the digest is stable across R versions, platforms, and
serialization-format changes. The set of fields hashed is exactly those
that determine the response: prompt + system_prompt + model +
temperature

- max_tokens + json_mode + response_schema + documents. Used as the
  cache key for replay_run() (OS.5).

## Usage

``` r
.compute_prompt_hash(
  prompt,
  system_prompt,
  model,
  temperature,
  max_tokens,
  json_mode,
  response_schema = NULL,
  documents = NULL
)
```

## Arguments

- prompt:

  User prompt string

- system_prompt:

  System prompt string (NULL becomes "")

- model:

  Model name

- temperature:

  Numeric temperature

- max_tokens:

  Integer max tokens

- json_mode:

  Logical

- response_schema:

  Optional JSON Schema (R list); NULL when no structured output was
  requested.

- documents:

  Optional list of source documents (Anthropic Citations API). NULL or
  empty list when citations were not requested. Hashing documents is
  required because the same prompt over different source corpora must
  produce different cache keys (otherwise replay_run() would silently
  return a citation-less response for a citations request, or vice
  versa).

## Value

Character: SHA-256 hex digest (64 chars)

## Details

Sprint-4 T1.2 added response_schema; T0.1 part 3b added documents. Pre-
addition callers (NULL for the new arg) produce the same hashes as
before because NULL serializes to "null" and the field was implicitly
absent.
