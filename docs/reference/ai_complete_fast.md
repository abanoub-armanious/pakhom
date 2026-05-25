# Send a quick completion using the fast/cheap model

Thin wrapper around
[`ai_complete`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md)
that selects the provider's `models$fast` model. Returns the same
structured list shape as
[`ai_complete()`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md)
– callers extract `$content` for the text.

## Usage

``` r
ai_complete_fast(
  provider,
  prompt,
  system_prompt = NULL,
  task = "sentiment",
  json_mode = FALSE,
  response_schema = NULL,
  documents = NULL
)
```

## Arguments

- provider:

  AIProvider object

- prompt:

  User prompt

- system_prompt:

  System prompt

- task:

  Task name

- json_mode:

  JSON mode

- response_schema:

  Optional JSON schema (see
  [`ai_complete`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md))

- documents:

  Optional source documents for Anthropic Citations API (see
  [`ai_complete`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md))

## Value

List of the same shape as
[`ai_complete`](https://abanoub-armanious.github.io/pakhom/reference/ai_complete.md).
