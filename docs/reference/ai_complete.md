# High-level AI completion with retry and error handling

Returns a structured list with the response `$content` alongside
provenance metadata (model, usage, raw_response, finish_reason,
prompt_hash, request_id). Callers that only need the response text
should extract `$content`; downstream audit-log capture (T1.4) and
`replay_run()` (OS.5) consume the other fields.

## Usage

``` r
ai_complete(
  provider,
  prompt,
  system_prompt = NULL,
  task = "coding",
  model = NULL,
  temperature = NULL,
  max_tokens = NULL,
  json_mode = FALSE,
  max_retries = 3,
  response_schema = NULL,
  documents = NULL,
  methodology_override = NULL
)
```

## Arguments

- provider:

  AIProvider object

- prompt:

  User prompt text

- system_prompt:

  Optional system prompt

- task:

  Task name for looking up max_tokens/temperature defaults

- model:

  Model override (NULL uses models\$primary)

- temperature:

  Temperature override

- max_tokens:

  Max tokens override

- json_mode:

  Logical: request JSON response format

- max_retries:

  Number of retry attempts on failure

- response_schema:

  Optional JSON Schema (as an R list) for the response shape. When
  provided (Sprint-4 T1.2), the providers enforce the schema
  server-side: OpenAI via
  `response_format = list(type = "json_schema", strict = TRUE, ...)`,
  Anthropic via a forced tool-use call whose `input_schema` is the
  schema. The returned `$content` is a JSON string that is guaranteed to
  parse and conform to the schema, so downstream
  [`parse_json_safely()`](https://abanoub-armanious.github.io/pakhom/reference/parse_json_safely.md)
  is a near-certain success path rather than a failure-tolerant
  fallback. When NULL, falls back to the pre-T1.2 `json_mode` path (see
  `R/structured_schemas.R` for the six task schemas the in-package
  callers use). Reasoning models (o1/o3/o4) silently fall back to
  `json_mode` because they don't support strict json_schema as of
  writing.

- documents:

  Optional list of source documents to enable Anthropic's Citations API
  (Sprint-4 T0.1 part 3b). Each element is a named list with `$id`
  (character, internal pakhom identifier preserved on the returned
  citations for downstream bridging), `$text` (character, the document
  content), and optional `$title` (character, becomes Anthropic's
  document title; defaults to `$id`). When non-empty, the user message
  is built as a content array with one `document` block per entry
  (`citations.enabled=TRUE`) followed by a `text` block carrying
  `prompt`. The model's response is parsed for citations; the returned
  `$citations` is a normalized list of citation objects. **Provider
  compatibility:** Citations API is Anthropic-only. Passing `documents`
  to an OpenAI provider raises an error. **Combining with
  `response_schema`:** Anthropic's Citations API is incompatible with
  the newer Structured Outputs (`output_config.format`); pakhom uses
  forced tool_use for `response_schema`, which is not formally
  documented as incompatible but produces no text blocks for citations
  to attach to. When both are passed the request is sent as-is and
  `$citations` will typically be empty – callers should choose one mode
  or the other per the architecture (phase 21c uses citations alone for
  Anthropic).

- methodology_override:

  Optional character (Phase 56; default NULL). When NULL, the call uses
  `provider$methodology_rules` as the system-prompt prefix (AC9
  default). When a non-NULL string is supplied, it replaces that prefix
  for this single call only – used by the Phase 54 inductive
  emergent-themes pass to swap in the inductive variant of the Mode 3
  rule (`generate_methodology_rules (config, inductive_pass = TRUE)`)
  instead of the default deductive rule that forbids new-construct
  generation. Empty string (`""`) suppresses the rules prefix entirely.

## Value

A list with the following fields (canonical shape, normalized across
OpenAI and Anthropic):

- `content`: character. The response text.

- `model`: character. Model that generated the response (echoed from the
  API; may differ from the requested model if the provider resolved an
  alias such as `gpt-4o` -\> `gpt-4o-2024-08-06`).

- `usage`: list with integer fields `prompt_tokens`,
  `completion_tokens`, `total_tokens`. Anthropic's
  `input_tokens`/`output_tokens` are remapped to the OpenAI-style names;
  total is computed when missing.

- `finish_reason`: character. Normalized to `"stop"`, `"length"`, or
  `"tool_use"` (Anthropic's
  `end_turn`/`max_tokens`/`stop_sequence`/`tool_use` are remapped).

- `raw_response`: list. Full parsed API response body, for replay (OS.5)
  and debugging.

- `prompt_hash`: character. SHA-256 hex digest of the request inputs
  (prompt + system_prompt + model + temperature + max_tokens +
  json_mode + response_schema + documents). Used as the cache key for
  `replay_run()`; stable across R versions and platforms because the
  underlying hash is computed over a JSON serialization of the inputs,
  not the R object.

- `request_id`: character or `NA_character_`. Provider-assigned request
  identifier (from the `x-request-id` header for OpenAI or `request-id`
  for Anthropic; falls back to `$id` from the response body).

- `citations`: list. Normalized citations extracted from text blocks in
  the response (Anthropic Citations API only). Each element is a list
  whose field names exactly mirror Anthropic's citation schema (`type`,
  `cited_text`, `document_index`, `document_title`, plus type-specific
  fields: `start_char_index`/`end_char_index` for char_location,
  `start_page_number`/`end_page_number` for page_location,
  `start_block_index`/`end_block_index` for content_block_location).
  Empty list when `documents` was NULL or no citations were returned.
  Phase 21b's
  [`make_quotes_from_citations()`](https://abanoub-armanious.github.io/pakhom/reference/make_quotes_from_citations.md)
  converts these to `QuoteProvenance` objects with
  `citation_source = "anthropic_citations_api"`.

## Details

Note: This is a Sprint-4 T1.1 refactor. Prior to T1.1 this function
returned a bare character string; the structured-list return is the
single most leveraged change in Sprint-4 because it unblocks T1.4 (audit
log raw-response capture), T1.2 (Structured Outputs migration), and OS.5
(replay_run from cached responses) simultaneously. The function is
internal (not exported), so the change touches only in-package callers.
