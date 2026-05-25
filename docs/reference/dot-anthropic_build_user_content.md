# Build the Anthropic user-message content array for a Citations API request

When `documents` is non-empty, the user message must be a content array
of one `document` block per source (with `citations.enabled=TRUE`)
followed by a `text` block carrying the prompt. When NULL/empty, returns
NULL so the caller falls back to the legacy `content = prompt` string
shape – this keeps existing (non-citations) request bodies bit-for-bit
identical.

## Usage

``` r
.anthropic_build_user_content(prompt, documents)
```

## Arguments

- prompt:

  User prompt string (the question/instruction to the model).

- documents:

  Validated documents list from
  [`.validate_documents`](https://abanoub-armanious.github.io/pakhom/reference/dot-validate_documents.md)
  or NULL.

## Value

List of content blocks, or NULL when no documents.

## Details

Anthropic accepts plain-text source via
`source = list(type="text", media_type="text/plain", data=text)` and
chunks it into sentences; returned citations carry char_location indices
into the original text. Phase 21a uses this mode exclusively;
custom_content / PDF / file-id sources can be added later by extending
this helper without breaking callers.
