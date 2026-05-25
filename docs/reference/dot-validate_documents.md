# Validate and normalize the documents argument

Accepts NULL, an empty list, or a list of named lists with required
`$id` and `$text` fields and an optional `$title` field. Returns the
normalized list (or NULL when empty); errors with a clear message on a
malformed input rather than producing an opaque API 400.

## Usage

``` r
.validate_documents(documents)
```

## Arguments

- documents:

  Caller-supplied documents list (or NULL).

## Value

NULL when input is NULL/empty, otherwise the normalized list.

## Details

Each document gets a defaulted `$title = $id` when not supplied, because
the model's citations include a `document_title` that downstream code
uses for human-readable display; without a title the field would
round-trip as NULL, complicating the bridge.
