# Lightweight check that a list looks like a valid JSON Schema for our use

Not a full JSON Schema validator – just enough to catch the OpenAI
strict-mode pitfalls at package-load / test time so a malformed schema
is caught before it hits the API. Specifically:

- Top-level must be an object schema with type = "object".

- Every object must have additionalProperties = FALSE.

- Every object's `required` must list every key in `properties`.

- Required and enum arrays must be lists (not character vectors) to
  avoid jsonlite auto_unbox collapsing single-element arrays.

## Usage

``` r
.validate_schema(schema, path = "$")
```

## Arguments

- schema:

  A schema list (e.g., the output of
  [`.coding_schema()`](https://abanoub-armanious.github.io/pakhom/reference/dot-coding_schema.md)).

- path:

  Internal: schema path, used for error messages.

## Value

TRUE invisibly if the schema is well-formed; otherwise stops with a
descriptive error pointing at the violating subschema.
