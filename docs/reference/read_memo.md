# Read a memo from a ResearcherReflectionLog by id

Returns NULL when no memo with the given id exists – callers should
handle the NULL case explicitly rather than relying on an error, because
read-by-id is sometimes used as an existence check.

## Usage

``` r
read_memo(log, id)
```

## Arguments

- log:

  A `ResearcherReflectionLog`.

- id:

  Character: a memo id.

## Value

The `Memo` object, or NULL when not found.
