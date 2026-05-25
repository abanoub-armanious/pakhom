# Generate a deterministic-ish memo id

Format: `memo_<ISO8601-with-dashes>_<3-char-random-suffix>`. The
timestamp is the canonical ordering key (chronological). The 3-char
suffix avoids collisions when two memos are added in the same second –
e.g., during a fast batch of provocation responses. The random component
is drawn from `[a-z0-9]` so the id is filesystem-safe and shell-safe
without quoting.

## Usage

``` r
.generate_memo_id(timestamp = NULL)
```

## Arguments

- timestamp:

  ISO-8601 timestamp (defaults to now). Colons in the time portion are
  converted to dashes so the id is path-safe on Windows.

## Value

Character: a memo id.
