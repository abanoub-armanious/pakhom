# Parse JSON safely with automatic repair for truncated/malformed responses

Tries standard parsing first, then progressively more aggressive repair
strategies. Returns NULL (not an error) when all strategies fail.

## Usage

``` r
parse_json_safely(response, expected_key = NULL, max_repair_attempts = 3)
```

## Arguments

- response:

  Raw JSON string from AI API

- expected_key:

  If provided, validates this key exists in parsed result

- max_repair_attempts:

  Number of repair strategies to try (1-3)

## Value

Parsed R object (list/data.frame) or NULL on failure
