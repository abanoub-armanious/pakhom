# Preprocess text data for analysis

Cleans text, removes artifacts, filters by length, and removes
duplicates. Supports platform-specific cleaning rules and custom regex
patterns.

## Usage

``` r
preprocess_text(data, config = list())
```

## Arguments

- data:

  Standardized tibble (must have std_text column)

- config:

  Preprocessing config section from YAML

## Value

Filtered tibble with cleaned text
