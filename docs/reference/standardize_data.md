# Standardize data to common schema

Standardize data to common schema

## Usage

``` r
standardize_data(data, column_map)
```

## Arguments

- data:

  Raw tibble

- column_map:

  Result of detect_columns()

## Value

tibble with std_id, std_text, std_author, std_timestamp, + original
metrics
