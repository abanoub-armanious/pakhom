# Count emotion occurrences across multi-label all_emotions column

Splits semicolon-separated all_emotions values and counts each emotion.
An entry expressing "sadness; anger" contributes one count to each.

## Usage

``` r
.count_multi_label_emotions(entries)
```

## Arguments

- entries:

  tibble with all_emotions column

## Value

tibble with emotion, n, pct columns
