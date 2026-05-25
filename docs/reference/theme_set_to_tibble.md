# Convert ThemeSet to tibble for export/inspection

Flattens to a per-theme tibble (one row per theme). Subtheme structure
is summarized via subtheme name + description columns; per-subtheme
detail is available through the hierarchy (subtheme_name resolves to
first subtheme\$name, etc.). Per-subtheme detail tables are produced
separately by the report renderer.

## Usage

``` r
theme_set_to_tibble(theme_set)
```

## Arguments

- theme_set:

  ThemeSet object

## Value

tibble with one row per theme
